class ZCL_ZOSQL_WHERE_PROCESSOR definition
  public
  inheriting from ZCL_ZOSQL_EXPRESSION_PROCESSOR
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT optional
      !IO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS
      !IO_ITERATOR type ref to ZIF_ZOSQL_ITERATOR
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE .

  methods ZIF_ZOSQL_EXPRESSION_PROCESSOR~CREATE_NEW_INSTANCE
    redefinition .
  methods ZIF_ZOSQL_EXPRESSION_PROCESSOR~INITIALIZE_BY_PARSED_SQL
    redefinition .
protected section.

  data MO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT .

  methods INITIALIZE
    redefinition .
  methods _CHECK_ELEMENTARY
    redefinition .
  methods _CHECK_WITH_COMPARE_OPERATOR
    redefinition .
  methods _GET_REF_TO_RIGHT_OPERAND
    redefinition .
  methods _INIT_ELEMENTARY
    redefinition .
private section.

  types:
    BEGIN OF TY_FIELD_PARAMETER_MAPPING,
           fieldname_in_sql  TYPE string,
           parameter_name    TYPE string,
         END OF TY_FIELD_PARAMETER_MAPPING .
  types:
    ty_field_parameter_mapping_tab TYPE STANDARD TABLE OF ty_field_parameter_mapping WITH KEY fieldname_in_sql .

  data MS_PARAMETER_FOR_VALUE type ZOSQL_DB_LAYER_PARAM .
  data MV_RIGHT_PART_SUBQUERY_SQL type STRING .
  data MV_SUBQUERY_SPECIFICATOR type STRING .
  constants C_EXISTS type STRING value 'EXISTS' ##NO_TEXT.

  methods _CHECK_FOR_IN_WHEN_SUBQUERY
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RV_CONDITIONS_TRUE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _CHECK_SUBQUERY_CORRECT
    raising
      ZCX_ZOSQL_ERROR .
  methods _GET_SUBQUERY_SPECIFICATOR
    importing
      !IO_NODE_ELEMENTARY_CONDITION type ref to ZCL_ZOSQL_PARSER_NODE
    returning
      value(RV_SUBQUERY_SPECIFICATOR) type STRING .
  methods _GET_SUBQUERY_SQL
    importing
      !IO_NODE_ELEMENTARY_CONDITION type ref to ZCL_ZOSQL_PARSER_NODE
    returning
      value(RV_SUBQUERY_SQL) type STRING .
  methods _CHECK_IF_VALUE_IS_SUBQUERY
    importing
      !IO_NODE_ELEMENTARY_CONDITION type ref to ZCL_ZOSQL_PARSER_NODE
    returning
      value(RV_VALUE_IS_SUBQUERY) type ABAP_BOOL .
  methods _GET_REF_TO_RIGHT_OPERAND_SUBQ
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RD_REF_TO_RIGHT_OPERAND) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods _IF_SUBQUERY_RETURNS_ANY_REC
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RV_SUBQUERY_RETURNS_ANY_REC) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _INIT_EXISTS
    importing
      !IO_NODE_EXISTS type ref to ZCL_ZOSQL_PARSER_NODE .
  methods _INIT_SUBQUERY
    importing
      !IO_NODE_ELEMENTARY_CONDITION type ref to ZCL_ZOSQL_PARSER_NODE .
  methods _RUN_SUBQUERY_AND_GET_RESULT
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RD_REF_TO_RESULT_AS_TABLE) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
ENDCLASS.



CLASS ZCL_ZOSQL_WHERE_PROCESSOR IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA: lo_factory TYPE REF TO zif_zosql_factory.

    super->constructor( io_parameters = io_parameters
                        io_iterator   = io_iterator
                        iv_new_syntax = iv_new_syntax ).

    IF io_zosql_test_environment IS BOUND.
      mo_zosql_test_environment = io_zosql_test_environment.
    ELSE.
      CREATE OBJECT lo_factory TYPE zcl_zosql_factory.
      mo_zosql_test_environment = lo_factory->get_test_environment( ).
    ENDIF.
  endmethod.


  method INITIALIZE.
    DATA: lt_child_nodes_next_level TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_first_node             TYPE REF TO zcl_zosql_parser_node.

    IF io_parent_node_of_expr IS NOT BOUND.
      mv_empty_condition_flag = abap_true.
      RETURN.
    ENDIF.

    lt_child_nodes_next_level = io_parent_node_of_expr->get_child_nodes( ).

    READ TABLE lt_child_nodes_next_level INDEX 1 INTO lo_first_node.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE lo_first_node->node_type.
      WHEN zcl_zosql_parser_recurs_desc=>node_type-expression_exists.
        _init_exists( io_parent_node_of_expr ).
      WHEN OTHERS.
        CALL METHOD super->initialize
          EXPORTING
            io_parent_node_of_expr = io_parent_node_of_expr.
    ENDCASE.
  endmethod.


  method ZIF_ZOSQL_EXPRESSION_PROCESSOR~CREATE_NEW_INSTANCE.
    CREATE OBJECT ro_processor TYPE zcl_zosql_where_processor
      EXPORTING
        io_zosql_test_environment = mo_zosql_test_environment
        io_parameters             = mo_parameters
        io_iterator               = mo_iterator
        iv_new_syntax             = mv_new_syntax.
  endmethod.


  METHOD zif_zosql_expression_processor~initialize_by_parsed_sql.
    super->zif_zosql_expression_processor~initialize_by_parsed_sql( io_parent_node_of_expr ).
    _check_subquery_correct( ).
  ENDMETHOD.


  METHOD _check_elementary.

    DATA: lv_processed TYPE abap_bool.

    CASE mv_operation.
      WHEN c_exists.
        rv_conditions_true = _if_subquery_returns_any_rec( io_iteration_position ).
        lv_processed = abap_true.
      WHEN c_in.
        IF mv_right_part_subquery_sql IS NOT INITIAL.
          rv_conditions_true = _check_for_in_when_subquery( io_iteration_position ).
          lv_processed = abap_True.
        ENDIF.
    ENDCASE.

    IF lv_processed = abap_true.
      IF mv_not_flag = abap_true.
        rv_conditions_true = zcl_zosql_utils=>boolean_not( rv_conditions_true ).
      ENDIF.
    ELSE.
      rv_conditions_true = super->_check_elementary( io_iteration_position ).
    ENDIF.
  ENDMETHOD.


  method _CHECK_FOR_IN_WHEN_SUBQUERY.

    DATA: ld_result              TYPE REF TO data,
          ld_ref_to_left_operand TYPE REF TO data.

    FIELD-SYMBOLS: <lt_result>                  TYPE STANDARD TABLE,
                   <lv_value_to_find_in_result> TYPE any,
                   <ls_result>                  TYPE any,
                   <lv_value_from_subquery>     TYPE any.

    ld_result = _run_subquery_and_get_result( io_iteration_position ).
    IF ld_result IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN ld_result->* TO <lt_result>.

    ld_ref_to_left_operand = _get_ref_to_left_operand( io_iteration_position ).

    IF ld_ref_to_left_operand IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN ld_ref_to_left_operand->* TO <lv_value_to_find_in_result>.

    LOOP AT <lt_result> ASSIGNING <ls_result>.
      ASSIGN COMPONENT 1 OF STRUCTURE <ls_result> TO <lv_value_from_subquery>.
      CHECK sy-subrc = 0.

      IF <lv_value_to_find_in_result> = <lv_value_from_subquery>.
        rv_conditions_true = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  endmethod.


  method _CHECK_IF_VALUE_IS_SUBQUERY.
    IF ms_right_operand-fieldname_or_value IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF io_node_elementary_condition->exists_child_node_with_type(
          zcl_zosql_parser_recurs_desc=>node_type-subquery_opening_bracket ) = abap_true.

      rv_value_is_subquery = abap_true.
    ENDIF.
  endmethod.


  method _CHECK_SUBQUERY_CORRECT.

    DATA: lo_subquery TYPE REF TO zcl_zosql_subquery_in_where.

    IF mv_right_part_subquery_sql IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_subquery
      EXPORTING
        io_zosql_test_environment     = mo_zosql_test_environment
        iv_subquery_sql               = mv_right_part_subquery_sql
        io_iterator_parent            = mo_iterator
        io_parameters_of_parent_query = mo_parameters
        iv_new_syntax                 = mv_new_syntax.

    lo_subquery->run_subquery_and_get_result( ).
  endmethod.


  METHOD _check_with_compare_operator.

    DATA: lv_all_values_should_match TYPE abap_bool,
          ld_result_of_subquery      TYPE REF TO data,
          lv_all_values_match        TYPE abap_bool,
          lv_any_value_matches       TYPE abap_bool,
          ld_ref_to_left_operand     TYPE REF TO data.

    FIELD-SYMBOLS: <lt_result_of_subquery>       TYPE STANDARD TABLE,
                   <ls_result_of_subquery>       TYPE any,
                   <lv_first_column_of_subquery> TYPE any,
                   <lv_value_of_left_operand>    TYPE any.

    IF mv_subquery_specificator IS NOT INITIAL.
      IF mv_subquery_specificator = 'ALL'.
        lv_all_values_should_match = abap_true.
      ENDIF.

      ld_result_of_subquery = _run_subquery_and_get_result( io_iteration_position ).
      ASSIGN ld_result_of_subquery->* TO <lt_result_of_subquery>.

      ld_ref_to_left_operand = _get_ref_to_left_operand( io_iteration_position ).
      ASSIGN ld_ref_to_left_operand->* TO <lv_value_of_left_operand>.

      lv_all_values_match = abap_true.
      LOOP AT <lt_result_of_subquery> ASSIGNING <ls_result_of_subquery>.
        ASSIGN COMPONENT 1 OF STRUCTURE <ls_result_of_subquery> TO <lv_first_column_of_subquery>.
        IF _compare_values( iv_value_left  = <lv_value_of_left_operand>
                            iv_operation   = mv_operation
                            iv_value_right = <lv_first_column_of_subquery> ) = abap_true.

          lv_any_value_matches = abap_true.
        ELSE.
          lv_all_values_match = abap_false.
        ENDIF.
      ENDLOOP.

      IF lv_all_values_should_match = abap_true.
        rv_conditions_true = lv_all_values_match.
      ELSE.
        rv_conditions_true = lv_any_value_matches.
      ENDIF.
    ELSE.
      rv_conditions_true = super->_check_with_compare_operator( io_iteration_position ).
    ENDIF.
  ENDMETHOD.


  method _GET_REF_TO_RIGHT_OPERAND.
    IF mv_right_part_subquery_sql IS NOT INITIAL.
      rd_ref_to_right_operand = _get_ref_to_right_operand_subq( io_iteration_position ).
    ELSE.
      rd_ref_to_right_operand = super->_get_ref_to_right_operand( io_iteration_position ).
    ENDIF.
  endmethod.


  method _GET_REF_TO_RIGHT_OPERAND_SUBQ.

    DATA: ld_result    TYPE REF TO data.

    FIELD-SYMBOLS: <lt_result>            TYPE STANDARD TABLE,
                   <ls_result_first_line> TYPE any,
                   <lv_value_of_first_field> TYPE any,
                   <lv_value_of_result>      TYPE any.

    ld_result = _run_subquery_and_get_result( io_iteration_position ).

    ASSIGN ld_result->* TO <lt_result>.
    READ TABLE <lt_result> INDEX 1 ASSIGNING <ls_result_first_line>.
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT 1 OF STRUCTURE <ls_result_first_line> TO <lv_value_of_first_field>.
    CHECK sy-subrc = 0.

    CREATE DATA rd_ref_to_right_operand LIKE <lv_value_of_first_field>.
    ASSIGN rd_ref_to_right_operand->* TO <lv_value_of_result>.

    <lv_value_of_result> = <lv_value_of_first_field>.
  endmethod.


  method _GET_SUBQUERY_SPECIFICATOR.
    rv_subquery_specificator =
      io_node_elementary_condition->get_child_node_token_with_type(
        iv_node_type = zcl_zosql_parser_recurs_desc=>node_type-subquery_specificator
        iv_ucase     = abap_true ).
  endmethod.


  method _GET_SUBQUERY_SQL.

    DATA: lt_nodes_of_subquery        TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_end_node_closing_bracket TYPE REF TO zcl_zosql_parser_node,
          lo_start_node_of_subquery   TYPE REF TO zcl_zosql_parser_node,
          lo_end_node_of_subquery     TYPE REF TO zcl_zosql_parser_node.

    lt_nodes_of_subquery = io_node_elementary_condition->get_child_nodes_recursive( ).

    READ TABLE lt_nodes_of_subquery INDEX LINES( lt_nodes_of_subquery ) INTO lo_end_node_closing_bracket.
    IF sy-subrc = 0.
      IF lo_end_node_closing_bracket->node_type = zcl_zosql_parser_recurs_desc=>node_type-closing_bracket.
        DELETE lt_nodes_of_subquery INDEX LINES( lt_nodes_of_subquery ).
      ENDIF.
    ENDIF.

    READ TABLE lt_nodes_of_subquery INDEX LINES( lt_nodes_of_subquery ) INTO lo_end_node_of_subquery.
    IF sy-subrc = 0.
      READ TABLE lt_nodes_of_subquery WITH KEY table_line->node_type = zcl_zosql_parser_recurs_desc=>node_type-select
        INTO lo_start_node_of_subquery.
      IF sy-subrc = 0.
        rv_subquery_sql =
          io_node_elementary_condition->get_sql_as_range_of_tokens(
            iv_start_token_index = lo_start_node_of_subquery->token_index
            iv_end_token_index   = lo_end_node_of_subquery->token_index ).
      ENDIF.
    ENDIF.
  endmethod.


  METHOD _if_subquery_returns_any_rec.
    DATA: ld_result  TYPE REF TO data.

    FIELD-SYMBOLS: <lt_result> TYPE STANDARD TABLE.

    ld_result = _run_subquery_and_get_result( io_iteration_position ).
    IF ld_result IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN ld_result->* TO <lt_result>.
    IF <lt_result> IS NOT INITIAL.
      rv_subquery_returns_any_rec = abap_true.
    ENDIF.
  ENDMETHOD.


  method _INIT_ELEMENTARY.
    super->_init_elementary( io_node_elementary_condition ).

    IF _check_if_value_is_subquery( io_node_elementary_condition ) = abap_true.
      _init_subquery( io_node_elementary_condition ).
    ENDIF.
  endmethod.


  method _INIT_EXISTS.
    CLEAR ms_left_operand-fieldname_or_value.
    mv_operation = c_exists.
    _init_subquery( io_node_exists ).
  endmethod.


  method _INIT_SUBQUERY.
    mv_right_part_subquery_sql = _get_subquery_sql( io_node_elementary_condition ).
    mv_subquery_specificator = _get_subquery_specificator( io_node_elementary_condition ).
  endmethod.


  METHOD _run_subquery_and_get_result.

    DATA: lo_subquery TYPE REF TO zcl_zosql_subquery_in_where.

    CREATE OBJECT lo_subquery
      EXPORTING
        io_zosql_test_environment     = mo_zosql_test_environment
        iv_subquery_sql               = mv_right_part_subquery_sql
        io_iteration_position_parent  = io_iteration_position
        io_parameters_of_parent_query = mo_parameters
        iv_new_syntax                 = mv_new_syntax.

    rd_ref_to_result_as_table = lo_subquery->run_subquery_and_get_result( ).
  ENDMETHOD.
ENDCLASS.
