class ZCL_ZOSQL_WHERE_PROCESSOR definition
  public
  inheriting from ZCL_ZOSQL_EXPRESSION_PROCESSOR
  create public .

public section.

  methods IS_PARAMETER_COMPARED_AS_RANGE
    importing
      !IV_PARAMETER_NAME_IN_SELECT type ZOSQL_PARAM_NAME_IN_SELECT
    returning
      value(RV_IS_COMPARED_AS_RANGE) type ABAP_BOOL .
  methods CONSTRUCTOR
    importing
      !IO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT optional
      !IO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE .

  methods ZIF_ZOSQL_EXPRESSION_PROCESSOR~CREATE_NEW_INSTANCE
    redefinition .
  methods ZIF_ZOSQL_EXPRESSION_PROCESSOR~INITIALIZE_BY_PARSED_SQL
    redefinition .
protected section.

  methods _GET_REF_TO_RIGHT_OPERAND
    redefinition .
  methods _PROCESS_ELEMENTARY
    redefinition .
  methods _CHECK_ELEMENTARY
    redefinition .
private section.

  types:
    BEGIN OF TY_FIELD_PARAMETER_MAPPING,
           fieldname_in_sql  TYPE string,
           parameter_name    TYPE string,
         END OF TY_FIELD_PARAMETER_MAPPING .
  types:
    ty_field_parameter_mapping_tab TYPE STANDARD TABLE OF ty_field_parameter_mapping WITH KEY fieldname_in_sql .

  data MO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS .
  data MS_PARAMETER_FOR_VALUE type ZOSQL_DB_LAYER_PARAM .
  data M_RIGHT_PART_SUBQUERY_SQL type STRING .
  data MO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT .
  constants C_EXISTS type STRING value 'EXISTS' ##NO_TEXT.

  methods _TRY_TO_GET_PARAMETER_NAME
    returning
      value(RV_PARAMETER_NAME) type STRING .
  methods _CHECK_IF_VALUE_IS_SUBQUERY
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      value(IV_ID_OF_NODE_ELEMENTARY_COND) type I
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
  methods _PROCESS_EXISTS
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      value(IV_ID_OF_NODE_EXISTS) type I .
  methods _PROCESS_SUBQUERY
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      value(IV_ID_OF_NODE_ELEMENTARY_COND) type I .
  methods _RUN_SUBQUERY_AND_GET_RESULT
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RD_REF_TO_RESULT_AS_TABLE) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods _CHECK_IF_VALUE_IS_PARAMETER
    returning
      value(RV_VALUE_IS_PARAMETER) type ABAP_BOOL .
  methods _FILL_PARAMETER_DATA_FOR_COND .
ENDCLASS.



CLASS ZCL_ZOSQL_WHERE_PROCESSOR IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA: lo_factory TYPE REF TO zif_zosql_factory.

    super->constructor( iv_new_syntax ).
    mo_parameters = io_parameters.

    IF io_zosql_test_environment IS BOUND.
      mo_zosql_test_environment = io_zosql_test_environment.
    ELSE.
      CREATE OBJECT lo_factory TYPE zcl_zosql_factory.
      mo_zosql_test_environment = lo_factory->get_test_environment( ).
    ENDIF.
  endmethod.


  METHOD IS_PARAMETER_COMPARED_AS_RANGE.

    DATA: lo_where_parser TYPE REF TO zcl_zosql_where_processor.

    FIELD-SYMBOLS: <ls_condition> LIKE LINE OF mt_or_conditions.

    LOOP AT mt_or_conditions ASSIGNING <ls_condition>.
      lo_where_parser ?= <ls_condition>-processor.
      IF lo_where_parser->is_parameter_compared_as_range( iv_parameter_name_in_select ) = abap_true.
        rv_is_compared_as_range = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    LOOP AT mt_and_conditions ASSIGNING <ls_condition>.
      lo_where_parser ?= <ls_condition>-processor.
      IF lo_where_parser->is_parameter_compared_as_range( iv_parameter_name_in_select ) = abap_true.
        rv_is_compared_as_range = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    IF ms_not_condition-processor IS BOUND.
      lo_where_parser ?= ms_not_condition-processor.
      IF lo_where_parser->is_parameter_compared_as_range( iv_parameter_name_in_select ) = abap_true.
        rv_is_compared_as_range = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF _check_if_value_is_parameter( ) = abap_true
      AND m_operation = c_in
      AND ms_parameter_for_value-param_name_in_select = iv_parameter_name_in_select.

      rv_is_compared_as_range = abap_true.
    ENDIF.
  ENDMETHOD.


  method ZIF_ZOSQL_EXPRESSION_PROCESSOR~CREATE_NEW_INSTANCE.
    CREATE OBJECT ro_processor TYPE zcl_zosql_where_processor
      EXPORTING
        io_parameters = mo_parameters.
  endmethod.


  METHOD zif_zosql_expression_processor~initialize_by_parsed_sql.

    DATA: lt_child_nodes_next_level TYPE zcl_zosql_parser_recurs_desc=>ty_tree,
          ls_first_node             TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    lt_child_nodes_next_level = io_sql_parser->get_child_nodes( iv_id_of_node_to_parse ).

    READ TABLE lt_child_nodes_next_level INDEX 1 INTO ls_first_node.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE ls_first_node-node_type.
      WHEN zcl_zosql_parser_recurs_desc=>node_type-expression_exists.
        _process_exists( io_sql_parser        = io_sql_parser
                         iv_id_of_node_exists = ls_first_node-id ).
      WHEN OTHERS.
        CALL METHOD super->zif_zosql_expression_processor~initialize_by_parsed_sql
          EXPORTING
            io_sql_parser          = io_sql_parser
            iv_id_of_node_to_parse = iv_id_of_node_to_parse.
    ENDCASE.
  ENDMETHOD.


  METHOD _check_elementary.

    CASE m_operation.
      WHEN c_exists.
        rv_conditions_true = _if_subquery_returns_any_rec( io_iteration_position ).
      WHEN OTHERS.
        rv_conditions_true = super->_check_elementary( io_iteration_position ).
    ENDCASE.
  ENDMETHOD.


  METHOD _CHECK_IF_VALUE_IS_PARAMETER.

    DATA: lv_parameter_name TYPE string.

    lv_parameter_name = _try_to_get_parameter_name( ).

    IF lv_parameter_name IS NOT INITIAL.
      rv_value_is_parameter = abap_true.
    ENDIF.
  ENDMETHOD.


  method _CHECK_IF_VALUE_IS_SUBQUERY.

    DATA: ls_node_subquery TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    IF m_fieldname_right_or_value IS NOT INITIAL.
      RETURN.
    ENDIF.

    ls_node_subquery =
      io_sql_parser->get_child_node_with_type( iv_node_id   = iv_id_of_node_elementary_cond
                                               iv_node_type = zcl_zosql_parser_recurs_desc=>node_type-subquery_opening_bracket ).

    IF ls_node_subquery IS NOT INITIAL.
      rv_value_is_subquery = abap_true.
    ENDIF.
  endmethod.


  method _FILL_PARAMETER_DATA_FOR_COND.

    DATA: lv_parameter_name TYPE string.

    lv_parameter_name = _try_to_get_parameter_name( ).
    ms_parameter_for_value = mo_parameters->get_parameter_data( lv_parameter_name ).
  endmethod.


  method _GET_REF_TO_RIGHT_OPERAND.
    IF _check_if_value_is_parameter( ) = abap_true.
      rd_ref_to_right_operand = mo_parameters->get_parameter_value_ref( ms_parameter_for_value-param_name_in_select ).
    ELSEIF m_right_part_subquery_sql IS NOT INITIAL.
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


  METHOD _process_elementary.
    super->_process_elementary( io_sql_parser                 = io_sql_parser
                                iv_id_of_node_elementary_cond = iv_id_of_node_elementary_cond ).

    IF _check_if_value_is_parameter( ) = abap_true.

      IF mv_new_syntax = abap_true.
        m_fieldname_right_or_value = delete_host_variable_symbol( m_fieldname_right_or_value ).
      ENDIF.

      _fill_parameter_data_for_cond( ).

    ELSEIF _check_if_value_is_subquery( io_sql_parser                 = io_sql_parser
                                        iv_id_of_node_elementary_cond = iv_id_of_node_elementary_cond ) = abap_true.

      _process_subquery( io_sql_parser                 = io_sql_parser
                         iv_id_of_node_elementary_cond = iv_id_of_node_elementary_cond ).
    ELSE.
      _clear_quotes_from_value( ).
    ENDIF.
  ENDMETHOD.


  method _PROCESS_EXISTS.
    CLEAR m_fieldname_left.
    m_operation = c_exists.
    _process_subquery( io_sql_parser                 = io_sql_parser
                       iv_id_of_node_elementary_cond = iv_id_of_node_exists ).
  endmethod.


  method _PROCESS_SUBQUERY.

    DATA: lt_nodes_of_subquery  TYPE zcl_zosql_parser_recurs_desc=>ty_tree.

    FIELD-SYMBOLS: <ls_end_node_closing_bracket> TYPE zcl_zosql_parser_recurs_desc=>ty_node,
                   <ls_start_node_of_subquery>   TYPE zcl_zosql_parser_recurs_desc=>ty_node,
                   <ls_end_node_of_subquery>     TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    lt_nodes_of_subquery = io_sql_parser->get_child_nodes_recursive( iv_id_of_node_elementary_cond ).

    READ TABLE lt_nodes_of_subquery INDEX LINES( lt_nodes_of_subquery ) ASSIGNING <ls_end_node_closing_bracket>.
    IF sy-subrc = 0.
      IF <ls_end_node_closing_bracket>-node_type = zcl_zosql_parser_recurs_desc=>node_type-closing_bracket.
        DELETE lt_nodes_of_subquery INDEX LINES( lt_nodes_of_subquery ).
      ENDIF.
    ENDIF.

    READ TABLE lt_nodes_of_subquery INDEX LINES( lt_nodes_of_subquery ) ASSIGNING <ls_end_node_of_subquery>.
    IF sy-subrc = 0.
      READ TABLE lt_nodes_of_subquery WITH KEY node_type = zcl_zosql_parser_recurs_desc=>node_type-select
        ASSIGNING <ls_start_node_of_subquery>.
      IF sy-subrc = 0.
        m_right_part_subquery_sql =
          io_sql_parser->get_sql_as_range_of_tokens( iv_start_token_index = <ls_start_node_of_subquery>-token_index
                                                     iv_end_token_index   = <ls_end_node_of_subquery>-token_index ).
      ENDIF.
    ENDIF.
  endmethod.


  METHOD _run_subquery_and_get_result.

    DATA: lo_subquery TYPE REF TO zcl_zosql_subquery_in_where.

    CREATE OBJECT lo_subquery
      EXPORTING
        io_zosql_test_environment     = mo_zosql_test_environment
        iv_subquery_sql               = m_right_part_subquery_sql
        io_iteration_position_parent  = io_iteration_position
        io_parameters_of_parent_query = mo_parameters
        iv_new_syntax                 = mv_new_syntax.

    rd_ref_to_result_as_table = lo_subquery->run_subquery_and_get_result( ).
  ENDMETHOD.


  method _TRY_TO_GET_PARAMETER_NAME.
    DATA: lv_fieldname_full TYPE string,
          lv_param_without_host_symbol TYPE string.

    IF mo_parameters->check_parameter_exists( m_fieldname_right_or_value ) = abap_true.
      rv_parameter_name = m_fieldname_right_or_value.
      RETURN.
    ENDIF.

    IF m_dataset_name_or_alias_right IS NOT INITIAL.
      CONCATENATE m_dataset_name_or_alias_right m_fieldname_right_or_value
        INTO lv_fieldname_full SEPARATED BY '~'.

      IF mo_parameters->check_parameter_exists( lv_fieldname_full ) = abap_true.
        rv_parameter_name = lv_fieldname_full.
        RETURN.
      ENDIF.
    ENDIF.

    IF mv_new_syntax = abap_true.

      lv_param_without_host_symbol = delete_host_variable_symbol( m_fieldname_right_or_value ).

      IF mo_parameters->check_parameter_exists( lv_param_without_host_symbol ) = abap_true.
        rv_parameter_name = lv_param_without_host_symbol.
      ENDIF.
    ENDIF.
  endmethod.
ENDCLASS.
