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
protected section.

  methods _GET_REF_TO_RIGHT_OPERAND
    redefinition .
  methods _PROCESS_ELEMENTARY
    redefinition .
private section.

  data MO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS .
  data MS_PARAMETER_FOR_VALUE type ZOSQL_DB_LAYER_PARAM .
  data M_RIGHT_PART_SUBQUERY_SQL type STRING .
  data MO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT .

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
  methods _PROCESS_SUBQUERY
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      value(IV_ID_OF_NODE_ELEMENTARY_COND) type I .
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


  METHOD _CHECK_IF_VALUE_IS_PARAMETER.
    rv_value_is_parameter = mo_parameters->check_parameter_exists( m_fieldname_right_or_value ).

    IF rv_value_is_parameter <> abap_true AND mv_new_syntax = abap_true.
      rv_value_is_parameter =
        mo_parameters->check_parameter_exists(
                         delete_host_variable_symbol( m_fieldname_right_or_value )
                       ).
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
    ms_parameter_for_value = mo_parameters->get_parameter_data( m_fieldname_right_or_value ).
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

    DATA: lo_subquery  TYPE REF TO zcl_zosql_db_layer_fake,
          ld_result    TYPE REF TO data.

    FIELD-SYMBOLS: <lt_result>            TYPE STANDARD TABLE,
                   <ls_result_first_line> TYPE any,
                   <lv_value_of_first_field> TYPE any,
                   <lv_value_of_result>      TYPE any.

    CREATE OBJECT lo_subquery
      EXPORTING
        io_zosql_test_environment = mo_zosql_test_environment.

    lo_subquery->zif_zosql_db_layer~select( EXPORTING iv_select          = m_right_part_subquery_sql
                                            IMPORTING ed_result_as_table = ld_result ).

    ASSIGN ld_result->* TO <lt_result>.
    READ TABLE <lt_result> INDEX 1 ASSIGNING <ls_result_first_line>.
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT 1 OF STRUCTURE <ls_result_first_line> TO <lv_value_of_first_field>.
    CHECK sy-subrc = 0.

    CREATE DATA rd_ref_to_right_operand LIKE <lv_value_of_first_field>.
    ASSIGN rd_ref_to_right_operand->* TO <lv_value_of_result>.

    <lv_value_of_result> = <lv_value_of_first_field>.
  endmethod.


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
ENDCLASS.
