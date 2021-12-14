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
      !IO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE .

  methods ZIF_ZOSQL_EXPRESSION_PROCESSOR~CREATE_NEW_INSTANCE
    redefinition .
protected section.

  methods _GET_REF_TO_RIGHT_OPERAND
    redefinition .
  methods _PARSE_ELEMENTARY
    redefinition .
private section.

  data MO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS .
  data MS_PARAMETER_FOR_VALUE type ZOSQL_DB_LAYER_PARAM .

  methods _CHECK_IF_VALUE_IS_PARAMETER
    returning
      value(RV_VALUE_IS_PARAMETER) type ABAP_BOOL .
  methods _FILL_PARAMETER_DATA_FOR_COND .
ENDCLASS.



CLASS ZCL_ZOSQL_WHERE_PROCESSOR IMPLEMENTATION.


  method CONSTRUCTOR.
    super->constructor( iv_new_syntax ).
    mo_parameters = io_parameters.
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


  method _FILL_PARAMETER_DATA_FOR_COND.
    ms_parameter_for_value = mo_parameters->get_parameter_data( m_fieldname_right_or_value ).
  endmethod.


  method _GET_REF_TO_RIGHT_OPERAND.
    IF _check_if_value_is_parameter( ) = abap_true.
      rd_ref_to_right_operand = mo_parameters->get_parameter_value_ref( ms_parameter_for_value-param_name_in_select ).
    ELSE.
      rd_ref_to_right_operand = super->_get_ref_to_right_operand( io_iteration_position ).
    ENDIF.
  endmethod.


  method _PARSE_ELEMENTARY.

    super->_parse_elementary( io_sql_parser                 = io_sql_parser
                              iv_id_of_node_elementary_cond = iv_id_of_node_elementary_cond ).

    IF _check_if_value_is_parameter( ) = abap_true.

      IF mv_new_syntax = abap_true.
        m_fieldname_right_or_value = delete_host_variable_symbol( m_fieldname_right_or_value ).
      ENDIF.

      _fill_parameter_data_for_cond( ).
    ELSE.
      _clear_quotes_from_value( ).
    ENDIF.
  endmethod.
ENDCLASS.
