class ZCL_ZOSQL_SET_PROCESSOR definition
  public
  inheriting from ZCL_ZOSQL_PROCESSOR_BASE
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    raising
      ZCX_ZOSQL_ERROR .
  methods UPDATE_RECORD
    importing
      !ID_REF_TO_RECORD type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
protected section.
private section.

  types:
    BEGIN OF TY_SET_FIELD,
           fieldname TYPE string,
           value_part TYPE string,
           parameter_data TYPE Zosql_DB_LAYER_PARAM,
         END OF ty_set_field .

  data MO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS .
  data:
    MT_SET_FIELDS  TYPE STANDARD TABLE OF ty_set_field WITH KEY fieldname .

  methods _CHECK_FIELD_NAME
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IS_SET_FIELD type TY_SET_FIELD
    raising
      ZCX_ZOSQL_ERROR .
  methods _CHECK_FOR_CORRECTNESS
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    raising
      ZCX_ZOSQL_ERROR .
  methods _CHECK_SET_FIELD_CORRECT
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IS_SET_FIELD type TY_SET_FIELD
    raising
      ZCX_ZOSQL_ERROR .
  methods _CHECK_SET_FIELD_VALUE
    importing
      !IS_SET_FIELD type TY_SET_FIELD
    raising
      ZCX_ZOSQL_ERROR .
  methods _IF_FIELD_EXISTS_IN_TABLE
    importing
      !IV_TABLE_NAME type CLIKE
      !IV_FIELDNAME type CLIKE
    returning
      value(RV_EXISTS) type ABAP_BOOL .
  methods _VALUE_IS_PARAMETER
    importing
      !IS_SET_FIELD type TY_SET_FIELD
    returning
      value(RV_IS_PARAMETER) type ABAP_BOOL .
  methods _INITIALIZE_BY_PARSED_SQL
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC .
ENDCLASS.



CLASS ZCL_ZOSQL_SET_PROCESSOR IMPLEMENTATION.


  method CONSTRUCTOR.
    super->constructor( ).
    mo_parameters = io_parameters.
    _initialize_by_parsed_sql( io_sql_parser ).
    _check_for_correctness( io_sql_parser ).
  endmethod.


  method UPDATE_RECORD.

    DATA: ld_ref_to_parameter_value TYPE REF TO data.

    FIELD-SYMBOLS: <ls_record>    TYPE any,
                   <ls_set_field> LIKE LINE OF mt_set_fields,
                   <lv_field>     TYPE any,
                   <lv_parameter_value> TYPE any.

    ASSIGN id_ref_to_record->* TO <ls_record>.

    LOOP AT mt_set_fields ASSIGNING <ls_set_field>.
      ASSIGN COMPONENT <ls_set_field>-fieldname OF STRUCTURE <ls_Record> TO <lv_field>.
      IF sy-subrc <> 0.
        MESSAGE e065 WITH <ls_set_field>-fieldname INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.

      IF <ls_set_field>-parameter_data IS NOT INITIAL.
        ld_ref_to_parameter_value = mo_parameters->get_parameter_value_ref( <ls_set_field>-parameter_data-param_name_in_select ).
        ASSIGN ld_ref_to_parameter_value->* TO <lv_parameter_value>.
        <lv_field> = <lv_parameter_value>.
      ELSE.
        <lv_field> = zcl_zosql_utils=>clear_quotes_from_value( <ls_set_field>-value_part ).
      ENDIF.
    ENDLOOP.
  endmethod.


  method _CHECK_FIELD_NAME.
    DATA: lo_parser_helper      TYPE REF TO zcl_zosql_parser_helper,
          lv_update_table_name  TYPE tabname.

    CREATE OBJECT lo_parser_helper
      EXPORTING
        io_sql_parser = io_sql_parser.

    lv_update_table_name = lo_parser_helper->get_update_table_name( ).

    IF _if_field_exists_in_table( iv_table_name = lv_update_table_name
                                  iv_fieldname  = is_set_field-fieldname ) <> abap_true.
      MESSAGE e097 WITH is_set_field-fieldname INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.
  endmethod.


  method _CHECK_FOR_CORRECTNESS.

    FIELD-SYMBOLS: <ls_set_field> LIKE LINE OF mt_set_fields.

    LOOP AT mt_set_fields ASSIGNING <ls_set_field>.
      _check_set_field_correct( io_sql_parser = io_sql_parser
                                is_set_field  = <ls_set_field> ).
    ENDLOOP.
  endmethod.


  method _CHECK_SET_FIELD_CORRECT.

    _check_field_name( io_sql_parser = io_sql_parser
                       is_set_field  = is_set_field ).

    _check_set_field_value( is_set_field ).
  endmethod.


  method _CHECK_SET_FIELD_VALUE.
    IF _value_is_parameter( is_set_field ) <> abap_true.
      zcl_zosql_utils=>raise_if_constant_incorrect( is_set_field-value_part ).
    ENDIF.
  endmethod.


  METHOD _if_field_exists_in_table.

    DATA: lv_table_name TYPE tabname,
          lv_fieldname  TYPE fieldname.

    lv_table_name = zcl_zosql_utils=>to_upper_case( iv_table_name ).
    lv_fieldname  = zcl_zosql_utils=>to_upper_case( iv_fieldname ).

    SELECT SINGLE fieldname
      INTO zcl_zosql_utils=>dummy
      FROM dd03l
      WHERE tabname   = lv_table_name
        AND fieldname = lv_fieldname
        AND as4local  = 'A'
        AND as4vers   = '0000'.

    IF sy-subrc = 0.
      rv_exists = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD _initialize_by_parsed_sql.

    DATA: lo_sql_parser_helper    TYPE REF TO zcl_zosql_parser_helper,
          lo_node_set             TYPE REF TO zcl_zosql_parser_node,
          lt_nodes_set_fields     TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          ls_set_field            TYPE ty_set_field,
          lt_child_nodes_of_field TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          ls_node_equation        TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          lo_node_new_value       TYPE REF TO zcl_zosql_parser_node,
          lv_is_parameter         TYPE abap_bool,
          lo_node_set_field       TYPE REF TO zcl_zosql_parser_node.

    CREATE OBJECT lo_sql_parser_helper
      EXPORTING
        io_sql_parser = io_sql_parser.
    lo_sql_parser_helper->get_key_nodes_of_sql_update( IMPORTING eo_node_set   = lo_node_set
                                                                 ev_new_syntax = mv_new_syntax ).

    lt_nodes_set_fields = lo_node_set->get_child_nodes( ).

    LOOP AT lt_nodes_set_fields INTO lo_node_set_field.
      CLEAR ls_set_field.
      ls_set_field-fieldname = lo_node_set_field->token_ucase.

      lo_node_new_value =
        lo_node_set_field->get_child_node_with_type(
          zcl_zosql_parser_recurs_desc=>node_type-update_set_value ).

      CHECK lo_node_new_value IS BOUND.

      ls_set_field-value_part = lo_node_new_value->token.
      APPEND ls_set_field TO mt_set_fields.
    ENDLOOP.

    LOOP AT mt_set_fields INTO ls_set_field.

      lv_is_parameter = mo_parameters->check_parameter_exists( ls_set_field-value_part ).
      IF lv_is_parameter <> abap_true AND mv_new_syntax = abap_true.
        lv_is_parameter =
          mo_parameters->check_parameter_exists(
            delete_host_variable_symbol( ls_set_field-value_part ) ).

        IF lv_is_parameter = abap_true.
          ls_set_field-value_part = delete_host_variable_symbol( ls_set_field-value_part ).
        ENDIF.
      ENDIF.

      IF lv_is_parameter = abap_true.
        ls_set_field-parameter_data = mo_parameters->get_parameter_data( ls_set_field-value_part ).
        MODIFY mt_set_fields FROM ls_set_field.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  method _VALUE_IS_PARAMETER.
    IF is_set_field-parameter_data IS NOT INITIAL.
      rv_is_parameter = abap_true.
    ENDIF.
  endmethod.
ENDCLASS.
