class ZCL_ZOSQL_SET_PARSER definition
  public
  inheriting from ZCL_ZOSQL_PARSER_BASE
  create public .

public section.

  methods CONSTRUCTOR
    importing
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE
      !IO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS .
  methods PARSE_SET
    importing
      !IV_SET_STATEMENT type CLIKE .
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
ENDCLASS.



CLASS ZCL_ZOSQL_SET_PARSER IMPLEMENTATION.


  method CONSTRUCTOR.
    super->constructor( iv_new_syntax ).
    mo_parameters = io_parameters.
  endmethod.


  METHOD PARSE_SET.

    DATA: ltd_words         TYPE TABLE OF string,
          lv_word           TYPE string,
          ls_set_field      TYPE ty_set_field,
          lv_value_expected TYPE abap_bool,
          lV_is_parameter   TYPE abap_bool.

    REFRESH mt_set_fields.

    SPLIT iv_set_statement AT space INTO TABLE ltd_words.

    LOOP AT ltd_words INTO lv_word.
      IF lv_word = '='.
        lv_value_expected = abap_true.
        CONTINUE.
      ENDIF.

      IF lv_value_expected = abap_true.
        ls_set_field-value_part = zcl_zosql_utils=>clear_quotes_from_value( lv_word ).
        APPEND ls_set_field TO mt_set_fields.
        lv_value_expected = abap_false.
      ELSE.
        ls_set_field-fieldname = lv_word.
      ENDIF.
    ENDLOOP.

    LOOP AT mt_set_fields INTO ls_set_field.

      lv_is_parameter = mo_parameters->check_parameter_exists( ls_set_field-value_part ).
      IF lv_is_parameter <> abap_true AND mv_new_syntax = abap_true.
        lv_is_parameter =
          mo_parameters->check_parameter_exists(
            _delete_host_variable_symbol( ls_set_field-value_part ) ).

        IF lv_is_parameter = abap_true.
          ls_set_field-value_part = _delete_host_variable_symbol( ls_set_field-value_part ).
        ENDIF.
      ENDIF.

      IF lv_is_parameter = abap_true.
        ls_set_field-parameter_data = mo_parameters->get_parameter_data( ls_set_field-value_part ).
        MODIFY mt_set_fields FROM ls_set_field.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


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
        <lv_field> = <ls_set_field>-value_part.
      ENDIF.
    ENDLOOP.
  endmethod.
ENDCLASS.
