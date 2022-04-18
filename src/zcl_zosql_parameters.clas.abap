class ZCL_ZOSQL_PARAMETERS definition
  public
  create public .

public section.

  methods ADD_IGNORE_PARAMETERS
    importing
      !IT_IGNORE_PARAMETERS type ZOSQL_DB_LAYER_PARAMS .
  methods IF_PARAMETER_MUST_BE_IGNORED
    importing
      !IV_PARAMETER_NAME_IN_SELECT type CLIKE
    returning
      value(RV_PARAM_MUST_BE_IGNORED) type ABAP_BOOL .
  methods CHECK_PARAMETER_EXISTS
    importing
      !IV_PARAMETER_NAME_IN_SELECT type CLIKE
    returning
      value(RV_EXISTS) type ABAP_BOOL .
  methods GET_PARAMETER_DATA
    importing
      !IV_PARAMETER_NAME_IN_SELECT type CLIKE
    returning
      value(RS_PARAMETER_DATA) type ZOSQL_DB_LAYER_PARAM .
  methods GET_PARAMETER_VALUE_REF
    importing
      !IV_PARAMETER_NAME_IN_SELECT type ZOSQL_PARAM_NAME_IN_SELECT
    returning
      value(RD_REF_TO_VALUE_OF_PARAMETER) type ref to DATA .
  methods GET_ALL_PARAMETERS
    returning
      value(RT_PARAMETERS) type ZOSQL_DB_LAYER_PARAMS .
  methods CONSTRUCTOR
    importing
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS .
protected section.
private section.

  types:
    BEGIN OF TY_PARAMETER .
    INCLUDE TYPE zosql_db_layer_param.
    TYPES: ignore_flag TYPE abap_bool,
         END OF ty_parameter .

  data:
    MT_PARAMETERS type STANDARD TABLE OF ty_parameter WITH KEY param_name_in_select .

  methods _APPEND_PARAMETERS
    importing
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS
      value(IV_IGNORE_FLAG) type ABAP_BOOL default ABAP_FALSE .
ENDCLASS.



CLASS ZCL_ZOSQL_PARAMETERS IMPLEMENTATION.


  method ADD_IGNORE_PARAMETERS.
    _append_parameters( it_parameters  = it_ignore_parameters
                        iv_ignore_flag = abap_true ).
  endmethod.


  METHOD CHECK_PARAMETER_EXISTS.

    DATA: ls_parameter_data TYPE zosql_db_layer_param.

    ls_parameter_data = get_parameter_data( iv_parameter_name_in_select ).
    IF ls_parameter_data IS NOT INITIAL.
      rv_exists = abap_true.
    ENDIF.
  ENDMETHOD.


  method CONSTRUCTOR.
    _append_parameters( it_parameters  = it_parameters
                        iv_ignore_flag = abap_false ).
  endmethod.


  method GET_ALL_PARAMETERS.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = mt_parameters
                                               IMPORTING et_table_dest = rt_parameters ).
  endmethod.


  method GET_PARAMETER_DATA.
    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF mt_parameters.

    LOOP AT mt_parameters ASSIGNING <ls_parameter>.
      IF zcl_zosql_utils=>to_upper_case( iv_parameter_name_in_select )
        = zcl_zosql_utils=>to_upper_case( <ls_parameter>-param_name_in_select ).

        MOVE-CORRESPONDING <ls_parameter> TO rs_parameter_data.
        EXIT.
      ENDIF.
    ENDLOOP.
  endmethod.


  method GET_PARAMETER_VALUE_REF.

    DATA: ls_parameter_data TYPE zosql_db_layer_param.

    ls_parameter_data = get_parameter_data( iv_parameter_name_in_select ).

    IF ls_parameter_data-parameter_value_ref IS BOUND.
      rd_ref_to_value_of_parameter = ls_parameter_data-parameter_value_ref.
    ELSEIF ls_parameter_data-parameter_value_range IS NOT INITIAL.
      rd_ref_to_value_of_parameter = zcl_zosql_utils=>copy_and_return_as_ref_to_data( ls_parameter_data-parameter_value_range ).
    ELSE.
      rd_ref_to_value_of_parameter = zcl_zosql_utils=>copy_and_return_as_ref_to_data( ls_parameter_data-parameter_value_single ).
    ENDIF.
  endmethod.


  method IF_PARAMETER_MUST_BE_IGNORED.

    DATA: ls_parameter_data TYPE zosql_db_layer_param.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF mt_parameters.

    ls_parameter_data = get_parameter_data( iv_parameter_name_in_select ).
    IF ls_parameter_data IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE mt_parameters WITH KEY param_name_in_select = ls_parameter_data-param_name_in_select
      ASSIGNING <ls_parameter>.
    IF sy-subrc = 0.
      rv_param_must_be_ignored = <ls_parameter>-ignore_flag.
    ENDIF.
  endmethod.


  method _APPEND_PARAMETERS.

    FIELD-SYMBOLS: <ls_parameter_src>  LIKE LINE OF it_parameters,
                   <ls_parameter_dest> LIKE LINE OF mt_parameters.

    LOOP AT it_parameters ASSIGNING <ls_parameter_src>.
      APPEND INITIAL LINE TO mt_parameters ASSIGNING <ls_parameter_dest>.
      MOVE-CORRESPONDING <ls_parameter_src> TO <ls_parameter_dest>.
      <ls_parameter_dest>-ignore_flag = iv_ignore_flag.
    ENDLOOP.
  endmethod.
ENDCLASS.
