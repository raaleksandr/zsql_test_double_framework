class ZCL_TESTABLE_DB_PARAMETERS definition
  public
  create public .

public section.

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
  methods CONSTRUCTOR
    importing
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS .
protected section.
private section.

  data MT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS .
ENDCLASS.



CLASS ZCL_TESTABLE_DB_PARAMETERS IMPLEMENTATION.


  METHOD check_parameter_exists.

    DATA: ls_parameter_data TYPE zosql_db_layer_param.

    ls_parameter_data = get_parameter_data( iv_parameter_name_in_select ).
    IF ls_parameter_data IS NOT INITIAL.
      rv_exists = abap_true.
    ENDIF.
  ENDMETHOD.


  method CONSTRUCTOR.
    mt_parameters = it_parameters.
  endmethod.


  method GET_PARAMETER_DATA.
    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF mt_parameters.

    LOOP AT mt_parameters ASSIGNING <ls_parameter>.
      IF zcl_testable_db_layer_utils=>to_upper_case( iv_parameter_name_in_select )
        = zcl_testable_db_layer_utils=>to_upper_case( <ls_parameter>-param_name_in_select ).

        rs_parameter_data = <ls_parameter>.
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
      rd_ref_to_value_of_parameter = zcl_testable_db_layer_utils=>copy_and_return_as_ref_to_data( ls_parameter_data-parameter_value_range ).
    ELSE.
      rd_ref_to_value_of_parameter = zcl_testable_db_layer_utils=>copy_and_return_as_ref_to_data( ls_parameter_data-parameter_value_single ).
    ENDIF.
  endmethod.
ENDCLASS.
