class ZCL_ZOSQL_GROUPBY_ITER_POS definition
  public
  inheriting from ZCL_ZOSQL_ITERATOR_POSITION
  create public .

public section.

  methods GET_GROUPED_VALUE_OF_DATA_SET
    importing
      !IV_DATASET_NAME_OR_ALIAS type CLIKE
      !IV_FIELDNAME type CLIKE
      !IV_GROUPBY_FUNCTION type CLIKE
      value(IV_DISTINCT) type ABAP_BOOL optional
    returning
      value(RD_REF_TO_AGGREGATED_VALUE) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .

  methods ADD_DATA_SET_DATA
    redefinition .
  methods GET_FIELD_REF_OF_DATA_SET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZOSQL_GROUPBY_ITER_POS IMPLEMENTATION.


  METHOD add_data_set_data.
    FIELD-SYMBOLS: <ls_data_set_data> LIKE LINE OF mt_data_sets_data,
                   <ls_current_line>  TYPE any,
                   <lt_data>          TYPE STANDARD TABLE.

    ASSIGN id_ref_to_current_line->* TO <ls_current_line>.

    READ TABLE mt_data_sets_data WITH KEY dataset_name  = iv_dataset_name
                                          dataset_alias = iv_dataset_alias
                                          ASSIGNING <ls_data_set_data>.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO mt_data_sets_data ASSIGNING <ls_data_set_data>.
      <ls_data_set_data>-dataset_name  = iv_dataset_name.
      <ls_data_set_data>-dataset_alias = iv_dataset_alias.

      CREATE DATA <ls_data_set_data>-ref_to_data LIKE STANDARD TABLE OF <ls_current_line>.
    ENDIF.

    ASSIGN <ls_data_set_data>-ref_to_data->* TO <lt_data>.
    APPEND <ls_current_line> TO <lt_data>.
  ENDMETHOD.


  method GET_FIELD_REF_OF_DATA_SET.
    DATA: ld_data TYPE REF TO data.

    FIELD-SYMBOLS: <ls_data> TYPE any,
                   <lt_data> TYPE STANDARD TABLE,
                   <lv_value> TYPE any.

    ld_data = get_line_for_data_set_ref( iv_dataset_name_or_alias ).
    ASSIGN ld_data->* TO <ls_data>.

    IF zcl_zosql_utils=>is_internal_table( <ls_data> ) = abap_true.
      ASSIGN <ls_data> TO <lt_data>.
      READ TABLE <lt_data> INDEX 1 ASSIGNING <ls_data>.
      IF sy-subrc = 0.
        ASSIGN COMPONENT iv_fieldname OF STRUCTURE <ls_data> TO <lv_value>.
        IF sy-subrc = 0.
          GET REFERENCE OF <lv_value> INTO rd_ref_to_field_value.
        ENDIF.
      ENDIF.
    ELSE.
      rd_ref_to_field_value = super->get_field_ref_of_data_set( iv_dataset_name_or_alias = iv_dataset_name_or_alias
                                                                iv_fieldname             = iv_fieldname ).
    ENDIF.
  endmethod.


  METHOD get_grouped_value_of_data_set.

    DATA: ls_data_set_struct      TYPE ty_data_set_data,
          ld_copy_of_dataset_data TYPE REF TO data,
          lo_aggr_func            TYPE REF TO zcl_zosql_aggr_func_processor,
          lt_fields_of_aggr_func  TYPE zcl_zosql_aggr_func_processor=>ty_fields_with_aggr_func.

    FIELD-SYMBOLS: <lt_dataset_data>         TYPE STANDARD TABLE,
                   <lt_copy_of_dataset_data> TYPE STANDARD TABLE,
                   <ls_field_with_aggr_func> LIKE LINE OF lt_fields_of_aggr_func,
                   <ls_aggregated_line>      TYPE any,
                   <lv_aggregated_value>     TYPE any.

    ls_data_set_struct = get_data_set_struct( iv_dataset_name_or_alias ).
    IF ls_data_set_struct IS INITIAL.
      MESSAGE e057 WITH iv_fieldname INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    ASSIGN ls_data_set_struct-ref_to_data->* TO <lt_dataset_data>.
    CREATE DATA ld_copy_of_dataset_data LIKE <lt_dataset_data>.
    ASSIGN ld_copy_of_dataset_data->* TO <lt_copy_of_dataset_data>.
    <lt_copy_of_dataset_data> = <lt_dataset_data>.

    CREATE OBJECT lo_aggr_func.

    APPEND INITIAL LINE TO lt_fields_of_aggr_func ASSIGNING <ls_field_with_aggr_func>.
    <ls_field_with_aggr_func>-dataset_name_or_alias = iv_dataset_name_or_alias.
    <ls_field_with_aggr_func>-fieldname             = iv_fieldname.
    <ls_field_with_aggr_func>-aggregation_function  = iv_groupby_function.
    <ls_field_with_aggr_func>-distinct_flag         = iv_distinct.

    IF <ls_field_with_aggr_func>-aggregation_function = zcl_zosql_aggr_func_processor=>c_function_count
      AND <ls_field_with_aggr_func>-distinct_flag <> abap_true.

      CREATE DATA rd_ref_to_aggregated_value TYPE i.
      ASSIGN rd_ref_to_aggregated_value->* TO <lv_aggregated_value>.
      <lv_aggregated_value> = LINES( <lt_copy_of_dataset_data> ).
      RETURN.
    ENDIF.

    lo_aggr_func->apply_aggregation( EXPORTING it_fields_with_aggr_func = lt_fields_of_aggr_func
                                     CHANGING  ct_data_set              = <lt_copy_of_dataset_data> ).

    READ TABLE <lt_copy_of_dataset_data> INDEX 1 ASSIGNING <ls_aggregated_line>.
    ASSIGN COMPONENT iv_fieldname OF STRUCTURE <ls_aggregated_line> TO <lv_aggregated_value>.
    IF sy-subrc = 0.
      GET REFERENCE OF <lv_aggregated_value> INTO rd_ref_to_aggregated_value.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
