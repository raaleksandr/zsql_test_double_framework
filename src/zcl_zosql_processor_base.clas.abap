class ZCL_ZOSQL_PROCESSOR_BASE definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE .
protected section.

  data MV_NEW_SYNTAX type ABAP_BOOL .

  methods DELETE_HOST_VARIABLE_SYMBOL
    importing
      !IV_VARIABLE_IN_SQL type CLIKE
    returning
      value(RV_VARIABLE_IN_SQL) type STRING .
  methods FILL_DATASET_WHERE_EMPTY
    importing
      !IO_FROM_ITERATOR type ref to ZCL_ZOSQL_FROM_ITERATOR
      value(IV_ERROR_IF_NOT_FOUND) type ABAP_BOOL default ABAP_TRUE
    changing
      !CT_TABLE_WHERE_TO_FILL type STANDARD TABLE
    raising
      ZCX_ZOSQL_ERROR .
private section.
ENDCLASS.



CLASS ZCL_ZOSQL_PROCESSOR_BASE IMPLEMENTATION.


  method CONSTRUCTOR.
    mv_new_syntax = iv_new_syntax.
  endmethod.


  method DELETE_HOST_VARIABLE_SYMBOL.

    IF iv_variable_in_sql IS INITIAL.
      RETURN.
    ENDIF.

    rv_variable_in_sql = iv_variable_in_sql.
    IF rv_variable_in_sql(1) = '@'.
      rv_variable_in_sql = rv_variable_in_sql+1.
    ENDIF.
  endmethod.


  METHOD FILL_DATASET_WHERE_EMPTY.

    DATA: lt_data_set_list          TYPE zcl_zosql_iterator_position=>ty_data_sets,
          ls_data_set               LIKE LINE OF lt_data_set_list,
          lt_components_of_data_set TYPE cl_abap_structdescr=>included_view.

    FIELD-SYMBOLS: <ls_table_line> TYPE any,
                   <lv_fieldname>  TYPE any,
                   <lv_dataset_name_or_alias> TYPE any.

    lt_data_set_list = io_from_iterator->get_data_set_list( ).

    LOOP AT ct_table_where_to_fill ASSIGNING <ls_table_line>
      WHERE ('DATASET_NAME_OR_ALIAS IS INITIAL').

      ASSIGN COMPONENT 'FIELDNAME'             OF STRUCTURE <ls_table_line> TO <lv_fieldname>.
      ASSIGN COMPONENT 'DATASET_NAME_OR_ALIAS' OF STRUCTURE <ls_table_line> TO <lv_dataset_name_or_alias>.

      LOOP AT lt_data_set_list INTO ls_data_set.
        lt_components_of_data_set = io_from_iterator->get_components_of_data_set( ls_data_set-dataset_name ).
        READ TABLE lt_components_of_data_set WITH KEY name = <lv_fieldname> TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <lv_dataset_name_or_alias> = ls_data_set-dataset_name.
          CONTINUE.
        ENDIF.
      ENDLOOP.

      IF iv_error_if_not_found = abap_true AND <lv_dataset_name_or_alias> IS INITIAL.
        MESSAGE e057 WITH <lv_fieldname> INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
