class ZCL_ZOSQL_AGGR_FUNC_PROCESSOR definition
  public
  inheriting from ZCL_ZOSQL_PROCESSOR_BASE
  create public .

public section.

  types:
    BEGIN OF ty_group_by_key_field,
           dataset_name_or_alias TYPE string,
           fieldname TYPE fieldname,
         END OF ty_group_by_key_field .
  types:
    ty_group_by_key_fields TYPE STANDARD TABLE OF ty_group_by_key_field WITH DEFAULT KEY .
  types:
    BEGIN OF TY_FIELD_WITH_AGGR_FUNC,
           dataset_name_or_alias TYPE string,
           fieldname TYPE fieldname,
           aggregation_function TYPE string,
           distinct_flag        TYPE abap_bool,
         END OF TY_FIELD_WITH_AGGR_FUNC .
  types:
    TY_FIELDS_WITH_AGGR_FUNC  TYPE STANDARD TABLE OF ty_field_with_aggr_func WITH KEY fieldname .

  constants C_FUNCTION_COUNT type STRING value 'COUNT' ##NO_TEXT.

  methods APPLY_AGGREGATION
    importing
      !IO_SELECT type ref to ZCL_ZOSQL_SELECT_PROCESSOR
    changing
      !CT_DATA_SET type STANDARD TABLE
    raising
      ZCX_ZOSQL_ERROR .
protected section.

  methods ADD_RECORD_TO_GROUPED_TABLE
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_GROUPBY_ITER_POS
      !IO_SELECT type ref to ZCL_ZOSQL_SELECT_PROCESSOR
    changing
      !CT_GROUPED_TABLE type STANDARD TABLE
    raising
      ZCX_ZOSQL_ERROR .
private section.

  constants C_FUNCTION_AVG type STRING value 'AVG' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_ZOSQL_AGGR_FUNC_PROCESSOR IMPLEMENTATION.


  method ADD_RECORD_TO_GROUPED_TABLE.

    DATA: ld_value             TYPE REF TO data,
          lt_select_parameters TYPE zcl_zosql_select_processor=>ty_select_parameters.

    FIELD-SYMBOLS: <ls_select_parameter> TYPE zcl_zosql_select_processor=>ty_select_parameter,
                   <ls_new_line>         TYPE any,
                   <lv_field_value>      TYPE any,
                   <lv_value>            TYPE any.

    APPEND INITIAL LINE TO ct_grouped_table ASSIGNING <ls_new_line>.

    lt_select_parameters = io_select->get_select_parameters( ).
    LOOP AT lt_select_parameters ASSIGNING <ls_select_parameter>.

      ASSIGN COMPONENT <ls_select_parameter>-fieldname OF STRUCTURE <ls_new_line> TO <lv_field_value>.
      IF sy-subrc <> 0.
        ASSIGN COMPONENT <ls_select_parameter>-field_name_in_result OF STRUCTURE <ls_new_line>
          TO <lv_field_value>.
        IF sy-subrc <> 0.
          MESSAGE e057 WITH <ls_select_parameter>-fieldname INTO zcl_zosql_utils=>dummy.
          zcl_zosql_utils=>raise_exception_from_sy_msg( ).
        ENDIF.
      ENDIF.

      IF <ls_select_parameter>-function_name IS NOT INITIAL.
        ld_value = io_iteration_position->get_grouped_value_of_data_set(
          iv_dataset_name_or_alias = <ls_select_parameter>-dataset_name_or_alias
          iv_fieldname             = <ls_select_parameter>-fieldname
          iv_groupby_function      = <ls_select_parameter>-function_name
          iv_distinct              = <ls_select_parameter>-distinct_flag ).

      ELSE.
        ld_value = io_iteration_position->get_field_ref_of_data_set(
          iv_dataset_name_or_alias = <ls_select_parameter>-dataset_name_or_alias
          iv_fieldname             = <ls_select_parameter>-fieldname ).
      ENDIF.

      IF ld_value IS BOUND.
        ASSIGN ld_value->* TO <lv_value>.
        <lv_field_value> = <lv_value>.
      ENDIF.
    ENDLOOP.
  endmethod.


  METHOD apply_aggregation.

    DATA: lo_groupby_iterator TYPE REF TO zcl_zosql_groupby_iterator,
          lo_iter_pos         TYPE REF TO zcl_zosql_groupby_iter_pos,
          ld_aggregated_table TYPE REF TO data.

    FIELD-SYMBOLS: <lt_aggregated_table> TYPE STANDARD TABLE.

    CREATE DATA ld_aggregated_table LIKE ct_data_set.
    ASSIGN ld_aggregated_table->* TO <lt_aggregated_table>.

    CREATE OBJECT lo_groupby_iterator.
    lo_groupby_iterator->init_iterator_when_no_group_by( io_select ).

    IF lo_groupby_iterator->zif_zosql_iterator~move_to_first( ) = abap_true.
      lo_iter_pos ?= lo_groupby_iterator->zif_zosql_iterator~get_iterator_position_object( ).

      add_record_to_grouped_table( EXPORTING io_iteration_position = lo_iter_pos
                                             io_select             = io_select
                                   CHANGING  ct_grouped_table      = <lt_aggregated_table> ).
    ENDIF.

    ct_data_set = <lt_aggregated_table>.
  ENDMETHOD.
ENDCLASS.
