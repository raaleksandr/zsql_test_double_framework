class ZCL_ZOSQL_AGGR_FUNC_PROCESSOR definition
  public
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
      !IT_FIELDS_WITH_AGGR_FUNC type TY_FIELDS_WITH_AGGR_FUNC
    changing
      !CT_DATA_SET type STANDARD TABLE
    raising
      ZCX_ZOSQL_ERROR .
protected section.

  methods FILL_AGGREGATION_FUNCTIONS
    importing
      !IT_TABLE_LINES_BY_KEY type STANDARD TABLE
      !IT_FIELDS_WITH_AGGR_FUNC type TY_FIELDS_WITH_AGGR_FUNC
    changing
      !CS_RESULT_LINE type ANY
    raising
      ZCX_ZOSQL_ERROR .
private section.

  constants C_FUNCTION_AVG type STRING value 'AVG' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_ZOSQL_AGGR_FUNC_PROCESSOR IMPLEMENTATION.


  method APPLY_AGGREGATION.

    DATA: ld_line_for_aggregation    TYPE REF TO data.

    FIELD-SYMBOLS: <ls_line_for_aggregation> TYPE any.

    CREATE DATA ld_line_for_aggregation LIKE LINE OF ct_data_set.
    ASSIGN ld_line_for_aggregation->* TO <ls_line_for_aggregation>.

    fill_aggregation_functions( EXPORTING it_table_lines_by_key    = ct_data_set
                                          it_fields_with_aggr_func = it_fields_with_aggr_func
                                CHANGING  cs_result_line           = <ls_line_for_aggregation> ).

    REFRESH ct_data_set.
    APPEND <ls_line_for_aggregation> TO ct_data_set.
  endmethod.


  METHOD FILL_AGGREGATION_FUNCTIONS.

    TYPES: BEGIN OF ty_count_distinct_buffer,
             fieldname    TYPE fieldname,
             values_table TYPE string_table,
           END OF ty_count_distinct_buffer.

    DATA: lt_count_distinct_buffer TYPE TABLE OF ty_count_distinct_buffer.

    FIELD-SYMBOLS: <ls_field_with_aggr_func>   LIKE LINE OF it_fields_with_aggr_func,
                   <lv_grouped_value>          TYPE any,
                   <ls_table_line_not_grouped> TYPE any,
                   <lv_not_grouped_value>      TYPE any,
                   <ls_count_distinct_buffer>  TYPE ty_count_distinct_buffer.

    LOOP AT it_fields_with_aggr_func ASSIGNING <ls_field_with_aggr_func>.

      ASSIGN COMPONENT <ls_field_with_aggr_func>-fieldname OF STRUCTURE cs_result_line TO <lv_grouped_value>.
      IF sy-subrc <> 0.
        MESSAGE e076 WITH <ls_field_with_aggr_func>-fieldname INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.
      CLEAR <lv_grouped_value>.

      LOOP AT it_table_lines_by_key ASSIGNING <ls_table_line_not_grouped>.
        ASSIGN COMPONENT <ls_field_with_aggr_func>-fieldname OF STRUCTURE <ls_table_line_not_grouped> TO <lv_not_grouped_value>.

        CASE zcl_zosql_utils=>to_upper_case( <ls_field_with_aggr_func>-aggregation_function ).
          WHEN 'SUM'.
            <lv_grouped_value> = <lv_grouped_value> + <lv_not_grouped_value>.
          WHEN 'MIN'.
            IF <lv_not_grouped_value> < <lv_grouped_value> OR <lv_grouped_value> IS INITIAL.
              <lv_grouped_value> = <lv_not_grouped_value>.
            ENDIF.
          WHEN 'MAX'.
            IF <lv_not_grouped_value> > <lv_grouped_value> OR <lv_grouped_value> IS INITIAL.
              <lv_grouped_value> = <lv_not_grouped_value>.
            ENDIF.
          WHEN c_function_count.
            IF <ls_field_with_aggr_func>-distinct_flag = abap_true.
              READ TABLE lt_count_distinct_buffer WITH KEY fieldname = <ls_field_with_aggr_func>-fieldname
                ASSIGNING <ls_count_distinct_buffer>.
              IF sy-subrc <> 0.
                APPEND INITIAL LINE TO lt_count_distinct_buffer ASSIGNING <ls_count_distinct_buffer>.
                <ls_count_distinct_buffer>-fieldname = <ls_field_with_aggr_func>-fieldname.
              ENDIF.

              READ TABLE <ls_count_distinct_buffer>-values_table WITH KEY table_line = <lv_not_grouped_value>
                TRANSPORTING NO FIELDS.
              IF sy-subrc <> 0.
                APPEND <lv_not_grouped_value> TO <ls_count_distinct_buffer>-values_table.
                <lv_grouped_value> = <lv_grouped_value> + 1.
              ENDIF.
            ELSE.
              <lv_grouped_value> = <lv_grouped_value> + 1.
            ENDIF.
          WHEN c_function_avg.
            <lv_grouped_value> = <lv_grouped_value> + <lv_not_grouped_value>.
          WHEN OTHERS.
            MESSAGE e060 WITH <ls_field_with_aggr_func>-aggregation_function INTO zcl_zosql_utils=>dummy.
            zcl_zosql_utils=>raise_exception_from_sy_msg( ).
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    LOOP AT it_fields_with_aggr_func ASSIGNING <ls_field_with_aggr_func>
      WHERE aggregation_function  = c_function_avg.

      ASSIGN COMPONENT <ls_field_with_aggr_func>-fieldname OF STRUCTURE cs_result_line TO <lv_grouped_value>.
      <lv_grouped_value> = <lv_grouped_value> / LINES( it_table_lines_by_key ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
