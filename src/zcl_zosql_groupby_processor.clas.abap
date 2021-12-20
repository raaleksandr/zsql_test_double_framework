class ZCL_ZOSQL_GROUPBY_PROCESSOR definition
  public
  create public .

public section.

  types:
    BEGIN OF TY_FIELD_WITH_AGGR_FUNC,
           fieldname TYPE fieldname,
           aggregation_function TYPE string,
           distinct_flag        TYPE abap_bool,
         END OF TY_FIELD_WITH_AGGR_FUNC .
  types:
    TY_FIELDS_WITH_AGGR_FUNC  TYPE STANDARD TABLE OF ty_field_with_aggr_func WITH KEY fieldname .

  data MT_GROUP_BY_FIELDS type FIELDNAME_TABLE read-only .

  methods APPLY_GROUP_BY
    importing
      !IT_FIELDS_WITH_AGGR_FUNC type TY_FIELDS_WITH_AGGR_FUNC
    changing
      !CT_DATA_SET type STANDARD TABLE
    raising
      ZCX_ZOSQL_ERROR .
  methods INITIALIZE_BY_PARSED_SQL
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IV_GROUP_BY_NODE_ID type I .
protected section.
private section.

  constants C_FUNCTION_COUNT type STRING value 'COUNT' ##NO_TEXT.
  constants C_FUNCTION_AVG type STRING value 'AVG' ##NO_TEXT.

  methods _GET_DATA_SET_LINE_FOR_GROUP
    importing
      !IS_STRUCT_WITH_GROUP_BY_KEYS type ANY
      !IT_DATA_SET type STANDARD TABLE
    exporting
      !ET_DATA_SUBSET_FOR_GROUP type STANDARD TABLE .
  methods _ADD_KEYS_WITHOUT_AGGR_FUNCS
    importing
      !ID_REF_TO_HASH_GROUP_BY_TABLE type ref to DATA
      !IT_DATA_SET type STANDARD TABLE .
  methods _FILL_FIELDS_WITH_AGGR_FUNCS
    importing
      !IT_FIELDS_WITH_AGGR_FUNC type TY_FIELDS_WITH_AGGR_FUNC
      !IT_DATA_SET type STANDARD TABLE
    changing
      !CT_GROUP_BY_TABLE type STANDARD TABLE
    raising
      ZCX_ZOSQL_ERROR .
  methods _FILL_AGGREGATION_FUNCTIONS
    importing
      !IT_TABLE_LINES_BY_KEY type STANDARD TABLE
      !IT_FIELDS_WITH_AGGR_FUNC type TY_FIELDS_WITH_AGGR_FUNC
    changing
      !CS_RESULT_LINE type ANY
    raising
      ZCX_ZOSQL_ERROR .
ENDCLASS.



CLASS ZCL_ZOSQL_GROUPBY_PROCESSOR IMPLEMENTATION.


  method APPLY_GROUP_BY.

    DATA: ld_group_by_hash_table     TYPE REF TO data,
          ld_group_by_standard_table TYPE REF TO data.

    FIELD-SYMBOLS: <lt_grouped_table_standard> TYPE STANDARD TABLE,
                   <lt_grouped_table_hashed>   TYPE HASHED TABLE.

    ld_group_by_hash_table =
      zcl_zosql_utils=>create_hash_tab_like_standard(
        it_standard_table_template = ct_data_set
        it_key_fields              = mt_group_by_fields ).

    _add_keys_without_aggr_funcs( id_ref_to_hash_group_by_table = ld_group_by_hash_table
                                  it_data_set                   = ct_data_set ).

    CREATE DATA ld_group_by_standard_table LIKE ct_data_set.
    ASSIGN ld_group_by_standard_table->* TO <lt_grouped_table_standard>.

    ASSIGN ld_group_by_hash_table->* TO <lt_grouped_table_hashed>.
    <lt_grouped_table_standard> = <lt_grouped_table_hashed>.

    _fill_fields_with_aggr_funcs( EXPORTING it_fields_with_aggr_func = it_fields_with_aggr_func
                                            it_data_set              = ct_data_set
                                  CHANGING  ct_group_by_table        = <lt_grouped_table_standard> ).

    ct_data_set = <lt_grouped_table_standard>.
  endmethod.


  method INITIALIZE_BY_PARSED_SQL.

    DATA: lt_child_nodes_of_group_by TYPE zcl_zosql_parser_recurs_desc=>ty_tree.

    FIELD-SYMBOLS: <ls_child_node> TYPE zcl_zosql_parser_recurs_desc=>ty_node,
                   <ls_group_by_field> LIKE LINE OF mt_group_by_fields.

    lt_child_nodes_of_group_by = io_sql_parser->get_child_nodes( iv_group_by_node_id ).

    LOOP AT lt_child_nodes_of_group_by ASSIGNING <ls_child_node>.
      IF <ls_child_node>-token_ucase = 'BY'.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO mt_group_by_fields ASSIGNING <ls_group_by_field>.
      FIND FIRST OCCURRENCE OF '~' IN <ls_child_node>-token.
      IF sy-subrc = 0.
        SPLIT <ls_child_node>-token_ucase AT '~' INTO zcl_zosql_utils=>dummy <ls_group_by_field>-fieldname.
      ELSE.
        <ls_group_by_field>-fieldname = <ls_child_node>-token_ucase.
      ENDIF.
    ENDLOOP.
  endmethod.


  method _ADD_KEYS_WITHOUT_AGGR_FUNCS.

    FIELD-SYMBOLS: <lt_group_by_table_hashed> TYPE HASHED TABLE,
                   <ls_table_line>            TYPE any.

    ASSIGN id_ref_to_hash_group_by_table->* TO <lt_group_by_table_hashed>.

    LOOP AT it_data_set ASSIGNING <ls_table_line>.
      READ TABLE <lt_group_by_table_hashed> FROM <ls_table_line> TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        INSERT <ls_table_line> INTO TABLE <lt_group_by_table_hashed>.
      ENDIF.
    ENDLOOP.
  endmethod.


  METHOD _FILL_AGGREGATION_FUNCTIONS.

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


  method _FILL_FIELDS_WITH_AGGR_FUNCS.

    DATA: ld_table_one_group         TYPE REF TO data.

    FIELD-SYMBOLS: <lt_table_one_group> TYPE STANDARD TABLE,
                   <ls_group_by_line>   TYPE any.

    CREATE DATA ld_table_one_group LIKE it_data_set.
    ASSIGN ld_table_one_group->* TO <lt_table_one_group>.

    LOOP AT ct_group_by_table ASSIGNING <ls_group_by_line>.
      _get_data_set_line_for_group( EXPORTING is_struct_with_group_by_keys = <ls_group_by_line>
                                              it_data_set                  = it_data_set
                                    IMPORTING et_data_subset_for_group     = <lt_table_one_group> ).

      _fill_aggregation_functions( EXPORTING it_table_lines_by_key    = <lt_table_one_group>
                                             it_fields_with_aggr_func = it_fields_with_aggr_func
                                   CHANGING  cs_result_line           = <ls_group_by_line> ).
    ENDLOOP.
  endmethod.


  METHOD _get_data_set_line_for_group.

    DATA: lv_same_key  TYPE abap_bool,
          lv_fieldname TYPE fieldname.

    FIELD-SYMBOLS: <ls_data_set_line>  TYPE any,
                   <lv_field_data_set> TYPE any,
                   <lv_field_key>      TYPE any.

    REFRESH et_data_subset_for_group.
    LOOP AT it_data_set ASSIGNING <ls_data_set_line>.

      lv_same_key = abap_true.
      LOOP AT mt_group_by_fields INTO lv_fieldname.
        ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_data_set_line>  TO <lv_field_data_set>.
        ASSIGN COMPONENT lv_fieldname OF STRUCTURE is_struct_with_group_by_keys TO <lv_field_key>.

        IF <lv_field_data_set> <> <lv_field_key>.
          lv_same_key = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.

      CHECK lv_same_key = abap_true.

      APPEND <ls_data_set_line> TO et_data_subset_for_group.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
