class ZCL_ZOSQL_GROUPBY_PROCESSOR definition
  public
  inheriting from ZCL_ZOSQL_AGGR_FUNC_PROCESSOR
  create public .

public section.

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

    LOOP AT lt_child_nodes_of_group_by ASSIGNING <ls_child_node>
      WHERE node_type = 'SELECT_FIELD'.

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

      fill_aggregation_functions( EXPORTING it_table_lines_by_key    = <lt_table_one_group>
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
