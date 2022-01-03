class ZCL_ZOSQL_PARSER_HELPER definition
  public
  create public .

public section.

  methods GET_DELETE_TABLE_NAME
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    returning
      value(RV_DELETE_TABLE_NAME) type STRING .
  methods GET_FOR_ALL_ENTRIES_TABNAME
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    returning
      value(RV_FOR_ALL_ENTRIES_TABNAME) type STRING .
  methods GET_UPDATE_TABLE_NAME
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    returning
      value(RV_UPDATE_TABLE_NAME) type STRING .
  methods GET_UP_TO_N_ROWS_VALUE
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    returning
      value(RV_UP_TO_N_ROWS_VALUE_STR) type STRING .
  methods GET_KEY_NODES_OF_SQL_DELETE
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    exporting
      !ES_NODE_DELETE_TABLE type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_NODE
      !ES_NODE_WHERE type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_NODE
      value(EV_NEW_SYNTAX) type ABAP_BOOL .
  methods GET_KEY_NODES_OF_SQL_SELECT
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    exporting
      !ES_NODE_DISTINCT type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_NODE
      !ES_NODE_SINGLE type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_NODE
      !ET_NODES_OF_SELECT_FIELD_LIST type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_TREE
      !ES_NODE_UP_TO_N_ROWS type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_NODE
      !ES_NODE_FROM type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_NODE
      !ES_NODE_FOR_ALL_ENTRIES type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_NODE
      !ES_NODE_WHERE type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_NODE
      !ES_NODE_GROUP_BY type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_NODE
      !ES_NODE_ORDER_BY type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_NODE
      value(EV_NEW_SYNTAX) type ABAP_BOOL .
  methods GET_KEY_NODES_OF_SQL_UPDATE
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    exporting
      !ES_NODE_UPDATE_TABLE type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_NODE
      !ES_NODE_SET type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_NODE
      !ES_NODE_WHERE type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_NODE
      value(EV_NEW_SYNTAX) type ABAP_BOOL .
protected section.
private section.

  methods _NEW_SYNTAX_HOST_CHAR_EXISTS
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IV_TOP_NODE_ID_TO_START_SEARCH type I
    returning
      value(RV_EXISTS) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_ZOSQL_PARSER_HELPER IMPLEMENTATION.


  method GET_DELETE_TABLE_NAME.

    DATA: ls_node_delete_table TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    get_key_nodes_of_sql_delete( EXPORTING io_sql_parser        = io_sql_parser
                                 IMPORTING es_node_delete_table = ls_node_delete_table ).

    rv_delete_table_name = ls_node_delete_table-token.
  endmethod.


  method GET_FOR_ALL_ENTRIES_TABNAME.

    DATA: ls_node_for_all_entries TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    get_key_nodes_of_sql_select( EXPORTING io_sql_parser           = io_sql_parser
                                 IMPORTING es_node_for_all_entries = ls_node_for_all_entries ).

    IF ls_node_for_all_entries IS NOT INITIAL.
      rv_for_all_entries_tabname =
          io_sql_parser->get_token_of_nth_child_node( iv_main_node_id = ls_node_for_all_entries-id
                                                      iv_n            = 4 ).
    ENDIF.
  endmethod.


  METHOD get_key_nodes_of_sql_delete.

    DATA: ls_delete_node           TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          lt_child_nodes_of_delete TYPE zcl_zosql_parser_recurs_desc=>ty_tree.

    FIELD-SYMBOLS: <ls_child_node_of_delete> TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    CLEAR: es_node_delete_table, es_node_where.

    ls_delete_node           = io_sql_parser->get_top_node( ).
    lt_child_nodes_of_delete = io_sql_parser->get_child_nodes( ls_delete_node-id ).

    LOOP AT lt_child_nodes_of_delete ASSIGNING <ls_child_node_of_delete>.

      CASE <ls_child_node_of_delete>-node_type.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-delete_tabname.
          es_node_delete_table = <ls_child_node_of_delete>.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-where.
          es_node_where = <ls_child_node_of_delete>.
      ENDCASE.
    ENDLOOP.

    IF _new_syntax_host_char_exists( io_sql_parser                  = io_sql_parser
                                     iv_top_node_id_to_start_search = es_node_where-id ) = abap_true.

      ev_new_syntax = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_key_nodes_of_sql_select.

    DATA: ls_select_node           TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          lt_child_nodes_of_select TYPE zcl_zosql_parser_recurs_desc=>ty_tree.

    FIELD-SYMBOLS: <ls_child_node_of_select> TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    CLEAR: es_node_distinct, es_node_single,
           et_nodes_of_select_field_list,
           es_node_up_to_n_rows, es_node_from, es_node_for_all_entries,
           es_node_where, es_node_group_by, es_node_order_by.

    ls_select_node           = io_sql_parser->get_top_node( ).
    lt_child_nodes_of_select = io_sql_parser->get_child_nodes( ls_select_node-id ).

    LOOP AT lt_child_nodes_of_select ASSIGNING <ls_child_node_of_select>.
      CASE <ls_child_node_of_select>-node_type. " token_ucase.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-distinct.
          es_node_distinct = <ls_child_node_of_select>.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-select_single.
          es_node_single = <ls_child_node_of_select>.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-select_field
          OR zcl_zosql_parser_recurs_desc=>node_type-function.

          IF <ls_child_node_of_select>-token = ','.
            ev_new_syntax = abap_true.
          ELSE.
            APPEND <ls_child_node_of_select> TO et_nodes_of_select_field_list.
          ENDIF.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-from.
          es_node_from = <ls_child_node_of_select>.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-up_to_n_rows.
          es_node_up_to_n_rows = <ls_child_node_of_select>.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-for_all_entries.
          es_node_for_all_entries = <ls_child_node_of_select>.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-where.
          es_node_where = <ls_child_node_of_select>.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-group_by.
          es_node_group_by = <ls_child_node_of_select>.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-order_by.
          es_node_order_by = <ls_child_node_of_select>.
      ENDCASE.
    ENDLOOP.

    IF ev_new_syntax <> abap_true.
      IF _new_syntax_host_char_exists( io_sql_parser                  = io_sql_parser
                                       iv_top_node_id_to_start_search = es_node_where-id ) = abap_true.
        ev_new_syntax = abap_true.
      ENDIF.
    ENDIF.

    IF ev_new_syntax <> abap_true.
      DATA: ls_child_node_subquery TYPE zcl_zosql_parser_recurs_desc=>ty_node.

      ls_child_node_subquery =
        io_sql_parser->get_child_node_with_type_recur( iv_node_id   = es_node_where-id
                                                       iv_node_type = zcl_zosql_parser_recurs_desc=>node_type-subquery_opening_bracket ).

      IF ls_child_node_subquery IS NOT INITIAL.
        ev_new_syntax = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method GET_KEY_NODES_OF_SQL_UPDATE.

    DATA: ls_update_node           TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          lt_child_nodes_of_update TYPE zcl_zosql_parser_recurs_desc=>ty_tree.

    FIELD-SYMBOLS: <ls_child_node_of_update> TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    CLEAR: es_node_update_table, es_node_set, es_node_where.

    ls_update_node           = io_sql_parser->get_top_node( ).
    lt_child_nodes_of_update = io_sql_parser->get_child_nodes( ls_update_node-id ).

    LOOP AT lt_child_nodes_of_update ASSIGNING <ls_child_node_of_update>.

      CASE <ls_child_node_of_update>-node_type.
         WHEN zcl_zosql_parser_recurs_desc=>node_type-update_set_fields.
          es_node_set = <ls_child_node_of_update>.
       WHEN zcl_zosql_parser_recurs_desc=>node_type-where.
          es_node_where = <ls_child_node_of_update>.
       WHEN zcl_zosql_parser_recurs_desc=>node_type-update_tabname.
         es_node_update_table = <ls_child_node_of_update>.
      ENDCASE.
    ENDLOOP.

    IF _new_syntax_host_char_exists( io_sql_parser                  = io_sql_parser
                                     iv_top_node_id_to_start_search = es_node_set-id ) = abap_true
      OR _new_syntax_host_char_exists( io_sql_parser                  = io_sql_parser
                                       iv_top_node_id_to_start_search = es_node_where-id ) = abap_true.

      ev_new_syntax = abap_true.
    ENDIF.
  endmethod.


  method GET_UPDATE_TABLE_NAME.

    DATA: ls_node_update_table TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    get_key_nodes_of_sql_update( EXPORTING io_sql_parser        = io_sql_parser
                                 IMPORTING es_node_update_table = ls_node_update_table ).

    rv_update_table_name = ls_node_update_table-token.
  endmethod.


  method GET_UP_TO_N_ROWS_VALUE.

    DATA: ls_node_up_to_n_rows TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    get_key_nodes_of_sql_select( EXPORTING io_sql_parser        = io_sql_parser
                                 IMPORTING es_node_up_to_n_rows = ls_node_up_to_n_rows ).

    IF ls_node_up_to_n_rows IS NOT INITIAL.
      rv_up_to_n_rows_value_str =
          io_sql_parser->get_token_of_nth_child_node( iv_main_node_id = ls_node_up_to_n_rows-id
                                                      iv_n            = 2 ).
    ENDIF.
  endmethod.


  method _NEW_SYNTAX_HOST_CHAR_EXISTS.

    DATA: lt_child_nodes TYPE zcl_zosql_parser_recurs_desc=>ty_tree.

    FIELD-SYMBOLS: <ls_node> TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    lt_child_nodes = io_sql_parser->get_child_nodes_recursive( iv_top_node_id_to_start_search ).

    LOOP AT lt_child_nodes ASSIGNING <ls_node>.
      IF <ls_node>-token(1) = '@'.
        rv_exists = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  endmethod.
ENDCLASS.
