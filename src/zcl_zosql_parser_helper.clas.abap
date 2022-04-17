class ZCL_ZOSQL_PARSER_HELPER definition
  public
  create public .

public section.

  methods GET_LIST_OF_SELECT_FROM_TABLES
    returning
      value(RT_DATASETS) type ZCL_ZOSQL_ITERATOR_POSITION=>TY_DATA_SETS .
  methods IS_SELECT
    returning
      value(RV_IS_SELECT) type ABAP_BOOL .
  methods CONSTRUCTOR
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC .
  methods GET_DELETE_TABLE_NAME
    returning
      value(RV_DELETE_TABLE_NAME) type STRING .
  methods GET_FOR_ALL_ENTRIES_TABNAME
    returning
      value(RV_FOR_ALL_ENTRIES_TABNAME) type STRING .
  methods GET_UPDATE_TABLE_NAME
    returning
      value(RV_UPDATE_TABLE_NAME) type STRING
    raising
      ZCX_ZOSQL_ERROR .
  methods GET_UP_TO_N_ROWS_VALUE
    returning
      value(RV_UP_TO_N_ROWS_VALUE_STR) type STRING .
  methods GET_KEY_NODES_OF_SQL_DELETE
    exporting
      !EO_NODE_DELETE_TABLE type ref to ZCL_ZOSQL_PARSER_NODE
      !EO_NODE_WHERE type ref to ZCL_ZOSQL_PARSER_NODE
      value(EV_NEW_SYNTAX) type ABAP_BOOL .
  methods GET_KEY_NODES_OF_SQL_SELECT
    exporting
      !EO_NODE_DISTINCT type ref to ZCL_ZOSQL_PARSER_NODE
      !EO_NODE_SINGLE type ref to ZCL_ZOSQL_PARSER_NODE
      !ET_NODES_OF_SELECT_FIELD_LIST type ZCL_ZOSQL_PARSER_NODE=>TY_PARSER_NODES
      !EO_NODE_UP_TO_N_ROWS type ref to ZCL_ZOSQL_PARSER_NODE
      !EO_NODE_FROM type ref to ZCL_ZOSQL_PARSER_NODE
      !EO_NODE_FOR_ALL_ENTRIES type ref to ZCL_ZOSQL_PARSER_NODE
      !EO_NODE_WHERE type ref to ZCL_ZOSQL_PARSER_NODE
      !EO_NODE_GROUP_BY type ref to ZCL_ZOSQL_PARSER_NODE
      !EO_NODE_HAVING type ref to ZCL_ZOSQL_PARSER_NODE
      !EO_NODE_ORDER_BY type ref to ZCL_ZOSQL_PARSER_NODE
      value(EV_NEW_SYNTAX) type ABAP_BOOL .
  methods GET_KEY_NODES_OF_SQL_UPDATE
    exporting
      !EO_NODE_UPDATE_TABLE type ref to ZCL_ZOSQL_PARSER_NODE
      !EO_NODE_SET type ref to ZCL_ZOSQL_PARSER_NODE
      !EO_NODE_WHERE type ref to ZCL_ZOSQL_PARSER_NODE
      value(EV_NEW_SYNTAX) type ABAP_BOOL .
protected section.
private section.

  data MO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC .

  methods _NEW_SYNTAX_HOST_CHAR_EXISTS
    importing
      !IO_TOP_NODE_TO_START_SEARCH type ref to ZCL_ZOSQL_PARSER_NODE
    returning
      value(RV_EXISTS) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_ZOSQL_PARSER_HELPER IMPLEMENTATION.


  method CONSTRUCTOR.
    mo_sql_parser = io_sql_parser.
  endmethod.


  method GET_DELETE_TABLE_NAME.

    DATA: lo_node_delete_table TYPE REF TO zcl_zosql_parser_node.

    get_key_nodes_of_sql_delete( IMPORTING eo_node_delete_table = lo_node_delete_table ).

    rv_delete_table_name = lo_node_delete_table->token.
  endmethod.


  method GET_FOR_ALL_ENTRIES_TABNAME.

    DATA: lo_node_for_all_entries TYPE REF TO zcl_zosql_parser_node.

    get_key_nodes_of_sql_select( IMPORTING eo_node_for_all_entries = lo_node_for_all_entries ).

    IF lo_node_for_all_entries IS BOUND.
      rv_for_all_entries_tabname = lo_node_for_all_entries->get_token_of_nth_child_node( 4 ).
    ENDIF.
  endmethod.


  METHOD get_key_nodes_of_sql_delete.

    DATA: lo_delete_node           TYPE REF TO zcl_zosql_parser_node,
          lt_child_nodes_of_delete TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_child_node_of_delete  TYPE REF TO zcl_zosql_parser_node.

    lo_delete_node           = mo_sql_parser->get_top_node_as_object( ).
    lt_child_nodes_of_delete = lo_delete_node->get_child_nodes( ).

    LOOP AT lt_child_nodes_of_delete INTO lo_child_node_of_delete.

      CASE lo_child_node_of_delete->node_type.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-delete_tabname.
          eo_node_delete_table = lo_child_node_of_delete.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-where.
          eo_node_where = lo_child_node_of_delete.
      ENDCASE.
    ENDLOOP.

    IF _new_syntax_host_char_exists( eo_node_where ) = abap_true.
      ev_new_syntax = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_key_nodes_of_sql_select.

    DATA: lo_select_node           TYPE REF TO zcl_zosql_parser_node,
          lt_child_nodes_of_select TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_child_node            TYPE REF TO zcl_zosql_parser_node.

    REFRESH et_nodes_of_select_field_list.

    lo_select_node           = mo_sql_parser->get_top_node_as_object( ).
    lt_child_nodes_of_select = lo_select_node->get_child_nodes( ).

    LOOP AT lt_child_nodes_of_select INTO lo_child_node.
      CASE lo_child_node->node_type. " token_ucase.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-distinct.
          eo_node_distinct = lo_child_node.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-select_single.
          eo_node_single = lo_child_node.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-select_field
          OR zcl_zosql_parser_recurs_desc=>node_type-function.

          IF lo_child_node->token = ','.
            ev_new_syntax = abap_true.
          ELSE.
            APPEND lo_child_node TO et_nodes_of_select_field_list.
          ENDIF.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-from.
          eo_node_from = lo_child_node.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-up_to_n_rows.
          eo_node_up_to_n_rows = lo_child_node.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-for_all_entries.
          eo_node_for_all_entries = lo_child_node.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-where.
          eo_node_where = lo_child_node.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-group_by.
          eo_node_group_by = lo_child_node.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-having.
          eo_node_having = lo_child_node.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-order_by.
          eo_node_order_by = lo_child_node.
      ENDCASE.
    ENDLOOP.

    IF ev_new_syntax <> abap_true.
      IF _new_syntax_host_char_exists( eo_node_where ) = abap_true.
        ev_new_syntax = abap_true.
      ENDIF.
    ENDIF.

    IF ev_new_syntax <> abap_true AND eo_node_where IS BOUND.
      IF eo_node_where->exists_child_node_oftype_recur(
          zcl_zosql_parser_recurs_desc=>node_type-subquery_opening_bracket ) = abap_true.

        ev_new_syntax = abap_true.
      ENDIF.
    ENDIF.

    IF ev_new_syntax <> abap_true AND eo_node_having IS BOUND.
      IF eo_node_having->exists_child_node_oftype_recur(
          zcl_zosql_parser_recurs_desc=>node_type-subquery_opening_bracket ) = abap_true.

        ev_new_syntax = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_key_nodes_of_sql_update.

    DATA: lo_update_node           TYPE REF TO zcl_zosql_parser_node,
          lt_child_nodes_of_update TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_child_node_of_update  TYPE REF TO zcl_zosql_parser_node.

    lo_update_node           = mo_sql_parser->get_top_node_as_object( ).
    lt_child_nodes_of_update = lo_update_node->get_child_nodes( ).

    LOOP AT lt_child_nodes_of_update INTO lo_child_node_of_update.

      CASE lo_child_node_of_update->node_type.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-update_set_fields.
          eo_node_set = lo_child_node_of_update.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-where.
          eo_node_where = lo_child_node_of_update.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-update_tabname.
          eo_node_update_table = lo_child_node_of_update.
      ENDCASE.
    ENDLOOP.

    IF _new_syntax_host_char_exists( eo_node_set ) = abap_true
      OR _new_syntax_host_char_exists( eo_node_where ) = abap_true.

      ev_new_syntax = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_list_of_select_from_tables.
    DATA: lo_top_node          TYPE REF TO zcl_zosql_parser_node,
          lt_child_nodes_table TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_node_table        TYPE REF TO zcl_zosql_parser_node,
          lo_node_table_alias  TYPE REF TO zcl_zosql_parser_node.

    FIELD-SYMBOLS: <ls_dataset> LIKE LINE OF rt_datasets.

    lo_top_node = mo_sql_parser->get_top_node_as_object( ).

    lt_child_nodes_table = lo_top_node->get_child_nodes_recursive( ).

    LOOP AT lt_child_nodes_table INTO lo_node_table
      WHERE table_line->node_type = zcl_zosql_parser_recurs_desc=>node_type-table.

      APPEND INITIAL LINE TO rt_datasets ASSIGNING <ls_dataset>.
      <ls_dataset>-dataset_name = lo_node_table->token_ucase.

      lo_node_table_alias =
        lo_node_table->get_child_node_with_type(
          zcl_zosql_parser_recurs_desc=>node_type-alias ).

      IF lo_node_table_alias IS BOUND.
        <ls_dataset>-dataset_alias = lo_node_table_alias->token.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  method GET_UPDATE_TABLE_NAME.

    DATA: lo_node_update_table TYPE REF TO zcl_zosql_parser_node.

    get_key_nodes_of_sql_update( IMPORTING eo_node_update_table = lo_node_update_table ).

    IF lo_node_update_table IS BOUND.
      rv_update_table_name = lo_node_update_table->token.
    ELSE.
      MESSAGE e087 INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.
  endmethod.


  method GET_UP_TO_N_ROWS_VALUE.

    DATA: lo_node_up_to_n_rows TYPE REF TO zcl_zosql_parser_node.

    get_key_nodes_of_sql_select( IMPORTING eo_node_up_to_n_rows = lo_node_up_to_n_rows ).

    IF lo_node_up_to_n_rows IS NOT INITIAL.
      rv_up_to_n_rows_value_str =
          lo_node_up_to_n_rows->get_token_of_nth_child_node( 2 ).
    ENDIF.
  endmethod.


  METHOD is_select.
    IF mo_sql_parser->get_top_node( )-node_type = zcl_zosql_parser_recurs_desc=>node_type-select.
      rv_is_select = abap_true.
    ENDIF.
  ENDMETHOD.


  method _NEW_SYNTAX_HOST_CHAR_EXISTS.

    DATA: lt_child_nodes TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_node        TYPE REF TO zcl_zosql_parser_node.

    IF io_top_node_to_start_search IS NOT BOUND.
      RETURN.
    ENDIF.

    lt_child_nodes = io_top_node_to_start_search->get_child_nodes_recursive( ).

    LOOP AT lt_child_nodes INTO lo_node.
      CHECK lo_node->token IS NOT INITIAL.

      IF lo_node->token(1) = '@'.
        rv_exists = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  endmethod.
ENDCLASS.
