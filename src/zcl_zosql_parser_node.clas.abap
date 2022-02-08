class ZCL_ZOSQL_PARSER_NODE definition
  public
  create public .

public section.

  types:
    TY_PARSER_NODES  TYPE STANDARD TABLE OF REF TO zcl_zosql_parser_node WITH DEFAULT KEY .

  data ID type I read-only .
  data PARENT_ID type I read-only .
  data TOKEN type STRING read-only .
  data TOKEN_UCASE type STRING read-only .
  data TOKEN_INDEX type I read-only .
  data NODE_TYPE type STRING read-only .

  methods GET_NODE_INFO
    returning
      value(RS_NODE_INFO) type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_NODE .
  methods GET_SQL_PARSER
    returning
      value(RO_SQL_PARSER) type ref to ZCL_ZOSQL_PARSER_RECURS_DESC .
  methods CONSTRUCTOR
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IV_NODE_ID type I .
  methods EXISTS_CHILD_NODE_OFTYPE_RECUR
    importing
      !IV_NODE_TYPE type CLIKE
    returning
      value(RV_EXISTS) type ABAP_BOOL .
  methods EXISTS_CHILD_NODE_WITH_TYPE
    importing
      !IV_NODE_TYPE type CLIKE
    returning
      value(RV_EXISTS) type ABAP_BOOL .
  methods GET_CHILD_NODE_TOKEN_WITH_TYPE
    importing
      !IV_NODE_TYPE type CLIKE
      value(IV_UCASE) type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_TOKEN) type STRING .
  methods GET_CHILD_NODE_WITH_TYPE
    importing
      !IV_NODE_TYPE type CLIKE
    returning
      value(RO_CHILD_NODE) type ref to ZCL_ZOSQL_PARSER_NODE .
  methods GET_CHILD_NODE_WITH_TYPE_RECUR
    importing
      !IV_NODE_TYPE type CLIKE
    returning
      value(RO_CHILD_NODE) type ref to ZCL_ZOSQL_PARSER_NODE .
  methods GET_CHILD_NODES
    returning
      value(RT_CHILD_NODES) type TY_PARSER_NODES .
  methods GET_CHILD_NODES_WITH_SELF
    returning
      value(RT_CHILD_NODES) type TY_PARSER_NODES .
  methods GET_CHILD_NODES_RECURSIVE
    returning
      value(RT_CHILD_NODES) type TY_PARSER_NODES .
  methods GET_NODE_END_TOKEN_INDEX
    returning
      value(RV_END_TOKEN_INDEX) type I .
  methods GET_NODE_SQL
    returning
      value(RV_SQL_PART) type STRING .
  methods GET_NODE_SQL_START_AT_OFFSET
    importing
      value(IV_NUMBER_OF_TOKENS_OFFSET) type I
    returning
      value(RV_SQL_PART) type STRING .
  methods GET_NODE_SQL_WITHOUT_SELF
    returning
      value(RV_SQL_PART) type STRING .
  methods GET_NODE_TOKEN_INDEX
    importing
      !IV_NODE_ID type I
    returning
      value(RV_TOKEN_INDEX) type I .
  methods GET_SQL_AS_RANGE_OF_TOKENS
    importing
      value(IV_START_TOKEN_INDEX) type I
      value(IV_END_TOKEN_INDEX) type I
    returning
      value(RV_SQL_PART) type STRING .
  methods GET_TOKEN_OF_NTH_CHILD_NODE
    importing
      value(IV_N) type I
    returning
      value(RV_TOKEN) type STRING .
protected section.
private section.

  data MO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC .

  methods _TY_TREE_TO_OBJECTS_TAB
    importing
      !IT_TREE type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_TREE
    returning
      value(RT_NODES_TAB) type TY_PARSER_NODES .
ENDCLASS.



CLASS ZCL_ZOSQL_PARSER_NODE IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA: ls_node_info   TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    mo_sql_parser = io_sql_parser.

    ls_node_info = mo_sql_parser->get_node_info( iv_node_id ).
    id          = ls_node_info-id.
    parent_id   = ls_node_info-parent_id.
    token       = ls_node_info-token.
    token_ucase = ls_node_info-token_ucase.
    token_index = ls_node_info-token_index.
    node_type   = ls_node_info-node_type.
  endmethod.


  METHOD EXISTS_CHILD_NODE_OFTYPE_RECUR.
    DATA: ls_child_node TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    ls_child_node = mo_sql_parser->get_child_node_with_type_recur( iv_node_id   = id
                                                                   iv_node_type = iv_node_type ).

    IF ls_child_node IS NOT INITIAL.
      rv_exists = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD EXISTS_CHILD_NODE_WITH_TYPE.
    DATA: ls_child_node TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    ls_child_node = mo_sql_parser->get_child_node_with_type( iv_node_id   = id
                                                             iv_node_type = iv_node_type ).

    IF ls_child_node IS NOT INITIAL.
      rv_exists = abap_true.
    ENDIF.
  ENDMETHOD.


  method GET_CHILD_NODES.
    DATA: lt_child_nodes  TYPE zcl_zosql_parser_recurs_desc=>ty_tree.

    lt_child_nodes = mo_sql_parser->get_child_nodes( id ).
    rt_child_nodes = _ty_tree_to_objects_tab( lt_child_nodes ).
  endmethod.


  method GET_CHILD_NODES_RECURSIVE.
    DATA: lt_child_nodes  TYPE zcl_zosql_parser_recurs_desc=>ty_tree.

    lt_child_nodes = mo_sql_parser->get_child_nodes_recursive( id ).
    rt_child_nodes = _ty_tree_to_objects_tab( lt_child_nodes ).
  endmethod.


  METHOD GET_CHILD_NODES_WITH_SELF.
    DATA: lt_child_nodes  TYPE zcl_zosql_parser_recurs_desc=>ty_tree.

    lt_child_nodes = mo_sql_parser->get_child_nodes_with_self( id ).
    rt_child_nodes = _ty_tree_to_objects_tab( lt_child_nodes ).
  ENDMETHOD.


  METHOD GET_CHILD_NODE_TOKEN_WITH_TYPE.
    DATA: lo_child_node TYPE REF TO zcl_zosql_parser_node.

    lo_child_node = get_child_node_with_type( iv_node_type ).

    IF lo_child_node IS NOT BOUND.
      RETURN.
    ENDIF.

    IF iv_ucase = abap_true.
      rv_token = lo_child_node->token_ucase.
    ELSE.
      rv_token = lo_child_node->token.
    ENDIF.
  ENDMETHOD.


  method GET_CHILD_NODE_WITH_TYPE.
    DATA: ls_child_node   TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    ls_child_node = mo_sql_parser->get_child_node_with_type( iv_node_id   = id
                                                             iv_node_type = iv_node_type ).

    IF ls_child_node IS NOT INITIAL.
      CREATE OBJECT ro_child_node
        EXPORTING
          io_sql_parser = mo_sql_parser
          iv_node_id    = ls_child_node-id.
    ENDIF.
  endmethod.


  method GET_CHILD_NODE_WITH_TYPE_RECUR.
    DATA: ls_child_node   TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    ls_child_node = mo_sql_parser->get_child_node_with_type_recur( iv_node_id   = id
                                                                   iv_node_type = iv_node_type ).

    IF ls_child_node IS NOT INITIAL.
      CREATE OBJECT ro_child_node
        EXPORTING
          io_sql_parser = mo_sql_parser
          iv_node_id    = ls_child_node-id.
    ENDIF.
  endmethod.


  METHOD GET_NODE_END_TOKEN_INDEX.
    rv_end_token_index = mo_sql_parser->get_node_end_token_index( id ).
  ENDMETHOD.


  method GET_NODE_INFO.
    rs_node_info = mo_sql_parser->get_node_info( id ).
  endmethod.


  method GET_NODE_SQL.
    rv_sql_part = get_node_sql_start_at_offset( iv_number_of_tokens_offset = 0 ).
  endmethod.


  method GET_NODE_SQL_START_AT_OFFSET.
    rv_sql_part =
      mo_sql_parser->get_node_sql_start_at_offset(
        iv_node_id = id
        iv_number_of_tokens_offset = iv_number_of_tokens_offset ).
  endmethod.


  method GET_NODE_SQL_WITHOUT_SELF.
    rv_sql_part = get_node_sql_start_at_offset( iv_number_of_tokens_offset = 1 ).
  endmethod.


  method GET_NODE_TOKEN_INDEX.
    DATA: ls_node_info TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    ls_node_info = mo_sql_parser->get_node_info( iv_node_id ).
    rv_token_index = ls_node_info-token_index.
  endmethod.


  method GET_SQL_AS_RANGE_OF_TOKENS.
    rv_sql_part = mo_sql_parser->get_sql_as_range_of_tokens( iv_start_token_index = iv_start_token_index
                                                             iv_end_token_index   = iv_end_token_index ).
  endmethod.


  method GET_SQL_PARSER.
    ro_sql_parser = mo_sql_parser.
  endmethod.


  method GET_TOKEN_OF_NTH_CHILD_NODE.
    rv_token = mo_sql_parser->get_token_of_nth_child_node( iv_main_node_id = id
                                                           iv_n            = iv_n ).
  endmethod.


  method _TY_TREE_TO_OBJECTS_TAB.

    DATA: lo_node TYPE REF TO zcl_zosql_parser_node.

    FIELD-SYMBOLS: <ls_node> LIKE LINE OF it_tree.

    LOOP AT it_tree ASSIGNING <ls_node>.
      CREATE OBJECT lo_node
        EXPORTING
          io_sql_parser = mo_sql_parser
          iv_node_id    = <ls_node>-id.

      APPEND lo_node TO rt_nodes_tab.
    ENDLOOP.
  endmethod.
ENDCLASS.
