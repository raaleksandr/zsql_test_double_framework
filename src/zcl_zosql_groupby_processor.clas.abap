class ZCL_ZOSQL_GROUPBY_PROCESSOR definition
  public
  create public .

public section.

  data MT_GROUP_BY_FIELDS type FIELDNAME_TABLE read-only .

  methods INITIALIZE_BY_PARSED_SQL
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IV_GROUP_BY_NODE_ID type I .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZOSQL_GROUPBY_PROCESSOR IMPLEMENTATION.


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
ENDCLASS.
