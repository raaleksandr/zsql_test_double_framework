class ZCL_ZOSQL_PARSER_RECURS_DESC definition
  public
  create public .

public section.

  types:
    BEGIN OF ty_node,
        id          TYPE i,
        parent_id   TYPE i,
        token       TYPE string,
        token_ucase TYPE string,
        token_index TYPE i,
        level       TYPE i,
      END OF ty_node .
  types:
    ty_tree  TYPE STANDARD TABLE OF ty_node WITH KEY id .

  methods GET_CHILD_NODES
    importing
      !IV_NODE_ID type I
    returning
      value(RT_CHILD_NODES_TREE) type TY_TREE .
  methods GET_NODE_END_TOKEN_INDEX
    importing
      !IV_NODE_ID type I
    returning
      value(RV_END_TOKEN_INDEX) type I .
  methods GET_NODE_INFO
    importing
      !IV_NODE_ID type I
    returning
      value(RS_NODE_INFO) type TY_NODE .
  methods GET_NODE_SQL
    importing
      !IV_NODE_ID type I
    returning
      value(RV_SQL_PART) type STRING .
  methods GET_NODE_SQL_START_AT_OFFSET
    importing
      !IV_NODE_ID type I
      value(IV_NUMBER_OF_TOKENS_OFFSET) type I
    returning
      value(RV_SQL_PART) type STRING .
  methods GET_NODE_SQL_WITHOUT_SELF
    importing
      !IV_NODE_ID type I
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
      value(IV_MAIN_NODE_ID) type I
      value(IV_N) type I
    returning
      value(RV_TOKEN) type STRING .
  methods GET_TOP_NODE
    returning
      value(RS_TOP_NODE) type TY_NODE .
  methods RUN_RECURSIVE_DESCENT_PARSER .
  methods SET_SQL
    importing
      !IV_SQL type CLIKE .
protected section.
private section.

  data MV_SQL type STRING .
  data MT_TOKENS type STRING_TABLE .
  data MV_CURRENT_TOKEN_INDEX type I .
  data MT_PARSED_TREE type TY_TREE .
  data MV_CURRENT_TOKEN type STRING .
  data MV_CURRENT_TOKEN_UCASE type STRING .
  data MV_REACHED_TO_END_FLAG type ABAP_BOOL .

  methods _UP_TO_N_ROWS
    importing
      !IV_PARENT_ID type I .
  methods _WHERE
    importing
      !IV_PARENT_ID type I .
  methods _UPDATE
    returning
      value(RV_IT_WAS_UPDATE) type ABAP_BOOL .
  methods _UPDATE_SET_FIELD
    importing
      !IV_PARENT_ID type I .
  methods _DELETE .
  methods _FOR_ALL_ENTRIES_IN
    importing
      !IV_PARENT_ID type I .
  methods _UPDATE_SET_FIELDS
    importing
      !IV_PARENT_ID type I .
  methods _GROUP_BY
    importing
      !IV_PARENT_ID type I .
  methods _GROUP_ORDER_FIELDS
    importing
      !IV_PARENT_ID type I .
  methods _ORDER_BY
    importing
      !IV_PARENT_ID type I .
  methods _RIGHT_OPERAND_IS_LIST_OF_VALS
    importing
      !IV_PARENT_ID type I
    returning
      value(RV_IS_LIST_OF_VALS) type ABAP_BOOL .
  methods _RIGHT_OPERAND
    importing
      !IV_PARENT_ID type I .
  methods _EXPRESSION_IN_BRACKETS
    importing
      !IV_PARENT_ID type I
    returning
      value(RV_IT_WAS_EXPR_IN_BRACKETS) type ABAP_BOOL .
  methods _EXPRESSION
    importing
      !IV_PARENT_ID type I .
  methods _EXPRESSION_AND
    importing
      !IV_PARENT_ID type I .
  methods _EXPRESSION_NOT
    importing
      !IV_PARENT_ID type I .
  methods _EXPRESSION_NOT_BETWEEN
    importing
      !IV_PARENT_ID type I
    returning
      value(RV_IT_WAS_BETWEEN) type ABAP_BOOL .
  methods _EXPRESSION_OR
    importing
      !IV_PARENT_ID type I .
  methods _JOIN
    importing
      !IV_PARENT_ID type I
    returning
      value(RV_IT_WAS_JOIN) type ABAP_BOOL .
  methods _ON_EXPRESSION
    importing
      !IV_PARENT_ID type I .
  methods _ALIAS
    importing
      !IV_PARENT_ID type I .
  methods _COL_SPEC
    importing
      !IV_PARENT_ID type I .
  methods _FROM
    importing
      !IV_PARENT_ID type I .
  methods _FUNCTION
    importing
      !IV_PARENT_ID type I
    returning
      value(RV_IT_IS_REALLY_FUNCTION) type ABAP_BOOL .
  methods _SELECT_FIELD
    importing
      !IV_PARENT_ID type I .
  methods _SELECT_FIELDS
    importing
      !IV_PARENT_ID type I .
  methods _STEP_FORWARD
    returning
      value(RV_MOVE_SUCCESSFUL) type ABAP_BOOL .
  methods _TABLE
    importing
      value(IV_PARENT_ID) type I .
  methods _ADD_NODE
    importing
      value(IV_PARENT_ID) type I optional
    returning
      value(RV_NEW_NODE_ID) type I .
  methods _SELECT
    returning
      value(RV_IT_WAS_SELECT) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_ZOSQL_PARSER_RECURS_DESC IMPLEMENTATION.


  method GET_CHILD_NODES.

    FIELD-SYMBOLS: <ls_node> LIKE LINE OF mt_parsed_tree.

    LOOP AT mt_parsed_tree ASSIGNING <ls_node>
      WHERE parent_id = iv_node_id.

      APPEND <ls_node> TO rt_child_nodes_tree.
    ENDLOOP.
  endmethod.


  METHOD GET_NODE_END_TOKEN_INDEX.

    DATA: lt_child_nodes              TYPE ty_tree,
          lv_end_token_index_of_child TYPE i.

    FIELD-SYMBOLS: <ls_node> TYPE ty_node.

    READ TABLE mt_parsed_tree WITH KEY id = iv_node_id ASSIGNING <ls_node>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    rv_end_token_index = <ls_node>-token_index.

    lt_child_nodes = get_child_nodes( iv_node_id ).

    LOOP AT lt_child_nodes ASSIGNING <ls_node>.
      lv_end_token_index_of_child = get_node_end_token_index( <ls_node>-id ).

      IF lv_end_token_index_of_child > rv_end_token_index.
        rv_end_token_index = lv_end_token_index_of_child.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  method GET_NODE_INFO.
    READ TABLE mt_parsed_tree WITH KEY id = iv_node_id INTO rs_node_info.
  endmethod.


  method GET_NODE_SQL.
    rv_sql_part = get_node_sql_start_at_offset( iv_node_id                 = iv_node_id
                                                iv_number_of_tokens_offset = 0 ).
  endmethod.


  method GET_NODE_SQL_START_AT_OFFSET.
    DATA: lv_start_token_index TYPE i,
          lv_end_token_index   TYPE i,
          lv_token             TYPE string.

    FIELD-SYMBOLS: <ls_node> LIKE LINE OF mt_parsed_tree.

    READ TABLE mt_parsed_tree WITH KEY id = iv_node_id ASSIGNING <ls_node>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lv_start_token_index = <ls_node>-token_index + iv_number_of_tokens_offset.
    lv_end_token_index   = get_node_end_token_index( iv_node_id ).

    rv_sql_part = get_sql_as_range_of_tokens( iv_start_token_index = lv_start_token_index
                                              iv_end_token_index   = lv_end_token_index ).
  endmethod.


  method GET_NODE_SQL_WITHOUT_SELF.

    rv_sql_part = get_node_sql_start_at_offset( iv_node_id                 = iv_node_id
                                                iv_number_of_tokens_offset = 1 ).
  endmethod.


  method GET_NODE_TOKEN_INDEX.
    DATA: ls_node_info TYPE ty_node.

    ls_node_info = get_node_info( iv_node_id ).
    rv_token_index = ls_node_info-token_index.
  endmethod.


  method GET_SQL_AS_RANGE_OF_TOKENS.

    DATA: lv_token TYPE string.

    LOOP AT mt_tokens INTO lv_token
      FROM iv_start_token_index TO iv_end_token_index.

      CONCATENATE rv_sql_part lv_token INTO rv_sql_part SEPARATED BY space.
    ENDLOOP.

    SHIFT rv_sql_part LEFT DELETING LEADING space.
  endmethod.


  method GET_TOKEN_OF_NTH_CHILD_NODE.

    DATA: lt_child_nodes    TYPE zcl_zosql_parser_recurs_desc=>ty_tree,
          ls_nth_child_node TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    lt_child_nodes = get_child_nodes( iv_main_node_id ).

    READ TABLE lt_child_nodes INDEX iv_n INTO ls_nth_child_node.
    IF sy-subrc = 0.
      rv_token = ls_nth_child_node-token.
    ENDIF.
  endmethod.


  method GET_TOP_NODE.
    READ TABLE mt_parsed_tree WITH KEY parent_id = 0 INTO rs_top_node.
  endmethod.


  method RUN_RECURSIVE_DESCENT_PARSER.

    " Backus–Naur Form for supported SQL
    " <SELECT> ::= SELECT <SELECT_FIELDS> <FROM> <UP_TO_N_ROWS> <FOR_ALL_ENTRIES> <WHERE> <GROUP_BY>
    " <SELECT_FIELDS> ::= * | <SELECT_FIELD> {<SELECT_FIELD>} | <SELECT_FIELD>,{<SELECT_FIELD>}
    " <SELECT_FIELD> ::= data_source~* | <FUNCTION> [<ALIAS>] | <COL_SPEC> [<ALIAS>]
    " <FUNCTION> ::= function_name( col_name )
    " <COL_SPEC> ::= col_name | data_source~col_name
    " <ALIAS> ::= AS alias
    " <FROM> ::= FROM tabname [<ALIAS>] { <JOIN> }
    " <JOIN> ::= [LEFT] [OUTER] JOIN tabname [<ALIAS>] [<ON_EXPRESSION>]
    " <ON_EXPRESSION> ::= ON <EXPRESSION>
    " <EXPRESSION> ::= <EXPRESSION_OR> OR <EXPRESSION> | <EXPRESSION_OR>
    " <EXPRESSION_OR> ::= <EXPRESSION_AND> AND <EXPRESSION_OR> | <EXPRESSION_AND>
    " <EXPRESSION_AND> ::= NOT <EXPRESSION_NOT> | <EXPRESSION_NOT>
    " <EXPRESSION_NOT> ::= left_operand <OPERATION> <RIGHT_OPERAND> |
    "                      left_operand BETWEEN value1 AND value2 |
    "                      ( <EXPRESSION> )
    " <OPERATION> ::= EQ | LE | LT | GE | GT | LIKE | BETWEEN | IN | = | <= | < | >= | >
    " <RIGHT_OPERAND> ::= right_operand | (value{,value})
    " <UP_TO_N_ROWS> ::= UP TO num_of_rows ROWS
    " <FOR_ALL_ENTRIES> ::= FOR ALL ENTRIES IN table_name
    " <WHERE> ::= WHERE <EXPRESSION>
    " <GROUP_BY> ::= GROUP BY <GROUP_ORDER_FIELDS>
    " <GROUP_ORDER_FIELDS> ::= <COL_SPEC> { <COL_SPEC> }
    "
    " <UPDATE> ::= UPDATE tabname SET <UPDATE_SET_FIELDS> <WHERE>
    " <UPDATE_SET_FIELDS> ::= <UPDATE_SET_FIELD> | {<UPDATE_SET_FIELD>}
    " <UPDATE_SET_FIELD> ::= col_name = value
    "
    " <DELETE> ::= DELETE FROM tabname <WHERE>

    mt_tokens = zcl_zosql_utils=>split_condition_into_tokens( mv_sql ).
    mv_current_token_index = 0.
    mv_reached_to_end_flag = abap_false.

    _step_forward( ).

    IF _select( ) = abap_true.
      RETURN.
    ENDIF.

    IF _update( ) = abap_true.
      RETURN.
    ENDIF.

    _delete( ).
  endmethod.


  method SET_SQL.
    mv_sql = iv_sql.
  endmethod.


  method _ADD_NODE.
    DATA: ls_node  TYPE ty_node.

    FIELD-SYMBOLS: <ls_parent_node> TYPE ty_node.

    ls_node-id = LINES( mt_parsed_tree ) + 1.
    ls_node-token = mv_current_token.
    ls_node-token_ucase = mv_current_token_ucase.
    ls_node-token_index = mv_current_token_index.

    ls_node-parent_id = iv_parent_id.

    IF ls_node-parent_id IS INITIAL.
      ls_node-level = 1.
    ELSE.
      READ TABLE mt_parsed_tree WITH KEY id = iv_parent_id ASSIGNING <ls_parent_node>.
      IF sy-subrc = 0.
        ls_node-level = <ls_parent_node>-level + 1.
      ENDIF.
    ENDIF.

    APPEND ls_node TO mt_parsed_tree.

    rv_new_node_id = ls_node-id.
  endmethod.


  method _ALIAS.
    IF mv_current_token_ucase = 'AS'.
      _add_node( iv_parent_id = iv_parent_id ).

      _step_forward( ).

      _add_node( iv_parent_id = iv_parent_id ).

      _step_forward( ).
    ENDIF.
  endmethod.


  method _COL_SPEC.

    DATA: lv_select_column_node_id TYPE i.

    lv_select_column_node_id = _add_node( iv_parent_id = iv_parent_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _alias( lv_select_column_node_id ).
  endmethod.


  method _DELETE.

    DATA: lv_delete_node_id TYPE i.

    IF mv_current_token_ucase <> 'DELETE'.
      RETURN.
    ENDIF.

    lv_delete_node_id = _add_node( ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'FROM'.
      RETURN.
    ENDIF.

    _add_node( lv_delete_node_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( lv_delete_node_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _where( lv_delete_node_id ).
  endmethod.


  method _EXPRESSION.

    DATA: lv_left_from_or_node_id TYPE i.

    lv_left_from_or_node_id = _add_node( iv_parent_id ).

    _expression_or( lv_left_from_or_node_id ).

    IF mv_current_token_ucase <> 'OR'.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id ).
    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _expression( iv_parent_id ).
  endmethod.


  method _EXPRESSION_AND.

    DATA: lv_not_before_compare_node_id TYPE i.

    IF mv_current_token_ucase = 'NOT'.
      lv_not_before_compare_node_id = _add_node( iv_parent_id ).

      IF _step_forward( ) <> abap_true.
        RETURN.
      ENDIF.

      _expression_not( lv_not_before_compare_node_id ).
    ELSE.
      _expression_not( iv_parent_id ).
    ENDIF.
  endmethod.


  METHOD _expression_in_brackets.

    DATA: lv_start_of_expression_node_id TYPE i.

    IF mv_current_token <> '('.
      RETURN.
    ENDIF.

    lv_start_of_expression_node_id = _add_node( iv_parent_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _expression( lv_start_of_expression_node_id ).

    IF mv_current_token = ')'.
      _add_node( lv_start_of_expression_node_id ).
      _step_forward( ).

      rv_it_was_expr_in_brackets = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD _expression_not.

    DATA: lv_start_of_comparison_node_id TYPE i,
          lv_is_expression_in_brackets   TYPE abap_bool,
          lv_is_operator_between         TYPE abap_bool.

    lv_is_expression_in_brackets = _expression_in_brackets( iv_parent_id ).

    IF lv_is_expression_in_brackets = abap_true.
      RETURN.
    ENDIF.

    lv_start_of_comparison_node_id = _add_node( iv_parent_id ).
    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    lv_is_operator_between = _expression_not_between( lv_start_of_comparison_node_id ).

    IF lv_is_operator_between <> abap_true.
      _add_node( lv_start_of_comparison_node_id ).

      IF _step_forward( ) <> abap_true.
        RETURN.
      ENDIF.

      _right_operand( lv_start_of_comparison_node_id ).
    ENDIF.
  ENDMETHOD.


  method _EXPRESSION_NOT_BETWEEN.

    IF mv_current_token_ucase <> 'BETWEEN'.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'AND'.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id ).

    _step_forward( ).

    rv_it_was_between = abap_true.
  endmethod.


  method _EXPRESSION_OR.

    DATA: lv_left_from_and_node_id TYPE i.

    lv_left_from_and_node_id = _add_node( iv_parent_id ).

    _expression_and( lv_left_from_and_node_id ).

    IF mv_current_token_ucase <> 'AND'.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id ).
    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _expression_or( iv_parent_id ).
  endmethod.


  method _FOR_ALL_ENTRIES_IN.

    DATA: lv_for_all_entries_node_id TYPE i.

    IF mv_current_token_ucase <> 'FOR'.
      RETURN.
    ENDIF.

    lv_for_all_entries_node_id = _add_node( iv_parent_id = iv_parent_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'ALL'.
      RETURN.
    ENDIF.

    _add_node( lv_for_all_entries_node_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'ENTRIES'.
      RETURN.
    ENDIF.

    _add_node( lv_for_all_entries_node_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'IN'.
      RETURN.
    ENDIF.

    _add_node( lv_for_all_entries_node_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( lv_for_all_entries_node_id ).

    _step_forward( ).
  endmethod.


  METHOD _from.

    DATA: lv_from_node_id         TYPE i.

    IF mv_current_token_ucase <> 'FROM'.
      RETURN.
    ENDIF.

    lv_from_node_id = _add_node( iv_parent_id = iv_parent_id ).

    _step_forward( ).

    _table( iv_parent_id = lv_from_node_id ).

    DO.

      IF _join( lv_from_node_id ) <> abap_true.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  method _FUNCTION.

    DATA: lv_function_field_node_id TYPE i.

    FIND FIRST OCCURRENCE OF '(' IN mv_current_token.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lv_function_field_node_id = _add_node( iv_parent_id = iv_parent_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_function_field_node_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token = ')'.
      _add_node( iv_parent_id = lv_function_field_node_id ).
      rv_it_is_really_function = abap_true.
    ENDIF.

    _step_forward( ).

    _alias( lv_function_field_node_id ).
  endmethod.


  method _GROUP_BY.

    DATA: lv_group_by_node_id TYPE i.

    IF mv_current_token_ucase <> 'GROUP'.
      RETURN.
    ENDIF.

    lv_group_by_node_id = _add_node( iv_parent_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'BY'.
      RETURN.
    ENDIF.

    _add_node( lv_group_by_node_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _group_order_fields( lv_group_by_node_id ).
  endmethod.


  method _GROUP_ORDER_FIELDS.

    DO.
      _col_spec( iv_parent_id ).

      IF mv_current_token_ucase = 'ORDER' OR mv_reached_to_end_flag = abap_true.
        EXIT.
      ENDIF.
    ENDDO.
  endmethod.


  method _JOIN.

    DATA: lv_join_node_id TYPE i.

    IF mv_current_token_ucase = 'LEFT' OR mv_current_token_ucase = 'JOIN'.
      lv_join_node_id = _add_node( iv_parent_id ).
    ELSE.
      RETURN.
    ENDIF.

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase = 'OUTER' OR mv_current_token_ucase = 'JOIN'.
      _add_node( lv_join_node_id ).

      IF _step_forward( ) <> abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF mv_current_token_ucase = 'JOIN'.
      _add_node( lv_join_node_id ).

      IF _step_forward( ) <> abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    _table( lv_join_node_id ).

    _on_expression( lv_join_node_id ).

    rv_it_was_join = abap_true.
  endmethod.


  method _ON_EXPRESSION.

    DATA: lv_on_node_id TYPE i.

    IF mv_current_token_ucase <> 'ON'.
      RETURN.
    ENDIF.

    lv_on_node_id = _add_node( iv_parent_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _expression( lv_on_node_id ).
  endmethod.


  method _ORDER_BY.

    DATA: lv_order_by_node_id TYPE i.

    IF mv_current_token_ucase <> 'ORDER'.
      RETURN.
    ENDIF.

    lv_order_by_node_id = _add_node( iv_parent_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'BY'.
      RETURN.
    ENDIF.

    _add_node( lv_order_by_node_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _group_order_fields( lv_order_by_node_id ).
  endmethod.


  method _RIGHT_OPERAND.

    DATA: lv_is_list_with_brackets TYPE abap_bool.

    lv_is_list_with_brackets = _right_operand_is_list_of_vals( iv_parent_id ).

    IF lv_is_list_with_brackets <> abap_true.
      _add_node( iv_parent_id ).
      _step_forward( ).
    ENDIF.
  endmethod.


  method _RIGHT_OPERAND_IS_LIST_OF_VALS.

    DATA: lv_list_of_values_node_id TYPE i.

    IF zcl_zosql_utils=>get_first_n_chars( iv_string              = mv_current_token
                                           iv_how_many_characters = 1 ) <> '('.
      RETURN.
    ENDIF.

    lv_list_of_values_node_id = _add_node( iv_parent_id ).

    DO.

      FIND FIRST OCCURRENCE OF ')' IN mv_current_token.
      IF sy-subrc = 0.
        rv_is_list_of_vals = abap_true.
        EXIT.
      ENDIF.

      IF _step_forward( ) <> abap_true.
        EXIT.
      ENDIF.

      _add_node( lv_list_of_values_node_id ).
    ENDDO.

    _step_forward( ).
  endmethod.


  method _SELECT.

    DATA: lv_select_node_id TYPE i.

    IF mv_current_token_ucase = 'SELECT'.
      rv_it_was_select = abap_true.
    ELSE.
      RETURN.
    ENDIF.

    lv_select_node_id = _add_node( ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _select_fields( lv_select_node_id ).

    _from( lv_select_node_id ).

    _up_to_n_rows( lv_select_node_id ).

    _for_all_entries_in( lv_select_node_id ).

    _where( lv_select_node_id ).

    _group_by( lv_select_node_id ).

    _order_by( lv_select_node_id ).
  endmethod.


  METHOD _SELECT_FIELD.

    DATA: lv_is_function TYPE abap_bool.

    IF zcl_zosql_utils=>get_last_n_chars( iv_string              = mv_current_token
                                          iv_how_many_characters = 2 ) = '~*'.
      _add_node( iv_parent_id = iv_parent_id ).
      RETURN.
    ENDIF.

    lv_is_function = _function( iv_parent_id ).

    IF lv_is_function <> abap_true.
      _col_spec( iv_parent_id ).
    ENDIF.
  ENDMETHOD.


  method _SELECT_FIELDS.
    DATA: lv_select_fields_node_id TYPE i.

    IF mv_current_token = '*'.
      _add_node( iv_parent_id = iv_parent_id ).
      _step_forward( ).
      RETURN.
    ENDIF.

    DO.

      IF mv_current_token_ucase = 'FROM'.
        RETURN.
      ENDIF.

      _select_field( iv_parent_id = iv_parent_id ).
    ENDDO.
  endmethod.


  METHOD _step_forward.

    mv_current_token_index = mv_current_token_index + 1.
    READ TABLE mt_tokens INDEX mv_current_token_index INTO mv_current_token.
    IF sy-subrc <> 0.
      rv_move_successful = abap_false.
      CLEAR: mv_current_token, mv_current_token_ucase.
      mv_reached_to_end_flag = abap_true.
      RETURN.
    ENDIF.

    mv_current_token_ucase = zcl_zosql_utils=>to_upper_case( mv_current_token ).

    rv_move_successful = abap_true.
  ENDMETHOD.


  method _TABLE.

    DATA: lv_from_table_node_id TYPE i.

    lv_from_table_node_id = _add_node( iv_parent_id = iv_parent_id ).

    _step_forward( ).

    _alias( lv_from_table_node_id ).
  endmethod.


  method _UPDATE.
    DATA: lv_update_node_id TYPE i.

    IF mv_current_token_ucase = 'UPDATE'.
      rv_it_was_update = abap_true.
    ELSE.
      RETURN.
    ENDIF.

    lv_update_node_id = _add_node( ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( lv_update_node_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _update_set_fields( lv_update_node_id ).
    _where( lv_update_node_id ).
  endmethod.


  method _UPDATE_SET_FIELD.

    DATA: lv_set_field_node_id TYPE i.

    lv_set_field_node_id = _add_node( iv_parent_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token <> '='.
      RETURN.
    ENDIF.

    _add_node( lv_set_field_node_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( lv_set_field_node_id ).

    _step_forward( ).
  endmethod.


  METHOD _update_set_fields.

    DATA: lv_set_node_id TYPE i.

    IF mv_current_token_ucase <> 'SET'.
      RETURN.
    ENDIF.

    lv_set_node_id = _add_node( iv_parent_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    DO.

      IF mv_current_token_ucase = 'WHERE' OR mv_reached_to_end_flag = abap_true.
        EXIT.
      ENDIF.

      _update_set_field( lv_set_node_id ).
    ENDDO.
  ENDMETHOD.


  method _UP_TO_N_ROWS.

    DATA: lv_up_to_n_rows_node_id TYPE i.

    IF mv_current_token_ucase <> 'UP'.
      RETURN.
    ENDIF.

    lv_up_to_n_rows_node_id = _add_node( iv_parent_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'TO'.
      RETURN.
    ENDIF.

    _add_node( lv_up_to_n_rows_node_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( lv_up_to_n_rows_node_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( lv_up_to_n_rows_node_id ).

    _step_forward( ).
  endmethod.


  method _WHERE.

    DATA: lv_where_node_id TYPE i.

    IF mv_current_token_ucase <> 'WHERE'.
      RETURN.
    ENDIF.

    lv_where_node_id = _add_node( iv_parent_id ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _expression( lv_where_node_id ).
  endmethod.
ENDCLASS.
