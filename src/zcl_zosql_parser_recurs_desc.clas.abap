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
        node_type   TYPE string,
      END OF ty_node .
  types:
    ty_tree  TYPE STANDARD TABLE OF ty_node WITH KEY id .

  constants:
    BEGIN OF node_type,
                 select                         TYPE string VALUE 'SELECT',
                 select_single                  TYPE string VALUE 'SELECT_SINGLE',
                 select_distinct                TYPE string VALUE 'SELECT_DISTINCT',
                 select_field                   TYPE string VALUE 'SELECT_FIELD',
                 function                       TYPE string VALUE 'FUNCTION',
                 distinct                       TYPE string VALUE 'DISTINCT',
                 function_argument              TYPE string VALUE 'FUNCTION_ARGUMENT',
                 opening_bracket                TYPE string VALUE 'OPENING_BRACKET',
                 closing_bracket                TYPE string VALUE 'CLOSING_BRACKET',
                 alias_as                       TYPE string VALUE 'ALIAS_AS',
                 alias                          TYPE string VALUE 'ALIAS',
                 from                           TYPE string VALUE 'FROM',
                 table                          TYPE string VALUE 'TABLE',
                 join                           TYPE string VALUE 'JOIN',
                 join_on                        TYPE string VALUE 'JOIN_ON',
                 expression_left_part           TYPE string VALUE 'EXPRESSION_LEFT_PART',
                 expression_logical_operator    TYPE string VALUE 'EXPRESSION_LOGICAL_OPERATOR',
                 expression_not                 TYPE string VALUE 'EXPRESSION_NOT',
                 expression_between             TYPE string VALUE 'EXPRESSION_BETWEEN',
                 expression_between_lower_bound TYPE string VALUE 'EXPRESSION_BETWEEN_LOWER_BOUND',
                 expression_between_and_oper    TYPE string VALUE 'EXPRESSION_BETWEEN_AND_OPERATOR',
                 expression_between_upper_bound TYPE string VALUE 'EXPRESSION_BETWEEN_UPPER_BOUND',
                 expression_comparison_operator TYPE string VALUE 'EXPRESSION_COMPARISON_OPERATOR',
                 expression_right_part          TYPE string VALUE 'EXPRESSION_RIGHT_PART',
                 expression_in                  TYPE string VALUE 'EXPRESSION_IN',
                 expression_exists              TYPE string VALUE 'EXISTS',
                 expression_like                TYPE string VALUE 'EXPRESSION_LIKE',
                 expression_like_escape         TYPE string VALUE 'EXPRESSION_LIKE_ESCAPE',
                 expression_like_escape_symbol  TYPE string VALUE 'EXPRESSION_LIKE_ESCAPE_SYMBOL',
                 subquery_opening_bracket       TYPE string VALUE 'SUBQUERY_OPENING_BRACKET',
                 up_to_n_rows                   TYPE string VALUE 'UP_TO_N_ROWS',
                 up_to_n_rows_count             TYPE string VALUE 'UP_TO_N_ROWS_COUNT',
                 for_all_entries                TYPE string VALUE 'FOR_ALL_ENTRIES',
                 for_all_entries_tabname        TYPE string VALUE 'FOR_ALL_ENTRIES_TABNAME',
                 where                          TYPE string VALUE 'WHERE',
                 group_by                       TYPE string VALUE 'GROUP_BY',
                 order_by                       TYPE string VALUE 'ORDER_BY',
                 update                         TYPE string VALUE 'UPDATE',
                 update_tabname                 TYPE string VALUE 'UPDATE_TABNAME',
                 update_set_fields              TYPE string VALUE 'UPDATE_SET_FIELDS',
                 update_set_field_name          TYPE string VALUE 'UPDATE_SET_FIELD_NAME',
                 update_set_equal_operator      TYPE string VALUE 'UPDATE_SET_EQUAL_OPERATOR',
                 update_set_value               TYPE string VALUE 'UPDATE_SET_VALUE',
                 delete                         TYPE string VALUE 'DELETE',
                 delete_tabname                 TYPE string VALUE 'DELETE_TABNAME',
                 list_of_vals_opening_bracket   TYPE string VALUE 'LIST_OF_VALS_OPENING_BRACKET',
                 list_of_vals_value             TYPE string VALUE 'LIST_OF_VALS_VALUE',
               END OF node_type .

  methods GET_CHILD_NODE_TOKEN_WITH_TYPE
    importing
      !IV_NODE_ID type I
      !IV_NODE_TYPE type CLIKE
      value(IV_UCASE) type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_TOKEN) type STRING .
  methods GET_CHILD_NODE_WITH_TYPE
    importing
      !IV_NODE_ID type I
      !IV_NODE_TYPE type CLIKE
    returning
      value(RS_CHILD_NODE) type TY_NODE .
  methods GET_CHILD_NODE_WITH_TYPE_RECUR
    importing
      !IV_NODE_ID type I
      !IV_NODE_TYPE type CLIKE
    returning
      value(RS_CHILD_NODE) type TY_NODE .
  methods GET_CHILD_NODES
    importing
      !IV_NODE_ID type I
    returning
      value(RT_CHILD_NODES_TREE) type TY_TREE .
  methods GET_CHILD_NODES_WITH_SELF
    importing
      !IV_NODE_ID type I
    returning
      value(RT_CHILD_NODES_TREE) type TY_TREE .
  methods GET_CHILD_NODES_RECURSIVE
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

  types:
    BEGIN OF TY_POS_AND_STATE_STACK_REC,
           current_position   TYPE i,
           parsed_tree        TYPE ty_tree,
         END OF ty_pos_and_state_stack_rec .

  data MV_SQL type STRING .
  data MT_TOKENS type STRING_TABLE .
  data MV_CURRENT_TOKEN_INDEX type I .
  data MT_PARSED_TREE type TY_TREE .
  data MV_CURRENT_TOKEN type STRING .
  data MV_CURRENT_TOKEN_UCASE type STRING .
  data MV_REACHED_TO_END_FLAG type ABAP_BOOL .
  data:
    MT_POS_and_state_STACK   TYPE STANDARD TABLE OF ty_pos_and_state_stack_rec .
  constants C_NOT type STRING value 'NOT' ##NO_TEXT.

  methods _EXPRESSION_EXISTS
    importing
      value(IV_PARENT_ID) type I
    returning
      value(RV_IT_WAS_EXISTS_CONDITION) type ABAP_BOOL .
  methods _POP_POSITION_AND_STATE .
  methods _PUSH_POSITION_AND_STATE .
  methods _SUBQUERY
    importing
      value(IV_PARENT_ID) type I
    returning
      value(RV_IT_WAS_SUBQUERY) type ABAP_BOOL .
  methods _DELETE_NODE
    importing
      !IV_NODE_ID type I .
  methods _EXPRESSION_IN
    importing
      value(IV_PARENT_ID) type I
      value(IV_IN_JOIN) type ABAP_BOOL
    returning
      value(RV_IT_WAS_IN) type ABAP_BOOL .
  methods _MINIMIZE_NESTED_NODE_CHAINS .
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
  methods _RIGHT_OPERAND_LIST_OF_VALS
    importing
      !IV_PARENT_ID type I
    returning
      value(RV_IS_LIST_OF_VALS) type ABAP_BOOL .
  methods _RIGHT_OPERAND_IN
    importing
      !IV_PARENT_ID type I
      value(IV_IN_JOIN) type ABAP_BOOL .
  methods _EXPRESSION_IN_BRACKETS
    importing
      value(IV_PARENT_ID) type I
      value(IV_IN_JOIN) type ABAP_BOOL
    returning
      value(RV_IT_WAS_EXPR_IN_BRACKETS) type ABAP_BOOL .
  methods _EXPRESSION
    importing
      value(IV_PARENT_ID) type I
      value(IV_IN_JOIN) type ABAP_BOOL .
  methods _EXPRESSION_AND
    importing
      value(IV_PARENT_ID) type I
      value(IV_IN_JOIN) type ABAP_BOOL .
  methods _EXPRESSION_NOT
    importing
      value(IV_PARENT_ID) type I
      value(IV_IN_JOIN) type ABAP_BOOL .
  methods _EXPRESSION_LIKE
    importing
      !IV_PARENT_ID type I
    returning
      value(RV_IT_WAS_LIKE) type ABAP_BOOL .
  methods _EXPRESSION_BETWEEN
    importing
      !IV_PARENT_ID type I
    returning
      value(RV_IT_WAS_BETWEEN) type ABAP_BOOL .
  methods _EXPRESSION_OR
    importing
      value(IV_PARENT_ID) type I
      value(IV_IN_JOIN) type ABAP_BOOL .
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
  methods _STEP_TO_INDEX
    importing
      value(IV_NEW_INDEX) type I
    returning
      value(RV_MOVE_SUCCESSFUL) type ABAP_BOOL .
  methods _STEP_FORWARD
    returning
      value(RV_MOVE_SUCCESSFUL) type ABAP_BOOL .
  methods _TABLE
    importing
      value(IV_PARENT_ID) type I .
  methods _ADD_NODE
    importing
      value(IV_PARENT_ID) type I optional
      !IV_NODE_TYPE type CLIKE
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


  method GET_CHILD_NODES_RECURSIVE.

    DATA: lt_child_nodes TYPE ty_tree.

    FIELD-SYMBOLS: <ls_node> LIKE LINE OF mt_parsed_tree.

    LOOP AT mt_parsed_tree ASSIGNING <ls_node>
      WHERE parent_id = iv_node_id.

      APPEND <ls_node> TO rt_child_nodes_tree.

      lt_child_nodes = get_child_nodes_recursive( <ls_node>-id ).
      APPEND LINES OF lt_child_nodes TO rt_child_nodes_tree.
    ENDLOOP.
  endmethod.


  METHOD get_child_nodes_with_self.

    DATA: ls_current_node TYPE ty_node.

    ls_current_node = get_node_info( iv_node_id ).
    rt_child_nodes_tree = get_child_nodes( iv_node_id ).
    INSERT ls_current_node INTO rt_child_nodes_tree INDEX 1.
  ENDMETHOD.


  METHOD get_child_node_token_with_type.
    DATA: ls_child_node   TYPE ty_node.

    ls_child_node = get_child_node_with_type( iv_node_id   = iv_node_id
                                              iv_node_type = iv_node_type ).

    IF iv_ucase = abap_true.
      rv_token = ls_child_node-token_ucase.
    ELSE.
      rv_token = ls_child_node-token.
    ENDIF.
  ENDMETHOD.


  method GET_CHILD_NODE_WITH_TYPE.
    DATA: lt_child_nodes TYPE ty_tree.

    lt_child_nodes = get_child_nodes( iv_node_id ).

    READ TABLE lt_child_nodes WITH KEY node_type = iv_node_type INTO rs_child_node.
  endmethod.


  method GET_CHILD_NODE_WITH_TYPE_RECUR.
    DATA: lt_child_nodes TYPE ty_tree.

    lt_child_nodes = get_child_nodes_recursive( iv_node_id ).

    READ TABLE lt_child_nodes WITH KEY node_type = iv_node_type INTO rs_child_node.
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

    DATA: lv_token                  TYPE string,
          ls_node_of_previous_token TYPE ty_node.

    LOOP AT mt_tokens INTO lv_token
      FROM iv_start_token_index TO iv_end_token_index.

      IF sy-tabix > 1.
        READ TABLE mt_parsed_tree WITH KEY token_index = sy-tabix - 1 INTO ls_node_of_previous_token.
        IF sy-subrc <> 0.
          CLEAR ls_node_of_previous_token.
        ENDIF.
      ENDIF.

      CASE ls_node_of_previous_token-node_type.
        WHEN node_type-function.
          CONCATENATE rv_sql_part lv_token INTO rv_sql_part.
        WHEN OTHERS.
          CONCATENATE rv_sql_part lv_token INTO rv_sql_part SEPARATED BY space.
      ENDCASE.
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
    " <FUNCTION> ::= function_name( [DISTINCT] col_name ) | COUNT(*)
    " <COL_SPEC> ::= col_name | data_source~col_name
    " <ALIAS> ::= AS alias
    " <FROM> ::= FROM tabname [<ALIAS>] { <JOIN> }
    " <JOIN> ::= [LEFT] [OUTER] JOIN tabname [<ALIAS>] [<ON_EXPRESSION>]
    " <ON_EXPRESSION> ::= ON <EXPRESSION_JOIN>
    " <EXPRESSION_JOIN> ::= <EXPRESSION_JOIN_OR> OR <EXPRESSION_JOIN> | <EXPRESSION_JOIN_OR>
    " <EXPRESSION_JOIN_OR> ::= <EXPRESSION_JOIN_AND> AND <EXPRESSION_JOIN_OR> | <EXPRESSION_JOIN_AND>
    " <EXPRESSION_JOIN_AND> ::= NOT <EXPRESSION_JOIN_NOT> | <EXPRESSION_JOIN_NOT>
    " <EXPRESSION_JOIN_NOT> ::= left_operand <OPERATION> right_operand |
    "                           left_operand [NOT] BETWEEN value1 AND value2 |
    "                           left_operand [NOT] LIKE mask [ESCAPE esc] |
    "                           left_operand IN <RIGHT_OPERAND_LIST_OF_VALS> |
    "                           ( <EXPRESSION_JOIN> )
    " <OPERATION> ::= EQ | = | NE | <> | LE | <= | LT | < | GE | >= | GT | > | LIKE | IN
    " <RIGHT_OPERAND_LIST_OF_VALS> ::= (value{,value})
    " <UP_TO_N_ROWS> ::= UP TO num_of_rows ROWS
    " <FOR_ALL_ENTRIES> ::= FOR ALL ENTRIES IN table_name
    " <WHERE> ::= WHERE <EXPRESSION>
    " <EXPRESSION> ::= <EXPRESSION_OR> OR <EXPRESSION> | <EXPRESSION_OR>
    " <EXPRESSION_OR> ::= <EXPRESSION_AND> AND <EXPRESSION_OR> | <EXPRESSION_AND>
    " <EXPRESSION_AND> ::= NOT <EXPRESSION_NOT> | <EXPRESSION_NOT>
    " <EXPRESSION_NOT> ::= left_operand <OPERATION> right_operand |
    "                      left_operand <OPERATION> ( <SUBQUERY> ) |
    "                      left_operand [NOT] BETWEEN value1 AND value2 |
    "                      left_operand [NOT] LIKE mask [ESCAPE esc] |
    "                      left_operand IN <RIGHT_OPERAND_IN> |
    "                      EXISTS ( <SUBQUERY> )
    "                      ( <EXPRESSION> )
    " <RIGHT_OPERAND_IN> ::= right_operand | <RIGHT_OPERAND_LIST_OF_VALS> | <SUBQUERY>
    " <SUBQUERY> ::= SELECT <SELECT_FIELDS> <FROM> <WHERE> <GROUP_BY>
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

    DO 1 TIMES.
      IF _select( ) = abap_true.
        EXIT.
      ENDIF.

      IF _update( ) = abap_true.
        EXIT.
      ENDIF.

      _delete( ).
    ENDDO.

    _minimize_nested_node_chains( ).
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

    ls_node-node_type = iv_node_type.

    APPEND ls_node TO mt_parsed_tree.

    rv_new_node_id = ls_node-id.
  endmethod.


  method _ALIAS.
    IF mv_current_token_ucase = 'AS'.
      _add_node( iv_parent_id = iv_parent_id
                 iv_node_type = node_type-alias_as ).

      _step_forward( ).

      _add_node( iv_parent_id = iv_parent_id
                 iv_node_type = node_type-alias ).

      _step_forward( ).
    ENDIF.
  endmethod.


  method _COL_SPEC.

    DATA: lv_select_column_node_id TYPE i.

    lv_select_column_node_id = _add_node( iv_parent_id = iv_parent_id
                                          iv_node_type = node_type-select_field ).

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

    lv_delete_node_id = _add_node( iv_node_type = node_type-delete ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'FROM'.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_delete_node_id
               iv_node_type = node_type-delete ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_delete_node_id
               iv_node_type = node_type-delete_tabname ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _where( lv_delete_node_id ).
  endmethod.


  method _DELETE_NODE.

    DATA: ls_node_to_delete TYPE ty_node.

    FIELD-SYMBOLS: <ls_child_node> TYPE ty_node.

    ls_node_to_delete = get_node_info( iv_node_id ).

    LOOP AT mt_parsed_tree ASSIGNING <ls_child_node>
      WHERE parent_id = iv_node_id.

      <ls_child_node>-parent_id = ls_node_to_delete-parent_id.
    ENDLOOP.

    DELETE mt_parsed_tree
      WHERE id = iv_node_id.
  endmethod.


  method _EXPRESSION.

    DATA: lv_left_from_or_node_id TYPE i.

    lv_left_from_or_node_id = _add_node( iv_parent_id = iv_parent_id
                                         iv_node_type = node_type-expression_left_part ).

    _expression_or( iv_parent_id = lv_left_from_or_node_id
                    iv_in_join   = iv_in_join ).

    IF mv_current_token_ucase <> 'OR'.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_left_from_or_node_id
               iv_node_type = node_type-expression_logical_operator ).
    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _expression( iv_parent_id = lv_left_from_or_node_id
                 iv_in_join   = iv_in_join ).
  endmethod.


  method _EXPRESSION_AND.

    DATA: lv_not_before_compare_node_id TYPE i.

    IF mv_current_token_ucase = c_not.
      lv_not_before_compare_node_id = _add_node( iv_parent_id = iv_parent_id
                                                 iv_node_type = node_type-expression_not ).

      IF _step_forward( ) <> abap_true.
        RETURN.
      ENDIF.

      _expression_not( iv_parent_id = lv_not_before_compare_node_id
                       iv_in_join   = iv_in_join ).
    ELSE.
      _expression_not( iv_parent_id = iv_parent_id
                       iv_in_join   = iv_in_join ).
    ENDIF.
  endmethod.


  method _EXPRESSION_BETWEEN.

    _push_position_and_state( ).

    IF mv_current_token_ucase = c_not.
      _add_node( iv_parent_id = iv_parent_id
                 iv_node_type = node_type-expression_not ).

      IF _step_forward( ) <> abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF mv_current_token_ucase <> 'BETWEEN'.
      _pop_position_and_state( ).
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = iv_parent_id
               iv_node_type = node_type-expression_between ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = iv_parent_id
               iv_node_type = node_type-expression_between_lower_bound ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'AND'.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = iv_parent_id
               iv_node_type = node_type-expression_between_and_oper ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = iv_parent_id
               iv_node_type = node_type-expression_between_upper_bound ).

    _step_forward( ).

    rv_it_was_between = abap_true.
  endmethod.


  method _EXPRESSION_EXISTS.

    DATA: lv_exists_node_id TYPE i.

    IF mv_current_token_ucase <> 'EXISTS'.
      RETURN.
    ENDIF.

    lv_exists_node_id = _add_node( iv_parent_id = iv_parent_id
                                   iv_node_type = node_type-expression_exists ).

     IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _subquery( lv_exists_node_id ).

    rv_it_was_exists_condition = abap_true.
  endmethod.


  method _EXPRESSION_IN.
    IF mv_current_token_ucase <> 'IN'.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = iv_parent_id
               iv_node_type = node_type-expression_in ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _right_operand_in( iv_parent_id = iv_parent_id
                       iv_in_join   = iv_in_join ).

    rv_it_was_in = abap_true.
  endmethod.


  METHOD _expression_in_brackets.

    DATA: lv_start_of_expression_node_id TYPE i.

    IF mv_current_token <> '('.
      RETURN.
    ENDIF.

    lv_start_of_expression_node_id = _add_node( iv_parent_id = iv_parent_id
                                                iv_node_type = node_type-opening_bracket ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _expression( iv_parent_id = lv_start_of_expression_node_id
                 iv_in_join   = iv_in_join ).

    IF mv_current_token = ')'.
      _add_node( iv_parent_id = lv_start_of_expression_node_id
                 iv_node_type = node_type-closing_bracket ).
      _step_forward( ).

      rv_it_was_expr_in_brackets = abap_true.
    ENDIF.
  ENDMETHOD.


  method _EXPRESSION_LIKE.

    _push_position_and_state( ).

    IF mv_current_token_ucase = c_not.
      _add_node( iv_parent_id = iv_parent_id
                 iv_node_type = node_type-expression_not ).

      IF _step_forward( ) <> abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF mv_current_token_ucase <> 'LIKE'.
      _pop_position_and_state( ).
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = iv_parent_id
               iv_node_type = node_type-expression_like ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = iv_parent_id
               iv_node_type = node_type-expression_right_part ).

    rv_it_was_like = abap_true.

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase = 'ESCAPE'.
      _add_node( iv_parent_id = iv_parent_id
                 iv_node_type = node_type-expression_like_escape ).

      IF _step_forward( ) <> abap_true.
        RETURN.
      ENDIF.

      _add_node( iv_parent_id = iv_parent_id
                 iv_node_type = node_type-expression_like_escape_symbol ).

      _step_forward( ).
    ENDIF.
  endmethod.


  METHOD _expression_not.

    DATA: lv_start_of_comparison_node_id TYPE i,
          lv_is_expression_in_brackets   TYPE abap_bool,
          lv_is_operator_exists          TYPE abap_bool,
          lv_is_operator_between         TYPE abap_bool,
          lv_is_operator_in              TYPE abap_bool,
          lv_is_operator_like            TYPE abap_bool,
          lv_is_subquery                 TYPE abap_bool.

    lv_is_expression_in_brackets = _expression_in_brackets( iv_parent_id = iv_parent_id
                                                            iv_in_join   = iv_in_join ).

    IF lv_is_expression_in_brackets = abap_true.
      RETURN.
    ENDIF.

    lv_is_operator_exists = _expression_exists( iv_parent_id ).

    IF lv_is_operator_exists = abap_true.
      RETURN.
    ENDIF.

    lv_start_of_comparison_node_id = _add_node( iv_parent_id = iv_parent_id
                                                iv_node_type = node_type-expression_left_part ).
    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    lv_is_operator_between = _expression_between( lv_start_of_comparison_node_id ).
    IF lv_is_operator_between = abap_true.
      RETURN.
    ENDIF.

    lv_is_operator_like = _expression_like( lv_start_of_comparison_node_id ).
    IF lv_is_operator_like = abap_true.
      RETURN.
    ENDIF.

    lv_is_operator_in = _expression_in( iv_parent_id = lv_start_of_comparison_node_id
                                        iv_in_join   = iv_in_join ).
    IF lv_is_operator_in = abap_true.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_start_of_comparison_node_id
               iv_node_type = node_type-expression_comparison_operator ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF iv_in_join <> abap_true.
      lv_is_subquery = _subquery( lv_start_of_comparison_node_id ).
      IF lv_is_subquery = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    _add_node( iv_parent_id = lv_start_of_comparison_node_id
               iv_node_type = node_type-expression_right_part ).

    _step_forward( ).
  ENDMETHOD.


  method _EXPRESSION_OR.

    DATA: lv_left_from_and_node_id TYPE i.

    lv_left_from_and_node_id = _add_node( iv_parent_id = iv_parent_id
                                          iv_node_type = node_type-expression_left_part ).

    _expression_and( iv_parent_id = lv_left_from_and_node_id
                     iv_in_join   = iv_in_join ).

    IF mv_current_token_ucase <> 'AND'.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_left_from_and_node_id
               iv_node_type = node_type-expression_logical_operator ).
    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _expression_or( iv_parent_id = lv_left_from_and_node_id
                    iv_in_join   = iv_in_join ).
  endmethod.


  method _FOR_ALL_ENTRIES_IN.

    DATA: lv_for_all_entries_node_id TYPE i.

    IF mv_current_token_ucase <> 'FOR'.
      RETURN.
    ENDIF.

    lv_for_all_entries_node_id = _add_node( iv_parent_id = iv_parent_id
                                            iv_node_type = node_type-for_all_entries ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'ALL'.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_for_all_entries_node_id
               iv_node_type = node_type-for_all_entries ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'ENTRIES'.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_for_all_entries_node_id
               iv_node_type = node_type-for_all_entries ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'IN'.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_for_all_entries_node_id
               iv_node_type = node_type-for_all_entries ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_for_all_entries_node_id
               iv_node_type = node_type-for_all_entries_tabname ).

    _step_forward( ).
  endmethod.


  METHOD _from.

    DATA: lv_from_node_id         TYPE i.

    IF mv_current_token_ucase <> 'FROM'.
      RETURN.
    ENDIF.

    lv_from_node_id = _add_node( iv_parent_id = iv_parent_id
                                 iv_node_type = node_type-from ).

    _step_forward( ).

    _table( iv_parent_id = lv_from_node_id ).

    DO.

      IF _join( lv_from_node_id ) <> abap_true.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD _function.

    DATA: lv_function_field_node_id TYPE i.

    _push_position_and_state( ).

    lv_function_field_node_id = _add_node( iv_parent_id = iv_parent_id
                                           iv_node_type = node_type-function ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token <> '('.
      _pop_position_and_state( ).
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_function_field_node_id
               iv_node_type = node_type-opening_bracket ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase = 'DISTINCT'.
      _add_node( iv_parent_id = lv_function_field_node_id
                 iv_node_type = node_type-distinct ).

      IF _step_forward( ) <> abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    _add_node( iv_parent_id = lv_function_field_node_id
               iv_node_type = node_type-function_argument ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token = ')'.
      _add_node( iv_parent_id = lv_function_field_node_id
                 iv_node_type = node_type-closing_bracket ).
      rv_it_is_really_function = abap_true.
    ENDIF.

    _step_forward( ).

    _alias( lv_function_field_node_id ).
  ENDMETHOD.


  method _GROUP_BY.

    DATA: lv_group_by_node_id TYPE i.

    IF mv_current_token_ucase <> 'GROUP'.
      RETURN.
    ENDIF.

    lv_group_by_node_id = _add_node( iv_parent_id = iv_parent_id
                                     iv_node_type = node_type-group_by ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'BY'.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_group_by_node_id
               iv_node_type = node_type-group_by ).

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
      lv_join_node_id = _add_node( iv_parent_id = iv_parent_id
                                   iv_node_type = node_type-join ).
    ELSE.
      RETURN.
    ENDIF.

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase = 'OUTER' OR mv_current_token_ucase = 'JOIN'.
      _add_node( iv_parent_id = lv_join_node_id
                 iv_node_type = node_type-join ).

      IF _step_forward( ) <> abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF mv_current_token_ucase = 'JOIN'.
      _add_node( iv_parent_id = lv_join_node_id
                 iv_node_type = node_type-join ).

      IF _step_forward( ) <> abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    _table( lv_join_node_id ).

    _on_expression( lv_join_node_id ).

    rv_it_was_join = abap_true.
  endmethod.


  method _MINIMIZE_NESTED_NODE_CHAINS.

    DATA: lt_child_nodes TYPE ty_tree,
          ls_child_node  TYPE ty_node.

    FIELD-SYMBOLS: <ls_node> TYPE ty_node.

    LOOP AT mt_parsed_tree ASSIGNING <ls_node>.
      lt_child_nodes = get_child_nodes( <ls_node>-id ).

      CHECK LINES( lt_child_nodes ) = 1.
      READ TABLE lt_child_nodes INDEX 1 INTO ls_child_node.

      IF ls_child_node-token_index = <ls_node>-token_index.
        _delete_node( <ls_node>-id ).
      ENDIF.
    ENDLOOP.
  endmethod.


  method _ON_EXPRESSION.

    DATA: lv_on_node_id TYPE i.

    IF mv_current_token_ucase <> 'ON'.
      RETURN.
    ENDIF.

    lv_on_node_id = _add_node( iv_parent_id = iv_parent_id
                               iv_node_type = node_type-join_on ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _expression( iv_parent_id = lv_on_node_id
                 iv_in_join   = abap_true ).
  endmethod.


  method _ORDER_BY.

    DATA: lv_order_by_node_id TYPE i.

    IF mv_current_token_ucase <> 'ORDER'.
      RETURN.
    ENDIF.

    lv_order_by_node_id = _add_node( iv_parent_id = iv_parent_id
                                     iv_node_type = node_type-order_by ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'BY'.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_order_by_node_id
               iv_node_type = node_type-order_by ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _group_order_fields( lv_order_by_node_id ).
  endmethod.


  method _POP_POSITION_AND_STATE.

    DATA: ls_stack_rec TYPE ty_pos_and_state_stack_rec.

    READ TABLE mt_pos_and_state_stack INDEX LINES( mt_pos_and_state_stack ) INTO ls_stack_rec.
    IF sy-subrc = 0.
      _step_to_index( ls_stack_rec-current_position ).
      mt_parsed_tree = ls_stack_rec-parsed_tree.
    ENDIF.
  endmethod.


  method _PUSH_POSITION_AND_STATE.

    DATA: ls_stack_rec  TYPE ty_pos_and_state_stack_rec.

    ls_stack_rec-current_position = mv_current_token_index.
    ls_stack_rec-parsed_tree      = mt_parsed_tree.
    APPEND ls_stack_rec TO mt_pos_and_state_stack.
  endmethod.


  METHOD _right_operand_in.

    DATA: lv_is_subquery           TYPE abap_bool,
          lv_is_list_with_brackets TYPE abap_bool.

    lv_is_subquery = _subquery( iv_parent_id ).

    IF lv_is_subquery = abap_true
      OR iv_in_join = abap_true.

      RETURN.
    ENDIF.

    lv_is_list_with_brackets = _right_operand_list_of_vals( iv_parent_id ).

    IF lv_is_list_with_brackets = abap_true
      OR iv_in_join = abap_true.

      RETURN.
    ENDIF.

    DATA: lv_it_was_subquery TYPE abap_bool.

    lv_it_was_subquery = _subquery( iv_parent_id ).

    IF lv_it_was_subquery = abap_true.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = iv_parent_id
               iv_node_type = node_type-expression_right_part ).
    _step_forward( ).
  ENDMETHOD.


  method _RIGHT_OPERAND_LIST_OF_VALS.

    IF zcl_zosql_utils=>get_first_n_chars( iv_string              = mv_current_token
                                           iv_how_many_characters = 1 ) <> '('.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = iv_parent_id
               iv_node_type = node_type-list_of_vals_opening_bracket ).

    DO.

      FIND FIRST OCCURRENCE OF ')' IN mv_current_token.
      IF sy-subrc = 0.
        rv_is_list_of_vals = abap_true.
        EXIT.
      ENDIF.

      IF _step_forward( ) <> abap_true.
        EXIT.
      ENDIF.

      _add_node( iv_parent_id = iv_parent_id
                 iv_node_type = node_type-list_of_vals_value ).
    ENDDO.

    _step_forward( ).
  endmethod.


  METHOD _select.

    DATA: lv_select_node_id TYPE i.

    IF mv_current_token_ucase = 'SELECT'.
      rv_it_was_select = abap_true.
    ELSE.
      RETURN.
    ENDIF.

    lv_select_node_id = _add_node( iv_node_type = node_type-select ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase = 'SINGLE'.
      _add_node( iv_parent_id = lv_select_node_id
                 iv_node_type = node_type-select_single ).

      IF _step_forward( ) <> abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF mv_current_token_ucase = 'DISTINCT'.
      _add_node( iv_parent_id = lv_select_node_id
                 iv_node_type = node_type-distinct ).

      IF _step_forward( ) <> abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    _select_fields( lv_select_node_id ).

    _from( lv_select_node_id ).

    _up_to_n_rows( lv_select_node_id ).

    _for_all_entries_in( lv_select_node_id ).

    _where( lv_select_node_id ).

    _group_by( lv_select_node_id ).

    _order_by( lv_select_node_id ).
  ENDMETHOD.


  METHOD _SELECT_FIELD.

    DATA: lv_is_function TYPE abap_bool.

    IF zcl_zosql_utils=>get_last_n_chars( iv_string              = mv_current_token
                                          iv_how_many_characters = 2 ) = '~*'.
      _add_node( iv_parent_id = iv_parent_id
                 iv_node_type = node_type-select_field ).
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
      _add_node( iv_parent_id = iv_parent_id
                 iv_node_type = node_type-select_field ).
      _step_forward( ).
      RETURN.
    ENDIF.

    DO.

      IF mv_current_token_ucase = 'FROM' OR mv_reached_to_end_flag = abap_true.
        RETURN.
      ENDIF.

      _select_field( iv_parent_id = iv_parent_id ).
    ENDDO.
  endmethod.


  METHOD _step_forward.
    rv_move_successful = _step_to_index( mv_current_token_index + 1 ).
  ENDMETHOD.


  METHOD _STEP_TO_INDEX.

    mv_current_token_index = iv_new_index.
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


  method _SUBQUERY.

    DATA: lv_node_subquery_open_bracket TYPE i.

    _push_position_and_state( ).

    IF mv_current_token <> '('.
      RETURN.
    ENDIF.

    lv_node_subquery_open_bracket = _add_node( iv_parent_id = iv_parent_id
                                               iv_node_type = node_type-subquery_opening_bracket ).

    IF _step_forward( ) <> abap_true.
      _pop_position_and_state( ).
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'SELECT'.
      _pop_position_and_state( ).
      RETURN.
    ENDIF.

    DATA: lv_select_node_id TYPE i.

    lv_select_node_id = _add_node( iv_parent_id = lv_node_subquery_open_bracket
                                   iv_node_type = node_type-select ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _select_fields( lv_select_node_id ).
    _from( lv_select_node_id ).
    _where( lv_select_node_id ).
    _group_by( lv_select_node_id ).

    IF mv_current_token = ')'.

      _add_node( iv_parent_id = lv_select_node_id
                 iv_node_type = node_type-closing_bracket ).

      rv_it_was_subquery = abap_true.
    ENDIF.

    _step_forward( ).
  endmethod.


  method _TABLE.

    DATA: lv_from_table_node_id TYPE i.

    lv_from_table_node_id = _add_node( iv_parent_id = iv_parent_id
                                       iv_node_type = node_type-table ).

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

    lv_update_node_id = _add_node( iv_node_type = node_type-update ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_update_node_id
               iv_node_type = node_type-update_tabname ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _update_set_fields( lv_update_node_id ).
    _where( lv_update_node_id ).
  endmethod.


  method _UPDATE_SET_FIELD.

    DATA: lv_set_field_node_id TYPE i.

    lv_set_field_node_id = _add_node( iv_parent_id = iv_parent_id
                                      iv_node_type = node_type-update_set_field_name ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token <> '='.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_set_field_node_id
               iv_node_type = node_type-update_set_equal_operator ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_set_field_node_id
               iv_node_type = node_type-update_set_value ).

    _step_forward( ).
  endmethod.


  METHOD _update_set_fields.

    DATA: lv_set_node_id TYPE i.

    IF mv_current_token_ucase <> 'SET'.
      RETURN.
    ENDIF.

    lv_set_node_id = _add_node( iv_parent_id = iv_parent_id
                                iv_node_type = node_type-update_set_fields ).

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

    lv_up_to_n_rows_node_id = _add_node( iv_parent_id = iv_parent_id
                                         iv_node_type = node_type-up_to_n_rows ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    IF mv_current_token_ucase <> 'TO'.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_up_to_n_rows_node_id
               iv_node_type = node_type-up_to_n_rows ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_up_to_n_rows_node_id
               iv_node_type = node_type-up_to_n_rows_count ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _add_node( iv_parent_id = lv_up_to_n_rows_node_id
               iv_node_type = node_type-up_to_n_rows ).

    _step_forward( ).
  endmethod.


  method _WHERE.

    DATA: lv_where_node_id TYPE i.

    IF mv_current_token_ucase <> 'WHERE'.
      RETURN.
    ENDIF.

    lv_where_node_id = _add_node( iv_parent_id = iv_parent_id
                                  iv_node_type = node_type-where ).

    IF _step_forward( ) <> abap_true.
      RETURN.
    ENDIF.

    _expression( iv_parent_id = lv_where_node_id
                 iv_in_join   = abap_false ).
  endmethod.
ENDCLASS.
