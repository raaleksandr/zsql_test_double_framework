class ZCL_ZOSQL_SQL_FSM_DEL definition
  public
  final
  create public .

public section.

  data SELECT_FIELD_LIST type STRING read-only .
  data IS_DISTINCT type ABAP_BOOL read-only .
  data IS_SELECT_SINGLE type ABAP_BOOL read-only .
  data FROM type STRING read-only .
  data FOR_ALL_ENTRIES_TABNAME type STRING read-only .
  data WHERE type STRING read-only .
  data GROUP_BY type STRING read-only .
  data ORDER_BY type STRING read-only .
  data NUMBER_OF_ROWS_EXPR type STRING .

  methods SET_SQL
    importing
      !IV_SQL type CLIKE .
  methods RUN_FSM .
protected section.
private section.

  data MV_SQL type STRING .
  data MT_TOKENS type STRING_TABLE .
  data MV_CURRENT_TOKEN_INDEX type I .
  constants:
    BEGIN OF C_STATE,
               select TYPE STRING VALUE 'SELECT',
               from   TYPE STRING VALUE 'FROM',
               up_to_n_rows TYPE STRING VALUE 'UP_TO_N_ROWS',
               for_all_entries TYPE STRING VALUE 'FOR_ALL_ENTRIES',
               where           TYPE STRING VALUE 'WHERE',
               group_by        TYPE STRING VALUE 'GROUP_BY',
               order_by        TYPE STRING VALUE 'ORDER_BY',
             END OF c_state .
  data MV_CURRENT_STATE type STRING .

  methods _CONDENSE_ATTRIBUTES .
  methods _DELETE_LAST_N_TOKENS_CUR_STR
    importing
      !IV_N type I .
  methods _GET_CURRENT_TOKEN_UCASE
    returning
      value(RV_CURRENT_TOKEN_UCASE) type STRING .
  methods _GET_NTH_NEXT_TOKEN_UCASE
    importing
      !IV_N type I
    returning
      value(RV_NTH_NEXT_TOKEN_UCASE) type STRING .
  methods _GET_NTH_PREVIOUS_TOKEN_UCASE
    importing
      !IV_N type I
    returning
      value(RV_NTH_PREVIOUS_TOKEN_UCASE) type STRING .
  methods _PROCESS_TOKEN .
  methods _SET_STATE
    importing
      !IV_NEW_STATE type CLIKE .
  methods _CHANGE_STATE_IF_NECESSARY .
  methods _CLEAR_ATTRIBUTES .
ENDCLASS.



CLASS ZCL_ZOSQL_SQL_FSM_DEL IMPLEMENTATION.


  method RUN_FSM.

    DATA: lv_previous_state TYPE string.

    _clear_attributes( ).
    mt_tokens = zcl_zosql_utils=>split_condition_into_tokens( mv_sql ).

    mv_current_token_index = 1.
    WHILE mv_current_token_index <= LINES( mt_tokens ).

      lv_previous_state = mv_current_state.
      _change_state_if_necessary( ).

      IF lv_previous_state = mv_current_state.
        _process_token( ).
      ENDIF.

      mv_current_token_index = mv_current_token_index + 1.
    ENDWHILE.

    _condense_attributes( ).
  endmethod.


  method SET_SQL.
    mv_sql = iv_sql.
  endmethod.


  method _CHANGE_STATE_IF_NECESSARY.

    DATA: lv_token_upper_case TYPE string.

    READ TABLE mt_tokens INDEX mv_current_token_index INTO lv_token_upper_case.
    lv_token_upper_case = zcl_zosql_utils=>to_upper_case( lv_token_upper_case ).

    CASE lv_token_upper_case.
      WHEN 'SELECT'.
        _set_state( c_state-select ).
      WHEN 'FROM'.
        _set_state( c_state-from ).
      WHEN 'IN'.
        IF _get_nth_previous_token_ucase( 1 ) = 'ENTRIES'
          AND _get_nth_previous_token_ucase( 2 ) = 'ALL'
          AND _get_nth_previous_token_ucase( 3 ) = 'FOR'.

          _set_state( c_state-for_all_entries ).
        ENDIF.
      WHEN 'TO'.
        IF _get_nth_previous_token_ucase( 1 ) = 'UP'
          AND _get_nth_next_token_ucase( 2 ) = 'ROWS'.

          _set_state( c_state-up_to_n_rows ).
        ENDIF.
      WHEN 'WHERE'.
        _set_state( c_state-where ).
      WHEN 'BY'.
        CASE _get_nth_previous_token_ucase( 1 ).
          WHEN 'GROUP'.
            _set_state( c_state-group_by ).
          WHEN 'ORDER'.
            _set_state( c_state-order_by ).
        ENDCASE.
    ENDCASE.
  endmethod.


  METHOD _CLEAR_ATTRIBUTES.
    CLEAR: select_field_list,
           is_distinct,
           is_select_single,
           number_of_rows_expr,
           from,
           for_all_entries_tabname,
           where,
           group_by,
           order_by.
  ENDMETHOD.


  method _CONDENSE_ATTRIBUTES.
    CONDENSE: select_field_list,
              is_distinct,
              is_select_single,
              number_of_rows_expr,
              from,
              for_all_entries_tabname,
              where,
              group_by,
              order_by.
  endmethod.


  method _DELETE_LAST_N_TOKENS_CUR_STR.
    CASE mv_current_state.
      WHEN c_state-select.
        zcl_zosql_utils=>delete_n_end_tokens( EXPORTING iv_n   = iv_n
                                              CHANGING  cv_sql = select_field_list ).
      WHEN c_state-from.
        zcl_zosql_utils=>delete_n_end_tokens( EXPORTING iv_n   = iv_n
                                              CHANGING  cv_sql = from ).
      WHEN c_state-where.
        zcl_zosql_utils=>delete_n_end_tokens( EXPORTING iv_n   = iv_n
                                              CHANGING  cv_sql = where ).
      WHEN c_state-group_by.
        zcl_zosql_utils=>delete_n_end_tokens( EXPORTING iv_n   = iv_n
                                              CHANGING  cv_sql = group_by ).
      WHEN c_state-order_by.
        zcl_zosql_utils=>delete_n_end_tokens( EXPORTING iv_n   = iv_n
                                              CHANGING  cv_sql = order_by ).
    ENDCASE.
  endmethod.


  method _GET_CURRENT_TOKEN_UCASE.
    READ TABLE mt_tokens INDEX mv_current_token_index INTO rv_current_token_ucase.
    rv_current_token_ucase = zcl_zosql_utils=>to_upper_case( rv_current_token_ucase ).
  endmethod.


  method _GET_NTH_NEXT_TOKEN_UCASE.

    DATA: lv_index_of_token   TYPE i.

    lv_index_of_token = mv_current_token_index + iv_n.
    READ TABLE mt_tokens INDEX lv_index_of_token INTO rv_nth_next_token_ucase.

    IF sy-subrc = 0.
      rv_nth_next_token_ucase = zcl_zosql_utils=>to_upper_case( rv_nth_next_token_ucase ).
    ENDIF.
  endmethod.


  method _GET_NTH_PREVIOUS_TOKEN_UCASE.

    DATA: lv_index_of_token   TYPE i.

    lv_index_of_token = mv_current_token_index - iv_n.
    IF lv_index_of_token > 0.
      READ TABLE mt_tokens INDEX lv_index_of_token INTO rv_nth_previous_token_ucase.
      rv_nth_previous_token_ucase = zcl_zosql_utils=>to_upper_case( rv_nth_previous_token_ucase ).
    ENDIF.
  endmethod.


  METHOD _PROCESS_TOKEN.

    DATA: lv_current_token_ucase TYPE string,
          lv_current_token       TYPE string.

    lv_current_token_ucase = _get_current_token_ucase( ).
    READ TABLE mt_tokens INDEX mv_current_token_index INTO lv_current_token.

    CASE mv_current_state.
      WHEN c_state-select.
        IF select_field_list IS INITIAL AND lv_current_token_ucase = 'DISTINCT'.
          is_distinct = abap_true.
        ELSEIF select_field_list IS INITIAL AND lv_current_token_ucase = 'SINGLE'.
          is_select_single = abap_true.
        ELSE.
          CONCATENATE select_field_list lv_current_token INTO select_field_list SEPARATED BY space.
        ENDIF.
      WHEN c_state-up_to_n_rows.
        IF number_of_rows_expr IS INITIAL.
          number_of_rows_expr = lv_current_token.
        ENDIF.
      WHEN c_state-from.
        CONCATENATE from lv_current_token INTO from SEPARATED BY space.
      WHEN c_state-for_all_entries.
        for_all_entries_tabname = lv_current_token.
      WHEN c_state-where.
        CONCATENATE where lv_current_token INTO where SEPARATED BY space.
      WHEN c_state-group_by.
        CONCATENATE group_by lv_current_token INTO group_by SEPARATED BY space.
      WHEN c_state-order_by.
        CONCATENATE order_by lv_current_token INTO order_by SEPARATED BY space.
    ENDCASE.
  ENDMETHOD.


  method _SET_STATE.

    IF iv_new_state = mv_current_state.
      RETURN.
    ENDIF.

    CASE iv_new_state.
      WHEN c_state-up_to_n_rows.
        _delete_last_n_tokens_cur_str( 1 ).
      WHEN c_state-for_all_entries.
        _delete_last_n_tokens_cur_str( 3 ).
      WHEN c_state-group_by.
        _delete_last_n_tokens_cur_str( 1 ).
      WHEN c_state-order_by.
        _delete_last_n_tokens_cur_str( 1 ).
    ENDCASE.

    mv_current_state = iv_new_state.
  endmethod.
ENDCLASS.
