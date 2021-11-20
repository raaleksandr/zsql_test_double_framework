class ZCL_ZOSQL_DB_LAYER_BASE definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_ZOSQL_DB_LAYER .
protected section.

  constants C_FROM type STRING value 'FROM' ##NO_TEXT.
  constants C_WHERE type STRING value 'WHERE' ##NO_TEXT.

  methods CREATE_DYNAMIC_TAB_FOR_RESULT
    importing
      !IV_SELECT_FIELD_LIST type CLIKE
      !IV_FROM type CLIKE
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE
    returning
      value(RD_DYNAMIC_TABLE_SELECT_RESULT) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods DELETE_BY_SQL_PARTS
  abstract
    importing
      !IV_TABLE_NAME type CLIKE
      !IV_WHERE type CLIKE
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS
    raising
      ZCX_ZOSQL_ERROR .
  methods RETURN_RESULT_OF_SELECT_TOITAB
    importing
      !IT_RESULT_TABLE type STANDARD TABLE
      value(IV_DO_INTO_CORRESPONDING) type ABAP_BOOL default ABAP_TRUE
    exporting
      !ET_RESULT_TABLE type ANY TABLE
      !ES_RESULT_LINE type ANY
      value(EV_SUBRC) type SYSUBRC .
  methods UPDATE_BY_SQL_PARTS
  abstract
    importing
      !IV_TABLE_NAME type CLIKE
      !IV_SET_STATEMENT type CLIKE
      !IV_WHERE type CLIKE
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS
    raising
      ZCX_ZOSQL_ERROR .
  methods DETECT_IF_NEW_SYNTAX_SELECT
    importing
      !IV_SELECT type STRING
    returning
      value(RV_IS_NEW_SYNTAX) type ABAP_BOOL .
  methods SELECT_BY_SQL_PARTS
  abstract
    importing
      !IV_SELECT type STRING default '*'
      !IV_FROM type STRING
      !IV_WHERE type STRING optional
      !IV_GROUP_BY type STRING optional
      !IV_ORDER_BY type STRING optional
      value(IV_DISTINCT) type ABAP_BOOL default ABAP_FALSE
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS optional
      !IT_FOR_ALL_ENTRIES_TABLE type ANY TABLE optional
      !IV_FOR_ALL_ENTRIES_TABNAME type CLIKE optional
      value(IV_DO_INTO_CORRESPONDING) type ABAP_BOOL default ABAP_TRUE
      !IV_NUMBER_OF_ROWS_EXPR type CLIKE optional
    exporting
      !ET_RESULT_TABLE type ANY TABLE
      !ES_RESULT_LINE type ANY
      !EV_SUBRC type SYSUBRC
    raising
      ZCX_ZOSQL_ERROR .
  methods SPLIT_SELECT_INTO_PARTS
    importing
      !IV_SELECT type CLIKE
    exporting
      !EV_SELECT_FIELD_LIST type CLIKE
      !EV_FROM type CLIKE
      !EV_FOR_ALL_ENTRIES_TABNAME type CLIKE
      !EV_WHERE type CLIKE
      !EV_GROUP_BY type CLIKE
      !EV_ORDER_BY type CLIKE
      value(EV_DISTINCT) type ABAP_BOOL
      value(EV_NEW_SYNTAX) type ABAP_BOOL
      !EV_NUMBER_OF_ROWS_EXPR type CLIKE
    raising
      ZCX_ZOSQL_ERROR .
  methods PREPARE_N_OF_ROWS_FOR_SELECT
    importing
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS
      !IV_NUMBER_OF_ROWS_EXPR type CLIKE
    returning
      value(RV_NUMBER_OF_ROWS_TO_SELECT) type I .
  methods IF_TABLE_CONSISTS_OF_STRUCTS
    importing
      !IT_TABLE type ANY TABLE
    returning
      value(RV_TABLE_CONSISTS_OF_STRUCTS) type ABAP_BOOL .
private section.

  constants C_SELECT type STRING value 'SELECT' ##NO_TEXT.
  constants C_DISTINCT type STRING value 'DISTINCT' ##NO_TEXT.
  constants C_FOR_ALL_ENTRIES_IN type STRING value 'FOR ALL ENTRIES IN' ##NO_TEXT.
  constants C_ORDER_BY type STRING value 'ORDER BY' ##NO_TEXT.
  constants C_GROUP_BY type STRING value 'GROUP BY' ##NO_TEXT.
  constants C_SET type STRING value 'SET' ##NO_TEXT.
  constants C_UP type STRING value 'UP' ##NO_TEXT.

  methods _POP_WHERE_UPDATE_DELETE
    importing
      !IV_SQL_STATEMENT type CLIKE
    returning
      value(RV_WHERE) type STRING .
  methods _POP_UP_TO_N_ROWS
    exporting
      !EV_NUMBER_OF_ROWS_EXPR type CLIKE
    changing
      !CV_SQL type CLIKE
    raising
      ZCX_ZOSQL_ERROR .
  methods _POP_SET_STATEMENT
    importing
      !IV_SQL_STATEMENT type CLIKE
    exporting
      !EV_SET_STATEMENT type CLIKE
      !EV_OTHER_SELECT type CLIKE .
  methods _POP_FOR_ALL_ENTRIES_TABNAME
    exporting
      !EV_FOR_ALL_ENTRIES_TABNAME type CLIKE
    changing
      !CV_SQL type CLIKE .
  methods _SPLIT_DELETE_INTO_PARTS
    importing
      !IV_DELETE_STATEMENT type CLIKE
    exporting
      !EV_TABLE_NAME type CLIKE
      !EV_WHERE type CLIKE
      !EV_NEW_SYNTAX type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _SPLIT_UPDATE_INTO_PARTS
    importing
      !IV_UPDATE_STATEMENT type CLIKE
    exporting
      !EV_TABLE_NAME type CLIKE
      !EV_SET_STATEMENT type CLIKE
      !EV_WHERE type CLIKE
      !EV_NEW_SYNTAX type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _FIND_WORD
    importing
      !IV_SQL type CLIKE
      !IV_WORD type CLIKE
    returning
      value(RS_RESULT) type MATCH_RESULT .
  methods _DETECT_IF_NEW_SYNTAX_WHERE
    importing
      !IV_WHERE type CLIKE
    returning
      value(RV_IS_NEW_SYNTAX) type ABAP_BOOL .
  methods _DETECT_IF_NEW_SYNTAX_SET
    importing
      !IV_SET_STATEMENT type CLIKE
    returning
      value(RV_IS_NEW_SYNTAX) type ABAP_BOOL .
  methods _GET_ORDER_BY
    importing
      !IV_SQL type CLIKE
    returning
      value(RV_ORDER_BY) type STRING .
  methods _RETURN_TABLE_TO_RESULT
    importing
      !IT_RESULT_TABLE_PREPARED type STANDARD TABLE
      value(IV_DO_INTO_CORRESPONDING) type ABAP_BOOL default ABAP_TRUE
    exporting
      !ET_RESULT_TABLE type ANY TABLE .
  methods _POP_GROUP_BY
    exporting
      !EV_GROUP_BY type CLIKE
    changing
      !CV_SQL type CLIKE .
  methods _RETURN_LINE_TO_RESULT
    importing
      !IS_LINE_OF_RESULT_TABLE type ANY
      value(IV_DO_INTO_CORRESPONDING) type ABAP_BOOL default ABAP_TRUE
    exporting
      !ES_RESULT_LINE type ANY .
  methods _POP_WHERE_SELECT
    exporting
      !EV_WHERE type CLIKE
    changing
      !CV_SQL type CLIKE .
  methods _POP_UPDATE_TABLE_NAME
    importing
      !IV_UPDATE_STATEMENT type CLIKE
    exporting
      !EV_TABLE_NAME type CLIKE
      !EV_OTHER_SELECT type CLIKE
    raising
      ZCX_ZOSQL_ERROR .
  methods _POP_DELETE_TABLE_NAME
    importing
      !IV_DELETE_STATEMENT type CLIKE
    exporting
      !EV_TABLE_NAME type CLIKE
      !EV_OTHER_SELECT type CLIKE
    raising
      ZCX_ZOSQL_ERROR .
  methods _POP_FIELD_LIST
    exporting
      !EV_SELECT_FIELD_LIST type CLIKE
      !EV_DISTINCT type ABAP_BOOL
      !EV_SINGLE type ABAP_BOOL
    changing
      !CV_SQL type CLIKE
    raising
      ZCX_ZOSQL_ERROR .
  methods _POP_FROM
    exporting
      !EV_FROM type CLIKE
    changing
      !CV_SQL type CLIKE
    raising
      ZCX_ZOSQL_ERROR .
  methods _REPLACE_SEPARATORS_TO_SPACE
    importing
      !IV_SOURCE_TEXT type CLIKE
    returning
      value(RV_DESTINATION_TEXT) type STRING .
ENDCLASS.



CLASS ZCL_ZOSQL_DB_LAYER_BASE IMPLEMENTATION.


  METHOD CREATE_DYNAMIC_TAB_FOR_RESULT.
    DATA: lo_select_parser TYPE REF TO zcl_zosql_select_parser,
          lo_from_iter     TYPE REF TO zcl_zosql_sqltables_iter,
          lo_iter_pos      TYPE REF TO zcl_zosql_sqltab_iterpos.

    FIELD-SYMBOLS: <lt_select_result> TYPE STANDARD TABLE.

    CREATE OBJECT lo_from_iter.
    lo_from_iter->init_by_from( iv_from ).

    CREATE OBJECT lo_select_parser.
    lo_select_parser->parse_field_list_in_select( iv_field_list_from_select = iv_select_field_list
                                                  io_sqltables_iterator     = lo_from_iter
                                                  iv_new_syntax             = iv_new_syntax ).

    rd_dynamic_table_select_result = lo_select_parser->get_result_as_ref_to_data( ).

    ASSIGN rd_dynamic_table_select_result->* TO <lt_select_result>.
    REFRESH <lt_select_result>.
  ENDMETHOD.


  METHOD DETECT_IF_NEW_SYNTAX_SELECT.

    FIND FIRST OCCURRENCE OF ',' IN iv_select.
    IF sy-subrc = 0.
      rv_is_new_syntax = abap_true.
    ENDIF.
  ENDMETHOD.


  method IF_TABLE_CONSISTS_OF_STRUCTS.

    DATA: ld_line TYPE REF TO data.

    FIELD-SYMBOLS: <ls_line> TYPE any.

    CREATE DATA ld_line LIKE LINE OF it_table.
    ASSIGN ld_line->* TO <ls_line>.

    IF zcl_zosql_utils=>is_structure( <ls_line> ) = abap_true.
      rv_table_consists_of_structs = abap_true.
    ENDIF.
  endmethod.


  METHOD prepare_n_of_rows_for_select.

    DATA: lo_parameters          TYPE REF TO zcl_zosql_parameters,
          ld_parameter_value_ref TYPE REF TO data,
          lv_number_of_rows_expr TYPE string.

    FIELD-SYMBOLS: <ls_parameter>       LIKE LINE OF it_parameters,
                   <lv_parameter_value> TYPE any.

    IF iv_number_of_rows_expr IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_parameters
      EXPORTING
        it_parameters = it_parameters.

    lv_number_of_rows_expr = iv_number_of_rows_expr.
    LOOP AT it_parameters ASSIGNING <ls_parameter>.
      FIND FIRST OCCURRENCE OF <ls_parameter>-param_name_in_select IN lv_number_of_rows_expr.
      IF sy-subrc = 0.
        ld_parameter_value_ref = lo_parameters->get_parameter_value_ref( <ls_parameter>-param_name_in_select ).
        ASSIGN ld_parameter_value_ref->* TO <lv_parameter_value>.
        REPLACE FIRST OCCURRENCE OF <ls_parameter>-param_name_in_select
          IN lv_number_of_rows_expr WITH <lv_parameter_value>.
        EXIT.
      ENDIF.
    ENDLOOP.

    rv_number_of_rows_to_select = lv_number_of_rows_expr.
  ENDMETHOD.


  method RETURN_RESULT_OF_SELECT_TOITAB.

    FIELD-SYMBOLS: <ls_result_first_line> TYPE any.

    LOOP AT it_result_table ASSIGNING <ls_result_first_line>.
      _return_line_to_result( EXPORTING is_line_of_result_table  = <ls_result_first_line>
                                        iv_do_into_corresponding = iv_do_into_corresponding
                              IMPORTING es_result_line           = es_result_line ).

      EXIT.
    ENDLOOP.

    ev_subrc = sy-subrc.

    _return_table_to_result( EXPORTING it_result_table_prepared = it_result_table
                                       iv_do_into_corresponding = iv_do_into_corresponding
                             IMPORTING et_result_table          = et_result_table ).
  endmethod.


  method SPLIT_SELECT_INTO_PARTS.

    DATA: lv_select  TYPE string,
          lv_single  TYPE abap_bool.

    CLEAR: ev_select_field_list, ev_from, ev_where, ev_group_by, ev_order_by, ev_distinct.

    lv_select = _replace_separators_to_space( iv_select ).

    _pop_field_list( IMPORTING ev_select_field_list = ev_select_field_list
                               ev_distinct          = ev_distinct
                               ev_single            = lv_single
                     CHANGING  cv_sql               = lv_select ).

    _pop_from( IMPORTING ev_from = ev_from
               CHANGING  cv_sql  = lv_select ).

    _pop_up_to_n_rows( IMPORTING ev_number_of_rows_expr = ev_number_of_rows_expr
                       CHANGING  cv_sql                 = lv_select ).

    _pop_for_all_entries_tabname( IMPORTING ev_for_all_entries_tabname = ev_for_all_entries_tabname
                                  CHANGING  cv_sql                     = lv_select ).

    _pop_where_select( IMPORTING ev_where = ev_where
                       CHANGING  cv_sql   = lv_select ).

    _pop_group_by( IMPORTING ev_group_by = ev_group_by
                   CHANGING  cv_sql      = lv_select ).

    ev_order_by = _get_order_by( lv_select ).

    IF lv_single = abap_true AND ev_number_of_rows_expr IS NOT INITIAL.
      MESSAGE e071 INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ELSEIF lv_single = abap_true.
      ev_number_of_rows_expr = '1'.
    ENDIF.

    ev_new_syntax = detect_if_new_syntax_select( ev_select_field_list ).
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~COMMIT.
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~DELETE.
    DATA: lv_table_name    TYPE string,
          lv_where         TYPE string,
          lv_new_syntax    TYPE abap_bool.

    _split_delete_into_parts( EXPORTING iv_delete_statement = iv_delete_statement
                              IMPORTING ev_table_name       = lv_table_name
                                        ev_where            = lv_where
                                        ev_new_syntax       = lv_new_syntax ).

    delete_by_sql_parts( iv_table_name = lv_table_name
                         iv_where      = lv_where
                         iv_new_syntax = lv_new_syntax
                         it_parameters = it_parameters ).
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~SELECT.
    DATA: lv_select_field_list       TYPE string,
          lv_from                    TYPE string,
          lv_new_syntax              TYPE abap_bool.

    FIELD-SYMBOLS: <lt_select_result> TYPE STANDARD TABLE.

    me->split_select_into_parts( EXPORTING iv_select                  = iv_select
                                 IMPORTING ev_select_field_list       = lv_select_field_list
                                           ev_from                    = lv_from
                                           ev_new_syntax              = lv_new_syntax ).

    ed_result_as_table = create_dynamic_tab_for_result( iv_select_field_list = lv_select_field_list
                                                        iv_from              = lv_from
                                                        iv_new_syntax        = lv_new_syntax ).
    ASSIGN ed_result_as_table->* TO <lt_select_result>.

    zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                = iv_select
                                                 it_parameters            = it_parameters
                                                 it_for_all_entries_table = it_for_all_entries_table
                                       IMPORTING et_result_table          = <lt_select_result>
                                                 ev_subrc                 = ev_subrc ).
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~SELECT_TO_ITAB.
    DATA: lv_select_field_list       TYPE string,
          lv_from                    TYPE string,
          lv_for_all_entries_tabname TYPE string,
          lv_where                   TYPE string,
          lv_group_by                TYPE string,
          lv_order_by                TYPE string,
          lv_distinct                TYPE abap_bool,
          lv_new_syntax              TYPE abap_bool,
          lv_number_of_rows_expr     TYPE string.

    me->split_select_into_parts( EXPORTING iv_select                  = iv_select
                                 IMPORTING ev_select_field_list       = lv_select_field_list
                                           ev_from                    = lv_from
                                           ev_for_all_entries_tabname = lv_for_all_entries_tabname
                                           ev_where                   = lv_where
                                           ev_group_by                = lv_group_by
                                           ev_order_by                = lv_order_by
                                           ev_distinct                = lv_distinct
                                           ev_new_syntax              = lv_new_syntax
                                           ev_number_of_rows_expr     = lv_number_of_rows_expr ).

    select_by_sql_parts( EXPORTING iv_select                  = lv_select_field_list
                                   iv_from                    = lv_from
                                   iv_where                   = lv_where
                                   iv_group_by                = lv_group_by
                                   iv_order_by                = lv_order_by
                                   iv_distinct                = lv_distinct
                                   iv_new_syntax              = lv_new_syntax
                                   it_parameters              = it_parameters
                                   it_for_all_entries_table   = it_for_all_entries_table
                                   iv_for_all_entries_tabname = lv_for_all_entries_tabname
                                   iv_do_into_corresponding   = iv_do_into_corresponding
                                   iv_number_of_rows_expr     = lv_number_of_rows_expr
                         IMPORTING et_result_table            = et_result_table
                                   es_result_line             = es_result_line
                                   ev_subrc                   = ev_subrc ).
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~UPDATE.
    DATA: lv_table_name    TYPE string,
          lv_set_statement TYPE string,
          lv_where         TYPE string,
          lv_new_syntax    TYPE abap_bool.

    _split_update_into_parts( EXPORTING iv_update_statement   = iv_update_statement
                              IMPORTING ev_table_name         = lv_table_name
                                        ev_set_statement      = lv_set_statement
                                        ev_where              = lv_where
                                        ev_new_syntax         = lv_new_syntax ).

    update_by_sql_parts( iv_table_name    = lv_table_name
                         iv_set_statement = lv_set_statement
                         iv_where         = lv_where
                         iv_new_syntax    = lv_new_syntax
                         it_parameters    = it_parameters ).
  endmethod.


  method _DETECT_IF_NEW_SYNTAX_SET.

    FIND FIRST OCCURRENCE OF '@' IN iv_set_statement.
    IF sy-subrc = 0.
      rv_is_new_syntax = abap_true.
    ENDIF.
  endmethod.


  method _DETECT_IF_NEW_SYNTAX_WHERE.
    rv_is_new_syntax = _detect_if_new_syntax_set( iv_where ).
  endmethod.


  method _FIND_WORD.

    DATA: lv_sql    TYPE string,
          lv_word   TYPE string,
          lt_result TYPE match_result_tab,
          lv_separate_word TYPE abap_bool,
          lv_offset_before TYPE i,
          lv_offset_after  TYPE i.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_Result.

    lv_sql = zcl_zosql_utils=>to_upper_case( iv_sql ).
    lv_word = zcl_zosql_utils=>to_upper_case( iv_word ).

    FIND ALL OCCURRENCES OF lv_word IN lv_sql RESULTS lt_result.

    LOOP AT lt_result ASSIGNING <ls_result>.

      lv_separate_word = abap_true.

      IF <ls_result>-offset > 0.
        lv_offset_before = <ls_result>-offset - 1.
        IF lv_sql+lv_offset_before(1) <> ` `.
          lv_separate_word = abap_false.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF <ls_result>-offset + <ls_result>-length < strlen( lv_sql ).
        lv_offset_after = <ls_result>-offset + <ls_result>-length.
        IF lv_sql+lv_offset_after(1) <> ` `.
          lv_separate_word = abap_false.
        ENDIF.
      ENDIF.

      IF lv_separate_word = abap_true.
        rs_result = <ls_result>.
        EXIT.
      ENDIF.
    ENDLOOP.
  endmethod.


  method _GET_ORDER_BY.

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = iv_sql
                                           iv_starts_with = c_order_by ) = abap_true.

      rv_order_by = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = iv_sql
                                                                  iv_start_word_to_delete = c_order_by ).
    ENDIF.
  endmethod.


  METHOD _POP_DELETE_TABLE_NAME.

    CONSTANTS: c_delete TYPE string VALUE 'DELETE'.

    DATA: lv_sql    TYPE string,
          ls_result TYPE match_result.

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = iv_delete_statement
                                           iv_starts_with = c_delete ) <> abap_true.

      MESSAGE e055 WITH c_delete INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    ev_other_select = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = iv_delete_statement
                                                                    iv_start_word_to_delete = c_delete ).

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = ev_other_select
                                           iv_starts_with = c_from ) <> abap_true.

      MESSAGE e064 INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    ev_other_select = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = ev_other_select
                                                                    iv_start_word_to_delete = c_from ).

    lv_sql = zcl_zosql_utils=>to_upper_case( ev_other_select ).
    FIND FIRST OCCURRENCE OF c_where IN lv_sql
      RESULTS ls_result.
    IF sy-subrc = 0.
      ev_table_name = ev_other_select(ls_result-offset).
      ev_other_select = ev_other_select+ls_result-offset.
    ELSE.
      CLEAR ev_table_name.
    ENDIF.
  ENDMETHOD.


  METHOD _POP_FIELD_LIST.

    CONSTANTS: lc_single  TYPE string VALUE 'SINGLE'.

    DATA: lv_sql    TYPE string,
          ls_result TYPE match_result.

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = cv_sql
                                           iv_starts_with = c_select ) <> abap_true.

      MESSAGE e055 WITH c_select INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    cv_sql = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = cv_sql
                                                           iv_start_word_to_delete = c_select ).

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = cv_sql
                                           iv_starts_with = c_distinct ) = abap_true.
      ev_distinct = abap_true.
      cv_sql = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = cv_sql
                                                             iv_start_word_to_delete = c_distinct ).
    ELSE.
      ev_distinct = abap_false.
    ENDIF.

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = cv_sql
                                           iv_starts_with = lc_single ) = abap_true.
      ev_single = abap_true.
      cv_sql = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = cv_sql
                                                             iv_start_word_to_delete = lc_single ).
    ENDIF.

    IF ev_single = abap_true AND ev_distinct = abap_true.
      MESSAGE e070 INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    lv_sql = zcl_zosql_utils=>to_upper_case( cv_sql ).
    FIND FIRST OCCURRENCE OF c_from IN lv_sql RESULTS ls_result.
    IF sy-subrc = 0.
      ev_select_field_list = cv_sql(ls_result-offset).
      cv_sql = cv_sql+ls_result-offset.
    ENDIF.
  ENDMETHOD.


  method _POP_FOR_ALL_ENTRIES_TABNAME.

    DATA: lv_sql    TYPE string,
          ls_result TYPE match_result.

    CLEAR ev_for_all_entries_tabname.

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = cv_sql
                                           iv_starts_with = c_for_all_entries_in ) <> abap_true.

      RETURN.
    ENDIF.

    cv_sql = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = cv_sql
                                                           iv_start_word_to_delete = c_for_all_entries_in ).

    lv_sql = zcl_zosql_utils=>to_upper_case( cv_sql ).
    FIND FIRST OCCURRENCE OF c_where IN lv_sql RESULTS ls_result.
    IF sy-subrc <> 0.
      FIND FIRST OCCURRENCE OF c_group_by IN lv_sql RESULTS ls_result.
      IF sy-subrc <> 0.
        FIND FIRST OCCURRENCE OF c_order_by IN lv_sql RESULTS ls_result.
      ENDIF.
    ENDIF.

    IF sy-subrc = 0.
      ev_for_all_entries_tabname = cv_sql(ls_result-offset).
      cv_sql                     = cv_sql+ls_result-offset.
    ELSE.
      ev_for_all_entries_tabname = cv_sql.
      CLEAR cv_sql.
    ENDIF.
  endmethod.


  METHOD _POP_FROM.

    DATA: lv_sql    TYPE string,
          ls_result TYPE match_result.

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = cv_sql
                                           iv_starts_with = c_from ) <> abap_true.

      MESSAGE e056 INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    cv_sql = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = cv_sql
                                                           iv_start_word_to_delete = c_from ).

    lv_sql = zcl_zosql_utils=>to_upper_case( cv_sql ).
    ls_result = _find_word( iv_sql  = lv_sql
                            iv_word = c_up ).
    IF ls_result IS INITIAL.
      FIND FIRST OCCURRENCE OF c_for_all_entries_in IN lv_sql RESULTS ls_result.
      IF sy-subrc <> 0.
        FIND FIRST OCCURRENCE OF c_where IN lv_sql RESULTS ls_result.
        IF sy-subrc <> 0.
          FIND FIRST OCCURRENCE OF c_group_by IN lv_sql RESULTS ls_result.
          IF sy-subrc <> 0.
            FIND FIRST OCCURRENCE OF c_order_by IN lv_sql RESULTS ls_result.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF ls_result IS NOT INITIAL.
      ev_from = cv_sql(ls_result-offset).
      cv_sql = cv_sql+ls_result-offset.
    ELSE.
      ev_from = cv_sql.
      CLEAR cv_sql.
    ENDIF.
  ENDMETHOD.


  method _POP_GROUP_BY.

    DATA: lv_sql    TYPE string,
          ls_result TYPE match_result.

    CLEAR ev_group_by.

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = cv_sql
                                           iv_starts_with = c_group_by ) <> abap_true.

      RETURN.
    ENDIF.

    cv_sql = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = cv_sql
                                                           iv_start_word_to_delete = c_group_by ).

    lv_sql = zcl_zosql_utils=>to_upper_case( cv_sql ).

    FIND FIRST OCCURRENCE OF c_order_by IN lv_sql RESULTS ls_result.
    IF sy-subrc = 0.
      ev_group_by = cv_sql(ls_result-offset).
      cv_sql      = cv_sql+ls_result-offset.
    ELSE.
      ev_group_by = cv_sql.
      CLEAR cv_sql.
    ENDIF.
  endmethod.


  method _POP_SET_STATEMENT.

    DATA: lv_sql    TYPE string,
          ls_result TYPE match_result.

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = iv_sql_statement
                                           iv_starts_with = c_set ) = abap_true.

      lv_sql = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = iv_sql_statement
                                                             iv_start_word_to_delete = c_set ).

      FIND FIRST OCCURRENCE OF c_where IN lv_sql RESULTS ls_result.
      IF sy-subrc = 0.
        ev_set_statement = lv_sql(ls_result-offset).
        lv_sql = lv_sql+ls_result-offset.
        ev_other_select = lv_sql.
      ELSE.
        CLEAR ev_other_select.
      ENDIF.
    ENDIF.
  endmethod.


  METHOD _POP_UPDATE_TABLE_NAME.

    CONSTANTS: c_update TYPE string VALUE 'UPDATE'.

    DATA: lv_sql    TYPE string,
          ls_result TYPE match_result.

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = iv_update_statement
                                           iv_starts_with = c_update ) <> abap_true.

      MESSAGE e055 WITH c_update INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    ev_other_select = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = iv_update_statement
                                                                    iv_start_word_to_delete = c_update ).

    lv_sql = zcl_zosql_utils=>to_upper_case( ev_other_select ).
    FIND FIRST OCCURRENCE OF c_set IN lv_sql
      RESULTS ls_result.
    IF sy-subrc <> 0.
      FIND FIRST OCCURRENCE OF c_where IN lv_sql
        RESULTS ls_result.
    ENDIF.
    IF sy-subrc = 0.
      ev_table_name = ev_other_select(ls_result-offset).
      ev_other_select = ev_other_select+ls_result-offset.
    ELSE.
      CLEAR ev_table_name.
    ENDIF.
  ENDMETHOD.


  method _POP_UP_TO_N_ROWS.

    CONSTANTS: lc_to   TYPE string VALUE 'TO',
               lc_rows TYPE string VALUE 'ROWS'.

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = cv_sql
                                           iv_starts_with = c_up ) <> abap_true.
      RETURN.
    ENDIF.

    cv_sql = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = cv_sql
                                                           iv_start_word_to_delete = c_up ).

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = cv_sql
                                           iv_starts_with = lc_to ) <> abap_true.
      MESSAGE e068 INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    cv_sql = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = cv_sql
                                                           iv_start_word_to_delete = lc_to ).

    ev_number_of_rows_expr = zcl_zosql_utils=>get_start_word( cv_sql ).
    cv_sql = zcl_zosql_utils=>delete_start_word( cv_sql ).

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = cv_sql
                                           iv_starts_with = lc_rows ) <> abap_true.

      MESSAGE e069 INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    cv_sql = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = cv_sql
                                                           iv_start_word_to_delete = lc_rows ).
  endmethod.


  method _POP_WHERE_SELECT.

    DATA: lv_sql    TYPE string,
          ls_result TYPE match_result.

    CLEAR ev_where.

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = cv_sql
                                           iv_starts_with = c_where ) <> abap_true.

      RETURN.
    ENDIF.

    cv_sql = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = cv_sql
                                                           iv_start_word_to_delete = c_where ).

    lv_sql = zcl_zosql_utils=>to_upper_case( cv_sql ).

    FIND FIRST OCCURRENCE OF c_group_by IN lv_sql RESULTS ls_result.
    IF sy-subrc <> 0.
      FIND FIRST OCCURRENCE OF c_order_by IN lv_sql RESULTS ls_result.
    ENDIF.

    IF sy-subrc = 0.
      ev_where = cv_sql(ls_result-offset).
      cv_sql   = cv_sql+ls_result-offset.
    ELSE.
      ev_where = cv_sql.
      CLEAR cv_sql.
    ENDIF.
  endmethod.


  method _POP_WHERE_UPDATE_DELETE.

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = iv_sql_statement
                                           iv_starts_with = c_where ) = abap_true.

      rv_where = zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = iv_sql_statement
                                                               iv_start_word_to_delete = c_where ).
    ENDIF.
  endmethod.


  METHOD _REPLACE_SEPARATORS_TO_SPACE.

    rv_destination_text = iv_source_text.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN rv_destination_text WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf(1) IN rv_destination_text WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf+1(1) IN rv_destination_text WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN rv_destination_text WITH space.
    CONDENSE rv_destination_text.
  ENDMETHOD.


  METHOD _RETURN_LINE_TO_RESULT.

    DATA: lv_field_number TYPE i.

    FIELD-SYMBOLS: <lv_field_src>  TYPE any,
                   <lv_field_dest> TYPE any.

    IF zcl_zosql_utils=>is_structure( es_result_line ) = abap_true.

      IF iv_do_into_corresponding = abap_true.
        MOVE-CORRESPONDING is_line_of_result_table TO es_result_line.
      ELSE.
        lv_field_number = 1.

        DO.

          UNASSIGN: <lv_field_src>, <lv_field_dest>.

          ASSIGN COMPONENT lv_field_number OF STRUCTURE is_line_of_result_table TO <lv_field_src>.
          ASSIGN COMPONENT lv_field_number OF STRUCTURE es_result_line TO <lv_field_dest>.

          IF <lv_field_src> IS ASSIGNED AND <lv_field_dest> IS ASSIGNED.
            <lv_field_dest> = <lv_field_src>.
          ELSE.
            EXIT.
          ENDIF.

          lv_field_number = lv_field_number + 1.
        ENDDO.
      ENDIF.
    ELSE.
      IF zcl_zosql_utils=>is_structure( is_line_of_result_table ) = abap_true.
        ASSIGN COMPONENT 1 OF STRUCTURE is_line_of_result_table TO <lv_field_src>.
        IF sy-subrc = 0.
          es_result_line = <lv_field_src>.
        ENDIF.
      ELSE.
        es_result_line = is_line_of_result_table.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method _RETURN_TABLE_TO_RESULT.

    DATA: ld_line_of_result_Table TYPE REF TO data.

    FIELD-SYMBOLS: <ls_line_of_result_table> TYPE any,
                   <ls_line_of_source>       TYPE any.

    IF if_table_consists_of_structs( et_result_table ) = abap_true
      AND iv_do_into_corresponding = abap_True.

      zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = it_result_table_prepared
                                                 IMPORTING et_table_dest = et_result_table ).
    ELSE.

      CREATE DATA ld_line_of_result_table LIKE LINE OF et_result_table.
      ASSIGN ld_line_of_result_table->* TO <ls_line_of_result_table>.

      LOOP AT it_result_table_prepared ASSIGNING <ls_line_of_source>.
        CLEAR <ls_line_of_result_table>.
        _return_line_to_result( EXPORTING is_line_of_result_table  = <ls_line_of_source>
                                          iv_do_into_corresponding = iv_do_into_corresponding
                                IMPORTING es_result_line           = <ls_line_of_result_table> ).

        INSERT <ls_line_of_result_table> INTO TABLE et_result_table.
      ENDLOOP.
    ENDIF.
  endmethod.


  method _SPLIT_DELETE_INTO_PARTS.

    DATA: lv_sql        TYPE string.

    _pop_delete_table_name( EXPORTING iv_delete_statement   = iv_delete_statement
                            IMPORTING ev_table_name         = ev_table_name
                                      ev_other_select       = lv_sql ).

    zcl_zosql_utils=>raise_if_transp_tab_not_exist( ev_table_name ).

    ev_where = _pop_where_update_delete( lv_sql ).

    ev_new_syntax = _detect_if_new_syntax_where( ev_where ).
  endmethod.


  method _SPLIT_UPDATE_INTO_PARTS.

    DATA: lv_sql        TYPE string.

    _pop_update_table_name( EXPORTING iv_update_statement   = iv_update_statement
                            IMPORTING ev_table_name         = ev_table_name
                                      ev_other_select       = lv_sql ).

    zcl_zosql_utils=>raise_if_transp_tab_not_exist( ev_table_name ).

    _pop_set_statement( EXPORTING iv_sql_statement = lv_sql
                        IMPORTING ev_set_statement = ev_set_statement
                                  ev_other_select  = lv_sql ).

    ev_where = _pop_where_update_delete( lv_sql ).

    IF _detect_if_new_syntax_set( ev_set_statement ) = abap_true
      OR _detect_if_new_syntax_where( ev_where ) = abap_true.

      ev_new_syntax = abap_true.
    ENDIF.
  endmethod.
ENDCLASS.
