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
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
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
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
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
      !EO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
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

  methods _DETECT_IF_NEW_SYNTAX_SET
    importing
      !IV_SET_STATEMENT type CLIKE
    returning
      value(RV_IS_NEW_SYNTAX) type ABAP_BOOL .
  methods _DETECT_IF_NEW_SYNTAX_WHERE
    importing
      !IV_WHERE type CLIKE
    returning
      value(RV_IS_NEW_SYNTAX) type ABAP_BOOL .
  methods _POP_SET_STATEMENT
    importing
      !IV_SQL_STATEMENT type CLIKE
    exporting
      !EV_SET_STATEMENT type CLIKE
      !EV_OTHER_SELECT type CLIKE .
  methods _POP_WHERE_UPDATE_DELETE
    importing
      !IV_SQL_STATEMENT type CLIKE
    returning
      value(RV_WHERE) type STRING .
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
  methods _RETURN_TABLE_TO_RESULT
    importing
      !IT_RESULT_TABLE_PREPARED type STANDARD TABLE
      value(IV_DO_INTO_CORRESPONDING) type ABAP_BOOL default ABAP_TRUE
    exporting
      !ET_RESULT_TABLE type ANY TABLE .
  methods _RETURN_LINE_TO_RESULT
    importing
      !IS_LINE_OF_RESULT_TABLE type ANY
      value(IV_DO_INTO_CORRESPONDING) type ABAP_BOOL default ABAP_TRUE
    exporting
      !ES_RESULT_LINE type ANY .
  methods _POP_DELETE_TABLE_NAME
    importing
      !IV_DELETE_STATEMENT type CLIKE
    exporting
      !EV_TABLE_NAME type CLIKE
      !EV_OTHER_SELECT type CLIKE
    raising
      ZCX_ZOSQL_ERROR .
  methods _POP_UPDATE_TABLE_NAME
    importing
      !IV_UPDATE_STATEMENT type CLIKE
    exporting
      !EV_TABLE_NAME type CLIKE
      !EV_OTHER_SELECT type CLIKE
    raising
      ZCX_ZOSQL_ERROR .
  methods _REPLACE_SEPARATORS_TO_SPACE
    importing
      !IV_SOURCE_TEXT type CLIKE
    returning
      value(RV_DESTINATION_TEXT) type STRING .
ENDCLASS.



CLASS ZCL_ZOSQL_DB_LAYER_BASE IMPLEMENTATION.


  METHOD create_dynamic_tab_for_result.
    DATA: lo_select    TYPE REF TO zcl_zosql_select_processor,
          lo_from_iter TYPE REF TO zcl_zosql_from_iterator,
          lo_iter_pos  TYPE REF TO zcl_zosql_iterator_position.

    FIELD-SYMBOLS: <lt_select_result> TYPE STANDARD TABLE.

    CREATE OBJECT lo_from_iter.
    lo_from_iter->init_by_sql_parser( io_sql_parser ).

    CREATE OBJECT lo_select.
    lo_select->initialize_by_parsed_sql( io_sql_parser    = io_sql_parser
                                         io_from_iterator = lo_from_iter ).

    rd_dynamic_table_select_result = lo_select->get_result_as_ref_to_data( ).

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


  METHOD split_select_into_parts.

    DATA: lv_select                  TYPE string,
          lv_single                  TYPE abap_bool,
          lo_sql_parser              TYPE REF TO zcl_zosql_parser_recurs_desc,
          lv_end_token_index         TYPE i,
          lv_select_field_list_start TYPE i,
          lv_select_field_list_end   TYPE i,
          lo_sql_parser_helper       TYPE REF TO zcl_zosql_parser_helper,
          ls_node_distinct           TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_single             TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          lt_nodes_select_field_list TYPE zcl_zosql_parser_recurs_desc=>ty_tree,
          ls_node_up_to_n_rows       TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_from               TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_for_all_entries    TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_where              TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_group_by           TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_order_by           TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    FIELD-SYMBOLS: <ls_node>                TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    CLEAR: ev_select_field_list, ev_from, ev_where, ev_group_by, ev_order_by, ev_distinct.

    lv_select = _replace_separators_to_space( iv_select ).

    CREATE OBJECT lo_sql_parser.
    lo_sql_parser->set_sql( lv_select ).
    lo_sql_parser->run_recursive_descent_parser( ).
    eo_sql_parser = lo_sql_parser.

    CREATE OBJECT lo_sql_parser_helper.
    lo_sql_parser_helper->get_key_nodes_of_sql_select( EXPORTING io_sql_parser                 = lo_sql_parser
                                                       IMPORTING es_node_distinct              = ls_node_distinct
                                                                 es_node_single                = ls_node_single
                                                                 et_nodes_of_select_field_list = lt_nodes_select_field_list
                                                                 es_node_up_to_n_rows          = ls_node_up_to_n_rows
                                                                 es_node_from                  = ls_node_from
                                                                 es_node_for_all_entries       = ls_node_for_all_entries
                                                                 es_node_where                 = ls_node_where
                                                                 es_node_group_by              = ls_node_group_by
                                                                 es_node_order_by              = ls_node_order_by
                                                                 ev_new_syntax                 = ev_new_syntax ).

    IF ls_node_distinct IS NOT INITIAL.
      ev_distinct = abap_true.
    ENDIF.

    IF ls_node_single IS NOT INITIAL.
      lv_single = abap_true.
    ENDIF.

    LOOP AT lt_nodes_select_field_list ASSIGNING <ls_node>.
      IF lv_select_field_list_start IS INITIAL.
        lv_select_field_list_start = <ls_node>-token_index.
      ELSEIF lv_select_field_list_start > <ls_node>-token_index.
        lv_select_field_list_start = <ls_node>-token_index.
      ENDIF.

      lv_end_token_index = lo_sql_parser->get_node_end_token_index( <ls_node>-id ).

      IF lv_end_token_index > lv_select_field_list_end.
        lv_select_field_list_end = lv_end_token_index.
      ENDIF.
    ENDLOOP.

    IF lv_select_field_list_start IS NOT INITIAL AND lv_select_field_list_end IS NOT INITIAL.
      ev_select_field_list =
        lo_sql_parser->get_sql_as_range_of_tokens( iv_start_token_index = lv_select_field_list_start
                                                   iv_end_token_index   = lv_select_field_list_end ).
    ENDIF.

    IF ls_node_up_to_n_rows IS NOT INITIAL.
      ev_number_of_rows_expr = lo_sql_parser->get_token_of_nth_child_node( iv_main_node_id = ls_node_up_to_n_rows-id
                                                                           iv_n            = 2 ).
    ENDIF.

    ev_from = lo_sql_parser->get_node_sql_without_self( ls_node_from-id ).

    IF ls_node_for_all_entries IS NOT INITIAL.
      ev_for_all_entries_tabname =
        lo_sql_parser->get_token_of_nth_child_node( iv_main_node_id = ls_node_for_all_entries-id
                                                    iv_n            = 4 ).
    ENDIF.

    IF ls_node_where IS NOT INITIAL.
      ev_where = lo_sql_parser->get_node_sql_without_self( ls_node_where-id ).
    ENDIF.

    IF ls_node_group_by IS NOT INITIAL.
      ev_group_by = lo_sql_parser->get_node_sql_start_at_offset( iv_node_id                 = ls_node_group_by-id
                                                                 iv_number_of_tokens_offset = 2 ).
    ENDIF.

    IF ls_node_order_by IS NOT INITIAL.
      ev_order_by = lo_sql_parser->get_node_sql_start_at_offset( iv_node_id                 = ls_node_order_by-id
                                                                 iv_number_of_tokens_offset = 2 ).
    ENDIF.

    IF lv_single = abap_true AND ev_number_of_rows_expr IS NOT INITIAL.
      MESSAGE e071 INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ELSEIF lv_single = abap_true.
      ev_number_of_rows_expr = '1'.
    ENDIF.

    "ev_new_syntax = detect_if_new_syntax_select( ev_select_field_list ).
  ENDMETHOD.


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


  METHOD zif_zosql_db_layer~select.
    DATA: lv_select_field_list TYPE string,
          lv_from              TYPE string,
          lv_new_syntax        TYPE abap_bool,
          lo_sql_parser        TYPE REF TO zcl_zosql_parser_recurs_desc.

    FIELD-SYMBOLS: <lt_select_result> TYPE STANDARD TABLE.

    me->split_select_into_parts( EXPORTING iv_select                  = iv_select
                                 IMPORTING ev_select_field_list       = lv_select_field_list
                                           ev_from                    = lv_from
                                           ev_new_syntax              = lv_new_syntax
                                           eo_sql_parser              = lo_sql_parser ).

    ed_result_as_table = create_dynamic_tab_for_result( lo_sql_parser ).
    ASSIGN ed_result_as_table->* TO <lt_select_result>.

    zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                = iv_select
                                                 it_parameters            = it_parameters
                                                 it_for_all_entries_table = it_for_all_entries_table
                                       IMPORTING et_result_table          = <lt_select_result>
                                                 ev_subrc                 = ev_subrc ).
  ENDMETHOD.


  method ZIF_ZOSQL_DB_LAYER~SELECT_TO_ITAB.
    DATA: lv_select_field_list       TYPE string,
          lv_from                    TYPE string,
          lv_for_all_entries_tabname TYPE string,
          lv_where                   TYPE string,
          lv_group_by                TYPE string,
          lv_order_by                TYPE string,
          lv_distinct                TYPE abap_bool,
          lv_new_syntax              TYPE abap_bool,
          lv_number_of_rows_expr     TYPE string,
          lo_sql_parser              TYPE REF TO zcl_zosql_parser_recurs_desc.

    me->split_select_into_parts( EXPORTING iv_select                  = iv_select
                                 IMPORTING ev_select_field_list       = lv_select_field_list
                                           ev_from                    = lv_from
                                           ev_for_all_entries_tabname = lv_for_all_entries_tabname
                                           ev_where                   = lv_where
                                           ev_group_by                = lv_group_by
                                           ev_order_by                = lv_order_by
                                           ev_distinct                = lv_distinct
                                           ev_new_syntax              = lv_new_syntax
                                           ev_number_of_rows_expr     = lv_number_of_rows_expr
                                           eo_sql_parser              = lo_sql_parser ).

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
                                   io_sql_parser              = lo_sql_parser
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


  METHOD _POP_DELETE_TABLE_NAME.

    CONSTANTS: c_delete TYPE string VALUE 'DELETE'.

    DATA: lv_sql    TYPE string,
          ls_result TYPE match_result.

    IF zcl_zosql_utils=>check_starts_with_token( iv_sql   = iv_delete_statement
                                                 iv_token = c_delete ) <> abap_true.

      MESSAGE e055 WITH c_delete INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    ev_other_select = zcl_zosql_utils=>delete_start_token_if_equals( iv_sql_source            = iv_delete_statement
                                                                     iv_start_token_to_delete = c_delete ).

    IF zcl_zosql_utils=>check_starts_with_token( iv_sql   = ev_other_select
                                                 iv_token = c_from ) <> abap_true.

      MESSAGE e064 INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    ev_other_select = zcl_zosql_utils=>delete_start_token_if_equals( iv_sql_source            = ev_other_select
                                                                     iv_start_token_to_delete = c_from ).

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


  method _POP_SET_STATEMENT.

    DATA: lv_sql    TYPE string,
          ls_result TYPE match_result.

    IF zcl_zosql_utils=>check_starts_with_token( iv_sql   = iv_sql_statement
                                                 iv_token = c_set ) = abap_true.

      lv_sql = zcl_zosql_utils=>delete_start_token_if_equals( iv_sql_source            = iv_sql_statement
                                                              iv_start_token_to_delete = c_set ).

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

    IF zcl_zosql_utils=>check_starts_with_token( iv_sql   = iv_update_statement
                                                 iv_token = c_update ) <> abap_true.

      MESSAGE e055 WITH c_update INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    ev_other_select = zcl_zosql_utils=>delete_start_token_if_equals( iv_sql_source            = iv_update_statement
                                                                     iv_start_token_to_delete = c_update ).

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


  method _POP_WHERE_UPDATE_DELETE.

    IF zcl_zosql_utils=>check_starts_with_token( iv_sql   = iv_sql_statement
                                                 iv_token = c_where ) = abap_true.

      rv_where = zcl_zosql_utils=>delete_start_token_if_equals( iv_sql_source            = iv_sql_statement
                                                                iv_start_token_to_delete = c_where ).
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
