class ZCL_ZOSQL_DB_LAYER_FAKE definition
  public
  inheriting from ZCL_ZOSQL_DB_LAYER_BASE
  create public .

public section.

  methods CREATE_EMPTY_TABLE_FOR_SELECT
    importing
      !IV_SELECT type CLIKE
    returning
      value(RD_DATA_SET_FOR_SELECT) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods CONSTRUCTOR
    importing
      !IO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT .

  methods ZIF_ZOSQL_DB_LAYER~COMMIT
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~DELETE_BY_ITAB
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~FETCH_NEXT_CURSOR
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~FETCH_NEXT_CURSOR_TO_ITAB
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~INSERT_BY_ITAB
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~MODIFY_BY_ITAB
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~OPEN_CURSOR
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~UPDATE_BY_ITAB
    redefinition .
protected section.

  methods DELETE_BY_SQL_PARTS
    redefinition .
  methods SELECT_BY_SQL_PARTS
    redefinition .
  methods UPDATE_BY_SQL_PARTS
    redefinition .
private section.

  types:
    BEGIN OF TY_CURSOR,
           cursor_number TYPE cursor,
           ref_to_result_of_select TYPE REF TO data,
           current_position        TYPE i,
           lines_count             TYPE i,
         END OF ty_cursor .

  data:
    mt_cursors TYPE STANDARD TABLE OF ty_cursor WITH KEY cursor_number .
  data MO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT .

  methods _EXECUTE_SQL
    importing
      !IV_WHERE type STRING
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE
      !IO_ITERATOR type ref to ZIF_ZOSQL_ITERATOR
      !IO_SQL_EXECUTOR_FOR_LINE type ref to ZIF_ZOSQL_SQL_EXEC_LINE
    raising
      ZCX_ZOSQL_ERROR .
  methods _PARSE_CONDITION_AS_PARAM
    importing
      !IV_FOR_ALL_ENTRIES_PARAM type STRING
      !IA_FOR_ALL_ENTRIES_TAB_LINE type ANY
    returning
      value(RS_PARAMETER) type ZOSQL_DB_LAYER_PARAM
    raising
      ZCX_ZOSQL_ERROR .
  methods _FIND_FOR_ALL_ENTRIES_CONDS
    importing
      !IV_WHERE type STRING
      !IV_FOR_ALL_ENTRIES_TABNAME type CLIKE
    exporting
      !ET_FOUND_PARAMETERS type STRING_TABLE .
  methods _ADD_FOR_ALL_ENTRIES_TO_PARAMS
    importing
      !IV_WHERE type STRING
      !IA_FOR_ALL_ENTRIES_TAB_LINE type ANY
      !IV_FOR_ALL_ENTRIES_TABNAME type CLIKE
    changing
      !CT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS
    raising
      ZCX_ZOSQL_ERROR .
  methods _SELECT
    importing
      !IV_SELECT type STRING
      !IV_FROM type STRING
      !IV_WHERE type STRING
      !IV_GROUP_BY type STRING
      !IV_ORDER_BY type STRING
      value(IV_DISTINCT) type ABAP_BOOL
      value(IV_NEW_SYNTAX) type ABAP_BOOL
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS
      !IT_FOR_ALL_ENTRIES_TABLE type ANY TABLE
      !IV_FOR_ALL_ENTRIES_TABNAME type CLIKE
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    exporting
      !ET_RESULT_TABLE type ANY TABLE
    raising
      ZCX_ZOSQL_ERROR .
  methods _SELECT_FOR_ALL_ENTRIES
    importing
      !IV_SELECT type STRING
      !IV_FROM type STRING
      !IV_WHERE type STRING
      !IV_GROUP_BY type STRING
      !IV_ORDER_BY type STRING
      value(IV_DISTINCT) type ABAP_BOOL
      value(IV_NEW_SYNTAX) type ABAP_BOOL
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS
      !IT_FOR_ALL_ENTRIES_TABLE type ANY TABLE
      !IV_FOR_ALL_ENTRIES_TABNAME type CLIKE
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    exporting
      !ET_RESULT_TABLE type ANY TABLE
    raising
      ZCX_ZOSQL_ERROR .
ENDCLASS.



CLASS ZCL_ZOSQL_DB_LAYER_FAKE IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    DATA: lo_factory TYPE REF TO zif_zosql_factory.

    super->constructor( ).

    IF io_zosql_test_environment IS BOUND.
      mo_zosql_test_environment = io_zosql_test_environment.
    ELSE.
      CREATE OBJECT lo_factory TYPE zcl_zosql_factory.
      mo_zosql_test_environment = lo_factory->get_test_environment( ).
    ENDIF.
  ENDMETHOD.


  METHOD CREATE_EMPTY_TABLE_FOR_SELECT.

    DATA: lv_select_field_list TYPE string,
          lv_from              TYPE string,
          lv_new_syntax        TYPE abap_bool,
          lo_from_iterator     TYPE REF TO zcl_zosql_from_iterator,
          lo_select            TYPE REF TO zcl_zosql_select_processor,
          lo_sql_parser        TYPE REF TO zcl_zosql_parser_recurs_desc.

    split_select_into_parts( EXPORTING iv_select            = iv_select
                             IMPORTING ev_from              = lv_from
                                       eo_sql_parser        = lo_sql_parser ).

    CREATE OBJECT lo_from_iterator
      EXPORTING
        io_zosql_test_environment = mo_zosql_test_environment.

    lo_from_iterator->init_by_from( lv_from ).

    CREATE OBJECT lo_select.

    lo_select->initialize_by_parsed_sql( io_sql_parser    = lo_sql_parser
                                         io_from_iterator = lo_from_iterator ).

    rd_data_set_for_select = lo_select->get_result_as_ref_to_data( ).
  ENDMETHOD.


  METHOD DELETE_BY_SQL_PARTS.

    DATA: lo_table_iterator           TYPE REF TO zcl_zosql_table_iterator,
          ld_ref_to_buffer_for_delete TYPE REF TO data,
          lo_sql_executor_for_line    TYPE REF TO zcl_zosql_sql_exec_delete.

    FIELD-SYMBOLS: <lt_buffer> TYPE STANDARD TABLE.

    CREATE OBJECT lo_table_iterator
      EXPORTING
        io_zosql_test_environment = mo_zosql_test_environment
        iv_table_name             = iv_table_name.

    CREATE OBJECT lo_sql_executor_for_line
      EXPORTING
        iv_table_name        = iv_table_name.

    _execute_sql( iv_where                 = iv_where
                  it_parameters            = it_parameters
                  iv_new_syntax            = iv_new_syntax
                  io_iterator              = lo_table_iterator
                  io_sql_executor_for_line = lo_sql_executor_for_line ).

    ld_ref_to_buffer_for_delete = lo_sql_executor_for_line->get_buffer_with_processed_recs( ).

    ASSIGN ld_ref_to_buffer_for_delete->* TO <lt_buffer>.
    IF <lt_buffer> IS NOT INITIAL.
      zif_zosql_db_layer~delete_by_itab( iv_table_name       = iv_table_name
                                         it_lines_for_delete = <lt_buffer> ).
    ENDIF.
  ENDMETHOD.


  METHOD select_by_sql_parts.

    DATA: ld_result_table     TYPE REF TO data,
          lv_up_to_n_rows     TYPE i,
          lv_delete_from_line TYPE i.

    FIELD-SYMBOLS: <lt_result_table>      TYPE STANDARD TABLE,
                   <ls_result_first_line> TYPE any.

    CLEAR: et_result_table, es_result_line, ev_subrc.

    IF iv_new_syntax <> abap_true.
      iv_new_syntax = detect_if_new_syntax_select( iv_select ).
    ENDIF.

    ld_result_table = create_dynamic_tab_for_result( iv_from              = iv_from
                                                     io_sql_parser        = io_sql_parser ).

    ASSIGN ld_result_table->* TO <lt_result_table>.

    IF it_for_all_entries_table IS NOT INITIAL.
      _select_for_all_entries( EXPORTING iv_select                  = iv_select
                                         iv_from                    = iv_from
                                         iv_where                   = iv_where
                                         iv_group_by                = iv_group_by
                                         iv_order_by                = iv_order_by
                                         iv_distinct                = iv_distinct
                                         iv_new_syntax              = iv_new_syntax
                                         it_parameters              = it_parameters
                                         it_for_all_entries_table   = it_for_all_entries_table
                                         iv_for_all_entries_tabname = iv_for_all_entries_tabname
                                         io_sql_parser              = io_sql_parser
                               IMPORTING et_result_table            = <lt_result_table> ).
    ELSE.
      _select( EXPORTING iv_select                  = iv_select
                         iv_from                    = iv_from
                         iv_where                   = iv_where
                         iv_group_by                = iv_group_by
                         iv_order_by                = iv_order_by
                         iv_distinct                = iv_distinct
                         iv_new_syntax              = iv_new_syntax
                         it_parameters              = it_parameters
                         it_for_all_entries_table   = it_for_all_entries_table
                         iv_for_all_entries_tabname = iv_for_all_entries_tabname
                         io_sql_parser              = io_sql_parser
               IMPORTING et_result_table            = <lt_result_table> ).
    ENDIF.

    IF iv_number_of_rows_expr IS NOT INITIAL.
      lv_up_to_n_rows = prepare_n_of_rows_for_select( it_parameters          = it_parameters
                                                      iv_number_of_rows_expr = iv_number_of_rows_expr ).
      lv_delete_from_line = lv_up_to_n_rows + 1.
      DELETE <lt_result_table> FROM lv_delete_from_line.
    ENDIF.

    return_result_of_select_toitab( EXPORTING it_result_table          = <lt_result_table>
                                              iv_do_into_corresponding = iv_do_into_corresponding
                                    IMPORTING et_result_table          = et_result_table
                                              es_result_line           = es_result_line
                                              ev_subrc                 = ev_subrc ).
  ENDMETHOD.


  METHOD update_by_sql_parts.

    DATA: lo_parameters               TYPE REF TO zcl_zosql_parameters,
          lo_set                      TYPE REF TO zcl_zosql_set_parser,
          lo_table_iterator           TYPE REF TO zcl_zosql_table_iterator,
          ld_ref_to_buffer_for_update TYPE REF TO data,
          lo_sql_executor_for_line    TYPE REF TO zcl_zosql_sql_exec_update.

    FIELD-SYMBOLS: <lt_buffer> TYPE STANDARD TABLE.

    CREATE OBJECT lo_parameters
      EXPORTING
        it_parameters = it_parameters.

    CREATE OBJECT lo_set
      EXPORTING
        iv_new_syntax = iv_new_syntax
        io_parameters = lo_parameters.

    lo_set->parse_set( iv_set_statement = iv_set_statement ).

    CREATE OBJECT lo_table_iterator
      EXPORTING
        io_zosql_test_environment = mo_zosql_test_environment
        iv_table_name             = iv_table_name.

    CREATE OBJECT lo_sql_executor_for_line
      EXPORTING
        iv_table_name        = iv_table_name
        io_set_in_update_sql = lo_set.

    _execute_sql( iv_where                 = iv_where
                  it_parameters            = it_parameters
                  iv_new_syntax            = iv_new_syntax
                  io_iterator              = lo_table_iterator
                  io_sql_executor_for_line = lo_sql_executor_for_line ).

    ld_ref_to_buffer_for_update = lo_sql_executor_for_line->get_buffer_with_processed_recs( ).
    ASSIGN ld_ref_to_buffer_for_update->* TO <lt_buffer>.
    IF <lt_buffer> IS NOT INITIAL.
      zif_zosql_db_layer~update_by_itab( iv_table_name       = iv_table_name
                                         it_lines_for_update = <lt_buffer> ).
    ENDIF.
  ENDMETHOD.


  method ZIF_ZOSQL_DB_LAYER~COMMIT.
* No commit needed for fake
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~DELETE_BY_ITAB.
    DATA: lv_table_name TYPE tabname16.

    lv_table_name = iv_table_name.
    mo_zosql_test_environment->delete_test_data_from_itab( it_lines_for_delete = it_lines_for_delete
                                                           iv_table_name       = lv_table_name ).
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~FETCH_NEXT_CURSOR.
    DATA: lv_start_line TYPE i,
          lv_end_line   TYPE i.

    FIELD-SYMBOLS: <ls_cursor>                  LIKE LINE OF mt_cursors,
                   <lt_result_of_select_all>    TYPE STANDARD TABLE,
                   <lt_result_of_select_return> TYPE STANDARD TABLE.

    READ TABLE mt_cursors WITH KEY cursor_number = iv_cursor ASSIGNING <ls_cursor>.
    IF sy-subrc <> 0.
      MESSAGE e072 WITH iv_cursor INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    ASSIGN <ls_cursor>-ref_to_result_of_select->* TO <lt_result_of_select_all>.

    CREATE DATA ed_result_as_table LIKE <lt_result_of_select_all>.
    ASSIGN ed_result_as_table->* TO <lt_result_of_select_return>.

    IF <ls_cursor>-current_position > <ls_cursor>-lines_count.
      ev_subrc = 4.
      RETURN.
    ENDIF.

    lv_start_line = <ls_cursor>-current_position.
    lv_end_line = lv_start_line + iv_package_size - 1.

    IF lv_end_line > <ls_cursor>-lines_count.
      lv_end_line = <ls_cursor>-lines_count.
    ENDIF.

    APPEND LINES OF <lt_result_of_select_all> FROM lv_start_line TO lv_end_line TO <lt_result_of_select_return>.

    <ls_cursor>-current_position = lv_end_line + 1.
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~FETCH_NEXT_CURSOR_TO_ITAB.
    DATA: ld_ref_to_result TYPE REF TO data.

    FIELD-SYMBOLS: <lt_result> TYPE STANDARD TABLE.

    REFRESH et_result_table.

    zif_zosql_db_layer~fetch_next_cursor( EXPORTING iv_cursor          = iv_cursor
                                                    iv_package_size    = iv_package_size
                                          IMPORTING ed_result_as_table = ld_ref_to_result
                                                    ev_subrc           = ev_subrc ).

    IF ev_subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN ld_ref_to_result->* TO <lt_result>.

    return_result_of_select_toitab( EXPORTING it_result_table          = <lt_result>
                                              iv_do_into_corresponding = iv_do_into_corresponding
                                    IMPORTING et_result_table          = et_result_table
                                              ev_subrc                 = ev_subrc ).
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~INSERT_BY_ITAB.
    DATA: lv_table_name TYPE tabname16.

    lv_table_name = iv_table_name.
    mo_zosql_test_environment->insert_test_data( it_table      = it_new_lines
                                                 iv_table_name = lv_table_name ).
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~MODIFY_BY_ITAB.
    zif_zosql_db_layer~insert_by_itab( iv_table_name = iv_table_name
                                       it_new_lines  = it_lines_for_modify ).
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~OPEN_CURSOR.
    FIELD-SYMBOLS: <ls_cursor>           LIKE LINE OF mt_cursors,
                   <lt_result_of_select> TYPE STANDARD TABLE.

    APPEND INITIAL LINE TO mt_cursors ASSIGNING <ls_cursor>.
    <ls_cursor>-cursor_number    = sy-tabix.
    <ls_cursor>-current_position = 1.

    zif_zosql_db_layer~select( EXPORTING iv_select                = iv_select
                                         it_parameters            = it_parameters
                                         it_for_all_entries_table = it_for_all_entries_table
                               IMPORTING ed_result_as_table       = <ls_cursor>-ref_to_result_of_select ).

    ASSIGN <ls_cursor>-ref_to_result_of_select->* TO <lt_result_of_select>.
    <ls_cursor>-lines_count = LINES( <lt_result_of_select> ).

    rv_cursor = <ls_cursor>-cursor_number.
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~UPDATE_BY_ITAB.
    zif_zosql_db_layer~insert_by_itab( iv_table_name = iv_table_name
                                       it_new_lines  = it_lines_for_update ).
  endmethod.


  method _ADD_FOR_ALL_ENTRIES_TO_PARAMS.

    DATA: lv_for_all_entries_tabname TYPE string,
          lv_for_all_entries_param   TYPE string,
          lt_for_all_entries_params  TYPE TABLE OF string,
          ls_param                   TYPE zosql_db_layer_param.

    IF iv_for_all_entries_tabname IS NOT INITIAL.
      lv_for_all_entries_tabname = iv_for_all_entries_tabname.
    ELSE.
      lv_for_all_entries_tabname = 'IT_FOR_ALL_ENTRIES_TABLE'.
    ENDIF.

    _find_for_all_entries_conds( EXPORTING iv_where                   = iv_where
                                           iv_for_all_entries_tabname = lv_for_all_entries_tabname
                                 IMPORTING et_found_parameters        = lt_for_all_entries_params ).

    LOOP AT lt_for_all_entries_params INTO lv_for_all_entries_param.

      ls_param = _parse_condition_as_param( iv_for_all_entries_param    = lv_for_all_entries_param
                                            ia_for_all_entries_tab_line = ia_for_all_entries_tab_line ).

      IF ls_param IS NOT INITIAL.
        APPEND ls_param TO ct_parameters.
      ENDIF.
    ENDLOOP.
  endmethod.


  method _EXECUTE_SQL.

    FIELD-SYMBOLS: <ls_result_first_line> TYPE any,
                   <ls_new_result_line>   TYPE any.

    DATA: lo_iter_pos        TYPE REF TO zcl_zosql_iterator_position,
          lo_where           TYPE REF TO zif_zosql_sqlcond_parser,
          lo_parameters      TYPE REF TO zcl_zosql_parameters,
          lv_not_end_of_data TYPE abap_bool.

    CREATE OBJECT lo_parameters
      EXPORTING
        it_parameters = it_parameters.

    CREATE OBJECT lo_where TYPE zcl_zosql_where_parser
      EXPORTING
        io_parameters = lo_parameters
        iv_new_syntax = iv_new_syntax.

    lo_where->parse_condition( iv_where ).

    lv_not_end_of_data = io_iterator->move_to_first( ).

    WHILE lv_not_end_of_data = abap_true.

      lo_iter_pos = io_iterator->get_iterator_position_object( ).

      IF lo_where->check_condition_for_cur_rec( lo_iter_pos ) = abap_true.
        io_sql_executor_for_line->execute_sql_for_line( lo_iter_pos ).
      ENDIF.

      lv_not_end_of_data = io_iterator->move_to_next( ).
    ENDWHILE.
  endmethod.


  method _FIND_FOR_ALL_ENTRIES_CONDS.

    DATA: lt_words            TYPE TABLE OF string,
          lv_search_substring TYPE string,
          lt_results          TYPE TABLE OF match_result,
          lv_for_all_entries_param TYPE string.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results.

    REFRESH et_found_parameters.

    SPLIT iv_where AT space INTO TABLE lt_words.
    CONCATENATE iv_for_all_entries_tabname '-' INTO lv_search_substring.
    CONDENSE lv_search_substring NO-GAPS.

    FIND ALL OCCURRENCES OF lv_search_substring IN TABLE lt_words IGNORING CASE RESULTS lt_results.
    LOOP AT lt_results ASSIGNING <ls_result>.
      READ TABLE lt_words INDEX <ls_result>-line INTO lv_for_all_entries_param.
      APPEND lv_for_all_entries_param TO et_found_parameters.
    ENDLOOP.
  endmethod.


  METHOD _parse_condition_as_param.

    DATA: lv_fieldname_for_all_entries TYPE fieldname.

    FIELD-SYMBOLS: <lv_value_for_all_entries> TYPE any.

    SPLIT iv_for_all_entries_param AT '-' INTO zcl_zosql_utils=>dummy lv_fieldname_for_all_entries.
    ASSIGN COMPONENT lv_fieldname_for_all_entries OF STRUCTURE ia_for_all_entries_tab_line TO <lv_value_for_all_entries>.
    IF sy-subrc <> 0.
      MESSAGE e054 WITH lv_fieldname_for_all_entries INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    rs_parameter-param_name_in_select   = iv_for_all_entries_param.
    rs_parameter-parameter_value_single = <lv_value_for_all_entries>.
  ENDMETHOD.


  METHOD _SELECT.

    DATA: lo_from_iterator         TYPE REF TO zcl_zosql_from_iterator,
          lo_select                TYPE REF TO zcl_zosql_select_processor,
          lo_group_by              TYPE REF TO zcl_zosql_groupby_parser,
          lo_sql_executor_for_line TYPE REF TO zif_zosql_sql_exec_line.

    REFRESH et_result_table.

    CREATE OBJECT lo_from_iterator
      EXPORTING
        io_zosql_test_environment = mo_zosql_test_environment.

    lo_from_iterator->init_by_from( iv_from ).

    CREATE OBJECT lo_select.

    lo_select->initialize_by_parsed_sql( io_sql_parser    = io_sql_parser
                                         io_from_iterator = lo_from_iterator ).

    CREATE OBJECT lo_sql_executor_for_line TYPE zcl_zosql_sql_exec_select
      EXPORTING
        io_select = lo_select.

    _execute_sql( iv_where                 = iv_where
                  it_parameters            = it_parameters
                  iv_new_syntax            = iv_new_syntax
                  io_iterator              = lo_from_iterator
                  io_sql_executor_for_line = lo_sql_executor_for_line ).

    IF iv_group_by IS NOT INITIAL.
      CREATE OBJECT lo_group_by.
      lo_group_by->parse_group_by( iv_group_by ).
      lo_select->apply_group_by( lo_group_by->mt_group_by_fields ).
    ENDIF.

    IF iv_distinct = abap_true.
      lo_select->apply_distinct( ).
    ENDIF.

    lo_select->get_result_move_corresponding( CHANGING ct_result_table = et_result_table ).
  ENDMETHOD.


  METHOD _SELECT_FOR_ALL_ENTRIES.

    DATA: lt_parameters   LIKE it_parameters,
          ld_result_table TYPE REF TO data.

    FIELD-SYMBOLS: <la_for_all_entries_line> TYPE any,
                   <lt_result_one_fae_line>  TYPE ANY TABLE.

    CREATE DATA ld_result_table LIKE et_result_table.
    ASSIGN ld_result_table->* TO <lt_result_one_fae_line>.

    LOOP AT it_for_all_entries_table ASSIGNING <la_for_all_entries_line>.

      lt_parameters = it_parameters.
      _add_for_all_entries_to_params( EXPORTING iv_where                    = iv_where
                                                ia_for_all_entries_tab_line = <la_for_all_entries_line>
                                                iv_for_all_entries_tabname  = iv_for_all_entries_tabname
                                      CHANGING  ct_parameters               = lt_parameters ).

      REFRESH <lt_result_one_fae_line>.
      select_by_sql_parts( EXPORTING iv_select              = iv_select
                                     iv_from                = iv_from
                                     iv_where               = iv_where
                                     iv_group_by            = iv_group_by
                                     iv_order_by            = iv_order_by
                                     iv_distinct            = iv_distinct
                                     iv_new_syntax          = iv_new_syntax
                                     it_parameters          = lt_parameters
                                     io_sql_parser          = io_sql_parser
                           IMPORTING et_result_table        = <lt_result_one_fae_line> ).

      INSERT LINES OF <lt_result_one_fae_line> INTO TABLE et_result_table.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
