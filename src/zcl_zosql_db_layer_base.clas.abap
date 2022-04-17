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
  methods PARSE_SQL
    importing
      !IV_SQL type STRING
    returning
      value(RO_SQL_PARSER) type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
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
  methods _NEW_SYNTAX_HOST_CHAR_EXISTS
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IV_TOP_NODE_ID_TO_START_SEARCH type I
    returning
      value(RV_EXISTS) type ABAP_BOOL .
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
ENDCLASS.



CLASS ZCL_ZOSQL_DB_LAYER_BASE IMPLEMENTATION.


  METHOD create_dynamic_tab_for_result.
    DATA: lo_select           TYPE REF TO zcl_zosql_select_processor,
          lo_from_iter        TYPE REF TO zcl_zosql_from_iterator,
          lo_iter_pos         TYPE REF TO zcl_zosql_iterator_position.

    FIELD-SYMBOLS: <lt_select_result> TYPE STANDARD TABLE.

    CREATE OBJECT lo_from_iter
      EXPORTING
        io_sql_parser = io_sql_parser.

    CREATE OBJECT lo_select
      EXPORTING
        io_sql_parser    = io_sql_parser
        io_from_iterator = lo_from_iter.

    rd_dynamic_table_select_result = lo_select->get_result_as_ref_to_data( ).

    ASSIGN rd_dynamic_table_select_result->* TO <lt_select_result>.
    REFRESH <lt_select_result>.
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


  method PARSE_SQL.
    CREATE OBJECT ro_sql_parser.
    ro_sql_parser->set_sql( iv_sql ).
    ro_sql_parser->run_recursive_descent_parser( ).
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


  method ZIF_ZOSQL_DB_LAYER~COMMIT.
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~DELETE.

  endmethod.


  method ZIF_ZOSQL_DB_LAYER~DELETE_BY_ITAB.
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~FETCH_NEXT_CURSOR.
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~FETCH_NEXT_CURSOR_TO_ITAB.
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~INSERT_BY_ITAB.
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~MODIFY_BY_ITAB.
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~OPEN_CURSOR.
  endmethod.


  METHOD zif_zosql_db_layer~select.
    DATA: lv_select_field_list TYPE string,
          lv_from              TYPE string,
          lv_new_syntax        TYPE abap_bool,
          lo_sql_parser        TYPE REF TO zcl_zosql_parser_recurs_desc.

    FIELD-SYMBOLS: <lt_select_result> TYPE STANDARD TABLE.

    CREATE OBJECT lo_sql_parser.
    lo_sql_parser->set_sql( iv_select ).
    lo_sql_parser->run_recursive_descent_parser( ).

    ed_result_as_table = create_dynamic_tab_for_result( lo_sql_parser ).
    ASSIGN ed_result_as_table->* TO <lt_select_result>.

    zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                = iv_select
                                                 it_parameters            = it_parameters
                                                 it_for_all_entries_table = it_for_all_entries_table
                                       IMPORTING et_result_table          = <lt_select_result>
                                                 ev_subrc                 = ev_subrc ).
  ENDMETHOD.


  method ZIF_ZOSQL_DB_LAYER~SELECT_TO_ITAB.

  endmethod.


  method ZIF_ZOSQL_DB_LAYER~UPDATE.

  endmethod.


  method ZIF_ZOSQL_DB_LAYER~UPDATE_BY_ITAB.
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
ENDCLASS.
