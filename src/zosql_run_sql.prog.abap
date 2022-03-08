*&---------------------------------------------------------------------*
*& Report ZOSQL_QUERY_RUNNER
*&---------------------------------------------------------------------*
*& A tool that can run any Open SQL query and output results
*&---------------------------------------------------------------------*
REPORT zosql_query_runner MESSAGE-ID zosql_db_layer.

CLASS lcl_controller DEFINITION DEFERRED.

DATA: gv_ok_code_100 TYPE syucomm,
      go_controller  TYPE REF TO lcl_controller.

CLASS lcl_controller DEFINITION.
  PUBLIC SECTION.
    METHODS: pbo,
      pai,
      constructor.

  PRIVATE SECTION.

    METHODS: _is_displayed RETURNING VALUE(rv_displayed) TYPE abap_bool,
      _display,
      _execute_sql RAISING zcx_zosql_error,
      _show_result_in_alv IMPORTING id_result TYPE REF TO data,
      _prepare_alv_before_show IMPORTING it_table TYPE ANY TABLE,
      _show_error IMPORTING io_error TYPE REF TO cx_root,
      _check_auth_on_select_tabs IMPORTING iv_select TYPE clike RAISING zcx_zosql_error,
      _check_auth_one_table IMPORTING iv_tabname TYPE clike RAISING zcx_zosql_error.

    DATA: gv_is_displayed         TYPE abap_bool,
          go_separator_of_alv     TYPE REF TO cl_gui_docking_container,
          go_sql_editor_container TYPE REF TO cl_gui_custom_container,
          go_sql_editor           TYPE REF TO cl_gui_textedit,
          go_db_layer             TYPE REF TO zif_zosql_db_layer,
          go_splitter_for_alv     TYPE REF TO cl_gui_splitter_container,
          go_alv_container        TYPE REF TO cl_gui_container,
          go_alv                  TYPE REF TO cl_salv_table.
ENDCLASS.

CLASS lcl_controller IMPLEMENTATION.

  METHOD constructor.
    go_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).
  ENDMETHOD.

  METHOD pbo.
    SET PF-STATUS '100'.
    SET TITLEBAR '100'.

    IF _is_displayed( ) <> abap_true.
      _display( ).
    ENDIF.
  ENDMETHOD.

  METHOD pai.
    DATA: lv_ucomm_save TYPE syucomm,
          lo_error      TYPE REF TO zcx_zosql_error.

    lv_ucomm_save = gv_ok_code_100.
    CLEAR gv_ok_code_100.

    TRY.
        CASE lv_ucomm_save.
          WHEN 'BACK' OR 'EXIT' OR 'CANC'.
            SET SCREEN 0.
          WHEN 'EXECUTE'.
            _execute_sql( ).
        ENDCASE.
      CATCH zcx_zosql_error INTO lo_error.
        _show_error( lo_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD _is_displayed.
    rv_displayed = gv_is_displayed.
  ENDMETHOD.

  METHOD _display.
    CREATE OBJECT go_separator_of_alv
      EXPORTING
        side      = cl_gui_docking_container=>dock_at_bottom
        extension = 200.

    CREATE OBJECT go_splitter_for_alv
      EXPORTING
        parent  = go_separator_of_alv
        rows    = 1
        columns = 1.

    go_alv_container = go_splitter_for_alv->get_container( row = 1 column = 1 ).

    CREATE OBJECT go_sql_editor_container
      EXPORTING
        container_name = 'SQL_EDITOR'.

    CREATE OBJECT go_sql_editor
      EXPORTING
        parent = go_sql_editor_container.

    gv_is_displayed = abap_true.
  ENDMETHOD.

  METHOD _execute_sql.

    DATA: lv_sql_entered_by_user TYPE string,
          ld_sql_result          TYPE REF TO data.

    go_sql_editor->get_textstream( IMPORTING text    = lv_sql_entered_by_user
                                   EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    cl_gui_cfw=>flush( ).

    _check_auth_on_select_tabs( lv_sql_entered_by_user ).

    go_db_layer->select( EXPORTING iv_select          = lv_sql_entered_by_user
                         IMPORTING ed_result_as_table = ld_sql_result ).

    _show_result_in_alv( ld_sql_result ).
  ENDMETHOD.

  METHOD _show_result_in_alv.

    DATA: lo_exception TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lt_result> TYPE STANDARD TABLE.

    ASSIGN id_result->* TO <lt_result>.

    TRY.
        IF go_alv IS BOUND.
          go_alv->set_data( CHANGING t_table = <lt_result> ).

          _prepare_alv_before_show( <lt_result> ).

          zcl_zosql_utils=>salv_set_fieldnames_to_col_tit( io_salv  = go_alv
                                                           it_table = <lt_result> ).

          go_alv->refresh( ).
        ELSE.
          cl_salv_table=>factory( EXPORTING r_container  = go_alv_container
                                  IMPORTING r_salv_table = go_alv
                                  CHANGING  t_table      = <lt_result> ).

          _prepare_alv_before_show( <lt_result> ).

          go_alv->display( ).
        ENDIF.

      CATCH cx_root INTO lo_exception.
        zcl_zosql_utils=>dummy = lo_exception->get_text( ).
        MESSAGE zcl_zosql_utils=>dummy TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD _prepare_alv_before_show.

    DATA: lo_columns TYPE REF TO cl_salv_columns_table.

    zcl_zosql_utils=>salv_set_fieldnames_to_col_tit( io_salv  = go_alv
                                                     it_table = it_table ).

    lo_columns = go_alv->get_columns( ).
    lo_columns->set_optimize( abap_true ).
  ENDMETHOD.

  METHOD _show_error.
    TYPES: BEGIN OF ty_error,
             error_icon    TYPE icon_d,
             error_message TYPE text255,
           END OF ty_error.

    DATA: lt_error     TYPE TABLE OF ty_error,
          lo_exception TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_error> LIKE LINE OF lt_error.

    APPEND INITIAL LINE TO lt_error ASSIGNING <ls_error>.
    <ls_error>-error_icon    = icon_message_error.
    <ls_error>-error_message = io_error->get_text( ).

    TRY.
        IF go_alv IS BOUND.
          go_alv->set_data( CHANGING t_table = lt_error ).
          go_alv->refresh( ).
        ELSE.
          cl_salv_table=>factory( EXPORTING r_container  = go_alv_container
                                  IMPORTING r_salv_table = go_alv
                                  CHANGING  t_table      = lt_error ).
          go_alv->display( ).
        ENDIF.
      CATCH cx_root INTO lo_exception.
        zcl_zosql_utils=>dummy = lo_exception->get_text( ).
        MESSAGE zcl_zosql_utils=>dummy TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD _check_auth_on_select_tabs.

    DATA: lo_sql_parser      TYPE REF TO zcl_zosql_parser_recurs_desc,
          lo_parser_helper   TYPE REF TO zcl_zosql_parser_helper,
          lt_names_of_tables TYPE zcl_zosql_iterator_position=>ty_data_sets.

    FIELD-SYMBOLS: <ls_name_of_table> LIKE LINE OF lt_names_of_tables.

    CREATE OBJECT lo_sql_parser.
    lo_sql_parser->set_sql( iv_select ).
    lo_sql_parser->run_recursive_descent_parser( ).

    CREATE OBJECT lo_parser_helper
      EXPORTING
        io_sql_parser = lo_sql_parser.

    lt_names_of_tables = lo_parser_helper->get_list_of_select_from_tables( ).

    LOOP AT lt_names_of_tables ASSIGNING <ls_name_of_table>.
      _check_auth_one_table( <ls_name_of_table>-dataset_name ).
    ENDLOOP.
  ENDMETHOD.

  METHOD _check_auth_one_table.

    CONSTANTS: lc_view_action_show TYPE char1 VALUE 'S'.

    DATA: lv_view_name TYPE dd25v-viewname.

    lv_view_name = iv_tabname.
    CALL FUNCTION 'VIEW_AUTHORITY_CHECK'
      EXPORTING
        view_action                    = lc_view_action_show
        view_name                      = lv_view_name
      EXCEPTIONS
        invalid_action                 = 1
        no_authority                   = 2
        no_clientindependent_authority = 3
        table_not_found                = 4
        no_linedependent_authority     = 5
        OTHERS                         = 6.

    IF sy-subrc <> 0.
      MESSAGE e080 WITH lv_view_name INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  CREATE OBJECT go_controller.
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  go_controller->pbo( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       Process toolbar command
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  go_controller->pai( ).
ENDMODULE.
