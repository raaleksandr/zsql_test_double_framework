*&---------------------------------------------------------------------*
*& Report ZOSQL_DB_EXAMPLE_DBCHANGE
*&---------------------------------------------------------------------*
*& Example program updating database
*& Demonstrates unit test of database updates with
*& Z-SQL Test Double Framework lib
*&
*& This code was downloaded from URL
*& https://github.com/raaleksandr/zsql_test_double_framework
*&
*& Full documentation is on Github
*&
*& If you find a bug please open Issue on github
*& https://github.com/raaleksandr/zsql_test_double_framework/issues/new
*&---------------------------------------------------------------------*

REPORT zosql_db_example_dbchange MESSAGE-ID zosql_db_layer.

CLASS lcl_controller_and_view DEFINITION DEFERRED.

DATA: go_app     TYPE REF TO lcl_controller_and_view,
      gv_ok_code TYPE syucomm.

CLASS lcl_model DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_db_layer TYPE REF TO zif_zosql_db_layer OPTIONAL,
      read_all_carriers RETURNING VALUE(rt_carriers) TYPE scarr_tab
                        RAISING   zcx_zosql_error,
      insert_new_carrier IMPORTING is_new_scarr TYPE scarr
                         RAISING   zcx_zosql_error,
      update_carrier IMPORTING is_changed_scarr TYPE scarr
                     RAISING   zcx_zosql_error,
      mass_delete_of_carriers IMPORTING it_ids_of_carriers_to_delete TYPE zosql_db_layer_carr_id_tab
                              RAISING   zcx_zosql_error.

  PRIVATE SECTION.
    DATA: mo_db_layer TYPE REF TO zif_zosql_db_layer.

    METHODS: _check_record IMPORTING is_scarr_to_check TYPE scarr
                           RAISING   zcx_zosql_error.
ENDCLASS.

CLASS lcl_controller_and_view DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
      display_alv,
      pbo,
      user_command IMPORTING VALUE(iv_command) TYPE syucomm.

  PRIVATE SECTION.

    CONSTANTS: c_function_insert TYPE syucomm VALUE 'INSERT',
               c_function_edit   TYPE syucomm VALUE 'EDIT',
               c_function_delete TYPE syucomm VALUE 'DELETE'.

    DATA: mo_model     TYPE REF TO lcl_model,
          mo_container TYPE REF TO cl_gui_custom_container,
          mo_alv       TYPE REF TO cl_salv_table,
          mt_alv_data  TYPE TABLE OF scarr.

    METHODS: _alv_on_user_command FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function,
      _insert_new_record,
      _edit_record,
      _delete_records,
      _display_edit_dialog IMPORTING is_scarr          TYPE scarr
                           EXPORTING es_scarr          TYPE scarr
                                     VALUE(ev_user_ok) TYPE abap_bool,
      _display_error IMPORTING io_exception TYPE REF TO zcx_zosql_error,
      _refresh_alv RAISING zcx_zosql_error.
ENDCLASS.

CLASS lcl_model IMPLEMENTATION.

  METHOD constructor.
    IF io_db_layer IS BOUND.
      mo_db_layer = io_db_layer.
    ELSE.
      mo_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).
    ENDIF.
  ENDMETHOD.

  METHOD read_all_carriers.
    mo_db_layer->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM SCARR ORDER BY PRIMARY KEY'
                                 IMPORTING et_result_table = rt_carriers ).
  ENDMETHOD.

  METHOD insert_new_carrier.

    DATA: lt_all_carriers      TYPE TABLE OF scarr,
          ls_param_for_select  TYPE zosql_db_layer_param,
          lt_params_for_select TYPE TABLE OF zosql_db_layer_param,
          lv_subrc             TYPE i,
          lt_new_lines         TYPE TABLE OF scarr.

    lt_all_carriers = read_all_carriers( ).

    READ TABLE lt_all_carriers WITH KEY carrid = is_new_scarr-carrid TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      MESSAGE e701 WITH is_new_scarr-carrid INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    _check_record( is_new_scarr ).

    APPEND is_new_scarr TO lt_new_lines.
    mo_db_layer->insert_by_itab( lt_new_lines ).
    mo_db_layer->commit( ).
  ENDMETHOD.

  METHOD update_carrier.

    DATA: lt_all_carriers     TYPE TABLE OF scarr,
          lt_lines_for_update TYPE TABLE OF scarr.

    lt_all_carriers = read_all_carriers( ).

    READ TABLE lt_all_carriers WITH KEY carrid = is_changed_scarr-carrid TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MESSAGE e705 WITH is_changed_scarr-carrid INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    _check_record( is_changed_scarr ).

    APPEND is_changed_scarr TO lt_lines_for_update.
    mo_db_layer->update_by_itab( lt_lines_for_update ).

    mo_db_layer->commit( ).
  ENDMETHOD.

  METHOD mass_delete_of_carriers.

    DATA: lv_select                    TYPE string,
          lv_subrc                     TYPE sysubrc,
          lt_data_that_will_be_deleted TYPE SORTED TABLE OF scarr
                                            WITH UNIQUE KEY carrid.

    FIELD-SYMBOLS: <ls_id_of_carrier> LIKE LINE OF it_ids_of_carriers_to_delete.

    IF it_ids_of_carriers_to_delete IS INITIAL.
      RETURN.
    ENDIF.

    CONCATENATE 'SELECT * FROM SCARR'
      'FOR ALL ENTRIES IN it_ids_of_carriers_to_delete'
      'WHERE CARRID = it_ids_of_carriers_to_delete-CARRID'
      INTO lv_select SEPARATED BY space.
    mo_db_layer->select_to_itab( EXPORTING iv_select                = lv_select
                                           it_for_all_entries_table = it_ids_of_carriers_to_delete
                                 IMPORTING et_result_table          = lt_data_that_will_be_deleted ).

    LOOP AT it_ids_of_carriers_to_delete ASSIGNING <ls_id_of_carrier>.
      READ TABLE lt_data_that_will_be_deleted WITH TABLE KEY carrid = <ls_id_of_carrier>-carrid
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        MESSAGE e708 WITH <ls_id_of_carrier>-carrid INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.
    ENDLOOP.

    CONCATENATE 'SELECT * FROM spfli'
      'FOR ALL ENTRIES IN it_ids_of_carriers_to_delete'
      'WHERE CARRID = it_ids_of_carriers_to_delete-carrid'
      INTO lv_select SEPARATED BY space.
    mo_db_layer->select( EXPORTING iv_select                = lv_select
                                   it_for_all_entries_table = it_ids_of_carriers_to_delete
                         IMPORTING ev_subrc                 = lv_subrc ).

    IF lv_subrc = 0.
      MESSAGE e706 INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    CONCATENATE 'SELECT * FROM sbook'
      'FOR ALL ENTRIES IN it_ids_of_carriers_to_delete'
      'WHERE CARRID = it_ids_of_carriers_to_delete-carrid'
      INTO lv_select SEPARATED BY space.
    mo_db_layer->select( EXPORTING iv_select                = lv_select
                                   it_for_all_entries_table = it_ids_of_carriers_to_delete
                         IMPORTING ev_subrc                 = lv_subrc ).

    IF lv_subrc = 0.
      MESSAGE e707 INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    mo_db_layer->delete_by_itab( iv_table_name       = 'SCARR'
                                 it_lines_for_delete = it_ids_of_carriers_to_delete ).

    mo_db_layer->commit( ).
  ENDMETHOD.

  METHOD _check_record.

    DATA: ls_param_for_select  TYPE zosql_db_layer_param,
          lt_params_for_select TYPE TABLE OF zosql_db_layer_param,
          lv_subrc             TYPE i.

    IF is_scarr_to_check-carrname IS INITIAL.
      MESSAGE e702 WITH 'Name of Carrier' INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    IF is_scarr_to_check-currcode IS NOT INITIAL.

      ls_param_for_select-param_name_in_select   = ':CURRCODE'.
      ls_param_for_select-parameter_value_single = is_scarr_to_check-currcode.
      APPEND ls_param_for_select TO lt_params_for_select.

      mo_db_layer->select( EXPORTING iv_select     = 'SELECT * FROM SCURX WHERE CURRKEY = :CURRCODE'
                                     it_parameters = lt_params_for_select
                           IMPORTING ev_subrc      = lv_subrc ).

      IF lv_subrc <> 0.
        MESSAGE e703 WITH is_scarr_to_check-currcode INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.
    ENDIF.

    IF is_scarr_to_check-url NP 'HTTP*'
      AND is_scarr_to_check-url NP 'HTTPS*'
      AND is_scarr_to_check-url NP 'http*'
      AND is_scarr_to_check-url NP 'https*'
      AND is_scarr_to_check-url NP '*WWW*'
      AND is_scarr_to_check-url NP '*www*'
      AND is_scarr_to_check-url IS NOT INITIAL.

      MESSAGE e704 WITH is_scarr_to_check-url INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_controller_and_view IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT mo_model.
  ENDMETHOD.

  METHOD display_alv.
    DATA: lo_exception  TYPE REF TO cx_root,
          lo_columns    TYPE REF TO cl_salv_columns_table,
          lo_functions  TYPE REF TO cl_salv_functions_list,
          lv_icon       TYPE string,
          lo_event      TYPE REF TO cl_salv_events_table,
          lo_selections TYPE REF TO cl_salv_selections.

    IF mo_alv IS BOUND.
      RETURN.
    ENDIF.

    IF mo_container IS BOUND.
      mo_container->free( ).

      CREATE OBJECT mo_container
        EXPORTING
          container_name = 'ALV'.
    ENDIF.

    TRY.
        mt_alv_data = mo_model->read_all_carriers( ).

        cl_salv_table=>factory( EXPORTING r_container  = mo_container
                                IMPORTING r_salv_table = mo_alv
                                CHANGING  t_table      = mt_alv_data ).

        lo_columns = mo_alv->get_columns( ).
        lo_columns->set_optimize( abap_true ).
        lo_columns->get_column( 'MANDT' )->set_visible( abap_false ).

        lo_functions = mo_alv->get_functions( ).

        lv_icon = icon_create.
        lo_functions->add_function( name     = c_function_insert
                                    icon     = lv_icon
                                    text     = 'New'
                                    tooltip  = 'Insert new carrier'
                                    position = if_salv_c_function_position=>right_of_salv_functions ).

        lv_icon = icon_change.
        lo_functions->add_function( name     = c_function_edit
                                    icon     = lv_icon
                                    text     = 'Edit'
                                    tooltip  = 'Edit carrier'
                                    position = if_salv_c_function_position=>right_of_salv_functions ).

        lv_icon = icon_delete.
        lo_functions->add_function( name     = c_function_delete
                                    icon     = lv_icon
                                    text     = 'Delete'
                                    tooltip  = 'Delete selected carriers'
                                    position = if_salv_c_function_position=>right_of_salv_functions ).

        lo_selections = mo_alv->get_selections( ).
        lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ). "cl_salv_selections=>multiple ).

        lo_event = mo_alv->get_event( ).
        SET HANDLER _alv_on_user_command FOR lo_event.

        mo_alv->display( ).
      CATCH cx_root INTO lo_exception.
        zcl_zosql_utils=>dummy = lo_exception->get_text( ).
        MESSAGE zcl_zosql_utils=>dummy TYPE 'I'.
    ENDTRY.
  ENDMETHOD.

  METHOD pbo.
    SET PF-STATUS '100'.
    SET TITLEBAR '100'.

    display_alv( ).
  ENDMETHOD.

  METHOD user_command.
    CASE iv_command.
      WHEN 'BACK' OR 'EXIT' OR 'CANC'.
        SET SCREEN 0.
      WHEN c_function_insert.
        _insert_new_record( ).
      WHEN c_function_edit.
        _edit_record( ).
      WHEN c_function_delete.
        _delete_records( ).
    ENDCASE.
  ENDMETHOD.

  METHOD _alv_on_user_command.
    user_command( e_salv_function ).
  ENDMETHOD.

  METHOD _insert_new_record.

    DATA: ls_new_record        TYPE scarr,
          lo_exception         TYPE REF TO zcx_zosql_error,
          lv_user_responded_ok TYPE abap_bool.

    CALL METHOD _display_edit_dialog
      EXPORTING
        is_scarr   = ls_new_record
      IMPORTING
        es_scarr   = ls_new_record
        ev_user_ok = lv_user_responded_ok.

    IF lv_user_responded_ok <> abap_true.
      RETURN.
    ENDIF.

    TRY.
        mo_model->insert_new_carrier( ls_new_record ).
        _refresh_alv( ).
        MESSAGE 'Data successfully inserted' TYPE 'S'.
      CATCH zcx_zosql_error INTO lo_exception.
        _display_error( lo_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD _display_error.
    MESSAGE ID io_exception->t100_message-msgid
            TYPE 'I'
            NUMBER io_exception->t100_message-msgno
            WITH io_exception->t100_message-msgv1
                 io_exception->t100_message-msgv2
                 io_exception->t100_message-msgv3
                 io_exception->t100_message-msgv4
           DISPLAY LIKE 'E'.
  ENDMETHOD.

  METHOD _refresh_alv.
    mt_alv_data = mo_model->read_all_carriers( ).
    mo_alv->refresh( ).
  ENDMETHOD.

  METHOD _edit_record.

    DATA: lo_selections         TYPE REF TO cl_salv_selections,
          lt_selected_rows      TYPE salv_t_row,
          lv_selected_row_index LIKE LINE OF lt_selected_rows,
          ls_updated_row        LIKE LINE OF mt_alv_data,
          lo_exception          TYPE REF TO zcx_zosql_error,
          lv_user_responded_ok  TYPE abap_bool.

    FIELD-SYMBOLS: <ls_alv_row> LIKE LINE OF mt_alv_data.

    lo_selections = mo_alv->get_selections( ).
    lt_selected_rows = lo_selections->get_selected_rows( ).

    IF lines( lt_selected_rows ) <> 1.
      MESSAGE 'Choose just one line' TYPE 'I'.
      RETURN.
    ENDIF.

    READ TABLE lt_selected_rows INDEX 1 INTO lv_selected_row_index.
    READ TABLE mt_alv_data INDEX lv_selected_row_index INTO ls_updated_row.

    CALL METHOD _display_edit_dialog
      EXPORTING
        is_scarr   = ls_updated_row
      IMPORTING
        es_scarr   = ls_updated_row
        ev_user_ok = lv_user_responded_ok.

    IF lv_user_responded_ok <> abap_true.
      RETURN.
    ENDIF.

    TRY.
        mo_model->update_carrier( ls_updated_row ).
        _refresh_alv( ).
        MESSAGE 'Data successfully updated' TYPE 'S'.
      CATCH zcx_zosql_error INTO lo_exception.
        _display_error( lo_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD _delete_records.
    DATA: lo_selections          TYPE REF TO cl_salv_selections,
          lt_selected_rows       TYPE salv_t_row,
          lv_prompt              TYPE string,
          lv_answer_code_of_user TYPE char1,
          lt_ids_to_delete       TYPE TABLE OF zosql_db_layer_carr_id,
          lv_index_of_row        TYPE i,
          lo_exception           TYPE REF TO zcx_zosql_error.

    FIELD-SYMBOLS: <ls_alv_line> LIKE LINE OF mt_alv_data.

    lo_selections = mo_alv->get_selections( ).
    lt_selected_rows = lo_selections->get_selected_rows( ).

    IF lt_selected_rows IS INITIAL.
      MESSAGE 'Please select row(s) to delete' TYPE 'I'.
      RETURN.
    ENDIF.

    lv_prompt = |{ lines( lt_selected_rows ) } rows will be deleted. Continue?|.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question  = lv_prompt
      IMPORTING
        answer         = lv_answer_code_of_user
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0 OR lv_answer_code_of_user <> '1'.
      RETURN.
    ENDIF.

    LOOP AT lt_selected_rows INTO lv_index_of_row.
      READ TABLE mt_alv_data INDEX lv_index_of_row ASSIGNING <ls_alv_line>.
      IF sy-subrc = 0.
        APPEND <ls_alv_line>-carrid TO lt_ids_to_delete.
      ENDIF.
    ENDLOOP.

    TRY.
        mo_model->mass_delete_of_carriers( lt_ids_to_delete ).
        _refresh_alv( ).
        MESSAGE 'Selected carriers successfully deleted' TYPE 'S'.
      CATCH zcx_zosql_error INTO lo_exception.
        _display_error( lo_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD _display_edit_dialog.

    DATA: lt_field_list_for_dialog      TYPE TABLE OF sval,
          ls_one_field_for_dialog       TYPE sval,
          lv_button_code_of_user_answer TYPE char1,
          lo_exception                  TYPE REF TO zcx_zosql_error.

    FIELD-SYMBOLS: <lv_ref_to_value_in_struct> TYPE any.

    es_scarr = is_scarr.

    ls_one_field_for_dialog-tabname = 'SCARR'.

    ls_one_field_for_dialog-fieldname = 'CARRID'.
    APPEND ls_one_field_for_dialog TO lt_field_list_for_dialog.

    ls_one_field_for_dialog-fieldname = 'CARRNAME'.
    APPEND ls_one_field_for_dialog TO lt_field_list_for_dialog.

    ls_one_field_for_dialog-fieldname = 'CURRCODE'.
    APPEND ls_one_field_for_dialog TO lt_field_list_for_dialog.

    ls_one_field_for_dialog-fieldname = 'URL'.
    APPEND ls_one_field_for_dialog TO lt_field_list_for_dialog.

    LOOP AT lt_field_list_for_dialog INTO ls_one_field_for_dialog.
      ASSIGN COMPONENT ls_one_field_for_dialog-fieldname OF STRUCTURE is_scarr TO <lv_ref_to_value_in_struct>.
      IF sy-subrc = 0.
        ls_one_field_for_dialog-value = <lv_ref_to_value_in_struct>.
        MODIFY lt_field_list_for_dialog FROM ls_one_field_for_dialog.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Insert new record'
      IMPORTING
        returncode      = lv_button_code_of_user_answer
      TABLES
        fields          = lt_field_list_for_dialog
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF sy-subrc <> 0 OR lv_button_code_of_user_answer = 'A'.
      ev_user_ok = abap_false.
      RETURN.
    ENDIF.

    LOOP AT lt_field_list_for_dialog INTO ls_one_field_for_dialog.
      ASSIGN COMPONENT ls_one_field_for_dialog-fieldname OF STRUCTURE es_scarr TO <lv_ref_to_value_in_struct>.
      IF sy-subrc = 0.
        <lv_ref_to_value_in_struct> = ls_one_field_for_dialog-value.
      ENDIF.
    ENDLOOP.

    ev_user_ok = abap_true.
  ENDMETHOD.
ENDCLASS.

END-OF-SELECTION.
  CREATE OBJECT go_app.
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       PAI
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  go_app->user_command( gv_ok_code ).
  CLEAR gv_ok_code.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*& PBO
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  go_app->pbo( ).
ENDMODULE.



CLASS ltc_unit_tests_on_prog DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_unit_tests_on_prog
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZOSQL_DB_EXAMPLE_DBCHANGE
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    CLASS-DATA: mo_test_environment TYPE REF TO zif_zosql_test_environment.
    DATA:
      f_cut            TYPE REF TO lcl_model,  "class under test
      mo_db_layer_fake TYPE REF TO zif_zosql_db_layer.

    CLASS-METHODS: class_setup.

    METHODS: setup,
      insert_successful FOR TESTING RAISING zcx_zosql_error,
      "
      insert_error_already_exist FOR TESTING RAISING zcx_zosql_error,
      insert_error_empty_carrname FOR TESTING RAISING zcx_zosql_error,
      insert_error_wrong_currency FOR TESTING RAISING zcx_zosql_error,
      insert_error_wrong_url FOR TESTING RAISING zcx_zosql_error,
      "
      update_successful FOR TESTING RAISING zcx_zosql_error,
      "
      update_error_carr_not_found FOR TESTING RAISING zcx_zosql_error,
      update_error_empty_carrname FOR TESTING RAISING zcx_zosql_error,
      "
      delete_successful FOR TESTING RAISING zcx_zosql_error,
      "
      delete_error_carr_not_exists FOR TESTING RAISING zcx_zosql_error,
      delete_error_carrid_in_sbook FOR TESTING RAISING zcx_zosql_error,
      "
      delete_multi_successful FOR TESTING RAISING zcx_zosql_error.
ENDCLASS.

CLASS ltc_unit_tests_on_prog IMPLEMENTATION.
  METHOD class_setup.
    mo_test_environment = zcl_zosql_test_environment=>create( ).
  ENDMETHOD.

  METHOD setup.

    mo_test_environment->clear_doubles( ).
    mo_db_layer_fake = mo_test_environment->get_db_layer_for_unit_tests( ).

    CREATE OBJECT f_cut
      EXPORTING
        io_db_layer = mo_db_layer_fake.
  ENDMETHOD.

  METHOD insert_successful.

    DATA: lt_curx TYPE TABLE OF scurx,
          ls_curx TYPE scurx.

    " GIVEN
    ls_curx-currkey = 'USD'.
    ls_curx-currdec = 2.
    APPEND ls_curx TO lt_curx.

    mo_test_environment->insert_test_data( lt_curx ).

    " WHEN
    DATA: ls_new_carrier  TYPE scarr.

    ls_new_carrier-carrid   = 'CA1'.
    ls_new_carrier-carrname = 'Test carrier 1'.
    ls_new_carrier-currcode = 'USD'.
    ls_new_carrier-url      = 'https://site.com'.

    f_cut->insert_new_carrier( ls_new_carrier ).

    " THEN
    DATA: ls_found_carrier_in_db TYPE scarr,
          ls_etalon              TYPE scarr.

    mo_db_layer_fake->select_to_itab( EXPORTING iv_select      = 'SELECT * FROM scarr WHERE carrid = ''CA1'''
                                      IMPORTING es_result_line = ls_found_carrier_in_db ).

    ls_etalon = ls_new_carrier.
    ls_etalon-mandt = sy-mandt.

    cl_aunit_assert=>assert_equals( act = ls_found_carrier_in_db exp = ls_etalon ).
  ENDMETHOD.

  METHOD insert_error_already_exist.

    DATA: lt_scarr_before TYPE TABLE OF scarr,
          ls_scarr_before TYPE scarr.

    " GIVEN
    ls_scarr_before-carrid   = 'CA1'.
    ls_scarr_before-carrname = 'Test carrier 1'.
    APPEND ls_scarr_before TO lt_scarr_before.

    mo_test_environment->insert_test_data( lt_scarr_before ).

    " WHEN
    DATA: ls_new_carrier TYPE scarr,
          lo_exception   TYPE REF TO cx_root.

    ls_new_carrier-carrid   = 'CA1'.
    ls_new_carrier-carrname = 'Test carrier 1'.

    TRY.
        f_cut->insert_new_carrier( ls_new_carrier ).
      CATCH cx_root INTO lo_exception.

    ENDTRY.

    " THEN
    cl_aunit_assert=>assert_bound( act = lo_exception
                                   msg = 'Error expected (test is negative), but the result is successful' ).
  ENDMETHOD.

  METHOD insert_error_empty_carrname.

    " WHEN
    DATA: ls_new_carrier TYPE scarr,
          lo_exception   TYPE REF TO cx_root.

    ls_new_carrier-carrid   = 'CA1'.
    CLEAR ls_new_carrier-carrname.

    TRY.
        f_cut->insert_new_carrier( ls_new_carrier ).
      CATCH cx_root INTO lo_exception.

    ENDTRY.

    " THEN
    cl_aunit_assert=>assert_bound( act = lo_exception
                                   msg = 'Error expected (test is negative), but the result is successful' ).
  ENDMETHOD.

  METHOD insert_error_wrong_currency.

    DATA: lt_curx TYPE TABLE OF scurx,
          ls_curx TYPE scurx.

    " GIVEN
    ls_curx-currkey = 'USD'.
    ls_curx-currdec = 2.
    APPEND ls_curx TO lt_curx.

    ls_curx-currkey = 'EUR'.
    ls_curx-currdec = 2.
    APPEND ls_curx TO lt_curx.

    ls_curx-currkey = 'RUB'.
    ls_curx-currdec = 2.
    APPEND ls_curx TO lt_curx.

    mo_test_environment->insert_test_data( lt_curx ).

    " WHEN
    DATA: ls_new_carrier TYPE scarr,
          lo_exception   TYPE REF TO cx_root.

    ls_new_carrier-carrid   = 'CA1'.
    ls_new_carrier-carrname = 'Test carrier 1'.
    ls_new_carrier-currcode = 'BAD'.

    TRY.
        f_cut->insert_new_carrier( ls_new_carrier ).
      CATCH cx_root INTO lo_exception.

    ENDTRY.

    " THEN
    cl_aunit_assert=>assert_bound( act = lo_exception
                                   msg = 'Error expected (test is negative), but the result is successful' ).
  ENDMETHOD.

  METHOD insert_error_wrong_url.
    " WHEN
    DATA: ls_new_carrier TYPE scarr,
          lo_exception   TYPE REF TO cx_root.

    ls_new_carrier-carrid   = 'CA1'.
    ls_new_carrier-carrname = 'Test carrier 1'.
    ls_new_carrier-url      = 'some bad url'.

    TRY.
        f_cut->insert_new_carrier( ls_new_carrier ).
      CATCH cx_root INTO lo_exception.

    ENDTRY.

    " THEN
    cl_aunit_assert=>assert_bound( act = lo_exception
                                   msg = 'Error expected (test is negative), but the result is successful' ).
  ENDMETHOD.

  METHOD update_successful.

    DATA: lt_curx          TYPE TABLE OF scurx,
          ls_curx          TYPE scurx,
          lt_scarr_initial TYPE TABLE OF scarr,
          ls_scarr_initial TYPE scarr.

    " GIVEN
    ls_curx-currkey = 'USD'.
    ls_curx-currdec = 2.
    APPEND ls_curx TO lt_curx.

    ls_curx-currkey = 'EUR'.
    ls_curx-currdec = 2.
    APPEND ls_curx TO lt_curx.

    mo_test_environment->insert_test_data( lt_curx ).

    ls_scarr_initial-carrid   = 'CA1'.
    ls_scarr_initial-carrname = 'Test carrier 1'.
    ls_scarr_initial-currcode = 'USD'.
    ls_scarr_initial-url      = 'https://site.com'.
    APPEND ls_scarr_initial TO lt_scarr_initial.

    mo_test_environment->insert_test_data( lt_scarr_initial ).

    " WHEN
    DATA: ls_scarr_updated  TYPE scarr.

    ls_scarr_updated-carrid   = 'CA1'.
    ls_scarr_updated-carrname = 'Test carrier 1 updated'.
    ls_scarr_updated-currcode = 'EUR'.
    ls_scarr_updated-url      = 'https://new-site.com'.

    f_cut->update_carrier( ls_scarr_updated ).

    " THEN
    DATA: ls_found_carrier_in_db TYPE scarr,
          ls_etalon              TYPE scarr.

    mo_db_layer_fake->select_to_itab( EXPORTING iv_select      = 'SELECT * FROM scarr WHERE carrid = ''CA1'''
                                      IMPORTING es_result_line = ls_found_carrier_in_db ).

    ls_etalon = ls_scarr_updated.
    ls_etalon-mandt = sy-mandt.

    cl_aunit_assert=>assert_equals( act = ls_found_carrier_in_db exp = ls_etalon ).
  ENDMETHOD.

  METHOD update_error_carr_not_found.
    DATA: lt_curx          TYPE TABLE OF scurx,
          ls_curx          TYPE scurx,
          lt_scarr_initial TYPE TABLE OF scarr,
          ls_scarr_initial TYPE scarr,
          lo_exception     TYPE REF TO cx_root.

    " GIVEN
    ls_curx-currkey = 'USD'.
    ls_curx-currdec = 2.
    APPEND ls_curx TO lt_curx.

    ls_curx-currkey = 'EUR'.
    ls_curx-currdec = 2.
    APPEND ls_curx TO lt_curx.

    mo_test_environment->insert_test_data( lt_curx ).

    ls_scarr_initial-carrid   = 'CA1'.
    ls_scarr_initial-carrname = 'Test carrier 1'.
    ls_scarr_initial-currcode = 'USD'.
    ls_scarr_initial-url      = 'https://site.com'.
    APPEND ls_scarr_initial TO lt_scarr_initial.

    mo_test_environment->insert_test_data( lt_scarr_initial ).

    " WHEN
    DATA: ls_scarr_updated  TYPE scarr.

    ls_scarr_updated-carrid   = 'CA2'.
    ls_scarr_updated-carrname = 'Some other carrier that does not exist'.
    ls_scarr_updated-currcode = 'EUR'.
    ls_scarr_updated-url      = 'https://new-site.com'.

    TRY.
        f_cut->update_carrier( ls_scarr_updated ).
      CATCH cx_root INTO lo_exception.

    ENDTRY.

    " THEN
    cl_aunit_assert=>assert_bound( act = lo_exception
                                   msg = 'Error expected (test is negative), but the result is successful' ).
  ENDMETHOD.

  METHOD update_error_empty_carrname.

    DATA: lt_scarr_initial TYPE TABLE OF scarr,
          ls_scarr_initial TYPE scarr.

    " GIVEN
    ls_scarr_initial-carrid   = 'CA1'.
    ls_scarr_initial-carrname = 'Test carrier 1'.
    ls_scarr_initial-currcode = 'USD'.
    ls_scarr_initial-url      = 'https://site.com'.
    APPEND ls_scarr_initial TO lt_scarr_initial.

    mo_test_environment->insert_test_data( lt_scarr_initial ).

    " WHEN
    DATA: ls_scarr_updated TYPE scarr,
          lo_exception     TYPE REF TO cx_root.

    ls_scarr_updated-carrid   = 'CA1'.
    CLEAR ls_scarr_updated-carrname.

    TRY.
        f_cut->update_carrier( ls_scarr_updated ).
      CATCH cx_root INTO lo_exception.

    ENDTRY.

    " THEN
    cl_aunit_assert=>assert_bound( act = lo_exception
                                   msg = 'Error expected (test is negative), but the result is successful' ).
  ENDMETHOD.

  METHOD delete_successful.
    DATA: lt_scarr_initial TYPE TABLE OF scarr,
          ls_scarr_initial TYPE scarr.

    " GIVEN
    ls_scarr_initial-carrid   = 'CA1'.
    ls_scarr_initial-carrname = 'Test carrier 1'.
    ls_scarr_initial-url      = 'https://site.com'.
    APPEND ls_scarr_initial TO lt_scarr_initial.

    mo_test_environment->insert_test_data( lt_scarr_initial ).

    " WHEN
    DATA: lt_scarr_ids_to_delete TYPE zosql_db_layer_carr_id_tab.

    APPEND 'CA1' TO lt_scarr_ids_to_delete.

    f_cut->mass_delete_of_carriers( lt_scarr_ids_to_delete ).

    " THEN
    DATA: lv_subrc  TYPE sysubrc.
    mo_db_layer_fake->select( EXPORTING iv_select = 'SELECT * FROM scarr WHERE carrid = ''CA1'''
                              IMPORTING ev_subrc  = lv_subrc ).

    cl_aunit_assert=>assert_differs( exp = 0 act = lv_subrc ).
  ENDMETHOD.

  METHOD delete_error_carr_not_exists.
    DATA: lt_scarr_initial TYPE TABLE OF scarr,
          ls_scarr_initial TYPE scarr.

    " GIVEN
    ls_scarr_initial-carrid   = 'CA1'.
    ls_scarr_initial-carrname = 'Test carrier 1'.
    ls_scarr_initial-url      = 'https://site.com'.
    APPEND ls_scarr_initial TO lt_scarr_initial.

    mo_test_environment->insert_test_data( lt_scarr_initial ).

    " WHEN
    DATA: lt_scarr_ids_to_delete TYPE zosql_db_layer_carr_id_tab,
          lv_exception_raised    TYPE abap_bool.

    APPEND 'CA2' TO lt_scarr_ids_to_delete.

    TRY.
        f_cut->mass_delete_of_carriers( lt_scarr_ids_to_delete ).
      CATCH cx_root.
        lv_exception_raised = abap_true.
    ENDTRY.

    " THEN
    cl_aunit_assert=>assert_equals( act = lv_exception_raised
                                    exp = abap_true
                                    msg = 'Error expected (test is negative), but the result is successful' ).

  ENDMETHOD.

  METHOD delete_error_carrid_in_sbook.

    DATA: lt_scarr_initial TYPE TABLE OF scarr,
          ls_scarr_initial TYPE scarr,
          lt_sbook         TYPE TABLE OF sbook,
          ls_sbook         TYPE sbook.

    " GIVEN
    ls_scarr_initial-carrid   = 'CA1'.
    ls_scarr_initial-carrname = 'Test carrier 1'.
    ls_scarr_initial-url      = 'https://site.com'.
    APPEND ls_scarr_initial TO lt_scarr_initial.

    mo_test_environment->insert_test_data( lt_scarr_initial ).

    ls_sbook-bookid = 1.
    ls_sbook-carrid = 'CA1'.
    APPEND ls_sbook TO lt_sbook.

    mo_test_environment->insert_test_data( lt_sbook ).

    " WHEN
    DATA: lt_scarr_ids_to_delete TYPE zosql_db_layer_carr_id_tab,
          lv_exception_raised    TYPE abap_bool.

    APPEND 'CA1' TO lt_scarr_ids_to_delete.

    TRY.
        f_cut->mass_delete_of_carriers( lt_scarr_ids_to_delete ).
      CATCH cx_root.
        lv_exception_raised = abap_true.
    ENDTRY.

    " THEN
    cl_aunit_assert=>assert_equals( act = lv_exception_raised
                                    exp = abap_true
                                    msg = 'Error expected (test is negative), but the result is successful' ).
  ENDMETHOD.

  METHOD delete_multi_successful.
    DATA: lt_scarr_initial TYPE TABLE OF scarr,
          ls_scarr_initial TYPE scarr.

    " GIVEN
    ls_scarr_initial-carrid   = 'CA1'.
    ls_scarr_initial-carrname = 'Test carrier 1'.
    ls_scarr_initial-url      = 'https://site.com'.
    APPEND ls_scarr_initial TO lt_scarr_initial.

    ls_scarr_initial-carrid   = 'CA2'.
    ls_scarr_initial-carrname = 'Test carrier 2'.
    ls_scarr_initial-url      = 'https://site.com'.
    APPEND ls_scarr_initial TO lt_scarr_initial.

    ls_scarr_initial-carrid   = 'CA3'.
    ls_scarr_initial-carrname = 'Test carrier 3'.
    ls_scarr_initial-url      = 'https://site.com'.
    APPEND ls_scarr_initial TO lt_scarr_initial.

    mo_test_environment->insert_test_data( lt_scarr_initial ).

    " WHEN
    DATA: lt_scarr_ids_to_delete TYPE zosql_db_layer_carr_id_tab.

    APPEND 'CA1' TO lt_scarr_ids_to_delete.
    APPEND 'CA3' TO lt_scarr_ids_to_delete.

    f_cut->mass_delete_of_carriers( lt_scarr_ids_to_delete ).

    " THEN
    DATA: lt_scarr_after_delete TYPE TABLE OF scarr,
          lt_scarr_etalon       TYPE TABLE OF scarr,
          ls_scarr_etalon       TYPE scarr.

    mo_db_layer_fake->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM scarr'
                                      IMPORTING et_result_table = lt_scarr_after_delete ).

    ls_scarr_etalon-mandt    = sy-mandt.
    ls_scarr_etalon-carrid   = 'CA2'.
    ls_scarr_etalon-carrname = 'Test carrier 2'.
    ls_scarr_etalon-url      = 'https://site.com'.
    APPEND ls_scarr_etalon TO lt_scarr_etalon.

    cl_aunit_assert=>assert_equals( act = lt_scarr_after_delete exp = lt_scarr_etalon ).
  ENDMETHOD.
ENDCLASS.
