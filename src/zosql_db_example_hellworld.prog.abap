*&---------------------------------------------------------------------*
*& Report ZOSQL_DB_EXAMPLE_HELLWORLD
*&---------------------------------------------------------------------*
*& Program with simple examples of using of Z-SQL Test Double
*& framework library
*&---------------------------------------------------------------------*
REPORT zosql_db_example_hellworld.

PARAMETERS: p_simp TYPE flag DEFAULT 'X' RADIOBUTTON GROUP typ,
            p_stat TYPE flag RADIOBUTTON GROUP typ,
            p_par  TYPE flag RADIOBUTTON GROUP typ,
            p_fae  TYPE flag RADIOBUTTON GROUP typ.

DATA: go_db_layer  TYPE REF TO zif_zosql_db_layer.

START-OF-SELECTION.
  PERFORM main.

*&---------------------------------------------------------------------*
*& Form MAIN
*&---------------------------------------------------------------------*
*& Starting point of program
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM main .

  DATA: ld_result    TYPE REF TO data,
        lo_exception TYPE REF TO cx_root.

  go_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).

  TRY.
      CASE abap_true.
        WHEN p_simp.
          PERFORM simple_select CHANGING ld_result.
        WHEN p_stat.
          PERFORM more_complicated_select CHANGING ld_result.
        WHEN p_par.
          PERFORM select_with_params CHANGING ld_result.
        WHEN p_fae.
          PERFORM select_for_all_entries CHANGING ld_result.
      ENDCASE.

      IF ld_result IS BOUND.
        PERFORM show_alv USING ld_result.
      ENDIF.

    CATCH cx_root INTO lo_exception.
      MESSAGE lo_exception->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_ALV
*&---------------------------------------------------------------------*
*& Shows ALV based upon dynamic table
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM show_alv USING id_dynamic_table TYPE REF TO data
              RAISING cx_salv_msg.

  DATA: lo_alv     TYPE REF TO cl_salv_table.

  FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

  ASSIGN id_dynamic_table->* TO <lt_table>.

  cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                          CHANGING  t_table      = <lt_table> ).

  PERFORM set_col_titles_as_fieldnames USING lo_alv <lt_table>.

  lo_alv->display( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_COL_TITLES_AS_FIELDNAMES
*&---------------------------------------------------------------------*
*& Sets column titles as field names
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_col_titles_as_fieldnames USING io_alv   TYPE REF TO cl_salv_table
                                        it_table TYPE STANDARD TABLE.

  DATA: lo_columns             TYPE REF TO cl_salv_columns_table,
        lo_table               TYPE REF TO cl_abap_tabledescr,
        lo_struct              TYPE REF TO cl_abap_structdescr,
        lt_components_of_table TYPE cl_abap_structdescr=>component_table,
        lo_column              TYPE REF TO cl_salv_column,
        lv_column_name         TYPE lvc_fname,
        lv_short_text          TYPE scrtext_s,
        lv_medium_text         TYPE scrtext_m,
        lv_long_text           TYPE scrtext_l.

  FIELD-SYMBOLS: <ls_component> LIKE LINE OF lt_components_of_table.

  lo_table ?= cl_abap_tabledescr=>describe_by_data( it_table ).
  lo_struct ?= lo_table->get_table_line_type( ).
  lt_components_of_table = lo_struct->get_components( ).

  lo_columns = io_alv->get_columns( ).
  LOOP AT lt_components_of_table ASSIGNING <ls_component>.
    lv_column_name = <ls_component>-name.
    TRY.
        lo_column = lo_columns->get_column( lv_column_name ).

        lv_short_text = <ls_component>-name.
        lo_column->set_short_text( lv_short_text ).

        lv_medium_text = <ls_component>-name.
        lo_column->set_medium_text( lv_medium_text ).

        lv_long_text = <ls_component>-name.
        lo_column->set_long_text( lv_long_text ).
      CATCH cx_salv_not_found.
        CONTINUE.
    ENDTRY.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SIMPLE_SELECT
*&---------------------------------------------------------------------*
*& Simple select
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM simple_select CHANGING cd_result TYPE REF TO data
                   RAISING zcx_zosql_error.
  go_db_layer->select( EXPORTING iv_select          = 'SELECT * FROM sbook'
                       IMPORTING ed_result_as_table = cd_result ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MORE_COMPLICATED_SELECT
*&---------------------------------------------------------------------*
*& More complicated select
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM more_complicated_select CHANGING cd_result TYPE REF TO data
                             RAISING zcx_zosql_error.

  DATA: lv_select  TYPE string.

  CONCATENATE 'SELECT * FROM sbook'
    'WHERE fldate >= ''20210101'''
    '  AND luggweight < 20'
    '  AND smoker = ''X'''
    INTO lv_select SEPARATED BY space.

  go_db_layer->select( EXPORTING iv_select          = lv_select
                       IMPORTING ed_result_as_table = cd_result ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_WITH_PARAMS
*&---------------------------------------------------------------------*
*& Select with parameters passed as variables
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM select_with_params CHANGING cd_result TYPE REF TO data
                        RAISING zcx_zosql_error.

  DATA: lv_select     TYPE string,
        lt_parameters TYPE zosql_db_layer_params,
        ls_parameter  TYPE zosql_db_layer_param.

  CONCATENATE 'SELECT * FROM sbook'
    'WHERE fldate >= :FLDATE'
    '  AND luggweight < 20'
    '  AND smoker = ''X'''
    INTO lv_select SEPARATED BY space.

  ls_parameter-param_name_in_select = ':FLDATE'. " Does not have to start with :, may be any value unique for the SQL
  CONCATENATE sy-datum(4) '0101' INTO ls_parameter-parameter_value_single.
  APPEND ls_parameter TO lt_parameters.

  go_db_layer->select( EXPORTING iv_select          = lv_select
                                 it_parameters      = lt_parameters
                       IMPORTING ed_result_as_table = cd_result ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_FOR_ALL_ENTRIES
*&---------------------------------------------------------------------*
*& Select for all entries example
*&---------------------------------------------------------------------*
*&      <-- LD_RESULT
*&---------------------------------------------------------------------*
FORM select_for_all_entries CHANGING cd_result TYPE REF TO data
                            RAISING zcx_zosql_error.

  TYPES: BEGIN OF ty_carrid,
           carrid TYPE sbook-carrid,
         END OF ty_carrid.

  DATA: lv_select TYPE string,
        lt_carrid TYPE TABLE OF ty_carrid,
        ls_carrid TYPE ty_carrid.

  ls_carrid-carrid = 'AA'.
  APPEND ls_carrid TO lt_carrid.

  ls_carrid-carrid = 'LH'.
  APPEND ls_carrid TO lt_carrid.

  CONCATENATE 'SELECT * FROM sbook'
    'FOR ALL ENTRIES IN itab'
    'WHERE carrid = itab-carrid'
    '  AND fldate >= ''20210101'''
    '  AND luggweight < 20'
    '  AND smoker = ''X'''
    INTO lv_select SEPARATED BY space.  " For all entries table can have any name inside select

  go_db_layer->select( EXPORTING iv_select                = lv_select
                                 it_for_all_entries_table = lt_carrid
                       IMPORTING ed_result_as_table       = cd_result ).
ENDFORM.

CLASS ltc_unit_tests_on_prog DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_Testable_Db_Layer
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_TESTABLE_DB_READER
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

    CLASS-DATA:
      mo_test_environment TYPE REF TO zif_zosql_test_environment.

    CLASS-METHODS: class_setup.

    METHODS: setup,
      simple_select FOR TESTING RAISING zcx_zosql_error,
      more_complicated_select FOR TESTING RAISING zcx_zosql_error,
      select_with_params FOR TESTING RAISING zcx_zosql_error,
      select_for_all_entries FOR TESTING RAISING zcx_zosql_error.
ENDCLASS.

CLASS ltc_unit_tests_on_prog IMPLEMENTATION.
  METHOD class_setup.
    mo_test_environment = zcl_zosql_test_environment=>create( ).
  ENDMETHOD.

  METHOD setup.
    mo_test_environment->clear_doubles( ).
    go_db_layer = mo_test_environment->get_db_layer_for_unit_tests( ).
  ENDMETHOD.

  METHOD simple_select.

    DATA: lt_sbook TYPE TABLE OF sbook,
          ls_sbook TYPE sbook.

    " GIVEN
    ls_sbook-bookid = '101'.
    ls_sbook-carrid = '1'.
    ls_sbook-fldate = '20211001'.
    APPEND ls_sbook TO lt_sbook.

    ls_sbook-bookid = '102'.
    ls_sbook-carrid = '2'.
    ls_sbook-fldate = '20211002'.
    APPEND ls_sbook TO lt_sbook.

    mo_test_environment->insert_test_data( lt_sbook ).

    " WHEN
    DATA: ld_data           TYPE REF TO data,
          lt_sbook_selected TYPE TABLE OF sbook.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

    PERFORM simple_select CHANGING ld_data.
    ASSIGN ld_data->* TO <lt_data>.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_data>
                                               IMPORTING et_table_dest = lt_sbook_selected ).

    " THEN
    DATA: lt_sbook_expected TYPE TABLE OF sbook,
          ls_sbook_expected TYPE sbook.

    ls_sbook_expected-mandt  = sy-mandt.
    ls_sbook_expected-bookid = '101'.
    ls_sbook_expected-carrid = '1'.
    ls_sbook_expected-fldate = '20211001'.
    APPEND ls_sbook_expected TO lt_sbook_expected.

    ls_sbook_expected-mandt  = sy-mandt.
    ls_sbook_expected-bookid = '102'.
    ls_sbook_expected-carrid = '2'.
    ls_sbook_expected-fldate = '20211002'.
    APPEND ls_sbook_expected TO lt_sbook_expected.

    SORT: lt_sbook_selected, lt_sbook_expected.

    cl_aunit_assert=>assert_equals( exp = lt_sbook_expected act = lt_sbook_selected ).
  ENDMETHOD.

  METHOD more_complicated_select.
    DATA: lt_sbook TYPE TABLE OF sbook,
          ls_sbook TYPE sbook.

    " GIVEN
    ls_sbook-bookid     = '101'.
    ls_sbook-carrid     = '1'.
    ls_sbook-fldate     = '20201001'.
    ls_sbook-luggweight = '15'.
    ls_sbook-smoker     = ' '.
    APPEND ls_sbook TO lt_sbook.

    ls_sbook-bookid     = '102'.
    ls_sbook-carrid     = '2'.
    ls_sbook-fldate     = '20211002'.
    ls_sbook-luggweight = '50'.
    ls_sbook-smoker     = 'X'.
    APPEND ls_sbook TO lt_sbook.

    ls_sbook-bookid     = '103'.
    ls_sbook-carrid     = '2'.
    ls_sbook-fldate     = '20211003'.
    ls_sbook-luggweight = '15'.
    ls_sbook-smoker     = ' '.
    APPEND ls_sbook TO lt_sbook.

    ls_sbook-bookid     = '104'.
    ls_sbook-carrid     = '5'.
    ls_sbook-fldate     = '20211005'.
    ls_sbook-luggweight = '15'.
    ls_sbook-smoker     = 'X'.
    APPEND ls_sbook TO lt_sbook.

    mo_test_environment->insert_test_data( lt_sbook ).

    " WHEN
    DATA: ld_data           TYPE REF TO data,
          lt_sbook_selected TYPE TABLE OF sbook.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

    PERFORM more_complicated_select CHANGING ld_data.
    ASSIGN ld_data->* TO <lt_data>.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_data>
                                               IMPORTING et_table_dest = lt_sbook_selected ).

    " THEN
    DATA: lt_sbook_expected TYPE TABLE OF sbook,
          ls_sbook_expected TYPE sbook.

    ls_sbook_expected-mandt      = sy-mandt.
    ls_sbook_expected-bookid     = '104'.
    ls_sbook_expected-carrid     = '5'.
    ls_sbook_expected-fldate     = '20211005'.
    ls_sbook_expected-luggweight = '15'.
    ls_sbook_expected-smoker     = 'X'.
    APPEND ls_sbook_expected TO lt_sbook_expected.

    cl_aunit_assert=>assert_equals( exp = lt_sbook_expected act = lt_sbook_selected ).
  ENDMETHOD.

  METHOD select_with_params.
    DATA: lt_sbook TYPE TABLE OF sbook,
          ls_sbook TYPE sbook.

    " GIVEN
    ls_sbook-bookid     = '101'.
    ls_sbook-carrid     = '1'.
    ls_sbook-fldate     = '20201001'.
    ls_sbook-luggweight = '15'.
    ls_sbook-smoker     = ' '.
    APPEND ls_sbook TO lt_sbook.

    ls_sbook-bookid     = '102'.
    ls_sbook-carrid     = '2'.
    ls_sbook-fldate     = '20211002'.
    ls_sbook-luggweight = '50'.
    ls_sbook-smoker     = 'X'.
    APPEND ls_sbook TO lt_sbook.

    ls_sbook-bookid     = '103'.
    ls_sbook-carrid     = '2'.
    ls_sbook-fldate     = '20211003'.
    ls_sbook-luggweight = '15'.
    ls_sbook-smoker     = ' '.
    APPEND ls_sbook TO lt_sbook.

    ls_sbook-bookid     = '104'.
    ls_sbook-carrid     = '5'.
    ls_sbook-fldate     = '20211005'.
    ls_sbook-luggweight = '15'.
    ls_sbook-smoker     = 'X'.
    APPEND ls_sbook TO lt_sbook.

    mo_test_environment->insert_test_data( lt_sbook ).

    " WHEN
    DATA: ld_data           TYPE REF TO data,
          lt_sbook_selected TYPE TABLE OF sbook.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

    PERFORM select_with_params CHANGING ld_data.
    ASSIGN ld_data->* TO <lt_data>.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_data>
                                               IMPORTING et_table_dest = lt_sbook_selected ).

    " THEN
    DATA: lt_sbook_expected TYPE TABLE OF sbook,
          ls_sbook_expected TYPE sbook.

    ls_sbook_expected-mandt      = sy-mandt.
    ls_sbook_expected-bookid     = '104'.
    ls_sbook_expected-carrid     = '5'.
    ls_sbook_expected-fldate     = '20211005'.
    ls_sbook_expected-luggweight = '15'.
    ls_sbook_expected-smoker     = 'X'.
    APPEND ls_sbook_expected TO lt_sbook_expected.

    cl_aunit_assert=>assert_equals( exp = lt_sbook_expected act = lt_sbook_selected ).
  ENDMETHOD.

  METHOD select_for_all_entries.
    DATA: lt_sbook  TYPE TABLE OF sbook,
          ls_sbook  TYPE sbook.

    " GIVEN
    ls_sbook-bookid     = '101'.
    ls_sbook-carrid     = 'AA'.
    ls_sbook-fldate     = '20211001'.
    ls_sbook-luggweight = '15'.
    ls_sbook-smoker     = 'X'.
    APPEND ls_sbook TO lt_sbook.

    ls_sbook-bookid     = '102'.
    ls_sbook-carrid     = 'BB'.
    ls_sbook-fldate     = '20211002'.
    ls_sbook-luggweight = '50'.
    ls_sbook-smoker     = 'X'.
    APPEND ls_sbook TO lt_sbook.

    ls_sbook-bookid     = '103'.
    ls_sbook-carrid     = 'LH'.
    ls_sbook-fldate     = '20211003'.
    ls_sbook-luggweight = '15'.
    ls_sbook-smoker     = ' '.
    APPEND ls_sbook TO lt_sbook.

    ls_sbook-bookid     = '104'.
    ls_sbook-carrid     = 'LH'.
    ls_sbook-fldate     = '20211005'.
    ls_sbook-luggweight = '15'.
    ls_sbook-smoker     = 'X'.
    APPEND ls_sbook TO lt_sbook.

    mo_test_environment->insert_test_data( lt_sbook ).

    " WHEN
    DATA: ld_data           TYPE REF TO data,
          lt_sbook_selected TYPE TABLE OF sbook.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

    PERFORM select_for_all_entries CHANGING ld_data.
    ASSIGN ld_data->* TO <lt_data>.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_data>
                                               IMPORTING et_table_dest = lt_sbook_selected ).

    " THEN
    DATA: lt_sbook_expected TYPE TABLE OF sbook,
          ls_sbook_expected TYPE sbook.

    ls_sbook_expected-mandt      = sy-mandt.
    ls_sbook_expected-bookid     = '101'.
    ls_sbook_expected-carrid     = 'AA'.
    ls_sbook_expected-fldate     = '20211001'.
    ls_sbook_expected-luggweight = '15'.
    ls_sbook_expected-smoker     = 'X'.
    APPEND ls_sbook_expected TO lt_sbook_expected.

    ls_sbook_expected-mandt      = sy-mandt.
    ls_sbook_expected-bookid     = '104'.
    ls_sbook_expected-carrid     = 'LH'.
    ls_sbook_expected-fldate     = '20211005'.
    ls_sbook_expected-luggweight = '15'.
    ls_sbook_expected-smoker     = 'X'.
    APPEND ls_sbook_expected TO lt_sbook_expected.

    SORT: lt_sbook_selected, lt_sbook_expected.

    cl_aunit_assert=>assert_equals( exp = lt_sbook_expected act = lt_sbook_selected ).
  ENDMETHOD.
ENDCLASS.
