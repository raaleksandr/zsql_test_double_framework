*&---------------------------------------------------------------------*
*& Report ZOSQL_DB_EXAMPLE_REP
*&---------------------------------------------------------------------*
*& Example program as regular report with some SQL queries
*& The program uses Z-SQL Test Double Framework library to make code
*& isolated from database and therefore testable with unit-tests
*&---------------------------------------------------------------------*
REPORT ztestable_db_example_rep_init.

CLASS lcl_model DEFINITION DEFERRED.

TABLES: sbook.

SELECT-OPTIONS: s_custid FOR sbook-customid.

DATA: lo_model TYPE REF TO lcl_model.

CLASS lcl_model DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_data_line,
             customid                     TYPE scustom-id,
             customer_name                TYPE scustom-name,
             count_tickets_bought         TYPE zosql_example_tickets_bought,
             date_of_first_flight         TYPE zosql_date_of_first_flight,
             name_of_first_carrier        TYPE zosql_example_first_carrier,
             name_of_favourite_carrier    TYPE zosql_example_favor_carrier,
             num_flight_favourite_carrier TYPE zosql_example_num_flight_fav,
             is_smoker                    TYPE sbook-smoker,
             which_class                  TYPE sbook-class,
           END OF ty_data_line.

    TYPES: ty_data TYPE STANDARD TABLE OF ty_data_line WITH KEY customid.

    METHODS: constructor IMPORTING io_db_layer TYPE REF TO zif_zosql_db_layer OPTIONAL.

    METHODS: read_data IMPORTING it_customid_range TYPE zosql_db_example_cust_rng
                       RAISING zcx_zosql_error,
      get_data RETURNING VALUE(rt_data) TYPE ty_data,
      check_test_tables_filled RETURNING VALUE(rv_filled) TYPE abap_bool.

  PRIVATE SECTION.
    DATA: mt_data     TYPE ty_data,
          mo_db_layer TYPE REF TO zif_zosql_db_layer.
ENDCLASS.

CLASS lcl_model IMPLEMENTATION.

  METHOD constructor.
    IF io_db_layer IS BOUND.
      mo_db_layer = io_db_layer.
    ELSE.
      mo_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).
    ENDIF.
  ENDMETHOD.

  METHOD read_data.

    TYPES: BEGIN OF ty_custom_first_carrier,
             customid TYPE scustom-id,
             carrname TYPE scarr-carrname,
             connid   TYPE sbook-connid,
           END OF ty_custom_first_carrier,
           BEGIN OF ty_custom_flight_by_carr,
             customid       TYPE scustom-id,
             carrid         TYPE scarr-carrid,
             carrname       TYPE scarr-carrname,
             num_of_flights TYPE i,
           END OF ty_custom_flight_by_carr,
           BEGIN OF ty_custom_class,
             customid  TYPE scustom-id,
             min_class TYPE sbook-class,
             max_class TYPE sbook-class,
           END OF ty_custom_class.

    DATA: lt_custom_first_carrier  TYPE SORTED TABLE OF ty_custom_first_carrier
                                       WITH NON-UNIQUE KEY customid connid,
          lt_custom_flight_by_carr TYPE TABLE OF ty_custom_flight_by_carr,
          lt_custom_class          TYPE HASHED TABLE OF ty_custom_class
                                        WITH UNIQUE KEY customid,
          lv_select                TYPE string,
          lt_select_params         TYPE TABLE OF zosql_db_layer_param,
          ls_select_param          TYPE zosql_db_layer_param.

    FIELD-SYMBOLS: <ls_data>                  LIKE LINE OF mt_data,
                   <ls_custom_first_carrier>  LIKE LINE OF lt_custom_first_carrier,
                   <ls_custom_fligth_by_carr> LIKE LINE OF lt_custom_flight_by_carr,
                   <ls_custom_class>          LIKE LINE OF lt_custom_class,
                   <ls_customid_range>        LIKE LINE OF it_customid_range,
                   <ls_param_range_line>      TYPE zosql_db_layer_range_line.

    REFRESH mt_data.

    CONCATENATE
      'SELECT sbook~customid AS customid'
           'MIN( scustom~name ) AS customer_name'
           'COUNT( * ) AS count_tickets_bought'
           'MIN( sflight~fldate ) AS date_of_first_flight'
           'MAX( sbook~smoker ) AS is_smoker'
      'FROM sbook'
      'JOIN scustom ON scustom~id = sbook~customid'
      'JOIN sflight ON sflight~carrid = sbook~carrid'
                  'AND sflight~connid = sbook~connid'
                  'AND sflight~fldate = sbook~fldate'
      'WHERE sbook~customid IN :it_customid_range'
      'GROUP BY customid'
      'ORDER BY customid'
      INTO lv_select SEPARATED BY space.

    ls_select_param-param_name_in_select = ':it_customid_range'.

    LOOP AT it_customid_range ASSIGNING <ls_customid_range>.
      APPEND INITIAL LINE TO ls_select_param-parameter_value_range ASSIGNING <ls_param_range_line>.
      <ls_param_range_line>-sign   = <ls_customid_range>-sign.
      <ls_param_range_line>-option = <ls_customid_range>-option.
      <ls_param_range_line>-low    = <ls_customid_range>-low.
      <ls_param_range_line>-high   = <ls_customid_range>-high.
    ENDLOOP.

    APPEND ls_select_param TO lt_select_params.

    mo_db_layer->select_to_itab( EXPORTING iv_select                = lv_select
                                           it_parameters            = lt_select_params
                                           iv_do_into_corresponding = abap_true
                                 IMPORTING et_result_table          = mt_data ).

    IF mt_data IS NOT INITIAL.

      CONCATENATE
        'SELECT sbook~customid scarr~carrname sbook~connid'
        'FROM sflight'
        'JOIN sbook ON sbook~carrid = sflight~carrid'
        ' AND sbook~connid = sflight~connid'
        ' AND sbook~fldate = sflight~fldate'
        'JOIN scarr ON scarr~carrid = sbook~carrid'
        'FOR ALL ENTRIES IN mt_data'
        'WHERE sbook~customid = mt_data-customid'
        '  AND sbook~fldate   = mt_data-date_of_first_flight'
        INTO lv_select SEPARATED BY space.

      mo_db_layer->select_to_itab( EXPORTING iv_select                = lv_select
                                             it_for_all_entries_table = mt_data
                                             iv_do_into_corresponding = abap_true
                                   IMPORTING et_result_table          = lt_custom_first_carrier ).

      CONCATENATE
        'SELECT sbook~customid'
             'sbook~carrid'
             'MIN( scarr~carrname ) AS carrname'
             'COUNT( * ) AS num_of_flights'
        'FROM sbook'
        'JOIN scarr ON scarr~carrid = sbook~carrid'
        'WHERE sbook~customid IN :it_customid_range'
        'GROUP BY sbook~customid sbook~carrid'
        INTO lv_select SEPARATED BY space.

      mo_db_layer->select_to_itab( EXPORTING iv_select                = lv_select
                                             it_parameters            = lt_select_params
                                             iv_do_into_corresponding = abap_true
                                   IMPORTING et_result_table          = lt_custom_flight_by_carr ).

      CONCATENATE
        'SELECT sbook~customid'
             'MIN( sbook~class ) AS min_class'
             'MAX( sbook~class ) AS max_class'
        'FROM sbook'
        'WHERE sbook~customid IN :it_customid_range'
        'GROUP BY customid'
        INTO lv_select SEPARATED BY space.

      mo_db_layer->select_to_itab( EXPORTING iv_select                = lv_select
                                             it_parameters            = lt_select_params
                                             iv_do_into_corresponding = abap_true
                                   IMPORTING et_result_table          = lt_custom_class ).
    ENDIF.

    SORT lt_custom_flight_by_carr BY customid num_of_flights DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_custom_flight_by_carr COMPARING customid.

    LOOP AT mt_data ASSIGNING <ls_data>.

      READ TABLE lt_custom_first_carrier WITH KEY customid = <ls_data>-customid
        ASSIGNING <ls_custom_first_carrier>.
      IF sy-subrc = 0.
        <ls_data>-name_of_first_carrier = <ls_custom_first_carrier>-carrname.
      ENDIF.

      READ TABLE lt_custom_flight_by_carr WITH KEY customid = <ls_data>-customid
        ASSIGNING <ls_custom_fligth_by_carr> BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_data>-name_of_favourite_carrier    = <ls_custom_fligth_by_carr>-carrname.
        <ls_data>-num_flight_favourite_carrier = <ls_custom_fligth_by_carr>-num_of_flights.
      ENDIF.

      READ TABLE lt_custom_class WITH TABLE KEY customid = <ls_data>-customid
        ASSIGNING <ls_custom_class>.
      IF sy-subrc = 0.
        IF <ls_custom_class>-min_class = <ls_custom_class>-max_class.
          <ls_data>-which_class = <ls_custom_class>-min_class.
        ELSE.
          <ls_data>-which_class = 'X'.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_data.
    rt_data = mt_data.
  ENDMETHOD.

  METHOD check_test_tables_filled.

    SELECT SINGLE sbook~connid
      INTO zcl_zosql_utils=>dummy
      FROM sbook.

    IF sy-subrc = 0.
      rv_filled = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  CREATE OBJECT lo_model.
  lo_model->read_data( it_customid_range = s_custid[] ).

END-OF-SELECTION.
  IF lo_model->check_test_tables_filled( ) = abap_true.
    PERFORM display_alv.
  ELSE.
    MESSAGE 'Test data empty, please run SE38->SAPBC_DATA_GENERATOR to generate' TYPE 'I'.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
*& Displays data as alv
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_alv .
  DATA: lt_alv       TYPE lcl_model=>ty_data,
        lo_alv       TYPE REF TO cl_salv_table,
        lo_exception TYPE REF TO cx_root.

  lt_alv = lo_model->get_data( ).

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                              CHANGING  t_table      = lt_alv ).
      lo_alv->display( ).
    CATCH cx_root INTO lo_exception.
      zcl_zosql_utils=>dummy = lo_exception->get_text( ).
      MESSAGE zcl_zosql_utils=>dummy TYPE 'I'.
  ENDTRY.
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

    DATA:
      f_cut               TYPE REF TO lcl_model.  "class under test.

    CLASS-METHODS: class_setup.

    METHODS: setup,
      customer_one_flight FOR TESTING RAISING zcx_zosql_error,
      name_of_first_carrier FOR TESTING RAISING zcx_zosql_error,
      favourite_carrier FOR TESTING RAISING zcx_zosql_error.
ENDCLASS.

CLASS ltc_unit_tests_on_prog IMPLEMENTATION.
  METHOD class_setup.
    mo_test_environment = zcl_zosql_test_environment=>create( ).
  ENDMETHOD.

  METHOD setup.
    DATA: lo_db_layer_fake   TYPE REF TO zif_zosql_db_layer.

    mo_test_environment->clear_doubles( ).
    lo_db_layer_fake = mo_test_environment->get_db_layer_for_unit_tests( ).

    CREATE OBJECT f_cut
      EXPORTING
        io_db_layer = lo_db_layer_fake.
  ENDMETHOD.

  METHOD customer_one_flight.

    " GIVEN

    DATA: lt_scustom        TYPE TABLE OF scustom,
          ls_scustom        TYPE scustom,
          lt_scarr          TYPE TABLE OF scarr,
          ls_scarr          TYPE scarr,
          lt_sbook          TYPE TABLE OF sbook,
          ls_sbook          TYPE sbook,
          lt_sflight        TYPE TABLE OF sflight,
          ls_sflight        TYPE sflight,
          lt_customid_range TYPE zosql_db_example_cust_rng.

    ls_scustom-id = 1.
    ls_scustom-name = 'Test Customer 1'.
    APPEND ls_scustom TO lt_scustom.

    mo_test_environment->insert_test_data( lt_scustom ).

    ls_scarr-carrid = 10.
    ls_scarr-carrname = 'Test carrier 1'.
    APPEND ls_scarr TO lt_scarr.

    mo_test_environment->insert_test_data( lt_scarr ).

    ls_sflight-carrid = 10.
    ls_sflight-connid = 100.
    ls_sflight-fldate = '20211026'.
    APPEND ls_sflight TO lt_sflight.

    mo_test_environment->insert_test_data( lt_sflight ).

    ls_sbook-carrid   = 10.
    ls_sbook-connid   = 100.
    ls_sbook-fldate   = '20211026'.
    ls_sbook-bookid   = 1000.
    ls_sbook-customid = 1.
    ls_sbook-smoker   = 'X'.
    ls_sbook-class    = 'Y'.
    APPEND ls_sbook TO lt_sbook.

    mo_test_environment->insert_test_data( lt_sbook ).

    " WHEN
    DATA: lt_result TYPE lcl_model=>ty_data.

    f_cut->read_data( lt_customid_range ).
    lt_result = f_cut->get_data( ).

    " THEN
    DATA: lt_expected TYPE lcl_model=>ty_data,
          ls_expected LIKE LINE OF lt_expected.

    ls_expected-customid                     = 1.
    ls_expected-customer_name                = 'Test Customer 1'.
    ls_expected-count_tickets_bought         = 1.
    ls_expected-date_of_first_flight         = '20211026'.
    ls_expected-name_of_first_carrier        = 'Test carrier 1'.
    ls_expected-name_of_favourite_carrier    = 'Test carrier 1'.
    ls_expected-num_flight_favourite_carrier = 1.
    ls_expected-is_smoker                    = 'X'.
    ls_expected-which_class                  = 'Y'.
    APPEND ls_expected TO lt_expected.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected ).
  ENDMETHOD.

  METHOD name_of_first_carrier.
    " GIVEN

    DATA: lt_scustom        TYPE TABLE OF scustom,
          ls_scustom        TYPE scustom,
          lt_scarr          TYPE TABLE OF scarr,
          ls_scarr          TYPE scarr,
          lt_sbook          TYPE TABLE OF sbook,
          ls_sbook          TYPE sbook,
          lt_sflight        TYPE TABLE OF sflight,
          ls_sflight        TYPE sflight,
          lt_customid_range TYPE zosql_db_example_cust_rng.

    ls_scustom-id = 1.
    APPEND ls_scustom TO lt_scustom.

    mo_test_environment->insert_test_data( lt_scustom ).

    ls_scarr-carrid = 10.
    ls_scarr-carrname = 'Not first carrier'.
    APPEND ls_scarr TO lt_scarr.

    ls_scarr-carrid = 20.
    ls_scarr-carrname = 'The first carrier'.
    APPEND ls_scarr TO lt_scarr.

    mo_test_environment->insert_test_data( lt_scarr ).

    ls_sflight-carrid = 10.
    ls_sflight-connid = 100.
    ls_sflight-fldate = '20211231'.
    APPEND ls_sflight TO lt_sflight.

    ls_sflight-carrid = 20.
    ls_sflight-connid = 200.
    ls_sflight-fldate = '20211201'.
    APPEND ls_sflight TO lt_sflight.

    mo_test_environment->insert_test_data( lt_sflight ).

    ls_sbook-carrid   = 10.
    ls_sbook-connid   = 100.
    ls_sbook-fldate   = '20211231'.
    ls_sbook-bookid   = 1000.
    ls_sbook-customid = 1.
    APPEND ls_sbook TO lt_sbook.

    ls_sbook-carrid   = 20.
    ls_sbook-connid   = 200.
    ls_sbook-fldate   = '20211201'.
    ls_sbook-bookid   = 2000.
    ls_sbook-customid = 1.
    APPEND ls_sbook TO lt_sbook.

    mo_test_environment->insert_test_data( lt_sbook ).

    " WHEN
    DATA: lt_result TYPE lcl_model=>ty_data,
          ls_result LIKE LINE OF lt_result.

    f_cut->read_data( lt_customid_range ).
    lt_result = f_cut->get_data( ).
    READ TABLE lt_result INDEX 1 INTO ls_result.

    " THEN
    cl_aunit_assert=>assert_equals( act = ls_result-name_of_first_carrier
                                    exp = 'The first carrier' ).
  ENDMETHOD.

  METHOD favourite_carrier.
    " GIVEN

    DATA: lt_scustom        TYPE TABLE OF scustom,
          ls_scustom        TYPE scustom,
          lt_scarr          TYPE TABLE OF scarr,
          ls_scarr          TYPE scarr,
          lt_sbook          TYPE TABLE OF sbook,
          ls_sbook          TYPE sbook,
          lt_sflight        TYPE TABLE OF sflight,
          ls_sflight        TYPE sflight,
          lt_customid_range TYPE zosql_db_example_cust_rng.

    ls_scustom-id = 1.
    APPEND ls_scustom TO lt_scustom.

    mo_test_environment->insert_test_data( lt_scustom ).

    ls_scarr-carrid = 10.
    ls_scarr-carrname = 'Not favourite carrier, but first'.
    APPEND ls_scarr TO lt_scarr.

    ls_scarr-carrid = 20.
    ls_scarr-carrname = 'The favourite carr'.
    APPEND ls_scarr TO lt_scarr.

    mo_test_environment->insert_test_data( lt_scarr ).

    ls_sflight-carrid = 10.
    ls_sflight-connid = 100.
    ls_sflight-fldate = '20210101'.
    APPEND ls_sflight TO lt_sflight.

    ls_sflight-carrid = 20.
    ls_sflight-connid = 200.
    ls_sflight-fldate = '20211202'.
    APPEND ls_sflight TO lt_sflight.

    ls_sflight-carrid = 20.
    ls_sflight-connid = 300.
    ls_sflight-fldate = '20211203'.
    APPEND ls_sflight TO lt_sflight.

    mo_test_environment->insert_test_data( lt_sflight ).

    ls_sbook-carrid   = 10.
    ls_sbook-connid   = 100.
    ls_sbook-fldate   = '20210101'.
    ls_sbook-bookid   = 1000.
    ls_sbook-customid = 1.
    APPEND ls_sbook TO lt_sbook.

    ls_sbook-carrid   = 20.
    ls_sbook-connid   = 200.
    ls_sbook-fldate   = '20211202'.
    ls_sbook-bookid   = 2000.
    ls_sbook-customid = 1.
    APPEND ls_sbook TO lt_sbook.

    ls_sbook-carrid   = 20.
    ls_sbook-connid   = 300.
    ls_sbook-fldate   = '20211203'.
    ls_sbook-bookid   = 3000.
    ls_sbook-customid = 1.
    APPEND ls_sbook TO lt_sbook.

    mo_test_environment->insert_test_data( lt_sbook ).

    " WHEN
    DATA: lt_result TYPE lcl_model=>ty_data,
          ls_result LIKE LINE OF lt_result.

    f_cut->read_data( lt_customid_range ).
    lt_result = f_cut->get_data( ).
    READ TABLE lt_result INDEX 1 INTO ls_result.

    " THEN
    cl_aunit_assert=>assert_equals( act = ls_result-name_of_favourite_carrier
                                    exp = 'The favourite carr' ).

    cl_aunit_assert=>assert_equals( act = ls_result-num_flight_favourite_carrier
                                    exp = 2 ).
  ENDMETHOD.
ENDCLASS.
