
CLASS ltc_get_list_of_select_tables DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_Get_List_Of_Select_Tables
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ZOSQL_PARSER_HELPER
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
    DATA:
      f_cut TYPE REF TO zcl_zosql_parser_helper.  "class under test

    METHODS: one_table_no_alias FOR TESTING RAISING zcx_zosql_error,
      one_table_with_alias FOR TESTING RAISING zcx_zosql_error,
      join FOR TESTING RAISING zcx_zosql_error,
      with_subquery FOR TESTING RAISING zcx_zosql_error,
      _run_method_and_get_result IMPORTING iv_sql             TYPE clike
                                 RETURNING VALUE(rt_datasets) TYPE zcl_zosql_iterator_position=>ty_data_sets
                                 RAISING   zcx_zosql_error.
ENDCLASS.       "ltc_Get_List_Of_Select_Tables

CLASS ltc_get_delete_table_name DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.

    METHODS: simple_delete FOR TESTING RAISING zcx_zosql_error,
      with_where FOR TESTING RAISING zcx_zosql_error,
      error_when_no_from_keyword FOR TESTING RAISING zcx_zosql_error.

  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_zosql_parser_helper.  "class under test

    METHODS: _parameterized_test IMPORTING iv_sql              TYPE clike
                                           iv_expected_tabname TYPE clike
                                 RAISING   zcx_zosql_error.
ENDCLASS.

CLASS ltc_is_select DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS: really_select FOR TESTING RAISING zcx_zosql_error,
             in_fact_delete FOR TESTING RAISING zcx_zosql_error.

  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_zosql_parser_helper.  "class under test

    METHODS: _parameterized_test IMPORTING iv_sql             TYPE clike
                                           iv_expected_result TYPE abap_bool
                                 RAISING   zcx_zosql_error.
ENDCLASS.

CLASS ltc_get_list_of_select_tables IMPLEMENTATION.

  METHOD one_table_no_alias.
    DATA: lv_select TYPE string.

    " GIVEN
    CONCATENATE 'SELECT *'
      'FROM table1'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result TYPE zcl_zosql_iterator_position=>ty_data_sets.
    lt_result = _run_method_and_get_result( lv_select ).

    " THEN
    DATA: lt_etalon TYPE zcl_zosql_iterator_position=>ty_data_sets,
          ls_etalon TYPE zcl_zosql_iterator_position=>ty_data_set.

    ls_etalon-dataset_name = 'TABLE1'.
    APPEND ls_etalon TO lt_etalon.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_etalon ).
  ENDMETHOD.

  METHOD one_table_with_alias.
    DATA: lv_select TYPE string.

    " GIVEN
    CONCATENATE 'SELECT *'
      'FROM table1 as t1'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result TYPE zcl_zosql_iterator_position=>ty_data_sets.
    lt_result = _run_method_and_get_result( lv_select ).

    " THEN
    DATA: lt_etalon TYPE zcl_zosql_iterator_position=>ty_data_sets,
          ls_etalon TYPE zcl_zosql_iterator_position=>ty_data_set.

    ls_etalon-dataset_name  = 'TABLE1'.
    ls_etalon-dataset_alias = 't1'.
    APPEND ls_etalon TO lt_etalon.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_etalon ).
  ENDMETHOD.

  METHOD join.
    DATA: lv_select TYPE string.

    " GIVEN
    CONCATENATE 'SELECT *'
      'FROM TABLE1 as T1'
      'JOIN TABLE2 as T2 ON t2~field1 = t1~field1'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result TYPE zcl_zosql_iterator_position=>ty_data_sets.
    lt_result = _run_method_and_get_result( lv_select ).

    " THEN
    DATA: lt_etalon TYPE zcl_zosql_iterator_position=>ty_data_sets,
          ls_etalon TYPE zcl_zosql_iterator_position=>ty_data_set.

    ls_etalon-dataset_name  = 'TABLE1'.
    ls_etalon-dataset_alias = 'T1'.
    APPEND ls_etalon TO lt_etalon.

    ls_etalon-dataset_name  = 'TABLE2'.
    ls_etalon-dataset_alias = 'T2'.
    APPEND ls_etalon TO lt_etalon.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_etalon ).
  ENDMETHOD.

  METHOD with_subquery.
    DATA: lv_select TYPE string.

    " GIVEN
    CONCATENATE 'SELECT *'
      'FROM table1 as t1'
      'WHERE t1~field1 = '
      '  ( SELECT field2'
      '      FROM table2'
      '      WHERE field2 = t1~field1 )'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result TYPE zcl_zosql_iterator_position=>ty_data_sets.
    lt_result = _run_method_and_get_result( lv_select ).

    " THEN
    DATA: lt_etalon TYPE zcl_zosql_iterator_position=>ty_data_sets,
          ls_etalon TYPE zcl_zosql_iterator_position=>ty_data_set.

    ls_etalon-dataset_name  = 'TABLE1'.
    ls_etalon-dataset_alias = 't1'.
    APPEND ls_etalon TO lt_etalon.

    CLEAR ls_etalon.
    ls_etalon-dataset_name  = 'TABLE2'.
    APPEND ls_etalon TO lt_etalon.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_etalon ).
  ENDMETHOD.

  METHOD _run_method_and_get_result.
    DATA: lo_sql_parser TYPE REF TO zcl_zosql_parser_recurs_desc.

    CREATE OBJECT lo_sql_parser.
    lo_sql_parser->set_sql( iv_sql ).
    lo_sql_parser->run_recursive_descent_parser( ).

    CREATE OBJECT f_cut
      EXPORTING
        io_sql_parser = lo_sql_parser.

    rt_datasets = f_cut->get_list_of_select_from_tables( ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_get_delete_table_name IMPLEMENTATION.
  METHOD simple_delete.
    _parameterized_test( iv_sql              = 'DELETE FROM zosql_for_tst'
                         iv_expected_tabname = 'ZOSQL_FOR_TST' ).
  ENDMETHOD.

  METHOD with_where.

    DATA: lv_delete TYPE string.

    CONCATENATE
      'DELETE FROM zosql_for_tst2'
      '  WHERE key_field = ''test'''
      INTO lv_delete SEPARATED BY space.

    _parameterized_test( iv_sql              = lv_delete
                         iv_expected_tabname = 'ZOSQL_FOR_TST2' ).
  ENDMETHOD.

  METHOD error_when_no_from_keyword.

    TRY.
        _parameterized_test( iv_sql              = 'DELETE zosql_for_tst'
                             iv_expected_tabname = '##DOES NOT MATTER' ).

        cl_aunit_assert=>fail( 'Exception should be raised in case of incorrect DELETE sql statement' ).
      CATCH zcx_zosql_error.
    ENDTRY.
  ENDMETHOD.

  METHOD _parameterized_test.
    DATA: lo_sql_parser          TYPE REF TO zcl_zosql_parser_recurs_desc,
          lv_result_table_name   TYPE string,
          lv_expected_table_name TYPE string.

    CREATE OBJECT lo_sql_parser.
    lo_sql_parser->set_sql( iv_sql ).
    lo_sql_parser->run_recursive_descent_parser( ).

    CREATE OBJECT f_cut
      EXPORTING
        io_sql_parser = lo_sql_parser.

    lv_result_table_name = zcl_zosql_utils=>to_upper_case( f_cut->get_delete_table_name( ) ).
    lv_expected_table_name = zcl_zosql_utils=>to_upper_case( iv_expected_tabname ).

    cl_aunit_assert=>assert_equals( act = lv_result_table_name exp = lv_expected_table_name ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_is_select IMPLEMENTATION.
  METHOD really_select.
    _parameterized_test( iv_sql             = 'SELECT * FROM zosql_for_tst'
                         iv_expected_result = abap_true ).
  ENDMETHOD.

  METHOD in_fact_delete.
    _parameterized_test( iv_sql             = 'DELETE FROM zosql_for_tst'
                         iv_expected_result = abap_false ).
  ENDMETHOD.

  METHOD _parameterized_test.
    DATA: lo_sql_parser          TYPE REF TO zcl_zosql_parser_recurs_desc,
          lv_result_table_name   TYPE string,
          lv_expected_table_name TYPE string.

    CREATE OBJECT lo_sql_parser.
    lo_sql_parser->set_sql( iv_sql ).
    lo_sql_parser->run_recursive_descent_parser( ).

    DATA: lv_result TYPE abap_bool.

    CREATE OBJECT f_cut
      EXPORTING
        io_sql_parser = lo_sql_parser.

    lv_result = f_cut->is_select( ).

    cl_aunit_assert=>assert_equals( act = lv_result exp = iv_expected_result ).
  ENDMETHOD.
ENDCLASS.
