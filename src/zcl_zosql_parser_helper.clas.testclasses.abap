
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

    METHODS: one_table_no_alias FOR TESTING,
             one_table_with_alias FOR TESTING,
             join FOR TESTING,
             with_subquery FOR TESTING,
      _run_method_and_get_result IMPORTING iv_sql             TYPE clike
                                 RETURNING VALUE(rt_datasets) TYPE zcl_zosql_iterator_position=>ty_data_sets.
ENDCLASS.       "ltc_Get_List_Of_Select_Tables


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
