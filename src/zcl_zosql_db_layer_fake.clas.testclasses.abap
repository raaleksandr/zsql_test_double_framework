INCLUDE zosql_test_cases_db_layer.

CLASS lcl_base_for_units_of_fake DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM zcl_zosql_unitbase.

  PROTECTED SECTION.
    CLASS-DATA: mo_test_environment TYPE REF TO zif_zosql_test_environment.

    DATA:
      f_cut TYPE REF TO zcl_zosql_db_layer_fake.  "class under test

    METHODS: insert_test_data REDEFINITION.

  PRIVATE SECTION.

    CLASS-METHODS: class_setup.

    METHODS: setup.
ENDCLASS.

CLASS ltc_cases_for_select_fake DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM ltc_cases_for_select.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_cases_for_select
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ZOSQL_DB_LAYER_FAKE
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PROTECTED SECTION.
    METHODS: insert_test_data REDEFINITION.
  PRIVATE SECTION.
    DATA: mo_test_environment TYPE REF TO zif_zosql_test_environment.

    METHODS: setup.
ENDCLASS.       "ltc_cases_for_select

CLASS ltc_cases_for_insert_fake DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM ltc_cases_for_insert.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_cases_for_insert
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ZOSQL_DB_LAYER_FAKE
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>

  PROTECTED SECTION.
    METHODS: insert_test_data REDEFINITION.
  PRIVATE SECTION.
    DATA: mo_test_environment TYPE REF TO zif_zosql_test_environment.

    METHODS: setup.
ENDCLASS.

CLASS ltc_cases_for_update_fake DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM ltc_cases_for_update.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_cases_for_update
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ZOSQL_DB_LAYER_FAKE
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PROTECTED SECTION.
    METHODS: insert_test_data REDEFINITION.
  PRIVATE SECTION.
    DATA: mo_test_environment TYPE REF TO zif_zosql_test_environment.

    METHODS: setup.
ENDCLASS.

CLASS ltc_cases_for_modify_fake DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM ltc_cases_for_modify.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_cases_for_modify
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ZOSQL_DB_LAYER_FAKE
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PROTECTED SECTION.
    METHODS: insert_test_data REDEFINITION.
  PRIVATE SECTION.
    DATA: mo_test_environment TYPE REF TO zif_zosql_test_environment.

    METHODS: setup.
ENDCLASS.

CLASS ltc_cases_for_delete DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM lcl_base_for_units_of_fake.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_cases_for_delete
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ZOSQL_DB_LAYER_FAKE
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PUBLIC SECTION.
    METHODS: delete_by_itab FOR TESTING RAISING zcx_zosql_error,
      by_sql_no_params FOR TESTING RAISING zcx_zosql_error,
      by_sql_with_params FOR TESTING RAISING zcx_zosql_error,
      delete_by_itab_subrc_4 FOR TESTING RAISING zcx_zosql_error,
      delete_by_sql_subrc_4 FOR TESTING RAISING zcx_zosql_error.
ENDCLASS.

CLASS lcl_base_for_units_of_fake IMPLEMENTATION.

  METHOD class_setup.
    mo_test_environment = zcl_zosql_test_environment=>create( ).
  ENDMETHOD.

  METHOD setup.
    mo_test_environment->clear_doubles( ).
    f_cut ?= mo_test_environment->get_db_layer_for_unit_tests( ).
  ENDMETHOD.

  METHOD insert_test_data.
    mo_test_environment->insert_test_data( it_table      = it_table
                                           iv_table_name = iv_table_name ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_select_fake IMPLEMENTATION.
  METHOD setup.
    mo_test_environment = zcl_zosql_test_environment=>create( ).
    f_cut ?= mo_test_environment->get_db_layer_for_unit_tests( ).
  ENDMETHOD.
  METHOD insert_test_data.
    mo_test_environment->insert_test_data( it_table      = it_table
                                           iv_table_name = iv_table_name ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_insert_fake IMPLEMENTATION.
  METHOD setup.
    mo_test_environment = zcl_zosql_test_environment=>create( ).
    f_cut ?= mo_test_environment->get_db_layer_for_unit_tests( ).
  ENDMETHOD.
  METHOD insert_test_data.
    mo_test_environment->insert_test_data( it_table      = it_table
                                           iv_table_name = iv_table_name ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_update_fake IMPLEMENTATION.
  METHOD setup.
    mo_test_environment = zcl_zosql_test_environment=>create( ).
    f_cut ?= mo_test_environment->get_db_layer_for_unit_tests( ).
  ENDMETHOD.
  METHOD insert_test_data.
    mo_test_environment->insert_test_data( it_table      = it_table
                                           iv_table_name = iv_table_name ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_modify_fake IMPLEMENTATION.
  METHOD setup.
    mo_test_environment = zcl_zosql_test_environment=>create( ).
    f_cut ?= mo_test_environment->get_db_layer_for_unit_tests( ).
  ENDMETHOD.
  METHOD insert_test_data.
    mo_test_environment->insert_test_data( it_table      = it_table
                                           iv_table_name = iv_table_name ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_delete IMPLEMENTATION.
  METHOD delete_by_itab.
    DATA: ls_line                TYPE zosql_for_tst,
          lt_table_before_modify TYPE TABLE OF zosql_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_modify.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_modify.

    insert_test_data( it_table = lt_table_before_modify ).

    " WHEN
    DATA: lt_delete_table TYPE TABLE OF zosql_for_tst.

    CLEAR ls_line.
    ls_line-key_field = 'KEY2'.
    APPEND ls_line TO lt_delete_table.

    f_cut->zif_zosql_db_layer~delete_by_itab( iv_table_name       = 'ZOSQL_FOR_TST'
                                              it_lines_for_delete = lt_delete_table ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
                                              IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD by_sql_no_params.
    DATA: ls_line                TYPE zosql_for_tst,
          lt_table_before_delete TYPE TABLE OF zosql_for_tst,
          lv_delete_statement    TYPE string.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_delete.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_delete.

    insert_test_data( lt_table_before_delete ).

    " WHEN
    CONCATENATE 'DELETE FROM ZOSQL_FOR_TST'
      'WHERE TEXT_FIELD1 = ''VALUE2_1'''
      INTO lv_delete_statement SEPARATED BY space.

    f_cut->zif_zosql_db_layer~delete( lv_delete_statement ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
                                              IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    CLEAR ls_line.
    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD by_sql_with_params.
    DATA: ls_line                TYPE zosql_for_tst,
          lt_table_before_delete TYPE TABLE OF zosql_for_tst,
          lv_delete_statement    TYPE string.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_delete.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_delete.

    insert_test_data( lt_table_before_delete ).

    " WHEN
    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param.

    ls_param-param_name_in_select   = ':TEXT_FIELD1'.
    ls_param-parameter_value_single = 'VALUE2_1'.
    APPEND ls_param TO lt_params.

    CONCATENATE 'DELETE FROM ZOSQL_FOR_TST'
      'WHERE TEXT_FIELD1 = :TEXT_FIELD1'
      INTO lv_delete_statement SEPARATED BY space.

    f_cut->zif_zosql_db_layer~delete( iv_delete_statement = lv_delete_statement
                                      it_parameters       = lt_params ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
                                              IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    CLEAR ls_line.
    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD delete_by_itab_subrc_4.
    DATA: ls_line         TYPE zosql_for_tst,
          lt_delete_table TYPE TABLE OF zosql_for_tst,
          lv_subrc        TYPE sysubrc.

    " WHEN
    CLEAR ls_line.
    ls_line-key_field = 'KEY2'.
    APPEND ls_line TO lt_delete_table.

    lv_subrc = f_cut->zif_zosql_db_layer~delete_by_itab( iv_table_name       = 'ZOSQL_FOR_TST'
                                                         it_lines_for_delete = lt_delete_table ).

    " THEN
    cl_aunit_assert=>assert_equals( act = lv_subrc exp = 4 ).
  ENDMETHOD.

  METHOD delete_by_sql_subrc_4.
    DATA: ls_line             TYPE zosql_for_tst,
          lv_delete_statement TYPE string,
          lv_subrc            TYPE sysubrc.

    " WHEN
    CONCATENATE 'DELETE FROM ZOSQL_FOR_TST'
      'WHERE TEXT_FIELD1 = ''VALUE2_1'''
      INTO lv_delete_statement SEPARATED BY space.

    lv_subrc = f_cut->zif_zosql_db_layer~delete( lv_delete_statement ).

    " THEN
    cl_aunit_assert=>assert_equals( exp = lv_subrc act = 4 ).
  ENDMETHOD.
ENDCLASS.
