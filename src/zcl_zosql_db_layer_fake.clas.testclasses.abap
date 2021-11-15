
CLASS lcl_base_for_units_of_fake DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM zcl_testable_db_layer_unitbase.

  PROTECTED SECTION.
    CLASS-DATA: mo_test_environment TYPE REF TO zif_zosql_test_environment.

    DATA:
      f_cut TYPE REF TO zcl_zosql_db_layer_fake.  "class under test

  PRIVATE SECTION.

    CLASS-METHODS: class_setup.

    METHODS: setup.
ENDCLASS.

CLASS ltc_cases_for_select DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM lcl_base_for_units_of_fake.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_cases_for_select
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_TESTABLE_DB_READER_FAKE
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

    METHODS: one_table_no_conditions FOR TESTING RAISING zcx_testable_db_layer,
      one_table_where_eq FOR TESTING RAISING zcx_testable_db_layer,
      one_table_where_eq_tabname FOR TESTING RAISING zcx_testable_db_layer,
      one_table_where_param_eq FOR TESTING RAISING zcx_testable_db_layer,
      one_table_where_param_range FOR TESTING RAISING zcx_testable_db_layer,
      one_table_where_param_ref FOR TESTING RAISING zcx_testable_db_layer,
      one_table_2_params_with_or FOR TESTING RAISING zcx_testable_db_layer,
      one_table_for_all_entries FOR TESTING RAISING zcx_testable_db_layer,
      one_table_group_by FOR TESTING RAISING zcx_testable_db_layer,
      one_table_distinct FOR TESTING RAISING zcx_testable_db_layer,
      operator_like FOR TESTING RAISING zcx_testable_db_layer,
      join FOR TESTING RAISING zcx_testable_db_layer,
      left_join FOR TESTING RAISING zcx_testable_db_layer,
      new_syntax FOR TESTING RAISING zcx_testable_db_layer,
      new_syntax_no_host_var FOR TESTING RAISING zcx_testable_db_layer,
      view_no_conditions FOR TESTING RAISING zcx_testable_db_layer,
      view_with_condition FOR TESTING RAISING zcx_testable_db_layer,
      view_with_several_conditions FOR TESTING RAISING zcx_testable_db_layer,
      as_ref_to_data FOR TESTING RAISING zcx_testable_db_layer,
      select_without_corresponding FOR TESTING RAISING zcx_testable_db_layer,
      select_into_tab_with_string FOR TESTING RAISING zcx_testable_db_layer,
      select_into_tab_no_struct FOR TESTING RAISING zcx_testable_db_layer,
      select_one_line FOR TESTING RAISING zcx_testable_db_layer,
      select_up_to_n_rows FOR TESTING RAISING zcx_testable_db_layer,
      select_up_to_n_rows_prm FOR TESTING RAISING zcx_testable_db_layer,
      select_single FOR TESTING RAISING zcx_testable_db_layer,
      select_with_empty_range FOR TESTING RAISING zcx_testable_db_layer,
      open_cursor_fetch_itab FOR TESTING RAISING zcx_testable_db_layer,
      open_cursor_fetch FOR TESTING RAISING zcx_testable_db_layer,
      where_with_brackets FOR TESTING RAISING zcx_testable_db_layer,
      where_or_inside_brackets FOR TESTING RAISING zcx_testable_db_layer,
      not_simple FOR TESTING RAISING zcx_testable_db_layer,
      not_and_brackets FOR TESTING RAISING zcx_testable_db_layer,
      bug_join_07_10_2021 FOR TESTING RAISING zcx_testable_db_layer,
      where_value_with_spaces FOR TESTING RAISING zcx_testable_db_layer,
      group_by_with_count_star FOR TESTING RAISING zcx_testable_db_layer,
      group_by_with_tabname FOR TESTING RAISING zcx_testable_db_layer,
      group_by_lower_case_alias FOR TESTING RAISING zcx_testable_db_layer,
      for_all_ent_compare_2_fld FOR TESTING RAISING zcx_testable_db_layer.
ENDCLASS.       "ltc_cases_for_select

CLASS ltc_cases_for_insert DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM lcl_base_for_units_of_fake.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_cases_for_insert
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_TESTABLE_DB_READER_FAKE
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
    METHODS: insert_by_itab FOR TESTING RAISING zcx_testable_db_layer.
ENDCLASS.

CLASS ltc_cases_for_update DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM lcl_base_for_units_of_fake.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_cases_for_update
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_TESTABLE_DB_READER_FAKE
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
    METHODS: update_by_itab FOR TESTING RAISING zcx_testable_db_layer,
      by_sql_no_params FOR TESTING RAISING zcx_testable_db_layer,
      by_sql_with_params FOR TESTING RAISING zcx_testable_db_layer,
      by_sql_params_set FOR TESTING RAISING zcx_testable_db_layer,
      by_sql_new_syntax FOR TESTING RAISING zcx_testable_db_layer.
ENDCLASS.

CLASS ltc_cases_for_modify DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM lcl_base_for_units_of_fake.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_cases_for_modify
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_TESTABLE_DB_READER_FAKE
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
    METHODS: modify_by_itab FOR TESTING RAISING zcx_testable_db_layer.
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
*?<OBJECT_UNDER_TEST>ZCL_TESTABLE_DB_READER_FAKE
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
    METHODS: delete_by_itab FOR TESTING RAISING zcx_testable_db_layer,
      by_sql_no_params FOR TESTING RAISING zcx_testable_db_layer,
      by_sql_with_params FOR TESTING RAISING zcx_testable_db_layer.
ENDCLASS.

CLASS lcl_base_for_units_of_fake IMPLEMENTATION.

  METHOD class_setup.
    mo_test_environment = zcl_zosql_test_environment=>create( ).
  ENDMETHOD.

  METHOD setup.
    mo_test_environment->clear_doubles( ).
    f_cut ?= mo_test_environment->get_db_layer_for_unit_tests( ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_select IMPLEMENTATION.

  METHOD one_table_no_conditions.

    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM zdblayr_for_tst'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst,
          ls_result_line  TYPE zdblayr_for_tst,
          lv_subrc        TYPE sysubrc.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table
                                                        es_result_line  = ls_result_line
                                                        ev_subrc        = lv_subrc ).

    " THEN
    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_initial_table msg = 'ET_RESULT_TABLE' ).

    DATA: ls_expected_line TYPE zdblayr_for_tst.
    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'VALUE1_1'.
    ls_expected_line-text_field2 = 'VALUE1_2'.

    cl_aunit_assert=>assert_equals( act = ls_result_line exp = ls_expected_line msg = 'ES_RESULT_LINE' ).
    cl_aunit_assert=>assert_equals( act = lv_subrc exp = 0 msg = 'EV_SUBRC' ).
  ENDMETHOD.

  METHOD one_table_where_eq.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM zdblayr_for_tst'
      'WHERE KEY_FIELD = ''KEY2'''
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zdblayr_for_tst,
          ls_expected_line  TYPE zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD one_table_where_eq_tabname.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst,
          lv_select        TYPE string.

    " Differs from previous case in providing table name in WHERE

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM zdblayr_for_tst'
      'WHERE ZDBLAYR_FOR_TST~KEY_FIELD = ''KEY2'''
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zdblayr_for_tst,
          ls_expected_line  TYPE zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD one_table_where_param_eq.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zdblayr_for_tst'
      'WHERE KEY_FIELD = :KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    ls_param-param_name_in_select = ':KEY_FIELD'.
    ls_param-parameter_value_single = 'KEY2'.
    APPEND ls_param TO lt_params.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                                        it_parameters   = lt_params
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zdblayr_for_tst,
          ls_expected_line  TYPE zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD one_table_where_param_range.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'VALUE3_1'.
    ls_line-text_field2 = 'VALUE3_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select  TYPE string.

    DATA: lt_params           TYPE zosql_db_layer_params,
          ls_param            TYPE zosql_db_layer_param,
          ls_param_range_line TYPE zosql_db_layer_range_line.

    ls_param-param_name_in_select = ':TEXT_FIELD'.
    ls_param_range_line-sign = 'I'.
    ls_param_range_line-option = 'EQ'.
    ls_param_range_line-low    = 'VALUE1_1'.
    APPEND ls_param_range_line TO ls_param-parameter_value_range.

    ls_param_range_line-low    = 'VALUE3_1'.
    APPEND ls_param_range_line TO ls_param-parameter_value_range.
    APPEND ls_param TO lt_params.

    CONCATENATE 'SELECT *'
      'FROM zdblayr_for_tst'
      'WHERE TEXT_FIELD1 IN :TEXT_FIELD'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                                        it_parameters   = lt_params
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zdblayr_for_tst,
          ls_expected_line  TYPE zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'VALUE1_1'.
    ls_expected_line-text_field2 = 'VALUE1_2'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY3'.
    ls_expected_line-text_field1 = 'VALUE3_1'.
    ls_expected_line-text_field2 = 'VALUE3_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD one_table_where_param_ref.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    DATA: lt_params     TYPE zosql_db_layer_params,
          ls_param      TYPE zosql_db_layer_param,
          lv_select     TYPE string,
          lv_some_value TYPE text50.

    ls_param-param_name_in_select   = ':KEY_FIELD'.
    lv_some_value = 'KEY2'.
    GET REFERENCE OF lv_some_value INTO ls_param-parameter_value_ref.
    APPEND ls_param TO lt_params.

    CONCATENATE 'SELECT *'
      'FROM zdblayr_for_tst'
      'WHERE KEY_FIELD = :KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                                        it_parameters   = lt_params
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zdblayr_for_tst,
          ls_expected_line  TYPE zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD one_table_2_params_with_or.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'VALUE3_1'.
    ls_line-text_field2 = 'VALUE3_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    DATA: lt_params           TYPE zosql_db_layer_params,
          ls_param            TYPE zosql_db_layer_param,
          ls_param_range_line TYPE zosql_db_layer_range_line,
          lv_select           TYPE string.

    ls_param-param_name_in_select   = ':TEXT_FIELD'.

    ls_param_range_line-sign = 'I'.
    ls_param_range_line-option = 'CP'.
    ls_param_range_line-low    = '*2*'.
    APPEND ls_param_range_line TO ls_param-parameter_value_range.

    APPEND ls_param TO lt_params.

    CLEAR ls_param.
    ls_param-param_name_in_select = ':KEY_FIELD'.
    ls_param-parameter_value_single = 'KEY1'.
    APPEND ls_param TO lt_params.

    CONCATENATE 'SELECT *'
      'FROM zdblayr_for_tst'
      'WHERE TEXT_FIELD1 IN :TEXT_FIELD OR KEY_FIELD = :KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                                        it_parameters   = lt_params
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'VALUE1_1'.
    ls_expected_line-text_field2 = 'VALUE1_2'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD one_table_for_all_entries.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst.

    " Check if for all entries works

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'VALUE3_1'.
    ls_line-text_field2 = 'VALUE3_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    DATA: lt_base_table           TYPE TABLE OF zdblayr_for_tst,
          ls_line_for_all_entries TYPE zdblayr_for_tst.

    ls_line_for_all_entries-key_field = 'KEY2'.
    APPEND ls_line_for_all_entries TO lt_base_table.

    ls_line_for_all_entries-key_field = 'KEY3'.
    APPEND ls_line_for_all_entries TO lt_base_table.

    DATA: lv_select TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zdblayr_for_tst'
      'FOR ALL ENTRIES IN lt_base_table'
      'WHERE KEY_FIELD = LT_BASE_TABLE-KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                  = lv_select
                                                        it_for_all_entries_table   = lt_base_table
                                              IMPORTING et_result_table            = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-key_field   = 'KEY3'.
    ls_expected_line-text_field1 = 'VALUE3_1'.
    ls_expected_line-text_field2 = 'VALUE3_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD one_table_group_by.
    DATA: ls_line          TYPE zdblayr_for_tst2,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst2.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_1'.
    ls_line-amount      = 10.
    ls_line-qty         = 2.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_2'.
    ls_line-amount      = 20.
    ls_line-qty         = 4.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-key_field2  = 'KEY2_1'.
    ls_line-amount      = 5.
    ls_line-qty         = 1.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select   TYPE string.

    CONCATENATE 'SELECT key_field sum( amount ) as amount sum( qty ) as qty'
      'FROM zdblayr_for_tst2'
      'GROUP BY key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst2.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                  = lv_select
                                              IMPORTING et_result_table            = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tst2,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst2.

    ls_expected_line-key_field = 'KEY1'.
    ls_expected_line-amount    = 30.
    ls_expected_line-qty       = 6.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-key_field = 'KEY2'.
    ls_expected_line-amount    = 5.
    ls_expected_line-qty       = 1.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD one_table_distinct.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'SOME VALUE'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'SOME VALUE'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'SOME VALUE2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst,
          lv_select       TYPE string.

    CONCATENATE 'SELECT DISTINCT TEXT_FIELD1'
      'FROM ZDBLAYR_FOR_TST'
      'ORDER BY TEXT_FIELD1'
      INTO lv_select SEPARATED BY space.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                  = lv_select
                                              IMPORTING et_result_table            = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    ls_expected_line-text_field1 = 'SOME VALUE'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-text_field1 = 'SOME VALUE2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD operator_like.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'AVALUE3_1'.
    ls_line-text_field2 = 'VALUE3_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select           TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zdblayr_for_tst'
      'WHERE TEXT_FIELD1 LIKE ''A%'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY3'.
    ls_expected_line-text_field1 = 'AVALUE3_1'.
    ls_expected_line-text_field2 = 'VALUE3_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD join.

    DATA: lt_table1_initial TYPE TABLE OF zdblayr_for_tst,
          ls_line1_initial  TYPE zdblayr_for_tst,
          lt_table2_initial TYPE TABLE OF zdblayr_for_tst2,
          ls_line2_initial  TYPE zdblayr_for_tst2.

    " GIVEN

    " Table 1
    ls_line1_initial-key_field   = 'KEY1'.
    ls_line1_initial-text_field1 = 'TEXT1_1'.
    APPEND ls_line1_initial TO lt_table1_initial.

    ls_line1_initial-key_field   = 'KEY2'.
    ls_line1_initial-text_field1 = 'TEXT2_1'.
    APPEND ls_line1_initial TO lt_table1_initial.

    ls_line1_initial-key_field   = 'KEY3'.
    ls_line1_initial-text_field1 = 'TEXT3_1'.
    APPEND ls_line1_initial TO lt_table1_initial.

    mo_test_environment->insert_test_data( it_table = lt_table1_initial ).

    " Table 2
    ls_line2_initial-key_field = 'KEY1'.
    ls_line2_initial-key_field2 = 'KEY1_1'.
    APPEND ls_line2_initial TO lt_table2_initial.

    ls_line2_initial-key_field = 'KEY1'.
    ls_line2_initial-key_field2 = 'KEY1_2'.
    APPEND ls_line2_initial TO lt_table2_initial.

    ls_line2_initial-key_field = 'KEY2'.
    ls_line2_initial-key_field2 = 'KEY2_1'.
    APPEND ls_line2_initial TO lt_table2_initial.

    mo_test_environment->insert_test_data( it_table = lt_table2_initial ).

    DATA: lv_select TYPE string,
          lv_from   TYPE string.

    CONCATENATE 'SELECT t1~key_field t2~key_field2'
      'FROM zdblayr_for_tst as t1 JOIN zdblayr_for_tst2 as t2 ON t1~key_field = t2~key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result_line,
             key_field  TYPE zdblayr_for_tst-key_field,
             key_field2 TYPE zdblayr_for_tst2-key_field2,
           END OF ty_result_line.

    DATA: lt_result_table TYPE TABLE OF ty_result_line.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table ).
    " THEN
    DATA: ls_expected_line  TYPE ty_result_line,
          lt_expected_table TYPE TABLE OF ty_result_line.

    ls_expected_line-key_field  = 'KEY1'.
    ls_expected_line-key_field2 = 'KEY1_1'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-key_field  = 'KEY1'.
    ls_expected_line-key_field2 = 'KEY1_2'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-key_field  = 'KEY2'.
    ls_expected_line-key_field2 = 'KEY2_1'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD left_join.
    DATA: lt_table1_initial TYPE TABLE OF zdblayr_for_tst,
          ls_line1_initial  TYPE zdblayr_for_tst,
          lt_table2_initial TYPE TABLE OF zdblayr_for_tst2,
          ls_line2_initial  TYPE zdblayr_for_tst2.

    " GIVEN

    " Table 1
    ls_line1_initial-key_field   = 'KEY1'.
    ls_line1_initial-text_field1 = 'TEXT1_1'.
    APPEND ls_line1_initial TO lt_table1_initial.

    ls_line1_initial-key_field   = 'KEY2'.
    ls_line1_initial-text_field1 = 'TEXT2_1'.
    APPEND ls_line1_initial TO lt_table1_initial.

    ls_line1_initial-key_field   = 'KEY3'.
    ls_line1_initial-text_field1 = 'TEXT3_1'.
    APPEND ls_line1_initial TO lt_table1_initial.

    mo_test_environment->insert_test_data( it_table = lt_table1_initial ).

    " Table 2
    ls_line2_initial-key_field = 'KEY1'.
    ls_line2_initial-key_field2 = 'KEY1_1'.
    APPEND ls_line2_initial TO lt_table2_initial.

    ls_line2_initial-key_field = 'KEY1'.
    ls_line2_initial-key_field2 = 'KEY1_2'.
    APPEND ls_line2_initial TO lt_table2_initial.

    ls_line2_initial-key_field = 'KEY2'.
    ls_line2_initial-key_field2 = 'KEY2_1'.
    APPEND ls_line2_initial TO lt_table2_initial.

    mo_test_environment->insert_test_data( it_table = lt_table2_initial ).

    DATA: lv_select TYPE string,
          lv_from   TYPE string.

    CONCATENATE 'SELECT t1~key_field t2~key_field2'
      'FROM zdblayr_for_tst as t1 LEFT JOIN zdblayr_for_tst2 as t2 ON t1~key_field = t2~key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result_line,
             key_field  TYPE zdblayr_for_tst-key_field,
             key_field2 TYPE zdblayr_for_tst2-key_field2,
           END OF ty_result_line.

    DATA: lt_result_table TYPE TABLE OF ty_result_line.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table ).
    " THEN
    DATA: ls_expected_line  TYPE ty_result_line,
          lt_expected_table TYPE TABLE OF ty_result_line.

    ls_expected_line-key_field  = 'KEY1'.
    ls_expected_line-key_field2 = 'KEY1_1'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-key_field  = 'KEY1'.
    ls_expected_line-key_field2 = 'KEY1_2'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-key_field  = 'KEY2'.
    ls_expected_line-key_field2 = 'KEY2_1'.
    APPEND ls_expected_line TO lt_expected_table.

    CLEAR ls_expected_line.
    ls_expected_line-key_field  = 'KEY3'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD new_syntax.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

    CONCATENATE 'SELECT key_field, text_field1'
      'FROM zdblayr_for_tst'
      'WHERE KEY_FIELD = @:KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    ls_param-param_name_in_select   = ':KEY_FIELD'.
    ls_param-parameter_value_single = 'KEY2'.
    APPEND ls_param TO lt_params.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                                        it_parameters   = lt_params
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD new_syntax_no_host_var.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

    CONCATENATE 'SELECT key_field, text_field1'
      'FROM zdblayr_for_tst'
      'WHERE KEY_FIELD = :KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    ls_param-param_name_in_select   = ':KEY_FIELD'.
    ls_param-parameter_value_single = 'KEY2'.
    APPEND ls_param TO lt_params.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                                        it_parameters   = lt_params
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD where_with_brackets.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'SOME_TEXT2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zdblayr_for_tst'
      'WHERE ( key_field >= ''KEY2'' AND text_field1 = ''SOME_TEXT1'' ) OR text_field1 <> ''SOME_TEXT1'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY3'.
    ls_expected_line-text_field1 = 'SOME_TEXT2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD where_or_inside_brackets.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY4'.
    ls_line-text_field1 = 'SOME_TEXT2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zdblayr_for_tst'
      'WHERE ( KEY_FIELD = ''KEY1'' OR KEY_FIELD = ''KEY3'' )'
      '  AND TEXT_FIELD1 = ''SOME_TEXT1'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY3'.
    ls_expected_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD not_simple.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'SOME_TEXT2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zdblayr_for_tst'
      'WHERE NOT text_field1 = ''SOME_TEXT2'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                                        it_parameters   = lt_params
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD not_and_brackets.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'SOME_TEXT2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zdblayr_for_tst'
      'WHERE NOT ( text_field1 = ''SOME_TEXT2'' )'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                                        it_parameters   = lt_params
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD as_ref_to_data.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZDBLAYR_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: ld_result       TYPE REF TO data,
          lt_result_table TYPE TABLE OF zdblayr_for_tst,
          lv_subrc        TYPE sysubrc.

    FIELD-SYMBOLS: <lt_result_table> TYPE STANDARD TABLE.

    f_cut->zif_zosql_db_layer~select( EXPORTING iv_select          = lv_select
                                      IMPORTING ed_result_as_table = ld_result
                                                ev_subrc           = lv_subrc ).

    ASSIGN ld_result->* TO <lt_result_table>.
    zcl_testable_db_layer_utils=>move_corresponding_table( EXPORTING it_table_src = <lt_result_table>
                                                           IMPORTING et_table_dest = lt_result_table ).

    " THEN
    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_initial_table ).
    cl_aunit_assert=>assert_equals( act = lv_subrc exp = 0 ).
  ENDMETHOD.

  METHOD select_without_corresponding.

    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT KEY_FIELD TEXT_FIELD1'
      'FROM ZDBLAYR_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             field1 TYPE string,
             field2 TYPE string,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                = lv_select
                                                        iv_do_into_corresponding = abap_false
                                              IMPORTING et_result_table          = lt_result_table ).

    " THEN
    DATA: lt_expected_result TYPE TABLE OF ty_result,
          ls_expected_result TYPE ty_result.

    ls_expected_result-field1 = 'KEY1'.
    ls_expected_result-field2 = 'VALUE1_1'.
    APPEND ls_expected_result TO lt_expected_result.

    ls_expected_result-field1 = 'KEY2'.
    ls_expected_result-field2 = 'VALUE2_1'.
    APPEND ls_expected_result TO lt_expected_result.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_result ).
  ENDMETHOD.

  METHOD select_into_tab_with_string.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT KEY_FIELD TEXT_FIELD1'
      'FROM ZDBLAYR_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field   TYPE string,
             text_field1 TYPE string,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                = lv_select
                                                        iv_do_into_corresponding = abap_false
                                              IMPORTING et_result_table          = lt_result_table ).

    " THEN
    DATA: lt_expected_result TYPE TABLE OF ty_result,
          ls_expected_result TYPE ty_result.

    ls_expected_result-key_field   = 'KEY1'.
    ls_expected_result-text_field1 = 'VALUE1_1'.
    APPEND ls_expected_result TO lt_expected_result.

    ls_expected_result-key_field   = 'KEY2'.
    ls_expected_result-text_field1 = 'VALUE2_1'.
    APPEND ls_expected_result TO lt_expected_result.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_result ).
  ENDMETHOD.

  METHOD select_into_tab_no_struct.

    " Select into table where each line is not a structure

    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT KEY_FIELD'
      'FROM ZDBLAYR_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF string.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                = lv_select
                                                        iv_do_into_corresponding = abap_false
                                              IMPORTING et_result_table          = lt_result_table ).

    " THEN
    DATA: lt_expected_result TYPE TABLE OF string.

    APPEND 'KEY1' TO lt_expected_result.
    APPEND 'KEY2' TO lt_expected_result.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_result ).
  ENDMETHOD.

  METHOD select_one_line.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT KEY_FIELD TEXT_FIELD1'
      'FROM ZDBLAYR_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: ls_result_line TYPE zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                = lv_select
                                              IMPORTING es_result_line           = ls_result_line ).

    " THEN
    DATA: ls_expected_result TYPE zdblayr_for_tst.

    ls_expected_result-key_field   = 'KEY1'.
    ls_expected_result-text_field1 = 'VALUE1_1'.

    cl_aunit_assert=>assert_equals( act = ls_result_line exp = ls_expected_result ).
  ENDMETHOD.

  METHOD select_up_to_n_rows.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'VALUE3_1'.
    ls_line-text_field2 = 'VALUE3_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZDBLAYR_FOR_TST'
      'UP TO 2 ROWS'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst,
          ls_result_line  TYPE zdblayr_for_tst,
          lv_subrc        TYPE sysubrc.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zdblayr_for_tst,
          ls_expected_line  TYPE zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'VALUE1_1'.
    ls_expected_line-text_field2 = 'VALUE1_2'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD select_up_to_n_rows_prm.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'VALUE3_1'.
    ls_line-text_field2 = 'VALUE3_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZDBLAYR_FOR_TST'
      'UP TO :LINES_COUNT ROWS'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst,
          lt_parameters   TYPE zosql_db_layer_params,
          ls_parameter    TYPE zosql_db_layer_param.

    ls_parameter-param_name_in_select   = ':LINES_COUNT'.
    ls_parameter-parameter_value_single = 2.
    APPEND ls_parameter TO lt_parameters.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                                        it_parameters   = lt_parameters
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zdblayr_for_tst,
          ls_expected_line  TYPE zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'VALUE1_1'.
    ls_expected_line-text_field2 = 'VALUE1_2'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD select_single.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT SINGLE *'
      'FROM ZDBLAYR_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst,
          ls_result_line  TYPE zdblayr_for_tst,
          lv_subrc        TYPE sysubrc.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zdblayr_for_tst,
          ls_expected_line  TYPE zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'VALUE1_1'.
    ls_expected_line-text_field2 = 'VALUE1_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD select_with_empty_range.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    " WHEN
    DATA: lt_parameters   TYPE zosql_db_layer_params,
          ls_parameter    LIKE LINE OF lt_parameters,
          lt_result_table TYPE TABLE OF zdblayr_for_tst.

    CONCATENATE 'SELECT *'
      'FROM ZDBLAYR_FOR_TST'
      'WHERE KEY_FIELD IN :KEY_FIELD_RANGE'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    ls_parameter-param_name_in_select = ':KEY_FIELD_RANGE'.
    REFRESH ls_parameter-parameter_value_range.
    APPEND ls_parameter TO lt_parameters.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                                        it_parameters   = lt_parameters
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_initial_table ).
  ENDMETHOD.

  METHOD open_cursor_fetch_itab.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'VALUE3_1'.
    ls_line-text_field2 = 'VALUE3_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY4'.
    ls_line-text_field1 = 'VALUE4_1'.
    ls_line-text_field2 = 'VALUE4_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZDBLAYR_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table1     TYPE TABLE OF zdblayr_for_tst,
          lt_result_table2     TYPE TABLE OF zdblayr_for_tst,
          lt_result_table3     TYPE TABLE OF zdblayr_for_tst,
          lv_subrc_after_call3 TYPE sysubrc,
          lv_cursor            TYPE cursor.

    lv_cursor = f_cut->zif_zosql_db_layer~open_cursor( lv_select ).
    f_cut->zif_zosql_db_layer~fetch_next_cursor_to_itab( EXPORTING iv_cursor       = lv_cursor
                                                                   iv_package_size = 2
                                                         IMPORTING et_result_table = lt_result_table1 ).

    f_cut->zif_zosql_db_layer~fetch_next_cursor_to_itab( EXPORTING iv_cursor       = lv_cursor
                                                                   iv_package_size = 2
                                                         IMPORTING et_result_table = lt_result_table2 ).

    f_cut->zif_zosql_db_layer~fetch_next_cursor_to_itab( EXPORTING iv_cursor       = lv_cursor
                                                                   iv_package_size = 2
                                                         IMPORTING et_result_table = lt_result_table3
                                                                   ev_subrc        = lv_subrc_after_call3 ).

    " THEN
    DATA: lt_expected_table1 TYPE TABLE OF zdblayr_for_tst,
          lt_expected_table2 TYPE TABLE OF zdblayr_for_tst,
          ls_expected_line   TYPE zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'VALUE1_1'.
    ls_expected_line-text_field2 = 'VALUE1_2'.
    APPEND ls_expected_line TO lt_expected_table1.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table1.

    cl_aunit_assert=>assert_equals( act = lt_result_table1 exp = lt_expected_table1 msg = 'Incorrect package 1' ).

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY3'.
    ls_expected_line-text_field1 = 'VALUE3_1'.
    ls_expected_line-text_field2 = 'VALUE3_2'.
    APPEND ls_expected_line TO lt_expected_table2.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY4'.
    ls_expected_line-text_field1 = 'VALUE4_1'.
    ls_expected_line-text_field2 = 'VALUE4_2'.
    APPEND ls_expected_line TO lt_expected_table2.

    cl_aunit_assert=>assert_equals( act = lt_result_table2 exp = lt_expected_table2 msg = 'Incorrect package 2' ).

    cl_aunit_assert=>assert_differs( act = lv_subrc_after_call3 exp = 0 msg = 'Incorrect sy-subrc after call 3 to fetch_next_cursor must be not 0' ).
  ENDMETHOD.

  METHOD open_cursor_fetch.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    ls_line-text_field2 = 'VALUE1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'VALUE2_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'VALUE3_1'.
    ls_line-text_field2 = 'VALUE3_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY4'.
    ls_line-text_field1 = 'VALUE4_1'.
    ls_line-text_field2 = 'VALUE4_2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZDBLAYR_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: ld_result_table1     TYPE REF TO data,
          ld_result_table2     TYPE REF TO data,
          ld_result_table3     TYPE REF TO data,
          lt_result_table1     TYPE TABLE OF zdblayr_for_tst,
          lt_result_table2     TYPE TABLE OF zdblayr_for_tst,
          lv_subrc_after_call3 TYPE sysubrc,
          lv_cursor            TYPE cursor.

    FIELD-SYMBOLS: <lt_result_table1> TYPE STANDARD TABLE,
                   <lt_result_table2> TYPE STANDARD TABLE.

    lv_cursor = f_cut->zif_zosql_db_layer~open_cursor( lv_select ).
    f_cut->zif_zosql_db_layer~fetch_next_cursor( EXPORTING iv_cursor          = lv_cursor
                                                           iv_package_size    = 2
                                                 IMPORTING ed_result_as_table = ld_result_table1 ).
    ASSIGN ld_result_table1->* TO <lt_result_table1>.
    zcl_testable_db_layer_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_result_table1>
                                                           IMPORTING et_table_dest = lt_result_table1 ).

    f_cut->zif_zosql_db_layer~fetch_next_cursor( EXPORTING iv_cursor          = lv_cursor
                                                           iv_package_size    = 2
                                                 IMPORTING ed_result_as_table = ld_result_table2 ).
    ASSIGN ld_result_table2->* TO <lt_result_table2>.
    zcl_testable_db_layer_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_result_table2>
                                                           IMPORTING et_table_dest = lt_result_table2 ).

    f_cut->zif_zosql_db_layer~fetch_next_cursor( EXPORTING iv_cursor          = lv_cursor
                                                           iv_package_size    = 2
                                                 IMPORTING ed_result_as_table = ld_result_table3
                                                           ev_subrc           = lv_subrc_after_call3 ).

    " THEN
    DATA: lt_expected_table1 TYPE TABLE OF zdblayr_for_tst,
          lt_expected_table2 TYPE TABLE OF zdblayr_for_tst,
          ls_expected_line   TYPE zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'VALUE1_1'.
    ls_expected_line-text_field2 = 'VALUE1_2'.
    APPEND ls_expected_line TO lt_expected_table1.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table1.

    cl_aunit_assert=>assert_equals( act = lt_result_table1 exp = lt_expected_table1 msg = 'Incorrect package 1' ).

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY3'.
    ls_expected_line-text_field1 = 'VALUE3_1'.
    ls_expected_line-text_field2 = 'VALUE3_2'.
    APPEND ls_expected_line TO lt_expected_table2.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY4'.
    ls_expected_line-text_field1 = 'VALUE4_1'.
    ls_expected_line-text_field2 = 'VALUE4_2'.
    APPEND ls_expected_line TO lt_expected_table2.

    cl_aunit_assert=>assert_equals( act = lt_result_table2 exp = lt_expected_table2 msg = 'Incorrect package 2' ).

    cl_aunit_assert=>assert_differs( act = lv_subrc_after_call3 exp = 0 msg = 'Incorrect sy-subrc after call 3 to fetch_next_cursor must be not 0' ).
  ENDMETHOD.

  METHOD view_no_conditions.
    DATA: ls_line           TYPE zdblayr_for_tst,
          lt_initial_table  TYPE TABLE OF zdblayr_for_tst,
          ls_line2          TYPE zdblayr_for_tst2,
          lt_initial_table2 TYPE TABLE OF zdblayr_for_tst2.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    ls_line2-key_field   = 'KEY1'.
    ls_line2-key_field2  = 'KEY1_1'.
    ls_line2-amount      = '500'.
    ls_line2-qty         = '2'.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-key_field   = 'KEY1'.
    ls_line2-key_field2  = 'KEY1_2'.
    ls_line2-amount      = '100'.
    ls_line2-qty         = '1'.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-key_field   = 'KEY2'.
    ls_line2-key_field2  = 'KEY2_1'.
    ls_line2-amount      = '200'.
    ls_line2-qty         = '3'.
    APPEND ls_line2 TO lt_initial_table2.

    mo_test_environment->insert_test_data( it_table = lt_initial_table2 ).

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tsv.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM ZDBLAYR_FOR_TSV'
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tsv,
          lt_expected_table TYPE TABLE OF zdblayr_for_tsv.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-key_field2  = 'KEY1_1'.
    ls_expected_line-text_field1 = 'VALUE1_1'.
    ls_expected_line-amount      = '500'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-key_field2  = 'KEY1_2'.
    ls_expected_line-text_field1 = 'VALUE1_1'.
    ls_expected_line-amount      = '100'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-key_field2  = 'KEY2_1'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-amount      = '200'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD view_with_condition.
    DATA: ls_line           TYPE zdblayr_for_tst,
          lt_initial_table  TYPE TABLE OF zdblayr_for_tst,
          ls_line2          TYPE zdblayr_for_tst2,
          lt_initial_table2 TYPE TABLE OF zdblayr_for_tst2.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'AVALUE2_1'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    ls_line2-key_field   = 'KEY1'.
    ls_line2-key_field2  = 'KEY1_1'.
    ls_line2-amount      = '500'.
    ls_line2-qty         = '2'.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-key_field   = 'KEY1'.
    ls_line2-key_field2  = 'KEY1_2'.
    ls_line2-amount      = '100'.
    ls_line2-qty         = '1'.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-key_field   = 'KEY2'.
    ls_line2-key_field2  = 'KEY2_1'.
    ls_line2-amount      = '200'.
    ls_line2-qty         = '3'.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-key_field   = 'KEY2'.
    ls_line2-key_field2  = 'KEY2_2'.
    ls_line2-amount      = '300'.
    ls_line2-qty         = '4'.
    APPEND ls_line2 TO lt_initial_table2.

    mo_test_environment->insert_test_data( it_table = lt_initial_table2 ).

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tsv.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM ZDBLAYR_FOR_TSV2'
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tsv,
          lt_expected_table TYPE TABLE OF zdblayr_for_tsv.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-key_field2  = 'KEY2_1'.
    ls_expected_line-text_field1 = 'AVALUE2_1'.
    ls_expected_line-amount      = '200'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-key_field2  = 'KEY2_2'.
    ls_expected_line-text_field1 = 'AVALUE2_1'.
    ls_expected_line-amount      = '300'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD view_with_several_conditions.
    DATA: ls_line           TYPE zdblayr_for_tst,
          lt_initial_table  TYPE TABLE OF zdblayr_for_tst,
          ls_line2          TYPE zdblayr_for_tst2,
          lt_initial_table2 TYPE TABLE OF zdblayr_for_tst2.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'AVALUE2_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'AVALUE3_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY4'.
    ls_line-text_field1 = 'AVALUE4_1'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    ls_line2-key_field   = 'KEY1'.
    ls_line2-key_field2  = 'KEY1_1'.
    ls_line2-amount      = '500'.
    ls_line2-qty         = '2'.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-key_field   = 'KEY1'.
    ls_line2-key_field2  = 'KEY1_2'.
    ls_line2-amount      = '100'.
    ls_line2-qty         = '1'.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-key_field   = 'KEY1'.
    ls_line2-key_field2  = 'KEY1_3'.
    ls_line2-amount      = '200'.
    ls_line2-qty         = '1'.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-key_field   = 'KEY2'.
    ls_line2-key_field2  = 'KEY2_1'.
    ls_line2-amount      = '200'.
    ls_line2-qty         = '3'.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-key_field   = 'KEY2'.
    ls_line2-key_field2  = 'KEY2_2'.
    ls_line2-amount      = '300'.
    ls_line2-qty         = '4'.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-key_field   = 'KEY3'.
    ls_line2-key_field2  = 'KEY3_1'.
    ls_line2-amount      = '100'.
    ls_line2-qty         = '3'.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-key_field   = 'KEY3'.
    ls_line2-key_field2  = 'KEY3_1'.
    ls_line2-amount      = '200'.
    ls_line2-qty         = '3'.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-key_field   = 'KEY4'.
    ls_line2-key_field2  = 'KEY4_1'.
    ls_line2-amount      = '100'.
    ls_line2-qty         = '3'.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-key_field   = 'KEY4'.
    ls_line2-key_field2  = 'KEY4_2'.
    ls_line2-amount      = '300'.
    ls_line2-qty         = '2'.
    APPEND ls_line2 TO lt_initial_table2.

    mo_test_environment->insert_test_data( it_table = lt_initial_table2 ).

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tsv.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM ZDBLAYR_FOR_TSV3'
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tsv,
          lt_expected_table TYPE TABLE OF zdblayr_for_tsv.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-key_field2  = 'KEY2_1'.
    ls_expected_line-text_field1 = 'AVALUE2_1'.
    ls_expected_line-amount      = '200'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY4'.
    ls_expected_line-key_field2  = 'KEY4_1'.
    ls_expected_line-text_field1 = 'AVALUE4_1'.
    ls_expected_line-amount      = '100'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD bug_join_07_10_2021.

    DATA: ls_line           TYPE zdblayr_for_tst,
          lt_initial_table  TYPE TABLE OF zdblayr_for_tst,
          ls_line2          TYPE zdblayr_for_tst2,
          lt_initial_table2 TYPE TABLE OF zdblayr_for_tst2.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( it_table = lt_initial_table ).

    ls_line2-key_field   = 'KEY1'.
    ls_line2-key_field2  = 'KEY1_1'.
    ls_line2-amount      = '500'.
    ls_line2-qty         = '2'.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-key_field   = 'KEY1'.
    ls_line2-key_field2  = 'KEY1_2'.
    ls_line2-amount      = '100'.
    ls_line2-qty         = '1'.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-key_field   = 'KEY2'.
    ls_line2-key_field2  = 'KEY2_1'.
    ls_line2-amount      = '200'.
    ls_line2-qty         = '3'.
    APPEND ls_line2 TO lt_initial_table2.

    mo_test_environment->insert_test_data( it_table = lt_initial_table2 ).

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tsv,
          lv_select       TYPE string.

    CONCATENATE 'SELECT ZDBLAYR_FOR_TST~MANDT AS MANDT ZDBLAYR_FOR_TST~KEY_FIELD AS KEY_FIELD'
      'ZDBLAYR_FOR_TST~TEXT_FIELD1 AS TEXT_FIELD1 ZDBLAYR_FOR_TST2~KEY_FIELD2 AS  KEY_FIELD2'
      'ZDBLAYR_FOR_TST2~AMOUNT AS AMOUNT'
      'FROM ZDBLAYR_FOR_TST JOIN ZDBLAYR_FOR_TST2 ON ZDBLAYR_FOR_TST~KEY_FIELD EQ ZDBLAYR_FOR_TST2~KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tsv,
          lt_expected_table TYPE TABLE OF zdblayr_for_tsv.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-key_field2  = 'KEY1_1'.
    ls_expected_line-text_field1 = 'VALUE1_1'.
    ls_expected_line-amount      = '500'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-key_field2  = 'KEY1_2'.
    ls_expected_line-text_field1 = 'VALUE1_1'.
    ls_expected_line-amount      = '100'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-key_field2  = 'KEY2_1'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-amount      = '200'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD where_value_with_spaces.
    DATA: ls_line          TYPE zdblayr_for_tst,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1 1'.
    ls_line-text_field2 = 'VALUE1 2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2 1'.
    ls_line-text_field2 = 'VALUE2 2'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZDBLAYR_FOR_TST'
      'WHERE TEXT_FIELD2 = ''VALUE1 2'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    FIELD-SYMBOLS: <lt_result_table> TYPE STANDARD TABLE.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'VALUE1 1'.
    ls_expected_line-text_field2 = 'VALUE1 2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD group_by_with_count_star.
    DATA: ls_line          TYPE zdblayr_for_tst2,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst2,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-key_field2  = 'KEY2_1'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT KEY_FIELD COUNT( * ) AS CNT'
      'FROM ZDBLAYR_FOR_TST2'
      'GROUP BY KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zdblayr_for_tst2-key_field,
             cnt       TYPE i,
           END OF ty_result.

    DATA: lt_result TYPE TABLE OF ty_result.

    FIELD-SYMBOLS: <lt_result> TYPE STANDARD TABLE.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result ).

    " THEN
    DATA: lt_expected TYPE TABLE OF ty_result,
          ls_expected TYPE ty_result.

    ls_expected-key_field = 'KEY1'.
    ls_expected-cnt       = 2.
    APPEND ls_expected TO lt_expected.

    ls_expected-key_field = 'KEY2'.
    ls_expected-cnt       = 1.
    APPEND ls_expected TO lt_expected.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected ).
  ENDMETHOD.

  METHOD group_by_with_tabname.

    " GROUP BY where in GROUP BY statement field is written in form of <tabname>~<fieldname>

    DATA: ls_line          TYPE zdblayr_for_tst2,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst2,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-key_field2  = 'KEY2_1'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT KEY_FIELD COUNT( * ) AS CNT'
      'FROM ZDBLAYR_FOR_TST2'
      'GROUP BY ZDBLAYR_FOR_TST2~KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zdblayr_for_tst2-key_field,
             cnt       TYPE i,
           END OF ty_result.

    DATA: lt_result TYPE TABLE OF ty_result.

    FIELD-SYMBOLS: <lt_result> TYPE STANDARD TABLE.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result ).

    " THEN
    DATA: lt_expected TYPE TABLE OF ty_result,
          ls_expected TYPE ty_result.

    ls_expected-key_field = 'KEY1'.
    ls_expected-cnt       = 2.
    APPEND ls_expected TO lt_expected.

    ls_expected-key_field = 'KEY2'.
    ls_expected-cnt       = 1.
    APPEND ls_expected TO lt_expected.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected ).
  ENDMETHOD.

  METHOD group_by_lower_case_alias.

    " Condition 1:
    "   SELECT ... GROUP BY in which fields are written in lower case (both in the SELECT and in the GROUP BY)
    " Condition 2:
    "   GROUP BY field in SELECT has alias written in lower case

    DATA: ls_line          TYPE zdblayr_for_tst2,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst2,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-key_field2  = 'KEY2_1'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    CONCATENATE 'select key_field as key_field count( * ) as cnt'
      'from zdblayr_for_tst2'
      'group by key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zdblayr_for_tst2-key_field,
             cnt       TYPE i,
           END OF ty_result.

    DATA: lt_result TYPE TABLE OF ty_result.

    FIELD-SYMBOLS: <lt_result> TYPE STANDARD TABLE.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result ).

    " THEN
    DATA: lt_expected TYPE TABLE OF ty_result,
          ls_expected TYPE ty_result.

    ls_expected-key_field = 'KEY1'.
    ls_expected-cnt       = 2.
    APPEND ls_expected TO lt_expected.

    ls_expected-key_field = 'KEY2'.
    ls_expected-cnt       = 1.
    APPEND ls_expected TO lt_expected.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected ).
  ENDMETHOD.

  METHOD for_all_ent_compare_2_fld.

    " SELECT ... FOR ALL ENTRIES IN ... and in where condition there is 2 comparison with FAE base table fields

    DATA: ls_line          TYPE zdblayr_for_tst2,
          lt_initial_table TYPE TABLE OF zdblayr_for_tst2,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-key_field2  = 'KEY2_1'.
    APPEND ls_line TO lt_initial_table.

    mo_test_environment->insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZDBLAYR_FOR_TST2'
      'FOR ALL ENTRIES IN LT_FOR_ALL_ENTRIES_TAB'
      'WHERE KEY_FIELD  = LT_FOR_ALL_ENTRIES_TAB-KEY_FIELD'
      '  AND KEY_FIELD2 = LT_FOR_ALL_ENTRIES_TAB-KEY_FIELD2'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_for_all_entries_tab  TYPE TABLE OF zdblayr_for_tst2,
          ls_for_all_entries_line TYPE zdblayr_for_tst2,
          lt_result               TYPE TABLE OF zdblayr_for_tst2.

    ls_for_all_entries_line-key_field  = 'KEY1'.
    ls_for_all_entries_line-key_field2 = 'KEY1_1'.
    APPEND ls_for_all_entries_line TO lt_for_all_entries_tab.

    ls_for_all_entries_line-key_field  = 'KEY2'.
    ls_for_all_entries_line-key_field2 = 'KEY2_1'.
    APPEND ls_for_all_entries_line TO lt_for_all_entries_tab.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                = lv_select
                                                        it_for_all_entries_table = lt_for_all_entries_tab
                                              IMPORTING et_result_table          = lt_result ).

    " THEN
    DATA: lt_expected TYPE TABLE OF zdblayr_for_tst2,
          ls_expected TYPE zdblayr_for_tst2.

    ls_expected-mandt       = sy-mandt.
    ls_expected-key_field   = 'KEY1'.
    ls_expected-key_field2  = 'KEY1_1'.
    APPEND ls_expected TO lt_expected.

    ls_expected-mandt       = sy-mandt.
    ls_expected-key_field   = 'KEY2'.
    ls_expected-key_field2  = 'KEY2_1'.
    APPEND ls_expected TO lt_expected.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_insert IMPLEMENTATION.
  METHOD insert_by_itab.
    DATA: ls_line         TYPE zdblayr_for_tst,
          lt_insert_table TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_insert_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_insert_table.

    " WHEN
    f_cut->zif_zosql_db_layer~insert_by_itab( iv_table_name = 'ZDBLAYR_FOR_TST'
                                              it_new_lines  = lt_insert_table ).

    " THEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zdblayr_for_tst'
                                              IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_insert_table ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_update IMPLEMENTATION.
  METHOD update_by_itab.
    DATA: ls_line                TYPE zdblayr_for_tst,
          lt_table_before_update TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_update.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_update.

    mo_test_environment->insert_test_data( it_table = lt_table_before_update ).

    " WHEN
    DATA: lt_update_table TYPE TABLE OF zdblayr_for_tst.

    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE_1_1_UPD'.
    APPEND ls_line TO lt_update_table.

    ls_line-key_field = 'KEY2'.
    ls_line-text_field1 = 'VALUE_2_1_UPD'.
    APPEND ls_line TO lt_update_table.

    f_cut->zif_zosql_db_layer~update_by_itab( iv_table_name       = 'ZDBLAYR_FOR_TST'
                                              it_lines_for_update = lt_update_table ).

    " THEN
    DATA: lt_result_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zdblayr_for_tst'
                                              IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_update_table ).
  ENDMETHOD.

  METHOD by_sql_no_params.
    DATA: ls_line                TYPE zdblayr_for_tst,
          lt_table_before_update TYPE TABLE OF zdblayr_for_tst,
          lv_update_statement    TYPE string.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_update.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_update.

    mo_test_environment->insert_test_data( lt_table_before_update ).

    " WHEN
    CONCATENATE 'UPDATE ZDBLAYR_FOR_TST'
      'SET text_field2 = ''NEW_VAL_FIELD2'''
      'WHERE TEXT_FIELD1 = ''VALUE2_1'''
      INTO lv_update_statement SEPARATED BY space.

    f_cut->zif_zosql_db_layer~update( lv_update_statement ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zdblayr_for_tst'
                                              IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    CLEAR ls_line.
    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_expected_table.

    CLEAR ls_line.
    ls_line-key_field = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'NEW_VAL_FIELD2'.
    APPEND ls_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD by_sql_with_params.
    DATA: ls_line                TYPE zdblayr_for_tst,
          lt_table_before_update TYPE TABLE OF zdblayr_for_tst,
          lv_update_statement    TYPE string.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_update.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_update.

    mo_test_environment->insert_test_data( lt_table_before_update ).

    " WHEN
    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param.

    ls_param-param_name_in_select   = ':TEXT_FIELD1'.
    ls_param-parameter_value_single = 'VALUE2_1'.
    APPEND ls_param TO lt_params.

    CONCATENATE 'UPDATE ZDBLAYR_FOR_TST'
      'SET text_field2 = ''NEW_VAL_FIELD2'''
      'WHERE TEXT_FIELD1 = :TEXT_FIELD1'
      INTO lv_update_statement SEPARATED BY space.

    f_cut->zif_zosql_db_layer~update( iv_update_statement = lv_update_statement
                                      it_parameters       = lt_params ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zdblayr_for_tst'
                                              IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    CLEAR ls_line.
    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_expected_table.

    CLEAR ls_line.
    ls_line-key_field = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'NEW_VAL_FIELD2'.
    APPEND ls_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD by_sql_params_set.
    DATA: ls_line                TYPE zdblayr_for_tst,
          lt_table_before_update TYPE TABLE OF zdblayr_for_tst,
          lv_update_statement    TYPE string.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_update.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_update.

    mo_test_environment->insert_test_data( lt_table_before_update ).

    " WHEN
    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param.

    ls_param-param_name_in_select   = ':TEXT_FIELD2'.
    ls_param-parameter_value_single = 'NEW_VAL_FIELD2'.
    APPEND ls_param TO lt_params.

    CONCATENATE 'UPDATE ZDBLAYR_FOR_TST'
      'SET text_field2 = :TEXT_FIELD2'
      'WHERE TEXT_FIELD1 = ''VALUE2_1'''
      INTO lv_update_statement SEPARATED BY space.

    f_cut->zif_zosql_db_layer~update( iv_update_statement = lv_update_statement
                                      it_parameters       = lt_params ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zdblayr_for_tst'
                                              IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    CLEAR ls_line.
    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_expected_table.

    CLEAR ls_line.
    ls_line-key_field = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'NEW_VAL_FIELD2'.
    APPEND ls_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD by_sql_new_syntax.
    DATA: ls_line                TYPE zdblayr_for_tst,
          lt_table_before_update TYPE TABLE OF zdblayr_for_tst,
          lv_update_statement    TYPE string.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_update.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_update.

    mo_test_environment->insert_test_data( lt_table_before_update ).

    " WHEN
    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param.

    ls_param-param_name_in_select   = ':TEXT_FIELD1'.
    ls_param-parameter_value_single = 'VALUE2_1'.
    APPEND ls_param TO lt_params.

    ls_param-param_name_in_select   = ':TEXT_FIELD2'.
    ls_param-parameter_value_single = 'NEW_VAL_FIELD2'.
    APPEND ls_param TO lt_params.

    CONCATENATE 'UPDATE ZDBLAYR_FOR_TST'
      'SET text_field2 = @:TEXT_FIELD2'
      'WHERE TEXT_FIELD1 = @:TEXT_FIELD1'
      INTO lv_update_statement SEPARATED BY space.

    f_cut->zif_zosql_db_layer~update( iv_update_statement = lv_update_statement
                                      it_parameters       = lt_params ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zdblayr_for_tst'
                                              IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    CLEAR ls_line.
    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_expected_table.

    CLEAR ls_line.
    ls_line-key_field = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'NEW_VAL_FIELD2'.
    APPEND ls_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_modify IMPLEMENTATION.
  METHOD modify_by_itab.
    DATA: ls_line                TYPE zdblayr_for_tst,
          lt_table_before_modify TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    DELETE FROM zdblayr_for_tst.

    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_modify.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_modify.

    mo_test_environment->insert_test_data( lt_table_before_modify ).

    " WHEN
    DATA: lt_modify_table TYPE TABLE OF zdblayr_for_tst.

    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE_1_1_MOD'.
    APPEND ls_line TO lt_modify_table.

    ls_line-key_field = 'KEY3'.
    ls_line-text_field1 = 'VALUE_3_1_MOD'.
    APPEND ls_line TO lt_modify_table.

    f_cut->zif_zosql_db_layer~modify_by_itab( iv_table_name       = 'ZDBLAYR_FOR_TST'
                                              it_lines_for_modify = lt_modify_table ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zdblayr_for_tst'
                                              IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE_1_1_MOD'.
    APPEND ls_line TO lt_expected_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_expected_table.

    ls_line-key_field = 'KEY3'.
    ls_line-text_field1 = 'VALUE_3_1_MOD'.
    APPEND ls_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_delete IMPLEMENTATION.
  METHOD delete_by_itab.
    DATA: ls_line                TYPE zdblayr_for_tst,
          lt_table_before_modify TYPE TABLE OF zdblayr_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_modify.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_modify.

    mo_test_environment->insert_test_data( it_table = lt_table_before_modify ).

    " WHEN
    DATA: lt_delete_table TYPE TABLE OF zdblayr_for_tst.

    CLEAR ls_line.
    ls_line-key_field = 'KEY2'.
    APPEND ls_line TO lt_delete_table.

    f_cut->zif_zosql_db_layer~delete_by_itab( iv_table_name       = 'ZDBLAYR_FOR_TST'
                                              it_lines_for_delete = lt_delete_table ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zdblayr_for_tst'
                                              IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD by_sql_no_params.
    DATA: ls_line                TYPE zdblayr_for_tst,
          lt_table_before_delete TYPE TABLE OF zdblayr_for_tst,
          lv_delete_statement    TYPE string.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_delete.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_delete.

    mo_test_environment->insert_test_data( lt_table_before_delete ).

    " WHEN
    CONCATENATE 'DELETE FROM ZDBLAYR_FOR_TST'
      'WHERE TEXT_FIELD1 = ''VALUE2_1'''
      INTO lv_delete_statement SEPARATED BY space.

    f_cut->zif_zosql_db_layer~delete( lv_delete_statement ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zdblayr_for_tst'
                                              IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    CLEAR ls_line.
    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD by_sql_with_params.
    DATA: ls_line                TYPE zdblayr_for_tst,
          lt_table_before_delete TYPE TABLE OF zdblayr_for_tst,
          lv_delete_statement    TYPE string.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_delete.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_delete.

    mo_test_environment->insert_test_data( lt_table_before_delete ).

    " WHEN
    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param.

    ls_param-param_name_in_select   = ':TEXT_FIELD1'.
    ls_param-parameter_value_single = 'VALUE2_1'.
    APPEND ls_param TO lt_params.

    CONCATENATE 'DELETE FROM ZDBLAYR_FOR_TST'
      'WHERE TEXT_FIELD1 = :TEXT_FIELD1'
      INTO lv_delete_statement SEPARATED BY space.

    f_cut->zif_zosql_db_layer~delete( iv_delete_statement = lv_delete_statement
                                      it_parameters       = lt_params ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zdblayr_for_tst,
          lt_expected_table TYPE TABLE OF zdblayr_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zdblayr_for_tst'
                                              IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    CLEAR ls_line.
    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.
ENDCLASS.
