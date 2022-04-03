*&---------------------------------------------------------------------*
*& Include          ZOSQL_TEST_CASES_DB_LAYER
*&---------------------------------------------------------------------*

CLASS ltc_cases_for_select DEFINITION ABSTRACT FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM zcl_zosql_unitbase.
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
  PUBLIC SECTION.

    METHODS: one_table_no_conditions FOR TESTING RAISING zcx_zosql_error,
      one_table_where_eq FOR TESTING RAISING zcx_zosql_error,
      one_table_where_eq_tabname FOR TESTING RAISING zcx_zosql_error,
      one_table_where_param_eq FOR TESTING RAISING zcx_zosql_error,
      select_in_range FOR TESTING RAISING zcx_zosql_error,
      select_not_in_range FOR TESTING RAISING zcx_zosql_error,
      one_table_where_param_ref FOR TESTING RAISING zcx_zosql_error,
      one_table_2_params_with_or FOR TESTING RAISING zcx_zosql_error,
      one_table_for_all_entries FOR TESTING RAISING zcx_zosql_error,
      one_table_group_by FOR TESTING RAISING zcx_zosql_error,
      one_table_distinct FOR TESTING RAISING zcx_zosql_error,
      like FOR TESTING RAISING zcx_zosql_error,
      not_like FOR TESTING RAISING zcx_zosql_error,
      like_with_star FOR TESTING RAISING zcx_zosql_error,
      join FOR TESTING RAISING zcx_zosql_error,
      left_join FOR TESTING RAISING zcx_zosql_error,
      join_3_tabs FOR TESTING RAISING zcx_zosql_error,
      view_no_conditions FOR TESTING RAISING zcx_zosql_error,
      view_with_condition FOR TESTING RAISING zcx_zosql_error,
      view_with_several_conditions FOR TESTING RAISING zcx_zosql_error,
      as_ref_to_data FOR TESTING RAISING zcx_zosql_error,
      select_without_corresponding FOR TESTING RAISING zcx_zosql_error,
      select_into_tab_with_string FOR TESTING RAISING zcx_zosql_error,
      select_into_tab_no_struct FOR TESTING RAISING zcx_zosql_error,
      select_one_line FOR TESTING RAISING zcx_zosql_error,
      select_up_to_n_rows FOR TESTING RAISING zcx_zosql_error,
      select_up_to_n_rows_prm FOR TESTING RAISING zcx_zosql_error,
      select_single FOR TESTING RAISING zcx_zosql_error,
      select_in_empty_range FOR TESTING RAISING zcx_zosql_error,
      select_in_list_of_vals FOR TESTING RAISING zcx_zosql_error,
      select_into_sorted_table FOR TESTING RAISING zcx_zosql_error,
      select_not_in_list_of_vals FOR TESTING RAISING zcx_zosql_error,
      select_count_star FOR TESTING RAISING zcx_zosql_error,
      select_count_star_no_space FOR TESTING RAISING zcx_zosql_error,
      select_between FOR TESTING RAISING zcx_zosql_error,
      select_not_between FOR TESTING RAISING zcx_zosql_error,
      select_like_escape FOR TESTING RAISING zcx_zosql_error,
      select_param_in_join FOR TESTING RAISING zcx_zosql_error,
      select_from_table_with_include FOR TESTING RAISING zcx_zosql_error,
      select_order_by FOR TESTING RAISING zcx_zosql_error,
      sql_separated_by_line_breaks FOR TESTING RAISING zcx_zosql_error,
      open_cursor_fetch_itab FOR TESTING RAISING zcx_zosql_error,
      open_cursor_fetch FOR TESTING RAISING zcx_zosql_error,
      where_with_brackets FOR TESTING RAISING zcx_zosql_error,
      where_or_inside_brackets FOR TESTING RAISING zcx_zosql_error,
      where_with_3_and_in_a_row FOR TESTING RAISING zcx_zosql_error,
      not_simple FOR TESTING RAISING zcx_zosql_error,
      not_and_brackets FOR TESTING RAISING zcx_zosql_error,
      bug_join_07_10_2021 FOR TESTING RAISING zcx_zosql_error,
      where_value_with_spaces FOR TESTING RAISING zcx_zosql_error,
      group_by_with_count_star FOR TESTING RAISING zcx_zosql_error,
      group_by_with_tabname FOR TESTING RAISING zcx_zosql_error,
      group_by_lower_case_alias FOR TESTING RAISING zcx_zosql_error,
      group_by_with_order_by FOR TESTING RAISING zcx_zosql_error,
      group_by_2_fields FOR TESTING RAISING zcx_zosql_error,
      group_by_with_avg FOR TESTING RAISING zcx_zosql_error,
      group_by_count_distinct FOR TESTING RAISING zcx_zosql_error,
      group_by_without_alias FOR TESTING RAISING zcx_zosql_error,
      group_by_having FOR TESTING RAISING zcx_zosql_error,
      group_by_having_with_param FOR TESTING RAISING zcx_zosql_error,
      group_by_without_aggr_funcs FOR TESTING RAISING zcx_zosql_error,
      group_by_field_not_selected FOR TESTING RAISING zcx_zosql_error,
      order_by_descending FOR TESTING RAISING zcx_zosql_error,
      order_by_ascending FOR TESTING RAISING zcx_zosql_error,
      order_by_field_not_selected FOR TESTING RAISING zcx_zosql_error,
      order_by_when_empty_result FOR TESTING RAISING zcx_zosql_error,
      order_by_2_fields FOR TESTING RAISING zcx_zosql_error,
      order_by_with_join FOR TESTING RAISING zcx_zosql_error,
      for_all_ent_compare_2_fld FOR TESTING RAISING zcx_zosql_error,
      param_with_name_like_field FOR TESTING RAISING zcx_zosql_error,
      view_user_addr FOR TESTING RAISING zcx_zosql_error.

  PROTECTED SECTION.
    DATA: f_cut  TYPE REF TO zif_zosql_db_layer.
ENDCLASS.       "ltc_cases_for_select

CLASS ltc_cases_for_select_740 DEFINITION ABSTRACT FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM zcl_zosql_unitbase.

  PUBLIC SECTION.
    METHODS: new_syntax FOR TESTING RAISING zcx_zosql_error,
      new_syntax_no_host_var FOR TESTING RAISING zcx_zosql_error,
      new_syntax_no_space_selfld FOR TESTING RAISING zcx_zosql_error,
      group_by_having_subquery FOR TESTING RAISING zcx_zosql_error,
      select_exists_subquery FOR TESTING RAISING zcx_zosql_error,
      select_not_exists_subquery FOR TESTING RAISING zcx_zosql_error,
      select_subquery_where_eq FOR TESTING RAISING zcx_zosql_error,
      select_subquery_any FOR TESTING RAISING zcx_zosql_error,
      select_subquery_all FOR TESTING RAISING zcx_zosql_error,
      subquery_in FOR TESTING RAISING zcx_zosql_error,
      subquery_not_in FOR TESTING RAISING zcx_zosql_error,
      error_equal_and_range_in_par FOR TESTING RAISING zcx_zosql_error.

  PROTECTED SECTION.
    DATA: f_cut  TYPE REF TO zif_zosql_db_layer.
ENDCLASS.

CLASS ltc_cases_for_insert DEFINITION ABSTRACT FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM zcl_zosql_unitbase.
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
  PUBLIC SECTION.
    METHODS: insert_by_itab FOR TESTING RAISING zcx_zosql_error,
      insert_by_itab_subrc_4 FOR TESTING RAISING zcx_zosql_error.

  PROTECTED SECTION.
    DATA: f_cut  TYPE REF TO zif_zosql_db_layer.
ENDCLASS.

CLASS ltc_cases_for_update DEFINITION ABSTRACT FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM zcl_zosql_unitbase.
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
  PUBLIC SECTION.
    METHODS: update_by_itab FOR TESTING RAISING zcx_zosql_error,
      by_sql_no_params FOR TESTING RAISING zcx_zosql_error,
      by_sql_with_params FOR TESTING RAISING zcx_zosql_error,
      by_sql_params_set FOR TESTING RAISING zcx_zosql_error,
      update_by_itab_subrc_4 FOR TESTING RAISING zcx_zosql_error,
      update_by_sql_subrc_4 FOR TESTING RAISING zcx_zosql_error.

  PROTECTED SECTION.
    DATA: f_cut  TYPE REF TO zif_zosql_db_layer.
ENDCLASS.

CLASS ltc_cases_for_update_740 DEFINITION ABSTRACT FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM zcl_zosql_unitbase.

  PUBLIC SECTION.
    METHODS: by_sql_new_syntax FOR TESTING RAISING zcx_zosql_error.
  PROTECTED SECTION.
    DATA: f_cut  TYPE REF TO zif_zosql_db_layer.
ENDCLASS.

CLASS ltc_cases_for_modify DEFINITION ABSTRACT FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM zcl_zosql_unitbase.
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
  PUBLIC SECTION.
    METHODS: modify_by_itab FOR TESTING RAISING zcx_zosql_error.

  PROTECTED SECTION.
    DATA: f_cut  TYPE REF TO zif_zosql_db_layer.
ENDCLASS.

CLASS ltc_cases_for_delete DEFINITION ABSTRACT FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM zcl_zosql_unitbase.
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

  PROTECTED SECTION.
    DATA: f_cut  TYPE REF TO zif_zosql_db_layer.
ENDCLASS.

CLASS ltc_cases_for_select IMPLEMENTATION.

  METHOD one_table_no_conditions.

    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( it_table = lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst,
          ls_result_line  TYPE zosql_for_tst,
          lv_subrc        TYPE sysubrc.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table
                                     es_result_line  = ls_result_line
                                     ev_subrc        = lv_subrc ).

    " THEN
    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_initial_table msg = 'ET_RESULT_TABLE' ).

    DATA: ls_expected_line TYPE zosql_for_tst.
    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'VALUE1_1'.
    ls_expected_line-text_field2 = 'VALUE1_2'.

    cl_aunit_assert=>assert_equals( act = ls_result_line exp = ls_expected_line msg = 'ES_RESULT_LINE' ).
    cl_aunit_assert=>assert_equals( act = lv_subrc exp = 0 msg = 'EV_SUBRC' ).
  ENDMETHOD.

  METHOD one_table_where_eq.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( it_table = lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'WHERE KEY_FIELD = ''KEY2'''
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zosql_for_tst,
          ls_expected_line  TYPE zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD one_table_where_eq_tabname.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( it_table = lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'WHERE ZOSQL_FOR_TST~KEY_FIELD = ''KEY2'''
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zosql_for_tst,
          ls_expected_line  TYPE zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD one_table_where_param_eq.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'WHERE KEY_FIELD = :KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    ls_param-param_name_in_select = ':KEY_FIELD'.
    ls_param-parameter_value_single = 'KEY2'.
    APPEND ls_param TO lt_params.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_params
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zosql_for_tst,
          ls_expected_line  TYPE zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD select_in_range.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

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

    insert_test_data( it_table = lt_initial_table ).

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
      'FROM zosql_for_tst'
      'WHERE TEXT_FIELD1 IN :TEXT_FIELD'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_params
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zosql_for_tst,
          ls_expected_line  TYPE zosql_for_tst.

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

    SORT: lt_result_table, lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD select_not_in_range.

    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select           TYPE string,
          lt_params           TYPE zosql_db_layer_params,
          ls_param            TYPE zosql_db_layer_param,
          ls_param_range_line TYPE zosql_db_layer_range_line.

    ls_param-param_name_in_select   = ':TEXT_FIELD'.

    ls_param_range_line-sign   = 'I'.
    ls_param_range_line-option = 'EQ'.
    ls_param_range_line-low    = 'VALUE1_1'.
    APPEND ls_param_range_line TO ls_param-parameter_value_range.

    ls_param_range_line-low    = 'VALUE3_1'.
    APPEND ls_param_range_line TO ls_param-parameter_value_range.
    APPEND ls_param TO lt_params.

    CONCATENATE 'SELECT KEY_FIELD'
      'FROM ZOSQL_FOR_TST'
      'WHERE TEXT_FIELD1 NOT IN :TEXT_FIELD'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst-key_field,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_params
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF ty_result.

    APPEND 'KEY2' TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD one_table_where_param_ref.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lt_params     TYPE zosql_db_layer_params,
          ls_param      TYPE zosql_db_layer_param,
          lv_select     TYPE string,
          lv_some_value TYPE text50.

    ls_param-param_name_in_select   = ':KEY_FIELD'.
    lv_some_value = 'KEY2'.
    GET REFERENCE OF lv_some_value INTO ls_param-parameter_value_ref.
    APPEND ls_param TO lt_params.

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'WHERE KEY_FIELD = :KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_params
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zosql_for_tst,
          ls_expected_line  TYPE zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD one_table_2_params_with_or.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

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

    insert_test_data( it_table = lt_initial_table ).

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
      'FROM zosql_for_tst'
      'WHERE TEXT_FIELD1 IN :TEXT_FIELD OR KEY_FIELD = :KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_params
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

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
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lt_base_table           TYPE TABLE OF zosql_for_tst,
          ls_line_for_all_entries TYPE zosql_for_tst.

    ls_line_for_all_entries-key_field = 'KEY2'.
    APPEND ls_line_for_all_entries TO lt_base_table.

    ls_line_for_all_entries-key_field = 'KEY3'.
    APPEND ls_line_for_all_entries TO lt_base_table.

    DATA: lv_select TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'FOR ALL ENTRIES IN lt_base_table'
      'WHERE KEY_FIELD = LT_BASE_TABLE-KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select                  = lv_select
                                     it_for_all_entries_table   = lt_base_table
                           IMPORTING et_result_table            = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

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
    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2.

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select   TYPE string.

    CONCATENATE 'SELECT key_field sum( amount ) as amount sum( qty ) as qty'
      'FROM zosql_for_tst2'
      'GROUP BY key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst2.

    f_cut->select_to_itab( EXPORTING iv_select                  = lv_select
                           IMPORTING et_result_table            = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst2,
          lt_expected_table TYPE TABLE OF zosql_for_tst2.

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
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

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

    insert_test_data( it_table = lt_initial_table ).

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst,
          lv_select       TYPE string.

    CONCATENATE 'SELECT DISTINCT TEXT_FIELD1'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY TEXT_FIELD1'
      INTO lv_select SEPARATED BY space.

    f_cut->select_to_itab( EXPORTING iv_select                  = lv_select
                           IMPORTING et_result_table            = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    ls_expected_line-text_field1 = 'SOME VALUE'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-text_field1 = 'SOME VALUE2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD like.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select           TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'WHERE TEXT_FIELD1 LIKE ''A%'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY3'.
    ls_expected_line-text_field1 = 'AVALUE3_1'.
    ls_expected_line-text_field2 = 'VALUE3_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD not_like.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select           TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'WHERE TEXT_FIELD1 NOT LIKE ''A%'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

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

  METHOD like_with_star.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = '*VALUE2_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'AVALUE3_1'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select           TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'WHERE TEXT_FIELD1 LIKE ''*%'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst-key_field,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF ty_result.

    APPEND 'KEY2' TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD join.

    DATA: lt_table1_initial TYPE TABLE OF zosql_for_tst,
          ls_line1_initial  TYPE zosql_for_tst,
          lt_table2_initial TYPE TABLE OF zosql_for_tst2,
          ls_line2_initial  TYPE zosql_for_tst2.

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

    insert_test_data( it_table = lt_table1_initial ).

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

    insert_test_data( it_table = lt_table2_initial ).

    DATA: lv_select TYPE string,
          lv_from   TYPE string.

    CONCATENATE 'SELECT t1~key_field t2~key_field2'
      'FROM zosql_for_tst as t1 JOIN zosql_for_tst2 as t2 ON t1~key_field = t2~key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result_line,
             key_field  TYPE zosql_for_tst-key_field,
             key_field2 TYPE zosql_for_tst2-key_field2,
           END OF ty_result_line.

    DATA: lt_result_table TYPE TABLE OF ty_result_line.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
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
    DATA: lt_table1_initial TYPE TABLE OF zosql_for_tst,
          ls_line1_initial  TYPE zosql_for_tst,
          lt_table2_initial TYPE TABLE OF zosql_for_tst2,
          ls_line2_initial  TYPE zosql_for_tst2.

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

    insert_test_data( it_table = lt_table1_initial ).

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

    insert_test_data( it_table = lt_table2_initial ).

    DATA: lv_select TYPE string,
          lv_from   TYPE string.

    CONCATENATE 'SELECT t1~key_field t2~key_field2'
      'FROM zosql_for_tst as t1 LEFT JOIN zosql_for_tst2 as t2 ON t1~key_field = t2~key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result_line,
             key_field  TYPE zosql_for_tst-key_field,
             key_field2 TYPE zosql_for_tst2-key_field2,
           END OF ty_result_line.

    DATA: lt_result_table TYPE TABLE OF ty_result_line.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
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

  METHOD join_3_tabs.
    DATA: lt_table1 TYPE TABLE OF zosql_for_tst,
          ls_table1 TYPE zosql_for_tst,
          lt_table2 TYPE TABLE OF zosql_for_tst2,
          ls_table2 TYPE zosql_for_tst2,
          lt_table3 TYPE TABLE OF zosql_for_tst3,
          ls_table3 TYPE zosql_for_tst3.

    " GIVEN
    ls_table1-mandt     = sy-mandt.
    ls_table1-key_field = 'KEY1'.
    APPEND ls_table1 TO lt_table1.

    ls_table1-mandt     = sy-mandt.
    ls_table1-key_field = 'KEY2'.
    APPEND ls_table1 TO lt_table1.

    ls_table2-mandt     = sy-mandt.
    ls_table2-key_field = 'KEY1'.
    APPEND ls_table2 TO lt_table2.

    ls_table2-mandt     = sy-mandt.
    ls_table2-key_field = 'KEY2'.
    APPEND ls_table2 TO lt_table2.

    ls_table3-mandt      = sy-mandt.
    ls_table3-key_field3 = 'KEY1'.
    APPEND ls_table3 TO lt_table3.

    ls_table3-mandt      = sy-mandt.
    ls_table3-key_field3 = 'KEY2'.
    APPEND ls_table3 TO lt_table3.

    insert_test_data( lt_table1 ).
    insert_test_data( lt_table2 ).
    insert_test_data( lt_table3 ).

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst-key_field,
           END OF ty_result.

    DATA: lv_select TYPE string,
          lt_result TYPE TABLE OF ty_result.

    CONCATENATE 'SELECT zosql_for_tst~key_field'
      'FROM zosql_for_tst'
      'JOIN zosql_for_tst2 ON zosql_for_tst2~key_field = zosql_for_tst~key_field'
      'JOIN zosql_for_tst3 ON zosql_for_tst3~key_field3 = zosql_for_tst2~key_field'
      INTO lv_select SEPARATED BY space.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result ).

    " THEN
    DATA: lt_expected_result TYPE TABLE OF ty_result.

    APPEND 'KEY1' TO lt_expected_result.
    APPEND 'KEY2' TO lt_expected_result.

    SORT: lt_result, lt_expected_result.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected_result ).
  ENDMETHOD.

  METHOD where_with_brackets.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'WHERE ( key_field >= ''KEY2'' AND text_field1 = ''SOME_TEXT1'' ) OR text_field1 <> ''SOME_TEXT1'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

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
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'WHERE ( KEY_FIELD = ''KEY1'' OR KEY_FIELD = ''KEY3'' )'
      '  AND TEXT_FIELD1 = ''SOME_TEXT1'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

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

  METHOD where_with_3_and_in_a_row.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'SOME_TEXT1'.
    ls_line-text_field2 = 'SOME_TEXT1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'SOME_TEXT1'.
    ls_line-text_field2 = 'SOME_TEXT1_2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY3'.
    ls_line-text_field1 = 'SOME_TEXT2'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'WHERE TEXT_FIELD1 = ''SOME_TEXT1'''
      '  AND TEXT_FIELD2 = ''SOME_TEXT1_2'''
      '  AND KEY_FIELD = ''KEY1'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'SOME_TEXT1'.
    ls_expected_line-text_field2 = 'SOME_TEXT1_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD not_simple.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'WHERE NOT text_field1 = ''SOME_TEXT2'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_params
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

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
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'WHERE NOT ( text_field1 = ''SOME_TEXT2'' )'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_params
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'SOME_TEXT1'.
    APPEND ls_expected_line TO lt_expected_table.

    SORT: lt_result_table, lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD as_ref_to_data.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: ld_result       TYPE REF TO data,
          lt_result_table TYPE TABLE OF zosql_for_tst,
          lv_subrc        TYPE sysubrc.

    FIELD-SYMBOLS: <lt_result_table> TYPE STANDARD TABLE.

    f_cut->select( EXPORTING iv_select          = lv_select
                   IMPORTING ed_result_as_table = ld_result
                             ev_subrc           = lv_subrc ).

    ASSIGN ld_result->* TO <lt_result_table>.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src = <lt_result_table>
                                               IMPORTING et_table_dest = lt_result_table ).

    " THEN
    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_initial_table ).
    cl_aunit_assert=>assert_equals( act = lv_subrc exp = 0 ).
  ENDMETHOD.

  METHOD select_without_corresponding.

    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT KEY_FIELD TEXT_FIELD1'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             field1 TYPE string,
             field2 TYPE string,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select                = lv_select
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
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT KEY_FIELD TEXT_FIELD1'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field   TYPE string,
             text_field1 TYPE string,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select                = lv_select
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

    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT KEY_FIELD'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF string.

    f_cut->select_to_itab( EXPORTING iv_select                = lv_select
                                     iv_do_into_corresponding = abap_false
                           IMPORTING et_result_table          = lt_result_table ).

    " THEN
    DATA: lt_expected_result TYPE TABLE OF string.

    APPEND 'KEY1' TO lt_expected_result.
    APPEND 'KEY2' TO lt_expected_result.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_result ).
  ENDMETHOD.

  METHOD select_one_line.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT KEY_FIELD TEXT_FIELD1'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: ls_result_line TYPE zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select                = lv_select
                           IMPORTING es_result_line           = ls_result_line ).

    " THEN
    DATA: ls_expected_result TYPE zosql_for_tst.

    ls_expected_result-key_field   = 'KEY1'.
    ls_expected_result-text_field1 = 'VALUE1_1'.

    cl_aunit_assert=>assert_equals( act = ls_result_line exp = ls_expected_result ).
  ENDMETHOD.

  METHOD select_up_to_n_rows.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'UP TO 2 ROWS'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst,
          ls_result_line  TYPE zosql_for_tst,
          lv_subrc        TYPE sysubrc.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zosql_for_tst,
          ls_expected_line  TYPE zosql_for_tst.

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
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'UP TO :LINES_COUNT ROWS'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst,
          lt_parameters   TYPE zosql_db_layer_params,
          ls_parameter    TYPE zosql_db_layer_param.

    ls_parameter-param_name_in_select   = ':LINES_COUNT'.
    ls_parameter-parameter_value_single = 2.
    APPEND ls_parameter TO lt_parameters.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_parameters
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zosql_for_tst,
          ls_expected_line  TYPE zosql_for_tst.

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
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT SINGLE *'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst,
          ls_result_line  TYPE zosql_for_tst,
          lv_subrc        TYPE sysubrc.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zosql_for_tst,
          ls_expected_line  TYPE zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'VALUE1_1'.
    ls_expected_line-text_field2 = 'VALUE1_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD select_in_empty_range.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( lt_initial_table ).

    " WHEN
    DATA: lt_parameters   TYPE zosql_db_layer_params,
          ls_parameter    LIKE LINE OF lt_parameters,
          lt_result_table TYPE TABLE OF zosql_for_tst.

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'WHERE KEY_FIELD IN :KEY_FIELD_RANGE'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    ls_parameter-param_name_in_select = ':KEY_FIELD_RANGE'.
    REFRESH ls_parameter-parameter_value_range.
    APPEND ls_parameter TO lt_parameters.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_parameters
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_initial_table ).
  ENDMETHOD.

  METHOD select_in_list_of_vals.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( lt_initial_table ).

    " WHEN

    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst-key_field,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'WHERE KEY_FIELD IN (''KEY1'',''KEY3'')'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF ty_result.

    APPEND 'KEY1' TO lt_expected_table.
    APPEND 'KEY3' TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD select_into_sorted_table.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE SORTED TABLE OF zosql_for_tst
                               WITH UNIQUE KEY key_field,
          ls_result_line  TYPE zosql_for_tst,
          lv_subrc        TYPE sysubrc.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_initial_table ).
  ENDMETHOD.

  METHOD select_not_in_list_of_vals.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( lt_initial_table ).

    " WHEN

    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst-key_field,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'WHERE KEY_FIELD NOT IN (''KEY1'',''KEY3'')'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF ty_result.

    APPEND 'KEY2' TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD select_count_star.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( it_table = lt_initial_table ).

    CONCATENATE 'SELECT COUNT( * ) AS CNT'
      'FROM zosql_for_tst'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             cnt TYPE i,
           END OF ty_result.

    DATA: ls_result_line  TYPE ty_result.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING es_result_line  = ls_result_line ).

    " THEN
    cl_aunit_assert=>assert_equals( act = ls_result_line-cnt exp = 2 ).
  ENDMETHOD.

  METHOD select_count_star_no_space.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( it_table = lt_initial_table ).

    CONCATENATE 'SELECT COUNT(*) AS CNT'
      'FROM ZOSQL_FOR_TST'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             cnt TYPE i,
           END OF ty_result.

    DATA: ls_result_line  TYPE ty_result.

    f_cut->select_to_itab( EXPORTING iv_select      = lv_select
                           IMPORTING es_result_line = ls_result_line ).

    " THEN
    cl_aunit_assert=>assert_equals( act = ls_result_line-cnt exp = 2 ).
  ENDMETHOD.

  METHOD select_between.
    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-amount      = 10.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-amount      = 20.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY3'.
    ls_line-amount      = 25.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST2'
      'WHERE amount between 20 AND 30'
      INTO lv_select SEPARATED BY space.

    " WHEN

    DATA: lt_result       TYPE TABLE OF zosql_for_tst2.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst2,
          lt_expected_table TYPE TABLE OF zosql_for_tst2.

    ls_expected_line-mandt     = sy-mandt.
    ls_expected_line-key_field = 'KEY2'.
    ls_expected_line-amount    = 20.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt     = sy-mandt.
    ls_expected_line-key_field = 'KEY3'.
    ls_expected_line-amount    = 25.
    APPEND ls_expected_line TO lt_expected_table.

    SORT: lt_result, lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected_table ).
  ENDMETHOD.

  METHOD select_not_between.
    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-amount      = 10.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-amount      = 20.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY3'.
    ls_line-amount      = 25.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST2'
      'WHERE amount not between 20 AND 30'
      INTO lv_select SEPARATED BY space.

    " WHEN

    DATA: lt_result       TYPE TABLE OF zosql_for_tst2.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst2,
          lt_expected_table TYPE TABLE OF zosql_for_tst2.

    ls_expected_line-mandt     = sy-mandt.
    ls_expected_line-key_field = 'KEY1'.
    ls_expected_line-amount    = 10.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected_table ).
  ENDMETHOD.

  METHOD select_like_escape.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = '1002'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = '100%2'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'WHERE TEXT_FIELD1 LIKE ''100#%%'' ESCAPE ''#'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result       TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = '100%2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected_table ).
  ENDMETHOD.

  METHOD select_param_in_join.
    DATA: ls_line           TYPE zosql_for_tst,
          lt_initial_table  TYPE TABLE OF zosql_for_tst,
          ls_line2          TYPE zosql_for_tst2,
          lt_initial_table2 TYPE TABLE OF zosql_for_tst2,
          lt_params         TYPE zosql_db_layer_params,
          ls_param          TYPE zosql_db_layer_param,
          lv_select         TYPE string.

    " GIVEN
    ls_line-mandt        = sy-mandt.
    ls_line-key_field    = 'KEY1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt        = sy-mandt.
    ls_line-key_field    = 'KEY2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt        = sy-mandt.
    ls_line-key_field    = 'KEY3'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( lt_initial_table ).

    ls_line-mandt        = sy-mandt.
    ls_line-key_field    = 'KEY3'.
    APPEND ls_line TO lt_initial_table.

    ls_line2-mandt       = sy-mandt.
    ls_line2-key_field   = 'KEY1'.
    ls_line2-amount      = 10.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-mandt       = sy-mandt.
    ls_line2-key_field   = 'KEY2'.
    ls_line2-amount      = 20.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-mandt       = sy-mandt.
    ls_line2-key_field   = 'KEY3'.
    ls_line2-amount      = 25.
    APPEND ls_line2 TO lt_initial_table2.

    insert_test_data( lt_initial_table2 ).

    CONCATENATE 'SELECT ZOSQL_FOR_TST~key_field'
      'FROM ZOSQL_FOR_TST'
      'JOIN ZOSQL_FOR_TST2 ON ZOSQL_FOR_TST2~KEY_FIELD = ZOSQL_FOR_TST~KEY_FIELD'
      '                   AND ZOSQL_FOR_TST2~AMOUNT >= :MIN_AMOUNT'
      INTO lv_select SEPARATED BY space.

    ls_param-param_name_in_select   = ':MIN_AMOUNT'.
    ls_param-parameter_value_single = 15.
    APPEND ls_param TO lt_params.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst-key_field,
           END OF ty_result.

    DATA: lt_result  TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_params
                           IMPORTING et_result_table = lt_result ).

    "THEN

    DATA: lt_expected  TYPE TABLE OF ty_result.

    APPEND 'KEY2' TO lt_expected.
    APPEND 'KEY3' TO lt_expected.

    SORT: lt_result, lt_expected.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected ).
  ENDMETHOD.

  METHOD open_cursor_fetch_itab.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table1     TYPE TABLE OF zosql_for_tst,
          lt_result_table2     TYPE TABLE OF zosql_for_tst,
          lt_result_table3     TYPE TABLE OF zosql_for_tst,
          lv_subrc_after_call3 TYPE sysubrc,
          lv_cursor            TYPE cursor.

    lv_cursor = f_cut->open_cursor( lv_select ).
    f_cut->fetch_next_cursor_to_itab( EXPORTING iv_cursor       = lv_cursor
                                                                   iv_package_size = 2
                                                         IMPORTING et_result_table = lt_result_table1 ).

    f_cut->fetch_next_cursor_to_itab( EXPORTING iv_cursor       = lv_cursor
                                                                   iv_package_size = 2
                                                         IMPORTING et_result_table = lt_result_table2 ).

    f_cut->fetch_next_cursor_to_itab( EXPORTING iv_cursor       = lv_cursor
                                                                   iv_package_size = 2
                                                         IMPORTING et_result_table = lt_result_table3
                                                                   ev_subrc        = lv_subrc_after_call3 ).

    " THEN
    DATA: lt_expected_table1 TYPE TABLE OF zosql_for_tst,
          lt_expected_table2 TYPE TABLE OF zosql_for_tst,
          ls_expected_line   TYPE zosql_for_tst.

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
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: ld_result_table1     TYPE REF TO data,
          ld_result_table2     TYPE REF TO data,
          ld_result_table3     TYPE REF TO data,
          lt_result_table1     TYPE TABLE OF zosql_for_tst,
          lt_result_table2     TYPE TABLE OF zosql_for_tst,
          lv_subrc_after_call3 TYPE sysubrc,
          lv_cursor            TYPE cursor.

    FIELD-SYMBOLS: <lt_result_table1> TYPE STANDARD TABLE,
                   <lt_result_table2> TYPE STANDARD TABLE.

    lv_cursor = f_cut->open_cursor( lv_select ).
    f_cut->fetch_next_cursor( EXPORTING iv_cursor          = lv_cursor
                                        iv_package_size    = 2
                              IMPORTING ed_result_as_table = ld_result_table1 ).
    ASSIGN ld_result_table1->* TO <lt_result_table1>.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_result_table1>
                                               IMPORTING et_table_dest = lt_result_table1 ).

    f_cut->fetch_next_cursor( EXPORTING iv_cursor          = lv_cursor
                                        iv_package_size    = 2
                              IMPORTING ed_result_as_table = ld_result_table2 ).
    ASSIGN ld_result_table2->* TO <lt_result_table2>.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_result_table2>
                                               IMPORTING et_table_dest = lt_result_table2 ).

    f_cut->fetch_next_cursor( EXPORTING iv_cursor          = lv_cursor
                                        iv_package_size    = 2
                              IMPORTING ed_result_as_table = ld_result_table3
                                                           ev_subrc           = lv_subrc_after_call3 ).

    " THEN
    DATA: lt_expected_table1 TYPE TABLE OF zosql_for_tst,
          lt_expected_table2 TYPE TABLE OF zosql_for_tst,
          ls_expected_line   TYPE zosql_for_tst.

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
    DATA: ls_line           TYPE zosql_for_tst,
          lt_initial_table  TYPE TABLE OF zosql_for_tst,
          ls_line2          TYPE zosql_for_tst2,
          lt_initial_table2 TYPE TABLE OF zosql_for_tst2.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( it_table = lt_initial_table ).

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

    insert_test_data( it_table = lt_initial_table2 ).

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tsv.

    f_cut->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM ZOSQL_FOR_TSV'
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tsv,
          lt_expected_table TYPE TABLE OF zosql_for_tsv.

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
    DATA: ls_line           TYPE zosql_for_tst,
          lt_initial_table  TYPE TABLE OF zosql_for_tst,
          ls_line2          TYPE zosql_for_tst2,
          lt_initial_table2 TYPE TABLE OF zosql_for_tst2.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'AVALUE2_1'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( it_table = lt_initial_table ).

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

    insert_test_data( it_table = lt_initial_table2 ).

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tsv2.

    f_cut->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM ZOSQL_FOR_TSV2'
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tsv2,
          lt_expected_table TYPE TABLE OF zosql_for_tsv2.

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
    DATA: ls_line           TYPE zosql_for_tst,
          lt_initial_table  TYPE TABLE OF zosql_for_tst,
          ls_line2          TYPE zosql_for_tst2,
          lt_initial_table2 TYPE TABLE OF zosql_for_tst2.

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

    insert_test_data( it_table = lt_initial_table ).

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

    insert_test_data( it_table = lt_initial_table2 ).

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tsv3.

    f_cut->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM ZOSQL_FOR_TSV3'
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tsv3,
          lt_expected_table TYPE TABLE OF zosql_for_tsv3.

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

    DATA: ls_line           TYPE zosql_for_tst,
          lt_initial_table  TYPE TABLE OF zosql_for_tst,
          ls_line2          TYPE zosql_for_tst2,
          lt_initial_table2 TYPE TABLE OF zosql_for_tst2.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( it_table = lt_initial_table ).

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

    insert_test_data( it_table = lt_initial_table2 ).

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tsv,
          lv_select       TYPE string.

    CONCATENATE 'SELECT ZOSQL_FOR_TST~MANDT AS MANDT ZOSQL_FOR_TST~KEY_FIELD AS KEY_FIELD'
      'ZOSQL_FOR_TST~TEXT_FIELD1 AS TEXT_FIELD1 ZOSQL_FOR_TST2~KEY_FIELD2 AS  KEY_FIELD2'
      'ZOSQL_FOR_TST2~AMOUNT AS AMOUNT'
      'FROM ZOSQL_FOR_TST JOIN ZOSQL_FOR_TST2 ON ZOSQL_FOR_TST~KEY_FIELD EQ ZOSQL_FOR_TST2~KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tsv,
          lt_expected_table TYPE TABLE OF zosql_for_tsv.

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
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
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

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'WHERE TEXT_FIELD2 = ''VALUE1 2'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    FIELD-SYMBOLS: <lt_result_table> TYPE STANDARD TABLE.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'VALUE1 1'.
    ls_expected_line-text_field2 = 'VALUE1 2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD group_by_with_count_star.
    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2,
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

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT KEY_FIELD COUNT( * ) AS CNT'
      'FROM ZOSQL_FOR_TST2'
      'GROUP BY KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
             cnt       TYPE i,
           END OF ty_result.

    DATA: lt_result TYPE TABLE OF ty_result.

    FIELD-SYMBOLS: <lt_result> TYPE STANDARD TABLE.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
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

    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2,
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

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT KEY_FIELD COUNT( * ) AS CNT'
      'FROM ZOSQL_FOR_TST2'
      'GROUP BY ZOSQL_FOR_TST2~KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
             cnt       TYPE i,
           END OF ty_result.

    DATA: lt_result TYPE TABLE OF ty_result.

    FIELD-SYMBOLS: <lt_result> TYPE STANDARD TABLE.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
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

    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2,
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

    insert_test_data( lt_initial_table ).

    CONCATENATE 'select key_field as key_field count( * ) as cnt'
      'from zosql_for_tst2'
      'group by key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
             cnt       TYPE i,
           END OF ty_result.

    DATA: lt_result TYPE TABLE OF ty_result.

    FIELD-SYMBOLS: <lt_result> TYPE STANDARD TABLE.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
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

  METHOD group_by_with_order_by.
    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2.

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select   TYPE string.

    CONCATENATE 'SELECT key_field sum( amount ) as amount sum( qty ) as qty'
      'FROM zosql_for_tst2'
      'GROUP BY key_field'
      'ORDER BY key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst2.

    f_cut->select_to_itab( EXPORTING iv_select                  = lv_select
                           IMPORTING et_result_table            = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst2,
          lt_expected_table TYPE TABLE OF zosql_for_tst2.

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

  METHOD group_by_2_fields.
    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2.

    " GIVEN
    ls_line-mandt            = sy-mandt.
    ls_line-key_field        = 'KEY1'.
    ls_line-key_field2       = 'KEY1_1'.
    ls_line-amount           = 10.
    ls_line-qty              = 2.
    ls_line-some_other_field = 'VAL1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt            = sy-mandt.
    ls_line-key_field        = 'KEY1'.
    ls_line-key_field2       = 'KEY1_2'.
    ls_line-amount           = 20.
    ls_line-qty              = 4.
    ls_line-some_other_field = 'VAL1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt            = sy-mandt.
    ls_line-key_field        = 'KEY1'.
    ls_line-key_field2       = 'KEY1_3'.
    ls_line-amount           = 5.
    ls_line-qty              = 1.
    ls_line-some_other_field = 'VAL2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt            = sy-mandt.
    ls_line-key_field        = 'KEY2'.
    ls_line-key_field2       = 'KEY2_1'.
    ls_line-amount           = 70.
    ls_line-qty              = 5.
    ls_line-some_other_field = 'VAL3'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select   TYPE string.

    CONCATENATE 'SELECT key_field some_other_field sum( amount ) as amount sum( qty ) as qty'
      'FROM zosql_for_tst2'
      'GROUP BY key_field some_other_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst2.

    f_cut->select_to_itab( EXPORTING iv_select                  = lv_select
                           IMPORTING et_result_table            = lt_result_table ).

    SORT lt_result_table BY key_field some_other_field.

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst2,
          lt_expected_table TYPE TABLE OF zosql_for_tst2.

    ls_expected_line-key_field        = 'KEY1'.
    ls_expected_line-amount           = 30.
    ls_expected_line-qty              = 6.
    ls_expected_line-some_other_field = 'VAL1'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-key_field        = 'KEY1'.
    ls_expected_line-amount           = 5.
    ls_expected_line-qty              = 1.
    ls_expected_line-some_other_field = 'VAL2'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-key_field        = 'KEY2'.
    ls_expected_line-amount           = 70.
    ls_expected_line-qty              = 5.
    ls_expected_line-some_other_field = 'VAL3'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD group_by_with_avg.
    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2.

    " GIVEN
    ls_line-mandt            = sy-mandt.
    ls_line-key_field        = 'KEY1'.
    ls_line-key_field2       = 'KEY1_1'.
    ls_line-amount           = 10.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt            = sy-mandt.
    ls_line-key_field        = 'KEY1'.
    ls_line-key_field2       = 'KEY1_2'.
    ls_line-amount           = 20.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt            = sy-mandt.
    ls_line-key_field        = 'KEY1'.
    ls_line-key_field2       = 'KEY1_3'.
    ls_line-amount           = 30.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select   TYPE string.

    CONCATENATE 'SELECT key_field avg( amount ) as amount'
      'FROM zosql_for_tst2'
      'GROUP BY key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst2.

    f_cut->select_to_itab( EXPORTING iv_select                  = lv_select
                           IMPORTING et_result_table            = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst2,
          lt_expected_table TYPE TABLE OF zosql_for_tst2.

    ls_expected_line-key_field        = 'KEY1'.
    ls_expected_line-amount           = 20.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD group_by_count_distinct.
    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2.

    " GIVEN
    ls_line-mandt            = sy-mandt.
    ls_line-key_field        = 'KEY1'.
    ls_line-key_field2       = 'KEY1_1'.
    ls_line-some_other_field = 'VAL1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt            = sy-mandt.
    ls_line-key_field        = 'KEY1'.
    ls_line-key_field2       = 'KEY1_2'.
    ls_line-some_other_field = 'VAL1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt            = sy-mandt.
    ls_line-key_field        = 'KEY1'.
    ls_line-key_field2       = 'KEY1_3'.
    ls_line-some_other_field = 'VAL2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt            = sy-mandt.
    ls_line-key_field        = 'KEY2'.
    ls_line-key_field2       = 'KEY2_1'.
    ls_line-some_other_field = 'VAL1'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select   TYPE string.

    CONCATENATE 'SELECT key_field COUNT( DISTINCT some_other_field ) as cnt'
      'FROM zosql_for_tst2'
      'GROUP BY key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
             cnt       TYPE i,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select                  = lv_select
                           IMPORTING et_result_table            = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE ty_result,
          lt_expected_table TYPE TABLE OF ty_result.

    ls_expected_line-key_field = 'KEY1'.
    ls_expected_line-cnt       = 2.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-key_field = 'KEY2'.
    ls_expected_line-cnt       = 1.
    APPEND ls_expected_line TO lt_expected_table.

    SORT: lt_result_table, lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD group_by_without_alias.

    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2.

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select   TYPE string.

    CONCATENATE 'SELECT key_field sum( amount ) sum( qty ) count( * )'
      'FROM zosql_for_tst2'
      'GROUP BY key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
             amount    TYPE zosql_for_tst2-amount,
             qty       TYPE zosql_for_tst2-qty,
             cnt       TYPE i,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select                = lv_select
                                     iv_do_into_corresponding = abap_false
                           IMPORTING et_result_table          = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE ty_result,
          lt_expected_table TYPE TABLE OF ty_result.

    ls_expected_line-key_field = 'KEY1'.
    ls_expected_line-amount    = 30.
    ls_expected_line-qty       = 6.
    ls_expected_line-cnt       = 2.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-key_field = 'KEY2'.
    ls_expected_line-amount    = 5.
    ls_expected_line-qty       = 1.
    ls_expected_line-cnt       = 1.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD group_by_having.
    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_1'.
    ls_line-amount      = 10.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_2'.
    ls_line-amount      = 20.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-key_field2  = 'KEY2_1'.
    ls_line-amount      = 5.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select   TYPE string.

    CONCATENATE 'SELECT key_field sum( amount ) as amount'
      'FROM zosql_for_tst2'
      'GROUP BY key_field'
      'HAVING SUM( AMOUNT ) = 30'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
             amount    TYPE zosql_for_tst2-amount,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE ty_result,
          lt_expected_table TYPE TABLE OF ty_result.

    ls_expected_line-key_field = 'KEY1'.
    ls_expected_line-amount    = 30.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD group_by_having_with_param.
    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_1'.
    ls_line-amount      = 10.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_2'.
    ls_line-amount      = 20.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-key_field2  = 'KEY2_1'.
    ls_line-amount      = 5.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select   TYPE string.

    CONCATENATE 'SELECT key_field sum( amount ) as amount'
      'FROM zosql_for_tst2'
      'GROUP BY key_field'
      'HAVING SUM( AMOUNT ) = :TOTAL_AMOUNT'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
             amount    TYPE zosql_for_tst2-amount,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result,
          lt_parameters   TYPE zosql_db_layer_params,
          ls_parameter    TYPE zosql_db_layer_param.

    ls_parameter-param_name_in_select   = ':TOTAL_AMOUNT'.
    ls_parameter-parameter_value_single = '30'.
    APPEND ls_parameter TO lt_parameters.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_parameters
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE ty_result,
          lt_expected_table TYPE TABLE OF ty_result.

    ls_expected_line-key_field = 'KEY1'.
    ls_expected_line-amount    = 30.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD group_by_without_aggr_funcs.
    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2.

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select   TYPE string.

    CONCATENATE 'SELECT key_field'
      'FROM zosql_for_tst2'
      'GROUP BY key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select                  = lv_select
                           IMPORTING et_result_table            = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF ty_result.

    APPEND 'KEY1' TO lt_expected_table.
    APPEND 'KEY2' TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD group_by_field_not_selected.
    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_1'.
    ls_line-amount      = 10.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_2'.
    ls_line-amount      = 20.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-key_field2  = 'KEY2_1'.
    ls_line-amount      = 5.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select   TYPE string.

    CONCATENATE 'SELECT sum( amount ) as amount'
      'FROM zosql_for_tst2'
      'GROUP BY key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             amount TYPE zosql_for_tst2-amount,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select                  = lv_select
                           IMPORTING et_result_table            = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF ty_result,
          ls_expected_table TYPE ty_result.

    ls_expected_table-amount = 30.
    APPEND ls_expected_table TO lt_expected_table.

    ls_expected_table-amount = 5.
    APPEND ls_expected_table TO lt_expected_table.

    SORT: lt_result_table BY amount, lt_expected_table BY amount.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table
                                    msg = 'Error in select with group by where ''GROUP BY'' field not present in ''SELECT'' field list' ).
  ENDMETHOD.

  METHOD order_by_descending.
    DATA: lt_initial_table TYPE TABLE OF zosql_for_tst,
          ls_initial_line  TYPE zosql_for_tst.

    " GIVEN
    ls_initial_line-mandt       = sy-mandt.
    ls_initial_line-key_field   = 'KEY1'.
    ls_initial_line-text_field1 = 'ZZZ'.
    APPEND ls_initial_line TO lt_initial_table.

    ls_initial_line-mandt       = sy-mandt.
    ls_initial_line-key_field   = 'KEY2'.
    ls_initial_line-text_field1 = 'AAA'.
    APPEND ls_initial_line TO lt_initial_table.

    ls_initial_line-mandt       = sy-mandt.
    ls_initial_line-key_field   = 'KEY3'.
    ls_initial_line-text_field1 = 'CCC'.
    APPEND ls_initial_line TO lt_initial_table.

    insert_test_data( lt_initial_table ).

    " WHEN
    DATA: lv_select TYPE string,
          lt_result TYPE TABLE OF zosql_for_tst.

    CONCATENATE 'SELECT *'
                '  FROM zosql_for_tst'
                '  ORDER BY text_field1 DESCENDING'
                INTO lv_select SEPARATED BY space.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zosql_for_tst,
          ls_expected_line  TYPE zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'ZZZ'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY3'.
    ls_expected_line-text_field1 = 'CCC'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'AAA'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected_table ).
  ENDMETHOD.

  METHOD order_by_ascending.
    DATA: lt_initial_table TYPE TABLE OF zosql_for_tst,
          ls_initial_line  TYPE zosql_for_tst.

    " GIVEN
    ls_initial_line-mandt       = sy-mandt.
    ls_initial_line-key_field   = 'KEY1'.
    ls_initial_line-text_field1 = 'ZZZ'.
    APPEND ls_initial_line TO lt_initial_table.

    ls_initial_line-mandt       = sy-mandt.
    ls_initial_line-key_field   = 'KEY2'.
    ls_initial_line-text_field1 = 'AAA'.
    APPEND ls_initial_line TO lt_initial_table.

    ls_initial_line-mandt       = sy-mandt.
    ls_initial_line-key_field   = 'KEY3'.
    ls_initial_line-text_field1 = 'CCC'.
    APPEND ls_initial_line TO lt_initial_table.

    insert_test_data( lt_initial_table ).

    " WHEN
    DATA: lv_select TYPE string,
          lt_result TYPE TABLE OF zosql_for_tst.

    CONCATENATE 'SELECT *'
                '  FROM zosql_for_tst'
                '  ORDER BY text_field1 ASCENDING'
                INTO lv_select SEPARATED BY space.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zosql_for_tst,
          ls_expected_line  TYPE zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'AAA'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY3'.
    ls_expected_line-text_field1 = 'CCC'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'ZZZ'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected_table ).
  ENDMETHOD.

  METHOD order_by_field_not_selected.
    DATA: lt_initial_table TYPE TABLE OF zosql_for_tst,
          ls_line          TYPE zosql_for_tst.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'TEXT3'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'TEXT2'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( lt_initial_table ).

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst-key_field,
           END OF ty_result.

    DATA: lt_result TYPE TABLE OF ty_result,
          lv_select TYPE string.

    CONCATENATE
      'SELECT KEY_FIELD FROM zosql_for_tst'
      '  ORDER BY text_field1'
      INTO lv_select SEPARATED BY space.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF ty_result.

    APPEND 'KEY2' TO lt_expected_table.
    APPEND 'KEY1' TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected_table
                                    msg = 'Error when ORDER BY is performed on field not selected to resulting data set' ).
  ENDMETHOD.

  METHOD order_by_when_empty_result.

    " WHEN
    DATA: lt_result TYPE TABLE OF zosql_for_tst,
          lv_select TYPE string.

    lv_select = 'SELECT * FROM zosql_for_tst ORDER BY PRIMARY KEY'.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result ).

    " THEN
    cl_aunit_assert=>assert_initial( lt_result ).
  ENDMETHOD.

  METHOD order_by_2_fields.

    DATA: lt_initial_table TYPE TABLE OF zosql_for_tst,
          ls_initial_line  TYPE zosql_for_tst.

    " GIVEN
    ls_initial_line-mandt       = sy-mandt.
    ls_initial_line-key_field   = 'KEY1'.
    ls_initial_line-text_field1 = 'ZZZ'.
    ls_initial_line-text_field2 = 'AAA'.
    APPEND ls_initial_line TO lt_initial_table.

    ls_initial_line-mandt       = sy-mandt.
    ls_initial_line-key_field   = 'KEY2'.
    ls_initial_line-text_field1 = 'ZZZ'.
    ls_initial_line-text_field2 = 'CCC'.
    APPEND ls_initial_line TO lt_initial_table.

    ls_initial_line-mandt       = sy-mandt.
    ls_initial_line-key_field   = 'KEY3'.
    ls_initial_line-text_field1 = 'BBB'.
    ls_initial_line-text_field2 = 'ZZZ'.
    APPEND ls_initial_line TO lt_initial_table.

    ls_initial_line-mandt       = sy-mandt.
    ls_initial_line-key_field   = 'KEY4'.
    ls_initial_line-text_field1 = 'BBB'.
    ls_initial_line-text_field2 = 'AAA'.
    APPEND ls_initial_line TO lt_initial_table.

    insert_test_data( lt_initial_table ).

    " WHEN
    DATA: lv_select TYPE string,
          lt_result TYPE TABLE OF zosql_for_tst.

    CONCATENATE 'SELECT *'
                '  FROM zosql_for_tst'
                '  ORDER BY text_field1 text_field2'
                INTO lv_select SEPARATED BY space.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zosql_for_tst,
          ls_expected_line  TYPE zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY4'.
    ls_expected_line-text_field1 = 'BBB'.
    ls_expected_line-text_field2 = 'AAA'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY3'.
    ls_expected_line-text_field1 = 'BBB'.
    ls_expected_line-text_field2 = 'ZZZ'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'ZZZ'.
    ls_expected_line-text_field2 = 'AAA'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'ZZZ'.
    ls_expected_line-text_field2 = 'CCC'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected_table ).
  ENDMETHOD.

  METHOD order_by_with_join.

    DATA: lt_initial_table  TYPE TABLE OF zosql_for_tst,
          ls_line           TYPE zosql_for_tst,
          lt_initial_table2 TYPE TABLE OF zosql_for_tst2,
          ls_line2          TYPE zosql_for_tst2.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'TEXT1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'TEXT2_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line2-mandt      = sy-mandt.
    ls_line2-key_field  = 'KEY1'.
    ls_line2-key_field2 = 'KEY1_1'.
    ls_line2-amount     = 50.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-mandt      = sy-mandt.
    ls_line2-key_field  = 'KEY1'.
    ls_line2-key_field2 = 'KEY1_2'.
    ls_line2-amount     = 30.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-mandt      = sy-mandt.
    ls_line2-key_field  = 'KEY2'.
    ls_line2-key_field2 = 'KEY2_1'.
    ls_line2-amount     = 20.
    APPEND ls_line2 TO lt_initial_table2.

    insert_test_data( lt_initial_table ).
    insert_test_data( lt_initial_table2 ).

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field   TYPE zosql_for_tst-key_field,
             key_field2  TYPE zosql_for_tst2-key_field2,
             text_field1 TYPE zosql_for_tst-text_field1,
             amount      TYPE zosql_for_tst2-amount,
           END OF ty_result.

    DATA: lv_select TYPE string,
          lt_result TYPE TABLE OF ty_result.

    CONCATENATE 'SELECT t1~key_field t2~key_field2 t1~text_field1 t2~amount'
                '  FROM zosql_for_tst AS t1'
                '  JOIN zosql_for_tst2 AS t2 ON t2~key_field = t1~key_field'
                '  ORDER BY t2~amount t1~text_field1'
                INTO lv_select SEPARATED BY space.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF ty_result,
          ls_expected_line  TYPE ty_result.

    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-key_field2  = 'KEY2_1'.
    ls_expected_line-text_field1 = 'TEXT2_1'.
    ls_expected_line-amount      = 20.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-key_field2  = 'KEY1_2'.
    ls_expected_line-text_field1 = 'TEXT1_1'.
    ls_expected_line-amount      = 30.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-key_field2  = 'KEY1_1'.
    ls_expected_line-text_field1 = 'TEXT1_1'.
    ls_expected_line-amount      = 50.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected_table ).
  ENDMETHOD.

  METHOD for_all_ent_compare_2_fld.

    " SELECT ... FOR ALL ENTRIES IN ... and in where condition there is 2 comparison with FAE base table fields

    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2,
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

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST2'
      'FOR ALL ENTRIES IN LT_FOR_ALL_ENTRIES_TAB'
      'WHERE KEY_FIELD  = LT_FOR_ALL_ENTRIES_TAB-KEY_FIELD'
      '  AND KEY_FIELD2 = LT_FOR_ALL_ENTRIES_TAB-KEY_FIELD2'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_for_all_entries_tab  TYPE TABLE OF zosql_for_tst2,
          ls_for_all_entries_line TYPE zosql_for_tst2,
          lt_result               TYPE TABLE OF zosql_for_tst2.

    ls_for_all_entries_line-key_field  = 'KEY1'.
    ls_for_all_entries_line-key_field2 = 'KEY1_1'.
    APPEND ls_for_all_entries_line TO lt_for_all_entries_tab.

    ls_for_all_entries_line-key_field  = 'KEY2'.
    ls_for_all_entries_line-key_field2 = 'KEY2_1'.
    APPEND ls_for_all_entries_line TO lt_for_all_entries_tab.

    f_cut->select_to_itab( EXPORTING iv_select                = lv_select
                                     it_for_all_entries_table = lt_for_all_entries_tab
                           IMPORTING et_result_table          = lt_result ).

    " THEN
    DATA: lt_expected TYPE TABLE OF zosql_for_tst2,
          ls_expected TYPE zosql_for_tst2.

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

  METHOD select_from_table_with_include.

    DATA: lt_initial_table TYPE TABLE OF zosql_for_tst4,
          ls_line          TYPE zosql_for_tst4.

    " GIVEN
    ls_line-mandt = sy-mandt.
    ls_line-key_field1 = 'KEY1_1'.
    ls_line-key_field2 = 'KEY2_1'.
    ls_line-field1     = 'FIELD1_1'.
    ls_line-field2     = 'FIELD2_1'.
    ls_line-field3     = 'FIELD3_1'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( lt_initial_table ).

    " WHEN
    DATA: ld_result TYPE REF TO data,
          lt_result TYPE TABLE OF zosql_for_tst4.

    FIELD-SYMBOLS: <lt_result> TYPE STANDARD TABLE.

    f_cut->select( EXPORTING iv_select          = 'SELECT * FROM zosql_for_tst4'
                   IMPORTING ed_result_as_table = ld_result ).

    ASSIGN ld_result->* TO <lt_result>.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_result>
                                               IMPORTING et_table_dest = lt_result ).

    " THEN
    DATA: lt_expected  TYPE TABLE OF zosql_for_tst4.

    lt_expected = lt_initial_table.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected ).
  ENDMETHOD.

  METHOD select_order_by.
    DATA: lt_initial_table TYPE TABLE OF zosql_for_tst,
          ls_line          TYPE zosql_for_tst.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'TEXT3'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'TEXT2'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( lt_initial_table ).

    " WHEN
    DATA: lt_result TYPE TABLE OF zosql_for_tst,
          lv_select TYPE string.

    CONCATENATE
      'SELECT * FROM zosql_for_tst'
      '  ORDER BY text_field1'
      INTO lv_select SEPARATED BY space.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zosql_for_tst,
          ls_expected_line  TYPE zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'TEXT2'.
    APPEND ls_expected_line TO lt_expected_table.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'TEXT3'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected_table ).
  ENDMETHOD.

  METHOD sql_separated_by_line_breaks.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

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

    insert_test_data( lt_initial_table ).

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

    CONCATENATE 'select *'
      'FROM zosql_for_tst'
      'where key_field = :KEY_FIELD'
      INTO lv_select SEPARATED BY cl_abap_char_utilities=>cr_lf.

    ls_param-param_name_in_select   = ':KEY_FIELD'.
    ls_param-parameter_value_single = 'KEY2'.
    APPEND ls_param TO lt_params.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.



    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_params
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD param_with_name_like_field.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'WHERE KEY_FIELD = IT_IS~PARAM'
      INTO lv_select SEPARATED BY space.

    ls_param-param_name_in_select   = 'IT_IS~PARAM'.
    ls_param-parameter_value_single = 'KEY2'.
    APPEND ls_param TO lt_params.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_params
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zosql_for_tst,
          ls_expected_line  TYPE zosql_for_tst.

    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    ls_expected_line-text_field2 = 'VALUE2_2'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD view_user_addr.

    " It is a bug found when selecting from view USER_ADDR in 'fake' mode

    DATA: lt_usr21     TYPE TABLE OF usr21,
          ls_usr21     TYPE usr21,
          lt_adrc      TYPE TABLE OF adrc,
          ls_adrc      TYPE adrc,
          lt_adrp      TYPE TABLE OF adrp,
          ls_adrp      TYPE adrp,
          lt_adcp      TYPE TABLE OF adcp,
          ls_adcp      TYPE adcp,
          lt_uscompany TYPE TABLE OF uscompany,
          ls_uscompany TYPE uscompany.

    " GIVEN
    ls_usr21-mandt      = sy-mandt.
    ls_usr21-bname      = 'USER1'.
    ls_usr21-addrnumber = 1001.
    ls_usr21-persnumber = 2001.
    APPEND ls_usr21 TO lt_usr21.

    ls_usr21-mandt      = sy-mandt.
    ls_usr21-bname      = 'USER2'.
    ls_usr21-addrnumber = 1002.
    ls_usr21-persnumber = 2002.
    APPEND ls_usr21 TO lt_usr21.

    ls_adrc-client     = sy-mandt.
    ls_adrc-addrnumber = 1001.
    APPEND ls_adrc TO lt_adrc.

    ls_adrc-client     = sy-mandt.
    ls_adrc-addrnumber = 1002.
    APPEND ls_adrc TO lt_adrc.

    ls_adrp-client     = sy-mandt.
    ls_adrp-persnumber = 2001.
    ls_adrp-name_first = 'USER1_Name'.
    ls_adrp-pers_group = 'BC01'.
    APPEND ls_adrp TO lt_adrp.

    ls_adrp-client     = sy-mandt.
    ls_adrp-persnumber = 2002.
    ls_adrp-name_first = 'USER2_Name'.
    ls_adrp-pers_group = 'BC01'.
    APPEND ls_adrp TO lt_adrp.

    ls_adcp-client     = sy-mandt.
    ls_adcp-addrnumber = 1001.
    ls_adcp-persnumber = 2001.
    APPEND ls_adcp TO lt_adcp.

    ls_adcp-client     = sy-mandt.
    ls_adcp-addrnumber = 1002.
    ls_adcp-persnumber = 2002.
    APPEND ls_adcp TO lt_adcp.

    ls_uscompany-mandt      = sy-mandt.
    ls_uscompany-company    = 3001.
    ls_uscompany-addrnumber = 1001.
    APPEND ls_uscompany TO lt_uscompany.

    ls_uscompany-mandt      = sy-mandt.
    ls_uscompany-company    = 3002.
    ls_uscompany-addrnumber = 1002.
    APPEND ls_uscompany TO lt_uscompany.

    insert_test_data( lt_usr21 ).
    insert_test_data( lt_adrc ).
    insert_test_data( lt_adrp ).
    insert_test_data( lt_adcp ).
    insert_test_data( lt_uscompany ).

    " WHEN
    TYPES: BEGIN OF ty_result,
             bname      TYPE user_addr-bname,
             name_first TYPE user_addr-name_first,
           END OF ty_result.

    DATA: lv_select TYPE string,
          lt_result TYPE TABLE OF ty_result.

    CONCATENATE 'SELECT bname name_first'
      'FROM user_addr'
      'WHERE bname = ''USER1'''
      INTO lv_select SEPARATED BY space.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result ).

    " THEN
    DATA: lt_expected_result TYPE TABLE OF ty_result,
          ls_expected_result TYPE ty_result.

    ls_expected_result-bname      = 'USER1'.
    ls_expected_result-name_first = 'USER1_Name'.
    APPEND ls_expected_result TO lt_expected_result.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected_result ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_select_740 IMPLEMENTATION.

  METHOD new_syntax.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( it_table = lt_initial_table ).

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

    CONCATENATE 'SELECT key_field, text_field1'
      'FROM zosql_for_tst'
      'WHERE KEY_FIELD = @:KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    ls_param-param_name_in_select   = ':KEY_FIELD'.
    ls_param-parameter_value_single = 'KEY2'.
    APPEND ls_param TO lt_params.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_params
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD new_syntax_no_host_var.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( it_table = lt_initial_table ).

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

    CONCATENATE 'SELECT key_field, text_field1'
      'FROM zosql_for_tst'
      'WHERE KEY_FIELD = :KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    ls_param-param_name_in_select   = ':KEY_FIELD'.
    ls_param-parameter_value_single = 'KEY2'.
    APPEND ls_param TO lt_params.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_params
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD new_syntax_no_space_selfld.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

    " Case of new syntax when there is no spaces between fields in select

    " GIVEN
    DELETE FROM zosql_for_tst.

    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( lt_initial_table ).

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

    CONCATENATE 'SELECT key_field,text_field1'
      'FROM zosql_for_tst'
      'WHERE KEY_FIELD = @:KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    ls_param-param_name_in_select   = ':KEY_FIELD'.
    ls_param-parameter_value_single = 'KEY2'.
    APPEND ls_param TO lt_params.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_params
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    ls_expected_line-key_field   = 'KEY2'.
    ls_expected_line-text_field1 = 'VALUE2_1'.
    APPEND ls_expected_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD group_by_having_subquery.
    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2.

    " GIVEN
    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_1'.
    ls_line-amount      = 10.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY1'.
    ls_line-key_field2  = 'KEY1_2'.
    ls_line-amount      = 20.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-key_field2  = 'KEY2_1'.
    ls_line-amount      = 5.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY2'.
    ls_line-key_field2  = 'KEY2_2'.
    ls_line-amount      = 10.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY3'.
    ls_line-key_field2  = 'KEY3_1'.
    ls_line-amount      = 3.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt       = sy-mandt.
    ls_line-key_field   = 'KEY3'.
    ls_line-key_field2  = 'KEY3_2'.
    ls_line-amount      = 7.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select   TYPE string.

    CONCATENATE 'SELECT key_field'
      'FROM zosql_for_tst2'
      'GROUP BY key_field'
      'HAVING SUM( AMOUNT ) = ( SELECT amount'
      '                           FROM zosql_for_tst2'
      '                           WHERE key_field = ''KEY1'''
      '                             AND key_field2 = ''KEY1_1'' )'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF ty_result.

    APPEND 'KEY3' TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD select_exists_subquery.
    " Test for subquery in where
    " Limitation: Only new syntax supports subquery in dynamic WHERE
    " Therefore feature is available only with new syntax since 7.40 version

    DATA: ls_line           TYPE zosql_for_tst,
          lt_initial_table  TYPE TABLE OF zosql_for_tst,
          ls_line2          TYPE zosql_for_tst2,
          lt_initial_table2 TYPE TABLE OF zosql_for_tst2,
          lv_select         TYPE string.

    " GIVEN
    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY2'.
    APPEND ls_line TO lt_initial_table.

    ls_line2-mandt     = sy-mandt.
    ls_line2-key_field = 'KEY2'.
    APPEND ls_line2 TO lt_initial_table2.

    insert_test_data( lt_initial_table ).
    insert_test_data( lt_initial_table2 ).

    CONCATENATE 'SELECT key_field'
      'FROM ZOSQL_FOR_TST AS T1'
      'WHERE EXISTS ( SELECT *'
      '                 FROM ZOSQL_FOR_TST2 AS T2'
      '                 WHERE T2~KEY_FIELD = T1~KEY_FIELD )'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
           END OF ty_result.

    DATA: ls_result_line  TYPE ty_result,
          lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected TYPE ty_result,
          lt_expected TYPE TABLE OF ty_result.

    ls_expected-key_field = 'KEY2'.
    APPEND ls_expected TO lt_expected.

    " THEN
    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected ).
  ENDMETHOD.

  METHOD select_not_exists_subquery.
    " Test for subquery in where
    " Limitation: Only new syntax supports subquery in dynamic WHERE
    " Therefore feature is available only with new syntax since 7.40 version

    DATA: ls_line           TYPE zosql_for_tst,
          lt_initial_table  TYPE TABLE OF zosql_for_tst,
          ls_line2          TYPE zosql_for_tst2,
          lt_initial_table2 TYPE TABLE OF zosql_for_tst2,
          lv_select         TYPE string.

    " GIVEN
    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY2'.
    APPEND ls_line TO lt_initial_table.

    ls_line2-mandt     = sy-mandt.
    ls_line2-key_field = 'KEY2'.
    APPEND ls_line2 TO lt_initial_table2.

    insert_test_data( lt_initial_table ).
    insert_test_data( lt_initial_table2 ).

    CONCATENATE 'SELECT key_field'
      'FROM ZOSQL_FOR_TST AS T1'
      'WHERE NOT EXISTS ( SELECT *'
      '                     FROM ZOSQL_FOR_TST2 AS T2'
      '                     WHERE T2~KEY_FIELD = T1~KEY_FIELD )'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
           END OF ty_result.

    DATA: ls_result_line  TYPE ty_result,
          lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected TYPE ty_result,
          lt_expected TYPE TABLE OF ty_result.

    ls_expected-key_field = 'KEY1'.
    APPEND ls_expected TO lt_expected.

    " THEN
    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected ).
  ENDMETHOD.

  METHOD select_subquery_where_eq.

    " Test for subquery in where
    " Limitation: Only new syntax supports subquery in dynamic WHERE
    " Therefore feature is available only with new syntax since 7.40 version

    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY1'.
    ls_line-amount    = 10.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY2'.
    ls_line-amount    = 20.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT key_field'
      'FROM ZOSQL_FOR_TST2'
      'WHERE AMOUNT = ( SELECT MAX( AMOUNT )'
      '                   FROM ZOSQL_FOR_TST2 )'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
           END OF ty_result.

    DATA: ls_result_line  TYPE ty_result.

    f_cut->select_to_itab( EXPORTING iv_select      = lv_select
                           IMPORTING es_result_line = ls_result_line ).

    " THEN
    cl_aunit_assert=>assert_equals( act = ls_result_line-key_field exp = 'KEY2' ).
  ENDMETHOD.

  METHOD select_subquery_any.
    " Test for subquery in where
    " Limitation: Only new syntax supports subquery in dynamic WHERE
    " Therefore feature is available only with new syntax since 7.40 version

    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY1'.
    ls_line-amount    = 10.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY2'.
    ls_line-amount    = 20.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY3'.
    ls_line-amount    = 15.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT key_field'
      'FROM ZOSQL_FOR_TST2'
      'WHERE AMOUNT > ANY ( SELECT AMOUNT'
      '                       FROM ZOSQL_FOR_TST2 )'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF ty_result.

    APPEND 'KEY2' TO lt_expected_table.
    APPEND 'KEY3' TO lt_expected_table.

    SORT: lt_result_table, lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD select_subquery_all.
    " Test for subquery in where
    " Limitation: Only new syntax supports subquery in dynamic WHERE
    " Therefore feature is available only with new syntax since 7.40 version

    DATA: ls_line          TYPE zosql_for_tst2,
          lt_initial_table TYPE TABLE OF zosql_for_tst2,
          lv_select        TYPE string.

    " GIVEN
    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY1'.
    ls_line-amount    = 10.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY2'.
    ls_line-amount    = 20.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY3'.
    ls_line-amount    = 15.
    APPEND ls_line TO lt_initial_table.

    insert_test_data( lt_initial_table ).

    CONCATENATE 'SELECT key_field'
      'FROM ZOSQL_FOR_TST2'
      'WHERE AMOUNT >= ALL ( SELECT AMOUNT'
      '                        FROM ZOSQL_FOR_TST2 )'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
           END OF ty_result.

    DATA: lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF ty_result.

    APPEND 'KEY2' TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD subquery_in.
    " Test for subquery in where
    " Limitation: Only new syntax supports subquery in dynamic WHERE
    " Therefore feature is available only with new syntax since 7.40 version

    DATA: ls_line           TYPE zosql_for_tst,
          lt_initial_table  TYPE TABLE OF zosql_for_tst,
          ls_line2          TYPE zosql_for_tst2,
          lt_initial_table2 TYPE TABLE OF zosql_for_tst2,
          lv_select         TYPE string.

    " GIVEN
    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY3'.
    APPEND ls_line TO lt_initial_table.

    ls_line2-mandt     = sy-mandt.
    ls_line2-key_field = 'KEY1'.
    ls_line2-amount    = 20.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-mandt     = sy-mandt.
    ls_line2-key_field = 'KEY2'.
    ls_line2-amount    = 10.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-mandt     = sy-mandt.
    ls_line2-key_field = 'KEY3'.
    ls_line2-amount    = 30.
    APPEND ls_line2 TO lt_initial_table2.

    insert_test_data( lt_initial_table ).
    insert_test_data( lt_initial_table2 ).

    CONCATENATE 'SELECT key_field'
      'FROM ZOSQL_FOR_TST'
      'WHERE KEY_FIELD IN ( SELECT KEY_FIELD'
      '                       FROM ZOSQL_FOR_TST2'
      '                       WHERE AMOUNT > 15 )'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
           END OF ty_result.

    DATA: ls_result_line  TYPE ty_result,
          lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected TYPE ty_result,
          lt_expected TYPE TABLE OF ty_result.

    ls_expected-key_field = 'KEY1'.
    APPEND ls_expected TO lt_expected.

    ls_expected-key_field = 'KEY3'.
    APPEND ls_expected TO lt_expected.

    " THEN
    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected ).
  ENDMETHOD.

  METHOD subquery_not_in.
    " Test for subquery in where
    " Limitation: Only new syntax supports subquery in dynamic WHERE
    " Therefore feature is available only with new syntax since 7.40 version

    DATA: ls_line           TYPE zosql_for_tst,
          lt_initial_table  TYPE TABLE OF zosql_for_tst,
          ls_line2          TYPE zosql_for_tst2,
          lt_initial_table2 TYPE TABLE OF zosql_for_tst2,
          lv_select         TYPE string.

    " GIVEN
    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY2'.
    APPEND ls_line TO lt_initial_table.

    ls_line-mandt     = sy-mandt.
    ls_line-key_field = 'KEY3'.
    APPEND ls_line TO lt_initial_table.

    ls_line2-mandt     = sy-mandt.
    ls_line2-key_field = 'KEY1'.
    ls_line2-amount    = 20.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-mandt     = sy-mandt.
    ls_line2-key_field = 'KEY2'.
    ls_line2-amount    = 10.
    APPEND ls_line2 TO lt_initial_table2.

    ls_line2-mandt     = sy-mandt.
    ls_line2-key_field = 'KEY3'.
    ls_line2-amount    = 30.
    APPEND ls_line2 TO lt_initial_table2.

    insert_test_data( lt_initial_table ).
    insert_test_data( lt_initial_table2 ).

    CONCATENATE 'SELECT key_field'
      'FROM ZOSQL_FOR_TST'
      'WHERE KEY_FIELD NOT IN ( SELECT KEY_FIELD'
      '                           FROM ZOSQL_FOR_TST2'
      '                           WHERE AMOUNT > 15 )'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
           END OF ty_result.

    DATA: ls_result_line  TYPE ty_result,
          lt_result_table TYPE TABLE OF ty_result.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected TYPE ty_result,
          lt_expected TYPE TABLE OF ty_result.

    APPEND 'KEY2' TO lt_expected.

    " THEN
    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected ).
  ENDMETHOD.

  METHOD error_equal_and_range_in_par.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst.

    " Here is no new syntax but in systems lower 7.40 in real database mode
    " it falls into not catchable dump unlike 7.40 and higher
    " That's whe the test was moved to 7.40+ group

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

    insert_test_data( it_table = lt_initial_table ).

    DATA: lv_select  TYPE string.

    DATA: lt_params           TYPE zosql_db_layer_params,
          ls_param            TYPE zosql_db_layer_param,
          ls_param_range_line TYPE zosql_db_layer_range_line.

    ls_param-param_name_in_select = ':TEXT_FIELD'.
    ls_param_range_line-sign   = 'I'.
    ls_param_range_line-option = 'EQ'.
    ls_param_range_line-low    = 'VALUE1_1'.
    APPEND ls_param_range_line TO ls_param-parameter_value_range.

    ls_param_range_line-low    = 'VALUE3_1'.
    APPEND ls_param_range_line TO ls_param-parameter_value_range.
    APPEND ls_param TO lt_params.

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'WHERE TEXT_FIELD1 = :TEXT_FIELD'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = lv_select
                                     it_parameters   = lt_params
                           IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF zosql_for_tst,
          ls_expected_line  TYPE zosql_for_tst.

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
ENDCLASS.

CLASS ltc_cases_for_insert IMPLEMENTATION.
  METHOD insert_by_itab.
    DATA: ls_line         TYPE zosql_for_tst,
          lt_insert_table TYPE TABLE OF zosql_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_insert_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_insert_table.

    " WHEN
    f_cut->insert_by_itab( iv_table_name = 'ZOSQL_FOR_TST'
                           it_new_lines  = lt_insert_table ).

    " THEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
                           IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_insert_table ).
  ENDMETHOD.

  METHOD insert_by_itab_subrc_4.

    DATA: ls_line         TYPE zosql_for_tst,
          lt_insert_table TYPE TABLE OF zosql_for_tst,
          lv_subrc        TYPE sysubrc.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_insert_table.

    insert_test_data( lt_insert_table ).

    " WHEN
    lv_subrc = f_cut->insert_by_itab( iv_table_name = 'ZOSQL_FOR_TST'
                                      it_new_lines  = lt_insert_table ).

    cl_aunit_assert=>assert_equals( act = lv_subrc exp = 4 ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_update IMPLEMENTATION.
  METHOD update_by_itab.
    DATA: ls_line                TYPE zosql_for_tst,
          lt_table_before_update TYPE TABLE OF zosql_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_update.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_update.

    insert_test_data( it_table = lt_table_before_update ).

    " WHEN
    DATA: lt_update_table TYPE TABLE OF zosql_for_tst.

    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE_1_1_UPD'.
    APPEND ls_line TO lt_update_table.

    ls_line-key_field = 'KEY2'.
    ls_line-text_field1 = 'VALUE_2_1_UPD'.
    APPEND ls_line TO lt_update_table.

    f_cut->update_by_itab( iv_table_name       = 'ZOSQL_FOR_TST'
                           it_lines_for_update = lt_update_table ).

    " THEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
                           IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_update_table ).
  ENDMETHOD.

  METHOD by_sql_no_params.
    DATA: ls_line                TYPE zosql_for_tst,
          lt_table_before_update TYPE TABLE OF zosql_for_tst,
          lv_update_statement    TYPE string.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_update.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_update.

    insert_test_data( lt_table_before_update ).

    " WHEN
    CONCATENATE 'UPDATE ZOSQL_FOR_TST'
      'SET text_field2 = ''NEW_VAL_FIELD2'''
      'WHERE TEXT_FIELD1 = ''VALUE2_1'''
      INTO lv_update_statement SEPARATED BY space.

    f_cut->update( lv_update_statement ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
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
    DATA: ls_line                TYPE zosql_for_tst,
          lt_table_before_update TYPE TABLE OF zosql_for_tst,
          lv_update_statement    TYPE string.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_update.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_update.

    insert_test_data( lt_table_before_update ).

    " WHEN
    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param.

    ls_param-param_name_in_select   = ':TEXT_FIELD1'.
    ls_param-parameter_value_single = 'VALUE2_1'.
    APPEND ls_param TO lt_params.

    CONCATENATE 'UPDATE ZOSQL_FOR_TST'
      'SET text_field2 = ''NEW_VAL_FIELD2'''
      'WHERE TEXT_FIELD1 = :TEXT_FIELD1'
      INTO lv_update_statement SEPARATED BY space.

    f_cut->update( iv_update_statement = lv_update_statement
                   it_parameters       = lt_params ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
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
    DATA: ls_line                TYPE zosql_for_tst,
          lt_table_before_update TYPE TABLE OF zosql_for_tst,
          lv_update_statement    TYPE string.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_update.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_update.

    insert_test_data( lt_table_before_update ).

    " WHEN
    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param.

    ls_param-param_name_in_select   = ':TEXT_FIELD2'.
    ls_param-parameter_value_single = 'NEW_VAL_FIELD2'.
    APPEND ls_param TO lt_params.

    CONCATENATE 'UPDATE ZOSQL_FOR_TST'
      'SET text_field2 = :TEXT_FIELD2'
      'WHERE TEXT_FIELD1 = ''VALUE2_1'''
      INTO lv_update_statement SEPARATED BY space.

    f_cut->update( iv_update_statement = lv_update_statement
                   it_parameters       = lt_params ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
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

  METHOD update_by_itab_subrc_4.
    DATA: ls_line  TYPE zosql_for_tst,
          lv_subrc TYPE sysubrc.

    " WHEN
    DATA: lt_update_table TYPE TABLE OF zosql_for_tst.

    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE_1_1_UPD'.
    APPEND ls_line TO lt_update_table.

    lv_subrc = f_cut->update_by_itab( iv_table_name       = 'ZOSQL_FOR_TST'
                                      it_lines_for_update = lt_update_table ).

    " THEN
    cl_aunit_assert=>assert_equals( act = lv_subrc exp = 4 ).
  ENDMETHOD.

  METHOD update_by_sql_subrc_4.
    DATA: ls_line             TYPE zosql_for_tst,
          lv_update_statement TYPE string,
          lv_subrc            TYPE sysubrc.

    " WHEN
    CONCATENATE 'UPDATE ZOSQL_FOR_TST'
      'SET text_field2 = ''NEW_VAL_FIELD2'''
      'WHERE TEXT_FIELD1 = ''VALUE2_1'''
      INTO lv_update_statement SEPARATED BY space.

    lv_subrc = f_cut->update( lv_update_statement ).

    " THEN
    cl_aunit_assert=>assert_equals( act = lv_subrc exp = 4 ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_update_740 IMPLEMENTATION.
  METHOD by_sql_new_syntax.
    DATA: ls_line                TYPE zosql_for_tst,
          lt_table_before_update TYPE TABLE OF zosql_for_tst,
          lv_update_statement    TYPE string.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_update.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_update.

    insert_test_data( lt_table_before_update ).

    " WHEN
    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param.

    ls_param-param_name_in_select   = ':TEXT_FIELD1'.
    ls_param-parameter_value_single = 'VALUE2_1'.
    APPEND ls_param TO lt_params.

    ls_param-param_name_in_select   = ':TEXT_FIELD2'.
    ls_param-parameter_value_single = 'NEW_VAL_FIELD2'.
    APPEND ls_param TO lt_params.

    CONCATENATE 'UPDATE ZOSQL_FOR_TST'
      'SET text_field2 = @:TEXT_FIELD2'
      'WHERE TEXT_FIELD1 = @:TEXT_FIELD1'
      INTO lv_update_statement SEPARATED BY space.

    f_cut->update( iv_update_statement = lv_update_statement
                   it_parameters       = lt_params ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
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
    DATA: ls_line                TYPE zosql_for_tst,
          lt_table_before_modify TYPE TABLE OF zosql_for_tst.

    " GIVEN
    DELETE FROM zosql_for_tst.

    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_modify.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_modify.

    insert_test_data( lt_table_before_modify ).

    " WHEN
    DATA: lt_modify_table TYPE TABLE OF zosql_for_tst.

    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE_1_1_MOD'.
    APPEND ls_line TO lt_modify_table.

    ls_line-key_field = 'KEY3'.
    ls_line-text_field1 = 'VALUE_3_1_MOD'.
    APPEND ls_line TO lt_modify_table.

    f_cut->modify_by_itab( iv_table_name       = 'ZOSQL_FOR_TST'
                           it_lines_for_modify = lt_modify_table ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
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

    f_cut->delete_by_itab( iv_table_name       = 'ZOSQL_FOR_TST'
                           it_lines_for_delete = lt_delete_table ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
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

    f_cut->delete( lv_delete_statement ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
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

    f_cut->delete( iv_delete_statement = lv_delete_statement
                   it_parameters       = lt_params ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    f_cut->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
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

    lv_subrc = f_cut->delete_by_itab( iv_table_name       = 'ZOSQL_FOR_TST'
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

    lv_subrc = f_cut->delete( lv_delete_statement ).

    " THEN
    cl_aunit_assert=>assert_equals( exp = lv_subrc act = 4 ).
  ENDMETHOD.
ENDCLASS.
