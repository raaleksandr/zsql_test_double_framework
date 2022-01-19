CLASS ltc_zosql_db_layer DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
 INHERITING FROM zcl_zosql_unitbase.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_zosql_db_layer
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ZOSQL_DB_LAYER
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
      f_cut TYPE REF TO zcl_zosql_db_layer.  "class under test

    METHODS: setup,
      teardown,
      _clear_test_database_tables,
      one_table_no_conditions FOR TESTING RAISING zcx_zosql_error,
      one_table_where_static FOR TESTING RAISING zcx_zosql_error,
      one_table_where_param_eq FOR TESTING RAISING zcx_zosql_error,
      select_in_range FOR TESTING RAISING zcx_zosql_error,
      select_not_in_range FOR TESTING RAISING zcx_zosql_error,
      one_table_where_param_ref FOR TESTING RAISING zcx_zosql_error,
      one_table_2_params_with_or FOR TESTING RAISING zcx_zosql_error,
      one_table_for_all_entries FOR TESTING RAISING zcx_zosql_error,
      one_table_distinct FOR TESTING RAISING zcx_zosql_error,
      group_by FOR TESTING RAISING zcx_zosql_error,
      group_by_count_distinct FOR TESTING RAISING zcx_zosql_error,
      group_by_without_alias FOR TESTING RAISING zcx_zosql_error,
      group_by_having FOR TESTING RAISING zcx_zosql_error,
      join FOR TESTING RAISING zcx_zosql_error,
      new_syntax FOR TESTING RAISING zcx_zosql_error,
      new_syntax_no_host_var FOR TESTING RAISING zcx_zosql_error,
      new_syntax_no_space_selfld FOR TESTING RAISING zcx_zosql_error,
      select_from_view FOR TESTING RAISING zcx_zosql_error,
      select_as_ref_to_data FOR TESTING RAISING zcx_zosql_error,
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
      select_count_star_no_space FOR TESTING RAISING zcx_zosql_error,
      select_subquery_where_eq FOR TESTING RAISING zcx_zosql_error,
      select_subquery_all FOR TESTING RAISING zcx_zosql_error,
      select_between FOR TESTING RAISING zcx_zosql_error,
      select_not_between FOR TESTING RAISING zcx_zosql_error,
      select_like_escape FOR TESTING RAISING zcx_zosql_error,
      select_param_in_join FOR TESTING RAISING zcx_zosql_error,
      select_exists_subquery FOR TESTING RAISING zcx_zosql_error,
      select_not_exists_subquery FOR TESTING RAISING zcx_zosql_error,
      open_cursor_fetch_itab FOR TESTING RAISING zcx_zosql_error,
      open_cursor_fetch FOR TESTING RAISING zcx_zosql_error,
      insert_by_itab FOR TESTING RAISING zcx_zosql_error,
      update_by_itab FOR TESTING RAISING zcx_zosql_error,
      modify_by_itab FOR TESTING RAISING zcx_zosql_error,
      delete_by_itab FOR TESTING RAISING zcx_zosql_error,
      update_by_sql_no_params FOR TESTING RAISING zcx_zosql_error,
      update_by_sql_with_params FOR TESTING RAISING zcx_zosql_error,
      update_by_sql_params_set FOR TESTING RAISING zcx_zosql_error,
      update_by_sql_new_syntax FOR TESTING RAISING zcx_zosql_error,
      delete_by_sql_no_params FOR TESTING RAISING zcx_zosql_error,
      delete_by_sql_with_params FOR TESTING RAISING zcx_zosql_error.
ENDCLASS.       "ltc_zosql_db_layer


CLASS ltc_zosql_db_layer IMPLEMENTATION.

  METHOD setup.
    f_cut ?= zcl_zosql_test_environment=>get_db_layer_for_production( ).
    _clear_test_database_tables( ).
  ENDMETHOD.

  METHOD teardown.
    _clear_test_database_tables( ).
  ENDMETHOD.

  METHOD _clear_test_database_tables.
    DELETE FROM zosql_for_tst.
    DELETE FROM zosql_for_tst2.
    DELETE FROM zosql_for_tst3.
  ENDMETHOD.

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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst,
          ls_result_line  TYPE zosql_for_tst,
          lv_subrc        TYPE sysubrc.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table
                                                        es_result_line  = ls_result_line
                                                        ev_subrc        = lv_subrc ).

    " THEN
    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_initial_table ).

    DATA: ls_expected_line TYPE zosql_for_tst.
    ls_expected_line-mandt       = sy-mandt.
    ls_expected_line-key_field   = 'KEY1'.
    ls_expected_line-text_field1 = 'VALUE1_1'.
    ls_expected_line-text_field2 = 'VALUE1_2'.

    cl_aunit_assert=>assert_equals( act = ls_result_line exp = ls_expected_line ).
    cl_aunit_assert=>assert_equals( act = lv_subrc exp = 0 ).
  ENDMETHOD.

  METHOD one_table_where_static.

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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'WHERE KEY_FIELD = ''KEY2'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

    CONCATENATE 'select *'
      'FROM zosql_for_tst'
      'where key_field = :KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    ls_param-param_name_in_select   = ':KEY_FIELD'.
    ls_param-parameter_value_single = 'KEY2'.
    APPEND ls_param TO lt_params.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.



    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    DATA: lv_select           TYPE string,
          lt_params           TYPE zosql_db_layer_params,
          ls_param            TYPE zosql_db_layer_param,
          ls_param_range_line TYPE zosql_db_layer_range_line.

    ls_param-param_name_in_select   = ':TEXT_FIELD'.

    ls_param_range_line-sign = 'I'.
    ls_param_range_line-option = 'EQ'.
    ls_param_range_line-low    = 'VALUE1_1'.
    APPEND ls_param_range_line TO ls_param-parameter_value_range.

    ls_param_range_line-low    = 'VALUE3_1'.
    APPEND ls_param_range_line TO ls_param-parameter_value_range.
    APPEND ls_param TO lt_params.

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'WHERE TEXT_FIELD1 IN :TEXT_FIELD'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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

    ls_expected_line-key_field   = 'KEY3'.
    ls_expected_line-text_field1 = 'VALUE3_1'.
    ls_expected_line-text_field2 = 'VALUE3_2'.
    APPEND ls_expected_line TO lt_expected_table.

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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    DATA: lv_select           TYPE string,
          lt_params           TYPE zosql_db_layer_params,
          ls_param            TYPE zosql_db_layer_param,
          ls_param_range_line TYPE zosql_db_layer_range_line.

    ls_param-param_name_in_select   = ':TEXT_FIELD'.

    ls_param_range_line-sign = 'I'.
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

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                                        it_parameters   = lt_params
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF ty_result.

    APPEND 'KEY2' TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD one_table_where_param_ref.
    DATA: ls_line          TYPE zosql_for_tst,
          lt_initial_table TYPE TABLE OF zosql_for_tst,
          lv_some_value    TYPE text50.

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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

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

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

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

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    DATA: lt_base_table           TYPE TABLE OF zosql_for_tst,
          ls_line_for_all_entries TYPE zosql_for_tst,
          lv_select               TYPE string.

    ls_line_for_all_entries-key_field = 'KEY2'.
    APPEND ls_line_for_all_entries TO lt_base_table.

    ls_line_for_all_entries-key_field = 'KEY3'.
    APPEND ls_line_for_all_entries TO lt_base_table.

    CONCATENATE 'SELECT *'
      'FROM zosql_for_tst'
      'FOR ALL ENTRIES IN lt_base_table'
      'WHERE KEY_FIELD = LT_BASE_TABLE-KEY_FIELD'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                  = lv_select
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

  METHOD group_by.

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

    INSERT zosql_for_tst2 FROM TABLE lt_initial_table.

    DATA: lv_select   TYPE string.

    CONCATENATE 'SELECT key_field sum( amount ) as amount sum( qty ) as qty'
      'FROM zosql_for_tst2'
      'GROUP BY key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst2.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                  = lv_select
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

    INSERT zosql_for_tst2 FROM TABLE lt_initial_table.

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

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                  = lv_select
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

    INSERT zosql_for_tst2 FROM TABLE lt_initial_table.

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

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                = lv_select
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

    INSERT zosql_for_tst2 FROM TABLE lt_initial_table.

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

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected_line  TYPE ty_result,
          lt_expected_table TYPE TABLE OF ty_result.

    ls_expected_line-key_field = 'KEY1'.
    ls_expected_line-amount    = 30.
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst,
          lv_select       TYPE string.

    CONCATENATE 'SELECT DISTINCT TEXT_FIELD1'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY TEXT_FIELD1'
      INTO lv_select SEPARATED BY space.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                  = lv_select
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

    INSERT zosql_for_tst FROM TABLE lt_table1_initial.

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

    INSERT zosql_for_tst2 FROM TABLE lt_table2_initial.

    DATA: lv_select TYPE string.

    CONCATENATE 'SELECT t1~key_field t2~key_field2'
      'FROM zosql_for_tst as t1 JOIN zosql_for_tst2 as t2 ON t1~key_field = t2~key_field'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result_line,
             key_field  TYPE zosql_for_tst-key_field,
             key_field2 TYPE zosql_for_tst2-key_field2,
           END OF ty_result_line.

    DATA: lt_result_table TYPE TABLE OF ty_result_line.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                  = lv_select
                                              IMPORTING et_result_table            = lt_result_table ).
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

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

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

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

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_initial_table.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_initial_table.

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

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

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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

  METHOD select_from_view.
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    DELETE FROM zosql_for_tst2.

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

    INSERT zosql_for_tst2 FROM TABLE lt_initial_table2.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tsv.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM ZOSQL_FOR_TSV'
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

  METHOD select_as_ref_to_data.
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: ld_result       TYPE REF TO data,
          lt_result_table TYPE TABLE OF zosql_for_tst,
          lv_subrc        TYPE sysubrc.

    FIELD-SYMBOLS: <lt_result_table> TYPE STANDARD TABLE.

    f_cut->zif_zosql_db_layer~select( EXPORTING iv_select          = lv_select
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    CONCATENATE 'SELECT KEY_FIELD'
      'FROM ZOSQL_FOR_TST'
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    CONCATENATE 'SELECT KEY_FIELD TEXT_FIELD1'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: ls_result_line TYPE zosql_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select                = lv_select
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'UP TO 2 ROWS'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst,
          ls_result_line  TYPE zosql_for_tst,
          lv_subrc        TYPE sysubrc.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

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

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    CONCATENATE 'SELECT SINGLE *'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

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

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    " WHEN

    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst-key_field,
           END OF ty_result.

    DATA: lt_parameters   TYPE zosql_db_layer_params,
          ls_parameter    LIKE LINE OF lt_parameters,
          lt_result_table TYPE TABLE OF ty_result.

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'WHERE KEY_FIELD IN (''KEY1'',''KEY3'')'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    ls_parameter-param_name_in_select = ':KEY_FIELD_RANGE'.
    REFRESH ls_parameter-parameter_value_range.
    APPEND ls_parameter TO lt_parameters.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                                        it_parameters   = lt_parameters
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'ORDER BY PRIMARY KEY'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result_table TYPE SORTED TABLE OF zosql_for_tst
                               WITH UNIQUE KEY key_field,
          ls_result_line  TYPE zosql_for_tst,
          lv_subrc        TYPE sysubrc.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_initial_table ).
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    CONCATENATE 'SELECT COUNT(*) AS CNT'
      'FROM ZOSQL_FOR_TST'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             cnt TYPE i,
           END OF ty_result.

    DATA: ls_result_line  TYPE ty_result.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select      = lv_select
                                              IMPORTING es_result_line = ls_result_line ).

    " THEN
    cl_aunit_assert=>assert_equals( act = ls_result_line-cnt exp = 2 ).
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

    INSERT zosql_for_tst2 FROM TABLE lt_initial_table.

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

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select      = lv_select
                                              IMPORTING es_result_line = ls_result_line ).

    " THEN
    cl_aunit_assert=>assert_equals( act = ls_result_line-key_field exp = 'KEY2' ).
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

    INSERT zosql_for_tst2 FROM TABLE lt_initial_table.

    CONCATENATE 'SELECT key_field'
      'FROM ZOSQL_FOR_TST2'
      'WHERE AMOUNT >= ALL ( SELECT AMOUNT'
      '                        FROM ZOSQL_FOR_TST2 )'
      INTO lv_select SEPARATED BY space.

    " WHEN
    TYPES: BEGIN OF ty_result,
             key_field TYPE zosql_for_tst2-key_field,
           END OF ty_result.

    DATA: lt_Result_table TYPE TABLE OF ty_result.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: lt_expected_table TYPE TABLE OF ty_Result.

    APPEND 'KEY2' TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
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

    INSERT zosql_for_tst2 FROM TABLE lt_initial_table.

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST2'
      'WHERE amount between 20 AND 30'
      INTO lv_select SEPARATED BY space.

    " WHEN

    DATA: lt_result       TYPE TABLE OF zosql_for_tst2.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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

    INSERT zosql_for_tst2 FROM TABLE lt_initial_table.

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST2'
      'WHERE amount not between 20 AND 30'
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result       TYPE TABLE OF zosql_for_tst2.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

    CONCATENATE 'SELECT *'
      'FROM ZOSQL_FOR_TST'
      'WHERE TEXT_FIELD1 LIKE ''100#%%'' ESCAPE ''#'''
      INTO lv_select SEPARATED BY space.

    " WHEN
    DATA: lt_result       TYPE TABLE OF zosql_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

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

    INSERT zosql_for_tst2 FROM TABLE lt_initial_table2.

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

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                                        it_parameters   = lt_params
                                              IMPORTING et_result_table = lt_result ).

    "THEN

    DATA: lt_expected  TYPE TABLE OF ty_result.

    APPEND 'KEY2' TO lt_expected.
    APPEND 'KEY3' TO lt_expected.

    SORT: lt_result, lt_expected.

    cl_aunit_assert=>assert_equals( act = lt_result exp = lt_expected ).
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.
    INSERT zosql_for_tst2 FROM TABLE lt_initial_table2.

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

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.
    INSERT zosql_for_tst2 FROM TABLE lt_initial_table2.

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

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = lv_select
                                              IMPORTING et_result_table = lt_result_table ).

    " THEN
    DATA: ls_expected TYPE ty_result,
          lt_expected TYPE TABLE OF ty_result.

    ls_expected-key_field = 'KEY1'.
    APPEND ls_expected TO lt_expected.

    " THEN
    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected ).
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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

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

    INSERT zosql_for_tst FROM TABLE lt_initial_table.

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

    lv_cursor = f_cut->zif_zosql_db_layer~open_cursor( lv_select ).
    f_cut->zif_zosql_db_layer~fetch_next_cursor( EXPORTING iv_cursor          = lv_cursor
                                                           iv_package_size    = 2
                                                 IMPORTING ed_result_as_table = ld_result_table1 ).
    ASSIGN ld_result_table1->* TO <lt_result_table1>.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_result_table1>
                                               IMPORTING et_table_dest = lt_result_table1 ).

    f_cut->zif_zosql_db_layer~fetch_next_cursor( EXPORTING iv_cursor          = lv_cursor
                                                           iv_package_size    = 2
                                                 IMPORTING ed_result_as_table = ld_result_table2 ).
    ASSIGN ld_result_table2->* TO <lt_result_table2>.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_result_table2>
                                               IMPORTING et_table_dest = lt_result_table2 ).

    f_cut->zif_zosql_db_layer~fetch_next_cursor( EXPORTING iv_cursor          = lv_cursor
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
    f_cut->zif_zosql_db_layer~insert_by_itab( iv_table_name = 'ZOSQL_FOR_TST'
                                              it_new_lines  = lt_insert_table ).

    " THEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
                                              IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_insert_table ).
  ENDMETHOD.

  METHOD update_by_itab.

    DATA: ls_line                TYPE zosql_for_tst,
          lt_table_before_update TYPE TABLE OF zosql_for_tst.

    " GIVEN
    DELETE FROM zosql_for_tst.

    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_update.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_update.

    INSERT zosql_for_tst FROM TABLE lt_table_before_update.

    " WHEN
    DATA: lt_update_table TYPE TABLE OF zosql_for_tst.

    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE_1_1_UPD'.
    APPEND ls_line TO lt_update_table.

    ls_line-key_field = 'KEY2'.
    ls_line-text_field1 = 'VALUE_2_1_UPD'.
    APPEND ls_line TO lt_update_table.

    f_cut->zif_zosql_db_layer~update_by_itab( iv_table_name       = 'ZOSQL_FOR_TST'
                                              it_lines_for_update = lt_update_table ).

    " THEN
    DATA: lt_result_table TYPE TABLE OF zosql_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
                                              IMPORTING et_result_table = lt_result_table ).

    clear_mandant_field( CHANGING ct_internal_table = lt_result_table ).

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_update_table ).
  ENDMETHOD.

  METHOD modify_by_itab.
    DATA: ls_line                TYPE zosql_for_tst,
          lt_table_before_modify TYPE TABLE OF zosql_for_tst.

    " GIVEN
    ls_line-key_field   = 'KEY1'.
    ls_line-text_field1 = 'VALUE1_1'.
    APPEND ls_line TO lt_table_before_modify.

    ls_line-key_field   = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    APPEND ls_line TO lt_table_before_modify.

    INSERT zosql_for_tst FROM TABLE lt_table_before_modify.

    " WHEN
    DATA: lt_modify_table TYPE TABLE OF zosql_for_tst.

    ls_line-key_field = 'KEY1'.
    ls_line-text_field1 = 'VALUE_1_1_MOD'.
    APPEND ls_line TO lt_modify_table.

    ls_line-key_field = 'KEY3'.
    ls_line-text_field1 = 'VALUE_3_1_MOD'.
    APPEND ls_line TO lt_modify_table.

    f_cut->zif_zosql_db_layer~modify_by_itab( iv_table_name       = 'ZOSQL_FOR_TST'
                                              it_lines_for_modify = lt_modify_table ).

    " THEN
    DATA: lt_result_table   TYPE TABLE OF zosql_for_tst,
          lt_expected_table TYPE TABLE OF zosql_for_tst.

    f_cut->zif_zosql_db_layer~select_to_itab( EXPORTING iv_select       = 'SELECT * FROM zosql_for_tst'
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

    INSERT zosql_for_tst FROM TABLE lt_table_before_modify.

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

  METHOD update_by_sql_no_params.
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

    INSERT zosql_for_tst FROM TABLE lt_table_before_update.

    " WHEN
    CONCATENATE 'UPDATE ZOSQL_FOR_TST'
      'SET text_field2 = ''NEW_VAL_FIELD2'''
      'WHERE TEXT_FIELD1 = ''VALUE2_1'''
      INTO lv_update_statement SEPARATED BY space.

    f_cut->zif_zosql_db_layer~update( lv_update_statement ).

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

    CLEAR ls_line.
    ls_line-key_field = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'NEW_VAL_FIELD2'.
    APPEND ls_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD update_by_sql_with_params.
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

    INSERT zosql_for_tst FROM TABLE lt_table_before_update.

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

    f_cut->zif_zosql_db_layer~update( iv_update_statement = lv_update_statement
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

    CLEAR ls_line.
    ls_line-key_field = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'NEW_VAL_FIELD2'.
    APPEND ls_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD update_by_sql_params_set.
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

    INSERT zosql_for_tst FROM TABLE lt_table_before_update.

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

    f_cut->zif_zosql_db_layer~update( iv_update_statement = lv_update_statement
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

    CLEAR ls_line.
    ls_line-key_field = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'NEW_VAL_FIELD2'.
    APPEND ls_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD update_by_sql_new_syntax.
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

    INSERT zosql_for_tst FROM TABLE lt_table_before_update.

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

    f_cut->zif_zosql_db_layer~update( iv_update_statement = lv_update_statement
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

    CLEAR ls_line.
    ls_line-key_field = 'KEY2'.
    ls_line-text_field1 = 'VALUE2_1'.
    ls_line-text_field2 = 'NEW_VAL_FIELD2'.
    APPEND ls_line TO lt_expected_table.

    cl_aunit_assert=>assert_equals( act = lt_result_table exp = lt_expected_table ).
  ENDMETHOD.

  METHOD delete_by_sql_no_params.
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

    INSERT zosql_for_tst FROM TABLE lt_table_before_delete.

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

  METHOD delete_by_sql_with_params.
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

    INSERT zosql_for_tst FROM TABLE lt_table_before_delete.

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
ENDCLASS.
