
CLASS ltc_zosql_utils DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_zosql_Utils
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ZOSQL_UTILS
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
      f_cut TYPE REF TO zcl_zosql_utils.  "class under test

    METHODS: clear_quotes_from_value FOR TESTING,
             ends_with_positive FOR TESTING,
             ends_with_negative FOR TESTING,
             ends_with_different_case FOR TESTING,
             delete_end_token FOR TESTING,
             delete_end_token_no_delete FOR TESTING,
             is_char_string FOR TESTING,
             is_char_char_var FOR TESTING,
             is_char_int_negative FOR TESTING.
ENDCLASS.       "ltc_zosql_Utils

CLASS ltc_split_cond_into_tokens DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_Zosql_Utils
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ZOSQL_UTILS
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
      f_cut TYPE REF TO zcl_zosql_utils.  "class under test

    METHODS: base_case FOR TESTING,
             values_in_quotes_with_spaces FOR TESTING.
ENDCLASS.       "ltc_split_cond_into_tokens

CLASS ltc_zosql_utils IMPLEMENTATION.

  METHOD clear_quotes_from_value.
    DATA: lv_result TYPE string.

    lv_result = zcl_zosql_utils=>clear_quotes_from_value( '''SOME VALUE''' ).
    cl_aunit_assert=>assert_equals( act = lv_result exp = 'SOME VALUE' ).
  ENDMETHOD.

  METHOD ends_with_positive.
    DATA: lv_result TYPE abap_bool.

    lv_result = zcl_zosql_utils=>check_ends_with_token( iv_sql   = 'SOME STRING ENDS WITH AND'
                                                        iv_token = 'AND' ).

    cl_aunit_assert=>assert_equals( act = lv_result exp = abap_true ).
  ENDMETHOD.

  METHOD ends_with_negative.
    DATA: lv_result TYPE abap_bool.

    lv_result = zcl_zosql_utils=>check_ends_with_token( iv_sql   = 'SOME STRING AND OTHER STRING NOT ENDS'
                                                        iv_token = 'AND' ).

    cl_aunit_assert=>assert_equals( act = lv_result exp = abap_false ).
  ENDMETHOD.

  METHOD ends_with_different_case.
    DATA: lv_result TYPE abap_bool.

    lv_result = zcl_zosql_utils=>check_ends_with_token( iv_sql   = 'Some String With And'
                                                        iv_token = 'AND' ).

    cl_aunit_assert=>assert_equals( act = lv_result exp = abap_true ).
  ENDMETHOD.

  METHOD delete_end_token.
    DATA: lv_result TYPE string.

    lv_result = zcl_zosql_utils=>delete_end_token_if_equals( iv_sql_source          = 'SOME STRING WITH AND'
                                                             iv_end_token_to_delete = 'AND' ).

    cl_aunit_assert=>assert_equals( act = lv_result exp = 'SOME STRING WITH' ).
  ENDMETHOD.

  METHOD delete_end_token_no_delete.
    DATA: lv_result TYPE string.

    lv_result = zcl_zosql_utils=>delete_end_token_if_equals(
      iv_sql_source          = 'SOME STRING AND OTHER STRING NOT ENDS'
      iv_end_token_to_delete = 'AND' ).

    cl_aunit_assert=>assert_equals( act = lv_result exp = 'SOME STRING AND OTHER STRING NOT ENDS' ).
  ENDMETHOD.

  METHOD is_char_string.
    DATA: lv_string TYPE string,
          lv_result TYPE abap_bool.

    lv_string = '123'.
    lv_result = zcl_zosql_utils=>is_char( lv_string ).

    cl_aunit_assert=>assert_equals( act = lv_result exp = abap_true ).
  ENDMETHOD.

  METHOD is_char_char_var.
    DATA: lv_char   TYPE char50,
          lv_result TYPE abap_bool.

    lv_char = '456'.
    lv_result = zcl_zosql_utils=>is_char( lv_char ).

    cl_aunit_assert=>assert_equals( act = lv_result exp = abap_true ).
  ENDMETHOD.

  METHOD is_char_int_negative.
    DATA: lv_int    TYPE i,
          lv_result TYPE abap_bool.

    lv_int = 4.
    lv_result = zcl_zosql_utils=>is_char( lv_int ).

    cl_aunit_assert=>assert_equals( act = lv_result exp = abap_false ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_split_cond_into_tokens IMPLEMENTATION.
  METHOD base_case.

    DATA: lt_result_tokens TYPE TABLE OF string.

    lt_result_tokens = zcl_zosql_utils=>split_condition_into_tokens( 'FIELD1 = ''value1''' ).

    DATA: lt_expected_tokens TYPE TABLE OF string.

    APPEND 'FIELD1' TO lt_expected_tokens.
    APPEND '=' TO lt_expected_tokens.
    APPEND '''value1''' TO lt_expected_tokens.

    cl_aunit_assert=>assert_equals( act = lt_result_tokens exp = lt_expected_tokens ).
  ENDMETHOD.

  METHOD values_in_quotes_with_spaces.
    DATA: lt_result_tokens TYPE TABLE OF string.

    lt_result_tokens = zcl_zosql_utils=>split_condition_into_tokens( 'FIELD1 = ''text with space''' ).

    DATA: lt_expected_tokens TYPE TABLE OF string.

    APPEND 'FIELD1' TO lt_expected_tokens.
    APPEND '=' TO lt_expected_tokens.
    APPEND '''text with space''' TO lt_expected_tokens.

    cl_aunit_assert=>assert_equals( act = lt_result_tokens exp = lt_expected_tokens ).
  ENDMETHOD.
ENDCLASS.
