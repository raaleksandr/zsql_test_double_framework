
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

    METHODS: ends_with_positive FOR TESTING,
             ends_with_negative FOR TESTING,
             ends_with_different_case FOR TESTING,
             delete_end_token FOR TESTING,
             delete_end_token_no_delete FOR TESTING,
             is_char_string FOR TESTING,
             is_char_char_var FOR TESTING,
             is_char_int_negative FOR TESTING,
             raise_exception_with_text FOR TESTING,
             is_same_var_positive FOR TESTING,
             is_same_var_negative FOR TESTING.
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
             values_in_quotes_with_spaces FOR TESTING,
             split_by_comma FOR TESTING,
             split_by_space_and_comma FOR TESTING,
             split_when_comma_at_end FOR TESTING,
             split_when_list_of_vals FOR TESTING,
             split_by_line_breaks FOR TESTING,
             split_by_tab FOR TESTING.
ENDCLASS.       "ltc_split_cond_into_tokens

CLASS ltc_char_can_convert_to_number DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS: integer_value FOR TESTING,
             float_value FOR TESTING,
             incorrect_value FOR TESTING.

  PRIVATE SECTION.
    METHODS: _run_the_test IMPORTING iv_char            TYPE clike
                                     iv_expected_result TYPE abap_bool.
ENDCLASS.

CLASS ltc_is_char_in_quotes DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS: in_quotes FOR TESTING,
             just_quotes FOR TESTING,
             not_in_quotes FOR TESTING,
             only_one_quote_at_start FOR TESTING.

  PRIVATE SECTION.
    METHODS: _run_the_test IMPORTING iv_char            TYPE clike
                                     iv_expected_result TYPE abap_bool.
ENDCLASS.

CLASS ltc_clear_quotes_from_value DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS: base_case FOR TESTING,
             empty_string_in_quotes FOR TESTING,
             one_quote_missing FOR TESTING,
             quotes_inside FOR TESTING,
             just_text_without_quotes FOR TESTING.

  PRIVATE SECTION.
    METHODS: _run_the_test IMPORTING iv_value_before    TYPE clike
                                     iv_expected_result TYPE clike.
ENDCLASS.

CLASS ltc_zosql_utils IMPLEMENTATION.

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

  METHOD raise_exception_with_text.

    CONSTANTS: lc_error_text TYPE string VALUE 'Some text'.

    DATA: lx_exception TYPE REF TO zcx_zosql_error,
          lv_text_from_exception TYPE string.

    TRY.
      zcl_zosql_utils=>raise_exception_with_text( lc_error_text ).
      cl_aunit_assert=>fail( 'Exception should be raised' ).
    CATCH zcx_zosql_error INTO lx_exception.
      lv_text_from_exception = lx_exception->get_text( ).
      cl_aunit_assert=>assert_equals( act = lv_text_from_exception exp = lc_error_text ).
    ENDTRY.
  ENDMETHOD.

  METHOD is_same_var_positive.
    DATA: lv_var    TYPE i,
          lv_result TYPE abap_bool.

    lv_result = zcl_zosql_utils=>is_same_variable( iv_var1 = lv_var
                                                   iv_var2 = lv_var ).

    cl_aunit_assert=>assert_equals( act = lv_result exp = abap_true ).
  ENDMETHOD.

  METHOD is_same_var_negative.
    DATA: lv_var       TYPE i,
          lv_other_var TYPE i,
          lv_result    TYPE abap_bool.

    lv_result = zcl_zosql_utils=>is_same_variable( iv_var1 = lv_var
                                                   iv_var2 = lv_other_var ).

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

  METHOD split_by_comma.
    DATA: lt_result_tokens TYPE TABLE OF string.

    lt_result_tokens = zcl_zosql_utils=>split_condition_into_tokens( 'field1,field2' ).

    DATA: lt_expected_tokens TYPE TABLE OF string.

    APPEND 'field1' TO lt_expected_tokens.
    APPEND ','      TO lt_expected_tokens.
    APPEND 'field2' TO lt_expected_tokens.

    cl_aunit_assert=>assert_equals( act = lt_result_tokens exp = lt_expected_tokens ).
  ENDMETHOD.

  METHOD split_by_space_and_comma.
    DATA: lt_result_tokens TYPE TABLE OF string.

    lt_result_tokens = zcl_zosql_utils=>split_condition_into_tokens( 'field1, field2' ).

    DATA: lt_expected_tokens TYPE TABLE OF string.

    APPEND 'field1' TO lt_expected_tokens.
    APPEND ','      TO lt_expected_tokens.
    APPEND 'field2' TO lt_expected_tokens.

    cl_aunit_assert=>assert_equals( act = lt_result_tokens exp = lt_expected_tokens ).
  ENDMETHOD.

  METHOD split_when_comma_at_end.
    DATA: lt_result_tokens TYPE TABLE OF string.

    lt_result_tokens = zcl_zosql_utils=>split_condition_into_tokens( 'field1,' ).

    DATA: lt_expected_tokens TYPE TABLE OF string.

    APPEND 'field1' TO lt_expected_tokens.
    APPEND ','      TO lt_expected_tokens.

    cl_aunit_assert=>assert_equals( act = lt_result_tokens exp = lt_expected_tokens ).
  ENDMETHOD.

  METHOD split_when_list_of_vals.
    DATA: lt_result_tokens TYPE TABLE OF string.

    lt_result_tokens = zcl_zosql_utils=>split_condition_into_tokens( '(''VALUE1'',''VALUE2'')' ).

    DATA: lt_expected_tokens TYPE TABLE OF string.

    APPEND '('          TO lt_expected_tokens.
    APPEND '''VALUE1''' TO lt_expected_tokens.
    APPEND ','          TO lt_expected_tokens.
    APPEND '''VALUE2''' TO lt_expected_tokens.
    APPEND ')'          TO lt_expected_tokens.

    cl_aunit_assert=>assert_equals( act = lt_result_tokens exp = lt_expected_tokens ).
  ENDMETHOD.

  METHOD split_by_line_breaks.
    DATA: lt_result_tokens TYPE TABLE OF string,
          lv_text          TYPE string.

    CONCATENATE 'FIELD1 =' '''value1''' INTO lv_text SEPARATED BY cl_abap_char_utilities=>cr_lf.

    lt_result_tokens = zcl_zosql_utils=>split_condition_into_tokens( lv_text ).

    DATA: lt_expected_tokens TYPE TABLE OF string.

    APPEND 'FIELD1' TO lt_expected_tokens.
    APPEND '=' TO lt_expected_tokens.
    APPEND '''value1''' TO lt_expected_tokens.

    cl_aunit_assert=>assert_equals( act = lt_result_tokens exp = lt_expected_tokens ).
  ENDMETHOD.

  METHOD split_by_tab.
    DATA: lt_result_tokens TYPE TABLE OF string,
          lv_text          TYPE string.

    CONCATENATE 'FIELD1 =' '''value1''' INTO lv_text SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

    lt_result_tokens = zcl_zosql_utils=>split_condition_into_tokens( lv_text ).

    DATA: lt_expected_tokens TYPE TABLE OF string.

    APPEND 'FIELD1' TO lt_expected_tokens.
    APPEND '=' TO lt_expected_tokens.
    APPEND '''value1''' TO lt_expected_tokens.

    cl_aunit_assert=>assert_equals( act = lt_result_tokens exp = lt_expected_tokens ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_char_can_convert_to_number IMPLEMENTATION.
  METHOD integer_value.
    _run_the_test( iv_char = '25' iv_expected_result = abap_true ).
  ENDMETHOD.

  METHOD float_value.
    _run_the_test( iv_char = '25.5' iv_expected_result = abap_true ).
  ENDMETHOD.

  METHOD incorrect_value.
    _run_the_test( iv_char = '25incorrect' iv_expected_result = abap_false ).
  ENDMETHOD.

  METHOD _run_the_test.
    DATA: lv_result TYPE abap_bool.

    lv_result = zcl_zosql_utils=>char_can_convert_to_number( iv_char ).
    cl_aunit_assert=>assert_equals( act = lv_result exp = iv_expected_result ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_is_char_in_quotes IMPLEMENTATION.
  METHOD in_quotes.
    _run_the_test( iv_char = '''Some text in quotes''' iv_expected_result = abap_true ).
  ENDMETHOD.

  METHOD just_quotes.
    _run_the_test( iv_char = '''''' iv_expected_result = abap_true ).
  ENDMETHOD.

  METHOD not_in_quotes.
    _run_the_test( iv_char = 'Some text not in quotes' iv_expected_result = abap_false ).
  ENDMETHOD.

  METHOD only_one_quote_at_start.
    _run_the_test( iv_char = '''Some text with only one quote at start' iv_expected_result = abap_false ).
  ENDMETHOD.

  METHOD _run_the_test.
    DATA: lv_result TYPE abap_bool.

    lv_result = zcl_zosql_utils=>is_char_in_quotes( iv_char ).
    cl_aunit_assert=>assert_equals( act = lv_result exp = iv_expected_result ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_clear_quotes_from_value IMPLEMENTATION.

  METHOD base_case.
    _run_the_test( iv_value_before    = '''SOME TEXT'''
                   iv_expected_result = 'SOME TEXT' ).
  ENDMETHOD.

  METHOD empty_string_in_quotes.
    _run_the_test( iv_value_before    = ''''''
                   iv_expected_result = '' ).
  ENDMETHOD.

  METHOD one_quote_missing.
    _run_the_test( iv_value_before    = '''SOME TEXT'
                   iv_expected_result = '''SOME TEXT' ).
  ENDMETHOD.

  METHOD quotes_inside.
    _run_the_test( iv_value_before    = '''Quote ''Inside'' text'''
                   iv_expected_result = 'Quote ''Inside'' text' ).
  ENDMETHOD.

  METHOD just_text_without_quotes.
    _run_the_test( iv_value_before    = 'SOME TEXT'
                   iv_expected_result = 'SOME TEXT' ).
  ENDMETHOD.

  METHOD _run_the_test.
    DATA: lv_result TYPE string.

    lv_result = zcl_zosql_utils=>clear_quotes_from_value( iv_value_before ).
    cl_aunit_assert=>assert_equals( act = lv_result exp = iv_expected_result ).
  ENDMETHOD.

ENDCLASS.
