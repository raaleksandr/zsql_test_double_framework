INCLUDE zosql_test_cases_db_layer.

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

CLASS ltc_cases_for_delete_fake DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM ltc_cases_for_delete.
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
  PROTECTED SECTION.
    METHODS: insert_test_data REDEFINITION.
  PRIVATE SECTION.
    DATA: mo_test_environment TYPE REF TO zif_zosql_test_environment.

    METHODS: setup.
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

CLASS ltc_cases_for_delete_fake IMPLEMENTATION.
  METHOD setup.
    mo_test_environment = zcl_zosql_test_environment=>create( ).
    f_cut ?= mo_test_environment->get_db_layer_for_unit_tests( ).
  ENDMETHOD.
  METHOD insert_test_data.
    mo_test_environment->insert_test_data( it_table      = it_table
                                           iv_table_name = iv_table_name ).
  ENDMETHOD.
ENDCLASS.
