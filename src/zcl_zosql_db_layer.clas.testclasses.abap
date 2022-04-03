INCLUDE zosql_test_cases_db_layer.

CLASS lcl_utils_for_unittests DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: clear_test_tables,
      get_f_cut RETURNING VALUE(ro_cut) TYPE REF TO zif_zosql_db_layer,
      insert_test_data IMPORTING it_table      TYPE ANY TABLE
                                 iv_table_name TYPE clike
                       RAISING   zcx_zosql_error.
ENDCLASS.

CLASS ltc_cases_for_select_real DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM ltc_cases_for_select.

  PUBLIC SECTION.
    METHODS: view_user_addr REDEFINITION,
      error_equal_and_range_in_par REDEFINITION.

  PROTECTED SECTION.
    METHODS: insert_test_data REDEFINITION.
  PRIVATE SECTION.
    DATA: mo_test_environment TYPE REF TO zif_zosql_test_environment.

    METHODS: setup.
ENDCLASS.

CLASS ltc_cases_for_select_740_real DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM ltc_cases_for_select_740.

  PROTECTED SECTION.
    METHODS: insert_test_data REDEFINITION.
  PRIVATE SECTION.
    DATA: mo_test_environment TYPE REF TO zif_zosql_test_environment.

    METHODS: setup.
ENDCLASS.

CLASS ltc_cases_for_insert_real DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM ltc_cases_for_insert.

  PROTECTED SECTION.
    METHODS: insert_test_data REDEFINITION.
  PRIVATE SECTION.
    DATA: mo_test_environment TYPE REF TO zif_zosql_test_environment.

    METHODS: setup.
ENDCLASS.

CLASS ltc_cases_for_update_real DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM ltc_cases_for_update.

  PROTECTED SECTION.
    METHODS: insert_test_data REDEFINITION.
  PRIVATE SECTION.
    DATA: mo_test_environment TYPE REF TO zif_zosql_test_environment.

    METHODS: setup.
ENDCLASS.

CLASS ltc_cases_for_update_740_real DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM ltc_cases_for_update_740.

  PROTECTED SECTION.
    METHODS: insert_test_data REDEFINITION.
  PRIVATE SECTION.
    DATA: mo_test_environment TYPE REF TO zif_zosql_test_environment.

    METHODS: setup.
ENDCLASS.

CLASS ltc_cases_for_modify_real DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM ltc_cases_for_modify.

  PROTECTED SECTION.
    METHODS: insert_test_data REDEFINITION.
  PRIVATE SECTION.
    DATA: mo_test_environment TYPE REF TO zif_zosql_test_environment.

    METHODS: setup.
ENDCLASS.

CLASS ltc_cases_for_delete_real DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM ltc_cases_for_delete.

  PROTECTED SECTION.
    METHODS: insert_test_data REDEFINITION.
  PRIVATE SECTION.
    DATA: mo_test_environment TYPE REF TO zif_zosql_test_environment.

    METHODS: setup.
ENDCLASS.

CLASS lcl_utils_for_unittests IMPLEMENTATION.
  METHOD clear_test_tables.
    DELETE FROM zosql_for_tst.
    DELETE FROM zosql_for_tst2.
    DELETE FROM zosql_for_tst3.
    DELETE FROM zosql_for_tst4.
  ENDMETHOD.

  METHOD get_f_cut.
    ro_cut = zcl_zosql_test_environment=>get_db_layer_for_production( ).
  ENDMETHOD.

  METHOD insert_test_data.
    DATA: lv_table_name  TYPE tabname.

    lv_table_name = iv_table_name.
    IF lv_table_name IS INITIAL.
      lv_table_name = zcl_zosql_utils=>try_to_guess_tabname_by_data( it_table ).
    ENDIF.

    IF lv_table_name IS INITIAL.
      MESSAGE e053 INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    lv_table_name = zcl_zosql_utils=>to_upper_case( lv_table_name ).

    IF lv_table_name <> 'ZOSQL_FOR_TST'
      AND lv_table_name <> 'ZOSQL_FOR_TST2'
      AND lv_table_name <> 'ZOSQL_FOR_TST3'
      AND lv_table_name <> 'ZOSQL_FOR_TST4'.

      MESSAGE e084 WITH lv_table_name INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    MODIFY (lv_table_name) FROM TABLE it_table.
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_select_real IMPLEMENTATION.
  METHOD setup.
    lcl_utils_for_unittests=>clear_test_tables( ).
    f_cut = lcl_utils_for_unittests=>get_f_cut( ).
  ENDMETHOD.

  METHOD insert_test_data.
    lcl_utils_for_unittests=>insert_test_data( it_table      = it_table
                                               iv_table_name = iv_table_name ).
  ENDMETHOD.

  METHOD view_user_addr.
    " Don't test for real database
  ENDMETHOD.

  METHOD error_equal_and_range_in_par.

    DATA: lx_error TYPE REF TO zcx_zosql_error.

    TRY.
        super->error_equal_and_range_in_par( ).
        cl_aunit_assert=>fail( 'Exception should be raised' ).
      CATCH zcx_zosql_error INTO lx_error.
        "success
      CATCH cx_root.
        cl_aunit_assert=>fail( 'Exception zcx_zosql_error should be raised' ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_select_740_real IMPLEMENTATION.
  METHOD setup.
    lcl_utils_for_unittests=>clear_test_tables( ).
    f_cut = lcl_utils_for_unittests=>get_f_cut( ).
  ENDMETHOD.

  METHOD insert_test_data.
    lcl_utils_for_unittests=>insert_test_data( it_table      = it_table
                                               iv_table_name = iv_table_name ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_insert_real IMPLEMENTATION.
  METHOD setup.
    lcl_utils_for_unittests=>clear_test_tables( ).
    f_cut = lcl_utils_for_unittests=>get_f_cut( ).
  ENDMETHOD.

  METHOD insert_test_data.
    lcl_utils_for_unittests=>insert_test_data( it_table      = it_table
                                               iv_table_name = iv_table_name ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_update_real IMPLEMENTATION.
  METHOD setup.
    lcl_utils_for_unittests=>clear_test_tables( ).
    f_cut = lcl_utils_for_unittests=>get_f_cut( ).
  ENDMETHOD.

  METHOD insert_test_data.
    lcl_utils_for_unittests=>insert_test_data( it_table      = it_table
                                               iv_table_name = iv_table_name ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_update_740_real IMPLEMENTATION.
  METHOD setup.
    lcl_utils_for_unittests=>clear_test_tables( ).
    f_cut = lcl_utils_for_unittests=>get_f_cut( ).
  ENDMETHOD.

  METHOD insert_test_data.
    lcl_utils_for_unittests=>insert_test_data( it_table      = it_table
                                               iv_table_name = iv_table_name ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_modify_real IMPLEMENTATION.
  METHOD setup.
    lcl_utils_for_unittests=>clear_test_tables( ).
    f_cut = lcl_utils_for_unittests=>get_f_cut( ).
  ENDMETHOD.

  METHOD insert_test_data.
    lcl_utils_for_unittests=>insert_test_data( it_table      = it_table
                                               iv_table_name = iv_table_name ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cases_for_delete_real IMPLEMENTATION.
  METHOD setup.
    lcl_utils_for_unittests=>clear_test_tables( ).
    f_cut = lcl_utils_for_unittests=>get_f_cut( ).
  ENDMETHOD.

  METHOD insert_test_data.
    lcl_utils_for_unittests=>insert_test_data( it_table      = it_table
                                               iv_table_name = iv_table_name ).
  ENDMETHOD.
ENDCLASS.
