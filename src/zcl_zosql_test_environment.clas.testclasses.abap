*"* use this source file for your ABAP unit test classes

CLASS ltc_tests DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS: delete_test_data FOR TESTING RAISING zcx_zosql_error,
             delete_test_data_no_tabname FOR TESTING RAISING zcx_zosql_error.
ENDCLASS.

CLASS ltc_error_insert_dupl_rec DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS: in_one_call FOR TESTING,
             in_two_calls FOR TESTING RAISING zcx_zosql_error.
ENDCLASS.

CLASS ltc_tests IMPLEMENTATION.

  METHOD delete_test_data.

    DATA: lo_env  TYPE REF TO zif_zosql_test_environment,
          lt_data TYPE TABLE OF zosql_for_tst,
          ls_data TYPE zosql_for_tst,
          lo_db_layer TYPE REF TO zif_zosql_db_layer.

    lo_env ?= zcl_zosql_test_environment=>create( ).

    " GIVEN
    ls_data-key_field = 'KEY1'.
    lo_env->insert_test_data_line( ls_data ).

    " WHEN
    ls_data-key_field = 'KEY1'.
    APPEND ls_data TO lt_data.
    lo_env->delete_test_data_from_itab( iv_table_name       = 'ZOSQL_FOR_TST'
                                        it_lines_for_delete = lt_data ).

    " THEN
    DATA: lv_subrc TYPE sysubrc.

    lo_db_layer = lo_env->get_db_layer_for_unit_tests( ).
    lo_db_layer->select( EXPORTING iv_select = 'SELECT * FROM zosql_for_tst'
                         IMPORTING ev_subrc  = lv_subrc ).

    cl_aunit_assert=>assert_equals( act = lv_subrc exp = 4 ).
  ENDMETHOD.

  METHOD delete_test_data_no_tabname.

    DATA: lo_env  TYPE REF TO zif_zosql_test_environment,
          lt_data TYPE TABLE OF zosql_for_tst,
          ls_data TYPE zosql_for_tst,
          lo_db_layer TYPE REF TO zif_zosql_db_layer.

    lo_env ?= zcl_zosql_test_environment=>create( ).

    " GIVEN
    ls_data-key_field = 'KEY1'.
    lo_env->insert_test_data_line( ls_data ).

    " WHEN
    ls_data-key_field = 'KEY1'.
    APPEND ls_data TO lt_data.
    lo_env->delete_test_data_from_itab( lt_data ).

    " THEN
    DATA: lv_subrc TYPE sysubrc.

    lo_db_layer = lo_env->get_db_layer_for_unit_tests( ).
    lo_db_layer->select( EXPORTING iv_select = 'SELECT * FROM zosql_for_tst'
                         IMPORTING ev_subrc  = lv_subrc ).

    cl_aunit_assert=>assert_equals( act = lv_subrc exp = 4 ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_error_insert_dupl_rec IMPLEMENTATION.
  METHOD in_one_call.
    DATA: lo_env                     TYPE REF TO zif_zosql_test_environment,
          lt_data_with_duplicate_key TYPE TABLE OF zosql_for_tst,
          ls_data                    TYPE zosql_for_tst.

    lo_env ?= zcl_zosql_test_environment=>create( ).

    ls_data-key_field   = 'DUPLICATE_KEY'.
    ls_data-text_field1 = 'TEXT1'.
    APPEND ls_data TO lt_data_with_duplicate_key.

    ls_data-key_field   = 'DUPLICATE_KEY'.
    ls_data-text_field1 = 'TEXT2'.
    APPEND ls_data TO lt_data_with_duplicate_key.

    TRY.
        lo_env->insert_test_data( lt_data_with_duplicate_key ).
        cl_aunit_assert=>fail( 'The should be exception about duplicate key' ).
      CATCH zcx_zosql_error.
    ENDTRY.
  ENDMETHOD.

  METHOD in_two_calls.
    DATA: lo_env                     TYPE REF TO zif_zosql_test_environment,
          lt_data_with_duplicate_key TYPE TABLE OF zosql_for_tst,
          ls_data                    TYPE zosql_for_tst.

    lo_env ?= zcl_zosql_test_environment=>create( ).

    ls_data-key_field   = 'DUPLICATE_KEY'.
    ls_data-text_field1 = 'TEXT1'.
    APPEND ls_data TO lt_data_with_duplicate_key.

    lo_env->insert_test_data( lt_data_with_duplicate_key ).

    TRY.
        lo_env->insert_test_data( lt_data_with_duplicate_key ).
        cl_aunit_assert=>fail( 'The should be exception about duplicate key' ).
      CATCH zcx_zosql_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
