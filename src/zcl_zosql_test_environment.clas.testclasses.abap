*"* use this source file for your ABAP unit test classes

CLASS lcl_error_insert_dupl_rec DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS: in_one_call FOR TESTING,
             in_two_calls FOR TESTING RAISING zcx_zosql_error.
ENDCLASS.

CLASS lcl_error_insert_dupl_rec IMPLEMENTATION.
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
