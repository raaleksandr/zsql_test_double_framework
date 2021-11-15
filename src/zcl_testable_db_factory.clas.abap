class ZCL_TESTABLE_DB_FACTORY definition
  public
  create public .

public section.

  interfaces ZIF_TESTABLE_DB_FACTORY .

  methods CONSTRUCTOR
    importing
      !IO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT optional .
  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZIF_TESTABLE_DB_FACTORY .
  class-methods GET_INSTANCE_FOR_FAKES
    importing
      !IO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT optional
    returning
      value(RO_INSTANCE) type ref to ZIF_TESTABLE_DB_FACTORY .
protected section.
private section.

  data MO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT .
ENDCLASS.



CLASS ZCL_TESTABLE_DB_FACTORY IMPLEMENTATION.


  method CONSTRUCTOR.
    IF io_zosql_test_environment IS BOUND.
      mo_zosql_test_environment = io_zosql_test_environment.
    ENDIF.
  endmethod.


  method GET_INSTANCE.
    CREATE OBJECT ro_instance TYPE zcl_testable_db_factory.
  endmethod.


  method GET_INSTANCE_FOR_FAKES.
    CREATE OBJECT ro_instance TYPE zcl_testable_db_factory
      EXPORTING
        io_zosql_test_environment = io_zosql_test_environment.
    ro_instance->test_mode_on( ).
  endmethod.


  method ZIF_TESTABLE_DB_FACTORY~GET_DATABASE_LAYER.
    IF zif_testable_db_factory~test_mode = abap_true.
      mo_zosql_test_environment = zif_testable_db_factory~get_test_environment( ).
      CREATE OBJECT ro_database_layer TYPE zcl_zosql_db_layer_fake
        EXPORTING
          io_zosql_test_environment = mo_zosql_test_environment.
    ELSE.
      CREATE OBJECT ro_database_layer TYPE zcl_zosql_db_layer.
    ENDIF.
  endmethod.


  method ZIF_TESTABLE_DB_FACTORY~GET_ONE_VIRTUAL_TABLE.
    CREATE OBJECT ro_virtual_db_one_table
      EXPORTING
        iv_table_name = iv_table_name.
  endmethod.


  method ZIF_TESTABLE_DB_FACTORY~GET_TEST_ENVIRONMENT.
    IF mo_zosql_test_environment IS NOT BOUND.
      mo_zosql_test_environment = zcl_zosql_test_environment=>create( ).
    ENDIF.

    ro_zosql_test_environment = mo_zosql_test_environment.
  endmethod.


  method ZIF_TESTABLE_DB_FACTORY~TEST_MODE_OFF.
    zif_testable_db_factory~test_mode = abap_false.
  endmethod.


  method ZIF_TESTABLE_DB_FACTORY~TEST_MODE_ON.
    zif_testable_db_factory~test_mode = abap_true.
  endmethod.
ENDCLASS.
