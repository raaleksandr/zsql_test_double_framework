# Reference
## Interface ZIF_ZOSQL_DB_LAYER
It is the main interface providing database layer which built in fake database. It contains methods for selecting and updating database data with Open SQL syntax and also with the help of internal tables.
### How to create instance
Instance of the interface can be created in different ways.

1. For production (regular) mode directly with CREATE OBJECT

        DATA: db_layer TYPE REF TO zif_zosql_db_layer
        
        CREATE OBJECT db_layer TYPE zcl_zosql_db_layer.

2. For production (regular) mode with factory method

        DATA: db_layer TYPE REF TO zif_zosql_db_layer.
        
        db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).

3. For test mode directly with CREATE OBJECT

        DATA: test_environment  TYPE REF TO zif_zosql_test_environment,
              db_layer      TYPE REF TO zif_zosql_db_layer.
        
        test_environment = zcl_zosql_test_environment=>create( ).
        CREATE OBJECT db_layer TYPE zcl_zosql_db_layer_fake
          EXPORTING
            io_zosql_test_environment = test_environment.

4. For test mode with special creation method (factory)

        DATA: test_environment  TYPE REF TO zif_zosql_test_environment,
              db_layer      TYPE REF TO zif_zosql_db_layer.
        
        test_environment = zcl_zosql_test_environment=>create( ).
        db_layer = test_environment->get_db_layer_for_unit_tests( ).

