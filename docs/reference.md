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

### Method SELECT
Lets to execute SQL select statement and get result.
The result is returned in REF TO DATA variable containing dynamically created internal table with result data set.
Parameters:
1. IV_SELECT – Open SQL statement as string

   You can get more information in detailed description of IV_SELECT parameter (TODO).
2. IT_PARAMETERS – lets to pass bind variables to SQL statement

   You can get more information in detailed description of parameter IT_PARAMETERS (TODO).
3. IT_FOR_ALL_ENTRIES_TABLE – lets to pass internal table as base table of FOR ALL ENTIRES SQL statement

   You can get more information in detailed description of parameter IT_FOR_ALL_ENTRIES_TABLE (TODO).
4. ED_RESULT_AS_TABLE – ref to internal table containing result

   If no rows returned then ref to empty internal table is returned.
5. EV_SUBRC – result of select: 0 if at least one row was selected and 4 if no rows selected

Example of simple select

    db->select ( EXPORTING iv_select          = ‘SELECT * FROM SFLIGHT’
                 IMPORTING ed_result_as_table = ld_result_table 
                           ev_subrc           = lv_subrc ).

Example of select with where

    db->select ( EXPORTING iv_select          = ‘SELECT * FROM SCARR WHERE carrid = ‘’AA’’’
                 IMPORTING ed_result_as_table = ld_result_table 
                           ev_subrc           = lv_subrc ).

Example of select with parameters
    ls_param-param_name_in_select = ':CARRID'.
    ls_param-parameter_value_single = 'AA'.
    APPEND ls_param TO lt_params.

    db->select ( EXPORTING iv_select          = ‘SELECT * FROM SCARR WHERE carrid = :CARRID’
                           it_parameters      = lt_params
                 IMPORTING ed_result_as_table = ld_result_table 
                           ev_subrc           = lv_subrc ).

Example of select with FOR ALL ENTRIES
    CONCATENATE 'SELECT *'
      'FROM scarr’
      'FOR ALL ENTRIES IN lt_scarr_base'
      'WHERE KEY_FIELD = lt_scarr_base-carrid'
      INTO lv_select SEPARATED BY space.

    ls_scarr-carrid = ‘AA’.
    APPEND ls_scarr TO lt_scarr_base.

    ls_scarr-carrid = ‘AB’.
    APPEND ls_scarr TO lt_scarr_base.

    db->select ( EXPORTING iv_select                = lv_select
                           it_for_all_entries_table = lt_scarr_base
                 IMPORTING ed_result_as_table       = ld_result_table 
                           ev_subrc                 = lv_subrc ).

Example of select with join

    CONCATENATE
      'SELECT sbook~customid AS customid'
           'MIN( scustom~name ) AS customer_name'
           'COUNT( * ) AS count_tickets_bought'
           'MIN( sflight~fldate ) AS date_of_first_flight'
           'MAX( sbook~smoker ) AS is_smoker'
      'FROM sbook'
      'JOIN scustom ON scustom~id = sbook~customid'
      'JOIN sflight ON sflight~carrid = sbook~carrid'
                  'AND sflight~connid = sbook~connid'
                  'AND sflight~fldate = sbook~fldate'
      'WHERE sbook~customid = ‘‘1000’’’
      'GROUP BY customid'
      'ORDER BY customid'
      INTO lv_select SEPARATED BY space.

    db->select ( EXPORTING iv_select          = lv_select
                 IMPORTING ed_result_as_table = ld_result_table 
                           ev_subrc           = lv_subrc ).

### Method SELECT_TO_ITAB
Lets to execute SQL select statement and get result.
The method is almost the same as SELECT but the result is returned into generic table with type ANY TABLE.

Parameters:
1. IV_SELECT – Open SQL statement as string

   You can get more information in detailed description of IV_SELECT parameter (TODO).
2. IT_PARAMETERS – lets to pass bind variables to SQL statement

   You can get more information in detailed description of parameter IT_PARAMETERS (TODO).
3. IT_FOR_ALL_ENTRIES_TABLE – lets to pass internal table as base table of FOR ALL ENTIRES SQL statement

   You can get more information in detailed description of parameter IT_FOR_ALL_ENTRIES_TABLE (TODO).
4. IV_DO_INTO_CORRESPONDING – if true then result is returned as by SELECT … INTO CORRESPONDING FIELDS OF TABLE. If false then result is returned as by SELECT … INTO TABLE
5. ET_RESULT_TABLE – target table to return result data. Any internal table can be set to this parameter. The result will be moved as move-corresponding or in field order depending on IV_DO_INTO_CORRESPONDING parameter
6. ES_RESULT_LINE – returns first line of result dataset
7. EV_SUBRC – result of select: 0 if at least one row was selected and 4 if no rows selected
