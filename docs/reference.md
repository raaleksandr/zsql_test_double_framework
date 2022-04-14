[Back to readme](https://github.com/raaleksandr/zsql_test_double_framework#readme)

# Reference

[Interface ZIF_ZOSQL_DB_LAYER](#Interface-ZIF_ZOSQL_DB_LAYER)
* [How to create instance](#How-to-create-instance)
* [Method SELECT](#Method-SELECT)
* [Method SELECT_TO_ITAB](#Method-SELECT_TO_ITAB)
* [Method OPEN_CURSOR](#Method-OPEN_CURSOR)
* [Method FETCH_NEXT_CURSOR](#Method-FETCH_NEXT_CURSOR)
* [Method FETCH_NEXT_CURSOR_TO_ITAB](#Method-FETCH_NEXT_CURSOR_TO_ITAB)
* [Method INSERT_BY_ITAB](#Method-INSERT_BY_ITAB)
* [Method UPDATE_BY_ITAB](#Method-UPDATE_BY_ITAB)
* [Method MODIFY_BY_ITAB](#Method-MODIFY_BY_ITAB)
* [Method DELETE_BY_ITAB](#Method-DELETE_BY_ITAB)
* [Method UPDATE](#Method-UPDATE)
* [Method DELETE](#Method-DELETE)
* [Method COMMIT](#Method-COMMIT)
* [Detailed description of parameter IV_SELECT](#Detailed-description-of-parameter-IV_SELECT)
* [Detailed description of parameter IT_PARAMETERS](#Detailed-description-of-parameter-IT_PARAMETERS)
* [Detailed description of parameter IT_FOR_ALL_ENTRIES_TABLE](#Detailed-description-of-parameter-IT_FOR_ALL_ENTRIES_TABLE)

[Interface ZIF_ZOSQL_TEST_ENVIRONMENT](Interface-ZIF_ZOSQL_TEST_ENVIRONMENT)
* [How to create instance](#How-to-create-instance)
* [Method CLEAR_ALL](#Method-CLEAR_ALL)
* [Method CLEAR_DOUBLES](#Method-CLEAR_DOUBLES)
* [Method CLEAR_ONE_TABLE](#Method-CLEAR_ONE_TABLE)
* [Method DELETE_TEST_DATA_FROM_ITAB](#Method-DELETE_TEST_DATA_FROM_ITAB)
* [Method GET_DATA_OF_TABLE](#Method-GET_DATA_OF_TABLE)
* [Method GET_DATA_OF_TABLE_AS_REF](#Method-GET_DATA_OF_TABLE_AS_REF)
* [Method INSERT_TEST_DATA](#Method-INSERT_TEST_DATA)

## Interface ZIF_ZOSQL_DB_LAYER
It is the main interface providing database layer which built in fake database. It contains methods for selecting and updating database data with Open SQL syntax and also with the help of internal tables.
### How to create instance
Instance of the interface can be created in different ways.

1. For production (regular) mode directly with CREATE OBJECT

        DATA: db_layer TYPE REF TO zif_zosql_db_layer.
        
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

   You can get more information in [detailed description of IV_SELECT parameter](#Detailed-description-of-parameter-IV_SELECT).
2. IT_PARAMETERS – lets to pass bind variables to SQL statement

   You can get more information in [detailed description of parameter IT_PARAMETERS](#Detailed-description-of-parameter-IT_PARAMETERS).
3. IT_FOR_ALL_ENTRIES_TABLE – lets to pass internal table as base table of FOR ALL ENTIRES SQL statement

   You can get more information in [detailed description of parameter IT_FOR_ALL_ENTRIES_TABLE](#Detailed-description-of-parameter-IT_FOR_ALL_ENTRIES_TABLE).
4. ED_RESULT_AS_TABLE – ref to internal table containing result

   If no rows returned then ref to empty internal table is returned.
5. EV_SUBRC – result of select: 0 if at least one row was selected and 4 if no rows selected

Example of simple select

    db_layer->select( EXPORTING iv_select          = 'SELECT * FROM SFLIGHT'
                      IMPORTING ed_result_as_table = ld_result_table 
                                ev_subrc           = lv_subrc ).

Example of select with where

    db_layer->select( EXPORTING iv_select          = 'SELECT * FROM SCARR WHERE carrid = ''AA'''
                      IMPORTING ed_result_as_table = ld_result_table 
                                ev_subrc           = lv_subrc ).

Example of select with parameters
    ls_param-param_name_in_select = ':CARRID'.
    ls_param-parameter_value_single = 'AA'.
    APPEND ls_param TO lt_params.

    db_layer->select( EXPORTING iv_select          = 'SELECT * FROM SCARR WHERE carrid = :CARRID'
                                it_parameters      = lt_params
                      IMPORTING ed_result_as_table = ld_result_table 
                                ev_subrc           = lv_subrc ).

Example of select with FOR ALL ENTRIES
    CONCATENATE 'SELECT *'
      'FROM scarr'
      'FOR ALL ENTRIES IN lt_scarr_base'
      'WHERE KEY_FIELD = lt_scarr_base-carrid'
      INTO lv_select SEPARATED BY space.

    ls_scarr-carrid = 'AA'.
    APPEND ls_scarr TO lt_scarr_base.

    ls_scarr-carrid = 'AB'.
    APPEND ls_scarr TO lt_scarr_base.

    db_layer->select( EXPORTING iv_select                = lv_select
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
      'WHERE sbook~customid = ''1000'''
      'GROUP BY customid'
      'ORDER BY customid'
      INTO lv_select SEPARATED BY space.

    db_layer->select( EXPORTING iv_select          = lv_select
                      IMPORTING ed_result_as_table = ld_result_table 
                                ev_subrc           = lv_subrc ).

### Method SELECT_TO_ITAB
Lets to execute SQL select statement and get result.
The method is almost the same as SELECT but the result is returned into generic table with type ANY TABLE.

Parameters:
1. IV_SELECT – Open SQL statement as string

   You can get more information in [detailed description of IV_SELECT parameter](#Detailed-description-of-parameter-IV_SELECT).
2. IT_PARAMETERS – lets to pass bind variables to SQL statement

   You can get more information in [detailed description of parameter IT_PARAMETERS](#Detailed-description-of-parameter-IT_PARAMETERS).
3. IT_FOR_ALL_ENTRIES_TABLE – lets to pass internal table as base table of FOR ALL ENTIRES SQL statement

   You can get more information in [detailed description of parameter IT_FOR_ALL_ENTRIES_TABLE](#Detailed-description-of-parameter-IT_FOR_ALL_ENTRIES_TABLE).
4. IV_DO_INTO_CORRESPONDING – if true then result is returned as by SELECT … INTO CORRESPONDING FIELDS OF TABLE. If false then result is returned as by SELECT … INTO TABLE
5. ET_RESULT_TABLE – target table to return result data. Any internal table can be set to this parameter. The result will be moved as move-corresponding or in field order depending on IV_DO_INTO_CORRESPONDING parameter
6. ES_RESULT_LINE – returns first line of result dataset
7. EV_SUBRC – result of select: 0 if at least one row was selected and 4 if no rows selected

### Method OPEN_CURSOR
Makes it possible to simulate OPEN CURSOR/ FETCH NEXT CURSOR in standard Open SQL.
Like in Open SQL first you have to call OPEN CURSOR for SELECT SQL statement. Then you make subsequent calls of FETCH_NEXT_CURSOR or FETCH_NEXT_CURSOR_TO_ITAB to get result data in packaged manner.

Parameters:
1. IV_SELECT – Open SQL statement as string

   You can get more information in [detailed description of IV_SELECT parameter](#Detailed-description-of-parameter-IV_SELECT).
2. IT_PARAMETERS – lets to pass bind variables to SQL statement

   You can get more information in [detailed description of parameter IT_PARAMETERS](#Detailed-description-of-parameter-IT_PARAMETERS).
3. IT_FOR_ALL_ENTRIES_TABLE – lets to pass internal table as base table of FOR ALL ENTIRES SQL statement

   You can get more information in [detailed description of parameter IT_FOR_ALL_ENTRIES_TABLE](#Detailed-description-of-parameter-IT_FOR_ALL_ENTRIES_TABLE).
4. RV_CURSOR – returns cursor identifier that you can use in FETCH_NEXT_CURSOR/FETCH_NEXT_CURSOR_TO_ITAB methods calls

See example in FETCH_NEXT_CURSOR_TO_ITAB method description.

### Method FETCH_NEXT_CURSOR
Returns necessary rows from cursor which was initialized previously in OPEN_CURSOR method call.
Unlike SELECT and SELECT_TO_ITAB methods FETCH_NEXT_CURSOR returns limited number of rows by one call. To retrieve all result rows of select you have to make subsequent calls of FETCH_NEXT_CURSOR until data set of cursor finishes.

Parameters:
1. IV_CURSOR – cursor identifier returned by OPEN_CURSOR
2. IV_PACKAGE_SIZE – maximum number of rows returned by the method
3. ED_RESULT_AS_TABLE – result data containing dynamic internal table with result. If current package is the last one it can contain less rows than IV_PACKAGE_SIZE or no rows at all
4. EV_SUBRC – 0 if any data returned and 4 if no data returned

See example in FETCH_NEXT_CURSOR_TO_ITAB method description.

### Method FETCH_NEXT_CURSOR_TO_ITAB
The method is almost the same as SELECT but the result is returned into generic table with type ANY TABLE.

Parameters:
1. IV_CURSOR – cursor identifier returned by OPEN_CURSOR
2. IV_DO_INTO_CORRESPONDING – if true then result is returned as by SELECT … INTO CORRESPONDING FIELDS OF TABLE. If false then result is returned as by SELECT … INTO TABLE
3. IV_PACKAGE_SIZE – maximum number of rows returned by the method
4. ET_RESULT_TABLE – target table to return result data. Any internal table can be set to this parameter. The result will be moved as move-corresponding or in field order depending on IV_DO_INTO_CORRESPONDING parameter
5. EV_SUBRC – 0 if any data returned and 4 if no data returned

Example

    DATA: lt_sflight TYPE TABLE OF sflight,
          lt_sflight_all TYPE TABLE OF sflight,
          lv_cursor TYPE cursor.

    lv_cursor = db->open_cursor( iv_select = 'SELECT * FROM sflight' ).

    DATA: lv_subrc TYPE sysubrc.

    DO.
      db_layer->fetch_next_cursor_to_itab( EXPORTING iv_cursor       = lv_cursor
                                                     iv_package_size = 200
                                           IMPORTING et_result_table = lt_sflight
                                                     ev_subrc        = lv_subrc ).
      IF lv_subrc <> 0.
        EXIT.
      ENDIF.

      APPEND lt_sflight TO lt_sflight_all.
    ENDDO.

### Method INSERT_BY_ITAB
Allows to insert data into database. Like for all other methods resulting database is either real SAP database in case of production mode or fake database in case of test mode.
Inserted data is passed as internal table.

Parameters:
1. IV_TABLE_NAME – table name of transparent table. Parameter is optional. If parameter is omitted then zif_zosql_db_layer tries to guess table name according to the type of internal table with inserted data
2. IT_NEW_LINES – inserted data as internal table. If IV_TABLE_NAME is empty then zif_zosql_ddb_layer tries to guess table name according to type of this parameter
3. RV_SUBRC – returns 0 if any record inserted and 4 otherwise

Example
    DATA: lt_scarr TYPE TABLE OF scarr,
          ls_scarr TYPE scar.

    ls_scarr-carrid = 'YY'.
    ls_scarr-carrname = 'Y test carrier'.
    ls_scarr-currcode = 'USD'.
    APPEND ls_scarr TO lt_scarr.

    db_layer->insert_by_itab( lt_scarr ).

### Method UPDATE_BY_ITAB
Performs UPDATE operation on database table.

Like for all other methods resulting database is either real SAP database in case of production mode or fake database in case of test mode.
Data to update is passed as internal table.

Parameters:
1. IV_TABLE_NAME – table name of transparent table. Parameter is optional. If parameter is omitted then zif_zosql_db_layer tries to guess table name according to the type of internal table with inserted data
2. IT_LINES_FOR_MODIFY – update data as internal table. If IV_TABLE_NAME is empty then zif_zosql_ddb_layer tries to guess table name according to type of this parameter
3. RV_SUBRC – returns 0 if any record updated and 4 otherwise

Example

    DATA: lt_scarr TYPE TABLE OF scarr,
          ls_scarr TYPE scar.

    ls_scarr-carrid = 'YY'.
    ls_scarr-carrname = 'Y test carrier'.
    ls_scarr-currcode = 'USD'.
    APPEND ls_scarr TO lt_scarr.

    db_layer->modify_by_itab( iv_table_name       = 'SCARR'
                              it_lines_for_modify = lt_scarr ).
                        
### Method MODIFY_BY_ITAB
Performs MODIFY operation on database table.
Like for all other methods resulting database is either real SAP database in case of production mode or fake database in case of test mode.
Data to modify is passed as internal table.

Parameters:
1. IV_TABLE_NAME – table name of transparent table. Parameter is optional. If parameter is omitted then zif_zosql_db_layer tries to guess table name according to the type of internal table with inserted data
2. IT_LINES_FOR_MODIFY – update data as internal table. If IV_TABLE_NAME is empty then zif_zosql_ddb_layer tries to guess table name according to type of this parameter

Example
    DATA: lt_scarr TYPE TABLE OF scarr,
          ls_scarr TYPE scar.

    ls_scarr-carrid = 'YY'.
    ls_scarr-carrname = 'Y test carrier'.
    ls_scarr-currcode = 'USD'.
    APPEND ls_scarr TO lt_scarr.

    db_layer->modify_by_itab( iv_table_name       = 'SCARR'
                              it_lines_for_modify = lt_scarr ).

### Method DELETE_BY_ITAB
Performs DELETE operation on database table.
Like for all other methods resulting database is either real SAP database in case of production mode or fake database in case of test mode.
Data to modify is passed as internal table. Only key fields of corresponding database (transparent) table are taken into account.
Parameters:
1. IV_TABLE_NAME – table name of transparent table. Parameter is optional. If parameter is omitted then zif_zosql_db_layer tries to guess table name according to the type of internal table with inserted data
2. IT_LINES_FOR_DELETE –internal table of rows which are necessary to delete. If IV_TABLE_NAME is empty then zif_zosql_ddb_layer tries to guess table name according to type of this parameter
3. RV_SUBRC – returns 0 if any record deleted and 4 otherwise

Example

    DATA: lt_scarr TYPE TABLE OF scarr,
          ls_scarr TYPE scar.

    ls_scarr-carrid = 'YY'.
    APPEND ls_scarr TO lt_scarr.

    db_layer->delete_by_itab( iv_table_name       = 'SCARR'
                              it_lines_for_delete = lt_scarr ).

### Method UPDATE
Allows execution of SQL Update statement passed as dynamic SQL query.
It supports Open SQL syntax with possibility of passing bind parameters.

Parameters:
1. IV_UPDATE_STATEMENT – text of Update Open SQL statement
2. IT_PARAMETERS – lets to pass bind variables to SQL statement
 
You can get more information in [detailed description of parameter IT_PARAMETERS](#Detailed-description-of-parameter-IT_PARAMETERS).
3. RV_SUBRC – returns 0 if any record updated and 4 otherwise

Example of simple update

    db_layer->update( iv_update_statement = 'UPDATE scarr SET carrname = ''New name'' WHERE carrid = ''AA''' ).

Example of update with parameter

    ls_param-parameter_name_in_select = ':CARRID'.
    ls_param-parameter_value_single = 'AA'.
    APPEND ls_param TO lt_params.

    DATA: lv_update_statement TYPE string.

    CONCATENATE 'UPDATE scarr'
      'SET carrname = ''New name'''
      'WHERE carrid = :CARRID'
      INTO lv_update_statement SEPARATED BY space.

    db_layer->update( iv_update_statement = lv_update_statement
                      it_parameters = lt_params ).

### Method DELETE
Allows execution of SQL Delete statement passed as dynamic SQL query.
It supports Open SQL syntax with possibility of passing bind parameters.

Parameters:
1. IV_DELETE_STATEMENT – text of delete Open SQL statement
2. IT_PARAMETERS – lets to pass bind variables to SQL statement

You can get more information in [detailed description of parameter IT_PARAMETERS](#Detailed-description-of-parameter-IT_PARAMETERS).
3. RV_SUBRC – returns 0 if any record deleted and 4 otherwise

Example of simple delete

    db_layer->delete( iv_update_statement = 'DELETE scarr WHERE carrid = ''AA''' ).

Example of delete with parameter

    ls_param-parameter_name_in_select = ':CARRID'.
    ls_param-parameter_value_single = 'AA'.
    APPEND ls_param TO lt_params.

    DATA: lv_delete_statement TYPE string.

    CONCATENATE 'DELETE scarr'
      'WHERE carrid = :CARRID'
      INTO lv_delete_statement SEPARATED BY space.

    db_layer->delete( iv_delete_statement = 'DELETE scarr WHERE carrid = :CARRID'
                      it_parameters       = lt_params ).

### Method COMMIT
Allows to commit changes to database

### Detailed description of parameter IV_SELECT
This parameter lets you pass SQL statement itself.

Example:

    …
    IV_SELECT = 'SELECT * FROM SFLIGHT'
    …

So the SQL statement which is executed with Z-SQL Test Double Framework layer ZIF_ZOSQL_DB_LAYER is a dynamic SQL statement in fact.
SQL statement can contain most of usual parts that regular Open SQL select statement has.

Detailed description of SQL select parts see in table below

|SQL Part           |May exist in IV_SELECT parameter|Comments                          |
|:------------------|:-------------------------------|:---------------------------------|
|SELECT <field_list>|Yes                             |Has the same syntax as in Open SQL|
|SINGLE, DISTINCT   |Yes                             |Has the same syntax as in Open SQL|
|FROM               |Yes                             |Has the same syntax as in Open SQL. Joins are supported including outer joins. Bind parameters in JOIN are also supported|
|INTO               |No                              |Unlike implicit 'INTO' statement in Z-SQL Test Double framework you pass parameter to select method with result table when you call ZIF_ZOSQL_DB_LAYER method that contains IV_SELECT parameter.|
|UP TO … ROWS       |Yes                             |Has the same syntax as in Open SQL. Bind parameters containing number of rows are also supported|
|FOR ALL ENTRIES IN |Yes                             |If you want to execute SELECT for all entries you should include 'FOR ALL ENTRIES IN' part in select text implicitly. The name of base table in for all entries does not matter because anyway it takes it's content from parameter IT_FOR_ALL_ENTRIES_TABLE|
|WHERE              |Yes                             |Has the same syntax as in Open SQL. Bind parameters containing number of rows are also supported. Subqueries are supported with limitation that you must have at least 740 version of SAP Basis.|
|GROUP BY           |Yes                             |Has the same syntax as in Open SQL|
|HAVING             |Yes                             |Has the same syntax as in Open SQL.  Bind parameters containing number of rows are also supported. Subqueries are supported with limitation that you must have at least 740 version of SAP Basis.|
|ORDER BY           |Yes                             |Has the same syntax as in Open SQL|

See [examples](examples.md) of converting Open SQL statements into Z-ZSQL Test Double Framework database layer calls.

### Detailed description of parameter IT_PARAMETERS
The parameter contains internal table with bind parameters which we need to pass to SQL call.
Each row describes one bind parameter.

Each line contains fields:
* PARAM_NAME_IN_SELECT – unique name which must be replaced with parameter value during execution of SQL. During SQL execution database layer will search for the name and replace it’s value with passed one. Name search is not case sensitive. The field is mandatory. 
* PARAMETER_VALUE_SINGLE – value of parameter in case it is single value
* PARAMETER_VALUE_RANGE – you can pass range table to use in 'IN’ SQL conditions
* PARAMETER_VALUE_REF – ref to value of parameter as alternative to PARAMETER_VALUE_SINGLE

You must fill one of fields PARAMETER_VALUE_REF, PARAMETER_VALUE_RANGE or PARAMETER_VALUE_REF

Example of single value parameter

    ls_param-parameter_name_in_select = ':CARRID'.
    ls_param-parameter_value_single = 'AA'.
    APPEND ls_param TO lt_params.

    db_layer->select_to_itab( EXPORTING iv_select = 'SELECT * FROM scarr WHERE carrid = :carrid'
                                        it_parameters = lt_params
                              IMPORTING et_result_table = lt_scarr ).

Example of parameter passed as ref to data
    
    ls_param-parameter_name_in_select = ':CARRID'.
    
    CREATE DATA ls_param-parameter_value_ref TYPE S_CARR_ID.
    ASSIGN ls_param-parameter_value_ref->* TO <scarr>.
    <scarr> = 'AA'.
    
    APPEND ls_param TO lt_params.

    db_layer->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM scarr WHERE carrid = :carrid'
                                        it_parameters   = lt_params
                              IMPORTING et_result_table = lt_scarr ).

Example of parameters passed as range to perform 'IN' comparison

    ls_param-parameter_name_in_select = ':CARRID'.

    ls_param_range_line-sign = 'I'.
    ls_param_range_line-option = 'EQ'.
    ls_param_range_line-low  = 'AA'.
    APPEND ls_param_range_line TO ls_param-parameter_value_range.

    ls_param_range_line-sign = 'I'.
    ls_param_range_line-option = 'EQ'.
    ls_param_range_line-low  = 'AB'.
    APPEND ls_param_range_line TO ls_param-parameter_value_range.

    APPEND ls_param TO lt_params.

    db_layer->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM scarr WHERE carrid IN :carrid'
                                        it_parameters   = lt_params
                              IMPORTING et_result_table = lt_scarr ).

If you use 'IN' comparison operator and pass empty range then the condition will be ignored just like in standard Open SQL.

### Detailed description of parameter IT_FOR_ALL_ENTRIES_TABLE
In order to execute select FOR ALL ENTRIES you need to pass base table in parameter IT_FOR_ALL_ENTRIES_TABLE.
The table can contain structure but not structured table is also available just like in standard Open SQL.

In select statement string (parameter IV_SELECT) it does not matter how you call the table in FOR ALL ENTRIES. But the name of base table in SELECT must correspond to the one in WHERE clause.

Example of simple select for all entries

    DATA: lt_search_scarr TYPE TABLE OF scarr,
          ls_scarr TYPE scarr.

    ls_scarr-carrid = 'AA'.
    APPEND ls_scarr TO lt_search_scarr.

    ls_scarr-carrid = 'AB'.
    APPEND ls_scarr TO lt_search_scarr.

    DATA: lt_selected_scarr TYPE TABLE OF scarr,
          lv_select               TYPE string.

    CONCATENATE 'SELECT * FROM scarr'
      'FOR ALL ENTRIES IN ITAB'
      'WHERE carrid = ITAB-carrid'
      INTO lv_select SEPARATED BY space.

    db_layer->select_to_itab( EXPORTING iv_select = 'SELECT * FROM scarr WHERE carrid IN :carrid'
                                        it_for_all_entries_table = lt_selected_scarr
                              IMPORTING et_result_table = lt_selected_scarr ).

In this example internal table of type SCARR is passed as for all entries base table with the field CARRID filled.
In select the for all entries table is called ITAB and in where the same name is used in condition:
carrid = ITAB-carrid
(upper/lower case does not matter)

Example of select for all entries with not structured base table

    DATA: lt_search_scarr TYPE TABLE OF char3,
          ls_scarr TYPE scarr.

    APPEND 'AA' TO lt_search_scarr.

    APPEND 'AB' TO lt_search_scarr.

    DATA: lt_selected_scarr TYPE TABLE OF scarr,
          lv_select         TYPE string.

    CONCATENATE 'SELECT * FROM scarr'
      'FOR ALL ENTRIES IN ITAB'
      'WHERE carrid = ITAB-TABLE_LINE'
      INTO lv_select SEPARATED BY space.

    db_layer->select_to_itab( EXPORTING iv_select                = 'SELECT * FROM scarr WHERE carrid IN :carrid'
                                        it_for_all_entries_table = lt_selected_scarr
                              IMPORTING et_result_table          = lt_selected_scarr ).

## Interface ZIF_ZOSQL_TEST_ENVIRONMENT
You can use the interface to manipulate virtual database data and to create instance of ZIF_ZOSQL_DB_LAYER for unit test mode.
If you want to test some database operations with ZIF_ZOSQL_DB_LAYER you may prepare some data in virtual database before with the instance of ZIF_ZOSQL_TEST_ENVIRONMENT interface.

Example:

    DATA: lo_test_environment TYPE REF TO zif_zosql_test_environment.

    lo_test_environment = Zcl_zosql_test_environtment=>create( ).
    DATA: lt_scarr TYPE TABLE OF scarr,
          ls_scarr TYPE scarr.

    ls_scarr-carrid = 'YY'.
    ls_scarr-carrname = 'Test carrier'.
    APPEND ls_scarr TO lt_scarr.
    
    lo_test_environment->insert_test_data( lt_scarr ).
    
    DATA: lo_db_layer TYPE REF TO zif_zolsq_db_layer.

    lo_db_layer = Lo_test_environment->get_db_layer_for_unit_tests( ).
    
    DATA: lt_scarr_selected TYPE TABLE OF scarr.
    
    lo_db_layer->select_to_itab( EXPORTING iv_select = 'SELECT * FROM sflight'
                                 IMPORTING et_result_table = lt_scarr_selected ).

More complicated example is contained inside project named ZOSQL_DB_EXAMPLE_REP (run and open with SE38).

### How to create instance
Just call this static method:

    ZCL_ZOSQL_TEST_ENVIRONMENT=>CREATE
    
### Method CLEAR_ALL
Clears all data in virtual database.
Data can appear in virtual database either after INSERT_TEST_DATA method call or after insert/update/modify/delete operations performed with ZIF_ZOSQL_DB_LAYER in test mode.
Works on virtual database and doesn't delete anything in real database.

### Method CLEAR_DOUBLES
The same as CLEAR_ALL

### Method CLEAR_ONE_TABLE
Clears data of one table in virtual database.
Data can appear in virtual database either after INSERT_TEST_DATA method call or after insert/update/modify/delete operations performed with ZIF_ZOSQL_DB_LAYER in test mode.

THe methods affects virtual database and doesn’t delete anything in real database.

Parameters:
* IV_TABLE_NAME – name of database table (transparent table)

### Method DELETE_TEST_DATA_FROM_ITAB
Lets to perform partial deletion of table in virtual database.
In order to delete some of table records in virtual database you need to pass internal table with database table key fields filled in. Deletion will be performed according to provided key field combinations.
Data can appear in virtual database either after INSERT_TEST_DATA method call or after insert/update/modify/delete operations performed with ZIF_ZOSQL_DB_LAYER in test mode.

Parameters:
* IT_LINES_FOR_DELETE – internal table which must contain key fields of database table with the same name
* IV_TABLE_NAME – name of database table (transparent table)
* RV_SUBRC – returns 0 if at least one record was deleted or 4 otherwise

### Method GET_DATA_OF_TABLE
Returns data of one database (transparent) table in virtual database.
Data is returned to internal table with move-corresponding statement.
Data can appear in virtual database either after INSERT_TEST_DATA method call or after insert/update/modify/delete operations performed with ZIF_ZOSQL_DB_LAYER in test mode.

Parameters:
* IV_TABLE_NAME – name of database table (transparent table)
* ET_TABLE – returns table data to any table with move-corresponding statement

### Method GET_DATA_OF_TABLE_AS_REF
Returns data of one database (transparent) table in virtual database.
Data is returned to internal as reference to internal table.
Data can appear in virtual database either after INSERT_TEST_DATA method call or after insert/update/modify/delete operations performed with ZIF_ZOSQL_DB_LAYER in test mode.

Parameters:
* IV_TABLE_NAME – name of database table (transparent table)
* RD_REF_TO_DATA – reference to internal table that contains data

### Method INSERT_TEST_DATA
Lets to insert data in virtual database.
Inserted data can be selected with ZIF_ZOSQL_DB_LAYER in form of Open SQL dynamic statements.

Parameters:
* IT_TABLE – data to be inserted
* IV_TABLE_NAME – name of database (transparent) table. Parameter is not mandatory. If parameter is omitted than method tries to get data type from internal table passed as IT_TABLE parameter. If it is a table type of database (transparent) table than the data will be inserted in virtual database table with the same name.
If IT_TABLE contains any other type than the parameter IV_TABLE_NAME is mandatory.
