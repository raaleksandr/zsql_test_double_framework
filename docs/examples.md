# Examples
## Examples inside project
Some examples you can find in programs inside project:
* ZTESTABLE_DB_EXAMPLE_HELLWORLD - a set of database select examples with some unit tests;
* ZTESTABLE_DB_EXAMPLE_REP - report with several selects and further data modification before alv output. Also contains unit tests for the report;
* ZTESTABLE_DB_EXAMPLE_DBCHANGE - example of program that writes data to database. Also contains unit tests.
* ZTESTABLE_DB_EXAMPLE_REP_INIT - the same as ZTESTABLE_DB_EXAMPLE_REP but it does not use the interface. You can compare these two reports to see the difference between code that uses Z-SQL Test Double Framework and the one written just with regular Open SQL

## Examples of code adaptation for Z-SQL Test Double Framework
Sometimes you need to create tests for the code that already exists.
Code can contain some select, insert, update, modify and delete database operations that you need replace if calls to ZIF_ZOSQL_DB_LAYER.
Adaptation must not be hard because SQL syntax will be saved.
There are some nuances for passing of parameters, returning of values, for all entires select calls.
Below are examples of such code transformations showing how to do that in different cases.
In examples creation of interface ZIF_ZOSQL_DB_LAYER is omitted.
You can see it in project examples, project unit tests or Reference chapter.

### Simple select
With Open SQL

    SELECT * FROM sflight INTO TABLE lt_sflight.

With Z-SQL Test Double Framework

    lo_db_layer->select_to_itab( EXPORTING iv_select = ‘SELECT * FROM sflight’
                                 IMPORTING et_result_table = lt_sflight ).

### Select with bind parameter
With open SQL

    DATA: lv_carrid TYPE scarr-carrid,
          lv_carrname TYPE scarr-carrname.
    
    lv_carrid = ‘AB’.
    SELECT SINGLE carrname 
      FROM scarr
      WHERE carrid = lv_carrid.

With Z-SQL Test Double Framework

    DATA: lv_carrid TYPE scarr-carrid,
          lv_carrname TYPE scarr-carrname.

    lv_carrid = ‘AB’.

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_select TYPE string.

    CONCATENATE 'SELECT SINGLE carrname'
      'FROM scarr'
      'WHERE CARRID = :CARRID'
      INTO lv_select SEPARATED BY space.

    ls_param-param_name_in_select = ':CARRID'.
    ls_param-parameter_value_single = lv_carrid.
    APPEND ls_param TO lt_params.

    DATA: ls_scarr TYPE scarr.

    lo_db_layer->select_to_itab( EXPORTING iv_select       = lv_select
                                           it_parameters   = lt_params
                                 IMPORTING es_result_line = ls_scarr ).
    lv_carrname = ls_scarr-carrname.
