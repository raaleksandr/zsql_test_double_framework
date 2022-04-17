[Back to readme](https://github.com/raaleksandr/zsql_test_double_framework#readme)

# Examples

# Contents

[Examples inside project](#Examples-inside-project)

[Examples of code adaptation for Z-SQL Test Double Framework](#Examples-of-code-adaptation-for-Z-SQL-Test-Double-Framework)
* [Simple select](#Simple-select)
* [Select with bind parameter](#Select-with-bind-parameter)
* [Select with IN operator](#Select-with-IN-operator)
* [Select for all entries](#Select-for-all-entries)
* [Open cursor / fetch next cursor](#Open-cursor--fetch-next-cursor)
* [Insert](#Insert)
* [Simple update](#Simple-update)
* [Update with WHERE condition](#Update-with-WHERE-condition)
* [Update with bind parameters](#Update-with-bind-parameters)
* [Modify](#Modify)
* [Simple delete](#Simple-delete)
* [Delete with WHERE condition](#Delete-with-WHERE-condition)
* [Delete with bind parameter](#Delete-with-bind-parameter)

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
    
    DATA: lt_sflight TYPE TABLE OF sflight.
    
    SELECT * FROM sflight INTO TABLE lt_sflight.

With Z-SQL Test Double Framework
        
    DATA: lo_db_layer  TYPE REF TO zif_zosql_db_layer,
          lt_sflight   TYPE TABLE OF sflight.
    
    lo_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).
    lo_db_layer->select_to_itab( EXPORTING iv_select       = 'SELECT * FROM sflight'
                                 IMPORTING et_result_table = lt_sflight ).
                                 
Examples contain production code.
In order to write unit tests you just need to initialize lo_db_layer in different way like that:

    DATA: lo_test_environment   TYPE REF TO zif_zosql_test_environment,
          lo_db_layer           TYPE REF TO zif_zosql_db_layer,
          ls_scarr              TYPE scarr,
          lt_scarr              TYPE TABLE OF scarr.
          
    lo_test_environment = zcl_zosql_test_environment=>create( ).
    
    ls_scarr-carrid = 'YY'.
    ls_scarr-carrname = 'Y Carrier'.
    APPEND ls_scarr TO lt_scarr.
    
    lo_test_environment->insert_test_data( lt_scarr ).
    
    lo_db_layer = lo_test_environment->get_db_layer_for_unit_tests( ).

### Select with bind parameter
With open SQL

    DATA: lv_carrid TYPE scarr-carrid,
          lv_carrname TYPE scarr-carrname.
    
    lv_carrid = 'AB'.
    SELECT SINGLE carrname
      INTO lv_carrname
      FROM scarr
      WHERE carrid = lv_carrid.

With Z-SQL Test Double Framework

    DATA: lo_db_layer  TYPE REF TO zif_zosql_db_layer,
          lv_carrid    TYPE scarr-carrid,
          lv_carrname  TYPE scarr-carrname.
    
    lo_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).
    
    lv_carrid = 'AB'.

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

### Select with IN operator
With open SQL

    DATA: lr_carrid TYPE RANGE OF scarr-carrid,
          lt_scarr  TYPE TABLE OF scarr.

    FIELD-SYMBOLS: <fs_carrid_line> LIKE LINE OF lr_carrid.

    APPEND INITIAL LINE TO lr_carrid ASSIGNING <fs_carrid_line>.
    <fs_carrid_line>-sign   = 'I'.
    <fs_carrid_line>-option = 'EQ'.
    <fs_carrid_line>-low    = 'AB'.

    SELECT * 
      FROM scarr
      INTO CORRESPONDING FIELDS OF TABLE lt_scarr
      WHERE carrid IN lr_carrid.

With Z-SQL Test Double Framework

    DATA: lo_db_layer  TYPE REF TO zif_zosql_db_layer,
          lt_scarr     TYPE TABLE OF scarr.

    DATA: lt_params           TYPE zosql_db_layer_params,
          ls_param            TYPE zosql_db_layer_param,
          ls_param_range_line TYPE zosql_db_layer_range_line,
          lv_select           TYPE string.
          
    lo_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).

    ls_param-param_name_in_select = ':CARRID_RANGE'.
    ls_param_range_line-sign      = 'I'.
    ls_param_range_line-option    = 'EQ'.
    ls_param_range_line-low       = 'AB'.
    APPEND ls_param_range_line TO ls_param-parameter_value_range.
    APPEND ls_param TO lt_params.

    CONCATENATE 'SELECT carrname'
      'FROM scarr'
      'WHERE CARRID IN :CARRID_RANGE'
      INTO lv_select SEPARATED BY space.

    lo_db_layer->select_to_itab( EXPORTING iv_select       = lv_select
                                           it_parameters   = lt_params
                                 IMPORTING et_result_table = lt_scarr ).

### Select for all entries
With Open SQL

    TYPES: BEGIN OF ty_scarr,
      Carrid TYPE scarr-carrid,
    END OF ty_scarr.

    DATA: lt_scarr_ids TYPE TABLE OF ty_scarr.

    APPEND 'AA' TO lt_scarr_ids.
    APPEND 'AB' TO lt_scarr_ids.

    DATA: lt_scarr TYPE TABLE OF scarr.

    SELECT *
      FROM scarr
      INTO CORRESPONDING FIELDS OF TABLE lt_scarr
      FOR ALL ENTRIES IN lt_scarr_ids
      WHERE carrid = lt_scarr_ids-carrid.

With Z-SQL Test Double Framework

    TYPES: BEGIN OF ty_scarr,
      carrid TYPE scarr-carrid,
    END OF ty_scarr.

    DATA: lo_db_layer  TYPE REF TO zif_zosql_db_layer,
          lt_scarr_ids TYPE TABLE OF ty_scarr.
          
    lo_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).

    APPEND 'AA' TO lt_scarr_ids.
    APPEND 'AB' TO lt_scarr_ids.

    DATA: lt_scarr TYPE TABLE OF scarr.
    DATA: lv_select TYPE string.

    CONCATENATE 'SELECT *'
      'FROM scarr'
      'FOR ALL ENTRIES IN lt_scarr_ids'
      'WHERE carrid = lt_scarr_ids-carrid'
      INTO lv_select SEPARATED BY SPACE.

    lo_db_layer->select_to_itab( EXPORTING iv_select                = lv_select
                                           it_for_all_entries_table = lt_scarr_ids
                                 IMPORTING et_result_table          = lt_scarr ).
                                 
### Open cursor / fetch next cursor
With open SQL

    DATA: lt_sflight_package TYPE TABLE OF sflight,
          lt_sflight_all     TYPE TABLE OF sflight,
          lv_package_size    TYPE I,
          lv_cursor          TYPE cursor.

    lv_package_size = 10.

    OPEN CURSOR lv_cursor FOR
      SELECT * FROM sflight.

    DO.
      FETCH NEXT CURSOR lv_cursor
      INTO CORRESPONDING FIELDS OF TABLE lt_sflight_package
      PACKAGE SIZE lv_package_size.

      IF sy-subrc = 0.
        APPEND LINES OF lt_sflight_package TO lt_sflight_all.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

With Z-SQL Test Double Framework

    DATA: lo_db_layer        TYPE REF TO zif_zosql_db_layer,
          lt_sflight_package TYPE TABLE OF sflight,
          lt_sflight_all     TYPE TABLE OF sflight,
          lv_package_size    TYPE I,
          lv_cursor          TYPE cursor,
          lv_subrc           TYPE sysubrc.
    
    lo_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).
    
    lv_package_size = 10.

    lv_cursor = lo_db_layer->open_cursor( 'SELECT * FROM sflight' ).

    DO.
      lo_db_layer->fetch_next_cursor_to_itab( EXPORTING iv_cursor       = lv_cursor
                                                        iv_package_size = lv_package_size
                                              IMPORTING et_result_table = lt_sflight_package
                                                        ev_subrc        = lv_subrc ).

      IF lv_subrc = 0.
        APPEND LINES OF lt_sflight_package TO lt_sflight_all.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    
### Insert
With open SQL

    DATA: ls_new_carrier TYPE scarr.

    ls_new_carrier-carrid   = 'YY'.
    ls_new_carrier-carrname = 'New carrier'.
    ls_new_carrier-currcode = 'USD'.
    INSERT scarr FROM ls_new_carrier.

    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      WRITE 'Insert failed'.
    ENDIF.

With Z-SQL Test Double Framework

    DATA: lo_db_layer    TYPE REF TO zif_zosql_db_layer,
          ls_new_carrier TYPE scarr.
    
    lo_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).
    
    ls_new_carrier-carrid = 'YY'.
    ls_new_carrier-carrname = 'New carrier'.
    ls_new_carrier-currcode = 'USD'.

    DATA: lt_new_lines_scarr TYPE TABLE OF scarr.

    APPEND Ls_new_carrier TO lt_new_lines_scarr.

    DATA: lv_subrc TYPE sysubrc.
    lv_subrc = lo_db_layer->insert_by_itab( iv_table_name = 'SCARR'
                                            it_new_lines  = lt_new_lines_scarr ).
    
    IF lv_subrc = 0.
      lo_db_layer->commit( ).
    ELSE.
      WRITE: 'Insert failed'.
    ENDIF.
    
### Simple update
With open SQL

    DATA: ls_updated_carrier TYPE scarr.

    ls_updated_carrier-carrid = 'YY'.
    ls_updated_carrier-carrname = 'New carrier'.
    ls_updated_carrier-currcode = 'USD'.

    UPDATE scarr FROM ls_updated_carrier.

    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      WRITE 'Update failed'.
    ENDIF.

With Z-SQL Test Double Framework

    DATA: lo_db_layer    TYPE REF TO zif_zosql_db_layer,
          ls_new_carrier TYPE scarr.
          
    lo_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).

    ls_new_carrier-carrid = 'YY'.
    ls_new_carrier-carrname = 'New carrier'.
    ls_new_carrier-currcode = 'USD'.

    DATA: lt_new_lines_scarr TYPE TABLE OF scarr.

    APPEND ls_new_carrier TO lt_new_lines_scarr.
    
    DATA: lv_subrc TYPE sysubrc.
    
    lv_subrc = lo_db_layer->update_by_itab( iv_table_name       = 'SCARR'
                                            it_lines_for_update = lt_new_lines_scarr ).
                                             
    IF lv_subrc = 0.
      lo_db_layer->commit( ).
    ELSE.
      WRITE: 'Update failed'.
    ENDIF.

### Update with WHERE condition
With Open SQL

    UPDATE scarr
      SET url = 'http://yycarrier.org'
      WHERE carrid = 'YY'.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      WRITE 'Update failed'.
    ENDIF.

With Z-SQL Test Double Framework

    DATA: lo_db_layer TYPE REF TO zif_zosql_db_layer,
          lv_update   TYPE string,
          lv_subrc    TYPE sysubrc.
          
    lo_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).

    CONCATENATE 'UPDATE scarr'
      'SET url = ''http://yycarrier.org'''
      'WHERE carrid = ''YY'''
      INTO lv_update SEPARATED BY space.

    lv_subrc = lo_db_layer->update( iv_update_statement = lv_update ).
    IF lv_subrc = 0.
      lo_db_layer->commit( ).
    ELSE.
      WRITE: 'Update failed'.
    ENDIF.

### Update with bind parameters
With Open SQL

    DATA: lv_new_url  TYPE string,
          lv_carrid      TYPE scarr-carrid.

    lv_carrid = 'YY'.
    lv_new_url = 'http://yycarrier.org'.

    UPDATE scarr
      SET url = lv_new_url
      WHERE carrid = lv_carrid.

    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      WRITE 'Update failed'.
    ENDIF.

With Z-SQL Test Double Framework

    DATA: lo_db_layer TYPE REF TO zif_zosql_db_layer,
          lv_new_url  TYPE string,
          lv_carrid   TYPE scarr-carrid.
          
    lo_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).

    lv_carrid = 'YY'.
    lv_new_url = 'http://yycarrier.org'.

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_update TYPE string,
          lv_subrc  TYPE sysubrc.

    ls_param-param_name_in_select = ':NEW_URL'.
    ls_param-parameter_value_single = lv_new_url.
    APPEND ls_param TO lt_params.

    ls_param-param_name_in_select = ':CARRID'.
    ls_param-parameter_value_single = lv_carrid.
    APPEND ls_param TO lt_params.
    
    CONCATENATE 'UPDATE scarr'
      'SET url = :NEW_URL'
      'WHERE carrid = :CARRID'
      INTO lv_update SEPARATED BY space.

    lv_subrc = lo_db_layer->update( iv_update_statement = lv_update
                                    it_parameters = lt_params ).
    IF lv_subrc = 0.
      lo_db_layer->commit( ).
    ELSE.
      WRITE: 'Update failed'.
    ENDIF.

### Modify
With open SQL

    DATA: ls_mod_carrier TYPE scarr.

    ls_mod_carrier-carrid = 'YY'.
    ls_mod_carrier-carrname = 'New carrier'.
    ls_mod_carrier-currcode = 'USD'.
    MODIFY scarr FROM ls_mod_carrier.
    COMMIT WORK.

With Z-SQL Test Double Framework

    DATA: lo_db_layer    TYPE REF TO zif_zosql_db_layer,
          ls_mod_carrier TYPE scarr.
          
    lo_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).

    ls_mod_carrier-carrid = 'YY'.
    ls_mod_carrier-carrname = 'New carrier'.
    ls_mod_carrier-currcode = 'USD'.

    DATA: lt_mod_lines_scarr TYPE TABLE OF scarr.

    APPEND ls_mod_carrier TO lt_mod_lines_scarr.

    lo_db_layer->modify_by_itab( iv_table_name       = 'SCARR'
                                 it_lines_for_modify = lt_mod_lines_scarr ).
    lo_db_layer->commit( ).

### Simple delete
With Open SQL

    DATA: ls_scarr_to_delete TYPE scarr.

    ls_scarr_to_delete-carrid = 'YY'.

    DELETE scarr FROM ls_scarr_to_delete.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      WRITE 'Delete failed'.
    ENDIF.

With Z-SQL Test Double Framework

    DATA: lo_db_layer        TYPE REF TO zif_zosql_db_layer,
          ls_scarr_to_delete TYPE scarr.
    
    lo_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).
    ls_scarr_to_delete-carrid = 'YY'.

    DATA: lt_scarr_to_delete TYPE TABLE OF scarr,
          lv_subrc                   TYPE sysubrc.

    APPEND ls_scarr_to_delete TO lt_scarr_to_delete.
    
    lv_subrc = lo_db_layer->delete_by_itab( iv_table_name       = 'SCARR'
                                            it_lines_for_delete = lt_scarr_to_delete ).
    IF lv_subrc = 0.
      lo_db_layer->commit( ).
    ELSE.
      WRITE 'Delete failed'.
    ENDIF.

### Delete with WHERE condition
With Open SQL

    DELETE FROM scarr
      WHERE carrid = 'YY'.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      WRITE 'Delete failed'.
    ENDIF.

With Z-SQL Test Double Framework

    DATA: lo_db_layer TYPE REF TO zif_zosql_db_layer,
          lv_delete   TYPE string,
          lv_subrc    TYPE sysubrc.
    
    lo_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).
    
    CONCATENATE 'DELETE FROM scarr'
      'WHERE carrid = ''YY'''
      INTO lv_delete SEPARATED BY space.

    lv_subrc = lo_db_layer->delete( lv_delete ).
    IF lv_subrc = 0.
      lo_db_layer->commit( ).
    ELSE.
      WRITE 'Delete failed'.
    ENDIF.

### Delete with bind parameter
With Open SQL

    DATA: lv_carrid_to_delete TYPE scarr-carrid.

    lv_carrid_to_delete = 'YY'.

    DELETE FROM scarr
      WHERE carrid = lv_carrid_to_delete.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      WRITE 'Delete failed'.
    ENDIF.

With Z-SQL Test Double Framework

    DATA: lo_db_layer         TYPE REF TO zif_zosql_db_layer,
          lv_carrid_to_delete TYPE scarr-carrid.
    
    lo_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).
    
    lv_carrid_to_delete = 'YY'.

    DATA: lt_params TYPE zosql_db_layer_params,
          ls_param  TYPE zosql_db_layer_param,
          lv_delete TYPE string,
          lv_subrc  TYPE sysubrc.

    ls_param-param_name_in_select = ':CARRID'.
    ls_param-parameter_value_single = lv_carrid_to_delete.
    APPEND ls_param TO lt_params.

    CONCATENATE 'DELETE FROM scarr'
      'WHERE carrid = :CARRID'
      INTO lv_delete SEPARATED BY SPACE.

    lv_subrc = lo_db_layer->delete( iv_delete_statement = lv_delete
                                    it_parameters             = lt_params ).
    IF lv_subrc = 0.
      COMMIT WORK.
    ELSE.
      WRITE 'Delete failed'.
    ENDIF.
