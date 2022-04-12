# zsql_test_double_framework
Z-SQL Test Double Framework - Z Downport of SQL Test Double Framework to BASIS version 7.50 and below.
This projects implements fake database to isolate open SQL operations. In general it copies features of Open SQL Test Double Framework available from BASIS version 7.51. 
The project Z-SQL Test Double Framework may be used from 7.02 version with some restrictions (no new 7.40 syntax, no subqueries). In 7.40 and higher version it works without any restrictions.

# Contents
[General description](#General-description)

[Quick start](#Quick-start)

[Installation](#Installation)

[Requirements](#Requirements)

[Examples](docs/examples.md)

[Reference](docs/reference.md)

# General description
ABAP unit tests should be fast and stable. If you want to test code that works with database then you should mock the database or it won't be a true unit test. SAP offers a very powerful library for this – Open SQL Test Double Framework, including Open SQL Test Double Frame work to mock database interaction from ABAP.
But unfortunately the tool is available only since 7.51 version. A lot of projects with older versions cannot use it. These includes all ERP installations, SAP Solution Manager etc. 
For older systems the solution is to use Z-SQL Test Double Framework, a library that allows to mock database interaction by using dynamic SQL calls and special interface called ZIF_ZOSQL_DB_LAYER.
The main idea is that you use object with interface ZIF_ZOSQL_DB_LAYER in your production code and just inject either production version or test version of the database layer depending on your mode. All Open SQL statements are passed to methods as string. SELECT, INSERT, UPDATE, MODIFY, DELETE statements are supported.

# Quick start
## Quick start example
Production code - initialization

    go_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).
    
    " Production code - database select
    FORM simple_select CHANGING cd_result TYPE REF TO data
                                RAISING zcx_testable_db_layer.
        go_db_layer->select( EXPORTING iv_select = 'SELECT * FROM sbook'
                             IMPORTING ed_result_as_table = cd_result ).
    ENDFORM.

Unit test code

    " Unit test code - initialization
    mo_test_environment = zcl_zosql_test_environment=>create( ).
    mo_test_environment->clear_doubles( ).
    go_db_layer = mo_test_environment->get_db_layer_for_unit_tests( ).
    
    " Unit test code - run
    ls_sbook-bookid = '102'.
    ls_sbook-carrid = '2'.
    ls_sbook-fldate = '20211002'.
    APPEND ls_sbook TO lt_sbook.
    
    mo_test_environment->insert_test_data( lt_sbook ).
    
    PERFORM simple_select CHANGING ld_data.
    ASSIGN ld_data->* TO <lt_data>.
    MOVE-CORRESPONDING <lt_data> TO lt_sbook_selected.
    
    cl_aunit_assert=>assert_equals( exp = lt_sbook act = lt_sbook_selected ).
    
Code of simple_select form:

    FORM simple_select CHANGING cd_result TYPE REF TO data.
        go_db_layer->select( EXPORTING iv_select = ‘SELECT * FROM sbook’
                             IMPORTING ed_result_as_table = cd_result ).
    ENDFORM.
  
# Installation
The project can be installed with ABAP Git. If you encounter error with dictionary types activation during 'Pull' of library with abap git please go to se11 and activate all objects manually. To ensure library is correctly installed you may run unit tests over the package and check whether tests have passed successfully.

# Requirements
It requires SAP BASIS version 702 or higher for most of functionality and SAP BASIS version 740 or higher for full functionality.

Restrictions for versions from 702 to 73x:
* No new 7.40 SQL syntax supported;
* No SQL subqueries supported

# Examples
[See more examples here](docs/examples.md)

# Reference
[Go to reference](docs/reference.md)
