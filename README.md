# zsql_test_double_framework
Z-SQL Test Double Framework - Z Downport of SQL Test Double Framework to versions lower than 7.51  

# General description
ABAP unit tests should be fast and stable. If you want to test code working with database then you should mock the database or it won't be a unit test at all.
SAP offers a very powerful library for this - SQL Test Double Framework, including Open SQL Test Double Frame work to mock database interaction from ABAP with Open SQL and CDS Test Double Framework to mock CDS View calls.  
But unfortunately the tool is available only since 7.51 version, but there is a lot of projects having maximum 7.50 version of SAP - all ERP installations, SAP Solution Manager etc. So a lot of projects are out of this possibility.  
For older systems the solution is to use Z-SQL Test Double Framework, a library that allows to mock database interaction by using dynamic SQL calls and special interface called ZIF_ZOSQL_DB_LAYER.  

# Quick start examples

" Production code - initialization  
go_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).  
  
" Production code - database select  
FORM simple_select CHANGING cd_result TYPE REF TO data  
                   RAISING zcx_testable_db_layer.  
  go_db_layer->select( EXPORTING iv_select          = 'SELECT * FROM sbook'  
                       IMPORTING ed_result_as_table = cd_result ).  
ENDFORM.  
                       
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
  
# More examples  
More examples you can find in programs inside project:  
ZTESTABLE_DB_EXAMPLE_HELLWORLD - a set of database select examples with some unit tests;  
ZTESTABLE_DB_EXAMPLE_REP - report with several selects and further data modification before alv output. Also contains unit tests for the report;  
ZTESTABLE_DB_EXAMPLE_REP_INIT - report not using library to show the difference of using Z-SQL Test double framework. Contains no tests;  
ZTESTABLE_DB_EXAMPLE_DBCHANGE - example of program that writes data to database. Also contains unit tests.  
