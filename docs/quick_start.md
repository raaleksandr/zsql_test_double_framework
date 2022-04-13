[Back to readme](https://github.com/raaleksandr/zsql_test_double_framework#readme)

    REPORT zosql_quick_start.

    DATA: go_db_layer TYPE REF TO zif_zosql_db_layer.

    START-OF-SELECTION.
      PERFORM main.

    FORM main.

      DATA: ld_result TYPE REF TO data,
            lo_alv    TYPE REF TO cl_salv_table.

      go_db_layer = zcl_zosql_test_environment=>get_db_layer_for_production( ).
      PERFORM simple_select CHANGING ld_result.
      PERFORM display_result USING ld_result.
    ENDFORM.

    FORM display_result USING id_dynamic_table TYPE REF TO data
                  RAISING cx_salv_msg.

      DATA: lo_alv     TYPE REF TO cl_salv_table.

      FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

      ASSIGN id_dynamic_table->* TO <lt_table>.

      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                              CHANGING  t_table      = <lt_table> ).

      lo_alv->display( ).
    ENDFORM.

    " Production code - database select
    FORM simple_select CHANGING cd_result TYPE REF TO data
                                RAISING zcx_zosql_error.
      go_db_layer->select( EXPORTING iv_select = 'SELECT * FROM sbook'
                           IMPORTING ed_result_as_table = cd_result ).
    ENDFORM.

    CLASS lcl_unittest DEFINITION FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

      PUBLIC SECTION.
        METHODS: test_the_code FOR TESTING.
    ENDCLASS.

    CLASS lcl_unittest IMPLEMENTATION.
      METHOD test_the_code.
        DATA: ls_sbook            TYPE sbook,
              lt_sbook            TYPE TABLE OF sbook,
              ld_data             TYPE REF TO data,
              lt_sbook_selected   TYPE TABLE OF sbook,
              lo_test_environment TYPE REF TO zif_zosql_test_environment.

        FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

        " Unit test code - initialization
        lo_test_environment = zcl_zosql_test_environment=>create( ).
        lo_test_environment->clear_doubles( ).
        go_db_layer = lo_test_environment->get_db_layer_for_unit_tests( ).

        " Unit test code - run
        ls_sbook-mandt  = sy-mandt.
        ls_sbook-bookid = '102'.
        ls_sbook-carrid = '2'.
        ls_sbook-fldate = '20211002'.
        APPEND ls_sbook TO lt_sbook.

        lo_test_environment->insert_test_data( lt_sbook ).

        PERFORM simple_select CHANGING ld_data.
        ASSIGN ld_data->* TO <lt_data>.
        MOVE-CORRESPONDING <lt_data> TO lt_sbook_selected.

        cl_aunit_assert=>assert_equals( exp = lt_sbook act = lt_sbook_selected ).
      ENDMETHOD.
    ENDCLASS.
