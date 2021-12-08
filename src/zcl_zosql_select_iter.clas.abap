class ZCL_ZOSQL_SELECT_ITER definition
  public
  create public .

public section.

  interfaces ZIF_ZOSQL_ITERATOR .

  methods CONSTRUCTOR
    importing
      !IO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT optional
      !IV_SELECT type CLIKE
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS optional
      !IT_FOR_ALL_ENTRIES_TABLE type ANY TABLE optional .
protected section.
private section.

  data MO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT .
  data MV_SELECT type STRING .
  data MT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS .
  data MR_FOR_ALL_ENTRIES_TABLE type ref to DATA .
  data MO_ZOSQL_DB_LAYER_FAKE type ref to ZCL_ZOSQL_DB_LAYER_FAKE .
  data MR_RESULT_OF_QUERY type ref to DATA .
  data MV_CURRENT_RECORD_NUM type INT4 .

  methods _EXECUTE_SQL_QUERY
    raising
      ZCX_ZOSQL_ERROR .
  methods _GET_COUNT
    returning
      value(RV_COUNT) type I .
  methods _IS_SQL_QUERY_EXECUTED
    returning
      value(RV_SQL_QUERY_EXECUTED) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_ZOSQL_SELECT_ITER IMPLEMENTATION.


  method CONSTRUCTOR.
    DATA: lo_factory TYPE REF TO zif_zosql_factory.

    FIELD-SYMBOLS: <lt_for_all_entries_table> TYPE ANY TABLE.

    IF io_zosql_test_environment IS BOUND.
      mo_zosql_test_environment = io_zosql_test_environment.
    ELSE.
      CREATE OBJECT lo_factory TYPE zcl_zosql_factory.
      mo_zosql_test_environment = lo_factory->get_test_environment( ).
    ENDIF.

    mv_select = iv_select.
    mt_parameters = it_parameters.
    IF it_for_all_entries_table IS NOT INITIAL.
      CREATE DATA mr_for_all_entries_table LIKE it_for_all_entries_table.
      ASSIGN mr_for_all_entries_table->* TO <lt_for_all_entries_table>.
      <lt_for_all_entries_table> = it_for_all_entries_table.
    ENDIF.

    CREATE OBJECT mo_zosql_db_layer_fake
      EXPORTING
        io_zosql_test_environment = mo_zosql_test_environment.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~CREATE_EMPTY_RECORD_AS_REF.
    DATA: ld_empty_table_for_select TYPE REF TO data.

    FIELD-SYMBOLS: <lt_empty_table_for_select> TYPE STANDARD TABLE.

    ld_empty_table_for_select = mo_zosql_db_layer_fake->create_empty_table_for_select( mv_select ).
    ASSIGN ld_empty_table_for_select->* TO <lt_empty_table_for_select>.

    CREATE DATA rd_ref_to_empty_record LIKE LINE OF <lt_empty_table_for_select>.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~DELETE_RECORD_BY_UNIQUE_ID.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~GET_CURRENT_RECORD_REF.
    DATA: lv_index    TYPE i.

    FIELD-SYMBOLS: <lt_result_of_query> TYPE STANDARD TABLE,
                   <ls_result_rec>      TYPE any.

    IF _is_sql_query_executed( ) <> abap_true.
      _execute_sql_query( ).
    ENDIF.

    IF zif_zosql_iterator~is_empty( ) <> abap_true.
      lv_index = mv_current_record_num.
      IF lv_index < 1.
        lv_index = 1.
      ELSEIF lv_index > _get_count( ).
        lv_index = _get_count( ).
      ENDIF.

      ASSIGN mr_result_of_query->* TO <lt_result_of_query>.
      CREATE DATA rd_ref_to_data_of_current_rec LIKE LINE OF <lt_result_of_query>.
      ASSIGN rd_ref_to_data_of_current_rec->* TO <ls_result_rec>.
      READ TABLE <lt_result_of_query> INDEX lv_index INTO <ls_result_rec>.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~GET_CURRENT_RECORD_UNIQUE_ID.
    rv_unique_id = |{ mv_current_record_num }|.
  endmethod.


  METHOD zif_zosql_iterator~get_iterator_position_object.
    CREATE OBJECT ro_iterator_pos.
    ro_iterator_pos->add_data_set_data( id_ref_to_current_line = zif_zosql_iterator~get_current_record_ref( ) ).
  ENDMETHOD.


  method ZIF_ZOSQL_ITERATOR~IS_EMPTY.
    IF _is_sql_query_executed( ) <> abap_true.
      _execute_sql_query( ).
    ENDIF.

    IF _get_count( ) < 1.
      rv_is_empty = abap_true.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~MOVE_TO_FIRST.
    IF _is_sql_query_executed( ) <> abap_true.
      _execute_sql_query( ).
    ENDIF.

    IF zif_zosql_iterator~is_empty( ) <> abap_true.
      mv_current_record_num = 1.
      rv_successful_move = abap_true.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~MOVE_TO_NEXT.
    IF mv_current_record_num < _get_count( ).
      mv_current_record_num = mv_current_record_num + 1.
      rv_successful_move    = abap_true.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~RECORD_IS_LAST.
    IF mv_current_record_num = _get_count( ).
      rv_is_last = abap_true.
    ENDIF.
  endmethod.


  method _EXECUTE_SQL_QUERY.

    DATA: ld_result_as_table  TYPE REF TO data.

    FIELD-SYMBOLS: <lt_for_all_entries_table> TYPE ANY TABLE.

    IF mr_for_all_entries_table IS BOUND.
      ASSIGN mr_for_all_entries_table->* TO <lt_for_all_entries_table>.
      mo_zosql_db_layer_fake->zif_zosql_db_layer~select( EXPORTING iv_select                = mv_select
                                                                   it_parameters            = mt_parameters
                                                                   it_for_all_entries_table = <lt_for_all_entries_table>
                                                         IMPORTING ed_result_as_table       = ld_result_as_table ).
    ELSE.
      mo_zosql_db_layer_fake->zif_zosql_db_layer~select( EXPORTING iv_select          = mv_select
                                                                   it_parameters      = mt_parameters
                                                         IMPORTING ed_result_as_table = ld_result_as_table ).
    ENDIF.

    mr_result_of_query = ld_result_as_table.
  endmethod.


  method _GET_COUNT.
    FIELD-SYMBOLS: <lt_sql_result> TYPE STANDARD TABLE.

    ASSIGN mr_result_of_query->* TO <lt_sql_result>.
    rv_count = LINES( <lt_sql_result> ).
  endmethod.


  method _IS_SQL_QUERY_EXECUTED.
    IF mr_result_of_query IS BOUND.
      rv_sql_query_executed = abap_true.
    ENDIF.
  endmethod.
ENDCLASS.
