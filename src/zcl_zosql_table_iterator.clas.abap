class ZCL_ZOSQL_TABLE_ITERATOR definition
  public
  create public .

public section.

  interfaces ZIF_ZOSQL_ITERATOR .

  methods CONSTRUCTOR
    importing
      !IO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT
      !IV_TABLE_NAME type CLIKE .
protected section.
private section.

  data MR_TABLE_DATA_REF type ref to DATA .
  data MV_CURRENT_RECORD type I .
  data MV_NUM_LINES type I .
  data MO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT .
  data MV_TABLE_NAME type TABNAME .

  methods _COUNT_NUM_LINES .
ENDCLASS.



CLASS ZCL_ZOSQL_TABLE_ITERATOR IMPLEMENTATION.


  method CONSTRUCTOR.
    mo_zosql_test_environment = io_zosql_test_environment.
    mv_table_name             = iv_table_name.

    mr_table_data_ref = mo_zosql_test_environment->get_data_of_table_as_ref( mv_table_name ).
    _count_num_lines( ).
  endmethod.


  method ZIF_ZOSQL_ITERATOR~CLONE.
    DATA: lo_clone_table_iterator TYPE REF TO zcl_zosql_table_iterator,
          lt_records_to_delete    LIKE it_records_to_delete.

    FIELD-SYMBOLS: <lt_table_this>  TYPE STANDARD TABLE,
                   <lt_table_clone> TYPE STANDARD TABLE,
                   <ls_record_to_delete> LIKE LINE OF lt_records_to_delete.

    CREATE OBJECT lo_clone_table_iterator
      EXPORTING
        io_zosql_test_environment = mo_zosql_test_environment
        iv_table_name             = mv_table_name.

    ASSIGN mr_table_data_ref->* TO <lt_table_this>.
    ASSIGN lo_clone_table_iterator->mr_table_data_ref->* TO <lt_table_clone>.
    <lt_table_clone> = <lt_table_this>.

    ro_copy_of_object = lo_clone_table_iterator.

    lt_records_to_delete = it_records_to_delete.
    SORT lt_records_to_delete DESCENDING.

    LOOP AT lt_records_to_delete ASSIGNING <ls_record_to_delete>.
      ro_copy_of_object->delete_record_by_unique_id( <ls_record_to_delete>-record_unique_id ).
    ENDLOOP.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~CREATE_EMPTY_RECORD_AS_REF.
    FIELD-SYMBOLS: <lt_table_data>   TYPE INDEX TABLE.

    ASSIGN mr_table_data_ref->* TO <lt_table_data>.
    CREATE DATA rd_ref_to_empty_record LIKE LINE OF <lt_table_data>.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~DELETE_RECORD_BY_UNIQUE_ID.
    DATA: lv_record_index  TYPE i.

    FIELD-SYMBOLS: <lt_table_data> TYPE STANDARD TABLE.

    lv_record_index = iv_record_unique_id.

    ASSIGN mr_table_data_ref->* TO <lt_table_data>.

    IF mv_num_lines >= lv_record_index.
      DELETE <lt_table_data> INDEX lv_record_index.
    ENDIF.

    IF mv_current_record > lv_record_index.
      mv_current_record = mv_current_record - 1.
    ENDIF.

    mv_num_lines = mv_num_lines - 1.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~GET_CURRENT_RECORD_REF.
    FIELD-SYMBOLS: <lt_table_data>   TYPE INDEX TABLE,
                   <ls_current_line> TYPE any,
                   <ls_copy_of_line> TYPE any.

    IF mv_current_record <= mv_num_lines.
      ASSIGN mr_table_data_ref->* TO <lt_table_data>.
      rd_ref_to_data_of_current_rec = zif_zosql_iterator~create_empty_record_as_ref( ).
      READ TABLE <lt_table_data> INDEX mv_current_record ASSIGNING <ls_current_line>.
      IF sy-subrc = 0.
        ASSIGN rd_ref_to_data_of_current_rec->* TO <ls_copy_of_line>.
        <ls_copy_of_line> = <ls_current_line>.
      ENDIF.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~GET_CURRENT_RECORD_UNIQUE_ID.
    rv_unique_id = |{ mv_current_record }|.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~IS_EMPTY.
    IF mv_num_lines = 0.
      rv_is_empty = abap_true.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~MOVE_TO_FIRST.
    IF mv_num_lines > 0.
      mv_current_record = 1.
      rv_successful_move = abap_true.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~MOVE_TO_NEXT.
    IF mv_current_record < mv_num_lines.
      mv_current_record = mv_current_record + 1.
      rv_successful_move = abap_true.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~RECORD_IS_LAST.
    IF mv_current_record = mv_num_lines.
      rv_is_last = abap_true.
    ENDIF.
  endmethod.


  method _COUNT_NUM_LINES.

    FIELD-SYMBOLS: <lt_table_data> TYPE ANY TABLE.

    mv_num_lines = 0.

    IF mr_table_data_ref IS BOUND.
      ASSIGN mr_table_data_ref->* TO <lt_table_data>.
      IF sy-subrc = 0.
        mv_num_lines = LINES( <lt_table_data> ).
      ENDIF.
    ENDIF.
  endmethod.
ENDCLASS.
