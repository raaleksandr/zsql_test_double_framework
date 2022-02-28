class ZCL_ZOSQL_SQL_EXEC_DELETE definition
  public
  create public .

public section.

  interfaces ZIF_ZOSQL_SQL_EXEC_LINE .

  methods CONSTRUCTOR
    importing
      !IV_TABLE_NAME type CLIKE .
  methods GET_BUFFER_WITH_PROCESSED_RECS
    returning
      value(RD_BUFFER_WITH_UPDATED_RECS) type ref to DATA .
protected section.

  methods UPDATE_REC_BEFORE_SET_TO_BUF
    importing
      !ID_REF_TO_RECORD type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods ADD_LINE_TO_BUFFER
    importing
      !ID_REF_TO_LINE type ref to DATA .
  methods CREATE_BUFFER_FOR_UPDATE
    returning
      value(RD_REF_TO_DATA_BUFFER) type ref to DATA .
private section.

  data MV_TABLE_NAME type TABNAME16 .
  data MD_BUFFER_FOR_UPDATE type ref to DATA .
  data MV_SUBRC type SYSUBRC .
ENDCLASS.



CLASS ZCL_ZOSQL_SQL_EXEC_DELETE IMPLEMENTATION.


  method ADD_LINE_TO_BUFFER.

    FIELD-SYMBOLS: <lt_buffer> TYPE STANDARD TABLE,
                   <ls_line>   TYPE any.

    ASSIGN md_buffer_for_update->* TO <lt_buffer>.
    ASSIGN id_ref_to_line->* TO <ls_line>.

    APPEND <ls_line> TO <lt_buffer>.
  endmethod.


  method CONSTRUCTOR.
    mv_table_name        = iv_table_name.
    create_buffer_for_update( ).
  endmethod.


  method CREATE_BUFFER_FOR_UPDATE.
    CREATE DATA md_buffer_for_update TYPE STANDARD TABLE OF (mv_table_name).
  endmethod.


  method GET_BUFFER_WITH_PROCESSED_RECS.
    rd_buffer_with_updated_recs = md_buffer_for_update.
  endmethod.


  method UPDATE_REC_BEFORE_SET_TO_BUF.

  endmethod.


  method ZIF_ZOSQL_SQL_EXEC_LINE~EXECUTE_SQL_FOR_LINE.
    DATA: ld_ref_to_line_for_update TYPE REF TO data.

    ld_ref_to_line_for_update = io_iterator_position->get_line_first_data_set_ref( ).
    update_rec_before_set_to_buf( ld_ref_to_line_for_update ).
    add_line_to_buffer( id_ref_to_line   = ld_ref_to_line_for_update ).
  endmethod.
ENDCLASS.
