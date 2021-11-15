class ZCL_TESTABLE_DB_SQL_EXEC_UPDAT definition
  public
  inheriting from ZCL_TESTABLE_DB_SQL_EXEC_DELET
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_TABLE_NAME type CLIKE
      !IO_SET_IN_UPDATE_SQL type ref to ZCL_TESTABLE_DB_SET_PARSER .
protected section.

  methods UPDATE_REC_BEFORE_SET_TO_BUF
    redefinition .
private section.

  data MO_SET_IN_UPDATE_SQL type ref to ZCL_TESTABLE_DB_SET_PARSER .
ENDCLASS.



CLASS ZCL_TESTABLE_DB_SQL_EXEC_UPDAT IMPLEMENTATION.


  method CONSTRUCTOR.
    super->constructor( iv_table_name ).
    mo_set_in_update_sql = io_set_in_update_sql.
  endmethod.


  method UPDATE_REC_BEFORE_SET_TO_BUF.
    mo_set_in_update_sql->update_record( id_ref_to_record ).
  endmethod.
ENDCLASS.
