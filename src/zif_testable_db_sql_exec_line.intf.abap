interface ZIF_TESTABLE_DB_SQL_EXEC_LINE
  public .


  methods EXECUTE_SQL_FOR_LINE
    importing
      !IO_ITERATOR_POSITION type ref to ZCL_TESTABLE_DB_SQLTAB_ITERPOS
    raising
      ZCX_TESTABLE_DB_LAYER .
endinterface.
