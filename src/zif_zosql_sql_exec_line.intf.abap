interface ZIF_ZOSQL_SQL_EXEC_LINE
  public .


  methods EXECUTE_SQL_FOR_LINE
    importing
      !IO_ITERATOR_POSITION type ref to ZCL_ZOSQL_SQLTAB_ITERPOS
    raising
      ZCX_ZOSQL_ERROR .
endinterface.
