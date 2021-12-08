interface ZIF_ZOSQL_SQL_EXEC_LINE
  public .


  methods EXECUTE_SQL_FOR_LINE
    importing
      !IO_ITERATOR_POSITION type ref to zcl_zosql_iterator_position
    raising
      ZCX_ZOSQL_ERROR .
endinterface.
