interface ZIF_ZOSQL_SQL_EXEC_LINE
  public .


  methods EXECUTE_SQL_FOR_LINE
    importing
      !IO_ITERATOR_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    raising
      ZCX_ZOSQL_ERROR .
  methods GET_SUBRC
    returning
      value(RV_SUBRC) type SYSUBRC .
endinterface.
