interface ZIF_ZOSQL_SQLCOND_PARSER
  public .


  methods PARSE_CONDITION
    importing
      !IV_SQL_CONDITION type CLIKE
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC optional
      value(IV_ID_OF_NODE_TO_PARSE) type I optional .
  methods GET_PARSER_INSTANCE
    returning
      value(RO_PARSER) type ref to ZIF_ZOSQL_SQLCOND_PARSER .
  methods CHECK_CONDITION_FOR_CUR_REC
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RV_CONDITION_TRUE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
endinterface.
