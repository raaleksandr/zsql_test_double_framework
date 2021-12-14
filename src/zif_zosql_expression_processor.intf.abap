interface ZIF_ZOSQL_EXPRESSION_PROCESSOR
  public .


  methods INITIALIZE_BY_PARSED_SQL
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC optional
      value(IV_ID_OF_NODE_TO_PARSE) type I optional .
  methods CREATE_NEW_INSTANCE
    returning
      value(RO_PROCESSOR) type ref to ZIF_ZOSQL_EXPRESSION_PROCESSOR .
  methods CHECK_CONDITION_FOR_CUR_REC
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RV_CONDITION_TRUE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
endinterface.
