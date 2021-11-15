interface ZIF_TESTABLE_DB_SQLCOND_PARSER
  public .


  methods PARSE_CONDITION
    importing
      !IV_SQL_CONDITION type CLIKE .
  methods GET_PARSER_INSTANCE
    returning
      value(RO_PARSER) type ref to ZIF_TESTABLE_DB_SQLCOND_PARSER .
  methods CHECK_CONDITION_FOR_CUR_REC
    importing
      !IO_ITERATION_POSITION type ref to ZCL_TESTABLE_DB_SQLTAB_ITERPOS
    returning
      value(RV_CONDITION_TRUE) type ABAP_BOOL
    raising
      ZCX_TESTABLE_DB_LAYER .
endinterface.
