interface ZIF_ZOSQL_TEST_ENVIRONMENT
  public .


  methods CLEAR_ONE_TABLE
    importing
      !IV_TABLE_NAME type CLIKE .
  methods DELETE_TEST_DATA_FROM_ITAB
    importing
      !IT_LINES_FOR_DELETE type ANY TABLE
      !IV_TABLE_NAME type CLIKE optional
    returning
      value(RV_SUBRC) type SYSUBRC
    raising
      ZCX_ZOSQL_ERROR .
  methods INSERT_TEST_DATA
    importing
      !IT_TABLE type ANY TABLE
      value(IV_TABLE_NAME) type CLIKE optional
    raising
      ZCX_ZOSQL_ERROR .
  methods INSERT_TEST_DATA_LINE
    importing
      !IS_LINE_AS_STRUCT type ANY
      value(IV_TABLE_NAME) type CLIKE optional
    raising
      ZCX_ZOSQL_ERROR .
  methods CLEAR_ALL .
  methods CLEAR_DOUBLES .
  methods GET_DATA_OF_TABLE
    importing
      !IV_TABLE_NAME type CLIKE
    exporting
      !ET_TABLE type ANY TABLE .
  methods GET_DB_LAYER_FOR_UNIT_TESTS
    returning
      value(RO_DB_LAYER_FOR_UNIT_TESTS) type ref to ZIF_ZOSQL_DB_LAYER .
  methods GET_DATA_OF_TABLE_AS_REF
    importing
      !IV_TABLE_NAME type CLIKE
    returning
      value(RD_REF_TO_DATA) type ref to DATA .
endinterface.
