interface ZIF_ZOSQL_FACTORY
  public .


  data TEST_MODE type ABAP_BOOL .

  methods GET_TEST_ENVIRONMENT
    returning
      value(RO_ZOSQL_TEST_ENVIRONMENT) type ref to ZIF_ZOSQL_TEST_ENVIRONMENT .
  methods GET_DATABASE_LAYER
    returning
      value(RO_DATABASE_LAYER) type ref to ZIF_ZOSQL_DB_LAYER .
  methods TEST_MODE_ON .
  methods TEST_MODE_OFF .
  methods GET_ONE_VIRTUAL_TABLE
    importing
      value(IV_TABLE_NAME) type CLIKE
    returning
      value(RO_VIRTUAL_DB_ONE_TABLE) type ref to ZCL_ZOSQL_ONE_VIRT_TABLE .
endinterface.
