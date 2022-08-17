interface ZIF_ZOSQL_STUB
  public .


  methods GET_KEY_FIELDS
    returning
      value(RT_KEY_FIELDS) type FIELDNAME_TABLE .
  methods GET_RECORD_BY_KEY
    importing
      !IS_RECORD type ANY
    returning
      value(RD_RECORD) type ref to DATA .
  methods DELETE
    importing
      !IT_LINES_FOR_DELETE type ANY TABLE
    returning
      value(RV_SUBRC) type SYSUBRC .
  methods GET_DATA
    exporting
      !ET_TABLE type ANY TABLE .
  methods GET_DATA_AS_REF
    returning
      value(RD_REF_TO_DATA) type ref to DATA .
  methods INSERT
    importing
      !IT_TABLE type ANY TABLE
      value(IV_ACCEPTING_DUPLICATE_KEYS) type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_SUBRC) type SYSUBRC
    raising
      ZCX_ZOSQL_ERROR .
  methods MODIFY
    importing
      !IT_TABLE type ANY TABLE .
  methods UPDATE
    importing
      !IT_TABLE type ANY TABLE
    returning
      value(RV_SUBRC) type SYSUBRC .
  methods CLEAR .
endinterface.
