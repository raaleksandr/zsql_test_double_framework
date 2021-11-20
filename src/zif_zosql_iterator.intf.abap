interface ZIF_ZOSQL_ITERATOR
  public .


  methods MOVE_TO_FIRST
    returning
      value(RV_SUCCESSFUL_MOVE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods MOVE_TO_NEXT
    returning
      value(RV_SUCCESSFUL_MOVE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods RECORD_IS_LAST
    returning
      value(RV_IS_LAST) type ABAP_BOOL .
  methods IS_EMPTY
    returning
      value(RV_IS_EMPTY) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods GET_CURRENT_RECORD_REF
    returning
      value(RD_REF_TO_DATA_OF_CURRENT_REC) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods CREATE_EMPTY_RECORD_AS_REF
    returning
      value(RD_REF_TO_EMPTY_RECORD) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods GET_CURRENT_RECORD_UNIQUE_ID
    returning
      value(RV_UNIQUE_ID) type ZOSQL_HASH
    raising
      ZCX_ZOSQL_ERROR .
  methods CLONE
    importing
      !IT_RECORDS_TO_DELETE type ZOSQL_DB_REC_UNIQUE_ID_TAB optional
    returning
      value(RO_COPY_OF_OBJECT) type ref to ZIF_ZOSQL_ITERATOR .
  methods DELETE_RECORD_BY_UNIQUE_ID
    importing
      value(IV_RECORD_UNIQUE_ID) type ZOSQL_HASH .
endinterface.
