interface ZIF_ZOSQL_DB_LAYER
  public .


  methods FETCH_NEXT_CURSOR
    importing
      !IV_CURSOR type CURSOR
      value(IV_PACKAGE_SIZE) type I optional
    exporting
      !ED_RESULT_AS_TABLE type ref to DATA
      value(EV_SUBRC) type SYSUBRC
    raising
      ZCX_ZOSQL_ERROR .
  methods FETCH_NEXT_CURSOR_TO_ITAB
    importing
      !IV_CURSOR type CURSOR
      value(IV_DO_INTO_CORRESPONDING) type ABAP_BOOL default ABAP_TRUE
      value(IV_PACKAGE_SIZE) type I optional
    exporting
      !ET_RESULT_TABLE type ANY TABLE
      value(EV_SUBRC) type SYSUBRC
    raising
      ZCX_ZOSQL_ERROR .
  methods OPEN_CURSOR
    importing
      !IV_SELECT type CLIKE
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS optional
      !IT_FOR_ALL_ENTRIES_TABLE type ANY TABLE optional
    returning
      value(RV_CURSOR) type CURSOR
    raising
      ZCX_ZOSQL_ERROR .
  methods SELECT
    importing
      !IV_SELECT type CLIKE
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS optional
      !IT_FOR_ALL_ENTRIES_TABLE type ANY TABLE optional
    exporting
      !ED_RESULT_AS_TABLE type ref to DATA
      !EV_SUBRC type SYSUBRC
    raising
      ZCX_ZOSQL_ERROR .
  methods SELECT_TO_ITAB
    importing
      !IV_SELECT type CLIKE
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS optional
      !IT_FOR_ALL_ENTRIES_TABLE type ANY TABLE optional
      value(IV_DO_INTO_CORRESPONDING) type ABAP_BOOL default ABAP_TRUE
    exporting
      !ET_RESULT_TABLE type ANY TABLE
      !ES_RESULT_LINE type ANY
      !EV_SUBRC type SYSUBRC
    raising
      ZCX_ZOSQL_ERROR .
  methods INSERT_BY_ITAB
    importing
      !IV_TABLE_NAME type CLIKE optional
      !IT_NEW_LINES type ANY TABLE
      value(IV_ACCEPTING_DUPLICATE_KEYS) type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_SUBRC) type SYSUBRC
    raising
      ZCX_ZOSQL_ERROR .
  methods UPDATE_BY_ITAB
    importing
      !IV_TABLE_NAME type CLIKE optional
      !IT_LINES_FOR_UPDATE type ANY TABLE
    returning
      value(RV_SUBRC) type SYSUBRC
    raising
      ZCX_ZOSQL_ERROR .
  methods MODIFY_BY_ITAB
    importing
      !IV_TABLE_NAME type CLIKE optional
      !IT_LINES_FOR_MODIFY type ANY TABLE
    raising
      ZCX_ZOSQL_ERROR .
  methods DELETE_BY_ITAB
    importing
      !IV_TABLE_NAME type CLIKE optional
      !IT_LINES_FOR_DELETE type ANY TABLE
    returning
      value(RV_SUBRC) type SYSUBRC
    raising
      ZCX_ZOSQL_ERROR .
  methods UPDATE
    importing
      !IV_UPDATE_STATEMENT type CLIKE
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS optional
    returning
      value(RV_SUBRC) type SYSUBRC
    raising
      ZCX_ZOSQL_ERROR .
  methods DELETE
    importing
      !IV_DELETE_STATEMENT type CLIKE
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS optional
    returning
      value(RV_SUBRC) type SYSUBRC
    raising
      ZCX_ZOSQL_ERROR .
  methods COMMIT
    importing
      value(IV_WAIT) type ABAP_BOOL default ABAP_TRUE .
endinterface.
