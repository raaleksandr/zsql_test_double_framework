interface ZIF_ZOSQL_ITERATOR
  public .


  types:
    BEGIN OF TY_DATA_SET,
           dataset_name TYPE string,
           dataset_alias TYPE string,
         END OF ty_data_set .
  types:
    ty_data_sets TYPE STANDARD TABLE OF ty_data_set WITH DEFAULT KEY .

  methods ADD_ADDITIONAL_DATASET
    importing
      !IV_DATASET_NAME type CLIKE
      !IV_DATASET_ALIAS type CLIKE optional
    raising
      ZCX_ZOSQL_ERROR .
  methods GET_KEY_FIELDS_OF_DATA_SET
    importing
      !IV_DATASET_NAME_OR_ALIAS type CLIKE
    returning
      value(RT_KEY_FIELDS) type FIELDNAME_TABLE .
  methods MOVE_TO_FIRST
    returning
      value(RV_SUCCESSFUL_MOVE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods GET_DATA_SET_LIST
    returning
      value(RT_DATA_SET_LIST) type TY_DATA_SETS .
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
  methods GET_LINE_FOR_DATA_SET_REF
    importing
      !IV_DATASET_NAME_OR_ALIAS type CLIKE
    returning
      value(RD_REF_TO_LINE) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods CREATE_EMPTY_RECORD_AS_REF
    returning
      value(RD_REF_TO_EMPTY_RECORD) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods GET_COMPONENTS_OF_DATA_SET
    importing
      !IV_DATASET_NAME_OR_ALIAS type CLIKE
    returning
      value(RT_DATA_SET_COMPONENTS) type CL_ABAP_STRUCTDESCR=>INCLUDED_VIEW
    raising
      ZCX_ZOSQL_ERROR .
  methods CHECK_DATA_SET_EXISTS
    importing
      !IV_DATASET_NAME_OR_ALIAS type CLIKE
    returning
      value(RV_EXISTS) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods GET_ITERATOR_POSITION_OBJECT
    returning
      value(RO_ITERATOR_POS) type ref to ZCL_ZOSQL_ITERATOR_POSITION
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
