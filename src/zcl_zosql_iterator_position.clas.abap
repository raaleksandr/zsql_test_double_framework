class ZCL_ZOSQL_ITERATOR_POSITION definition
  public
  create public .

public section.

  types:
    BEGIN OF TY_DATA_SET,
           dataset_name TYPE string,
           dataset_alias TYPE string,
         END OF ty_data_set .
  types:
    ty_data_sets TYPE STANDARD TABLE OF ty_data_set WITH DEFAULT KEY .

  methods GET_DATA_SET_LIST
    returning
      value(RT_DATA_SET_LIST) type TY_DATA_SETS .
  methods GET_FIRST_DATA_SET
    returning
      value(RS_DATA_SET) type TY_DATA_SET .
  methods GET_LINE_FOR_DATA_SET_REF
    importing
      !IV_DATASET_NAME_OR_ALIAS type CLIKE
    returning
      value(RD_REF_TO_LINE) type ref to DATA .
  methods GET_LINE_FIRST_DATA_SET_REF
    returning
      value(RD_REF_TO_LINE) type ref to DATA .
  methods GET_FIELD_REF_OF_DATA_SET
    importing
      !IV_DATASET_NAME_OR_ALIAS type CLIKE
      !IV_FIELDNAME type CLIKE
    returning
      value(RD_REF_TO_FIELD_VALUE) type ref to DATA .
  methods ADD_DATA_SET_DATA
    importing
      !IV_DATASET_NAME type CLIKE optional
      !IV_DATASET_ALIAS type CLIKE optional
      !ID_REF_TO_CURRENT_LINE type ref to DATA .
  methods CHECK_DATA_SET_EXISTS
    importing
      !IV_DATASET_NAME_OR_ALIAS type CLIKE
    returning
      value(RV_EXISTS) type ABAP_BOOL .
protected section.
private section.

  types:
    BEGIN OF ty_data_set_data .
           INCLUDE TYPE ty_data_set.
           TYPES: ref_to_data TYPE REF TO data,
         END OF ty_data_set_data .

  data MT_DATA_SETS_DATA  TYPE TABLE OF ty_data_set_data.
ENDCLASS.



CLASS ZCL_ZOSQL_ITERATOR_POSITION IMPLEMENTATION.


  method ADD_DATA_SET_DATA.

    FIELD-SYMBOLS: <ls_data_set_data> LIKE LINE OF mt_data_sets_data.

    READ TABLE mt_data_sets_data WITH KEY dataset_name  = iv_dataset_name
                                          dataset_alias = iv_dataset_alias
                                          ASSIGNING <ls_data_set_data>.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO mt_data_sets_data ASSIGNING <ls_data_set_data>.
      <ls_data_set_data>-dataset_name  = iv_dataset_name.
      <ls_data_set_data>-dataset_alias = iv_dataset_alias.
    ENDIF.

    <ls_data_set_data>-ref_to_data = id_ref_to_current_line.
  endmethod.


  method CHECK_DATA_SET_EXISTS.

    DATA: lv_dataset_name_or_alias_ucase TYPE string.

    lv_dataset_name_or_alias_ucase = zcl_zosql_utils=>to_upper_case( iv_dataset_name_or_alias ).
    READ TABLE mt_data_sets_data WITH KEY dataset_name = lv_dataset_name_or_alias_ucase
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      READ TABLE mt_data_sets_data WITH KEY dataset_alias = lv_dataset_name_or_alias_ucase
        TRANSPORTING NO FIELDS.
    ENDIF.

    IF sy-subrc = 0.
      rv_exists = abap_true.
    ENDIF.
  endmethod.


  method GET_DATA_SET_LIST.

    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = mt_data_sets_data
                                               IMPORTING et_table_dest = rt_data_set_list ).

  endmethod.


  method GET_FIELD_REF_OF_DATA_SET.
    DATA: ld_ref_to_line TYPE REF TO data,
          lv_fieldname   TYPE string.

    FIELD-SYMBOLS: <ls_line>  TYPE any,
                   <lv_value> TYPE any.

    ld_ref_to_line = get_line_for_data_set_ref( iv_dataset_name_or_alias ).
    ASSIGN ld_ref_to_line->* TO <ls_line>.

    lv_fieldname = zcl_zosql_utils=>to_upper_case( iv_fieldname ).
    CONDENSE lv_fieldname.

    ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_line> TO <lv_value>.
    IF sy-subrc = 0.
      GET REFERENCE OF <lv_value> INTO rd_ref_to_field_value.
    ENDIF.
  endmethod.


  method GET_FIRST_DATA_SET.
    DATA: lt_data_sets TYPE TY_DATA_SETS.

    lt_data_sets = get_data_set_list( ).
    READ TABLE lt_data_sets INDEX 1 INTO rs_data_set.
  endmethod.


  method GET_LINE_FIRST_DATA_SET_REF.

    FIELD-SYMBOLS: <ls_first_data_set_data> LIKE LINE OF mt_data_sets_data.

    READ TABLE mt_data_sets_data INDEX 1 ASSIGNING <ls_first_data_set_data>.
    IF sy-subrc = 0.
      rd_ref_to_line = get_line_for_data_set_ref( <ls_first_data_set_data>-dataset_name ).
    ENDIF.
  endmethod.


  method GET_LINE_FOR_DATA_SET_REF.

    DATA: lv_dataset_name_or_alias_ucase TYPE string.

    FIELD-SYMBOLS: <ls_data_set_data> LIKE LINE OF mt_data_sets_data,
                   <ls_data_set_line> TYPE any.

    lv_dataset_name_or_alias_ucase = zcl_zosql_utils=>to_upper_case( iv_dataset_name_or_alias ).

    READ TABLE mt_data_sets_data WITH KEY dataset_name = lv_dataset_name_or_alias_ucase ASSIGNING <ls_data_set_data>.
    IF sy-subrc <> 0.
      READ TABLE mt_data_sets_data WITH KEY dataset_alias = lv_dataset_name_or_alias_ucase ASSIGNING <ls_data_set_data>.
    ENDIF.

    IF <ls_data_set_data> IS ASSIGNED.
      rd_ref_to_line = <ls_data_set_data>-ref_to_data.
    ENDIF.
  endmethod.
ENDCLASS.
