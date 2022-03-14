class ZCL_ZOSQL_ONE_VIRT_TABLE definition
  public
  create public .

public section.

  data M_TABLE_NAME type TABNAME16 read-only .
  data COUNT_INSERTED type I .
  data COUNT_UPDATED type I .
  data COUNT_DELETED type I .

  methods GET_KEY_FIELDS
    returning
      value(RT_KEY_FIELDS) type FIELDNAME_TABLE .
  methods DELETE_TEST_DATA_FROM_ITAB
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
  methods CONSTRUCTOR
    importing
      !IV_TABLE_NAME type CLIKE .
  methods INSERT_TEST_DATA_FROM_ITAB
    importing
      !IT_TABLE type ANY TABLE .
  methods CLEAR .
protected section.
private section.

  data M_MANDANT_FIELD_NAME type FIELDNAME .
  data M_VIRTUAL_TABLE_BUFFER_REF type ref to DATA .

  methods _MODIFY_BUFFERLINE_FROM_STRUCT
    importing
      !IS_RECORD type ANY
      !IV_DELETE type ABAP_BOOL
    returning
      value(RV_SUBRC) type SYSUBRC .
  methods _FILL_MANDANT_FIELD
    changing
      !CS_STRUCT type ANY .
  methods _PERFORM_UPD_DEL_OPERATION
    importing
      !IT_TABLE type ANY TABLE
      !IV_DELETE type ABAP_BOOL
    returning
      value(RV_SUBRC) type SYSUBRC .
  methods _FIND_MANDANT_FIELD .
  methods _GET_FIELD_LIST
    returning
      value(RT_FIELD_LIST) type DDFIELDS .
  methods _CREATE_INTERNAL_HASH_TABLE .
ENDCLASS.



CLASS ZCL_ZOSQL_ONE_VIRT_TABLE IMPLEMENTATION.


  method CLEAR.

    FIELD-SYMBOLS: <lt_table_buffer> TYPE HASHED TABLE.

    ASSIGN m_virtual_table_buffer_ref->* TO <lt_table_buffer>.
    CLEAR <lt_table_buffer>.
  endmethod.


  method CONSTRUCTOR.
    m_table_name = iv_table_name.

    _find_mandant_field( ).
    _create_internal_hash_table( ).
  endmethod.


  method DELETE_TEST_DATA_FROM_ITAB.

    rv_subrc = _perform_upd_del_operation( it_table  = it_lines_for_delete
                                           iv_delete = abap_true ).
  endmethod.


  method GET_DATA.

    FIELD-SYMBOLS: <lt_table_buffer> TYPE HASHED TABLE.

    REFRESH et_table.

    ASSIGN m_virtual_table_buffer_ref->* TO <lt_table_buffer>.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_table_buffer>
                                               IMPORTING et_table_dest = et_table ).
  endmethod.


  method GET_DATA_AS_REF.

    FIELD-SYMBOLS: <lt_table_buffer> TYPE HASHED TABLE,
                   <lt_table_copy>   TYPE STANDARD TABLE.

    ASSIGN m_virtual_table_buffer_ref->* TO <lt_table_buffer>.

    CREATE DATA rd_ref_to_data TYPE TABLE OF (m_table_name).
    ASSIGN rd_ref_to_data->* TO <lt_table_copy>.

    <lt_table_copy> = <lt_table_buffer>.
  endmethod.


  METHOD get_key_fields.

    DATA: lt_all_fields TYPE ddfields.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_all_fields.

    lt_all_fields = _get_field_list( ).

    LOOP AT lt_all_fields ASSIGNING <ls_field>
      WHERE keyflag = abap_true.

      APPEND <ls_field>-fieldname TO rt_key_fields.
    ENDLOOP.
  ENDMETHOD.


  METHOD INSERT_TEST_DATA_FROM_ITAB.

    _perform_upd_del_operation( it_table  = it_table
                                iv_delete = abap_false ).
  ENDMETHOD.


  method _CREATE_INTERNAL_HASH_TABLE.

    DATA: lt_field_list TYPE ddfields,
          lo_line_type  TYPE REF TO cl_abap_structdescr,
          lo_hashed_table TYPE REF TO cl_abap_tabledescr,
          lt_key          TYPE abap_keydescr_tab.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_field_list.

    lo_line_type ?= cl_abap_structdescr=>describe_by_name( m_table_name ).

    lt_field_list = _get_field_list( ).

    LOOP AT lt_field_list ASSIGNING <ls_field>
      WHERE keyflag = abap_true.

      APPEND <ls_field>-fieldname TO lt_key.
    ENDLOOP.

    lo_hashed_table = cl_abap_tabledescr=>create( p_line_type  = lo_line_type
                                                  p_table_kind = cl_abap_tabledescr=>tablekind_hashed
                                                  p_unique     = abap_true
                                                  p_key        = lt_key ).

    CREATE DATA m_virtual_table_buffer_ref TYPE HANDLE lo_hashed_table.
  endmethod.


  method _FILL_MANDANT_FIELD.

    FIELD-SYMBOLS: <lv_value> TYPE any.

    IF m_mandant_field_name IS NOT INITIAL.
      ASSIGN COMPONENT m_mandant_field_name OF STRUCTURE cs_struct TO <lv_value>.
      IF sy-subrc = 0.
        <lv_value> = sy-mandt.
      ENDIF.
    ENDIF.
  endmethod.


  METHOD _FIND_MANDANT_FIELD.

    DATA: lt_field_list   TYPE ddfields.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_field_list.

    lt_field_list = _get_field_list( ).

    LOOP AT lt_field_list ASSIGNING <ls_field>
      WHERE datatype = 'CLNT'.

      m_mandant_field_name = <ls_field>-fieldname.
      EXIT.
    ENDLOOP.
  ENDMETHOD.


  method _GET_FIELD_LIST.

    DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr.

    lo_struct_descr ?= cl_abap_structdescr=>describe_by_name( m_table_name ).
    rt_field_list = lo_struct_descr->get_ddic_field_list( ).
  endmethod.


  METHOD _MODIFY_BUFFERLINE_FROM_STRUCT.

    FIELD-SYMBOLS: <lt_table_buffer> TYPE HASHED TABLE.

    ASSIGN m_virtual_table_buffer_ref->* TO <lt_table_buffer>.

    IF iv_delete = abap_true.
      DELETE TABLE <lt_table_buffer> FROM is_record.
      count_deleted = count_deleted + 1.
    ELSE.
      READ TABLE <lt_table_buffer> FROM is_record TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        MODIFY TABLE <lt_table_buffer> FROM is_record.
        count_updated = count_updated + 1.
      ELSE.
        INSERT is_record INTO TABLE <lt_table_buffer>.
        count_inserted = count_inserted + 1.
      ENDIF.
    ENDIF.

    rv_subrc = sy-subrc.
  ENDMETHOD.


  METHOD _PERFORM_UPD_DEL_OPERATION.
    DATA: ld_table_line TYPE REF TO data.

    FIELD-SYMBOLS: <ls_table_line> TYPE any,
                   <ls_param_line> TYPE any.

    CREATE DATA ld_table_line TYPE (m_table_name).
    ASSIGN ld_table_line->* TO <ls_table_line>.

    rv_subrc = 4.

    LOOP AT it_table ASSIGNING <ls_param_line>.
      CLEAR <ls_table_line>.
      MOVE-CORRESPONDING <ls_param_line> TO <ls_table_line>.

      _fill_mandant_field( CHANGING cs_struct = <ls_table_line> ).

      IF _modify_bufferline_from_struct( is_record = <ls_table_line>
                                         iv_delete = iv_delete ) = 0.

        rv_subrc = 0.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
