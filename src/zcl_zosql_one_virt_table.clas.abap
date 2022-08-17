class ZCL_ZOSQL_ONE_VIRT_TABLE definition
  public
  create public .

public section.

  interfaces ZIF_ZOSQL_STUB .

  data M_TABLE_NAME type TABNAME16 read-only .
  data COUNT_INSERTED type I .
  data COUNT_UPDATED type I .
  data COUNT_DELETED type I .

  methods CONSTRUCTOR
    importing
      !IV_TABLE_NAME type CLIKE .
protected section.
private section.

  types TY_OPERATION type CHAR1 .

  data M_MANDANT_FIELD_NAME type FIELDNAME .
  data M_VIRTUAL_TABLE_BUFFER_REF type ref to DATA .
  constants:
    BEGIN OF OPERATION,
               insert TYPE ty_operation VALUE 'I',
               update TYPE ty_operation VALUE 'U',
               delete TYPE ty_operation VALUE 'D',
             END OF operation .

  methods _DELETE_ONE_REC
    importing
      !IS_RECORD type ANY
    returning
      value(RV_SUBRC) type SYSUBRC .
  methods _GET_RECORD_PREPARED_FOR_BUF
    importing
      !IS_RECORD type ANY
    returning
      value(RD_RECORD_PREPARED) type ref to DATA .
  methods _INSERT_ONE_REC
    importing
      !IS_RECORD type ANY
    returning
      value(RV_SUBRC) type SYSUBRC .
  methods _UPDATE_ONE_REC
    importing
      !IS_RECORD type ANY
    returning
      value(RV_SUBRC) type SYSUBRC .
  methods _PERFORM_INS_UPD_DEL_OPERATION
    importing
      !IS_RECORD type ANY
      !IV_OPERATION type TY_OPERATION
    returning
      value(RV_SUBRC) type SYSUBRC .
  methods _RAISE_IF_ITAB_HAS_DUPL_KEYS
    importing
      !IT_TABLE type ANY TABLE
    raising
      ZCX_ZOSQL_ERROR .
  methods _RECORD_ALREADY_EXISTS
    importing
      !IS_RECORD type ANY
    returning
      value(RV_EXISTS) type ABAP_BOOL .
  methods _FILL_MANDANT_FIELD
    changing
      !CS_STRUCT type ANY .
  methods _FIND_MANDANT_FIELD .
  methods _GET_FIELD_LIST
    returning
      value(RT_FIELD_LIST) type DDFIELDS .
  methods _CREATE_INTERNAL_HASH_TABLE .
ENDCLASS.



CLASS ZCL_ZOSQL_ONE_VIRT_TABLE IMPLEMENTATION.


  method CONSTRUCTOR.
    m_table_name = iv_table_name.

    _find_mandant_field( ).
    _create_internal_hash_table( ).
  endmethod.


  method ZIF_ZOSQL_STUB~CLEAR.
    FIELD-SYMBOLS: <lt_table_buffer> TYPE HASHED TABLE.

    ASSIGN m_virtual_table_buffer_ref->* TO <lt_table_buffer>.
    CLEAR <lt_table_buffer>.
  endmethod.


  method ZIF_ZOSQL_STUB~DELETE.

    FIELD-SYMBOLS: <ls_line> TYPE any.

    rv_subrc = 4.

    LOOP AT it_lines_for_delete ASSIGNING <ls_line>.
      IF _record_already_exists( <ls_line> ) = abap_true.
        IF _delete_one_rec( <ls_line> ) = 0.
          rv_subrc = 0.
        ENDIF.
      ENDIF.
    ENDLOOP.
  endmethod.


  method ZIF_ZOSQL_STUB~GET_DATA.
    FIELD-SYMBOLS: <lt_table_buffer> TYPE HASHED TABLE.

    REFRESH et_table.

    ASSIGN m_virtual_table_buffer_ref->* TO <lt_table_buffer>.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_table_buffer>
                                               IMPORTING et_table_dest = et_table ).
  endmethod.


  method ZIF_ZOSQL_STUB~GET_DATA_AS_REF.
    FIELD-SYMBOLS: <lt_table_buffer> TYPE HASHED TABLE,
                   <lt_table_copy>   TYPE STANDARD TABLE.

    ASSIGN m_virtual_table_buffer_ref->* TO <lt_table_buffer>.

    CREATE DATA rd_ref_to_data TYPE TABLE OF (m_table_name).
    ASSIGN rd_ref_to_data->* TO <lt_table_copy>.

    <lt_table_copy> = <lt_table_buffer>.
  endmethod.


  method ZIF_ZOSQL_STUB~GET_KEY_FIELDS.
    DATA: lt_all_fields TYPE ddfields.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_all_fields.

    lt_all_fields = _get_field_list( ).

    LOOP AT lt_all_fields ASSIGNING <ls_field>
      WHERE keyflag = abap_true.

      APPEND <ls_field>-fieldname TO rt_key_fields.
    ENDLOOP.
  endmethod.


  METHOD zif_zosql_stub~get_record_by_key.

    DATA: ld_record_prepared TYPE REF TO data.

    FIELD-SYMBOLS: <lt_buffer>            TYPE HASHED TABLE,
                   <ls_record_prepared>   TYPE any,
                   <ls_found_record>      TYPE any,
                   <ls_record_for_return> TYPE any.

    ld_record_prepared = _get_record_prepared_for_buf( is_record ).
    ASSIGN ld_record_prepared->* TO <ls_record_prepared>.
    ASSIGN m_virtual_table_buffer_ref->* TO <lt_buffer>.

    READ TABLE <lt_buffer> FROM <ls_record_prepared> ASSIGNING <ls_found_record>.
    IF sy-subrc = 0.
      CREATE DATA rd_record LIKE <ls_found_record>.
      ASSIGN rd_record->* TO <ls_record_for_return>.
      <ls_record_for_return> = <ls_found_record>.
    ENDIF.
  ENDMETHOD.


  method ZIF_ZOSQL_STUB~INSERT.

    FIELD-SYMBOLS: <ls_line> TYPE any.

    IF iv_accepting_duplicate_keys <> abap_true.
      _raise_if_itab_has_dupl_keys( it_table ).
    ENDIF.

    rv_subrc = 4.

    LOOP AT it_table ASSIGNING <ls_line>.

      IF _record_already_exists( <ls_line> ) <> abap_true.
        IF _insert_one_rec( <ls_line> ) = 0.
          rv_subrc = 0.
        ENDIF.
      ENDIF.
    ENDLOOP.
  endmethod.


  method ZIF_ZOSQL_STUB~MODIFY.
    FIELD-SYMBOLS: <ls_line> TYPE any.

    LOOP AT it_table ASSIGNING <ls_line>.

      IF _record_already_exists( <ls_line> ) = abap_true.
        _update_one_rec( <ls_line> ).
      ELSE.
        _insert_one_rec( <ls_line> ).
      ENDIF.
    ENDLOOP.
  endmethod.


  method ZIF_ZOSQL_STUB~UPDATE.
    FIELD-SYMBOLS: <ls_line> TYPE any.

    rv_subrc = 4.

    LOOP AT it_table ASSIGNING <ls_line>.

      IF _record_already_exists( <ls_line> ) = abap_true.
        IF _update_one_rec( <ls_line> ) = 0.
          rv_subrc = 0.
        ENDIF.
      ENDIF.
    ENDLOOP.
  endmethod.


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


  method _DELETE_ONE_REC.
    rv_subrc = _perform_ins_upd_del_operation( is_record    = is_record
                                               iv_operation = operation-delete ).
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


  method _GET_RECORD_PREPARED_FOR_BUF.

    FIELD-SYMBOLS: <ls_record_prepared> TYPE any.

    CREATE DATA rd_record_prepared TYPE (m_table_name).
    ASSIGN rd_record_prepared->* TO <ls_record_prepared>.
    MOVE-CORRESPONDING is_record TO <ls_record_prepared>.

    _fill_mandant_field( CHANGING cs_struct = <ls_record_prepared> ).
  endmethod.


  method _INSERT_ONE_REC.
    rv_subrc = _perform_ins_upd_del_operation( is_record    = is_record
                                               iv_operation = operation-insert ).
  endmethod.


  method _PERFORM_INS_UPD_DEL_OPERATION.

    DATA: ld_record_prepared TYPE REF TO data.

    FIELD-SYMBOLS: <lt_buffer>          TYPE HASHED TABLE,
                   <ls_record_prepared> TYPE any.

    ld_record_prepared = _get_record_prepared_for_buf( is_record ).
    ASSIGN ld_record_prepared->* TO <ls_record_prepared>.

    ASSIGN m_virtual_table_buffer_ref->* TO <lt_buffer>.

    CASE iv_operation.
      WHEN operation-insert.
        INSERT <ls_record_prepared> INTO TABLE <lt_buffer>.
      WHEN operation-update.
        MODIFY TABLE <lt_buffer> FROM <ls_record_prepared>.
      WHEN operation-delete.
        DELETE TABLE <lt_buffer> FROM <ls_record_prepared>.
    ENDCASE.
  endmethod.


  method _RAISE_IF_ITAB_HAS_DUPL_KEYS.
    DATA: ld_duplicate_keys_buffer TYPE REF TO data,
          ld_line                  TYPE REF TO data.

    FIELD-SYMBOLS: <ls_line>                  TYPE any,
                   <lt_buffer>                TYPE HASHED TABLE,
                   <lt_duplicate_keys_buffer> TYPE HASHED TABLE,
                   <ls_new_line>              TYPE any.

    ASSIGN m_virtual_table_buffer_ref->* TO <lt_buffer>.
    CREATE DATA ld_duplicate_keys_buffer LIKE <lt_buffer>.
    ASSIGN ld_duplicate_keys_buffer->* TO <lt_duplicate_keys_buffer>.

    CREATE DATA ld_line LIKE LINE OF <lt_duplicate_keys_buffer>.
    ASSIGN ld_line->* TO <ls_new_line>.

    LOOP AT it_table ASSIGNING <ls_line>.

      CLEAR <ls_new_line>.
      MOVE-CORRESPONDING <ls_line> TO <ls_new_line>.

      _fill_mandant_field( CHANGING cs_struct =  <ls_new_line> ).

      READ TABLE <lt_duplicate_keys_buffer> FROM <ls_new_line> TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        MESSAGE e101 INTO zcl_zosqL_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.

      INSERT <Ls_new_line> INTO TABLE <lt_duplicate_keys_buffer>.
    ENDLOOP.
  endmethod.


  method _RECORD_ALREADY_EXISTS.

    DATA: ld_record_prepared TYPE REF TO data.

    FIELD-SYMBOLS: <ls_record_prepared> TYPE any,
                   <lt_buffer>          TYPE HASHED TABLE.

    ld_record_prepared = _get_record_prepared_for_buf( is_record ).
    ASSIGN ld_record_prepared->* TO <ls_record_prepared>.

    ASSIGN m_virtual_table_buffer_ref->* TO <lt_buffer>.
    READ TABLE <lt_buffer> FROM <ls_record_prepared> TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      rv_exists = abap_true.
    ENDIF.
  endmethod.


  method _UPDATE_ONE_REC.
    rv_subrc = _perform_ins_upd_del_operation( is_record    = is_record
                                               iv_operation = operation-update ).
  endmethod.
ENDCLASS.
