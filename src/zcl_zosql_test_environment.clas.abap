class ZCL_ZOSQL_TEST_ENVIRONMENT definition
  public
  create public .

public section.

  interfaces ZIF_ZOSQL_TEST_ENVIRONMENT .

  class-methods CREATE
    returning
      value(RO_RESULT) type ref to ZIF_ZOSQL_TEST_ENVIRONMENT .
  class-methods GET_DB_LAYER_FOR_PRODUCTION
    returning
      value(RO_DB_LAYER_FOR_PROD) type ref to ZIF_ZOSQL_DB_LAYER .
  methods CONSTRUCTOR
    importing
      !IO_FACTORY type ref to ZIF_ZOSQL_FACTORY optional .
protected section.
private section.

  types:
    BEGIN OF TY_VIRTUAL_TABLE,
           table_name TYPE tabname16,
           virt_table TYPE REF TO zcl_zosql_one_virt_table,
         END OF ty_virtual_table .

  data MO_FACTORY type ref to ZIF_ZOSQL_FACTORY .
  data:
    MT_VIRTUAL_TABLES  TYPE HASHED TABLE OF ty_virtual_table WITH UNIQUE KEY table_name .

  methods _CREATE_TABLE_RECORD
    importing
      !IV_TABLE_NAME type CLIKE .
  methods _RAISE_CANNOT_DETECT_TABNAME
    raising
      ZCX_ZOSQL_ERROR .
  methods _IS_TRANSPARENT_TABLE
    importing
      !IV_DICTIONARY_TYPE type CLIKE
    returning
      value(RV_IS_TRANSPARENT_TABLE) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_ZOSQL_TEST_ENVIRONMENT IMPLEMENTATION.


  method CONSTRUCTOR.

    IF io_factory IS BOUND.
      mo_factory = io_factory.
    ELSE.
      CREATE OBJECT mo_factory TYPE zcl_zosql_factory.
    ENDIF.
  endmethod.


  method CREATE.
    CREATE OBJECT ro_result TYPE zcl_zosql_test_environment.
  endmethod.


  method GET_DB_LAYER_FOR_PRODUCTION.
    CREATE OBJECT ro_db_layer_for_prod TYPE zcl_zosql_db_layer.
  endmethod.


  method ZIF_ZOSQL_TEST_ENVIRONMENT~CLEAR_ALL.
    REFRESH mt_virtual_tables.
  endmethod.


  method ZIF_ZOSQL_TEST_ENVIRONMENT~CLEAR_DOUBLES.
    zif_zosql_test_environment~clear_all( ).
  endmethod.


  method ZIF_ZOSQL_TEST_ENVIRONMENT~CLEAR_ONE_TABLE.
    DATA: lv_table_name TYPE tabname16.

    lv_table_name = zcl_zosql_utils=>to_upper_case( iv_table_name ).
    DELETE mt_virtual_tables WHERE table_name = lv_table_name.
  endmethod.


  method ZIF_ZOSQL_TEST_ENVIRONMENT~CLEAR_UPDATE_COUNTERS.

    FIELD-SYMBOLS: <ls_virtual_table> LIKE LINE OF mt_virtual_tables.

    CLEAR: zif_zosql_test_environment~count_inserted,
           zif_zosql_test_environment~count_updated,
           zif_zosql_test_environment~count_deleted.

    LOOP AT mt_virtual_tables ASSIGNING <ls_virtual_table>.
      CLEAR: <ls_virtual_table>-virt_table->count_inserted,
             <ls_virtual_table>-virt_table->count_updated,
             <ls_virtual_table>-virt_table->count_deleted.
    ENDLOOP.
  endmethod.


  method ZIF_ZOSQL_TEST_ENVIRONMENT~DELETE_TEST_DATA_FROM_ITAB.
    DATA: lv_table_name    TYPE tabname16.

    FIELD-SYMBOLS: <ls_virtual_table> LIKE LINE OF mt_virtual_tables.

    lv_table_name = iv_table_name.

    IF lv_table_name IS INITIAL.
      lv_table_name = zcl_zosql_utils=>try_to_guess_tabname_by_data( it_lines_for_delete ).
    ENDIF.

    IF lv_table_name IS INITIAL.
      _raise_cannot_detect_tabname( ).
    ENDIF.

    rv_subrc = 4.

    READ TABLE mt_virtual_tables WITH TABLE KEY table_name = lv_table_name ASSIGNING <ls_virtual_table>.
    IF sy-subrc = 0.
      IF <ls_virtual_table>-virt_table->delete_test_data_from_itab( it_lines_for_delete ) = 0.
        rv_subrc = 0.
      ENDIF.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_TEST_ENVIRONMENT~DESTROY.
  endmethod.


  method ZIF_ZOSQL_TEST_ENVIRONMENT~GET_DATA_OF_TABLE.
    FIELD-SYMBOLS: <ls_virtual_table> LIKE LINE OF mt_virtual_tables.

    REFRESH et_table[].
    READ TABLE mt_virtual_tables WITH TABLE KEY table_name = iv_table_name ASSIGNING <ls_virtual_table>.
    IF sy-subrc = 0.
      <ls_virtual_table>-virt_table->get_data( IMPORTING et_table = et_table ).
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_TEST_ENVIRONMENT~GET_DATA_OF_TABLE_AS_REF.
    FIELD-SYMBOLS: <ls_virtual_table> LIKE LINE OF mt_virtual_tables.

    READ TABLE mt_virtual_tables WITH TABLE KEY table_name = iv_table_name ASSIGNING <ls_virtual_table>.
    IF sy-subrc = 0.
      rd_ref_to_data = <ls_virtual_table>-virt_table->get_data_as_ref( ).
    ELSEIF _is_transparent_table( iv_table_name ) = abap_true.
      _create_table_record( iv_table_name ).
      rd_ref_to_data = zif_zosql_test_environment~get_data_of_table_as_ref( iv_table_name ).
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_TEST_ENVIRONMENT~GET_DB_LAYER_FOR_UNIT_TESTS.
    CREATE OBJECT ro_db_layer_for_unit_tests TYPE zcl_zosql_db_layer_fake
      EXPORTING
        io_zosql_test_environment = me.
  endmethod.


  method ZIF_ZOSQL_TEST_ENVIRONMENT~INSERT_TEST_DATA.
    DATA: lv_table_name    TYPE tabname16.

    FIELD-SYMBOLS: <ls_virtual_table> LIKE LINE OF mt_virtual_tables.

    lv_table_name = iv_table_name.

    IF lv_table_name IS INITIAL.
      lv_table_name = zcl_zosql_utils=>try_to_guess_tabname_by_data( it_table ).
    ENDIF.

    IF lv_table_name IS INITIAL.
      _raise_cannot_detect_tabname( ).
    ENDIF.

    READ TABLE mt_virtual_tables WITH TABLE KEY table_name = lv_table_name ASSIGNING <ls_virtual_table>.
    IF sy-subrc <> 0.
      _create_table_record( lv_table_name ).
      READ TABLE mt_virtual_tables WITH TABLE KEY table_name = lv_table_name ASSIGNING <ls_virtual_table>.
    ENDIF.
    <ls_virtual_table>-virt_table->insert_test_data_from_itab( it_table ).
  endmethod.


  method _CREATE_TABLE_RECORD.

    DATA: ls_virtual_table LIKE LINE OF mt_virtual_tables.

    ls_virtual_table-table_name = iv_table_name.
    ls_virtual_table-virt_table = mo_factory->get_one_virtual_table( iv_table_name ).
    INSERT ls_virtual_table INTO TABLE mt_virtual_tables.
  endmethod.


  METHOD _IS_TRANSPARENT_TABLE.
    rv_is_transparent_table = zcl_zosql_utils=>transparent_table_exists( iv_dictionary_type ).
  ENDMETHOD.


  METHOD _RAISE_CANNOT_DETECT_TABNAME.
    MESSAGE e053 INTO zcl_zosql_utils=>dummy.
    zcl_zosql_utils=>raise_exception_from_sy_msg( ).
  ENDMETHOD.
ENDCLASS.
