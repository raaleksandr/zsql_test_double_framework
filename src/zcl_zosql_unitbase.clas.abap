class ZCL_ZOSQL_UNITBASE definition
  public
  create public .

public section.
protected section.

  methods CLEAR_MANDANT_FIELD
    changing
      !CT_INTERNAL_TABLE type ANY TABLE .
  methods INSERT_TEST_DATA
    importing
      !IT_TABLE type ANY TABLE
      value(IV_TABLE_NAME) type CLIKE optional
    raising
      ZCX_ZOSQL_ERROR .
private section.
ENDCLASS.



CLASS ZCL_ZOSQL_UNITBASE IMPLEMENTATION.


  method CLEAR_MANDANT_FIELD.
    CONSTANTS: lc_mandant_fieldname TYPE fieldname VALUE 'MANDT'.

    FIELD-SYMBOLS: <ls_table_line> TYPE any,
                   <lv_mandant>    TYPE mandt.

    LOOP AT ct_internal_table ASSIGNING <ls_table_line>.
      ASSIGN COMPONENT lc_mandant_fieldname OF STRUCTURE <ls_table_line> TO <lv_mandant>.
      IF sy-subrc = 0.
        CLEAR <lv_mandant>.
      ENDIF.
    ENDLOOP.
  endmethod.


  method INSERT_TEST_DATA.
    MESSAGE e083 INTO zcl_zosql_utils=>dummy.
    zcl_zosql_utils=>raise_exception_from_sy_msg( ).
  endmethod.
ENDCLASS.
