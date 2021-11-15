class ZCL_TESTABLE_DB_LAYER_UNITBASE definition
  public
  create public .

public section.
protected section.

  methods CLEAR_MANDANT_FIELD
    changing
      !CT_INTERNAL_TABLE type ANY TABLE .
private section.
ENDCLASS.



CLASS ZCL_TESTABLE_DB_LAYER_UNITBASE IMPLEMENTATION.


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
ENDCLASS.
