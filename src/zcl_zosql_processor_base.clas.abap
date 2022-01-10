class ZCL_ZOSQL_PROCESSOR_BASE definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE .
protected section.

  data MV_NEW_SYNTAX type ABAP_BOOL .

  methods DELETE_HOST_VARIABLE_SYMBOL
    importing
      !IV_VARIABLE_IN_SQL type CLIKE
    returning
      value(RV_VARIABLE_IN_SQL) type STRING .
private section.
ENDCLASS.



CLASS ZCL_ZOSQL_PROCESSOR_BASE IMPLEMENTATION.


  method CONSTRUCTOR.
    mv_new_syntax = iv_new_syntax.
  endmethod.


  method DELETE_HOST_VARIABLE_SYMBOL.

    IF iv_variable_in_sql IS INITIAL.
      RETURN.
    ENDIF.

    rv_variable_in_sql = iv_variable_in_sql.
    IF rv_variable_in_sql(1) = '@'.
      rv_variable_in_sql = rv_variable_in_sql+1.
    ENDIF.
  endmethod.
ENDCLASS.
