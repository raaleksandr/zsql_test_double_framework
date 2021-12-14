class ZCL_ZOSQL_JOIN_PROCESSOR definition
  public
  inheriting from ZCL_ZOSQL_EXPRESSION_PROCESSOR
  create public .

public section.

  methods ZIF_ZOSQL_EXPRESSION_PROCESSOR~CREATE_NEW_INSTANCE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZOSQL_JOIN_PROCESSOR IMPLEMENTATION.


  method ZIF_ZOSQL_EXPRESSION_PROCESSOR~CREATE_NEW_INSTANCE.
    CREATE OBJECT ro_processor TYPE zcl_zosql_join_processor.
  endmethod.
ENDCLASS.
