class ZCL_ZOSQL_JOIN_COND_PARSER definition
  public
  inheriting from ZCL_ZOSQL_SQLCOND_PARSER
  create public .

public section.

  methods ZIF_ZOSQL_SQLCOND_PARSER~GET_PARSER_INSTANCE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZOSQL_JOIN_COND_PARSER IMPLEMENTATION.


  method ZIF_ZOSQL_SQLCOND_PARSER~GET_PARSER_INSTANCE.
    CREATE OBJECT ro_parser TYPE zcl_zosql_join_cond_parser.
  endmethod.
ENDCLASS.
