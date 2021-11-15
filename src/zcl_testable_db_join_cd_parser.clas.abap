class ZCL_TESTABLE_DB_JOIN_CD_PARSER definition
  public
  inheriting from ZCL_TESTABLE_DB_SQLCOND_PARSER
  create public .

public section.

  methods ZIF_TESTABLE_DB_SQLCOND_PARSER~GET_PARSER_INSTANCE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TESTABLE_DB_JOIN_CD_PARSER IMPLEMENTATION.


  method ZIF_TESTABLE_DB_SQLCOND_PARSER~GET_PARSER_INSTANCE.
    CREATE OBJECT ro_parser TYPE zcl_testable_db_join_cd_parser.
  endmethod.
ENDCLASS.
