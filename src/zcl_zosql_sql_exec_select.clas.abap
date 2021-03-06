class ZCL_ZOSQL_SQL_EXEC_SELECT definition
  public
  create public .

public section.

  interfaces ZIF_ZOSQL_SQL_EXEC_LINE .

  methods CONSTRUCTOR
    importing
      !IO_SELECT type ref to ZCL_ZOSQL_SELECT_PROCESSOR .
protected section.
private section.

  data MO_SELECT type ref to ZCL_ZOSQL_SELECT_PROCESSOR .
  data MV_SUBRC type SYSUBRC value 4 ##NO_TEXT.
ENDCLASS.



CLASS ZCL_ZOSQL_SQL_EXEC_SELECT IMPLEMENTATION.


  method CONSTRUCTOR.
    mo_select = io_select.
  endmethod.


  method ZIF_ZOSQL_SQL_EXEC_LINE~EXECUTE_SQL_FOR_LINE.
    mo_select->add_line_to_result( io_iterator_position ).
    mv_subrc = 0.
  endmethod.


  method ZIF_ZOSQL_SQL_EXEC_LINE~GET_SUBRC.
    rv_subrc = mv_subrc.
  endmethod.
ENDCLASS.
