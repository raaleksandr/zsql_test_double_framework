class ZCL_ZOSQL_SQL_EXEC_SELEC definition
  public
  create public .

public section.

  interfaces ZIF_ZOSQL_SQL_EXEC_LINE .

  methods CONSTRUCTOR
    importing
      !IO_SELECT type ref to ZCL_ZOSQL_SELECT_PARSER .
protected section.
private section.

  data MO_SELECT type ref to ZCL_ZOSQL_SELECT_PARSER .
ENDCLASS.



CLASS ZCL_ZOSQL_SQL_EXEC_SELEC IMPLEMENTATION.


  method CONSTRUCTOR.
    mo_select = io_select.
  endmethod.


  method ZIF_ZOSQL_SQL_EXEC_LINE~EXECUTE_SQL_FOR_LINE.
    mo_select->add_line_to_result( io_iterator_position ).
  endmethod.
ENDCLASS.
