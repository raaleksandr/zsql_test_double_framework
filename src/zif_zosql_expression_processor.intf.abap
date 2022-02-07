interface ZIF_ZOSQL_EXPRESSION_PROCESSOR
  public .


  types:
    BEGIN OF ty_operand,
           dataset_name_or_alias TYPE string,
           fieldname             TYPE fieldname,
           constant_value        TYPE string,
         END OF ty_operand .
  types:
    TY_OPERANDS  TYPE STANDARD TABLE OF ty_operand WITH DEFAULT KEY .

  methods INITIALIZE_BY_PARSED_SQL
    importing
      !IO_PARENT_NODE_OF_EXPR type ref to ZCL_ZOSQL_PARSER_NODE
    raising
      ZCX_ZOSQL_ERROR .
  methods CREATE_NEW_INSTANCE
    returning
      value(RO_PROCESSOR) type ref to ZIF_ZOSQL_EXPRESSION_PROCESSOR .
  methods CHECK_CONDITION_FOR_CUR_REC
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RV_CONDITION_TRUE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods GET_LIST_OF_OPERANDS
    returning
      value(RT_OPERANDS) type TY_OPERANDS .
endinterface.
