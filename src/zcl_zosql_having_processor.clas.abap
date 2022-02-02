class ZCL_ZOSQL_HAVING_PROCESSOR definition
  public
  inheriting from ZCL_ZOSQL_WHERE_PROCESSOR
  create public .

public section.

  methods ZIF_ZOSQL_EXPRESSION_PROCESSOR~CREATE_NEW_INSTANCE
    redefinition .
protected section.

  methods _GET_REF_TO_LEFT_OPERAND
    redefinition .
  methods _INIT_LEFT_PART
    redefinition .
private section.

  data MV_LEFT_PART_FUNCTION_NAME type STRING .
ENDCLASS.



CLASS ZCL_ZOSQL_HAVING_PROCESSOR IMPLEMENTATION.


  method ZIF_ZOSQL_EXPRESSION_PROCESSOR~CREATE_NEW_INSTANCE.
    CREATE OBJECT ro_processor TYPE zcl_zosql_having_processor
      EXPORTING
        io_parameters             = mo_parameters
        io_zosql_test_environment = mo_zosql_test_environment.
  endmethod.


  METHOD _get_ref_to_left_operand.

    DATA: lo_iteration_position_groupby TYPE REF TO zcl_zosql_groupby_iter_pos,
          ls_data_set                   TYPE zcl_zosql_iterator_position=>ty_data_set,
          ls_operand                    LIKE ms_left_operand.

    IF mv_left_part_function_name IS NOT INITIAL.
      lo_iteration_position_groupby ?= io_iteration_position.

      ls_operand = ms_left_operand.
      IF ls_operand-dataset_name_or_alias IS INITIAL.
        ls_data_set = io_iteration_position->get_first_data_set( ).
        ls_operand-dataset_name_or_alias = ls_data_set-dataset_name.
      ENDIF.

      rd_ref_to_left_operand =
        lo_iteration_position_groupby->get_grouped_value_of_data_set(
          iv_dataset_name_or_alias = ls_operand-dataset_name_or_alias
          iv_fieldname             = ls_operand-fieldname_or_value
          iv_groupby_function      = mv_left_part_function_name ).
    ELSE.
      rd_ref_to_left_operand = super->_get_ref_to_left_operand( io_iteration_position ).
    ENDIF.
  ENDMETHOD.


  method _INIT_LEFT_PART.
    DATA: ls_node_left_part         TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_function_argument TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    ls_node_left_part = io_sql_parser->get_node_info( iv_id_of_node_left_part ).

    IF ls_node_left_part-node_type <> zcl_zosql_parser_recurs_desc=>node_type-function.
      super->_init_left_part( io_sql_parser           = io_sql_parser
                              iv_id_of_node_left_part = iv_id_of_node_left_part ).
      RETURN.
    ENDIF.

    mv_left_part_function_name = ls_node_left_part-token_ucase.

    ls_node_function_argument =
      io_sql_parser->get_child_node_with_type(
        iv_node_id = iv_id_of_node_left_part
        iv_node_type = zcl_zosql_parser_recurs_desc=>node_type-function_argument ).

    ms_left_operand = _convert_sqlfield_to_operand( ls_node_function_argument-token ).
  endmethod.
ENDCLASS.
