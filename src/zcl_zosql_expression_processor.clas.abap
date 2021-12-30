class ZCL_ZOSQL_EXPRESSION_PROCESSOR definition
  public
  inheriting from ZCL_ZOSQL_PROCESSOR_BASE
  abstract
  create public .

public section.

  interfaces ZIF_ZOSQL_EXPRESSION_PROCESSOR .

  types:
    BEGIN OF TY_DATA_SET,
           data_set_name   TYPE string,
           data_set_alias  TYPE string,
           data_set_fields TYPE fieldname_table,
         END OF ty_data_set .
  types:
    ty_data_sets TYPE STANDARD TABLE OF ty_data_set WITH KEY data_set_name .
protected section.

  types:
    BEGIN OF TY_block,
           processor TYPE REF TO zif_zosql_expression_processor,
         END OF ty_block .
  types:
    ty_blocks TYPE STANDARD TABLE OF ty_block WITH DEFAULT KEY .

  data M_OPERATION type STRING .
  data M_DATASET_NAME_OR_ALIAS_LEFT type STRING .
  data M_FIELDNAME_LEFT type FIELDNAME .
  data M_DATASET_NAME_OR_ALIAS_RIGHT type STRING .
  data M_FIELDNAME_RIGHT_OR_VALUE type STRING .
  data MT_OR_CONDITIONS type TY_BLOCKS .
  data MT_AND_CONDITIONS type TY_BLOCKS .
  data MS_NOT_CONDITION type TY_BLOCK .
  constants C_IN type STRING value 'IN' ##NO_TEXT.

  methods _GET_REF_TO_RIGHT_OPERAND
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RD_REF_TO_RIGHT_OPERAND) type ref to DATA .
  methods _CLEAR_QUOTES_FROM_VALUE .
  methods _CHECK_ELEMENTARY
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RV_CONDITIONS_TRUE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _PARSE_ELEMENTARY
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IV_ID_OF_NODE_ELEMENTARY_COND type I .
private section.

  data M_EMPTY_CONDITION_FLAG type ABAP_BOOL .
  constants C_NOT type STRING value 'NOT' ##NO_TEXT.

  methods _PARSE_LOGICAL_CONDITION
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IV_PARENT_NODE_ID_OF_CONDITION type I
    raising
      ZCX_ZOSQL_ERROR .
  methods _CONVERT_LIKE_TO_CP
    importing
      !IA_MASK_FOR_LIKE type ANY
    returning
      value(RV_MASK_FOR_CP) type STRING .
  methods _GET_REF_TO_CONDITION_OPERAND
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
      !IV_DATASET_NAME_OR_ALIAS type STRING
      !IV_FIELDNAME_OR_VALUE type CLIKE
    returning
      value(RD_REF_TO_OPERAND) type ref to DATA .
  methods _GET_REF_TO_LEFT_OPERAND
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RD_REF_TO_LEFT_OPERAND) type ref to DATA .
ENDCLASS.



CLASS ZCL_ZOSQL_EXPRESSION_PROCESSOR IMPLEMENTATION.


  method ZIF_ZOSQL_EXPRESSION_PROCESSOR~CHECK_CONDITION_FOR_CUR_REC.
    FIELD-SYMBOLS: <ls_condition> LIKE LINE OF mt_or_conditions.

    IF m_empty_condition_flag = abap_true.
      rv_condition_true = abap_true.
      RETURN.
    ENDIF.

    LOOP AT mt_or_conditions ASSIGNING <ls_condition>.
      IF <ls_condition>-processor->check_condition_for_cur_rec( io_iteration_position ) = abap_true.
        rv_condition_true = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    rv_condition_true = abap_true.
    LOOP AT mt_and_conditions ASSIGNING <ls_condition>.
      IF <ls_condition>-processor->check_condition_for_cur_rec( io_iteration_position ) <> abap_true.
        rv_condition_true = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    IF ms_not_condition-processor IS BOUND.
      rv_condition_true =
        zcl_zosql_utils=>boolean_not(
          ms_not_condition-processor->check_condition_for_cur_rec( io_iteration_position ) ).
      RETURN.
    ENDIF.

    rv_condition_true = _check_elementary( io_iteration_position ).
  endmethod.


  METHOD zif_zosql_expression_processor~initialize_by_parsed_sql.

    DATA: lt_child_nodes_next_level TYPE zcl_zosql_parser_recurs_desc=>ty_tree,
          ls_first_node             TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_second_node            TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_third_node             TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    IF iv_id_of_node_to_parse IS INITIAL.
      m_empty_condition_flag = abap_true.
      RETURN.
    ENDIF.

    lt_child_nodes_next_level = io_sql_parser->get_child_nodes( iv_id_of_node_to_parse ).

    READ TABLE lt_child_nodes_next_level INDEX 1 INTO ls_first_node.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE lt_child_nodes_next_level INDEX 2 INTO ls_second_node.
    READ TABLE lt_child_nodes_next_level INDEX 3 INTO ls_third_node.

    IF ls_first_node-node_type = 'EXPRESSION_NOT'.

      ms_not_condition-processor = zif_zosql_expression_processor~create_new_instance( ).
      ms_not_condition-processor->initialize_by_parsed_sql( io_sql_parser          = io_sql_parser
                                                            iv_id_of_node_to_parse = ls_first_node-id ).

    ELSEIF lines( lt_child_nodes_next_level ) = 1
        OR ( ls_second_node-node_type = 'CLOSING_BRACKET' AND ls_third_node IS INITIAL ).

      zif_zosql_expression_processor~initialize_by_parsed_sql( io_sql_parser          = io_sql_parser
                                                               iv_id_of_node_to_parse = ls_first_node-id ).

    ELSEIF ls_second_node-node_type = 'EXPRESSION_LOGICAL_OPERATOR'.

      _parse_logical_condition( io_sql_parser                  = io_sql_parser
                                iv_parent_node_id_of_condition = iv_id_of_node_to_parse ).
    ELSE.
      _parse_elementary( io_sql_parser                 = io_sql_parser
                         iv_id_of_node_elementary_cond = iv_id_of_node_to_parse ).
    ENDIF.
  ENDMETHOD.


  METHOD _CHECK_ELEMENTARY.

    DATA: ld_ref_to_left_operand  TYPE REF TO data,
          ld_ref_to_right_operand TYPE REF TO data.

    FIELD-SYMBOLS: <lv_value_of_left_operand>  TYPE any,
                   <lv_value_or_right_operand> TYPE any,
                   <lt_value_as_range>         TYPE STANDARD TABLE.

    ld_ref_to_left_operand = _get_ref_to_left_operand( io_iteration_position ).

    IF ld_ref_to_left_operand IS NOT BOUND.
      RETURN.
    ENDIF.

    ld_ref_to_right_operand = _get_ref_to_right_operand( io_iteration_position ).

    IF ld_ref_to_right_operand IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN ld_ref_to_left_operand->* TO <lv_value_of_left_operand>.
    ASSIGN ld_ref_to_right_operand->* TO <lv_value_or_right_operand>.

    CASE m_operation.
      WHEN 'EQ' OR '='.
        IF <lv_value_of_left_operand> = <lv_value_or_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'GE' OR '>='.
        IF <lv_value_of_left_operand> >= <lv_value_or_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'GT' OR '>'.
        IF <lv_value_of_left_operand> > <lv_value_or_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'LE' OR '<='.
        IF <lv_value_of_left_operand> <= <lv_value_or_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'LT' OR '<'.
        IF <lv_value_of_left_operand> < <lv_value_or_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'NE' OR '<>'.
        IF <lv_value_of_left_operand> <> <lv_value_or_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN c_in.
        IF zcl_zosql_utils=>is_internal_table( <lv_value_or_right_operand> ) = abap_true.
          ASSIGN <lv_value_or_right_operand> TO <lt_value_as_range>.
          IF <lv_value_of_left_operand> IN <lt_value_as_range>.
            rv_conditions_true = abap_true.
          ENDIF.
        ELSE.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'LIKE'.
        IF <lv_value_of_left_operand> CP _convert_like_to_cp( <lv_value_or_right_operand> ).
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN OTHERS.
        MESSAGE e066 WITH m_operation INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDCASE.
  ENDMETHOD.


  method _CLEAR_QUOTES_FROM_VALUE.
    m_fieldname_right_or_value = zcl_zosql_utils=>clear_quotes_from_value( m_fieldname_right_or_value ).
  endmethod.


  method _CONVERT_LIKE_TO_CP.
    rv_mask_for_cp = ia_mask_for_like.
    REPLACE ALL OCCURRENCES OF '%' IN rv_mask_for_cp WITH '*'.
    REPLACE ALL OCCURRENCES OF '_' IN rv_mask_for_cp WITH '+'.
  endmethod.


  METHOD _GET_REF_TO_CONDITION_OPERAND.

    DATA: ls_data_set            TYPE zcl_zosql_iterator_position=>ty_data_set,
          lv_dataset_name        LIKE m_dataset_name_or_alias_left.

    IF iv_dataset_name_or_alias IS INITIAL.
      ls_data_set = io_iteration_position->get_first_data_set( ).
      lv_dataset_name = ls_data_set-dataset_name.
    ELSE.
      lv_dataset_name = iv_dataset_name_or_alias.
    ENDIF.

    rd_ref_to_operand = io_iteration_position->get_field_ref_of_data_set( iv_dataset_name_or_alias = lv_dataset_name
                                                                          iv_fieldname             = iv_fieldname_or_value ).
  ENDMETHOD.


  METHOD _GET_REF_TO_LEFT_OPERAND.
    rd_ref_to_left_operand = _get_ref_to_condition_operand( io_iteration_position = io_iteration_position
                                                            iv_dataset_name_or_alias = m_dataset_name_or_alias_left
                                                            iv_fieldname_or_value    = m_fieldname_left ).
  ENDMETHOD.


  method _GET_REF_TO_RIGHT_OPERAND.

    FIELD-SYMBOLS: <lv_return_value> TYPE any.

    rd_ref_to_right_operand = _get_ref_to_condition_operand( io_iteration_position    = io_iteration_position
                                                             iv_dataset_name_or_alias = m_dataset_name_or_alias_right
                                                             iv_fieldname_or_value    = m_fieldname_right_or_value ).

    IF rd_ref_to_right_operand IS NOT BOUND.
      CREATE DATA rd_ref_to_right_operand LIKE m_fieldname_right_or_value.
      ASSIGN rd_ref_to_right_operand->* TO <lv_return_value>.
      <lv_return_value> = m_fieldname_right_or_value.
    ENDIF.
  endmethod.


  METHOD _PARSE_ELEMENTARY.

    DATA: ls_node_left_operand  TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_operator      TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_right_operand TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          lt_child_nodes        TYPE zcl_zosql_parser_recurs_desc=>ty_tree.

    FIELD-SYMBOLS: <ls_node> TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    ls_node_left_operand = io_sql_parser->get_node_info( iv_id_of_node_elementary_cond ).

    lt_child_nodes = io_sql_parser->get_child_nodes( iv_id_of_node_elementary_cond ).

    LOOP AT lt_child_nodes ASSIGNING <ls_node>.
      CASE <ls_node>-node_type.
        WHEN 'EXPRESSION_COMPARISON_OPERATOR'.
          ls_node_operator = <ls_node>.
        WHEN 'EXPRESSION_RIGHT_PART'.
          ls_node_right_operand = <ls_node>.
      ENDCASE.
    ENDLOOP.

    SPLIT ls_node_left_operand-token AT '~' INTO m_dataset_name_or_alias_left m_fieldname_left.
    IF m_fieldname_left IS INITIAL.
      m_fieldname_left = m_dataset_name_or_alias_left.
      CLEAR m_dataset_name_or_alias_left.
    ENDIF.

    m_operation = ls_node_operator-token_ucase.

    SPLIT ls_node_right_operand-token AT '~' INTO m_dataset_name_or_alias_right m_fieldname_right_or_value.
    IF m_fieldname_right_or_value IS INITIAL.
      m_fieldname_right_or_value = m_dataset_name_or_alias_right.
      CLEAR m_dataset_name_or_alias_right.
    ENDIF.
  ENDMETHOD.


  METHOD _parse_logical_condition.

    DATA: lt_nodes_of_logical_condition TYPE zcl_zosql_parser_recurs_desc=>ty_tree.

    FIELD-SYMBOLS: <ls_node>       TYPE zcl_zosql_parser_recurs_desc=>ty_node,
                   <lt_conditions> LIKE mt_or_conditions,
                   <ls_condition>  LIKE LINE OF mt_or_conditions.

    lt_nodes_of_logical_condition = io_sql_parser->get_child_nodes( iv_parent_node_id_of_condition ).

    READ TABLE lt_nodes_of_logical_condition WITH KEY node_type = 'EXPRESSION_LOGICAL_OPERATOR' ASSIGNING <ls_node>.
    IF sy-subrc <> 0.
      MESSAGE e077 INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    CASE <ls_node>-token_ucase.
      WHEN 'OR'.
        ASSIGN mt_or_conditions TO <lt_conditions>.
      WHEN 'AND'.
        ASSIGN mt_and_conditions TO <lt_conditions>.
    ENDCASE.

    LOOP AT lt_nodes_of_logical_condition ASSIGNING <ls_node>
      WHERE node_type <> 'EXPRESSION_LOGICAL_OPERATOR'.
      APPEND INITIAL LINE TO <lt_conditions> ASSIGNING <ls_condition>.
      <ls_condition>-processor = zif_zosql_expression_processor~create_new_instance( ).
      <ls_condition>-processor->initialize_by_parsed_sql( io_sql_parser          = io_sql_parser
                                                          iv_id_of_node_to_parse = <ls_node>-id ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
