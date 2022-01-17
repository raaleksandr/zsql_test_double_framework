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

  methods CONSTRUCTOR
    importing
      !IO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE .
  methods IS_PARAMETER_COMPARED_AS_RANGE
    importing
      !IV_PARAMETER_NAME_IN_SELECT type ZOSQL_PARAM_NAME_IN_SELECT
    returning
      value(RV_IS_COMPARED_AS_RANGE) type ABAP_BOOL .
protected section.

  types:
    BEGIN OF TY_block,
           processor TYPE REF TO zif_zosql_expression_processor,
         END OF ty_block .
  types:
    ty_blocks TYPE STANDARD TABLE OF ty_block WITH DEFAULT KEY .
  types:
    BEGIN OF TY_FIELD,
           dataset_name_or_alias  TYPE string,
           fieldname_or_value     TYPE string,
           parameter_info         TYPE ZOSQL_DB_LAYER_PARAM,
         END OF ty_field .

  data MS_LEFT_OPERAND type TY_FIELD .
  data MV_OPERATION type STRING .
  data MS_RIGHT_OPERAND type TY_FIELD .
  data MS_BETWEEN_LOWER_BOUND type TY_FIELD .
  data MS_BETWEEN_UPPER_BOUND type TY_FIELD .
  data MT_OR_CONDITIONS type TY_BLOCKS .
  data MT_AND_CONDITIONS type TY_BLOCKS .
  data MS_NOT_CONDITION type TY_BLOCK .
  constants C_IN type STRING value 'IN' ##NO_TEXT.
  data MO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS .

  methods _GET_REF_TO_RIGHT_OPERAND
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RD_REF_TO_RIGHT_OPERAND) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods _CLEAR_QUOTES_FROM_VALUE .
  methods _CHECK_ELEMENTARY
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RV_CONDITIONS_TRUE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _GET_REF_TO_LEFT_OPERAND
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RD_REF_TO_LEFT_OPERAND) type ref to DATA .
  methods _PROCESS_ELEMENTARY
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IV_ID_OF_NODE_ELEMENTARY_COND type I .
private section.

  data MV_NOT_FLAG type ABAP_BOOL .
  data MV_ESCAPE_SYMBOL_FOR_LIKE type CHAR1 .
  data MV_EMPTY_CONDITION_FLAG type ABAP_BOOL .
  constants C_NOT type STRING value 'NOT' ##NO_TEXT.
  constants C_BETWEEN type STRING value 'BETWEEN' ##NO_TEXT.
  constants C_LIKE type STRING value 'LIKE' ##NO_TEXT.

  methods _CHECK_LIKE
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RV_CONDITIONS_TRUE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _PROCESS_LIKE
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      value(IV_ID_OF_NODE_ELEMENTARY_COND) type I .
  methods _TRY_TO_GET_PARAMETER_NAME
    importing
      !IS_OPERAND type TY_FIELD
    returning
      value(RV_PARAMETER_NAME) type STRING .
  methods _CHECK_BETWEEN
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RV_CONDITIONS_TRUE) type ABAP_BOOL .
  methods _CHECK_WITH_COMPARE_OPERATOR
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RV_CONDITIONS_TRUE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _CONVERT_SQLFIELD_TO_OPERAND
    importing
      !IV_FIELDNAME_IN_SQL type CLIKE
    returning
      value(RS_FIELD) type TY_FIELD .
  methods _PROCESS_BETWEEN
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IV_ID_OF_NODE_ELEMENTARY_COND type I .
  methods _RAISE_IF_CANNOT_COMPARE_VALS
    importing
      !I_FIRST_VALUE type ANY
      !I_SECOND_VALUE type ANY
    raising
      ZCX_ZOSQL_ERROR .
  methods _PROCESS_LOGICAL_CONDITION
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
  methods _CHECK_IF_OPERAND_IS_PARAMETER
    importing
      !IS_OPERAND type TY_FIELD
    returning
      value(RV_IS_PARAMETER) type ABAP_BOOL .
  methods _GET_REF_TO_OPERAND
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
      !IS_OPERAND type TY_FIELD
    returning
      value(RD_REF_TO_OPERAND) type ref to DATA .
  methods _GET_REF_TO_OPERAND_DATASET
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
      !IS_OPERAND type TY_FIELD
    returning
      value(RD_REF_TO_OPERAND) type ref to DATA .
ENDCLASS.



CLASS ZCL_ZOSQL_EXPRESSION_PROCESSOR IMPLEMENTATION.


  method CONSTRUCTOR.
    super->constructor( iv_new_syntax ).
    mo_parameters = io_parameters.
  endmethod.


  METHOD IS_PARAMETER_COMPARED_AS_RANGE.

    DATA: lo_where_parser TYPE REF TO zcl_zosql_where_processor.

    FIELD-SYMBOLS: <ls_condition> LIKE LINE OF mt_or_conditions.

    LOOP AT mt_or_conditions ASSIGNING <ls_condition>.
      lo_where_parser ?= <ls_condition>-processor.
      IF lo_where_parser->is_parameter_compared_as_range( iv_parameter_name_in_select ) = abap_true.
        rv_is_compared_as_range = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    LOOP AT mt_and_conditions ASSIGNING <ls_condition>.
      lo_where_parser ?= <ls_condition>-processor.
      IF lo_where_parser->is_parameter_compared_as_range( iv_parameter_name_in_select ) = abap_true.
        rv_is_compared_as_range = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    IF ms_not_condition-processor IS BOUND.
      lo_where_parser ?= ms_not_condition-processor.
      IF lo_where_parser->is_parameter_compared_as_range( iv_parameter_name_in_select ) = abap_true.
        rv_is_compared_as_range = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF _check_if_operand_is_parameter( ms_right_operand ) = abap_true
      AND mv_operation = c_in
      AND ms_right_operand-parameter_info-param_name_in_select = iv_parameter_name_in_select.

      rv_is_compared_as_range = abap_true.
    ENDIF.
  ENDMETHOD.


  method ZIF_ZOSQL_EXPRESSION_PROCESSOR~CHECK_CONDITION_FOR_CUR_REC.
    FIELD-SYMBOLS: <ls_condition> LIKE LINE OF mt_or_conditions.

    IF mv_empty_condition_flag = abap_true.
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


  METHOD zif_zosql_expression_processor~get_list_of_operands.

    DATA: lt_operands LIKE rt_operands.

    FIELD-SYMBOLS: <ls_condition> LIKE LINE OF mt_or_conditions,
                   <ls_operand>   LIKE LINE OF rt_operands.

    IF mv_empty_condition_flag = abap_true.
      RETURN.
    ENDIF.

    DO 1 TIMES.
      LOOP AT mt_or_conditions ASSIGNING <ls_condition>.
        lt_operands = <ls_condition>-processor->get_list_of_operands( ).
        APPEND LINES OF lt_operands TO rt_operands.
      ENDLOOP.

      CHECK sy-subrc <> 0.

      LOOP AT mt_and_conditions ASSIGNING <ls_condition>.
        lt_operands = <ls_condition>-processor->get_list_of_operands( ).
        APPEND LINES OF lt_operands TO rt_operands.
      ENDLOOP.

      CHECK sy-subrc <> 0.

      IF ms_not_condition-processor IS BOUND.
        rt_operands = ms_not_condition-processor->get_list_of_operands( ).
      ELSE.
        APPEND INITIAL LINE TO rt_operands ASSIGNING <ls_operand>.
        <ls_operand>-dataset_name_or_alias = ms_left_operand-dataset_name_or_alias.
        <ls_operand>-fieldname             = ms_left_operand-fieldname_or_value.

        APPEND INITIAL LINE TO rt_operands ASSIGNING <ls_operand>.
        <ls_operand>-dataset_name_or_alias =  ms_right_operand-dataset_name_or_alias.

        IF <ls_operand>-dataset_name_or_alias IS NOT INITIAL.
          <ls_operand>-fieldname      = ms_right_operand-fieldname_or_value.
        ELSE.
          <ls_operand>-constant_value = ms_right_operand-fieldname_or_value.
        ENDIF.
      ENDIF.
    ENDDO.

    SORT rt_operands.
    DELETE ADJACENT DUPLICATES FROM rt_operands.
  ENDMETHOD.


  METHOD zif_zosql_expression_processor~initialize_by_parsed_sql.

    DATA: lt_child_nodes_next_level TYPE zcl_zosql_parser_recurs_desc=>ty_tree,
          ls_first_node             TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_second_node            TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_third_node             TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    IF iv_id_of_node_to_parse IS INITIAL.
      mv_empty_condition_flag = abap_true.
      RETURN.
    ENDIF.

    lt_child_nodes_next_level = io_sql_parser->get_child_nodes( iv_id_of_node_to_parse ).

    READ TABLE lt_child_nodes_next_level INDEX 1 INTO ls_first_node.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE lt_child_nodes_next_level INDEX 2 INTO ls_second_node.
    READ TABLE lt_child_nodes_next_level INDEX 3 INTO ls_third_node.

    IF ls_first_node-node_type = zcl_zosql_parser_recurs_desc=>node_type-expression_not
      AND ls_second_node-node_type <> zcl_zosql_parser_recurs_desc=>node_type-expression_between
      AND ls_second_node-node_type <> zcl_zosql_parser_recurs_desc=>node_type-expression_like.

      ms_not_condition-processor = zif_zosql_expression_processor~create_new_instance( ).
      ms_not_condition-processor->initialize_by_parsed_sql( io_sql_parser          = io_sql_parser
                                                            iv_id_of_node_to_parse = ls_first_node-id ).

    ELSEIF lines( lt_child_nodes_next_level ) = 1
        OR ( ls_second_node-node_type = zcl_zosql_parser_recurs_desc=>node_type-closing_bracket AND ls_third_node IS INITIAL ).

      zif_zosql_expression_processor~initialize_by_parsed_sql( io_sql_parser          = io_sql_parser
                                                               iv_id_of_node_to_parse = ls_first_node-id ).

    ELSEIF ls_second_node-node_type = zcl_zosql_parser_recurs_desc=>node_type-expression_logical_operator.

      _process_logical_condition( io_sql_parser                  = io_sql_parser
                                  iv_parent_node_id_of_condition = iv_id_of_node_to_parse ).
    ELSE.
      _process_elementary( io_sql_parser                 = io_sql_parser
                           iv_id_of_node_elementary_cond = iv_id_of_node_to_parse ).
    ENDIF.
  ENDMETHOD.


  method _CHECK_BETWEEN.
    DATA: ld_ref_to_value_to_compare  TYPE REF TO data,
          ld_ref_to_lower_bound       TYPE REF TO data,
          ld_ref_to_upper_bound       TYPE REF TO data.

    FIELD-SYMBOLS: <lv_value_to_compare> TYPE any,
                   <lv_lower_bound>      TYPE any,
                   <lv_upper_bound>      TYPE any.

    ld_ref_to_value_to_compare = _get_ref_to_left_operand( io_iteration_position ).
    ld_ref_to_lower_bound = _get_ref_to_operand( io_iteration_position = io_iteration_position
                                                 is_operand            = ms_between_lower_bound ).
    ld_ref_to_upper_bound = _get_ref_to_operand( io_iteration_position = io_iteration_position
                                                 is_operand            = ms_between_upper_bound ).

    ASSIGN ld_ref_to_value_to_compare->* TO <lv_value_to_compare>.
    CHECK sy-subrc = 0.

    ASSIGN ld_ref_to_lower_bound->* TO <lv_lower_bound>.
    CHECK sy-subrc = 0.

    ASSIGN ld_ref_to_upper_bound->* TO <lv_upper_bound>.
    CHECK sy-subrc = 0.

    IF <lv_value_to_compare> BETWEEN <lv_lower_bound> AND <lv_upper_bound>.
      rv_conditions_true = abap_true.
    ENDIF.
  endmethod.


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

    CASE mv_operation.
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
        rv_conditions_true = _check_like( io_iteration_position ).
      WHEN c_between.
        rv_conditions_true = _check_between( io_iteration_position ).
      WHEN OTHERS.
        rv_conditions_true = _check_with_compare_operator( io_iteration_position ).
    ENDCASE.

    IF mv_not_flag = abap_true.
      rv_conditions_true = zcl_zosql_utils=>boolean_not( rv_conditions_true ).
    ENDIF.
  ENDMETHOD.


  METHOD _CHECK_IF_OPERAND_IS_PARAMETER.

    DATA: lv_parameter_name TYPE string.

    IF is_operand-parameter_info IS NOT INITIAL.
      rv_is_parameter = abap_true.
      RETURN.
    ENDIF.

    lv_parameter_name = _try_to_get_parameter_name( is_operand ).

    IF lv_parameter_name IS NOT INITIAL.
      rv_is_parameter = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD _check_like.

    DATA: ld_ref_to_left_operand  TYPE REF TO data,
          ld_ref_to_right_operand TYPE REF TO data.

    FIELD-SYMBOLS: <lv_value_of_left_operand>  TYPE any,
                   <lv_value_or_right_operand> TYPE any.

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

    IF <lv_value_of_left_operand> CP _convert_like_to_cp( <lv_value_or_right_operand> ).
      rv_conditions_true = abap_true.
    ENDIF.
  ENDMETHOD.


  method _CHECK_WITH_COMPARE_OPERATOR.

    DATA: ld_ref_to_left_operand  TYPE REF TO data,
          ld_ref_to_right_operand TYPE REF TO data.

    FIELD-SYMBOLS: <lv_value_of_left_operand>  TYPE any,
                   <lv_value_of_right_operand> TYPE any.

    ld_ref_to_left_operand = _get_ref_to_left_operand( io_iteration_position ).

    IF ld_ref_to_left_operand IS NOT BOUND.
      RETURN.
    ENDIF.

    ld_ref_to_right_operand = _get_ref_to_right_operand( io_iteration_position ).

    IF ld_ref_to_right_operand IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN ld_ref_to_left_operand->* TO <lv_value_of_left_operand>.
    ASSIGN ld_ref_to_right_operand->* TO <lv_value_of_right_operand>.

    _raise_if_cannot_compare_vals( i_first_value  = <lv_value_of_left_operand>
                                   i_second_value = <lv_value_of_right_operand> ).

    CASE mv_operation.
      WHEN 'EQ' OR '='.
        IF <lv_value_of_left_operand> = <lv_value_of_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'GE' OR '>='.
        IF <lv_value_of_left_operand> >= <lv_value_of_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'GT' OR '>'.
        IF <lv_value_of_left_operand> > <lv_value_of_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'LE' OR '<='.
        IF <lv_value_of_left_operand> <= <lv_value_of_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'LT' OR '<'.
        IF <lv_value_of_left_operand> < <lv_value_of_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'NE' OR '<>'.
        IF <lv_value_of_left_operand> <> <lv_value_of_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN OTHERS.
        MESSAGE e066 WITH mv_operation INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDCASE.
  endmethod.


  method _CLEAR_QUOTES_FROM_VALUE.
    ms_right_operand-fieldname_or_value = zcl_zosql_utils=>clear_quotes_from_value( ms_right_operand-fieldname_or_value ).
  endmethod.


  METHOD _convert_like_to_cp.

    DATA: lv_mask_for_like       TYPE string,
          lv_position            TYPE i,
          lv_current_char        TYPE c LENGTH 1,
          lv_previous_was_escape TYPE abap_bool.

    lv_mask_for_like = ia_mask_for_like.

    lv_position = 0.

    WHILE lv_position < strlen( lv_mask_for_like ).

      lv_current_char = lv_mask_for_like+lv_position(1).

      DO 1 TIMES.
        IF mv_escape_symbol_for_like IS NOT INITIAL
          AND mv_escape_symbol_for_like = lv_current_char
          AND lv_previous_was_escape = abap_false.

          lv_previous_was_escape = abap_true.
          EXIT.
        ENDIF.

        CASE lv_current_char.
          WHEN '%' OR '_'.
            IF lv_previous_was_escape <> abap_true.
              CASE lv_current_char.
                WHEN '%'.
                  lv_current_char = '*'.
                WHEN '_'.
                  lv_current_char = '+'.
              ENDCASE.
            ENDIF.

          WHEN '*' OR '+'.
            CONCATENATE rv_mask_for_cp '#' INTO rv_mask_for_cp.
        ENDCASE.

        CONCATENATE rv_mask_for_cp lv_current_char INTO rv_mask_for_cp.

        lv_previous_was_escape = abap_false.
      ENDDO.

      lv_position = lv_position + 1.
    ENDWHILE.
  ENDMETHOD.


  method _CONVERT_SQLFIELD_TO_OPERAND.

    DATA: lv_parameter_name TYPE string.

    SPLIT iv_fieldname_in_sql AT '~' INTO rs_field-dataset_name_or_alias rs_field-fieldname_or_value.
    IF rs_field-fieldname_or_value IS INITIAL.
      rs_field-fieldname_or_value = rs_field-dataset_name_or_alias.
      CLEAR rs_field-dataset_name_or_alias.
    ENDIF.

    IF _check_if_operand_is_parameter( rs_field ) = abap_true.
      lv_parameter_name = _try_to_get_parameter_name( rs_field ).
      rs_field-parameter_info = mo_parameters->get_parameter_data( lv_parameter_name ).
    ENDIF.
  endmethod.


  METHOD _GET_REF_TO_LEFT_OPERAND.
    rd_ref_to_left_operand = _get_ref_to_operand_dataset( io_iteration_position    = io_iteration_position
                                                          is_operand               = ms_left_operand ).
  ENDMETHOD.


  METHOD _GET_REF_TO_OPERAND.

    FIELD-SYMBOLS: <lv_return_value> TYPE any.

    IF _check_if_operand_is_parameter( is_operand ) = abap_true.
      rd_ref_to_operand = mo_parameters->get_parameter_value_ref( is_operand-parameter_info-param_name_in_select ).
    ENDIF.

    IF rd_ref_to_operand IS BOUND.
      RETURN.
    ENDIF.

    rd_ref_to_operand = _get_ref_to_operand_dataset( io_iteration_position = io_iteration_position
                                                     is_operand            = is_operand ).

    IF rd_ref_to_operand IS BOUND.
      RETURN.
    ENDIF.

    IF rd_ref_to_operand IS NOT BOUND.
      CREATE DATA rd_ref_to_operand LIKE is_operand-fieldname_or_value.
      ASSIGN rd_ref_to_operand->* TO <lv_return_value>.
      <lv_return_value> = is_operand-fieldname_or_value.
    ENDIF.
  ENDMETHOD.


  METHOD _GET_REF_TO_OPERAND_DATASET.

    DATA: ls_data_set TYPE zcl_zosql_iterator_position=>ty_data_set,
          ls_operand  LIKE is_operand.

    ls_operand = is_operand.
    IF ls_operand-dataset_name_or_alias IS INITIAL.
      ls_data_set = io_iteration_position->get_first_data_set( ).
      ls_operand-dataset_name_or_alias = ls_data_set-dataset_name.
    ENDIF.

    rd_ref_to_operand = io_iteration_position->get_field_ref_of_data_set(
      iv_dataset_name_or_alias = ls_operand-dataset_name_or_alias
      iv_fieldname             = ls_operand-fieldname_or_value ).
  ENDMETHOD.


  method _GET_REF_TO_RIGHT_OPERAND.
    rd_ref_to_right_operand = _get_ref_to_operand( io_iteration_position = io_iteration_position
                                                   is_operand            = ms_right_operand ).
  endmethod.


  method _PROCESS_BETWEEN.

    DATA: ls_node_left_operand        TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_not                 TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_between_lower_bound TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_between_upper_bound TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    FIELD-SYMBOLS: <ls_node> TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    ls_node_left_operand = io_sql_parser->get_node_info( iv_id_of_node_elementary_cond ).

    ls_node_not =
      io_sql_parser->get_child_node_with_type(
        iv_node_id   = iv_id_of_node_elementary_cond
        iv_node_type = zcl_zosql_parser_recurs_desc=>node_type-expression_not ).

    IF ls_node_not IS NOT INITIAL.
      mv_not_flag = abap_true.
    ENDIF.

    ls_node_between_lower_bound =
      io_sql_parser->get_child_node_with_type(
        iv_node_id   = ls_node_left_operand-id
        iv_node_type = zcl_zosql_parser_recurs_desc=>node_type-expression_between_lower_bound ).

    ls_node_between_upper_bound =
      io_sql_parser->get_child_node_with_type(
        iv_node_id   = ls_node_left_operand-id
        iv_node_type = zcl_zosql_parser_recurs_desc=>node_type-expression_between_upper_bound ).

    ms_left_operand = _convert_sqlfield_to_operand( ls_node_left_operand-token ).
    mv_operation = c_between.
    ms_between_lower_bound = _convert_sqlfield_to_operand( ls_node_between_lower_bound-token ).
    ms_between_upper_bound = _convert_sqlfield_to_operand( ls_node_between_upper_bound-token ).
  endmethod.


  METHOD _PROCESS_ELEMENTARY.

    DATA: ls_node_left_operand  TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_operator      TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_right_operand TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          lt_child_nodes        TYPE zcl_zosql_parser_recurs_desc=>ty_tree.

    FIELD-SYMBOLS: <ls_node> TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    ls_node_left_operand = io_sql_parser->get_node_info( iv_id_of_node_elementary_cond ).

    lt_child_nodes = io_sql_parser->get_child_nodes( iv_id_of_node_elementary_cond ).

    LOOP AT lt_child_nodes ASSIGNING <ls_node>.
      CASE <ls_node>-node_type.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-expression_not.

          mv_not_flag = abap_true.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-expression_comparison_operator
          OR zcl_zosql_parser_recurs_desc=>node_type-expression_in.

          ls_node_operator = <ls_node>.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-expression_between.
          _process_between( io_sql_parser                 = io_sql_parser
                            iv_id_of_node_elementary_cond = iv_id_of_node_elementary_cond ).
          RETURN.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-expression_like.
          _process_like( io_sql_parser                 = io_sql_parser
                         iv_id_of_node_elementary_cond = iv_id_of_node_elementary_cond ).
          RETURN.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-expression_right_part.
          ls_node_right_operand = <ls_node>.
      ENDCASE.
    ENDLOOP.

    ms_left_operand = _convert_sqlfield_to_operand( ls_node_left_operand-token ).

    mv_operation = ls_node_operator-token_ucase.

    ms_right_operand = _convert_sqlfield_to_operand( ls_node_right_operand-token ).
  ENDMETHOD.


  METHOD _process_like.

    DATA: ls_node_left_operand  TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_escape_symbol TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_right_operand TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    ls_node_left_operand = io_sql_parser->get_node_info( iv_id_of_node_elementary_cond ).
    ms_left_operand = _convert_sqlfield_to_operand( ls_node_left_operand-token ).

    mv_operation = c_like.

    ls_node_right_operand =
      io_sql_parser->get_child_node_with_type(
        iv_node_id   = ls_node_left_operand-id
        iv_node_type = zcl_zosql_parser_recurs_desc=>node_type-expression_right_part ).

    ms_right_operand = _convert_sqlfield_to_operand( ls_node_right_operand-token ).

    ls_node_escape_symbol =
      io_sql_parser->get_child_node_with_type(
        iv_node_id = iv_id_of_node_elementary_cond
        iv_node_type = zcl_zosql_parser_recurs_desc=>node_type-expression_like_escape_symbol ).

    IF ls_node_escape_symbol IS NOT INITIAL.
      mv_escape_symbol_for_like = zcl_zosql_utils=>clear_quotes_from_value( ls_node_escape_symbol-token ).
    ENDIF.
  ENDMETHOD.


  METHOD _PROCESS_LOGICAL_CONDITION.

    DATA: lt_nodes_of_logical_condition TYPE zcl_zosql_parser_recurs_desc=>ty_tree.

    FIELD-SYMBOLS: <ls_node>       TYPE zcl_zosql_parser_recurs_desc=>ty_node,
                   <lt_conditions> LIKE mt_or_conditions,
                   <ls_condition>  LIKE LINE OF mt_or_conditions.

    lt_nodes_of_logical_condition = io_sql_parser->get_child_nodes( iv_parent_node_id_of_condition ).

    READ TABLE lt_nodes_of_logical_condition WITH KEY node_type = zcl_zosql_parser_recurs_desc=>node_type-expression_logical_operator ASSIGNING <ls_node>.
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
      WHERE node_type <> zcl_zosql_parser_recurs_desc=>node_type-expression_logical_operator.

      APPEND INITIAL LINE TO <lt_conditions> ASSIGNING <ls_condition>.
      <ls_condition>-processor = zif_zosql_expression_processor~create_new_instance( ).
      <ls_condition>-processor->initialize_by_parsed_sql( io_sql_parser          = io_sql_parser
                                                          iv_id_of_node_to_parse = <ls_node>-id ).
    ENDLOOP.
  ENDMETHOD.


  method _RAISE_IF_CANNOT_COMPARE_VALS.

    DATA: ld_copy_of_first    TYPE REF TO data,
          ld_copy_of_second   TYPE REF TO data,
          lv_first_as_string  TYPE string,
          lv_second_as_string TYPE string.

    FIELD-SYMBOLS: <lv_copy_of_first>  TYPE any,
                   <lv_copy_of_second> TYPE any.

    CREATE DATA ld_copy_of_first LIKE i_first_value.
    CREATE DATA ld_copy_of_second LIKE i_second_value.
    ASSIGN ld_copy_of_first->* TO <lv_copy_of_first>.
    ASSIGN ld_copy_of_second->* TO <lv_copy_of_second>.

    TRY.
      <lv_copy_of_second> = i_first_value.
      <lv_copy_of_first> = i_second_value.
    CATCH cx_root.
      lv_first_as_string = i_first_value.
      CONDENSE lv_first_as_string.
      lv_second_as_string = i_second_value.
      CONDENSE lv_second_as_string.

      MESSAGE e078 WITH lv_first_as_string lv_second_as_string INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDTRY.
  endmethod.


  method _TRY_TO_GET_PARAMETER_NAME.
    DATA: lv_fieldname_full TYPE string,
          lv_param_without_host_symbol TYPE string.

    IF mo_parameters->check_parameter_exists( is_operand-fieldname_or_value ) = abap_true.
      rv_parameter_name = is_operand-fieldname_or_value.
      RETURN.
    ENDIF.

    IF is_operand-dataset_name_or_alias IS NOT INITIAL.
      CONCATENATE is_operand-dataset_name_or_alias is_operand-fieldname_or_value
        INTO lv_fieldname_full SEPARATED BY '~'.

      IF mo_parameters->check_parameter_exists( lv_fieldname_full ) = abap_true.
        rv_parameter_name = lv_fieldname_full.
        RETURN.
      ENDIF.
    ENDIF.

    IF mv_new_syntax = abap_true.

      lv_param_without_host_symbol = delete_host_variable_symbol( is_operand-fieldname_or_value ).

      IF mo_parameters->check_parameter_exists( lv_param_without_host_symbol ) = abap_true.
        rv_parameter_name = lv_param_without_host_symbol.
      ENDIF.
    ENDIF.
  endmethod.
ENDCLASS.
