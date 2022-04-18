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
  types:
    TY_FIELDS   TYPE STANDARD TABLE OF ty_field WITH KEY dataset_name_or_alias fieldname_or_value .

  data MS_LEFT_OPERAND type TY_FIELD .
  data MV_NOT_FLAG type ABAP_BOOL .
  data MV_OPERATION type STRING .
  data MS_RIGHT_OPERAND type TY_FIELD .
  data MS_BETWEEN_LOWER_BOUND type TY_FIELD .
  data MV_EMPTY_CONDITION_FLAG type ABAP_BOOL .
  data MS_BETWEEN_UPPER_BOUND type TY_FIELD .
  data MT_OR_CONDITIONS type TY_BLOCKS .
  data MT_AND_CONDITIONS type TY_BLOCKS .
  data MS_NOT_CONDITION type TY_BLOCK .
  constants C_IN type STRING value 'IN' ##NO_TEXT.
  data MO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS .

  methods _INIT_LEFT_PART
    importing
      !IO_NODE_LEFT_PART type ref to ZCL_ZOSQL_PARSER_NODE .
  methods _COMPARE_VALUES
    importing
      !IV_VALUE_LEFT type ANY
      !IV_OPERATION type CLIKE
      !IV_VALUE_RIGHT type ANY
    returning
      value(RV_CONDITION_TRUE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _GET_REF_TO_RIGHT_OPERAND
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RD_REF_TO_RIGHT_OPERAND) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods _CHECK_WITH_COMPARE_OPERATOR
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RV_CONDITIONS_TRUE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _CHECK_ELEMENTARY
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
  methods _GET_REF_TO_LEFT_OPERAND
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RD_REF_TO_LEFT_OPERAND) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods _INIT_ELEMENTARY
    importing
      !IO_NODE_ELEMENTARY_CONDITION type ref to ZCL_ZOSQL_PARSER_NODE .
private section.

  data MV_ESCAPE_SYMBOL_FOR_LIKE type CHAR1 .
  data MT_LIST_OF_VALUES_FOR_IN type TY_FIELDS .
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
  methods _CHECK_IN
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RV_CONDITIONS_TRUE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _CONSIDER_NOT_IN_ELEMENTARY
    importing
      !IO_NODE_ELEMENTARY_CONDITION type ref to ZCL_ZOSQL_PARSER_NODE .
  methods _TRY_TO_GET_PARAMETER_NAME
    importing
      !IS_OPERAND type TY_FIELD
    returning
      value(RV_PARAMETER_NAME) type STRING .
  methods _RAISE_IF_RIGHT_OPERAND_RANGE
    importing
      !IV_RIGHT_OPERAND type ANY
    raising
      ZCX_ZOSQL_ERROR .
  methods _INIT_COMPARISON
    importing
      !IO_NODE_ELEMENTARY_CONDITION type ref to ZCL_ZOSQL_PARSER_NODE .
  methods _INIT_IN
    importing
      !IO_NODE_ELEMENTARY_CONDITION type ref to ZCL_ZOSQL_PARSER_NODE .
  methods _INIT_LIKE
    importing
      !IO_NODE_ELEMENTARY_CONDITION type ref to ZCL_ZOSQL_PARSER_NODE .
  methods _NEED_TO_IGNORE_CONDITION
    returning
      value(RV_CONDITION_MUST_BE_IGNORED) type ABAP_BOOL .
  methods _CHECK_BETWEEN
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    returning
      value(RV_CONDITIONS_TRUE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _INIT_BETWEEN
    importing
      !IO_NODE_ELEMENTARY_CONDITION type ref to ZCL_ZOSQL_PARSER_NODE .
  methods _RAISE_IF_CANNOT_COMPARE_VALS
    importing
      !I_FIRST_VALUE type ANY
      !I_SECOND_VALUE type ANY
    raising
      ZCX_ZOSQL_ERROR .
  methods _INIT_LOGICAL_CONDITION
    importing
      !IO_PARENT_NODE_OF_CONDITION type ref to ZCL_ZOSQL_PARSER_NODE
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


  method ZIF_ZOSQL_EXPRESSION_PROCESSOR~CREATE_NEW_INSTANCE.
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

    DATA: lt_child_nodes_next_level TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_first_node             TYPE REF TO zcl_zosql_parser_node,
          lo_second_node            TYPE REF TO zcl_zosql_parser_node,
          lo_third_node             TYPE REF TO zcl_zosql_parser_node,
          lv_second_node_type       TYPE string,
          lv_third_node_type        TYPE string.

    IF io_parent_node_of_expr IS NOT BOUND.
      mv_empty_condition_flag = abap_true.
      RETURN.
    ENDIF.

    lt_child_nodes_next_level = io_parent_node_of_expr->get_child_nodes( ).

    READ TABLE lt_child_nodes_next_level INDEX 1 INTO lo_first_node.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE lt_child_nodes_next_level INDEX 2 INTO lo_second_node.
    IF sy-subrc = 0.
      lv_second_node_type = lo_second_node->node_type.
    ENDIF.

    READ TABLE lt_child_nodes_next_level INDEX 3 INTO lo_third_node.
    IF sy-subrc = 0.
      lv_third_node_type = lo_third_node->node_type.
    ENDIF.

    IF lo_first_node->node_type = zcl_zosql_parser_recurs_desc=>node_type-expression_not
      AND lv_second_node_type <> zcl_zosql_parser_recurs_desc=>node_type-expression_between
      AND lv_second_node_type <> zcl_zosql_parser_recurs_desc=>node_type-expression_like
      AND lv_second_node_type <> zcl_zosql_parser_recurs_desc=>node_type-expression_in.

      ms_not_condition-processor = zif_zosql_expression_processor~create_new_instance( ).
      ms_not_condition-processor->initialize_by_parsed_sql( lo_first_node ).

    ELSEIF lines( lt_child_nodes_next_level ) = 1
        OR ( lv_second_node_type = zcl_zosql_parser_recurs_desc=>node_type-closing_bracket
             AND lo_third_node IS NOT BOUND ).

      zif_zosql_expression_processor~initialize_by_parsed_sql( lo_first_node ).

    ELSEIF lv_second_node_type = zcl_zosql_parser_recurs_desc=>node_type-expression_logical_operator.

      _init_logical_condition( io_parent_node_of_expr ).
    ELSE.
      _init_elementary( io_parent_node_of_expr ).
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


  METHOD _check_elementary.

    IF _need_to_ignore_condition( ) = abap_true.
      rv_conditions_true = abap_true.
      RETURN.
    ENDIF.

    CASE mv_operation.
      WHEN c_in.
        rv_conditions_true = _check_in( io_iteration_position ).
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


  METHOD _check_in.

    DATA: ld_ref_to_left_operand  TYPE REF TO data,
          ld_ref_to_right_operand TYPE REF TO data,
          ld_ref_to_item_value    TYPE REF TO data.

    FIELD-SYMBOLS: <lv_value_of_left_operand>  TYPE any,
                   <lv_value_or_right_operand> TYPE any,
                   <lt_value_as_range>         TYPE STANDARD TABLE,
                   <ls_list_of_value_item>     TYPE ty_field,
                   <lv_value_of_list_item>     TYPE any.

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

    IF zcl_zosql_utils=>is_internal_table( <lv_value_or_right_operand> ) = abap_true.
      ASSIGN <lv_value_or_right_operand> TO <lt_value_as_range>.
      IF <lv_value_of_left_operand> IN <lt_value_as_range>.
        rv_conditions_true = abap_true.
      ENDIF.
    ELSEIF mt_list_of_values_for_in IS NOT INITIAL.

      LOOP AT mt_list_of_values_for_in ASSIGNING <ls_list_of_value_item>.
        ld_ref_to_item_value =
          _get_ref_to_operand( io_iteration_position = io_iteration_position
                               is_operand            = <ls_list_of_value_item> ).

        ASSIGN ld_ref_to_item_value->* TO <lv_value_of_list_item>.
        IF <lv_value_of_left_operand> = <lv_value_of_list_item>.
          rv_conditions_true = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
    ELSE.
      rv_conditions_true = abap_true.
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

    rv_conditions_true = _compare_values( iv_value_left  = <lv_value_of_left_operand>
                                          iv_operation   = mv_operation
                                          iv_value_right = <lv_value_of_right_operand> ).
  endmethod.


  method _COMPARE_VALUES.

    DATA: lv_operation TYPE string.

    _raise_if_right_operand_range( iv_value_right ).

    _raise_if_cannot_compare_vals( i_first_value  = iv_value_left
                                   i_second_value = iv_value_right ).

    lv_operation = zcl_zosql_utils=>to_upper_case( iv_operation ).

    CASE lv_operation.
      WHEN 'EQ' OR '='.
        IF iv_value_left = iv_value_right.
          rv_condition_true = abap_true.
        ENDIF.
      WHEN 'GE' OR '>='.
        IF iv_value_left >= iv_value_right.
          rv_condition_true = abap_true.
        ENDIF.
      WHEN 'GT' OR '>'.
        IF iv_value_left > iv_value_right.
          rv_condition_true = abap_true.
        ENDIF.
      WHEN 'LE' OR '<='.
        IF iv_value_left <= iv_value_right.
          rv_condition_true = abap_true.
        ENDIF.
      WHEN 'LT' OR '<'.
        IF iv_value_left < iv_value_right.
          rv_condition_true = abap_true.
        ENDIF.
      WHEN 'NE' OR '<>'.
        IF iv_value_left <> iv_value_right.
          rv_condition_true = abap_true.
        ENDIF.
      WHEN OTHERS.
        MESSAGE e066 WITH mv_operation INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDCASE.
  endmethod.


  method _CONSIDER_NOT_IN_ELEMENTARY.
    IF io_node_elementary_condition->exists_child_node_with_type(
         zcl_zosql_parser_recurs_desc=>node_type-expression_not ) = abap_true.

      mv_not_flag = abap_true.
    ENDIF.
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

    IF rs_field-dataset_name_or_alias IS INITIAL.
      rs_field-fieldname_or_value =
        zcl_zosql_utils=>clear_quotes_from_value( rs_field-fieldname_or_value ).
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
      iv_dataset_name_or_alias = is_operand-dataset_name_or_alias
      iv_fieldname             = is_operand-fieldname_or_value ).
  ENDMETHOD.


  method _GET_REF_TO_RIGHT_OPERAND.
    rd_ref_to_right_operand = _get_ref_to_operand( io_iteration_position = io_iteration_position
                                                   is_operand            = ms_right_operand ).
  endmethod.


  method _INIT_BETWEEN.

    DATA: lo_node_between_lower_bound TYPE REF TO zcl_zosql_parser_node,
          lo_node_between_upper_bound TYPE REF TO zcl_zosql_parser_node.

    _init_left_part( io_node_elementary_condition ).

    lo_node_between_lower_bound =
      io_node_elementary_condition->get_child_node_with_type(
        zcl_zosql_parser_recurs_desc=>node_type-expression_between_lower_bound ).

    lo_node_between_upper_bound =
      io_node_elementary_condition->get_child_node_with_type(
        zcl_zosql_parser_recurs_desc=>node_type-expression_between_upper_bound ).

    mv_operation = c_between.
    ms_between_lower_bound = _convert_sqlfield_to_operand( lo_node_between_lower_bound->token ).
    ms_between_upper_bound = _convert_sqlfield_to_operand( lo_node_between_upper_bound->token ).
  endmethod.


  METHOD _INIT_COMPARISON.

    DATA: lo_node_right_operand TYPE REF TO zcl_zosql_parser_node.

    _init_left_part( io_node_elementary_condition ).

    mv_operation =
      io_node_elementary_condition->get_child_node_token_with_type(
        iv_node_type = zcl_zosql_parser_recurs_desc=>node_type-expression_comparison_operator
        iv_ucase     = abap_true ).

    lo_node_right_operand =
      io_node_elementary_condition->get_child_node_with_type(
        zcl_zosql_parser_recurs_desc=>node_type-expression_right_part ).

    IF lo_node_right_operand IS BOUND.
      ms_right_operand = _convert_sqlfield_to_operand( lo_node_right_operand->token ).
    ENDIF.
  ENDMETHOD.


  METHOD _INIT_ELEMENTARY.

    IF io_node_elementary_condition->exists_child_node_with_type(
         zcl_zosql_parser_recurs_desc=>node_type-expression_comparison_operator ) = abap_true.

      _init_comparison( io_node_elementary_condition ).
    ELSEIF io_node_elementary_condition->exists_child_node_with_type(
              zcl_zosql_parser_recurs_desc=>node_type-expression_between ) = abap_true.

      _init_between( io_node_elementary_condition ).
    ELSEIF io_node_elementary_condition->exists_child_node_with_type(
              zcl_zosql_parser_recurs_desc=>node_type-expression_like ) = abap_true.

      _init_like( io_node_elementary_condition ).
    ELSEIF io_node_elementary_condition->exists_child_node_with_type(
              zcl_zosql_parser_recurs_desc=>node_type-expression_in ) = abap_true.

      _init_in( io_node_elementary_condition ).
    ENDIF.

    _consider_not_in_elementary( io_node_elementary_condition ).
  ENDMETHOD.


  METHOD _INIT_IN.

    DATA: lo_node_right_operand   TYPE REF TO zcl_zosql_parser_node,
          lt_child_nodes          TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lt_nodes_values_of_list TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          ls_list_item            TYPE ty_field,
          lo_node                 TYPE REF TO zcl_zosql_parser_node.

    FIELD-SYMBOLS: <ls_child_node>      LIKE LINE OF lt_child_nodes,
                   <ls_node_with_value> LIKE LINE OF lt_nodes_values_of_list.

    _init_left_part( io_node_elementary_condition ).

    mv_operation = c_in.

    lt_child_nodes = io_node_elementary_condition->get_child_nodes( ).

    LOOP AT lt_child_nodes INTO lo_node.
      CASE lo_node->node_type.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-expression_right_part.
          lo_node_right_operand = lo_node.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-list_of_vals_value.
          APPEND lo_node TO lt_nodes_values_of_list.
      ENDCASE.
    ENDLOOP.

    IF lt_nodes_values_of_list IS NOT INITIAL.
      LOOP AT lt_nodes_values_of_list INTO lo_node.
        ls_list_item = _convert_sqlfield_to_operand( lo_node->token ).
        APPEND ls_list_item TO mt_list_of_values_for_in.
      ENDLOOP.
    ELSE.
      IF lo_node_right_operand IS BOUND.
        ms_right_operand = _convert_sqlfield_to_operand( lo_node_right_operand->token ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method _INIT_LEFT_PART.
    ms_left_operand = _convert_sqlfield_to_operand( io_node_left_part->token ).
  endmethod.


  METHOD _INIT_LIKE.

    DATA: lo_node_escape_symbol TYPE REF TO zcl_zosql_parser_node,
          lo_node_right_operand TYPE REF TO zcl_zosql_parser_node.

    _init_left_part( io_node_elementary_condition ).

    mv_operation = c_like.

    lo_node_right_operand =
      io_node_elementary_condition->get_child_node_with_type(
        zcl_zosql_parser_recurs_desc=>node_type-expression_right_part ).

    ms_right_operand = _convert_sqlfield_to_operand( lo_node_right_operand->token ).

    lo_node_escape_symbol =
      io_node_elementary_condition->get_child_node_with_type(
        zcl_zosql_parser_recurs_desc=>node_type-expression_like_escape_symbol ).

    IF lo_node_escape_symbol IS NOT INITIAL.
      mv_escape_symbol_for_like = zcl_zosql_utils=>clear_quotes_from_value( lo_node_escape_symbol->token ).
    ENDIF.
  ENDMETHOD.


  METHOD _INIT_LOGICAL_CONDITION.

    DATA: lo_node_logical_operator      TYPE REF TO zcl_zosql_parser_node,
          lt_nodes_of_logical_condition TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_node_operand               TYPE REF TO zcl_zosql_parser_node.

    FIELD-SYMBOLS: <ls_node>       TYPE zcl_zosql_parser_recurs_desc=>ty_node,
                   <lt_conditions> LIKE mt_or_conditions,
                   <ls_condition>  LIKE LINE OF mt_or_conditions.

    lo_node_logical_operator =
      io_parent_node_of_condition->get_child_node_with_type(
        zcl_zosql_parser_recurs_desc=>node_type-expression_logical_operator ).

    IF lo_node_logical_operator IS NOT BOUND.
      MESSAGE e077 INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    CASE lo_node_logical_operator->token_ucase.
      WHEN 'OR'.
        ASSIGN mt_or_conditions TO <lt_conditions>.
      WHEN 'AND'.
        ASSIGN mt_and_conditions TO <lt_conditions>.
    ENDCASE.

    lt_nodes_of_logical_condition = io_parent_node_of_condition->get_child_nodes( ).

    LOOP AT lt_nodes_of_logical_condition INTO lo_node_operand
      WHERE table_line->node_type <> zcl_zosql_parser_recurs_desc=>node_type-expression_logical_operator.

      APPEND INITIAL LINE TO <lt_conditions> ASSIGNING <ls_condition>.
      <ls_condition>-processor = zif_zosql_expression_processor~create_new_instance( ).
      <ls_condition>-processor->initialize_by_parsed_sql( lo_node_operand ).
    ENDLOOP.
  ENDMETHOD.


  method _NEED_TO_IGNORE_CONDITION.

    DATA: lv_parameter_name TYPE string.

    IF _check_if_operand_is_parameter( ms_right_operand ) <> abap_true.
      RETURN.
    ENDIF.

    lv_parameter_name = _try_to_get_parameter_name( ms_right_operand ).
    IF lv_parameter_name IS INITIAL.
      RETURN.
    ENDIF.

    IF mo_parameters->if_parameter_must_be_ignored( lv_parameter_name ) = abap_true.
      rv_condition_must_be_ignored = abap_true.
    ENDIF.
  endmethod.


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

    IF zcl_zosql_utils=>is_internal_table( <lv_copy_of_second> ) = abap_true
      AND zcl_zosql_utils=>is_internal_table( i_first_value ) <> abap_true.

      MESSAGE e085 WITH i_first_value INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    TRY.
      <lv_copy_of_second> = i_first_value.
      <lv_copy_of_first>  = i_second_value.
    CATCH cx_root.
      lv_first_as_string = i_first_value.
      CONDENSE lv_first_as_string.
      lv_second_as_string = i_second_value.
      CONDENSE lv_second_as_string.

      MESSAGE e078 WITH lv_first_as_string lv_second_as_string INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDTRY.
  endmethod.


  method _RAISE_IF_RIGHT_OPERAND_RANGE.
    IF zcl_zosql_utils=>is_internal_table( iv_right_operand ) = abap_true.
      MESSAGE e086 WITH ms_right_operand-parameter_info-param_name_in_select INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.
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
