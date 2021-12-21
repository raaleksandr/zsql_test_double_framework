class ZCL_ZOSQL_ONE_SELECT_FIELD definition
  public
  create public .

public section.

  types:
    BEGIN OF TY_SELECT_PARAMETER,
           parameter_type TYPE char20,
           dataset_name   TYPE string,
           field_name     TYPE string,
           field_alias    TYPE string,
           function_name  TYPE string,
           distinct_flag  TYPE abap_bool,
           field_name_in_result TYPE string,
         END OF ty_select_parameter .

  constants:
    BEGIN OF PARAMETER_TYPE,
               field  TYPE char20 VALUE 'FIELD',
               groupby_function TYPE char20 VALUE 'GROUPBY_FUNCTION',
             END OF parameter_type .

  methods GET_PARAMETERS_OF_SELECT_FIELD
    returning
      value(RS_SELECT_PARAMETER) type TY_SELECT_PARAMETER .
  methods FILL_SELECT_FIELD_PARAMS .
  methods CONSTRUCTOR
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      value(IV_SELECT_FIELD_NODE_ID) type I .
protected section.
private section.

  data MT_NODES type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_TREE .
  data MS_PARAMETERS_OF_SELECT_FIELD type TY_SELECT_PARAMETER .
  data MV_CURRENT_NODE_INDEX type I .
  data MS_CURRENT_NODE type ZCL_ZOSQL_PARSER_RECURS_DESC=>TY_NODE .

  methods _ALIAS .
  methods _FIELD .
  methods _FILL_CURRENT_NODE .
  methods _FUNCTION
    returning
      value(RV_IT_WAS_REALLY_FUNCTION) type ABAP_BOOL .
  methods _GET_FUNCTION_NAME
    returning
      value(RV_FUNCTION_NAME) type STRING .
  methods _MOVE_TO_FIRST
    returning
      value(RV_SUCCESSFUL_MOVE) type ABAP_BOOL .
  methods _MOVE_TO_NEXT
    returning
      value(RV_MOVE_SUCCESSFUL) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_ZOSQL_ONE_SELECT_FIELD IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA: ls_main_node_of_select_field LIKE LINE OF mt_nodes.

    mt_nodes = io_sql_parser->get_child_nodes( iv_select_field_node_id ).
    ls_main_node_of_select_field = io_sql_parser->get_node_info( iv_select_field_node_id ).
    INSERT ls_main_node_of_select_field INTO mt_nodes INDEX 1.
  endmethod.


  method FILL_SELECT_FIELD_PARAMS.
    IF _move_to_first( ) <> abap_true.
      RETURN.
    ENDIF.

    IF _function( ) <> abap_true.
      _field( ).
    ENDIF.

    _alias( ).
  endmethod.


  method GET_PARAMETERS_OF_SELECT_FIELD.
    rs_select_parameter = ms_parameters_of_select_field.
  endmethod.


  method _ALIAS.
    IF ms_current_node-token_ucase <> 'AS'.
      RETURN.
    ENDIF.

    IF _move_to_next( ) <> abap_true.
      RETURN.
    ENDIF.

    ms_parameters_of_select_field-field_alias = ms_current_node-token.
  endmethod.


  method _FIELD.

    DATA: lv_dataset_name_or_alias TYPE string,
          lv_field_name            TYPE string.

    ms_parameters_of_select_field-parameter_type = parameter_type-field.

    SPLIT ms_current_node-token AT '~' INTO lv_dataset_name_or_alias lv_field_name.
    IF lv_field_name IS INITIAL.
      lv_field_name = lv_dataset_name_or_alias.
      CLEAR lv_dataset_name_or_alias.
    ENDIF.

    ms_parameters_of_select_field-dataset_name = zcl_zosql_utils=>condense( zcl_zosql_utils=>to_upper_case( lv_dataset_name_or_alias ) ).
    ms_parameters_of_select_field-field_name   = zcl_zosql_utils=>condense( zcl_zosql_utils=>to_upper_case( lv_field_name ) ).

    _move_to_next( ).
  endmethod.


  method _FILL_CURRENT_NODE.
    READ TABLE mt_nodes INDEX mv_current_node_index INTO ms_current_node.
    IF sy-subrc <> 0.
      CLEAR ms_current_node.
    ENDIF.
  endmethod.


  method _FUNCTION.
    IF ms_current_node-token_ucase = 'COUNT(*)'.
      ms_parameters_of_select_field-field_name     = '*'.
      ms_parameters_of_select_field-parameter_type = parameter_type-groupby_function.
      ms_parameters_of_select_field-function_name  = _get_function_name( ).

      rv_it_was_really_function = abap_true.
    ELSEIF ms_current_node-token CP '*(*'.
      ms_parameters_of_select_field-function_name = _get_function_name( ).

      IF _move_to_next( ) <> abap_true.
        RETURN.
      ENDIF.

      IF ms_current_node-token_ucase = 'DISTINCT'.
        ms_parameters_of_select_field-distinct_flag = abap_true.

        IF _move_to_next( ) <> abap_true.
          RETURN.
        ENDIF.
      ENDIF.

      _field( ).

      IF ms_current_node-token <> ')'.
        RETURN.
      ENDIF.

      ms_parameters_of_select_field-parameter_type = parameter_type-groupby_function.

      rv_it_was_really_function = abap_true.
    ENDIF.

    IF rv_it_was_really_function = abap_true.
      _move_to_next( ).
    ENDIF.
  endmethod.


  method _GET_FUNCTION_NAME.
    rv_function_name = ms_current_node-token.
    REPLACE FIRST OCCURRENCE OF '(' IN rv_function_name WITH space.
    REPLACE FIRST OCCURRENCE OF '*' IN rv_function_name WITH space.
    REPLACE FIRST OCCURRENCE OF ')' IN rv_function_name WITH space.
    CONDENSE rv_function_name.
    rv_function_name = zcl_zosql_utils=>to_upper_case( rv_function_name ).
  endmethod.


  method _MOVE_TO_FIRST.

    IF mt_nodes IS NOT INITIAL.
      mv_current_node_index = 1.
      _fill_current_node( ).
      rv_successful_move = abap_true.
    ENDIF.
  endmethod.


  method _MOVE_TO_NEXT.
    mv_current_node_index = mv_current_node_index + 1.

    READ TABLE mt_nodes INDEX mv_current_node_index INTO ms_current_node.
    IF sy-subrc = 0.
      rv_move_successful = abap_true.
    ELSE.
      mv_current_node_index = mv_current_node_index - 1.
    ENDIF.
  endmethod.
ENDCLASS.
