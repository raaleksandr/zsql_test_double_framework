class ZCL_ZOSQL_SELECT_PROCESSOR definition
  public
  inheriting from ZCL_ZOSQL_PROCESSOR_BASE
  create public .

public section.

  types:
    BEGIN OF ty_select_parameter,
        parameter_type        TYPE char20,
        dataset_name_or_alias TYPE string,
        fieldname             TYPE string,
        field_alias           TYPE string,
        function_name         TYPE string,
        distinct_flag         TYPE abap_bool,
        field_name_in_result  TYPE string,
      END OF ty_select_parameter .
  types:
    ty_select_parameters TYPE STANDARD TABLE OF ty_select_parameter
                                     WITH KEY dataset_name_or_alias fieldname .

  types:
    BEGIN OF ty_iterator_position,
      line_index TYPE i,
      iterator_position TYPE REF TO zcl_zosql_iterator_position,
    END OF ty_iterator_position.
  types:
    ty_iterator_positions  TYPE STANDARD TABLE OF ty_iterator_position WITH KEY line_index .

  methods APPLY_ORDER_BY
    importing
      !IO_ORDER_BY type ref to ZCL_ZOSQL_ORDERBY_PROCESSOR .
  methods CONSTRUCTOR
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IO_ITERATOR type ref to ZIF_ZOSQL_ITERATOR
    raising
      ZCX_ZOSQL_ERROR .
  methods GET_SELECT_PARAMETERS
    returning
      value(RT_SELECT_PARAMETERS) type TY_SELECT_PARAMETERS .
  methods GET_ITER_POSITIONS_OF_LINES
    returning
      value(RT_ITERATOR_POSITIONS) type TY_ITERATOR_POSITIONS .
  methods APPLY_AGGR_FUNC_NO_GROUP_BY
    raising
      ZCX_ZOSQL_ERROR .
  methods HAS_AGGREGATION_FUNCTIONS
    returning
      value(RV_HAS_AGGREGATION_FUNCTIONS) type ABAP_BOOL .
  methods APPLY_DISTINCT .
  methods APPLY_GROUP_BY
    importing
      !IO_GROUP_BY type ref to ZCL_ZOSQL_GROUPBY_PROCESSOR
    raising
      ZCX_ZOSQL_ERROR .
  methods GET_RESULT_AS_REF_TO_DATA
    returning
      value(RD_REF_TO_RESULT_SET) type ref to DATA .
  methods GET_RESULT_MOVE_CORRESPONDING
    changing
      !CT_RESULT_TABLE type ANY TABLE .
  methods ADD_LINE_TO_RESULT
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_ITERATOR_POSITION
    raising
      ZCX_ZOSQL_ERROR .
protected section.
private section.

  constants:
    BEGIN OF PARAMETER_TYPE,
               field  TYPE char20 VALUE 'FIELD',
               groupby_function TYPE char20 VALUE 'GROUPBY_FUNCTION',
             END OF parameter_type .
  data MT_SELECT_PARAMETERS type TY_SELECT_PARAMETERS .
  data MD_DATA_SET type ref to DATA .
  constants C_FUNCTION_COUNT type STRING value 'COUNT' ##NO_TEXT.
  constants C_FUNCTION_AVG type STRING value 'AVG' ##NO_TEXT.
  data MT_ITER_POSITIONS_OF_DATA_SET type TY_ITERATOR_POSITIONS .

  methods _IS_DATASET_EMPTY
    returning
      value(RV_IS_EMPTY) type ABAP_BOOL .
  methods _RAISE_IF_NOT_SELECT
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    raising
      ZCX_ZOSQL_ERROR .
  methods _CHECK_ON_DUPLICATES
    importing
      !IT_COMPONENTS type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE
    raising
      ZCX_ZOSQL_ERROR .
  methods _FILL_SELECT_FIELD
    importing
      !IO_SQL_PARSER_NODE type ref to ZCL_ZOSQL_PARSER_NODE
    returning
      value(RS_SELECT_FIELD_PARAMETERS) type TY_SELECT_PARAMETER .
  methods _FILL_SELECT_FIELD_AS_FIELD
    importing
      !IO_SQL_PARSER_NODE type ref to ZCL_ZOSQL_PARSER_NODE
    returning
      value(RS_SELECT_FIELD_PARAMETERS) type TY_SELECT_PARAMETER .
  methods _FILL_SELECT_FIELD_AS_FUNCTION
    importing
      !IO_SQL_PARSER_NODE type ref to ZCL_ZOSQL_PARSER_NODE
    returning
      value(RS_SELECT_FIELD_PARAMETERS) type TY_SELECT_PARAMETER .
  methods _GET_ALIAS
    importing
      !IO_SQL_PARSER_NODE type ref to ZCL_ZOSQL_PARSER_NODE
    returning
      value(RV_ALIAS) type STRING .
  methods _GET_FUNCTION_NAME_FROM_TOKEN
    importing
      !IV_TOKEN type CLIKE
    returning
      value(RV_FUNCTION_NAME) type STRING .
  methods _FILL_SELECT_FIELDS
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC .
  methods _GET_COMPONENT_FOR_FIELD
    importing
      !IS_SELECT_PARAMETER type TY_SELECT_PARAMETER
      !IO_ITERATOR type ref to ZIF_ZOSQL_ITERATOR
    returning
      value(RS_COMPONENT) type ABAP_COMPONENTDESCR
    raising
      ZCX_ZOSQL_ERROR .
  methods _GET_COMPONENT_FOR_COUNT_STAR
    importing
      !IV_NAME type CLIKE
    returning
      value(RS_COMPONENT) type ABAP_COMPONENTDESCR .
  methods _GET_ALL_FIELDS_OF_DATASET
    returning
      value(RT_ALL_FIELDS_OF_DATASET) type FIELDNAME_TABLE .
  methods _FILL_DATASET_WHERE_EMPTY
    importing
      !IO_ITERATOR type ref to ZIF_ZOSQL_ITERATOR
    raising
      ZCX_ZOSQL_ERROR .
  methods _STAR_TO_FIELD_LIST
    importing
      !IO_ITERATOR type ref to ZIF_ZOSQL_ITERATOR
    raising
      ZCX_ZOSQL_ERROR .
  methods _CREATE_DATA_SET_FOR_SELECT
    importing
      !IO_ITERATOR type ref to ZIF_ZOSQL_ITERATOR
    raising
      ZCX_ZOSQL_ERROR .
ENDCLASS.



CLASS ZCL_ZOSQL_SELECT_PROCESSOR IMPLEMENTATION.


  METHOD add_line_to_result.

    DATA: ld_ref_to_dataset_data    TYPE REF TO data,
          lv_index_of_inserted_line TYPE i,
          ls_iterator_position      TYPE ty_iterator_position.

    FIELD-SYMBOLS: <ls_select_parameter>    LIKE LINE OF mt_select_parameters,
                   <ls_ref_to_dataset_data> TYPE any,
                   <lv_dataset_field_value> TYPE any,
                   <lv_result_field_value>  TYPE any,
                   <lt_result>              TYPE STANDARD TABLE,
                   <ls_result>              TYPE any.

    ASSIGN md_data_set->* TO <lt_result>.
    APPEND INITIAL LINE TO <lt_result> ASSIGNING <ls_result>.
    lv_index_of_inserted_line = sy-tabix.

    LOOP AT mt_select_parameters ASSIGNING <ls_select_parameter>.
      ld_ref_to_dataset_data =
        io_iteration_position->get_line_for_data_set_ref(
          <ls_select_parameter>-dataset_name_or_alias ).

      ASSIGN ld_ref_to_dataset_data->* TO <ls_ref_to_dataset_data>.
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT <ls_select_parameter>-fieldname OF STRUCTURE <ls_ref_to_dataset_data> TO <lv_dataset_field_value>.
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT <ls_select_parameter>-field_name_in_result OF STRUCTURE <ls_result> TO <lv_result_field_value>.
      CHECK sy-subrc = 0.

      <lv_result_field_value> = <lv_dataset_field_value>.
    ENDLOOP.

    ls_iterator_position-line_index = lv_index_of_inserted_line.
    ls_iterator_position-iterator_position = io_iteration_position.
    APPEND ls_iterator_position TO mt_iter_positions_of_data_set.
  ENDMETHOD.


  method APPLY_AGGR_FUNC_NO_GROUP_BY.

    DATA: lo_aggregation_function  TYPE REF TO zcl_zosql_aggr_func_processor.

    FIELD-SYMBOLS: <lt_data_set> TYPE STANDARD TABLE.

    ASSIGN md_data_set->* TO <lt_data_set>.

    CREATE OBJECT lo_aggregation_function.
    lo_aggregation_function->apply_aggregation( EXPORTING io_select   = me
                                                CHANGING  ct_data_set = <lt_data_set> ).
  endmethod.


  method APPLY_DISTINCT.

    DATA: ld_hash_table_with_all_fields TYPE REF TO data.

    FIELD-SYMBOLS: <lt_hash_tab_with_all_fields> TYPE HASHED TABLE,
                   <lt_data_set>                 TYPE STANDARD TABLE,
                   <ls_data_set>                 TYPE any.

    ASSIGN md_data_set->* TO <lt_data_set>.

    ld_hash_table_with_all_fields =
      zcl_zosql_utils=>create_hash_tab_like_standard(
        it_standard_table_template = <lt_data_set>
        it_key_fields              = _get_all_fields_of_dataset( ) ).

    ASSIGN ld_hash_table_with_all_fields->* TO <lt_hash_tab_with_all_fields>.

    LOOP AT <lt_data_set> ASSIGNING <ls_data_set>.
      READ TABLE <lt_hash_tab_with_all_fields> FROM <ls_data_set> TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE <lt_data_set>.
      ELSE.
        INSERT <ls_data_set> INTO TABLE <lt_hash_tab_with_all_fields>.
      ENDIF.
    ENDLOOP.
  endmethod.


  METHOD apply_group_by.

    FIELD-SYMBOLS: <lt_data_set>             TYPE STANDARD TABLE.

    IF _is_dataset_empty( ) = abap_true.
      RETURN.
    ENDIF.

    ASSIGN md_data_set->* TO <lt_data_set>.

    io_group_by->apply_group_by( EXPORTING io_select   = me
                                 CHANGING  ct_data_set = <lt_data_set> ).

    mt_iter_positions_of_data_set = io_group_by->mt_iter_positions_of_data_set.
  ENDMETHOD.


  method APPLY_ORDER_BY.

    FIELD-SYMBOLS: <lt_data_set>             TYPE STANDARD TABLE.

    ASSIGN md_data_set->* TO <lt_data_set>.

    io_order_by->apply_order_by( EXPORTING io_select   = me
                                 CHANGING  ct_data_set = <lt_data_set> ).
  endmethod.


  method CONSTRUCTOR.
    super->constructor( ).

    _raise_if_not_select( io_sql_parser ).
    _fill_select_fields( io_sql_parser ).
    _star_to_field_list( io_iterator ).
    _fill_dataset_where_empty( io_iterator ).
    _create_data_set_for_select( io_iterator ).
  endmethod.


  method GET_ITER_POSITIONS_OF_LINES.
    rt_iterator_positions = mt_iter_positions_of_data_set.
  endmethod.


  method GET_RESULT_AS_REF_TO_DATA.
    rd_ref_to_result_set = md_data_set.
  endmethod.


  method GET_RESULT_MOVE_CORRESPONDING.

    FIELD-SYMBOLS: <lt_result> TYPE STANDARD TABLE.

    IF md_data_set IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN md_data_set->* TO <lt_result>.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_result>
                                               IMPORTING et_table_dest = ct_result_table ).
  endmethod.


  method GET_SELECT_PARAMETERS.
    rt_select_parameters = mt_select_parameters.
  endmethod.


  method HAS_AGGREGATION_FUNCTIONS.

    READ TABLE mt_select_parameters WITH KEY parameter_type = parameter_type-groupby_function
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      rv_has_aggregation_functions = abap_true.
    ENDIF.
  endmethod.


  method _CHECK_ON_DUPLICATES.

    TYPES: BEGIN OF ty_name,
             name  TYPE fieldname,
             count TYPE i,
           END OF ty_name.

    DATA: ls_name  TYPE ty_name,
          lt_names TYPE TABLE OF ty_name.

    FIELD-SYMBOLS: <Ls_comp> LIKE LINE OF it_components.

    LOOP AT it_components ASSIGNING <ls_comp>.
      ls_name-name  = <ls_comp>-name.
      ls_name-count = 1.
      COLLECT ls_name INTO lt_names.
    ENDLOOP.

    LOOP AT lt_names INTO ls_name
      WHERE count > 1.

      MESSAGE e098 WITH ls_name-name INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDLOOP.
  endmethod.


  METHOD _create_data_set_for_select.

    DATA: lo_struct                TYPE REF TO cl_abap_structdescr,
          lt_target_set_components TYPE cl_abap_structdescr=>component_table,
          lo_table                 TYPE REF TO cl_abap_tabledescr,
          ls_new_component         TYPE LINE OF cl_abap_structdescr=>component_table.

    FIELD-SYMBOLS: <ls_select_parameter> LIKE LINE OF mt_select_parameters.

    LOOP AT mt_select_parameters ASSIGNING <ls_select_parameter>.

      IF <ls_select_parameter>-function_name = c_function_count
        AND <ls_select_parameter>-distinct_flag <> abap_true.

        ls_new_component = _get_component_for_count_star( <ls_select_parameter>-field_alias ).
      ELSE.

        ls_new_component = _get_component_for_field( is_select_parameter = <ls_select_parameter>
                                                     io_iterator         = io_iterator ).
      ENDIF.

      <ls_select_parameter>-field_name_in_result = zcl_zosql_utils=>to_upper_case( ls_new_component-name ).

      APPEND ls_new_component TO lt_target_set_components.
    ENDLOOP.

    _check_on_duplicates( lt_target_set_components ).

    lo_struct = cl_abap_structdescr=>create( lt_target_set_components ).
    lo_table = cl_abap_tabledescr=>create( lo_struct ).

    CREATE DATA md_data_set TYPE HANDLE lo_table.
  ENDMETHOD.


  METHOD _FILL_DATASET_WHERE_EMPTY.

    FIELD-SYMBOLS: <ls_select_parameter> LIKE LINE OF mt_select_parameters.

    fill_dataset_where_empty( EXPORTING io_iterator            = io_iterator
                                        iv_error_if_not_found  = abap_false
                              CHANGING  ct_table_where_to_fill = mt_select_parameters ).

    LOOP AT mt_select_parameters ASSIGNING <ls_select_parameter>
      WHERE dataset_name_or_alias IS INITIAL
        AND function_name <> c_function_count.

      MESSAGE e057 WITH <ls_select_parameter>-fieldname INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDLOOP.
  ENDMETHOD.


  METHOD _fill_select_field.

    CASE io_sql_parser_node->node_type.
      WHEN 'SELECT_FIELD'.
        rs_select_field_parameters =
          _fill_select_field_as_field( io_sql_parser_node ).
      WHEN 'FUNCTION'.
        rs_select_field_parameters =
          _fill_select_field_as_function( io_sql_parser_node ).
    ENDCASE.

    rs_select_field_parameters-field_alias = _get_alias( io_sql_parser_node ).
  ENDMETHOD.


  METHOD _fill_select_fields.

    DATA: lo_sql_parser_helper        TYPE REF TO zcl_zosql_parser_helper,
          lt_nodes_with_select_fields TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          ls_parameter                TYPE ty_select_parameter,
          lo_node_select_field        TYPE REF TO zcl_zosql_parser_node.

    CREATE OBJECT lo_sql_parser_helper
      EXPORTING
        io_sql_parser = io_sql_parser.
    lo_sql_parser_helper->get_key_nodes_of_sql_select( IMPORTING et_nodes_of_select_field_list = lt_nodes_with_select_fields ).

    LOOP AT lt_nodes_with_select_fields INTO lo_node_select_field.
      ls_parameter = _fill_select_field( lo_node_select_field ).
      APPEND ls_parameter TO mt_select_parameters.
    ENDLOOP.
  ENDMETHOD.


  method _FILL_SELECT_FIELD_AS_FIELD.

    DATA: lv_dataset_name_or_alias TYPE string,
          lv_field_name            TYPE string.

    rs_select_field_parameters-parameter_type = parameter_type-field.

    SPLIT io_sql_parser_node->token AT '~' INTO lv_dataset_name_or_alias lv_field_name.
    IF lv_field_name IS INITIAL.
      lv_field_name = lv_dataset_name_or_alias.
      CLEAR lv_dataset_name_or_alias.
    ENDIF.

    rs_select_field_parameters-dataset_name_or_alias = zcl_zosql_utils=>condense( zcl_zosql_utils=>to_upper_case( lv_dataset_name_or_alias ) ).
    rs_select_field_parameters-fieldname             = zcl_zosql_utils=>condense( zcl_zosql_utils=>to_upper_case( lv_field_name ) ).
  endmethod.


  METHOD _fill_select_field_as_function.

    DATA: lt_nodes_of_select_field     TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          ls_argument_parameters       TYPE ty_select_parameter,
          lo_node_part_of_select_field TYPE REF TO zcl_zosql_parser_node.

    lt_nodes_of_select_field = io_sql_parser_node->get_child_nodes_with_self( ).

    LOOP AT lt_nodes_of_select_field INTO lo_node_part_of_select_field.
      CASE lo_node_part_of_select_field->node_type.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-function.
          rs_select_field_parameters-function_name =
            _get_function_name_from_token( lo_node_part_of_select_field->token_ucase ).

          IF rs_select_field_parameters-function_name = 'COUNT(*)'.
            rs_select_field_parameters-fieldname = '*'.
          ENDIF.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-function_argument.
          ls_argument_parameters = _fill_select_field_as_field( lo_node_part_of_select_field ).
          rs_select_field_parameters-dataset_name_or_alias = ls_argument_parameters-dataset_name_or_alias.
          rs_select_field_parameters-fieldname             = ls_argument_parameters-fieldname.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-distinct.
          rs_select_field_parameters-distinct_flag = abap_true.
      ENDCASE.
    ENDLOOP.

    rs_select_field_parameters-parameter_type = parameter_type-groupby_function.
  ENDMETHOD.


  method _GET_ALIAS.
    rv_alias =
      io_sql_parser_node->get_child_node_token_with_type(
        zcl_zosql_parser_recurs_desc=>node_type-alias ).
  endmethod.


  METHOD _GET_ALL_FIELDS_OF_DATASET.

    DATA: lo_table_data_set  TYPE REF TO cl_abap_tabledescr,
          lo_struct_data_set TYPE REF TO cl_abap_structdescr,
          lt_components      TYPE cl_abap_structdescr=>component_table.

    FIELD-SYMBOLS: <ls_component> LIKE LINE OF lt_components.

    lo_table_data_set ?= cl_abap_tabledescr=>describe_by_data_ref( md_data_set ).
    lo_struct_data_set ?= lo_table_data_set->get_table_line_type( ).
    lt_components = lo_struct_data_set->get_components( ).

    LOOP AT lt_components ASSIGNING <ls_component>.
      APPEND <ls_component>-name TO rt_all_fields_of_dataset.
    ENDLOOP.
  ENDMETHOD.


  method _GET_COMPONENT_FOR_COUNT_STAR.

    IF iv_name IS NOT INITIAL.
      rs_component-name = iv_name.
    ELSE.
      rs_component-name = 'COUNT_ALL'.
    ENDIF.

    rs_component-type = cl_abap_elemdescr=>get_i( ).
  endmethod.


  METHOD _GET_COMPONENT_FOR_FIELD.

    DATA: ld_ref_to_dataset_data   TYPE REF TO data,
          lo_struct                TYPE REF TO cl_abap_structdescr,
          lt_dataset_components    TYPE cl_abap_structdescr=>included_view,
          ls_component             TYPE abap_simple_componentdescr.

    FIELD-SYMBOLS: <ls_component> LIKE LINE OF lt_dataset_components.

    ld_ref_to_dataset_data = io_iterator->get_line_for_data_set_ref( is_select_parameter-dataset_name_or_alias ).

    lo_struct ?= cl_abap_structdescr=>describe_by_data_ref( ld_ref_to_dataset_data ).
    lt_dataset_components = lo_struct->get_included_view( ).

    READ TABLE lt_dataset_components WITH KEY name = is_select_parameter-fieldname INTO ls_component.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_component TO rs_component.
      IF is_select_parameter-field_alias IS NOT INITIAL.
        rs_component-name = is_select_parameter-field_alias.
      ENDIF.
    ELSE.
      MESSAGE e073 WITH is_select_parameter-fieldname is_select_parameter-field_alias
        INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    IF is_select_parameter-parameter_type = parameter_type-groupby_function
      AND is_select_parameter-field_alias IS INITIAL.

      CONCATENATE is_select_parameter-function_name is_select_parameter-fieldname INTO rs_component-name SEPARATED BY '_'.
    ENDIF.
   ENDMETHOD.


  method _GET_FUNCTION_NAME_FROM_TOKEN.
    rv_function_name = iv_token.
    REPLACE FIRST OCCURRENCE OF '(' IN rv_function_name WITH space.
    REPLACE FIRST OCCURRENCE OF '*' IN rv_function_name WITH space.
    REPLACE FIRST OCCURRENCE OF ')' IN rv_function_name WITH space.
    CONDENSE rv_function_name.
    rv_function_name = zcl_zosql_utils=>to_upper_case( rv_function_name ).
  endmethod.


  METHOD _is_dataset_empty.

    FIELD-SYMBOLS: <lt_data_set> TYPE STANDARD TABLE.

    IF md_data_set IS NOT BOUND.
      rv_is_empty = abap_true.
      RETURN.
    ENDIF.

    ASSIGN md_data_set->* TO <lt_data_set>.
    IF <lt_data_set> IS INITIAL.
      rv_is_empty = abap_true.
    ENDIF.
  ENDMETHOD.


  method _RAISE_IF_NOT_SELECT.

    DATA: lo_parser_helper TYPE REF TO zcl_zosql_parser_helper,
          lv_all_sql       TYPE string.

    CREATE OBJECT lo_parser_helper
      EXPORTING
        io_sql_parser = io_sql_parser.

    IF lo_parser_helper->is_select( ) <> abap_true.
      lv_all_sql = io_sql_parser->get_sql( ).
      MESSAGE e089 WITH lv_all_sql INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.
  endmethod.


  method _STAR_TO_FIELD_LIST.

    TYPES: BEGIN OF ty_field,
             fieldname TYPE string,
             dataset_index TYPE i,
           END OF ty_field.

    DATA: lt_new_select_parameters LIKE mt_select_parameters,
          lt_data_set_list         TYPE zcl_zosql_iterator_position=>ty_data_sets,
          ls_data_set              LIKE LINE OF lt_data_set_list,
          lt_components            TYPE cl_abap_structdescr=>included_view,
          lv_dataset_index         TYPE i,
          lt_fields                TYPE TABLE OF ty_field,
          ls_field                 TYPE ty_field.

    FIELD-SYMBOLS: <ls_select_parameter> LIKE LINE OF mt_select_parameters,
                   <ls_component>        LIKE LINE OF lt_components,
                   <ls_new_select_parameter> LIKE LINE OF mt_select_parameters.

    lt_data_set_list = io_iterator->get_data_set_list( ).

    LOOP AT mt_select_parameters ASSIGNING <ls_select_parameter>.

      IF <ls_select_parameter>-fieldname = '*' AND <ls_select_parameter>-function_name IS INITIAL.

        LOOP AT lt_data_set_list INTO ls_data_set.

          lv_dataset_index = sy-tabix.

          IF <ls_select_parameter>-dataset_name_or_alias IS NOT INITIAL
            AND <ls_select_parameter>-dataset_name_or_alias <> ls_data_set-dataset_name
            AND <ls_select_parameter>-dataset_name_or_alias <> ls_data_set-dataset_alias.

            CONTINUE.
          ENDIF.

          lt_components = io_iterator->get_components_of_data_set( ls_data_set-dataset_name ).

          LOOP AT lt_components ASSIGNING <ls_component>.
            ls_field-fieldname = <ls_component>-name.
            ls_field-dataset_index = lv_dataset_index.
            APPEND ls_field TO lt_fields.
          ENDLOOP.
        ENDLOOP.

        LOOP AT lt_fields INTO ls_field.
          APPEND INITIAL LINE TO lt_new_select_parameters ASSIGNING <ls_new_select_parameter>.
          <ls_new_select_parameter>-fieldname = ls_field-fieldname.
          READ TABLE lt_data_set_list INDEX ls_field-dataset_index INTO ls_data_set.
          IF sy-subrc = 0.
            <ls_new_select_parameter>-dataset_name_or_alias = ls_data_set-dataset_name.
          ENDIF.
        ENDLOOP.
      ELSE.
        APPEND <ls_select_parameter> TO lt_new_select_parameters.
      ENDIF.
    ENDLOOP.

    mt_select_parameters = lt_new_select_parameters.
  endmethod.
ENDCLASS.
