class ZCL_ZOSQL_SELECT_PROCESSOR definition
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
  types:
    ty_select_parameters TYPE STANDARD TABLE OF ty_select_parameter
                                   WITH KEY dataset_name field_name .
  types:
    TY_ITERATOR_POSITIONS  TYPE STANDARD TABLE OF REF TO zcl_zosql_iterator_position WITH DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IO_FROM_ITERATOR type ref to ZCL_ZOSQL_FROM_ITERATOR
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
      !IO_FROM_ITERATOR type ref to ZCL_ZOSQL_FROM_ITERATOR
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
      !IO_FROM_ITERATOR type ref to ZCL_ZOSQL_FROM_ITERATOR
    raising
      ZCX_ZOSQL_ERROR .
  methods _STAR_TO_FIELD_LIST
    importing
      !IO_FROM_ITERATOR type ref to ZCL_ZOSQL_FROM_ITERATOR
    raising
      ZCX_ZOSQL_ERROR .
  methods _CREATE_DATA_SET_FOR_SELECT
    importing
      !IO_FROM_ITERATOR type ref to ZCL_ZOSQL_FROM_ITERATOR
    raising
      ZCX_ZOSQL_ERROR .
ENDCLASS.



CLASS ZCL_ZOSQL_SELECT_PROCESSOR IMPLEMENTATION.


  METHOD ADD_LINE_TO_RESULT.

    DATA: ld_ref_to_dataset_data TYPE REF TO data.

    FIELD-SYMBOLS: <ls_select_parameter>    LIKE LINE OF mt_select_parameters,
                   <ls_ref_to_dataset_data> TYPE any,
                   <lv_dataset_field_value> TYPE any,
                   <lv_result_field_value>  TYPE any,
                   <lt_result>              TYPE STANDARD TABLE,
                   <ls_result>              TYPE any.

    ASSIGN md_data_set->* TO <lt_result>.
    APPEND INITIAL LINE TO <lt_result> ASSIGNING <ls_result>.

    LOOP AT mt_select_parameters ASSIGNING <ls_select_parameter>.
      ld_ref_to_dataset_data = io_iteration_position->get_line_for_data_set_ref( <ls_select_parameter>-dataset_name ).

      ASSIGN ld_ref_to_dataset_data->* TO <ls_ref_to_dataset_data>.
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT <ls_select_parameter>-field_name OF STRUCTURE <ls_ref_to_dataset_data> TO <lv_dataset_field_value>.
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT <ls_select_parameter>-field_name_in_result OF STRUCTURE <ls_result> TO <lv_result_field_value>.
      CHECK sy-subrc = 0.

      <lv_result_field_value> = <lv_dataset_field_value>.
    ENDLOOP.

    APPEND io_iteration_position TO mt_iter_positions_of_data_set.
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


  METHOD APPLY_GROUP_BY.

    FIELD-SYMBOLS: <lt_data_set>             TYPE STANDARD TABLE.

    ASSIGN md_data_set->* TO <lt_data_set>.

    io_group_by->apply_group_by( EXPORTING io_select   = me
                                 CHANGING  ct_data_set = <lt_data_set> ).
  ENDMETHOD.


  method CONSTRUCTOR.
    _fill_select_fields( io_sql_parser ).
    _star_to_field_list( io_from_iterator ).
    _fill_dataset_where_empty( io_from_iterator ).
    _create_data_set_for_select( io_from_iterator ).
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
                                                     io_from_iterator    = io_from_iterator ).
      ENDIF.

      <ls_select_parameter>-field_name_in_result = ls_new_component-name.

      APPEND ls_new_component TO lt_target_set_components.
    ENDLOOP.

*    CLEAR ls_new_component.
*    ls_new_component-name = c_special_field_with_datasets.
*    ls_new_component-type ?= cl_abap_typedescr=>describe_by_name( 'ZCL_ZOSQL_ITERATOR_POSITION' ).
*    APPEND ls_new_component TO lt_target_set_components.

    lo_struct = cl_abap_structdescr=>create( lt_target_set_components ).
    lo_table = cl_abap_tabledescr=>create( lo_struct ).

    CREATE DATA md_data_set TYPE HANDLE lo_table.
  ENDMETHOD.


  METHOD _FILL_DATASET_WHERE_EMPTY.

    DATA: lt_data_set_list          TYPE zcl_zosql_iterator_position=>ty_data_sets,
          ls_data_set               LIKE LINE OF lt_data_set_list,
          lt_components_of_data_set TYPE cl_abap_structdescr=>component_table.

    FIELD-SYMBOLS: <ls_select_parameter> LIKE LINE OF mt_select_parameters.

    lt_data_set_list = io_from_iterator->get_data_set_list( ).

    LOOP AT mt_select_parameters ASSIGNING <ls_select_parameter>
      WHERE dataset_name IS INITIAL.

      LOOP AT lt_data_set_list INTO ls_data_set.
        lt_components_of_data_set = io_from_iterator->get_components_of_data_set( ls_data_set-dataset_name ).
        READ TABLE lt_components_of_data_set WITH KEY name = <ls_select_parameter>-field_name TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <ls_select_parameter>-dataset_name = ls_data_set-dataset_name.
          CONTINUE.
        ENDIF.
      ENDLOOP.

      IF <ls_select_parameter>-dataset_name IS INITIAL AND <ls_select_parameter>-function_name <> c_function_count.
        MESSAGE e057 WITH <ls_select_parameter>-field_name INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.
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

    rs_select_field_parameters-dataset_name = zcl_zosql_utils=>condense( zcl_zosql_utils=>to_upper_case( lv_dataset_name_or_alias ) ).
    rs_select_field_parameters-field_name   = zcl_zosql_utils=>condense( zcl_zosql_utils=>to_upper_case( lv_field_name ) ).
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
            rs_select_field_parameters-field_name = '*'.
          ENDIF.
        WHEN zcl_zosql_parser_recurs_desc=>node_type-function_argument.
          ls_argument_parameters = _fill_select_field_as_field( lo_node_part_of_select_field ).
          rs_select_field_parameters-dataset_name = ls_argument_parameters-dataset_name.
          rs_select_field_parameters-field_name   = ls_argument_parameters-field_name.
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
          lt_dataset_components    TYPE cl_abap_structdescr=>component_table.

    FIELD-SYMBOLS: <ls_component> LIKE LINE OF lt_dataset_components.

    ld_ref_to_dataset_data = io_from_iterator->get_line_for_data_set_ref( is_select_parameter-dataset_name ).

    lo_struct ?= cl_abap_structdescr=>describe_by_data_ref( ld_ref_to_dataset_data ).
    lt_dataset_components = lo_struct->get_components( ).

    READ TABLE lt_dataset_components WITH KEY name = is_select_parameter-field_name INTO rs_component.
    IF sy-subrc = 0.
      IF is_select_parameter-field_alias IS NOT INITIAL.
        rs_component-name = is_select_parameter-field_alias.
      ENDIF.
    ELSE.
      MESSAGE e073 WITH is_select_parameter-field_name is_select_parameter-field_alias
        INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    IF is_select_parameter-parameter_type = parameter_type-groupby_function
      AND is_select_parameter-field_alias IS INITIAL.

      CONCATENATE is_select_parameter-function_name is_select_parameter-field_name INTO rs_component-name SEPARATED BY '_'.
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


  method _STAR_TO_FIELD_LIST.

    TYPES: BEGIN OF ty_field,
             fieldname TYPE string,
             dataset_index TYPE i,
           END OF ty_field.

    DATA: lt_new_select_parameters LIKE mt_select_parameters,
          lt_data_set_list         TYPE zcl_zosql_iterator_position=>ty_data_sets,
          ls_data_set              LIKE LINE OF lt_data_set_list,
          lt_components            TYPE cl_abap_structdescr=>component_table,
          lv_dataset_index         TYPE i,
          lt_fields                TYPE TABLE OF ty_field,
          ls_field                 TYPE ty_field.

    FIELD-SYMBOLS: <ls_select_parameter> LIKE LINE OF mt_select_parameters,
                   <ls_component>        LIKE LINE OF lt_components,
                   <ls_new_select_parameter> LIKE LINE OF mt_select_parameters.

    lt_data_set_list = io_from_iterator->get_data_set_list( ).

    LOOP AT mt_select_parameters ASSIGNING <ls_select_parameter>.

      IF <ls_select_parameter>-field_name = '*' AND <ls_select_parameter>-function_name IS INITIAL.

        LOOP AT lt_data_set_list INTO ls_data_set.

          lv_dataset_index = sy-tabix.

          IF <ls_select_parameter>-dataset_name IS NOT INITIAL
            AND <ls_select_parameter>-dataset_name <> ls_data_set-dataset_name
            AND <ls_select_parameter>-dataset_name <> ls_data_set-dataset_alias.

            CONTINUE.
          ENDIF.

          lt_components = io_from_iterator->get_components_of_data_set( ls_data_set-dataset_name ).

          LOOP AT lt_components ASSIGNING <ls_component>.
            ls_field-fieldname = <ls_component>-name.
            ls_field-dataset_index = lv_dataset_index.
            APPEND ls_field TO lt_fields.
          ENDLOOP.
        ENDLOOP.

        SORT lt_fields BY fieldname dataset_index.
        DELETE ADJACENT DUPLICATES FROM lt_fields COMPARING fieldname.

        LOOP AT lt_fields INTO ls_field.
          APPEND INITIAL LINE TO lt_new_select_parameters ASSIGNING <ls_new_select_parameter>.
          <ls_new_select_parameter>-field_name = ls_field-fieldname.
          READ TABLE lt_data_set_list INDEX ls_field-dataset_index INTO ls_data_set.
          IF sy-subrc = 0.
            <ls_new_select_parameter>-dataset_name = ls_data_set-dataset_name.
          ENDIF.
        ENDLOOP.
      ELSE.
        APPEND <ls_select_parameter> TO lt_new_select_parameters.
      ENDIF.
    ENDLOOP.

    mt_select_parameters = lt_new_select_parameters.
  endmethod.
ENDCLASS.
