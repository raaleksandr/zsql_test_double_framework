class ZCL_ZOSQL_SELECT_PROCESSOR definition
  public
  create public .

public section.

  constants:
    BEGIN OF PARAMETER_TYPE,
               field  TYPE char20 VALUE 'FIELD',
               groupby_function TYPE char20 VALUE 'GROUPBY_FUNCTION',
             END OF parameter_type .

  methods APPLY_DISTINCT .
  methods APPLY_GROUP_BY
    importing
      !IT_GROUP_BY_FIELDS type FIELDNAME_TABLE
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
  methods INITIALIZE_BY_PARSED_SQL
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IO_FROM_ITERATOR type ref to ZCL_ZOSQL_FROM_ITERATOR
    raising
      ZCX_ZOSQL_ERROR .
protected section.
private section.

  types:
    BEGIN OF TY_SELECT_PARAMETER,
           parameter_type TYPE char20,
           dataset_name   TYPE string,
           field_name     TYPE string,
           field_alias    TYPE string,
           function_name  TYPE string,
         END OF ty_select_parameter .

  data:
    MT_SELECT_PARAMETERS  TYPE STANDARD TABLE OF ty_select_parameter WITH KEY field_name .
  data MD_DATA_SET type ref to DATA .
  constants C_AS type STRING value 'AS' ##NO_TEXT.
  constants C_FUNCTION_COUNT type STRING value 'COUNT' ##NO_TEXT.
  constants C_FUNCTION_AVG type STRING value 'AVG' ##NO_TEXT.

  methods _PREPARE_COUNT_FOR_GROUP_BY .
  methods _GET_COMPONENT_FOR_FIELD
    importing
      !IS_SELECT_PARAMETER type TY_SELECT_PARAMETER
      !IO_FROM_ITERATOR type ref to ZCL_ZOSQL_FROM_ITERATOR
    returning
      value(RS_COMPONENT) type ABAP_COMPONENTDESCR
    raising
      ZCX_ZOSQL_ERROR .
  methods _GET_COMPONENT_FOR_COUNT
    importing
      !IV_NAME type CLIKE
    returning
      value(RS_COMPONENT) type ABAP_COMPONENTDESCR .
  methods _APPEND_TO_LAST_FIELD
    importing
      !IV_ALIAS type CLIKE
    changing
      !CT_FIELD_LIST type STRING_TABLE .
  methods _IS_FUNCTION
    importing
      !IV_FIELD type CLIKE
    returning
      value(RV_IS_FUNCTION) type ABAP_BOOL .
  methods _IS_FUNCTION_NAME
    importing
      !IV_TOKEN type CLIKE
    returning
      value(RV_IS_FUNCTION_NAME) type ABAP_BOOL .
  methods _PARSE_FIELD
    importing
      !IV_FIELD type CLIKE
    changing
      !CS_PARAMETER type TY_SELECT_PARAMETER .
  methods _PARSE_FUNCTION
    importing
      !IV_FIELD type CLIKE
    changing
      value(CS_PARAMETER) type TY_SELECT_PARAMETER .
  methods _SEPARATE_ALIAS
    importing
      !IV_WHOLE_EXPRESSION type CLIKE
    exporting
      !EV_EXPRESSION_WITHOUT_ALIAS type CLIKE
      !EV_ALIAS type CLIKE .
  methods _SPLIT_SELECT_INTO_FIELDS
    importing
      !IV_FIELD_LIST_FROM_SELECT type STRING
      value(IV_NEW_SYNTAX) type ABAP_BOOL
    returning
      value(RT_FIELDS_IN_RESULT_SET) type STRING_TABLE .
  methods _CREATE_HASH_TABLE_TO_GROUP
    importing
      !IT_GROUP_BY_FIELDS type FIELDNAME_TABLE
    returning
      value(RD_GROUP_BY_HASH_TABLE) type ref to DATA .
  methods _GET_ALL_FIELDS_OF_DATASET
    returning
      value(RT_ALL_FIELDS_OF_DATASET) type FIELDNAME_TABLE .
  methods _GROUP_INTO_HASHED_TABLE
    importing
      !IT_GROUP_BY_FIELDS type FIELDNAME_TABLE
      !ID_REF_TO_HASH_GROUP_BY_TABLE type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods _RUN_GROUP_BY_FUNCTION
    importing
      !IT_TABLE_LINES_BY_KEY type STANDARD TABLE
    changing
      !CS_LINE_TO_GROUP type ANY
    raising
      ZCX_ZOSQL_ERROR .
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

      IF <ls_select_parameter>-field_alias IS NOT INITIAL.
        ASSIGN COMPONENT <ls_select_parameter>-field_alias OF STRUCTURE <ls_result> TO <lv_result_field_value>.
      ELSE.
        ASSIGN COMPONENT <ls_select_parameter>-field_name OF STRUCTURE <ls_result> TO <lv_result_field_value>.
      ENDIF.
      CHECK sy-subrc = 0.

      <lv_result_field_value> = <lv_dataset_field_value>.
    ENDLOOP.
  ENDMETHOD.


  method APPLY_DISTINCT.

    DATA: ld_hash_table_with_all_fields TYPE REF TO data.

    FIELD-SYMBOLS: <lt_hash_tab_with_all_fields> TYPE HASHED TABLE,
                   <lt_data_set>                 TYPE STANDARD TABLE,
                   <ls_data_set>                 TYPE any.

    ld_hash_table_with_all_fields = _create_hash_table_to_group( _get_all_fields_of_dataset( ) ).
    ASSIGN ld_hash_table_with_all_fields->* TO <lt_hash_tab_with_all_fields>.

    ASSIGN md_data_set->* TO <lt_data_set>.

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

    DATA: ld_hash_table_to_group TYPE REF TO data.

    ld_hash_table_to_group = _create_hash_table_to_group( it_group_by_fields ).

    _group_into_hashed_table( it_group_by_fields            = it_group_by_fields
                              id_ref_to_hash_group_by_table = ld_hash_table_to_group ).
  ENDMETHOD.


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


  METHOD INITIALIZE_BY_PARSED_SQL.

    CONSTANTS: lc_state_fieldname    TYPE i VALUE 0,
               lc_state_in_function  TYPE i VALUE 1,
               lc_state_expect_alias TYPE i VALUE 2.

    DATA: lo_sql_parser_helper        TYPE REF TO zcl_zosql_parser_helper,
          lt_nodes_with_select_fields TYPE zcl_zosql_parser_recurs_desc=>ty_tree,
          lt_nodes_of_field           TYPE zcl_zosql_parser_recurs_desc=>ty_tree,
          lv_state                    TYPE i,
          ls_parameter                TYPE ty_select_parameter.

    FIELD-SYMBOLS: <ls_node_select_field> TYPE zcl_zosql_parser_recurs_desc=>ty_node,
                   <ls_node_of_field>     TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    CREATE OBJECT lo_sql_parser_helper.
    lo_sql_parser_helper->get_key_nodes_of_sql_select( EXPORTING io_sql_parser                 = io_sql_parser
                                                       IMPORTING et_nodes_of_select_field_list = lt_nodes_with_select_fields ).

    LOOP AT lt_nodes_with_select_fields ASSIGNING <ls_node_select_field>.

      CLEAR ls_parameter.
      lt_nodes_of_field = io_sql_parser->get_child_nodes( <ls_node_select_field>-id ).
      INSERT <ls_node_select_field> INTO lt_nodes_of_field INDEX 1.

      lv_state = lc_state_fieldname.
      LOOP AT lt_nodes_of_field ASSIGNING <ls_node_of_field>.
        IF <ls_node_of_field>-token CP '*(*'.
          lv_state = lc_state_in_function.

          ls_parameter-function_name = <ls_node_of_field>-token.
          REPLACE FIRST OCCURRENCE OF '(' IN ls_parameter-function_name WITH space.
          CONDENSE ls_parameter-function_name.
          ls_parameter-function_name = zcl_zosql_utils=>to_upper_case( ls_parameter-function_name ).
          ls_parameter-parameter_type = parameter_type-groupby_function.

          CONTINUE.
        ELSEIF <ls_node_of_field>-token = ')'.
          lv_state = lc_state_fieldname.
          CONTINUE.
        ELSEIF <ls_node_of_field>-token_ucase = 'AS'.
          lv_state = lc_state_expect_alias.
          CONTINUE.
        ENDIF.

        CASE lv_state.
          WHEN lc_state_in_function
            OR lc_state_fieldname.

            _parse_field( EXPORTING iv_field     = <ls_node_of_field>-token
                          CHANGING  cs_parameter = ls_parameter ).

            IF lv_state = lc_state_fieldname.
              ls_parameter-parameter_type = parameter_type-field.
            ENDIF.
          WHEN lc_state_expect_alias.
            ls_parameter-field_alias = <ls_node_of_field>-token.
        ENDCASE.
      ENDLOOP.

      APPEND ls_parameter TO mt_select_parameters.
    ENDLOOP.

    _star_to_field_list( io_from_iterator ).
    _fill_dataset_where_empty( io_from_iterator ).
    _create_data_set_for_select( io_from_iterator ).
  ENDMETHOD.


  method _APPEND_TO_LAST_FIELD.

    DATA: lv_index_of_last_record TYPE i,
          lv_value_of_last_record TYPE string,
          lv_alias                TYPE string.

    IF ct_field_list IS INITIAL.
      RETURN.
    ENDIF.

    lv_alias = zcl_zosql_utils=>condense( iv_alias ).

    lv_index_of_last_record = LINES( ct_field_list ).
    READ TABLE ct_field_list INDEX lv_index_of_last_record INTO lv_value_of_last_record.

    IF zcl_zosql_utils=>check_starts_with_token( iv_sql   = lv_alias
                                                 iv_token = c_as ) = abap_true.

      CONCATENATE lv_value_of_last_record lv_alias INTO lv_value_of_last_record SEPARATED BY space.
    ELSE.
      CONCATENATE lv_value_of_last_record c_as lv_alias INTO lv_value_of_last_record SEPARATED BY space.
    ENDIF.

    MODIFY ct_field_list INDEX lv_index_of_last_record FROM lv_value_of_last_record.
  endmethod.


  METHOD _CREATE_DATA_SET_FOR_SELECT.

    DATA: lo_struct                TYPE REF TO cl_abap_structdescr,
          lt_target_set_components TYPE cl_abap_structdescr=>component_table,
          lo_table                 TYPE REF TO cl_abap_tabledescr,
          ls_new_component         TYPE LINE OF cl_abap_structdescr=>component_table.

    FIELD-SYMBOLS: <ls_select_parameter> LIKE LINE OF mt_select_parameters.

    LOOP AT mt_select_parameters ASSIGNING <ls_select_parameter>.

      IF <ls_select_parameter>-function_name = c_function_count.
        ls_new_component = _get_component_for_count( <ls_select_parameter>-field_alias ).
      ELSE.

        ls_new_component = _get_component_for_field( is_select_parameter = <ls_select_parameter>
                                                     io_from_iterator    = io_from_iterator ).
      ENDIF.
      APPEND ls_new_component TO lt_target_set_components.
    ENDLOOP.

    lo_struct = cl_abap_structdescr=>create( lt_target_set_components ).
    lo_table = cl_abap_tabledescr=>create( lo_struct ).

    CREATE DATA md_data_set TYPE HANDLE lo_table.
  ENDMETHOD.


  METHOD _CREATE_HASH_TABLE_TO_GROUP.

    DATA: lo_table_data_set      TYPE REF TO cl_abap_tabledescr,
          lo_struct_data_set     TYPE REF TO cl_abap_structdescr,
          lt_key                 TYPE abap_keydescr_tab,
          lo_group_by_hash_table TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <ls_group_by_field>   LIKE LINE OF it_group_by_fields,
                   <ls_key>              LIKE LINE OF lt_key.

    lo_table_data_set ?= cl_abap_tabledescr=>describe_by_data_ref( md_data_set ).
    lo_struct_data_set ?= lo_table_data_set->get_table_line_type( ).

    LOOP AT it_group_by_fields ASSIGNING <ls_group_by_field>.
      APPEND INITIAL LINE TO lt_key ASSIGNING <ls_key>.
      <ls_key>-name = <ls_group_by_field>-fieldname.
    ENDLOOP.

    lo_group_by_hash_table = cl_abap_tabledescr=>create( p_line_type  = lo_struct_data_set
                                                         p_table_kind = cl_abap_tabledescr=>tablekind_hashed
                                                         p_unique     = abap_true
                                                         p_key        = lt_key ).

    CREATE DATA rd_group_by_hash_table TYPE HANDLE lo_group_by_hash_table.
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


  method _GET_COMPONENT_FOR_COUNT.
    rs_component-name = iv_name.
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
  ENDMETHOD.


  METHOD _group_into_hashed_table.

    DATA: lv_fieldname         TYPE fieldname,
          ld_table_one_group   TYPE REF TO data,
          lv_same_key          TYPE abap_bool,
          ld_line_grouped_copy TYPE REF TO data.

    FIELD-SYMBOLS: <lt_table_before>      TYPE STANDARD TABLE,
                   <lt_table_grouped>     TYPE HASHED TABLE,
                   <ls_line_before>       TYPE any,
                   <ls_line_grouped>      TYPE any,
                   <lv_field_before>      TYPE any,
                   <lv_field_grouped>     TYPE any,
                   <lt_table_one_group>   TYPE STANDARD TABLE,
                   <ls_line_grouped_copy> TYPE any.

    ASSIGN id_ref_to_hash_group_by_table->* TO <lt_table_grouped>.

    _prepare_count_for_group_by( ).

    ASSIGN md_data_set->* TO <lt_table_before>.

    LOOP AT <lt_table_before> ASSIGNING <ls_line_before>.
      READ TABLE <lt_table_grouped> FROM <ls_line_before> ASSIGNING <ls_line_grouped>.
      IF sy-subrc <> 0.
        INSERT <ls_line_before> INTO TABLE <lt_table_grouped>.
      ENDIF.
    ENDLOOP.

    CREATE DATA ld_table_one_group LIKE <lt_table_before>.
    ASSIGN ld_table_one_group->* TO <lt_table_one_group>.

    CREATE DATA ld_line_grouped_copy LIKE LINE OF <lt_table_grouped>.
    ASSIGN ld_line_grouped_copy->* TO <ls_line_grouped_copy>.

    LOOP AT <lt_table_grouped> ASSIGNING <ls_line_grouped>.

      REFRESH <lt_table_one_group>.
      LOOP AT <lt_table_before> ASSIGNING <ls_line_before>.

        lv_same_key = abap_true.
        LOOP AT it_group_by_fields INTO lv_fieldname.
          ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_line_before>  TO <lv_field_before>.
          ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_line_grouped> TO <lv_field_grouped>.

          IF <lv_field_before> <> <lv_field_grouped>.
            lv_same_key = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.

        CHECK lv_same_key = abap_true.

        APPEND <ls_line_before> TO <lt_table_one_group>.
      ENDLOOP.

      <ls_line_grouped_copy> = <ls_line_grouped>.
      _run_group_by_function( EXPORTING it_table_lines_by_key = <lt_table_one_group>
                              CHANGING  cs_line_to_group      = <ls_line_grouped_copy> ).

      IF <ls_line_grouped_copy> <> <ls_line_grouped>.
        MODIFY TABLE <lt_table_grouped> FROM <ls_line_grouped_copy>.
      ENDIF.
    ENDLOOP.

    <lt_table_before> = <lt_table_grouped>.
  ENDMETHOD.


  method _IS_FUNCTION.

    IF iv_field CP '*(*)*'.
      rv_is_function = abap_true.
    ENDIF.
  endmethod.


  method _IS_FUNCTION_NAME.

    IF iv_token CP '*('.
      rv_is_function_name = abap_true.
    ENDIF.
  endmethod.


  METHOD _PARSE_FIELD.

    DATA: lv_dataset_name_or_alias TYPE string,
          lv_field_name            TYPE string.

    SPLIT iv_field AT '~' INTO lv_dataset_name_or_alias lv_field_name.
    IF lv_field_name IS INITIAL.
      lv_field_name = lv_dataset_name_or_alias.
      CLEAR lv_dataset_name_or_alias.
    ENDIF.

    cs_parameter-dataset_name = zcl_zosql_utils=>condense( zcl_zosql_utils=>to_upper_case( lv_dataset_name_or_alias ) ).
    cs_parameter-field_name   = zcl_zosql_utils=>condense( zcl_zosql_utils=>to_upper_case( lv_field_name ) ).
  ENDMETHOD.


  method _PARSE_FUNCTION.

    DATA: ltd_parts TYPE TABLE OF string,
          lv_part   TYPE string.

    SPLIT iv_field AT space INTO TABLE ltd_parts.

    LOOP AT ltd_parts INTO lv_part.
      IF _is_function_name( lv_part ) = abap_true.
        cs_parameter-function_name = zcl_zosql_utils=>to_upper_case( lv_part ).
        REPLACE FIRST OCCURRENCE OF '(' IN cs_parameter-function_name WITH space.
        CONDENSE cs_parameter-function_name.
        cs_parameter-parameter_type = parameter_type-groupby_function.
      ELSEIF lv_part = ')'.
        EXIT.
      ELSE.
        _parse_field( EXPORTING iv_field     = lv_part
                      CHANGING  cs_parameter = cs_parameter ).
      ENDIF.
    ENDLOOP.
  endmethod.


  METHOD _PREPARE_COUNT_FOR_GROUP_BY.

    FIELD-SYMBOLS: <lt_table>     TYPE STANDARD TABLE,
                   <ls_select_parameter> LIKE LINE OF mt_select_parameters,
                   <ls_table_line>     TYPE any,
                   <lv_count_field>    TYPE i.

    READ TABLE mt_select_parameters WITH KEY function_name = c_function_count TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN md_data_set->* TO <lt_table>.

    LOOP AT <lt_table> ASSIGNING <ls_table_line>.
      LOOP AT mt_select_parameters ASSIGNING <ls_select_parameter>
        WHERE function_name = c_function_count.

        ASSIGN COMPONENT <ls_select_parameter>-field_alias OF STRUCTURE <ls_table_line> TO <lv_count_field>.
        IF sy-subrc = 0.
          <lv_count_field> = 1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD _run_group_by_function.

    DATA: lv_fieldname TYPE fieldname.

    FIELD-SYMBOLS: <ls_select_parameter>       LIKE LINE OF mt_select_parameters,
                   <lv_grouped_value>          TYPE any,
                   <ls_table_line_not_grouped> TYPE any,
                   <lv_not_grouped_value>      TYPE any.

    LOOP AT mt_select_parameters ASSIGNING <ls_select_parameter>
      WHERE parameter_type = parameter_type-groupby_function.

      IF <ls_select_parameter>-field_alias IS NOT INITIAL.
        lv_fieldname = <ls_select_parameter>-field_alias.
      ELSE.
        lv_fieldname = <ls_select_parameter>-field_name.
      ENDIF.
      lv_fieldname = zcl_zosql_utils=>to_upper_case( lv_fieldname ).

      ASSIGN COMPONENT lv_fieldname OF STRUCTURE cs_line_to_group TO <lv_grouped_value>.
      CLEAR <lv_grouped_value>.

      LOOP AT it_table_lines_by_key ASSIGNING <ls_table_line_not_grouped>.
        ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_table_line_not_grouped> TO <lv_not_grouped_value>.

        CASE zcl_zosql_utils=>to_upper_case( <ls_select_parameter>-function_name ).
          WHEN 'SUM'.
            <lv_grouped_value> = <lv_grouped_value> + <lv_not_grouped_value>.
          WHEN 'MIN'.
            IF <lv_not_grouped_value> < <lv_grouped_value> OR <lv_grouped_value> IS INITIAL.
              <lv_grouped_value> = <lv_not_grouped_value>.
            ENDIF.
          WHEN 'MAX'.
            IF <lv_not_grouped_value> > <lv_grouped_value> OR <lv_grouped_value> IS INITIAL.
              <lv_grouped_value> = <lv_not_grouped_value>.
            ENDIF.
          WHEN c_function_count.
            <lv_grouped_value> = <lv_grouped_value> + 1.
          WHEN c_function_avg.
            <lv_grouped_value> = <lv_grouped_value> + <lv_not_grouped_value>.
          WHEN OTHERS.
            MESSAGE e060 WITH <ls_select_parameter>-function_name INTO zcl_zosql_utils=>dummy.
            zcl_zosql_utils=>raise_exception_from_sy_msg( ).
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    LOOP AT mt_select_parameters ASSIGNING <ls_select_parameter>
      WHERE parameter_type = parameter_type-groupby_function
        AND function_name  = c_function_avg.

      ASSIGN COMPONENT lv_fieldname OF STRUCTURE cs_line_to_group TO <lv_grouped_value>.
      <lv_grouped_value> = <lv_grouped_value> / LINES( it_table_lines_by_key ).
    ENDLOOP.
  ENDMETHOD.


  METHOD _SEPARATE_ALIAS.

    DATA: lt_tokens        TYPE TABLE OF string,
          lv_token         TYPE string,
          lv_alias_started TYPE abap_bool.

    lt_tokens = zcl_zosql_utils=>split_condition_into_tokens( iv_whole_expression ).
    CLEAR: ev_expression_without_alias, ev_alias.

    LOOP AT lt_tokens INTO lv_token.

      IF zcl_zosql_utils=>to_upper_case( lv_token ) = 'AS'.
        lv_alias_started = abap_true.
        CONTINUE.
      ENDIF.

      IF lv_alias_started = abap_true.
        ev_alias = lv_token.
      ELSE.
        CONCATENATE ev_expression_without_alias lv_token INTO ev_expression_without_alias
          SEPARATED BY space.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD _SPLIT_SELECT_INTO_FIELDS.

    DATA: lv_field_after_split         TYPE string,
          lv_field                     TYPE string,
          lv_alias_expected            TYPE abap_bool,
          lt_fields_in_result_set_copy LIKE rt_fields_in_result_set,
          lv_inside_function           TYPE abap_bool,
          lv_add_field                 TYPE abap_bool,
          lv_append_to_last_field      TYPE abap_bool.

    IF iv_new_syntax = abap_true.
      SPLIT iv_field_list_from_select AT ',' INTO TABLE rt_fields_in_result_set.
    ELSE.
      rt_fields_in_result_set = zcl_zosql_utils=>split_condition_into_tokens( iv_field_list_from_select ).

      lt_fields_in_result_set_copy = rt_fields_in_result_set.
      REFRESH rt_fields_in_result_set.

      LOOP AT lt_fields_in_result_set_copy INTO lv_field_after_split.

        lv_add_field            = abap_true.
        lv_append_to_last_field = abap_false.

        IF zcl_zosql_utils=>to_upper_case( lv_field_after_split ) = c_as.
          lv_alias_expected = abap_true.
          lv_add_field = abap_false.
        ELSEIF lv_field_after_split CP '*(*'.
          lv_inside_function = abap_true.
          lv_add_field = abap_false.
        ELSEIF lv_field_after_split = ')'.
          lv_inside_function = abap_false.
        ELSEIF lv_inside_function = abap_true.
          lv_add_field = abap_false.
        ELSEIF lv_alias_expected = abap_true.
          lv_append_to_last_field = abap_true.
          lv_add_field            = abap_false.
          lv_alias_expected       = abap_false.
        ENDIF.

        CONCATENATE lv_field lv_field_after_split INTO lv_field SEPARATED BY space.

        IF lv_add_field = abap_true.
          IF lv_field IS NOT INITIAL.
            APPEND zcl_zosql_utils=>condense( lv_field ) TO rt_fields_in_result_set.
          ENDIF.

          CLEAR lv_field.
        ELSEIF lv_append_to_last_field = abap_true.
          _append_to_last_field( EXPORTING iv_alias      = lv_field
                                 CHANGING  ct_field_list = rt_fields_in_result_set ).
          CLEAR lv_field.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


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
