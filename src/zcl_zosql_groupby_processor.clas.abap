class ZCL_ZOSQL_GROUPBY_PROCESSOR definition
  public
  inheriting from ZCL_ZOSQL_AGGR_FUNC_PROCESSOR
  create public .

public section.

  data MT_GROUP_BY_KEY_FIELDS type TY_GROUP_BY_KEY_FIELDS read-only .

  methods CONSTRUCTOR
    importing
      !IO_HAVING_PROCESSOR type ref to ZIF_ZOSQL_EXPRESSION_PROCESSOR optional .
  methods APPLY_GROUP_BY
    importing
      !IO_SELECT type ref to ZCL_ZOSQL_SELECT_PROCESSOR
      !IT_SELECT_PARAMETERS type ZCL_ZOSQL_SELECT_PROCESSOR=>TY_SELECT_PARAMETERS
      !IO_HAVING type ref to ZCL_ZOSQL_HAVING_PROCESSOR optional
    changing
      !CT_DATA_SET type STANDARD TABLE
    raising
      ZCX_ZOSQL_ERROR .
  methods INITIALIZE_BY_PARSED_SQL
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IV_GROUP_BY_NODE_ID type I
      !IO_FROM_ITERATOR type ref to ZCL_ZOSQL_FROM_ITERATOR .
protected section.
private section.

  data MO_HAVING_PROCESSOR type ref to ZIF_ZOSQL_EXPRESSION_PROCESSOR .

  methods _ADD_RECORD_TO_GROUPED_TABLE
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_GROUPBY_ITER_POS
      !IT_SELECT_PARAMETERS type ZCL_ZOSQL_SELECT_PROCESSOR=>TY_SELECT_PARAMETERS
    changing
      !CT_GROUPED_TABLE type STANDARD TABLE
    raising
      ZCX_ZOSQL_ERROR .
  methods _CHECK_CONDITIONS_WITH_HAVING
    importing
      !IS_CURRENT_LINE_OF_GROUPBY_TAB type ANY
      !IT_DATA_SET type STANDARD TABLE
    returning
      value(RV_HAVING_CONDITIONS_TRUE) type ABAP_BOOL .
  methods _GET_DATA_SET_LINE_FOR_GROUP
    importing
      !IS_STRUCT_WITH_GROUP_BY_KEYS type ANY
      !IT_DATA_SET type STANDARD TABLE
    exporting
      !ET_DATA_SUBSET_FOR_GROUP type STANDARD TABLE .
  methods _ADD_KEYS_WITHOUT_AGGR_FUNCS
    importing
      !ID_REF_TO_HASH_GROUP_BY_TABLE type ref to DATA
      !IT_DATA_SET type STANDARD TABLE .
  methods _FILL_FIELDS_WITH_AGGR_FUNCS
    importing
      !IT_FIELDS_WITH_AGGR_FUNC type TY_FIELDS_WITH_AGGR_FUNC
      !IT_DATA_SET type STANDARD TABLE
    changing
      !CT_GROUP_BY_TABLE type STANDARD TABLE
    raising
      ZCX_ZOSQL_ERROR .
  methods _FILL_DATASET_WHERE_EMPTY
    importing
      !IO_FROM_ITERATOR type ref to ZCL_ZOSQL_FROM_ITERATOR
    raising
      ZCX_ZOSQL_ERROR .
ENDCLASS.



CLASS ZCL_ZOSQL_GROUPBY_PROCESSOR IMPLEMENTATION.


  METHOD apply_group_by.

    DATA: lo_groupby_iterator        TYPE REF TO zcl_zosql_groupby_iterator,
          lt_group_by_key_field_list TYPE zcl_zosql_groupby_processor=>ty_group_by_key_fields,
          lv_not_end_of_data         TYPE abap_bool,
          lo_iter_pos                TYPE REF TO zcl_zosql_groupby_iter_pos,
          lv_add_record              TYPE abap_bool,
          ld_grouped_table           TYPE REF TO data.

    FIELD-SYMBOLS: <lt_grouped_table> TYPE STANDARD TABLE.

    CREATE DATA ld_grouped_table LIKE ct_data_set.
    ASSIGN ld_grouped_table->* TO <lt_grouped_table>.

    CREATE OBJECT lo_groupby_iterator.

    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = mt_group_by_key_fields
                                               IMPORTING et_table_dest = lt_group_by_key_field_list ).

    lo_groupby_iterator->init_groupby_iterator( it_group_by_key_fields = lt_group_by_key_field_list
                                                io_select              = io_select ).

    lv_not_end_of_data = lo_groupby_iterator->zif_zosql_iterator~move_to_first( ).

    WHILE lv_not_end_of_data = abap_true.

      lo_iter_pos ?= lo_groupby_iterator->zif_zosql_iterator~get_iterator_position_object( ).

      lv_add_record = abap_true.
      IF mo_having_processor IS BOUND.
        lv_add_record = mo_having_processor->check_condition_for_cur_rec( lo_iter_pos ).
      ENDIF.

      IF lv_add_record = abap_true.
        _add_record_to_grouped_table( EXPORTING it_select_parameters  = it_select_parameters
                                                io_iteration_position = lo_iter_pos
                                      CHANGING  ct_grouped_table      = <lt_grouped_table> ).
      ENDIF.

      lv_not_end_of_data = lo_groupby_iterator->zif_zosql_iterator~move_to_next( ).
    ENDWHILE.

    ct_data_set = <lt_grouped_table>.


*    DATA: ld_group_by_hash_table     TYPE REF TO data,
*          ld_group_by_standard_table TYPE REF TO data,
*          lt_group_by_key_field_list TYPE fieldname_table.
*
*    FIELD-SYMBOLS: <lt_grouped_table_standard> TYPE STANDARD TABLE,
*                   <lt_grouped_table_hashed>   TYPE HASHED TABLE.
*
*    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = mt_group_by_key_fields
*                                               IMPORTING et_table_dest = lt_group_by_key_field_list ).
*    ld_group_by_hash_table =
*      zcl_zosql_utils=>create_hash_tab_like_standard(
*        it_standard_table_template = ct_data_set
*        it_key_fields              = lt_group_by_key_field_list ).
*
*    _add_keys_without_aggr_funcs( id_ref_to_hash_group_by_table = ld_group_by_hash_table
*                                  it_data_set                   = ct_data_set ).
*
*    CREATE DATA ld_group_by_standard_table LIKE ct_data_set.
*    ASSIGN ld_group_by_standard_table->* TO <lt_grouped_table_standard>.
*
*    ASSIGN ld_group_by_hash_table->* TO <lt_grouped_table_hashed>.
*    <lt_grouped_table_standard> = <lt_grouped_table_hashed>.
*
*    _fill_fields_with_aggr_funcs( EXPORTING it_fields_with_aggr_func = it_fields_with_aggr_func
*                                            it_data_set              = ct_data_set
*                                  CHANGING  ct_group_by_table        = <lt_grouped_table_standard> ).
*
*    ct_data_set = <lt_grouped_table_standard>.
  ENDMETHOD.


  method CONSTRUCTOR.
    super->constructor( ).
    mo_having_processor = io_having_processor.
  endmethod.


  METHOD initialize_by_parsed_sql.

    DATA: lt_child_nodes_of_group_by TYPE zcl_zosql_parser_recurs_desc=>ty_tree.

    FIELD-SYMBOLS: <ls_child_node>         TYPE zcl_zosql_parser_recurs_desc=>ty_node,
                   <ls_group_by_key_field> LIKE LINE OF mt_group_by_key_fields.

    lt_child_nodes_of_group_by = io_sql_parser->get_child_nodes( iv_group_by_node_id ).

    LOOP AT lt_child_nodes_of_group_by ASSIGNING <ls_child_node>
      WHERE node_type = zcl_zosql_parser_recurs_desc=>node_type-select_field.

      APPEND INITIAL LINE TO mt_group_by_key_fields ASSIGNING <ls_group_by_key_field>.
      FIND FIRST OCCURRENCE OF '~' IN <ls_child_node>-token.
      IF sy-subrc = 0.
        SPLIT <ls_child_node>-token_ucase AT '~' INTO <ls_group_by_key_field>-dataset_name_or_alias <ls_group_by_key_field>-fieldname.
      ELSE.
        <ls_group_by_key_field>-fieldname = <ls_child_node>-token_ucase.
      ENDIF.
    ENDLOOP.

    _fill_dataset_where_empty( io_from_iterator ).
  ENDMETHOD.


  method _ADD_KEYS_WITHOUT_AGGR_FUNCS.

    FIELD-SYMBOLS: <lt_group_by_table_hashed> TYPE HASHED TABLE,
                   <ls_table_line>            TYPE any.

    ASSIGN id_ref_to_hash_group_by_table->* TO <lt_group_by_table_hashed>.

    LOOP AT it_data_set ASSIGNING <ls_table_line>.
      READ TABLE <lt_group_by_table_hashed> FROM <ls_table_line> TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        INSERT <ls_table_line> INTO TABLE <lt_group_by_table_hashed>.
      ENDIF.
    ENDLOOP.
  endmethod.


  method _ADD_RECORD_TO_GROUPED_TABLE.

    DATA: ld_value TYPE REF TO data.

    FIELD-SYMBOLS: <ls_select_parameter> LIKE LINE OF it_select_parameters,
                   <ls_new_line>         TYPE any,
                   <lv_field_value>      TYPE any,
                   <lv_value>            TYPE any.

    APPEND INITIAL LINE TO ct_grouped_table ASSIGNING <ls_new_line>.

    LOOP AT it_select_parameters ASSIGNING <ls_select_parameter>.

      ASSIGN COMPONENT <ls_select_parameter>-field_name OF STRUCTURE <ls_new_line> TO <lv_field_value>.
      IF sy-subrc <> 0.
        ASSIGN COMPONENT <ls_select_parameter>-field_name_in_result OF STRUCTURE <ls_new_line>
          TO <lv_field_value>.
        IF sy-subrc <> 0.
          MESSAGE e057 WITH <ls_select_parameter>-field_name INTO zcl_zosql_utils=>dummy.
          zcl_zosql_utils=>raise_exception_from_sy_msg( ).
        ENDIF.
      ENDIF.

      IF <ls_select_parameter>-function_name IS NOT INITIAL.
        ld_value = io_iteration_position->get_grouped_value_of_data_set(
          iv_dataset_name_or_alias = <ls_select_parameter>-dataset_name
          iv_fieldname             = <ls_select_parameter>-field_name
          iv_groupby_function      = <ls_select_parameter>-function_name
          iv_distinct              = <ls_select_parameter>-distinct_flag ).

      ELSE.
        ld_value = io_iteration_position->get_field_ref_of_data_set(
          iv_dataset_name_or_alias = <ls_select_parameter>-dataset_name
          iv_fieldname             = <ls_select_parameter>-field_name ).
      ENDIF.

      IF ld_value IS BOUND.
        ASSIGN ld_value->* TO <lv_value>.
        <lv_field_value> = <lv_value>.
      ENDIF.
    ENDLOOP.
  endmethod.


  method _CHECK_CONDITIONS_WITH_HAVING.
    IF mo_having_processor IS NOT BOUND.
      rv_having_conditions_true = abap_true.
    ENDIF.
  endmethod.


  METHOD _FILL_DATASET_WHERE_EMPTY.

    DATA: lt_data_set_list          TYPE zcl_zosql_iterator_position=>ty_data_sets,
          ls_data_set               LIKE LINE OF lt_data_set_list,
          lt_components_of_data_set TYPE cl_abap_structdescr=>component_table.

    FIELD-SYMBOLS: <ls_group_by_key_field> LIKE LINE OF mt_group_by_key_fields.

    lt_data_set_list = io_from_iterator->get_data_set_list( ).

    LOOP AT mt_group_by_key_fields ASSIGNING <ls_group_by_key_field>
      WHERE dataset_name_or_alias IS INITIAL.

      LOOP AT lt_data_set_list INTO ls_data_set.
        lt_components_of_data_set = io_from_iterator->get_components_of_data_set( ls_data_set-dataset_name ).
        READ TABLE lt_components_of_data_set WITH KEY name = <ls_group_by_key_field>-fieldname TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <ls_group_by_key_field>-dataset_name_or_alias = ls_data_set-dataset_name.
          CONTINUE.
        ENDIF.
      ENDLOOP.

      IF <ls_group_by_key_field>-dataset_name_or_alias IS INITIAL.
        MESSAGE e057 WITH <ls_group_by_key_field>-fieldname INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  method _FILL_FIELDS_WITH_AGGR_FUNCS.

    DATA: ld_table_one_group         TYPE REF TO data.

    FIELD-SYMBOLS: <lt_table_one_group> TYPE STANDARD TABLE,
                   <ls_group_by_line>   TYPE any.

    CREATE DATA ld_table_one_group LIKE it_data_set.
    ASSIGN ld_table_one_group->* TO <lt_table_one_group>.

    LOOP AT ct_group_by_table ASSIGNING <ls_group_by_line>.
      _get_data_set_line_for_group( EXPORTING is_struct_with_group_by_keys = <ls_group_by_line>
                                              it_data_set                  = it_data_set
                                    IMPORTING et_data_subset_for_group     = <lt_table_one_group> ).

      fill_aggregation_functions( EXPORTING it_table_lines_by_key    = <lt_table_one_group>
                                            it_fields_with_aggr_func = it_fields_with_aggr_func
                                  CHANGING  cs_result_line           = <ls_group_by_line> ).

      IF _check_conditions_with_having( is_current_line_of_groupby_tab = <ls_group_by_line>
                                        it_data_set                    = it_data_set ) <> abap_true.
        DELETE ct_group_by_table.
      ENDIF.
    ENDLOOP.
  endmethod.


  METHOD _get_data_set_line_for_group.

    DATA: lv_same_key  TYPE abap_bool,
          lv_fieldname TYPE fieldname.

    FIELD-SYMBOLS: <ls_data_set_line>  TYPE any,
                   <lv_field_data_set> TYPE any,
                   <lv_field_key>      TYPE any,
                   <ls_group_by_key_field> LIKE LINE OF mt_group_by_key_fields.

    REFRESH et_data_subset_for_group.
    LOOP AT it_data_set ASSIGNING <ls_data_set_line>.

      lv_same_key = abap_true.
      LOOP AT mt_group_by_key_fields ASSIGNING <ls_group_by_key_field>.
        ASSIGN COMPONENT <ls_group_by_key_field>-fieldname OF STRUCTURE <ls_data_set_line>  TO <lv_field_data_set>.
        ASSIGN COMPONENT <ls_group_by_key_field>-fieldname OF STRUCTURE is_struct_with_group_by_keys TO <lv_field_key>.

        IF <lv_field_data_set> <> <lv_field_key>.
          lv_same_key = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.

      CHECK lv_same_key = abap_true.

      APPEND <ls_data_set_line> TO et_data_subset_for_group.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
