class ZCL_ZOSQL_FROM_ITERATOR definition
  public
  create public .

public section.

  interfaces ZIF_ZOSQL_ITERATOR .

  types:
    BEGIN OF TY_DATA_SET,
           dataset_name TYPE string,
           dataset_alias TYPE string,
         END OF ty_data_set .
  types:
    ty_data_sets TYPE STANDARD TABLE OF ty_data_set WITH DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      !IO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT optional
      !IO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS optional
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC optional
    raising
      ZCX_ZOSQL_ERROR .
  methods GET_DATA_SET_LIST
    returning
      value(RT_DATA_SET_LIST) type TY_DATA_SETS .
  methods GET_LINE_FOR_DATA_SET_REF
    importing
      !IV_DATASET_NAME_OR_ALIAS type CLIKE
    returning
      value(RD_REF_TO_LINE) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods GET_COMPONENTS_OF_DATA_SET
    importing
      !IV_DATASET_NAME_OR_ALIAS type CLIKE
    returning
      value(RT_DATA_SET_COMPONENTS) type CL_ABAP_STRUCTDESCR=>INCLUDED_VIEW
    raising
      ZCX_ZOSQL_ERROR .
protected section.
private section.

  types:
    BEGIN OF ty_record_statistic,
      record_unique_id       TYPE zosql_hash,
      was_inner_join_right   TYPE abap_bool,
      was_inner_joined_left  TYPE abap_bool,
      was_empty_record_added TYPE abap_bool,
    END OF ty_record_statistic .
  types:
    ty_records_statistic TYPE STANDARD TABLE OF ty_record_statistic WITH KEY record_unique_id .
  types:
    BEGIN OF ty_dataset_with_position,
      dataset_name      TYPE string,
      dataset_alias     TYPE string,
      type_of_join      TYPE i,
      iterator          TYPE REF TO zif_zosql_iterator,
      records_statistic TYPE ty_records_statistic,
    END OF ty_dataset_with_position .
  types:
    ty_datasets_with_position TYPE STANDARD TABLE OF ty_dataset_with_position
    WITH KEY dataset_name dataset_alias .
  types:
    BEGIN OF ty_join_condition,
      left_dataset_name    TYPE string,
      left_dataset_alias   TYPE string,
      right_dataset_name   TYPE string,
      right_dataset_alias  TYPE string,
      type_of_join         TYPE i,
      expression_processor TYPE REF TO zif_zosql_expression_processor,
    END OF ty_join_condition .
  types:
    ty_join_conditions TYPE STANDARD TABLE OF ty_join_condition .
  types:
    BEGIN OF ty_outer_join_add_iter,
      from_iterator TYPE REF TO zif_zosql_iterator,
    END OF ty_outer_join_add_iter .

  data MO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT .
  data MT_DATASETS_WITH_POSITION type TY_DATASETS_WITH_POSITION .
  data MT_JOIN_CONDITIONS type TY_JOIN_CONDITIONS .
  constants C_INNER_JOIN type I value 0 ##NO_TEXT.
  constants C_LEFT_JOIN type I value 1 ##NO_TEXT.
  constants C_RIGHT_JOIN type I value 2 ##NO_TEXT.
  data MV_OUTER_JOIN_ADD_MODE type ABAP_BOOL .
  data:
    mt_outer_join_add_chain TYPE STANDARD TABLE OF ty_outer_join_add_iter WITH DEFAULT KEY .
  data MV_OUTER_JOIN_ADD_MODE_INDEX type I .
  data MO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS .

  methods _POSITIONS_AS_STRING
    returning
      value(RV_POSITIONS_AS_STRING) type STRING .
  methods _CREATE_DATASET_ITERATOR
    importing
      !IV_DATASET_NAME type CLIKE
    returning
      value(RO_ITERATOR) type ref to ZIF_ZOSQL_ITERATOR
    raising
      ZCX_ZOSQL_ERROR .
  methods _GET_DATASET_NAME_AND_ALIAS
    importing
      !IO_PARENT_NODE_OF_JOIN type ref to ZCL_ZOSQL_PARSER_NODE
    exporting
      !EV_NAME type CLIKE
      !EV_ALIAS type CLIKE .
  methods _ADD_ANOTHER_JOIN
    importing
      !IO_PARENT_NODE_OF_JOIN type ref to ZCL_ZOSQL_PARSER_NODE
    raising
      ZCX_ZOSQL_ERROR .
  methods _ADD_LEFT_JOIN_ITER
    importing
      !IS_JOIN_CONDITION type TY_JOIN_CONDITION
    raising
      ZCX_ZOSQL_ERROR .
  methods _CHECK_CONDITIONS_CURRENT_POS
    returning
      value(RV_CONDITIONS_ARE_TRUE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _INIT_BY_SQL_PARSER
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    raising
      ZCX_ZOSQL_ERROR .
  methods _INIT_BY_ATTRIBUTES
    importing
      !IT_DATASETS_WITH_POSITION type TY_DATASETS_WITH_POSITION
      !IT_JOIN_CONDITIONS type TY_JOIN_CONDITIONS .
  methods _INIT_OUTER_JOIN_ADD_MODE
    returning
      value(RV_OUTER_JOIN_FOUND) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _FILL_RECORD_STATISTIC
    raising
      ZCX_ZOSQL_ERROR .
  methods _CONSIDER_TYPE_OF_JOIN
    importing
      !IO_PARENT_NODE_OF_JOIN type ref to ZCL_ZOSQL_PARSER_NODE .
  methods _ADD_JOIN_CONDITION
    importing
      !IO_PARENT_NODE_OF_JOIN type ref to ZCL_ZOSQL_PARSER_NODE
    raising
      ZCX_ZOSQL_ERROR .
  methods _CONDITIONS_FAIL_FOR_ALL_LINES
    returning
      value(RV_COND_FAIL_ALL_LINES) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _FIND_NEXT_POSITION_FOR_CONDIT
    returning
      value(RV_NEXT_POSITION_FOUND) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _NEXT_POS_OUTER_JOIN_ADD_MODE
    returning
      value(RV_NEXT_POSITION_FOUND) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _ADD_DATASET
    importing
      !IO_PARENT_NODE_OF_JOIN type ref to ZCL_ZOSQL_PARSER_NODE
    raising
      ZCX_ZOSQL_ERROR .
  methods _MOVE_TO_NEXT_POSITION
    returning
      value(RV_MOVE_SUCCESSFUL) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _RESET_POSITION
    returning
      value(RV_FIRST_RECORD_SELECT_SUCCESS) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
ENDCLASS.



CLASS ZCL_ZOSQL_FROM_ITERATOR IMPLEMENTATION.


  METHOD constructor.

    DATA: lo_factory          TYPE REF TO zif_zosql_factory,
          lt_parameters_empty TYPE zosql_db_layer_params.

    IF io_parameters IS BOUND.
      mo_parameters = io_parameters.
    ELSE.
      CREATE OBJECT mo_parameters
        EXPORTING
          it_parameters = lt_parameters_empty.
    ENDIF.

    IF io_zosql_test_environment IS BOUND.
      mo_zosql_test_environment = io_zosql_test_environment.
    ELSE.
      CREATE OBJECT lo_factory TYPE zcl_zosql_factory.
      mo_zosql_test_environment = lo_factory->get_test_environment( ).
    ENDIF.

    IF io_sql_parser IS BOUND.
      _init_by_sql_parser( io_sql_parser ).
    ENDIF.
  ENDMETHOD.


  method GET_COMPONENTS_OF_DATA_SET.

    DATA: ld_ref_to_dataset_data TYPE REF TO data,
          lo_struct              TYPE REF TO cl_abap_structdescr.

    ld_ref_to_dataset_data = get_line_for_data_set_ref( iv_dataset_name_or_alias ).
    lo_struct ?= cl_abap_structdescr=>describe_by_data_ref( ld_ref_to_dataset_data ).
    rt_data_set_components = lo_struct->get_included_view( ).
  endmethod.


  method GET_DATA_SET_LIST.

    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = mt_datasets_with_position
                                               IMPORTING et_table_dest = rt_data_set_list ).

  endmethod.


  method GET_LINE_FOR_DATA_SET_REF.

    FIELD-SYMBOLS: <ls_data_set_data> LIKE LINE OF mt_datasets_with_position,
                   <ls_data_set_line> TYPE any.

    READ TABLE mt_datasets_with_position WITH KEY dataset_name = iv_dataset_name_or_alias ASSIGNING <ls_data_set_data>.
    IF sy-subrc <> 0.
      READ TABLE mt_datasets_with_position WITH KEY dataset_alias = iv_dataset_name_or_alias ASSIGNING <ls_data_set_data>.
    ENDIF.

    IF <ls_data_set_data> IS ASSIGNED.
      rd_ref_to_line = <ls_data_set_data>-iterator->create_empty_record_as_ref( ).
    ELSE.
      MESSAGE e075 WITH iv_dataset_name_or_alias INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~GET_CURRENT_RECORD_UNIQUE_ID.
    DATA: lv_unique_id               TYPE zosql_hash,
          lv_unique_ids_concatenated TYPE string.

    FIELD-SYMBOLS: <ls_dataset_with_position> LIKE LINE OF mt_datasets_with_position.

    LOOP AT mt_datasets_with_position ASSIGNING <ls_dataset_with_position>.

      lv_unique_id = <ls_dataset_with_position>-iterator->get_current_record_unique_id( ).
      CONCATENATE lv_unique_ids_concatenated lv_unique_id INTO lv_unique_ids_concatenated SEPARATED BY ','.
    ENDLOOP.

    CONDENSE lv_unique_ids_concatenated.

    IF strlen( lv_unique_ids_concatenated ) > 32.

      CALL FUNCTION 'MD5_CALCULATE_HASH_FOR_CHAR'
        EXPORTING
          data           = lv_unique_ids_concatenated
        IMPORTING
          hash           = rv_unique_id
        EXCEPTIONS
          no_data        = 1
          internal_error = 2
          OTHERS         = 3.

      IF sy-subrc <> 0.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.
    ELSE.
      rv_unique_id = lv_unique_ids_concatenated.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~GET_ITERATOR_POSITION_OBJECT.
    DATA: ld_ref_to_line          TYPE REF TO data,
          ls_outer_join_add_elem  LIKE LINE OF mt_outer_join_add_chain,
          lt_data_sets_outer_join TYPE zcl_zosql_iterator_position=>ty_data_sets.

    FIELD-SYMBOLS: <ls_dataset_with_position> LIKE LINE OF mt_datasets_with_position,
                   <lt_data_lines>            TYPE STANDARD TABLE.

    CREATE OBJECT ro_iterator_pos.

    IF mv_outer_join_add_mode = abap_true.

      READ TABLE mt_outer_join_add_chain INDEX mv_outer_join_add_mode_index INTO ls_outer_join_add_elem.
      ro_iterator_pos = ls_outer_join_add_elem-from_iterator->get_iterator_position_object( ).
      lt_data_sets_outer_join = ro_iterator_pos->get_data_set_list( ).

      LOOP AT mt_datasets_with_position ASSIGNING <ls_dataset_with_position>.
        READ TABLE lt_data_sets_outer_join WITH KEY dataset_name  = <ls_dataset_with_position>-dataset_name
                                                    dataset_alias = <ls_dataset_with_position>-dataset_alias
                                                    TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          ld_ref_to_line = <ls_dataset_with_position>-iterator->create_empty_record_as_ref( ).
          ro_iterator_pos->add_data_set_data( iv_dataset_name        = <ls_dataset_with_position>-dataset_name
                                              iv_dataset_alias       = <ls_dataset_with_position>-dataset_alias
                                              id_ref_to_current_line = ld_ref_to_line ).
        ENDIF.
      ENDLOOP.


    ELSE.
      LOOP AT mt_datasets_with_position ASSIGNING <ls_dataset_with_position>.

        ld_ref_to_line = <ls_dataset_with_position>-iterator->get_current_record_ref( ).

        ro_iterator_pos->add_data_set_data( iv_dataset_name        = <ls_dataset_with_position>-dataset_name
                                            iv_dataset_alias       = <ls_dataset_with_position>-dataset_alias
                                            id_ref_to_current_line = ld_ref_to_line ).
      ENDLOOP.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~IS_EMPTY.
    IF _conditions_fail_for_all_lines( ) = abap_true.
      rv_is_empty = abap_true.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~MOVE_TO_FIRST.
    rv_successful_move = _reset_position( ).
    IF rv_successful_move = abap_true.
      rv_successful_move = _find_next_position_for_condit( ).
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~MOVE_TO_NEXT.
    rv_successful_move = _move_to_next_position( ).

    IF rv_successful_move = abap_true.
      rv_successful_move = _find_next_position_for_condit( ).
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~RECORD_IS_LAST.
    FIELD-SYMBOLS: <ls_dataset_with_position> LIKE LINE OF mt_datasets_with_position.

    rv_is_last = abap_true.

    LOOP AT mt_datasets_with_position ASSIGNING <ls_dataset_with_position>.
      IF <ls_dataset_with_position>-iterator->record_is_last( ) <> abap_true.
        rv_is_last = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.
  endmethod.


  METHOD _ADD_ANOTHER_JOIN.
    _add_dataset( io_parent_node_of_join ).
    _add_join_condition( io_parent_node_of_join ).
  ENDMETHOD.


  method _ADD_DATASET.

    DATA: ls_dataset_with_position  LIKE LINE OF mt_datasets_with_position.

    _get_dataset_name_and_alias( EXPORTING io_parent_node_of_join    = io_parent_node_of_join
                                 IMPORTING ev_name                   = ls_dataset_with_position-dataset_name
                                           ev_alias                  = ls_dataset_with_position-dataset_alias ).

    ls_dataset_with_position-iterator = _create_dataset_iterator( ls_dataset_with_position-dataset_name ).

    APPEND ls_dataset_with_position TO mt_datasets_with_position.

    _consider_type_of_join( io_parent_node_of_join ).
  endmethod.


  METHOD _add_join_condition.

    DATA: ls_join_condition          TYPE ty_join_condition,
          lv_index                   TYPE i,
          ls_pre_last_dataset        LIKE LINE OF mt_datasets_with_position,
          ls_last_dataset            LIKE LINE OF mt_datasets_with_position,
          lo_node_join_on            TYPE REF TO zcl_zosql_parser_node.

    lo_node_join_on =
      io_parent_node_of_join->get_child_node_with_type(
        zcl_zosql_parser_recurs_desc=>node_type-join_on ).

    CREATE OBJECT ls_join_condition-expression_processor TYPE zcl_zosql_join_processor
      EXPORTING
        io_parameters = mo_parameters.

    ls_join_condition-expression_processor->initialize_by_parsed_sql( lo_node_join_on ).

    lv_index = lines( mt_datasets_with_position ) - 1.
    READ TABLE mt_datasets_with_position INDEX lv_index INTO ls_pre_last_dataset.

    ls_join_condition-left_dataset_name = ls_pre_last_dataset-dataset_name.
    ls_join_condition-left_dataset_alias = ls_pre_last_dataset-dataset_alias.

    lv_index = lines( mt_datasets_with_position ).
    READ TABLE mt_datasets_with_position INDEX lv_index INTO ls_last_dataset.

    ls_join_condition-right_dataset_name = ls_last_dataset-dataset_name.
    ls_join_condition-right_dataset_alias = ls_last_dataset-dataset_alias.

    ls_join_condition-type_of_join = ls_last_dataset-type_of_join.

    APPEND ls_join_condition TO mt_join_conditions.
  ENDMETHOD.


  METHOD _ADD_LEFT_JOIN_ITER.

    DATA: lt_datasets_with_records  LIKE mt_datasets_with_position,
          lt_join_conditions_needed TYPE ty_join_conditions,
          lt_records_that_joined    TYPE TABLE OF zosql_db_rec_unique_id,
          lo_next_from_iter         TYPE REF TO zcl_zosql_from_iterator,
          ls_outer_join_add_iter    TYPE ty_outer_join_add_iter.

    FIELD-SYMBOLS: <ls_dataset_with_position> LIKE LINE OF mt_datasets_with_position,
                   <ls_join_condition>        LIKE LINE OF mt_join_conditions,
                   <ls_last_dataset>          LIKE LINE OF lt_datasets_with_records,
                   <ls_record_statistic>      TYPE ty_record_statistic.

    LOOP AT mt_datasets_with_position ASSIGNING <ls_dataset_with_position>.
      IF <ls_dataset_with_position>-dataset_name = is_join_condition-right_dataset_name
        AND <ls_dataset_with_position>-dataset_alias = is_join_condition-right_dataset_alias.

        EXIT.
      ENDIF.

      APPEND <ls_dataset_with_position> TO lt_datasets_with_records.
    ENDLOOP.

    LOOP AT mt_join_conditions ASSIGNING <ls_join_condition>.
      IF <ls_join_condition> = is_join_condition.
        EXIT.
      ENDIF.
      APPEND <ls_join_condition> TO lt_join_conditions_needed.
    ENDLOOP.

    READ TABLE lt_datasets_with_records INDEX LINES( lt_datasets_with_records ) ASSIGNING <ls_last_dataset>.
    LOOP AT <ls_last_dataset>-records_statistic ASSIGNING <ls_record_statistic>
      WHERE was_inner_join_right = abap_true.

      APPEND <ls_record_statistic>-record_unique_id TO lt_records_that_joined.
    ENDLOOP.

    <ls_last_dataset>-iterator = <ls_last_dataset>-iterator->clone( it_records_to_delete = lt_records_that_joined ).

    CREATE OBJECT lo_next_from_iter
      EXPORTING
        io_zosql_test_environment = mo_zosql_test_environment
        io_parameters             = mo_parameters.

    lo_next_from_iter->_init_by_attributes( it_datasets_with_position = lt_datasets_with_records
                                            it_join_conditions        = lt_join_conditions_needed ).

    ls_outer_join_add_iter-from_iterator = lo_next_from_iter.
    APPEND ls_outer_join_add_iter TO mt_outer_join_add_chain.
  ENDMETHOD.


  method _CHECK_CONDITIONS_CURRENT_POS.

    DATA: lo_current_iterator_pos TYPE REF TO zcl_zosql_iterator_position,
          lv_all_conditions_true  TYPE abap_bool.

    FIELD-SYMBOLS: <ls_join_condition> LIKE LINE OF mt_join_conditions.

    lo_current_iterator_pos = zif_zosql_iterator~get_iterator_position_object( ).

    lv_all_conditions_true = abap_true.
    LOOP AT mt_join_conditions ASSIGNING <ls_join_condition>.
      IF <ls_join_condition>-expression_processor->check_condition_for_cur_rec( lo_current_iterator_pos ) <> abap_true.
        lv_all_conditions_true = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.

    rv_conditions_are_true = lv_all_conditions_true.
  endmethod.


  method _CONDITIONS_FAIL_FOR_ALL_LINES.

    DATA: lt_datasets_with_position LIKE mt_datasets_with_position.

    lt_datasets_with_position = mt_datasets_with_position.

    IF _reset_position( ) <> abap_true.
      rv_cond_fail_all_lines = abap_true.
      RETURN.
    ENDIF.

    IF _find_next_position_for_condit( ) <> abap_true.
      rv_cond_fail_all_lines = abap_true.
    ENDIF.

    mt_datasets_with_position = lt_datasets_with_position.
  endmethod.


  METHOD _consider_type_of_join.

    DATA: lv_datasets_count   TYPE i,
          lv_pre_last_dataset TYPE i,
          lt_nodes_of_join    TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_node             TYPE REF TO zcl_zosql_parser_node,
          lv_type_of_join     TYPE i.

    FIELD-SYMBOLS: <ls_dataset_with_position> LIKE LINE OF mt_datasets_with_position.

    lt_nodes_of_join = io_parent_node_of_join->get_child_nodes( ).
    INSERT io_parent_node_of_join INTO lt_nodes_of_join INDEX 1.

    READ TABLE lt_nodes_of_join
      WITH KEY table_line->node_type   = zcl_zosql_parser_recurs_desc=>node_type-join
               table_line->token_ucase = 'LEFT'
               TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.
      lv_type_of_join = c_left_join.
    ELSE.
      lv_type_of_join = c_inner_join.
    ENDIF.

    lv_datasets_count = lines( mt_datasets_with_position ).
    lv_pre_last_dataset = lv_datasets_count - 1.

    CASE lv_type_of_join.
      WHEN c_left_join.
        READ TABLE mt_datasets_with_position INDEX lv_datasets_count ASSIGNING <ls_dataset_with_position>.
      WHEN c_right_join.
        READ TABLE mt_datasets_with_position INDEX lv_pre_last_dataset ASSIGNING <ls_dataset_with_position>.
    ENDCASE.

    IF <ls_dataset_with_position> IS ASSIGNED.
      <ls_dataset_with_position>-type_of_join = lv_type_of_join.
    ENDIF.
  ENDMETHOD.


  method _CREATE_DATASET_ITERATOR.
    IF zcl_zosql_view_iterator=>is_database_view( iv_dataset_name ) = abap_true.
      CREATE OBJECT ro_iterator TYPE zcl_zosql_view_iterator
        EXPORTING
          io_zosql_test_environment = mo_zosql_test_environment
          iv_view_name              = iv_dataset_name.
    ELSE.
      CREATE OBJECT ro_iterator TYPE zcl_zosql_table_iterator
        EXPORTING
          io_zosql_test_environment = mo_zosql_test_environment
          iv_table_name             = iv_dataset_name.
    ENDIF.
  endmethod.


  method _FILL_RECORD_STATISTIC.

    DATA: lv_current_record_unique_id TYPE zosql_hash,
          lv_first_dataset            TYPE abap_bool,
          lv_last_dataset             TYPE abap_bool.

    FIELD-SYMBOLS: <ls_dataset_with_position> LIKE LINE OF mt_datasets_with_position,
                   <ls_record_statistic>      TYPE ty_record_statistic.

    lv_first_dataset = abap_true.
    LOOP AT mt_datasets_with_position ASSIGNING <ls_dataset_with_position>.

      AT LAST.
        lv_last_dataset = abap_true.
      ENDAT.

      lv_current_record_unique_id = <ls_dataset_with_position>-iterator->get_current_record_unique_id( ).

      READ TABLE <ls_dataset_with_position>-records_statistic WITH KEY record_unique_id = lv_current_record_unique_id
        ASSIGNING <ls_record_statistic>.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO <ls_dataset_with_position>-records_statistic ASSIGNING <ls_record_statistic>.
        <ls_record_statistic>-record_unique_id = lv_current_record_unique_id.
      ENDIF.

      IF lv_first_dataset <> abap_true.
        <ls_record_statistic>-was_inner_joined_left = abap_true.
      ENDIF.

      IF lv_last_dataset <> abap_true.
        <ls_record_statistic>-was_inner_join_right = abap_true.
      ENDIF.

      lv_first_dataset = abap_false.
    ENDLOOP.
  endmethod.


  METHOD _FIND_NEXT_POSITION_FOR_CONDIT.

    IF mv_outer_join_add_mode = abap_true.
      rv_next_position_found = _next_pos_outer_join_add_mode( ).
      RETURN.
    ENDIF.

    IF mt_join_conditions IS INITIAL.
      rv_next_position_found = abap_true.
      RETURN.
    ENDIF.

    IF _check_conditions_current_pos( ) = abap_true.
      _fill_record_statistic( ).
      rv_next_position_found = abap_true.
    ELSE.
      IF _move_to_next_position( ) = abap_true.
        rv_next_position_found = _find_next_position_for_condit( ).
      ENDIF.
    ENDIF.

    IF rv_next_position_found <> abap_true.
      rv_next_position_found = _init_outer_join_add_mode( ).
    ENDIF.
  ENDMETHOD.


  method _GET_DATASET_NAME_AND_ALIAS.

    DATA: lt_nodes_of_join          TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_node_with_dataset      TYPE REF TO zcl_zosql_parser_node.

    FIELD-SYMBOLS: <ls_node> TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    IF io_parent_node_of_join->node_type = zcl_zosql_parser_recurs_desc=>node_type-table.
      lo_node_with_dataset = io_parent_node_of_join.
    ELSE.
      lo_node_with_dataset =
        io_parent_node_of_join->get_child_node_with_type(
          zcl_zosql_parser_recurs_desc=>node_type-table ).
    ENDIF.

    IF lo_node_with_dataset IS BOUND.
      ev_name  = lo_node_with_dataset->token_ucase.
      ev_alias =
        lo_node_with_dataset->get_child_node_token_with_type(
          iv_node_type = zcl_zosql_parser_recurs_desc=>node_type-alias
          iv_ucase     = abap_true ).
    ENDIF.
  endmethod.


  method _INIT_BY_ATTRIBUTES.
    mt_datasets_with_position = it_datasets_with_position.
    mt_join_conditions        = it_join_conditions.
  endmethod.


  method _INIT_BY_SQL_PARSER.

    DATA: lo_parser_helper        TYPE REF TO zcl_zosql_parser_helper,
          lo_node_from            TYPE REF TO zcl_zosql_parser_node,
          lt_nodes_from_tables    TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_child_node_of_from   TYPE REF TO zcl_zosql_parser_node.

    FIELD-SYMBOLS: <ls_node_from_table>     LIKE LINE OF lt_nodes_from_tables.

    CREATE OBJECT lo_parser_helper
      EXPORTING
        io_sql_parser = io_sql_parser.
    lo_parser_helper->get_key_nodes_of_sql_select( IMPORTING eo_node_from  = lo_node_from ).

    lt_nodes_from_tables = lo_node_from->get_child_nodes( ).

    LOOP AT lt_nodes_from_tables INTO lo_child_node_of_from.
      _add_another_join( lo_child_node_of_from ).
    ENDLOOP.
  endmethod.


  METHOD _INIT_OUTER_JOIN_ADD_MODE.

    FIELD-SYMBOLS: <ls_dataset_with_position> LIKE LINE OF mt_datasets_with_position,
                   <ls_join_condition>        LIKE LINE OF mt_join_conditions,
                   <ls_outer_join_add_elem>   LIKE LINE OF mt_outer_join_add_chain.

    mv_outer_join_add_mode = abap_true.

    LOOP AT mt_join_conditions ASSIGNING <ls_join_condition>
      WHERE type_of_join = c_left_join.

      _add_left_join_iter( <ls_join_condition> ).
    ENDLOOP.

    LOOP AT mt_outer_join_add_chain ASSIGNING <ls_outer_join_add_elem>.
      IF <ls_outer_join_add_elem>-from_iterator->move_to_first( ) <> abap_true.
        DELETE mt_outer_join_add_chain.
      ENDIF.
    ENDLOOP.

    IF mt_outer_join_add_chain IS NOT INITIAL.
      mv_outer_join_add_mode_index = 1.
      rv_outer_join_found = abap_true.
    ENDIF.
  ENDMETHOD.


  method _MOVE_TO_NEXT_POSITION.

    DATA: lv_current_dataset  TYPE i.

    FIELD-SYMBOLS: <ls_current_dataset> LIKE LINE OF mt_datasets_with_position.

    lv_current_dataset = LINES( mt_datasets_with_position ).

    WHILE lv_current_dataset > 0.

      READ TABLE mt_datasets_with_position INDEX lv_current_dataset ASSIGNING <ls_current_dataset>.
      IF sy-subrc = 0.
        IF <ls_current_dataset>-iterator->move_to_next( ) = abap_true.
          rv_move_successful = abap_true.
          EXIT.
        ELSE.
          IF <ls_current_dataset>-iterator->move_to_first( ) <> abap_true.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.

      lv_current_dataset = lv_current_dataset - 1.
    ENDWHILE.

    DATA: lv_datasets_positions TYPE string.

    lv_datasets_positions = _positions_as_string( ).
  endmethod.


  METHOD _NEXT_POS_OUTER_JOIN_ADD_MODE.

    DATA: ls_outer_join_add_iter LIKE LINE OF mt_outer_join_add_chain.

    IF mv_outer_join_add_mode_index > LINES( mt_outer_join_add_chain ).
      RETURN.
    ENDIF.

    READ TABLE mt_outer_join_add_chain INDEX mv_outer_join_add_mode_index INTO ls_outer_join_add_iter.
    rv_next_position_found = ls_outer_join_add_iter-from_iterator->move_to_next( ).

    IF rv_next_position_found <> abap_true AND mv_outer_join_add_mode_index <= LINES( mt_outer_join_add_chain ).
      mv_outer_join_add_mode_index = mv_outer_join_add_mode_index + 1.
    ENDIF.
  ENDMETHOD.


  METHOD _positions_as_string.

    DATA: lo_table_iterator TYPE REF TO zcl_zosql_table_iterator,
          lv_position       TYPE string.

    FIELD-SYMBOLS: <ls_dataset> LIKE LINE OF mt_datasets_with_position.

    LOOP AT mt_datasets_with_position ASSIGNING <ls_dataset>.
      TRY.
          lo_table_iterator ?= <ls_dataset>-iterator.
          lv_position = |{ lo_table_iterator->current_record }|.
        CATCH cx_root.
          lv_position = 'UNKNOWN'.
      ENDTRY.

      IF rv_positions_as_string IS INITIAL.
        rv_positions_as_string = lv_position.
      ELSE.
        CONCATENATE rv_positions_as_string lv_position INTO rv_positions_as_string SEPARATED BY '-'.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  method _RESET_POSITION.

    FIELD-SYMBOLS: <ls_dataset_with_position> LIKE LINE OF mt_datasets_with_position.

    mv_outer_join_add_mode = abap_false.

    rv_first_record_select_success = abap_true.

    LOOP AT mt_datasets_with_position ASSIGNING <ls_dataset_with_position>.
      IF <ls_dataset_with_position>-iterator->move_to_first( ) <> abap_true.
        rv_first_record_select_success = abap_false.
      ENDIF.
    ENDLOOP.
  endmethod.
ENDCLASS.
