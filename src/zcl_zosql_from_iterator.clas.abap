class ZCL_ZOSQL_FROM_ITERATOR definition
  public
  inheriting from ZCL_ZOSQL_ITERATOR_BASE
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT optional
      !IO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS optional
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC optional
    raising
      ZCX_ZOSQL_ERROR .

  methods ZIF_ZOSQL_ITERATOR~ADD_ADDITIONAL_DATASET
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~CHECK_DATA_SET_EXISTS
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~GET_CURRENT_RECORD_UNIQUE_ID
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~GET_DATA_SET_LIST
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~GET_ITERATOR_POSITION_OBJECT
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~GET_KEY_FIELDS_OF_DATA_SET
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~GET_LINE_FOR_DATA_SET_REF
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~IS_EMPTY
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~MOVE_TO_FIRST
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~MOVE_TO_NEXT
    redefinition .
protected section.
private section.

  types:
    BEGIN OF ty_dataset_with_position,
      dataset_name          TYPE string,
      dataset_alias         TYPE string,
      type_of_join          TYPE string,
      iterator              TYPE REF TO zif_zosql_iterator,
      join_expression       TYPE REF TO zif_zosql_expression_processor,
      outer_join_empty_flag TYPE abap_bool,
    END OF ty_dataset_with_position .
  types:
    ty_datasets_with_position TYPE STANDARD TABLE OF ty_dataset_with_position
WITH KEY dataset_name dataset_alias .

  data MO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT .
  data MO_PARAMETERS type ref to ZCL_ZOSQL_PARAMETERS .
  data MT_DATASETS_WITH_POSITION type TY_DATASETS_WITH_POSITION .
  constants C_INNER_JOIN type STRING value '' ##NO_TEXT.
  constants C_LEFT_JOIN type STRING value 'LEFT' ##NO_TEXT.
  constants C_RIGHT_JOIN type STRING value 'RIGHT' ##NO_TEXT.

  methods _UPDATE_OUTER_JOIN_FLAG
    raising
      ZCX_ZOSQL_ERROR .
  methods _ANY_REC_CONDIT_TRUE_DATASET
    importing
      !IV_DATASET_INDEX type I
    returning
      value(RV_JOIN_COND_TRUE_FOR_SOME_REC) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _IS_DATASET_OUTER_JOINED
    importing
      !IS_DATASET_WITH_POSITION type TY_DATASET_WITH_POSITION
    returning
      value(RV_IS_OUTER_JOINED) type ABAP_BOOL .
  methods _CREATE_DATASET_ITERATOR
    importing
      !IV_DATASET_NAME type CLIKE
    returning
      value(RO_ITERATOR) type ref to ZIF_ZOSQL_ITERATOR
    raising
      ZCX_ZOSQL_ERROR .
  methods _IF_DATASET_IS_TABLE
    importing
      !IO_ITERATOR_OF_DATASET type ref to ZIF_ZOSQL_ITERATOR
    returning
      value(RV_IS_TABLE) type ABAP_BOOL .
  methods _GET_DATASET_NAME_AND_ALIAS
    importing
      !IO_PARENT_NODE_OF_JOIN type ref to ZCL_ZOSQL_PARSER_NODE
    exporting
      !EV_NAME type CLIKE
      !EV_ALIAS type CLIKE .
  methods _RAISE_IF_NOT_SELECT
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    raising
      ZCX_ZOSQL_ERROR .
  methods _ADD_JOIN
    importing
      !IO_PARENT_NODE_OF_JOIN type ref to ZCL_ZOSQL_PARSER_NODE
    raising
      ZCX_ZOSQL_ERROR .
  methods _POSITIONS_AS_STRING
    returning
      value(RV_POSITIONS_AS_STRING) type STRING .
  methods _INIT_BY_SQL_PARSER
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    raising
      ZCX_ZOSQL_ERROR .
  methods _CONSIDER_TYPE_OF_JOIN
    importing
      !IO_PARENT_NODE_OF_JOIN type ref to ZCL_ZOSQL_PARSER_NODE .
  methods _FILL_JOIN_CONDITION
    importing
      !IO_PARENT_NODE_OF_JOIN type ref to ZCL_ZOSQL_PARSER_NODE
    raising
      ZCX_ZOSQL_ERROR .
  methods _ADD_JOIN_AND_FILL_DATASET
    importing
      !IO_PARENT_NODE_OF_JOIN type ref to ZCL_ZOSQL_PARSER_NODE
    raising
      ZCX_ZOSQL_ERROR .
  methods _CHECK_CONDITIONS_CURRENT_POS
    returning
      value(RV_CONDITIONS_ARE_TRUE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _GET_ITERATOR_POSITION_OBJECT
    importing
      value(IV_START_INDEX) type I optional
      value(IV_END_INDEX) type I optional
    returning
      value(RO_ITERATOR_POS) type ref to ZCL_ZOSQL_ITERATOR_POSITION
    raising
      ZCX_ZOSQL_ERROR .
  methods _FIND_NEXT_POSITION_FOR_CONDIT
    returning
      value(RV_NEXT_POSITION_FOUND) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _CONDITIONS_FAIL_FOR_ALL_LINES
    returning
      value(RV_COND_FAIL_ALL_LINES) type ABAP_BOOL
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


  method CONSTRUCTOR.

    DATA: lo_factory          TYPE REF TO zif_zosql_factory,
          lt_parameters_empty TYPE zosql_db_layer_params.

    super->constructor( ).

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
  endmethod.


  METHOD zif_zosql_iterator~add_additional_dataset.
    DATA: ls_dataset_with_position  LIKE LINE OF mt_datasets_with_position.

    ls_dataset_with_position-dataset_name  = zcl_zosql_utils=>to_upper_case( iv_dataset_name ).
    ls_dataset_with_position-dataset_alias = zcl_zosql_utils=>to_upper_case( iv_dataset_alias ).
    ls_dataset_with_position-iterator = _create_dataset_iterator( ls_dataset_with_position-dataset_name ).

    IF iv_dataset_alias IS NOT INITIAL.
      READ TABLE mt_datasets_with_position WITH KEY dataset_alias = ls_dataset_with_position-dataset_alias
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        MESSAGE e093 WITH iv_dataset_alias INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.
    ENDIF.

    APPEND ls_dataset_with_position TO mt_datasets_with_position.
  ENDMETHOD.


  method ZIF_ZOSQL_ITERATOR~CHECK_DATA_SET_EXISTS.
    DATA: ld_ref_to_dataset_line TYPE REF TO data.

    ld_ref_to_dataset_line = zif_zosql_iterator~get_line_for_data_set_ref( iv_dataset_name_or_alias ).

    IF ld_ref_to_dataset_line IS BOUND.
      rv_exists = abap_true.
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


  method ZIF_ZOSQL_ITERATOR~GET_DATA_SET_LIST.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = mt_datasets_with_position
                                               IMPORTING et_table_dest = rt_data_set_list ).
  endmethod.


  METHOD zif_zosql_iterator~get_iterator_position_object.
    ro_iterator_pos = _get_iterator_position_object( ).
  ENDMETHOD.


  method ZIF_ZOSQL_ITERATOR~GET_KEY_FIELDS_OF_DATA_SET.
    DATA: lo_virt_table            TYPE REF TO zcl_zosql_one_virt_table,
          lv_dataset_name_or_alias TYPE string.

    FIELD-SYMBOLS: <ls_data_set_data> LIKE LINE OF mt_datasets_with_position.

    lv_dataset_name_or_alias = zcl_zosql_utils=>to_upper_case( iv_dataset_name_or_alias ).
    READ TABLE mt_datasets_with_position WITH KEY dataset_name = lv_dataset_name_or_alias ASSIGNING <ls_data_set_data>.
    IF sy-subrc <> 0.
      READ TABLE mt_datasets_with_position WITH KEY dataset_alias = lv_dataset_name_or_alias ASSIGNING <ls_data_set_data>.
    ENDIF.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF _if_dataset_is_table( <ls_data_set_data>-iterator ) <> abap_true.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_virt_table
      EXPORTING
        iv_table_name = <ls_data_set_data>-dataset_name.

    rt_key_fields = lo_virt_table->zif_zosql_stub~get_key_fields( ).
  endmethod.


  method ZIF_ZOSQL_ITERATOR~GET_LINE_FOR_DATA_SET_REF.
    DATA: lv_dataset_name_or_alias TYPE string.

    FIELD-SYMBOLS: <ls_data_set_data> LIKE LINE OF mt_datasets_with_position,
                   <ls_data_set_line> TYPE any.

    lv_dataset_name_or_alias = zcl_zosql_utils=>to_upper_case( iv_dataset_name_or_alias ).
    READ TABLE mt_datasets_with_position WITH KEY dataset_name = lv_dataset_name_or_alias ASSIGNING <ls_data_set_data>.
    IF sy-subrc <> 0.
      READ TABLE mt_datasets_with_position WITH KEY dataset_alias = lv_dataset_name_or_alias ASSIGNING <ls_data_set_data>.
    ENDIF.

    IF <ls_data_set_data> IS ASSIGNED.
      rd_ref_to_line = <ls_data_set_data>-iterator->create_empty_record_as_ref( ).
    ELSE.
      MESSAGE e075 WITH iv_dataset_name_or_alias INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
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


  METHOD _ADD_JOIN.
    _add_join_and_fill_dataset( io_parent_node_of_join ).
    _fill_join_condition( io_parent_node_of_join ).
  ENDMETHOD.


  method _ADD_JOIN_AND_FILL_DATASET.

    DATA: ls_dataset_with_position  LIKE LINE OF mt_datasets_with_position.

    _get_dataset_name_and_alias( EXPORTING io_parent_node_of_join    = io_parent_node_of_join
                                 IMPORTING ev_name                   = ls_dataset_with_position-dataset_name
                                           ev_alias                  = ls_dataset_with_position-dataset_alias ).

    ls_dataset_with_position-iterator = _create_dataset_iterator( ls_dataset_with_position-dataset_name ).

    APPEND ls_dataset_with_position TO mt_datasets_with_position.

    _consider_type_of_join( io_parent_node_of_join ).
  endmethod.


  METHOD _any_rec_condit_true_dataset.

    DATA: ls_dataset_with_position TYPE ty_dataset_with_position,
          lo_clone_of_cur_ds_iterator TYPE REF TO zif_zosql_iterator,
          lv_not_end_of_data          TYPE abap_bool,
          lo_iterator_pos             TYPE REF TO zcl_zosql_iterator_position,
          lv_previous_dataset_index   TYPE i,
          ld_ref_to_line              TYPE REF TO data.

    lv_previous_dataset_index = iv_dataset_index - 1.

    READ TABLE mt_datasets_with_position INDEX iv_dataset_index INTO ls_dataset_with_position.
    lo_clone_of_cur_ds_iterator = ls_dataset_with_position-iterator->clone( ).

    lv_not_end_of_data = lo_clone_of_cur_ds_iterator->move_to_first( ).

    WHILE lv_not_end_of_data = abap_true.

      ld_ref_to_line = lo_clone_of_cur_ds_iterator->get_current_record_ref( ).

      lo_iterator_pos = _get_iterator_position_object( iv_end_index = lv_previous_dataset_index ).
      lo_iterator_pos->add_data_set_data( iv_dataset_name        = ls_dataset_with_position-dataset_name
                                          iv_dataset_alias       = ls_dataset_with_position-dataset_alias
                                          id_ref_to_current_line = ld_ref_to_line ).

      IF ls_dataset_with_position-join_expression->check_condition_for_cur_rec( lo_iterator_pos ) = abap_true.
        rv_join_cond_true_for_some_rec = abap_true.
        EXIT.
      ENDIF.

      lv_not_end_of_data = lo_clone_of_cur_ds_iterator->move_to_next( ).
    ENDWHILE.
  ENDMETHOD.


  METHOD _check_conditions_current_pos.

    DATA: lo_current_iterator_pos TYPE REF TO zcl_zosql_iterator_position.

    FIELD-SYMBOLS: <ls_dataset_with_position> LIKE LINE OF mt_datasets_with_position.

    _update_outer_join_flag( ).

    lo_current_iterator_pos = zif_zosql_iterator~get_iterator_position_object( ).

    rv_conditions_are_true = abap_true.
    LOOP AT mt_datasets_with_position ASSIGNING <ls_dataset_with_position>
      WHERE outer_join_empty_flag <> abap_true.

      IF <ls_dataset_with_position>-join_expression->check_condition_for_cur_rec(
        lo_current_iterator_pos ) <> abap_true.

        rv_conditions_are_true = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


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


  METHOD _CONSIDER_TYPE_OF_JOIN.

    DATA: lv_datasets_count   TYPE i,
          lv_pre_last_dataset TYPE i,
          lt_nodes_of_join    TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_node             TYPE REF TO zcl_zosql_parser_node,
          lv_type_of_join     TYPE string.

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


  METHOD _fill_join_condition.

    DATA: lo_node_join_on TYPE REF TO zcl_zosql_parser_node.

    FIELD-SYMBOLS: <ls_current_dataset> TYPE ty_dataset_with_position.

    lo_node_join_on =
      io_parent_node_of_join->get_child_node_with_type(
        zcl_zosql_parser_recurs_desc=>node_type-join_on ).

    READ TABLE mt_datasets_with_position INDEX lines( mt_datasets_with_position ) ASSIGNING <ls_current_dataset>.

    CREATE OBJECT <ls_current_dataset>-join_expression TYPE zcl_zosql_join_processor
      EXPORTING
        io_parameters = mo_parameters
        io_iterator   = me.

    <ls_current_dataset>-join_expression->initialize_by_parsed_sql( lo_node_join_on ).
  ENDMETHOD.


  METHOD _FIND_NEXT_POSITION_FOR_CONDIT.

    IF _check_conditions_current_pos( ) = abap_true.
      rv_next_position_found = abap_true.
    ELSE.
      IF _move_to_next_position( ) = abap_true.
        rv_next_position_found = _find_next_position_for_condit( ).
      ENDIF.
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


  METHOD _GET_ITERATOR_POSITION_OBJECT.

    DATA: ld_ref_to_line TYPE REF TO data,
          lv_from        TYPE i,
          lv_to          TYPE i.

    FIELD-SYMBOLS: <ls_dataset_with_position> LIKE LINE OF mt_datasets_with_position.

    IF iv_start_index > 0.
      lv_from = iv_start_index.
    ELSE.
      lv_from = 1.
    ENDIF.

    IF iv_end_index > 0.
      lv_to = iv_end_index.
    ELSE.
      lv_to = LINES( mt_datasets_with_position ).
    ENDIF.

    CREATE OBJECT ro_iterator_pos.

    LOOP AT mt_datasets_with_position ASSIGNING <ls_dataset_with_position>
      FROM lv_from TO lv_to.

      IF <ls_dataset_with_position>-outer_join_empty_flag = abap_true.
        ro_iterator_pos->add_dataset_as_empty_outerjoin(
          iv_dataset_name        = <ls_dataset_with_position>-dataset_name
          iv_dataset_alias       = <ls_dataset_with_position>-dataset_alias ).
      ELSE.
        ld_ref_to_line = <ls_dataset_with_position>-iterator->get_current_record_ref( ).
        ro_iterator_pos->add_data_set_data( iv_dataset_name        = <ls_dataset_with_position>-dataset_name
                                            iv_dataset_alias       = <ls_dataset_with_position>-dataset_alias
                                            id_ref_to_current_line = ld_ref_to_line ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD _IF_DATASET_IS_TABLE.

    DATA: lo_table_iterator TYPE REF TO zcl_zosql_table_iterator.

    TRY.
        lo_table_iterator ?= io_iterator_of_dataset.
        rv_is_table = abap_true.
      CATCH cx_root.
        rv_is_table = abap_false.
    ENDTRY.
  ENDMETHOD.


  method _INIT_BY_SQL_PARSER.

    DATA: lo_parser_helper        TYPE REF TO zcl_zosql_parser_helper,
          lo_node_from            TYPE REF TO zcl_zosql_parser_node,
          lt_nodes_from_tables    TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_child_node_of_from   TYPE REF TO zcl_zosql_parser_node.

    FIELD-SYMBOLS: <ls_node_from_table>     LIKE LINE OF lt_nodes_from_tables.

    _raise_if_not_select( io_sql_parser ).

    CREATE OBJECT lo_parser_helper
      EXPORTING
        io_sql_parser = io_sql_parser.
    lo_parser_helper->get_key_nodes_of_sql_select( IMPORTING eo_node_from  = lo_node_from ).

    lt_nodes_from_tables = lo_node_from->get_child_nodes( ).

    LOOP AT lt_nodes_from_tables INTO lo_child_node_of_from.
      _add_join( lo_child_node_of_from ).
    ENDLOOP.
  endmethod.


  method _IS_DATASET_OUTER_JOINED.
    IF is_dataset_with_position-type_of_join = c_left_join.
      rv_is_outer_joined = abap_true.
    ENDIF.
  endmethod.


  METHOD _move_to_next_position.

    DATA: lv_current_dataset  TYPE i.

    FIELD-SYMBOLS: <ls_current_dataset> LIKE LINE OF mt_datasets_with_position.

    lv_current_dataset = lines( mt_datasets_with_position ).

    WHILE lv_current_dataset > 0.

      READ TABLE mt_datasets_with_position INDEX lv_current_dataset ASSIGNING <ls_current_dataset>.
      IF sy-subrc = 0.

        IF <ls_current_dataset>-iterator->is_empty( ) = abap_true.
          IF <ls_current_dataset>-type_of_join <> c_left_join.
            EXIT.
          ENDIF.
        ELSE.

          IF <ls_current_dataset>-outer_join_empty_flag <> abap_true.
            IF <ls_current_dataset>-iterator->move_to_next( ) = abap_true.
              rv_move_successful = abap_true.
              EXIT.
            ELSE.
              <ls_current_dataset>-iterator->move_to_first( ).
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      lv_current_dataset = lv_current_dataset - 1.
    ENDWHILE.

    DATA: lv_datasets_positions TYPE string.

    lv_datasets_positions = _positions_as_string( ).
  ENDMETHOD.


  METHOD _POSITIONS_AS_STRING.

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


  method _RESET_POSITION.

    FIELD-SYMBOLS: <ls_dataset_with_position> LIKE LINE OF mt_datasets_with_position.

    rv_first_record_select_success = abap_true.

    LOOP AT mt_datasets_with_position ASSIGNING <ls_dataset_with_position>.
      IF <ls_dataset_with_position>-iterator->move_to_first( ) <> abap_true.
        IF <ls_dataset_with_position>-type_of_join <> c_left_join.
          rv_first_record_select_success = abap_false.
        ENDIF.
      ENDIF.
    ENDLOOP.
  endmethod.


  method _UPDATE_OUTER_JOIN_FLAG.

    DATA: lv_index_of_dataset TYPE i.

    FIELD-SYMBOLS: <ls_dataset_with_position> LIKE LINE OF mt_datasets_with_position.

    LOOP AT mt_datasets_with_position ASSIGNING <ls_dataset_with_position>.

      lv_index_of_dataset = sy-tabix.

      IF _is_dataset_outer_joined( <ls_dataset_with_position> ) = abap_true
        AND _any_rec_condit_true_dataset( lv_index_of_dataset ) <> abap_true.

        <ls_dataset_with_position>-outer_join_empty_flag = abap_true.
      ELSE.
        <ls_dataset_with_position>-outer_join_empty_flag = abap_false.
      ENDIF.
    ENDLOOP.
  endmethod.
ENDCLASS.
