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
      !IO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT optional .
  methods GET_DATA_SET_LIST
    returning
      value(RT_DATA_SET_LIST) type TY_DATA_SETS .
  methods INIT_BY_FROM
    importing
      !IV_FROM type STRING
    raising
      ZCX_ZOSQL_ERROR .
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
      value(RT_DATA_SET_COMPONENTS) type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE
    raising
      ZCX_ZOSQL_ERROR .
protected section.
PRIVATE SECTION.

  TYPES:
    BEGIN OF ty_record_statistic,
      record_unique_id       TYPE zosql_hash,
      was_inner_join_right   TYPE abap_bool,
      was_inner_joined_left  TYPE abap_bool,
      was_empty_record_added TYPE abap_bool,
    END OF ty_record_statistic .
  TYPES:
    ty_records_statistic TYPE STANDARD TABLE OF ty_record_statistic WITH KEY record_unique_id .
  TYPES:
    BEGIN OF ty_dataset_with_position,
      dataset_name      TYPE string,
      dataset_alias     TYPE string,
      type_of_join      TYPE i,
      iterator          TYPE REF TO zif_zosql_iterator,
      records_statistic TYPE ty_records_statistic,
    END OF ty_dataset_with_position .
  TYPES:
    ty_datasets_with_position TYPE STANDARD TABLE OF ty_dataset_with_position
    WITH KEY dataset_name dataset_alias .
  TYPES:
    BEGIN OF ty_join_condition,
      left_dataset_name   TYPE string,
      left_dataset_alias  TYPE string,
      right_dataset_name  TYPE string,
      right_dataset_alias TYPE string,
      type_of_join        TYPE i,
      parser              TYPE REF TO zif_zosql_sqlcond_parser,
    END OF ty_join_condition .
  TYPES:
    ty_join_conditions TYPE STANDARD TABLE OF ty_join_condition .
  TYPES:
    BEGIN OF ty_outer_join_add_iter,
      from_iterator TYPE REF TO zif_zosql_iterator,
    END OF ty_outer_join_add_iter .

  DATA mo_zosql_test_environment TYPE REF TO zif_zosql_test_environment .
  DATA mt_datasets_with_position TYPE ty_datasets_with_position .
  DATA mt_join_conditions TYPE ty_join_conditions .
  CONSTANTS c_inner_join TYPE i VALUE 0 ##NO_TEXT.
  CONSTANTS c_left_join TYPE i VALUE 1 ##NO_TEXT.
  CONSTANTS c_right_join TYPE i VALUE 2 ##NO_TEXT.
  DATA mv_outer_join_add_mode TYPE abap_bool .
  DATA:
    mt_outer_join_add_chain TYPE STANDARD TABLE OF ty_outer_join_add_iter WITH DEFAULT KEY .
  DATA mv_outer_join_add_mode_index TYPE i .

  METHODS _add_left_join_iter
    IMPORTING
      !is_join_condition TYPE ty_join_condition .
  METHODS _check_conditions_current_pos
    RETURNING
      VALUE(rv_conditions_are_true) TYPE abap_bool
    RAISING
      zcx_zosql_error .
  METHODS _init_by_attributes
    IMPORTING
      !it_datasets_with_position TYPE ty_datasets_with_position
      !it_join_conditions        TYPE ty_join_conditions .
  METHODS _init_outer_join_add_mode
    RETURNING
      VALUE(rv_outer_join_found) TYPE abap_bool
    RAISING
      zcx_zosql_error .
  METHODS _fill_record_statistic
    RAISING
      zcx_zosql_error .
  METHODS _consider_type_of_join
    IMPORTING
      VALUE(iv_type_of_join) TYPE i .
  METHODS _add_join_condition
    IMPORTING
      !iv_join_condition_string TYPE string .
  METHODS _conditions_fail_for_all_lines
    RETURNING
      VALUE(rv_cond_fail_all_lines) TYPE abap_bool
    RAISING
      zcx_zosql_error .
  METHODS _find_next_position_for_condit
    RETURNING
      VALUE(rv_next_position_found) TYPE abap_bool
    RAISING
      zcx_zosql_error .
  METHODS _next_pos_outer_join_add_mode
    RETURNING
      VALUE(rv_next_position_found) TYPE abap_bool
    RAISING
      zcx_zosql_error .
  METHODS _add_dataset
    IMPORTING
      !iv_dictionary_name    TYPE clike
      VALUE(iv_type_of_join) TYPE i
    RAISING
      zcx_zosql_error .
  METHODS _move_to_next_position
    RETURNING
      VALUE(rv_move_successful) TYPE abap_bool
    RAISING
      zcx_zosql_error .
  METHODS _reset_position
    RETURNING
      VALUE(rv_first_record_select_success) TYPE abap_bool
    RAISING
      zcx_zosql_error .
ENDCLASS.



CLASS ZCL_ZOSQL_FROM_ITERATOR IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA: lo_factory TYPE REF TO zif_zosql_factory.

    IF io_zosql_test_environment IS BOUND.
      mo_zosql_test_environment = io_zosql_test_environment.
    ELSE.
      CREATE OBJECT lo_factory TYPE zcl_zosql_factory.
      mo_zosql_test_environment = lo_factory->get_test_environment( ).
    ENDIF.
  endmethod.


  method GET_COMPONENTS_OF_DATA_SET.

    DATA: ld_ref_to_dataset_data TYPE REF TO data,
          lo_struct              TYPE REF TO cl_abap_structdescr.

    ld_ref_to_dataset_data = get_line_for_data_set_ref( iv_dataset_name_or_alias ).
    lo_struct ?= cl_abap_structdescr=>describe_by_data_ref( ld_ref_to_dataset_data ).
    rt_data_set_components = lo_struct->get_components( ).
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
    ENDIF.
  endmethod.


  method INIT_BY_FROM.

    DATA: lt_tokens                TYPE TABLE OF string,
          lv_token                 TYPE string,
          lv_token_upper           TYPE string,
          ls_dataset_with_position TYPE ty_dataset_with_position.

    DATA: lv_table_name_expected  TYPE abap_bool,
          lv_table_alias_expected TYPE abap_bool,
          lv_current_table_name   TYPE string,
          lv_conditions_expected  TYPE string,
          lv_conditions           TYPE string,
          lv_type_of_join         TYPE i.

    FIELD-SYMBOLS: <ls_dataset_with_position> LIKE LINE OF mt_datasets_with_position.

    lt_tokens = zcl_zosql_utils=>split_condition_into_tokens( iv_from ).

    lv_table_name_expected = abap_true.

    LOOP AT lt_tokens iNTO lv_token.

      lv_token_upper = zcl_zosql_utils=>to_upper_case( lv_token ).

      IF lv_token_upper = 'LEFT'.
        lv_type_of_join = c_left_join.
      ENDIF.

      IF lv_token_upper = 'JOIN'.
        lv_table_name_expected = abap_true.
        lv_conditions_expected = abap_false.
        _add_join_condition( lv_conditions ).
        CLEAR lv_conditions.
        CONTINUE.
      ENDIF.

      IF lv_table_name_expected = abap_true.
        _add_dataset( iv_dictionary_name = lv_token_upper
                      iv_type_of_join    = lv_type_of_join ).
        lv_table_name_expected = abap_false.
        lv_current_table_name  = lv_token_upper.
        CONTINUE.
      ENDIF.

      IF lv_token_upper = 'AS'.
        lv_table_alias_expected = abap_true.
        CONTINUE.
      ENDIF.

      IF lv_table_alias_expected = abap_true.
        READ TABLE mt_datasets_with_position WITH KEY dataset_name = lv_current_table_name
          ASSIGNING <ls_dataset_with_position>.
        IF sy-subrc = 0.
          <ls_dataset_with_position>-dataset_alias = lv_token_upper.
        ENDIF.

        lv_table_alias_expected = abap_false.
      ENDIF.

      IF lv_token_upper = 'ON'.
        lv_conditions_expected = abap_true.
        lv_type_of_join = c_inner_join.
        CLEAR lv_conditions.
        CONTINUE.
      ENDIF.

      IF lv_conditions_expected = abap_true.
        CONCATENATE lv_conditions lv_token INTO lv_conditions SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    IF lv_conditions IS NOT INITIAL.
      _add_join_condition( lv_conditions ).
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


  method _ADD_DATASET.

    DATA: ls_dataset_with_position LIKE LINE OF mt_datasets_with_position.

    FIELD-SYMBOLS: <lt_data_lines> TYPE STANDARD TABLE.

    ls_dataset_with_position-dataset_name = iv_dictionary_name.

    IF zcl_zosql_view_iterator=>is_database_view( iv_dictionary_name ) = abap_true.
      CREATE OBJECT ls_dataset_with_position-iterator TYPE zcl_zosql_view_iterator
        EXPORTING
          io_zosql_test_environment = mo_zosql_test_environment
          iv_view_name              = iv_dictionary_name.
    ELSE.
      CREATE OBJECT ls_dataset_with_position-iterator TYPE zcl_zosql_table_iterator
        EXPORTING
          io_zosql_test_environment = mo_zosql_test_environment
          iv_table_name             = iv_dictionary_name.
    ENDIF.

    APPEND ls_dataset_with_position TO mt_datasets_with_position.

    _consider_type_of_join( iv_type_of_join ).
  endmethod.


  method _ADD_JOIN_CONDITION.

    DATA: ls_join_condition TYPE ty_join_condition,
          lv_index          TYPE i,
          ls_pre_last_dataset LIKE LINE OF mt_datasets_with_position,
          ls_last_dataset     LIKE LINE OF mt_datasets_with_position.

    IF iv_join_condition_string IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT ls_join_condition-parser TYPE zcl_zosql_join_cond_parser.
    ls_join_condition-parser->parse_condition( iv_join_condition_string ).

    lv_index = LINES( mt_datasets_with_position ) - 1.
    READ TABLE mt_datasets_with_position INDEX lv_index INTO ls_pre_last_dataset.

    ls_join_condition-left_dataset_name = ls_pre_last_dataset-dataset_name.
    ls_join_condition-left_dataset_alias = ls_pre_last_dataset-dataset_alias.

    lv_index = LINES( mt_datasets_with_position ).
    READ TABLE mt_datasets_with_position INDEX lv_index INTO ls_last_dataset.

    ls_join_condition-right_dataset_name = ls_last_dataset-dataset_name.
    ls_join_condition-right_dataset_alias = ls_last_dataset-dataset_alias.

    ls_join_condition-type_of_join = ls_last_dataset-type_of_join.

    APPEND ls_join_condition TO mt_join_conditions.
  endmethod.


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
        io_zosql_test_environment = mo_zosql_test_environment.

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
      IF <ls_join_condition>-parser->check_condition_for_cur_rec( lo_current_iterator_pos ) <> abap_true.
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


  method _CONSIDER_TYPE_OF_JOIN.

    DATA: lv_datasets_count   TYPE i,
          lv_pre_last_dataset TYPE i.

    FIELD-SYMBOLS: <ls_dataset_with_position> LIKE LINE OF mt_datasets_with_position.

    lv_datasets_count = LINES( mt_datasets_with_position ).
    lv_pre_last_dataset = lv_datasets_count - 1.

    CASE iv_type_of_join.
      WHEN c_left_join.
        READ TABLE mt_datasets_with_position INDEX lv_datasets_count ASSIGNING <ls_dataset_with_position>.
      WHEN c_right_join.
        READ TABLE mt_datasets_with_position INDEX lv_pre_last_dataset ASSIGNING <ls_dataset_with_position>.
    ENDCASE.

    IF <ls_dataset_with_position> IS ASSIGNED.
      <ls_dataset_with_position>-type_of_join = iv_type_of_join.
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


  method _INIT_BY_ATTRIBUTES.
    mt_datasets_with_position = it_datasets_with_position.
    mt_join_conditions        = it_join_conditions.
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
