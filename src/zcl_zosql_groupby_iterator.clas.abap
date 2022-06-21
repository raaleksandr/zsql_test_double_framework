class ZCL_ZOSQL_GROUPBY_ITERATOR definition
  public
  create public .

public section.

  interfaces ZIF_ZOSQL_ITERATOR .

  methods INIT_GROUPBY_ITERATOR
    importing
      !IT_GROUP_BY_KEY_FIELDS type ZCL_ZOSQL_GROUPBY_PROCESSOR=>TY_GROUP_BY_KEY_FIELDS
      !IO_SELECT type ref to ZCL_ZOSQL_SELECT_PROCESSOR
    raising
      ZCX_ZOSQL_ERROR .
  methods INIT_ITERATOR_WHEN_NO_GROUP_BY
    importing
      !IO_SELECT type ref to ZCL_ZOSQL_SELECT_PROCESSOR
    raising
      ZCX_ZOSQL_ERROR .
protected section.
private section.

  data MD_TABLE_NOT_GROUPED type ref to DATA .
  data MD_GROUP_BY_KEY_VALUES type ref to DATA .
  data MT_GROUP_BY_KEY_FIELDS type ZCL_ZOSQL_GROUPBY_PROCESSOR=>TY_GROUP_BY_KEY_FIELDS .
  data MV_CURRENT_RECORD_NUM type INT4 .
  data MT_ITER_POSITIONS_OF_LINES type ZCL_ZOSQL_SELECT_PROCESSOR=>TY_ITERATOR_POSITIONS .

  methods _ADD_RECORD_NUM_TO_CUR_GROUPBY
    importing
      !IO_ITERATOR_POS_GROUPBY type ref to ZCL_ZOSQL_GROUPBY_ITER_POS
      !IV_RECORD_NUM_IN_NOT_GROUPED type I
    raising
      ZCX_ZOSQL_ERROR .
  methods _CREATE_DYNAMIC_TABLE_FOR_KEYS
    raising
      ZCX_ZOSQL_ERROR .
  methods _FILL_DYNAMIC_TABLE_WITH_KEYS .
  methods _FILL_KEY_VALUES
    raising
      ZCX_ZOSQL_ERROR .
  methods _GET_COUNT
    returning
      value(RV_COUNT) type I .
  methods _RECORD_EQUALS_TO_CURRENT_KEY
    importing
      !IV_RECORD_NUM_IN_NOT_GROUPED type I
    returning
      value(RV_EQUALS_TO_GROUPBY_KEY) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
ENDCLASS.



CLASS ZCL_ZOSQL_GROUPBY_ITERATOR IMPLEMENTATION.


  method INIT_GROUPBY_ITERATOR.

    DATA: ld_table_not_grouped TYPE REF TO data.

    FIELD-SYMBOLS: <lt_table_not_grouped>      TYPE STANDARD TABLE,
                   <lt_table_not_grouped_copy> TYPE STANDARD TABLE.

    mt_group_by_key_fields = it_group_by_key_fields.

    ld_table_not_grouped = io_select->get_result_as_ref_to_data( ).
    ASSIGN ld_table_not_grouped->* TO <lt_table_not_grouped>.

    CREATE DATA md_table_not_grouped LIKE <lt_table_not_grouped>.
    ASSIGN md_table_not_grouped->* TO <lt_table_not_grouped_copy>.
    <lt_table_not_grouped_copy> = <lt_table_not_grouped>.

    mt_iter_positions_of_lines = io_select->get_iter_positions_of_lines( ).

    _fill_key_values( ).
  endmethod.


  method INIT_ITERATOR_WHEN_NO_GROUP_BY.

    DATA: ld_table_not_grouped TYPE REF TO data.

    FIELD-SYMBOLS: <lt_table_not_grouped>      TYPE STANDARD TABLE,
                   <lt_table_not_grouped_copy> TYPE STANDARD TABLE.

    REFRESH mt_group_by_key_fields.

    ld_table_not_grouped = io_select->get_result_as_ref_to_data( ).
    ASSIGN ld_table_not_grouped->* TO <lt_table_not_grouped>.

    CREATE DATA md_table_not_grouped LIKE <lt_table_not_grouped>.
    ASSIGN md_table_not_grouped->* TO <lt_table_not_grouped_copy>.
    <lt_table_not_grouped_copy> = <lt_table_not_grouped>.

    mt_iter_positions_of_lines = io_select->get_iter_positions_of_lines( ).
  endmethod.


  METHOD zif_zosql_iterator~get_iterator_position_object.

    DATA: lo_iterator_pos_groupby     TYPE REF TO zcl_zosql_groupby_iter_pos,
          lv_table_record_num         TYPE i,
          lv_not_grouped_line_count   TYPE i.

    FIELD-SYMBOLS: <lt_table_not_grouped>   TYPE STANDARD TABLE.

    CREATE OBJECT lo_iterator_pos_groupby.

    ASSIGN md_table_not_grouped->* TO <lt_table_not_grouped>.
    lv_not_grouped_line_count = lines( <lt_table_not_grouped> ).

    DO lv_not_grouped_line_count TIMES.

      lv_table_record_num = sy-index.

      IF _record_equals_to_current_key( lv_table_record_num ) = abap_true.
        _add_record_num_to_cur_groupby( io_iterator_pos_groupby      = lo_iterator_pos_groupby
                                        iv_record_num_in_not_grouped = lv_table_record_num ).
      ENDIF.
    ENDDO.

    ro_iterator_pos = lo_iterator_pos_groupby.
  ENDMETHOD.


  method ZIF_ZOSQL_ITERATOR~IS_EMPTY.
    IF _get_count( ) < 1.
      rv_is_empty = abap_true.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~MOVE_TO_FIRST.
    IF zif_zosql_iterator~is_empty( ) <> abap_true.
      mv_current_record_num = 1.
      rv_successful_move = abap_true.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~MOVE_TO_NEXT.
    IF mv_current_record_num < _get_count( ).
      mv_current_record_num = mv_current_record_num + 1.
      rv_successful_move    = abap_true.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~RECORD_IS_LAST.
    IF mv_current_record_num = _get_count( ).
      rv_is_last = abap_true.
    ENDIF.
  endmethod.


  METHOD _add_record_num_to_cur_groupby.

    DATA: ls_iter_position_of_dataset TYPE zcl_zosql_select_processor=>ty_iterator_position,
          lt_data_sets                TYPE zcl_zosql_iterator_position=>ty_data_sets,
          ld_line_of_dataset          TYPE REF TO data.

    FIELD-SYMBOLS: <ls_data_set>            LIKE LINE OF lt_data_sets.

    READ TABLE mt_iter_positions_of_lines INDEX iv_record_num_in_not_grouped INTO ls_iter_position_of_dataset.
    IF sy-subrc <> 0.
      MESSAGE e079 WITH 'ZCL_ZOSQL_GROUPBY_ITERATOR'
                        '_ADD_RECORD_NUM_TO_CUR_GROUPBY'
                        '1' INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    lt_data_sets = ls_iter_position_of_dataset-iterator_position->get_data_set_list( ).

    LOOP AT lt_data_sets ASSIGNING <ls_data_set>.
      ld_line_of_dataset = ls_iter_position_of_dataset-iterator_position->get_line_for_data_set_ref( <ls_data_set>-dataset_name ).
      io_iterator_pos_groupby->add_data_set_data( iv_dataset_name        = <ls_data_set>-dataset_name
                                                  iv_dataset_alias       = <ls_data_set>-dataset_alias
                                                  id_ref_to_current_line = ld_line_of_dataset ).
    ENDLOOP.
  ENDMETHOD.


  METHOD _create_dynamic_table_for_keys.

    DATA: ls_iter_pos           TYPE zcl_zosql_select_processor=>ty_iterator_position,
          lt_components_of_keys TYPE cl_abap_structdescr=>component_table,
          lo_struct_of_keys     TYPE REF TO cl_abap_structdescr,
          lo_table_of_keys      TYPE REF TO cl_abap_tabledescr,
          ld_value              TYPE REF TO data.

    FIELD-SYMBOLS: <ls_group_by_key_field> LIKE LINE OF mt_group_by_key_fields,
                   <ls_component>          LIKE LINE OF lt_components_of_keys.

    READ TABLE mt_iter_positions_of_lines INDEX 1 INTO ls_iter_pos.

    LOOP AT mt_group_by_key_fields ASSIGNING <ls_group_by_key_field>.
      ld_value =
        ls_iter_pos-iterator_position->get_field_ref_of_data_set(
          iv_dataset_name_or_alias = <ls_group_by_key_field>-dataset_name_or_alias
          iv_fieldname             = <ls_group_by_key_field>-fieldname ).

      IF ld_value IS INITIAL.
        MESSAGE e057 WITH <ls_group_by_key_field>-fieldname INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.

      APPEND INITIAL LINE TO lt_components_of_keys ASSIGNING <ls_component>.
      <ls_component>-name = <ls_group_by_key_field>-fieldname.
      <ls_component>-type ?= cl_abap_typedescr=>describe_by_data_ref( ld_value ).
    ENDLOOP.

    lo_struct_of_keys = cl_abap_structdescr=>create( lt_components_of_keys ).
    lo_table_of_keys = cl_abap_tabledescr=>create( lo_struct_of_keys ).

    CREATE DATA md_group_by_key_values TYPE HANDLE lo_table_of_keys.
  ENDMETHOD.


  method _FILL_DYNAMIC_TABLE_WITH_KEYS.

    DATA: ld_key                    TYPE REF TO data,
          ld_value                  TYPE REF TO data,
          ls_iter_pos               TYPE zcl_zosql_select_processor=>ty_iterator_position.

    FIELD-SYMBOLS: <ls_group_by_key_field> LIKE LINE OF mt_group_by_key_fields,
                   <lt_key_values>         TYPE STANDARD TABLE,
                   <lt_table_not_grouped>  TYPE STANDARD TABLE,
                   <ls_table_line>         TYPE any,
                   <ls_key>                TYPE any,
                   <lv_value>              TYPE any,
                   <lv_value_in_key>       TYPE any.

    ASSIGN md_group_by_key_values->* TO <lt_key_values>.

    ASSIGN md_table_not_grouped->* TO <lt_table_not_grouped>.
    CREATE DATA ld_key LIKE LINE OF <lt_key_values>.
    ASSIGN ld_key->* TO <ls_key>.

    LOOP AT <lt_table_not_grouped> ASSIGNING <ls_table_line>.
      READ TABLE mt_iter_positions_of_lines INDEX sy-tabix INTO ls_iter_pos.

      CLEAR <ls_key>.

      LOOP AT mt_group_by_key_fields ASSIGNING <ls_group_by_key_field>.
        ld_value =
          ls_iter_pos-iterator_position->get_field_ref_of_data_set(
            iv_dataset_name_or_alias = <ls_group_by_key_field>-dataset_name_or_alias
            iv_fieldname             = <ls_group_by_key_field>-fieldname ).

        ASSIGN ld_value->* TO <lv_value>.
        ASSIGN COMPONENT <ls_group_by_key_field>-fieldname OF STRUCTURE <ls_key> TO <lv_value_in_key>.
        <lv_value_in_key> = <lv_value>.
      ENDLOOP.

      READ TABLE <lt_key_values> FROM <ls_key> TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND <ls_key> TO <lt_key_values>.
      ENDIF.
    ENDLOOP.
  endmethod.


  METHOD _fill_key_values.
    _create_dynamic_table_for_keys( ).
    _fill_dynamic_table_with_keys( ).
  ENDMETHOD.


  method _GET_COUNT.
    FIELD-SYMBOLS: <lt_table_with_keys> TYPE STANDARD TABLE.

    IF mt_group_by_key_fields IS NOT INITIAL.
      ASSIGN md_group_by_key_values->* TO <lt_table_with_keys>.
      rv_count = LINES( <lt_table_with_keys> ).
    ELSE.
      rv_count = 1.
    ENDIF.
  endmethod.


  METHOD _record_equals_to_current_key.

    DATA: ls_iter_position_of_dataset TYPE zcl_zosql_select_processor=>ty_iterator_position,
          ld_ref_to_groupby_key_field TYPE REF TO data.

    FIELD-SYMBOLS: <lt_group_by_key_values>     TYPE STANDARD TABLE,
                   <ls_current_groupby_key>     TYPE any,
                   <ls_group_by_key_field>      LIKE LINE OF mt_group_by_key_fields,
                   <lv_value_of_key_in_cur_rec> TYPE any,
                   <lv_value_of_key>            TYPE any.

    IF mt_group_by_key_fields IS NOT INITIAL.
      ASSIGN md_group_by_key_values->* TO <lt_group_by_key_values>.
      READ TABLE <lt_group_by_key_values> INDEX mv_current_record_num ASSIGNING <ls_current_groupby_key>.
      IF sy-subrc <> 0.
        MESSAGE e079 WITH 'ZCL_ZOSQL_GROUPBY_ITERATOR'
                          '_RECORD_EQUALS_TO_CURRENT_KEY'
                          '1' INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.
    ENDIF.

    READ TABLE mt_iter_positions_of_lines INDEX iv_record_num_in_not_grouped INTO ls_iter_position_of_dataset.
    IF sy-subrc <> 0.
      MESSAGE e079 WITH 'ZCL_ZOSQL_GROUPBY_ITERATOR'
                        '_RECORD_EQUALS_TO_CURRENT_KEY'
                        '2' INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    rv_equals_to_groupby_key = abap_true.
    LOOP AT mt_group_by_key_fields ASSIGNING <ls_group_by_key_field>.

      ld_ref_to_groupby_key_field =
        ls_iter_position_of_dataset-iterator_position->get_field_ref_of_data_set(
          iv_dataset_name_or_alias = <ls_group_by_key_field>-dataset_name_or_alias
          iv_fieldname             = <ls_group_by_key_field>-fieldname ).

      UNASSIGN <lv_value_of_key_in_cur_rec>.
      IF ld_ref_to_groupby_key_field IS BOUND.
        ASSIGN ld_ref_to_groupby_key_field->* TO <lv_value_of_key_in_cur_rec>.
      ENDIF.

      IF <lv_value_of_key_in_cur_rec> IS NOT ASSIGNED.
        MESSAGE e079 WITH 'ZCL_ZOSQL_GROUPBY_ITERATOR'
                          '_RECORD_EQUALS_TO_CURRENT_KEY'
                          '3' INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.

      ASSIGN COMPONENT <ls_group_by_key_field>-fieldname OF STRUCTURE <ls_current_groupby_key>
        TO <lv_value_of_key>.

      IF sy-subrc <> 0.
        MESSAGE e079 WITH 'ZCL_ZOSQL_GROUPBY_ITERATOR'
                          '_RECORD_EQUALS_TO_CURRENT_KEY'
                          '4' INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.

      IF <lv_value_of_key> <> <lv_value_of_key_in_cur_rec>.
        rv_equals_to_groupby_key = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
