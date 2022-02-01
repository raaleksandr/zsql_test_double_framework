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
protected section.
private section.

  data MD_TABLE_NOT_GROUPED type ref to DATA .
  data MD_GROUP_BY_KEY_VALUES type ref to DATA .
  data MT_GROUP_BY_KEY_FIELDS type ZCL_ZOSQL_GROUPBY_PROCESSOR=>TY_GROUP_BY_KEY_FIELDS .
  data MV_CURRENT_RECORD_NUM type INT4 .
  data MT_ITER_POSITIONS_OF_LINES type ZCL_ZOSQL_SELECT_PROCESSOR=>TY_ITERATOR_POSITIONS .

  methods _FILL_KEY_VALUES
    raising
      ZCX_ZOSQL_ERROR .
  methods _GET_COUNT
    returning
      value(RV_COUNT) type I .
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


  METHOD zif_zosql_iterator~get_iterator_position_object.

    DATA: lo_iterator_pos_groupby     TYPE REF TO zcl_zosql_groupby_iter_pos,
          lv_key_is_equal             TYPE abap_bool,
          lv_table_record_num         TYPE i,
          lo_iter_position_of_dataset TYPE REF TO zcl_zosql_iterator_position,
          lt_data_sets                TYPE zcl_zosql_iterator_position=>ty_data_sets,
          ld_line_of_dataset          TYPE REF TO data.

    FIELD-SYMBOLS: <lt_group_by_key_values> TYPE STANDARD TABLE,
                   <ls_current_key>         TYPE any,
                   <lt_table_not_grouped>   TYPE STANDARD TABLE,
                   <ls_table_line>          TYPE any,
                   <ls_group_by_key_field>  LIKE LINE OF mt_group_by_key_fields,
                   <lv_value_in_key>        TYPE any,
                   <lv_value_in_table>      TYPE any,
                   <ls_data_set>            LIKE LINE OF lt_data_sets.

    ASSIGN md_group_by_key_values->* TO <lt_group_by_key_values>.
    READ TABLE <lt_group_by_key_values> INDEX mv_current_record_num ASSIGNING <ls_current_key>.
    IF sy-subrc <> 0.
      MESSAGE e079 WITH 'ZCL_ZOSQL_GROUPBY_ITERATOR'
                        'ZIF_ZOSQL_ITERATOR~GET_ITERATOR_POSITION_OBJECT'
                        '1' INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    CREATE OBJECT lo_iterator_pos_groupby.

    ASSIGN md_table_not_grouped->* TO <lt_table_not_grouped>.

    LOOP AT <lt_table_not_grouped> ASSIGNING <ls_table_line>.

      lv_table_record_num = sy-tabix.

      lv_key_is_equal = abap_true.
      LOOP AT mt_group_by_key_fields ASSIGNING <ls_group_by_key_field>.

        ASSIGN COMPONENT <ls_group_by_key_field>-fieldname OF STRUCTURE <ls_table_line>
          TO <lv_value_in_table>.

        IF sy-subrc <> 0.
          MESSAGE e079 WITH 'ZCL_ZOSQL_GROUPBY_ITERATOR'
                            'ZIF_ZOSQL_ITERATOR~GET_ITERATOR_POSITION_OBJECT'
                            '2' INTO zcl_zosql_utils=>dummy.
          zcl_zosql_utils=>raise_exception_from_sy_msg( ).
        ENDIF.

        ASSIGN COMPONENT <ls_group_by_key_field>-fieldname OF STRUCTURE <ls_current_key>
          TO <lv_value_in_key>.

        IF sy-subrc <> 0.
          MESSAGE e079 WITH 'ZCL_ZOSQL_GROUPBY_ITERATOR'
                            'ZIF_ZOSQL_ITERATOR~GET_ITERATOR_POSITION_OBJECT'
                            '3' INTO zcl_zosql_utils=>dummy.
          zcl_zosql_utils=>raise_exception_from_sy_msg( ).
        ENDIF.

        IF <lv_value_in_key> <> <lv_value_in_table>.
          lv_key_is_equal = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.

      CHECK lv_key_is_equal = abap_true.

      READ TABLE mt_iter_positions_of_lines INDEX lv_table_record_num INTO lo_iter_position_of_dataset.
      IF sy-subrc <> 0.
        MESSAGE e079 WITH 'ZCL_ZOSQL_GROUPBY_ITERATOR'
                          'ZIF_ZOSQL_ITERATOR~GET_ITERATOR_POSITION_OBJECT'
                          '3' INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.

      lt_data_sets = lo_iter_position_of_dataset->get_data_set_list( ).

      LOOP AT lt_data_sets ASSIGNING <ls_data_set>.
        ld_line_of_dataset = lo_iter_position_of_dataset->get_line_for_data_set_ref( <ls_data_set>-dataset_name ).
        lo_iterator_pos_groupby->add_data_set_data( iv_dataset_name        = <ls_data_set>-dataset_name
                                                    iv_dataset_alias       = <ls_data_set>-dataset_alias
                                                    id_ref_to_current_line = ld_line_of_dataset ).
      ENDLOOP.
    ENDLOOP.

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


  METHOD _fill_key_values.

    DATA: lo_struct_of_keys         TYPE REF TO cl_abap_structdescr,
          lt_components_of_keys     TYPE cl_abap_structdescr=>component_table,
          lo_table_of_keys          TYPE REF TO cl_abap_tabledescr,
          lo_table_not_grouped      TYPE REF TO cl_abap_tabledescr,
          lo_struct_not_grouped     TYPE REF TO cl_abap_structdescr,
          lt_components_not_grouped TYPE cl_abap_structdescr=>component_table,
          ld_key                    TYPE REF TO data,
          lo_iter_pos               TYPE REF TO zcl_zosql_iterator_position,
          ld_value                  TYPE REF TO data.

    FIELD-SYMBOLS: <ls_group_by_key_field> LIKE LINE OF mt_group_by_key_fields,
                   <ls_component>          LIKE LINE OF lt_components_of_keys,
                   <lt_key_values>         TYPE STANDARD TABLE,
                   <lt_table_not_grouped>  TYPE STANDARD TABLE,
                   <ls_table_line>         TYPE any,
                   <ls_key>                TYPE any.

    lo_table_not_grouped ?= cl_abap_tabledescr=>describe_by_data_ref( md_table_not_grouped ).
    lo_struct_not_grouped ?= lo_table_not_grouped->get_table_line_type( ).
    lt_components_not_grouped = lo_struct_not_grouped->get_components( ).

    READ TABLE mt_iter_positions_of_lines INDEX 1 INTO lo_iter_pos.

    LOOP AT mt_group_by_key_fields ASSIGNING <ls_group_by_key_field>.
      READ TABLE lt_components_not_grouped WITH KEY name = <ls_group_by_key_field>-fieldname
        ASSIGNING <ls_component>.
      IF sy-subrc = 0.
        APPEND <ls_component> TO lt_components_of_keys.
      ELSE.
        ld_value =
          lo_iter_pos->get_field_ref_of_data_set(
            iv_dataset_name_or_alias = <ls_group_by_key_field>-dataset_name_or_alias
            iv_fieldname             = <ls_group_by_key_field>-fieldname ).

        IF ld_value IS INITIAL.
          MESSAGE e057 WITH <ls_group_by_key_field>-fieldname INTO zcl_zosql_utils=>dummy.
          zcl_zosql_utils=>raise_exception_from_sy_msg( ).
        ENDIF.

        APPEND INITIAL LINE TO lt_components_of_keys ASSIGNING <ls_component>.
        <ls_component>-name = <ls_group_by_key_field>-fieldname.
        <ls_component>-type ?= cl_abap_typedescr=>describe_by_data_ref( ld_value ).
      ENDIF.
    ENDLOOP.

    lo_struct_of_keys = cl_abap_structdescr=>create( lt_components_of_keys ).
    lo_table_of_keys = cl_abap_tabledescr=>create( lo_struct_of_keys ).

    CREATE DATA md_group_by_key_values TYPE HANDLE lo_table_of_keys.
    ASSIGN md_group_by_key_values->* TO <lt_key_values>.

    ASSIGN md_table_not_grouped->* TO <lt_table_not_grouped>.
    CREATE DATA ld_key LIKE LINE OF <lt_key_values>.
    ASSIGN ld_key->* TO <ls_key>.

    LOOP AT <lt_table_not_grouped> ASSIGNING <ls_table_line>.
      CLEAR <ls_key>.
      MOVE-CORRESPONDING <ls_table_line> TO <ls_key>.

      READ TABLE <lt_key_values> FROM <ls_key> TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND <ls_key> TO <lt_key_values>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  method _GET_COUNT.
    FIELD-SYMBOLS: <lt_table_with_keys> TYPE STANDARD TABLE.

    ASSIGN md_group_by_key_values->* TO <lt_table_with_keys>.
    rv_count = LINES( <lt_table_with_keys> ).
  endmethod.
ENDCLASS.
