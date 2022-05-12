class ZCL_ZOSQL_ORDERBY_PROCESSOR definition
  public
  inheriting from ZCL_ZOSQL_PROCESSOR_BASE
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IO_ITERATOR type ref to ZIF_ZOSQL_ITERATOR
    raising
      ZCX_ZOSQL_ERROR .
  methods APPLY_ORDER_BY
    importing
      !IO_SELECT type ref to ZCL_ZOSQL_SELECT_PROCESSOR
    changing
      !CT_DATA_SET type STANDARD TABLE .
protected section.
private section.

  types:
    BEGIN OF ty_order_by_field,
           dataset_name_or_alias TYPE string,
           fieldname             TYPE fieldname,
           descending            TYPE abap_bool,
         END OF ty_order_by_field .
  types:
    ty_order_by_fields TYPE STANDARD TABLE OF ty_order_by_field WITH DEFAULT KEY .

  data MT_ORDER_BY_FIELDS type TY_ORDER_BY_FIELDS .

  methods _GET_ORDER_BY_FIELDNAME
    importing
      !IV_ORDER_BY_FIELD_NUMBER type I
    returning
      value(RV_TECHNICAL_FIELDNAME) type FIELDNAME .
  methods _GET_TAB_WITH_SORTFIELDS_ADDED
    importing
      !IO_SELECT type ref to ZCL_ZOSQL_SELECT_PROCESSOR
    returning
      value(RD_REF_TO_TABLE) type ref to DATA .
  methods _INIT_ORDER_BY_PRIMARY_KEY
    importing
      !IO_ITERATOR type ref to ZIF_ZOSQL_ITERATOR .
ENDCLASS.



CLASS ZCL_ZOSQL_ORDERBY_PROCESSOR IMPLEMENTATION.


  METHOD apply_order_by.

    DATA: lt_sortorder_tab TYPE abap_sortorder_tab,
          ls_sortorder     TYPE abap_sortorder,
          ld_tab_with_sortfields TYPE REF TO data.

    FIELD-SYMBOLS: <lt_tab_with_sortfields> TYPE STANDARD TABLE.

    FIELD-SYMBOLS: <ls_order_by_field> LIKE LINE OF mt_order_by_fields.

    IF mt_order_by_fields IS INITIAL.
      RETURN.
    ENDIF.

    ld_tab_with_sortfields = _get_tab_with_sortfields_added( io_select ).
    IF ld_tab_with_sortfields IS NOT BOUND.
      RETURN.
    ENDIF.

    LOOP AT mt_order_by_fields ASSIGNING <ls_order_by_field>.
      ls_sortorder-name       = _get_order_by_fieldname( sy-tabix ).
      ls_sortorder-descending = <ls_order_by_field>-descending.
      APPEND ls_sortorder TO lt_sortorder_tab.
    ENDLOOP.

    ASSIGN ld_tab_with_sortfields->* TO <lt_tab_with_sortfields>.
    SORT <lt_tab_with_sortfields> BY (lt_sortorder_tab).

    REFRESH ct_data_set.
    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_tab_with_sortfields>
                                               IMPORTING et_table_dest = ct_data_set ).
  ENDMETHOD.


  METHOD constructor.

    DATA: lo_parser_helper           TYPE REF TO zcl_zosql_parser_helper,
          lo_order_by_node           TYPE REF TO zcl_zosql_parser_node,
          lt_child_nodes_of_order_by TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_child_node              TYPE REF TO zcl_zosql_parser_node.

    FIELD-SYMBOLS: <ls_order_by_field> LIKE LINE OF mt_order_by_fields.

    super->constructor( ).

    CREATE OBJECT lo_parser_helper
      EXPORTING
        io_sql_parser = io_sql_parser.

    lo_parser_helper->get_key_nodes_of_sql_select( IMPORTING eo_node_order_by = lo_order_by_node ).

    IF lo_order_by_node->exists_child_node_with_type(
        zcl_zosql_parser_recurs_desc=>node_type-order_by_primary_key ) = abap_true.

      _init_order_by_primary_key( io_iterator ).
      RETURN.
    ENDIF.

    lt_child_nodes_of_order_by = lo_order_by_node->get_child_nodes( ).

    LOOP AT lt_child_nodes_of_order_by INTO lo_child_node
      WHERE table_line->node_type = zcl_zosql_parser_recurs_desc=>node_type-order_by_field.

      APPEND INITIAL LINE TO mt_order_by_fields ASSIGNING <ls_order_by_field>.
      FIND FIRST OCCURRENCE OF '~' IN lo_child_node->token.
      IF sy-subrc = 0.
        SPLIT lo_child_node->token_ucase AT '~' INTO <ls_order_by_field>-dataset_name_or_alias <ls_order_by_field>-fieldname.
      ELSE.
        <ls_order_by_field>-fieldname = lo_child_node->token_ucase.
      ENDIF.

      IF lo_child_node->exists_child_node_with_type(
            zcl_zosql_parser_recurs_desc=>node_type-order_by_descending ) = abap_true.

        <ls_order_by_field>-descending = abap_true.
      ENDIF.
    ENDLOOP.

    fill_dataset_where_empty( EXPORTING io_iterator            = io_iterator
                              CHANGING  ct_table_where_to_fill = mt_order_by_fields ).
  ENDMETHOD.


  method _GET_ORDER_BY_FIELDNAME.
    rv_technical_fieldname = |__ORDERBY_{ iv_order_by_field_number }|.
  endmethod.


  METHOD _get_tab_with_sortfields_added.

    DATA: ld_select_data_set         TYPE REF TO data,
          lt_iter_positions_of_lines TYPE zcl_zosql_select_processor=>ty_iterator_positions,
          lo_table                   TYPE REF TO cl_abap_tabledescr,
          lo_struct                  TYPE REF TO cl_abap_structdescr,
          lt_comp_simple             TYPE cl_abap_structdescr=>included_view,
          lv_order_by_field_index    TYPE i,
          lo_first_iter_pos          TYPE REF TO zcl_zosql_iterator_position,
          ld_ref_to_field            TYPE REF TO data,
          lo_struct_added            TYPE REF TO cl_abap_structdescr,
          lo_table_added             TYPE REF TO cl_abap_tabledescr,
          lt_comp                    TYPE cl_abap_structdescr=>component_table,
          lv_line_index              TYPE i,
          lv_orderby_fieldname       TYPE fieldname,
          lo_iter_pos                TYPE REF TO zcl_zosql_iterator_position.

    FIELD-SYMBOLS: <ls_comp_simple>              LIKE LINE OF lt_comp_simple,
                   <ls_order_by_field>           LIKE LINE OF mt_order_by_fields,
                   <lt_select_data_set>          TYPE STANDARD TABLE,
                   <lt_result_with_added_fields> TYPE STANDARD TABLE,
                   <ls_table_line>               TYPE any,
                   <lv_orderby_field>            TYPE any,
                   <lv_value_of_orderby_field>   TYPE any.

    ld_select_data_set = io_select->get_result_as_ref_to_data( ).
    lt_iter_positions_of_lines = io_select->get_iter_positions_of_lines( ).

    lo_table ?= cl_abap_tabledescr=>describe_by_data_ref( ld_select_data_set ).
    lo_struct ?= lo_table->get_table_line_type( ).
    lt_comp_simple = lo_struct->get_included_view( ).

    READ TABLE lt_iter_positions_of_lines INDEX 1 INTO lo_first_iter_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT mt_order_by_fields ASSIGNING <ls_order_by_field>.
      lv_order_by_field_index = sy-tabix.

      APPEND INITIAL LINE TO lt_comp_simple ASSIGNING <ls_comp_simple>.
      <ls_comp_simple>-name = _get_order_by_fieldname( lv_order_by_field_index ).

      ld_ref_to_field =
        lo_first_iter_pos->get_field_ref_of_data_set(
          iv_dataset_name_or_alias = <ls_order_by_field>-dataset_name_or_alias
          iv_fieldname             = <ls_order_by_field>-fieldname ).

      <ls_comp_simple>-type ?= cl_abap_typedescr=>describe_by_data_ref( ld_ref_to_field ).
    ENDLOOP.

    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = lt_comp_simple
                                               IMPORTING et_table_dest = lt_comp ).
    lo_struct_added = cl_abap_structdescr=>create( lt_comp ).
    lo_table_added = cl_abap_tabledescr=>create( lo_struct_added ).

    CREATE DATA rd_ref_to_table TYPE HANDLE lo_table_added.

    ASSIGN ld_select_data_set->* TO <lt_select_data_set>.
    ASSIGN rd_ref_to_table->* TO <lt_result_with_added_fields>.

    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = <lt_select_data_set>
                                               IMPORTING et_table_dest = <lt_result_with_added_fields> ).

    LOOP AT <lt_result_with_added_fields> ASSIGNING <ls_table_line>.
      lv_line_index = sy-tabix.

      READ TABLE lt_iter_positions_of_lines INDEX lv_line_index INTO lo_iter_pos.

      LOOP AT mt_order_by_fields ASSIGNING <ls_order_by_field>.
        lv_orderby_fieldname = _get_order_by_fieldname( sy-tabix ).
        ASSIGN COMPONENT lv_orderby_fieldname OF STRUCTURE <ls_table_line> TO <lv_orderby_field>.

        ld_ref_to_field =
          lo_iter_pos->get_field_ref_of_data_set(
            iv_dataset_name_or_alias = <ls_order_by_field>-dataset_name_or_alias
            iv_fieldname             = <ls_order_by_field>-fieldname ).

        ASSIGN ld_ref_to_field->* TO <lv_value_of_orderby_field>.
        <lv_orderby_field> = <lv_value_of_orderby_field>.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD _init_order_by_primary_key.

    DATA: lt_data_sets                   TYPE zif_zosql_iterator=>ty_data_sets,
          ls_first_data_set              TYPE zif_zosql_iterator=>ty_data_set,
          lt_key_fields_of_first_dataset TYPE fieldname_table.

    FIELD-SYMBOLS: <ls_key_field>      LIKE LINE OF lt_key_fields_of_first_dataset,
                   <ls_order_by_field> LIKE LINE OF mt_order_by_fields.

    lt_data_sets = io_iterator->get_data_set_list( ).
    READ TABLE lt_data_sets INDEX 1 INTO ls_first_data_set.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lt_key_fields_of_first_dataset = io_iterator->get_key_fields_of_data_set( ls_first_data_set-dataset_name ).

    LOOP AT lt_key_fields_of_first_dataset ASSIGNING <ls_key_field>.
      APPEND INITIAL LINE TO mt_order_by_fields ASSIGNING <ls_order_by_field>.
      <ls_order_by_field>-fieldname = <ls_key_field>-fieldname.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
