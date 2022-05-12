class ZCL_ZOSQL_GROUPBY_PROCESSOR definition
  public
  inheriting from ZCL_ZOSQL_AGGR_FUNC_PROCESSOR
  create public .

public section.

  data MT_GROUP_BY_KEY_FIELDS type TY_GROUP_BY_KEY_FIELDS read-only .

  methods CONSTRUCTOR
    importing
      !IO_HAVING_PROCESSOR type ref to ZIF_ZOSQL_EXPRESSION_PROCESSOR optional
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IO_ITERATOR type ref to ZIF_ZOSQL_ITERATOR
    raising
      ZCX_ZOSQL_ERROR .
  methods APPLY_GROUP_BY
    importing
      !IO_SELECT type ref to ZCL_ZOSQL_SELECT_PROCESSOR
      !IO_HAVING type ref to ZCL_ZOSQL_HAVING_PROCESSOR optional
    changing
      !CT_DATA_SET type STANDARD TABLE
    raising
      ZCX_ZOSQL_ERROR .
protected section.
private section.

  data MO_HAVING_PROCESSOR type ref to ZIF_ZOSQL_EXPRESSION_PROCESSOR .

  methods _GET_GROUP_BY_NODE
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    returning
      value(RO_NODE_GROUP_BY) type ref to ZCL_ZOSQL_PARSER_NODE .
  methods _INITIALIZE_BY_PARSED_SQL
    importing
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
      !IO_ITERATOR type ref to ZIF_ZOSQL_ITERATOR
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
        add_record_to_grouped_table( EXPORTING io_iteration_position = lo_iter_pos
                                               io_select             = io_select
                                     CHANGING  ct_grouped_table      = <lt_grouped_table> ).
      ENDIF.

      lv_not_end_of_data = lo_groupby_iterator->zif_zosql_iterator~move_to_next( ).
    ENDWHILE.

    ct_data_set = <lt_grouped_table>.
  ENDMETHOD.


  method CONSTRUCTOR.
    super->constructor( ).
    mo_having_processor = io_having_processor.
    _initialize_by_parsed_sql( io_sql_parser = io_sql_parser
                               io_iterator   = io_iterator ).
  endmethod.


  method _GET_GROUP_BY_NODE.

    DATA: lo_parser_helper TYPE REF TO zcl_zosql_parser_helper.

    CREATE OBJECT lo_parser_helper
      EXPORTING
        io_sql_parser = io_sql_parser.
    lo_parser_helper->get_key_nodes_of_sql_select( IMPORTING eo_node_group_by = ro_node_group_by ).
  endmethod.


  METHOD _initialize_by_parsed_sql.

    DATA: lo_group_by_node           TYPE REF TO zcl_zosql_parser_node,
          lt_child_nodes_of_group_by TYPE zcl_zosql_parser_node=>ty_parser_nodes,
          lo_child_node              TYPE REF TO zcl_zosql_parser_node.

    FIELD-SYMBOLS: <ls_group_by_key_field> LIKE LINE OF mt_group_by_key_fields.

    lo_group_by_node = _get_group_by_node( io_sql_parser ).

    lt_child_nodes_of_group_by = lo_group_by_node->get_child_nodes( ).

    LOOP AT lt_child_nodes_of_group_by INTO lo_child_node
      WHERE table_line->node_type = zcl_zosql_parser_recurs_desc=>node_type-select_field.

      APPEND INITIAL LINE TO mt_group_by_key_fields ASSIGNING <ls_group_by_key_field>.
      FIND FIRST OCCURRENCE OF '~' IN lo_child_node->token.
      IF sy-subrc = 0.
        SPLIT lo_child_node->token_ucase AT '~' INTO <ls_group_by_key_field>-dataset_name_or_alias <ls_group_by_key_field>-fieldname.
      ELSE.
        <ls_group_by_key_field>-fieldname = lo_child_node->token_ucase.
      ENDIF.
    ENDLOOP.

    fill_dataset_where_empty( EXPORTING io_iterator            = io_iterator
                              CHANGING  ct_table_where_to_fill = mt_group_by_key_fields ).
  ENDMETHOD.
ENDCLASS.
