class ZCL_ZOSQL_SUBQUERY_IN_WHERE definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT
      !IV_SUBQUERY_SQL type CLIKE
      !IO_ITERATION_POSITION_PARENT type ref to ZCL_ZOSQL_ITERATOR_POSITION optional
      !IO_ITERATOR_PARENT type ref to ZIF_ZOSQL_ITERATOR optional
      !IO_PARAMETERS_OF_PARENT_QUERY type ref to ZCL_ZOSQL_PARAMETERS
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE .
  methods RUN_SUBQUERY_AND_GET_RESULT
    returning
      value(RD_REF_TO_RESULT_AS_TABLE) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
protected section.

  data MV_NEW_SYNTAX type ABAP_BOOL .
private section.

  data MO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT .
  data MV_SUBQUERY_SQL type STRING .
  data MO_CURRENT_REC_OF_PARENT_QUERY type ref to ZCL_ZOSQL_ITERATOR_POSITION .
  data MO_ITERATOR_OF_PARENT_QUERY type ref to ZIF_ZOSQL_ITERATOR .
  data MO_PARAMETERS_OF_PARENT_QUERY type ref to ZCL_ZOSQL_PARAMETERS .
  data MO_SUBQUERY_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC .

  methods _ADD_DATASETS_OF_PARENT_QUERY
    importing
      !IO_ITERATOR type ref to ZIF_ZOSQL_ITERATOR
    raising
      ZCX_ZOSQL_ERROR .
  methods _GET_PARAMETERS_FROM_PARENT
    returning
      value(RT_PARAMETERS_OF_PARENT) type ZOSQL_DB_LAYER_PARAMS .
  methods _GET_SUBQUERY_FROM_ITERATOR
    returning
      value(RO_ITERATOR) type ref to ZIF_ZOSQL_ITERATOR
    raising
      ZCX_ZOSQL_ERROR .
  methods _RUN_QUERY_AND_GET_RESULT
    importing
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS
    returning
      value(RD_REF_TO_RESULT_AS_TABLE) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods _DATA_SET_EXISTS_IN_PARENT
    importing
      !IV_DATASET_NAME_OR_ALIAS type CLIKE
    returning
      value(RV_EXISTS) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _DATA_SET_EXISTS_IN_SUBQUERY
    importing
      !IV_DATASET_NAME_OR_ALIAS type CLIKE
    returning
      value(RV_EXISTS) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _GET_DATA_SET_LIST_OF_PARENT
    returning
      value(RT_DATA_SET_LIST_OF_PARENT) type ZIF_ZOSQL_ITERATOR=>TY_DATA_SETS .
  methods _GET_DATA_SET_LIST_OF_SUBQUERY
    returning
      value(RT_DATA_SET_LIST_OF_SUBQUERY) type ZIF_ZOSQL_ITERATOR=>TY_DATA_SETS
    raising
      ZCX_ZOSQL_ERROR .
  methods _GET_SUBQUERY_DATASET_USAGES
    returning
      value(RT_OPERANDS_WITH_DATASETS) type ZIF_ZOSQL_EXPRESSION_PROCESSOR=>TY_OPERANDS
    raising
      ZCX_ZOSQL_ERROR .
  methods _PARSE_SUBQUERY_SQL
    raising
      ZCX_ZOSQL_ERROR .
  methods _ADD_PARENT_FIELDS_TO_PARAMS
    returning
      value(RT_PARENT_FIELDS_AS_PARAMETERS) type ZOSQL_DB_LAYER_PARAMS
    raising
      ZCX_ZOSQL_ERROR .
ENDCLASS.



CLASS ZCL_ZOSQL_SUBQUERY_IN_WHERE IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA: lo_factory TYPE REF TO zif_zosql_factory.

    IF io_zosql_test_environment IS BOUND.
      mo_zosql_test_environment = io_zosql_test_environment.
    ELSE.
      CREATE OBJECT lo_factory TYPE zcl_zosql_factory.
      mo_zosql_test_environment = lo_factory->get_test_environment( ).
    ENDIF.

    mv_subquery_sql                = iv_subquery_sql.
    mo_current_rec_of_parent_query = io_iteration_position_parent.
    mo_iterator_of_parent_query    = io_iterator_parent.
    mo_parameters_of_parent_query  = io_parameters_of_parent_query.
    mv_new_syntax                  = iv_new_syntax.
  endmethod.


  METHOD run_subquery_and_get_result.

    DATA: lt_parameters_field_values TYPE zosql_db_layer_params,
          lt_parameters              TYPE zosql_db_layer_params.

    lt_parameters = _get_parameters_from_parent( ).

    lt_parameters_field_values = _add_parent_fields_to_params( ).
    APPEND LINES OF lt_parameters_field_values TO lt_parameters.

    rd_ref_to_result_as_table = _run_query_and_get_result( lt_parameters ).
  ENDMETHOD.


  METHOD _add_datasets_of_parent_query.

    DATA: lt_datasets_of_parent TYPE zcl_zosql_iterator_position=>ty_data_sets.

    FIELD-SYMBOLS: <ls_dataset_of_parent> LIKE LINE OF lt_datasets_of_parent.

    IF mo_current_rec_of_parent_query IS BOUND.
      lt_datasets_of_parent = mo_current_rec_of_parent_query->get_data_set_list( ).
    ELSE.
      lt_datasets_of_parent = mo_iterator_of_parent_query->get_data_set_list( ).
    ENDIF.

    LOOP AT lt_datasets_of_parent ASSIGNING <ls_dataset_of_parent>.
      io_iterator->add_additional_dataset( iv_dataset_name  = <ls_dataset_of_parent>-dataset_name
                                           iv_dataset_alias = <ls_dataset_of_parent>-dataset_alias ).
    ENDLOOP.
  ENDMETHOD.


  METHOD _ADD_PARENT_FIELDS_TO_PARAMS.

* Referenced fields of parent query we consider as external parameters
* when running subquery

    DATA: lt_operands                    TYPE zif_zosql_expression_processor=>ty_operands,
          ld_ref_to_parent_dataset_value TYPE REF TO data.

    FIELD-SYMBOLS: <ls_operand>               LIKE LINE OF lt_operands,
                   <ls_parameter>             LIKE LINE OF rt_parent_fields_as_parameters.

    _parse_subquery_sql( ).

    lt_operands = _get_subquery_dataset_usages( ).

    LOOP AT lt_operands ASSIGNING <ls_operand>
      WHERE dataset_name_or_alias IS NOT INITIAL.

      IF _data_set_exists_in_subquery( <ls_operand>-dataset_name_or_alias ) <> abap_true
        AND _data_set_exists_in_parent( <ls_operand>-dataset_name_or_alias ) = abap_true.

        APPEND INITIAL LINE TO rt_parent_fields_as_parameters ASSIGNING <ls_parameter>.

        CONCATENATE <ls_operand>-dataset_name_or_alias
          <ls_operand>-fieldname
          INTO <ls_parameter>-param_name_in_select
          SEPARATED BY '~'.

        CHECK mo_current_rec_of_parent_query IS BOUND.

        ld_ref_to_parent_dataset_value =
          mo_current_rec_of_parent_query->get_field_ref_of_data_set(
            iv_dataset_name_or_alias = <ls_operand>-dataset_name_or_alias
            iv_fieldname             = <ls_operand>-fieldname ).

        CHECK ld_ref_to_parent_dataset_value IS BOUND.

        <ls_parameter>-parameter_value_ref = ld_ref_to_parent_dataset_value.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD _data_set_exists_in_parent.
    IF mo_iterator_of_parent_query IS BOUND.
      rv_exists = mo_iterator_of_parent_query->check_data_set_exists( iv_dataset_name_or_alias ).
    ELSE.
      rv_exists = mo_current_rec_of_parent_query->check_data_set_exists( iv_dataset_name_or_alias ).
    ENDIF.
  ENDMETHOD.


  METHOD _data_set_exists_in_subquery.

    DATA: lv_dataset_name_or_alias TYPE string,
          lt_data_sets_subquery    TYPE zif_zosql_iterator=>ty_data_sets.

    lv_dataset_name_or_alias = zcl_zosql_utils=>to_upper_case( iv_dataset_name_or_alias ).
    lt_data_sets_subquery = _get_data_set_list_of_subquery( ).

    READ TABLE lt_data_sets_subquery WITH KEY dataset_name = lv_dataset_name_or_alias
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      READ TABLE lt_data_sets_subquery WITH KEY dataset_alias = lv_dataset_name_or_alias
        TRANSPORTING NO FIELDS.
    ENDIF.

    IF sy-subrc = 0.
      rv_exists = abap_true.
    ENDIF.
  ENDMETHOD.


  method _GET_DATA_SET_LIST_OF_PARENT.
    rt_data_set_list_of_parent = mo_current_rec_of_parent_query->get_data_set_list( ).
  endmethod.


  METHOD _get_data_set_list_of_subquery.

    DATA: lo_subquery_from               TYPE REF TO zif_zosql_iterator.

    CREATE OBJECT lo_subquery_from TYPE zcl_zosql_from_iterator
      EXPORTING
        io_zosql_test_environment = mo_zosql_test_environment
        io_sql_parser             = mo_subquery_sql_parser.

    rt_data_set_list_of_subquery = lo_subquery_from->get_data_set_list( ).
  ENDMETHOD.


  method _GET_PARAMETERS_FROM_PARENT.
    rt_parameters_of_parent = mo_parameters_of_parent_query->get_all_parameters( ).
  endmethod.


  method _GET_SUBQUERY_DATASET_USAGES.

    DATA: lo_subquery_where              TYPE REF TO zif_zosql_expression_processor,
          lo_node_where                  TYPE REF TO zcl_zosql_parser_node,
          lo_zosql_parser_helper         TYPE REF TO zcl_zosql_parser_helper,
          lo_from_iterator               TYPE REF TO zif_zosql_iterator.

    lo_from_iterator = _get_subquery_from_iterator( ).

    CREATE OBJECT lo_subquery_where TYPE zcl_zosql_where_processor
      EXPORTING
        io_zosql_test_environment = mo_zosql_test_environment
        io_parameters             = mo_parameters_of_parent_query
        io_iterator               = lo_from_iterator
        iv_new_syntax             = mv_new_syntax.

    CREATE OBJECT lo_zosql_parser_helper
      EXPORTING
        io_sql_parser = mo_subquery_sql_parser.
    lo_zosql_parser_helper->get_key_nodes_of_sql_select( IMPORTING eo_node_where = lo_node_where ).

    lo_subquery_where->initialize_by_parsed_sql( lo_node_where ).

    rt_operands_with_datasets = lo_subquery_where->get_list_of_operands( ).
  endmethod.


  method _GET_SUBQUERY_FROM_ITERATOR.

    DATA: lo_from_iterator TYPE REF TO zcl_zosql_from_iterator.

    CREATE OBJECT lo_from_iterator
      EXPORTING
        io_zosql_test_environment = mo_zosql_test_environment
        io_sql_parser             = mo_subquery_sql_parser
        io_parameters             = mo_parameters_of_parent_query.

    _add_datasets_of_parent_query( lo_from_iterator ).

    ro_iterator = lo_from_iterator.
  endmethod.


  method _PARSE_SUBQUERY_SQL.
    CREATE OBJECT mo_subquery_sql_parser.
    mo_subquery_sql_parser->set_sql( mv_subquery_sql ).
    mo_subquery_sql_parser->run_recursive_descent_parser( ).
  endmethod.


  method _RUN_QUERY_AND_GET_RESULT.

    DATA: lo_subquery TYPE REF TO zcl_zosql_db_layer_fake.

    CREATE OBJECT lo_subquery
      EXPORTING
        io_zosql_test_environment = mo_zosql_test_environment.

    lo_subquery->zif_zosql_db_layer~select( EXPORTING iv_select          = mv_subquery_sql
                                                      it_parameters      = it_parameters
                                            IMPORTING ed_result_as_table = rd_ref_to_result_as_table ).
  endmethod.
ENDCLASS.
