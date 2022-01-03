class ZCL_ZOSQL_DB_LAYER definition
  public
  inheriting from ZCL_ZOSQL_DB_LAYER_BASE
  create public .

public section.

  methods ZIF_ZOSQL_DB_LAYER~COMMIT
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~DELETE
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~DELETE_BY_ITAB
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~FETCH_NEXT_CURSOR
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~FETCH_NEXT_CURSOR_TO_ITAB
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~INSERT_BY_ITAB
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~MODIFY_BY_ITAB
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~OPEN_CURSOR
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~SELECT_TO_ITAB
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~UPDATE
    redefinition .
  methods ZIF_ZOSQL_DB_LAYER~UPDATE_BY_ITAB
    redefinition .
protected section.
private section.

  types:
    BEGIN OF ty_parameter_with_name.
           INCLUDE TYPE zosql_db_layer_param.
           TYPES: param_name TYPE fieldname,
         END OF ty_parameter_with_name .
  types:
    ty_parameters_with_name TYPE STANDARD TABLE OF ty_parameter_with_name WITH KEY param_name .
  types:
    BEGIN OF TY_DATABASE_CURSOR_PARAMETERS,
           cursor TYPE cursor,
           ref_to_result_dataset TYPE REF TO data,
         END OF TY_DATABASE_CURSOR_PARAMETERS .

  data:
    MT_DATABASE_CURSOR_PARAMETERS   TYPE STANDARD TABLE OF ty_database_cursor_parameters
                                            WITH KEY cursor .

  methods _UPDATE_BY_SQL_PARTS
    importing
      !IV_TABLE_NAME type CLIKE
      !IV_SET_STATEMENT type CLIKE
      !IV_WHERE type CLIKE
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    raising
      ZCX_ZOSQL_ERROR .
  methods _SPLIT_UPDATE_INTO_PARTS
    importing
      !IV_UPDATE_STATEMENT type CLIKE
    exporting
      !EV_TABLE_NAME type CLIKE
      !EV_SET_STATEMENT type CLIKE
      !EV_WHERE type CLIKE
      !EV_NEW_SYNTAX type ABAP_BOOL
      !EO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    raising
      ZCX_ZOSQL_ERROR .
  methods _SPLIT_DELETE_INTO_PARTS
    importing
      !IV_DELETE_STATEMENT type CLIKE
    exporting
      !EV_TABLE_NAME type CLIKE
      !EV_WHERE type CLIKE
      !EV_NEW_SYNTAX type ABAP_BOOL
      !EO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    raising
      ZCX_ZOSQL_ERROR .
  methods _DELETE_BY_SQL_PARTS
    importing
      !IV_TABLE_NAME type CLIKE
      !IV_WHERE type CLIKE
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    raising
      ZCX_ZOSQL_ERROR .
  methods _SPLIT_SELECT_INTO_PARTS
    importing
      !IV_SELECT type CLIKE
    exporting
      !EV_SELECT_FIELD_LIST type CLIKE
      !EV_FROM type CLIKE
      !EV_FOR_ALL_ENTRIES_TABNAME type CLIKE
      !EV_WHERE type CLIKE
      !EV_GROUP_BY type CLIKE
      !EV_ORDER_BY type CLIKE
      value(EV_DISTINCT) type ABAP_BOOL
      value(EV_NEW_SYNTAX) type ABAP_BOOL
      !EV_NUMBER_OF_ROWS_EXPR type CLIKE
      !EO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    raising
      ZCX_ZOSQL_ERROR .
  methods _SELECT_BY_SQL_PARTS
    importing
      !IV_SELECT type STRING default '*'
      !IV_FROM type STRING
      !IV_WHERE type STRING optional
      !IV_GROUP_BY type STRING optional
      !IV_ORDER_BY type STRING optional
      value(IV_DISTINCT) type ABAP_BOOL default ABAP_FALSE
      value(IV_NEW_SYNTAX) type ABAP_BOOL default ABAP_FALSE
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS optional
      !IT_FOR_ALL_ENTRIES_TABLE type ANY TABLE optional
      !IV_FOR_ALL_ENTRIES_TABNAME type CLIKE optional
      value(IV_DO_INTO_CORRESPONDING) type ABAP_BOOL default ABAP_TRUE
      !IV_NUMBER_OF_ROWS_EXPR type CLIKE optional
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    exporting
      !ET_RESULT_TABLE type ANY TABLE
      !ES_RESULT_LINE type ANY
      !EV_SUBRC type SYSUBRC
    raising
      ZCX_ZOSQL_ERROR .
  methods _CREATE_STANDARD_LIKE_ANY_TAB
    importing
      !IT_ANY_TABLE type ANY TABLE
    returning
      value(RD_REF_TO_STANDARD_TABLE) type ref to DATA .
  methods _PREPARE_SET_FOR_UPDATE
    importing
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS
      !IV_NAME_OF_STRUCT_WITH_PARAMS type FIELDNAME
    changing
      !CV_SET_STATEMENT type CLIKE .
  methods _PREPARE_RESULT_TABLE_FOR_SEL
    importing
      !IT_RESULT_TABLE type ANY TABLE
      !IS_RESULT_LINE type ANY
    returning
      value(RD_RESULT_TABLE) type ref to DATA .
  methods _EXECUTE_UPDATE
    importing
      !IV_TABLE_NAME type CLIKE
      !IV_SET_STATEMENT type CLIKE
      !IV_WHERE type CLIKE
      !IS_DYNAMIC_STRUCT_WITH_PARAMS type ANY .
  methods _EXECUTE_DELETE
    importing
      !IV_TABLE_NAME type CLIKE
      !IV_WHERE type CLIKE
      !IS_DYNAMIC_STRUCT_WITH_PARAMS type ANY .
  methods _CONSIDER_IF_HOST_ALREADY_WAS
    changing
      !CV_SQL type CLIKE .
  methods _CONVERT_ITAB_FOR_DB_OPERATION
    importing
      !IV_TABLE_NAME type CLIKE
      !IT_INPUT_ITAB type ANY TABLE
    returning
      value(RD_ITAB_FOR_DB_OPERATION) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods _CREATE_INTERNAL_TABLE_FOR_TAB
    importing
      !IV_TABLE_NAME type CLIKE
    returning
      value(RD_INTERNAL_TABLE) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods _REPLACE_FOR_ALL_ENTRIES_TAB
    importing
      !IV_NAME_OF_FOR_ALL_ENT_IN_SEL type CLIKE
      !IV_NAME_OF_FOR_ALL_ENT_VAR type FIELDNAME
      !IV_NEW_SYNTAX type ABAP_BOOL
    changing
      !CV_WHERE type STRING .
  methods _REPLACE_PARAM_NAMES_IN_SQL
    importing
      !IT_PARAMETERS_WITH_NAME type TY_PARAMETERS_WITH_NAME
      !IV_NAME_OF_STRUCT_WITH_PARAMS type FIELDNAME
      !IV_NEW_SYNTAX type ABAP_BOOL
    changing
      !CV_SQL type STRING .
  methods _CREATE_DUMMY_STRUCTURE
    returning
      value(RD_REF_TO_DUMMY_STRUCTURE) type ref to DATA .
  methods _CREATE_DYNAMIC_STRUCT_FORPARS
    importing
      !IT_PARAMETERS_WITH_NAME type TY_PARAMETERS_WITH_NAME
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC optional
    returning
      value(RD_DYNAMIC_STRUCT_WITH_PARAMS) type ref to DATA
    raising
      ZCX_ZOSQL_ERROR .
  methods _CREATE_TYPE_FOR_PARAMETER
    importing
      !IS_PARAMETER type TY_PARAMETER_WITH_NAME
      !IO_WHERE_PROCESSOR type ref to ZCL_ZOSQL_WHERE_PROCESSOR
    returning
      value(RO_TYPE) type ref to CL_ABAP_DATADESCR .
  methods _EXECUTE_SELECT
    importing
      !IV_SELECT type STRING default '*'
      !IV_FROM type STRING
      !IV_WHERE type STRING
      !IV_GROUP_BY type STRING
      !IV_ORDER_BY type STRING
      value(IV_DISTINCT) type ABAP_BOOL
      !IV_NEW_SYNTAX type ABAP_BOOL
      !IT_FOR_ALL_ENTRIES_TABLE type ANY TABLE
      !IS_DYNAMIC_STRUCT_WITH_PARAMS type ANY
      value(IV_DO_INTO_CORRESPONDING) type ABAP_BOOL default ABAP_TRUE
      value(IV_NUMBER_OF_ROWS_TO_SELECT) type I optional
    exporting
      !ET_RESULT_TABLE type ANY TABLE
    raising
      ZCX_ZOSQL_ERROR .
  methods _OPEN_CURSOR
    importing
      !IV_SELECT type STRING default '*'
      !IV_FROM type STRING
      !IV_WHERE type STRING
      !IV_GROUP_BY type STRING
      !IV_ORDER_BY type STRING
      value(IV_DISTINCT) type ABAP_BOOL
      !IV_NEW_SYNTAX type ABAP_BOOL
      !IT_FOR_ALL_ENTRIES_TABLE type ANY TABLE
      !IS_DYNAMIC_STRUCT_WITH_PARAMS type ANY
      value(IV_NUMBER_OF_ROWS_TO_SELECT) type I optional
    returning
      value(RV_CURSOR) type CURSOR
    raising
      ZCX_ZOSQL_ERROR .
  methods _PREPARE_WHERE_FOR_SELECT
    importing
      !IT_PARAMETERS_WITH_NAME type TY_PARAMETERS_WITH_NAME
      !IV_NAME_OF_STRUCT_WITH_PARAMS type FIELDNAME
      !IV_NAME_OF_FOR_ALL_ENT_IN_SEL type CLIKE
      !IV_NAME_OF_FOR_ALL_ENT_VAR type FIELDNAME
      !IV_NEW_SYNTAX type ABAP_BOOL
    changing
      !CV_WHERE type STRING .
  methods _SET_PARAM_VALUE_TO_STRUCT
    importing
      !IS_PARAMETER type TY_PARAMETER_WITH_NAME
    changing
      !CS_DYNAMIC_STRUCT type ANY .
  methods _COMPUTE_COMP_NAMES_FOR_PARAMS
    importing
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS
    returning
      value(RT_PARAMETERS_WITH_NAMES) type TY_PARAMETERS_WITH_NAME .
  methods _CHECK_COMP_NAME_UNIQUE
    importing
      !IV_COMPONENT_NAME type CLIKE
      !IT_PARAMETERS_WITH_NAME type TY_PARAMETERS_WITH_NAME
    returning
      value(RV_IS_UNIQUE) type ABAP_BOOL .
  methods _COMPUTE_COMPONENT_NAME
    importing
      !IT_PARAMETERS_WITH_NAME type TY_PARAMETERS_WITH_NAME
      !IS_PARAMETER type ZOSQL_DB_LAYER_PARAM
    returning
      value(RV_COMPONENT_NAME) type FIELDNAME .
  methods _PREPARE_FOR_SELECT
    importing
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS
      !IV_NAME_OF_STRUCT_WITH_PARAMS type FIELDNAME
      !IV_NAME_OF_FOR_ALL_ENT_IN_SEL type CLIKE
      !IV_NAME_OF_FOR_ALL_ENT_VAR type FIELDNAME
      !IV_NEW_SYNTAX type ABAP_BOOL
      !IV_NUMBER_OF_ROWS_EXPR type CLIKE
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    exporting
      !ED_DYNAMIC_STRUCT_WITH_PARAMS type ref to DATA
      value(EV_NUMBER_OF_ROWS_TO_SELECT) type I
    changing
      !CV_WHERE type STRING
    raising
      ZCX_ZOSQL_ERROR .
  methods _PREPARE_FOR_UPDATE_DELETE
    importing
      !IT_PARAMETERS type ZOSQL_DB_LAYER_PARAMS
      !IV_NAME_OF_STRUCT_WITH_PARAMS type FIELDNAME
      !IO_SQL_PARSER type ref to ZCL_ZOSQL_PARSER_RECURS_DESC
    exporting
      !ED_DYNAMIC_STRUCT_WITH_PARAMS type ref to DATA
    changing
      !CV_WHERE type STRING
    raising
      ZCX_ZOSQL_ERROR .
ENDCLASS.



CLASS ZCL_ZOSQL_DB_LAYER IMPLEMENTATION.


  method ZIF_ZOSQL_DB_LAYER~COMMIT.
    IF iv_wait = abap_true.
      COMMIT WORK AND WAIT.
    ELSE.
      COMMIT WORK.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~DELETE.
    DATA: lv_table_name    TYPE string,
          lv_where         TYPE string,
          lv_new_syntax    TYPE abap_bool,
          lo_sql_parser    TYPE REF TO zcl_zosql_parser_recurs_desc.

    _split_delete_into_parts( EXPORTING iv_delete_statement = iv_delete_statement
                              IMPORTING ev_table_name       = lv_table_name
                                        ev_where            = lv_where
                                        ev_new_syntax       = lv_new_syntax
                                        eo_sql_parser       = lo_sql_parser ).

    _delete_by_sql_parts( iv_table_name = lv_table_name
                          iv_where      = lv_where
                          iv_new_syntax = lv_new_syntax
                          it_parameters = it_parameters
                          io_sql_parser = lo_sql_parser ).
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~DELETE_BY_ITAB.
    DATA: ld_itab_for_db_operation TYPE REF TO data,
          lv_table_name            TYPE tabname.

    FIELD-SYMBOLS: <lt_itab_for_db_operation> TYPE STANDARD TABLE.

    lv_table_name = iv_table_name.

    IF lv_table_name IS INITIAL.
      lv_table_name = zcl_zosql_utils=>try_to_guess_tabname_by_data( it_lines_for_delete ).
    ENDIF.

    ld_itab_for_db_operation = _convert_itab_for_db_operation( iv_table_name = lv_table_name
                                                               it_input_itab = it_lines_for_delete ).

    ASSIGN ld_itab_for_db_operation->* TO <lt_itab_for_db_operation>.

    DELETE (lv_table_name) FROM TABLE <lt_itab_for_db_operation>.
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~FETCH_NEXT_CURSOR.
    FIELD-SYMBOLS: <ls_cursor_parameters> LIKE LINE OF mt_database_cursor_parameters,
                   <lt_result_set>        TYPE STANDARD TABLE,
                   <lt_result_of_method>  TYPE STANDARD TABLE.

    READ TABLE mt_database_cursor_parameters WITH KEY cursor = iv_cursor
      ASSIGNING <ls_cursor_parameters>.
    IF sy-subrc <> 0.
      ev_subrc = 4.
      RETURN.
    ENDIF.

    ASSIGN <ls_cursor_parameters>-ref_to_result_dataset->* TO <lt_result_set>.

    CREATE DATA ed_result_as_table LIKE <lt_result_set>.
    ASSIGN ed_result_as_table->* TO <lt_result_of_method>.

    zif_zosql_db_layer~fetch_next_cursor_to_itab( EXPORTING iv_cursor       = iv_cursor
                                                            iv_package_size = iv_package_size
                                                  IMPORTING et_result_table = <lt_result_of_method>
                                                            ev_subrc        = ev_subrc ).
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~FETCH_NEXT_CURSOR_TO_ITAB.
    DATA: lt_dummy_table TYPE TABLE OF t000.

    FIELD-SYMBOLS: <lt_result_table> TYPE any table.

    IF if_table_consists_of_structs( et_result_table ) = abap_true.
      ASSIGN et_result_table TO <lt_result_table>.
    ELSE.
      ASSIGN lt_dummy_table TO <lt_result_table>.
    ENDIF.

    IF iv_do_into_corresponding = abap_true.
      FETCH NEXT CURSOR iv_cursor
        INTO CORRESPONDING FIELDS OF TABLE <lt_result_table>
        PACKAGE SIZE iv_package_size.
    ELSE.
      FETCH NEXT CURSOR iv_cursor
        INTO TABLE <lt_result_table>
        PACKAGE SIZE iv_package_size.
    ENDIF.

    ev_subrc = sy-subrc.
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~INSERT_BY_ITAB.
    DATA: ld_itab_for_db_operation TYPE REF TO data,
          lv_table_name            TYPE tabname.

    FIELD-SYMBOLS: <lt_itab_for_db_operation> TYPE STANDARD TABLE.

    lv_table_name = iv_table_name.

    IF lv_table_name IS INITIAL.
      lv_table_name = zcl_zosql_utils=>try_to_guess_tabname_by_data( it_new_lines ).
    ENDIF.

    ld_itab_for_db_operation = _convert_itab_for_db_operation( iv_table_name = lv_table_name
                                                               it_input_itab = it_new_lines ).

    ASSIGN ld_itab_for_db_operation->* TO <lt_itab_for_db_operation>.

    INSERT (lv_table_name) FROM TABLE <lt_itab_for_db_operation>.
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~MODIFY_BY_ITAB.
    DATA: ld_itab_for_db_operation TYPE REF TO data,
          lv_table_name            TYPE tabname.

    FIELD-SYMBOLS: <lt_itab_for_db_operation> TYPE STANDARD TABLE.

    lv_table_name = iv_table_name.

    IF lv_table_name IS INITIAL.
      lv_table_name = zcl_zosql_utils=>try_to_guess_tabname_by_data( it_lines_for_modify ).
    ENDIF.

    ld_itab_for_db_operation = _convert_itab_for_db_operation( iv_table_name = lv_table_name
                                                               it_input_itab = it_lines_for_modify ).

    ASSIGN ld_itab_for_db_operation->* TO <lt_itab_for_db_operation>.

    MODIFY (lv_table_name) FROM TABLE <lt_itab_for_db_operation>.
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~OPEN_CURSOR.
    DATA: lv_select_field_list        TYPE string,
          lv_from                     TYPE string,
          lv_for_all_entries_tabname  TYPE string,
          lv_where                    TYPE string,
          lv_group_by                 TYPE string,
          lv_order_by                 TYPE string,
          lv_distinct                 TYPE abap_bool,
          lv_new_syntax               TYPE abap_bool,
          lv_number_of_rows_expr      TYPE string,
          lv_where_ready_for_select   TYPE string,
          ld_struct_with_params       TYPE REF TO data,
          ld_result_table_prepared    TYPE REF TO data,
          lv_number_of_rows_to_select TYPE i,
          ls_cursor_parameters        TYPE ty_database_cursor_parameters,
          lo_sql_parser               TYPE REF TO zcl_zosql_parser_recurs_desc.

    FIELD-SYMBOLS: <ls_result_first_line>  TYPE any,
                   <ls_struct_with_params> TYPE any,
                   <lt_result_table>       TYPE ANY TABLE.

    _split_select_into_parts( EXPORTING iv_select                  = iv_select
                              IMPORTING ev_select_field_list       = lv_select_field_list
                                        ev_from                    = lv_from
                                        ev_for_all_entries_tabname = lv_for_all_entries_tabname
                                        ev_where                   = lv_where
                                        ev_group_by                = lv_group_by
                                        ev_order_by                = lv_order_by
                                        ev_distinct                = lv_distinct
                                        ev_new_syntax              = lv_new_syntax
                                        ev_number_of_rows_expr     = lv_number_of_rows_expr
                                        eo_sql_parser              = lo_sql_parser ).

    lv_where_ready_for_select = lv_where.
    _prepare_for_select( EXPORTING it_parameters                 = it_parameters
                                   iv_name_of_struct_with_params = 'IS_DYNAMIC_STRUCT_WITH_PARAMS'
                                   iv_name_of_for_all_ent_in_sel = lv_for_all_entries_tabname
                                   iv_name_of_for_all_ent_var    = 'IT_FOR_ALL_ENTRIES_TABLE'
                                   iv_new_syntax                 = lv_new_syntax
                                   iv_number_of_rows_expr        = lv_number_of_rows_expr
                                   io_sql_parser                 = lo_sql_parser
                         IMPORTING ed_dynamic_struct_with_params = ld_struct_with_params
                                   ev_number_of_rows_to_select   = lv_number_of_rows_to_select
                         CHANGING  cv_where                      = lv_where_ready_for_select
                         ).

    ASSIGN ld_struct_with_params->* TO <ls_struct_with_params>.

    rv_cursor = _open_cursor( iv_select                     = lv_select_field_list
                              iv_from                       = lv_from
                              iv_where                      = lv_where_ready_for_select
                              iv_group_by                   = lv_group_by
                              iv_order_by                   = lv_order_by
                              iv_distinct                   = lv_distinct
                              iv_new_syntax                 = lv_new_syntax
                              it_for_all_entries_table      = it_for_all_entries_table
                              is_dynamic_struct_with_params = <ls_struct_with_params>
                              iv_number_of_rows_to_select   = lv_number_of_rows_to_select ).

    ls_cursor_parameters-cursor = rv_cursor.
    ls_cursor_parameters-ref_to_result_dataset =
      create_dynamic_tab_for_result( lo_sql_parser ).
    APPEND ls_cursor_parameters TO mt_database_cursor_parameters.
  endmethod.


  method ZIF_ZOSQL_DB_LAYER~SELECT_TO_ITAB.

    DATA: lv_select_field_list       TYPE string,
          lv_from                    TYPE string,
          lv_for_all_entries_tabname TYPE string,
          lv_where                   TYPE string,
          lv_group_by                TYPE string,
          lv_order_by                TYPE string,
          lv_distinct                TYPE abap_bool,
          lv_new_syntax              TYPE abap_bool,
          lv_number_of_rows_expr     TYPE string,
          lo_sql_parser              TYPE REF TO zcl_zosql_parser_recurs_desc.

    _split_select_into_parts( EXPORTING iv_select                  = iv_select
                              IMPORTING ev_select_field_list       = lv_select_field_list
                                        ev_from                    = lv_from
                                        ev_for_all_entries_tabname = lv_for_all_entries_tabname
                                        ev_where                   = lv_where
                                        ev_group_by                = lv_group_by
                                        ev_order_by                = lv_order_by
                                        ev_distinct                = lv_distinct
                                        ev_new_syntax              = lv_new_syntax
                                        ev_number_of_rows_expr     = lv_number_of_rows_expr
                                        eo_sql_parser              = lo_sql_parser ).

    _select_by_sql_parts( EXPORTING iv_select                  = lv_select_field_list
                                    iv_from                    = lv_from
                                    iv_where                   = lv_where
                                    iv_group_by                = lv_group_by
                                    iv_order_by                = lv_order_by
                                    iv_distinct                = lv_distinct
                                    iv_new_syntax              = lv_new_syntax
                                    it_parameters              = it_parameters
                                    it_for_all_entries_table   = it_for_all_entries_table
                                    iv_for_all_entries_tabname = lv_for_all_entries_tabname
                                    iv_do_into_corresponding   = iv_do_into_corresponding
                                    iv_number_of_rows_expr     = lv_number_of_rows_expr
                                    io_sql_parser              = lo_sql_parser
                          IMPORTING et_result_table            = et_result_table
                                    es_result_line             = es_result_line
                                    ev_subrc                   = ev_subrc ).
  endmethod.


  METHOD zif_zosql_db_layer~update.
    DATA: lv_table_name    TYPE string,
          lv_set_statement TYPE string,
          lv_where         TYPE string,
          lv_new_syntax    TYPE abap_bool,
          lo_sql_parser    TYPE REF TO zcl_zosql_parser_recurs_desc.

    _split_update_into_parts( EXPORTING iv_update_statement   = iv_update_statement
                              IMPORTING ev_table_name         = lv_table_name
                                        ev_set_statement      = lv_set_statement
                                        ev_where              = lv_where
                                        ev_new_syntax         = lv_new_syntax
                                        eo_sql_parser         = lo_sql_parser ).

    _update_by_sql_parts( iv_table_name    = lv_table_name
                          iv_set_statement = lv_set_statement
                          iv_where         = lv_where
                          iv_new_syntax    = lv_new_syntax
                          it_parameters    = it_parameters
                          io_sql_parser    = lo_sql_parser ).
  ENDMETHOD.


  method ZIF_ZOSQL_DB_LAYER~UPDATE_BY_ITAB.
    DATA: ld_itab_for_db_operation TYPE REF TO data,
          lv_table_name            TYPE tabname.

    FIELD-SYMBOLS: <lt_itab_for_db_operation> TYPE STANDARD TABLE.

    lv_table_name = iv_table_name.

    IF lv_table_name IS INITIAL.
      lv_table_name = zcl_zosql_utils=>try_to_guess_tabname_by_data( it_lines_for_update ).
    ENDIF.

    ld_itab_for_db_operation = _convert_itab_for_db_operation( iv_table_name = lv_table_name
                                                               it_input_itab = it_lines_for_update ).

    ASSIGN ld_itab_for_db_operation->* TO <lt_itab_for_db_operation>.

    UPDATE (lv_table_name) FROM TABLE <lt_itab_for_db_operation>.
  endmethod.


method _CHECK_COMP_NAME_UNIQUE.

  READ TABLE it_parameters_with_name WITH KEY param_name = iv_component_name TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    rv_is_unique = abap_true.
  ENDIF.
endmethod.


method _COMPUTE_COMPONENT_NAME.

  DATA: lv_component_num TYPE N LENGTH 3.

  DO.
    lv_component_num = lv_component_num + 1.

    CONCATENATE 'PARAM_' lv_component_num INTO rv_component_name.

    IF _check_comp_name_unique( iv_component_name       = rv_component_name
                                it_parameters_with_name = it_parameters_with_name ) = abap_true.
      EXIT.
    ENDIF.
  ENDDO.
endmethod.


  METHOD _COMPUTE_COMP_NAMES_FOR_PARAMS.

    DATA: lv_component_name TYPE fieldname.

    FIELD-SYMBOLS: <ls_parameter>           LIKE LINE OF it_parameters,
                   <ls_parameter_with_name> LIKE LINE OF rt_parameters_with_names.

    LOOP AT it_parameters ASSIGNING <ls_parameter>.
      lv_component_name = _compute_component_name( it_parameters_with_name = rt_parameters_with_names
                                                   is_parameter          = <ls_parameter> ).

      APPEND INITIAL LINE TO rt_parameters_with_names ASSIGNING <ls_parameter_with_name>.
      MOVE-CORRESPONDING <ls_parameter> TO <ls_parameter_with_name>.
      <ls_parameter_with_name>-param_name = lv_component_name.
    ENDLOOP.
  ENDMETHOD.


  method _CONSIDER_IF_HOST_ALREADY_WAS.
    REPLACE ALL OCCURRENCES OF '@@' IN cv_sql WITH '@'.
  endmethod.


  method _CONVERT_ITAB_FOR_DB_OPERATION.

    FIELD-SYMBOLS: <lt_table_of_database_struct> TYPE STANDARD TABLE.

    rd_itab_for_db_operation = _create_internal_table_for_tab( iv_table_name ).
    ASSIGN rd_itab_for_db_operation->* TO <lt_table_of_database_struct>.

    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = it_input_itab
                                               IMPORTING et_table_dest = <lt_table_of_database_struct> ).
  endmethod.


  METHOD _CREATE_DUMMY_STRUCTURE.
    CREATE DATA rd_ref_to_dummy_structure TYPE t000.
  ENDMETHOD.


  METHOD _create_dynamic_struct_forpars.

    DATA: lt_dynamic_components         TYPE cl_abap_structdescr=>component_table,
          ls_dynamic_component          LIKE LINE OF lt_dynamic_components,
          lo_dynamic_struct_with_params TYPE REF TO cl_abap_structdescr,
          lt_parameters                 TYPE zosql_db_layer_params,
          lo_where_processor            TYPE REF TO zcl_zosql_where_processor,
          lo_parameters                 TYPE REF TO zcl_zosql_parameters,
          lo_parser_helper              TYPE REF TO zcl_zosql_parser_helper,
          ls_node_where                 TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    FIELD-SYMBOLS: <ls_parameter_with_name>      LIKE LINE OF it_parameters_with_name,
                   <ls_dynamic_struct_with_pars> TYPE any,
                   <lv_value>                    TYPE any.

    IF it_parameters_with_name IS INITIAL.
      rd_dynamic_struct_with_params = _create_dummy_structure( ).
      RETURN.
    ENDIF.

    zcl_zosql_utils=>move_corresponding_table( EXPORTING it_table_src  = it_parameters_with_name
                                               IMPORTING et_table_dest = lt_parameters ).
    CREATE OBJECT lo_parameters
      EXPORTING
        it_parameters = lt_parameters.

    CREATE OBJECT lo_where_processor
      EXPORTING
        io_parameters = lo_parameters.

    IF io_sql_parser IS BOUND.
      CREATE OBJECT lo_parser_helper.
      lo_parser_helper->get_key_nodes_of_sql_select( EXPORTING io_sql_parser = io_sql_parser
                                                     IMPORTING es_node_where = ls_node_where ).
    ENDIF.

    lo_where_processor->zif_zosql_expression_processor~initialize_by_parsed_sql( io_sql_parser          = io_sql_parser
                                                                                 iv_id_of_node_to_parse = ls_node_where-id ).

    LOOP AT it_parameters_with_name ASSIGNING <ls_parameter_with_name>.
      ls_dynamic_component-name = <ls_parameter_with_name>-param_name.

      ls_dynamic_component-type = _create_type_for_parameter( is_parameter       = <ls_parameter_with_name>
                                                              io_where_processor = lo_where_processor ).
      APPEND ls_dynamic_component TO lt_dynamic_components.
    ENDLOOP.

    lo_dynamic_struct_with_params = cl_abap_structdescr=>create( lt_dynamic_components ).

    CREATE DATA rd_dynamic_struct_with_params TYPE HANDLE lo_dynamic_struct_with_params.
    ASSIGN rd_dynamic_struct_with_params->* TO <ls_dynamic_struct_with_pars>.

    LOOP AT it_parameters_with_name ASSIGNING <ls_parameter_with_name>.
      _set_param_value_to_struct( EXPORTING is_parameter      = <ls_parameter_with_name>
                                  CHANGING  cs_dynamic_struct = <ls_dynamic_struct_with_pars> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD _CREATE_INTERNAL_TABLE_FOR_TAB.

    DATA: lo_struct TYPE REF TO cl_abap_structdescr,
          lo_table  TYPE REF TO cl_abap_tabledescr.

    zcl_zosql_utils=>raise_if_transp_tab_not_exist( iv_table_name ).

    lo_struct ?= cl_abap_structdescr=>describe_by_name( iv_table_name ).
    lo_table = cl_abap_tabledescr=>create( lo_struct ).
    CREATE DATA rd_internal_table TYPE HANDLE lo_table.
  ENDMETHOD.


  METHOD _CREATE_STANDARD_LIKE_ANY_TAB.

    DATA: lo_any_table      TYPE REF TO cl_abap_tabledescr,
          lo_line           TYPE REF TO cl_abap_datadescr,
          lo_standard_table TYPE REF TO cl_abap_tabledescr.

    lo_any_table ?= cl_abap_tabledescr=>describe_by_data( it_any_table ).
    lo_line = lo_any_table->get_table_line_type( ).
    lo_standard_table = cl_abap_tabledescr=>create( lo_line ).

    CREATE DATA rd_ref_to_standard_table TYPE HANDLE lo_standard_table.
  ENDMETHOD.


  method _CREATE_TYPE_FOR_PARAMETER.

    IF is_parameter-parameter_value_ref IS BOUND.
      ro_type ?= cl_abap_typedescr=>describe_by_data_ref( is_parameter-parameter_value_ref ).
    ELSEIF is_parameter-parameter_value_range IS NOT INITIAL
      OR io_where_processor->is_parameter_compared_as_range( is_parameter-param_name_in_select ) = abap_true.

      ro_type ?= cl_abap_typedescr=>describe_by_data( is_parameter-parameter_value_range ).
    ELSE.
      ro_type ?= cl_abap_typedescr=>describe_by_data( is_parameter-parameter_value_single ).
    ENDIF.
  endmethod.


  METHOD _DELETE_BY_SQL_PARTS.

    DATA: lv_where                      TYPE string,
          ld_dynamic_struct_with_params TYPE REF TO data.

    FIELD-SYMBOLS: <ls_dynamic_struct_with_pars> TYPE any.

    lv_where = iv_where.
    _prepare_for_update_delete( EXPORTING it_parameters                 = it_parameters
                                          iv_name_of_struct_with_params = 'IS_DYNAMIC_STRUCT_WITH_PARAMS'
                                          io_sql_parser                 = io_sql_parser
                                IMPORTING ed_dynamic_struct_with_params = ld_dynamic_struct_with_params
                                CHANGING  cv_where                      = lv_where ).

    ASSIGN ld_dynamic_struct_with_params->* TO <ls_dynamic_struct_with_pars>.

    _execute_delete( iv_table_name                 = iv_table_name
                     iv_where                      = lv_where
                     is_dynamic_struct_with_params = <ls_dynamic_struct_with_pars> ).
  ENDMETHOD.


  method _EXECUTE_DELETE.
    IF iv_where IS NOT INITIAL.
      DELETE FROM (iv_table_name)
        WHERE (iv_where).
    ELSE.
      DELETE FROM (iv_table_name).
    ENDIF.
  endmethod.


  METHOD _EXECUTE_SELECT.

    IF iv_new_syntax = abap_true.

      " Dynamic call for backward compatibility with older versions
      CALL METHOD ('ZCL_ZOSQL_UTILS_740')=>('EXECUTE_SELECT_740')
        EXPORTING
          iv_select                     = iv_select
          iv_from                       = iv_from
          iv_where                      = iv_where
          iv_group_by                   = iv_group_by
          iv_order_by                   = iv_order_by
          iv_distinct                   = iv_distinct
          it_for_all_entries_table      = it_for_all_entries_table
          is_dynamic_struct_with_params = is_dynamic_struct_with_params
          iv_do_into_corresponding      = iv_do_into_corresponding
          iv_number_of_rows_to_select   = iv_number_of_rows_to_select
        IMPORTING
          et_result_table               = et_result_table.

      RETURN.
    ENDIF.

    IF it_for_all_entries_table IS NOT INITIAL.

      IF iv_order_by IS NOT INITIAL.
        MESSAGE e051 INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.

      IF iv_group_by IS NOT INITIAL.
        MESSAGE e052 INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.

      IF iv_distinct = abap_true.

        IF iv_do_into_corresponding = abap_true.
          SELECT DISTINCT (iv_select)
            FROM (iv_from)
            INTO CORRESPONDING FIELDS OF TABLE et_result_table
            UP TO iv_number_of_rows_to_select ROWS
            FOR ALL ENTRIES IN it_for_all_entries_table
            WHERE (iv_where).
        ELSE.
          SELECT DISTINCT (iv_select)
            FROM (iv_from)
            INTO TABLE et_result_table
            UP TO iv_number_of_rows_to_select ROWS
            FOR ALL ENTRIES IN it_for_all_entries_table
            WHERE (iv_where).
        ENDIF.
      ELSE.

        IF iv_do_into_corresponding = abap_true.
          SELECT (iv_select)
            FROM (iv_from)
            INTO CORRESPONDING FIELDS OF TABLE et_result_table
            UP TO iv_number_of_rows_to_select ROWS
            FOR ALL ENTRIES IN it_for_all_entries_table
            WHERE (iv_where).
        ELSE.
          SELECT (iv_select)
            FROM (iv_from)
            INTO TABLE et_result_table
            UP TO iv_number_of_rows_to_select ROWS
            FOR ALL ENTRIES IN it_for_all_entries_table
            WHERE (iv_where).
        ENDIF.
      ENDIF.
    ELSE.

      IF iv_distinct = abap_true.

        IF iv_do_into_corresponding = abap_true.
          SELECT DISTINCT (iv_select)
            FROM (iv_from)
            INTO CORRESPONDING FIELDS OF TABLE et_result_table
            UP TO iv_number_of_rows_to_select ROWS
            WHERE (iv_where)
            GROUP BY (iv_group_by)
            ORDER BY (iv_order_by).
        ELSE.
          SELECT DISTINCT (iv_select)
            FROM (iv_from)
            INTO TABLE et_result_table
            UP TO iv_number_of_rows_to_select ROWS
            WHERE (iv_where)
            GROUP BY (iv_group_by)
            ORDER BY (iv_order_by).
        ENDIF.
      ELSE.
        IF iv_do_into_corresponding = abap_true.
          SELECT (iv_select)
            FROM (iv_from)
            INTO CORRESPONDING FIELDS OF TABLE et_result_table
            UP TO iv_number_of_rows_to_select ROWS
            WHERE (iv_where)
            GROUP BY (iv_group_by)
            ORDER BY (iv_order_by).
        ELSE.
          SELECT (iv_select)
            FROM (iv_from)
            INTO TABLE et_result_table
            UP TO iv_number_of_rows_to_select ROWS
            WHERE (iv_where)
            GROUP BY (iv_group_by)
            ORDER BY (iv_order_by).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method _EXECUTE_UPDATE.
    IF iv_where IS NOT INITIAL.
      UPDATE (iv_table_name)
        SET (iv_set_statement)
        WHERE (iv_where).
    ELSE.
      UPDATE (iv_table_name)
        SET (iv_set_statement).
    ENDIF.
  endmethod.


  METHOD _OPEN_CURSOR.

    IF iv_new_syntax = abap_true.

      " Dynamic call for backward compatibility with older versions
      CALL METHOD ('ZCL_ZOSQL_DB_LAYER_UTILS_74')=>('EXECUTE_SELECT_740')
        EXPORTING
          iv_select                     = iv_select
          iv_from                       = iv_from
          iv_where                      = iv_where
          iv_group_by                   = iv_group_by
          iv_order_by                   = iv_order_by
          iv_distinct                   = iv_distinct
          it_for_all_entries_table      = it_for_all_entries_table
          is_dynamic_struct_with_params = is_dynamic_struct_with_params
          iv_number_of_rows_to_select   = iv_number_of_rows_to_select
        RECEIVING
          rv_cursor                     = rv_cursor.

      RETURN.
    ENDIF.

    IF it_for_all_entries_table IS NOT INITIAL.

      IF iv_order_by IS NOT INITIAL.
        MESSAGE e051 INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.

      IF iv_group_by IS NOT INITIAL.
        MESSAGE e052 INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
      ENDIF.

      IF iv_distinct = abap_true.

        OPEN CURSOR rv_cursor FOR
        SELECT DISTINCT (iv_select)
          FROM (iv_from)
          UP TO iv_number_of_rows_to_select ROWS
          FOR ALL ENTRIES IN it_for_all_entries_table
          WHERE (iv_where).

      ELSE.

        OPEN CURSOR rv_cursor FOR
        SELECT (iv_select)
          FROM (iv_from)
          UP TO iv_number_of_rows_to_select ROWS
          FOR ALL ENTRIES IN it_for_all_entries_table
          WHERE (iv_where).
      ENDIF.
    ELSE.

      IF iv_distinct = abap_true.

        OPEN CURSOR rv_cursor FOR
        SELECT DISTINCT (iv_select)
          FROM (iv_from)
          UP TO iv_number_of_rows_to_select ROWS
          WHERE (iv_where)
          GROUP BY (iv_group_by)
          ORDER BY (iv_order_by).

      ELSE.

        OPEN CURSOR rv_cursor FOR
        SELECT (iv_select)
          FROM (iv_from)
          UP TO iv_number_of_rows_to_select ROWS
          WHERE (iv_where)
          GROUP BY (iv_group_by)
          ORDER BY (iv_order_by).

      ENDIF.
    ENDIF.
  ENDMETHOD.


METHOD _PREPARE_FOR_SELECT.

  DATA: lt_parameters_with_name TYPE ty_parameters_with_name.

  lt_parameters_with_name = _compute_comp_names_for_params( it_parameters ).

  ed_dynamic_struct_with_params = _create_dynamic_struct_forpars( it_parameters_with_name = lt_parameters_with_name
                                                                  io_sql_parser           = io_sql_parser ).

  _prepare_where_for_select( EXPORTING it_parameters_with_name       = lt_parameters_with_name
                                       iv_name_of_struct_with_params = iv_name_of_struct_with_params
                                       iv_name_of_for_all_ent_in_sel = iv_name_of_for_all_ent_in_sel
                                       iv_name_of_for_all_ent_var    = iv_name_of_for_all_ent_var
                                       iv_new_syntax                 = iv_new_syntax
                             CHANGING  cv_where                      = cv_where ).

  ev_number_of_rows_to_select = prepare_n_of_rows_for_select( it_parameters          = it_parameters
                                                              iv_number_of_rows_expr = iv_number_of_rows_expr ).
ENDMETHOD.


METHOD _PREPARE_FOR_UPDATE_DELETE.

  DATA: lt_parameters_with_name TYPE ty_parameters_with_name.

  lt_parameters_with_name = _compute_comp_names_for_params( it_parameters ).

  ed_dynamic_struct_with_params = _create_dynamic_struct_forpars( it_parameters_with_name = lt_parameters_with_name
                                                                  io_sql_parser           = io_sql_parser ).

  _replace_param_names_in_sql( EXPORTING it_parameters_with_name       = lt_parameters_with_name
                                         iv_name_of_struct_with_params = iv_name_of_struct_with_params
                                         iv_new_syntax                 = abap_false
                               CHANGING  cv_sql                        = cv_where ).
ENDMETHOD.


  METHOD _PREPARE_RESULT_TABLE_FOR_SEL.

    IF if_table_consists_of_structs( it_result_table ) <> abap_true
      AND zcl_zosql_utils=>is_structure( is_result_line ) = abap_true.

      CREATE DATA rd_result_table LIKE TABLE OF is_result_line.
    ELSE.

      rd_result_table = _create_standard_like_any_tab( it_result_table ).
    ENDIF.
  ENDMETHOD.


  method _PREPARE_SET_FOR_UPDATE.

    DATA: lt_parameters_with_name TYPE ty_parameters_with_name.

    lt_parameters_with_name = _compute_comp_names_for_params( it_parameters ).

    _replace_param_names_in_sql( EXPORTING it_parameters_with_name       = lt_parameters_with_name
                                           iv_name_of_struct_with_params = iv_name_of_struct_with_params
                                           iv_new_syntax                 = abap_false
                                 CHANGING  cv_sql                        = cv_set_statement ).
  endmethod.


  METHOD _PREPARE_WHERE_FOR_SELECT.
    _replace_param_names_in_sql( EXPORTING it_parameters_with_name       = it_parameters_with_name
                                           iv_name_of_struct_with_params = iv_name_of_struct_with_params
                                           iv_new_syntax                 = iv_new_syntax
                                 CHANGING  cv_sql                        = cv_where ).
    _replace_for_all_entries_tab( EXPORTING iv_name_of_for_all_ent_in_sel = iv_name_of_for_all_ent_in_sel
                                            iv_name_of_for_all_ent_var    = iv_name_of_for_all_ent_var
                                            iv_new_syntax                 = iv_new_syntax
                                  CHANGING  cv_where                      = cv_where ).
  ENDMETHOD.


  METHOD _REPLACE_FOR_ALL_ENTRIES_TAB.

    DATA: lv_what_replace TYPE string,
          lv_replace_with TYPE string.

    IF iv_name_of_for_all_ent_in_sel IS NOT INITIAL.
      lv_what_replace = iv_name_of_for_all_ent_in_sel.
      lv_replace_with = iv_name_of_for_all_ent_var.
    ELSEIF iv_new_syntax = abap_true.
      lv_what_replace = lv_replace_with = iv_name_of_for_all_ent_var.
    ELSE.
      RETURN.
    ENDIF.

    CONDENSE lv_what_replace.

    IF iv_new_syntax = abap_true.
      CONCATENATE '@' lv_replace_with INTO lv_replace_with.

      REPLACE FIRST OCCURRENCE OF '@@' IN lv_replace_with WITH '@'.

      FIND FIRST OCCURRENCE OF lv_what_replace IN cv_where.
      IF sy-subrc <> 0.
        CONCATENATE '@' lv_what_replace INTO lv_what_replace.
      ENDIF.

      REPLACE FIRST OCCURRENCE OF '@@' IN lv_what_replace WITH '@'.
    ENDIF.

    REPLACE ALL OCCURRENCES OF lv_what_replace IN cv_where WITH lv_replace_with IGNORING CASE.
  ENDMETHOD.


  method _REPLACE_PARAM_NAMES_IN_SQL.

    DATA: lv_param_name_in_select TYPE string.

    FIELD-SYMBOLS: <ls_parameter_with_name> LIKE LINE OF it_parameters_with_name.

    LOOP AT it_parameters_with_name ASSIGNING <ls_parameter_with_name>.
      CONCATENATE iv_name_of_struct_with_params '-' <ls_parameter_with_name>-param_name
        INTO lv_param_name_in_select.

      IF iv_new_syntax = abap_true.
        CONCATENATE '@' lv_param_name_in_select INTO lv_param_name_in_select.
      ENDIF.

      REPLACE ALL OCCURRENCES OF <ls_parameter_with_name>-param_name_in_select IN cv_sql
        WITH lv_param_name_in_select.
    ENDLOOP.

    _consider_if_host_already_was( CHANGING cv_sql = cv_sql ).
  endmethod.


  METHOD _SELECT_BY_SQL_PARTS.
    DATA: lv_where_ready_for_select   TYPE string,
          ld_struct_with_params       TYPE REF TO data,
          ld_result_table_prepared    TYPE REF TO data,
          lv_number_of_rows_to_select TYPE i.

    FIELD-SYMBOLS: <ls_result_first_line>  TYPE any,
                   <ls_struct_with_params> TYPE any,
                   <lt_result_table>       TYPE ANY TABLE.

    CLEAR: et_result_table, es_result_line, ev_subrc.

    lv_where_ready_for_select = iv_where.
    _prepare_for_select( EXPORTING it_parameters                 = it_parameters
                                   iv_name_of_struct_with_params = 'IS_DYNAMIC_STRUCT_WITH_PARAMS'
                                   iv_name_of_for_all_ent_in_sel = iv_for_all_entries_tabname
                                   iv_name_of_for_all_ent_var    = 'IT_FOR_ALL_ENTRIES_TABLE'
                                   iv_new_syntax                 = iv_new_syntax
                                   iv_number_of_rows_expr        = iv_number_of_rows_expr
                                   io_sql_parser                 = io_sql_parser
                         IMPORTING ed_dynamic_struct_with_params = ld_struct_with_params
                                   ev_number_of_rows_to_select   = lv_number_of_rows_to_select
                         CHANGING  cv_where                      = lv_where_ready_for_select
                         ).

    ASSIGN ld_struct_with_params->* TO <ls_struct_with_params>.

    ld_result_table_prepared = _prepare_result_table_for_sel( it_result_table = et_result_table
                                                              is_result_line  = es_result_line ).

    ASSIGN ld_result_table_prepared->* TO <lt_result_table>.

    _execute_select( EXPORTING iv_select                     = iv_select
                               iv_from                       = iv_from
                               iv_where                      = lv_where_ready_for_select
                               iv_group_by                   = iv_group_by
                               iv_order_by                   = iv_order_by
                               iv_distinct                   = iv_distinct
                               iv_new_syntax                 = iv_new_syntax
                               it_for_all_entries_table      = it_for_all_entries_table
                               is_dynamic_struct_with_params = <ls_struct_with_params>
                               iv_do_into_corresponding      = iv_do_into_corresponding
                               iv_number_of_rows_to_select   = lv_number_of_rows_to_select
                     IMPORTING et_result_table               = <lt_result_table> ).

    return_result_of_select_toitab( EXPORTING it_result_table          = <lt_result_table>
                                              iv_do_into_corresponding = iv_do_into_corresponding
                                    IMPORTING et_result_table          = et_result_table
                                              es_result_line           = es_result_line
                                              ev_subrc                 = ev_subrc ).
  ENDMETHOD.


  METHOD _SET_PARAM_VALUE_TO_STRUCT.

    FIELD-SYMBOLS: <lv_value>     TYPE any,
                   <lv_parameter> TYPE any.

    ASSIGN COMPONENT is_parameter-param_name OF STRUCTURE cs_dynamic_struct TO <lv_value>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF is_parameter-parameter_value_ref IS BOUND.
      ASSIGN is_parameter-parameter_value_ref->* TO <lv_parameter>.
      <lv_value> = <lv_parameter>.
    ELSEIF is_parameter-parameter_value_range IS NOT INITIAL.
      <lv_value> = is_parameter-parameter_value_range.
    ELSEIF zcl_zosql_utils=>is_internal_table( <lv_value> ) <> abap_true.
      <lv_value> = is_parameter-parameter_value_single.
    ENDIF.
  ENDMETHOD.


  method _SPLIT_DELETE_INTO_PARTS.

    DATA: lo_parser_helper     TYPE REF TO zcl_zosql_parser_helper,
          ls_node_delete_table TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_where        TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    CREATE OBJECT eo_sql_parser.
    eo_sql_parser->set_sql( iv_delete_statement ).
    eo_sql_parser->run_recursive_descent_parser( ).

    CREATE OBJECT lo_parser_helper.
    lo_parser_helper->get_key_nodes_of_sql_delete( EXPORTING io_sql_parser        = eo_sql_parser
                                                   IMPORTING es_node_delete_table = ls_node_delete_table
                                                             es_node_where        = ls_node_where
                                                             ev_new_syntax        = ev_new_syntax ).

    ev_table_name    = lo_parser_helper->get_delete_table_name( eo_sql_parser ).
    ev_where         = eo_sql_parser->get_node_sql_without_self( ls_node_where-id ).
  endmethod.


  METHOD _SPLIT_SELECT_INTO_PARTS.

    DATA: lv_select                  TYPE string,
          lv_single                  TYPE abap_bool,
          lv_end_token_index         TYPE i,
          lv_select_field_list_start TYPE i,
          lv_select_field_list_end   TYPE i,
          lo_sql_parser_helper       TYPE REF TO zcl_zosql_parser_helper,
          ls_node_distinct           TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_single             TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          lt_nodes_select_field_list TYPE zcl_zosql_parser_recurs_desc=>ty_tree,
          ls_node_up_to_n_rows       TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_from               TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_for_all_entries    TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_where              TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_group_by           TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_order_by           TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    FIELD-SYMBOLS: <ls_node>                TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    CLEAR: ev_select_field_list, ev_from, ev_where, ev_group_by, ev_order_by, ev_distinct.

    eo_sql_parser = parse_sql( iv_select ).

    CREATE OBJECT lo_sql_parser_helper.
    lo_sql_parser_helper->get_key_nodes_of_sql_select( EXPORTING io_sql_parser                 = eo_sql_parser
                                                       IMPORTING es_node_distinct              = ls_node_distinct
                                                                 es_node_single                = ls_node_single
                                                                 et_nodes_of_select_field_list = lt_nodes_select_field_list
                                                                 es_node_up_to_n_rows          = ls_node_up_to_n_rows
                                                                 es_node_from                  = ls_node_from
                                                                 es_node_for_all_entries       = ls_node_for_all_entries
                                                                 es_node_where                 = ls_node_where
                                                                 es_node_group_by              = ls_node_group_by
                                                                 es_node_order_by              = ls_node_order_by
                                                                 ev_new_syntax                 = ev_new_syntax ).

    IF ls_node_distinct IS NOT INITIAL.
      ev_distinct = abap_true.
    ENDIF.

    IF ls_node_single IS NOT INITIAL.
      lv_single = abap_true.
    ENDIF.

    LOOP AT lt_nodes_select_field_list ASSIGNING <ls_node>.
      IF lv_select_field_list_start IS INITIAL.
        lv_select_field_list_start = <ls_node>-token_index.
      ELSEIF lv_select_field_list_start > <ls_node>-token_index.
        lv_select_field_list_start = <ls_node>-token_index.
      ENDIF.

      lv_end_token_index = eo_sql_parser->get_node_end_token_index( <ls_node>-id ).

      IF lv_end_token_index > lv_select_field_list_end.
        lv_select_field_list_end = lv_end_token_index.
      ENDIF.
    ENDLOOP.

    IF lv_select_field_list_start IS NOT INITIAL AND lv_select_field_list_end IS NOT INITIAL.
      ev_select_field_list =
        eo_sql_parser->get_sql_as_range_of_tokens( iv_start_token_index = lv_select_field_list_start
                                                   iv_end_token_index   = lv_select_field_list_end ).
    ENDIF.

    ev_number_of_rows_expr = lo_sql_parser_helper->get_up_to_n_rows_value( eo_sql_parser ).

    IF ls_node_up_to_n_rows IS NOT INITIAL.
      ev_number_of_rows_expr = eo_sql_parser->get_token_of_nth_child_node( iv_main_node_id = ls_node_up_to_n_rows-id
                                                                           iv_n            = 2 ).
    ENDIF.

    ev_from = eo_sql_parser->get_node_sql_without_self( ls_node_from-id ).

    ev_for_all_entries_tabname = lo_sql_parser_helper->get_for_all_entries_tabname( io_sql_parser = eo_sql_parser ).

    IF ls_node_where IS NOT INITIAL.
      ev_where = eo_sql_parser->get_node_sql_without_self( ls_node_where-id ).
    ENDIF.

    IF ls_node_group_by IS NOT INITIAL.
      ev_group_by = eo_sql_parser->get_node_sql_start_at_offset( iv_node_id                 = ls_node_group_by-id
                                                                 iv_number_of_tokens_offset = 2 ).
    ENDIF.

    IF ls_node_order_by IS NOT INITIAL.
      ev_order_by = eo_sql_parser->get_node_sql_start_at_offset( iv_node_id                 = ls_node_order_by-id
                                                                 iv_number_of_tokens_offset = 2 ).
    ENDIF.

    IF lv_single = abap_true AND ev_number_of_rows_expr IS NOT INITIAL.
      MESSAGE e071 INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ELSEIF lv_single = abap_true.
      ev_number_of_rows_expr = '1'.
    ENDIF.
  ENDMETHOD.


  METHOD _SPLIT_UPDATE_INTO_PARTS.

    DATA: lo_parser_helper     TYPE REF TO zcl_zosql_parser_helper,
          ls_node_update_table TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_set          TYPE zcl_zosql_parser_recurs_desc=>ty_node,
          ls_node_where        TYPE zcl_zosql_parser_recurs_desc=>ty_node.

    CREATE OBJECT eo_sql_parser.
    eo_sql_parser->set_sql( iv_update_statement ).
    eo_sql_parser->run_recursive_descent_parser( ).

    CREATE OBJECT lo_parser_helper.
    lo_parser_helper->get_key_nodes_of_sql_update( EXPORTING io_sql_parser        = eo_sql_parser
                                                   IMPORTING es_node_update_table = ls_node_update_table
                                                             es_node_set          = ls_node_set
                                                             es_node_where        = ls_node_where
                                                             ev_new_syntax        = ev_new_syntax ).

    ev_table_name    = lo_parser_helper->get_update_table_name( eo_sql_parser ).
    ev_set_statement = eo_sql_parser->get_node_sql_without_self( ls_node_set-id ).
    ev_where         = eo_sql_parser->get_node_sql_without_self( ls_node_where-id ).
  ENDMETHOD.


  method _UPDATE_BY_SQL_PARTS.
    DATA: lt_parameters_with_name       TYPE ty_parameters_with_name,
          ld_dynamic_struct_with_params TYPE REF TO data,
          lv_set_statement              TYPE string,
          lv_where                      TYPE string.

    FIELD-SYMBOLS: <ls_dynamic_struct_with_pars> TYPE any.

    lv_set_statement = iv_set_statement.
    lv_where         = iv_where.

    _prepare_for_update_delete( EXPORTING it_parameters                 = it_parameters
                                          iv_name_of_struct_with_params = 'IS_DYNAMIC_STRUCT_WITH_PARAMS'
                                          io_sql_parser                 = io_sql_parser
                                IMPORTING ed_dynamic_struct_with_params = ld_dynamic_struct_with_params
                                CHANGING  cv_where                      = lv_where ).

    ASSIGN ld_dynamic_struct_with_params->* TO <ls_dynamic_struct_with_pars>.

    _prepare_set_for_update( EXPORTING it_parameters                 = it_parameters
                                       iv_name_of_struct_with_params = 'IS_DYNAMIC_STRUCT_WITH_PARAMS'
                             CHANGING  cv_set_statement              = lv_set_statement ).

    _execute_update( iv_table_name                 = iv_table_name
                     iv_set_statement              = lv_set_statement
                     iv_where                      = lv_where
                     is_dynamic_struct_with_params = <ls_dynamic_struct_with_pars> ).
  endmethod.
ENDCLASS.
