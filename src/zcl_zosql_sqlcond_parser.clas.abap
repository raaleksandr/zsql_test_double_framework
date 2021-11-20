class ZCL_ZOSQL_SQLCOND_PARSER definition
  public
  inheriting from ZCL_ZOSQL_PARSER_BASE
  abstract
  create public .

public section.

  interfaces ZIF_ZOSQL_SQLCOND_PARSER .

  types:
    BEGIN OF TY_DATA_SET,
           data_set_name   TYPE string,
           data_set_alias  TYPE string,
           data_set_fields TYPE fieldname_table,
         END OF ty_data_set .
  types:
    ty_data_sets TYPE STANDARD TABLE OF ty_data_set WITH KEY data_set_name .
protected section.

  types:
    BEGIN OF TY_block,
           parser TYPE REF TO zif_zosql_sqlcond_parser,
         END OF ty_block .
  types:
    ty_blocks TYPE STANDARD TABLE OF ty_block WITH DEFAULT KEY .

  data M_OPERATION type STRING .
  data M_DATASET_NAME_OR_ALIAS_LEFT type STRING .
  data M_FIELDNAME_LEFT type FIELDNAME .
  data M_DATASET_NAME_OR_ALIAS_RIGHT type STRING .
  data M_FIELDNAME_RIGHT_OR_VALUE type STRING .
  data MT_OR_CONDITIONS type TY_BLOCKS .
  data MT_AND_CONDITIONS type TY_BLOCKS .
  data MS_NOT_CONDITION type TY_BLOCK .
  constants C_IN type STRING value 'IN' ##NO_TEXT.

  methods _GET_REF_TO_RIGHT_OPERAND
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_SQLTAB_ITERPOS
    returning
      value(RD_REF_TO_RIGHT_OPERAND) type ref to DATA .
  methods _CLEAR_QUOTES_FROM_VALUE .
  methods _CHECK_ELEMENTARY
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_SQLTAB_ITERPOS
    returning
      value(RV_CONDITIONS_TRUE) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  methods _PARSE_ELEMENTARY
    importing
      !IV_SQL_CONDITION type STRING .
private section.

  types:
    BEGIN OF ty_one_condition,
           where_text       TYPE string,
           data_set_name    TYPE string,
           field_name       TYPE string,
           compare_function TYPE string,
           operand_value    TYPE string,
           ref_to_parser    TYPE REF TO zcl_zosql_where_parser,
         END OF ty_one_condition .
  types:
    ty_conditions TYPE STANDARD TABLE OF ty_one_condition WITH DEFAULT KEY .
  types:
    BEGIN OF TY_ONE_filter_BLOCK, " Block separated by 'OR' from other on same level
           where_text TYPE string,
           conditions TYPE ty_one_condition, " Blocks separated by 'AND' on one level
         END OF ty_one_filter_block .
  types:
    ty_filter_blocks TYPE STANDARD TABLE OF ty_one_filter_block WITH DEFAULT KEY .

  data M_EMPTY_CONDITION_FLAG type ABAP_BOOL .
  constants C_NOT type STRING value 'NOT' ##NO_TEXT.

  methods _CONVERT_LIKE_TO_CP
    importing
      !IA_MASK_FOR_LIKE type ANY
    returning
      value(RV_MASK_FOR_CP) type STRING .
  methods _GET_REF_TO_CONDITION_OPERAND
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_SQLTAB_ITERPOS
      !IV_DATASET_NAME_OR_ALIAS type STRING
      !IV_FIELDNAME_OR_VALUE type CLIKE
    returning
      value(RD_REF_TO_OPERAND) type ref to DATA .
  methods _GET_REF_TO_LEFT_OPERAND
    importing
      !IO_ITERATION_POSITION type ref to ZCL_ZOSQL_SQLTAB_ITERPOS
    returning
      value(RD_REF_TO_LEFT_OPERAND) type ref to DATA .
  methods _SPLIT_BY_AND_FILL_CONDITIONS
    importing
      !IV_CONDITION type STRING
      !IV_OR_OR_AND type STRING
    exporting
      !ET_CONDITION type TY_BLOCKS
      value(EV_CONDITION_FOUND) type ABAP_BOOL .
  methods _DELETE_BRACKETS_AROUND
    changing
      !CV_SQL_CONDITION type STRING .
  methods _HAS_BRACKETS_AROUND
    importing
      !IV_SQL_CONDITION type STRING
    returning
      value(RV_HAS_BRACKETS_AROUND) type ABAP_BOOL .
  methods _SPLIT_BY_OR_AND
    importing
      !IV_CONDITION type STRING
      !IV_OR_OR_AND type STRING
    exporting
      !ET_PARTS_AFTER_SPLIT type STRING_TABLE
      value(EV_CONDITION_FOUND) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_ZOSQL_SQLCOND_PARSER IMPLEMENTATION.


  method ZIF_ZOSQL_SQLCOND_PARSER~CHECK_CONDITION_FOR_CUR_REC.
    FIELD-SYMBOLS: <ls_condition> LIKE LINE OF mt_or_conditions.

    IF m_empty_condition_flag = abap_true.
      rv_condition_true = abap_true.
      RETURN.
    ENDIF.

    LOOP AT mt_or_conditions ASSIGNING <ls_condition>.
      IF <ls_condition>-parser->check_condition_for_cur_rec( io_iteration_position ) = abap_true.
        rv_condition_true = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    rv_condition_true = abap_true.
    LOOP AT mt_and_conditions ASSIGNING <ls_condition>.
      IF <ls_condition>-parser->check_condition_for_cur_rec( io_iteration_position ) <> abap_true.
        rv_condition_true = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    IF ms_not_condition-parser IS BOUND.
      rv_condition_true =
        zcl_zosql_utils=>boolean_not(
          ms_not_condition-parser->check_condition_for_cur_rec( io_iteration_position ) ).
      RETURN.
    ENDIF.

    rv_condition_true = _check_elementary( io_iteration_position ).
  endmethod.


  method ZIF_ZOSQL_SQLCOND_PARSER~GET_PARSER_INSTANCE.
  endmethod.


  method ZIF_ZOSQL_SQLCOND_PARSER~PARSE_CONDITION.
    DATA: lv_condition_or_found  TYPE abap_bool,
          lv_condition_and_found TYPE abap_bool.

    FIELD-SYMBOLS: <ls_or_condition>  LIKE LINE OF mt_or_conditions,
                   <ls_and_condition> LIKE LINE OF mt_and_conditions.

    IF iv_sql_condition IS INITIAL.
      m_empty_condition_flag = abap_true.
      RETURN.
    ENDIF.

    _split_by_and_fill_conditions( EXPORTING iv_condition       = iv_sql_condition
                                             iv_or_or_and       = 'OR'
                                   IMPORTING et_condition       = mt_or_conditions
                                             ev_condition_found = lv_condition_or_found ).

    IF lv_condition_or_found = abap_true.
      RETURN.
    ENDIF.

    _split_by_and_fill_conditions( EXPORTING iv_condition       = iv_sql_condition
                                             iv_or_or_and       = 'AND'
                                   IMPORTING et_condition       = mt_and_conditions
                                             ev_condition_found = lv_condition_and_found ).

    IF lv_condition_and_found = abap_true.
      RETURN.
    ENDIF.

    IF zcl_zosql_utils=>check_starts_with( iv_sql         = iv_sql_condition
                                           iv_starts_with = c_not ) = abap_true.

      DATA: lv_condition_without_not TYPE string.

      ms_not_condition-parser = zif_zosql_sqlcond_parser~get_parser_instance( ).
      lv_condition_without_not =
        zcl_zosql_utils=>delete_start_word_if_equals( iv_sql_source           = iv_sql_condition
                                                      iv_start_word_to_delete = c_not ).

      ms_not_condition-parser->parse_condition( lv_condition_without_not ).
      RETURN.
    ENDIF.

    IF _has_brackets_around( iv_sql_condition ) = abap_true.

      DATA: lv_condition_no_brackets TYPE string.

      lv_condition_no_brackets = iv_sql_condition.
      _delete_brackets_around( CHANGING cv_sql_condition = lv_condition_no_brackets ).
      zif_zosql_sqlcond_parser~parse_condition( iv_sql_condition = lv_condition_no_brackets ).
      RETURN.
    ENDIF.

    _parse_elementary( iv_sql_condition ).
  endmethod.


  METHOD _CHECK_ELEMENTARY.

    DATA: ld_ref_to_left_operand  TYPE REF TO data,
          ld_ref_to_right_operand TYPE REF TO data.

    FIELD-SYMBOLS: <lv_value_of_left_operand>  TYPE any,
                   <lv_value_or_right_operand> TYPE any,
                   <lt_value_as_range>         TYPE STANDARD TABLE.

    ld_ref_to_left_operand = _get_ref_to_left_operand( io_iteration_position ).

    IF ld_ref_to_left_operand IS NOT BOUND.
      RETURN.
    ENDIF.

    ld_ref_to_right_operand = _get_ref_to_right_operand( io_iteration_position ).

    IF ld_ref_to_right_operand IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN ld_ref_to_left_operand->* TO <lv_value_of_left_operand>.
    ASSIGN ld_ref_to_right_operand->* TO <lv_value_or_right_operand>.

    CASE m_operation.
      WHEN 'EQ' OR '='.
        IF <lv_value_of_left_operand> = <lv_value_or_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'GE' OR '>='.
        IF <lv_value_of_left_operand> >= <lv_value_or_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'GT' OR '>'.
        IF <lv_value_of_left_operand> > <lv_value_or_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'LE' OR '<='.
        IF <lv_value_of_left_operand> <= <lv_value_or_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'LT' OR '<'.
        IF <lv_value_of_left_operand> < <lv_value_or_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'NE' OR '<>'.
        IF <lv_value_of_left_operand> <> <lv_value_or_right_operand>.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN c_in.
        IF zcl_zosql_utils=>is_internal_table( <lv_value_or_right_operand> ) = abap_true.
          ASSIGN <lv_value_or_right_operand> TO <lt_value_as_range>.
          IF <lv_value_of_left_operand> IN <lt_value_as_range>.
            rv_conditions_true = abap_true.
          ENDIF.
        ELSE.
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN 'LIKE'.
        IF <lv_value_of_left_operand> CP _convert_like_to_cp( <lv_value_or_right_operand> ).
          rv_conditions_true = abap_true.
        ENDIF.
      WHEN OTHERS.
        MESSAGE e066 WITH m_operation INTO zcl_zosql_utils=>dummy.
        zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDCASE.
  ENDMETHOD.


  method _CLEAR_QUOTES_FROM_VALUE.
    m_fieldname_right_or_value = zcl_zosql_utils=>clear_quotes_from_value( m_fieldname_right_or_value ).
  endmethod.


  method _CONVERT_LIKE_TO_CP.
    rv_mask_for_cp = ia_mask_for_like.
    REPLACE ALL OCCURRENCES OF '%' IN rv_mask_for_cp WITH '*'.
    REPLACE ALL OCCURRENCES OF '_' IN rv_mask_for_cp WITH '+'.
  endmethod.


  METHOD _DELETE_BRACKETS_AROUND.

    DATA: lv_condition_len TYPE i,
          lv_new_len       TYPE i.

    IF _has_brackets_around( cv_sql_condition ) = abap_true.
      cv_sql_condition = zcl_zosql_utils=>condense( cv_sql_condition ).
      lv_condition_len = strlen( cv_sql_condition ).
      lv_new_len = lv_condition_len - 2.
      cv_sql_condition = cv_sql_condition+1(lv_new_len).
    ENDIF.
  ENDMETHOD.


  METHOD _GET_REF_TO_CONDITION_OPERAND.

    DATA: ls_data_set            TYPE zcl_zosql_sqltab_iterpos=>ty_data_set,
          lv_dataset_name        LIKE m_dataset_name_or_alias_left.

    IF iv_dataset_name_or_alias IS INITIAL.
      ls_data_set = io_iteration_position->get_first_data_set( ).
      lv_dataset_name = ls_data_set-dataset_name.
    ELSE.
      lv_dataset_name = iv_dataset_name_or_alias.
    ENDIF.

    IF lv_dataset_name IS INITIAL.
      RETURN.
    ENDIF.

    rd_ref_to_operand = io_iteration_position->get_field_ref_of_data_set( iv_dataset_name_or_alias = lv_dataset_name
                                                                          iv_fieldname             = iv_fieldname_or_value ).
  ENDMETHOD.


  METHOD _GET_REF_TO_LEFT_OPERAND.
    rd_ref_to_left_operand = _get_ref_to_condition_operand( io_iteration_position = io_iteration_position
                                                            iv_dataset_name_or_alias = m_dataset_name_or_alias_left
                                                            iv_fieldname_or_value    = m_fieldname_left ).
  ENDMETHOD.


  method _GET_REF_TO_RIGHT_OPERAND.

    FIELD-SYMBOLS: <lv_return_value> TYPE any.

    rd_ref_to_right_operand = _get_ref_to_condition_operand( io_iteration_position    = io_iteration_position
                                                             iv_dataset_name_or_alias = m_dataset_name_or_alias_right
                                                             iv_fieldname_or_value    = m_fieldname_right_or_value ).

    IF rd_ref_to_right_operand IS NOT BOUND.
      CREATE DATA rd_ref_to_right_operand LIKE m_fieldname_right_or_value.
      ASSIGN rd_ref_to_right_operand->* TO <lv_return_value>.
      <lv_return_value> = m_fieldname_right_or_value.
    ENDIF.
  endmethod.


  method _HAS_BRACKETS_AROUND.

    DATA: lv_sql_condition TYPE string,
          lv_condition_len TYPE i.

    lv_sql_condition = zcl_zosql_utils=>condense( iv_sql_condition ).

    lv_condition_len = strlen( lv_sql_condition ).
    IF lv_condition_len <= 1.
      RETURN.
    ENDIF.

    IF lv_sql_condition(1) = '(' AND zcl_zosql_utils=>get_last_n_chars( iv_string              = lv_sql_condition
                                                                        iv_how_many_characters = 1 ) = ')'.
      rv_has_brackets_around = abap_true.
    ENDIF.
  endmethod.


  METHOD _PARSE_ELEMENTARY.

    DATA: lv_condition     TYPE string,
          lv_operand_left  TYPE string,
          lv_operation     TYPE string,
          lv_operand_right TYPE string.

    lv_condition = zcl_zosql_utils=>to_upper_case( iv_sql_condition ).
    CONDENSE lv_condition.
    SPLIT lv_condition AT space INTO lv_operand_left m_operation lv_operand_right.

    SPLIT lv_operand_left AT '~' INTO m_dataset_name_or_alias_left m_fieldname_left.

    IF m_fieldname_left IS INITIAL.
      m_fieldname_left = m_dataset_name_or_alias_left.
      CLEAR m_dataset_name_or_alias_left.
    ENDIF.

    SPLIT lv_operand_right AT '~' INTO m_dataset_name_or_alias_right m_fieldname_right_or_value.

    IF m_fieldname_right_or_value IS INITIAL.
      m_fieldname_right_or_value = m_dataset_name_or_alias_right.
      CLEAR m_dataset_name_or_alias_right.
    ENDIF.
  ENDMETHOD.


  method _SPLIT_BY_AND_FILL_CONDITIONS.

    DATA: lt_parts_after_split  TYPE TABLE OF string,
          lv_part               TYPE string.

    FIELD-SYMBOLS: <ls_condition> LIKE LINE OF et_condition.

    REFRESH et_condition.

    _split_by_or_and( EXPORTING iv_condition         = iv_condition
                                iv_or_or_and         = iv_or_or_and
                      IMPORTING et_parts_after_split = lt_parts_after_split
                                ev_condition_found   = ev_condition_found ).

    IF ev_condition_found = abap_true.
      LOOP AT lt_parts_after_split INTO lv_part.
        APPEND INITIAL LINE TO et_condition ASSIGNING <ls_condition>.
        <ls_condition>-parser = zif_zosql_sqlcond_parser~get_parser_instance( ).
        _delete_brackets_around( CHANGING cv_sql_condition = lv_part ).
        <ls_condition>-parser->parse_condition( iv_sql_condition = lv_part ).
      ENDLOOP.
    ENDIF.
  endmethod.


  method _SPLIT_BY_OR_AND.

    DATA: ltd_words     TYPE TABLE OF string,
          lv_word       TYPE string,
          lv_word_upper TYPE string,
          lv_where_part TYPE string,
          lv_bracket_depth TYPE i.

    CLEAR et_parts_after_split.

    SPLIT iv_condition AT space INTO TABLE ltd_words.

    LOOP AT ltd_words INTO lv_word.
      lv_word_upper = zcl_zosql_utils=>to_upper_case( lv_word ).

      IF lv_word = '('.
        lv_bracket_depth = lv_bracket_depth + 1.
      ELSEIF lv_word = ')'.
        lv_bracket_depth = lv_bracket_depth - 1.
      ENDIF.

      IF lv_word_upper = iv_or_or_and AND lv_bracket_depth = 0.
        APPEND lv_where_part TO et_parts_after_split.
        CLEAR lv_where_part.
        ev_condition_found = abap_true.
      ELSE.
        CONCATENATE lv_where_part lv_word INTO lv_where_part SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    APPEND lv_where_part TO et_parts_after_split.
  endmethod.
ENDCLASS.
