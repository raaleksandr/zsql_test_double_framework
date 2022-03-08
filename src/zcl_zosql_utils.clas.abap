class ZCL_ZOSQL_UTILS definition
  public
  final
  create public .

public section.

  class-data DUMMY type TEXT255 .

  class-methods CREATE_HASH_TAB_LIKE_STANDARD
    importing
      !IT_STANDARD_TABLE_TEMPLATE type STANDARD TABLE
      !IT_KEY_FIELDS type FIELDNAME_TABLE
    returning
      value(RD_HASHED_TABLE) type ref to DATA .
  class-methods DELETE_N_END_TOKENS
    importing
      value(IV_N) type I
    changing
      !CV_SQL type CLIKE .
  class-methods SPLIT_CONDITION_INTO_TOKENS
    importing
      !IV_SQL_CONDITION type CLIKE
    returning
      value(RT_TOKENS) type STRING_TABLE .
  class-methods SALV_SET_FIELDNAMES_TO_COL_TIT
    importing
      !IO_SALV type ref to CL_SALV_TABLE
      !IT_TABLE type ANY TABLE .
  class-methods IS_VERSION_740_AND_ABOVE
    returning
      value(RV_IS_740_OR_ABOVE) type ABAP_BOOL .
  class-methods BOOLEAN_NOT
    importing
      !IV_BOOLEAN type ABAP_BOOL
    returning
      value(RV_RESULT) type ABAP_BOOL .
  class-methods TRY_TO_GUESS_TABNAME_BY_DATA
    importing
      !ITA_TABLE type ANY TABLE
    returning
      value(RV_TABLE_NAME) type TABNAME16 .
  class-methods CONDENSE
    importing
      !IV_TEXT_TO_CONDENSE type CLIKE
    returning
      value(RV_CONDENSED_TEXT) type STRING .
  class-methods COPY_AND_RETURN_AS_REF_TO_DATA
    importing
      !IV_VALUE type ANY
    returning
      value(RD_REF_TO_COPY_OF_DATA) type ref to DATA .
  class-methods GET_FIRST_N_CHARS
    importing
      !IV_STRING type CLIKE
      value(IV_HOW_MANY_CHARACTERS) type INT4 default 1
    returning
      value(RV_FIRST_N_CHARACTERS) type STRING .
  class-methods GET_LAST_N_CHARS
    importing
      !IV_STRING type CLIKE
      value(IV_HOW_MANY_CHARACTERS) type INT4 default 1
    returning
      value(RV_LAST_N_CHARACTERS) type STRING .
  class-methods GET_START_TOKEN
    importing
      !IV_SQL type CLIKE
    returning
      value(RV_FIRST_WORD) type STRING .
  class-methods MOVE_CORRESPONDING_TABLE
    importing
      !IT_TABLE_SRC type ANY TABLE
    exporting
      !ET_TABLE_DEST type ANY TABLE .
  class-methods MOVE_ONE_REF_TO_ANOTHER
    importing
      !IO_REF_TO_DATA_SOURCE type ref to DATA
      !IO_REF_TO_DATA_DESTINATION type ref to DATA .
  class-methods RAISE_EXCEPTION_FROM_SY_MSG
    importing
      value(IV_MSGID) type SYMSGID default SY-MSGID
      value(IV_MSGNO) type SYMSGNO default SY-MSGNO
      value(IV_MSGV1) type SYMSGV default SY-MSGV1
      value(IV_MSGV2) type SYMSGV default SY-MSGV2
      value(IV_MSGV3) type SYMSGV default SY-MSGV3
      value(IV_MSGV4) type SYMSGV default SY-MSGV4
    raising
      ZCX_ZOSQL_ERROR .
  class-methods TO_UPPER_CASE
    importing
      !IV_STRING type CLIKE
    returning
      value(RV_STRING_IN_UPPER_CASE) type STRING .
  class-methods TRANSPARENT_TABLE_EXISTS
    importing
      !IV_TABLE_NAME type CLIKE
    returning
      value(RV_TRANSPARENT_TABLE_EXISTS) type ABAP_BOOL .
  class-methods IS_NUMBER
    importing
      !I_VARIABLE type ANY
    returning
      value(RV_IS_NUMBER) type ABAP_BOOL .
  class-methods IS_CHAR
    importing
      !I_VARIABLE type ANY
    returning
      value(RV_IS_CHAR) type ABAP_BOOL .
  class-methods IS_STRUCTURE
    importing
      !I_VARIABLE type ANY
    returning
      value(RV_IS_STRUCTURE) type ABAP_BOOL .
  class-methods IS_INTERNAL_TABLE
    importing
      !I_VARIABLE type ANY
    returning
      value(RV_IS_INTERNAL_TABLE) type ABAP_BOOL .
  class-methods CHECK_ENDS_WITH_TOKEN
    importing
      !IV_SQL type CLIKE
      !IV_TOKEN type CLIKE
    returning
      value(RV_ENDS_WITH_THE_TOKEN) type ABAP_BOOL .
  class-methods CHECK_STARTS_WITH_TOKEN
    importing
      !IV_SQL type CLIKE
      !IV_TOKEN type CLIKE
    returning
      value(RV_STARTS_WITH_THE_TOKEN) type ABAP_BOOL .
  class-methods RAISE_IF_TRANSP_TAB_NOT_EXIST
    importing
      !IV_TABLE_NAME type CLIKE
    returning
      value(RV_TRANSPARENT_TABLE_EXISTS) type ABAP_BOOL
    raising
      ZCX_ZOSQL_ERROR .
  class-methods CLEAR_QUOTES_FROM_VALUE
    importing
      !IV_VALUE type CLIKE
    returning
      value(RV_VALUE) type STRING .
  class-methods DELETE_END_TOKEN_IF_EQUALS
    importing
      !IV_SQL_SOURCE type CLIKE
      !IV_END_TOKEN_TO_DELETE type CLIKE
    returning
      value(RV_SQL_WITHOUT_TOKEN) type STRING .
  class-methods DELETE_START_TOKEN
    importing
      !IV_SQL_SOURCE type CLIKE
    returning
      value(RV_SQL_WITHOUT_TOKEN) type STRING .
  class-methods DELETE_START_TOKEN_IF_EQUALS
    importing
      !IV_SQL_SOURCE type CLIKE
      !IV_START_TOKEN_TO_DELETE type CLIKE
    returning
      value(RV_SQL_WITHOUT_TOKEN) type STRING .
protected section.
private section.

  class-methods _SPLIT_INTO_TOKENS_AT_SEP_ITAB
    importing
      !IT_CONDITIONS_TABLE type STRING_TABLE
      value(IV_SEPARATOR) type CLIKE optional
    returning
      value(RT_TOKENS) type STRING_TABLE .
  class-methods _SPLIT_INTO_TOKENS_AT_SEP
    importing
      !IV_SQL_CONDITION type CLIKE
      value(IV_SEPARATOR) type CLIKE optional
    returning
      value(RT_TOKENS) type STRING_TABLE .
ENDCLASS.



CLASS ZCL_ZOSQL_UTILS IMPLEMENTATION.


  method BOOLEAN_NOT.
    IF iv_boolean = abap_true.
      rv_result = abap_false.
    ELSE.
      rv_result = abap_true.
    ENDIF.
  endmethod.


  METHOD CHECK_ENDS_WITH_TOKEN.

    DATA: lv_sql                      TYPE string,
          lv_ends_with                TYPE string,
          lv_length_of_whole_string   TYPE i,
          lv_length_of_string_to_find TYPE i,
          lv_i                        TYPE i,
          lt_results                  TYPE match_result_tab,
          ls_result                   TYPE match_result.

    lv_sql = zcl_zosql_utils=>to_upper_case( iv_sql ).
    CONDENSE lv_sql.
    lv_length_of_whole_string = strlen( lv_sql ).

    lv_ends_with = zcl_zosql_utils=>to_upper_case( iv_token ).
    lv_length_of_string_to_find = strlen( lv_ends_with ).

    lv_i = lv_length_of_whole_string - lv_length_of_string_to_find.

    FIND ALL OCCURRENCES OF lv_ends_with IN lv_sql RESULTS lt_results.
    READ TABLE lt_results INDEX lines( lt_results ) INTO ls_result.
    IF sy-subrc = 0 AND ls_result-offset >= lv_i.
      rv_ends_with_the_token = abap_true.
    ENDIF.
  ENDMETHOD.


  method CHECK_STARTS_WITH_TOKEN.

    DATA: lv_sql         TYPE string,
          lv_starts_with TYPE string,
          lv_offset      TYPE i.

    lv_sql = zcl_zosql_utils=>to_upper_case( iv_sql ).
    CONDENSE lv_sql.

    lv_starts_with = zcl_zosql_utils=>to_upper_case( iv_token ).

    FIND FIRST OCCURRENCE OF lv_starts_with IN lv_sql MATCH OFFSET lv_offset.
    IF sy-subrc = 0 AND lv_offset = 0.
      rv_starts_with_the_token = abap_true.
    ENDIF.
  endmethod.


  method CLEAR_QUOTES_FROM_VALUE.

    DATA: lv_value_len TYPE i,
          lv_new_len   TYPE i.

    IF iv_value IS INITIAL.
      RETURN.
    ENDIF.

    rv_value = iv_value.
    IF rv_value(1) = ''''
      AND zcl_zosql_utils=>get_last_n_chars( iv_string              = rv_value
                                             iv_how_many_characters = 1 ) = ''''
      AND strlen( rv_value ) > 2.

      lv_value_len = strlen( rv_value ).
      lv_new_len = lv_value_len - 2.
      rv_value = rv_value+1(lv_new_len).
    ENDIF.
  endmethod.


  method CONDENSE.
    rv_condensed_text = iv_text_to_condense.
    CONDENSE rv_condensed_text.
  endmethod.


  method COPY_AND_RETURN_AS_REF_TO_DATA.

    FIELD-SYMBOLS: <lv_value_copy> TYPE any.

    CREATE DATA rd_ref_to_copy_of_data LIKE iv_value.
    ASSIGN rd_ref_to_copy_of_data->* TO <lv_value_copy>.
    <lv_value_copy> = iv_value.
  endmethod.


  METHOD create_hash_tab_like_standard.
    DATA: lo_table_data_set  TYPE REF TO cl_abap_tabledescr,
          lo_struct_data_set TYPE REF TO cl_abap_structdescr,
          lt_key             TYPE abap_keydescr_tab,
          lo_hashed_table    TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <ls_key_field> LIKE LINE OF it_key_fields,
                   <ls_key>       LIKE LINE OF lt_key.

    lo_table_data_set ?= cl_abap_tabledescr=>describe_by_data( it_standard_table_template ).
    lo_struct_data_set ?= lo_table_data_set->get_table_line_type( ).

    LOOP AT it_key_fields ASSIGNING <ls_key_field>.
      APPEND INITIAL LINE TO lt_key ASSIGNING <ls_key>.
      <ls_key>-name = <ls_key_field>-fieldname.
    ENDLOOP.

    lo_hashed_table = cl_abap_tabledescr=>create( p_line_type  = lo_struct_data_set
                                                  p_table_kind = cl_abap_tabledescr=>tablekind_hashed
                                                  p_unique     = abap_true
                                                  p_key        = lt_key ).

    CREATE DATA rd_hashed_table TYPE HANDLE lo_hashed_table.
  ENDMETHOD.


  method DELETE_END_TOKEN_IF_EQUALS.

    DATA: lv_end_token_len       TYPE i,
          lv_end_token_to_delete TYPE string,
          lv_return_len          TYPE i,
          lv_sql_source          TYPE string,
          lv_all_sql_len         TYPE i.

    IF check_ends_with_token( iv_sql   = iv_sql_source
                              iv_token = iv_end_token_to_delete ) = abap_true.

      lv_end_token_to_delete = iv_end_token_to_delete.
      lv_end_token_len = strlen( lv_end_token_to_delete ).
      lv_sql_source = iv_sql_source.
      lv_all_sql_len = strlen( lv_sql_source ).
      IF lv_end_token_len < strlen( iv_sql_source ).
        lv_return_len = lv_all_sql_len - lv_end_token_len.
        rv_sql_without_token = iv_sql_source(lv_return_len).
        CONDENSE rv_sql_without_token.
      ENDIF.
    ELSE.
      rv_sql_without_token = iv_sql_source.
    ENDIF.
  endmethod.


  method DELETE_N_END_TOKENS.
    DATA: lt_tokens    TYPE TABLE OF string,
          lv_end_index TYPE i,
          lv_token     TYPE string.

    lt_tokens = split_condition_into_tokens( cv_sql ).
    lv_end_index = LINES( lt_tokens ) - iv_n.

    CLEAR cv_sql.

    LOOP AT lt_tokens INTO lv_token
      TO lv_end_index.

      CONCATENATE cv_sql lv_token INTO cv_sql SEPARATED BY space.
    ENDLOOP.
  endmethod.


  method DELETE_START_TOKEN.
    SPLIT iv_sql_source AT space INTO zcl_zosql_utils=>dummy rv_sql_without_token.
  endmethod.


  method DELETE_START_TOKEN_IF_EQUALS.

    DATA: lv_start_token_len TYPE i,
          lv_sql_source      TYPE string.

    IF check_starts_with_token( iv_sql   = iv_sql_source
                                iv_token = iv_start_token_to_delete ) = abap_true.

      lv_sql_source = iv_sql_source.
      SHIFT lv_sql_source LEFT DELETING LEADING space.
      lv_start_token_len = strlen( iv_start_token_to_delete ).
      IF lv_start_token_len < strlen( lv_sql_source ).
        rv_sql_without_token = lv_sql_source+lv_start_token_len.
        CONDENSE rv_sql_without_token.
      ENDIF.
    ELSE.
      rv_sql_without_token = iv_sql_source.
    ENDIF.
  endmethod.


  method GET_FIRST_N_CHARS.

    DATA: lv_string TYPE string.

    lv_string = iv_string.

    IF strlen( lv_string ) >= iv_how_many_characters.
      rv_first_n_characters = lv_string(iv_how_many_characters).
    ELSE.
      rv_first_n_characters = iv_string.
    ENDIF.
  endmethod.


  method GET_LAST_N_CHARS.

    DATA: lv_len TYPE i,
          lv_str TYPE string,
          lv_start_pos TYPE i.

    lv_str = iv_string.
    lv_len = strlen( lv_str ).

    IF lv_len <= iv_how_many_characters.
      rv_last_n_characters = iv_string.
    ELSE.
      lv_start_pos = lv_len - iv_how_many_characters.
      rv_last_n_characters = lv_str+lv_start_pos.
    ENDIF.
  endmethod.


  method GET_START_TOKEN.
    SPLIT iv_sql AT space INTO rv_first_word zcl_zosql_utils=>dummy.
  endmethod.


  METHOD IS_CHAR.

    DATA: lv_type TYPE char1.

    DESCRIBE FIELD i_variable TYPE lv_type.
    IF lv_type CA 'cgCG'.
      rv_is_char = abap_true.
    ENDIF.
  ENDMETHOD.


  method IS_INTERNAL_TABLE.
    DATA: lv_type TYPE char1.

    DESCRIBE FIELD i_variable TYPE lv_type.
    IF lv_type CA 'Hh'.
      rv_is_internal_table = abap_true.
    ENDIF.
  endmethod.


  METHOD IS_NUMBER.

    DATA: lv_type TYPE char1.

    DESCRIBE FIELD i_variable TYPE lv_type.
    IF lv_type CA 'bsIPaeF'.
      rv_is_number = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD IS_STRUCTURE.
    CASE cl_abap_datadescr=>get_data_type_kind( i_variable ).
      WHEN cl_abap_typedescr=>typekind_struct1
        OR cl_abap_typedescr=>typekind_struct2.

        rv_is_structure = abap_true.
    ENDCASE.
  ENDMETHOD.


  METHOD is_version_740_and_above.
    IF sy-saprl >= '740'.
      rv_is_740_or_above = abap_true.
    ENDIF.
  ENDMETHOD.


  method MOVE_CORRESPONDING_TABLE.

    DATA: ld_table_dest_line TYPE REF TO data.

    FIELD-SYMBOLS: <ls_table_src>  TYPE any,
                   <ls_table_dest> TYPE any.

    CLEAR et_table_dest.

    CREATE DATA ld_table_dest_line LIKE LINE OF et_table_dest.
    ASSIGN ld_table_dest_line->* TO <ls_table_dest>.

    LOOP AT it_table_src ASSIGNING <ls_table_src>.
      CLEAR <ls_table_dest>.
      MOVE-CORRESPONDING <ls_table_src> TO <ls_table_dest>.
      INSERT <ls_table_dest> INTO TABLE et_table_dest.
    ENDLOOP.
  endmethod.


  method MOVE_ONE_REF_TO_ANOTHER.

    FIELD-SYMBOLS: <lv_value_of_source> TYPE any,
                   <lv_value_of_destionation> TYPE any.

    IF io_ref_to_data_source IS NOT BOUND OR io_ref_to_data_destination IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN io_ref_to_data_source->* TO <lv_value_of_source>.
    ASSIGN io_ref_to_data_destination->* TO <lv_value_of_destionation>.

    <lv_value_of_destionation> = <lv_value_of_source>.
  endmethod.


  METHOD RAISE_EXCEPTION_FROM_SY_MSG.

    DATA: ls_t100_msg TYPE symsg.

    ls_t100_msg-msgid = iv_msgid.
    ls_t100_msg-msgno = iv_msgno.
    ls_t100_msg-msgv1 = iv_msgv1.
    ls_t100_msg-msgv2 = iv_msgv2.
    ls_t100_msg-msgv3 = iv_msgv3.
    ls_t100_msg-msgv4 = iv_msgv4.

    RAISE EXCEPTION TYPE zcx_zosql_error
      EXPORTING
        t100_message = ls_t100_msg.
  ENDMETHOD.


  method RAISE_IF_TRANSP_TAB_NOT_EXIST.

    DATA: lv_tabclass TYPE dd02l-tabclass.

    SELECT SINGLE TABCLASS
      INTO lv_tabclass
      FROM dd02l
      WHERE tabname = iv_table_name
        AND as4local = 'A'
        AND as4vers  = '0000'.

    IF sy-subrc <> 0.
      MESSAGE e062 WITH iv_table_name INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    IF lv_tabclass <> 'TRANSP'.
      MESSAGE e063 WITH iv_table_name INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.
  endmethod.


  METHOD salv_set_fieldnames_to_col_tit.
    DATA: lo_columns             TYPE REF TO cl_salv_columns_table,
          lo_table               TYPE REF TO cl_abap_tabledescr,
          lo_struct              TYPE REF TO cl_abap_structdescr,
          lt_components_of_table TYPE cl_abap_structdescr=>component_table,
          lo_column              TYPE REF TO cl_salv_column,
          lv_column_name         TYPE lvc_fname,
          lv_short_text          TYPE scrtext_s,
          lv_medium_text         TYPE scrtext_m,
          lv_long_text           TYPE scrtext_l.

    FIELD-SYMBOLS: <ls_component> LIKE LINE OF lt_components_of_table.

    lo_table ?= cl_abap_tabledescr=>describe_by_data( it_table ).
    lo_struct ?= lo_table->get_table_line_type( ).
    lt_components_of_table = lo_struct->get_components( ).

    lo_columns = io_salv->get_columns( ).
    LOOP AT lt_components_of_table ASSIGNING <ls_component>.
      lv_column_name = <ls_component>-name.
      TRY.
          lo_column = lo_columns->get_column( lv_column_name ).

          lv_short_text = <ls_component>-name.
          lo_column->set_short_text( lv_short_text ).

          lv_medium_text = <ls_component>-name.
          lo_column->set_medium_text( lv_medium_text ).

          lv_long_text = <ls_component>-name.
          lo_column->set_long_text( lv_long_text ).
        CATCH cx_salv_not_found.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  method SPLIT_CONDITION_INTO_TOKENS.

    rt_tokens = _split_into_tokens_at_sep( iv_sql_condition = iv_sql_condition
                                           iv_separator     = space ).

    rt_tokens = _split_into_tokens_at_sep_itab( it_conditions_table = rt_tokens
                                                iv_separator        = cl_abap_char_utilities=>cr_lf ).

    rt_tokens = _split_into_tokens_at_sep_itab( it_conditions_table = rt_tokens
                                                iv_separator        = cl_abap_char_utilities=>horizontal_tab ).

    rt_tokens = _split_into_tokens_at_sep_itab( it_conditions_table = rt_tokens
                                                iv_separator        = ',' ).

    rt_tokens = _split_into_tokens_at_sep_itab( it_conditions_table = rt_tokens
                                                iv_separator        = '(' ).

    rt_tokens = _split_into_tokens_at_sep_itab( it_conditions_table = rt_tokens
                                                iv_separator        = ')' ).
  endmethod.


  method TO_UPPER_CASE.
    rv_string_in_upper_case = iv_string.
    TRANSLATE rv_string_in_upper_case TO UPPER CASE.
  endmethod.


  method TRANSPARENT_TABLE_EXISTS.
    SELECT SINGLE tabclass
      INTO zcl_zosql_utils=>dummy
      FROM dd02l
      WHERE tabname = iv_table_name
        AND as4local = 'A'
        AND as4vers  = '0000'
        AND tabclass = 'TRANSP'.

    IF sy-subrc = 0.
      rv_transparent_table_exists = abap_true.
    ENDIF.
  endmethod.


  method TRY_TO_GUESS_TABNAME_BY_DATA.

    DATA: lo_table_descr TYPE REF TO cl_abap_tabledescr,
          lo_struct_descr TYPE REF TO cl_abap_structdescr,
          lv_ddic_type    TYPE tabname.

    lo_table_descr ?= cl_abap_tabledescr=>describe_by_data( ita_table ).
    lo_struct_descr ?= lo_table_descr->get_table_line_type( ).
    IF lo_struct_descr->is_ddic_type( ) = abap_true.
      lv_ddic_type = lo_struct_descr->get_relative_name( ).
      IF transparent_table_exists( lv_ddic_type ) = abap_true.
        rv_table_name = lv_ddic_type.
      ENDIF.
    ENDIF.
  endmethod.


  method _SPLIT_INTO_TOKENS_AT_SEP.

    CONSTANTS: lc_quote  TYPE char1 VALUE ''''.

    DATA: lt_words         TYPE TABLE OF string,
          lv_word          TYPE string,
          lv_inside_quote  TYPE abap_bool,
          lv_new_token     TYPE string,
          lt_results       TYPE match_result_tab,
          lv_first         TYPE abap_bool,
          lv_separator     TYPE string,
          lv_separator_len TYPE i.

    lv_separator = iv_separator.
    lv_separator_len = strlen( lv_separator ).

    SPLIT iv_sql_condition AT iv_separator INTO TABLE lt_words.

    lv_first = abap_true.
    LOOP AT lt_words INTO lv_word.

      REFRESH lt_results.
      FIND ALL OCCURRENCES OF lc_quote IN lv_word RESULTS lt_results.
      IF LINES( lt_results ) MOD 2 = 1.
        lv_inside_quote = zcl_zosql_utils=>boolean_not( lv_inside_quote ).
      ENDIF.

      IF lv_new_token IS INITIAL.
        lv_new_token = lv_word.
      ELSE.
        CONCATENATE lv_new_token lv_word INTO lv_new_token SEPARATED BY space.
      ENDIF.

      IF lv_inside_quote <> abap_true.

        IF lv_first <> abap_true.
          APPEND iv_separator TO rt_tokens.
        ENDIF.

        APPEND lv_new_token TO rt_tokens.
        CLEAR lv_new_token.
      ENDIF.

      lv_first = abap_false.
    ENDLOOP.

    IF get_last_n_chars( iv_string              = iv_sql_condition
                         iv_how_many_characters = lv_separator_len ) = iv_separator.

      APPEND iv_separator TO rt_tokens.
    ENDIF.

    DELETE rt_tokens
      WHERE table_line IS INITIAL
         OR table_line = cl_abap_char_utilities=>cr_lf
         OR table_line = cl_abap_char_utilities=>horizontal_tab.
  endmethod.


  method _SPLIT_INTO_TOKENS_AT_SEP_ITAB.

    DATA: lv_sql_condition           TYPE string,
          lt_tokens_of_one_condition TYPE TABLE OF string.

    LOOP AT it_conditions_table INTO lv_sql_condition.
      lt_tokens_of_one_condition = _split_into_tokens_at_sep( iv_sql_condition = lv_sql_condition
                                                              iv_separator     = iv_separator ).
      APPEND LINES OF lt_tokens_of_one_condition TO rt_tokens.
    ENDLOOP.
  endmethod.
ENDCLASS.
