class ZCL_ZOSQL_VIEW_ITERATOR definition
  public
  inheriting from ZCL_ZOSQL_SELECT_ITER
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT optional
      !IV_VIEW_NAME type CLIKE
    raising
      ZCX_ZOSQL_ERROR .
  class-methods IS_DATABASE_VIEW
    importing
      !IV_NAME_OF_OBJECT type CLIKE
    returning
      value(RV_IS_DATABASE_VIEW) type ABAP_BOOL .
protected section.
private section.

  data MV_VIEW_NAME type VIEWNAME .

  class-methods _CONVERT_VIEW_OPERATOR_TO_SQL
    importing
      value(IV_OPERATOR) type VSOPERATOR
    returning
      value(RV_SQL_OPERATOR) type STRING .
  class-methods _GET_FROM_SQL_PART
    importing
      value(IV_VIEW_NAME) type VIEWNAME
    returning
      value(RV_FROM_SQL_PART) type STRING .
  class-methods _GET_SELECT_SQL_PART
    importing
      value(IV_VIEW_NAME) type VIEWNAME
    returning
      value(RV_SELECT_SQL_PART) type STRING .
  class-methods _GET_VIEW_SQL
    importing
      value(IV_VIEW_NAME) type VIEWNAME
    returning
      value(RV_VIEW_SQL) type STRING .
  class-methods _GET_WHERE_SQL_PART
    importing
      value(IV_VIEW_NAME) type VIEWNAME
    returning
      value(RV_WHERE_SQL_PART) type STRING .
ENDCLASS.



CLASS ZCL_ZOSQL_VIEW_ITERATOR IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA: lv_view_sql  TYPE string,
          lv_view_name TYPE viewname.

    IF is_database_view( iv_view_name ) <> abap_true.
      MESSAGE e067 WITH iv_view_name INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.

    lv_view_name = zcl_zosql_utils=>to_upper_case( iv_view_name ).
    lv_view_sql = _get_view_sql( lv_view_name ).
    super->constructor( io_zosql_test_environment = io_zosql_test_environment
                        iv_select                 = lv_view_sql ).

    mv_view_name = lv_view_name.
  endmethod.


  method IS_DATABASE_VIEW.

    CONSTANTS: lc_viewclass_database TYPE VIEWCLASS VALUE 'D'.

    DATA: lv_viewname TYPE viewname.

    lv_viewname = zcl_zosql_utils=>to_upper_case( iv_name_of_object ).

    SELECT SINGLE viewname
      INTO lv_viewname
      FROM dd25l
      WHERE viewname  = lv_viewname
        AND as4local  = 'A'
        AND as4vers   = '0000'
        AND viewclass = lc_viewclass_database.

    IF sy-subrc = 0.
      rv_is_database_view = abap_true.
    ENDIF.
  endmethod.


  method _CONVERT_VIEW_OPERATOR_TO_SQL.

    IF iv_operator = 'LK'.
      rv_sql_operator = 'LIKE'.
    ELSE.
      rv_sql_operator = iv_operator.
    ENDIF.
  endmethod.


  METHOD _GET_FROM_SQL_PART.

    CONSTANTS: lc_negation_left_part TYPE vsnegation VALUE 'JL',
               lc_negation_right_part TYPE vsnegation VALUE 'JR'.

    TYPES: BEGIN OF ty_view_join_condition,
             left_tabname    TYPE tabname,
             left_fieldname  TYPE fieldname,
             operator        TYPE vsoperator,
             right_tabname   TYPE tabname,
             right_fieldname TYPE fieldname,
             and_or_after    TYPE vsconj,
           END OF ty_view_join_condition.

    DATA: lt_dd26s           TYPE TABLE OF dd26s,
          lt_dd28s           TYPE TABLE OF dd28s,
          lv_join_conditions TYPE string,
          lv_table_and_field TYPE string,
          lv_current_table   TYPE tabname,
          lt_join_conditions TYPE TABLE OF ty_view_join_condition.

    FIELD-SYMBOLS: <ls_dd26s>          LIKE LINE OF lt_dd26s,
                   <ls_dd28s>          LIKE LINE OF lt_dd28s,
                   <ls_join_condition> LIKE LINE OF lt_join_conditions.

    SELECT *
      FROM dd26s
      INTO CORRESPONDING FIELDS OF TABLE lt_dd26s
      WHERE viewname = iv_view_name
        AND as4local = 'A'
      ORDER BY tabpos.

    SELECT *
      FROM dd28s
      INTO CORRESPONDING FIELDS OF TABLE lt_dd28s
      WHERE condname = iv_view_name
        AND as4local = 'A'
        AND as4vers  = '0000'
      ORDER BY position.

    LOOP AT lt_dd28s ASSIGNING <ls_dd28s>.

      CASE <ls_dd28s>-negation.
        WHEN lc_negation_left_part.
          APPEND INITIAL LINE TO lt_join_conditions ASSIGNING <ls_join_condition>.
          <ls_join_condition>-left_tabname   = <ls_dd28s>-tabname.
          <ls_join_condition>-left_fieldname = <ls_dd28s>-fieldname.
          <ls_join_condition>-operator       = <ls_dd28s>-operator.
        WHEN lc_negation_right_part.
          <ls_join_condition>-right_tabname   = <ls_dd28s>-tabname.
          <ls_join_condition>-right_fieldname = <ls_dd28s>-fieldname.
          <ls_join_condition>-and_or_after    = <ls_dd28s>-and_or.
      ENDCASE.
    ENDLOOP.

    LOOP AT lt_dd26s ASSIGNING <ls_dd26s>.

      IF rv_from_sql_part IS INITIAL.
        rv_from_sql_part = <ls_dd26s>-tabname.
      ELSE.
        CONCATENATE rv_from_sql_part 'JOIN' <ls_dd26s>-tabname INTO rv_from_sql_part SEPARATED BY space.

        CLEAR lv_join_conditions.
        LOOP AT lt_join_conditions ASSIGNING <ls_join_condition>
          WHERE right_tabname = <ls_dd26s>-tabname.

          CONCATENATE <ls_join_condition>-left_tabname '~' <ls_join_condition>-left_fieldname INTO lv_table_and_field.
          CONCATENATE lv_table_and_field <ls_join_condition>-operator INTO lv_join_conditions SEPARATED BY space.

          CONCATENATE <ls_join_condition>-right_tabname '~' <ls_join_condition>-right_fieldname INTO lv_table_and_field.
          CONCATENATE lv_join_conditions lv_table_and_field <ls_join_condition>-and_or_after
            INTO lv_join_conditions SEPARATED BY space.
        ENDLOOP.

        IF lv_join_conditions IS NOT INITIAL.
          CONCATENATE rv_from_sql_part 'ON' lv_join_conditions INTO rv_from_sql_part SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF zcl_zosql_utils=>check_ends_with_token( iv_sql   = rv_from_sql_part
                                               iv_token = 'AND' ) = abap_true.

      rv_from_sql_part = zcl_zosql_utils=>delete_end_token_if_equals( iv_sql_source          = rv_from_sql_part
                                                                      iv_end_token_to_delete = 'AND' ).
    ENDIF.
  ENDMETHOD.


  METHOD _GET_SELECT_SQL_PART.

    DATA: lt_dd27s TYPE TABLE OF dd27s,
          lv_table_and_field TYPE string.

    FIELD-SYMBOLS: <ls_dd27s> LIKE LINE OF lt_dd27s.

    SELECT *
      FROM dd27s
      INTO CORRESPONDING FIELDS OF TABLE lt_dd27s
      WHERE viewname = iv_view_name
        AND as4local = 'A'
      ORDER BY objpos.

    LOOP AT lt_dd27s ASSIGNING <ls_dd27s>.
      CONCATENATE <ls_dd27s>-tabname '~' <ls_dd27s>-fieldname INTO lv_table_and_field.
      CONCATENATE rv_select_sql_part lv_table_and_field 'AS' <ls_dd27s>-viewfield
        INTO rv_select_sql_part SEPARATED BY space.
    ENDLOOP.

    CONDENSE rv_select_sql_part.
  ENDMETHOD.


  METHOD _GET_VIEW_SQL.

    DATA: lv_select TYPE string,
          lv_from   TYPE string,
          lv_where  TYPE string.

    lv_select = _get_select_sql_part( iv_view_name ).
    lv_from   = _get_from_sql_part( iv_view_name ).
    lv_where  = _get_where_sql_part( iv_view_name ).

    CONCATENATE 'SELECT' lv_select 'FROM' lv_from 'WHERE' lv_where INTO rv_view_sql SEPARATED BY space.
  ENDMETHOD.


  METHOD _GET_WHERE_SQL_PART.

    DATA: lt_dd28s           TYPE TABLE OF dd28s,
          lv_table_and_field TYPE string,
          lv_sql_operator    TYPE string,
          lv_bracket_opened  TYPE abap_bool.

    FIELD-SYMBOLS: <ls_dd28s> LIKE LINE OF lt_dd28s.

    SELECT *
      FROM dd28s
      INTO CORRESPONDING FIELDS OF TABLE lt_dd28s
      WHERE condname = iv_view_name
        AND as4local = 'A'
        AND as4vers  = '0000'
        AND negation = space
      ORDER BY position.

    LOOP AT lt_dd28s ASSIGNING <ls_dd28s>.
      CONCATENATE <ls_dd28s>-tabname <ls_dd28s>-fieldname INTO lv_table_and_field SEPARATED BY '~'.
      lv_sql_operator = _convert_view_operator_to_sql( <ls_dd28s>-operator ).

      IF <ls_dd28s>-and_or = 'OR'.
        CONCATENATE rv_where_sql_part '(' lv_table_and_field lv_sql_operator <ls_dd28s>-constants <ls_dd28s>-and_or
          INTO rv_where_sql_part SEPARATED BY space.
        lv_bracket_opened = abap_true.
      ELSE.

        CONCATENATE rv_where_sql_part lv_table_and_field lv_sql_operator <ls_dd28s>-constants
          INTO rv_where_sql_part SEPARATED BY space.

        IF lv_bracket_opened = abap_true.
          CONCATENATE rv_where_sql_part ')' INTO rv_where_sql_part SEPARATED BY space.
          lv_bracket_opened = abap_false.
        ENDIF.

        CONCATENATE rv_where_sql_part <ls_dd28s>-and_or INTO rv_where_sql_part SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    IF lv_bracket_opened = abap_true.
      CONCATENATE rv_where_sql_part ')' INTO rv_where_sql_part SEPARATED BY space.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
