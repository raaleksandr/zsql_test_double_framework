class ZCL_ZOSQL_UTILS_740 definition
  public
  final
  create public .

public section.

  class-methods EXECUTE_SELECT_740
    importing
      !IV_SELECT type STRING default '*'
      !IV_FROM type STRING
      !IV_WHERE type STRING
      !IV_GROUP_BY type STRING
      !IV_HAVING type STRING
      !IV_ORDER_BY type STRING
      value(IV_DISTINCT) type ABAP_BOOL
      !IT_FOR_ALL_ENTRIES_TABLE type ANY TABLE
      !IV_FOR_ALL_ENTRIES type ABAP_BOOL
      !IS_DYNAMIC_STRUCT_WITH_PARAMS type ANY
      value(IV_DO_INTO_CORRESPONDING) type ABAP_BOOL default ABAP_TRUE
      value(IV_NUMBER_OF_ROWS_TO_SELECT) type I optional
    exporting
      !ET_RESULT_TABLE type ANY TABLE
    raising
      ZCX_ZOSQL_ERROR .
  class-methods OPEN_CURSOR_740
    importing
      !IV_SELECT type STRING default '*'
      !IV_FROM type STRING
      !IV_WHERE type STRING
      !IV_GROUP_BY type STRING
      !IV_ORDER_BY type STRING
      value(IV_DISTINCT) type ABAP_BOOL
      !IT_FOR_ALL_ENTRIES_TABLE type ANY TABLE
      !IS_DYNAMIC_STRUCT_WITH_PARAMS type ANY
      value(IV_NUMBER_OF_ROWS_TO_SELECT) type I optional
    returning
      value(RV_CURSOR) type CURSOR
    raising
      ZCX_ZOSQL_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZOSQL_UTILS_740 IMPLEMENTATION.


  METHOD EXECUTE_SELECT_740.

    IF it_for_all_entries_table IS NOT INITIAL OR iv_for_all_entries = abap_true.

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
            INTO CORRESPONDING FIELDS OF TABLE @et_result_table
            UP TO @iv_number_of_rows_to_select ROWS
            FOR ALL ENTRIES IN @it_for_all_entries_table
            WHERE (iv_where).
        ELSE.
          SELECT DISTINCT (iv_select)
            FROM (iv_from)
            INTO TABLE @et_result_table
            UP TO @iv_number_of_rows_to_select ROWS
            FOR ALL ENTRIES IN @it_for_all_entries_table
            WHERE (iv_where).
        ENDIF.
      ELSE.

        IF iv_do_into_corresponding = abap_true.
          SELECT (iv_select)
            FROM (iv_from)
            INTO CORRESPONDING FIELDS OF TABLE @et_result_table
            UP TO @iv_number_of_rows_to_select ROWS
            FOR ALL ENTRIES IN @it_for_all_entries_table
            WHERE (iv_where).
        ELSE.
          SELECT (iv_select)
            FROM (iv_from)
            INTO TABLE @et_result_table
            UP TO @iv_number_of_rows_to_select ROWS
            FOR ALL ENTRIES IN @it_for_all_entries_table
            WHERE (iv_where).
        ENDIF.
      ENDIF.
    ELSE.

      IF iv_distinct = abap_true.

        IF iv_do_into_corresponding = abap_true.
          SELECT DISTINCT (iv_select)
            FROM (iv_from)
            INTO CORRESPONDING FIELDS OF TABLE @et_result_table
            UP TO @iv_number_of_rows_to_select ROWS
            WHERE (iv_where)
            GROUP BY (iv_group_by)
            HAVING (iv_having)
            ORDER BY (iv_order_by).
        ELSE.
          SELECT DISTINCT (iv_select)
            FROM (iv_from)
            INTO TABLE @et_result_table
            UP TO @iv_number_of_rows_to_select ROWS
            WHERE (iv_where)
            GROUP BY (iv_group_by)
            HAVING (iv_having)
            ORDER BY (iv_order_by).
        ENDIF.
      ELSE.

        IF iv_do_into_corresponding = abap_true.
          SELECT (iv_select)
            FROM (iv_from)
            INTO CORRESPONDING FIELDS OF TABLE @et_result_table
            UP TO @iv_number_of_rows_to_select ROWS
            WHERE (iv_where)
            GROUP BY (iv_group_by)
            HAVING (iv_having)
            ORDER BY (iv_order_by).
        ELSE.
          SELECT (iv_select)
            FROM (iv_from)
            INTO TABLE @et_result_table
            UP TO @iv_number_of_rows_to_select ROWS
            WHERE (iv_where)
            GROUP BY (iv_group_by)
            HAVING (iv_having)
            ORDER BY (iv_order_by).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD OPEN_CURSOR_740.

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

        OPEN CURSOR @rv_cursor FOR
        SELECT DISTINCT (iv_select)
          FROM (iv_from)
          UP TO @iv_number_of_rows_to_select ROWS
          FOR ALL ENTRIES IN @it_for_all_entries_table
          WHERE (iv_where).

      ELSE.

        OPEN CURSOR @rv_cursor FOR
        SELECT (iv_select)
          FROM (iv_from)
          UP TO @iv_number_of_rows_to_select ROWS
          FOR ALL ENTRIES IN @it_for_all_entries_table
          WHERE (iv_where).

      ENDIF.
    ELSE.

      IF iv_distinct = abap_true.

        OPEN CURSOR @rv_cursor FOR
        SELECT DISTINCT (iv_select)
          FROM (iv_from)
          UP TO @iv_number_of_rows_to_select ROWS
          WHERE (iv_where)
          GROUP BY (iv_group_by)
          ORDER BY (iv_order_by).

      ELSE.

        OPEN CURSOR @rv_cursor FOR
        SELECT (iv_select)
          FROM (iv_from)
          UP TO @iv_number_of_rows_to_select ROWS
          WHERE (iv_where)
          GROUP BY (iv_group_by)
          ORDER BY (iv_order_by).

      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
