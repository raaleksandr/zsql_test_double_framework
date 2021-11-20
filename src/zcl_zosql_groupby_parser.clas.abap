class ZCL_ZOSQL_GROUPBY_PARSER definition
  public
  create public .

public section.

  data MT_GROUP_BY_FIELDS type FIELDNAME_TABLE read-only .

  methods PARSE_GROUP_BY
    importing
      !IV_GROUP_BY_CLAUSE type CLIKE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZOSQL_GROUPBY_PARSER IMPLEMENTATION.


  method PARSE_GROUP_BY.

    DATA: lt_group_by_fields TYPE TABLE OF string,
          lv_group_by_field  TYPE string.

    FIELD-SYMBOLS: <ls_group_by_field> LIKE LINE OF mt_group_by_fields.

    REFRESH mt_group_by_fields.

    IF iv_group_by_clause IS NOT INITIAL.
      SPLIT zcl_zosql_utils=>to_upper_case( iv_group_by_clause ) AT space INTO TABLE lt_group_by_fields.
    ENDIF.

    LOOP AT lt_group_by_fields INTO lv_group_by_field.
      APPEND INITIAL LINE TO mt_group_by_fields ASSIGNING <ls_group_by_field>.

      CONDENSE lv_group_by_field.
      FIND FIRST OCCURRENCE OF '~' IN lv_group_by_field.
      IF sy-subrc = 0.
        SPLIT lv_group_by_field AT '~' INTO zcl_zosql_utils=>dummy <ls_group_by_field>-fieldname.
      ELSE.
        <ls_group_by_field>-fieldname = lv_group_by_field.
      ENDIF.
    ENDLOOP.
  endmethod.
ENDCLASS.
