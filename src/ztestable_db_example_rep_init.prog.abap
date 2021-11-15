*&---------------------------------------------------------------------*
*& Report ZTESTABLE_DB_EXAMPLE_REP_INIT
*&---------------------------------------------------------------------*
*& Example program with regular report with some SQL queries
*&---------------------------------------------------------------------*
REPORT ztestable_db_example_rep_init.

CLASS lcl_model DEFINITION DEFERRED.

TABLES: sbook.

SELECT-OPTIONS: s_custid FOR sbook-customid.

DATA: lo_model TYPE REF TO lcl_model.

CLASS lcl_model DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_data_line,
             customid                     TYPE scustom-id,
             customer_name                TYPE scustom-name,
             count_tickets_bought         TYPE zdblayr_example_tickets_bought,
             date_of_first_flight         TYPE zdblayr_date_of_first_flight,
             name_of_first_carrier        TYPE zdblayr_example_first_carrier,
             name_of_favourite_carrier    TYPE zdblayr_example_favor_carrier,
             num_flight_favourite_carrier TYPE zdblayr_example_num_flight_fav,
             is_smoker                    TYPE sbook-smoker,
             which_class                  TYPE sbook-class,
           END OF ty_data_line.

    TYPES: ty_data TYPE STANDARD TABLE OF ty_data_line WITH KEY customid.

    METHODS: read_data IMPORTING it_customid_range TYPE ztestable_db_example_cust_rng,
      get_data RETURNING VALUE(rt_data) TYPE ty_data,
      check_test_tables_filled RETURNING VALUE(rv_filled) TYPE abap_bool.

  PRIVATE SECTION.
    DATA: mt_data TYPE ty_data.
ENDCLASS.

CLASS lcl_model IMPLEMENTATION.
  METHOD read_data.

    TYPES: BEGIN OF ty_custom_first_carrier,
             customid TYPE scustom-id,
             carrname TYPE scarr-carrname,
           END OF ty_custom_first_carrier,
           BEGIN OF ty_custom_flight_by_carr,
             customid       TYPE scustom-id,
             carrid         TYPE scarr-carrid,
             carrname       TYPE scarr-carrname,
             num_of_flights TYPE i,
           END OF ty_custom_flight_by_carr,
           BEGIN OF ty_custom_class,
             customid  TYPE scustom-id,
             min_class TYPE sbook-class,
             max_class TYPE sbook-class,
           END OF ty_custom_class.

    DATA: lt_custom_first_carrier  TYPE SORTED TABLE OF ty_custom_first_carrier
                                       WITH NON-UNIQUE KEY customid,
          lt_custom_flight_by_carr TYPE TABLE OF ty_custom_flight_by_carr,
          lt_custom_class          TYPE HASHED TABLE OF ty_custom_class
                                        WITH UNIQUE KEY customid.

    FIELD-SYMBOLS: <ls_data>                  LIKE LINE OF mt_data,
                   <ls_custom_first_carrier>  LIKE LINE OF lt_custom_first_carrier,
                   <ls_custom_fligth_by_carr> LIKE LINE OF lt_custom_flight_by_carr,
                   <ls_custom_class>          LIKE LINE OF lt_custom_class.

    REFRESH mt_data.

    SELECT sbook~customid AS customid
           MIN( scustom~name ) AS customer_name
           COUNT( * ) AS count_tickets_bought
           MIN( sflight~fldate ) AS date_of_first_flight
           MAX( sbook~smoker ) AS is_smoker
      FROM sbook
      JOIN scustom ON scustom~id = sbook~customid
      JOIN sflight ON sflight~carrid = sbook~carrid
                  AND sflight~connid = sbook~connid
                  AND sflight~fldate = sbook~fldate
      INTO CORRESPONDING FIELDS OF TABLE mt_data
      WHERE sbook~customid IN it_customid_range
      GROUP BY customid
      ORDER BY customid.

    IF mt_data IS NOT INITIAL.
      SELECT sbook~customid scarr~carrname
        FROM sflight
        JOIN sbook ON sbook~carrid = sflight~carrid
                  AND sbook~connid = sflight~connid
                  AND sbook~fldate = sflight~fldate
        JOIN scarr ON scarr~carrid = sbook~carrid
        INTO CORRESPONDING FIELDS OF TABLE lt_custom_first_carrier
        FOR ALL ENTRIES IN mt_data
        WHERE sbook~customid = mt_data-customid
          AND sbook~fldate   = mt_data-date_of_first_flight.

      SELECT sbook~customid
             sbook~carrid
             MIN( scarr~carrname ) AS carrname
             COUNT( * ) AS num_of_flights
        FROM sbook
        JOIN scarr ON scarr~carrid = sbook~carrid
        INTO CORRESPONDING FIELDS OF TABLE lt_custom_flight_by_carr
        WHERE sbook~customid IN it_customid_range
        GROUP BY sbook~customid sbook~carrid.

      SELECT sbook~customid
             MIN( sbook~class ) AS min_class
             MAX( sbook~class ) AS max_class
        FROM sbook
        INTO CORRESPONDING FIELDS OF TABLE lt_custom_class
        WHERE sbook~customid IN it_customid_range
        GROUP BY customid.
    ENDIF.

    SORT lt_custom_flight_by_carr BY customid num_of_flights DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_custom_flight_by_carr COMPARING customid.

    LOOP AT mt_data ASSIGNING <ls_data>.

      READ TABLE lt_custom_first_carrier WITH TABLE KEY customid = <ls_data>-customid
        ASSIGNING <ls_custom_first_carrier>.
      IF sy-subrc = 0.
        <ls_data>-name_of_first_carrier = <ls_custom_first_carrier>-carrname.
      ENDIF.

      READ TABLE lt_custom_flight_by_carr WITH KEY customid = <ls_data>-customid
        ASSIGNING <ls_custom_fligth_by_carr> BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_data>-name_of_favourite_carrier    = <ls_custom_fligth_by_carr>-carrname.
        <ls_data>-num_flight_favourite_carrier = <ls_custom_fligth_by_carr>-num_of_flights.
      ENDIF.

      READ TABLE lt_custom_class WITH TABLE KEY customid = <ls_data>-customid
        ASSIGNING <ls_custom_class>.
      IF sy-subrc = 0.
        IF <ls_custom_class>-min_class = <ls_custom_class>-max_class.
          <ls_data>-which_class = <ls_custom_class>-min_class.
        ELSE.
          <ls_data>-which_class = 'X'.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_data.
    rt_data = mt_data.
  ENDMETHOD.

  METHOD check_test_tables_filled.

    SELECT SINGLE sbook~connid
      INTO zcl_testable_db_layer_utils=>dummy
      FROM sbook.

    IF sy-subrc = 0.
      rv_filled = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  CREATE OBJECT lo_model.
  lo_model->read_data( it_customid_range = s_custid[] ).

END-OF-SELECTION.
  IF lo_model->check_test_tables_filled( ) = abap_true.
    PERFORM display_alv.
  ELSE.
    MESSAGE 'Test data empty, please run SE38->SAPBC_DATA_GENERATOR to generate' TYPE 'I'.
  ENDIF.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
*& Displays data as alv
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_alv .
  DATA: lt_alv       TYPE lcl_model=>ty_data,
        lo_alv       TYPE REF TO cl_salv_table,
        lo_exception TYPE REF TO cx_root.

  lt_alv = lo_model->get_data( ).

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                              CHANGING  t_table      = lt_alv ).
      lo_alv->display( ).
    CATCH cx_root INTO lo_exception.
      zcl_testable_db_layer_utils=>dummy = lo_exception->get_text( ).
      MESSAGE zcl_testable_db_layer_utils=>dummy TYPE 'I'.
  ENDTRY.
ENDFORM.
