class ZCL_ZOSQL_TABLE_ITERATOR definition
  public
  inheriting from ZCL_ZOSQL_ITERATOR_BASE
  create public .

public section.

  data CURRENT_RECORD type I read-only .

  methods CONSTRUCTOR
    importing
      !IO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT
      !IV_TABLE_NAME type CLIKE .

  methods ZIF_ZOSQL_ITERATOR~ADD_ADDITIONAL_DATASET
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~CHECK_DATA_SET_EXISTS
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~CLONE
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~CREATE_EMPTY_RECORD_AS_REF
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~GET_CURRENT_RECORD_REF
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~GET_CURRENT_RECORD_UNIQUE_ID
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~GET_DATA_SET_LIST
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~GET_ITERATOR_POSITION_OBJECT
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~GET_KEY_FIELDS_OF_DATA_SET
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~GET_LINE_FOR_DATA_SET_REF
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~IS_EMPTY
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~MOVE_TO_FIRST
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~MOVE_TO_NEXT
    redefinition .
  methods ZIF_ZOSQL_ITERATOR~RECORD_IS_LAST
    redefinition .
protected section.
private section.

  data MR_TABLE_DATA_REF type ref to DATA .
  data MV_NUM_LINES type I .
  data MO_ZOSQL_TEST_ENVIRONMENT type ref to ZIF_ZOSQL_TEST_ENVIRONMENT .
  data MV_TABLE_NAME type TABNAME .

  methods _COUNT_NUM_LINES .
ENDCLASS.



CLASS ZCL_ZOSQL_TABLE_ITERATOR IMPLEMENTATION.


  method CONSTRUCTOR.

    super->constructor( ).

    mo_zosql_test_environment = io_zosql_test_environment.
    mv_table_name             = zcl_zosql_utils=>to_upper_case( iv_table_name ).

    mr_table_data_ref = mo_zosql_test_environment->get_data_of_table_as_ref( mv_table_name ).
    _count_num_lines( ).
  endmethod.


  method ZIF_ZOSQL_ITERATOR~ADD_ADDITIONAL_DATASET.
**TRY.
*CALL METHOD SUPER->ZIF_ZOSQL_ITERATOR~ADD_ADDITIONAL_DATASET
*  EXPORTING
*    IV_DATASET_NAME  =
**    iv_dataset_alias =
*    .
** CATCH zcx_zosql_error .
**ENDTRY.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~CHECK_DATA_SET_EXISTS.
**TRY.
*CALL METHOD SUPER->ZIF_ZOSQL_ITERATOR~CHECK_DATA_SET_EXISTS
*  EXPORTING
*    IV_DATASET_NAME_OR_ALIAS =
*  RECEIVING
*    RV_EXISTS                =
*    .
** CATCH zcx_zosql_error .
**ENDTRY.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~CLONE.
    DATA: lo_clone_table_iterator TYPE REF TO zcl_zosql_table_iterator.

    FIELD-SYMBOLS: <lt_table_this>  TYPE STANDARD TABLE,
                   <lt_table_clone> TYPE STANDARD TABLE.

    CREATE OBJECT lo_clone_table_iterator
      EXPORTING
        io_zosql_test_environment = mo_zosql_test_environment
        iv_table_name             = mv_table_name.

    ASSIGN mr_table_data_ref->* TO <lt_table_this>.
    ASSIGN lo_clone_table_iterator->mr_table_data_ref->* TO <lt_table_clone>.
    <lt_table_clone> = <lt_table_this>.

    ro_copy_of_object = lo_clone_table_iterator.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~CREATE_EMPTY_RECORD_AS_REF.
    FIELD-SYMBOLS: <lt_table_data>   TYPE INDEX TABLE.

    ASSIGN mr_table_data_ref->* TO <lt_table_data>.
    IF <lt_table_data> IS ASSIGNED.
      CREATE DATA rd_ref_to_empty_record LIKE LINE OF <lt_table_data>.
    ELSE.
      MESSAGE e074 WITH mv_table_name INTO zcl_zosql_utils=>dummy.
      zcl_zosql_utils=>raise_exception_from_sy_msg( ).
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~GET_CURRENT_RECORD_REF.
    FIELD-SYMBOLS: <lt_table_data>   TYPE INDEX TABLE,
                   <ls_current_line> TYPE any,
                   <ls_copy_of_line> TYPE any.

    IF current_record <= mv_num_lines.
      ASSIGN mr_table_data_ref->* TO <lt_table_data>.
      rd_ref_to_data_of_current_rec = zif_zosql_iterator~create_empty_record_as_ref( ).
      READ TABLE <lt_table_data> INDEX current_record ASSIGNING <ls_current_line>.
      IF sy-subrc = 0.
        ASSIGN rd_ref_to_data_of_current_rec->* TO <ls_copy_of_line>.
        <ls_copy_of_line> = <ls_current_line>.
      ENDIF.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~GET_CURRENT_RECORD_UNIQUE_ID.
    rv_unique_id = |{ current_record }|.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~GET_DATA_SET_LIST.

    FIELD-SYMBOLS: <ls_data_set> LIKE LINE OF rt_data_set_list.

    APPEND INITIAL LINE TO rt_data_set_list ASSIGNING <ls_data_set>.
    <ls_data_set>-dataset_name = mv_table_name.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~GET_ITERATOR_POSITION_OBJECT.
    CREATE OBJECT ro_iterator_pos.
    ro_iterator_pos->add_data_set_data( iv_dataset_name        = mv_table_name
                                        id_ref_to_current_line = zif_zosql_iterator~get_current_record_ref( ) ).
  endmethod.


  method ZIF_ZOSQL_ITERATOR~GET_KEY_FIELDS_OF_DATA_SET.
*CALL METHOD SUPER->ZIF_ZOSQL_ITERATOR~GET_KEY_FIELDS_OF_DATA_SET
*  EXPORTING
*    IV_DATASET_NAME_OR_ALIAS =
*  RECEIVING
*    RT_KEY_FIELDS            =
*    .
  endmethod.


  method ZIF_ZOSQL_ITERATOR~GET_LINE_FOR_DATA_SET_REF.
    rd_ref_to_line = zif_zosql_iterator~create_empty_record_as_ref( ).
  endmethod.


  method ZIF_ZOSQL_ITERATOR~IS_EMPTY.
    IF mv_num_lines = 0.
      rv_is_empty = abap_true.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~MOVE_TO_FIRST.
    IF mv_num_lines > 0.
      current_record = 1.
      rv_successful_move = abap_true.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~MOVE_TO_NEXT.
    IF current_record < mv_num_lines.
      current_record = current_record + 1.
      rv_successful_move = abap_true.
    ENDIF.
  endmethod.


  method ZIF_ZOSQL_ITERATOR~RECORD_IS_LAST.
    IF current_record = mv_num_lines.
      rv_is_last = abap_true.
    ENDIF.
  endmethod.


  method _COUNT_NUM_LINES.

    FIELD-SYMBOLS: <lt_table_data> TYPE ANY TABLE.

    mv_num_lines = 0.

    IF mr_table_data_ref IS BOUND.
      ASSIGN mr_table_data_ref->* TO <lt_table_data>.
      IF sy-subrc = 0.
        mv_num_lines = LINES( <lt_table_data> ).
      ENDIF.
    ENDIF.
  endmethod.
ENDCLASS.
