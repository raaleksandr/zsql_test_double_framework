class ZCL_ZOSQL_ITERATOR_BASE definition
  public
  create public .

public section.

  interfaces ZIF_ZOSQL_ITERATOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZOSQL_ITERATOR_BASE IMPLEMENTATION.


  method ZIF_ZOSQL_ITERATOR~GET_COMPONENTS_OF_DATA_SET.
    DATA: ld_ref_to_dataset_data TYPE REF TO data,
          lo_struct              TYPE REF TO cl_abap_structdescr.

    ld_ref_to_dataset_data = zif_zosql_iterator~get_line_for_data_set_ref( iv_dataset_name_or_alias ).
    lo_struct ?= cl_abap_structdescr=>describe_by_data_ref( ld_ref_to_dataset_data ).
    rt_data_set_components = lo_struct->get_included_view( ).
  endmethod.
ENDCLASS.
