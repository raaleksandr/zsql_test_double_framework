class ZCX_ZOSQL_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  data T100_MESSAGE type SYMSG .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !T100_MESSAGE type SYMSG optional .

  methods IF_MESSAGE~GET_LONGTEXT
    redefinition .
  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ZOSQL_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->T100_MESSAGE = T100_MESSAGE .
  endmethod.


  method IF_MESSAGE~GET_LONGTEXT.
    result = get_text( ).
  endmethod.


  METHOD IF_MESSAGE~GET_TEXT.
    IF t100_message-msgid IS NOT INITIAL.

      IF t100_message-msgty NA 'SIWEAX'.
        t100_message-msgty = 'E'.
      ENDIF.

      MESSAGE ID t100_message-msgid TYPE t100_message-msgty NUMBER t100_message-msgno
        WITH t100_message-msgv1 t100_message-msgv2 t100_message-msgv3 t100_message-msgv4
        INTO result.
    ELSE.
      MESSAGE e081(zosql_db_layer) INTO result.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
