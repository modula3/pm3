INTERFACE AttributeValue;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.2  1997/10/31 14:23:14  roland
    Prettyprinting.

    Revision 1.1  1997/05/01 13:23:50  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:34:55  roland
    AttributeValues ease typed access to attributes.

*)
(***************************************************************************)

(* An AttributeValue.T stores the value of one typed attribute of a node.
   It has methods to convert its value into a text form that can be written
   as raw attribute in ChgMgmtGraph.  The text written has to contain all
   information to reconstruct the AttributeValue from it alone, i.e.  it
   should at least contain a type code and a length information.  The
   typecodes for the standard modula-3 types are predefined. *)

CONST
  DummyTypeCode    = 0;
  BooleanTypeCode  = 1;
  IntegerTypeCode  = 2;
  CardinalTypeCode = 3;
  RealTypeCode     = 4;
  CharTypeCode     = 5;
  TextTypeCode     = 6;

  ReservedTypeCodes = 100;
  (* ReservedTypeCodes is the number up to which GRAS might use type
     encodings in future releases.  Applications should start their type
     codes with ReservedTypeCodes + 1. *)

TYPE
  T = OBJECT
        defined: BOOLEAN := FALSE; (* must be set to TRUE by methods
                                      fromText and set*)
      METHODS
        type     (): CARDINAL                  := NIL;
        toText   (VAR len: CARDINAL): TEXT     := NIL;
        fromText (info: TEXT) RAISES {Invalid} := NIL;

        (* To access the value stored, each specialization should have
           methods set and get with appropriate formal parameters *)
      END;

(* Useful procedures for conversion *)

PROCEDURE IntToText (val: INTEGER): TEXT;
  (* Delivers a 4 byte text that encodes val.  The first character of the
     resulting text holds the least significant byte of val, the fourth
     character holds the most significant byte of val. *)

PROCEDURE TextToInt (t: TEXT): INTEGER;
  (* The first 4 bytes (if any) of t are taken as the lower bytes of the
     resulting integer number.  If t has more than 4 bytes, the rest is
     ignored. *)

PROCEDURE CardToText (val: CARDINAL): TEXT;
  (* Delivers a 4 byte text that encodes val.  The first character of the
     resulting text holds the least significant byte of val, the fourth
     character holds the most significant byte of val. *)

PROCEDURE TextToCard (t: TEXT): CARDINAL;
  (* The first 4 bytes (if any) of t are taken as the lower bytes of the
     resulting cardinal number.  If t has more than 4 bytes, the rest is
     ignored (as well as the most significant bit of the fourth character
     to avoid runtime errors). *)

PROCEDURE StandardEncoding (type: CARDINAL; val: TEXT; VAR len: CARDINAL):
  TEXT;
  (* Generates a text that encodes type and val as well as length of val
     and that can be decoded by StandardDecoding.  len contains the length
     of the encoding.  All predefined types use this encoding format. *)

PROCEDURE StandardDecoding (    code     : TEXT;
                            VAR type, len: CARDINAL;
                            VAR val      : TEXT      ) RAISES {Invalid};
  (* Decodes a text that has standard encoding format.  len gives the
     length of val.  If the text cannot be decoded, Invalid is raised. *)

EXCEPTION Invalid;               (* A text cannot be converted *)


END AttributeValue.
