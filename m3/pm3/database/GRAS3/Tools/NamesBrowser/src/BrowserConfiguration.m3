MODULE BrowserConfiguration;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/06/06 14:24:38  roland
    Some runtime configuration support added. A file-name can be given as
    command line parameter with '-config'. From this file the collection
    name and representation types for names attributes will be read.

*)
(***************************************************************************)

IMPORT Rd, Wr, Fmt, Text, Thread, Lex, ASCII;
IMPORT AttributeValue;

TYPE
  ADesc = REF AttributeDescription;
  AttributeDescription = RECORD
                           name  : TEXT;
                           format: AttributeFormat;
                           next  : ADesc;
                         END;

VAR
  collectionName: TEXT  := NIL;
  attributes    : ADesc := NIL;

  collKey := "Collection";
  formatKey := ARRAY AttributeFormat OF
                 TEXT{"NonReadable", "Readable", "Integer", "Cardinal"};

PROCEDURE ReadConfiguration (rd: Rd.T) =
  VAR
    desc: ADesc;
    sym : TEXT;
  BEGIN
    TRY
      WHILE NOT Rd.EOF(rd) DO
        Lex.Skip(rd);
        sym := Lex.Scan(rd);
        Lex.Skip(rd);
        IF Text.Equal(UpperCase(sym), UpperCase(collKey)) THEN
          collectionName := Lex.Scan(rd);
        ELSE
          FOR i := FIRST(AttributeFormat) TO LAST(AttributeFormat) DO
            IF Text.Equal(UpperCase(formatKey[i]), UpperCase(sym)) THEN
              sym := Lex.Scan(rd);
              desc := GetADesc(sym);
              desc.format := i;
            END;
          END;
        END;
      END;
    EXCEPT
      Thread.Alerted, Rd.Failure =>
    END;
  END ReadConfiguration;

PROCEDURE WriteConfiguration (wr: Wr.T) =
  VAR desc: ADesc;
  BEGIN
    TRY
      IF collectionName # NIL THEN
        Wr.PutText(wr, collKey & " " & collectionName & "\n");
      END;
      desc := attributes;
      WHILE desc # NIL DO
        Wr.PutText(wr, formatKey[desc.format] & " " & desc^.name & "\n");
      END;
    EXCEPT
      Thread.Alerted, Wr.Failure =>
    END;
  END WriteConfiguration;

PROCEDURE GetCollectionName (): TEXT =
  BEGIN
    IF collectionName # NIL THEN
      RETURN collectionName;
    ELSE
      RETURN "";
    END;
  END GetCollectionName;

PROCEDURE SetCollectionName (name: TEXT) =
  BEGIN
    collectionName := name;
  END SetCollectionName;

PROCEDURE SetAttributeFormat (attr: TEXT; format: AttributeFormat) =
  VAR desc: ADesc;
  BEGIN
    desc := GetADesc(attr);
    desc.format := format;
  END SetAttributeFormat;

PROCEDURE GetAttributeFormat (attr: TEXT): AttributeFormat =
  VAR desc: ADesc;
  BEGIN
    desc := GetADesc(attr);
    RETURN desc.format;
  END GetAttributeFormat;

PROCEDURE FormatAttribute (attr, val: TEXT): TEXT =
  VAR desc: ADesc;
  BEGIN
    desc := GetADesc(attr);
    CASE desc.format OF
      AttributeFormat.NonReadableText => RETURN GraphicRep(val);
    | AttributeFormat.ReadableText => RETURN val;
    | AttributeFormat.Integer =>
        RETURN Fmt.Int(AttributeValue.TextToInt(val));
    | AttributeFormat.Cardinal =>
        RETURN Fmt.Int(AttributeValue.TextToCard(val));
    END;
  END FormatAttribute;

PROCEDURE GetADesc (attr: TEXT): ADesc =
  VAR h, prev: ADesc;
  BEGIN
    IF attributes = NIL OR Text.Compare(attr, attributes.name) < 0 THEN
      (* empty list or insert before first element *)
      attributes :=
        NEW(ADesc, name := attr, format := AttributeFormat.NonReadableText,
            next := attributes);
      RETURN attributes;
    ELSE
      h := attributes;
      WHILE h # NIL AND Text.Compare(h^.name, attr) < 0 DO
        prev := h;
        h := h^.next;
      END;
      IF h # NIL AND Text.Equal(h^.name, attr) THEN
        (* found matching element *)
        RETURN h;
      ELSE
        (* test if match *)
        prev.next :=
          NEW(ADesc, name := attr,
              format := AttributeFormat.NonReadableText, next := h);
        RETURN prev.next;
      END;
    END;
  END GetADesc;

PROCEDURE GraphicRep (t: TEXT): TEXT =
  VAR
    ok       : BOOLEAN  := TRUE;
    act, prev: CARDINAL := 0;
    len      : CARDINAL := Text.Length(t);
    res      : TEXT     := "";
  BEGIN
    WHILE ok AND act < len DO
      ok := Text.GetChar(t, act) IN ASCII.Graphics;
      INC(act);
    END;
    IF NOT ok THEN
      act := 0;
      WHILE act < len DO
        prev := act;
        WHILE act < len AND Text.GetChar(t, act) IN ASCII.Graphics DO
          INC(act)
        END;
        IF act # prev THEN res := res & Text.Sub(t, prev, act - prev); END;
        WHILE act < len AND NOT Text.GetChar(t, act) IN ASCII.Graphics DO
          res := res & "\\" & Fmt.Pad(Fmt.Int(ORD(Text.GetChar(t, act)),
                                              base := 8), 3, '0');
          INC(act);
        END;
      END;
      RETURN res;
    ELSE
      RETURN t;
    END;
  END GraphicRep;

PROCEDURE UpperCase(t: TEXT): TEXT =
  VAR res: REF ARRAY OF CHAR;
  BEGIN
    IF t # NIL THEN
      res := NEW(REF ARRAY OF CHAR, Text.Length(t));
      FOR i := 0 TO LAST(res^) DO
        res^[i] := ASCII.Upper[Text.GetChar(t, i)];
      END;
    END;
    RETURN Text.FromChars(res^);
  END UpperCase;

BEGIN
END BrowserConfiguration.
