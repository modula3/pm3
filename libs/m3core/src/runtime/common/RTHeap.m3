(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*| Last modified on Wed Jun  9 09:35:13 PDT 1993 by kalsow  *)
(*|      modified on Wed Jun  2 15:00:17 PDT 1993 by muller  *)
(*|      modified on Wed Apr 21 13:14:37 PDT 1993 by mcjones *)
(*|      modified on Wed Mar 10 11:01:47 PST 1993 by mjordan *)
(*|      modified on Tue Mar  9 08:45:18 PST 1993 by jdd     *)

UNSAFE MODULE RTHeap;

IMPORT RTType, RT0, TextLiteral;

TYPE TK = RT0.TypeKind;

PROCEDURE GetDataAdr (r: REFANY): ADDRESS =
  VAR def := RTType.Get(TYPECODE(r));
  BEGIN
    IF r = NIL THEN
      EVAL LOOPHOLE (r, UNTRACED REF INTEGER)^;  (* force a NIL fault *)
    END;
    CASE def.kind OF  <*NOWARN*>
    | ORD(TK.Ref) =>
          RETURN LOOPHOLE(r, ADDRESS);
    | ORD(TK.Obj) =>
          RETURN LOOPHOLE(r, ADDRESS) + ADRSIZE(ADDRESS);
    | ORD(TK.Array) =>
          RETURN LOOPHOLE(r, UNTRACED REF ADDRESS)^;
    END;
  END GetDataAdr;

PROCEDURE GetDataSize (r: REFANY): CARDINAL =
  VAR def := RTType.Get(TYPECODE(r));
  BEGIN
    IF r = NIL THEN
      EVAL LOOPHOLE (r, UNTRACED REF INTEGER)^;  (* force a NIL fault *)
    END;
    CASE def.kind OF <*NOWARN*>
    | ORD(TK.Ref) =>
          RETURN def.dataSize;
    | ORD(TK.Obj) =>
          IF def.typecode = RT0.TextLitTypecode THEN
            VAR
              txt := LOOPHOLE (r, TextLiteral.T);
              len : INTEGER := txt.cnt;
            BEGIN
              IF (len >= 0)
                THEN INC (len); (* null CHAR *)
                ELSE len := 2 (*null WIDECHAR*) - len - len;
              END;
              RETURN ADR (txt.buf[len]) - ADR (txt.cnt)
            END;
          ELSE
            RETURN def.dataSize - BYTESIZE(ADDRESS);
          END;
    | ORD(TK.Array) =>
          VAR
            adef := LOOPHOLE (def, RT0.ArrayTypeDefn);
            n_elts: INTEGER := 1;
            sizes: UNTRACED REF INTEGER := LOOPHOLE(r, ADDRESS) + ADRSIZE(ADDRESS);
          BEGIN
            FOR i := 0 TO adef.nDimensions - 1 DO
              n_elts := n_elts * sizes^;
              INC(sizes, ADRSIZE(INTEGER));
            END;
            RETURN n_elts * adef.elementSize;
          END;
    END;
  END GetDataSize;

PROCEDURE GetArrayShape (r: REFANY; VAR s: ARRAY OF INTEGER) =
  VAR
    def := LOOPHOLE (RTType.Get(TYPECODE(r)), RT0.ArrayTypeDefn);
    sizes: UNTRACED REF INTEGER := LOOPHOLE(r, ADDRESS) + ADRSIZE(ADDRESS);
  BEGIN
    IF (def.common.kind = ORD (TK.Array)) THEN
      FOR i := 0 TO MIN(NUMBER(s), def.nDimensions) - 1 DO
        s[i] := sizes^;
        INC(sizes, ADRSIZE(sizes^));
      END;
    END;
  END GetArrayShape;

BEGIN
END RTHeap.
