(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Decl.m3                                               *)
(* Last modified on Thu Sep  5 11:55:15 PDT 1996 by heydon     *)
(*      modified on Tue Dec 20 14:54:22 PST 1994 by kalsow     *)
(*      modified on Sat Mar 16 01:56:20 1991 by muller         *)

MODULE Decl;

IMPORT M3, M3ID, M3String, Token, Error, ESet, Module, Exceptionz;
IMPORT Constant, Tipe, Variable, Procedure, Revelation, CG, Target;
FROM Scanner IMPORT GetToken, Match, cur;

TYPE
  TK = Token.T;

PROCEDURE Parse (interface, top_level: BOOLEAN;  VAR fails: M3.ExSet) =
  VAR att: Attributes;
  BEGIN
    att.isInline    := FALSE;
    att.isExternal  := FALSE;
    att.isUnused    := FALSE;
    att.isObsolete  := FALSE;
    att.alias       := M3ID.NoID;
    att.callingConv := NIL;
    LOOP
      CASE cur.token OF
      | TK.tEXTERNAL =>
          IF NOT Module.IsInterface () THEN
            Error.Msg ("External declarations only allowed in interfaces");
          END;
          ParseExternalPragma (att.alias, att.callingConv);
          att.isExternal := TRUE;
      | TK.tINLINE   =>
          att.isInline := TRUE;
          GetToken (); (* INLINE *)
          Match (TK.tENDPRAGMA);
      | TK.tUNUSED   =>
          att.isUnused := TRUE;
          GetToken (); (* UNUSED *)
          Match (TK.tENDPRAGMA);
      | TK.tOBSOLETE =>
          att.isObsolete := TRUE;
          GetToken (); (* OBSOLETE *)
          Match (TK.tENDPRAGMA);
      | TK.tCALLCONV   =>
          att.callingConv := Target.FindConvention (M3ID.ToText (cur.id));
          GetToken (); (* convention name *)
          Match (TK.tENDPRAGMA);
      ELSE EXIT;
      END;
    END;

    CASE cur.token OF
    | TK.tCONST =>
        Constant.ParseDecl (att);
    | TK.tTYPE =>
        Tipe.Parse (att);
    | TK.tVAR =>
        att.isExternal := att.isExternal OR Module.IsExternal ();
        Variable.ParseDecl (att);
    | TK.tPROCEDURE =>
        att.isExternal := att.isExternal OR Module.IsExternal ();
        Procedure.ParseDecl (att, interface);
    | TK.tREVEAL =>
        IF (NOT top_level) THEN Error.Msg ("nested revelation") END;
        Revelation.Parse (att);
    | TK.tEXCEPTION =>
        IF (NOT top_level) THEN Error.Msg ("nested exception declaration") END;
        att.isExternal := att.isExternal OR Module.IsExternal ();
        Exceptionz.ParseDecl (att);
    | TK.tFATAL =>
        fails := ESet.ParseFails (fails);
    ELSE 
        IF att.isInline OR att.isExternal OR att.isUnused
           OR att.isObsolete THEN
          Error.Msg ("declaration pragma not followed by a declaration");
        END;
    END;
  END Parse;

PROCEDURE ParseExternalPragma (VAR alias : M3ID.T;
                               VAR cc    : CG.CallingConvention) =
  BEGIN
    <* ASSERT cur.token = TK.tEXTERNAL *>
    GetToken (); (* EXTERNAL *)

    alias := M3ID.NoID;  (* default => use the Modula-3 name *)
    cc    := Target.DefaultCall;

    IF (cur.token = TK.tIDENT) OR (cur.token = TK.tTEXTCONST) THEN
      IF (cur.token = TK.tIDENT)
        THEN alias := cur.id;
        ELSE alias := M3ID.Add (M3String.ToText (cur.str));
      END;
      GetToken (); (* IDENT, TEXTCONST *)

      IF (cur.token = TK.tCOLON) THEN
        GetToken (); (* : *)
        IF (cur.token = TK.tIDENT) OR (cur.token = TK.tTEXTCONST) THEN
          cc := Target.FindConvention (M3ID.ToText (cur.id));
          IF (cc = NIL) THEN
            Error.ID (cur.id, "unsupported language or calling convention");
            cc := Target.DefaultCall;
          END;
          GetToken (); (* IDENT/TEXTCONST *)
        ELSE
          Error.Msg ("Missing language for <*EXTERNAL*> pragma");
        END;
      END;

    END;

    Match (TK.tENDPRAGMA);
  END ParseExternalPragma;

BEGIN
END Decl.
