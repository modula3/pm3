(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Feb 17 15:30:49 PST 1995 by kalsow     *)
(*      modified on Fri Apr  1 13:49:15 PST 1994 by harrison   *)

INTERFACE QScanner;

IMPORT File, M3ID, QToken;

VAR
  WhiteSpace := ARRAY CHAR OF BOOLEAN { FALSE, .. };

TYPE
  T <: T_; T_ = OBJECT
    token    : QToken.T;  (* current token class *)
    line     : INTEGER;   (* source line number *)
    string   : M3ID.T;    (* token = QToken.T.{Name,String} *)
    cardinal : CARDINAL;  (* token = OToken.T.Cardinal *)
  METHODS
    init (f: File.T): T;
    next ();  (* update the fields above *)
  END;

END QScanner.
