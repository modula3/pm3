(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: BuiltinTypes.m3                                       *)
(* Last Modified On Mon Mar  1 17:24:04 PST 1993 By kalsow     *)
(*      Modified On Fri Aug  3 01:38:59 1990 By muller         *)

MODULE BuiltinTypes;

IMPORT Int, Card, Bool, Reel, LReel, EReel, CChar, Addr;
IMPORT Null, Reff, Textt, Mutex, ErrType, ObjectRef, ObjectAdr;


PROCEDURE Initialize () =
  BEGIN
    (* builtin types *)
    (* NOTE: this list is ordered! *)
    ErrType.Initialize ();
    Int.Initialize ();
    Card.Initialize ();
    Bool.Initialize ();
    Reel.Initialize ();
    LReel.Initialize ();
    EReel.Initialize ();
    CChar.Initialize ();
    Null.Initialize ();
    Addr.Initialize ();
    Reff.Initialize ();
    ObjectRef.Initialize ();
    ObjectAdr.Initialize ();
    Textt.Initialize ();
    Mutex.Initialize ();
  END Initialize;

BEGIN
END BuiltinTypes.
