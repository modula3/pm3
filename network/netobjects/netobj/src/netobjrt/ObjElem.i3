(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last modified on Sun Sep 25 18:45:17 PDT 1994 by heydon     *)
(*      modified on Thu May  6 15:42:17 PDT 1993 by wobber     *)
(*      modified on Wed Sep  2 16:55:52 PDT 1992 by evers      *)

INTERFACE ObjElem;

IMPORT NetObj, WeakRef;

CONST Brand = "ObjElem";

TYPE
  T = RECORD 
    ref: NetObj.T := NIL; 
    weakRef := WeakRef.T{ARRAY [0..7] OF BITS 8 FOR [0..255] {0, ..}}; 
    ready := FALSE; (* ready becomes TRUE when weakRef becomes valid *)
  END;

END ObjElem.
