(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Fri Jan 28 10:55:29 PST 1994 by detlefs   *)

GENERIC MODULE CITextTable(Tbl);

IMPORT CIText;

REVEAL
  Default = Tbl.Default BRANDED OBJECT
  OVERRIDES
    keyEqual := CIEqual;
    keyHash := CIHash;
  END;

PROCEDURE CIEqual(<*UNUSED*> self: T; READONLY a, b: TEXT): BOOLEAN=
  BEGIN
    RETURN CIText.Equal(a, b);
  END CIEqual;

PROCEDURE CIHash(<*UNUSED*> self: T; READONLY a: TEXT): INTEGER=
  BEGIN
    RETURN CIText.Hash(a);
  END CIHash;

BEGIN
END CITextTable.
