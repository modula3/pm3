(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Dec  7 13:18:17 PST 1994 by kalsow     *)

INTERFACE WebFile;

IMPORT M3Driver;

PROCEDURE Update (file, info: TEXT);

PROCEDURE Dump () RAISES {M3Driver.Error};

END WebFile.
