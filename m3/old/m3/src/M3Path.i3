(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jul 18 13:11:25 PDT 1994 by kalsow     *)

INTERFACE M3Path;

TYPE
  T = RECORD
    dir  : TEXT;
    base : TEXT;
    kind : Kind;
  END;

TYPE
  Kind = { I3, IC, IS, IO, M3, MC, MS, MO,
           IG, MG, C, H, S, O, A, AX, PX, Unknown };

TYPE
  OSKind = { Unix, GrumpyUnix, Win32 };

PROCEDURE SetOS (os: OSKind;  host: BOOLEAN);
(* Set the conventions for the specifed platform *)

PROCEDURE Join (dir, base: TEXT;  k: Kind;  host: BOOLEAN): TEXT;
(* Build and return the full path name. *)

PROCEDURE Parse (nm: TEXT;  host: BOOLEAN): T;
(* Parse 'nm' into its pieces using the specified platform's conventions. *)

PROCEDURE DefaultProgram (host: BOOLEAN): TEXT;
(* Return the default program name for the specified platform. *)

PROCEDURE Convert (nm: TEXT;  host: BOOLEAN): TEXT;
(* Convert the slashes in 'nm' to match the specified platform. *)

PROCEDURE Escape (nm: TEXT): TEXT;
(* Return 'nm' with and embedded backslashes doubled. *)

END M3Path.
