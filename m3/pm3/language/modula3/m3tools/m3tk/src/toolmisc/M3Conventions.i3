(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

INTERFACE M3Conventions;

IMPORT M3CUnit, M3Time;

(* Conventions used by M3CGoList *)

CONST
  Standard = "M3__Base__";
  VersionSep = '-';  (* separates module name from version tag *)
  PrimarySource = M3CUnit.State.User1;


PROCEDURE IsStandard(name: TEXT): BOOLEAN RAISES {};
(* TRUE iff 'name' = 'Standard' *)

PROCEDURE ModuleName(name: TEXT): TEXT RAISES {};
(* Removes any version tag on 'name'. *)

(* Support for timing. Create via NEW(CompTime).init() *)

TYPE
  CompTime <: CompTime_public;
  CompTime_public = OBJECT
    open, parse, semantic: M3Time.T := NIL;
  METHODS
    init(): CompTime;
  END;

END M3Conventions.

