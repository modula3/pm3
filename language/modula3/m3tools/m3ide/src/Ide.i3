(*  This file is part of m3ide, a simple development environment for M3    *)
(*  Copyright (C) 1995 Michel Dagenais                                     *)
(*                                                                         *)
(*  This program is free software; you can redistribute it and/or modify   *)
(*  it under the terms of the GNU General Public License as published by   *)
(*  the Free Software Foundation; either version 2 of the License, or      *)
(*  (at your option) any later version.                                    *)
(*                                                                         *)
(*  This program is distributed in the hope that it will be useful,        *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *)
(*  GNU General Public License for more details.                           *)
(*                                                                         *)
(*  You should have received a copy of the GNU General Public License      *)
(*  along with this program; if not, write to the Free Software            *)
(*  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.              *)
(*                                                                         *)
(*  For more information on this program, contact Michel Dagenais at       *)
(*  dagenais@vlsi.polymtl.ca or Electrical Eng. Dept., Ecole Polytechnique *)
(*  P.O. Box 6079, Station A, Montreal, Quebec, Canada, H3C 3A7.           *)

INTERFACE Ide;

PROCEDURE ShowForm(fileName: TEXT; form: TEXT): TEXT;
(* Show a FormsVBT form in a Trestle window *)

PROCEDURE PPrint(program: TEXT; VAR error: TEXT): TEXT;
(* Pass the program text through the m3pp pretty printer *)

PROCEDURE PackagePrefix(fileName: TEXT): TEXT;
(* Find the package path (up to but excluding src) *)

PROCEDURE PackageSuffix(fileName: TEXT): TEXT;
(* Find the package path from src *)

PROCEDURE PackageM3makefile(fileName: TEXT): TEXT;
(* Find the top level m3makefile for a package, given a file in the package *)

PROCEDURE PackageProgram(fileName: TEXT; buildSuffix: TEXT): TEXT;
(* Find the executable produced by a package *)

PROCEDURE BuildDir(buildSuffix: TEXT): TEXT;
(* Find the build directory name for the package *)

PROCEDURE BuildPath(fileName: TEXT; buildSuffix: TEXT): TEXT;
(* Find the complete path to the build directory of a package *)

PROCEDURE SourcePaths(fileName, buildSuffix: TEXT): REF ARRAY OF TEXT;
(* Return all the paths where source files for the program, and imported
   libraries, are located. *)

END Ide.
