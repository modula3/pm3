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

INTERFACE IdeUtil;

IMPORT Rd, Thread, Pathname, OSError, AtomList;

PROCEDURE ListFiles(name: Pathname.T; recursive := FALSE): 
    REF ARRAY OF Pathname.T RAISES{OSError.E};
(* List all files in a directory *)

PROCEDURE ListRegularFiles(name: Pathname.T; recursive:= FALSE): 
    REF ARRAY OF Pathname.T RAISES{OSError.E};
(* List only regular files *)

PROCEDURE ListDirectories(name: Pathname.T; recursive := FALSE): 
    REF ARRAY OF Pathname.T RAISES{OSError.E};
(* List only subdirectories *)

PROCEDURE RunFilter(cmd: Pathname.T; input: TEXT; VAR output, error: TEXT;
    params: REF ARRAY OF TEXT := NIL; env: REF ARRAY OF TEXT := NIL; 
    wd: Pathname.T := NIL): INTEGER RAISES{OSError.E};
(* Run a filter and wait for the output asynchronously. This avoids
   blocking if a filter does not follow the expected behavior
   (read all input then start its output). *)

PROCEDURE OSMessage(list: AtomList.T): TEXT;
(* Convert a list of atoms, often returned as an OSError, to a single TEXT *)

(* Asynchronous reader, used by RunFilter but possibly useful by itself *)

TYPE
  ReadClosure <: PublicRead;
  PublicRead = Thread.Closure OBJECT
      rd: Rd.T;
      text: TEXT;
    END;

END IdeUtil.
