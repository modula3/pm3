(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan  6 13:00:00 1997 by collin@vlsi.polymtl.ca       *)
(*      under a new license:                                                 *)
(*                                                                           *)
(*      Copyright (C) 1997 Jerome Collin                                     *)
(*                                                                           *)
(*      This is a free software; you can redistribute it and/or modify       *)
(*      it under the terms of the GNU General Public License as published    *)
(*      by the Free Software Foundation; either version 2, or                *)
(*      (at your option) any later version.                                  *)
(*                                                                           *)
(*      This software is distributed in the hope that it will be useful,     *)
(*      but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(*      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(*      GNU General Public License for more details.                         *)
(*                                                                           *)
(* modified on Mon Sep 26 14:21:02 PDT 1994 by isard                         *)
(* modified on Fri Nov 19 09:30:31 PST 1993 by kalsow                        *)
(* modified on Mon Apr 13 09:55:12 PDT 1992 by muller                        *)

INTERFACE M3x86;

IMPORT M3CG, M3ObjFile, Wr;

PROCEDURE New (logfile: Wr.T; obj: M3ObjFile.T; READONLY options  : 
						ARRAY OF TEXT): M3CG.T;
(* returns a fresh, initialized code generator that writes its
   log on 'logfile', or is silent if logfile = NIL. *)

END M3x86.
