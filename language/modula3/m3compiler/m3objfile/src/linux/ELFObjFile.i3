(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(*  Created on Tue Jun  6 14:00:00 PDT 1994 by collin                        *)
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
(*  by modifying NTObjfile created and			                     *)
(*  last modified on Mon Sep 26 14:17:32 PDT 1994 by kalsow                  *)
(*							                     *)
(* An "ELFObjFile.T" is a LINUX object file (ELF format).                    *)

INTERFACE ELFObjFile;  (* This was taken from NTObjFile *) 

IMPORT M3ObjFile, Wr, Thread;

TYPE
  T <: M3ObjFile.T;

PROCEDURE New (): T;

PROCEDURE Dump (t: T; wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted};

END ELFObjFile.


