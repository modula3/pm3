(*                                                                           *)
(*  Created on Fri March 22 1996 by collin                                   *)
(*      under license:                                                       *)
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

INTERFACE DebugProcTypes;

FROM M3CG IMPORT ByteSize, ByteOffset, TypeUID, Type, Name;

CONST
 Brand = "DebugProcTypes.T";

TYPE
  Proc_Param = RECORD
    n, parent_n : Name;
    return_type : Type;  
    exported    : BOOLEAN;
  END;

  Param = REF RECORD
    n     : Name;  
    s     : ByteSize;
    frame : ByteOffset;
    m3t   : TypeUID;
    t     : Type;
    line  : INTEGER;
    next  : Param:= NIL;
  END;

  Local = REF RECORD
    n     : Name;  
    s     : ByteSize;
    frame : ByteOffset;
    m3t   : TypeUID;
    t     : Type;
    line  : INTEGER;
    prev  : Local:= NIL;
    next  : Local:= NIL;
  END;

  Bloc = REF RECORD
    value       : INTEGER;
    begin_block := FALSE;
    local_head  : Local:= NIL;
    local_tail  : Local:= NIL;
    next        : Bloc:= NIL;
  END;

  Line = REF RECORD
    line, addr : INTEGER;
    next : Line := NIL;
  END;

  Case = REF RECORD
    line       : INTEGER;
    label_loc  : INTEGER;
    label_n    : INTEGER;
    indexreg   : INTEGER;
    prev       : Case := NIL;
    next       : Case := NIL;
  END;

  T = REF RECORD
    proc_param : Proc_Param;
    param_head : Param := NIL;
    param_tail : Param := NIL;
    local_head : Local := NIL;
    local_tail : Local := NIL;
    bloc_head  : Bloc  := NIL;
    bloc_tail  : Bloc  := NIL;
    line_head  : Line  := NIL;
    line_tail  : Line  := NIL;
    case_head  : Case  := NIL;
    case_tail  : Case  := NIL;
  END;

END DebugProcTypes.
