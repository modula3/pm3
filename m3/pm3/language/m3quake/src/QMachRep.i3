INTERFACE QMachRep;

IMPORT QMachine, QCode, QValue, QVTbl, QVSeq, M3Buf, IntRefTbl, TextSeq;
IMPORT Pathname, Wr;

FROM Quake IMPORT Error;

REVEAL
  QMachine.T <: Rep;

TYPE
  Rep = QMachine.T_ BRANDED OBJECT
    reg       : Registers;
    scopes    : ScopeStack     := NIL;
    stack     : ValueStack     := NIL;
    loops     : LoopStack      := NIL;
    output    : OutputStack    := NIL;
    frames    : FrameStack     := NIL;
    includes  : IncludeStack   := NIL;
    globals   : IntRefTbl.T    := NIL;  (* M3ID.T -> QValue.Binding *)
    tmp_files : TextSeq.T      := NIL;
    tracing   : BOOLEAN        := FALSE;
    last_cp   : QCode.Stream   := NIL;
    bindings  : QValue.Binding := NIL;
    buffers   : BufStack;
    writer    : Wr.T           := NIL;
  END;

TYPE
  Registers = RECORD
    cp : QCode.Stream   := NIL; (* code pointer *)
    pc : INTEGER        := 0;   (* program counter *)
    xp : INTEGER        := 0;   (* scope stack pointer *)
    sp : INTEGER        := 0;   (* value stack pointer *)
    lp : INTEGER        := 0;   (* loop stack pointer *)
    op : INTEGER        := 0;   (* output stack pointer *)
    fp : INTEGER        := 0;   (* frame pointer *)
    ln : INTEGER        := 0;   (* line number *)
    ip : INTEGER        := 0;   (* include stack pointer *)
    pi : QCode.ProcInfo := NIL; (* procedure info *)
    fn : BOOLEAN        := FALSE; (* => expect return result *)
  END;

TYPE
  ScopeStack   = REF ARRAY OF QValue.Scope;
  ValueStack   = REF ARRAY OF QValue.T;
  LoopStack    = REF ARRAY OF LoopInfo;
  OutputStack  = REF ARRAY OF OutputInfo;
  FrameStack   = REF ARRAY OF FrameInfo;
  IncludeStack = REF ARRAY OF IncludeInfo;

TYPE
  LoopInfo = RECORD
    iter     : QVTbl.Iterator  := NIL;
    array    : QVSeq.T         := NIL;
    next_elt : INTEGER         := 0;
    variable : QValue.Binding  := NIL;
  END;

TYPE
  OutputInfo = RECORD
    name : TEXT := NIL;
    wr   : Wr.T := NIL;
  END;

TYPE
  FrameInfo = RECORD
    proc   : QValue.Proc := NIL;
    saved  : Registers;
  END;

TYPE
  IncludeInfo = RECORD
    file   : QCode.Stream;
    old_cp : QCode.Stream;
    old_pc : INTEGER;
  END;

TYPE
  BufStack = RECORD
    tos  : INTEGER := 0;
    bufs : ARRAY [0..9] OF M3Buf.T;
  END;

PROCEDURE CanonicalizePath (path: Pathname.Arcs): Pathname.Arcs;
PROCEDURE StripPrefix (t: QMachine.T;  prefix, 
                       path: Pathname.Arcs): Pathname.Arcs RAISES {Error};
PROCEDURE Include(t: QMachine.T; path: TEXT) RAISES {Error};
PROCEDURE PushString (t: QMachine.T;  s: TEXT);

END QMachRep.
