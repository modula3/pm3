(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Feb 20 13:33:35 PST 1995 by kalsow     *)

INTERFACE QMachine;

IMPORT Wr, M3ID, QValue, QCode;
FROM Quake IMPORT Machine, Error;

REVEAL
  T <: T_;
TYPE
  T = Machine;
  T_ = OBJECT METHODS
    init     (writer: Wr.T): T;
    evaluate (s: QCode.Stream)                                RAISES {Error};
    get      (nm: M3ID.T;  VAR(*OUT*) val: QValue.T): BOOLEAN RAISES {Error};
    put      (nm: M3ID.T; READONLY val: QValue.T; 
              readonly: BOOLEAN := FALSE)                     RAISES {Error};
    lookup   (nm: M3ID.T): QValue.Binding                     RAISES {Error};
    push     (READONLY val: QValue.T)                         RAISES {Error};
    pop      (VAR(*OUT*) val: QValue.T)                       RAISES {Error};
    error    (msg: TEXT)                                      RAISES {Error};
    cleanup  ()                                               RAISES {Error};
    call     (proc: QValue.Proc; args: REF ARRAY OF QValue.T; 
              isFunc: BOOLEAN)                                RAISES {Error};
    cur_wr   (): Wr.T;
  END;

END QMachine.
