MODULE VolatileMetaOpStack;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:35:25  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.1  1996/09/23 08:35:54  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

*)
(***************************************************************************)

IMPORT MetaOpStack AS Super;
IMPORT CardStack;

REVEAL
  T = Public BRANDED OBJECT
        stack: CardStack.T;
      OVERRIDES
        init    := Init;
        push    := Push;
        pop     := Pop;
        isEmpty := IsEmpty;
        clear   := Clear;
      END;

<* FATAL CardStack.Undefined, CardStack.Full *>

PROCEDURE Init (s: T): T =
  BEGIN
    s.stack := NEW(CardStack.T).init();
    RETURN s;
  END Init;

PROCEDURE Push (s: T; x: CARDINAL) RAISES {} =
  BEGIN
    s.stack.push(x);
  END Push;

PROCEDURE Pop (s: T): CARDINAL RAISES {Super.Empty} =
  VAR res: CARDINAL := 0;
  BEGIN
    TRY
      res := s.stack.pop()
    EXCEPT
      CardStack.Empty => RAISE Super.Empty;
    END;
    RETURN res;
  END Pop;

PROCEDURE Clear (s: T) RAISES {} =
  BEGIN
    s.stack.clear();
  END Clear;

PROCEDURE IsEmpty (s: T): BOOLEAN =
  BEGIN
    RETURN s.stack.isEmpty();
  END IsEmpty;

BEGIN
END VolatileMetaOpStack.
