INTERFACE OO1Graph;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.3  1998/03/18 09:26:56  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.2  1998/03/17 14:13:32  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.1  1997/02/20 16:08:50  roland
    OO1 rewritten with graphical user interface.

*)
(***************************************************************************)

IMPORT Pathname, Access, CardSet, Thread;

TYPE
  TypeString = ARRAY [0 .. 9] OF CHAR;
  Date = RECORD year, month, day, hour, minute, second: CARDINAL;  END;

  T =
    OBJECT
    METHODS
      open (name          : Pathname.T;
            access        : Access.Mode;
            new           : BOOLEAN;
            local         : BOOLEAN;
            simpleConnects: BOOLEAN      ): T
            RAISES {Failure, Thread.Alerted} := NIL;

      close () := NIL;

      beginTransaction () RAISES {Failure, Thread.Alerted} := NIL;

      commitTransaction () RAISES {Failure, Thread.Alerted} := NIL;

      abortTransaction () RAISES {Failure, Thread.Alerted} := NIL;

      createPart (n: CARDINAL): CARDINAL RAISES {Failure, Thread.Alerted} := NIL;

      connect (source, target: CARDINAL; type: TypeString; length: CARDINAL)
               RAISES {Failure, Thread.Alerted} := NIL;

      connected (source, target: CARDINAL): BOOLEAN
                 RAISES {Failure, Thread.Alerted} := NIL;

      connectionInfo (    source, target: CARDINAL;
                      VAR type          : TypeString;
                      VAR len           : CARDINAL;
                      VAR ok            : BOOLEAN     )
                      RAISES {Failure, Thread.Alerted} := NIL;

      sources (target: CARDINAL): CardSet.T
               RAISES {Failure, Thread.Alerted} := NIL;

      targets (source: CARDINAL): CardSet.T
               RAISES {Failure, Thread.Alerted} := NIL;

      putPartAttributes (         part : CARDINAL;
                         READONLY type : TypeString;
                                  x, y : INTEGER;
                         READONLY build: Date        )
                         RAISES {Failure, Thread.Alerted} := NIL;

      getPartAttributes (    part : CARDINAL;
                         VAR type : TypeString;
                         VAR x, y : INTEGER;
                         VAR build: Date        )
                         RAISES {Failure, Thread.Alerted} := NIL;
    END;

EXCEPTION Failure(TEXT);

END OO1Graph.
