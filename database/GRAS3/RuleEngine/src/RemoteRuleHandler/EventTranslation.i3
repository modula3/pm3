(*!  FUNCTIONAL MODULE *)
INTERFACE EventTranslation;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:50  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:40  hosking
    Import of GRAS3 1.1

    Revision 1.1  1997/10/31 14:05:00  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The remote rule handler
    connects to a special server object to communicate with other rule engines.

*)
(***************************************************************************)

IMPORT IntIntTransientTbl AS IntIntTbl, IntTextTransientTbl AS IntTextTbl;
IMPORT EventPattern, Event;

EXCEPTION
  UnknownType;
  AttributeMismatch;

PROCEDURE ComposeEvent (type : TEXT;
                        bools: IntIntTbl.T;
                        ints : IntIntTbl.T;
                        texts: IntTextTbl.T): Event.T
  RAISES {UnknownType, AttributeMismatch};

PROCEDURE DecomposeEvent (    event: Event.T;
                          VAR type : TEXT;
                          VAR bools: IntIntTbl.T;
                          VAR ints : IntIntTbl.T;
                          VAR texts: IntTextTbl.T);

PROCEDURE ComposePattern (type : TEXT;
                          bools: IntIntTbl.T;
                          ints : IntIntTbl.T;
                          texts: IntTextTbl.T ): EventPattern.T
  RAISES {UnknownType, AttributeMismatch};

PROCEDURE DecomposePattern (    pattern: EventPattern.T;
                            VAR type   : TEXT;
                            VAR bools  : IntIntTbl.T;
                            VAR ints   : IntIntTbl.T;
                            VAR texts  : IntTextTbl.T    );

END EventTranslation.
