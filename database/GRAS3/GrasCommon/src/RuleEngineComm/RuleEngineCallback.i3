INTERFACE RuleEngineCallback;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:44  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:28  hosking
    Import of GRAS3 1.1

    Revision 1.1  1997/10/31 14:09:26  roland
    Communication interfaces for the rule engine.

*)
(***************************************************************************)

IMPORT NetObj, Thread;
IMPORT IntIntTransientTbl AS IntIntTbl,
       IntTextTransientTbl AS IntTextTbl,
       TextTransientSeq AS TextSeq;

CONST
  ImmediateCoupling = 0;
  DeferredCoupling  = 1;
  DecoupledCoupling = 2;

CONST Brand = "RuleEngineCallack";

TYPE
  T =
    NetObj.T OBJECT
    METHODS
      registerTrigger (id        : CARDINAL;
                       client    : CARDINAL;
                       pType     : TEXT;
                       pBools    : IntIntTbl.T;
                       pInts     : IntIntTbl.T;
                       pTexts    : IntTextTbl.T;
                       coupling  : CARDINAL;
                       priority  : CARDINAL;
                       inh, perm : TextSeq.T     )
                       RAISES {NetObj.Error, Thread.Alerted};
                       (* Monitor events according to the parameters (see
                          RuleEngineServer).  Use id to report occurred
                          events back to the server. *)

      unregisterTrigger (trigger: CARDINAL)
                         RAISES {NetObj.Error, Thread.Alerted};
                         (* Stop Monitoring trigger. *)

      killClient (client: CARDINAL) RAISES {NetObj.Error, Thread.Alerted};
                  (* Kill all triggers and activated actions for client. *)

      notifyEvent (trigger: CARDINAL;
                   client : CARDINAL;
                   eBools : IntIntTbl.T;
                   eInts  : IntIntTbl.T;
                   eTexts : IntTextTbl.T;
                   context: TextSeq.T     )
                   RAISES {NetObj.Error, Thread.Alerted};
                   (* client has monitored the event described by the
                      parameters. *)

    END;

END RuleEngineCallback.
