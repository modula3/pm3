INTERFACE RuleEngineServer;

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

    Revision 1.2  1997/11/03 12:40:11  roland
    New procedures to check connection to rule server.

    Revision 1.1  1997/10/31 14:09:28  roland
    Communication interfaces for the rule engine.

*)
(***************************************************************************)

IMPORT NetObj, Thread;
IMPORT IntIntTransientTbl AS IntIntTbl,
       IntTextTransientTbl AS IntTextTbl,
       TextTransientSeq AS TextSeq;
IMPORT RuleEngineCallback;

PROCEDURE ComposeServerId(id: TEXT): TEXT;

TYPE
  T =
    NetObj.T OBJECT
    METHODS
      (* Client administration *)
      register (callback: RuleEngineCallback.T): CARDINAL
                RAISES {Thread.Alerted, NetObj.Error};
                (* A client registers itself with this method.  The number
                   returned serves as identification in all other methods
                   of this interface. *)

      unregister (client: CARDINAL) RAISES {Thread.Alerted, NetObj.Error};
                  (* Unregister client.  All pending triggers and actions
                     for this client will be removed. *)

      (* Triggers *)
      registerTrigger (client   : CARDINAL;
                       pType    : TEXT;
                       pBools   : IntIntTbl.T;
                       pInts    : IntIntTbl.T;
                       pTexts   : IntTextTbl.T;
                       coupling : CARDINAL;
                       priority : CARDINAL;
                       inh, perm: TextSeq.T     ): CARDINAL
                       RAISES {Thread.Alerted, NetObj.Error};
                       (* Broadcast a trigger to all other clients.  pType
                          is the type of the monitored events.  pBools,
                          pInts, and pTexts are mappings attributeno->value
                          for boolean, integer, and text attributes of the
                          monitored pattern, respectively.  All attributes
                          not in these mapping are set to wildcards.  Note
                          that all RefAny attributes have to be wildcards.
                          coupling and priority are analogous to non-remote
                          triggers.  inh and perm are the inhibiting and
                          permitting contexts of the trigger listed in text
                          form.  The returned number serves as
                          identification for unregistering. *)

      unregisterTrigger (client, trigger: CARDINAL)
                         RAISES {Thread.Alerted, NetObj.Error};
                         (* Unregister trigger in all other clients.  If
                            trigger was not registered by client, nothing
                            happens. *)

      (* Observed events *)

      reportEvent (client : CARDINAL;
                   trigger: CARDINAL;
                   eBools : IntIntTbl.T;
                   eInts  : IntIntTbl.T;
                   eTexts : IntTextTbl.T;
                   context: TextSeq.T     )
                   RAISES {NetObj.Error, Thread.Alerted};
                   (* Report an event occurence to the client that
                      registered trigger.  eBools, eInts, eTexts, and eRefs
                      are the boolean, integer, text, and refany attributes
                      of the event, respectively.  The context is the
                      context set in which the event occured in text
                      form. *)

      ping() RAISES {NetObj.Error, Thread.Alerted};
      (* server still alive? *)
    END;

END RuleEngineServer.
