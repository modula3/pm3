MODULE TypeCache;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:35  hosking
    Initial revision

    Revision 1.2  1997/10/31 14:23:28  roland
    Adapted to new RuleEngine.

    Revision 1.1  1997/07/07 15:56:50  roland
    New caches for node types of a typed graph.

*)
(***************************************************************************)

IMPORT Node, TypedGraphPool;
IMPORT NodeToTextTbl;
IMPORT Trigger, Action, ContextSet, VirtualPageEventPattern,
       VirtualPageEvent, RuleEngine, EventType;

REVEAL
  T = Public BRANDED OBJECT
        pool   : TypedGraphPool.T;
        entries: NodeToTextTbl.T;
        trigger: CARDINAL;
        mutex  : MUTEX;
      OVERRIDES
        init   := Init;
        put    := Put;
        delete := Delete;
        get    := Get;
        flush  := Flush;
      END;


PROCEDURE Init (cache: T; pool: TypedGraphPool.T): T =
  VAR
    pattern: VirtualPageEventPattern.T;
    trigger: Trigger.T;
    action    := NEW(Action.Local).init(TriggerAction);
    coupling  := Trigger.CouplingMode.Immediate;
    inh, perm := ContextSet.Empty();
  <* FATAL EventType.Mismatch, EventType.Unknown *>
  BEGIN
    cache.entries := NEW(NodeToTextTbl.Default).init();
    cache.pool := pool;
    pattern := VirtualPageEventPattern.Create(VirtualPageEvent.Operation.RemoteCommit);
    VirtualPageEventPattern.SetResource(pattern, pool);
    VirtualPageEventPattern.SetResourceName(pattern, pool.getBaseName());
    trigger := Trigger.Create(pattern, action, coupling, 0, inh, perm);
    cache.trigger :=
      RuleEngine.RegisterTrigger(trigger, RuleEngine.Interest.Self, cache);
    cache.mutex := NEW(MUTEX);
    RETURN cache;
  END Init;

PROCEDURE Get (cache: T; node: Node.T; VAR val: TEXT): BOOLEAN =
  BEGIN
    LOCK cache.mutex DO RETURN cache.entries.get(node, val); END;
  END Get;

PROCEDURE Put (cache: T; node: Node.T; val: TEXT) =
  BEGIN
    LOCK cache.mutex DO EVAL cache.entries.put(node, val); END;
  END Put;

PROCEDURE Delete (cache: T; node: Node.T) =
  VAR t: TEXT;
  BEGIN
    LOCK cache.mutex DO EVAL cache.entries.delete(node, t); END;
  END Delete;

PROCEDURE Flush (cache: T) =
  BEGIN
    LOCK cache.mutex DO
      cache.entries := NEW(NodeToTextTbl.Default).init();
    END;
  END Flush;

(* Trigger for cache coherence *)
(* TYPE *)
(* CacheTrigger = VirtualResourceTrigger.T OBJECT *)
(* cache: T; *)
(* METHODS *)
(* init (cache: T): CacheTrigger := TriggerInit; *)
(* OVERRIDES *)
(* match := TriggerMatch; *)
(* action := TriggerAction; *)
(* END; *)

(* PROCEDURE TriggerInit (self: CacheTrigger; cache: T): CacheTrigger = *)
(* BEGIN *)
(* self.cache := cache; *)
(* RETURN VirtualResourceTrigger.T.init(self); *)
(* END TriggerInit; *)


(* PROCEDURE TriggerMatch (<* UNUSED *> trigger: CacheTrigger; *)
(* event : ResourceEvent.T): BOOLEAN = *)
(* BEGIN *)
(* RETURN event.getOperation() =
   ResourceEvent.Operation.CommitTransaction *)
(* AND event.fromOtherClient(); *)
(* END TriggerMatch; *)

PROCEDURE TriggerAction (<* UNUSED *> event  : VirtualPageEvent.T;
                         <* UNUSED *> context: ContextSet.T;
                         <* UNUSED *> local  : BOOLEAN;
                                      cache  : REFANY              ) =
  BEGIN
    Flush(NARROW(cache, T));
  END TriggerAction;

BEGIN
END TypeCache.
