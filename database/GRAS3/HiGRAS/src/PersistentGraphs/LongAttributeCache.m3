MODULE LongAttributeCache;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:46  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:31  hosking
    Import of GRAS3 1.1

    Revision 1.4  1998/06/10 14:37:33  roland
    Ensure to make a copy of chache contents for getAttribute.

    Revision 1.3  1997/11/10 10:43:21  roland
    Bugfix in statistics.

    Revision 1.2  1997/10/31 14:20:31  roland
    Adapted to new RuleEngine.

    Revision 1.1  1997/03/26 11:39:13  roland
    Subsystem PersistentGraph adapted to handle graph boundary crossing
    edges. This has consequences on the architecture of the subsystem as
    well as on the graph model and interface.

    Graphs are organized in pools. Every graph has a number in the
    pool. Pools are the units of transaction management. Two graphs might
    be related by one external relation storage storing the edges between
    nodes of them. Nodes are identified by pairs (graph, entity), where
    graph is the number of the graph in the pool and entity the node
    number within the graph. Graphs and external relation storages are
    administered by the pool in a separate graph.

    Revision 1.3  1997/03/25 17:04:02  roland
    To avoid deadlocks with propagate callbacks, the cache now does'nt
    access LongAttributeStorage directly. This is the responsibility of
    PersistentGraph again.

    Revision 1.2  1997/02/20 16:18:05  roland
    Attribute cache now uses write through policy to avoid inconsistencies
    in multi-user access.

    Revision 1.1  1997/02/06 13:44:12  roland
    Attributes of LongAttributeStorage are now cached in LongAttributeCache.

*)
(***************************************************************************)

IMPORT VirtualResource;
IMPORT Fmt, Text, TextF;
IMPORT Journal, Variant;
IMPORT Trigger, Action, ContextSet, VirtualPageEventPattern,
       VirtualPageEvent, RuleEngine, EventType;

CONST Size = 51;

TYPE
  CacheEntry = RECORD
                 entity, attributeNo: CARDINAL;
                 val                : TEXT;
                 length             : CARDINAL;
                 valid              : BOOLEAN;
               END;

  CacheIndex = [0 .. Size - 1];
  EntryArray = ARRAY CacheIndex OF CacheEntry;

CONST NullEntry = CacheEntry{0, 0, NIL, 0, FALSE};

REVEAL
  T = Public BRANDED OBJECT
        resource               : VirtualResource.T;
        entries                : EntryArray;
        trigger                : CARDINAL;
        mutex                  : MUTEX;
        hits, misses, conflicts: CARDINAL            := 0;
      OVERRIDES
        init         := Init;
        close        := Close;
        put          := PutAttribute;
        delete       := DeleteAttribute;
        get          := GetAttribute;
        truncate     := TruncateAttribute;
        has          := HasAttribute;
        store        := StoreAttribute;
        clear        := Clear;
        removeEntity := RemoveEntity;
      END;

PROCEDURE Init (cache: T; resource: VirtualResource.T): T =
  VAR
    pattern: VirtualPageEventPattern.T;
    trigger: Trigger.T;
    action    := NEW(Action.Local).init(TriggerAction);
    coupling  := Trigger.CouplingMode.Immediate;
    inh, perm := ContextSet.Empty();
  <* FATAL EventType.Mismatch, EventType.Unknown *>
  BEGIN
    cache.resource := resource;
    cache.entries := EntryArray{NullEntry, ..};
    pattern := VirtualPageEventPattern.Create(
                 VirtualPageEvent.Operation.RemoteCommit);
    VirtualPageEventPattern.SetResource(pattern, resource);
    VirtualPageEventPattern.SetResourceName(
      pattern, resource.getBaseName());
    trigger := Trigger.Create(pattern, action, coupling, 0, inh, perm);
    cache.trigger :=
      RuleEngine.RegisterTrigger(trigger, RuleEngine.Interest.Self, cache);
    cache.mutex := NEW(MUTEX);
    RETURN cache;
  END Init;

PROCEDURE Close (cache: T) =
  BEGIN
    RuleEngine.UnregisterTrigger(cache.trigger);
    IF Variant.TestAttributeCache THEN
      Journal.Add("Attribute cache statistics");
      Journal.Add(
        "  total accesses: " & Fmt.Int(cache.hits + cache.misses));
      Journal.Add("            hits: " & Fmt.Int(cache.hits));
      Journal.Add("          misses: " & Fmt.Int(cache.misses));
      Journal.Add("       conflicts: " & Fmt.Int(cache.conflicts) & "\n");
    END;
  END Close;

PROCEDURE StoreAttribute (cache: T; ent, attr: CARDINAL; val: TEXT) =
  VAR ind: CacheIndex;
  BEGIN
    LOCK cache.mutex DO
      ind := Hash(ent, attr);
      cache.entries[ind] :=
        CacheEntry{entity := ent, attributeNo := attr, val := val, valid :=
                   TRUE, length := Text.Length(val)};
    END;
  END StoreAttribute;

PROCEDURE HasAttribute (cache: T; attr, ent: CARDINAL): BOOLEAN =
  VAR
    di : CacheIndex;
    res: BOOLEAN;
  BEGIN
    LOCK cache.mutex DO res := CheckEntry(cache, attr, ent, di); END;
    RETURN res;
  END HasAttribute;

PROCEDURE PutAttribute (cache      : T;
                        entity     : CARDINAL;
                        attributeNo: CARDINAL;
                        start      : CARDINAL;
                        attribute  : TEXT      ) RAISES {} =
  VAR
    begin, middle, end: TEXT;
    len               : CARDINAL;
    ind               : CacheIndex;
  BEGIN
    LOCK cache.mutex DO
      IF CheckEntry(cache, entity, attributeNo, ind) THEN
        len := Text.Length(attribute);
        IF len > 0 THEN
          begin := Text.Sub(cache.entries[ind].val, 0, start);
          end := Text.Sub(cache.entries[ind].val, start + len);
          (* If the start position of writing is higher than th length of
             the cached attribute we must fill the gap with '\000'. *)
          middle :=
            Fmt.Pad(
              attribute, MAX(len, start + len - cache.entries[ind].length),
              '\000', Fmt.Align.Right);
          cache.entries[ind].val := begin & middle & end;
          cache.entries[ind].length :=
            MAX(cache.entries[ind].length, start + len);
        END;
      END;
    END;
  END PutAttribute;


PROCEDURE DeleteAttribute (cache      : T;
                           entity     : CARDINAL;
                           attributeNo: CARDINAL  ) RAISES {} =
  VAR ind: CacheIndex;
  BEGIN
    LOCK cache.mutex DO
      IF CheckEntry(cache, entity, attributeNo, ind) THEN
        IF cache.entries[ind].length > 0 THEN
          cache.entries[ind].val := "";
          cache.entries[ind].length := 0;
        END;
      END;
    END;
  END DeleteAttribute;



PROCEDURE GetAttribute (    cache      : T;
                            entity     : CARDINAL;
                            attributeNo: CARDINAL;
                            start      : CARDINAL;
                            length     : CARDINAL;
                        VAR found      : BOOLEAN   ): TEXT RAISES {} =
  VAR
    res: TEXT;
    ind: CacheIndex;

  PROCEDURE Sub (t: Text.T; start, length: CARDINAL): Text.T =
    (* Modified Text.Sub *)
  BEGIN
    WITH n   = NUMBER (t^) - 1,
         len = MIN (n - start, length) DO
      IF (len <= 0) THEN RETURN "" END;
      IF len = 1 THEN RETURN Text.FromChar (t [start]) END;
      WITH res = NEW (Text.T, len + 1) DO
        SUBARRAY(res^, 0, len) := SUBARRAY(t^, start, len);
        res [len] := '\000';
        RETURN res;
      END;
    END;
  END Sub;

  BEGIN
    LOCK cache.mutex DO
      IF CheckEntry(cache, entity, attributeNo, ind) THEN
        (* We want to enforce a copy. Text.Sub does not copy the text 
           when all of it is requested. *)
        res := Sub(cache.entries[ind].val, start, length);
        found := TRUE;
      ELSE
        found := FALSE;
      END;
    END;
    RETURN res;
  END GetAttribute;


PROCEDURE TruncateAttribute (cache      : T;
                             entity     : CARDINAL;
                             attributeNo: CARDINAL;
                             size       : CARDINAL  ) RAISES {} =
  VAR ind: CacheIndex;
  BEGIN
    LOCK cache.mutex DO
      IF CheckEntry(cache, entity, attributeNo, ind) THEN
        IF cache.entries[ind].length > size THEN
          cache.entries[ind].val :=
            Text.Sub(cache.entries[ind].val, 0, size);
          cache.entries[ind].length := size;
        END;
      END;
    END;
  END TruncateAttribute;


PROCEDURE Clear (cache: T) RAISES {} =
  BEGIN
    LOCK cache.mutex DO cache.entries := EntryArray{NullEntry, ..} END;
  END Clear;

PROCEDURE RemoveEntity (cache: T; ent: CARDINAL) =
  BEGIN
    LOCK cache.mutex DO
      FOR i := FIRST(cache.entries) TO LAST(cache.entries) DO
        IF cache.entries[i].valid AND cache.entries[i].entity = ent THEN
          cache.entries[i] := NullEntry;
        END;
      END;
    END;
  END RemoveEntity;

(* local procedures *)

PROCEDURE Hash (ent, attr: CARDINAL): CacheIndex =
  BEGIN
    RETURN (2 * attr + ent) MOD Size;
  END Hash;


PROCEDURE CheckEntry (cache: T; ent, attr: CARDINAL; VAR index: CacheIndex):
  BOOLEAN RAISES {} =
  BEGIN
    (* Is the attribute cached? *)
    index := Hash(ent, attr);
    IF cache.entries[index].entity # ent
         OR cache.entries[index].attributeNo # attr
         OR NOT cache.entries[index].valid THEN
      IF Variant.TestAttributeCache THEN
        INC(cache.misses);
        IF cache.entries[index].valid THEN INC(cache.conflicts); END;
      END;
      RETURN FALSE;
    ELSE
      IF Variant.TestAttributeCache THEN INC(cache.hits); END;
      RETURN TRUE;
    END;
  END CheckEntry;


PROCEDURE TriggerAction (<* UNUSED *> event  : VirtualPageEvent.T;
                         <* UNUSED *> context: ContextSet.T;
                         <* UNUSED *> local  : BOOLEAN;
                                      cache  : <*TRANSIENT*> REFANY) =
  BEGIN
    Clear(NARROW(cache, T));
  END TriggerAction;

BEGIN
END LongAttributeCache.
