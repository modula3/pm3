GENERIC MODULE Cache(Key, Value, Table, Storage);

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:24:32  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:36:56  roland
    Simple caches for attributes, labels, names and identifeiers of a
    scheme. Used only for read-only or exclusiv access to a scheme.

*)
(***************************************************************************)

IMPORT ErrorSupport, AtomList;

REVEAL
  T = Public BRANDED Brand OBJECT
        storage: Storage.T;
        table: Table.Default;
      OVERRIDES
        init   := Init;
        get    := Get;
        put    := Put;
        delete := Delete;
        flush  := Flush;
      END;

PROCEDURE Init (cache: T; storage: Storage.T): T =
  BEGIN
    cache.storage := storage;
    cache.table := NEW(Table.Default).init();
    RETURN cache;
  END Init;

PROCEDURE Get (cache: T; key: Key.T; VAR val: Value.T; VAR found: BOOLEAN)
  RAISES {Error} =
  BEGIN
    found := cache.table.get(key, val);
    IF NOT found THEN
      TRY
        cache.storage.read(key, val, found);
      EXCEPT
        Storage.Error (info) =>
          RAISE Error(ErrorSupport.Propagate(
                        "Cache.Get", "Storage.Error", info));
      END;
      IF found THEN EVAL cache.table.put(key, val) END;
    END;
  END Get;

PROCEDURE Put (cache       : T;
               key         : Key.T;
               val         : Value.T;
               writeThrough: BOOLEAN   := FALSE) RAISES {Error} =
  BEGIN
    EVAL cache.table.put(key, val);
    TRY
      IF writeThrough THEN cache.storage.write(key, val) END;
    EXCEPT
      Storage.Error (info) =>
        RAISE
          Error(ErrorSupport.Propagate("Cache.Get", "Storage.Error", info));
    END;
  END Put;

PROCEDURE Delete (cache: T; key: Key.T) RAISES {} =
  VAR val: Value.T;
  BEGIN
    EVAL cache.table.delete(key, val);
  END Delete;

PROCEDURE Flush (cache: T; save: BOOLEAN := TRUE) RAISES {Error} =
  VAR
    it     : Table.Iterator;
    key    : Key.T;
    val    : Value.T;
    lastExc: AtomList.T     := NIL;
  BEGIN
    IF save THEN
      it := cache.table.iterate();
      WHILE it.next(key, val) DO
        TRY
          cache.storage.write(key, val);
        EXCEPT
          Storage.Error (info) => lastExc := info;
        END;
      END;
    END;
    cache.table := NEW(Table.Default).init();
    IF lastExc # NIL THEN RAISE Error(lastExc) END;
  END Flush;

BEGIN
END Cache.
