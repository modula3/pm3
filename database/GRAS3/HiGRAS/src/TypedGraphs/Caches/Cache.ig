GENERIC INTERFACE Cache(Key, Value, Storage);

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.2  1998/01/21 12:40:39  roland
    Prettyprinting.

    Revision 1.1  1997/05/01 13:24:30  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:36:54  roland
    Simple caches for attributes, labels, names and identifeiers of a
    scheme. Used only for read-only or exclusiv access to a scheme.

*)
(***************************************************************************)

(* A Cache.T maps keys (Key.T) to values (Value.T).  Cache.T internally
   uses Tables.Default from the generic module Table, so Key and Value must
   have the same properties as stated in the Table interface.

   The third generic parameter of Cache defines the storage that is cached.
   It has to contain a type T which is subtype of an object type of the
   following form

   T <: OBJECT METHODS
     read(key: Key.T; VAR val: Value.T; VAR found: BOOLEAN) RAISES {Error};
     write(st: T; key: Key.T; val: Value.T) RAISES {Error};
   END;

   These methods are used to read and write cache contents from/to the
   cached (secondary) storage.  Error must be defined like the exception
   Error in this interface.  To allow for caches with the same key and
   value but different storages, Storage has to contain a Brand, too.

   The implementation of Cache needs another generic parameter Table which
   must be a Table(Key, Value) instance. *)

IMPORT AtomList;

CONST
  Brand = "(Cache " & Key.Brand & " " & Value.Brand & " " & Storage.Brand
            & ")";

TYPE
  T <: Public;

  Public =
    OBJECT
    METHODS
      init (storage: Storage.T): T;

      get (key: Key.T; VAR val: Value.T; VAR found: BOOLEAN)
           RAISES {Error};
           (* Return the value stored for key.  If it is not in the cache
              and connot be read from secondary storage, found will be
              FALSE. *)
      put (key: Key.T; val: Value.T; writeThrough: BOOLEAN := FALSE)
           RAISES {Error};
           (* Stores val as value for key.  If writeThroug is TRUE then the
              value is immediately written to secondary storage. *)
      delete (key: Key.T) RAISES {Error};
              (* If cache has an entry for key, it is removed. *)
      flush (save: BOOLEAN := TRUE) RAISES {Error};
             (* If save, writes all entries to secondary storage and clears
                the cache.  If not save, clears the cache. *)
    END;

EXCEPTION Error(AtomList.T);

END Cache.
