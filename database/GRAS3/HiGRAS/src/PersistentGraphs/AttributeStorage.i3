INTERFACE AttributeStorage;

(***************************************************************************)
(* This part of the file associates attributes to entities.  Several
   attributes can be attached to an entity; they are distinguished by an
   attribute number.  For each attribute number, there may be several
   attribute records distinguished by record numbers. *)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.2  1998/03/17 14:14:11  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.1  1997/03/26 11:38:54  roland
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

    Revision 1.5  1996/12/03 13:13:09  roland
    (Still) Replaced Type.Triple by CARDINAL, because of compiler error.

    Revision 1.4  1996/12/03 09:53:56  roland
    Type.Triple replaced by CARDINAL because of compiler bug.

    Revision 1.3  1996/11/20 12:22:46  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/08/06 16:26:21  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.2  1996/07/24 09:19:48  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.1.2.1  1996/04/29 13:43:29  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

# Revision 1.1  1994/01/20  18:41:12  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT AtomList;
IMPORT Access, ITPFile, CardRelation;

PROCEDURE Init (file: ITPFile.T) RAISES {Access.Locked, InternalError};
  (* Initialize the AttributeStorage for the file. *)


PROCEDURE PutAttribute (file       : ITPFile.T;
                        entity     : CARDINAL;
                        attributeNo: CARDINAL;
                        recordNo   : CARDINAL (* Type.Triple *);
                        attribute  : TEXT         )
  RAISES {Access.Locked, InternalError};
  (* Stores an attribute record at entity. *)


PROCEDURE GetAttribute (    file       : ITPFile.T;
                            entity     : CARDINAL;
                            attributeNo: CARDINAL;
                            recordNo   : CARDINAL (* Type.Triple *);
                        VAR found      : BOOLEAN      ): TEXT
  RAISES {Access.Locked, InternalError};
  (* Retrieves an attribute record at entity. *)


PROCEDURE DeleteAttribute (    file       : ITPFile.T;
                               entity     : CARDINAL;
                               attributeNo: CARDINAL;
                               recordNo   : CARDINAL (* Type.Triple *);
                           VAR found      : BOOLEAN      )
  RAISES {Access.Locked, InternalError};
  (* Deletes an attribute record at entity. *)


PROCEDURE DeleteAllAttributes (file: ITPFile.T; entity: CARDINAL)
  RAISES {Access.Locked, InternalError};
  (* Deletes all attribute records at entity. *)


PROCEDURE GetAllAttributes (file: ITPFile.T; entity: CARDINAL): CardRelation.T
  RAISES {Access.Locked, InternalError};
  (* Returns a set with all (attributeNo, recordNo) pairs stored for
     entity. *)

EXCEPTION
  InternalError(AtomList.T);

END AttributeStorage.
