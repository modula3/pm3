INTERFACE ChgMgmtGraph;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.5  1998/05/19 10:17:26  roland
    Support for log-groups implemented.

    Revision 1.4  1998/03/18 09:27:06  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.3  1998/03/17 14:13:49  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.2  1997/04/24 14:29:02  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 14:09:05  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary
    crossing edges.

    Revision 1.6  1997/02/04 11:15:11  roland
    It is now possible to disable logging in ChgMgmtGraph completely.

    Revision 1.5  1996/11/25 08:21:11  roland
    Exception PendingTransaction is raised when client tries to close a
    graph and has a running transaction.

    Revision 1.4  1996/11/20 12:20:13  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/11/14 14:16:47  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Access in mode ReadOnlyShared is now considered when opening graphs.

    Revision 1.2  1996/09/20 13:58:13  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:56:40  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)


(** A change management graph has abilities for undo/redo and versioning.
    All actions on the graph are logged to a command stream (short: delta)
    from which commands can be restored. *)

IMPORT PersistentGraph AS Super;
IMPORT ChgMgmtGraphPool;
IMPORT Pathname, AtomList;
IMPORT PageFile, Access, NodeLabelRelation, AttributeDescriptionSet,
       TaggedNameSet, Node;

TYPE LogMode = ChgMgmtGraphPool.LogMode;

(* This class overrides almost every method of its supertype.  Only those
   methodes, which do not change the state of the graph are left
   unchanged. *)

TYPE
  AccessMode = {Inherit,         (* = take access mode of pool *)
                ReadWriteShared, ReadWriteExclusive, ReadOnlyShared};

  T <: Public;

  Public =
    Super.T OBJECT
    METHODS
      open (pool            : ChgMgmtGraphPool.T;
            baseName        : Pathname.T;
            access          : AccessMode;
            new, errorChecks: BOOLEAN;
            local           : BOOLEAN;
            log             : LogMode              := LogMode.Linear;
            logGroup        : Pathname.T           := NIL): T
            RAISES {InUse, NotExistent, Access.Locked, Access.Denied,
                    PageFile.NoAccess, InternalError};
            (* Opens a ChgMgmtGraph.T.  In addition to the opening of a
               graph of type Super.T, here exists the argument 'log', which
               indicates whether the log should be linear, tree-like, or
               completely omitted.  A linear log keeps the applied commands
               in a list.  The commands are grouped between checkpoints.
               This list can be traversed along the checkpoints with the
               undo/redo commands.  New commands overwrite the end of the
               list, so that redo always has only one target.  If the log
               is tree-like, the commands are stored in a tree like
               fashion, i.e.  after an undo, new commands will not
               overwrite old commands in the log but will create a new
               branch in the tree.  The tree can be traversed with undo,
               redoPrev, redoNext, redoIth, backstep, and forstep.  Redo is
               also possible and will take the branch of the tree which was
               last undone.  The checkpoints are the nodes of the tree.  If
               log = LogMode.None commands are not logged and no undo/redo
               is possible. *)

      close (keepLog: BOOLEAN) RAISES {InternalError};
             (* close the Graph.  If keepLog is FALSE, the log will be
                deleted.  If not, the current log may be used during the
                next session. *)

      (* Commands for user-recovery.  Unlike all other graph changing
         commands, these commands can be used outside of transactions.
         Note that use of these commands in a multi-user environment will
         lead to highly unpredictable behaviour. *)

      getLogMode (): LogMode;
                  (* The log kind of the graph is not necessarily the one
                     given as argument to open, because other users might
                     have opened the graph in a different mode. *)

      setCheckpoint () RAISES {NoLog, Access.Locked, LogError,
                               InternalError};
                     (* Set a checkpoint for user recovery.  Undo and redo
                        will step from checkpoint to checkpoint.  So if
                        every single action should be recoverable, a
                        checkpoint should be set between every two
                        commands.  Note, that this will surely degrade
                        performance.  The new checkpoint becomes the
                        current checkpoint. *)

      setCheckpointLabel (label: CARDINAL)
                          RAISES {NoLog, Access.Locked, LogError,
                                  InternalError};
                          (* labels the current checkpoint. *)

      removeCheckpointLabel () RAISES {NoLog, Access.Locked, LogError,
                                       InternalError};
                             (* removes the label from the current
                                checkpoint *)

      undo (VAR N: CARDINAL)
            RAISES {NoLog, Access.Locked, LogError, InternalError};
            (* Tries do undo the last N steps.  Each step is marked with a
               checkpoint.  If there are only M checkpoints and M<N then M
               steps will be undone and N:= M.*)

      redo (VAR N: CARDINAL)
            RAISES {NoLog, Access.Locked, LogError, InternalError};
            (* Tries to redo N steps. *)

      redoNext () RAISES {NoLog, Access.Locked, NoSuchCheckpoint,
                          NotInTreeMode, LogError, InternalError};
                (* Tries to redo one step, but not to the actual undone.
                   the next (younger) son is choosen. *)

      redoPrev () RAISES {NoLog, Access.Locked, NoSuchCheckpoint,
                          NotInTreeMode, LogError, InternalError};
                (* Tries to redo one step, but not to the actual undone.
                   the previous (older) son is choosen. *)

      redoIth (i: CARDINAL)
               RAISES {NoLog, Access.Locked, NoSuchCheckpoint,
                       NotInTreeMode, LogError, InternalError};
               (* Tries to redo the i-th son. *)

      backstep (VAR N: CARDINAL)
                RAISES {NoLog, Access.Locked, NoSuchCheckpoint,
                        NotInTreeMode, LogError, InternalError};
                (* Tries to go N steps back, including write operations and
                   undo - and redo commands If there are only M possible
                   commands to go back and M<N then M steps will be stepped
                   back and N:= M. *)

      forstep (VAR N: CARDINAL)
               RAISES {NoLog, Access.Locked, NoSuchCheckpoint,
                       NotInTreeMode, LogError, InternalError};
               (* Tries to go N steps forward, including write operations
                  and undo - and redo commands.  opposite command to
                  backstep. *)

      gotoCheckpointLabel (label: CARDINAL)
                           RAISES {NoLog, Access.Locked, NoSuchCheckpoint,
                                   LogError, InternalError};
                           (* get the situation of the Checkpoint with
                              label label *)

      (* All graph changing commands may now raise the exception LogError,
         indicating that operations on the log failed.  Normal operation
         should be aborted after such an exception. *)


      createNode (label: CARDINAL): Node.T
                  RAISES {Access.Locked, LogError, InternalError};
                  (* Creates the node with the number returned by
                     createNodeNumber and the given label.  This must be
                     immediately preceeded by createNodeNumber.  The node
                     number is returned again. *)

      deleteNode (    node      : Node.T;
                  VAR attributes: AttributeDescriptionSet.T;
                  VAR indexes   : TaggedNameSet.T;
                  VAR outEdges  : NodeLabelRelation.T;
                  VAR inEdges   : NodeLabelRelation.T        )
                  RAISES {Super.NotOwner, Access.Locked, LogError,
                          InternalError};
                  (* Deletes the given node.  The method returns the
                     attributes and indexes stored for the node and all
                     incident edges.  Edges are returned as (label, target)
                     tuples.  These attributes, names, and edges were
                     deleted with the node. *)

      deleteNodeNoInfo (node: Node.T)
                        RAISES {Super.NotOwner, Access.Locked, LogError,
                                InternalError};
                        (* Same as deleteNode, but no information abput
                           attributes, indexes, and edges is returned. *)

      putNodeLabel (node: Node.T; label: CARDINAL)
                    RAISES {Super.NotOwner, Access.Locked, LogError,
                            InternalError, Super.NodeNotFound};
                    (* Stores label at node. *)

      putAttribute (node       : Node.T;
                    attributeNo: CARDINAL;
                    start      : CARDINAL;
                    attribute  : TEXT      )
                    RAISES {Super.NotOwner, Access.Locked,
                            Super.NodeNotFound, LogError, InternalError};
                    (* Stores the attribute at attributeNo of the given
                       node starting at start. *)

      deleteAttribute (node: Node.T; attributeNo: CARDINAL)
                       RAISES {Super.NotOwner, Access.Locked,
                               Super.NodeNotFound, LogError, InternalError};
                       (* Deletes the attribute with attributeNo of the
                          given node. *)

      truncateAttribute (node       : Node.T;
                         attributeNo: CARDINAL;
                         size       : CARDINAL  )
                         RAISES {Super.NotOwner, Access.Locked,
                                 Super.NodeNotFound, LogError,
                                 InternalError};
                         (* The given attribute is truncated to size bytes.
                            The attribute must contain at least size bytes
                            before. *)

      putIndex (node: Node.T; indexNo: CARDINAL; index: TEXT)
                RAISES {Super.NotOwner, Access.Locked, Super.NodeNotFound,
                        LogError, Super.IndexUsed, InternalError};
                (* Stores index as index attribute for the node. *)

      deleteIndex (node: Node.T; indexNo: CARDINAL; index: TEXT)
                   RAISES {Super.NotOwner, Access.Locked,
                           Super.NodeNotFound, LogError, Super.IndexUnused,
                           InternalError};
                   (* Delete the given index attribute. *)

      createEdge (source: Node.T; target: Node.T; label: CARDINAL)
                  RAISES {Super.NotOwner, Access.Locked,
                          Super.NodeNotFound, LogError, InternalError};
                  (* Creates an edge between source and target with the
                     given label. *)

      deleteEdge (source: Node.T; target: Node.T; label: CARDINAL)
                  RAISES {Super.NotOwner, Access.Locked,
                          Super.NodeNotFound, LogError, InternalError};
                  (* Deletes the edge between source and target with the
                     given label. *)


    END;

EXCEPTION
  Existent;                      (* graph exists/ not exists *)
  NotExistent;
  InUse;

  NoSuchCheckpoint;              (* Raised by redoPrev, redoNext, redoIth,
                                    backstep, and forstep if the checkpoint
                                    the refer to does not exist. *)
  NoLog;                         (* Graph is in LogMode.None.
                                    User-Recovery is not allowed. *)
  NotInTreeMode;                 (* Raised by redoPrev, redoNext, redoIth,
                                    backstep, and forstep if they are
                                    called in linear mode. *)
  LogError(AtomList.T);          (* Operations on the log failed. *)
  InternalError(AtomList.T);

END ChgMgmtGraph.
