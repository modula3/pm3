INTERFACE Command;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:26  hosking
    Initial revision

    Revision 1.1  1998/01/21 14:23:33  roland
    Tree example demonstrates derived attributes, triggers, and user-recovery.

*)
(***************************************************************************)

IMPORT TypedGraphPool, TypedGraph, Node;

TYPE
  UserCommand =
    {Quit, CreateLeaf, DeleteSubtree, BeginTransaction,
     CommitTransaction, AbortTransaction, Undo, Redo, RedoPrev, RedoNext,
     RedoIth, Backstep, Forstep, LabelCheckpoint, GotoCheckpoint};

CONST
  CommandName = ARRAY UserCommand OF
                  TEXT{"Quit", "CreateLeaf", "DeleteSubtree",
                       "BeginTransaction", "CommitTransaction",
                       "AbortTransaction", "Undo", "Redo", "RedoPrev",
                       "RedoNext", "RedoIth", "Backstep", "Forstep",
                       "LabelCheckpoint", "GotoCheckpoint"};

  NeedsParam = ARRAY UserCommand OF
                 BOOLEAN{
                 FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
                 FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE};

  ParamName = ARRAY UserCommand OF
                TEXT{"", "Father", "Node", "", "", "", "", "", "", "",
                     "Redo branch", "", "", "Checkpoint label",
                     "Checkpoint label"};

PROCEDURE Init (pool: TypedGraphPool.T; graph: TypedGraph.T);

PROCEDURE ExecuteCommand (pool   : TypedGraphPool.T;
                          graph  : TypedGraph.T;
                          command: UserCommand;
                          param  : CARDINAL;
                          VAR root: Node.T) RAISES {Error};

EXCEPTION Error(TEXT);

END Command.
