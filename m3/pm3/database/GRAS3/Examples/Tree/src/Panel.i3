INTERFACE Panel;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:26  hosking
    Initial revision

    Revision 1.1  1998/01/21 14:23:35  roland
    Tree example demonstrates derived attributes, triggers, and user-recovery.

*)
(***************************************************************************)

IMPORT Command;

PROCEDURE Open(): BOOLEAN;
PROCEDURE Close();

PROCEDURE QueryCommand(): Command.UserCommand;
PROCEDURE QueryParameter(name: TEXT; VAR com: CARDINAL; VAR ok: BOOLEAN);

PROCEDURE SetRoot(root: CARDINAL);
PROCEDURE SetMaxDegree(deg: CARDINAL);
PROCEDURE SetLevel(level: CARDINAL);
PROCEDURE SetNodes(nodes: CARDINAL);

END Panel.
