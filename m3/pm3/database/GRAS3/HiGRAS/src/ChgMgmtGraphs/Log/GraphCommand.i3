INTERFACE GraphCommand;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.2  1998/05/19 10:17:45  roland
    Support for log-groups implemented.

    Revision 1.1  1997/04/23 13:32:43  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.2  1996/12/03 09:52:33  roland
    Bugfix. Undo and Redo for PutAttribute should now work properly.

    Revision 1.1  1996/09/17 12:57:12  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

(* This module provides information on how commands should be stored in a
   command stream.  It also coverts a command into an array of bytes and
   vice versa. *)

IMPORT Type, Node;

CONST Brand = "GraphCommand";

(**
   A command is stored in the following format:

   op{args}[(len|min.max).text]

   - op         is the kind of operation stored, length: one byte
   - args       numerical arguments for op; length: the number of
                arguments is determined by op, each argument represents
                a cardinal and hence is stored in four bytes
   - len        the length of the optional text; length: four bytes
   - min, max   start and end of the optional text; length: four bytes each
                NOTE: len and min,max are mutually exclusiv and op
                      determines which to choose.
                NOTE: text ranges from min to max-1. In this way it is
                      possible to have length 0 with max=min
   - text       the text argument to op; length: determined by len or
                max - min, depending on the actual format of the command.
**)

TYPE

  Operation = {NoOperation, CreateNode, DeleteNode, PutNodeLabel,
               PutAttribute, TruncateAttribute, PutIndex, DeleteIndex,
               CreateEdge, DeleteEdge, DeleteAttribute};
    (* The types of commands, that can be stored in a command stream.  To
       ensure compatibility, new commands should only be appended to the
       list, not inserted. *)

  (**
      Commands have the following arguments (node ids have two numbers):

      op                | args                  | len/min, max | text
      =====================================================================
      NoOperation       | -                     | -            | -		
      ---------------------------------------------------------------------
      CreateNode        | node id and label     | -            | -		
      ---------------------------------------------------------------------
      DeleteNode        | node id               | -            | -		
      ---------------------------------------------------------------------
      PutNodeLabel      | node id and label     | -            | -		
      ---------------------------------------------------------------------
      PutAttribute      | node and attr number  | min/max      | attr val	
      ---------------------------------------------------------------------
      TruncateAttribute | node, attr, lenght    | -            | -		
      ---------------------------------------------------------------------
      PutIndex          | node and index number | len          | index val	
      ---------------------------------------------------------------------
      DeleteIndex       | node and index number | len          | index val	
      ---------------------------------------------------------------------
      CreateEdge        | source, target, label | -            | -		
      ---------------------------------------------------------------------
      DeleteEdge        | source, target, label | -            | -		
      ---------------------------------------------------------------------
      DeleteAttribute   | node and attr number  | -            | -		
      ---------------------------------------------------------------------

  *)

  T = RECORD
        operation       : Operation;
        args            : ARRAY [0 .. 4] OF CARDINAL;
        text            : TEXT;
        length, from, to: CARDINAL;
      END;
    (* A Variable of type T can store all information concerning a command
       (what operation, arguments, texlength etc.) *)

(* Operations to fill T records *)
PROCEDURE CreateNode (VAR com: T; node: Node.T; label: CARDINAL);
PROCEDURE DeleteNode (VAR com: T; node: Node.T);
PROCEDURE PutNodeLabel (VAR com: T; node: Node.T; label: CARDINAL);
PROCEDURE PutAttribute (VAR com                  : T;
                            node                 : Node.T;
                            attrno, start, length: CARDINAL;
                            value                : TEXT      );
PROCEDURE TruncateAttribute (VAR com           : T;
                                 node          : Node.T;
                                 attrno, length: CARDINAL);
PROCEDURE DeleteAttribute (VAR com: T; node: Node.T; attrno: CARDINAL);
PROCEDURE PutIndex (VAR com            : T;
                        node           : Node.T;
                        indexno, length: CARDINAL;
                        value          : TEXT      );
PROCEDURE DeleteIndex (VAR com            : T;
                           node           : Node.T;
                           indexno, length: CARDINAL;
                           value          : TEXT      );
PROCEDURE CreateEdge (VAR com: T; source, target: Node.T; label: CARDINAL);
PROCEDURE DeleteEdge (VAR com: T; source, target: Node.T; label: CARDINAL);


PROCEDURE NumOfArgs (o: Operation): CARDINAL;
  (* Delivers the number of numerical arguments the operation requires.
     Extra parameters for operations with a text argument (length / start
     and end postion) are not included here. *)

PROCEDURE HasText (o: Operation): BOOLEAN;
  (* Is true if the operation requires a text argument (partial or not) *)

PROCEDURE HasPartialText (o: Operation): BOOLEAN;
  (* Is true if the operation requires a text argument and only a substring
     is stored *)

PROCEDURE ToByteArray (READONLY co : T;
                       VAR      len: CARDINAL;
                       VAR      ba : REF Type.ByteArray);
  (* Store all information about a command in an array of bytes ba.  ba can
     be easily transferred to physical storage.  The actual length of the
     stored information is returned in len.  If the array ba can not hold
     all information (NUMBER(ba) < len) then err will be TRUE and len holds
     the required length.  ba will not be modified in this case. *)

PROCEDURE FromByteArray (READONLY ba : Type.ByteArray;
                                  len: CARDINAL;
                         VAR      co : T;
                         VAR      ok : BOOLEAN         );
  (* Convert the information in ba to a command.  If the contents of ba do
     not represent a command ok will be FALSE. *)

(* The procedures Eq and Lt are used for generic instatiation of
   ListOfCommands *)
PROCEDURE Equal (READONLY a, b: T): BOOLEAN;
  (* a and b are equal iff their components are *)

PROCEDURE Compare (READONLY a, b: T): [-1 .. 1];
  (* a <= b iff a.operation <= b.operation *)

END GraphCommand.

