(*! DATA TYPE MODULE *)
INTERFACE ContextSet;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:50  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:40  hosking
    Import of GRAS3 1.1

    Revision 1.1  1997/10/31 14:06:13  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. RuleTypes and EventTypes
    subsystems implement basic types of the rule engine.

*)
(***************************************************************************)

IMPORT Word, TextTransientSeq AS TextSeq;

TYPE
  T = Word.T;                    (* This is opaque ! *)
  Index = [0 .. BITSIZE(T) - 1];

EXCEPTION
  Unknown;
  
PROCEDURE DeclareContext (name: TEXT): Index;

PROCEDURE ContextIndex(name: TEXT): Index RAISES {Unknown};
PROCEDURE ContextName (context: Index): TEXT RAISES {Unknown};
PROCEDURE ExistsContextByName (name: TEXT): BOOLEAN;
PROCEDURE ExistsContext (context: Index): BOOLEAN;

PROCEDURE Empty (): T;
PROCEDURE Insert (cs: T; context: Index): T;
PROCEDURE Remove (cs: T; context: Index): T;
PROCEDURE Contains (cs: T; context: Index): BOOLEAN;

PROCEDURE FromSeq (s: TextSeq.T): T RAISES {Unknown};
PROCEDURE ToSeq (cs: T): TextSeq.T;

PROCEDURE Inhibits (inh, act: T): BOOLEAN;
  (* Context inh is inhibiting for context act, iff their intersection is
     not empty, i.e. if any context of inh is also in act *)
PROCEDURE Permits (perm, act: T): BOOLEAN;
  (* Context perm is permitting for context act, iff perm is a subset of
     act, i.e. if all context of perm are in act. *)

END ContextSet.
