MODULE LogContexts;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.1  1997/12/02 17:56:38  roland
    New event types and event contexts for user recovery operations
    introduced.

*)
(***************************************************************************)

IMPORT RuleEngine, ContextSet;

VAR context: ARRAY Context OF ContextSet.Index;

PROCEDURE Set(unit: CARDINAL; c: Context) =
  BEGIN
    RuleEngine.ActivateContext(unit, context[c]);
  END Set;
    
PROCEDURE Clear(unit: CARDINAL; c: Context) =
  BEGIN
    RuleEngine.DeactivateContext(unit, context[c]);
  END Clear; 
    
BEGIN
  (* declare contexts to RuleEngine *)
  FOR c := FIRST(Context) TO LAST(Context) DO
    context[c] := ContextSet.DeclareContext(Names[c]);
  END;
END LogContexts.
