INTERFACE InternScheme;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:34  hosking
    Initial revision

    Revision 1.2  1997/05/05 10:50:38  roland
    Bugfixes in open routines for schemes.
    Dependency information moved from intern to public interface.

    Revision 1.1  1997/05/01 13:23:11  roland
    TypedGraph layer adapted to graph boundary crossing edges.

*)
(***************************************************************************)

IMPORT Scheme;

REVEAL Scheme.T <: Intern;

TYPE
  Intern =
    Scheme.Public OBJECT
    METHODS
      isGuardedAttribute (attr: Scheme.ID): BOOLEAN
                          RAISES {Scheme.NotDeclared, Scheme.InternalError};
                          (* An attribute is guarded, if it is an index
                             attribute or a non-intrinsic key attribute.
                             Guarded attributes must be marked as invalid
                             and evaluated before any index query. *)

      getGuard (attr: Scheme.ID): Scheme.ID
                RAISES {Scheme.NotDeclared, Scheme.InternalError};
                (* If attr is guarded, the number returned is that of a
                   reserved guard attribute.  This can be used to mark
                   guarded attributes as valid or invalid. *)

      getGuardsOfNodeClassOrType (CoT: Scheme.ID): Scheme.IDSet
                                  RAISES {Scheme.NotDeclared,
                                          Scheme.InternalError};
                                  (* Returns the guards of all guarded
                                     attributes of CoT *)

      getDependingOnAttribute (attr: Scheme.ID): Scheme.IDSet
                               RAISES {Scheme.NotDeclared,
                                       Scheme.InternalError};
                               (* Purpose: Return all static dependencies
                                  that depend on attribute attr. *)

      getDependingOnEdgeType (edgeType: Scheme.ID; incoming: BOOLEAN):
                              Scheme.IDSet RAISES {Scheme.NotDeclared,
                                                   Scheme.InternalError};
                              (* Purpose: Return all static dependencies
                                 that propagate via an incoming/outgoing
                                 edge of type edgeType. *)

      getDependingOnClassOrType (CoT: Scheme.ID): Scheme.IDSet
                                 RAISES {Scheme.NotDeclared,
                                         Scheme.InternalError};
                                 (* Purpose: Return all static dependencies
                                    that depend on an attribute of
                                    class/type CoT. *)

    END;

END InternScheme.
