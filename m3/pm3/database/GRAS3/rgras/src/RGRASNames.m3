MODULE RGRASNames;

(***************************************************************************)
(** Created by:  Rene Huelswitt						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:44  hosking
    Initial revision

    Revision 1.1  1997/10/24 14:39:14  renehuel
    These files implement the new RGRASGraph interface.

*)
(***************************************************************************)

IMPORT ChgMgmtGraphPool, Pathname, Access, Names, ErrorSupport,
       AttributeValue;
IMPORT TypedNames AS Super;

REVEAL
  T = Public BRANDED OBJECT
        counterAttribute: CARDINAL;
      OVERRIDES
        getUniqueId := GetUniqueId;
        login       := Login
      END;

CONST counterAttributeName = "RGRASUniqueCounter";

PROCEDURE GetUniqueId (self: T): CARDINAL
  RAISES {InternalError, Access.Locked, Names.Undeclared, Names.Unknown} =
  VAR value: TEXT;
  BEGIN
    TRY
      value := self.getCollectionAttribute(self.counterAttribute);
      IF value = NIL THEN
        value := AttributeValue.CardToText(1);
      ELSE
        value :=
          AttributeValue.CardToText(AttributeValue.TextToCard(value) + 1);
      END;
      self.setCollectionAttribute(self.counterAttribute, value);
    EXCEPT
    | Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "RGRASNames.GetUniqueId", "Names.InternalError", info));
    END;
    RETURN AttributeValue.TextToCard(value) - 1
  END GetUniqueId;

PROCEDURE Login (self: T; pool: ChgMgmtGraphPool.T; collection: Pathname.T)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY     
      Super.T.login(self, pool, collection);
      self.counterAttribute :=
        self.declareCollectionAttribute(counterAttributeName);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "RGRASNames.Login", "TypedNames.InternalError", info));
    | Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "RGRASNames.Login", "Names.InternalError", info));
    END;
  END Login;

BEGIN
END RGRASNames.
