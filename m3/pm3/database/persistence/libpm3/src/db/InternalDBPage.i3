INTERFACE InternalDBPage;

IMPORT BaseDBPage, DBPage;

REVEAL
  DBPage.T <: Internal;

TYPE
  Internal = BaseDBPage.T OBJECT
  END;

END InternalDBPage.
