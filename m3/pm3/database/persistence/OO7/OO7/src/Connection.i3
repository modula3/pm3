INTERFACE Connection;
IMPORT Refany;
FROM OO7 IMPORT Connection;
TYPE T <: Connection;
CONST
  Brand = "Connection";
  Equal = Refany.Equal;
END Connection.
