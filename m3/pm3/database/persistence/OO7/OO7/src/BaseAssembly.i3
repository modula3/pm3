INTERFACE BaseAssembly;
IMPORT Refany;
FROM OO7 IMPORT BaseAssembly;
TYPE T <: BaseAssembly;
CONST
  Brand = "BaseAssembly";
  Equal = Refany.Equal;
END BaseAssembly.
