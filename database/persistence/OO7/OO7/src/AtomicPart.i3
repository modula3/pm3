INTERFACE AtomicPart;
IMPORT Refany;
FROM OO7 IMPORT AtomicPart;
TYPE T <: AtomicPart;
VAR
  RealWork := FALSE;			 (* set to make doNothing do work *)
  WorkAmount: INTEGER;			 (* amount of work to do *)
CONST
  Brand = "AtomicPart";
  Equal = Refany.Equal;
END AtomicPart.
