INTERFACE BldFace;

IMPORT M3Driver, BldQuake;

EXCEPTION Error;

TYPE
  T <: Public;
  Public = M3Driver.Interface OBJECT
  METHODS
    init(m: BldQuake.T): T RAISES {Error};
  END;

END BldFace.
