INTERFACE BldQuake;

IMPORT QMachine, Wr, Pathname;
FROM Quake IMPORT Error;

TYPE 
  T <: Public;
  Public = QMachine.T OBJECT
  METHODS
    init(wr: Wr.T; package: TEXT; package_dir: Pathname.T;
         build_dir: Pathname.T): T RAISES {Error};
    setup() RAISES {Error};
    getsl(): TEXT;  (* call t.setup() before using this *)
  END;

PROCEDURE CopyIfNew (t: T; src, dest: TEXT) RAISES {Error};

END BldQuake.
