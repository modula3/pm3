INTERFACE CRandom;

IMPORT Ctypes;

<*EXTERNAL*> PROCEDURE random(): Ctypes.int;
<*EXTERNAL*> PROCEDURE srandom(seed: Ctypes.unsigned_int);
<*EXTERNAL*> PROCEDURE initstate(seed: Ctypes.unsigned_int;
                                 state: Ctypes.void_star;
                                 size: Ctypes.int): Ctypes.void_star;
<*EXTERNAL*> PROCEDURE setstate(state: Ctypes.void_star): Ctypes.void_star;

PROCEDURE State(VAR state: ARRAY[1..256] OF CHAR): ADDRESS;

END CRandom.
