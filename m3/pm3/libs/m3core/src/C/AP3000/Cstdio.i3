(* Copyright (C) 1990, 1992, Digital Equipment Corporation.                  *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Thu Mar 05 14:51:28 PST 1992 by muller                   *)

INTERFACE Cstdio;

CONST 
  N_STATIC_IOBS = 3;
  IOEOF  = 8_20;

TYPE
  Base = [2..16];
  iobuf = RECORD 
            cnt: INTEGER;
            ptr: UNTRACED REF CHAR;
            base: UNTRACED REF CHAR;
            bufsiz: INTEGER;
            flag: [0..16_FFFF];
            file: [0..16_FF]; END;

<*EXTERNAL "_iob"*> VAR iob: ARRAY [0..N_STATIC_IOBS - 1] OF iobuf;
<*EXTERNAL "_flsbuf"*> PROCEDURE flsbuf (c: CHAR; f: UNTRACED REF iobuf);
<*EXTERNAL "_filbuf"*> PROCEDURE filbuf (f: UNTRACED REF iobuf): CHAR;
<*EXTERNAL "ungetc"*>  PROCEDURE ungetc (c: CHAR; f: UNTRACED REF iobuf);
<*EXTERNAL "fflush"*> PROCEDURE flush (f: UNTRACED REF iobuf);

END Cstdio.

