(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Sep 10 14:32:57 PDT 1993 by muller        *)

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
            flag: [16_0..16_FF];
            file: [16_0..16_FF]; END;

<*EXTERNAL "_iob"*> VAR iob: ARRAY [0..N_STATIC_IOBS - 1] OF iobuf;
<*EXTERNAL "_filbuf"*> PROCEDURE filbuf (f: UNTRACED REF iobuf): CHAR;
<*EXTERNAL "_flsbuf"*> PROCEDURE flsbuf (c: CHAR; f: UNTRACED REF iobuf);
<*EXTERNAL "ungetc"*>  PROCEDURE ungetc (c: CHAR; f: UNTRACED REF iobuf);
<*EXTERNAL "fflush"*> PROCEDURE flush (f: UNTRACED REF iobuf);

END Cstdio.

