(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* File: Uutmp.i3                                             *)
(* Last modified on Mon Apr 16 15:59:54 1990 by jerome        *)

INTERFACE Uutmp;

FROM Ctypes IMPORT char, char_star, short, int, long;
FROM Utypes IMPORT pid_t;

(*** <utmp.h> ***)

(*
 * Structure of utmp and wtmp files.
 *
 *)

TYPE
    struct_utmp_star = UNTRACED REF struct_utmp;

    struct_utmp = RECORD
      ut_user : ARRAY [0..63] OF char;  (* user login name *)
      ut_id   : ARRAY [0..13] OF char;  (* inittab id *)
      ut_line : ARRAY [0..31] OF char;  (* device name *)
      ut_type : short;                  (* type of entry *)
      ut_pid  : pid_t;                  (* process ID *)
      ut_exit : RECORD                  (* exit status of a DEAD_PROCESS *)
	e_termination : short;		(* process termination status *)
	e_exit        : short;          (* process exit status *)
      END;
      ut_tv   : RECORD                  (* time entry was made *)
        tv_sec   : long;                (* seconds *)
        tv_usec  : int;                 (* microseconds *)
        reserved : int;
      END;
      ut_host : ARRAY [0..255] OF char; (* host name *)
    END;

CONST
   UTMP_FILE	= "/var/adm/utmp";
   WTMP_FILE	= "/var/adm/wtmp";

(*	Special strings or formats used in the "ut_line" field when	*)
(*	accounting for something other than a process.			*)
   RUNLVL_MSG   = "run-level %c";
   BOOT_MSG     = "system boot";
   OTIME_MSG    = "old time";
   NTIME_MSG    = "new time";


(*** getlogin(3) ***)

<*EXTERNAL*>
PROCEDURE getlogin (): char_star;

<*EXTERNAL "_Egetutent"*>
PROCEDURE getutent(): struct_utmp_star;

<*EXTERNAL "_Egetutid"*>
PROCEDURE getutid(id: struct_utmp_star): struct_utmp_star;

<*EXTERNAL "_Egetutline"*>
PROCEDURE getutline(line: struct_utmp_star): struct_utmp_star;

<*EXTERNAL "_Epututline"*>
PROCEDURE pututline(utmp_ptr: struct_utmp_star): struct_utmp_star;

<*EXTERNAL*>
PROCEDURE setutent();

<*EXTERNAL*>
PROCEDURE endutent();

END Uutmp.
