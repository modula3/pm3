(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Tue Jun 27 13:59:11 PDT 1995 by steveg *)

INTERFACE TCPExtras;

IMPORT Ctypes, IP, TCP;

EXCEPTION Failure;

(* return the local endpoint of a TCP connection *)
PROCEDURE LocalEndpoint(conn: TCP.T): IP.Endpoint RAISES {Failure};

PROCEDURE htons(s: Ctypes.unsigned_short_int): Ctypes.unsigned_short_int;
PROCEDURE ntohs(s: Ctypes.unsigned_short_int): Ctypes.unsigned_short_int;

END TCPExtras.
