(*-------------------------------------------------------------------------
 *
 * libpq-fe.h--
 *    This file contains definitions for structures and
 *    externs for functions used by frontend postgres applications.
 *
 * Copyright (c) 1994, Regents of the University of California
 *
 * libpq-fe.h,v 1.6 1995/08/01 20:28:21 jolly Exp
 *
 *-------------------------------------------------------------------------
 *)

(* ----------------
 *	include stuff common to fe and be
 * ----------------
 *)

UNSAFE INTERFACE PQ;

IMPORT PQcomm;
FROM Ctypes IMPORT char_star, int, char, short, int_star;
FROM Postgres IMPORT NAMEDATALEN, Oid;


TYPE
  CONNECTION = { OK, BAD };
  ConnStatusType = CONNECTION;
  
CONST
  CONNECTION_OK: ConnStatusType = CONNECTION.OK;
  CONNECTION_BAD: ConnStatusType = CONNECTION.BAD;

TYPE
  ExecStatusType = PGRS;
  PGRS = {
       EMPTY_QUERY,
       COMMAND_OK,  (* a query command that doesn't return *)
                          (* anything was executed properly by the backend *)
       TUPLES_OK,   (* a query command that returns tuples *)
                          (* was executed properly by the backend, PGresult *)
                          (* contains the resulttuples *)
       COPY_OUT, 
       COPY_IN,
       BAD_RESPONSE,(* an unexpected response was recv'd *)
                          (* from the backend *)
       NONFATAL_ERROR,
       FATAL_ERROR
    };

CONST
  PGRS_EMPTY_QUERY: PGRS = PGRS.EMPTY_QUERY;
  PGRS_COMMAND_OK: PGRS = PGRS.COMMAND_OK; 
  PGRS_TUPLES_OK: PGRS = PGRS.TUPLES_OK;
  PGRS_COPY_OUT: PGRS = PGRS.COPY_OUT;
  PGRS_COPY_IN: PGRS = PGRS.COPY_IN;
  PGRS_BAD_RESPONSE: PGRS = PGRS.BAD_RESPONSE;
  PGRS_NONFATAL_ERROR: PGRS = PGRS.NONFATAL_ERROR;
  PGRS_FATAL_ERROR: PGRS = PGRS.FATAL_ERROR;
  

(* string descriptions of the ExecStatusTypes *)
<*EXTERNAL*> VAR 
  pgresStatus: ARRAY PGRS OF char_star;

(* 
 * POSTGRES backend dependent Constants. 
 *)

(* ERROR_MSG_LENGTH should really be the same as ELOG_MAXLEN in utils/elog.h*)

CONST ERROR_MSG_LENGTH = 4096;
CONST COMMAND_LENGTH = 20;
CONST REMARK_LENGTH = 80;
CONST PORTAL_NAME_LENGTH = 16;

(* ----------------
 * PQArgBlock --
 *	Information (pointer to array of this structure) required
 *	for the PQfn() call.
 * ----------------
 *)

TYPE
  PQArgBlock = RECORD
    len: int;
    isint: int;  (* Q: Should this be BOOLEAN? *)
    u: int;  
  END;
  PQArgBlock_star = UNTRACED REF PQArgBlock;

  PGresAttDesc = RECORD
    name: char_star;   (* type name *)
    adtid: Oid;        (* type id *)
    adtsize: int2;     (* type size *)
  END;
  PGresAttDesc_star = UNTRACED REF PGresAttDesc;
  
(* use char* for Attribute values, ASCII tuples are guaranteed to be
   null-terminated For binary tuples, the first four bytes of the
   value is the size, and the bytes afterwards are the value.  The
   binary value is not guaranteed to be null-terminated.  In fact, it
   can have embedded nulls. 
*)

TYPE
  PGresAttValue = RECORD
    len: int;
    value: char_star;
  END;
  PGresAttValue_star = UNTRACED REF PGresAttValue;

  PGnotify = RECORD
    relname: ARRAY [0..NAMEDATALEN-1] OF char;
    be_pid: int;
  END;
  PGnotify_star = UNTRACED REF PGnotify;

(* PGconn encapsulates a connection to the backend *)
TYPE
  PGconn = RECORD
    pghost    : char_star; (* the machine on which the server is running *)
    pgtty     : char_star; (* tty on which the backend messages is displayed *)
    pgport    : char_star; (* the communication port with the backend *)
    pgoptions : char_star; (* options to start the backend with *)
    dbName    : char_star; (* database name *)
    status    : ConnStatusType;
    errorMessage: ARRAY [0..ERROR_MSG_LENGTH-1] OF char;
    
    (* pipes for be/fe communication *)
    Pfin      : FILE;
    Pfout     : FILE;
    Pfdebug   : FILE;
    port      : Port;
    asyncNotifyWaiting: int;
    notifyList: Dllist; (* was Dllist *)
  END;
  PGconn_star = UNTRACED REF PGconn;

CONST CMDSTATUS_LEN = 40;

(* PGresult encapsulates the result of a query *)
(* unlike the old libpq, we assume that queries only return in one group *)

TYPE
  PGresult = RECORD
    ntups: int;
    numAttributes: int;
    attDescs: PGresAttDesc_star;
    tuples: PGresAttValue_star; 
                          (* each PGresTuple is an array of PGresAttValue's *)
    tupArrSize: int;      (* size of tuples array allocated *)
    resultStatus: ExecStatusType;
    cmdStatus: ARRAY [0..CMDSTATUS_LEN-1] OF char; 
                             (* cmd status from the last insert query*)
    binary: int;          (* binary tuple values if binary == 1, 
                             otherwise ASCII *)
    conn: PGconn;
  END;
  PGresult_star = UNTRACED REF PGresult;

<*EXTERNAL*> PROCEDURE PQsetdb (pghost: char_star;
                                pgport: char_star; 
                                pgoptions: char_star;
                                pgtty: char_star;
                                dbName: char_star): PGconn_star;
  (* Make a new client connection to the backend *)

<*EXTERNAL*> PROCEDURE PQfinish (conn: PGconn_star);
  (* close the current connection and free the PGconn data structure 
     parameters *)
<*EXTERNAL*> PROCEDURE PQreset (conn: PGconn_star);
  (* close the current connection and restablish a new one with the same 
     parameters *)

<*EXTERNAL*> PROCEDURE PQdb(conn: PGconn_star): char_star;
<*EXTERNAL*> PROCEDURE PQhost(conn: PGconn_star): char_star;
<*EXTERNAL*> PROCEDURE PQoptions(conn: PGconn_star): char_star;
<*EXTERNAL*> PROCEDURE PQport(conn: PGconn_star): char_star;
<*EXTERNAL*> PROCEDURE PQtty(conn: PGconn_star): char_star;
<*EXTERNAL*> PROCEDURE PQstatus (conn: PGconn_star): ConnStatusType;
<*EXTERNAL*> PROCEDURE PQerrorMessage(conn: PGconn_star): char_star;
<*EXTERNAL*> PROCEDURE PQtrace (conn: PGconn; debug_port: FILE);
<*EXTERNAL*> PROCEDURE PQuntrace (conn: PGconn);

(* === in fe-exec.c === *)
<*EXTERNAL*> PROCEDURE PQexec (conn: PGconn_star;  query: char_star): PGresult_star;
<*EXTERNAL*> PROCEDURE PQgetline (conn: PGconn; 
                                  string: char_star; 
                                  length: int): int;
<*EXTERNAL*> PROCEDURE PQendcopy (conn: PGconn): int;
<*EXTERNAL*> PROCEDURE PQputline (conn: PGconn;  string: char_star);
<*EXTERNAL*> PROCEDURE PQresultStatus (res: PGresult_star): ExecStatusType;
<*EXTERNAL*> PROCEDURE PQntuples (res: PGresult_star): int;
<*EXTERNAL*> PROCEDURE PQnfields (res: PGresult_star): int;
<*EXTERNAL*> PROCEDURE PQfname (res: PGresult_star; field_num: int): char_star;
<*EXTERNAL*> PROCEDURE PQfnumber (res: PGresult_star;  field_name: char_star): int;
<*EXTERNAL*> PROCEDURE PQftype (res: PGresult_star; field_num: int): Oid;
<*EXTERNAL*> PROCEDURE PQfsize (res: PGresult_star; field_num: int): int2;
<*EXTERNAL*> PROCEDURE PQcmdStatus (res: PGresult_star): char_star;
<*EXTERNAL*> PROCEDURE PQoidStatus (res: PGresult_star): char_star;
<*EXTERNAL*> PROCEDURE PQgetvalue (res: PGresult_star; 
                                   tup_num: int; 
                                   field_num: int): char_star;

<*EXTERNAL*> PROCEDURE PQgetlength(res: PGresult_star; 
                                   tup_num: int; 
                                   field_num: int): int;

<*EXTERNAL*> PROCEDURE PQclear(res: PGresult_star);

<*EXTERNAL*> PROCEDURE PQprintTuples(res: PGresult_star; 
			  fout: FILE;      (* output stream *)
			  printAttName: int;(* print attribute names or not*)
			  terseOutput: int; (* delimiter bars or not?*)
			  width: int        (* width of column; 
					      if 0; use variable width *)
			  );
<*EXTERNAL*> PROCEDURE PQnotifies(conn: PGconn): PGnotify_star;
<*EXTERNAL*> PROCEDURE PQfn(conn: PGconn_star;
                            fnid: int; 
                            result_buf: int_star; 
                            result_len: int_star;
                            result_is_int: int;
                            args: PQArgBlock_star;
                            nargs: int): PGresult_star;
(* === in fe-auth.c === *)
<*EXTERNAL*> PROCEDURE fe_getauthsvc( PQerrormsg: char_star): PQcomm.MsgType ;
<*EXTERNAL*> PROCEDURE fe_setauthsvc(name: char_star;  PQerrormsg: char_star);
<*EXTERNAL*> PROCEDURE fe_getauthname ( PQerrormsg: char_star): char_star;

(* pqGets and pqPuts gets and sends strings to the file stream returns
   0 if successful if debug is non-null; debugging output is sent to
   that stream. *)

<*EXTERNAL*> PROCEDURE pqGets( s: char_star; maxlen: int; 
                              stream: FILE; debug: FILE): int ;

<*EXTERNAL*> PROCEDURE pqGetnchar(s: char_star; maxlen: int; 
                                  stream: FILE; 
                                  debug: FILE): int;

<*EXTERNAL*> PROCEDURE pqPutnchar( s: char_star; maxlen: int; 
                                   stream: FILE; 
                                   debug: FILE): int;
<*EXTERNAL*> PROCEDURE pqPuts( s: char_star; 
                              stream: FILE; debug: FILE ): int;

<*EXTERNAL*> PROCEDURE pqGetc(stream: FILE; debug: FILE): int;
(* get a n-byte integer from the stream into result *)
(* returns 0 if successful *)

<*EXTERNAL*> PROCEDURE pqGetInt(result: int_star; 
                                bytes: int; 
                                stream: FILE; 
                                debug: FILE): int;

(* put a n-byte integer into the stream *)
(* returns 0 if successful *)
<*EXTERNAL*> PROCEDURE pqPutInt(n: int; 
                                bytes: int; 
                                stream: FILE; 
                                debug: FILE ): int;

<*EXTERNAL*> PROCEDURE pqFlush(stream: FILE; 
                               debug: FILE);

<*EXTERNAL*> PROCEDURE lo_open(conn: PGconn; 
                               lobjId: Oid; 
                               mode: int): PGconn;

<*EXTERNAL*> PROCEDURE lo_close(conn: PGconn; fd: int): int;
<*EXTERNAL*> PROCEDURE lo_read(conn: PGconn; 
                               fd: int; 
                               buf: char_star; 
                               len: int): int;
<*EXTERNAL*> PROCEDURE lo_write(conn: PGconn; 
                                fd: int; 
                                buf: char_star; 
                                len: int): int;

<*EXTERNAL*> PROCEDURE lo_lseek(conn: PGconn; 
                                fd: int; 
                                offset: int; 
                                whence: int): int;

<*EXTERNAL*> PROCEDURE lo_creat(conn: PGconn; 
                                mode: int): Oid;

<*EXTERNAL*> PROCEDURE lo_tell(conn: PGconn; 
                               fd: int): int;

<*EXTERNAL*> PROCEDURE lo_unlink(conn: PGconn; 
                                 lobjId: Oid): int;

<*EXTERNAL*> PROCEDURE lo_import(conn: PGconn; 
                                 filename: char_star): Oid;

<*EXTERNAL*> PROCEDURE lo_export(conn: PGconn; 
                                 lobjId: Oid; 
                                 filename: char_star): int;

(* max length of message to send  *)
CONST MAX_MESSAGE_LEN = 8193;

(* maximum number of fields in a tuple *)
CONST BYTELEN = 8;
CONST MAX_FIELDS = 512;

TYPE
  TUPLE = ADDRESS;
  FILE = ADDRESS;
  Dllist = ADDRESS;
  Port = ADDRESS;
  int2 = short;
  
END PQ.
