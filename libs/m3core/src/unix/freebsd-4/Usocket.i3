(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk     *)
(*      modified on Fri Apr 30 14:46:21 PDT 1993 by muller     *)
(*      modified on Wed Jul 11  9:34:54 PDT 1990 by mjordan    *)

INTERFACE Usocket;


IMPORT Ctypes, Utypes, Uuio;

(*** sys/socket.h ***)

(*
 * Definitions related to sockets: types, address families, options.
 *)

(*
 * Types
 *)
CONST
  SOCK_STREAM    = 1;            (* stream socket *)
  SOCK_DGRAM     = 2;            (* datagram socket *)
  SOCK_RAW       = 3;            (* raw-protocol interface *)
  SOCK_RDM       = 4;            (* reliably-delivered message *)
  SOCK_SEQPACKET = 5;            (* sequenced packet stream *)

(*
 * Option flags per-socket.
 *)
  SO_DEBUG       = 16_0001;      (* turn on debugging info recording *)
  SO_ACCEPTCONN  = 16_0002;      (* socket has had listen() *)
  SO_REUSEADDR   = 16_0004;      (* allow local address reuse *)
  SO_KEEPALIVE   = 16_0008;      (* keep connections alive *)
  SO_DONTROUTE   = 16_0010;      (* just use interface addresses *)
  SO_BROADCAST   = 16_0020;      (* permit sending of broadcast msgs *)
  SO_USELOOPBACK = 16_0040;      (* bypass hardware when possible *)
  SO_LINGER      = 16_0080;      (* linger on close if data present *)
  SO_OOBINLINE   = 16_0100;      (* leave received OOB data in line *)
  SO_REUSEPORT   = 16_0200;      (* allow local address & port reuse *)
  SO_TIMESTAMP   = 16_0400;      (* timestamp received dgram traffic *)

(*
 * Additional options, not kept in so_options.
 *)
  SO_SNDBUF      = 16_1001;      (* send buffer size *)
  SO_RCVBUF      = 16_1002;      (* receive buffer size *)
  SO_SNDLOWAT    = 16_1003;      (* send low-water mark *)
  SO_RCVLOWAT    = 16_1004;      (* receive low-water mark *)
  SO_SNDTIMEO    = 16_1005;      (* send timeout *)
  SO_RCVTIMEO    = 16_1006;      (* receive timeout *)
  SO_ERROR       = 16_1007;      (* get error status and clear *)
  SO_TYPE        = 16_1008;      (* get socket type *)
  SO_PRIVSTATE   = 16_1009;      (* get/deny privileged state *)
  
(*
 * Structure used for manipulating linger option.
 *)
TYPE
  struct_linger = RECORD
    l_onoff: Ctypes.int;		(* option on/off *)
    l_linger: Ctypes.int;		(* linger time *)
  END;


(*
 * Level number for (get/set)sockopt() to apply to socket itself.
 *)
CONST
  SOL_SOCKET     = 16_ffff;      (* options for socket level *)


(*
 * Address families.
 *)
  AF_UNSPEC       = 0;             (* unspecified *)
  AF_LOCAL        = 1;             (* local to host (pipes, portals) *)
  AF_UNIX         = AF_LOCAL;      (* backward compatibility *)
  AF_INET         = 2;             (* internetwork: UDP, TCP, etc. *)
  AF_IMPLINK      = 3;             (* arpanet imp addresses *)
  AF_PUP          = 4;             (* pup protocols: e.g. BSP *)
  AF_CHAOS        = 5;             (* mit CHAOS protocols *)
  AF_NS           = 6;             (* XEROX NS protocols *)
  AF_ISO          = 7;             (* ISO protocols *)
  AF_OSI          = AF_ISO;
  AF_ECMA         = 8;             (* European computer manufacturers *)
  AF_DATAKIT      = 9;             (* datakit protocols *)
  AF_CCITT        = 10;            (* CCITT protocols, X.25 etc *)
  AF_SNA          = 11;            (* IBM SNA *)
  AF_DECnet       = 12;            (* DECnet *)
  AF_DLI          = 13;            (* DEC Direct data link interface *)
  AF_LAT          = 14;            (* LAT *)
  AF_HYLINK       = 15;            (* NSC Hyperchannel *)
  AF_APPLETALK    = 16;            (* Apple Talk *)
  AF_ROUTE        = 17;            (* Internal Routing Protocol *)
  AF_LINK         = 18;            (* Link layer interface *)
  pseudo_AF_XTP   = 19;            (* eXpress Transfer Protocol (no AF) *)
  AF_COIP         = 20;            (* connection-oriented IP, aka ST II *)
  AF_CNT          = 21;            (* Computer Network Technology *)
  pseudo_AF_RTIP  = 22;            (* Help Identify RTIP packets *)
  AF_IPX          = 23;            (* Novell Internet Protocol *)
  AF_SIP          = 24;            (* Simple Internet Protocol *)
  pseudo_AF_PIP   = 25;            (* Help Identify PIP packets *)
  AF_ISDN         = 26;            (* Integrated Services Digital Network*)
  AF_E164         = AF_ISDN;       (* CCITT E.164 recommendation *)
  pseudo_AF_KEY   = 27;            (* Internal key-management function *)

  AF_MAX          = 28;

(*
 * Structure used by kernel to store most
 * addresses.
 *)
TYPE
  struct_sockaddr = RECORD
    sa_len: Ctypes.unsigned_char;            (* total length *)
    sa_family: Ctypes.unsigned_char;         (* address family *)
    sa_data: ARRAY [0..13] OF Ctypes.char;   (* address; actually longer *)
  END;


(*
 * Structure used by kernel to pass protocol
 * information in raw sockets.
 *)
  struct_sockproto = RECORD
    sp_family: Ctypes.unsigned_short;        (* address family *)
    sp_protocol: Ctypes.unsigned_short;      (* protocol *)
  END;

(*
 * Protocol families, same as address families for now.
 *)
CONST
  PF_UNSPEC       = AF_UNSPEC;
  PF_LOCAL        = AF_LOCAL;
  PF_UNIX         = PF_LOCAL;      (* backward compatibility *)
  PF_INET         = AF_INET;
  PF_IMPLINK      = AF_IMPLINK;
  PF_PUP          = AF_PUP;
  PF_CHAOS        = AF_CHAOS;
  PF_NS           = AF_NS;
  PF_ISO          = AF_ISO;
  PF_OSI          = AF_ISO;
  PF_ECMA         = AF_ECMA;
  PF_DATAKIT      = AF_DATAKIT;
  PF_CCITT        = AF_CCITT;
  PF_SNA          = AF_SNA;
  PF_DECnet       = AF_DECnet;
  PF_DLI          = AF_DLI;
  PF_LAT          = AF_LAT;
  PF_HYLINK       = AF_HYLINK;
  PF_APPLETALK    = AF_APPLETALK;
  PF_ROUTE        = AF_ROUTE;
  PF_LINK         = AF_LINK;
  PF_XTP          = pseudo_AF_XTP; (* really just proto family, no AF *)
  PF_COIP         = AF_COIP;
  PF_CNT          = AF_CNT;
  PF_SIP          = AF_SIP;
  PF_IPX          = AF_IPX;                (* same format as AF_NS *)
  PF_RTIP         = pseudo_AF_RTIP;        (* same format as AF_INET *)
  PF_PIP          = pseudo_AF_PIP;
  PF_ISDN         = AF_ISDN;
  PF_KEY          = pseudo_AF_KEY;

  PF_MAX          = AF_MAX;

(*
 * Maximum queue length specifiable by listen.
 *)
  SOMAXCONN      = 128;

(*
 * Message header for recvmsg and sendmsg calls.
 *)
TYPE
  struct_msghdr = RECORD
    msg_name: Utypes.caddr_t;            (* optional address *)
    msg_namelen: Ctypes.unsigned_int;    (* size of address *)
    msg_iov: Uuio.struct_iovec_star;     (* scatter/gather array *)
    msg_iovlen: Ctypes.unsigned_int;     (* # elements in msg_iov *)
    msg_control: Utypes.caddr_t;         (* ancillary data, see below *)
    msg_controllen: Ctypes.unsigned_int; (* ancillary data buffer len *)
    msg_flags: Ctypes.int;               (* flags on received message *)
  END;


CONST
  MSG_OOB         = 16_1;             (* process out-of-band data *)
  MSG_PEEK        = 16_2;             (* peek at incoming message *)
  MSG_DONTROUTE   = 16_4;             (* send without using routing tables *)
  MSG_EOR         = 16_8;             (* data completes record *)
  MSG_TRUNC       = 16_10;            (* data discarded before delivery *)
  MSG_CTRUNC      = 16_20;            (* control data lost before delivery *)
  MSG_WAITALL     = 16_40;            (* wait for full request or error *)
  MSG_DONTWAIT    = 16_80;            (* this message should be nonblocking *)
  MSG_EOF         = 16_100;           (* data completes connection *)
  MSG_COMPAT      = 16_8000;          (* used in sendit() *)

(*
 * Header for ancillary data objects in msg_control buffer.
 * Used for additional information with/about a datagram
 * not expressible by flags.  The format is a sequence
 * of message elements headed by cmsghdr structures.
 *)
TYPE
  struct_cmsghdr = RECORD
    cmsg_len: Ctypes.unsigned_int;  (* data byte count, including hdr *)
    cmsg_level: Ctypes.int;         (* originating protocol *)
    cmsg_type: Ctypes.int;          (* protocol-specific type *)
    (* followed by  u_char cmsg_data[]; *)
  END;

(* "Socket"-level control message types: *)
CONST
  SCM_RIGHTS    = 16_01;            (* access rights (array of int) *)
  SCM_TIMESTAMP = 16_02;            (* timestamp (struct timeval) *)

(*
 * Definitions for UNIX IPC domain.
 *)
TYPE
  struct_sockaddr_un = RECORD
    sun_len: Ctypes.unsigned_char;            (* sockaddr len including null *)
    sun_family: Ctypes.unsigned_char;         (* AF_UNIX *)
    sun_path: ARRAY [0..103] OF Ctypes.char;  (* path name (gag) *)
  END;

<*EXTERNAL "m3_accept"*>
PROCEDURE accept(
    s: Ctypes.int;
    addr: UNTRACED REF struct_sockaddr;
    addrlen: Ctypes.int_star)
    : Ctypes.int
    RAISES {};

<*EXTERNAL "m3_bind"*>
PROCEDURE bind(
    s: Ctypes.int;
    name: UNTRACED REF struct_sockaddr;
    namelen: Ctypes.int)
    : Ctypes.int
    RAISES {};

<*EXTERNAL "m3_connect"*>
PROCEDURE connect(
    s: Ctypes.int;
    name: UNTRACED REF struct_sockaddr;
    namelen: Ctypes.int)
    : Ctypes.int
    RAISES {};

<*EXTERNAL "m3_getpeername"*>
PROCEDURE getpeername(
    s: Ctypes.int;
    name: UNTRACED REF struct_sockaddr;
    namelen: Ctypes.int_star)
    : Ctypes.int
    RAISES {};

<*EXTERNAL "m3_getsockname"*>
PROCEDURE getsockname(
    s: Ctypes.int;
    name: UNTRACED REF struct_sockaddr;
    namelen: Ctypes.int_star)
    : Ctypes.int
    RAISES {};

<*EXTERNAL*>
PROCEDURE getsockopt(
    s, level, optname: Ctypes.int;
    optval: Ctypes.void_star;
    optlen: Ctypes.int_star)
    : Ctypes.int
    RAISES {};

<*EXTERNAL "m3_listen"*>
PROCEDURE listen(s, backlog: Ctypes.int): Ctypes.int RAISES {};

<*EXTERNAL "m3_recv"*>
PROCEDURE recv(
    s: Ctypes.int;
    buf: Ctypes.void_star;
    len: Utypes.size_t;
    flags: Ctypes.int)
    : Ctypes.int
    RAISES {};

<*EXTERNAL "m3_recvfrom"*>
PROCEDURE recvfrom(
    s: Ctypes.int;
    buf: Ctypes.void_star;
    len: Utypes.size_t;
    flags: Ctypes.int;
    from: UNTRACED REF struct_sockaddr;
    fromlen: Ctypes.int_star)
    : Ctypes.int
    RAISES {};

(* FIXME - recvmsg *)

<*EXTERNAL "m3_send"*>
PROCEDURE send(
    s: Ctypes.int;
    msg: Ctypes.const_void_star;
    len: Utypes.size_t;
    flags: Ctypes.int)
    : Ctypes.int
    RAISES {};

<*EXTERNAL "m3_sendto"*>
PROCEDURE sendto(
    s: Ctypes.int;
    msg: Ctypes.const_void_star;
    len: Utypes.size_t;
    flags: Ctypes.int;
    to: UNTRACED REF struct_sockaddr;
    tolen: Ctypes.int)
    : Ctypes.int
    RAISES {};

(* FIXME - sendmsg *)

<*EXTERNAL*>
PROCEDURE setsockopt(
    s, level, optname: Ctypes.int;
    optval: Ctypes.const_void_star;
    optlen: Ctypes.int)
    : Ctypes.int
    RAISES {};

<*EXTERNAL "m3_shutdown"*>
PROCEDURE shutdown(s, how: Ctypes.int): Ctypes.int RAISES {};

<*EXTERNAL "m3_socket" *>
PROCEDURE socket(af, type, protocol: Ctypes.int): Ctypes.int RAISES {};

<*EXTERNAL*>
PROCEDURE socketpair(
    d, type, protocol: Ctypes.int;
    sv: UNTRACED REF ARRAY [0..1] OF Ctypes.int)
    : Ctypes.int
    RAISES {};

END Usocket.
