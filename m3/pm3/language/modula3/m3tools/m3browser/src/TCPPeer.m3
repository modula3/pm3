(* Copyright (C) 1995, Digital Equipment Corporation             *)
(* All rights reserved.                                          *)
(* See the file COPYRIGHT for a full description.                *)
(*                                                               *)
(* Last modified on Mon May  8 07:51:23 PDT 1995 by kalsow       *)
(*                                                               *)
(* Contributed by Peter Klein (pk@i3.informatik.rwth-aachen.de)  *)
(*    - Mar 7, 1995                                              *)

UNSAFE MODULE TCPPeer;

IMPORT TCP, TCPPosix, IP, Uin, Usocket, Ctypes, Atom, AtomList, Cerrno;
IMPORT Fmt, Word, Int32;

TYPE Addr = Uin.struct_sockaddr_in;

PROCEDURE Get (channel: TCP.T): IP.Endpoint RAISES {IP.Error} =
  VAR addr: Addr;  endpoint: IP.Endpoint;
  BEGIN
    GetSockAddr(channel, addr);
    endpoint.port := addr.sin_port;
    endpoint.addr := LOOPHOLE (addr.sin_addr.s_addr, IP.Address);
    RETURN endpoint;
  END Get;

PROCEDURE GetName (channel: TCP.T): TEXT RAISES {IP.Error} =
  VAR addr: Addr;
  BEGIN
    GetSockAddr (channel, addr);
    RETURN IP.GetCanonicalByAddr (LOOPHOLE (addr.sin_addr.s_addr, IP.Address));
  END GetName;

PROCEDURE Match (channel: TCP.T; address: IP.Address; maskBits: [0 .. 32]):
  BOOLEAN RAISES {IP.Error} =
  VAR addr: Addr;  peer, mask: INTEGER;
  BEGIN
    GetSockAddr(channel, addr);
    peer := Word.Extract (addr.sin_addr.s_addr, 32 - maskBits, maskBits);
    mask := Word.Extract (LOOPHOLE(address, Int32.T), 32 - maskBits, maskBits);
    RETURN (peer = mask);
  END Match;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE GetSockAddr (channel: TCP.T;  VAR(*OUT*) addr: Addr)
  RAISES {IP.Error} =
  VAR len: Ctypes.int := BYTESIZE (addr);
  BEGIN
    IF (channel.closed) THEN
      RAISE IP.Error (AtomList.List1 (Atom.FromText ("TCP.Closed")));
    END;
    LOCK channel DO
      IF (Usocket.getpeername (channel.fd, ADR (addr), ADR (len)) < 0) THEN
        RAISE IP.Error (AtomList.List1 (Atom.FromText(Fmt.Int(Cerrno.errno))));
      END;
    END;
  END GetSockAddr;

BEGIN
END TCPPeer.
