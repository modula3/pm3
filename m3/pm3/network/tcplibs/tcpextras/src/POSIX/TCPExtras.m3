(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Tue Jun 27 15:07:52 PDT 1995 by steveg *)

UNSAFE MODULE TCPExtras;

IMPORT Ctypes, IP, TCP, TCPPosix, Uin, Usocket;

PROCEDURE LocalEndpoint(conn: TCP.T): IP.Endpoint RAISES {Failure} =
  VAR
    sockaddr := NEW(Uin.struct_sockaddr_in_star);
    lenSA := BYTESIZE(Usocket.struct_sockaddr);
    fd: Ctypes.int := conn.fd;
    res: IP.Endpoint;
  BEGIN
    TRY
      IF Usocket.getsockname(fd,
                             LOOPHOLE(sockaddr, 
                                      UNTRACED REF Usocket.struct_sockaddr), 
                             ADR(lenSA)) # 0 THEN
        RAISE Failure;
      END;
      res.addr := LOOPHOLE(sockaddr.sin_addr, IP.Address);
      res.port := sockaddr.sin_port;
    FINALLY
      DISPOSE(sockaddr);
    END;
    RETURN res;
  END LocalEndpoint;

PROCEDURE htons(s: Ctypes.unsigned_short_int): Ctypes.unsigned_short_int =
  BEGIN
    RETURN Uin.htons(s);
  END htons;

PROCEDURE ntohs(s: Ctypes.unsigned_short_int): Ctypes.unsigned_short_int =
  BEGIN
    RETURN Uin.ntohs(s);
  END ntohs;

BEGIN
END TCPExtras.
