(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Tue Jun 27 15:08:05 PDT 1995 by steveg *)

UNSAFE MODULE TCPExtras;

IMPORT Ctypes, IP, TCP, TCPWin32, WinSock;

PROCEDURE LocalEndpoint(conn: TCP.T): IP.Endpoint RAISES {Failure} =
  VAR
    sockaddr := NEW(UNTRACED REF WinSock.struct_sockaddr_in);
    lenSA := BYTESIZE(WinSock.struct_sockaddr);
    fd := conn.sock;
    res: IP.Endpoint;
  BEGIN
    TRY
      IF WinSock.getsockname(fd,
                             LOOPHOLE(sockaddr, 
                                      UNTRACED REF WinSock.struct_sockaddr), 
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
    RETURN WinSock.htons(s);
  END htons;

PROCEDURE ntohs(s: Ctypes.unsigned_short_int): Ctypes.unsigned_short_int =
  BEGIN
    RETURN WinSock.ntohs(s);
  END ntohs;

BEGIN
END TCPExtras.
