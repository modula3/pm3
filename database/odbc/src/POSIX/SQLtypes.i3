(* Copyright (C) 1996-2000, Critical Mass, Inc.  All Rights Reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE SQLtypes;

FROM Ctypes IMPORT unsigned_char, signed_char, long_int,
  unsigned_long_int, signed_long_int, short_int, signed_short_int,
  unsigned_short_int, void_star, double, float;

TYPE
  HWND     = ADDRESS;

TYPE
  UCHAR    = unsigned_char;
  SCHAR    = signed_char;
  SDWORD   = long_int;
  SWORD    = short_int;
  UDWORD   = unsigned_long_int;
  UWORD    = unsigned_short_int;

  SLONG    = signed_long_int;
  SSHORT   = signed_short_int;
  ULONG    = unsigned_long_int;
  USHORT   = unsigned_short_int;

  LDOUBLE  = double;
  SDOUBLE  = double;
  SFLOAT   = float;

  PTR      = void_star;
  HENV     = void_star;
  HDBC     = void_star;
  HSTMT    = void_star;

  RETCODE  = signed_short_int;

  SQLCHAR      = UCHAR;     SQLCHAR_star      = UNTRACED REF SQLCHAR;
  SQLSCHAR     = SCHAR;     SQLSCHAR_star     = UNTRACED REF SQLSCHAR;
  SQLINTEGER   = SDWORD;    SQLINTEGER_star   = UNTRACED REF SQLINTEGER;
  SQLSMALLINT  = SWORD;     SQLSMALLINT_star  = UNTRACED REF SQLSMALLINT;
  SQLUINTEGER  = UDWORD;    SQLUINTEGER_star  = UNTRACED REF SQLUINTEGER;
  SQLUSMALLINT = UWORD;     SQLUSMALLINT_star = UNTRACED REF SQLUSMALLINT;
  SQLPOINTER   = void_star; SQLPOINTER_star   = UNTRACED REF SQLPOINTER;

  SQLHENV   = HENV;         SQLHENV_star  = UNTRACED REF SQLHENV;
  SQLHDBC   = HDBC;         SQLHDBC_star  = UNTRACED REF SQLHDBC;
  SQLHSTMT  = HSTMT;        SQLHSTMT_star = UNTRACED REF SQLHSTMT;
  SQLRETURN = SQLSMALLINT;
  SQLHWND   = HWND;

  DATE_STRUCT = RECORD
    year  : SQLSMALLINT;
    month : SQLUSMALLINT;
    day   : SQLUSMALLINT;
  END;

  TIME_STRUCT = RECORD
    hour   : SQLUSMALLINT;
    minute : SQLUSMALLINT;
    second : SQLUSMALLINT;
  END;

  TIMESTAMP_STRUCT = RECORD
    year     : SQLSMALLINT;
    month    : SQLUSMALLINT;
    day      : SQLUSMALLINT;
    hour     : SQLUSMALLINT;
    minute   : SQLUSMALLINT;
    second   : SQLUSMALLINT;
    fraction : SQLUINTEGER;
  END;

  BOOKMARK = unsigned_long_int;

END SQLtypes.
