(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Nov 16 09:40:41 PST 1993 by kalsow     *)
(*      modified on Sat Jun 27 15:27:28 PDT 1992 by muller     *)

INTERFACE Ustat;

FROM Ctypes IMPORT int, char_star, short, long;
FROM Utypes IMPORT dev_t, ino_t, off_t, time_t, uint_t,
                   mode_t, nlink_t, uid_t, gid_t;


TYPE
  struct_stat_star = UNTRACED REF struct_stat;

  struct_stat = RECORD
    st_dev            : dev_t;   (* ID of device containing entry for file. *)
    st_retired1       : int;
    st_mode           : mode_t;  (* file mode *)
    st_nlink          : nlink_t; (* number of links *)
    st_nlink_reserved : short;
    st_uid            : uid_t;   (* user ID of the file's owner *)
    st_gid            : gid_t;   (* group ID of the file's group *)
    st_rdev           : dev_t;   (* ID of device *)
    st_ldev           : dev_t;   (* Local ID of device *)
    st_size           : off_t;   (* file size in bytes *)
    st_retired2       : time_t;
    st_uatime         : int;     (* microseconds of st_atime *)
    st_retired3       : time_t;
    st_umtime         : int;     (* microseconds of st_mtime *)
    st_retired4       : time_t;
    st_uctime         : int;     (* microseconds of st_ctime *)
    st_retired5       : int;
    st_retired6       : int;
    st_flags          : uint_t;  (* user defined flags for file *)
    st_gen            : uint_t;  (* file generation number *)
    st_spare          : ARRAY [0..3] OF long;
    st_ino            : ino_t;   (* file serial number *)
    st_ino_reserved   : int;
    st_atime          : time_t;  (* time of last access *)
    st_atime_reserved : int;
    st_mtime          : time_t;  (* time of last data modification *)
    st_mtime_reserved : int;
    st_ctime          : time_t;  (* time of last file status change *)
    st_ctime_reserved : int;
    st_blksize        : long;    (* size of block in file *)
    st_blocks         : long;    (* blocks allocated for file *)
  END;

<*EXTERNAL*> PROCEDURE mkdir(path: char_star;  mode: mode_t): int;

<*EXTERNAL*> PROCEDURE umask(mode: mode_t): mode_t;

<*EXTERNAL "_F64_stat"*>
PROCEDURE stat (path: char_star;  buf: struct_stat_star): int;

<*EXTERNAL "_F64_fstat"*>
PROCEDURE fstat (fd: int;  buf: struct_stat_star): int;

<*EXTERNAL "_F64_lstat"*>
PROCEDURE lstat (path: char_star;  buf: struct_stat_star): int;

<*EXTERNAL*> PROCEDURE chmod (path: char_star;  mode: mode_t): int;

<*EXTERNAL*> PROCEDURE mkfifo (path: char_star;  mode: mode_t): int;


CONST
  s_BLKSIZE = 512; (* block size used in struct_stat *)

CONST
  S_ISUID  = 8_0004000;   (* set user id on execution *)
  S_ISGID  = 8_0002000;   (* set group id on execution *)

CONST                     (* user / group / other definitions *)
  S_IRWXU  = 8_0000700;   (* read,write,execute perm: owner *)
  S_IRUSR  = 8_0000400;   (* read permission: owner *)
  S_IWUSR  = 8_0000200;   (* write permission: owner *)
  S_IXUSR  = 8_0000100;   (* execute/search permission: owner *)

  S_IRWXG  = 8_0000070;   (* read,write,execute perm: group *)
  S_IRGRP  = 8_0000040;   (* read permission: group *)
  S_IWGRP  = 8_0000020;   (* write permission: group *)
  S_IXGRP  = 8_0000010;   (* execute/search permission: group *)

  S_IRWXO  = 8_0000007;   (* read,write,execute perm: other *)
  S_IROTH  = 8_0000004;   (* read permission: other *)
  S_IWOTH  = 8_0000002;   (* write permission: other *)
  S_IXOTH  = 8_0000001;   (* execute/search permission: other *)

  S_IFMT   = 8_0170000;   (* type of file *)
  S_IFSOCK = 8_0140000;   (*   socket *)
  S_IFLNK  = 8_0120000;   (*   symbolic link *)
  S_IFREG  = 8_0100000;   (*   regular *)
  S_IFBLK  = 8_0060000;   (*   block special *)
  S_IFDIR  = 8_0040000;   (*   directory *)
  S_IFCHR  = 8_0020000;   (*   character special *)
  S_IFIFO  = 8_0010000;   (*   fifo *)
  S_IFPIPE = 8_0000000;   (*   pipe *)
  S_IFPORT = S_IFIFO;

  S_ISVTX  = 8_0001000;   (* save text even after use *)

  S_ENFMT  = S_ISGID;     (* record locking enforcement flag *)

CONST  (* deprecated: use S_IRUSR, ... defined above *)
  S_IREAD  = S_IRUSR;
  S_IWRITE = S_IWUSR;
  S_IEXEC  = S_IXUSR;
  S_GREAD  = S_IRGRP;
  S_GWRITE = S_IWGRP;
  S_GEXEC  = S_IXGRP;
  S_OREAD  = S_IROTH;
  S_OWRITE = S_IWOTH;
  S_OEXEC  = S_IXOTH;

END Ustat.
