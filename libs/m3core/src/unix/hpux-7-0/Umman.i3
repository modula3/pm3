(* Copyright (C) 1999, Digital Equipment Corporation                     
*)
(* All rights reserved.                              
*)
(* See the file COPYRIGHT for a full description.                        
*)
(*   
*)
(* Last modified on Wed Sep 22 15:14:54 EDT 1999 by jensh@e-technik.uni-kl.de *)

INTERFACE Umman;

FROM Ctypes IMPORT int, unsigned_long, const_char_star;
FROM Utypes IMPORT caddr_t, size_t, off_t, mode_t;
FROM Word   IMPORT Or;

(*** sys/mman.h ***)

CONST
  PROT_NONE    = 0;
  PROT_USER    = PROT_NONE;
  PROT_READ    = 16_1;
  PROT_WRITE   = 16_2;
  PROT_EXECUTE = 16_4;
  PROT_EXEC    = PROT_EXECUTE;
  PROT_KERNEL  = 16_8;

  PROT_URWX    = Or (PROT_USER,   Or (PROT_WRITE, 
     	       	    	              Or (PROT_READ, PROT_EXECUTE)));
  PROT_URX     = Or (PROT_USER,   Or (PROT_READ, PROT_EXECUTE));
  PROT_URW     = Or (PROT_USER,   Or (PROT_WRITE, PROT_READ));
  PROT_KRW     = Or (PROT_KERNEL, Or (PROT_READ, PROT_WRITE));

  MAP_SHARED    = 16_1;  (* share changes *)
  MAP_PRIVATE   = 16_2;  (* changes are private *)
  MAP_FIXED     = 16_4;  (* fixed mapping *)
  MAP_VARIABLE  = 16_8;  (* variable mapping (default) *)
  MAP_ANONYMOUS = 16_10; (* map from anonymous file -- zero *)
  MAP_FILE      = 16_20; (* map from file (default) *)
  MAP_SHLIB     = 16_40; (* map shared library *)
  MAP_STATICPREDICTION = 16_80;  (* map static prediction  *)

  MADV_NORMAL     = 0;  (* no further special treatment *)
  MADV_RANDOM	  = 1;  (* expect random page references *)
  MADV_SEQUENTIAL = 2;  (* expect sequential page references *)
  MADV_WILLNEED	  = 3;  (* will need these pages *)
  MADV_DONTNEED	  = 4;  (* dont need these pages *)
  MADV_SPACEAVAIL = 5;  (* insure that resources are reserved *)

  MS_SYNC         = 16_1;  (* wait until I/O is complete *)
  MS_ASYNC        = 16_2;  (* perform I/O asynchronously *)
  MS_INVALIDATE   = 16_4;  (* invalidate pages after I/O done *)

  MCL_CURRENT     = 16_1;
  MCL_FUTURE      = 16_2;
   
(* Can't figure out at the moment how to implement constants starting
   with "__" *)
(*  __MCAS_OK       = 0;*) (* Lw_mcas_util was successful *)
(*  __MCAS_NOLOCKID = 1;*) (* No msemaphore lock id allocated for process
*)
(*  __MCAS_PFAULT   = 2;*) (* page fault *)
(*  __MCAS_BUSY     = 3;*) (* can't get lock *)
(*  __MCAS_ALIGN    = 4;*) (* semaphore address is not 16 byte aligned *)

  TYPE
  msemaphore = RECORD
    msem_lock: unsigned_long;
    magic:     unsigned_long;
    locker:    unsigned_long;
    wanted:    unsigned_long;
  END;

  msemaphore_star = UNTRACED REF msemaphore;

<*EXTERNAL*>
PROCEDURE msem_init (sem: msemaphore_star; intital_value: int)
 : msemaphore_star;

<*EXTERNAL*>
PROCEDURE msem_remove (sem: msemaphore_star): int;

<*EXTERNAL*>
PROCEDURE msem_lock (sem: msemaphore_star; condition: int): int;

<*EXTERNAL*>
PROCEDURE msem_unlock (sem: msemaphore_star; condition: int): int;

<*EXTERNAL*>
PROCEDURE madvise (addr: caddr_t; len: size_t; behav: int): int;


<*EXTERNAL*>
PROCEDURE mmap (addr: caddr_t; len: size_t; prot,flags,fd: int; off:
off_t)
  : caddr_t;

<*EXTERNAL*>
PROCEDURE mprotect(addr: caddr_t; len: size_t; prot: int): int;

<*EXTERNAL*>
PROCEDURE msync(addr: caddr_t; len: size_t; flags: int): int;

<*EXTERNAL*>
PROCEDURE munmap (addr: caddr_t; len: size_t): int;

<*EXTERNAL*>
PROCEDURE mlockall (flags: int): int;

<*EXTERNAL*>
PROCEDURE munlockall (): int;

<*EXTERNAL*>
PROCEDURE mlock (addr: caddr_t; len: size_t): int;


<*EXTERNAL*>
PROCEDURE munlock (addr: caddr_t; len: size_t): int;

<*EXTERNAL*>
PROCEDURE shm_open (name: const_char_star; oflag: int; mode: mode_t): int;

<*EXTERNAL*>
PROCEDURE shm_unlink (name: const_char_star): int;

END Umman.

