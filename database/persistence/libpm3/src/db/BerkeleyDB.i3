INTERFACE BerkeleyDB;

IMPORT Ctypes, Utypes;

(*
 * !!!
 * Berkeley DB uses specifically sized types.  If they're not provided by
 * the system, typedef them here.
 *
 * We protect them against multiple inclusion using __BIT_TYPES_DEFINED__,
 * as does BIND and Kerberos, since we don't know for sure what #include
 * files the user is using.
 *
 * !!!
 * We also provide the standard u_int, u_long etc., if they're not provided
 * by the system.
 *)
TYPE
  u_int8_t  = Ctypes.unsigned_char;
  int16_t   = Ctypes.short;
  u_int16_t = Ctypes.unsigned_short;
  int32_t   = Ctypes.int;
  u_int32_t = Ctypes.unsigned_int;
  u_long    = Ctypes.unsigned_long;
  int       = Ctypes.int;

(* Basic types that are exported or quasi-exported. *)
TYPE
  db_pgno_t    = u_int32_t;		 (* Page number type. *)
  db_indx_t    = u_int16_t;		 (* Page offset type. *)
CONST
  DB_MAX_PAGES   = 16_ffffffff; (* >= # of pages in a file *)

TYPE
  db_recno_t   = u_int32_t;		 (* Record number type. *)
CONST
  DB_MAX_RECORDS = 16_ffffffff;		 (* >= # of records in a tree *)

TYPE
  db_timeout_t = u_int32_t;		 (* Type of a timeout. *)

(*
 * Region offsets are currently limited to 32-bits.  I expect that's going
 * to have to be fixed in the not-too-distant future, since we won't want to
 * split 100Gb memory pools into that many different regions.
 *)
TYPE roff_t = u_int32_t;

(*
 * Forward structure declarations, so we can declare pointers and
 * applications can get type checking.
 *)
TYPE
  DB           = ADDRESS;
  DB_ENV       = ADDRESS;
  DB_LOGC      = ADDRESS;
  DB_MPOOLFILE = ADDRESS;
  DB_TXN       = ADDRESS;
  DBC          = ADDRESS;

(* Key/data structure -- a Data-Base Thang. *)
TYPE
  DBT = RECORD
    (*
     * data/size must be fields 1 and 2 for DB 1.85 compatibility.
     *)
    data: Ctypes.void_star := NIL;	 (* Key/data *)
    size: u_int32_t := 0;		 (* key/data length *)

    ulen: u_int32_t := 0;		 (* RO: length of user buffer. *)
    dlen: u_int32_t := 0;		 (* RO: get/put record length. *)
    doff: u_int32_t := 0;		 (* RO: get/put record offset. *)

    flags: u_int32_t := 0;
  END;


CONST
  DB_DBT_APPMALLOC = 16_001; (* Callback allocated memory. *)
  DB_DBT_ISSET     = 16_002; (* Lower level calls set value. *)
  DB_DBT_MALLOC    = 16_004; (* Return in malloc'd memory. *)
  DB_DBT_PARTIAL   = 16_008; (* Partial put/get. *)
  DB_DBT_REALLOC   = 16_010; (* Return in realloc'd memory. *)
  DB_DBT_USERMEM   = 16_020; (* Return in user's memory. *)
  DB_DBT_DUPOK     = 16_040; (* Insert if duplicate. *)

(*
 * Common flags --
 *	Interfaces which use any of these common flags should never have
 *	interface specific flags in this range.
 *)
CONST
  DB_CREATE            = 16_000001; (* Create file as necessary. *)
  DB_CXX_NO_EXCEPTIONS = 16_000002; (* C++: return error values. *)
  DB_FORCE             = 16_000004; (* Force (anything). *)
  DB_NOMMAP            = 16_000008; (* Don't mmap underlying file. *)
  DB_RDONLY            = 16_000010; (* Read-only (O_RDONLY). *)
  DB_RECOVER           = 16_000020; (* Run normal recovery. *)
  DB_THREAD            = 16_000040; (* Applications are threaded. *)
  DB_TRUNCATE          = 16_000080; (* Discard existing DB (O_TRUNC). *)
  DB_TXN_NOSYNC        = 16_000100; (* Do not sync log on commit. *)
  DB_USE_ENVIRON       = 16_000200; (* Use the environment. *)
  DB_USE_ENVIRON_ROOT  = 16_000400; (* Use the environment if root. *)

(*
 * Common flags --
 *	Interfaces which use any of these common flags should never have
 *	interface specific flags in this range.
 *
 * DB_AUTO_COMMIT:
 *	DB_ENV->set_flags, DB->associate, DB->del, DB->put, DB->open,
 *	DB->remove, DB->rename, DB->truncate
 * DB_DIRTY_READ:
 *	DB->cursor, DB->get, DB->join, DB->open, DBcursor->c_get,
 *	DB_ENV->txn_begin
 *
 *	   Shared flags up to 0x000400 *)
CONST
  DB_AUTO_COMMIT = 16_00800000; (* Implied transaction. *)
  DB_DIRTY_READ  = 16_01000000; (* Dirty Read. *)

(*
 * Flags private to db_env_create.
 *)
CONST
  DB_CLIENT = 16_000001; (* Open for a client environment. *)

(*
 * Flags private to db_create.
 *)
CONST
  DB_XA_CREATE = 16_000001; (* Open in an XA environment. *)

(*
 * Flags private to DB_ENV->open.
 *	   Shared flags up to 0x000400 *)
CONST
  DB_INIT_CDB      = 16_000800; (* Concurrent Access Methods. *)
  DB_INIT_LOCK     = 16_001000; (* Initialize locking. *)
  DB_INIT_LOG      = 16_002000; (* Initialize logging. *)
  DB_INIT_MPOOL    = 16_004000; (* Initialize mpool. *)
  DB_INIT_TXN      = 16_008000; (* Initialize transactions. *)
  DB_JOINENV       = 16_010000; (* Initialize all subsystems present. *)
  DB_LOCKDOWN      = 16_020000; (* Lock memory into physical core. *)
  DB_PRIVATE       = 16_040000; (* DB_ENV is process local. *)
  DB_RECOVER_FATAL = 16_080000; (* Run catastrophic recovery. *)
  DB_SYSTEM_MEM    = 16_100000; (* Use system-backed memory. *)

(*
 * Flags private to DB->open.
 *	   Shared flags up to 0x000400 *)
CONST
  DB_EXCL          = 16_000800; (* Exclusive open (O_EXCL). *)
  DB_FCNTL_LOCKING = 16_001000; (* UNDOC: fcntl(2) locking. *)
  DB_RDWRMASTER    = 16_002000; (* UNDOC: allow subdb master open R/W *)
  DB_WRITEOPEN     = 16_004000; (* UNDOC: open with write lock. *)

(*
 * Flags private to DB_ENV->txn_begin.
 *	   Shared flags up to 0x000400 *)
CONST
  DB_TXN_NOWAIT = 16_000800; (* Do not wait for locks in this TXN. *)
  DB_TXN_SYNC   = 16_001000; (* Always sync log on commit. *)

(*
 * Flags private to DB_ENV->set_encrypt.
 *)
CONST
  DB_ENCRYPT_AES = 16_000001; (* AES, assumes SHA1 checksum *)

(*
 * Flags private to DB_ENV->set_flags.
 *	   Shared flags up to 0x000400 *)
CONST
  DB_CDB_ALLDB         = 16_000800; (* Set CDB locking per environment. *)
  DB_DIRECT_DB         = 16_001000; (* Don't buffer databases in the OS. *)
  DB_DIRECT_LOG        = 16_002000; (* Don't buffer log files in the OS. *)
  DB_NOLOCKING         = 16_004000; (* Set locking/mutex behavior. *)
  DB_NOPANIC           = 16_008000; (* Set panic state per DB_ENV. *)
  DB_OVERWRITE         = 16_010000; (* Overwrite unlinked region files. *)
  DB_PANIC_ENVIRONMENT = 16_020000; (* Set panic state per environment. *)
  DB_REGION_INIT       = 16_040000; (* Page-fault regions on open. *)
  DB_TXN_WRITE_NOSYNC  = 16_080000; (* Write, don't sync, on txn commit. *)
  DB_YIELDCPU          = 16_100000; (* Yield the CPU (a lot). *)

(*
 * Flags private to DB->set_feedback's callback.
 *)
CONST
  DB_UPGRADE = 16_000001; (* Upgrading. *)
  DB_VERIFY  = 16_000002; (* Verifying. *)

(*
 * Flags private to DB_MPOOLFILE->open.
 *	   Shared flags up to 0x000400 *)
CONST
  DB_DIRECT      = 16_000800; (* Don't buffer the file in the OS. *)
  DB_EXTENT      = 16_001000; (* UNDOC: dealing with an extent. *)
  DB_ODDFILESIZE = 16_002000; (* Truncate file to N * pgsize. *)

(*
 * Flags private to DB->set_flags.
 *)
CONST
  DB_CHKSUM_SHA1 = 16_000001; (* Use SHA1 checksumming *)
  DB_DUP         = 16_000002; (* Btree, Hash: duplicate keys. *)
  DB_DUPSORT     = 16_000004; (* Btree, Hash: duplicate keys. *)
  DB_ENCRYPT     = 16_000008; (* Btree, Hash: duplicate keys. *)
  DB_RECNUM      = 16_000010; (* Btree: record numbers. *)
  DB_RENUMBER    = 16_000020; (* Recno: renumber on insert/delete. *)
  DB_REVSPLITOFF = 16_000040; (* Btree: turn off reverse splits. *)
  DB_SNAPSHOT    = 16_000080; (* Recno: snapshot the input. *)

(*
 * Flags private to the DB->stat methods.
 *)
CONST
  DB_STAT_CLEAR = 16_000001; (* Clear stat after returning values. *)

(*
 * Flags private to DB->join.
 *)
CONST
  DB_JOIN_NOSORT = 16_000001; (* Don't try to optimize join. *)

(*
 * Flags private to DB->verify.
 *)
CONST
  DB_AGGRESSIVE      = 16_000001; (* Salvage whatever could be data.*)
  DB_NOORDERCHK      = 16_000002; (* Skip sort order/hashing check. *)
  DB_ORDERCHKONLY    = 16_000004; (* Only perform the order check. *)
  DB_PR_PAGE         = 16_000008; (* Show page contents (-da). *)
  DB_PR_RECOVERYTEST = 16_000010; (* Recovery test (-dr). *)
  DB_PRINTABLE       = 16_000020; (* Use printable format for salvage. *)
  DB_SALVAGE         = 16_000040; (* Salvage what looks like data. *)
(*
 * !!!
 * These must not go over 0x8000, or they will collide with the flags
 * used by __bam_vrfy_subtree.
 *)

(*
 * Flags private to DB->set_rep_transport's send callback.
 *)
CONST
  DB_REP_PERMANENT = 16_0001; (* Important--app. may want to flush. *)

(*******************************************************
 * Locking.
 *******************************************************)
CONST
  DB_LOCKVERSION = 1;

  DB_FILE_ID_LEN = 20;			 (* Unique file ID length. *)

(*
 * Deadlock detector modes; used in the DB_ENV structure to configure the
 * locking subsystem.
 *)
CONST
  DB_LOCK_NORUN    = 0;
  DB_LOCK_DEFAULT  = 1; (* Default policy. *)
  DB_LOCK_EXPIRE   = 2; (* Only expire locks, no detection. *)
  DB_LOCK_MAXLOCKS = 3; (* Abort txn with maximum # of locks. *)
  DB_LOCK_MINLOCKS = 4; (* Abort txn with minimum # of locks. *)
  DB_LOCK_MINWRITE = 5; (* Abort txn with minimum writelocks. *)
  DB_LOCK_OLDEST   = 6; (* Abort oldest transaction. *)
  DB_LOCK_RANDOM   = 7; (* Abort random transaction. *)
  DB_LOCK_YOUNGEST = 8; (* Abort youngest transaction. *)

(* Flag values for lock_vec(), lock_get(). *)
CONST
  DB_LOCK_FREE_LOCKER = 16_001; (* Internal: Free locker as well. *)
  DB_LOCK_NOWAIT      = 16_002; (* Don't wait on unavailable lock. *)
  DB_LOCK_RECORD      = 16_004; (* Internal: record lock. *)
  DB_LOCK_REMOVE      = 16_008; (* Internal: flag object removed. *)
  DB_LOCK_SET_TIMEOUT = 16_010; (* Internal: set lock timeout. *)
  DB_LOCK_SWITCH      = 16_020; (* Internal: switch existing lock. *)
  DB_LOCK_UPGRADE     = 16_040; (* Internal: upgrade existing lock. *)

(*
 * Simple R/W lock modes and for multi-granularity intention locking.
 *
 * !!!
 * These values are NOT random, as they are used as an index into the lock
 * conflicts arrays, i.e., DB_LOCK_IWRITE must be == 3, and DB_LOCK_IREAD
 * must be == 4.
 *)
TYPE
  db_lockmode_t = { DB_LOCK_NG,		 (* Not granted. *)
                    DB_LOCK_READ,	 (* Shared/read. *)
                    DB_LOCK_WRITE,	 (* Exclusive/write. *)
                    DB_LOCK_WAIT,	 (* Wait for event *)
                    DB_LOCK_IWRITE,	 (* Intent exclusive/write. *)
                    DB_LOCK_IREAD,	 (* Intent to share/read. *)
                    DB_LOCK_IWR,	 (* Intent to read and write. *)
                    DB_LOCK_DIRTY,	 (* Dirty Read. *)
                    DB_LOCK_WWRITE	 (* Was Written. *)
  };

(*
 * Request types.
 *)
TYPE
  db_lockop_t = { DB_LOCK_DUMP,		 (* Display held locks. *)
                  DB_LOCK_GET,		 (* Get the lock. *)
                  DB_LOCK_GET_TIMEOUT,	 (* Get lock with a timeout. *)
                  DB_LOCK_INHERIT,	 (* Pass locks to parent. *)
                  DB_LOCK_PUT,		 (* Release the lock. *)
                  DB_LOCK_PUT_ALL,	 (* Release locker's locks. *)
                  DB_LOCK_PUT_OBJ,	 (* Release locker's locks on obj. *)
                  DB_LOCK_PUT_READ,	 (* Release locker's read locks. *)
                  DB_LOCK_TIMEOUT,	 (* Force a txn to timeout. *)
                  DB_LOCK_TRADE,	 (* Trade locker ids on a lock. *)
                  DB_LOCK_UPGRADE_WRITE	 (* Upgrade writes for dirty reads. *)
  };

(*
 * Status of a lock.
 *)
TYPE
  db_status_t = { XXX,
                  DB_LSTAT_ABORTED,	 (* Lock belongs to an aborted txn. *)
                  DB_LSTAT_ERR,		 (* Lock is bad. *)
                  DB_LSTAT_EXPIRED,	 (* Lock has expired. *)
                  DB_LSTAT_FREE,	 (* Lock is unallocated. *)
                  DB_LSTAT_HELD,	 (* Lock is currently held. *)
                  DB_LSTAT_NOTEXIST,	 (* Object on which lock was waiting
                                          * was removed *)
                  DB_LSTAT_PENDING,	 (* Lock was waiting and has been
                                          * promoted; waiting for the owner
                                          * to run and upgrade it to held. *)
                  DB_LSTAT_WAITING	 (* Lock is on the wait queue. *)
  };

(* Lock statistics structure. *)
TYPE
  DB_LOCK_STAT = RECORD
    st_id: u_int32_t;			 (* Last allocated locker ID. *)
    st_cur_maxid: u_int32_t;		 (* Current maximum unused ID. *)
    st_maxlocks: u_int32_t;		 (* Maximum number of locks in table. *)
    st_maxlockers: u_int32_t;		 (* Maximum num of lockers in table. *)
    st_maxobjects: u_int32_t;		 (* Maximum num of objects in table. *)
    st_nmodes: u_int32_t;		 (* Number of lock modes. *)
    st_nlocks: u_int32_t;		 (* Current number of locks. *)
    st_maxnlocks: u_int32_t;		 (* Maximum number of locks so far. *)
    st_nlockers: u_int32_t;		 (* Current number of lockers. *)
    st_maxnlockers: u_int32_t;		 (* Maximum number of lockers so far. *)
    st_nobjects: u_int32_t;		 (* Current number of objects. *)
    st_maxnobjects: u_int32_t;		 (* Maximum number of objects so far. *)
    st_nconflicts: u_int32_t;		 (* Number of lock conflicts. *)
    st_nrequests: u_int32_t;		 (* Number of lock gets. *)
    st_nreleases: u_int32_t;		 (* Number of lock puts. *)
    st_nnowaits: u_int32_t;		 (* Number of requests that would have
					    waited, but NOWAIT was set. *)
    st_ndeadlocks: u_int32_t;		 (* Number of lock deadlocks. *)
    st_locktimeout: db_timeout_t;	 (* Lock timeout. *)
    st_nlocktimeouts: u_int32_t;	 (* Number of lock timeouts. *)
    st_txntimeout: db_timeout_t;	 (* Transaction timeout. *)
    st_ntxntimeouts: u_int32_t;		 (* Number of transaction timeouts. *)
    st_region_wait: u_int32_t;		 (* Region lock granted after wait. *)
    st_region_nowait: u_int32_t;	 (* Region lock granted without wait. *)
    st_regsize: u_int32_t;		 (* Region size. *)
  END;

(*
 * DB_LOCK --
 *	The structure is allocated by the caller and filled in during a
 *	lock_get request (or a lock_vec/DB_LOCK_GET).
 *)
TYPE
  DB_LOCK = RECORD
    off: Utypes.size_t;			 (* Offset of the lock in the region *)
    ndx: u_int32_t;			 (* Index of the object referenced by
					  * this lock; used for locking. *)
    gen: u_int32_t;			 (* Generation number of this lock. *)
    mode: db_lockmode_t;		 (* mode of this lock. *)
  END;

(* Lock request structure. *)
TYPE
  DB_LOCKREQ = RECORD
    op: db_lockop_t;			 (* Operation. *)
    mode: db_lockmode_t;		 (* Requested mode. *)
    timeout: db_timeout_t;		 (* Time to expire lock. *)
    obj: UNTRACED REF DBT;		 (* Object being locked. *)
    lock: DB_LOCK;			 (* Lock returned. *)
  END;

(*******************************************************
 * Logging.
 *******************************************************)
CONST
  DB_LOGVERSION = 7; (* Current log version. *)
  DB_LOGOLDVER  = 7; (* Oldest log version supported. *)
  DB_LOGMAGIC   = 16_040988;

(* Flag values for log_archive(). *)
CONST
  DB_ARCH_ABS  = 16_001; (* Absolute pathnames. *)
  DB_ARCH_DATA = 16_002; (* Data files. *)
  DB_ARCH_LOG  = 16_004; (* Log files. *)

(*
 * A DB_LSN has two parts, a fileid which identifies a specific file, and an
 * offset within that file.  The fileid is an unsigned 4-byte quantity that
 * uniquely identifies a file within the log directory -- currently a simple
 * counter inside the log.  The offset is also an unsigned 4-byte value.  The
 * log manager guarantees the offset is never more than 4 bytes by switching
 * to a new log file before the maximum length imposed by an unsigned 4-byte
 * offset is reached.
 *)
TYPE
  DB_LSN = RECORD
    file: u_int32_t;			 (* File ID. *)
    offset: u_int32_t;			 (* File offset. *)
  END;

CONST
  DB_LOGC_BUF_SIZE  = (32 * 1024);
  DB_LOG_DISK       = 16_01; (* Log record came from disk. *)
  DB_LOG_LOCKED     = 16_02; (* Log region already locked *)
  DB_LOG_SILENT_ERR = 16_04; (* Turn-off error messages. *)

(* Log statistics structure. *)
TYPE
  DB_LOG_STAT = RECORD
    st_magic: u_int32_t;		 (* Log file magic number. *)
    st_version: u_int32_t;		 (* Log file version number. *)
    st_mode: int;			 (* Log file mode. *)
    st_lg_bsize: u_int32_t;		 (* Log buffer size. *)
    st_lg_size: u_int32_t;		 (* Log file size. *)
    st_w_bytes: u_int32_t;		 (* Bytes to log. *)
    st_w_mbytes: u_int32_t;		 (* Megabytes to log. *)
    st_wc_bytes: u_int32_t;		 (* Bytes to log since checkpoint. *)
    st_wc_mbytes: u_int32_t;		 (* Megabytes to log since checkpoint. *)
    st_wcount: u_int32_t;		 (* Total writes to the log. *)
    st_wcount_fill: u_int32_t;		 (* Overflow writes to the log. *)
    st_scount: u_int32_t;		 (* Total syncs to the log. *)
    st_region_wait: u_int32_t;		 (* Region lock granted after wait. *)
    st_region_nowait: u_int32_t;	 (* Region lock granted without wait. *)
    st_cur_file: u_int32_t;		 (* Current log file number. *)
    st_cur_offset: u_int32_t;		 (* Current log file offset. *)
    st_disk_file: u_int32_t;		 (* Known on disk log file number. *)
    st_disk_offset: u_int32_t;		 (* Known on disk log file offset. *)
    st_regsize: u_int32_t;		 (* Region size. *)
    st_maxcommitperflush: u_int32_t;	 (* Max number of commits in a flush. *)
    st_mincommitperflush: u_int32_t;	 (* Min number of commits in a flush. *)
  END;

(*******************************************************
 * Shared buffer cache (mpool).
 *******************************************************)
(* Flag values for DB_MPOOLFILE->get. *)
CONST
  DB_MPOOL_CREATE = 16_001; (* Create a page. *)
  DB_MPOOL_LAST   = 16_002; (* Return the last page. *)
  DB_MPOOL_NEW    = 16_004; (* Create a new page. *)

(* Flag values for DB_MPOOLFILE->put, DB_MPOOLFILE->set. *)
CONST
  DB_MPOOL_CLEAN   = 16_001; (* Page is not modified. *)
  DB_MPOOL_DIRTY   = 16_002; (* Page is modified. *)
  DB_MPOOL_DISCARD = 16_004; (* Don't cache the page. *)

(* Priority values for DB_MPOOLFILE->set_priority. *)
TYPE
  DB_CACHE_PRIORITY = { XXX,
                        DB_PRIORITY_VERY_LOW,
                        DB_PRIORITY_LOW,
                        DB_PRIORITY_DEFAULT,
                        DB_PRIORITY_HIGH,
                        DB_PRIORITY_VERY_HIGH
  };

CONST
  MP_FLUSH        = 16_001; (* Was opened to flush a buffer. *)
  MP_OPEN_CALLED  = 16_002; (* File opened. *)
  MP_READONLY     = 16_004; (* File is readonly. *)
  MP_UPGRADE      = 16_008; (* File descriptor is readwrite. *)
  MP_UPGRADE_FAIL = 16_010; (* Upgrade wasn't possible. *)

(*
 * Mpool statistics structure.
 *)
TYPE
  DB_MPOOL_STAT = RECORD
    st_gbytes: u_int32_t;		 (* Total cache size: GB. *)
    st_bytes: u_int32_t;		 (* Total cache size: B. *)
    st_ncache: u_int32_t;		 (* Number of caches. *)
    st_regsize: u_int32_t;		 (* Cache size. *)
    st_map: u_int32_t;			 (* Pages from mapped files. *)
    st_cache_hit: u_int32_t;		 (* Pages found in the cache. *)
    st_cache_miss: u_int32_t;		 (* Pages not found in the cache. *)
    st_page_create: u_int32_t;		 (* Pages created in the cache. *)
    st_page_in: u_int32_t;		 (* Pages read in. *)
    st_page_out: u_int32_t;		 (* Pages written out. *)
    st_ro_evict: u_int32_t;		 (* Clean pages forced from the cache. *)
    st_rw_evict: u_int32_t;		 (* Dirty pages forced from the cache. *)
    st_page_trickle: u_int32_t;		 (* Pages written by memp_trickle. *)
    st_pages: u_int32_t;		 (* Total number of pages. *)
    st_page_clean: u_int32_t;		 (* Clean pages. *)
    st_page_dirty: u_int32_t;		 (* Dirty pages. *)
    st_hash_buckets: u_int32_t;		 (* Number of hash buckets. *)
    st_hash_searches: u_int32_t;	 (* Total hash chain searches. *)
    st_hash_longest: u_int32_t;		 (* Longest hash chain searched. *)
    st_hash_examined: u_int32_t;	 (* Total hash entries searched. *)
    st_hash_nowait: u_int32_t;		 (* Hash lock granted with nowait. *)
    st_hash_wait: u_int32_t;		 (* Hash lock granted after wait. *)
    st_hash_max_wait: u_int32_t;	 (* Max hash lock granted after wait. *)
    st_region_nowait: u_int32_t;	 (* Region lock granted with nowait. *)
    st_region_wait: u_int32_t;		 (* Region lock granted after wait. *)
    st_alloc: u_int32_t;		 (* Number of page allocations. *)
    st_alloc_buckets: u_int32_t;	 (* Buckets checked during allocation. *)
    st_alloc_max_buckets: u_int32_t;	 (* Max checked during allocation. *)
    st_alloc_pages: u_int32_t;		 (* Pages checked during allocation. *)
    st_alloc_max_pages: u_int32_t;	 (* Max checked during allocation. *)
  END;

(* Mpool file statistics structure. *)
TYPE
  DB_MPOOL_FSTAT = RECORD
    file_name: Ctypes.char_star;	 (* File name. *)
    st_pagesize: Utypes.size_t;		 (* Page size. *)
    st_map: u_int32_t;			 (* Pages from mapped files. *)
    st_cache_hit: u_int32_t;		 (* Pages found in the cache. *)
    st_cache_miss: u_int32_t;		 (* Pages not found in the cache. *)
    st_page_create: u_int32_t;		 (* Pages created in the cache. *)
    st_page_in: u_int32_t;		 (* Pages read in. *)
    st_page_out: u_int32_t;		 (* Pages written out. *)
  END;

(*******************************************************
 * Transactions and recovery.
 *******************************************************)
CONST
  DB_TXNVERSION = 1;

TYPE
  db_recops = {	DB_TXN_ABORT,		 (* Public. *)
                DB_TXN_APPLY,		 (* Public. *)
                DB_TXN_BACKWARD_ALLOC,	 (* Internal. *)
                DB_TXN_BACKWARD_ROLL,	 (* Public. *)
                DB_TXN_FORWARD_ROLL,	 (* Public. *)
                DB_TXN_GETPGNOS,	 (* Internal. *)
                DB_TXN_OPENFILES,	 (* Internal. *)
                DB_TXN_POPENFILES,	 (* Internal. *)
                DB_TXN_PRINT		 (* Public. *)
  };

(*
 * BACKWARD_ALLOC is used during the forward pass to pick up any aborted
 * allocations for files that were created during the forward pass.
 * The main difference between _ALLOC and _ROLL is that the entry for
 * the file not exist during the rollforward pass.
 *)
<*EXTERNAL*> PROCEDURE db_undo(op: db_recops): BOOLEAN;
<*EXTERNAL*> PROCEDURE db_redo(op: db_recops): BOOLEAN;

CONST
  TXN_CHILDCOMMIT = 16_01; (* Transaction that has committed. *)
  TXN_COMPENSATE  = 16_02; (* Compensating transaction. *)
  TXN_DIRTY_READ  = 16_04; (* Transaction does dirty reads. *)
  TXN_LOCKTIMEOUT = 16_08; (* Transaction has a lock timeout. *)
  TXN_MALLOC      = 16_10; (* Structure allocated by TXN system. *)
  TXN_NOSYNC      = 16_20; (* Do not sync on prepare and commit. *)
  TXN_NOWAIT      = 16_40; (* Do not wait on locks. *)
  TXN_SYNC        = 16_80; (* Sync on prepare and commit. *)

(* Transaction statistics structure. *)
TYPE
  DB_TXN_ACTIVE = RECORD
    txnid: u_int32_t;                    (* Transaction ID *)
    parentid: u_int32_t;		 (* Transaction ID of parent *)
    lsn: DB_LSN;			 (* LSN when transaction began *)
  END;

  DB_TXN_STAT = RECORD
    st_last_ckp: DB_LSN;		 (* lsn of the last checkpoint *)
    st_time_ckp: Utypes.time_t;		 (* time of last checkpoint *)
    st_last_txnid: u_int32_t;		 (* last transaction id given out *)
    st_maxtxns: u_int32_t;		 (* maximum txns possible *)
    st_naborts: u_int32_t;		 (* number of aborted transactions *)
    st_nbegins: u_int32_t;		 (* number of begun transactions *)
    st_ncommits: u_int32_t;		 (* number of committed transactions *)
    st_nactive: u_int32_t;		 (* number of active transactions *)
    st_nrestores: u_int32_t;		 (* number of restored transactions
					    recovery after. *)
    st_maxnactive: u_int32_t;		 (* maximum active transactions *)
    st_txnarray: UNTRACED REF DB_TXN_ACTIVE; (* array of active transactions *)
    st_region_wait: u_int32_t;		 (* Region lock granted after wait. *)
    st_region_nowait: u_int32_t;	 (* Region lock granted without wait. *)
    st_regsize: u_int32_t;		 (* Region size. *)
  END;

(*
 * Structure used for two phase commit interface.  Berkeley DB support for two
 * phase commit is compatible with the X/open XA interface.  The xa #define
 * XIDDATASIZE defines the size of a global transaction ID.  We have our own
 * version here which must have the same value.
 *)
CONST
  DB_XIDDATASIZE = 128;
TYPE
  DB_PREPLIST = RECORD
    txn: UNTRACED REF DB_TXN;
    gid: ARRAY [0 .. DB_XIDDATASIZE-1] OF u_int8_t;
  END;

(*******************************************************
 * Replication.
 *******************************************************)
(* Special, out-of-band environment IDs. *)
CONST
  DB_EID_BROADCAST = -1;
  DB_EID_INVALID   = -2;

(* rep_start flags values *)
CONST
  DB_REP_CLIENT   = 16_001;
  DB_REP_LOGSONLY = 16_002;
  DB_REP_MASTER   = 16_004;

(* Replication statistics. *)
TYPE
  DB_REP_STAT = RECORD
    (* !!!
     * Many replication statistics fields cannot be protected by a mutex
     * without an unacceptable performance penalty, since most message
     * processing is done without the need to hold a region-wide lock.
     * Fields whose comments end with a '+' may be updated without holding
     * the replication or log mutexes (as appropriate), and thus may be
     * off somewhat (or, on unreasonable architectures under unlucky
     * circumstances, garbaged).
     *)
    st_status: u_int32_t;		 (* Current replication status. *)
    st_next_lsn: DB_LSN;		 (* Next LSN to use or expect. *)
    st_waiting_lsn: DB_LSN;		 (* LSN we're awaiting, if any. *)

    st_dupmasters: u_int32_t;		 (* # of times a duplicate master
					    condition was detected.+ *)
    st_env_id: int;			 (* Current environment ID. *)
    st_env_priority: int;		 (* Current environment priority. *)
    st_gen: u_int32_t;			 (* Current generation number. *)
    st_log_duplicated: u_int32_t;	 (* Log records received multiply.+ *)
    st_log_queued: u_int32_t;		 (* Log records currently queued.+ *)
    st_log_queued_max: u_int32_t;	 (* Max. log records queued at once.+ *)
    st_log_queued_total: u_int32_t;	 (* Total # of log recs. ever queued.+ *)
    st_log_records: u_int32_t;		 (* Log records received and put.+ *)
    st_log_requested: u_int32_t;	 (* Log recs. missed and requested.+ *)
    st_master: int;			 (* Env. ID of the current master. *)
    st_master_changes: u_int32_t;	 (* # of times we've switched masters. *)
    st_msgs_badgen: u_int32_t;		 (* Messages with a bad generation #.+ *)
    st_msgs_processed: u_int32_t;	 (* Messages received and processed.+ *)
    st_msgs_recover: u_int32_t;		 (* Messages ignored because this site
					    a was client in recovery.+ *)
    st_msgs_send_failures: u_int32_t;	 (* # of failed message sends.+ *)
    st_msgs_sent: u_int32_t;		 (* # of successful message sends.+ *)
    st_newsites: u_int32_t;		 (* # of NEWSITE msgs. received.+ *)
    st_nsites: int;			 (* Current number of sites we will
					    assume during elections. *)
    st_nthrottles: u_int32_t;		 (* # of times we were throttled. *)
    st_outdated: u_int32_t;		 (* # of times we detected and returned
					    an OUTDATED condition.+ *)
    st_txns_applied: u_int32_t;		 (* # of transactions applied.+ *)

    (* Elections generally. *)
    st_elections: u_int32_t;		 (* # of elections held.+ *)
    st_elections_won: u_int32_t;	 (* # of elections won by this site.+ *)

    (* Statistics about an in-progress election. *)
    st_election_cur_winner: int;	 (* Current front-runner. *)
    st_election_gen: u_int32_t;		 (* Election generation number. *)
    st_election_lsn: DB_LSN;		 (* Max. LSN of current winner. *)
    st_election_nsites: int;		 (* # of "registered voters". *)
    st_election_priority: int;		 (* Current election priority. *)
    st_election_status: int;		 (* Current election status. *)
    st_election_tiebreaker: int;	 (* Election tiebreaker value. *)
    st_election_votes: int;		 (* Votes received in this round. *)
  END;

(*******************************************************
 * Access methods.
 *******************************************************)
TYPE
  DBTYPE = { XXX,
             DB_BTREE,
             DB_HASH,
             DB_RECNO,
             DB_QUEUE,
             DB_UNKNOWN			 (* Figure it out on open. *)
  };

CONST
  DB_RENAMEMAGIC = 16_030800;		 (* File has been renamed. *)

  DB_BTREEVERSION = 9;			 (* Current btree version. *)
  DB_BTREEOLDVER  = 8;			 (* Oldest btree version supported. *)
  DB_BTREEMAGIC   = 16_053162;

  DB_HASHVERSION  = 8;			 (* Current hash version. *)
  DB_HASHOLDVER   = 7;			 (* Oldest hash version supported. *)
  DB_HASHMAGIC    = 16_061561;

  DB_QAMVERSION   = 4;			 (* Current queue version. *)
  DB_QAMOLDVER    = 3;			 (* Oldest queue version supported. *)
  DB_QAMMAGIC     = 16_042253;

(*
 * DB access method and cursor operation values.  Each value is an operation
 * code to which additional bit flags are added.
 *)
CONST
  DB_AFTER            = 1;		 (* c_put() *)
  DB_APPEND           = 2;		 (* put() *)
  DB_BEFORE           = 3;		 (* c_put() *)
  DB_CACHED_COUNTS    = 4;		 (* stat() *)
  DB_COMMIT           = 5;		 (* log_put() (internal) *)
  DB_CONSUME          = 6;		 (* get() *)
  DB_CONSUME_WAIT     = 7;		 (* get() *)
  DB_CURRENT          = 8;		 (* c_get(), c_put(), DB_LOGC->get() *)
  DB_FAST_STAT        = 9;		 (* stat() *)
  DB_FIRST            = 10;		 (* c_get(), DB_LOGC->get() *)
  DB_GET_BOTH         = 11;		 (* get(), c_get() *)
  DB_GET_BOTHC        = 12;		 (* c_get() (internal) *)
  DB_GET_BOTH_RANGE   = 13;		 (* get(), c_get() *)
  DB_GET_RECNO        = 14;		 (* c_get() *)
  DB_JOIN_ITEM        = 15;		 (* c_get(); do not do primary lookup *)
  DB_KEYFIRST         = 16;		 (* c_put() *)
  DB_KEYLAST          = 17;		 (* c_put() *)
  DB_LAST             = 18;		 (* c_get(), DB_LOGC->get() *)
  DB_NEXT             = 19;		 (* c_get(), DB_LOGC->get() *)
  DB_NEXT_DUP         = 20;		 (* c_get() *)
  DB_NEXT_NODUP       = 21;		 (* c_get() *)
  DB_NODUPDATA        = 22;		 (* put(), c_put() *)
  DB_NOOVERWRITE      = 23;		 (* put() *)
  DB_NOSYNC           = 24;		 (* close() *)
  DB_POSITION         = 25;		 (* c_dup() *)
  DB_POSITIONI        = 26;		 (* c_dup() (internal) *)
  DB_PREV             = 27;		 (* c_get(), DB_LOGC->get() *)
  DB_PREV_NODUP       = 28;		 (* c_get(), DB_LOGC->get() *)
  DB_RECORDCOUNT      = 29;		 (* stat() *)
  DB_SET              = 30;		 (* c_get(), DB_LOGC->get() *)
  DB_SET_LOCK_TIMEOUT = 31;		 (* set_timout() *)
  DB_SET_RANGE        = 32;		 (* c_get() *)
  DB_SET_RECNO        = 33;		 (* get(), c_get() *)
  DB_SET_TXN_NOW      = 34;		 (* set_timout() (internal) *)
  DB_SET_TXN_TIMEOUT  = 35;		 (* set_timout() *)
  DB_UPDATE_SECONDARY = 36;		 (* c_get(), c_del() (internal) *)
  DB_WRITECURSOR      = 37;		 (* cursor() *)
  DB_WRITELOCK        = 38;		 (* cursor() (internal) *)

(* This has to change when the max opcode hits 255. *)
CONST
  DB_OPFLAGS_MASK       = 16_000000ff;	 (* Mask for operations flags. *)
  (*
  DB_DIRTY_READ         = 16_01000000;	 (* Dirty Read. *)
  *)
  DB_FLUSH        = 16_02000000; (* Flush data to disk. *)
  DB_MULTIPLE     = 16_04000000; (* Return multiple data values. *)
  DB_MULTIPLE_KEY = 16_08000000; (* Return multiple data/key pairs. *)
  DB_NOCOPY       = 16_10000000; (* Don't copy data *)
  DB_PERMANENT    = 16_20000000; (* Flag record with REP_PERMANENT. *)
  DB_RMW          = 16_40000000; (* Acquire write flag immediately. *)
  DB_WRNOSYNC     = 16_80000000; (* Private: write, don't sync log_put *)

(*
 * DB (user visible) error return codes.
 *
 * !!!
 * For source compatibility with DB 2.X deadlock return (EAGAIN), use the
 * following:
 *	#include <errno.h>
 *	#define DB_LOCK_DEADLOCK EAGAIN
 *
 * !!!
 * We don't want our error returns to conflict with other packages where
 * possible, so pick a base error value that's hopefully not common.  We
 * document that we own the error name space from -30,800 to -30,999.
 *)
(* DB (public) error return codes. *)
CONST
  DB_DONOTINDEX       = (-30999);	 (* "Null" return from 2ndary callbk. *)
  DB_KEYEMPTY         = (-30998);	 (* Key/data deleted or never created. *)
  DB_KEYEXIST         = (-30997);	 (* The key/data pair already exists. *)
  DB_LOCK_DEADLOCK    = (-30996);	 (* Deadlock. *)
  DB_LOCK_NOTGRANTED  = (-30995);	 (* Lock unavailable. *)
  DB_NOSERVER         = (-30994);	 (* Server panic return. *)
  DB_NOSERVER_HOME    = (-30993);	 (* Bad home sent to server. *)
  DB_NOSERVER_ID      = (-30992);	 (* Bad ID sent to server. *)
  DB_NOTFOUND         = (-30991);	 (* Key/data pair not found (EOF). *)
  DB_OLD_VERSION      = (-30990);	 (* Out-of-date version. *)
  DB_PAGE_NOTFOUND    = (-30989);	 (* Requested page not found. *)
  DB_REP_DUPMASTER    = (-30988);	 (* There are two masters. *)
  DB_REP_HOLDELECTION = (-30987);	 (* Time to hold an election. *)
  DB_REP_NEWMASTER    = (-30986);	 (* We have learned of a new master. *)
  DB_REP_NEWSITE      = (-30985);	 (* New site entered system. *)
  DB_REP_OUTDATED     = (-30984);	 (* Site is too far behind master. *)
  DB_REP_UNAVAIL      = (-30983);	 (* Site cannot currently be reached. *)
  DB_RUNRECOVERY      = (-30982);	 (* Panic return. *)
  DB_SECONDARY_BAD    = (-30981);	 (* Secondary index corrupt. *)
  DB_VERIFY_BAD       = (-30980);	 (* Verify failed; bad format. *)

(* DB (private) error return codes. *)
CONST
  DB_ALREADY_ABORTED  = (-30899);
  DB_DELETED          = (-30898);	 (* Recovery file marked deleted. *)
  DB_JAVA_CALLBACK    = (-30897);	 (* Exception during a java callback. *)
  DB_LOCK_NOTEXIST    = (-30896);	 (* Object to lock is gone. *)
  DB_NEEDSPLIT        = (-30895);	 (* Page needs to be split. *)
  DB_SURPRISE_KID     = (-30894);	 (* Child commit where parent
					    didn't know it was a parent. *)
  DB_SWAPBYTES        = (-30893);	 (* Database needs byte swapping. *)
  DB_TIMEOUT          = (-30892);	 (* Timed out waiting for election. *)
  DB_TXN_CKP          = (-30891);	 (* Encountered ckp record in log. *)
  DB_VERIFY_FATAL     = (-30890);	 (* DB->verify cannot proceed. *)

CONST
  DB_LOGFILEID_INVALID = -1;

  DB_OK_BTREE = 16_01;
  DB_OK_HASH  = 16_02;
  DB_OK_QUEUE = 16_04;
  DB_OK_RECNO = 16_08;

  DB_AM_CHKSUM       = 16_00000001; (* Checksumming. *)
  DB_AM_CL_WRITER    = 16_00000002; (* Allow writes in client replica. *)
  DB_AM_COMPENSATE   = 16_00000004; (* Created by compensating txn. *)
  DB_AM_CREATED      = 16_00000008; (* Database was created upon open. *)
  DB_AM_CREATED_MSTR = 16_00000010; (* Encompassing file was created. *)
  DB_AM_DBM_ERROR    = 16_00000020; (* Error in DBM/NDBM database. *)
  DB_AM_DELIMITER    = 16_00000040; (* Variable length delimiter set. *)
  DB_AM_DIRTY        = 16_00000080; (* Support Dirty Reads. *)
  DB_AM_DISCARD      = 16_00000100; (* Discard any cached pages. *)
  DB_AM_DUP          = 16_00000200; (* DB_DUP. *)
  DB_AM_DUPSORT      = 16_00000400; (* DB_DUPSORT. *)
  DB_AM_ENCRYPT      = 16_00000800; (* Encryption. *)
  DB_AM_FIXEDLEN     = 16_00001000; (* Fixed-length records. *)
  DB_AM_INMEM        = 16_00002000; (* In-memory; no sync on close. *)
  DB_AM_IN_RENAME    = 16_00004000; (* File is being renamed. *)
  DB_AM_OPEN_CALLED  = 16_00008000; (* DB->open called. *)
  DB_AM_PAD          = 16_00010000; (* Fixed-length record pad. *)
  DB_AM_PGDEF        = 16_00020000; (* Page size was defaulted. *)
  DB_AM_RDONLY       = 16_00040000; (* Database is readonly. *)
  DB_AM_RECNUM       = 16_00080000; (* DB_RECNUM. *)
  DB_AM_RECOVER      = 16_00100000; (* DB opened by recovery routine. *)
  DB_AM_RENUMBER     = 16_00200000; (* DB_RENUMBER. *)
  DB_AM_REVSPLITOFF  = 16_00400000; (* DB_REVSPLITOFF. *)
  DB_AM_SECONDARY    = 16_00800000; (* Database is a secondary index. *)
  DB_AM_SNAPSHOT     = 16_01000000; (* DB_SNAPSHOT. *)
  DB_AM_SUBDB        = 16_02000000; (* Subdatabases supported. *)
  DB_AM_SWAP         = 16_04000000; (* Pages need to be byte-swapped. *)
  DB_AM_TXN          = 16_08000000; (* Opened in a transaction. *)
  DB_AM_VERIFYING    = 16_10000000; (* DB handle is in the verifier. *)

(*
 * Macros for bulk get.  Note that wherever we use a DBT *, we explicitly
 * cast it; this allows the same macros to work with C++ Dbt *'s, as Dbt
 * is a subclass of struct DBT in C++.
 *)
<*EXTERNAL*>
PROCEDURE db_multiple_init
  (VAR pointer: UNTRACED REF u_int8_t; VAR dbt: DBT);

<*EXTERNAL*>
PROCEDURE db_multiple_next
  (VAR pointer: UNTRACED REF u_int32_t;
   VAR dbt: DBT;
   VAR retdata: UNTRACED REF u_int8_t;
   VAR retdlen: u_int32_t);

<*EXTERNAL*>
PROCEDURE db_multiple_key_next
  (VAR pointer: UNTRACED REF u_int32_t;
   VAR dbt: DBT;
   VAR retkey: UNTRACED REF u_int8_t;
   VAR retklen: u_int32_t;
   VAR retdata: UNTRACED REF u_int8_t;
   VAR retdlen: u_int32_t);

<*EXTERNAL*>
PROCEDURE db_multiple_recno_next
  (VAR pointer: UNTRACED REF u_int32_t;
   VAR dbt: DBT;
   VAR recno: u_int32_t;
   VAR retdata: UNTRACED REF u_int8_t;
   VAR retdlen: u_int32_t);

(*******************************************************
 * Access method cursors.
 *******************************************************)
CONST
  DBC_ACTIVE       = 16_0001; (* Cursor in use. *)
  DBC_COMPENSATE   = 16_0002; (* Cursor compensating, don't lock. *)
  DBC_DIRTY_READ   = 16_0004; (* Cursor supports dirty reads. *)
  DBC_OPD          = 16_0008; (* Cursor references off-page dups. *)
  DBC_RECOVER      = 16_0010; (* Recovery cursor; don't log/lock. *)
  DBC_RMW          = 16_0020; (* Acquire write flag in read op. *)
  DBC_TRANSIENT    = 16_0040; (* Cursor is transient. *)
  DBC_WRITECURSOR  = 16_0080; (* Cursor may be used to write (CDB). *)
  DBC_WRITEDUP     = 16_0100; (* idup'ed DBC_WRITECURSOR (CDB). *)
  DBC_WRITER       = 16_0200; (* Cursor immediately writing (CDB). *)
  DBC_MULTIPLE     = 16_0400; (* Return Multiple data. *)
  DBC_MULTIPLE_KEY = 16_0800; (* Return Multiple keys and data. *)
  DBC_OWN_LID      = 16_1000; (* Free lock id on destroy. *)

(* Key range statistics structure *)
TYPE
  DB_KEY_RANGE = RECORD
    less: Ctypes.double;
    equal: Ctypes.double;
    greater: Ctypes.double;
  END;

(* Btree/Recno statistics structure. *)
TYPE
  DB_BTREE_STAT = RECORD
    bt_magic: u_int32_t;		 (* Magic number. *)
    bt_version: u_int32_t;		 (* Version number. *)
    bt_metaflags: u_int32_t;		 (* Metadata flags. *)
    bt_nkeys: u_int32_t;		 (* Number of unique keys. *)
    bt_ndata: u_int32_t;		 (* Number of data items. *)
    bt_pagesize: u_int32_t;		 (* Page size. *)
    bt_maxkey: u_int32_t;		 (* Maxkey value. *)
    bt_minkey: u_int32_t;		 (* Minkey value. *)
    bt_re_len: u_int32_t;		 (* Fixed-length record length. *)
    bt_re_pad: u_int32_t;		 (* Fixed-length record pad. *)
    bt_levels: u_int32_t;		 (* Tree levels. *)
    bt_int_pg: u_int32_t;		 (* Internal pages. *)
    bt_leaf_pg: u_int32_t;		 (* Leaf pages. *)
    bt_dup_pg: u_int32_t;		 (* Duplicate pages. *)
    bt_over_pg: u_int32_t;		 (* Overflow pages. *)
    bt_free: u_int32_t;			 (* Pages on the free list. *)
    bt_int_pgfree: u_int32_t;		 (* Bytes free in internal pages. *)
    bt_leaf_pgfree: u_int32_t;		 (* Bytes free in leaf pages. *)
    bt_dup_pgfree: u_int32_t;		 (* Bytes free in duplicate pages. *)
    bt_over_pgfree: u_int32_t;		 (* Bytes free in overflow pages. *)
  END;

(* Hash statistics structure. *)
TYPE
  DB_HASH_STAT = RECORD
    hash_magic: u_int32_t;		 (* Magic number. *)
    hash_version: u_int32_t;		 (* Version number. *)
    hash_metaflags: u_int32_t;		 (* Metadata flags. *)
    hash_nkeys: u_int32_t;		 (* Number of unique keys. *)
    hash_ndata: u_int32_t;		 (* Number of data items. *)
    hash_pagesize: u_int32_t;		 (* Page size. *)
    hash_ffactor: u_int32_t;		 (* Fill factor specified at create. *)
    hash_buckets: u_int32_t;		 (* Number of hash buckets. *)
    hash_free: u_int32_t;		 (* Pages on the free list. *)
    hash_bfree: u_int32_t;		 (* Bytes free on bucket pages. *)
    hash_bigpages: u_int32_t;		 (* Number of big key/data pages. *)
    hash_big_bfree: u_int32_t;		 (* Bytes free on big item pages. *)
    hash_overflows: u_int32_t;		 (* Number of overflow pages. *)
    hash_ovfl_free: u_int32_t;		 (* Bytes free on ovfl pages. *)
    hash_dup: u_int32_t;		 (* Number of dup pages. *)
    hash_dup_free: u_int32_t;		 (* Bytes free on duplicate pages. *)
  END;

(* Queue statistics structure. *)
TYPE
  DB_QUEUE_STAT = RECORD
    qs_magic: u_int32_t;		 (* Magic number. *)
    qs_version: u_int32_t;		 (* Version number. *)
    qs_metaflags: u_int32_t;		 (* Metadata flags. *)
    qs_nkeys: u_int32_t;		 (* Number of unique keys. *)
    qs_ndata: u_int32_t;		 (* Number of data items. *)
    qs_pagesize: u_int32_t;		 (* Page size. *)
    qs_extentsize: u_int32_t;		 (* Pages per extent. *)
    qs_pages: u_int32_t;		 (* Data pages. *)
    qs_re_len: u_int32_t;		 (* Fixed-length record length. *)
    qs_re_pad: u_int32_t;		 (* Fixed-length record pad. *)
    qs_pgfree: u_int32_t;		 (* Bytes free in data pages. *)
    qs_first_recno: u_int32_t;		 (* First not deleted record. *)
    qs_cur_recno: u_int32_t;		 (* Next available record number. *)
  END;

(*******************************************************
 * Environment.
 *******************************************************)
CONST DB_REGION_MAGIC = 16_120897;	 (* Environment magic number. *)

CONST
  DB_VERB_CHKPOINT    = 16_0001; (* List checkpoints. *)
  DB_VERB_DEADLOCK    = 16_0002; (* Deadlock detection information. *)
  DB_VERB_RECOVERY    = 16_0004; (* Recovery information. *)
  DB_VERB_REPLICATION = 16_0008; (* Replication information. *)
  DB_VERB_WAITSFOR    = 16_0010; (* Dump waits-for table. *)

  DB_TEST_ELECTINIT   = 1;		 (* after __rep_elect_init *)
  DB_TEST_ELECTSEND   = 2;		 (* after REP_ELECT msgnit *)
  DB_TEST_ELECTVOTE1  = 3;		 (* after __rep_send_vote 1 *)
  DB_TEST_ELECTVOTE2  = 4;		 (* after __rep_wait *)
  DB_TEST_ELECTWAIT1  = 5;		 (* after REP_VOTE2 *)
  DB_TEST_ELECTWAIT2  = 6;		 (* after __rep_wait 2 *)
  DB_TEST_PREDESTROY  = 7;		 (* before destroy op *)
  DB_TEST_PREOPEN     = 8;		 (* before __os_open *)
  DB_TEST_POSTDESTROY = 9;		 (* after destroy op *)
  DB_TEST_POSTLOG     = 10;		 (* after logging all pages *)
  DB_TEST_POSTLOGMETA = 11;		 (* after logging meta in btree *)
  DB_TEST_POSTOPEN    = 12;		 (* after __os_open *)
  DB_TEST_POSTSYNC    = 13;		 (* after syncing the log *)
  DB_TEST_SUBDB_LOCKS = 14;		 (* subdb locking tests *)

  DB_ENV_AUTO_COMMIT      = 16_0000001;	 (* DB_AUTO_COMMIT. *)
  DB_ENV_CDB              = 16_0000002;	 (* DB_INIT_CDB. *)
  DB_ENV_CDB_ALLDB        = 16_0000004;	 (* CDB environment wide locking. *)
  DB_ENV_CREATE           = 16_0000008;	 (* DB_CREATE set. *)
  DB_ENV_DBLOCAL          = 16_0000010;	 (* DB_ENV allocated for private DB. *)
  DB_ENV_DIRECT_DB        = 16_0000020;	 (* DB_DIRECT_DB set. *)
  DB_ENV_DIRECT_LOG       = 16_0000040;	 (* DB_DIRECT_LOG set. *)
  DB_ENV_FATAL            = 16_0000080;	 (* Doing fatal recovery in env. *)
  DB_ENV_LOCKDOWN         = 16_0000100;	 (* DB_LOCKDOWN set. *)
  DB_ENV_NOLOCKING        = 16_0000200;	 (* DB_NOLOCKING set. *)
  DB_ENV_NOMMAP           = 16_0000400;	 (* DB_NOMMAP set. *)
  DB_ENV_NOPANIC          = 16_0000800;	 (* Okay if panic set. *)
  DB_ENV_OPEN_CALLED      = 16_0001000;	 (* DB_ENV->open called. *)
  DB_ENV_OVERWRITE        = 16_0002000;	 (* DB_OVERWRITE set. *)
  DB_ENV_PRIVATE          = 16_0004000;	 (* DB_PRIVATE set. *)
  DB_ENV_REGION_INIT      = 16_0008000;	 (* DB_REGION_INIT set. *)
  DB_ENV_REP_CLIENT       = 16_0010000;	 (* Replication client. *)
  DB_ENV_REP_LOGSONLY     = 16_0020000;	 (* Log files only replication site. *)
  DB_ENV_REP_MASTER       = 16_0040000;	 (* Replication master. *)
  DB_ENV_RPCCLIENT        = 16_0080000;	 (* DB_CLIENT set. *)
  DB_ENV_RPCCLIENT_GIVEN  = 16_0100000;	 (* User-supplied RPC client struct *)
  DB_ENV_SYSTEM_MEM       = 16_0200000;	 (* DB_SYSTEM_MEM set. *)
  DB_ENV_THREAD           = 16_0400000;	 (* DB_THREAD set. *)
  DB_ENV_TXN_NOSYNC       = 16_0800000;	 (* DB_TXN_NOSYNC set. *)
  DB_ENV_TXN_WRITE_NOSYNC = 16_1000000;	 (* DB_TXN_WRITE_NOSYNC set. *)
  DB_ENV_YIELDCPU         = 16_2000000;	 (* DB_YIELDCPU set. *)




(* Database Environment *)

<*EXTERNAL*>
PROCEDURE db_env_create (dbenv: UNTRACED REF DB_ENV; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_close (dbenv: DB_ENV; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_dbremove
  (dbenv: DB_ENV; txnid: DB_TXN; file, database: Ctypes.const_char_star;
   flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_dbrename
  (dbenv: DB_ENV; txnid: DB_TXN; file, database, new: Ctypes.const_char_star;
   flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_err
  (dbenv: DB_ENV; error: int; format: Ctypes.const_char_star);

<*EXTERNAL*>
PROCEDURE db_env_errx
  (dbenv: DB_ENV; format: Ctypes.const_char_star);

<*EXTERNAL*>
PROCEDURE db_env_open
  (dbenv: DB_ENV; db_home: Ctypes.const_char_star; flags: u_int32_t;
   mode: int): int;

<*EXTERNAL*>
PROCEDURE db_env_remove
  (dbenv: DB_ENV; db_home: Ctypes.const_char_star; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_strerror (error: int): Ctypes.char_star;

<*EXTERNAL*>
PROCEDURE db_version (VAR major, minor, patch: int): Ctypes.char_star;

(* Environment Configuration *)

<*EXTERNAL*>
PROCEDURE db_env_set_app_dispatch
  (dbenv: DB_ENV; tx_recover: db_env_app_dispatch): int;
TYPE db_env_app_dispatch = PROCEDURE
  (dbenv: DB_ENV; READONLY data: DBT; READONLY lsn: DB_LSN;
   op: db_recops): int;

<*EXTERNAL*>
PROCEDURE db_env_set_alloc
  (dbenv: DB_ENV; malloc: malloc; realloc: realloc; free: free): int;
TYPE malloc  = PROCEDURE (size: Utypes.size_t): Ctypes.void_star;
TYPE realloc = PROCEDURE
  (p: Ctypes.void_star; size: Utypes.size_t): Ctypes.void_star;
TYPE free    = PROCEDURE (p: Ctypes.void_star);

<*EXTERNAL*>
PROCEDURE db_env_set_data_dir
  (dbenv: DB_ENV; dir: Ctypes.const_char_star): int;

<*EXTERNAL*>
PROCEDURE db_env_set_encrypt
  (dbenv: DB_ENV; passwd: Ctypes.const_char_star; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_set_errcall (dbenv: DB_ENV; db_errcall_fcn: errcall);
TYPE errcall = PROCEDURE
  (errpfx: Ctypes.const_char_star; msg: Ctypes.char_star);

<*EXTERNAL*>
PROCEDURE db_env_set_errfile (dbenv: DB_ENV; errfile: Ctypes.void_star);

<*EXTERNAL*>
PROCEDURE db_env_set_errpfx (dbenv: DB_ENV; errpfx: Ctypes.const_char_star);

<*EXTERNAL*>
PROCEDURE db_env_set_feedback (dbenv: DB_ENV; feedback: db_env_feedback): int;
TYPE db_env_feedback  = PROCEDURE (dbenv: DB_ENV; opcode, pct: int);

<*EXTERNAL*>
PROCEDURE db_env_set_flags (dbenv: DB_ENV; flags: u_int32_t; onoff: int): int;

<*EXTERNAL*>
PROCEDURE db_env_set_paniccall
  (dbenv: DB_ENV; paniccall: db_env_paniccall): int;
TYPE db_env_paniccall = PROCEDURE (dbenv: DB_ENV; errval: int);

<*EXTERNAL*>
PROCEDURE db_env_set_rpc_server
  (dbenv: DB_ENV; client: Ctypes.char_star; host: Ctypes.char_star;
   cl_timeout, sv_timeout: Ctypes.long; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_set_shm_key (dbenv: DB_ENV; shm_key: Ctypes.long): int;

<*EXTERNAL*>
PROCEDURE db_env_set_tas_spins (dbenv: DB_ENV; tas_spins: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_set_timeout
  (dbenv: DB_ENV; timeout: db_timeout_t; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_set_tmp_dir (dbenv: DB_ENV; dir: Ctypes.const_char_star): int;

<*EXTERNAL*>
PROCEDURE db_env_set_verbose
  (dbenv: DB_ENV; which: u_int32_t; onoff: int): int;

(* Database Operations *)

<*EXTERNAL*>
PROCEDURE db_create (VAR db: DB; dbenv: DB_ENV; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_associate
  (primary: DB; txnid: DB_TXN; secondary: DB; callback: db_callback;
   flags: u_int32_t): int;
TYPE db_callback = PROCEDURE
  (db: DB; READONLY key, data: DBT; VAR key2: DBT): int;

<*EXTERNAL*>
PROCEDURE db_close (db: DB; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_del
  (db: DB; txnid: DB_TXN; READONLY key: DBT; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_err (db: DB; error: int; format: Ctypes.const_char_star);

<*EXTERNAL*>
PROCEDURE db_errx (db: DB; format: Ctypes.const_char_star);

<*EXTERNAL*>
PROCEDURE db_fd (db: DB; VAR fd: int): int;

<*EXTERNAL*>
PROCEDURE db_get
  (db: DB; txnid: DB_TXN; READONLY key: DBT; VAR data: DBT;
   flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_pget
  (db: DB; txnid: DB_TXN; READONLY key: DBT; VAR pkey, data: DBT;
   flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_getbyteswapped (db: DB; VAR isswapped: int): int;

<*EXTERNAL*>
PROCEDURE db_get_type (db: DB; VAR type: DBTYPE): int;

<*EXTERNAL*>
PROCEDURE db_join
  (primary: DB; curslist: UNTRACED REF DBC; VAR dbcp: DBC;
   flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_key_range
  (db: DB; txnid: DB_TXN; READONLY key: DBT; VAR key_range: DB_KEY_RANGE;
   flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_open
  (db: DB; txnid: DB_TXN; file, database: Ctypes.const_char_star;
   type: DBTYPE; flags: u_int32_t; mode: int): int;

<*EXTERNAL*>
PROCEDURE db_put
  (db: DB; txnid: DB_TXN; READONLY key, data: DBT; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_remove
  (db: DB; file, database: Ctypes.const_char_star; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_rename
  (db: DB; file, database, new: Ctypes.const_char_star; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_stat (db: DB; VAR stat: ADDRESS; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_sync (db: DB; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_truncate
  (db: DB; txnid: DB_TXN; VAR countp: u_int32_t; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_upgrade
  (db: DB; file: Ctypes.const_char_star; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_verify
  (db: DB; file, database: Ctypes.const_char_star; outfile: Ctypes.void_star;
   flags: u_int32_t): int;

(* Database Configuration *)

<*EXTERNAL*>
PROCEDURE db_set_alloc
  (db: DB; malloc: malloc; realloc: realloc; free: free): int;

<*EXTERNAL*>
PROCEDURE db_set_cache_priority (db: DB; priority: DB_CACHE_PRIORITY): int;

<*EXTERNAL*>
PROCEDURE db_set_cachesize
  (db: DB; gbytes, bytes: u_int32_t; ncache: int): int;

<*EXTERNAL*>
PROCEDURE db_set_dup_compare (db: DB; dup_compare: dup_compare): int;
TYPE dup_compare  = PROCEDURE (db: DB; READONLY data1, data2: DBT): int;

<*EXTERNAL*>
PROCEDURE db_set_encrypt
  (dbenv: DB_ENV; passwd: Ctypes.const_char_star; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_errcall (db: DB; errcall: errcall);

<*EXTERNAL*>
PROCEDURE db_errfile (db: DB; errfile: Ctypes.void_star);

<*EXTERNAL*>
PROCEDURE db_set_errpfx (db: DB; errpfx: Ctypes.const_char_star);

<*EXTERNAL*>
PROCEDURE db_set_feedback (db: DB; feedback: db_feedback): int;
TYPE db_feedback  = PROCEDURE (db: DB; opcode, pct: int);

<*EXTERNAL*>
PROCEDURE db_set_flags (db: DB; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_set_lorder (db: DB; lorder: int): int;

<*EXTERNAL*>
PROCEDURE db_set_pagesize (db: DB; pagesize: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_set_paniccall (db: DB; paniccall: db_paniccall): int;
TYPE db_paniccall = PROCEDURE (db: DB; errval: int);

(* Btree/Recno Configuration *)

<*EXTERNAL*>
PROCEDURE db_set_append_recno (db: DB; fcn: db_append_recno): int;
TYPE db_append_recno = PROCEDURE
  (db: DB; VAR data: DBT; recno: db_recno_t): int;

<*EXTERNAL*>
PROCEDURE db_set_bt_compare (db: DB; bt_compare: db_bt_compare): int;
TYPE db_bt_compare = PROCEDURE (db: DB; READONLY data1, data2: DBT): int;

<*EXTERNAL*>
PROCEDURE db_set_bt_minkey (db: DB; minkey: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_set_bt_prefix (db: DB; bt_prefix: db_bt_prefix): int;
TYPE db_bt_prefix = PROCEDURE
  (db: DB; READONLY data1, data2: DBT): Utypes.size_t;

<*EXTERNAL*>
PROCEDURE db_set_re_delim (db: DB; re_delim: int): int;

<*EXTERNAL*>
PROCEDURE db_set_re_len (db: DB; re_len: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_set_re_pad (db: DB; re_pad: int): int;

<*EXTERNAL*>
PROCEDURE db_set_re_source (db: DB; re_source: Ctypes.char_star): int;

(* Hash Configuration *)

<*EXTERNAL*>
PROCEDURE db_set_h_ffactor (db: DB; h_ffactor: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_set_h_hash ( db: DB; h_hash: db_h_hash): int;
TYPE db_h_hash = PROCEDURE
  (db: DB; bytes: Ctypes.const_void_star; length: u_int32_t): u_int32_t;

<*EXTERNAL*>
PROCEDURE db_set_h_nelem (db: DB; h_nelem: u_int32_t): int;

(* Queue Configuration *)

<*EXTERNAL*>
PROCEDURE db_set_q_extentsize (db: DB; q_extentsize: u_int32_t): int;

(* Database Cursor Operations *)

<*EXTERNAL*>
PROCEDURE db_cursor
  (db: DB; txnid: DB_TXN; VAR cursorp: DBC; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE dbcursor_c_close (cursor: DBC): int;

<*EXTERNAL*>
PROCEDURE dbcursor_c_count
  (cursor: DBC; VAR count: db_recno_t; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE dbcursor_c_del (cursor: DBC; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE dbcursor_c_dup (cursor: DBC; VAR dup: DBC; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE dbcursor_c_get
  (cursor: DBC; VAR key, data: DBT; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE dbcursor_c_pget
  (cursor: DBC; VAR key, pkey, data: DBT; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE dbcursor_c_put
  (cursor: DBC; READONLY key, data: DBT; flags: u_int32_t): int;

(* Lock Subsystem *)

<*EXTERNAL*>
PROCEDURE db_env_set_lk_conflicts
  (dbenv: DB_ENV; conflicts: UNTRACED REF u_int8_t; nmodes: int): int;

<*EXTERNAL*>
PROCEDURE db_env_set_lk_detect (dbenv: DB_ENV; detect: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_set_lk_max_lockers (dbenv: DB_ENV; max: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_set_lk_max_locks (dbenv: DB_ENV; max: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_set_lk_max_objects (dbenv: DB_ENV; max: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_lock_detect
  (dbenv: DB_ENV; flags, atype: u_int32_t; VAR aborted: int): int;
<*EXTERNAL*>
PROCEDURE db_env_lock_get
  (dbenv: DB_ENV; locker, flags: u_int32_t; READONLY obj: DBT;
   lock_mode: db_lockmode_t; VAR lock: DB_LOCK): int;

<*EXTERNAL*>
PROCEDURE db_env_lock_id (dbenv: DB_ENV; VAR id: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_lock_id_free (dbenv: DB_ENV; id: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_lock_put (dbenv: DB_ENV; READONLY lock: DB_LOCK): int;

<*EXTERNAL*>
PROCEDURE db_env_lock_stat
  (dbenv: DB_ENV; VAR stat: UNTRACED REF DB_LOCK_STAT; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_lock_vec
  (dbenv: DB_ENV; locker, flags: u_int32_t; list: UNTRACED REF DB_LOCKREQ;
   nlist: int; VAR elist: UNTRACED REF DB_LOCKREQ): int;

(* Log Subsystem *)

<*EXTERNAL*>
PROCEDURE db_env_set_lg_bsize (dbenv: DB_ENV; lg_bsize: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_set_lg_dir (dbenv: DB_ENV; dir: Ctypes.const_char_star): int;

<*EXTERNAL*>
PROCEDURE db_env_set_lg_max (dbenv: DB_ENV; max: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_set_lg_regionmax (dbenv: DB_ENV; max: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_log_archive
  (dbenv: DB_ENV; VAR list: UNTRACED REF Ctypes.char_star;
   flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_log_file
  (dbenv: DB_ENV; READONLY lsn: DB_LSN; namep: Ctypes.char_star;
   len: Utypes.size_t): int;

<*EXTERNAL*>
PROCEDURE db_env_log_flush (dbenv: DB_ENV; READONLY lsn: DB_LSN): int;

<*EXTERNAL*>
PROCEDURE db_env_log_put
  (dbenv: DB_ENV; VAR lsn: DB_LSN; READONLY data: DBT; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_log_stat
  (dbenv: DB_ENV; VAR stat: UNTRACED REF DB_LOG_STAT; flags: u_int32_t): int;

(* Log Cursor Operations *)

<*EXTERNAL*>
PROCEDURE db_env_log_cursor
  (dbenv: DB_ENV; VAR cursor: DB_LOGC; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_logc_close (logc: DB_LOGC; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_logc_get
  (logc: DB_LOGC; READONLY lsn: DB_LSN; VAR data: DBT; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE log_compare (READONLY lsn0, lsn1: DB_LSN): int;

(* Memory Pool Subsystem *)

<*EXTERNAL*>
PROCEDURE db_env_set_cachesize
  (dbenv: DB_ENV; gbytes, bytes: u_int32_t; ncache: int): int;

<*EXTERNAL*>
PROCEDURE db_env_set_mp_mmapsize
  (dbenv: DB_ENV; mp_mmapsize: Utypes.size_t): int;

<*EXTERNAL*>
PROCEDURE db_env_memp_register
  (dbenv: DB_ENV; ftype: int; pgin, pgout: pginout): int;
TYPE pginout = PROCEDURE
  (dbenv: DB_ENV; pgno: db_pgno_t; pgaddr: Ctypes.void_star;
   VAR pgcookie: DBT): int;

<*EXTERNAL*>
PROCEDURE db_env_memp_stat
  (dbenv: DB_ENV; VAR gsp: UNTRACED REF DB_MPOOL_STAT;
   VAR fsp: UNTRACED REF UNTRACED REF DB_MPOOL_FSTAT; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_memp_sync (dbenv: DB_ENV; READONLY lsn: DB_LSN): int;

<*EXTERNAL*>
PROCEDURE db_env_memp_trickle (dbenv: DB_ENV; pct: int; VAR nwrotep: int): int;

(* Memory Pool Files *)

<*EXTERNAL*>
PROCEDURE db_env_memp_fcreate
  (dbenv: DB_ENV; VAR dbmfp: UNTRACED REF DB_MPOOLFILE; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_mpoolfile_close (mpf: DB_MPOOLFILE; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_mpoolfile_get
  (mpf: DB_MPOOLFILE; VAR pageno: db_pgno_t; flags: u_int32_t;
   VAR pagep: Ctypes.void_star): int;

<*EXTERNAL*>
PROCEDURE db_mpoolfile_open
  (mpf: DB_MPOOLFILE; file: Ctypes.const_char_star; flags: u_int32_t;
   mode: int; pagesize: Utypes.size_t): int;

<*EXTERNAL*>
PROCEDURE db_mpoolfile_put
  (mpf: DB_MPOOLFILE; pgaddr: Ctypes.void_star; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_mpoolfile_set
  (mpf: DB_MPOOLFILE; pgaddr: Ctypes.void_star; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_mpoolfile_sync (mpf: DB_MPOOLFILE): int;

<*EXTERNAL*>
PROCEDURE db_mpoolfile_set_clear_len (mpf: DB_MPOOLFILE; len: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_mpoolfile_set_fileid
  (mpf: DB_MPOOLFILE; VAR fileid: u_int8_t): int;

<*EXTERNAL*>
PROCEDURE db_mpoolfile_set_ftype (mpf: DB_MPOOLFILE; ftype: int): int;

<*EXTERNAL*>
PROCEDURE db_mpoolfile_set_lsn_offset
  (mpf: DB_MPOOLFILE; lsn_offset: int32_t): int;

<*EXTERNAL*>
PROCEDURE db_mpoolfile_set_pgcookie
  (mpf: DB_MPOOLFILE; READONLY pgcookie: DBT): int;

(* Transaction Subsystem *)

<*EXTERNAL*>
PROCEDURE db_env_set_tx_max (dbenv: DB_ENV; max: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_set_tx_timestamp
  (dbenv: DB_ENV; timestamp: Utypes.time_t): int;

<*EXTERNAL*>
PROCEDURE db_env_txn_checkpoint
  (dbenv: DB_ENV; kbyte, min, flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_txn_recover
  (dbenv: DB_ENV; preplist: UNTRACED REF DB_PREPLIST; count: Ctypes.long;
   VAR retp: Ctypes.long; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_txn_stat
  (dbenv: DB_ENV; VAR stat: UNTRACED REF DB_TXN_STAT; flags: u_int32_t): int;

(* Transactions *)

<*EXTERNAL*>
PROCEDURE db_env_txn_begin
  (dbenv: DB_ENV; parent: DB_TXN; VAR id: DB_TXN; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_txn_abort (tid: DB_TXN): int;

<*EXTERNAL*>
PROCEDURE db_txn_commit (tid: DB_TXN; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_txn_discard (tid: DB_TXN; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_txn_id (tid: DB_TXN): u_int32_t;

<*EXTERNAL*>
PROCEDURE db_txn_prepare
  (tid: DB_TXN; gid: ARRAY [0..DB_XIDDATASIZE-1] OF u_int8_t): int;

<*EXTERNAL*>
PROCEDURE db_txn_set_timeout
  (tid: DB_TXN; timeout: db_timeout_t; flags: u_int32_t): u_int32_t;

<*EXTERNAL*>
PROCEDURE db_txn_last_lsn
  (tid: DB_TXN; VAR lsn: DB_LSN);

<*EXTERNAL*>
PROCEDURE db_env_set_rep_transport
  (dbenv: DB_ENV; envid: int; send: db_env_send): int;
TYPE db_env_send = PROCEDURE
  (dbenv: DB_ENV; READONLY control, rec: DBT; envid: int;
   flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_rep_elect
  (dbenv: DB_ENV; nsites, priority: int; timeout: u_int32_t;
   VAR envid: int): int;

<*EXTERNAL*>
PROCEDURE db_env_set_rep_limit
  (dbenv: DB_ENV; gbytes, bytes: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_rep_process_message
  (dbenv: DB_ENV; VAR control, rec: DBT; VAR envid: int): int;

<*EXTERNAL*>
PROCEDURE db_env_rep_start
  (dbenv: DB_ENV; VAR cdata: DBT; flags: u_int32_t): int;

<*EXTERNAL*>
PROCEDURE db_env_rep_stat
  (dbenv: DB_ENV; VAR statp: UNTRACED REF DB_REP_STAT; flags: u_int32_t): int;

TYPE yieldProc = PROCEDURE(): INTEGER;
<*EXTERNAL*>
PROCEDURE db_env_set_func_yield
  (proc: yieldProc): INTEGER;

END BerkeleyDB.
