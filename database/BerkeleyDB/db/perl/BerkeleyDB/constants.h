#define PERL_constant_NOTFOUND	1
#define PERL_constant_NOTDEF	2
#define PERL_constant_ISIV	3
#define PERL_constant_ISNO	4
#define PERL_constant_ISNV	5
#define PERL_constant_ISPV	6
#define PERL_constant_ISPVN	7
#define PERL_constant_ISSV	8
#define PERL_constant_ISUNDEF	9
#define PERL_constant_ISUV	10
#define PERL_constant_ISYES	11

#ifndef NVTYPE
typedef double NV; /* 5.6 and later define NVTYPE, and typedef NV to it.  */
#endif
#ifndef aTHX_
#define aTHX_ /* 5.6 or later define this for threading support.  */
#endif
#ifndef pTHX_
#define pTHX_ /* 5.6 or later define this for threading support.  */
#endif

static int
constant_6 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_DUP DB_PAD DB_RMW DB_SET */
  /* Offset 3 gives the best switch position.  */
  switch (name[3]) {
  case 'D':
    if (memEQ(name, "DB_DUP", 6)) {
    /*                  ^        */
#ifdef DB_DUP
      *iv_return = DB_DUP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "DB_PAD", 6)) {
    /*                  ^        */
#ifdef DB_PAD
      *iv_return = DB_PAD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "DB_RMW", 6)) {
    /*                  ^        */
#ifdef DB_RMW
      *iv_return = DB_RMW;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "DB_SET", 6)) {
    /*                  ^        */
#ifdef DB_SET
      *iv_return = DB_SET;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_7 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_EXCL DB_HASH DB_LAST DB_NEXT DB_PREV */
  /* Offset 3 gives the best switch position.  */
  switch (name[3]) {
  case 'E':
    if (memEQ(name, "DB_EXCL", 7)) {
    /*                  ^         */
#ifdef DB_EXCL
      *iv_return = DB_EXCL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'H':
    if (memEQ(name, "DB_HASH", 7)) {
    /*                  ^         */
#if (DB_VERSION_MAJOR > 2) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \
     DB_VERSION_PATCH >= 0)
      *iv_return = DB_HASH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "DB_LAST", 7)) {
    /*                  ^         */
#ifdef DB_LAST
      *iv_return = DB_LAST;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "DB_NEXT", 7)) {
    /*                  ^         */
#ifdef DB_NEXT
      *iv_return = DB_NEXT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "DB_PREV", 7)) {
    /*                  ^         */
#ifdef DB_PREV
      *iv_return = DB_PREV;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_8 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_AFTER DB_BTREE DB_FIRST DB_FLUSH DB_FORCE DB_QUEUE DB_RECNO */
  /* Offset 4 gives the best switch position.  */
  switch (name[4]) {
  case 'E':
    if (memEQ(name, "DB_RECNO", 8)) {
    /*                   ^         */
#if (DB_VERSION_MAJOR > 2) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \
     DB_VERSION_PATCH >= 0)
      *iv_return = DB_RECNO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'F':
    if (memEQ(name, "DB_AFTER", 8)) {
    /*                   ^         */
#ifdef DB_AFTER
      *iv_return = DB_AFTER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "DB_FIRST", 8)) {
    /*                   ^         */
#ifdef DB_FIRST
      *iv_return = DB_FIRST;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "DB_FLUSH", 8)) {
    /*                   ^         */
#ifdef DB_FLUSH
      *iv_return = DB_FLUSH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "DB_FORCE", 8)) {
    /*                   ^         */
#ifdef DB_FORCE
      *iv_return = DB_FORCE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "DB_BTREE", 8)) {
    /*                   ^         */
#if (DB_VERSION_MAJOR > 2) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \
     DB_VERSION_PATCH >= 0)
      *iv_return = DB_BTREE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'U':
    if (memEQ(name, "DB_QUEUE", 8)) {
    /*                   ^         */
#if (DB_VERSION_MAJOR > 3) || \
    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR > 0) || \
    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR == 0 && \
     DB_VERSION_PATCH >= 55)
      *iv_return = DB_QUEUE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_9 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_APPEND DB_BEFORE DB_CLIENT DB_COMMIT DB_CREATE DB_CURLSN DB_DIRECT
     DB_EXTENT DB_GETREC DB_NOCOPY DB_NOMMAP DB_NOSYNC DB_RDONLY DB_RECNUM
     DB_THREAD DB_VERIFY */
  /* Offset 7 gives the best switch position.  */
  switch (name[7]) {
  case 'A':
    if (memEQ(name, "DB_NOMMAP", 9)) {
    /*                      ^       */
#ifdef DB_NOMMAP
      *iv_return = DB_NOMMAP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_THREAD", 9)) {
    /*                      ^       */
#ifdef DB_THREAD
      *iv_return = DB_THREAD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'C':
    if (memEQ(name, "DB_DIRECT", 9)) {
    /*                      ^       */
#ifdef DB_DIRECT
      *iv_return = DB_DIRECT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "DB_GETREC", 9)) {
    /*                      ^       */
#ifdef DB_GETREC
      *iv_return = DB_GETREC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'F':
    if (memEQ(name, "DB_VERIFY", 9)) {
    /*                      ^       */
#ifdef DB_VERIFY
      *iv_return = DB_VERIFY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "DB_COMMIT", 9)) {
    /*                      ^       */
#ifdef DB_COMMIT
      *iv_return = DB_COMMIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "DB_RDONLY", 9)) {
    /*                      ^       */
#ifdef DB_RDONLY
      *iv_return = DB_RDONLY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "DB_APPEND", 9)) {
    /*                      ^       */
#ifdef DB_APPEND
      *iv_return = DB_APPEND;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_CLIENT", 9)) {
    /*                      ^       */
#ifdef DB_CLIENT
      *iv_return = DB_CLIENT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_EXTENT", 9)) {
    /*                      ^       */
#ifdef DB_EXTENT
      *iv_return = DB_EXTENT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_NOSYNC", 9)) {
    /*                      ^       */
#ifdef DB_NOSYNC
      *iv_return = DB_NOSYNC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "DB_NOCOPY", 9)) {
    /*                      ^       */
#ifdef DB_NOCOPY
      *iv_return = DB_NOCOPY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "DB_BEFORE", 9)) {
    /*                      ^       */
#ifdef DB_BEFORE
      *iv_return = DB_BEFORE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "DB_CURLSN", 9)) {
    /*                      ^       */
#ifdef DB_CURLSN
      *iv_return = DB_CURLSN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "DB_CREATE", 9)) {
    /*                      ^       */
#ifdef DB_CREATE
      *iv_return = DB_CREATE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'U':
    if (memEQ(name, "DB_RECNUM", 9)) {
    /*                      ^       */
#ifdef DB_RECNUM
      *iv_return = DB_RECNUM;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_10 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_CONSUME DB_CURRENT DB_DELETED DB_DUPSORT DB_ENCRYPT DB_ENV_CDB
     DB_ENV_TXN DB_JOINENV DB_KEYLAST DB_NOPANIC DB_OK_HASH DB_PRIVATE
     DB_PR_PAGE DB_RECOVER DB_SALVAGE DB_TIMEOUT DB_TXN_CKP DB_UNKNOWN
     DB_UPGRADE */
  /* Offset 8 gives the best switch position.  */
  switch (name[8]) {
  case 'D':
    if (memEQ(name, "DB_ENV_CDB", 10)) {
    /*                       ^        */
#ifdef DB_ENV_CDB
      *iv_return = DB_ENV_CDB;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_UPGRADE", 10)) {
    /*                       ^        */
#ifdef DB_UPGRADE
      *iv_return = DB_UPGRADE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "DB_DELETED", 10)) {
    /*                       ^        */
#ifdef DB_DELETED
      *iv_return = DB_DELETED;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_RECOVER", 10)) {
    /*                       ^        */
#ifdef DB_RECOVER
      *iv_return = DB_RECOVER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'G':
    if (memEQ(name, "DB_PR_PAGE", 10)) {
    /*                       ^        */
#ifdef DB_PR_PAGE
      *iv_return = DB_PR_PAGE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_SALVAGE", 10)) {
    /*                       ^        */
#ifdef DB_SALVAGE
      *iv_return = DB_SALVAGE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "DB_NOPANIC", 10)) {
    /*                       ^        */
#ifdef DB_NOPANIC
      *iv_return = DB_NOPANIC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'K':
    if (memEQ(name, "DB_TXN_CKP", 10)) {
    /*                       ^        */
#ifdef DB_TXN_CKP
      *iv_return = DB_TXN_CKP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "DB_CONSUME", 10)) {
    /*                       ^        */
#ifdef DB_CONSUME
      *iv_return = DB_CONSUME;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "DB_CURRENT", 10)) {
    /*                       ^        */
#ifdef DB_CURRENT
      *iv_return = DB_CURRENT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_JOINENV", 10)) {
    /*                       ^        */
#ifdef DB_JOINENV
      *iv_return = DB_JOINENV;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "DB_ENCRYPT", 10)) {
    /*                       ^        */
#ifdef DB_ENCRYPT
      *iv_return = DB_ENCRYPT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "DB_DUPSORT", 10)) {
    /*                       ^        */
#ifdef DB_DUPSORT
      *iv_return = DB_DUPSORT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "DB_KEYLAST", 10)) {
    /*                       ^        */
#ifdef DB_KEYLAST
      *iv_return = DB_KEYLAST;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_OK_HASH", 10)) {
    /*                       ^        */
#ifdef DB_OK_HASH
      *iv_return = DB_OK_HASH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "DB_PRIVATE", 10)) {
    /*                       ^        */
#ifdef DB_PRIVATE
      *iv_return = DB_PRIVATE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'U':
    if (memEQ(name, "DB_TIMEOUT", 10)) {
    /*                       ^        */
#ifdef DB_TIMEOUT
      *iv_return = DB_TIMEOUT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'W':
    if (memEQ(name, "DB_UNKNOWN", 10)) {
    /*                       ^        */
#if (DB_VERSION_MAJOR > 2) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \
     DB_VERSION_PATCH >= 0)
      *iv_return = DB_UNKNOWN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'X':
    if (memEQ(name, "DB_ENV_TXN", 10)) {
    /*                       ^        */
#ifdef DB_ENV_TXN
      *iv_return = DB_ENV_TXN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_11 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_APP_INIT DB_ARCH_ABS DB_ARCH_LOG DB_FIXEDLEN DB_GET_BOTH DB_INIT_CDB
     DB_INIT_LOG DB_INIT_TXN DB_KEYEMPTY DB_KEYEXIST DB_KEYFIRST DB_LOCKDOWN
     DB_LOCK_GET DB_LOCK_PUT DB_LOGMAGIC DB_LOG_DISK DB_MULTIPLE DB_NEXT_DUP
     DB_NOSERVER DB_NOTFOUND DB_OK_BTREE DB_OK_QUEUE DB_OK_RECNO DB_POSITION
     DB_QAMMAGIC DB_RENUMBER DB_SNAPSHOT DB_TRUNCATE DB_TXNMAGIC DB_TXN_LOCK
     DB_TXN_REDO DB_TXN_SYNC DB_TXN_UNDO DB_WRNOSYNC DB_YIELDCPU */
  /* Offset 8 gives the best switch position.  */
  switch (name[8]) {
  case 'A':
    if (memEQ(name, "DB_ARCH_ABS", 11)) {
    /*                       ^         */
#ifdef DB_ARCH_ABS
      *iv_return = DB_ARCH_ABS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TRUNCATE", 11)) {
    /*                       ^         */
#ifdef DB_TRUNCATE
      *iv_return = DB_TRUNCATE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'B':
    if (memEQ(name, "DB_RENUMBER", 11)) {
    /*                       ^         */
#ifdef DB_RENUMBER
      *iv_return = DB_RENUMBER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'C':
    if (memEQ(name, "DB_INIT_CDB", 11)) {
    /*                       ^         */
#ifdef DB_INIT_CDB
      *iv_return = DB_INIT_CDB;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_OK_RECNO", 11)) {
    /*                       ^         */
#ifdef DB_OK_RECNO
      *iv_return = DB_OK_RECNO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_YIELDCPU", 11)) {
    /*                       ^         */
#ifdef DB_YIELDCPU
      *iv_return = DB_YIELDCPU;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'D':
    if (memEQ(name, "DB_NEXT_DUP", 11)) {
    /*                       ^         */
#ifdef DB_NEXT_DUP
      *iv_return = DB_NEXT_DUP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "DB_OK_QUEUE", 11)) {
    /*                       ^         */
#ifdef DB_OK_QUEUE
      *iv_return = DB_OK_QUEUE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_REDO", 11)) {
    /*                       ^         */
#ifdef DB_TXN_REDO
      *iv_return = DB_TXN_REDO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'G':
    if (memEQ(name, "DB_LOCK_GET", 11)) {
    /*                       ^         */
#if (DB_VERSION_MAJOR > 2) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \
     DB_VERSION_PATCH >= 0)
      *iv_return = DB_LOCK_GET;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOGMAGIC", 11)) {
    /*                       ^         */
#ifdef DB_LOGMAGIC
      *iv_return = DB_LOGMAGIC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_QAMMAGIC", 11)) {
    /*                       ^         */
#ifdef DB_QAMMAGIC
      *iv_return = DB_QAMMAGIC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXNMAGIC", 11)) {
    /*                       ^         */
#ifdef DB_TXNMAGIC
      *iv_return = DB_TXNMAGIC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'H':
    if (memEQ(name, "DB_SNAPSHOT", 11)) {
    /*                       ^         */
#ifdef DB_SNAPSHOT
      *iv_return = DB_SNAPSHOT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "DB_KEYEXIST", 11)) {
    /*                       ^         */
#ifdef DB_KEYEXIST
      *iv_return = DB_KEYEXIST;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOG_DISK", 11)) {
    /*                       ^         */
#ifdef DB_LOG_DISK
      *iv_return = DB_LOG_DISK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_POSITION", 11)) {
    /*                       ^         */
#ifdef DB_POSITION
      *iv_return = DB_POSITION;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "DB_ARCH_LOG", 11)) {
    /*                       ^         */
#ifdef DB_ARCH_LOG
      *iv_return = DB_ARCH_LOG;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_FIXEDLEN", 11)) {
    /*                       ^         */
#ifdef DB_FIXEDLEN
      *iv_return = DB_FIXEDLEN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_INIT_LOG", 11)) {
    /*                       ^         */
#ifdef DB_INIT_LOG
      *iv_return = DB_INIT_LOG;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "DB_APP_INIT", 11)) {
    /*                       ^         */
#ifdef DB_APP_INIT
      *iv_return = DB_APP_INIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_UNDO", 11)) {
    /*                       ^         */
#ifdef DB_TXN_UNDO
      *iv_return = DB_TXN_UNDO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "DB_GET_BOTH", 11)) {
    /*                       ^         */
#ifdef DB_GET_BOTH
      *iv_return = DB_GET_BOTH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCKDOWN", 11)) {
    /*                       ^         */
#ifdef DB_LOCKDOWN
      *iv_return = DB_LOCKDOWN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_LOCK", 11)) {
    /*                       ^         */
#ifdef DB_TXN_LOCK
      *iv_return = DB_TXN_LOCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "DB_KEYEMPTY", 11)) {
    /*                       ^         */
#ifdef DB_KEYEMPTY
      *iv_return = DB_KEYEMPTY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_PUT", 11)) {
    /*                       ^         */
#if (DB_VERSION_MAJOR > 2) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \
     DB_VERSION_PATCH >= 0)
      *iv_return = DB_LOCK_PUT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_MULTIPLE", 11)) {
    /*                       ^         */
#ifdef DB_MULTIPLE
      *iv_return = DB_MULTIPLE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "DB_KEYFIRST", 11)) {
    /*                       ^         */
#ifdef DB_KEYFIRST
      *iv_return = DB_KEYFIRST;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_OK_BTREE", 11)) {
    /*                       ^         */
#ifdef DB_OK_BTREE
      *iv_return = DB_OK_BTREE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "DB_INIT_TXN", 11)) {
    /*                       ^         */
#ifdef DB_INIT_TXN
      *iv_return = DB_INIT_TXN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'U':
    if (memEQ(name, "DB_NOTFOUND", 11)) {
    /*                       ^         */
#ifdef DB_NOTFOUND
      *iv_return = DB_NOTFOUND;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'V':
    if (memEQ(name, "DB_NOSERVER", 11)) {
    /*                       ^         */
#ifdef DB_NOSERVER
      *iv_return = DB_NOSERVER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'Y':
    if (memEQ(name, "DB_TXN_SYNC", 11)) {
    /*                       ^         */
#ifdef DB_TXN_SYNC
      *iv_return = DB_TXN_SYNC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_WRNOSYNC", 11)) {
    /*                       ^         */
#ifdef DB_WRNOSYNC
      *iv_return = DB_WRNOSYNC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_12 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_ARCH_DATA DB_CDB_ALLDB DB_CL_WRITER DB_DELIMITER DB_DIRECT_DB
     DB_DUPCURSOR DB_ENV_FATAL DB_FAST_STAT DB_GET_BOTHC DB_GET_RECNO
     DB_HASHMAGIC DB_INIT_LOCK DB_JOIN_ITEM DB_LOCKMAGIC DB_LOCK_DUMP
     DB_LOCK_RW_N DB_LOGOLDVER DB_MAX_PAGES DB_MPOOL_NEW DB_NEEDSPLIT
     DB_NODUPDATA DB_NOLOCKING DB_NORECURSE DB_OVERWRITE DB_PAGEYIELD
     DB_PAGE_LOCK DB_PERMANENT DB_POSITIONI DB_PRINTABLE DB_QAMOLDVER
     DB_SET_RANGE DB_SET_RECNO DB_SWAPBYTES DB_TEMPORARY DB_TXN_ABORT
     DB_TXN_APPLY DB_TXN_PRINT DB_WRITELOCK DB_WRITEOPEN DB_XA_CREATE */
  /* Offset 3 gives the best switch position.  */
  switch (name[3]) {
  case 'A':
    if (memEQ(name, "DB_ARCH_DATA", 12)) {
    /*                  ^               */
#ifdef DB_ARCH_DATA
      *iv_return = DB_ARCH_DATA;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'C':
    if (memEQ(name, "DB_CDB_ALLDB", 12)) {
    /*                  ^               */
#ifdef DB_CDB_ALLDB
      *iv_return = DB_CDB_ALLDB;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_CL_WRITER", 12)) {
    /*                  ^               */
#ifdef DB_CL_WRITER
      *iv_return = DB_CL_WRITER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'D':
    if (memEQ(name, "DB_DELIMITER", 12)) {
    /*                  ^               */
#ifdef DB_DELIMITER
      *iv_return = DB_DELIMITER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_DIRECT_DB", 12)) {
    /*                  ^               */
#ifdef DB_DIRECT_DB
      *iv_return = DB_DIRECT_DB;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_DUPCURSOR", 12)) {
    /*                  ^               */
#ifdef DB_DUPCURSOR
      *iv_return = DB_DUPCURSOR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "DB_ENV_FATAL", 12)) {
    /*                  ^               */
#ifdef DB_ENV_FATAL
      *iv_return = DB_ENV_FATAL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'F':
    if (memEQ(name, "DB_FAST_STAT", 12)) {
    /*                  ^               */
#ifdef DB_FAST_STAT
      *iv_return = DB_FAST_STAT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'G':
    if (memEQ(name, "DB_GET_BOTHC", 12)) {
    /*                  ^               */
#ifdef DB_GET_BOTHC
      *iv_return = DB_GET_BOTHC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_GET_RECNO", 12)) {
    /*                  ^               */
#ifdef DB_GET_RECNO
      *iv_return = DB_GET_RECNO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'H':
    if (memEQ(name, "DB_HASHMAGIC", 12)) {
    /*                  ^               */
#ifdef DB_HASHMAGIC
      *iv_return = DB_HASHMAGIC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "DB_INIT_LOCK", 12)) {
    /*                  ^               */
#ifdef DB_INIT_LOCK
      *iv_return = DB_INIT_LOCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'J':
    if (memEQ(name, "DB_JOIN_ITEM", 12)) {
    /*                  ^               */
#ifdef DB_JOIN_ITEM
      *iv_return = DB_JOIN_ITEM;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "DB_LOCKMAGIC", 12)) {
    /*                  ^               */
#ifdef DB_LOCKMAGIC
      *iv_return = DB_LOCKMAGIC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_DUMP", 12)) {
    /*                  ^               */
#if (DB_VERSION_MAJOR > 2) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \
     DB_VERSION_PATCH >= 0)
      *iv_return = DB_LOCK_DUMP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_RW_N", 12)) {
    /*                  ^               */
#ifdef DB_LOCK_RW_N
      *iv_return = DB_LOCK_RW_N;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOGOLDVER", 12)) {
    /*                  ^               */
#ifdef DB_LOGOLDVER
      *iv_return = DB_LOGOLDVER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "DB_MAX_PAGES", 12)) {
    /*                  ^               */
#ifdef DB_MAX_PAGES
      *iv_return = DB_MAX_PAGES;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_MPOOL_NEW", 12)) {
    /*                  ^               */
#ifdef DB_MPOOL_NEW
      *iv_return = DB_MPOOL_NEW;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "DB_NEEDSPLIT", 12)) {
    /*                  ^               */
#ifdef DB_NEEDSPLIT
      *iv_return = DB_NEEDSPLIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_NODUPDATA", 12)) {
    /*                  ^               */
#ifdef DB_NODUPDATA
      *iv_return = DB_NODUPDATA;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_NOLOCKING", 12)) {
    /*                  ^               */
#ifdef DB_NOLOCKING
      *iv_return = DB_NOLOCKING;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_NORECURSE", 12)) {
    /*                  ^               */
#ifdef DB_NORECURSE
      *iv_return = DB_NORECURSE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "DB_OVERWRITE", 12)) {
    /*                  ^               */
#ifdef DB_OVERWRITE
      *iv_return = DB_OVERWRITE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "DB_PAGEYIELD", 12)) {
    /*                  ^               */
#ifdef DB_PAGEYIELD
      *iv_return = DB_PAGEYIELD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_PAGE_LOCK", 12)) {
    /*                  ^               */
#ifdef DB_PAGE_LOCK
      *iv_return = DB_PAGE_LOCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_PERMANENT", 12)) {
    /*                  ^               */
#ifdef DB_PERMANENT
      *iv_return = DB_PERMANENT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_POSITIONI", 12)) {
    /*                  ^               */
#ifdef DB_POSITIONI
      *iv_return = DB_POSITIONI;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_PRINTABLE", 12)) {
    /*                  ^               */
#ifdef DB_PRINTABLE
      *iv_return = DB_PRINTABLE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'Q':
    if (memEQ(name, "DB_QAMOLDVER", 12)) {
    /*                  ^               */
#ifdef DB_QAMOLDVER
      *iv_return = DB_QAMOLDVER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "DB_SET_RANGE", 12)) {
    /*                  ^               */
#ifdef DB_SET_RANGE
      *iv_return = DB_SET_RANGE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_SET_RECNO", 12)) {
    /*                  ^               */
#ifdef DB_SET_RECNO
      *iv_return = DB_SET_RECNO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_SWAPBYTES", 12)) {
    /*                  ^               */
#ifdef DB_SWAPBYTES
      *iv_return = DB_SWAPBYTES;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "DB_TEMPORARY", 12)) {
    /*                  ^               */
#ifdef DB_TEMPORARY
      *iv_return = DB_TEMPORARY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_ABORT", 12)) {
    /*                  ^               */
#if (DB_VERSION_MAJOR > 3) || \
    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR > 1) || \
    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR == 1 && \
     DB_VERSION_PATCH >= 12)
      *iv_return = DB_TXN_ABORT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_APPLY", 12)) {
    /*                  ^               */
#if (DB_VERSION_MAJOR > 4) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 0 && \
     DB_VERSION_PATCH >= 7)
      *iv_return = DB_TXN_APPLY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_PRINT", 12)) {
    /*                  ^               */
#if (DB_VERSION_MAJOR > 4) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \
     DB_VERSION_PATCH >= 17)
      *iv_return = DB_TXN_PRINT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'W':
    if (memEQ(name, "DB_WRITELOCK", 12)) {
    /*                  ^               */
#ifdef DB_WRITELOCK
      *iv_return = DB_WRITELOCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_WRITEOPEN", 12)) {
    /*                  ^               */
#ifdef DB_WRITEOPEN
      *iv_return = DB_WRITEOPEN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'X':
    if (memEQ(name, "DB_XA_CREATE", 12)) {
    /*                  ^               */
#ifdef DB_XA_CREATE
      *iv_return = DB_XA_CREATE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_13 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_AGGRESSIVE DB_BTREEMAGIC DB_CHECKPOINT DB_DIRECT_LOG DB_DIRTY_READ
     DB_DONOTINDEX DB_ENV_CREATE DB_ENV_NOMMAP DB_ENV_THREAD DB_HASHOLDVER
     DB_INCOMPLETE DB_INIT_MPOOL DB_LOCK_NORUN DB_LOCK_RIW_N DB_LOCK_TRADE
     DB_LOGVERSION DB_LOG_LOCKED DB_MPOOL_LAST DB_MUTEXDEBUG DB_MUTEXLOCKS
     DB_NEXT_NODUP DB_NOORDERCHK DB_PREV_NODUP DB_PR_HEADERS DB_QAMVERSION
     DB_RDWRMASTER DB_REGISTERED DB_REP_CLIENT DB_REP_MASTER DB_SEQUENTIAL
     DB_STAT_CLEAR DB_SYSTEM_MEM DB_TXNVERSION DB_TXN_NOSYNC DB_TXN_NOWAIT
     DB_VERIFY_BAD */
  /* Offset 5 gives the best switch position.  */
  switch (name[5]) {
  case 'A':
    if (memEQ(name, "DB_STAT_CLEAR", 13)) {
    /*                    ^              */
#ifdef DB_STAT_CLEAR
      *iv_return = DB_STAT_CLEAR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'C':
    if (memEQ(name, "DB_INCOMPLETE", 13)) {
    /*                    ^              */
#ifdef DB_INCOMPLETE
      *iv_return = DB_INCOMPLETE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_NORUN", 13)) {
    /*                    ^              */
#ifdef DB_LOCK_NORUN
      *iv_return = DB_LOCK_NORUN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_RIW_N", 13)) {
    /*                    ^              */
#ifdef DB_LOCK_RIW_N
      *iv_return = DB_LOCK_RIW_N;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_TRADE", 13)) {
    /*                    ^              */
#if (DB_VERSION_MAJOR > 4) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \
     DB_VERSION_PATCH >= 17)
      *iv_return = DB_LOCK_TRADE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "DB_CHECKPOINT", 13)) {
    /*                    ^              */
#ifdef DB_CHECKPOINT
      *iv_return = DB_CHECKPOINT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_PREV_NODUP", 13)) {
    /*                    ^              */
#ifdef DB_PREV_NODUP
      *iv_return = DB_PREV_NODUP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'G':
    if (memEQ(name, "DB_AGGRESSIVE", 13)) {
    /*                    ^              */
#ifdef DB_AGGRESSIVE
      *iv_return = DB_AGGRESSIVE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOGVERSION", 13)) {
    /*                    ^              */
#ifdef DB_LOGVERSION
      *iv_return = DB_LOGVERSION;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOG_LOCKED", 13)) {
    /*                    ^              */
#ifdef DB_LOG_LOCKED
      *iv_return = DB_LOG_LOCKED;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_REGISTERED", 13)) {
    /*                    ^              */
#ifdef DB_REGISTERED
      *iv_return = DB_REGISTERED;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "DB_INIT_MPOOL", 13)) {
    /*                    ^              */
#ifdef DB_INIT_MPOOL
      *iv_return = DB_INIT_MPOOL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "DB_QAMVERSION", 13)) {
    /*                    ^              */
#ifdef DB_QAMVERSION
      *iv_return = DB_QAMVERSION;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "DB_DONOTINDEX", 13)) {
    /*                    ^              */
#ifdef DB_DONOTINDEX
      *iv_return = DB_DONOTINDEX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXNVERSION", 13)) {
    /*                    ^              */
#ifdef DB_TXNVERSION
      *iv_return = DB_TXNVERSION;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_NOSYNC", 13)) {
    /*                    ^              */
#ifdef DB_TXN_NOSYNC
      *iv_return = DB_TXN_NOSYNC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_NOWAIT", 13)) {
    /*                    ^              */
#ifdef DB_TXN_NOWAIT
      *iv_return = DB_TXN_NOWAIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "DB_MPOOL_LAST", 13)) {
    /*                    ^              */
#ifdef DB_MPOOL_LAST
      *iv_return = DB_MPOOL_LAST;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_NOORDERCHK", 13)) {
    /*                    ^              */
#ifdef DB_NOORDERCHK
      *iv_return = DB_NOORDERCHK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "DB_REP_CLIENT", 13)) {
    /*                    ^              */
#ifdef DB_REP_CLIENT
      *iv_return = DB_REP_CLIENT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_REP_MASTER", 13)) {
    /*                    ^              */
#ifdef DB_REP_MASTER
      *iv_return = DB_REP_MASTER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'Q':
    if (memEQ(name, "DB_SEQUENTIAL", 13)) {
    /*                    ^              */
#ifdef DB_SEQUENTIAL
      *iv_return = DB_SEQUENTIAL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "DB_BTREEMAGIC", 13)) {
    /*                    ^              */
#ifdef DB_BTREEMAGIC
      *iv_return = DB_BTREEMAGIC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_DIRECT_LOG", 13)) {
    /*                    ^              */
#ifdef DB_DIRECT_LOG
      *iv_return = DB_DIRECT_LOG;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_DIRTY_READ", 13)) {
    /*                    ^              */
#ifdef DB_DIRTY_READ
      *iv_return = DB_DIRTY_READ;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_VERIFY_BAD", 13)) {
    /*                    ^              */
#ifdef DB_VERIFY_BAD
      *iv_return = DB_VERIFY_BAD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "DB_HASHOLDVER", 13)) {
    /*                    ^              */
#ifdef DB_HASHOLDVER
      *iv_return = DB_HASHOLDVER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_SYSTEM_MEM", 13)) {
    /*                    ^              */
#ifdef DB_SYSTEM_MEM
      *iv_return = DB_SYSTEM_MEM;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "DB_MUTEXDEBUG", 13)) {
    /*                    ^              */
#ifdef DB_MUTEXDEBUG
      *iv_return = DB_MUTEXDEBUG;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_MUTEXLOCKS", 13)) {
    /*                    ^              */
#ifdef DB_MUTEXLOCKS
      *iv_return = DB_MUTEXLOCKS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'V':
    if (memEQ(name, "DB_ENV_CREATE", 13)) {
    /*                    ^              */
#ifdef DB_ENV_CREATE
      *iv_return = DB_ENV_CREATE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_ENV_NOMMAP", 13)) {
    /*                    ^              */
#ifdef DB_ENV_NOMMAP
      *iv_return = DB_ENV_NOMMAP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_ENV_THREAD", 13)) {
    /*                    ^              */
#ifdef DB_ENV_THREAD
      *iv_return = DB_ENV_THREAD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'W':
    if (memEQ(name, "DB_RDWRMASTER", 13)) {
    /*                    ^              */
#ifdef DB_RDWRMASTER
      *iv_return = DB_RDWRMASTER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'X':
    if (memEQ(name, "DB_NEXT_NODUP", 13)) {
    /*                    ^              */
#ifdef DB_NEXT_NODUP
      *iv_return = DB_NEXT_NODUP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '_':
    if (memEQ(name, "DB_PR_HEADERS", 13)) {
    /*                    ^              */
#ifdef DB_PR_HEADERS
      *iv_return = DB_PR_HEADERS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_14 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_AUTO_COMMIT DB_BTREEOLDVER DB_CHKSUM_SHA1 DB_EID_INVALID DB_ENCRYPT_AES
     DB_ENV_APPINIT DB_ENV_DBLOCAL DB_ENV_LOCKING DB_ENV_LOGGING DB_ENV_NOPANIC
     DB_ENV_PRIVATE DB_FILE_ID_LEN DB_HANDLE_LOCK DB_HASHVERSION DB_INVALID_EID
     DB_JOIN_NOSORT DB_LOCKVERSION DB_LOCK_EXPIRE DB_LOCK_NOWAIT DB_LOCK_OLDEST
     DB_LOCK_RANDOM DB_LOCK_RECORD DB_LOCK_REMOVE DB_LOCK_SWITCH DB_MAX_RECORDS
     DB_MPOOL_CLEAN DB_MPOOL_DIRTY DB_NOOVERWRITE DB_NOSERVER_ID DB_ODDFILESIZE
     DB_OLD_VERSION DB_OPEN_CALLED DB_RECORDCOUNT DB_RECORD_LOCK DB_REGION_ANON
     DB_REGION_INIT DB_REGION_NAME DB_RENAMEMAGIC DB_REP_NEWSITE DB_REP_UNAVAIL
     DB_REVSPLITOFF DB_RUNRECOVERY DB_SET_TXN_NOW DB_USE_ENVIRON DB_WRITECURSOR
     DB_XIDDATASIZE */
  /* Offset 9 gives the best switch position.  */
  switch (name[9]) {
  case 'A':
    if (memEQ(name, "DB_LOCK_RANDOM", 14)) {
    /*                        ^           */
#ifdef DB_LOCK_RANDOM
      *iv_return = DB_LOCK_RANDOM;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_OPEN_CALLED", 14)) {
    /*                        ^           */
#ifdef DB_OPEN_CALLED
      *iv_return = DB_OPEN_CALLED;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_REP_UNAVAIL", 14)) {
    /*                        ^           */
#ifdef DB_REP_UNAVAIL
      *iv_return = DB_REP_UNAVAIL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_XIDDATASIZE", 14)) {
    /*                        ^           */
#ifdef DB_XIDDATASIZE
      *iv_return = DB_XIDDATASIZE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'C':
    if (memEQ(name, "DB_ENV_LOCKING", 14)) {
    /*                        ^           */
#ifdef DB_ENV_LOCKING
      *iv_return = DB_ENV_LOCKING;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_MAX_RECORDS", 14)) {
    /*                        ^           */
#ifdef DB_MAX_RECORDS
      *iv_return = DB_MAX_RECORDS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_MPOOL_CLEAN", 14)) {
    /*                        ^           */
#ifdef DB_MPOOL_CLEAN
      *iv_return = DB_MPOOL_CLEAN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_RECORDCOUNT", 14)) {
    /*                        ^           */
#ifdef DB_RECORDCOUNT
      *iv_return = DB_RECORDCOUNT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'D':
    if (memEQ(name, "DB_FILE_ID_LEN", 14)) {
    /*                        ^           */
#ifdef DB_FILE_ID_LEN
      *iv_return = DB_FILE_ID_LEN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_INVALID_EID", 14)) {
    /*                        ^           */
#ifdef DB_INVALID_EID
      *iv_return = DB_INVALID_EID;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_MPOOL_DIRTY", 14)) {
    /*                        ^           */
#ifdef DB_MPOOL_DIRTY
      *iv_return = DB_MPOOL_DIRTY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "DB_LOCK_RECORD", 14)) {
    /*                        ^           */
#ifdef DB_LOCK_RECORD
      *iv_return = DB_LOCK_RECORD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_REMOVE", 14)) {
    /*                        ^           */
#ifdef DB_LOCK_REMOVE
      *iv_return = DB_LOCK_REMOVE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_NOSERVER_ID", 14)) {
    /*                        ^           */
#ifdef DB_NOSERVER_ID
      *iv_return = DB_NOSERVER_ID;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_ODDFILESIZE", 14)) {
    /*                        ^           */
#ifdef DB_ODDFILESIZE
      *iv_return = DB_ODDFILESIZE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'G':
    if (memEQ(name, "DB_ENV_LOGGING", 14)) {
    /*                        ^           */
#ifdef DB_ENV_LOGGING
      *iv_return = DB_ENV_LOGGING;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "DB_ENV_PRIVATE", 14)) {
    /*                        ^           */
#ifdef DB_ENV_PRIVATE
      *iv_return = DB_ENV_PRIVATE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_REVSPLITOFF", 14)) {
    /*                        ^           */
#ifdef DB_REVSPLITOFF
      *iv_return = DB_REVSPLITOFF;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "DB_BTREEOLDVER", 14)) {
    /*                        ^           */
#ifdef DB_BTREEOLDVER
      *iv_return = DB_BTREEOLDVER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_ENV_DBLOCAL", 14)) {
    /*                        ^           */
#ifdef DB_ENV_DBLOCAL
      *iv_return = DB_ENV_DBLOCAL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_OLDEST", 14)) {
    /*                        ^           */
#ifdef DB_LOCK_OLDEST
      *iv_return = DB_LOCK_OLDEST;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "DB_RENAMEMAGIC", 14)) {
    /*                        ^           */
#ifdef DB_RENAMEMAGIC
      *iv_return = DB_RENAMEMAGIC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "DB_SET_TXN_NOW", 14)) {
    /*                        ^           */
#ifdef DB_SET_TXN_NOW
      *iv_return = DB_SET_TXN_NOW;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "DB_AUTO_COMMIT", 14)) {
    /*                        ^           */
#ifdef DB_AUTO_COMMIT
      *iv_return = DB_AUTO_COMMIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_JOIN_NOSORT", 14)) {
    /*                        ^           */
#ifdef DB_JOIN_NOSORT
      *iv_return = DB_JOIN_NOSORT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_NOWAIT", 14)) {
    /*                        ^           */
#ifdef DB_LOCK_NOWAIT
      *iv_return = DB_LOCK_NOWAIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_RUNRECOVERY", 14)) {
    /*                        ^           */
#ifdef DB_RUNRECOVERY
      *iv_return = DB_RUNRECOVERY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "DB_ENV_APPINIT", 14)) {
    /*                        ^           */
#ifdef DB_ENV_APPINIT
      *iv_return = DB_ENV_APPINIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_ENV_NOPANIC", 14)) {
    /*                        ^           */
#ifdef DB_ENV_NOPANIC
      *iv_return = DB_ENV_NOPANIC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "DB_HASHVERSION", 14)) {
    /*                        ^           */
#ifdef DB_HASHVERSION
      *iv_return = DB_HASHVERSION;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCKVERSION", 14)) {
    /*                        ^           */
#ifdef DB_LOCKVERSION
      *iv_return = DB_LOCKVERSION;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_OLD_VERSION", 14)) {
    /*                        ^           */
#ifdef DB_OLD_VERSION
      *iv_return = DB_OLD_VERSION;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "DB_ENCRYPT_AES", 14)) {
    /*                        ^           */
#ifdef DB_ENCRYPT_AES
      *iv_return = DB_ENCRYPT_AES;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'U':
    if (memEQ(name, "DB_WRITECURSOR", 14)) {
    /*                        ^           */
#ifdef DB_WRITECURSOR
      *iv_return = DB_WRITECURSOR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'V':
    if (memEQ(name, "DB_EID_INVALID", 14)) {
    /*                        ^           */
#ifdef DB_EID_INVALID
      *iv_return = DB_EID_INVALID;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_USE_ENVIRON", 14)) {
    /*                        ^           */
#ifdef DB_USE_ENVIRON
      *iv_return = DB_USE_ENVIRON;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'W':
    if (memEQ(name, "DB_LOCK_SWITCH", 14)) {
    /*                        ^           */
#ifdef DB_LOCK_SWITCH
      *iv_return = DB_LOCK_SWITCH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_NOOVERWRITE", 14)) {
    /*                        ^           */
#ifdef DB_NOOVERWRITE
      *iv_return = DB_NOOVERWRITE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_REP_NEWSITE", 14)) {
    /*                        ^           */
#ifdef DB_REP_NEWSITE
      *iv_return = DB_REP_NEWSITE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'X':
    if (memEQ(name, "DB_LOCK_EXPIRE", 14)) {
    /*                        ^           */
#ifdef DB_LOCK_EXPIRE
      *iv_return = DB_LOCK_EXPIRE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '_':
    if (memEQ(name, "DB_CHKSUM_SHA1", 14)) {
    /*                        ^           */
#ifdef DB_CHKSUM_SHA1
      *iv_return = DB_CHKSUM_SHA1;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_HANDLE_LOCK", 14)) {
    /*                        ^           */
#ifdef DB_HANDLE_LOCK
      *iv_return = DB_HANDLE_LOCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_RECORD_LOCK", 14)) {
    /*                        ^           */
#ifdef DB_RECORD_LOCK
      *iv_return = DB_RECORD_LOCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_REGION_ANON", 14)) {
    /*                        ^           */
#ifdef DB_REGION_ANON
      *iv_return = DB_REGION_ANON;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_REGION_INIT", 14)) {
    /*                        ^           */
#ifdef DB_REGION_INIT
      *iv_return = DB_REGION_INIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_REGION_NAME", 14)) {
    /*                        ^           */
#ifdef DB_REGION_NAME
      *iv_return = DB_REGION_NAME;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_15 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_APPLY_LOGREG DB_BTREEVERSION DB_CONSUME_WAIT DB_ENV_LOCKDOWN
     DB_ENV_PANIC_OK DB_ENV_YIELDCPU DB_LOCK_DEFAULT DB_LOCK_INHERIT
     DB_LOCK_NOTHELD DB_LOCK_PUT_ALL DB_LOCK_PUT_OBJ DB_LOCK_TIMEOUT
     DB_LOCK_UPGRADE DB_MPOOL_CREATE DB_MPOOL_EXTENT DB_MULTIPLE_KEY
     DB_OPFLAGS_MASK DB_ORDERCHKONLY DB_PRIORITY_LOW DB_REGION_MAGIC
     DB_REP_LOGSONLY DB_REP_OUTDATED DB_SURPRISE_KID DB_TEST_POSTLOG
     DB_TEST_PREOPEN DB_TXN_GETPGNOS DB_TXN_LOCK_2PL DB_TXN_LOG_MASK
     DB_TXN_LOG_REDO DB_TXN_LOG_UNDO DB_VERIFY_FATAL */
  /* Offset 10 gives the best switch position.  */
  switch (name[10]) {
  case 'D':
    if (memEQ(name, "DB_REP_OUTDATED", 15)) {
    /*                         ^           */
#ifdef DB_REP_OUTDATED
      *iv_return = DB_REP_OUTDATED;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "DB_MULTIPLE_KEY", 15)) {
    /*                         ^           */
#ifdef DB_MULTIPLE_KEY
      *iv_return = DB_MULTIPLE_KEY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_SURPRISE_KID", 15)) {
    /*                         ^           */
#ifdef DB_SURPRISE_KID
      *iv_return = DB_SURPRISE_KID;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TEST_PREOPEN", 15)) {
    /*                         ^           */
#ifdef DB_TEST_PREOPEN
      *iv_return = DB_TEST_PREOPEN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'F':
    if (memEQ(name, "DB_LOCK_DEFAULT", 15)) {
    /*                         ^           */
#ifdef DB_LOCK_DEFAULT
      *iv_return = DB_LOCK_DEFAULT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_VERIFY_FATAL", 15)) {
    /*                         ^           */
#ifdef DB_VERIFY_FATAL
      *iv_return = DB_VERIFY_FATAL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'G':
    if (memEQ(name, "DB_LOCK_UPGRADE", 15)) {
    /*                         ^           */
#ifdef DB_LOCK_UPGRADE
      *iv_return = DB_LOCK_UPGRADE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'H':
    if (memEQ(name, "DB_LOCK_INHERIT", 15)) {
    /*                         ^           */
#if (DB_VERSION_MAJOR > 2) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 7) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 7 && \
     DB_VERSION_PATCH >= 1)
      *iv_return = DB_LOCK_INHERIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "DB_ENV_PANIC_OK", 15)) {
    /*                         ^           */
#ifdef DB_ENV_PANIC_OK
      *iv_return = DB_ENV_PANIC_OK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'K':
    if (memEQ(name, "DB_ENV_LOCKDOWN", 15)) {
    /*                         ^           */
#ifdef DB_ENV_LOCKDOWN
      *iv_return = DB_ENV_LOCKDOWN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_ORDERCHKONLY", 15)) {
    /*                         ^           */
#ifdef DB_ORDERCHKONLY
      *iv_return = DB_ORDERCHKONLY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_LOCK_2PL", 15)) {
    /*                         ^           */
#ifdef DB_TXN_LOCK_2PL
      *iv_return = DB_TXN_LOCK_2PL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "DB_ENV_YIELDCPU", 15)) {
    /*                         ^           */
#ifdef DB_ENV_YIELDCPU
      *iv_return = DB_ENV_YIELDCPU;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "DB_LOCK_TIMEOUT", 15)) {
    /*                         ^           */
#if (DB_VERSION_MAJOR > 4) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 0 && \
     DB_VERSION_PATCH >= 7)
      *iv_return = DB_LOCK_TIMEOUT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_REGION_MAGIC", 15)) {
    /*                         ^           */
#ifdef DB_REGION_MAGIC
      *iv_return = DB_REGION_MAGIC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "DB_APPLY_LOGREG", 15)) {
    /*                         ^           */
#ifdef DB_APPLY_LOGREG
      *iv_return = DB_APPLY_LOGREG;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "DB_TXN_GETPGNOS", 15)) {
    /*                         ^           */
#if (DB_VERSION_MAJOR > 4) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \
     DB_VERSION_PATCH >= 17)
      *iv_return = DB_TXN_GETPGNOS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "DB_BTREEVERSION", 15)) {
    /*                         ^           */
#ifdef DB_BTREEVERSION
      *iv_return = DB_BTREEVERSION;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_MPOOL_CREATE", 15)) {
    /*                         ^           */
#ifdef DB_MPOOL_CREATE
      *iv_return = DB_MPOOL_CREATE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "DB_REP_LOGSONLY", 15)) {
    /*                         ^           */
#ifdef DB_REP_LOGSONLY
      *iv_return = DB_REP_LOGSONLY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TEST_POSTLOG", 15)) {
    /*                         ^           */
#ifdef DB_TEST_POSTLOG
      *iv_return = DB_TEST_POSTLOG;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "DB_LOCK_NOTHELD", 15)) {
    /*                         ^           */
#ifdef DB_LOCK_NOTHELD
      *iv_return = DB_LOCK_NOTHELD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_PUT_ALL", 15)) {
    /*                         ^           */
#if (DB_VERSION_MAJOR > 2) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \
     DB_VERSION_PATCH >= 0)
      *iv_return = DB_LOCK_PUT_ALL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_PUT_OBJ", 15)) {
    /*                         ^           */
#if (DB_VERSION_MAJOR > 2) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \
    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \
     DB_VERSION_PATCH >= 0)
      *iv_return = DB_LOCK_PUT_OBJ;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'X':
    if (memEQ(name, "DB_MPOOL_EXTENT", 15)) {
    /*                         ^           */
#ifdef DB_MPOOL_EXTENT
      *iv_return = DB_MPOOL_EXTENT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'Y':
    if (memEQ(name, "DB_PRIORITY_LOW", 15)) {
    /*                         ^           */
#if (DB_VERSION_MAJOR > 4) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \
     DB_VERSION_PATCH >= 17)
      *iv_return = DB_PRIORITY_LOW;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '_':
    if (memEQ(name, "DB_CONSUME_WAIT", 15)) {
    /*                         ^           */
#ifdef DB_CONSUME_WAIT
      *iv_return = DB_CONSUME_WAIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_OPFLAGS_MASK", 15)) {
    /*                         ^           */
#ifdef DB_OPFLAGS_MASK
      *iv_return = DB_OPFLAGS_MASK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_LOG_MASK", 15)) {
    /*                         ^           */
#ifdef DB_TXN_LOG_MASK
      *iv_return = DB_TXN_LOG_MASK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_LOG_REDO", 15)) {
    /*                         ^           */
#ifdef DB_TXN_LOG_REDO
      *iv_return = DB_TXN_LOG_REDO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_LOG_UNDO", 15)) {
    /*                         ^           */
#ifdef DB_TXN_LOG_UNDO
      *iv_return = DB_TXN_LOG_UNDO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_16 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_BROADCAST_EID DB_CACHED_COUNTS DB_EID_BROADCAST DB_ENV_CDB_ALLDB
     DB_ENV_DIRECT_DB DB_ENV_NOLOCKING DB_ENV_OVERWRITE DB_ENV_RPCCLIENT
     DB_FCNTL_LOCKING DB_JAVA_CALLBACK DB_LOCK_CONFLICT DB_LOCK_DEADLOCK
     DB_LOCK_MAXLOCKS DB_LOCK_MINLOCKS DB_LOCK_MINWRITE DB_LOCK_NOTEXIST
     DB_LOCK_PUT_READ DB_LOCK_YOUNGEST DB_LOGC_BUF_SIZE DB_MPOOL_DISCARD
     DB_MPOOL_PRIVATE DB_NOSERVER_HOME DB_PAGE_NOTFOUND DB_PRIORITY_HIGH
     DB_RECOVER_FATAL DB_REP_DUPMASTER DB_REP_NEWMASTER DB_REP_PERMANENT
     DB_SECONDARY_BAD DB_TEST_POSTOPEN DB_TEST_POSTSYNC DB_TXN_LOCK_MASK
     DB_TXN_OPENFILES DB_VERB_CHKPOINT DB_VERB_DEADLOCK DB_VERB_RECOVERY
     DB_VERB_WAITSFOR DB_VERSION_MAJOR DB_VERSION_MINOR DB_VERSION_PATCH
     DB_VRFY_FLAGMASK */
  /* Offset 12 gives the best switch position.  */
  switch (name[12]) {
  case 'A':
    if (memEQ(name, "DB_RECOVER_FATAL", 16)) {
    /*                           ^          */
#ifdef DB_RECOVER_FATAL
      *iv_return = DB_RECOVER_FATAL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_VERSION_MAJOR", 16)) {
    /*                           ^          */
#ifdef DB_VERSION_MAJOR
      *iv_return = DB_VERSION_MAJOR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_VERSION_PATCH", 16)) {
    /*                           ^          */
#ifdef DB_VERSION_PATCH
      *iv_return = DB_VERSION_PATCH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'B':
    if (memEQ(name, "DB_JAVA_CALLBACK", 16)) {
    /*                           ^          */
#ifdef DB_JAVA_CALLBACK
      *iv_return = DB_JAVA_CALLBACK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'C':
    if (memEQ(name, "DB_EID_BROADCAST", 16)) {
    /*                           ^          */
#ifdef DB_EID_BROADCAST
      *iv_return = DB_EID_BROADCAST;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_MPOOL_DISCARD", 16)) {
    /*                           ^          */
#ifdef DB_MPOOL_DISCARD
      *iv_return = DB_MPOOL_DISCARD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'G':
    if (memEQ(name, "DB_LOCK_YOUNGEST", 16)) {
    /*                           ^          */
#ifdef DB_LOCK_YOUNGEST
      *iv_return = DB_LOCK_YOUNGEST;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'H':
    if (memEQ(name, "DB_NOSERVER_HOME", 16)) {
    /*                           ^          */
#ifdef DB_NOSERVER_HOME
      *iv_return = DB_NOSERVER_HOME;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_PRIORITY_HIGH", 16)) {
    /*                           ^          */
#if (DB_VERSION_MAJOR > 4) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \
     DB_VERSION_PATCH >= 17)
      *iv_return = DB_PRIORITY_HIGH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "DB_ENV_RPCCLIENT", 16)) {
    /*                           ^          */
#ifdef DB_ENV_RPCCLIENT
      *iv_return = DB_ENV_RPCCLIENT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_OPENFILES", 16)) {
    /*                           ^          */
#if (DB_VERSION_MAJOR > 3) || \
    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR > 1) || \
    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR == 1 && \
     DB_VERSION_PATCH >= 12)
      *iv_return = DB_TXN_OPENFILES;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_VERSION_MINOR", 16)) {
    /*                           ^          */
#ifdef DB_VERSION_MINOR
      *iv_return = DB_VERSION_MINOR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'K':
    if (memEQ(name, "DB_ENV_NOLOCKING", 16)) {
    /*                           ^          */
#ifdef DB_ENV_NOLOCKING
      *iv_return = DB_ENV_NOLOCKING;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_FCNTL_LOCKING", 16)) {
    /*                           ^          */
#ifdef DB_FCNTL_LOCKING
      *iv_return = DB_FCNTL_LOCKING;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "DB_ENV_CDB_ALLDB", 16)) {
    /*                           ^          */
#ifdef DB_ENV_CDB_ALLDB
      *iv_return = DB_ENV_CDB_ALLDB;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_CONFLICT", 16)) {
    /*                           ^          */
#ifdef DB_LOCK_CONFLICT
      *iv_return = DB_LOCK_CONFLICT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_DEADLOCK", 16)) {
    /*                           ^          */
#ifdef DB_LOCK_DEADLOCK
      *iv_return = DB_LOCK_DEADLOCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_VERB_DEADLOCK", 16)) {
    /*                           ^          */
#ifdef DB_VERB_DEADLOCK
      *iv_return = DB_VERB_DEADLOCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "DB_TXN_LOCK_MASK", 16)) {
    /*                           ^          */
#ifdef DB_TXN_LOCK_MASK
      *iv_return = DB_TXN_LOCK_MASK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_VRFY_FLAGMASK", 16)) {
    /*                           ^          */
#ifdef DB_VRFY_FLAGMASK
      *iv_return = DB_VRFY_FLAGMASK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "DB_REP_PERMANENT", 16)) {
    /*                           ^          */
#ifdef DB_REP_PERMANENT
      *iv_return = DB_REP_PERMANENT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "DB_LOCK_MAXLOCKS", 16)) {
    /*                           ^          */
#ifdef DB_LOCK_MAXLOCKS
      *iv_return = DB_LOCK_MAXLOCKS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_MINLOCKS", 16)) {
    /*                           ^          */
#ifdef DB_LOCK_MINLOCKS
      *iv_return = DB_LOCK_MINLOCKS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_PAGE_NOTFOUND", 16)) {
    /*                           ^          */
#ifdef DB_PAGE_NOTFOUND
      *iv_return = DB_PAGE_NOTFOUND;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TEST_POSTOPEN", 16)) {
    /*                           ^          */
#ifdef DB_TEST_POSTOPEN
      *iv_return = DB_TEST_POSTOPEN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_VERB_CHKPOINT", 16)) {
    /*                           ^          */
#ifdef DB_VERB_CHKPOINT
      *iv_return = DB_VERB_CHKPOINT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "DB_ENV_OVERWRITE", 16)) {
    /*                           ^          */
#ifdef DB_ENV_OVERWRITE
      *iv_return = DB_ENV_OVERWRITE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_MINWRITE", 16)) {
    /*                           ^          */
#ifdef DB_LOCK_MINWRITE
      *iv_return = DB_LOCK_MINWRITE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_PUT_READ", 16)) {
    /*                           ^          */
#if (DB_VERSION_MAJOR > 4) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 0 && \
     DB_VERSION_PATCH >= 7)
      *iv_return = DB_LOCK_PUT_READ;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "DB_LOGC_BUF_SIZE", 16)) {
    /*                           ^          */
#ifdef DB_LOGC_BUF_SIZE
      *iv_return = DB_LOGC_BUF_SIZE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_REP_DUPMASTER", 16)) {
    /*                           ^          */
#ifdef DB_REP_DUPMASTER
      *iv_return = DB_REP_DUPMASTER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_REP_NEWMASTER", 16)) {
    /*                           ^          */
#ifdef DB_REP_NEWMASTER
      *iv_return = DB_REP_NEWMASTER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TEST_POSTSYNC", 16)) {
    /*                           ^          */
#ifdef DB_TEST_POSTSYNC
      *iv_return = DB_TEST_POSTSYNC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_VERB_WAITSFOR", 16)) {
    /*                           ^          */
#ifdef DB_VERB_WAITSFOR
      *iv_return = DB_VERB_WAITSFOR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "DB_ENV_DIRECT_DB", 16)) {
    /*                           ^          */
#ifdef DB_ENV_DIRECT_DB
      *iv_return = DB_ENV_DIRECT_DB;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'U':
    if (memEQ(name, "DB_CACHED_COUNTS", 16)) {
    /*                           ^          */
#ifdef DB_CACHED_COUNTS
      *iv_return = DB_CACHED_COUNTS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'V':
    if (memEQ(name, "DB_MPOOL_PRIVATE", 16)) {
    /*                           ^          */
#ifdef DB_MPOOL_PRIVATE
      *iv_return = DB_MPOOL_PRIVATE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_VERB_RECOVERY", 16)) {
    /*                           ^          */
#ifdef DB_VERB_RECOVERY
      *iv_return = DB_VERB_RECOVERY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'X':
    if (memEQ(name, "DB_LOCK_NOTEXIST", 16)) {
    /*                           ^          */
#ifdef DB_LOCK_NOTEXIST
      *iv_return = DB_LOCK_NOTEXIST;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '_':
    if (memEQ(name, "DB_BROADCAST_EID", 16)) {
    /*                           ^          */
#ifdef DB_BROADCAST_EID
      *iv_return = DB_BROADCAST_EID;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_SECONDARY_BAD", 16)) {
    /*                           ^          */
#ifdef DB_SECONDARY_BAD
      *iv_return = DB_SECONDARY_BAD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_17 (pTHX_ const char *name, IV *iv_return, const char **pv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_ENV_DIRECT_LOG DB_ENV_REP_CLIENT DB_ENV_REP_MASTER DB_ENV_STANDALONE
     DB_ENV_SYSTEM_MEM DB_ENV_TXN_NOSYNC DB_ENV_USER_ALLOC DB_GET_BOTH_RANGE
     DB_LOG_SILENT_ERR DB_RPC_SERVERPROG DB_RPC_SERVERVERS DB_TEST_ELECTINIT
     DB_TEST_ELECTSEND DB_TEST_PRERENAME DB_TXN_POPENFILES DB_VERSION_STRING */
  /* Offset 14 gives the best switch position.  */
  switch (name[14]) {
  case 'A':
    if (memEQ(name, "DB_TEST_PRERENAME", 17)) {
    /*                             ^         */
#ifdef DB_TEST_PRERENAME
      *iv_return = DB_TEST_PRERENAME;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "DB_ENV_REP_CLIENT", 17)) {
    /*                             ^         */
#ifdef DB_ENV_REP_CLIENT
      *iv_return = DB_ENV_REP_CLIENT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOG_SILENT_ERR", 17)) {
    /*                             ^         */
#ifdef DB_LOG_SILENT_ERR
      *iv_return = DB_LOG_SILENT_ERR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_RPC_SERVERVERS", 17)) {
    /*                             ^         */
#ifdef DB_RPC_SERVERVERS
      *iv_return = DB_RPC_SERVERVERS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TEST_ELECTSEND", 17)) {
    /*                             ^         */
#ifdef DB_TEST_ELECTSEND
      *iv_return = DB_TEST_ELECTSEND;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "DB_VERSION_STRING", 17)) {
    /*                             ^         */
#ifdef DB_VERSION_STRING
      *pv_return = DB_VERSION_STRING;
      return PERL_constant_ISPV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "DB_ENV_DIRECT_LOG", 17)) {
    /*                             ^         */
#ifdef DB_ENV_DIRECT_LOG
      *iv_return = DB_ENV_DIRECT_LOG;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_ENV_USER_ALLOC", 17)) {
    /*                             ^         */
#ifdef DB_ENV_USER_ALLOC
      *iv_return = DB_ENV_USER_ALLOC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_POPENFILES", 17)) {
    /*                             ^         */
#if (DB_VERSION_MAJOR > 3) || \
    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR > 3) || \
    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR == 3 && \
     DB_VERSION_PATCH >= 4)
      *iv_return = DB_TXN_POPENFILES;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "DB_ENV_SYSTEM_MEM", 17)) {
    /*                             ^         */
#ifdef DB_ENV_SYSTEM_MEM
      *iv_return = DB_ENV_SYSTEM_MEM;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "DB_GET_BOTH_RANGE", 17)) {
    /*                             ^         */
#ifdef DB_GET_BOTH_RANGE
      *iv_return = DB_GET_BOTH_RANGE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TEST_ELECTINIT", 17)) {
    /*                             ^         */
#ifdef DB_TEST_ELECTINIT
      *iv_return = DB_TEST_ELECTINIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "DB_ENV_STANDALONE", 17)) {
    /*                             ^         */
#ifdef DB_ENV_STANDALONE
      *iv_return = DB_ENV_STANDALONE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "DB_RPC_SERVERPROG", 17)) {
    /*                             ^         */
#ifdef DB_RPC_SERVERPROG
      *iv_return = DB_RPC_SERVERPROG;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "DB_ENV_REP_MASTER", 17)) {
    /*                             ^         */
#ifdef DB_ENV_REP_MASTER
      *iv_return = DB_ENV_REP_MASTER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'Y':
    if (memEQ(name, "DB_ENV_TXN_NOSYNC", 17)) {
    /*                             ^         */
#ifdef DB_ENV_TXN_NOSYNC
      *iv_return = DB_ENV_TXN_NOSYNC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_18 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_ALREADY_ABORTED DB_ENV_AUTO_COMMIT DB_ENV_OPEN_CALLED
     DB_ENV_REGION_INIT DB_LOCK_NOTGRANTED DB_MPOOL_NEW_GROUP
     DB_PR_RECOVERYTEST DB_SET_TXN_TIMEOUT DB_TEST_ELECTVOTE1
     DB_TEST_ELECTVOTE2 DB_TEST_ELECTWAIT1 DB_TEST_ELECTWAIT2
     DB_TEST_POSTRENAME DB_TEST_PREDESTROY DB_TEST_PREEXTOPEN */
  /* Offset 13 gives the best switch position.  */
  switch (name[13]) {
  case 'A':
    if (memEQ(name, "DB_ENV_OPEN_CALLED", 18)) {
    /*                            ^           */
#ifdef DB_ENV_OPEN_CALLED
      *iv_return = DB_ENV_OPEN_CALLED;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_NOTGRANTED", 18)) {
    /*                            ^           */
#ifdef DB_LOCK_NOTGRANTED
      *iv_return = DB_LOCK_NOTGRANTED;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "DB_TEST_POSTRENAME", 18)) {
    /*                            ^           */
#ifdef DB_TEST_POSTRENAME
      *iv_return = DB_TEST_POSTRENAME;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'G':
    if (memEQ(name, "DB_MPOOL_NEW_GROUP", 18)) {
    /*                            ^           */
#ifdef DB_MPOOL_NEW_GROUP
      *iv_return = DB_MPOOL_NEW_GROUP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "DB_SET_TXN_TIMEOUT", 18)) {
    /*                            ^           */
#ifdef DB_SET_TXN_TIMEOUT
      *iv_return = DB_SET_TXN_TIMEOUT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "DB_ALREADY_ABORTED", 18)) {
    /*                            ^           */
#ifdef DB_ALREADY_ABORTED
      *iv_return = DB_ALREADY_ABORTED;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_ENV_AUTO_COMMIT", 18)) {
    /*                            ^           */
#ifdef DB_ENV_AUTO_COMMIT
      *iv_return = DB_ENV_AUTO_COMMIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "DB_TEST_PREDESTROY", 18)) {
    /*                            ^           */
#ifdef DB_TEST_PREDESTROY
      *iv_return = DB_TEST_PREDESTROY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "DB_TEST_PREEXTOPEN", 18)) {
    /*                            ^           */
#ifdef DB_TEST_PREEXTOPEN
      *iv_return = DB_TEST_PREEXTOPEN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'V':
    if (memEQ(name, "DB_TEST_ELECTVOTE1", 18)) {
    /*                            ^           */
#ifdef DB_TEST_ELECTVOTE1
      *iv_return = DB_TEST_ELECTVOTE1;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TEST_ELECTVOTE2", 18)) {
    /*                            ^           */
#ifdef DB_TEST_ELECTVOTE2
      *iv_return = DB_TEST_ELECTVOTE2;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'W':
    if (memEQ(name, "DB_TEST_ELECTWAIT1", 18)) {
    /*                            ^           */
#ifdef DB_TEST_ELECTWAIT1
      *iv_return = DB_TEST_ELECTWAIT1;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TEST_ELECTWAIT2", 18)) {
    /*                            ^           */
#ifdef DB_TEST_ELECTWAIT2
      *iv_return = DB_TEST_ELECTWAIT2;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'Y':
    if (memEQ(name, "DB_PR_RECOVERYTEST", 18)) {
    /*                            ^           */
#ifdef DB_PR_RECOVERYTEST
      *iv_return = DB_PR_RECOVERYTEST;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '_':
    if (memEQ(name, "DB_ENV_REGION_INIT", 18)) {
    /*                            ^           */
#ifdef DB_ENV_REGION_INIT
      *iv_return = DB_ENV_REGION_INIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_19 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_ENV_REP_LOGSONLY DB_LOCK_FREE_LOCKER DB_LOCK_GET_TIMEOUT
     DB_LOCK_SET_TIMEOUT DB_PRIORITY_DEFAULT DB_REP_HOLDELECTION
     DB_SET_LOCK_TIMEOUT DB_TEST_POSTDESTROY DB_TEST_POSTEXTOPEN
     DB_TEST_POSTLOGMETA DB_TEST_SUBDB_LOCKS DB_TXN_FORWARD_ROLL
     DB_TXN_LOG_UNDOREDO DB_TXN_WRITE_NOSYNC DB_UNRESOLVED_CHILD
     DB_UPDATE_SECONDARY DB_USE_ENVIRON_ROOT DB_VERB_REPLICATION */
  /* Offset 9 gives the best switch position.  */
  switch (name[9]) {
  case 'C':
    if (memEQ(name, "DB_SET_LOCK_TIMEOUT", 19)) {
    /*                        ^                */
#ifdef DB_SET_LOCK_TIMEOUT
      *iv_return = DB_SET_LOCK_TIMEOUT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "DB_LOCK_GET_TIMEOUT", 19)) {
    /*                        ^                */
#if (DB_VERSION_MAJOR > 4) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 0 && \
     DB_VERSION_PATCH >= 7)
      *iv_return = DB_LOCK_GET_TIMEOUT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_LOCK_SET_TIMEOUT", 19)) {
    /*                        ^                */
#ifdef DB_LOCK_SET_TIMEOUT
      *iv_return = DB_LOCK_SET_TIMEOUT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_VERB_REPLICATION", 19)) {
    /*                        ^                */
#ifdef DB_VERB_REPLICATION
      *iv_return = DB_VERB_REPLICATION;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'G':
    if (memEQ(name, "DB_TXN_LOG_UNDOREDO", 19)) {
    /*                        ^                */
#ifdef DB_TXN_LOG_UNDOREDO
      *iv_return = DB_TXN_LOG_UNDOREDO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "DB_TXN_WRITE_NOSYNC", 19)) {
    /*                        ^                */
#ifdef DB_TXN_WRITE_NOSYNC
      *iv_return = DB_TXN_WRITE_NOSYNC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "DB_REP_HOLDELECTION", 19)) {
    /*                        ^                */
#ifdef DB_REP_HOLDELECTION
      *iv_return = DB_REP_HOLDELECTION;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_UNRESOLVED_CHILD", 19)) {
    /*                        ^                */
#ifdef DB_UNRESOLVED_CHILD
      *iv_return = DB_UNRESOLVED_CHILD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "DB_TEST_POSTDESTROY", 19)) {
    /*                        ^                */
#ifdef DB_TEST_POSTDESTROY
      *iv_return = DB_TEST_POSTDESTROY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TEST_POSTEXTOPEN", 19)) {
    /*                        ^                */
#ifdef DB_TEST_POSTEXTOPEN
      *iv_return = DB_TEST_POSTEXTOPEN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TEST_POSTLOGMETA", 19)) {
    /*                        ^                */
#ifdef DB_TEST_POSTLOGMETA
      *iv_return = DB_TEST_POSTLOGMETA;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "DB_ENV_REP_LOGSONLY", 19)) {
    /*                        ^                */
#ifdef DB_ENV_REP_LOGSONLY
      *iv_return = DB_ENV_REP_LOGSONLY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "DB_LOCK_FREE_LOCKER", 19)) {
    /*                        ^                */
#ifdef DB_LOCK_FREE_LOCKER
      *iv_return = DB_LOCK_FREE_LOCKER;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_FORWARD_ROLL", 19)) {
    /*                        ^                */
#if (DB_VERSION_MAJOR > 3) || \
    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR > 1) || \
    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR == 1 && \
     DB_VERSION_PATCH >= 12)
      *iv_return = DB_TXN_FORWARD_ROLL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "DB_PRIORITY_DEFAULT", 19)) {
    /*                        ^                */
#if (DB_VERSION_MAJOR > 4) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \
     DB_VERSION_PATCH >= 17)
      *iv_return = DB_PRIORITY_DEFAULT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'U':
    if (memEQ(name, "DB_TEST_SUBDB_LOCKS", 19)) {
    /*                        ^                */
#ifdef DB_TEST_SUBDB_LOCKS
      *iv_return = DB_TEST_SUBDB_LOCKS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'V':
    if (memEQ(name, "DB_USE_ENVIRON_ROOT", 19)) {
    /*                        ^                */
#ifdef DB_USE_ENVIRON_ROOT
      *iv_return = DB_USE_ENVIRON_ROOT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '_':
    if (memEQ(name, "DB_UPDATE_SECONDARY", 19)) {
    /*                        ^                */
#ifdef DB_UPDATE_SECONDARY
      *iv_return = DB_UPDATE_SECONDARY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_20 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_CXX_NO_EXCEPTIONS DB_LOGFILEID_INVALID DB_PANIC_ENVIRONMENT
     DB_PRIORITY_VERY_LOW DB_TEST_PREEXTDELETE DB_TEST_PREEXTUNLINK
     DB_TXN_BACKWARD_ROLL DB_TXN_LOCK_OPTIMIST */
  /* Offset 14 gives the best switch position.  */
  switch (name[14]) {
  case 'D':
    if (memEQ(name, "DB_TEST_PREEXTDELETE", 20)) {
    /*                             ^            */
#ifdef DB_TEST_PREEXTDELETE
      *iv_return = DB_TEST_PREEXTDELETE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DB_TXN_BACKWARD_ROLL", 20)) {
    /*                             ^            */
#if (DB_VERSION_MAJOR > 3) || \
    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR > 1) || \
    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR == 1 && \
     DB_VERSION_PATCH >= 12)
      *iv_return = DB_TXN_BACKWARD_ROLL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "DB_LOGFILEID_INVALID", 20)) {
    /*                             ^            */
#ifdef DB_LOGFILEID_INVALID
      *iv_return = DB_LOGFILEID_INVALID;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "DB_PANIC_ENVIRONMENT", 20)) {
    /*                             ^            */
#ifdef DB_PANIC_ENVIRONMENT
      *iv_return = DB_PANIC_ENVIRONMENT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "DB_CXX_NO_EXCEPTIONS", 20)) {
    /*                             ^            */
#ifdef DB_CXX_NO_EXCEPTIONS
      *iv_return = DB_CXX_NO_EXCEPTIONS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "DB_PRIORITY_VERY_LOW", 20)) {
    /*                             ^            */
#if (DB_VERSION_MAJOR > 4) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \
     DB_VERSION_PATCH >= 17)
      *iv_return = DB_PRIORITY_VERY_LOW;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "DB_TXN_LOCK_OPTIMIST", 20)) {
    /*                             ^            */
#ifdef DB_TXN_LOCK_OPTIMIST
      *iv_return = DB_TXN_LOCK_OPTIMIST;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'U':
    if (memEQ(name, "DB_TEST_PREEXTUNLINK", 20)) {
    /*                             ^            */
#ifdef DB_TEST_PREEXTUNLINK
      *iv_return = DB_TEST_PREEXTUNLINK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_21 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DB_LOCK_UPGRADE_WRITE DB_PRIORITY_VERY_HIGH DB_TEST_POSTEXTDELETE
     DB_TEST_POSTEXTUNLINK DB_TXN_BACKWARD_ALLOC */
  /* Offset 16 gives the best switch position.  */
  switch (name[16]) {
  case 'A':
    if (memEQ(name, "DB_TXN_BACKWARD_ALLOC", 21)) {
    /*                               ^           */
#if (DB_VERSION_MAJOR > 4) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \
     DB_VERSION_PATCH >= 17)
      *iv_return = DB_TXN_BACKWARD_ALLOC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "DB_TEST_POSTEXTDELETE", 21)) {
    /*                               ^           */
#ifdef DB_TEST_POSTEXTDELETE
      *iv_return = DB_TEST_POSTEXTDELETE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "DB_TEST_POSTEXTUNLINK", 21)) {
    /*                               ^           */
#ifdef DB_TEST_POSTEXTUNLINK
      *iv_return = DB_TEST_POSTEXTUNLINK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'W':
    if (memEQ(name, "DB_LOCK_UPGRADE_WRITE", 21)) {
    /*                               ^           */
#if (DB_VERSION_MAJOR > 3) || \
    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR > 3) || \
    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR == 3 && \
     DB_VERSION_PATCH >= 4)
      *iv_return = DB_LOCK_UPGRADE_WRITE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '_':
    if (memEQ(name, "DB_PRIORITY_VERY_HIGH", 21)) {
    /*                               ^           */
#if (DB_VERSION_MAJOR > 4) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \
    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \
     DB_VERSION_PATCH >= 17)
      *iv_return = DB_PRIORITY_VERY_HIGH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant (pTHX_ const char *name, STRLEN len, IV *iv_return, const char **pv_return) {
  /* Initially switch on the length of the name.  */
  /* When generated this function returned values for the list of names given
     in this section of perl code.  Rather than manually editing these functions
     to add or remove constants, which would result in this comment and section
     of code becoming inaccurate, we recommend that you edit this section of
     code, and use it to regenerate a new set of constant functions which you
     then use to replace the originals.

     Regenerate these constant functions by feeding this entire source file to
     perl -x

#!/home/paul/perl/install/redhat6.1/5.8.0/bin/perl5.8.0 -w
use ExtUtils::Constant qw (constant_types C_constant XS_constant);

my $types = {map {($_, 1)} qw(IV PV)};
my @names = (qw(DB_AFTER DB_AGGRESSIVE DB_ALREADY_ABORTED DB_APPEND
	       DB_APPLY_LOGREG DB_APP_INIT DB_ARCH_ABS DB_ARCH_DATA DB_ARCH_LOG
	       DB_AUTO_COMMIT DB_BEFORE DB_BROADCAST_EID DB_BTREEMAGIC
	       DB_BTREEOLDVER DB_BTREEVERSION DB_CACHED_COUNTS DB_CDB_ALLDB
	       DB_CHECKPOINT DB_CHKSUM_SHA1 DB_CLIENT DB_CL_WRITER DB_COMMIT
	       DB_CONSUME DB_CONSUME_WAIT DB_CREATE DB_CURLSN DB_CURRENT
	       DB_CXX_NO_EXCEPTIONS DB_DELETED DB_DELIMITER DB_DIRECT
	       DB_DIRECT_DB DB_DIRECT_LOG DB_DIRTY_READ DB_DONOTINDEX DB_DUP
	       DB_DUPCURSOR DB_DUPSORT DB_EID_BROADCAST DB_EID_INVALID
	       DB_ENCRYPT DB_ENCRYPT_AES DB_ENV_APPINIT DB_ENV_AUTO_COMMIT
	       DB_ENV_CDB DB_ENV_CDB_ALLDB DB_ENV_CREATE DB_ENV_DBLOCAL
	       DB_ENV_DIRECT_DB DB_ENV_DIRECT_LOG DB_ENV_FATAL DB_ENV_LOCKDOWN
	       DB_ENV_LOCKING DB_ENV_LOGGING DB_ENV_NOLOCKING DB_ENV_NOMMAP
	       DB_ENV_NOPANIC DB_ENV_OPEN_CALLED DB_ENV_OVERWRITE
	       DB_ENV_PANIC_OK DB_ENV_PRIVATE DB_ENV_REGION_INIT
	       DB_ENV_REP_CLIENT DB_ENV_REP_LOGSONLY DB_ENV_REP_MASTER
	       DB_ENV_RPCCLIENT DB_ENV_RPCCLIENT_GIVEN DB_ENV_STANDALONE
	       DB_ENV_SYSTEM_MEM DB_ENV_THREAD DB_ENV_TXN DB_ENV_TXN_NOSYNC
	       DB_ENV_TXN_WRITE_NOSYNC DB_ENV_USER_ALLOC DB_ENV_YIELDCPU
	       DB_EXCL DB_EXTENT DB_FAST_STAT DB_FCNTL_LOCKING DB_FILE_ID_LEN
	       DB_FIRST DB_FIXEDLEN DB_FLUSH DB_FORCE DB_GETREC DB_GET_BOTH
	       DB_GET_BOTHC DB_GET_BOTH_RANGE DB_GET_RECNO DB_HANDLE_LOCK
	       DB_HASHMAGIC DB_HASHOLDVER DB_HASHVERSION DB_INCOMPLETE
	       DB_INIT_CDB DB_INIT_LOCK DB_INIT_LOG DB_INIT_MPOOL DB_INIT_TXN
	       DB_INVALID_EID DB_JAVA_CALLBACK DB_JOINENV DB_JOIN_ITEM
	       DB_JOIN_NOSORT DB_KEYEMPTY DB_KEYEXIST DB_KEYFIRST DB_KEYLAST
	       DB_LAST DB_LOCKDOWN DB_LOCKMAGIC DB_LOCKVERSION DB_LOCK_CONFLICT
	       DB_LOCK_DEADLOCK DB_LOCK_DEFAULT DB_LOCK_EXPIRE
	       DB_LOCK_FREE_LOCKER DB_LOCK_MAXLOCKS DB_LOCK_MINLOCKS
	       DB_LOCK_MINWRITE DB_LOCK_NORUN DB_LOCK_NOTEXIST
	       DB_LOCK_NOTGRANTED DB_LOCK_NOTHELD DB_LOCK_NOWAIT DB_LOCK_OLDEST
	       DB_LOCK_RANDOM DB_LOCK_RECORD DB_LOCK_REMOVE DB_LOCK_RIW_N
	       DB_LOCK_RW_N DB_LOCK_SET_TIMEOUT DB_LOCK_SWITCH DB_LOCK_UPGRADE
	       DB_LOCK_YOUNGEST DB_LOGC_BUF_SIZE DB_LOGFILEID_INVALID
	       DB_LOGMAGIC DB_LOGOLDVER DB_LOGVERSION DB_LOG_DISK DB_LOG_LOCKED
	       DB_LOG_SILENT_ERR DB_MAX_PAGES DB_MAX_RECORDS DB_MPOOL_CLEAN
	       DB_MPOOL_CREATE DB_MPOOL_DIRTY DB_MPOOL_DISCARD DB_MPOOL_EXTENT
	       DB_MPOOL_LAST DB_MPOOL_NEW DB_MPOOL_NEW_GROUP DB_MPOOL_PRIVATE
	       DB_MULTIPLE DB_MULTIPLE_KEY DB_MUTEXDEBUG DB_MUTEXLOCKS
	       DB_NEEDSPLIT DB_NEXT DB_NEXT_DUP DB_NEXT_NODUP DB_NOCOPY
	       DB_NODUPDATA DB_NOLOCKING DB_NOMMAP DB_NOORDERCHK DB_NOOVERWRITE
	       DB_NOPANIC DB_NORECURSE DB_NOSERVER DB_NOSERVER_HOME
	       DB_NOSERVER_ID DB_NOSYNC DB_NOTFOUND DB_ODDFILESIZE DB_OK_BTREE
	       DB_OK_HASH DB_OK_QUEUE DB_OK_RECNO DB_OLD_VERSION DB_OPEN_CALLED
	       DB_OPFLAGS_MASK DB_ORDERCHKONLY DB_OVERWRITE DB_PAD DB_PAGEYIELD
	       DB_PAGE_LOCK DB_PAGE_NOTFOUND DB_PANIC_ENVIRONMENT DB_PERMANENT
	       DB_POSITION DB_POSITIONI DB_PREV DB_PREV_NODUP DB_PRINTABLE
	       DB_PRIVATE DB_PR_HEADERS DB_PR_PAGE DB_PR_RECOVERYTEST
	       DB_QAMMAGIC DB_QAMOLDVER DB_QAMVERSION DB_RDONLY DB_RDWRMASTER
	       DB_RECNUM DB_RECORDCOUNT DB_RECORD_LOCK DB_RECOVER
	       DB_RECOVER_FATAL DB_REGION_ANON DB_REGION_INIT DB_REGION_MAGIC
	       DB_REGION_NAME DB_REGISTERED DB_RENAMEMAGIC DB_RENUMBER
	       DB_REP_CLIENT DB_REP_DUPMASTER DB_REP_HOLDELECTION
	       DB_REP_LOGSONLY DB_REP_MASTER DB_REP_NEWMASTER DB_REP_NEWSITE
	       DB_REP_OUTDATED DB_REP_PERMANENT DB_REP_UNAVAIL DB_REVSPLITOFF
	       DB_RMW DB_RPC_SERVERPROG DB_RPC_SERVERVERS DB_RUNRECOVERY
	       DB_SALVAGE DB_SECONDARY_BAD DB_SEQUENTIAL DB_SET
	       DB_SET_LOCK_TIMEOUT DB_SET_RANGE DB_SET_RECNO DB_SET_TXN_NOW
	       DB_SET_TXN_TIMEOUT DB_SNAPSHOT DB_STAT_CLEAR DB_SURPRISE_KID
	       DB_SWAPBYTES DB_SYSTEM_MEM DB_TEMPORARY DB_TEST_ELECTINIT
	       DB_TEST_ELECTSEND DB_TEST_ELECTVOTE1 DB_TEST_ELECTVOTE2
	       DB_TEST_ELECTWAIT1 DB_TEST_ELECTWAIT2 DB_TEST_POSTDESTROY
	       DB_TEST_POSTEXTDELETE DB_TEST_POSTEXTOPEN DB_TEST_POSTEXTUNLINK
	       DB_TEST_POSTLOG DB_TEST_POSTLOGMETA DB_TEST_POSTOPEN
	       DB_TEST_POSTRENAME DB_TEST_POSTSYNC DB_TEST_PREDESTROY
	       DB_TEST_PREEXTDELETE DB_TEST_PREEXTOPEN DB_TEST_PREEXTUNLINK
	       DB_TEST_PREOPEN DB_TEST_PRERENAME DB_TEST_SUBDB_LOCKS DB_THREAD
	       DB_TIMEOUT DB_TRUNCATE DB_TXNMAGIC DB_TXNVERSION DB_TXN_CKP
	       DB_TXN_LOCK DB_TXN_LOCK_2PL DB_TXN_LOCK_MASK
	       DB_TXN_LOCK_OPTIMIST DB_TXN_LOCK_OPTIMISTIC DB_TXN_LOG_MASK
	       DB_TXN_LOG_REDO DB_TXN_LOG_UNDO DB_TXN_LOG_UNDOREDO
	       DB_TXN_NOSYNC DB_TXN_NOWAIT DB_TXN_REDO DB_TXN_SYNC DB_TXN_UNDO
	       DB_TXN_WRITE_NOSYNC DB_UNRESOLVED_CHILD DB_UPDATE_SECONDARY
	       DB_UPGRADE DB_USE_ENVIRON DB_USE_ENVIRON_ROOT DB_VERB_CHKPOINT
	       DB_VERB_DEADLOCK DB_VERB_RECOVERY DB_VERB_REPLICATION
	       DB_VERB_WAITSFOR DB_VERIFY DB_VERIFY_BAD DB_VERIFY_FATAL
	       DB_VERSION_MAJOR DB_VERSION_MINOR DB_VERSION_PATCH
	       DB_VRFY_FLAGMASK DB_WRITECURSOR DB_WRITELOCK DB_WRITEOPEN
	       DB_WRNOSYNC DB_XA_CREATE DB_XIDDATASIZE DB_YIELDCPU),
            {name=>"DB_BTREE", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 2) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \\\n     DB_VERSION_PATCH >= 0)\n", "#endif\n"]},
            {name=>"DB_HASH", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 2) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \\\n     DB_VERSION_PATCH >= 0)\n", "#endif\n"]},
            {name=>"DB_LOCK_DUMP", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 2) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \\\n     DB_VERSION_PATCH >= 0)\n", "#endif\n"]},
            {name=>"DB_LOCK_GET", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 2) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \\\n     DB_VERSION_PATCH >= 0)\n", "#endif\n"]},
            {name=>"DB_LOCK_GET_TIMEOUT", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 4) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 0 && \\\n     DB_VERSION_PATCH >= 7)\n", "#endif\n"]},
            {name=>"DB_LOCK_INHERIT", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 2) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 7) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 7 && \\\n     DB_VERSION_PATCH >= 1)\n", "#endif\n"]},
            {name=>"DB_LOCK_PUT", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 2) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \\\n     DB_VERSION_PATCH >= 0)\n", "#endif\n"]},
            {name=>"DB_LOCK_PUT_ALL", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 2) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \\\n     DB_VERSION_PATCH >= 0)\n", "#endif\n"]},
            {name=>"DB_LOCK_PUT_OBJ", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 2) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \\\n     DB_VERSION_PATCH >= 0)\n", "#endif\n"]},
            {name=>"DB_LOCK_PUT_READ", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 4) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 0 && \\\n     DB_VERSION_PATCH >= 7)\n", "#endif\n"]},
            {name=>"DB_LOCK_TIMEOUT", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 4) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 0 && \\\n     DB_VERSION_PATCH >= 7)\n", "#endif\n"]},
            {name=>"DB_LOCK_TRADE", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 4) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \\\n     DB_VERSION_PATCH >= 17)\n", "#endif\n"]},
            {name=>"DB_LOCK_UPGRADE_WRITE", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 3) || \\\n    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR > 3) || \\\n    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR == 3 && \\\n     DB_VERSION_PATCH >= 4)\n", "#endif\n"]},
            {name=>"DB_PRIORITY_DEFAULT", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 4) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \\\n     DB_VERSION_PATCH >= 17)\n", "#endif\n"]},
            {name=>"DB_PRIORITY_HIGH", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 4) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \\\n     DB_VERSION_PATCH >= 17)\n", "#endif\n"]},
            {name=>"DB_PRIORITY_LOW", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 4) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \\\n     DB_VERSION_PATCH >= 17)\n", "#endif\n"]},
            {name=>"DB_PRIORITY_VERY_HIGH", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 4) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \\\n     DB_VERSION_PATCH >= 17)\n", "#endif\n"]},
            {name=>"DB_PRIORITY_VERY_LOW", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 4) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \\\n     DB_VERSION_PATCH >= 17)\n", "#endif\n"]},
            {name=>"DB_QUEUE", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 3) || \\\n    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR > 0) || \\\n    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR == 0 && \\\n     DB_VERSION_PATCH >= 55)\n", "#endif\n"]},
            {name=>"DB_RECNO", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 2) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \\\n     DB_VERSION_PATCH >= 0)\n", "#endif\n"]},
            {name=>"DB_TXN_ABORT", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 3) || \\\n    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR > 1) || \\\n    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR == 1 && \\\n     DB_VERSION_PATCH >= 12)\n", "#endif\n"]},
            {name=>"DB_TXN_APPLY", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 4) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 0 && \\\n     DB_VERSION_PATCH >= 7)\n", "#endif\n"]},
            {name=>"DB_TXN_BACKWARD_ALLOC", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 4) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \\\n     DB_VERSION_PATCH >= 17)\n", "#endif\n"]},
            {name=>"DB_TXN_BACKWARD_ROLL", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 3) || \\\n    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR > 1) || \\\n    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR == 1 && \\\n     DB_VERSION_PATCH >= 12)\n", "#endif\n"]},
            {name=>"DB_TXN_FORWARD_ROLL", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 3) || \\\n    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR > 1) || \\\n    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR == 1 && \\\n     DB_VERSION_PATCH >= 12)\n", "#endif\n"]},
            {name=>"DB_TXN_GETPGNOS", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 4) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \\\n     DB_VERSION_PATCH >= 17)\n", "#endif\n"]},
            {name=>"DB_TXN_OPENFILES", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 3) || \\\n    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR > 1) || \\\n    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR == 1 && \\\n     DB_VERSION_PATCH >= 12)\n", "#endif\n"]},
            {name=>"DB_TXN_POPENFILES", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 3) || \\\n    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR > 3) || \\\n    (DB_VERSION_MAJOR == 3 && DB_VERSION_MINOR == 3 && \\\n     DB_VERSION_PATCH >= 4)\n", "#endif\n"]},
            {name=>"DB_TXN_PRINT", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 4) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 1) || \\\n    (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR == 1 && \\\n     DB_VERSION_PATCH >= 17)\n", "#endif\n"]},
            {name=>"DB_UNKNOWN", type=>"IV", macro=>["#if (DB_VERSION_MAJOR > 2) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR > 0) || \\\n    (DB_VERSION_MAJOR == 2 && DB_VERSION_MINOR == 0 && \\\n     DB_VERSION_PATCH >= 0)\n", "#endif\n"]},
            {name=>"DB_VERSION_STRING", type=>"PV"});

print constant_types(); # macro defs
foreach (C_constant ("BerkeleyDB", 'constant', 'IV', $types, undef, 3, @names) ) {
    print $_, "\n"; # C constant subs
}
print "#### XS Section:\n";
print XS_constant ("BerkeleyDB", $types);
__END__
   */

  switch (len) {
  case 6:
    return constant_6 (aTHX_ name, iv_return);
    break;
  case 7:
    return constant_7 (aTHX_ name, iv_return);
    break;
  case 8:
    return constant_8 (aTHX_ name, iv_return);
    break;
  case 9:
    return constant_9 (aTHX_ name, iv_return);
    break;
  case 10:
    return constant_10 (aTHX_ name, iv_return);
    break;
  case 11:
    return constant_11 (aTHX_ name, iv_return);
    break;
  case 12:
    return constant_12 (aTHX_ name, iv_return);
    break;
  case 13:
    return constant_13 (aTHX_ name, iv_return);
    break;
  case 14:
    return constant_14 (aTHX_ name, iv_return);
    break;
  case 15:
    return constant_15 (aTHX_ name, iv_return);
    break;
  case 16:
    return constant_16 (aTHX_ name, iv_return);
    break;
  case 17:
    return constant_17 (aTHX_ name, iv_return, pv_return);
    break;
  case 18:
    return constant_18 (aTHX_ name, iv_return);
    break;
  case 19:
    return constant_19 (aTHX_ name, iv_return);
    break;
  case 20:
    return constant_20 (aTHX_ name, iv_return);
    break;
  case 21:
    return constant_21 (aTHX_ name, iv_return);
    break;
  case 22:
    /* Names all of length 22.  */
    /* DB_ENV_RPCCLIENT_GIVEN DB_TXN_LOCK_OPTIMISTIC */
    /* Offset 8 gives the best switch position.  */
    switch (name[8]) {
    case 'O':
      if (memEQ(name, "DB_TXN_LOCK_OPTIMISTIC", 22)) {
      /*                       ^                    */
#ifdef DB_TXN_LOCK_OPTIMISTIC
        *iv_return = DB_TXN_LOCK_OPTIMISTIC;
        return PERL_constant_ISIV;
#else
        return PERL_constant_NOTDEF;
#endif
      }
      break;
    case 'P':
      if (memEQ(name, "DB_ENV_RPCCLIENT_GIVEN", 22)) {
      /*                       ^                    */
#ifdef DB_ENV_RPCCLIENT_GIVEN
        *iv_return = DB_ENV_RPCCLIENT_GIVEN;
        return PERL_constant_ISIV;
#else
        return PERL_constant_NOTDEF;
#endif
      }
      break;
    }
    break;
  case 23:
    if (memEQ(name, "DB_ENV_TXN_WRITE_NOSYNC", 23)) {
#ifdef DB_ENV_TXN_WRITE_NOSYNC
      *iv_return = DB_ENV_TXN_WRITE_NOSYNC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

