#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sched.h>
  
int ipc_debug = 0;

#ifdef linux
#if defined(_SEM_SEMUN_UNDEFINED)
/* according to X/OPEN we have to define it ourselves */
union semun {
  int val;                  /* value for SETVAL */
  struct semid_ds *buf;     /* buffer for IPC_STAT, IPC_SET */
  unsigned short *array;    /* array for GETALL, SETALL */
  /* Linux specific part: */
  struct seminfo *__buf;    /* buffer for IPC_INFO */
};
#else
/* union semun is defined by including <sys/sem.h> */
#endif
#else
union semun {
  int     val;            /* value for SETVAL */
  struct  semid_ds *buf;  /* buffer for IPC_STAT & IPC_SET */
  u_short *array;         /* array for GETALL & SETALL */
};
#endif 

/* Various operations */
#define LOCKSZ 2
static struct sembuf op_lock[LOCKSZ] = {
  {2, 0, 0},        /* wait for [2] (lock) to equal 0 */
  {2, 1, SEM_UNDO}  /* then increment [2] to 1 - this locks it */
  /* UNDO to release the lock if processes exits */                               /* before explicitly unlocking */
};
#define UNLOCKSZ 1
static struct sembuf op_unlock[UNLOCKSZ] = {
  {2, -1, SEM_UNDO} /* decrement [2] (lock) back to 0 */
};

static int semid;

void
ipc_reset()
{
  key_t key;
  FILE * fp;

  if (ipc_debug) fprintf(stderr, "ipc_reset\n");

  fp = fopen("_ipc", "a+");
  if(!fp) abort();
  fclose(fp);
  key = ftok("_ipc",'g');
  
  semid = semget(key, 3, IPC_CREAT|S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP);
  if (semid < 0) {
    fprintf(stderr, "semget failed: %s\n", strerror(errno));
    abort();
  }
  if (semctl(semid, 0, IPC_RMID, 0)) {
    fprintf(stderr, "can't remove semaphore: %s\n", strerror(errno));
    abort();
  }
  unlink("_ipc");
}

void
ipc_open(int clients, int threads)
{
  key_t key;
  FILE * fp;
 
  if (ipc_debug)
    fprintf(stderr, "ipc_open clients=%d threads=%d\n", clients, threads);

  fp = fopen("_ipc", "a+");
  if(!fp) abort();
  fclose(fp);
  key = ftok("_ipc",'g');

  semid = semget(key, 3, IPC_CREAT|S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP);
  if (semid < 0) {
    fprintf(stderr, "semget failed: %s\n", strerror(errno));
    abort();
  }

  while (semop(semid, &op_lock[0], LOCKSZ)) {
    if (errno == EINTR) {
      sched_yield();
      continue;
    }
    fprintf(stderr, "can't lock semaphore: %s\n", strerror(errno));
    abort();
  }
  {
    ushort values[3];
    union semun arg;
    arg.array = values;
    if (semctl(semid, 0, GETALL, arg)) {
      fprintf(stderr, "can't get values: %s\n", strerror(errno));
      abort();
    }
    if (ipc_debug) fprintf(stderr, "values %d: %d %d %d\n",
			   getpid(), values[0], values[1], values[2]);
    if (values[0] == 0) {
      values[0] = clients;
      values[1] = clients;
      if (semctl(semid, 0, SETALL, arg)) {
	fprintf(stderr, "can't set values: %s\n", strerror(errno));
	abort();
      }
    }
  }
  while (semop(semid, &op_unlock[0], UNLOCKSZ)) {
    if (errno == EINTR) {
      sched_yield();
      continue;
    }
    fprintf(stderr, "can't unlock semaphore: %s\n", strerror(errno));
    abort();
  }

  {
    struct sembuf op = { 1, -threads, SEM_UNDO };
    while (semop(semid, &op, 1)) {
      if (errno == EINTR) {
	sched_yield();
	continue;
      }
      fprintf(stderr, "can't unlock semaphore: %s\n", strerror(errno));
      abort();
    }
  }
  {
    struct sembuf op = { 1, 0, 0 };
    while (semop(semid, &op, 1)) {
      if (errno == EINTR) {
	sched_yield();
	continue;
      }
      fprintf(stderr, "can't unlock semaphore: %s\n", strerror(errno));
      abort();
    }
  }

}

void
ipc_start(int threads)
{
  if (ipc_debug) {
    fprintf(stderr, "Starting %d\n", getpid());

    while (semop(semid, &op_lock[0], LOCKSZ)) {
      if (errno == EINTR) {
	sched_yield();
	continue;
      }
      fprintf(stderr, "can't lock semaphore: %s\n", strerror(errno));
      abort();
    }
    {
      ushort values[3];
      union semun arg;
      arg.array = values;
      if (semctl(semid, 0, GETALL, arg)) {
	fprintf(stderr, "can't get values: %s\n", strerror(errno));
	abort();
      }
      fprintf(stderr, "values %d: %d %d %d\n",
	      getpid(), values[0], values[1], values[2]);
    }
    while (semop(semid, &op_unlock[0], UNLOCKSZ)) {
      if (errno == EINTR) {
	sched_yield();
	continue;
      }
      fprintf(stderr, "can't unlock semaphore: %s\n", strerror(errno));
      abort();
    }
  }

  {
    struct sembuf op = { 0, -threads, SEM_UNDO};
    while (semop(semid, &op, 1)) {
      if (errno == EINTR) {
	sched_yield();
	continue;
      }
      fprintf(stderr, "can't unlock semaphore: %s\n", strerror(errno));
      abort();
    }
  }

  {
    struct sembuf op = { 0, 0, 0 };
    while (semop(semid, &op, 1)) {
      if (errno == EINTR) {
	sched_yield();
	continue;
      }
      fprintf(stderr, "can't unlock semaphore: %s\n", strerror(errno));
      abort();
    }
  }

  {
    struct sembuf op = { 1, threads, SEM_UNDO };
    while (semop(semid, &op, 1)) {
      if (errno == EINTR) {
	sched_yield();
	continue;
      }
      fprintf(stderr, "can't unlock semaphore: %s\n", strerror(errno));
      abort();
    }
  }

  if (ipc_debug) fprintf(stderr, "Started %d\n", getpid());
}

void
ipc_stop(int threads)
{
  if (ipc_debug) {
    fprintf(stderr, "Stopping %d\n", getpid());

    while (semop(semid, &op_lock[0], LOCKSZ)) {
      if (errno == EINTR) {
	sched_yield();
	continue;
      }
      fprintf(stderr, "can't lock semaphore: %s\n", strerror(errno));
      abort();
    }
    {
      ushort values[3];
      union semun arg;
      arg.array = values;
      if (semctl(semid, 0, GETALL, arg)) {
	fprintf(stderr, "can't get values: %s\n", strerror(errno));
	abort();
      }
      fprintf(stderr, "values %d: %d %d %d\n",
	      getpid(), values[0], values[1], values[2]);
      if (semctl(semid, 0, SETALL, arg)) {
	fprintf(stderr, "can't set values: %s\n", strerror(errno));
	abort();
      }
    }
    while (semop(semid, &op_unlock[0], UNLOCKSZ)) {
      if (errno == EINTR) {
	sched_yield();
	continue;
      }
      fprintf(stderr, "can't unlock semaphore: %s\n", strerror(errno));
      abort();
    }
  }

  {
    struct sembuf op = { 1, -threads, SEM_UNDO };
    while (semop(semid, &op, 1)) {
      if (errno == EINTR) {
	sched_yield();
	continue;
      }
      fprintf(stderr, "can't unlock semaphore: %s\n", strerror(errno));
      abort();
    }
  }

  {
    struct sembuf op = { 1, 0, 0 };
    while (semop(semid, &op, 1)) {
      if (errno == EINTR) {
	sched_yield();
	continue;
      }
      fprintf(stderr, "can't unlock semaphore: %s\n", strerror(errno));
      abort();
    }
  }

  {
    struct sembuf op = { 0, threads, SEM_UNDO };
    while (semop(semid, &op, 1)) {
      if (errno == EINTR) {
	sched_yield();
	continue;
      }
      fprintf(stderr, "can't unlock semaphore: %s\n", strerror(errno));
      abort();
    }
  }

  if (ipc_debug) fprintf(stderr, "Stopped %d\n", getpid());
}

void
ipc_close(int threads)
{
  if (ipc_debug) fprintf(stderr, "Close %d\n", getpid());

  {
    struct sembuf op = { 0, -threads, SEM_UNDO};
    while (semop(semid, &op, 1)) {
      if (errno == EIDRM) return;
      if (errno == EINVAL) return;
      if (errno == EINTR) {
	sched_yield();
	continue;
      }
      fprintf(stderr, "can't unlock semaphore: %s\n", strerror(errno));
      abort();
    }
  }

  {
    struct sembuf op = { 0, 0, 0 };
    while (semop(semid, &op, 1)) {
      if (errno == EIDRM) return;
      if (errno == EINVAL) return;
      if (errno == EINTR) {
	sched_yield();
	continue;
      }
      fprintf(stderr, "can't unlock semaphore: %s\n", strerror(errno));
      abort();
    }
  }

  while (semop(semid, &op_lock[0], LOCKSZ)) {
    switch (errno) {
    case EINVAL:
    case EIDRM:
      return;
    case EINTR:
      sched_yield();
      continue;
    default:
      fprintf(stderr, "can't lock semaphore: %s\n", strerror(errno));
      abort();
    }
  }
  if (semctl(semid, 0, IPC_RMID, 0)) {
    fprintf(stderr, "can't remove semaphore: %s\n", strerror(errno));
    abort();
  }
  unlink("_ipc");
}
