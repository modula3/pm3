INTERFACE Multi;

FROM Ctypes IMPORT int;

<*EXTERNAL*> PROCEDURE ipc_reset();
<*EXTERNAL*> PROCEDURE ipc_open(clients, threads: int);
<*EXTERNAL*> PROCEDURE ipc_start(threads: int);
<*EXTERNAL*> PROCEDURE ipc_stop(threads: int);
<*EXTERNAL*> PROCEDURE ipc_close(threads: int);

END Multi.
