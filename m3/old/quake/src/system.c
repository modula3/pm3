/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Mon Mar 14 09:56:15 PST 1994 by harrison
 */

/*
 * Here's a simple implementation of system(3) for Un*x systems without it.
 * These are usually really anal about being POSIX compliant and nothing more.
 * Read Lunix here.  Funny how quake ported flawlessly to NT whereas Linux
 * bungled it totally.  /S */

#include <signal.h>
#include <unistd.h>

#define	DEFAULT_SHELL	"/bin/sh"

int system(command)
char *command;
{
    int status, pid, child;
    void (*interupt_signal)(), (*quit_signal)();

    /* Are we just checking for the shell? */
    if (command == NULL)
	return access(DEFAULT_SHELL, X_OK) == 0 ? 1 : 0; /* not a Boolean */

    if ((pid = vfork()) == 0) {
	execl(DEFAULT_SHELL, "sh", "-c", command, (char *) NULL);
	_exit(127);
    }

    interupt_signal = signal(SIGINT,  SIG_IGN);
    quit_signal     = signal(SIGQUIT, SIG_IGN);

    while ((child = wait(&status)) != pid && child != -1)
	;
    if (child == -1)
	status = -1;

    signal(SIGINT,  interupt_signal);
    signal(SIGQUIT, quit_signal);

    return status;
}
    
