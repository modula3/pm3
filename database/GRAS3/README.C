The C Interface

Since version 1.1, the C interface of the rgras package is included
in the distribution. It contains all procedures and functions the 
RGRASGraph.i3 interface offers as C functions. Before you try to
compile and use this interface, please read this file.

First of all, it isn't easy to call Modula-3 routines without a
Modula-3 main program. To achieve this, we need to read out the
interface records of the modules we are interested in and build a
jump-table from this information during program startup. The code to
obtain this information differs slightly form platform to platform and
also between different Modula-3 compilers.  To achieve highest
flexibility, we use the m4 macro processor to generate the proper code.
Currently the build procedure only knows about three Modula-3 compilers,
namely SRC, PM3 and Critical Mass. If you use a different compiler, it
might be that building the C-interface will not work properly.

The jump tables are only one part of the C interface of GRAS. 
To be able to use GRAS without a Modula-3 main program, the
Modula-3 runtime system - featuring garbage collection, threads
and inter-process communication - must be explicitly initialized.
This is done by calling the C function 'M3Initialize' of the CM3Init
package. An example for a C program using GRAS' rgras interface is
the SEB (Software Engineering Benchmark) in the Benchmarks directory.
It demonstrates how to use the interface and also contains a Makefile
that shows what libraries and include files are necessary to build
such a program.

For further information on the technical issues of the C interface,
have a look at the README file in CM3Init/src. This file describes how
to create the runtime initialization code for a Modula-3 library. This
is necessary, if your platform is currently not directly supported by
this release. Normally, however, the startup code of CM3init is gernerated
during system build, so you do not have to worry about that.

**** IMPORTANT ****

Before you are able to compile CM3Init, you have to make the two
interfaces 'RT0u.i3' and 'RTThreadInit.i3' visible. These interfaces
are imported by RTLinker, which we had to patch for the explicit
startup. The easisiest way to achieve this is to edit the file
.M3EXPORTS under m3core/TARGET in your Modula-3 compilers directory
hierarchy. There, delete the word 'hidden' from the two corresponding
lines. (The hard way would be to edit the makefiles of m3core and
rebuild it.) This step is not necessary for the critical mass
compiler.
