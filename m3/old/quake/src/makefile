# Copyright (C) 1993, Digital Equipment Corporation
# All rights reserved.
# See the file COPYRIGHT for a full description.

# Last modified on Wed Nov 30 08:31:59 PST 1994 by kalsow   
#      modified on Tue Jan 25 16:47:36 1994 by harrison 
#      modified on Tue May  4 21:20:29 PDT 1993 by mjordan 

TARGET = "target_unknown"

PACKAGE = quake
PROGRAM = quake

#
# parser.y generates y.tab.h for lexical.c, so parser.o must come first
#

OBJECTS = \
	parser.o \
	lexical.o \
	array.o \
	atom.o \
	hash.o \
	builtin.o \
	code.o \
	dict.o \
	execute.o \
	fileio.o \
	iostack.o \
	list.o \
	name.o \
	operator.o \
	path.o \
	quake.o \
	table.o \
	stack.o \
	strng.o \
	utils.o

SRC = ../src
LOCAL = .

SOURCES = \
	$(SRC)/parser.c \
	$(SRC)/lexical.c \
	$(SRC)/array.c \
	$(SRC)/atom.c \
	$(SRC)/hash.c \
	$(SRC)/builtin.c \
	$(SRC)/code.c \
	$(SRC)/dict.c \
	$(SRC)/execute.c \
	$(SRC)/fileio.c \
	$(SRC)/iostack.c \
	$(SRC)/list.c \
	$(SRC)/name.c \
	$(SRC)/operator.c \
	$(SRC)/path.c \
	$(SRC)/quake.c \
	$(SRC)/table.c \
	$(SRC)/stack.c \
	$(SRC)/strng.c \
	$(SRC)/utils.c

MAKEFILE = $(SRC)/makefile

DEBUG_DEFINES = # -DDEBUG_RANGE_CHECKING -DDEBUG_MALLOC -DHASH_PROFILE -DYYDEBUG

# If DEBUG_MALLOC is enabled include thes following two lines ...
#	INCLUDES = -I. -I$(SRC) -I../dbmalloc
#	LIBRARIES = -L../dbmalloc -ldbmalloc -lm
# ... otherwise, these line
INCLUDES = -I. -I$(SRC)
LIBRARIES = -lm

DEFINES = -DTARGET_$(TARGET) $(DEBUG_DEFINES)

CC = cc
## need
##   CC = cc -posix -Wno-precomp   on NEXT
##   CC = gcc                      on VAX and HPPA
##   CDEBUG = ""                   on HP300

YACC = bison -y
COPT = 
CDEBUG = -g
CFLAGS = $(COPT) $(CDEBUG) $(INCLUDES) $(DEFINES)
YFLAGS = -d

all: $(PROGRAM)

$(PROGRAM): $(OBJECTS) $(MAKEFILE)
	$(CC) $(CFLAGS) $(OBJECTS) $(LIBRARIES) -o $@

clean: 
	-rm -f $(PROGRAM) $(OBJECTS)

depend: $(MAKEFILE) $(SOURCES)
	makedepend -f$(MAKEFILE) $(INCLUDES) $(DEFINES) $(SOURCES)

array.o:    $(SRC)/array.c;    $(CC) -c $(CFLAGS) $(SRC)/array.c
atom.o:     $(SRC)/atom.c;     $(CC) -c $(CFLAGS) $(SRC)/atom.c
builtin.o:  $(SRC)/builtin.c;  $(CC) -c $(CFLAGS) $(SRC)/builtin.c
code.o:     $(SRC)/code.c;     $(CC) -c $(CFLAGS) $(SRC)/code.c
dict.o:     $(SRC)/dict.c;     $(CC) -c $(CFLAGS) $(SRC)/dict.c
execute.o:  $(SRC)/execute.c;  $(CC) -c $(CFLAGS) $(SRC)/execute.c
fileio.o:   $(SRC)/fileio.c;   $(CC) -c $(CFLAGS) $(SRC)/fileio.c
hash.o:     $(SRC)/hash.c;     $(CC) -c $(CFLAGS) $(SRC)/hash.c
iostack.o:  $(SRC)/iostack.c;  $(CC) -c $(CFLAGS) $(SRC)/iostack.c
lexical.o:  $(SRC)/lexical.c;  $(CC) -c $(CFLAGS) $(SRC)/lexical.c
list.o:     $(SRC)/list.c;     $(CC) -c $(CFLAGS) $(SRC)/list.c
name.o:     $(SRC)/name.c;     $(CC) -c $(CFLAGS) $(SRC)/name.c
operator.o: $(SRC)/operator.c; $(CC) -c $(CFLAGS) $(SRC)/operator.c
parser.o:   $(SRC)/parser.c;   $(CC) -c $(CFLAGS) $(SRC)/parser.c
path.o:     $(SRC)/path.c;     $(CC) -c $(CFLAGS) $(SRC)/path.c
quake.o:    $(SRC)/quake.c;    $(CC) -c $(CFLAGS) $(SRC)/quake.c
stack.o:    $(SRC)/stack.c;    $(CC) -c $(CFLAGS) $(SRC)/stack.c
strng.o:    $(SRC)/strng.c;    $(CC) -c $(CFLAGS) $(SRC)/strng.c
table.o:    $(SRC)/table.c;    $(CC) -c $(CFLAGS) $(SRC)/table.c
utils.o:    $(SRC)/utils.c;    $(CC) -c $(CFLAGS) $(SRC)/utils.c
