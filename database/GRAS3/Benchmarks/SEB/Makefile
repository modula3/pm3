# if you use an SRC Modula-3 compiler (including PM3 and relatives), set
# M3COMPILER to src. If you use the critical mass compiler, set it to cm3.
# The variable M3HOME contains the path to the installed packages of your
# Modula-3 compiler.

M3COMPILER=src
CC=gcc

ifeq ($(M3COMPILER), src)
  M3HOME = /usr/local/public/packages/m3/lib/pkg
else
  M3HOME = /home/systems/reactor/reactor4.1-1.i3/pkg
endif

#
# Normally, you don't need to change anything below
# NOTE: For Linux libc6, change the TARGET variable to LINUXLIBC6 !
#

#
# set M3 TARGET
#
os=$(shell uname)
ifeq ($(os), Linux)
   TARGET = LINUXELF
else
  osver=$(shell uname -r)
  ifeq ($(osver), 4.1.3)
     TARGET = SPARC
  else
     TARGET = SOLgnu
  endif
endif

CC = gcc
INCLUDES = 	-I.\
		-I$(GRASHOME)/rgras/src \
		-I$(GRASHOME)/rgras/$(TARGET)

CFLAGS = -g $(INCLUDES)

LDPATHS = \
	$(GRASHOME)/CM3Init/$(TARGET) \
	$(GRASHOME)/rgras/$(TARGET) \
	$(GRASHOME)/HiGRAS/$(TARGET) \
	$(GRASHOME)/PageClient/$(TARGET) \
	$(GRASHOME)/RuleEngine/$(TARGET) \
	$(M3HOME)/set/$(TARGET) \
	$(GRASHOME)/GrasCommon/$(TARGET) \
	$(M3HOME)/parseparams/$(TARGET) \
	$(M3HOME)/netobj/$(TARGET) \
	$(M3HOME)/tcp/$(TARGET) \
	$(M3HOME)/libm3/$(TARGET) \
	$(M3HOME)/m3core/$(TARGET)

GRAS3LIBS = -lCM3Init -lRGRASGraphs -lHiGRAS -lPageClient \
	-lRuleEngine -lGrasCommon
M3LIBS = -lset -lm3parseparams -lm3netobj -lm3tcp -lm3 -lm3core

ifeq ($(TARGET), SOLgnu)
  LDFLAGS = \
	$(patsubst %, -L%, $(LDPATHS)) \
	$(patsubst %, -R%, $(LDPATHS))
  LDLIBS = $(GRAS3LIBS) $(M3LIBS) -lsocket -lm
else
  COMMA=,
  LDFLAGS = \
	$(patsubst %, -L%, $(LDPATHS)) \
	$(patsubst %, -Wl$(COMMA)-R%, $(LDPATHS))
  LDLIBS = $(GRAS3LIBS) $(M3LIBS) -lm
endif

SRC = benchmark.c \
	bm_basic.c \
	bm_buildDB.c \
	bm_clock.c \
	bm_database.c \
	bm_global.c \
	bm_log.c \
	bm_phase1.c \
	bm_phase2.c \
	bm_phase3.c \
	bm_random.c \
	bm_smallset.c \
	bm_system.c

OBJECTS = $(patsubst %.c, %.o, $(SRC))

all: benchmark

benchmark: $(OBJECTS)
	$(CC) -o benchmark $(OBJECTS) $(LDFLAGS) $(LDLIBS)

clean:
	rm $(OBJECTS)
