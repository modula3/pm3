# Copyright 1989 Digital Equipment Corporation.
# Distributed only by permission.
# Last modified on Fri Jun 14 19:07:44 1991 by kalsow
#      modified on Wed Jun  5 03:20:17 1991 by muller
#      modified on Thu Feb  7 12:23:03 PST 1991 by stolfi
#****************************************************************
# Caller must define the environment vars 
#   ${CPU_TYPE} ${M3COMPILER}

#*************************************************************************

DIRS = p001 p002 p003 p004 p005 p006 p007 p008 p009 p010 p011 p012 p013\
  p014 p015 p016 p017 p018 p019 p020 p021 p022 p023 p024 p025 p026 p027\
  p028 p029 p030 p031 p032 p033 p034 p035 p036 p037 p038 p039 p040 p041\
  p042 p043 p044 p045 p046 p047 p048 p049 p050 p051 p052 p053 p054 p055\
  p056 p057 p058 p059 p060 p061 p062 p063 p064 p065 p066 p067 p068 p069\
  p070 p071 p072 p073 p074 p075 p076 p077 p078 p079 p080 p081 p082 p083

#*************************************************************************
# Recursive make calls:

SUBMAKE = make $(MFLAGS) \
  PACKAGE="$(PACKAGE)" \
  PROJECT="$(PROJECT)" \
  CPU_TYPE="$${CPU_TYPE}" \
  M3COMPILER="$${M3COMPILER}"

# ***************************************************************
# Entry to bring the package up to date ("all" must be first)

all: ;\
  @for dir in $(DIRS);\
     do (cd $$dir ; echo --- $$dir ---- ; $(SUBMAKE) all);\
   done

# ***************************************************************
# Standard entries to remove files from the directories
#
#    tidy    -- eliminate garbage files
#    scratch -- delete derived files in preparation for rebuild

tidy: localtidy ;\
  @for dir in $(DIRS); do (cd $$dir ; $(SUBMAKE) tidy); done

localtidy: ;\
  /bin/rm -f ,* .,* *~ .emacs_[0-9]* core a.out m3_* m3*.c

scratch: localscratch ;\
  @for dir in $(DIRS); do (cd $$dir ; $(SUBMAKE) scratch); done

localscratch: localtidy  
  
# *****************************************************************


