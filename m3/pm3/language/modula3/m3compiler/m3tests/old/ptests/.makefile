# 1 ""



# 1 "/proj/m3/lib.mips/toplevel.tmpl.SRC"















# 1 "/proj/m3/lib.mips/config"






















CPU_TYPE = mips















PREFIX = /proj
INSTALL_PREFIX = $(PREFIX)


BIN = mips/bin
BIN_USE     = $(PREFIX)/$(BIN)
BIN_INSTALL = $(INSTALL_PREFIX)/$(BIN)


PUB = m3/pub.mips
PUB_USE     = $(PREFIX)/$(PUB)
PUB_INSTALL = $(INSTALL_PREFIX)/$(PUB)


LIB = m3/lib.mips
LIB_USE     = $(PREFIX)/$(LIB)
LIB_INSTALL = $(INSTALL_PREFIX)/$(LIB)


GNUEMACS_INSTALL = $(INSTALL_PREFIX)/generic/lib/elisp




MAN = man/mips
MAN_USE  = $(PREFIX)/$(MAN)
MAN_INSTALL = $(INSTALL_PREFIX)/$(MAN)



DOC = doc
DOC_USE     = $(PREFIX)/$(DOC)
DOC_INSTALL = $(INSTALL_PREFIX)/$(DOC)















XLIBPATH = /usr/local/lib
XLIB     = -lX11
















CC = cc





CC_WARNS_FOR_UNKNOWN_FILES = 0


CC_G = @-g2@


CC_O = @-O2@




KEEP_LIBRARIES_RESOLVED = 1


CFLAGS = 
 

M3OPT = -g


BOOTOPT = -g


IMAKEFLAGS =



SHELL = /bin/sh



SHELL_PGMS = csh




CPP = /lib/cpp




MAKE = make
   
























SEP   = @
PASS0 = @$(LIB_USE)/m3compiler@
PASS1 = @cc@-G@0@-Wf,-XNp200000@-Wf,-XNd150000@
PASS2 = @cc@-G@0@
PASS3 = @/bin/ar@cru@
PASS4 = @ranlib@
PASS5 = @/bin/ld@-A@
OVERLAY_0 = @-G@0@-g2@
OVERLAY_1 = @-lc_G0@
BASE_ARGS = @-N@




SERVER_LIMIT = 0




DEFPATH = .:$(PUB_USE)






LIBPATH = .:$(LIB_USE):$(XLIBPATH)
# 207 "/proj/m3/lib.mips/config"




LINKBFILES = @-lm@
LINKFILES = @-lm3@-lm@


LINKCOVER  = $(LIB_USE)/report_coverage.o



INCL = $(LIB_USE)


TEMPLATE = toplevel.tmpl.SRC
# 16 "/proj/m3/lib.mips/toplevel.tmpl.SRC"


all::


M3        = $(BIN_USE)/m3
		
M3FLAGS   = -w1 -make -why
M3DEFPATH =
M3LIBPATH =
DO_M3     = $(M3) $(M3FLAGS) $(M3OPT) $(M3OPTIONS) $(M3DEFPATH) $(M3LIBPATH)


X11LIBS   = -L$(XLIBPATH) -lXaw -lXmu -lXext -lXt $(XLIB)
M3X11LIBS = -lm3X11R4 $(X11LIBS)
UILIB = -lm3ui -lm3X11R4 $(XLIB)
FORMSVBTLIB = -lm3formsvbt -lm3vbtkit $(UILIB)
ZEUSLIB = -lm3mgkit -lm3mg -lm3zeus -lm3codeview $(FORMSVBTLIB)





# 42 "/proj/m3/lib.mips/toplevel.tmpl.SRC"


all:: .pkgs

.pkgs:: $(M3MAKEFILE) ;\
  @echo "building import links..." ;\
  rm -rf .pkgs ;\
  mkdir .pkgs ;\
  pkgnames="$(PACKAGES)" ;\
  for PKG in $$pkgnames; do (\
     echo "   " $$PKG ;\
     $(LIB_USE)/m3mkpath .pkgs/$$PKG ;\
     rm -f .pkgs/$$PKG ;\
     ln -s  /proj/m3/pkg/$$PKG .pkgs/$$PKG \
  ); done





# 62 "/proj/m3/lib.mips/toplevel.tmpl.SRC"





# 67 "/proj/m3/lib.mips/toplevel.tmpl.SRC"




# 71 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 74 "/proj/m3/lib.mips/toplevel.tmpl.SRC"




# 78 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 81 "/proj/m3/lib.mips/toplevel.tmpl.SRC"




# 85 "/proj/m3/lib.mips/toplevel.tmpl.SRC"




# 89 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 92 "/proj/m3/lib.mips/toplevel.tmpl.SRC"




# 96 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 99 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 102 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 105 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 108 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 111 "/proj/m3/lib.mips/toplevel.tmpl.SRC"




TANGLEFLAGS = -L'<*LINE %L "%F" *>%N'
WEAVEFLAGS = -n





# 122 "/proj/m3/lib.mips/toplevel.tmpl.SRC"







# 129 "/proj/m3/lib.mips/toplevel.tmpl.SRC"




# 133 "/proj/m3/lib.mips/toplevel.tmpl.SRC"




























# 161 "/proj/m3/lib.mips/toplevel.tmpl.SRC"









# 170 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 173 "/proj/m3/lib.mips/toplevel.tmpl.SRC"









# 182 "/proj/m3/lib.mips/toplevel.tmpl.SRC"







# 189 "/proj/m3/lib.mips/toplevel.tmpl.SRC"





# 194 "/proj/m3/lib.mips/toplevel.tmpl.SRC"




# 198 "/proj/m3/lib.mips/toplevel.tmpl.SRC"




# 202 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 205 "/proj/m3/lib.mips/toplevel.tmpl.SRC"











# 216 "/proj/m3/lib.mips/toplevel.tmpl.SRC"







# 223 "/proj/m3/lib.mips/toplevel.tmpl.SRC"




# 227 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 230 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 233 "/proj/m3/lib.mips/toplevel.tmpl.SRC"







# 240 "/proj/m3/lib.mips/toplevel.tmpl.SRC"


# 248 "/proj/m3/lib.mips/toplevel.tmpl.SRC"

# 253 "/proj/m3/lib.mips/toplevel.tmpl.SRC"










# 263 "/proj/m3/lib.mips/toplevel.tmpl.SRC"

  

# 266 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 269 "/proj/m3/lib.mips/toplevel.tmpl.SRC"






# 275 "/proj/m3/lib.mips/toplevel.tmpl.SRC"








# 283 "/proj/m3/lib.mips/toplevel.tmpl.SRC"




# 287 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 290 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 293 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 296 "/proj/m3/lib.mips/toplevel.tmpl.SRC"



# 299 "/proj/m3/lib.mips/toplevel.tmpl.SRC"





# 304 "/proj/m3/lib.mips/toplevel.tmpl.SRC"






# 310 "/proj/m3/lib.mips/toplevel.tmpl.SRC"





# 1 "./.makefile.0"
% Copyright (C) 1990, Digital Equipment Corporation.
% All rights reserved.
% See the file COPYRIGHT for a full description.
% Last modified on Wed Jun 16 15:34:15 PDT 1993 by kalsow
%      modified on Tue Mar 12 22:05:06 1991 by muller

M3       = "/proj/m3/pkg/driver2-ip/DSC/m3.DS"
PASS_0   = "-Y0@/proj/m3/pkg/compiler2-ip/DSC/m3c@"
PASS_6   = "-Y6@/proj/m3/pkg/m3cc/src/m3cc@"
LIBM3    = "/proj/m3/pkg/libm32-ip"

M3OPT    = { PASS_0, PASS_6, "-g", "-w1", "-D../..", "-keep" }
M3LIBS   = { "-T" & LIBM3 & "/DS/.M3IMPTAB", LIBM3 & "/DS/libm3.a" }


%------------------------------------------------------------------------
% build the test 
                                                       @@all:: lib##.a                                                           @@clean:: ; rm -f lib##.a lib##.ax                                    @@lib##.a: FRC ; $(DO_M3) -a lib##.a $(PGM_SOURCES) $(IMPORT_LIBS)
if stale ("libtest.a", "m3makefile")
  or stale ("libtest.a", "../Test.i3")
  or stale ("libtest.a", "../Test.m3")
  exec ([M3, M3OPT, "-make", "-why", "-a", "libtest.a",
         "../Test.i3", "../Test.m3", M3LIBS])
end

%------------------------------------------------------------------------
% build a shell script to run a test
%
if stale ("DO_TEST", "m3makefile")
  local m3_cmd = M3
  foreach o in M3OPT
    m3_cmd = m3_cmd & " " & o
  end
  m3_cmd = m3_cmd & " -o pgm ../$1 ../libtest.a"
  foreach l in M3LIBS
    m3_cmd = m3_cmd & " " & l
  end
  > "DO_TEST" in
    echo ("#! /bin/sh")
    echo ("")
    echo ("rm -rf $2")
    echo ("mkdir $2")
    echo ("cd $2")
    echo ("ln -s ../$1/Main.m3")
    echo (m3_cmd & " > stdout 2> stderr")
    echo ("pgm >> stdout 2>> stderr")
    echo ("diff stdout ../$1/stdout")
    echo ("diff stderr ../$1/stderr")
    % echo ("rm -f pgm core")
  end
  exec (["-chmod", "+x", "DO_TEST"])
end

%------------------------------------------------------------------------
% run a test case
%
proc testdir (dir, comment) is
  local ddir = "_" & dir
  if defined (@ddir) or defined (_all)
    echo ("--- " & dir & " --- " & comment)
    exec (["-DO_TEST", dir, dir & "_X"])
  end
end

testdir ("p001", "Hello world")
testdir ("p002", "Text")
testdir ("p003", "Fmt")
testdir ("p004", "exception mechanism")
testdir ("p005", "a simple thread program")
testdir ("p006", "a bit more complicated")
testdir ("p007", "a whole bunch of threads - does the memory grow ?")
testdir ("p008", "thread alerts")
testdir ("p009", "ORD VAL NUMBER FIRST LAST")
testdir ("p010", "TYPECODE NARROW")
testdir ("p011", "BITSIZE BYTESIZE ADRSIZE")
testdir ("p014", "variables in nested blocks and procedures")
testdir ("p015", "simple procedure with integer argument")
testdir ("p016", "CASE statements")
testdir ("p017", "FOR and EXIT statements")
testdir ("p018", "IF statements")
testdir ("p019", "REPEAT and EXIT statements")
testdir ("p020", "TRY FINALLY and RETURN statements")
testdir ("p021", "Imbricated TRY FINALLY statements")
testdir ("p022", "TYPECASE statements")
testdir ("p023", "WHILE and EXIT statements")
testdir ("p024", "WITH statements")
testdir ("p025", "assignment of INTEGER subranges")
testdir ("p026", "user and language specified variable initialization")
testdir ("p027", "RECORD types variables and assignments")
testdir ("p028", "fixed ARRAY types variables assignments and subscripting")
testdir ("p029", "nested procedures with up-level variable references")
testdir ("p030", "non-opaque OBJECTs")
testdir ("p034", "equality of open arrays and records")
testdir ("p035", "equality of procedures")
testdir ("p036", "fibonacci")
testdir ("p037", "tests Time.LongPause")
testdir ("p038", "sizes of two-byte signed integer subranges")
testdir ("p039", "VERY LONG thread test - commented out")
testdir ("p040", "binary <-> ASCII conversion routines")
testdir ("p041", "floating point comparison and SIGN function")
testdir ("p042", "floating point ABS, MAX, divide")
testdir ("p043", "List.Sort, NARROW, and NEW")
testdir ("p044", "MIN, MAX, WITH and record constructors")
testdir ("p045", "nested procedures")
testdir ("p046", "Word.Insert, array of [0..255]")
testdir ("p048", "nested procedures")
testdir ("p049", "procedure parameters")
testdir ("p050", "open array parameters")
testdir ("p051", "external variables")
testdir ("p052", "indexing of ref open array")
testdir ("p053", "set operations")
testdir ("p054", "Richards' simulation benchmark")
testdir ("p055", "recursive fibonacci")
testdir ("p056", "subarray assignment")
testdir ("p057", "open array parameter")
testdir ("p058", "subarray")
testdir ("p059", "cc optimizer bug")
testdir ("p060", "opaque types")
testdir ("p061", "object types & NEW")
testdir ("p062", "typecase and List")
testdir ("p063", "structural equivalence")
testdir ("p064", "procedure parameters")
testdir ("p065", "typecode")
testdir ("p066", "exception handler stack screwed up")
testdir ("p067", "mixup in imported names")
% testdir ("p068", "PrintProc for arrays and array of chars and Text.T")
testdir ("p069", "two object types that differs only by default overwriting")
testdir ("p070", "procedure registration")
testdir ("p071", "SUBARRAY := SUBARRAY")
testdir ("p072", "CHAR literals > 127")
testdir ("p073", "MOD and DIV - new fast versions")
testdir ("p074", "b2tests/b004 - initialization of REF RECORD")
testdir ("p075", "casting open/fixed array to/from open/fixed array")
testdir ("p076", "b3tests/b005 - more array problems")
testdir ("p077", "BITS 2 FOR [-1..1]")
testdir ("p078", "use of constant open arrays in constants")
testdir ("p079", "up-level reference of a FOR variable")
testdir ("p080", "order of initialization between interfaces")
testdir ("p081", "open array constants")
testdir ("p082", "operations constant set expressions")
testdir ("p083", "mixing of exceptions in version stamps")
testdir ("p084", "method overrides and NARROW")
testdir ("p085", "various combinations of FATAL pragma")
testdir ("p086", "constant folding of FIRST (open array)")
testdir ("p087", "set constructors with variable sized ranges")
testdir ("p088", "importing an interface under different names")
testdir ("p089", "revelations of renamed types")
testdir ("p090", "REAL parameter passing - MIPS cc bug")
testdir ("p091", "NULL <: PROCEDURE in array constructor")
testdir ("p092", "array of real parameters")
testdir ("p093", "Rd.GetLine (long line)")
testdir ("p094", "array constants and forward references")
testdir ("p095", "thread.signal/wait exercise")
testdir ("p096", "procedure/method constants")
testdir ("p097", "NEW with method overrides")
testdir ("p098", "procedure compatibility within an array constructor")
testdir ("p099", "array constructors and procedure parameters")
testdir ("p100", "unnamed types within LOOPHOLE")
testdir ("p101", "nested procedure in a module's main body")
testdir ("p102", "C reserved words in record constructors and NEW")
testdir ("p103", "runtime test of TextRd")
testdir ("p104", "obsolete pragma")
testdir ("p105", "FROM-IMPORT vs. revelations")
testdir ("p106", "ambiguous desugaring from manual")
testdir ("p107", "interface versus implementation names for keyword binding")
testdir ("p108", "array of procedure constants")
testdir ("p109", "FIRST/LAST of enumerated types in record constants")
testdir ("p110", "FIRST/LAST of enumerated types")
testdir ("p111", "C reserved words in call to NEW")
testdir ("p112", "local revelation that a type is an object")
testdir ("p113", "FIRST/LAST of REAL")
testdir ("p114", "array expressions")
testdir ("p115", "reuse of open array conversion within an expression")
testdir ("p116", "IEEE floating point tests from Xerox PARC")
testdir ("p117", "SUBARRAY (LOOPHOLE)")
testdir ("p118", "LAST (REAL)")
testdir ("p119", "small exception arguments (big-endian problem)")
testdir ("p120", "small exception arguments (big-endian problem)")
testdir ("p121", "real parameters vs. the C compiler")
testdir ("p122", "RETURN from within TRY-FINALLY")
testdir ("p123", "simple arithmetic expresssions")
testdir ("p124", "negative DIV of subrange")
testdir ("p125", "MOD of subrange")
testdir ("p126", "REAL arithmetic")
testdir ("p127", "TRUNC, ROUND, FLOOR, CEILING")
testdir ("p128", "simple FLOOR test")
testdir ("p129", "simple DIV test")
% testdir ("p130", "signed/unsigned conversions")
testdir ("p131", "TRUNC, ROUND, FLOOR, CEILING again")
testdir ("p132", "assignments of builtin types")
testdir ("p133", "assignments of records and arrays")
testdir ("p134", "BITS FOR tests") % -- current compiler chokes
testdir ("p135", "more BITS FOR tests")
testdir ("p136", "bit operations")
testdir ("p137", "bit insert and extract")
testdir ("p138", "bit field assignments")
testdir ("p139", "memory copy")
testdir ("p140", "exceptions")
testdir ("p141", "RTMisc.Exit")
testdir ("p142", "procedure defaults")
testdir ("p143", "variable initialization")
testdir ("p144", "imported variables and order of evaluation")
testdir ("p145", "pass-thru REF types")
testdir ("p146", "NUMBER")
testdir ("p148", "simple procedure calls")
testdir ("p149", "up-level addressing")
testdir ("p150", "up-level addressing with INLINEs")
testdir ("p151", "by-value open array parameters")
% testdir ("p152", "simple SCANF tests")
% testdir ("p153", "more elaborate SCANF tests")
% testdir ("p154", "simple SCANF tests on RdV")
testdir ("p155", "operations on small sets")
testdir ("p156", "operations on medium-sized sets")
testdir ("p157", "operations on big sets in the heap")
testdir ("p158", "operations on small packed sets")
% testdir ("p159", "operations on very big sets in the heap")
testdir ("p160", "more operations on very big sets in the heap")
% testdir ("p161", "more operations on very big sets in the heap")
testdir ("p162", "more operations on very big sets in the heap")
testdir ("p163", "simple statements")
testdir ("p164", "simple thread tests")
testdir ("p165", "simple TYPECODE test")
testdir ("p166", "RTMisc.Zero test")
testdir ("p167", "array constructors as parameters")
testdir ("p168", "record constructor with loopholed argument")
testdir ("p169", "runtime fingerprint")
testdir ("p170", "simple BITS FOR test")
testdir ("p171", "array constructor as record default")
testdir ("p172", "REAL vs. C's float")
testdir ("p173", "LONGREAL vs. C's double")
testdir ("p174", "large enumeration")
testdir ("p175", "CASE stmt with very large bounds")
testdir ("p176", "need full assignment in NEW and constructors")
testdir ("p177", "unaligned object field references")
testdir ("p178", "packed fields vs. C's unsigned arithmetic")
testdir ("p179", "alignment of ARRAY OF BITS 32 FOR INTEGER")
testdir ("p180", "simple generic test")
testdir ("p181", "<*NOWARN*>")
testdir ("p182", "renamed hello world")
testdir ("p183", "hidden object field")
testdir ("p184", "fold constant to check type equality")
testdir ("p185", "REAL vs. C float")
testdir ("p186", "case statement with large labels")
testdir ("p187", "array assignment when index type changes")
testdir ("p188", "initialized globals")
testdir ("p189", "module initialization order")
testdir ("p190", "order of evaluation for messy method calls")
testdir ("p191", "array assignment generates bad C  !!BUG!!")
testdir ("p192", "recursive declarations")
testdir ("p193", "implicit narrow on '&' operands")
testdir ("p194", "open arrays in fixed array initializers !!BUG!!")
testdir ("p195", "messy method calls")
testdir ("p196", "procedure valued defaults")
testdir ("p197", "appending pickles to files")
testdir ("p198", "compiler hash function")
testdir ("p199", "exporting an obsolete function")
testdir ("p200", "recursive types again")
testdir ("p201", "recursive values")
testdir ("p202", "SUBARRAY actual parameter")

# 315 "/proj/m3/lib.mips/toplevel.tmpl.SRC"




scratch:: clean 

tidy::
	find . \( -name ",*" -o -name ".,*" -o -name ".emacs_[0-9]*" \
	    -o -name "*~" -o -name core -o -name a.out \) -print | xargs rm -f

clean:: tidy
	find . \( -name "*.o" -o -name "*.io" -o -name "*.ix" \             @@\
		  -o -name "*.mo" -o -name "*.mx" \) -print | xargs rm -f
	rm -f .makefile .m3path* .PGM_SOURCES .EXPORT* .RSRC_CMD
	rm -f .m3imports*




checkin::     ; vmake -noeval ; vmake -ci < /dev/null
checkout::    ; vmake -co $(PACKAGE) < /dev/null
vestacreate:: ; vmake -co $(PACKAGE).1


FRC:
# 4 ""

