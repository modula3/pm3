(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3Args;

(* This package provides argument decoding and help support for tools
that operate in either a Unix style command line environment, or in
a window based environment.  In particular, several tools can coexist
in a single program. 

*)


TYPE
  T <: REFANY;  (* tool handle *)

  Opt = {Optional, Required, Positional};

PROCEDURE New(
    toolName, toolDescription, version: TEXT;
    master := FALSE;
    ): T RAISES {};
(* Register a new tool, returning a handle for later use *)

(* "toolName" is a short name for the tool, for example, "m3c".  It
will used to prefix any messages generated by other calls.
"toolDescription" is a longer explanation that is output when help is
requested, e.g. "Modula-3 Compiler Front End". "version" is the
version of the tool.  All of these arguments are required to be
non-empty "TEXTs".  "master" indicates whether this is the master (or
controlling) tool.  The master tool information will always be
displayed first in the help output. It is a checked run-time error
if a tool named "toolName" has already been created. *)

PROCEDURE SetMaster(t: T): T RAISES {};
(* Set "t" to be the master tool, returning the previous master,
  if any, else NIL. *)

(* The following three procedures register keyword arguments with a
tool.  The values associated with a keyword take one of three types: a
flag (boolean), a single string or a a sequence of strings. "usage" is
displayed in response to a request for help. If "opt = Required", it
is an error for the argument to be unset, and this will cause "Find"
to return "FALSE".  Flag keywords are always optional; their omission
is equivalent to a value of "FALSE". The "Positional" setting, which
also imples "Optional", is only relevant to a command line
implementation, in which case, this and only this argument can be
supplies without the keyword.  Keywords are assigned <I>short forms</I>
by concatenating all the upper-case letters in "argName" from left to
right. So the short form of "PrintUnit" would be "pu". Numerics are
not included in short-forms, so the the short-form of "LM3Check" would
be "lmc". For any tool, all it's keyword names and their short forms
must be unique. In addition, an implementation, for example based on a
commmand line, may require that the keywords be unique across all the
registered tools. A keyword may be denoted as <I>shared</I> by setting
"shared = TRUE", in which case the same value will be propagated to
all tools. If duplicates are not allowed and "shared = FALSE", a
duplicate will cause a checked runtime error will occur. Prefix
keywords are somewhat like flags; any keyword that begins with
"argName" will match but, since there may be several such matches, the
"Get"/"Set" procedures return/takes text arrays like
"RegisterStringList".  Prefix arguments support the "-Idir" idiom
beloved by some tools.

Clients of this interface can update the keyword argument values
directly by the calls "SetFlag", "SetString", "SetStringList" and
"SetPrefix".  In addition from time to time values may be transferred
from the "outside", for example a dialog box, by a daemon. The "Find"
call is guaranteed to force the transfer, so it is required that each
tool call this procedure before accessing values. All calls are atomic
with respect to the daemon. *)

PROCEDURE RegisterFlag(
    t: T;
    argName: TEXT;
    usage: TEXT;
    shared := FALSE);
(* Register a "Flag" keyword called "argName" *)

PROCEDURE RegisterString(
    t:T;
    argName: TEXT;
    usage: TEXT;
    opt: Opt := Opt.Optional;
    shared := FALSE);
(* Register a "String" keyword called "argName" *)

PROCEDURE RegisterStringList(
    t:T;
    argName: TEXT;
    usage: TEXT;
    opt: Opt := Opt.Optional;
    shared := FALSE);
(* Register a "StringList" keyword called "argName" *)

PROCEDURE RegisterPrefix(
    t:T;
    argName: TEXT;
    usage: TEXT;
    opt: Opt := Opt.Optional;
    shared := FALSE);
(* Register a "Prefix" keyword called "argName" *)

(* <H2> Help information </H2> *)

(* The way in which help information is requested and displayed is
implementation dependent. For example, in a command line
implementation, the keyword "Help" might be reserved for this purpose.
The "CheckHelp" procedure is provided to discover if help has been
requested. If so, and "display=TRUE", then "CheckHelp" calls
"Help(t)", for all registered tools, It returns TRUE if help was
requested.  If "display=FALSE" the calls on "Help(t)" are suppressed,
which enables a master tool to subvert the normal behaviour. *)

PROCEDURE Help(t: T; preamble := TRUE) RAISES {};
(* Give help on tool "t", showing the "usage" text passed to
registration procs.  If "preamble = TRUE", general information on
keyword types and short forms is output first. *)

PROCEDURE CheckHelp(display := TRUE): BOOLEAN RAISES {};
(* Check for help requested and if "display=TRUE", call "Help(t)"
for all "t" in the set of registered tools. *)

PROCEDURE HelpPreamble(t: T) RAISES {};
(* Display the general information on keyword types and short forms. *)

(* <H2> Getting and Setting Argument Values </H2> *)

(* Unbound arguments return "NIL" for "String" and "StringList"
keywords.  The empty string list is denoted by a result "r" where
NUMBER(r) = 0.  It is a checked runtime error to provide an argument
name that has not been registered. *)

PROCEDURE Find(t: T): BOOLEAN RAISES {};
(* Gather the arguments for tool "t". This ensures that arguments from
the "outside" are transferred into the tool's maps. The result will be
"TRUE" unless the arguments are incorrect, for example, a missing
"Required" argument. *)

PROCEDURE GetFlag(t: T; argName: TEXT): BOOLEAN;
(* Return the value of "Flag" keyword "argName" in tool "t". *)

PROCEDURE GetString(t: T; argName: TEXT): TEXT;
(* Return the value of "String" keyword "argName" in tool "t". *)

PROCEDURE GetStringList(t: T; argName: TEXT): REF ARRAY OF TEXT;
(* Return the value of "StringList" keyword "argName" in tool "t". *)

PROCEDURE GetPrefix(t: T; s: TEXT): REF ARRAY OF TEXT;
(* Return the value of "Prefix" keyword "argName" in tool "t". *)

PROCEDURE SetFlag(t: T; argName: TEXT; f: BOOLEAN);
(* Set the value of "Flag" keyword "argName" in tool "t" to "f". *)

PROCEDURE SetString(t: T; argName: TEXT; val: TEXT); 
(* Set the value of "String" keyword "argName" in tool "t" to "val". *)

PROCEDURE SetStringList(t: T; argName: TEXT; sl: REF ARRAY OF TEXT); 
(* Set the value of "StringList" keyword "argName" in tool "t" to "val". *)

PROCEDURE SetPrefix(t: T; s: TEXT; sl: REF ARRAY OF TEXT); 
(* Set the value of "Prefix" keyword "argName" in tool "t" to "val". *)

PROCEDURE SetStringAsList(t: T; argName: TEXT; val: TEXT);
(* Takes a comma or space separated string "val", breaks it into
sub-strings at the boundaries defined by the comma or space characters
and sets the value of "StringList" keyword "argName" in tool "t" to
rge resulting list of sub-strings. *)

(* <H2> Example </H2> *)

(* In the initialisation code for the tool, place a call to "New", and
record the tool handle.  For example:

| MODULE MyTool;
| ..
| BEGIN
|   tool_g := M3Args.New("mytool", "My Wonderful Tool", "1.1");
|   (* now register argument keywords, e.g. *)
|   M3Args.RegisterFlag(tool_g, "PrintUnits", 
|        "print name of units as they are compiled");
| END MyTool.

In the action routines of the tool, call "Find" to gather arguments
and then "GetFlag, GetString, GetStringList" as appropriate.

In a main program which includes "MyTool", always start with a call
to "CheckHelp" to ensure that a simple call by the user for help works,
and then go about your business as usual. *)

END M3Args.
