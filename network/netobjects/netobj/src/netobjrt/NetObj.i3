(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* NetObj.i3 *)
(* Last modified on Thu Feb 24 17:11:41 PST 1994 by wobber     *)
(*      modified on Mon Dec  7 18:34:14 1992 by gnelson    *)
(*      modified on Thu Jul 30 18:51:00 PDT 1992 by evers  *)
(*      modified on Mon Jun 29  8:45:58 PDT 1992 by owicki *)

(* A <I>network object</I> is an object whose methods can be invoked by
   other programs, in addition to the program that allocated the object.
   The program invoking the method is called the <I>client</I> and the
   program containing the network object is called the <I>owner</I>.  The
   client and owner can be running on different machines or in different
   address spaces on the same machine.
<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>network object</SPAN>
</SPAN>

<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>network object</SPAN>
<SPAN CLASS=INDEX.KEY>client</SPAN>
</SPAN>

<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>network object</SPAN>
<SPAN CLASS=INDEX.KEY>owner</SPAN>
</SPAN>
*)

(* This is the primary public interface for using network objects.
   Before listing the interface, here are a few definitions.

   A <I>program instance</I> is an activation of a program.
   The same program can have many instances running concurrently
   or consecutively.  A program instance can be thought of 
   as an address space, although the design does not 
   preclude the implementation of a program instance by a
   suite of address spaces.<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>program instance</SPAN>
</SPAN>


   An <I>agent</I><SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>agent</SPAN>
</SPAN>
 is a program that provides a table that
   maps names to network objects.  Any program can be an agent, but every
   machine has a particular default agent.  Owners typically make 
   network objects available to clients by inserting them 
   into an agent's table, using the procedure "NetObj.Export".  
   Clients typically use "NetObj.Import" to retrieve network objects 
   from the table.
*)

INTERFACE NetObj;

IMPORT Atom, AtomList, Thread;

TYPE
  T <: ROOT;
  Address <: REFANY;

(* "NetObj.T" is the root type of all network objects.
   A "NetObj.Address" designates a program instance.
<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>NetObj.T</SPAN>
<SPAN CLASS=INDEX.TEXT><TT>NetObj.T</TT></SPAN>
</SPAN>
<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>NetObj.Address</SPAN>
<SPAN CLASS=INDEX.TEXT><TT>NetObj.Address</TT></SPAN>
</SPAN>
*)

PROCEDURE Locate (host: TEXT): Address
    RAISES {Invalid, Error, Thread.Alerted};
(* Return an address for the standard agent at the machine whose
   human-sensible name is "host".
<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>NetObj.Locate</SPAN>
<SPAN CLASS=INDEX.TEXT><TT>NetObj.Locate</TT></SPAN>
</SPAN>
*)

(* The naming convention used by "Locate" is system-dependent.  For 
   example, in an Internet environment, 

| Locate("decsrc.pa.dec.com") 

   returns the address of the default agent on the machine "decsrc" in 
   the DEC Palo Alto Internet domain.

   "Locate" raises "Invalid" if it determines that "host" is not a 
   valid name.  It raises "Error" if it is unable to interpret the 
   name or determine its validity, typically because it is unable 
   to contact the naming authority, or if there is no standard
   agent running on the specified host.  *)

PROCEDURE Export(
    name: TEXT; obj: T; where: Address := NIL)
    RAISES {Error, Thread.Alerted};
(* Set "table[name] := obj" where "table" is the table provided by the
   agent whose address is "where", or by the default agent for the local
   machine if "where = NIL".  This can be used with "obj=NIL" to
   remove an entry from the table.
<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>NetObj.Export</SPAN>
<SPAN CLASS=INDEX.TEXT><TT>NetObj.Export</TT></SPAN>
</SPAN>
*)

PROCEDURE Import(name: TEXT; where: Address := NIL): T
    RAISES {Error, Thread.Alerted};
(* Return "table[name]" where "table" is the table provided by the
   agent whose address is "where", or by the default agent for the local
   machine if "where = NIL".  "Import" returns "NIL" if "table"
   contains no entry for "name".
<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>NetObj.Import</SPAN>
<SPAN CLASS=INDEX.TEXT><TT>NetObj.Import</TT></SPAN>
</SPAN>
*)
    
EXCEPTION
  Error(AtomList.T);  
  Invalid;

VAR (*CONST*)  
  CommFailure, MissingObject,
    NoResources, NoTransport,
    UnsupportedDataRep, Alerted: Atom.T;

END NetObj.

(*
<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>NetObj.Error</SPAN>
<SPAN CLASS=INDEX.TEXT><TT>NetObj.Error</TT></SPAN>
</SPAN>
   The exception "NetObj.Error" indicates possible failures in a remote 
   method invocation.  Every remote method should therefore include
   "NetObj.Error" in its raises clause.  If "NetObj.Error" is not raised,
   then the invocation completed successfully.  If it is raised, it may
   or may not have completed successfully.  It is possible that an
   <I>orphaned</I> remote invocation continued to execute at the owner,
   while the client raised "NetObj.Error".<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>orphan computation</SPAN>
</SPAN>


   The first atom in the argument to "NetObj.Error" explains the
   reason for the failure.  The subsequent atoms may provide additional
   implementation-dependent details about the failure.  Here are the
   interpretations of the first atom:

   The atom "CommFailure" indicates communication failure, which might be 
   network failure or a crash on a remote machine.  "MissingObject" 
   indicates that some network object, either the one whose method 
   is invoked or an argument to that method, has been garbage-collected 
   by its owner.  (This indicates that the owner mistakenly determined 
   that one of its clients was dead.)  "NoResources" indicates that 
   the call failed because of a lack of resources, for example Unix 
   file descriptors.  "NoTransport" indicates that an attempt to 
   unmarshal an object failed because the client and owner shared no
   common transport protocol implementation and were therefore unable
   to communicate.  "UnsupportedDataRep" indicates a mismatch 
   between the network representation of data and the ability of a 
   receiver to handle it, for example a 64-bit "INTEGER" with non-zero 
   high-order bits is not meaningful as an "INTEGER" on a 32-bit 
   machine. "Alerted" indicates that a client thread was alerted in 
   the middle of a remote call and that an orphaned remote 
   computation might still be in progress.  (Threads alerted in remote 
   calls might also raise "Thread.Alerted"; in which case 
   it is guaranteed that no orphans remain.)  Any other atoms indicate 
   errors inside the network object runtime. 
*)
