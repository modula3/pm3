(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 *
 * This file is released under the same conditions as Pickle.m3. See COPYRIGHT.
 * 
 * Author          : Blair MacIntyre
 * Created On      : Fri Jul 21 21:52:00 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Jul 11 13:05:05 1997
 * Update Count    : 23
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  1998/02/26 16:37:34  dagenais
 * Enhanced pickler which allows communicating pickles between machines
 * with different endianess.
 *
 * 
 * HISTORY
 *)

UNSAFE INTERFACE PklTipeMap;

IMPORT Rd, Wr, RTPacking, Thread, ConvertPacking;

EXCEPTION Error(TEXT);

TYPE
  TypeCode = INTEGER; 

PROCEDURE Read (v: ConvertPacking.ReadVisitor; r: REFANY; tc: TypeCode; 
                from: RTPacking.T; to: RTPacking.T; 
                READONLY shape: ARRAY OF INTEGER)  
    RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted };

(* Read in "r" with type "tc" using "v".  The packing of the data in
   the file is defined by "v.from", the packing of the data in memory
   in "v.to".  "shape" is the dimmensions of the reference, if it is
   an Open Array.  Otherwise, it is ignored.  Proper conversions are
   applied.  It is assumed that "r" has been properly allocated to
   handle the incoming data.  *)

PROCEDURE Write (v: ConvertPacking.WriteVisitor; r: REFANY; tc: TypeCode; 
                from: RTPacking.T; READONLY shape: ARRAY OF INTEGER; n: INTEGER)  
    RAISES { Error, Wr.Failure, Thread.Alerted };

(* Write "r" using "v".  The data is writen in the local data format. *)

END PklTipeMap. 
