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
 * Created On      : Sun Jul 23 00:27:29 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Wed Oct 23 10:16:20 1996
 * Update Count    : 24
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

INTERFACE PklAction;

CONST Brand = "Tipe Conversion Action 1.0";

TYPE 
  T = OBJECT kind: Kind; length: INTEGER END;

TYPE
  Kind = {
     (* For moving data between same size, same endian *)
     Copy,                          (* Straight copy. *)

     (* Skipping bytes on input, output or both *)
     SkipFrom,                      (* Skip input bytes. *)
     SkipTo,                        (* Skip output bytes. *)
     Skip,                          (* Skip both *)

     (* For moving packed data between different endian machines.
        Only need one, since a set of packed fields being converted
        between different endian, different size must fit in the
        smaller word size (32 bits) *)
     SwapPacked,                    (* Copy and swap around the fields *)

     (* For moving data between same size, different endian *)
     Swap16,                        (* Copy N 16 bit words, swapping. *)
     Swap32,                        (* Copy N 32 bit words, swapping. *)
     Swap64,                        (* Copy N 64 bit words, swapping. *)

     (* For moving data between different size, different endian *)
     Copy32to64,                    (* Copy N 32 bit words to 64
                                       bit words. *)
     Copy64to32,                    (* Copy N 64 bit words to 32
                                       bit words. *)
     Swap32to64,		    (* Copy N 32 bit words to 64
                                       bit words, swapping. *)
     Swap64to32,		    (* Copy N 64 bit words to 32
                                       bit words, swapping. *)
     ReadRef,
     Done
  };


  Copy32to64 = T OBJECT signed: BOOLEAN END;

  SwapPacked = T OBJECT 
    size  : CARDINAL;
    field : REF ARRAY OF CARDINAL;
  END;

  Ref = T OBJECT refType: RefType; END;
  RefType = {Ref, UntracedRef, Proc};

END PklAction.
