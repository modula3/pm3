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
 * Created On      : Sun Jul 30 14:59:48 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Wed Oct 23 10:13:17 1996
 * Update Count    : 8
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  1998/02/26 16:37:33  dagenais
 * Enhanced pickler which allows communicating pickles between machines
 * with different endianess.
 *
 * 
 * HISTORY
 *)

INTERFACE PackingTypeCode;

IMPORT Word;

TYPE T = RECORD from, to, tc: Word.T END;

CONST Brand = "Packing Typecode";

PROCEDURE Equal(a, b: T): BOOLEAN;
(* Return "a = b". *)

PROCEDURE Hash(a: T): Word.T;
(* Return "a". *)

PROCEDURE Compare(a, b: T): [-1..1];
(* Return "-1" if "a < b", "0" if "a = b", or "+1" if "a > b". *)

END PackingTypeCode.
