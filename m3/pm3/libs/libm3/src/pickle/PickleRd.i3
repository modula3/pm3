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
 * Created On      : Wed Jul 26 16:19:13 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Wed Oct 23 10:11:33 1996
 * Update Count    : 2
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

UNSAFE INTERFACE PickleRd;

IMPORT RTPacking, Pickle, ConvertPacking;

REVEAL
  Pickle.Reader <: Private;

TYPE
  Private = Pickle.ReaderPublic OBJECT
      packing: RTPacking.T;
      packingCode: INTEGER;
      conversion: ConvertPacking.Kind;
    END;

VAR myPacking: RTPacking.T;       (* our local packing. *)

END PickleRd.
