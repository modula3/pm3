(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * See file COPYRIGHT-COLUMBIA for details.
 * 
 * Author          : Blair MacIntyre
 * Created On      : Mon Jan 26 12:07:04 1998
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Wed Apr  8 17:40:13 1998
 * Update Count    : 4
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  2003/04/08 22:00:41  hosking
 * Merge of PM3 with Persistent M3 and CM3 release 5.1.8
 *
 * Revision 1.1.3.1  2003/01/26 01:09:52  hosking
 * Import of CM3 5.1.8
 *
 * Revision 1.2  2001/12/02 00:06:28  wagner
 * add copyright notes and fix overrides for cm3
 *
 * added: embutils/src/COPYRIGHT-COLUMBIA
 * modified: embutils/src/EmbProxiedObj.m3
 * modified: embutils/src/m3makefile
 * modified: embutils/src/m3overrides
 *
 * Revision 1.1.1.1  2001/12/01 23:53:22  wagner
 * Blair MacIntyre's embutils library
 *
 * Revision 1.1  1998/05/13 17:28:21  bm
 * new file
 *
 * 
 * HISTORY
 *)

MODULE EmbProxiedObj;

REVEAL
  Proxy = PublicProxy BRANDED "EmbProxiedObj v1.0" OBJECT OVERRIDES
    init := Init;
    changeProxy := ChangeProxy;
  END;

PROCEDURE Init (self: Proxy; obj: REFANY): Proxy =
  BEGIN
    self.obj := obj;
    RETURN self;
  END Init; 

PROCEDURE ChangeProxy (self: Proxy; obj: REFANY) =
  BEGIN
    self.obj := obj;
  END ChangeProxy;

BEGIN
END EmbProxiedObj.
