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
 * Author          : Tobias Hoellerer (htobias)
 * Created On      : Fri Nov 10 17:37:04 EST 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sat Aug  9 13:53:55 1997
 * Update Count    : 8
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  2003/04/08 22:00:52  hosking
 * Merge of PM3 with Persistent M3 and CM3 release 5.1.8
 *
 * Revision 1.1.3.1  2003/01/26 00:35:47  hosking
 * Import of CM3 5.1.8
 *
 * Revision 1.2  2001/12/03 17:23:37  wagner
 * add copyright notes and overrides
 *
 * added: sharedobjgen/COPYRIGHT
 * added: sharedobjgen/COPYRIGHT-COLUMBIA
 * added: sharedobjgen/src/COPYRIGHT-COLUMBIA
 * added: sharedobjgen/src/m3overrides
 * modified: sharedobjgen/src/SOxCodeFiles.i3
 * modified: sharedobjgen/src/SOxCodeFiles.m3
 * modified: sharedobjgen/src/SOxCodeGenError.i3
 * modified: sharedobjgen/src/SOxCodeGenError.m3
 * modified: sharedobjgen/src/SOxCodeUtils.i3
 * modified: sharedobjgen/src/SOxCodeUtils.m3
 * modified: sharedobjgen/src/SOxCoder.i3
 * modified: sharedobjgen/src/SOxDummyCode.i3
 * modified: sharedobjgen/src/SOxDummyCode.m3
 * modified: sharedobjgen/src/SOxIntfCBCode.i3
 * modified: sharedobjgen/src/SOxIntfCBCode.m3
 * modified: sharedobjgen/src/SOxIntfCBProxyCode.i3
 * modified: sharedobjgen/src/SOxIntfCBProxyCode.m3
 * modified: sharedobjgen/src/SOxIntfPklCode.i3
 * modified: sharedobjgen/src/SOxIntfPklCode.m3
 * modified: sharedobjgen/src/SOxIntfProxyCode.i3
 * modified: sharedobjgen/src/SOxIntfProxyCode.m3
 * modified: sharedobjgen/src/SOxModuleCBCode.i3
 * modified: sharedobjgen/src/SOxModuleCBCode.m3
 * modified: sharedobjgen/src/SOxModuleProxyCode.i3
 * modified: sharedobjgen/src/SOxModuleProxyCode.m3
 * modified: sharedobjgen/src/SOxModuleSOCode.i3
 * modified: sharedobjgen/src/SOxModuleSOCode.m3
 * modified: sharedobjgen/src/StubGenTool.i3
 * modified: sharedobjgen/src/StubGenTool.m3
 *
 * Revision 1.1.1.1  2001/12/02 13:15:54  wagner
 * Blair MacIntyre's sharedobjgen package
 *
 * Revision 1.3  1997/08/11 20:36:37  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

INTERFACE SOxModuleCBCode;

IMPORT SOxCoder;

TYPE T <: SOxCoder.T;

END SOxModuleCBCode.
