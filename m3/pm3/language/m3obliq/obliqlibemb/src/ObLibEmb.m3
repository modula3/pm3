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
 * Author          : Blair MacIntyre
 * Created On      : Thu Aug 10 09:32:23 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Wed Mar 12 16:36:14 1997
 * Update Count    : 27
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  2003/04/08 21:57:56  hosking
 * Merge of PM3 with Persistent M3 and CM3 release 5.1.8
 *
 * Revision 1.1.3.1  2003/01/27 00:35:57  hosking
 * Import of CM3 5.1.8
 *
 * Revision 1.1.1.1  2001/01/24 21:52:38  wagner
 * import of Critical Mass sources of release 5.1
 *
 * Revision 1.6  1997/03/12 21:39:44  bm
 * Small bug
 *
 * Revision 1.5  1997/03/12 21:34:57  bm
 * Moved sharedobj from coterie
 *
 * Revision 1.4  1996/10/12 23:42:51  bm
 * fixed header
 *
 * Revision 1.3  1996/10/12 23:41:36  bm
 * silenced debug shit.
 *
 * 
 * HISTORY
 *)

MODULE ObLibEmb;

IMPORT ObEmbProxiedObj, ObLoader, ObEmbBundle, ObSharedObj, LibEmbDirs, SynWr;

VAR setupDone := FALSE;

PROCEDURE PackageSetup (): ObLoader.T =
  BEGIN
    SetupPackages();
    RETURN SetupModules();
  END PackageSetup;

PROCEDURE SetupPackages () =
  BEGIN
    IF NOT setupDone THEN
      ObEmbProxiedObj.SetupPackage();
      ObSharedObj.SetupPackage();
      setupDone := TRUE;
    END;
  END SetupPackages;

PROCEDURE SetupModules (): ObLoader.T =
  VAR 
    (* Use the Bundle as a fallback. *)
    loader: ObLoader.T := NEW(ObLoader.BundleT).init(ObEmbBundle.Get());
  BEGIN
    (* Search the dirs first, in case we change the files during
       testing. *)
    loader:= ObLoader.NewDirs(LibEmbDirs.dirs, NIL, loader);

    SynWr.PushSilence(SynWr.out);
    ObEmbProxiedObj.SetupModule(loader);
    ObSharedObj.SetupModule(loader);
    SynWr.PopSilence(SynWr.out);
    RETURN loader;
  END SetupModules;

BEGIN
END ObLibEmb.
