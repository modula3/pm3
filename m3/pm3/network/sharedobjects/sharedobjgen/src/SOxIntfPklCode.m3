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
 * Created On      : Tue Feb 25 16:15:24 1997
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Tue Feb 25 16:56:46 1997
 * Update Count    : 3
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
 * Revision 1.1.3.1  2003/01/26 00:35:46  hosking
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
 * Revision 1.1  1997/05/25 20:27:50  bm
 * Pickling code for SharedObjects
 *
 * 
 * HISTORY
 *)

MODULE SOxIntfPklCode;

IMPORT SOxCoder, Atom, Formatter, SOxCodeUtils, SOxCodeFiles, Type,
       Wr, ImportList, CodeForType, AtomList;

REVEAL T = SOxCoder.T BRANDED OBJECT 
OVERRIDES
  InitImports := initImports;
  Import := import;
  Head := head;
  Decls := decls; 
  Main := noMain;
  Bottom := bottom;
END;

<* FATAL Wr.Failure*>

PROCEDURE PutLine (fmtWr: Formatter.T; text: TEXT) =
  BEGIN
    Formatter.PutText(fmtWr, text);
    Formatter.NewLine(fmtWr, freshLine := FALSE);
  END PutLine;

VAR
  extraImports := ARRAY [1..1] OF Atom.T{Atom.FromText("SharedObj")};

PROCEDURE initImports(<*UNUSED*>self: T;
                      <*UNUSED*>basename: TEXT; 
                      imports: ImportList.T) =
  BEGIN
    CodeForType.AugmentImportList(imports, extraImports);
  END initImports;

PROCEDURE import(<*UNUSED*>self: T;
                 <*UNUSED*>type: Type.Object;  
                 <*UNUSED*>methods: ImportList.MethodList;
                 <*UNUSED*>umethods: AtomList.T;
                 <*UNUSED*>imports: ImportList.T) =
  BEGIN
  END import;

PROCEDURE head(self: T;
               wr: Formatter.T;
               filename: TEXT;  
               basename: TEXT; 
               imports: ImportList.T) =
  BEGIN
    WITH Nl     = Formatter.NewLine DO
      SOxCodeUtils.HeaderComment(wr, filename);
      self.fbasename := SOxCodeUtils.FileName(basename, SOxCodeFiles.T.PKL_I3);

      PutLine(wr, "INTERFACE " & self.fbasename & ";\n");
      CodeForType.ProduceImports(wr, imports);

      Nl(wr, freshLine := FALSE); 
    END;
  END head;

PROCEDURE decls(<*UNUSED*>self: T;
                wr: Formatter.T;
                typeID: Type.Qid;  
                <*UNUSED*>stypeID: Type.Qid;  
                <*UNUSED*>implName: TEXT; 
                <*UNUSED*>methods: ImportList.MethodList;
                <*UNUSED*>umethods: AtomList.T) =
  VAR typTxt : TEXT;
      identfTxt : TEXT;
  BEGIN
    WITH Put    = Formatter.PutText,
         Nl     = Formatter.NewLine,
         Tab    = Formatter.Begin,
         EndTab = Formatter.End     DO
      typTxt := CodeForType.QidToText(typeID);
      identfTxt := CodeForType.QidToIdentf(typeID);

      Tab(wr, 2);
      Put(wr, "TYPE"); 

      EndTab(wr);
      Nl(wr, freshLine := FALSE); 
      PutLine(wr, identfTxt & "Special <: SharedObj.Special;");

      PutLine(wr, "PROCEDURE RegisterSpecial_" & identfTxt & "(sp: " &
        identfTxt & "Special);");
    END
 END decls;
 
PROCEDURE noMain(<*UNUSED*> self: T;
                 <*UNUSED*> wr: Formatter.T;
                 <*UNUSED*> typeID: Type.Qid;  
                 <*UNUSED*> type    : Type.Object;  
                 <*UNUSED*> stypeID: Type.Qid;  
                 <*UNUSED*> implName: TEXT; 
                 <*UNUSED*> methods: ImportList.MethodList;
                 <*UNUSED*> umethods: AtomList.T) = 
BEGIN
END noMain;

PROCEDURE bottom(self: T;
                 wr: Formatter.T; 
                 <*UNUSED*>fname: TEXT) =
  BEGIN
    PutLine(wr, "END " & self.fbasename & ".");
  END bottom;


BEGIN   
END SOxIntfPklCode.

