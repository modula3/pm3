(*  SGML parser library                                                    *)
(*  Copyright (C) 1997 Michel Dagenais                                     *)
(*                                                                         *)
(*  This library is free software; you can redistribute it and/or          *)
(*  modify it under the terms of the GNU Library General Public            *)
(*  License as published by the Free Software Foundation; either           *)
(*  version 2 of the License, or (at your option) any later version.       *)
(*                                                                         *)
(*  This library is distributed in the hope that it will be useful,        *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      *)
(*  Library General Public License for more details.                       *)
(*                                                                         *)
(*  You should have received a copy of the GNU Library General Public      *)
(*  License along with this library; if not, write to the Free             *)
(*  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.     *)
(*                                                                         *)
(*  For more information on this program, contact Michel Dagenais at       *)
(*  dagenais@vlsi.polymtl.ca or Electrical Eng. Dept., Ecole Polytechnique *)
(*  P.O. Box 6079, Station A, Montreal, Quebec, Canada, H3C 3A7.           *)


MODULE SGMLPrintStructure;

IMPORT SGML, SGMLPrint, Wr, Thread;

REVEAL
  T = SGMLPrint.T BRANDED OBJECT
      depth: INTEGER;
    OVERRIDES
      init := Init;
      appInfo := AppInfo;
      startDtd := StartDtd;
      endDtd := EndDtd;
      endProlog := EndProlog;
      startElement := StartElement;
      endElement := EndElement;
      data := Data;
      sdata := SData;
      pi := Pi;
      externalDataEntityRef := ExternalDataEntityRef;
      subdocEntityRef := SubdocEntityRef;
      nonSgmlChar := NonSgmlChar;
      commentDecl := CommentDecl;
      markedSectionStart := MarkedSectionStart;
      markedSectionEnd := MarkedSectionEnd;
      ignoredChars := IgnoredChars;
      generalEntity := GeneralEntity;
      error := Error;
      openEntityChange:= OpenEntityChange;
    END;

<*FATAL Thread.Alerted*>
<*FATAL Wr.Failure*>

(* For each parsing event, print a message before and after the content
   of the event. This is useful to determine the exact content of each
   parsing event and thus the SGML tree structure. *)

PROCEDURE AppInfo(self: T; READONLY e: SGML.AppinfoEvent) =
  VAR
  BEGIN
    Enter(self,"appInfo");
    SGMLPrint.T.appInfo(self,e);
    Leave(self,"appInfo");
  END AppInfo;

PROCEDURE StartDtd(self: T; 
    READONLY e: SGML.StartDtdEvent) =
  VAR
  BEGIN
    Enter(self,"startDtd");
    SGMLPrint.T.startDtd(self,e);
    Leave(self,"startDtd");
  END StartDtd;

PROCEDURE EndDtd(self: T; READONLY e: SGML.EndDtdEvent) =
  VAR
  BEGIN
    Enter(self,"endDtd");
    SGMLPrint.T.endDtd(self,e);
    Leave(self,"endDtd");
  END EndDtd;

PROCEDURE EndProlog(self: T; READONLY e: SGML.EndPrologEvent) =
  VAR
  BEGIN
    Enter(self,"endProlog");
    SGMLPrint.T.endProlog(self,e);
    Leave(self,"endProlog");
  END EndProlog;

PROCEDURE StartElement(self: T; READONLY e: SGML.StartElementEvent) =
  VAR
  BEGIN
    Enter(self,"startElement");
    SGMLPrint.T.startElement(self,e);
    Leave(self,"startElement");
    INC(self.depth);
  END StartElement;

PROCEDURE EndElement(self: T; READONLY e: SGML.EndElementEvent) =
  BEGIN
    DEC(self.depth);
    Enter(self,"endElement");
    SGMLPrint.T.endElement(self,e);
    Leave(self,"endElement");
  END EndElement;

PROCEDURE Data(self: T; READONLY e: SGML.DataEvent) =
  BEGIN
    Enter(self,"data");
    SGMLPrint.T.data(self,e);
    Leave(self,"data");
  END Data;

PROCEDURE SData(self: T; READONLY e: SGML.SdataEvent) =
  BEGIN
    Enter(self,"sdata");
    SGMLPrint.T.sdata(self,e);
    Leave(self,"sdata");
  END SData;

PROCEDURE Pi(self: T; READONLY e: SGML.PiEvent) =
  BEGIN
    Enter(self,"pi");
    SGMLPrint.T.pi(self,e);
    Leave(self,"pi");
  END Pi;

PROCEDURE ExternalDataEntityRef(self: T; 
    READONLY e: SGML.ExternalDataEntityRefEvent) =
  BEGIN
    Enter(self,"externalDataEntityRef");
    SGMLPrint.T.externalDataEntityRef(self,e);
    Leave(self,"externalDataEntityRef");
  END ExternalDataEntityRef;

PROCEDURE SubdocEntityRef(self: T; 
    READONLY e: SGML.SubdocEntityRefEvent) =
  BEGIN
    Enter(self,"subdocEntityRef");
    SGMLPrint.T.subdocEntityRef(self,e);
    Leave(self,"subdocEntityRef");
  END SubdocEntityRef;

PROCEDURE NonSgmlChar(self: T; 
    READONLY e: SGML.NonSgmlCharEvent) =
  BEGIN
    Enter(self,"nonSgmlChar");
    SGMLPrint.T.nonSgmlChar(self,e);
    Leave(self,"nonSgmlChar");  
  END NonSgmlChar;

PROCEDURE CommentDecl(self: T; 
    READONLY e: SGML.CommentDeclEvent) =
  BEGIN
    Enter(self,"commentDecl");
    SGMLPrint.T.commentDecl(self,e);
    Leave(self,"commentDecl");
  END CommentDecl;

PROCEDURE MarkedSectionStart(self: T; 
    READONLY e: SGML.MarkedSectionStartEvent) =
  BEGIN
    Enter(self,"markedSectionStart");
    SGMLPrint.T.markedSectionStart(self,e);
    Leave(self,"markedSectionStart");
  END MarkedSectionStart;

PROCEDURE MarkedSectionEnd(self: T; READONLY e: SGML.MarkedSectionEndEvent) =
  BEGIN
    Enter(self,"markedSectionEnd");
    SGMLPrint.T.markedSectionEnd(self,e);
    Leave(self,"markedSectionEnd");
  END MarkedSectionEnd;

PROCEDURE IgnoredChars(self: T; 
    READONLY e: SGML.IgnoredCharsEvent) =
  BEGIN
    Enter(self,"ignoredChars");
    SGMLPrint.T.ignoredChars(self,e);
    Leave(self,"ignoredChars");
  END IgnoredChars;

PROCEDURE GeneralEntity(self: T; 
    READONLY e: SGML.GeneralEntityEvent) =
  VAR
  BEGIN
    Enter(self,"generalEntity");
    SGMLPrint.T.generalEntity(self,e);
    Leave(self,"generalEntity");
  END GeneralEntity;

PROCEDURE Error(self: T; READONLY e: SGML.ErrorEvent) =
  VAR
  BEGIN
    Enter(self,"error");
    SGMLPrint.T.error(self,e);
    Leave(self,"error");
  END Error;

PROCEDURE OpenEntityChange(self: T) =
  VAR
  BEGIN
    Enter(self,"openEntityChange");
    SGMLPrint.T.openEntityChange(self);
    Leave(self,"openEntityChange");
  END OpenEntityChange;

PROCEDURE Init(self: T): SGMLPrint.T =
  VAR
  BEGIN
    self.depth := 0;
    EVAL SGMLPrint.T.init(self);
    RETURN self;
  END Init;

PROCEDURE Enter(self: T; name: TEXT) =
  BEGIN
    Wr.PutText(self.wr,"\n");
    FOR i := 1 TO self.depth DO
      Wr.PutText(self.wr,"  ");
    END;
    Wr.PutText(self.wr,"BEGIN " & name & "\n");
  END Enter;

PROCEDURE Leave(self: T; name: TEXT) =
  BEGIN
    Wr.PutText(self.wr,"\n");
    FOR i := 1 TO self.depth DO
      Wr.PutText(self.wr,"  ");
    END;
    Wr.PutText(self.wr,"END " & name & "\n");
  END Leave;

BEGIN
END SGMLPrintStructure.
