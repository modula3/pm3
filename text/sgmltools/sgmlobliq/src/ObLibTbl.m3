(*  SGML obliq interpreter                                                 *)
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

UNSAFE MODULE ObLibTbl;
IMPORT ObLib, ObValue, SynLocation, TextRefTbl, SortedTextRefTbl, IntRefTbl,
       SortedIntRefTbl, ObCommand, Text, SynWr;

TYPE 
  ValTbl = ObValue.ValAnything BRANDED OBJECT
        value: REFANY;
      OVERRIDES 
        Is := IsTbl; 
        Copy := CopyTbl;
      END;

  ValIter = ObValue.ValAnything BRANDED OBJECT
        value: REFANY;
        itemValue: REFANY;
        end: BOOLEAN;
      OVERRIDES
        Is := IsIter; 
        Copy := CopyIter;
      END;

VAR setupDone := FALSE;

PROCEDURE PackageSetup() =
BEGIN
  IF NOT setupDone THEN
    setupDone := TRUE;
    SetupTbl();
  END;
END PackageSetup;

TYPE
  TblCode = {Error, Create, Get, Put, Delete, Size, Iterate, NextItem, 
      NextValue, Seek};

  TblOpCode =  
    ObLib.OpCode OBJECT
        code: TblCode;
      END;
    
  PackageTbl = 
    ObLib.T OBJECT
      OVERRIDES
        Eval:=EvalTbl;
      END;

CONST
  HelpSummary = 
      "  tbl                (the built-in table library)\n";
  HelpText = 
      "  create(sorted: BOOLEAN; text: BOOLEAN): Tbl;\n" &
      "  get(tbl: Tbl; key: TEXT or INTEGER): REFANY;\n" &
      "  put(tbl: Tbl; key: TEXT or INTEGER; value: REFANY);\n" &
      "  delete(tbl: Tbl; key: TEXT or INTEGER): REFANY;\n" &
      "  size(tbl: Tbl): INTEGER;\n" &
      "  iterate(tbl: Tbl; up: BOOLEAN): Iter;\n" &
      "  nextitem(iter: Iter): TEXT or INTEGER;\n" &
      "  nextvalue(iter: Iter): REFANY;\n" &
      "  seek(iter: Iter; key: TEXT or INTEGER);\n";

PROCEDURE IsTbl(self: ValTbl; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF ValTbl(oth)=> RETURN self.value = oth.value;
    ELSE RETURN FALSE END;
  END IsTbl;

PROCEDURE CopyTbl(<*UNUSED*>self: ObValue.ValAnything; 
    <*UNUSED*>tbl: ObValue.Tbl;
    loc: SynLocation.T): ObValue.ValAnything RAISES {ObValue.Error} =
  BEGIN
    ObValue.RaiseError("Cannot copy tbl tbl", loc);
  END CopyTbl;

PROCEDURE IsIter(self: ValIter; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF ValIter(oth)=> RETURN self.value = oth.value;
    ELSE RETURN FALSE END;
  END IsIter;

PROCEDURE CopyIter(<*UNUSED*>self: ObValue.ValAnything; 
    <*UNUSED*>tbl: ObValue.Tbl;
    loc: SynLocation.T): ObValue.ValAnything RAISES {ObValue.Error} =
  BEGIN
    ObValue.RaiseError("Cannot copy tbl iter", loc);
  END CopyIter;

VAR tblException: ObValue.ValException;

PROCEDURE HelpTbl(self: ObCommand.T; arg: TEXT; 
      <*UNUSED*>data: REFANY:=NIL)  =
  BEGIN
    IF Text.Equal(arg, "!") THEN
      SynWr.Text(SynWr.out,HelpSummary);
    ELSIF Text.Equal(arg, "?") THEN
      SynWr.Text(SynWr.out, HelpText);
      SynWr.NewLine(SynWr.out);
    ELSE
      SynWr.Text(SynWr.out, "Command " & self.name & ": bad argument: " & arg);
      SynWr.NewLine(SynWr.out);
    END;
  END HelpTbl;

PROCEDURE NewTblOC(name: TEXT; arity: INTEGER; code: TblCode)
    : TblOpCode =
  BEGIN
    RETURN NEW(TblOpCode, name:=name, arity:=arity, code:=code);
  END NewTblOC;

PROCEDURE SetupTbl() =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(TblCode));
    opCodes^ :=
      OpCodes{
        NewTblOC("failure", -1, TblCode.Error),
        NewTblOC("create", 2, TblCode.Create),
        NewTblOC("get", 2, TblCode.Get),
        NewTblOC("put", 3, TblCode.Put),
        NewTblOC("delete", 2, TblCode.Delete),
        NewTblOC("size", 1, TblCode.Size),
        NewTblOC("iterate", 2, TblCode.Iterate),
        NewTblOC("nextitem", 1, TblCode.NextItem),
        NewTblOC("nextvalue", 1, TblCode.NextValue),
        NewTblOC("seek",1, TblCode.Seek)
      };
    ObLib.Register(
      NEW(PackageTbl, name:="tbl", opCodes:=opCodes));
    tblException := NEW(ObValue.ValException, name:="tbl_failure");
    ObLib.RegisterHelp("tbl", HelpTbl);
  END SetupTbl;

PROCEDURE EvalTbl(self: PackageTbl; opCode: ObLib.OpCode; 
    <*UNUSED*>arity: ObLib.OpArity; READONLY args: ObValue.ArgArray; 
    <*UNUSED*>temp: BOOLEAN; loc: SynLocation.T)
    : ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    text1: TEXT;
    int1: INTEGER;
    bool1, bool2: BOOLEAN;
    tbl, result: REFANY;
    iter: ValIter;
  BEGIN
    CASE NARROW(opCode, TblOpCode).code OF
    | TblCode.Error => 
        RETURN tblException;

    | TblCode.Create =>
        TYPECASE args[1] OF | ObValue.ValBool(node) => bool1:=node.bool;
        ELSE ObValue.BadArgType(1, "bool", self.name, opCode.name, loc); END;
        TYPECASE args[2] OF | ObValue.ValBool(node) => bool2:=node.bool;
        ELSE ObValue.BadArgType(2, "bool", self.name, opCode.name, loc); END;
        IF bool1 THEN
          IF bool2 THEN
            tbl := NEW(SortedTextRefTbl.Default).init();
          ELSE
            tbl := NEW(SortedIntRefTbl.Default).init();
          END;
        ELSE
          IF bool2 THEN
            tbl := NEW(TextRefTbl.Default).init();
          ELSE
            tbl := NEW(IntRefTbl.Default).init();
          END;
        END;
        RETURN NEW(ValTbl, what:="<a table>", picklable:=FALSE,
            value:= tbl);

    | TblCode.Get =>
        TYPECASE args[1] OF | ValTbl(node) => tbl:=node.value;
        ELSE ObValue.BadArgType(1, "tbl", self.name, opCode.name, loc); END;
        IF ISTYPE(tbl,TextRefTbl.T) THEN
          TYPECASE args[2] OF | ObValue.ValText(node) => text1:=node.text;
          ELSE ObValue.BadArgType(2, "text", self.name, opCode.name, loc); END;
          IF NARROW(tbl,TextRefTbl.T).get(text1,result) THEN RETURN result;
          ELSE RETURN ObValue.valOk;
          END;
        ELSE
          TYPECASE args[2] OF | ObValue.ValInt(node) => int1:=node.int;
          ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); END;
          IF NARROW(tbl,IntRefTbl.T).get(int1,result) THEN RETURN result;
          ELSE RETURN ObValue.valOk;
          END;
        END;

    | TblCode.Put =>
        TYPECASE args[1] OF | ValTbl(node) => tbl:=node.value;
        ELSE ObValue.BadArgType(1, "tbl", self.name, opCode.name, loc); END;
        IF ISTYPE(tbl,TextRefTbl.T) THEN
          TYPECASE args[2] OF | ObValue.ValText(node) => text1:=node.text;
          ELSE ObValue.BadArgType(2, "text", self.name, opCode.name, loc); END;
          EVAL NARROW(tbl,TextRefTbl.T).put(text1,args[3]);
          RETURN ObValue.valOk;
        ELSE
          TYPECASE args[2] OF | ObValue.ValInt(node) => int1:=node.int;
          ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); END;
          EVAL NARROW(tbl,IntRefTbl.T).put(int1,args[3]);
          RETURN ObValue.valOk;
        END;

    | TblCode.Delete =>
        TYPECASE args[1] OF | ValTbl(node) => tbl:=node.value;
        ELSE ObValue.BadArgType(1, "tbl", self.name, opCode.name, loc); END;
        IF ISTYPE(tbl,TextRefTbl.T) THEN
          TYPECASE args[2] OF | ObValue.ValText(node) => text1:=node.text;
          ELSE ObValue.BadArgType(2, "text", self.name, opCode.name, loc); END;
          IF NARROW(tbl,TextRefTbl.T).delete(text1,result) THEN RETURN result;
          ELSE RETURN ObValue.valOk;
          END;
        ELSE
          TYPECASE args[2] OF | ObValue.ValInt(node) => int1:=node.int;
          ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); END;
          IF NARROW(tbl,IntRefTbl.T).delete(int1,result) THEN RETURN result;
          ELSE RETURN ObValue.valOk;
          END;
        END;

    | TblCode.Size =>
        TYPECASE args[1] OF | ValTbl(node) => tbl:=node.value;
        ELSE ObValue.BadArgType(1, "tbl", self.name, opCode.name, loc); END;
        IF ISTYPE(tbl,TextRefTbl.T) THEN
          RETURN NEW(ObValue.ValInt, int := NARROW(tbl,TextRefTbl.T).size());
        ELSE
          RETURN NEW(ObValue.ValInt, int := NARROW(tbl,IntRefTbl.T).size());
        END;

    | TblCode.Iterate =>
        TYPECASE args[1] OF | ValTbl(node) => tbl:=node.value;
        ELSE ObValue.BadArgType(1, "tbl", self.name, opCode.name, loc); END;
        TYPECASE args[2] OF | ObValue.ValBool(node) => bool1:=node.bool;
        ELSE ObValue.BadArgType(2, "bool", self.name, opCode.name, loc); END;
        IF ISTYPE(tbl,SortedTextRefTbl.T) THEN
          iter := NEW(ValIter, value := NARROW(tbl,SortedTextRefTbl.T).
              iterateOrdered(bool1));
        ELSIF ISTYPE(tbl,SortedIntRefTbl.T) THEN
          iter := NEW(ValIter, value := NARROW(tbl,SortedIntRefTbl.T).
              iterateOrdered(bool1));
        ELSIF ISTYPE(tbl,TextRefTbl.T) THEN
          iter := NEW(ValIter, value := NARROW(tbl,TextRefTbl.T).iterate());
        ELSIF ISTYPE(tbl,IntRefTbl.T) THEN
          iter := NEW(ValIter, value := NARROW(tbl,IntRefTbl.T).iterate());
        END;
        iter.end := FALSE;
        iter.value := ObValue.valOk;
        RETURN iter;

    | TblCode.NextItem =>
        TYPECASE args[1] OF | ValIter(node) => iter:=node;
        ELSE ObValue.BadArgType(1, "iter", self.name, opCode.name, loc); 
        END;
        IF NOT iter.end THEN
          IF ISTYPE(iter.value,TextRefTbl.Iterator) THEN
            TYPECASE args[2] OF | ObValue.ValText(node) => text1:=node.text;
            ELSE ObValue.BadArgType(2, "text", self.name, opCode.name, loc); 
            END;
            iter.end := NARROW(iter.value,TextRefTbl.Iterator).
                next(text1,iter.itemValue);
            RETURN ObValue.NewText(text1);
          ELSE
            TYPECASE args[2] OF | ObValue.ValInt(node) => int1:=node.int;
            ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); 
            END;
            iter.end := NARROW(iter.value,IntRefTbl.Iterator).
                next(int1,iter.itemValue);
            RETURN NEW(ObValue.ValInt, int := int1);
          END;
        ELSE
          RETURN ObValue.valOk;
        END;

    | TblCode.NextValue =>
        TYPECASE args[1] OF | ValIter(node) => iter:=node;
        ELSE ObValue.BadArgType(1, "iter", self.name, opCode.name, loc); 
        END;
        RETURN iter.itemValue;

    | TblCode.Seek =>
        TYPECASE args[1] OF | ValIter(node) => iter:=node;
        ELSE ObValue.BadArgType(1, "iter", self.name, opCode.name, loc); 
        END;
        IF ISTYPE(iter.value,SortedTextRefTbl.Iterator) THEN
          TYPECASE args[2] OF | ObValue.ValText(node) => text1:=node.text;
          ELSE ObValue.BadArgType(2, "text", self.name, opCode.name, loc); END;
          NARROW(iter.value,SortedTextRefTbl.Iterator).seek(text1);
          RETURN ObValue.valOk;
        ELSIF ISTYPE(iter.value,SortedIntRefTbl.Iterator) THEN
          TYPECASE args[2] OF | ObValue.ValInt(node) => int1:=node.int;
          ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); END;
          NARROW(iter.value,SortedIntRefTbl.Iterator).seek(int1);
          RETURN ObValue.valOk;
        ELSE RETURN tblException;
        END;
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc);
    END;
  END EvalTbl;

BEGIN
END ObLibTbl.
