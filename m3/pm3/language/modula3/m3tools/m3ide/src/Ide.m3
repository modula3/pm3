(*  This file is part of m3ide, a simple development environment for M3    *)
(*  Copyright (C) 1995 Michel Dagenais                                     *)
(*                                                                         *)
(*  This program is free software; you can redistribute it and/or modify   *)
(*  it under the terms of the GNU General Public License as published by   *)
(*  the Free Software Foundation; either version 2 of the License, or      *)
(*  (at your option) any later version.                                    *)
(*                                                                         *)
(*  This program is distributed in the hope that it will be useful,        *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *)
(*  GNU General Public License for more details.                           *)
(*                                                                         *)
(*  You should have received a copy of the GNU General Public License      *)
(*  along with this program; if not, write to the Free Software            *)
(*  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.              *)
(*                                                                         *)
(*  For more information on this program, contact Michel Dagenais at       *)
(*  dagenais@vlsi.polymtl.ca or Electrical Eng. Dept., Ecole Polytechnique *)
(*  P.O. Box 6079, Station A, Montreal, Quebec, Canada, H3C 3A7.           *)

MODULE Ide;

IMPORT Text, IO, Rd, VBT, Trestle, FormsVBT, Rsrc, 
    Pathname, IdeUtil, TrestleComm, OSError,
    M3Config, Lex, TextSeq;

VAR
  formShown: VBT.T := NIL;

PROCEDURE InstallForm(fv: VBT.T) RAISES{TrestleComm.Failure} =
  BEGIN
    IF formShown # NIL THEN
      Trestle.Delete(formShown);
    END;
    Trestle.Install(fv);
    formShown := fv;
  END InstallForm;

PROCEDURE RemoveForm() =
  BEGIN
    Trestle.Delete(formShown);
    formShown := NIL;
  END RemoveForm;

PROCEDURE ShowForm(fileName: TEXT; form: TEXT): TEXT =
  VAR
    path: Rsrc.Path;
    fv: VBT.T;
  BEGIN
    TRY                          (* EXCEPT *)
      IF Text.Length(form) = 0 THEN
        RemoveForm();
        RETURN "";
      END;
      path := Rsrc.BuildPath(Pathname.Prefix(fileName));
      fv := NEW(FormsVBT.T).init(form,FALSE,path);
      InstallForm(fv);
    EXCEPT
    | FormsVBT.Error(msg) =>
        RETURN(msg);
    | TrestleComm.Failure =>
        RETURN("Cannot connect to window system");
    END;
    RETURN "";
  END ShowForm;

PROCEDURE PPrint(program: TEXT; VAR error: TEXT): TEXT =
  VAR
    output: TEXT;
  BEGIN
    TRY
      error := "";
      EVAL IdeUtil.RunFilter("m3pp",program,output,error);
      RETURN output;
    EXCEPT
    | OSError.E(list) =>
        error := IdeUtil.OSMessage(list);
        RETURN program;
    END;
  END PPrint;

PROCEDURE PackageM3makefile(fileName: TEXT): TEXT =
  VAR
    result := PackagePrefix(fileName);
  BEGIN
    IF NOT Text.Equal(result,"") THEN
      result := Pathname.Join(Pathname.Join(result,"src",NIL),"m3makefile",NIL);
    END;
    RETURN result;
  END PackageM3makefile;

CONST
  AllChar = SET OF CHAR{FIRST(CHAR) .. LAST(CHAR)};
  ProgramNameChar = (AllChar - Lex.Blanks) - SET OF CHAR{'"'};

PROCEDURE PackageProgram(fileName: TEXT; buildSuffix: TEXT): TEXT =
  VAR
    result: TEXT := PackagePrefix(fileName);
    rd: Rd.T;
    programName: TEXT;
  BEGIN
    IF NOT Text.Equal(result,"") THEN
      TRY
        result := Pathname.Join(result,M3Config.BUILD_DIR & buildSuffix,NIL);
        rd := IO.OpenRead(Pathname.Join(result,".M3EXPORTS",NIL));
        EVAL IO.GetLine(rd);
        Lex.Match(rd,"_define_pgm(\"");
        programName := Lex.Scan(rd,ProgramNameChar);
        Lex.Match(rd,"\")");
        result := Pathname.Join(result,programName,NIL);
      EXCEPT ELSE
        result := "";
      END;
    END;
    RETURN result;
  END PackageProgram;

PROCEDURE BuildDir(buildSuffix: TEXT): TEXT =
  BEGIN
    RETURN M3Config.BUILD_DIR & buildSuffix;
  END BuildDir;

PROCEDURE BuildPath(fileName: TEXT; buildSuffix: TEXT): TEXT =
  VAR
    result: TEXT := PackagePrefix(fileName);
  BEGIN
    IF NOT Text.Equal(result,"") THEN
      result := Pathname.Join(result,M3Config.BUILD_DIR & buildSuffix,NIL);
    END;
    RETURN result;
  END BuildPath;

PROCEDURE SourcePaths(fileName, buildSuffix: TEXT): REF ARRAY OF TEXT =
  VAR
    packageDir: TEXT := PackagePrefix(fileName);
    buildDir: TEXT := packageDir;
    paths := NEW(TextSeq.T).init();
    result: REF ARRAY OF TEXT;
    rd: Rd.T;
    line: TEXT;
    start, end: INTEGER;
  BEGIN
    IF NOT Text.Equal(buildDir,"") THEN
      buildDir := Pathname.Join(buildDir,M3Config.BUILD_DIR & buildSuffix,NIL);
      TRY
        rd := IO.OpenRead(Pathname.Join(buildDir,".M3EXPORTS",NIL));
        WHILE NOT Rd.EOF(rd) DO
          line := IO.GetLine(rd);
          IF Text.Equal("_map_add_",Text.Sub(line,0,9)) THEN
            start := Text.FindChar(line,'"') + 1;
            start := Text.FindChar(line,'"',start) + 1;
            start := Text.FindChar(line,'"',start) + 1;
            start := Text.FindChar(line,'"',start) + 1;
            start := Text.FindChar(line,'"',start) + 1;
            end := Text.FindChar(line,'"',start);
            paths.addhi(Pathname.Join(packageDir,
                Text.Sub(line,start,end - start),NIL));
          END;
        END;
        Rd.Close(rd);

        rd := IO.OpenRead(Pathname.Join(buildDir,".M3IMPTAB",NIL));
        WHILE NOT Rd.EOF(rd) DO
          line := IO.GetLine(rd);
          IF Text.Equal("@",Text.Sub(line,0,1)) THEN
            paths.addhi(Text.Sub(line,1));
          END;
        END;
        Rd.Close(rd);
      EXCEPT ELSE
      END;
    END;
    result := NEW(REF ARRAY OF TEXT,paths.size());
    FOR i := 0 TO LAST(result^) DO
      result[i] := paths.get(i);
    END;
    RETURN result;
  END SourcePaths;

PROCEDURE PackagePrefix(file: Pathname.T): Pathname.T =
  VAR
    arcs: Pathname.Arcs;
    nbArcs: INTEGER;
  BEGIN
    TRY
      arcs := Pathname.Decompose(file);
      nbArcs := arcs.size() - 1;
      WHILE nbArcs > 0 DO
        IF Text.Equal("src",arcs.get(nbArcs)) THEN
          EVAL arcs.remhi();
          RETURN Pathname.Compose(arcs);
        ELSE
          EVAL arcs.remhi();
        END;
        DEC(nbArcs);
      END;
    EXCEPT
    | Pathname.Invalid =>
    END;
    RETURN "";
  END PackagePrefix;

PROCEDURE PackageSuffix(file: Pathname.T): Pathname.T =
  VAR
    arcs: Pathname.Arcs;
    nbArcs: INTEGER;
  BEGIN
    TRY
      arcs := Pathname.Decompose(file);
      nbArcs := arcs.size() - 1;
      WHILE nbArcs > 0 DO
        IF Text.Equal("src",arcs.get(nbArcs)) THEN
          DEC(nbArcs);
          FOR i := 0 TO nbArcs DO
            EVAL arcs.remlo();
          END;
          arcs.addlo(NIL);
          RETURN Pathname.Compose(arcs);
        END;
      END;
    EXCEPT
    | Pathname.Invalid =>
    END;
    RETURN "";
  END PackageSuffix;

BEGIN
END Ide.
