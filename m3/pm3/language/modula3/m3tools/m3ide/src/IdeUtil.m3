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

MODULE IdeUtil;

IMPORT Thread, Rd, Wr, TextWr, Pipe, FileWr, FileRd, Process, FS, File,
       Pathname, TextSeq, RegularFile, OSError, AtomList, Atom;

TYPE
  FileType = {All, Regular, Directory};

PROCEDURE RecursiveList(name: Pathname.T; recursive: BOOLEAN;
    type: FileType; files: TextSeq.T; path: Pathname.T) RAISES{OSError.E} =
  VAR
    iterator := FS.Iterate(Pathname.Join(name,path,NIL));
    filename: TEXT;
    status: File.Status;
  BEGIN
    WHILE iterator.nextWithStatus(filename,status) DO
      IF type = FileType.All OR
         (type = FileType.Directory AND status.type = FS.DirectoryFileType) OR
         (type = FileType.Regular AND status.type = RegularFile.FileType) THEN
        files.addhi(Pathname.Join(path,filename,NIL));
      END;
      IF recursive AND status.type = FS.DirectoryFileType THEN
        RecursiveList(name,recursive,type,files,
            Pathname.Join(path,filename,NIL));
      END;
    END;
    iterator.close();
  END RecursiveList;

PROCEDURE ListFiles(name: Pathname.T; recursive := FALSE): 
    REF ARRAY OF Pathname.T RAISES{OSError.E} =
  VAR
    files := NEW(TextSeq.T).init();
    path: Pathname.T := "";
    array: REF ARRAY OF Pathname.T;
  BEGIN
    RecursiveList(name,recursive,FileType.All,files,path);
    array := NEW(REF ARRAY OF Pathname.T,files.size());
    FOR i := 0 TO LAST(array^) DO
      array[i] := files.get(i);
    END;
    RETURN array;
  END ListFiles;

PROCEDURE ListRegularFiles(name: Pathname.T; recursive:= FALSE): 
    REF ARRAY OF Pathname.T RAISES{OSError.E} =
  VAR
    files := NEW(TextSeq.T).init();
    path: Pathname.T := "";
    array: REF ARRAY OF Pathname.T;
  BEGIN
    RecursiveList(name,recursive,FileType.Regular,files,path);
    array := NEW(REF ARRAY OF Pathname.T,files.size());
    FOR i := 0 TO LAST(array^) DO
      array[i] := files.get(i);
    END;
    RETURN array;
  END ListRegularFiles;

PROCEDURE ListDirectories(name: Pathname.T; recursive := FALSE): 
    REF ARRAY OF Pathname.T RAISES{OSError.E} =
  VAR
    files := NEW(TextSeq.T).init();
    path: Pathname.T := "";
    array: REF ARRAY OF Pathname.T;
  BEGIN
    RecursiveList(name,recursive,FileType.Directory,files,path);
    array := NEW(REF ARRAY OF Pathname.T,files.size());
    FOR i := 0 TO LAST(array^) DO
      array[i] := files.get(i);
    END;
    RETURN array;
  END ListDirectories;

REVEAL
  ReadClosure = PublicRead BRANDED OBJECT METHODS 
    OVERRIDES
      apply := ApplyRead;
    END;

<*FATAL Rd.Failure*>

PROCEDURE ApplyRead(self: ReadClosure): REFANY =
  VAR
    wr := NEW(TextWr.T).init();
  BEGIN
    self.text := NIL;
    WHILE NOT Rd.EOF(self.rd) DO
      Wr.PutText(wr,Rd.GetText(self.rd,4096));
    END;
    Rd.Close(self.rd);
    self.text := TextWr.ToText(wr);
    RETURN NIL;
  END ApplyRead;

<*FATAL Thread.Alerted*>
<*FATAL Wr.Failure*>

PROCEDURE RunFilter(cmd: Pathname.T; input: TEXT; VAR output, error: TEXT;
    params: REF ARRAY OF TEXT := NIL; env: REF ARRAY OF TEXT := NIL; 
    wd: Pathname.T := NIL): INTEGER RAISES{OSError.E} =
  VAR
    hrChild, hwChild, hrSelf, hwSelf, hwChild2, hrSelf2: Pipe.T;
    p: Process.T;
    wr: Wr.T;
    rd: Rd.T;
    rd2: Rd.T;
    readClosure: ReadClosure;
    readClosure2: ReadClosure;
    thread: Thread.T;
    thread2: Thread.T;
  BEGIN
    Pipe.Open(hr := hrChild, hw := hwSelf);
    Pipe.Open(hr := hrSelf, hw := hwChild);
    Pipe.Open(hr := hrSelf2, hw := hwChild2);
    IF params = NIL THEN params := EmptyArray; END;

    p := Process.Create(cmd,params^,env,wd,hrChild,
        hwChild,hwChild2);
    hrChild.close();
    hwChild.close();
    hwChild2.close();
    wr := NEW(FileWr.T).init(hwSelf);
    rd := NEW(FileRd.T).init(hrSelf);
    rd2 := NEW(FileRd.T).init(hrSelf2);

    readClosure := NEW(ReadClosure,rd := rd);
    readClosure2 := NEW(ReadClosure,rd := rd2);
    thread := Thread.Fork(readClosure);
    thread2 := Thread.Fork(readClosure2);
    IF input # NIL THEN Wr.PutText(wr,input); END;
    Wr.Close(wr);
    EVAL Thread.Join(thread);
    EVAL Thread.Join(thread2);
    output := readClosure.text;
    error := readClosure2.text;
    RETURN Process.Wait(p);
  END RunFilter;

PROCEDURE OSMessage(list: AtomList.T): TEXT =
  VAR
    message := "";
  BEGIN
    WHILE list # NIL DO 
      message := message & " " & Atom.ToText(list.head); 
    END;
    RETURN message;
  END OSMessage;

VAR
  EmptyArray := NEW(REF ARRAY OF TEXT,0);
BEGIN
END IdeUtil.
