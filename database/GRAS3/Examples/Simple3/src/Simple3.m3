(************************************************************************)
(**									*)
(**   GRAS - A Graph-Oriented Database System for SE Applications	*)
(**   Copyright (C) 1987-1992  Lehrstuhl Informatik III, RWTH Aachen	*)
(**                                                                     *)
(**   This library is free software; you can redistribute it and/or	*)
(**   modify it under the terms of the GNU Library General Public	*)
(**   License as published by the Free Software Foundation; either	*)
(**   version 2 of the License, or (at your option) any later version.	*)
(**    								        *)
(**   This library is distributed in the hope that it will be useful,	*)
(**   but WITHOUT ANY WARRANTY; without even the implied warranty of	*)
(**   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *)
(**   Library General Public License for more details.			*)
(**    								        *)
(**   You should have received a copy of the GNU Library General Public *)
(**   License along with this library; if not, write to the Free	*)
(**   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.*)
(**    								        *)
(**   Contact Adresses:						        *)
(**    								        *)
(**    Roland Baumann,                                                  *)
(**    Lehrstuhl f"ur Informatik III,					*)
(**    University of Technology Aachen (RWTH Aachen),			*)
(**    Ahornstr. 55,							*)
(**    D-52074 Aachen							*)
(**    								        *)
(**   Email to								*)
(**    							 	        *)
(**	roland@i3.informatik.rwth-aachen.de				*)
(**									*)
(************************************************************************)

MODULE Simple3 EXPORTS Main;

IMPORT PersistentGraphPool, PersistentGraphSystem;
IMPORT ErrorSupport, Access, PageFile;
IMPORT IO, Process;

(* This program deletes the graphpool 'ExamplePool'. *)

VAR pool: PersistentGraphPool.T;

BEGIN
  TRY

    (* Initialize GRAS. *)
    PersistentGraphSystem.Login("/tmp");

    (* Open graphpool with name 'ExamplePool' *)
    IF NOT PersistentGraphSystem.ExistsPool("ExamplePool") THEN
      IO.Put("Graphpool 'ExamplePool' does not exists! \n");
      IO.Put("Use 'Simple1' to create it. \n");
    ELSE
      TRY
        pool :=
          NEW(PersistentGraphPool.T).open(
            "ExamplePool", Access.Mode.ReadWriteExclusive, new := FALSE);
        IO.Put("Graphpool 'ExamplePool' opened. \n");
      EXCEPT
        PageFile.NoAccess (msg) =>
          IO.Put("Unable to open pool: " & msg & "\n");
          Process.Exit(1);
      | Access.Denied (msg) =>
          IO.Put("Unable to open pool: " & msg & "\n");
          Process.Exit(1);
      END;

      pool.beginTransaction();
      IF pool.existsGraph("ExampleGraph") THEN
        TRY
          pool.deleteGraph("ExampleGraph");
          IO.Put("Graph 'ExampleGraph' deleted.\n");
        EXCEPT
          PersistentGraphPool.NotExistent => <* ASSERT FALSE *>
        | PersistentGraphPool.InUse =>
            IO.Put("Cannot delete graph: it is in use.\n");
        END;
      ELSE
        IO.Put("Graph 'ExampleGraph' does not exist!\n");
      END;
      pool.commitTransaction();
    END;
    pool.close();
    TRY
      PersistentGraphSystem.DeletePool("ExamplePool");
    EXCEPT
      PageFile.NoAccess (msg) =>
        IO.Put("Cannot delete pool: " & msg & "\n");
    END;

  EXCEPT
    PersistentGraphPool.InternalError (e) =>
      IO.Put("Pool: Internal Error\n" & ErrorSupport.ToText(e) & "\n");
  | PersistentGraphSystem.InternalError (e) =>
      IO.Put("System: Internal Error\n" & ErrorSupport.ToText(e) & "\n");
  | Access.Locked => IO.Put("Deadlock!\n");
  | PersistentGraphPool.NotInTransaction =>
      IO.Put("Not in transaction!\n");
  END;

END Simple3.
