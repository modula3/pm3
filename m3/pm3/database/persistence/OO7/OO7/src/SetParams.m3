MODULE SetParams EXPORTS SetParams, VarParams;

IMPORT Rd, Stdio, Lex, Thread, FloatMode;
FROM IO IMPORT OpenRead, Put, PutInt;
FROM Lex IMPORT Int, Match, Skip;

(*
//////////////////////////////////////////////////////////////////
// Set generation parameters for the benchmark.
//
// Assumes a configuration file "OO7.config" of the
// following format:
//
// NumAssmPerAssm     	n1
// NumCompPerAssm     	n2
// NumSharPerAssm	n3
// NumCompPerModule   	n4
// NumAssmLevels      	n5
// TotalModules       	n6
// NumAtomicPerComp	n7
// NumConnPerAtomic 	n8
// DocumentSize		n9
// ManualSize           n10
//
// where n1 through n10 are integers.  The order of
// parameters is critical!  (This is a dumb parser
// after all.) 
//////////////////////////////////////////////////////////////////
*)

PROCEDURE FromFile(configFileName: TEXT) RAISES { Error } =
  VAR
    configFile := OpenRead(configFileName);
  PROCEDURE GetParam(param: TEXT): INTEGER RAISES { Error } =
    VAR value: INTEGER;
    BEGIN
      TRY
        Skip(configFile);
        Match(configFile, param);
        value := Int(configFile);
      EXCEPT
      | Rd.Failure, Lex.Error, Thread.Alerted, FloatMode.Trap =>
        Put("Error reading configuration parameter ", Stdio.stderr);
        Put(param, Stdio.stderr); Put("\n", Stdio.stderr);
        RAISE Error;
      END;
      Put(param); Put(" = "); PutInt(value); Put(".\n");
      RETURN value;
    END GetParam;
  BEGIN
    IF configFile = NIL THEN
      Put("Couldn't open config file: ", Stdio.stderr);
      Put(configFileName, Stdio.stderr); Put("\n", Stdio.stderr);
      RAISE Error;
    END;

    (* Get parameters. *)
    NumAssmPerAssm    := GetParam("NumAssmPerAssm");
    NumCompPerAssm    := GetParam("NumCompPerAssm");
    NumSharPerAssm    := GetParam("NumSharPerAssm");
    NumCompPerModule  := GetParam("NumCompPerModule");
    NumAssmLevels     := GetParam("NumAssmLevels");
    TotalModules      := GetParam("TotalModules");
    NumAtomicPerComp  := GetParam("NumAtomicPerComp");
    NumConnPerAtomic  := GetParam("NumConnPerAtomic");
    DocumentSize      := GetParam("DocumentSize");
    ManualSize        := GetParam("ManualSize");

    TotalCompParts := NumCompPerModule * TotalModules;
    Put("Setting TotalCompParts to "); PutInt(TotalCompParts); Put(".\n");

    TotalAtomicParts := TotalCompParts * NumAtomicPerComp;
    Put("Setting TotalAtomicParts to "); PutInt(TotalAtomicParts); Put(".\n" );
  END FromFile; 

BEGIN
END SetParams.
