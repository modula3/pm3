INTERFACE Globals;

IMPORT OO7, RefList, RefSeq, GenParams, ODMG, IntRefBPlusTree,
       TextRefBPlusTree, IntRefBagBPlusTree, Utime;

VAR
  (* index mapping atomic part ids to atomic parts *)
  AtomicPartIdx: IntRefBPlusTree.T;
  (* index mapping composite part ids to composite parts *)
  CompPartIdx: IntRefBPlusTree.T;
  (* index mapping document titles to documents *)
  DocumentIdx: TextRefBPlusTree.T;
  (* index mapping document ids to documents *)
  DocumentIdIdx: IntRefBPlusTree.T;
  (* index mapping assembly ids to base assemblies *)
  BaseAssemblyIdx: IntRefBPlusTree.T;
  (* index mapping module names to modules *)
  ModuleIdx: TextRefBPlusTree.T;
  (* index mapping build dates to atomic parts *)
  BuildDateIdx: IntRefBagBPlusTree.T;
  (* extent of all modules *)
  AllModules: RefSeq.T;
  (* these are used to sync inter-process for multiuser benchmark *)
  ClientsReady: REF INTEGER;
  ClientsDone: REF INTEGER;
  TotalAborts: REF INTEGER;
  StartUser, EndUser, StartSystem, EndSystem, StartWallTime, EndWallTime: REF Utime.struct_timeval;
  chain_tx := TRUE;
  nextAtomicId: INTEGER;
  nextCompositeId: INTEGER;
  nextComplexAssemblyId: INTEGER;
  nextBaseAssemblyId: INTEGER;
  nextModuleId: INTEGER;
  shared_cp: REF ARRAY OF RefList.T;
  private_cp: REF ARRAY OF RefList.T;
  types: ARRAY [0..GenParams.NumTypes - 1] OF OO7.Type;
  debugMode := FALSE;

EXCEPTION Error;

PROCEDURE InitGlobals(db: ODMG.T) RAISES { Error };
PROCEDURE ResetTimes();

END Globals.
