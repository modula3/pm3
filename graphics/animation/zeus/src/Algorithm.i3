(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Jun 23 14:59:30 PDT 1993 by steveg *)
(*      modified on Thu Sep 24 10:28:53 PDT 1992 by mhb *)
(* modified on Tue Aug 18 14:04:03 PDT 1992 by johnh *)
(* modified on Tue Jul 21 0:56:50 PDT 1992 by sclafani *)

(* An Algorithm.T is a subclass of a ZeusClass.T with two
   additional methods and four additional fields:

   The "run" method is invoked when the user clicks on "GO" in
   the control panel.  It typically contains the algorithm as one
   would find in a text book.  The default method raises a fatal
   error (i.e., there is no default).

   If the user requests that the algorithm be aborted (via the
   Zeus control panel), a Thread.Alert exception is raised to
   abort the algorithm.  Thus, if the algorithm spawns any
   threads, it should handle the Thread.Alerted exception by
   aborting its children threads and then exiting.  Most
   single-threaded algorithms can just ignore the Thread.Alerted
   exception.

   The "init" method should be called right after an Algorithm.T
   is created with NEW.  The default method is a no-op (returns
   itself), but sub-classes may use it for necessary set-up.
   Therefore, any user-supplied override to the init method must
   invoke the init method of the supertype.

   The "data" field is a VBT.T (usually a FormsVBT.T), which will
   be displayed to the user in the session control panel.  The
   algorithm may use it to get input data from the user.  (Don't
   forget to lock VBT.mu before calling FormsVBT procedures to
   return data from the form!) Subclasses of Algorithm.T should
   ensure that the data field is initialized, either in the init
   method or before.  It's OK for this field to be NIL; in that
   case, Zeus will not call the display anything to the user.

   The "codeViews" field is an association list (a list of
   two-element lists).  In each tuple, the key is the public name
   of the view (a TEXT), and the second element is the name of a
   resource containing its source (also a TEXT).  The "codePath"
   field specifies the path to use when looking for resources.
   If "codePath" is "NIL", then "ZeusPanel.GetPath()" is used.

   One way to construct this list is to load it from a file using
   Sx.Read.  Here's what such a file specifying 3 elements would
   contain:

|    (("Insertion.m3" "Ins.m3.txt")
|     ("Insertion.c"  "Ins.c.txt")
|     ("Insertion"    "Ins.pseudo.txt"))

   Alternatively, it's easy to construct such a list on the fly:

|    RefList.List3(
|        RefList.List2("Insertion.m3", "Ins.m3.txt"),
|        RefList.List2("Insertion.c",  "Ins.c.txt"),
|        RefList.List2("Insertion",    "Ins.pseudo.txt"))


   The "varView" field contains the DataView.T to be used to the
   algorithm.
   
   Typically, the variable view is generated by ZeusDataView. 
   The procedure "ZeusDataView.New" should be passed when the view
   is registered. The "Startrun" method will fill in the "varView"
   field, based on the values in the "varRsrc" and "varPath" fields.
   If "varPath" is "NIL", then "ZeusPanel.GetPath()" is used.

   The default ZeusClass methods "install", "delete", "config",
   and "reactivity" are all noops.

   The default "snapshot" and "restore" methods test whether the
   "data" field is subclass of FormsVBT.T, and if so uses the
   FormsVBT "snapshot" and "restore" methods to save and restore
   the state of the data form.  If an algorithm keeps its primary
   data in the data form, then it does not need to override the
   snapshot/restore methods.  Otherwise, it needs to override the
   restore method with a procedure that invokes the supertype's
   restore method (restoring the FormsVBT.T), and then loads its
   variables from the "data" FormsVBT.T.

   Subclasses of Algorithm.T that are created by zume will be
   extended with methods for each FEEDBACK event in the .evt
   file.  The feedback methods must be invoked by a view while
   holding VBT.mu.  User-supplied overrides for FEEDBACK methods
   MUST NOT invoke the corresponding supertype methods. *)

INTERFACE Algorithm;

IMPORT DataView, RefList, Rsrc, Thread, VBT, ZeusClass;

TYPE
  T <: Public;
  Public = ZeusClass.T OBJECT
             data     : VBT.T      := NIL;
             codeViews: RefList.T  := NIL;
             codePath : Rsrc.Path  := NIL;
             varView  : DataView.T := NIL;
             varRsrc  : TEXT       := NIL;
             varPath  : Rsrc.Path  := NIL;
           METHODS
             (* LL = VBT.mu *)
             init (): T;
             (* LL < VBT.mu *)
             run () RAISES {Thread.Alerted};
           END;

END Algorithm.
