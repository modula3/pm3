INTERFACE GenParams;

(* parameters for generating design information fields

   First parameters for dates.  Each type of object has
   a distinct date range, to make it easier to control the 
   results of queries that compare dates.

   Currently the important relationship is between assembly
   objects and composite parts, since queries #5 and #6
   ask for assemblies that use composite parts with build
   dates later than the date for the assembly.

   The overall picture is that composite parts are divided into
   two classes, "old" and "young", such that we have the following
   picture:

    "Old" composite parts   |  assembly object    |  "young" composite    | 
    have build dates in     |  have build dates   |  parts have build     |
    this range.             |  in this range.     |  dates in this range. |
                            |                     |                       |
   t -----------------------|---------------------|-----------------------|->

   The constant "YoungCompFrac" determines the fraction of composite 
   parts that are "young" --- about 1/YoungCompFrac of the composite parts 
   are young.  ("About" due to randomness in how the young parts are
   chosen.) *)
CONST

  MinModuleDate = 1000;			 (* lower bound for module          *)
					 (* buildDate values                *)

  MaxModuleDate = 1999;			 (* upper bound for module          *)
					 (* buildDate values                *)

  MinAssmDate = 1000;			 (* lower bound for assembly        *)
					 (* buildDate values                *)

  MaxAssmDate = 1999;			 (* upper bound for assembly        *)
					 (* buildDate values                *)

  MinAtomicDate = 1000;			 (* lower bound for atomic part     *)
					 (* buildDate values                *)

  MaxAtomicDate = 1999;			 (* upper bound for atomic part     *)
					 (* buildDate values                *)

  MinOldCompDate = 0;			 (* lower bound for "old" composite *)
					 (* part buildDate values           *)

  MaxOldCompDate = 999;			 (* upper bound for "old" composite *)
					 (* part buildDate values           *)

  MinYoungCompDate = 2000;		 (* lower bound for "young"         *)
					 (* composite part buildDate values *)

  MaxYoungCompDate = 2999;		 (* upper bound for "young"         *)
					 (* composite part buildDate values *)

  YoungCompFrac = 10;			 (* 1/YoungCompFrac composite parts *)
					 (* have "young" build dates (for   *)
					 (* queries #5 and #6.)             *)

  NumTypes = 10;			 (* # different design type names   *)


  (* parameters for generating AtomicParts and Connections *)

  XYRange = 100000;			 (* number of x or y values         *)

  (* parameters for generating CompositeParts and Documents *)

  DocumentText = "I am the documentation for composite part #%08s.\n";

  ManualText = "I am the manual for module #%08s.\n";

  BytesPerCompositePart = 15000;

END GenParams.
