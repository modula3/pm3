(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Mar 21 16:29:11 PST 1993 by meehan     *)
(*      modified on Tue Jun 16 13:12:32 PDT 1992 by muller     *)
(*      modified on Mon Nov 12 17:20:07 1990 by jdd            *)

INTERFACE VTextDef;

IMPORT MText, Font, VBT, VTDef;

TYPE
  Pixels = INTEGER;
  T = REF RECORD
            regionMax: Region;   (* highest-numbered region = number of
                                    regions - 1 *)
            region: ARRAY Region OF RegionRec;
            closed: BOOLEAN;
            vt    : VTDef.T;
            mtext : MText.T;                    (* the mutable text itself *)
            vbt   : VBT.T;                      (* the bitmap we display in *)
            font  : Font.T;                     (* the font for displaying *)
            vOptions     : VTDef.VOptions;  (* the VOptions for the VText *)
            north, height: Pixels;
            west         : Pixels;          (* origin of display rectangle *)
            width        : Pixels;          (* size of display rectangle *)
            leftMargin, rightMargin: Pixels;  (* left and right margins *)
            left                   : Pixels;  (* = west + left margin *)
            turnMargin             : Pixels;
            (* margin for displaying turning indicator *)
            topMargin  : Pixels;  (* top margin of every region *)
            leading    : Pixels;  (* how many pixels between lines *)
            lineSpacing: Pixels;  (* leading plus character height *)
            lineOffset : Pixels;
            (* how far a line's baseline is below its top *)
            caretState   : VTDef.OnOffState;
            dividersDirty: BOOLEAN;
            (* whether to redraw dividers at next Update *)
          END;

  RegionRec = RECORD
                view  : VTDef.View;
                north : Pixels;   (* top of region's rectangle (absolute) *)
                height: Pixels;   (* height of region's rectangle *)
                top   : Pixels;   (* = north + top margin *)
                nLines: INTEGER;  (* number of lines displayed (computed) *)
              END;

  Region = [0 .. 3];

END VTextDef.


