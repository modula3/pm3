(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Wed Jul  6 17:02:59 PDT 1994 by bharat *)
(* modified on Fri Jul 2 16:59:25 PDT 1993 by mhb *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "ZHandleVBT" is a child of a "ZSplit" and it can reposition and resize
   its child. *)

INTERFACE ZHandleVBT;

IMPORT  FormsVBT, Rect, VBT, ZSplit;


TYPE
  <* SUBTYPE T <: MultiFilter.T *>
  T <: Public;
  Public = ZSplit.T OBJECT
             selection: Selection;
           METHODS
             <* LL <= VBT.mu *>
             init     (ch: VBT.T; selection: Selection := NIL): T;
             on       (singlemode: BOOLEAN);
             off      ();
             getchild (): VBT.T;
             getDomain () : Rect.T;
             replaceChild(fv : VBT.T);
           END;
  (* The call "v.init(ch)" initialize "v" as a "ZHandleVBT".  Und so
     weiter. *)
  (* If a selection parameter is given it should have already been
     initialized *)
  (* v.on(sm) turns the handle on and sets feedback style depending on
     sm *)
  (* v.off(0 turns the handle off -- note these methods are used by the *)
  (* Selection object to implement the protocol *)

  Selection <: PublicSelect;

  PublicSelect =
    OBJECT
    METHODS
      init (size: CARDINAL; singlemode: BOOLEAN; dialog:FormsVBT.T);

      (* This initializes a selection table of size size *)
      (* ZHandleVBTs are given a Selection parameter which determines
         the *)
      (* Selection Protocol.  If none is defined an arbitray number of
         objects can *)
      (* be selected *)

      on (v: T; singlemode: BOOLEAN);
      (* This is called by ZHandleVBT.Ts when they are turned ON *)

      off (v: T; singlemode: BOOLEAN);
      (* This is called by ZHandleVBT.Ts when they are turned OFF *)

      (* There are two modes - single-mode and multiple-mode *)
      (* You may choose to override these methods to enforce a different *)
      (* protocol.  The built in protocol is as follows :- *)

      (* ZHandleVBTs call the on/off method of their associated
         Selection *)
      (* if one is defined when they are turned on or off *)
      (* the Selection object calls the on/off methods *)
      (* of ZHandleVBTs in its purview *)
      (* if none is defined they just turn on/off in single mode *)

      (* 1.  ON in single mode : Flushes selection, adds current vbt *)
      (* 2.  ON in multiple mode : Flushes selection except for siblings&
         adds*)
      (* 3.  OFF in single mode : Flushes selection *)
      (* 4.  OFF in multiple mode : Removes only current element *)

      (* The default feedback is Black in single mode, Gray in multiple
         Mode *)

      getSelectionSize (): CARDINAL;
      getSelection     (indx: CARDINAL): T;
      inSingleMode     (): BOOLEAN;

      alignSelectedObjects(kind: TEXT; alignmentStyle: TEXT; dontStretch:= FALSE);
      shapeSelectedObjects(kind: TEXT;  shapingStyle: TEXT);
      distributeSelectedObjects(mode: TEXT);
    END;

PROCEDURE NewSelection(n : T; s : Selection);
(* changes the selection of a ZHandleVBT *)

END ZHandleVBT.










