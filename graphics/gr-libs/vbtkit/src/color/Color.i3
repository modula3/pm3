(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Created by stolfi on Mon Mar 28 20:52:56 1988 *)
(* Last modified on Sat Nov 28 11:43:44 PST 1992 by mhb *)
(* modified on Wed Jun 17 11:45:38 PDT 1992 by stolfi *)
(* modified on Fri Mar 13 14:53:31 PST 1992 by muller *)

(* A "Color.T" describes a color as a mixture of the three color TV
   primaries (Red, Green and Blue), in a linear scale
   (proportional to luminous power), where 0.0 = black and 1.0 =
   maximum screen intensity.

   The set of all colors with RGB coordinates in the range
   0.0--1.0 is the {\em unit RGB cube}.  The colors along the
   main diagonal of the unit cube (from (0,0,0) to (1,1,1))
   contain equal amounts of all three primaries; they represent
   gray levels.  RGB triples outside the unit cube cannot be
   displayed on typical color monitors, but are still legal as
   far as this interface is concerned, make perfect physical
   sense, and are useful in some color computations. 

   This interface also provides routines to convert colors between 
   the HSV (Hue, Saturation, Value) and RGB color models. *)

INTERFACE Color;

TYPE 
  T = RECORD r, g, b: REAL;  END;

CONST
  (* The vertices of the unit RGB cube: *)
  Black   = T{0.0, 0.0, 0.0};
  Red     = T{1.0, 0.0, 0.0};
  Green   = T{0.0, 1.0, 0.0};
  Blue    = T{0.0, 0.0, 1.0};
  Cyan    = T{0.0, 1.0, 1.0};
  Magenta = T{1.0, 0.0, 1.0};
  Yellow  = T{1.0, 1.0, 0.0};
  White   = T{1.0, 1.0, 1.0};

(* The following procedures are useful for converting a color
   into a shade of gray: *)

PROCEDURE Brightness (READONLY rgb: T): REAL;
(* Return the intensity of "rgb" in a linear scale.  The formula used is
|  0.239 * rgb.r + 0.686 * rgb.g + 0.075 * rgb.b
   clipped to the range 0.0--1.0. *)

(* An "HSV" is a color represented as a (Hue, Saturation,
   Value) triple.  The HSV color model is somewhat more intuitive
   than the RGB color model.  It's based on mimicking the way
   that an artist mixes paint: ``He chooses a pure hue, or
   pigment and lightens it to a {\it tint\/} of that hue by
   adding white, or darkens it to a {\it shade\/} of that hue by
   adding black, or in general obtains a {\it tone\/} of that hue
   by adding some mixture of white and black.''

   So, varying hue corresponds to selecting a pure color along a
   color wheel where 0 is red, .167 is yellow, .333 is green, .5
   is cyan, .667 is blue, and .833 is magenta, and 1.0 is red
   again.  Decreasing the saturation (from 1 down to 0)
   corresponds to adding white.  Decreasing the value (from 1
   down to 0) corresponds to adding black.

   This interface provides procedures to map between RGB and HSV
   color models.  Note that white and black have indeterminate
   hue and saturation.  Pure colors have saturation=1 and
   value=1, whereas grey levels have saturation=0,
   value=brightness, and indeterminate hue. *)

TYPE 
  HSV = RECORD h, s, v: REAL END;

(* The following procedures convert between RGB and HSV color
   models: *)

PROCEDURE ToHSV (READONLY rgb: T): HSV;
(* Convert from RGB to HSV coordinates.  By convention, gray
   colors (including white and black) get hue=0.0.  In addition,
   black gets saturation=0.0. *)

PROCEDURE FromHSV (READONLY hsv: HSV): T;
(* Convert from HSV to RGB coordinates.  If value=0 (black),
   saturation and hue are irrelevant.  If saturation=0 (gray),
   hue is irrelevant. *)

END Color.


