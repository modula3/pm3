(* Copyright (C) 1992, Digital Equipment Corporation              *)
(* All rights reserved.                                           *)
(* See the file COPYRIGHT for a full description.                 *)
(* Created by stolfi on Wed Apr 19 01:33:17 1989                  *)
(* Last modified on Mon Jul 12 21:06:12 PDT 1993 by mhb       *)
(*      modified on Fri May 14 16:45:56 PDT 1993 by meehan    *)
(*      modified on Wed Jun  3 18:06:16 PDT 1992 by stolfi        *)
(*      modified on Tue Feb 11 21:39:49 PST 1992 by muller        *)

(* The "ColorName" interface provides a standard mapping between color
   names and linear RGB triples. The implementation recognizes
   the following names, based on those found in 
   "/usr/lib/X11/rgb.txt": 

<TABLE>
<TR><TD>AliceBlue <TD> ForestGreen <TD> MintCream  <TD> SandyBrown
<TR><TD>AntiqueWhite *<TD> Gainsboro <TD> MistyRose *<TD> SeaGreen *
<TR><TD>  Aquamarine *<TD> GhostWhite <TD> Moccasin <TD> Seashell *
<TR><TD>  Azure *<TD> Gold *<TD> NavajoWhite *<TD> Sienna *
<TR><TD>  Beige <TD> Goldenrod *<TD> Navy <TD> SkyBlue *
<TR><TD>  Bisque <TD> GoldenrodYellow<TD> NavyBlue <TD> SlateBlue *
<TR><TD>  Black <TD> Gray **<TD> OldLace <TD> SlateGray *
<TR><TD>  BlanchedAlmond <TD> Green *<TD> OliveDrab *<TD> SlateGrey 
<TR><TD>  Blue *<TD> GreenYellow    <TD> OliveGreen *<TD> Snow *
<TR><TD>  BlueViolet <TD> Grey **<TD> Orange *<TD> SpringGreen*
<TR><TD>  Brown *<TD> Honeydew *<TD> OrangeRed *<TD> SteelBlue *
<TR><TD>  Burlywood *<TD> HotPink *<TD> Orchid *<TD> Tan *
<TR><TD>  CadetBlue *<TD> IndianRed *<TD> PapayaWhip <TD> Thistle *
<TR><TD>  Chartreuse *<TD> Ivory *<TD> PeachPuff *<TD> Tomato *
<TR><TD>  Chocolate *<TD> Khaki *<TD> Peru <TD> Turquoise *
<TR><TD>  Coral *<TD> Lavender <TD> Pink *<TD> Violet 
<TR><TD>  CornflowerBlue <TD> LavenderBlush *<TD> Plum *<TD> VioletRed *
<TR><TD>  Cornsilk *<TD> LawnGreen <TD> Powderblue <TD> Wheat *
<TR><TD>  Cyan *<TD> LemonChiffon *<TD> Purple *<TD> White 
<TR><TD>  DeepPink *<TD> LimeGreen <TD> Red *<TD> WhiteSmoke 
<TR><TD>  DeepSkyBlue *<TD> Linen <TD> RosyBrown*<TD> Yellow *
<TR><TD>  DodgerBlue *<TD> Magenta *<TD> Royalblue*<TD> YellowGreen 
<TR><TD>  Firebrick *<TD> Maroon *<TD> SaddleBrown 
<TR><TD>  FloralWhite <TD> MidnightBlue <TD> Salmon *
</TABLE>

    The star ("*") indicates that the implementation recognizes
    a name along with the suffixes "1"-"4"; e.g., "Red", "Red1", "Red2",
    "Red3", and "Red4".
    
   The double star ("**") indicates that the 
   implementation also recognizes the names with the 
   suffixes "0" through "100". That is, "Gray0", "Gray1", ... ,
   "Gray100", as well as "Grey0", "Grey1", ... , "Grey100".
   
   In addition, the name of a color "C" from this list
   can be prefixed by one or more of the following modifiers:


<TABLE><TR><TD><I>Term</I><TD><I>Meaning</I>

<TR><TD>"Light" or "Pale" <TD> 1/3 of the way from "C" to white
<TR><TD>"Dim" or "Dark" <TD> 1/3 of the way from "C" to black
<TR><TD>"Weak", "Drab", or "Dull" <TD> 1/3 of the way from "C" to 
    the gray with the same brightness as "C"
<TR><TD>"Strong", "Vivid" or "Bright" <TD> 
    1/3 of the way from "C" to the purest color
    with the same hue as "C"
<TR><TD>"Reddish" <TD> 1/3 of the way from "C" to red
<TR><TD>"Greenish" <TD> 1/3 of the way from "C" to green
<TR><TD>"Bluish" <TD> 1/3 of the way from "C" to blue
<TR><TD>"Yellowish" <TD> 1/3 of the way from "C" to yellow
</TABLE>

Each of these modifiers can be modified in turn by the following
prefixes, which replace ``1/3 of the way'' by the indicated fraction:

<TABLE><TR><TD><I>Term</I><TD> <I>Degree</I><TD> <I>% (approx.)
<TR><TD>    "VeryVerySlightly"   <TD> 1/16 of the way   <TD>  6% 
<TR><TD>    "VerySlightly"       <TD> 1/8 of the way    <TD> 13% 
<TR><TD>    "Slightly"           <TD> 1/4 of the way    <TD> 25% 
<TR><TD>    "Somewhat"           <TD> 3/8 of the way    <TD> 38% 
<TR><TD>    "Rather"             <TD> 1/2 of the way    <TD> 50% 
<TR><TD>    "Quite"              <TD> 5/8 of the way    <TD> 63% 
<TR><TD>    "Very"               <TD> 3/4 of the way    <TD> 75% 
<TR><TD>    "VeryVery"           <TD> 7/8 of the way    <TD> 88% 
<TR><TD>    "VeryVeryVery"       <TD> 15/16 of the way  <TD> 94% 
</TABLE>

   The modifier "Medium" is also recognized as a
   shorthand for "SlightlyDark". (But you cannot use "VeryMedium".)

*)

INTERFACE ColorName;

IMPORT Color, TextList;

EXCEPTION NotFound;

PROCEDURE ToRGB (name: TEXT): Color.T RAISES {NotFound};
(* Give the "RGB.T" value described by "name", ignoring case and
   whitespace.  A cache of unnormalized names is maintained, so
   this procedure should be pretty fast for repeated lookups of
   the same name. *)

PROCEDURE NameList (): TextList.T;
(* Return a list of all the ``basic'' (unmodified) color names known
   to this module, as lower-case "TEXT"s, in alphabetical order. *)

END ColorName.

