(* Copyright (C) 1989, 1990 Digital Equipment Corporation	*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)

(* File: XtR.i3							*)
(* Last modified on Fri May  7 15:09:06 PDT 1993 by mjordan     *)
(*      modified on Fri Apr 13 14:10:16 1990 by jerome		*)
(*      modified on Mon Feb 26 21:59:52 1990 by muller		*)


INTERFACE XtR;

(*==============================================================*)
(*	The X11 R4 Interface for Modula 3			*)
(*								*)
(*	Representation Types  corresponding to:			*)
(*			X11 R4 Intrinsic			*)
(*			X11 R4 Athena Widget Set		*)
(*==============================================================*)


FROM Ctypes IMPORT char_star;

VAR
  AcceleratorTable               : char_star;
  AsciiType                      : char_star;
  Atom                           : char_star;
  BackingStore                   : char_star;
  Bitmap                         : char_star;
  Bool                           : char_star;
  Boolean                        : char_star;
  CallProc                       : char_star;
  Callback                       : char_star;
  Cardinal                       : char_star;
  Color                          : char_star;
  Colormap                       : char_star;
  Cursor                         : char_star;
  Dimension                      : char_star;
  Display                        : char_star;
  EditMode                       : char_star;
  Enum                           : char_star;
  File                           : char_star;
  Float                          : char_star;
  Font                           : char_star;
  FontStruct                     : char_star;
  Function                       : char_star;
  Geometry                       : char_star;
  Immediate                      : char_star;
  InitialState                   : char_star;
  Int                            : char_star;
  Justify                        : char_star;
  Long                           : char_star;
  LongBoolean                    : char_star;
  Object                         : char_star;
  Orientation                    : char_star;
  Pixel                          : char_star;
  Pixmap                         : char_star;
  Pointer                        : char_star;
  Position                       : char_star;
  Screen                         : char_star;
  ShapeStyle                     : char_star;
  Short                          : char_star;
  String                         : char_star;
  StringArray                    : char_star;
  StringTable                    : char_star;
  TranslationTable               : char_star;
  UnsignedChar                   : char_star;
  Visual                         : char_star;
  Widget                         : char_star;
  WidgetClass                    : char_star;
  WidgetList                     : char_star;
  Window                         : char_star;


PROCEDURE ForceToLoadAnImplementation ();

END XtR.
