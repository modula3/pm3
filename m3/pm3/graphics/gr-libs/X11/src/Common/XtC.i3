(* Copyright (C) 1989, 1990 Digital Equipment Corporation	*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)

(* File: XtC.i3							*)
(* Last modified on Fri May  7 15:08:58 PDT 1993 by mjordan     *)  
(*      modified on Fri Apr 13 14:08:45 1990 by jerome		*)
(*      modified on Sat Feb 24 02:18:46 1990 by muller		*)


INTERFACE XtC;

(*==============================================================*)
(*	The X11 R4 Interface for Modula 3			*)
(*								*)
(*	Class Types   corresponding to:				*)
(*			X11 R4 Intrinsic			*)
(*			X11 R4 Athena Widget Set		*)
(*==============================================================*)


FROM Ctypes IMPORT char_star;

VAR
  Accelerators                   : char_star;
  AllowShellResize               : char_star;
  Argc                           : char_star;
  Argv                           : char_star;
  AutoFill                       : char_star;
  Background                     : char_star;
  BackingStore                   : char_star;
  BaseHeight                     : char_star;
  BaseWidth                      : char_star;
  Bitmap                         : char_star;
  Boolean                        : char_star;
  BorderColor                    : char_star;
  BorderWidth                    : char_star;
  Callback                       : char_star;
  CheckCommand                   : char_star;
  Color                          : char_star;
  Colormap                       : char_star;
  ColumnWidth                    : char_star;
  Columns                        : char_star;
  CornerRoundPercent             : char_star;
  CreatePopupChildProc           : char_star;
  Cursor                         : char_star;
  DataCompression                : char_star;
  Depth                          : char_star;
  Edge                           : char_star;
  EditType                       : char_star;
  EventBindings                  : char_star;
  File                           : char_star;
  Flip                           : char_star;
  Font                           : char_star;
  Foreground                     : char_star;
  Fraction                       : char_star;
  Function                       : char_star;
  Geometry                       : char_star;
  GripIndent                     : char_star;
  HSpace                         : char_star;
  Height                         : char_star;
  HeightInc                      : char_star;
  HorizontalMargins              : char_star;
  Icon                           : char_star;
  IconMask                       : char_star;
  IconName                       : char_star;
  IconNameEncoding               : char_star;
  IconPixmap                     : char_star;
  IconWindow                     : char_star;
  IconX                          : char_star;
  IconY                          : char_star;
  Iconic                         : char_star;
  Index                          : char_star;
  InitialResourcesPersistent     : char_star;
  InitialState                   : char_star;
  Input                          : char_star;
  Insensitive                    : char_star;
  InsertPosition                 : char_star;
  Interval                       : char_star;
  JumpScroll                     : char_star;
  Justify                        : char_star;
  KnobIndent                     : char_star;
  KnobPixel                      : char_star;
  Label                          : char_star;
  LabelClass                     : char_star;
  LeftBitmap                     : char_star;
  Length                         : char_star;
  LineWidth                      : char_star;
  List                           : char_star;
  Longest                        : char_star;
  MappedWhenManaged              : char_star;
  Margin                         : char_star;
  Max                            : char_star;
  MaxAspectX                     : char_star;
  MaxAspectY                     : char_star;
  MaxHeight                      : char_star;
  MaxWidth                       : char_star;
  MenuEntry                      : char_star;
  MenuName                       : char_star;
  MenuOnScreen                   : char_star;
  Min                            : char_star;
  MinAspectX                     : char_star;
  MinAspectY                     : char_star;
  MinHeight                      : char_star;
  MinWidth                       : char_star;
  MinimumThumb                   : char_star;
  Notify                         : char_star;
  NumberStrings                  : char_star;
  Orientation                    : char_star;
  Output                         : char_star;
  OverrideRedirect               : char_star;
  Parameter                      : char_star;
  PieceSize                      : char_star;
  Pixmap                         : char_star;
  PixmapMask                     : char_star;
  PopupOnEntry                   : char_star;
  Position                       : char_star;
  PreferredPaneSize              : char_star;
  RadioData                      : char_star;
  RadioGroup                     : char_star;
  ReadOnly                       : char_star;
  Resize                         : char_star;
  ReverseVideo                   : char_star;
  RightBitmap                    : char_star;
  RowHeight                      : char_star;
  SaveUnder                      : char_star;
  Scale                          : char_star;
  Screen                         : char_star;
  Scroll                         : char_star;
  ScrollDCursor                  : char_star;
  ScrollHCursor                  : char_star;
  ScrollLCursor                  : char_star;
  ScrollProc                     : char_star;
  ScrollRCursor                  : char_star;
  ScrollUCursor                  : char_star;
  ScrollVCursor                  : char_star;
  SelectTypes                    : char_star;
  Selection                      : char_star;
  SelectionArray                 : char_star;
  Sensitive                      : char_star;
  ShapeStyle                     : char_star;
  ShapeWindow                    : char_star;
  ShowGrip                       : char_star;
  Shown                          : char_star;
  Space                          : char_star;
  Spacing                        : char_star;
  State                          : char_star;
  Stipple                        : char_star;
  String                         : char_star;
  TemplateResource               : char_star;
  TextOptions                    : char_star;
  TextPosition                   : char_star;
  TextSink                       : char_star;
  TextSource                     : char_star;
  Thickness                      : char_star;
  Thumb                          : char_star;
  Title                          : char_star;
  TitleEncoding                  : char_star;
  TopOfThumb                     : char_star;
  Transient                      : char_star;
  TransientFor                   : char_star;
  Translations                   : char_star;
  Type                           : char_star;
  UseStringInPlace               : char_star;
  VSpace                         : char_star;
  Value                          : char_star;
  VertSpace                      : char_star;
  VerticalMargins                : char_star;
  Visual                         : char_star;
  Volume                         : char_star;
  WaitForWm                      : char_star;
  Widget                         : char_star;
  Width                          : char_star;
  WidthInc                       : char_star;
  WinGravity                     : char_star;
  Window                         : char_star;
  WindowGroup                    : char_star;
  WmTimeout                      : char_star;
  Wrap                           : char_star;
  X                              : char_star;
  Y                              : char_star;

PROCEDURE ForceToLoadAnImplementation ();

END XtC.
