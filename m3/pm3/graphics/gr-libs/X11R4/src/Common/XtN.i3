(* Copyright (C) 1989, 1990 Digital Equipment Corporation	*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)

(* File: XtN.i3							*)
(* Last modified on Fri May  7 15:09:15 PDT 1993 by mjordan     *)
(*      modified on Fri Apr 13 14:10:01 1990 by jerome		*)
(*      modified on Sat Feb 24 02:19:12 1990 by muller		*)


INTERFACE XtN;

(*==============================================================*)
(*	The X11 R4 Interface for Modula 3			*)
(*								*)
(*	Resource names corresponding to:			*)
(*			X11 R4 Intrinsic			*)
(*			X11 R4 Athena Widget Set		*)
(*==============================================================*)


FROM Ctypes IMPORT char_star;

VAR
  accelerators                   : char_star;
  allowHoriz                     : char_star;
  allowResize                    : char_star;
  allowShellResize               : char_star;
  allowVert                      : char_star;
  analog                         : char_star;
  ancestorSensitive              : char_star;
  argc                           : char_star;
  argv                           : char_star;
  autoFill                       : char_star;
  background                     : char_star;
  backgroundPixmap               : char_star;
  backingStore                   : char_star;
  baseHeight                     : char_star;
  baseWidth                      : char_star;
  betweenCursor                  : char_star;
  bitmap                         : char_star;
  border                         : char_star;
  borderColor                    : char_star;
  borderPixmap                   : char_star;
  borderWidth                    : char_star;
  bottom                         : char_star;
  bottomMargin                   : char_star;
  callback                       : char_star;
  checkCommand                   : char_star;
  children                       : char_star;
  chime                          : char_star;
  colormap                       : char_star;
  columnSpacing                  : char_star;
  columnWidth                    : char_star;
  cornerRoundPercent             : char_star;
  createPopupChildProc           : char_star;
  cursor                         : char_star;
  dataCompression                : char_star;
  defaultColumns                 : char_star;
  defaultDistance                : char_star;
  depth                          : char_star;
  destroyCallback                : char_star;
  dialogHOffset                  : char_star;
  dialogVOffset                  : char_star;
  displayCaret                   : char_star;
  displayNonprinting             : char_star;
  displayPosition                : char_star;
  echo                           : char_star;
  editType                       : char_star;
  emptyPixmap                    : char_star;
  emptyPixmapMask                : char_star;
  file                           : char_star;
  flip                           : char_star;
  font                           : char_star;
  forceBars                      : char_star;
  forceColumns                   : char_star;
  foreground                     : char_star;
  fromHoriz                      : char_star;
  fromVert                       : char_star;
  fullPixmap                     : char_star;
  fullPixmapMask                 : char_star;
  function                       : char_star;
  geometry                       : char_star;
  getValue                       : char_star;
  gripCursor                     : char_star;
  gripIndent                     : char_star;
  gripTranslations               : char_star;
  hSpace                         : char_star;
  hand                           : char_star;
  height                         : char_star;
  heightInc                      : char_star;
  highlight                      : char_star;
  highlightThickness             : char_star;
  horizDistance                  : char_star;
  horizontalBetweenCursor        : char_star;
  horizontalGripCursor           : char_star;
  icon                           : char_star;
  iconMask                       : char_star;
  iconName                       : char_star;
  iconNameEncoding               : char_star;
  iconPixmap                     : char_star;
  iconWindow                     : char_star;
  iconX                          : char_star;
  iconY                          : char_star;
  iconic                         : char_star;
  index                          : char_star;
  initialResourcesPersistent     : char_star;
  initialState                   : char_star;
  innerHeight                    : char_star;
  innerWidth                     : char_star;
  innerWindow                    : char_star;
  input                          : char_star;
  insensitiveBorder              : char_star;
  insertPosition                 : char_star;
  internalBorderColor            : char_star;
  internalBorderWidth            : char_star;
  internalHeight                 : char_star;
  internalWidth                  : char_star;
  jumpProc                       : char_star;
  jumpScroll                     : char_star;
  justify                        : char_star;
  knobHeight                     : char_star;
  knobIndent                     : char_star;
  knobPixel                      : char_star;
  knobWidth                      : char_star;
  label                          : char_star;
  labelClass                     : char_star;
  left                           : char_star;
  leftBitmap                     : char_star;
  leftCursor                     : char_star;
  leftMargin                     : char_star;
  length                         : char_star;
  lineWidth                      : char_star;
  list                           : char_star;
  longest                        : char_star;
  lowerCursor                    : char_star;
  lowerRight                     : char_star;
  mappedWhenManaged              : char_star;
  max                            : char_star;
  maxAspectX                     : char_star;
  maxAspectY                     : char_star;
  maxHeight                      : char_star;
  maxWidth                       : char_star;
  menuEntry                      : char_star;
  menuName                       : char_star;
  menuOnScreen                   : char_star;
  min                            : char_star;
  minAspectX                     : char_star;
  minAspectY                     : char_star;
  minHeight                      : char_star;
  minScale                       : char_star;
  minWidth                       : char_star;
  minimumThumb                   : char_star;
  name                           : char_star;
  notify                         : char_star;
  numChildren                    : char_star;
  numberStrings                  : char_star;
  onceOnly                       : char_star;
  orientation                    : char_star;
  overrideRedirect               : char_star;
  padding                        : char_star;
  parameter                      : char_star;
  pasteBuffer                    : char_star;
  pieceSize                      : char_star;
  pixmap                         : char_star;
  popdownCallback                : char_star;
  popupCallback                  : char_star;
  popupOnEntry                   : char_star;
  position                       : char_star;
  preferredPaneSize              : char_star;
  radioData                      : char_star;
  radioGroup                     : char_star;
  refigureMode                   : char_star;
  resizable                      : char_star;
  resize                         : char_star;
  resizeToPreferred              : char_star;
  reverseVideo                   : char_star;
  right                          : char_star;
  rightBitmap                    : char_star;
  rightCursor                    : char_star;
  rightMargin                    : char_star;
  rowHeight                      : char_star;
  rowSpacing                     : char_star;
  saveUnder                      : char_star;
  scale                          : char_star;
  screen                         : char_star;
  scrollDCursor                  : char_star;
  scrollHCursor                  : char_star;
  scrollHorizontal               : char_star;
  scrollLCursor                  : char_star;
  scrollProc                     : char_star;
  scrollRCursor                  : char_star;
  scrollUCursor                  : char_star;
  scrollVCursor                  : char_star;
  scrollVertical                 : char_star;
  selectTypes                    : char_star;
  selection                      : char_star;
  selectionArray                 : char_star;
  sensitive                      : char_star;
  shapeStyle                     : char_star;
  shapeWindow                    : char_star;
  showGrip                       : char_star;
  shown                          : char_star;
  skipAdjust                     : char_star;
  space                          : char_star;
  state                          : char_star;
  stipple                        : char_star;
  string                         : char_star;
  templateResource               : char_star;
  textOptions                    : char_star;
  textSink                       : char_star;
  textSource                     : char_star;
  thickness                      : char_star;
  thumb                          : char_star;
  thumbProc                      : char_star;
  title                          : char_star;
  titleEncoding                  : char_star;
  top                            : char_star;
  topMargin                      : char_star;
  topOfThumb                     : char_star;
  transient                      : char_star;
  transientFor                   : char_star;
  translations                   : char_star;
  type                           : char_star;
  unrealizeCallback              : char_star;
  update                         : char_star;
  upperCursor                    : char_star;
  useBottom                      : char_star;
  useRight                       : char_star;
  useStringInPlace               : char_star;
  vSpace                         : char_star;
  value                          : char_star;
  vertDistance                   : char_star;
  vertSpace                      : char_star;
  verticalBetweenCursor          : char_star;
  verticalGripCursor             : char_star;
  verticalList                   : char_star;
  visual                         : char_star;
  vmunix                         : char_star;
  volume                         : char_star;
  waitForWm                      : char_star;
  width                          : char_star;
  widthInc                       : char_star;
  winGravity                     : char_star;
  window                         : char_star;
  windowGroup                    : char_star;
  wmTimeout                      : char_star;
  wrap                           : char_star;
  x                              : char_star;
  y                              : char_star;


PROCEDURE ForceToLoadAnImplementation ();

END XtN.
