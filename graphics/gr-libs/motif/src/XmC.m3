(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 16 08:09:48 PDT 1995 by kalsow   *)
(*                                                           *)
(* Contributed by Harry George (hgeorge@eskimo.com), 5/16/95 *)

UNSAFE MODULE XmC;
(*Generated from Xm.h by ctom3 on 7/26/93*)

FROM M3toC IMPORT TtoS;
IMPORT XtC;

PROCEDURE Init () =
  BEGIN
    IF (Accelerator # NIL) THEN RETURN END;

    Accelerator := TtoS("Accelerator");
    Accelerators := TtoS("Accelerators");
    AcceleratorText := TtoS("AcceleratorText");
    AdjustLast := TtoS("AdjustLast");
    AdjustMargin := TtoS("AdjustMargin");
    Alignment := TtoS("Alignment");
    ArmCallback := TtoS("ArmCallback");
    Background := TtoS("Background");
    Bitmap := TtoS("Bitmap");
    Boolean := TtoS("Boolean");
    BorderColor := TtoS("BorderColor");
    BorderWidth := TtoS("BorderWidth");
    ButtonAccelerators := TtoS("ButtonAccelerators");
    ButtonAcceleratorText := TtoS("ButtonAcceleratorText");
    ButtonCount := TtoS("ButtonCount");
    ButtonMnemoniccharSets := TtoS("ButtonMnemoniccharSets");
    ButtonMnemonics := TtoS("ButtonMnemonics");
    Buttons := TtoS("Buttons");
    ButtonSet := TtoS("ButtonSet");
    ButtonType := TtoS("ButtonType");
    Callback := TtoS("Callback");
    Children := TtoS("Children");
    Colormap := TtoS("Colormap");
    Color := TtoS("Color");
    CommandWindowLocation := TtoS("CommandWindowLocation");
    Cursor := TtoS("Cursor");
    DefaultFontList := TtoS("DefaultFontList");
    Depth := TtoS("Depth");
    Dimension := TtoS("Dimension");
    DisarmCallback := TtoS("DisarmCallback");
    EditType := TtoS("EditType");
    EntryBorder := TtoS("EntryBorder");
    EntryClass := TtoS("EntryClass");
    EventBindings := TtoS("EventBindings");
    ExposeCallback := TtoS("ExposeCallback");
    File := TtoS("File");
    FillOnSelect := TtoS("FillOnSelect");
    FilterLabelString := TtoS("FilterLabelString");
    Font := TtoS("Font");
    FontList := TtoS("FontList");
    Foreground := TtoS("Foreground");
    Fraction := TtoS("Fraction");
    Function := TtoS("Function");
    Height := TtoS("Height");
    HSpace := TtoS("HSpace");
    Index := TtoS("Index");
    IndicatorOn := TtoS("IndicatorOn");
    IndicatorSize := TtoS("IndicatorSize");
    IndicatorType := TtoS("IndicatorType");
    InitialResourcesPersistent := TtoS("InitialResourcesPersistent");
    InsertPosition := TtoS("InsertPosition");
    interval := TtoS("interval");
    IsAligned := TtoS("IsAligned");
    IsHomogeneous := TtoS("IsHomogeneous");
    Justify := TtoS("Justify");
    Label := TtoS("Label");
    LabelInsensitivePixmap := TtoS("LabelInsensitivePixmap");
    LabelPixmap := TtoS("LabelPixmap");
    LabelString := TtoS("LabelString");
    LabelType := TtoS("LabelType");
    Length := TtoS("Length");
    ListUpdated := TtoS("ListUpdated");
    MappedWhenManaged := TtoS("MappedWhenManaged");
    MappingDelay := TtoS("MappingDelay");
    Margin := TtoS("Margin");
    MarginHeight := TtoS("MarginHeight");
    MarginWidth := TtoS("MarginWidth");
    MarginLeft := TtoS("MarginLeft");
    MarginRight := TtoS("MarginRight");
    MarginTop := TtoS("MarginTop");
    MarginBottom := TtoS("MarginBottom");
    MenuEntry := TtoS("MenuEntry");
    MenuPost := TtoS("MenuPost");
    MenuWidget := TtoS("MenuWidget");
    Mnemonic := TtoS("Mnemonic");
    MnemoniccharSet := TtoS("MnemoniccharSet");
    NavigationType := TtoS("NavigationType");
    Notify := TtoS("Notify");
    NumChildren := TtoS("NumChildren");
    NumColumns := TtoS("NumColumns");
    OptionLabel := TtoS("OptionLabel");
    OptionMnemonic := TtoS("OptionMnemonic");
    Orientation := TtoS("Orientation");
    Packing := TtoS("Packing");
    Parameter := TtoS("Parameter");
    Pixmap := TtoS("Pixmap");
    PopupEnabled := TtoS("PopupEnabled");
    Position := TtoS("Position");
    PostFromButton := TtoS("PostFromButton");
    PostFromCount := TtoS("PostFromCount");
    PostFromList := TtoS("PostFromList");
    RadioAlwaysOne := TtoS("RadioAlwaysOne");
    RadioBehavior := TtoS("RadioBehavior");
    RecomputeSize := TtoS("RecomputeSize");
    Resize := TtoS("Resize");
    ResizeCallback := TtoS("ResizeCallback");
    ReverseVideo := TtoS("ReverseVideo");
    RowColumnType := TtoS("RowColumnType");
    ScaleMultiple := TtoS("ScaleMultiple");
    Screen := TtoS("Screen");
    ScrollProc := TtoS("ScrollProc");
    ScrollDCursor := TtoS("ScrollDCursor");
    ScrollHCursor := TtoS("ScrollHCursor");
    ScrollLCursor := TtoS("ScrollLCursor");
    ScrollRCursor := TtoS("ScrollRCursor");
    ScrollUCursor := TtoS("ScrollUCursor");
    ScrollVCursor := TtoS("ScrollVCursor");
    SelectColor := TtoS("SelectColor");
    Selection := TtoS("Selection");
    Sensitive := TtoS("Sensitive");
    SelectionArray := TtoS("SelectionArray");
    SelectInsensitivePixmap := TtoS("SelectInsensitivePixmap");
    SelectPixmap := TtoS("SelectPixmap");
    Set := TtoS("Set");
    SimpleCheckBox := TtoS("SimpleCheckBox");
    SimpleMenuBar := TtoS("SimpleMenuBar");
    SimplePopupMenu := TtoS("SimplePopupMenu");
    SimplePulldownMenu := TtoS("SimplePulldownMenu");
    SimpleOptionMenu := TtoS("SimpleOptionMenu");
    SimpleRadioBox := TtoS("SimpleRadioBox");
    Space := TtoS("Space");
    Spacing := TtoS("Spacing");
    String := TtoS("String");
    StringDirection := TtoS("StringDirection");
    TextOptions := TtoS("TextOptions");
    TextPosition := TtoS("TextPosition");
    TextSink := TtoS("TextSink");
    TextSource := TtoS("TextSource");
    Thickness := TtoS("Thickness");
    Thumb := TtoS("Thumb");
    TroughColor := TtoS("TroughColor");
    Translations := TtoS("Translations");
    TraversalType := TtoS("TraversalType");
    UnselectPixmap := TtoS("UnselectPixmap");
    Value := TtoS("Value");
    ValueChangedCallback := TtoS("ValueChangedCallback");
    VisibleWhenOff := TtoS("VisibleWhenOff");
    VSpace := TtoS("VSpace");
    WhichButton := TtoS("WhichButton");
    Width := TtoS("Width");
    Window := TtoS("Window");
    XmString := TtoS("XmString");
    X := TtoS("X");
    Y := TtoS("Y");
    IconName := TtoS("IconName");
    IconPixmap := TtoS("IconPixmap");
    IconWindow := TtoS("IconWindow");
    IconMask := TtoS("IconMask");
    WindowGroup := TtoS("WindowGroup");
    SaveUnder := TtoS("SaveUnder");
    Transient := TtoS("Transient");
    OverrideRedirect := TtoS("OverrideRedirect");
    AllowShellResize := TtoS("AllowShellResize");
    CreatePopupChildProc := TtoS("CreatePopupChildProc");
    Title := TtoS("Title");
    Argc := TtoS("Argc");
    Argv := TtoS("Argv");
    IconX := TtoS("IconX");
    IconY := TtoS("IconY");
    Input := TtoS("Input");
    Iconic := TtoS("Iconic");
    InitialState := TtoS("InitialState");
    Geometry := TtoS("Geometry");
    MinWidth := TtoS("MinWidth");
    MinHeight := TtoS("MinHeight");
    MaxWidth := TtoS("MaxWidth");
    MaxHeight := TtoS("MaxHeight");
    WidthInc := TtoS("WidthInc");
    HeightInc := TtoS("HeightInc");
    MinAspectY := TtoS("MinAspectY");
    MaxAspectY := TtoS("MaxAspectY");
    MinAspectX := TtoS("MinAspectX");
    MaxAspectX := TtoS("MaxAspectX");
    WmTimeout := TtoS("WmTimeout");
    WaitForWm := TtoS("Waitforwm");
    Foreground := TtoS("Foreground");
    BackgroundPixmap := TtoS("BackgroundPixmap");
    TraversalOn := TtoS("TraversalOn");
    HighlightOnEnter := TtoS("HighlightOnEnter");
    SizePolicy := TtoS("SizePolicy");
    HighlightThickness := TtoS("HighlightThickness");
    HighlightColor := TtoS("HighlightColor");
    HighlightPixmap := TtoS("HighlightPixmap");
    ShadowThickness := TtoS("ShadowThickness");
    TopShadowColor := TtoS("TopShadowColor");
    TopShadowPixmap := TtoS("TopShadowPixmap");
    BottomShadowColor := TtoS("BottomShadowColor");
    BottomShadowPixmap := TtoS("BottomShadowPixmap");
    UnitType := TtoS("UnitType");
    UserData := TtoS("UserData");
    MaxValue := TtoS("MaxValue");
    RubberPositioning := TtoS("RubberPositioning");
    ResizePolicy := TtoS("ResizePolicy");
    Attachment := TtoS("Attachment");
    Widget := TtoS("Widget");
    Offset := TtoS("Offset");
    FillOnArm := TtoS("FillOnArm");
    ArmColor := TtoS("ArmColor");
    ArmPixmap := TtoS("ArmPixmap");
    ShowAsDefault := TtoS("ShowAsDefault");
    DefaultButtonShadowThickness := TtoS("DefaultButtonShadowThickness");
    MultiClick := TtoS("MultiClick");
    PushButtonEnabled := TtoS("PushButtonEnabled");
    ShadowType := TtoS("ShadowType");
    ArrowDirection := TtoS("ArrowDirection");
    SeparatorType := TtoS("SeparatorType");
    Minimum := TtoS("Minimum");
    Maximum := TtoS("Maximum");
    SliderSize := TtoS("SliderSize");
    ShowArrows := TtoS("ShowArrows");
    ProcessingDirection := TtoS("ProcessingDirection");
    Increment := TtoS("Increment");
    PageIncrement := TtoS("PageIncrement");
    InitialDelay := TtoS("InitialDelay");
    RepeatDelay := TtoS("RepeatDelay");
    ListSpacing := TtoS("ListSpacing");
    ListMarginWidth := TtoS("ListMarginWidth");
    ListMarginHeight := TtoS("ListMarginHeight");
    Items := TtoS("Items");
    ItemCount := TtoS("ItemCount");
    SelectedItems := TtoS("SelectedItems");
    SelectedItemCount := TtoS("SelectedItemCount");
    VisibleItemCount := TtoS("VisibleItemCount");
    SelectionPolicy := TtoS("SelectionPolicy");
    ListSizePolicy := TtoS("ListSizePolicy");
    DoubleClickinterval := TtoS("DoubleClickinterval");
    AutomaticSelection := TtoS("AutomaticSelection");
    TopItemPosition := TtoS("TopItemPosition");
    HorizontalScrollBar := TtoS("HorizontalScrollBar");
    VerticalScrollBar := TtoS("VerticalScrollBar");
    WorkWindow := TtoS("WorkWindow");
    ClipWindow := TtoS("ClipWindow");
    ScrollingPolicy := TtoS("ScrollingPolicy");
    VisualPolicy := TtoS("VisualPolicy");
    ScrollBarDisplayPolicy := TtoS("ScrollBarDisplayPolicy");
    ScrollBarPlacement := TtoS("ScrollBarPlacement");
    UpdateSliderSize := TtoS("UpdateSliderSize");
    ScrolledWindowMarginHeight := TtoS("ScrolledWindowMarginHeight");
    ScrolledWindowMarginWidth := TtoS("ScrolledWindowMarginWidth");
    CommandWindow := TtoS("CommandWindow");
    MenuBar := TtoS("MenuBar");
    MessageWindow := TtoS("MessageWindow");
    MainWindowMarginHeight := TtoS("MainWindowMarginHeight");
    MainWindowMarginWidth := TtoS("MainWindowMarginWidth");
    ShowSeparator := TtoS("ShowSeparator");
    Source := TtoS("Source");
    OutputCreate := TtoS("OutputCreate");
    InputCreate := TtoS("InputCreate");
    AutoShowCursorPosition := TtoS("AutoShowCursorPosition");
    CursorPosition := TtoS("CursorPosition");
    Editable := TtoS("Editable");
    MaxLength := TtoS("MaxLength");
    VerifyBell := TtoS("VerifyBell");
    WordWrap := TtoS("WordWrap");
    BlinkRate := TtoS("BlinkRate");
    ResizeWidth := TtoS("ResizeWidth");
    ResizeHeight := TtoS("ResizeHeight");
    Scroll := TtoS("Scroll");
    ScrollSide := TtoS("ScrollSide");
    CursorPositionVisible := TtoS("CursorPositionVisible");
    Columns := TtoS("Columns");
    Rows := TtoS("Rows");
    SelectThreshold := TtoS("SelectThreshold");
    SelectionArrayCount := TtoS("SelectionArrayCount");
    PendingDelete := TtoS("PendingDelete");
    EditMode := TtoS("EditMode");
    topcharacter := TtoS("Topcharacter");
    SeparatorOn := TtoS("SeparatorOn");
    SashIndent := TtoS("SashIndent");
    SashWidth := TtoS("SashWidth");
    SashHeight := TtoS("SashHeight");
    PaneMinimum := TtoS("PaneMinimum");
    PaneMaximum := TtoS("PaneMaximum");
    OkLabelString := TtoS("OkLabelString");
    CancelLabelString := TtoS("CancelLabelString");
    HelpLabelString := TtoS("HelpLabelString");
    ApplyLabelString := TtoS("ApplyLabelString");
    SelectionLabelString := TtoS("SelectionLabelString");
    ListLabelString := TtoS("ListLabelString");
    PromptString := TtoS("PromptString");
    MessageString := TtoS("MessageString");
    ButtonFontList := TtoS("ButtonFontList");
    LabelFontList := TtoS("LabelFontList");
    TextFontList := TtoS("TextFontList");
    AllowOverlap := TtoS("AllowOverlap");
    DefaultPosition := TtoS("DefaultPosition");
    AutoUnmanage := TtoS("AutoUnmanage");
    DialogTitle := TtoS("DialogTitle");
    NoResize := TtoS("NoResize");
    DialogStyle := TtoS("DialogStyle");
    MustMatch := TtoS("MustMatch");
    NoMatchString := TtoS("NoMatchString");
    Directory := TtoS("Directory");
    Pattern := TtoS("Pattern");
    DirSpec := TtoS("DirSpec");
    DirMask := TtoS("DirMask");
    FileTypeMask := TtoS("FileTypeMask");
    DirectoryValid := TtoS("DirectoryValid");
    DirListItems := TtoS("DirListItems");
    DirListItemCount := TtoS("DirListItemCount");
    DirListLabelString := TtoS("DirListLabelString");
    FileListItems := TtoS("FileListItems");
    FileListItemCount := TtoS("FileListItemCount");
    FileListLabelString := TtoS("FileListLabelString");
    QualifySearchDataProc := TtoS("QualifySearchDataProc");
    DirSearchProc := TtoS("DirSearchProc");
    FileSearchProc := TtoS("FileSearchProc");
    MaxItems := TtoS("MaxItems");
    TextValue := TtoS("TextValue"          (* used in text widget *));
    TextString := TtoS("TextString");
    DefaultButtonType := TtoS("DefaultButtonType");
    MinimizeButtons := TtoS("MinimizeButtons");
    DialogType := TtoS("DialogType");
    ScaleWidth := TtoS("ScaleWidth");
    ScaleHeight := TtoS("ScaleHeight");
    DecimalPoints := TtoS("DecimalPoints");
    ShowValue := TtoS("ShowValue");
    TitleString := TtoS("TitleString");

    (*load from Xt:*)
    ReadOnly := XtC.ReadOnly;
    Visual := XtC.Visual;
    IconNameEncoding := XtC.IconNameEncoding;
    TransientFor := XtC.TransientFor;
    BaseHeight := XtC.BaseHeight;
    BaseWidth := XtC.BaseWidth;
    TitleEncoding := XtC.TitleEncoding;
    WinGravity := XtC.WinGravity;
  END Init;

BEGIN
  Init ();
END XmC.