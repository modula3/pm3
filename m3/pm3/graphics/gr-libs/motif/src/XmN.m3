(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 16 08:02:30 PDT 1995 by kalsow   *)
(*                                                           *)
(* Contributed by Harry George (hgeorge@eskimo.com), 5/16/95 *)

UNSAFE MODULE XmN;
(*Generated from Xm.h by ctom3 on 7/26/93*)

FROM M3toC IMPORT TtoS;
IMPORT XtN;

PROCEDURE Init () =
  BEGIN
    IF (accelerator # NIL) THEN RETURN END;

    accelerator := TtoS("accelerator");
    accelerators := TtoS("accelerators");
    acceleratorText := TtoS("acceleratorText");
    adjustLast := TtoS("adjustLast");
    adjustMargin := TtoS("adjustMargin");
    alignment := TtoS("alignment");
    ancestorSensitive := TtoS("ancestorSensitive");
    armCallback := TtoS("armCallback");
    background := TtoS("background");
    backgroundPixmap := TtoS("backgroundPixmap");
    bitmap := TtoS("bitmap");
    borderColor := TtoS("borderColor");
    border := TtoS("borderColor");
    borderPixmap := TtoS("borderPixmap");
    borderWidth := TtoS("borderWidth");
    buttonAccelerators := TtoS("buttonAccelerators");
    buttonAcceleratorText := TtoS("buttonAcceleratorText");
    buttonCount := TtoS("buttonCount");
    buttonMnemoniccharSets := TtoS("buttonMnemoniccharSets");
    buttonMnemonics := TtoS("buttonMnemonics");
    buttons := TtoS("buttons");
    buttonSet := TtoS("buttonSet");
    buttonType := TtoS("buttonType");
    cascadePixmap := TtoS("cascadePixmap");
    cascadingCallback := TtoS("cascadingCallback");
    children := TtoS("children");
    colormap := TtoS("colormap");
    commandWindowLocation := TtoS("commandWindowLocation");
    defaultFontList := TtoS("defaultFontList");
    depth := TtoS("depth");
    destroyCallback := TtoS("destroyCallback");
    disarmCallback := TtoS("disarmCallback");
    editType := TtoS("editType");
    entryAlignment := TtoS("entryAlignment");
    entryBorder := TtoS("entryBorder");
    entryClass := TtoS("entryClass");
    entryCallback := TtoS("entryCallback");
    exposeCallback := TtoS("exposeCallback");
    file := TtoS("file");
    fillOnSelect := TtoS("fillOnSelect");
    filterLabelString := TtoS("filterLabelString");
    font := TtoS("font");
    fontList := TtoS("fontList");
    forceBars := TtoS("forceBars");
    foreground := TtoS("foreground");
    function := TtoS("function");
    height := TtoS("height");
    highlight := TtoS("highlight");
    index := TtoS("index");
    indicatorOn := TtoS("indicatorOn");
    indicatorSize := TtoS("indicatorSize");
    indicatorType := TtoS("indicatorType");
    initialResourcesPersistent := TtoS("initialResourcesPersistent");
    innerHeight := TtoS("innerHeight");
    innerWidth := TtoS("innerWidth");
    innerWindow := TtoS("innerWindow");
    insertPosition := TtoS("insertPosition");
    internalHeight := TtoS("internalHeight");
    internalWidth := TtoS("internalWidth");
    isAligned := TtoS("isAligned");
    isHomogeneous := TtoS("isHomogeneous");
    jumpProc := TtoS("jumpProc");
    justify := TtoS("justify");
    labelInsensitivePixmap := TtoS("labelInsensitivePixmap");
    labelPixmap := TtoS("labelPixmap");
    labelString := TtoS("labelString");
    labelType := TtoS("labelType");
    length := TtoS("length");
    listUpdated := TtoS("listUpdated");
    lowerRight := TtoS("lowerRight");
    mapCallback := TtoS("mapCallback");
    mappedWhenManaged := TtoS("mappedWhenManaged");
    mappingDelay := TtoS("mappingDelay");
    marginHeight := TtoS("marginHeight");
    marginTop := TtoS("marginTop");
    marginBottom := TtoS("marginBottom");
    marginWidth := TtoS("marginWidth");
    marginRight := TtoS("marginRight");
    marginLeft := TtoS("marginLeft");
    menuAccelerator := TtoS("menuAccelerator");
    menuCursor := TtoS("menuCursor");
    menuEntry := TtoS("menuEntry");
    menuHelpWidget := TtoS("menuHelpWidget");
    menuHistory := TtoS("menuHistory");
    menuPost := TtoS("menuPost");
    mnemonic := TtoS("mnemonic");
    mnemoniccharSet := TtoS("mnemoniccharSet");
    name := TtoS("name");
    navigationType := TtoS("navigationType");
    notify := TtoS("notify");
    numChildren := TtoS("numChildren");
    numColumns := TtoS("numColumns");
    optionLabel := TtoS("optionLabel");
    optionMnemonic := TtoS("optionMnemonic");
    orientation := TtoS("orientation");
    packing := TtoS("packing");
    parameter := TtoS("parameter");
    popdownCallback := TtoS("popdownCallback");
    popupCallback := TtoS("popupCallback");
    popupEnabled := TtoS("popupEnabled");
    postFromButton := TtoS("postFromButton");
    postFromCount := TtoS("postFromCount");
    postFromList := TtoS("postFromList");
    radioAlwaysOne := TtoS("radioAlwaysOne");
    radioBehavior := TtoS("radioBehavior");
    recomputeSize := TtoS("recomputeSize");
    resize := TtoS("resize");
    resizeCallback := TtoS("resizeCallback");
    reverseVideo := TtoS("reverseVideo");
    rowColumnType := TtoS("rowColumnType");
    scaleMultiple := TtoS("scaleMultiple");
    screen := TtoS("screen");
    scrollProc := TtoS("scrollProc");
    scrollDCursor := TtoS("scrollDCursor");
    scrollHCursor := TtoS("scrollHCursor");
    scrollLCursor := TtoS("scrollLCursor");
    scrollRCursor := TtoS("scrollRCursor");
    scrollUCursor := TtoS("scrollUCursor");
    scrollVCursor := TtoS("scrollVCursor");
    selectColor := TtoS("selectColor");
    selection := TtoS("selection");
    selectionArray := TtoS("selectionArray");
    selectInsensitivePixmap := TtoS("selectInsensitivePixmap");
    selectPixmap := TtoS("selectPixmap");
    sensitive := TtoS("sensitive");
    set := TtoS("set");
    shadow := TtoS("shadow");
    shown := TtoS("shown");
    simpleCallback := TtoS("simpleCallback");
    space := TtoS("space");
    spacing := TtoS("spacing");
    string := TtoS("string");
    stringDirection := TtoS("stringDirection");
    subMenuId := TtoS("subMenuId");
    textOptions := TtoS("textOptions");
    textSink := TtoS("textSink");
    textSource := TtoS("textSource");
    thickness := TtoS("thickness");
    thumb := TtoS("thumb");
    thumbProc := TtoS("thumbProc");
    top := TtoS("top");
    translations := TtoS("translations");
    traversalType := TtoS("traversalType");
    troughColor := TtoS("troughColor");
    unmapCallback := TtoS("unmapCallback");
    unselectPixmap := TtoS("unselectPixmap");
    update := TtoS("update");
    useBottom := TtoS("useBottom");
    useRight := TtoS("useRight");
    value := TtoS("value");
    visibleWhenOff := TtoS("visibleWhenOff");
    whichButton := TtoS("whichButton");
    width := TtoS("width");
    window := TtoS("window");
    x := TtoS("x");
    y := TtoS("y");
    iconName := TtoS("iconName");
    iconPixmap := TtoS("iconPixmap");
    iconWindow := TtoS("iconWindow");
    iconMask := TtoS("iconMask");
    windowGroup := TtoS("windowGroup");
    saveUnder := TtoS("saveUnder");
    transient := TtoS("transient");
    overrideRedirect := TtoS("overrideRedirect");
    allowShellResize := TtoS("allowShellResize");
    createPopupChildProc := TtoS("createPopupChildProc");
    title := TtoS("title");
    argc := TtoS("argc");
    argv := TtoS("argv");
    iconX := TtoS("iconX");
    iconY := TtoS("iconY");
    input := TtoS("input");
    iconic := TtoS("iconic");
    initialState := TtoS("initialState");
    geometry := TtoS("geometry");
    minWidth := TtoS("minWidth");
    minHeight := TtoS("minHeight");
    maxWidth := TtoS("maxWidth");
    maxHeight := TtoS("maxHeight");
    widthInc := TtoS("widthInc");
    heightInc := TtoS("heightInc");
    minAspectY := TtoS("minAspectY");
    maxAspectY := TtoS("maxAspectY");
    minAspectX := TtoS("minAspectX");
    maxAspectX := TtoS("maxAspectX");
    wmTimeout := TtoS("wmTimeout");
    waitForWm := TtoS("waitforwm");
    foreground := TtoS("foreground");
    traversalOn := TtoS("traversalOn");
    highlightOnEnter := TtoS("highlightOnEnter");
    sizePolicy := TtoS("sizePolicy");
    highlightThickness := TtoS("highlightThickness");
    highlightColor := TtoS("highlightColor");
    highlightPixmap := TtoS("highlightPixmap");
    shadowThickness := TtoS("shadowThickness");
    topShadowColor := TtoS("topShadowColor");
    topShadowPixmap := TtoS("topShadowPixmap");
    bottomShadowColor := TtoS("bottomShadowColor");
    bottomShadowPixmap := TtoS("bottomShadowPixmap");
    unitType := TtoS("unitType");
    helpCallback := TtoS("helpCallback");
    userData := TtoS("userData");
    childPosition := TtoS("childPosition");
    horizontalSpacing := TtoS("horizontalSpacing");
    verticalSpacing := TtoS("verticalSpacing");
    fractionBase := TtoS("fractionBase");
    rubberPositioning := TtoS("rubberPositioning");
    resizePolicy := TtoS("resizePolicy");
    topAttachment := TtoS("topAttachment");
    bottomAttachment := TtoS("bottomAttachment");
    leftAttachment := TtoS("leftAttachment");
    rightAttachment := TtoS("rightAttachment");
    topWidget := TtoS("topWidget");
    bottomWidget := TtoS("bottomWidget");
    leftWidget := TtoS("leftWidget");
    rightWidget := TtoS("rightWidget");
    topPosition := TtoS("topPosition");
    bottomPosition := TtoS("bottomPosition");
    leftPosition := TtoS("leftPosition");
    rightPosition := TtoS("rightPosition");
    topOffset := TtoS("topOffset");
    bottomOffset := TtoS("bottomOffset");
    leftOffset := TtoS("leftOffset");
    rightOffset := TtoS("rightOffset");
    resizable := TtoS("resizable");
    fillOnArm := TtoS("fillOnArm");
    armColor := TtoS("armColor");
    armPixmap := TtoS("armPixmap");
    showAsDefault := TtoS("showAsDefault");
    defaultButtonShadowThickness := TtoS("defaultButtonShadowThickness");
    multiClick := TtoS("multiClick");
    pushButtonEnabled := TtoS("pushButtonEnabled");
    shadowType := TtoS("shadowType");
    arrowDirection := TtoS("arrowDirection");
    activateCallback := TtoS("activateCallback");
    helpCallback := TtoS("helpCallback");
    margin := TtoS("margin");
    separatorType := TtoS("separatorType");
    minimum := TtoS("minimum");
    maximum := TtoS("maximum");
    sliderSize := TtoS("sliderSize");
    showArrows := TtoS("showArrows");
    processingDirection := TtoS("processingDirection");
    increment := TtoS("increment");
    pageIncrement := TtoS("pageIncrement");
    initialDelay := TtoS("initialDelay");
    repeatDelay := TtoS("repeatDelay");
    valueChangedCallback := TtoS("valueChangedCallback");
    incrementCallback := TtoS("incrementCallback");
    decrementCallback := TtoS("decrementCallback");
    pageIncrementCallback := TtoS("pageIncrementCallback");
    pageDecrementCallback := TtoS("pageDecrementCallback");
    toTopCallback := TtoS("toTopCallback");
    toBottomCallback := TtoS("toBottomCallback");
    dragCallback := TtoS("dragCallback");
    listSpacing := TtoS("listSpacing");
    listMarginWidth := TtoS("listMarginWidth");
    listMarginHeight := TtoS("listMarginHeight");
    items := TtoS("items");
    itemCount := TtoS("itemCount");
    selectedItems := TtoS("selectedItems");
    selectedItemCount := TtoS("selectedItemCount");
    visibleItemCount := TtoS("visibleItemCount");
    selectionPolicy := TtoS("selectionPolicy");
    listSizePolicy := TtoS("listSizePolicy");
    doubleClickinterval := TtoS("doubleClickinterval");
    singleSelectionCallback := TtoS("singleSelectionCallback");
    multipleSelectionCallback := TtoS("multipleSelectionCallback");
    extendedSelectionCallback := TtoS("extendedSelectionCallback");
    browseSelectionCallback := TtoS("browseSelectionCallback");
    defaultActionCallback := TtoS("defaultActionCallback");
    automaticSelection := TtoS("automaticSelection");
    topItemPosition := TtoS("topItemPosition");
    horizontalScrollBar := TtoS("horizontalScrollBar");
    verticalScrollBar := TtoS("verticalScrollBar");
    workWindow := TtoS("workWindow");
    clipWindow := TtoS("clipWindow");
    scrollingPolicy := TtoS("scrollingPolicy");
    visualPolicy := TtoS("visualPolicy");
    scrollBarDisplayPolicy := TtoS("scrollBarDisplayPolicy");
    scrollBarPlacement := TtoS("scrollBarPlacement");
    updateSliderSize := TtoS("updateSliderSize");
    scrolledWindowMarginHeight := TtoS("scrolledWindowMarginHeight");
    scrolledWindowMarginWidth := TtoS("scrolledWindowMarginWidth");
    commandWindow := TtoS("commandWindow");
    menuBar := TtoS("menuBar");
    messageWindow := TtoS("messageWindow");
    mainWindowMarginHeight := TtoS("mainWindowMarginHeight");
    mainWindowMarginWidth := TtoS("mainWindowMarginWidth");
    showSeparator := TtoS("showSeparator");
    source := TtoS("source");
    outputCreate := TtoS("outputCreate");
    inputCreate := TtoS("inputCreate");
    autoShowCursorPosition := TtoS("autoShowCursorPosition");
    cursorPosition := TtoS("cursorPosition");
    editable := TtoS("editable");
    maxLength := TtoS("maxLength");
    focusCallback := TtoS("focusCallback");
    losingFocusCallback := TtoS("losingFocusCallback");
    modifyVerifyCallback := TtoS("modifyVerifyCallback");
    motionVerifyCallback := TtoS("motionVerifyCallback");
    gainPrimaryCallback := TtoS("gainPrimaryCallback");
    losePrimaryCallback := TtoS("losePrimaryCallback");
    verifyBell := TtoS("verifyBell");
    wordWrap := TtoS("wordWrap");
    blinkRate := TtoS("blinkRate");
    resizeWidth := TtoS("resizeWidth");
    resizeHeight := TtoS("resizeHeight");
    scrollHorizontal := TtoS("scrollHorizontal");
    scrollVertical := TtoS("scrollVertical");
    scrollLeftSide := TtoS("scrollLeftSide");
    scrollTopSide := TtoS("scrollTopSide");
    cursorPositionVisible := TtoS("cursorPositionVisible");
    toPositionCallback := TtoS("toPositionCallback");
    columns := TtoS("columns");
    rows := TtoS("rows");
    selectThreshold := TtoS("selectThreshold");
    selectionArrayCount := TtoS("selectionArrayCount");
    pendingDelete := TtoS("pendingDelete");
    editMode := TtoS("editMode");
    topcharacter := TtoS("topcharacter");
    refigureMode := TtoS("refigureMode");
    separatorOn := TtoS("separatorOn");
    sashIndent := TtoS("sashIndent");
    sashWidth := TtoS("sashWidth");
    sashHeight := TtoS("sashHeight");
    sashShadowThickness := TtoS("sashShadowThickness");
    allowResize := TtoS("allowResize");
    skipAdjust := TtoS("skipAdjust");
    paneMinimum := TtoS("paneMinimum");
    paneMaximum := TtoS("paneMaximum");
    inputCallback := TtoS("inputCallback");
    okCallback := TtoS("okCallback");
    cancelCallback := TtoS("cancelCallback");
    applyCallback := TtoS("applyCallback");
    noMatchCallback := TtoS("noMatchCallback");
    commandEnteredCallback := TtoS("commandEnteredCallback");
    commandChangedCallback := TtoS("commandChangedCallback");
    okLabelString := TtoS("okLabelString");
    cancelLabelString := TtoS("cancelLabelString");
    helpLabelString := TtoS("helpLabelString");
    applyLabelString := TtoS("applyLabelString");
    selectionLabelString := TtoS("selectionLabelString");
    listLabelString := TtoS("listLabelString");
    promptString := TtoS("promptString");
    defaultButton := TtoS("defaultButton");
    cancelButton := TtoS("cancelButton");
    buttonFontList := TtoS("buttonFontList");
    labelFontList := TtoS("labelFontList");
    textFontList := TtoS("textFontList");
    textTranslations := TtoS("textTranslations");
    allowOverlap := TtoS("allowOverlap");
    defaultPosition := TtoS("defaultPosition");
    autoUnmanage := TtoS("autoUnmanage");
    allowShellResize := TtoS("allowShellResize");
    dialogTitle := TtoS("dialogTitle");
    noResize := TtoS("noResize");
    dialogStyle := TtoS("dialogStyle");
    mustMatch := TtoS("mustMatch");
    noMatchString := TtoS("noMatchString");
    directory := TtoS("directory");
    pattern := TtoS("pattern");
    dirSpec := TtoS("dirSpec");
    dirMask := TtoS("dirMask");
    fileTypeMask := TtoS("fileTypeMask");
    directoryValid := TtoS("directoryValid");
    dirListItems := TtoS("dirListItems");
    dirListItemCount := TtoS("dirListItemCount");
    dirListLabelString := TtoS("dirListLabelString");
    fileListItems := TtoS("fileListItems");
    fileListItemCount := TtoS("fileListItemCount");
    fileListLabelString := TtoS("fileListLabelString");
    qualifySearchDataProc := TtoS("qualifySearchDataProc");
    dirSearchProc := TtoS("dirSearchProc");
    fileSearchProc := TtoS("fileSearchProc");
    listItems := TtoS("listItems");
    listItemCount := TtoS("listItemCount");
    listVisibleItemCount := TtoS("listVisibleItemCount");
    historyItems := TtoS("historyItems");
    historyItemCount := TtoS("historyItemCount");
    historyVisibleItemCount := TtoS("historyVisibleItemCount");
    historyMaxItems := TtoS("historyMaxItems");
    textAccelerators := TtoS("textAccelerators");
    textValue := TtoS("textValue"          (* used in text widget *));
    textString := TtoS("textString");
    textColumns := TtoS("textColumns");
    command := TtoS("command");
    defaultButtonType := TtoS("defaultButtonType");
    minimizeButtons := TtoS("minimizeButtons");
    messageString := TtoS("messageString");
    messageAlignment := TtoS("messageAlignment");
    symbolPixmap := TtoS("symbolPixmap");
    dialogType := TtoS("dialogType");
    scaleWidth := TtoS("scaleWidth");
    scaleHeight := TtoS("scaleHeight");
    decimalPoints := TtoS("decimalPoints");
    showValue := TtoS("showValue");
    titleString := TtoS("titleString");

    (*load from XtN*)
    visual := XtN.visual;
    iconNameEncoding := XtN.iconNameEncoding;
    transientFor := XtN.transientFor;
    baseHeight := XtN.baseHeight;
    baseWidth := XtN.baseWidth;
    titleEncoding := XtN.titleEncoding;
    winGravity := XtN.winGravity;
  END Init;

BEGIN
  Init ();
END XmN.
