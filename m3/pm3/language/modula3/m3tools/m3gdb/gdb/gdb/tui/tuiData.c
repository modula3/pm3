/* TUI data manipulation routines.
   Copyright 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Hewlett-Packard Company.

   This file is part of GDB.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "defs.h"
#include "tui.h"
#include "tuiData.h"
#include "tuiGeneralWin.h"

/****************************
** GLOBAL DECLARATIONS
****************************/
TuiWinInfoPtr winList[MAX_MAJOR_WINDOWS];

/***************************
** Private Definitions
****************************/
#define FILE_WIDTH   30
#define PROC_WIDTH   40
#define LINE_WIDTH   4
#define PC_WIDTH     8

/***************************
** Private data
****************************/
static char *_tuiNullStr = TUI_NULL_STR;
static char *_tuiBlankStr = "   ";
static char *_tuiLocationStr = "  >";
static char *_tuiBreakStr = " * ";
static char *_tuiBreakLocationStr = " *>";
static TuiLayoutType _currentLayout = UNDEFINED_LAYOUT;
static int _termHeight, _termWidth;
static int _historyLimit = DEFAULT_HISTORY_COUNT;
static TuiGenWinInfo _locator;
static TuiGenWinInfo _execInfo[2];
static TuiWinInfoPtr _srcWinList[2];
static TuiList _sourceWindows =
{(OpaqueList) _srcWinList, 0};
static int _defaultTabLen = DEFAULT_TAB_LEN;
static TuiWinInfoPtr _winWithFocus = (TuiWinInfoPtr) NULL;
static TuiLayoutDef _layoutDef =
{SRC_WIN,			/* displayMode */
 FALSE,				/* split */
 TUI_UNDEFINED_REGS,		/* regsDisplayType */
 TUI_SFLOAT_REGS};		/* floatRegsDisplayType */
static int _winResized = FALSE;


/*********************************
** Static function forward decls
**********************************/
static void freeContent (TuiWinContent, int, TuiWinType);
static void freeContentElements (TuiWinContent, int, TuiWinType);



/*********************************
** PUBLIC FUNCTIONS
**********************************/

/******************************************
** ACCESSORS & MUTATORS FOR PRIVATE DATA
******************************************/

/*
   ** tuiWinResized().
   **        Answer a whether the terminal window has been resized or not
 */
int
tuiWinResized (void)
{
  return _winResized;
}				/* tuiWinResized */


/*
   ** tuiSetWinResized().
   **        Set a whether the terminal window has been resized or not
 */
void
tuiSetWinResizedTo (int resized)
{
  _winResized = resized;

  return;
}				/* tuiSetWinResizedTo */


/*
   ** tuiLayoutDef().
   **        Answer a pointer to the current layout definition
 */
TuiLayoutDefPtr
tuiLayoutDef (void)
{
  return &_layoutDef;
}				/* tuiLayoutDef */


/*
   ** tuiWinWithFocus().
   **        Answer the window with the logical focus
 */
TuiWinInfoPtr
tuiWinWithFocus (void)
{
  return _winWithFocus;
}				/* tuiWinWithFocus */


/*
   ** tuiSetWinWithFocus().
   **        Set the window that has the logical focus
 */
void
tuiSetWinWithFocus (TuiWinInfoPtr winInfo)
{
  _winWithFocus = winInfo;

  return;
}				/* tuiSetWinWithFocus */


/*
   ** tuiDefaultTabLen().
   **        Answer the length in chars, of tabs
 */
int
tuiDefaultTabLen (void)
{
  return _defaultTabLen;
}				/* tuiDefaultTabLen */


/*
   ** tuiSetDefaultTabLen().
   **        Set the length in chars, of tabs
 */
void
tuiSetDefaultTabLen (int len)
{
  _defaultTabLen = len;

  return;
}				/* tuiSetDefaultTabLen */


/*
   ** currentSourceWin()
   **        Accessor for the current source window.  Usually there is only
   **        one source window (either source or disassembly), but both can
   **        be displayed at the same time.
 */
TuiListPtr
sourceWindows (void)
{
  return &_sourceWindows;
}				/* currentSourceWindows */


/*
   ** clearSourceWindows()
   **        Clear the list of source windows.  Usually there is only one
   **        source window (either source or disassembly), but both can be
   **        displayed at the same time.
 */
void
clearSourceWindows (void)
{
  _sourceWindows.list[0] = (Opaque) NULL;
  _sourceWindows.list[1] = (Opaque) NULL;
  _sourceWindows.count = 0;

  return;
}				/* currentSourceWindows */


/*
   ** clearSourceWindowsDetail()
   **        Clear the pertinant detail in the source windows.
 */
void
clearSourceWindowsDetail (void)
{
  int i;

  for (i = 0; i < (sourceWindows ())->count; i++)
    clearWinDetail ((TuiWinInfoPtr) (sourceWindows ())->list[i]);

  return;
}				/* currentSourceWindows */


/*
   ** addSourceWindowToList().
   **       Add a window to the list of source windows.  Usually there is
   **       only one source window (either source or disassembly), but
   **       both can be displayed at the same time.
 */
void
addToSourceWindows (TuiWinInfoPtr winInfo)
{
  if (_sourceWindows.count < 2)
    _sourceWindows.list[_sourceWindows.count++] = (Opaque) winInfo;

  return;
}				/* addToSourceWindows */


/*
   ** clearWinDetail()
   **        Clear the pertinant detail in the windows.
 */
void
clearWinDetail (TuiWinInfoPtr winInfo)
{
  if (m_winPtrNotNull (winInfo))
    {
      switch (winInfo->generic.type)
	{
	case SRC_WIN:
	case DISASSEM_WIN:
	  winInfo->detail.sourceInfo.startLineOrAddr.addr = 0;
	  winInfo->detail.sourceInfo.horizontalOffset = 0;
	  break;
	case CMD_WIN:
	  winInfo->detail.commandInfo.curLine =
	    winInfo->detail.commandInfo.curch = 0;
	  break;
	case DATA_WIN:
	  winInfo->detail.dataDisplayInfo.dataContent =
	    (TuiWinContent) NULL;
	  winInfo->detail.dataDisplayInfo.dataContentCount = 0;
	  winInfo->detail.dataDisplayInfo.regsContent =
	    (TuiWinContent) NULL;
	  winInfo->detail.dataDisplayInfo.regsContentCount = 0;
	  winInfo->detail.dataDisplayInfo.regsDisplayType =
	    TUI_UNDEFINED_REGS;
	  winInfo->detail.dataDisplayInfo.regsColumnCount = 1;
	  winInfo->detail.dataDisplayInfo.displayRegs = FALSE;
	  break;
	default:
	  break;
	}
    }

  return;
}				/* clearWinDetail */


/*
   ** blankStr()
   **        Accessor for the blank string.
 */
char *
blankStr (void)
{
  return _tuiBlankStr;
}				/* blankStr */


/*
   ** locationStr()
   **        Accessor for the location string.
 */
char *
locationStr (void)
{
  return _tuiLocationStr;
}				/* locationStr */


/*
   ** breakStr()
   **        Accessor for the break string.
 */
char *
breakStr (void)
{
  return _tuiBreakStr;
}				/* breakStr */


/*
   ** breakLocationStr()
   **        Accessor for the breakLocation string.
 */
char *
breakLocationStr (void)
{
  return _tuiBreakLocationStr;
}				/* breakLocationStr */


/*
   ** nullStr()
   **        Accessor for the null string.
 */
char *
nullStr (void)
{
  return _tuiNullStr;
}				/* nullStr */


/*
   ** sourceExecInfoPtr().
   **        Accessor for the source execution info ptr.
 */
TuiGenWinInfoPtr
sourceExecInfoWinPtr (void)
{
  return &_execInfo[0];
}				/* sourceExecInfoWinPtr */


/*
   ** disassemExecInfoPtr().
   **        Accessor for the disassem execution info ptr.
 */
TuiGenWinInfoPtr
disassemExecInfoWinPtr (void)
{
  return &_execInfo[1];
}				/* disassemExecInfoWinPtr */


/*
   ** locatorWinInfoPtr().
   **        Accessor for the locator win info.  Answers a pointer to the
   **        static locator win info struct.
 */
TuiGenWinInfoPtr
locatorWinInfoPtr (void)
{
  return &_locator;
}				/* locatorWinInfoPtr */


/*
   ** historyLimit().
   **        Accessor for the history limit
 */
int
historyLimit (void)
{
  return _historyLimit;
}				/* historyLimit */


/*
   ** setHistoryLimitTo().
   **        Mutator for the history limit
 */
void
setHistoryLimitTo (int h)
{
  _historyLimit = h;

  return;
}				/* setHistoryLimitTo */

/*
   ** termHeight().
   **        Accessor for the termHeight
 */
int
termHeight (void)
{
  return _termHeight;
}				/* termHeight */


/*
   ** setTermHeightTo().
   **        Mutator for the term height
 */
void
setTermHeightTo (int h)
{
  _termHeight = h;

  return;
}				/* setTermHeightTo */


/*
   ** termWidth().
   **        Accessor for the termWidth
 */
int
termWidth (void)
{
  return _termWidth;
}				/* termWidth */


/*
   ** setTermWidth().
   **        Mutator for the termWidth
 */
void
setTermWidthTo (int w)
{
  _termWidth = w;

  return;
}				/* setTermWidthTo */


/*
   ** currentLayout().
   **        Accessor for the current layout
 */
TuiLayoutType
currentLayout (void)
{
  return _currentLayout;
}				/* currentLayout */


/*
   ** setCurrentLayoutTo().
   **        Mutator for the current layout
 */
void
setCurrentLayoutTo (TuiLayoutType newLayout)
{
  _currentLayout = newLayout;

  return;
}				/* setCurrentLayoutTo */


/*
   ** setGenWinOrigin().
   **        Set the origin of the window
 */
void
setGenWinOrigin (TuiGenWinInfoPtr winInfo, int x, int y)
{
  winInfo->origin.x = x;
  winInfo->origin.y = y;

  return;
}				/* setGenWinOrigin */


/*****************************
** OTHER PUBLIC FUNCTIONS
*****************************/


/*
   ** tuiNextWin().
   **        Answer the next window in the list, cycling back to the top
   **        if necessary
 */
TuiWinInfoPtr
tuiNextWin (TuiWinInfoPtr curWin)
{
  TuiWinType type = curWin->generic.type;
  TuiWinInfoPtr nextWin = (TuiWinInfoPtr) NULL;

  if (curWin->generic.type == CMD_WIN)
    type = SRC_WIN;
  else
    type = curWin->generic.type + 1;
  while (type != curWin->generic.type && m_winPtrIsNull (nextWin))
    {
      if (winList[type] && winList[type]->generic.isVisible)
	nextWin = winList[type];
      else
	{
	  if (type == CMD_WIN)
	    type = SRC_WIN;
	  else
	    type++;
	}
    }

  return nextWin;
}				/* tuiNextWin */


/*
   ** tuiPrevWin().
   **        Answer the prev window in the list, cycling back to the bottom
   **        if necessary
 */
TuiWinInfoPtr
tuiPrevWin (TuiWinInfoPtr curWin)
{
  TuiWinType type = curWin->generic.type;
  TuiWinInfoPtr prev = (TuiWinInfoPtr) NULL;

  if (curWin->generic.type == SRC_WIN)
    type = CMD_WIN;
  else
    type = curWin->generic.type - 1;
  while (type != curWin->generic.type && m_winPtrIsNull (prev))
    {
      if (winList[type]->generic.isVisible)
	prev = winList[type];
      else
	{
	  if (type == SRC_WIN)
	    type = CMD_WIN;
	  else
	    type--;
	}
    }

  return prev;
}				/* tuiPrevWin */


/*
   ** displayableWinContentOf().
   **        Answer a the content at the location indicated by index.  Note
   **        that if this is a locator window, the string returned should be
   **        freed after use.
 */
char *
displayableWinContentOf (TuiGenWinInfoPtr winInfo, TuiWinElementPtr elementPtr)
{

  char *string = nullStr ();

  if (elementPtr != (TuiWinElementPtr) NULL || winInfo->type == LOCATOR_WIN)
    {
      /*
         ** Now convert the line to a displayable string
       */
      switch (winInfo->type)
	{
	case SRC_WIN:
	case DISASSEM_WIN:
	  string = elementPtr->whichElement.source.line;
	  break;
	case CMD_WIN:
	  string = elementPtr->whichElement.command.line;
	  break;
	case LOCATOR_WIN:
	  if ((string = (char *) xmalloc (
		      (termWidth () + 1) * sizeof (char))) == (char *) NULL)
	      string = nullStr ();
	  else
	    {
	      char lineNo[50], pc[50], buf[50], *fname, *pname;
	      register int strSize = termWidth (), i, procWidth, fileWidth;

	      /*
	         ** First determine the amount of file/proc name width
	         ** we have available
	       */
	      i = strSize - (PC_WIDTH + LINE_WIDTH
			     + 25	/* pc and line labels */
			     + strlen (FILE_PREFIX) + 1		/* file label */
			     + 15 /* procedure label */ );
	      if (i >= FILE_WIDTH + PROC_WIDTH)
		{
		  fileWidth = FILE_WIDTH;
		  procWidth = PROC_WIDTH;
		}
	      else
		{
		  fileWidth = i / 2;
		  procWidth = i - fileWidth;
		}

	      /* Now convert elements to string form */
	      if (elementPtr != (TuiWinElementPtr) NULL &&
		  *elementPtr->whichElement.locator.fileName != (char) 0 &&
		  srcWin->generic.isVisible)
		fname = elementPtr->whichElement.locator.fileName;
	      else
		fname = "??";
	      if (elementPtr != (TuiWinElementPtr) NULL &&
		  *elementPtr->whichElement.locator.procName != (char) 0)
		pname = elementPtr->whichElement.locator.procName;
	      else
		pname = "??";
	      if (elementPtr != (TuiWinElementPtr) NULL &&
		  elementPtr->whichElement.locator.lineNo > 0)
		sprintf (lineNo, "%d",
			 elementPtr->whichElement.locator.lineNo);
	      else
		strcpy (lineNo, "??");
	      if (elementPtr != (TuiWinElementPtr) NULL &&
		  elementPtr->whichElement.locator.addr != 0)
		sprintf (pc, "0x%lx",
			 (long) elementPtr->whichElement.locator.addr);
	      else
		strcpy (pc, "??");
	      /*
	         ** Now create the locator line from the string version
	         ** of the elements.  We could use sprintf() here but
	         ** that wouldn't ensure that we don't overrun the size
	         ** of the allocated buffer.  strcat_to_buf() will.
	       */
	      *string = (char) 0;
	      /* Filename */
	      strcat_to_buf (string, strSize, " ");
	      strcat_to_buf (string, strSize, FILE_PREFIX);
	      if (strlen (fname) > fileWidth)
		{
		  strncpy (buf, fname, fileWidth - 1);
		  buf[fileWidth - 1] = '*';
		  buf[fileWidth] = (char) 0;
		}
	      else
		strcpy (buf, fname);
	      strcat_to_buf (string, strSize, buf);
	      /* procedure/class name */
	      sprintf (buf, "%15s", PROC_PREFIX);
	      strcat_to_buf (string, strSize, buf);
	      if (strlen (pname) > procWidth)
		{
		  strncpy (buf, pname, procWidth - 1);
		  buf[procWidth - 1] = '*';
		  buf[procWidth] = (char) 0;
		}
	      else
		strcpy (buf, pname);
	      strcat_to_buf (string, strSize, buf);
	      sprintf (buf, "%10s", LINE_PREFIX);
	      strcat_to_buf (string, strSize, buf);
	      strcat_to_buf (string, strSize, lineNo);
	      sprintf (buf, "%10s", PC_PREFIX);
	      strcat_to_buf (string, strSize, buf);
	      strcat_to_buf (string, strSize, pc);
	      for (i = strlen (string); i < strSize; i++)
		string[i] = ' ';
	      string[strSize] = (char) 0;
	    }
	  break;
	case EXEC_INFO_WIN:
	  string = elementPtr->whichElement.simpleString;
	  break;
	default:
	  break;
	}
    }
  return string;
}				/* displayableWinContentOf */


/*
   **    winContentAt().
   **        Answer a the content at the location indicated by index
 */
char *
displayableWinContentAt (TuiGenWinInfoPtr winInfo, int index)
{
  return (displayableWinContentOf (winInfo, (TuiWinElementPtr) winInfo->content[index]));
}				/* winContentAt */


/*
   ** winElementHeight().
   **        Answer the height of the element in lines
 */
int
winElementHeight (TuiGenWinInfoPtr winInfo, TuiWinElementPtr element)
{
  int h;

  if (winInfo->type == DATA_WIN)
/* FOR NOW SAY IT IS ONLY ONE LINE HIGH */
    h = 1;
  else
    h = 1;

  return h;
}				/* winElementHeight */


/*
   **  winByName().
   **      Answer the window represented by name
 */
TuiWinInfoPtr
winByName (char *name)
{
  TuiWinInfoPtr winInfo = (TuiWinInfoPtr) NULL;
  int i = 0;

  while (i < MAX_MAJOR_WINDOWS && m_winPtrIsNull (winInfo))
    {
      if (strcmp (name, winName (&(winList[i]->generic))) == 0)
	winInfo = winList[i];
      i++;
    }

  return winInfo;
}				/* winByName */


/*
   **  partialWinByName().
   **      Answer the window represented by name
 */
TuiWinInfoPtr
partialWinByName (char *name)
{
  TuiWinInfoPtr winInfo = (TuiWinInfoPtr) NULL;

  if (name != (char *) NULL)
    {
      int i = 0;

      while (i < MAX_MAJOR_WINDOWS && m_winPtrIsNull (winInfo))
	{
          if (winList[i] != 0)
            {
              char *curName = winName (&winList[i]->generic);
              if (strlen (name) <= strlen (curName) &&
                  strncmp (name, curName, strlen (name)) == 0)
                winInfo = winList[i];
            }
	  i++;
	}
    }

  return winInfo;
}				/* partialWinByName */


/*
   ** winName().
   **      Answer the name of the window
 */
char *
winName (TuiGenWinInfoPtr winInfo)
{
  char *name = (char *) NULL;

  switch (winInfo->type)
    {
    case SRC_WIN:
      name = SRC_NAME;
      break;
    case CMD_WIN:
      name = CMD_NAME;
      break;
    case DISASSEM_WIN:
      name = DISASSEM_NAME;
      break;
    case DATA_WIN:
      name = DATA_NAME;
      break;
    default:
      name = "";
      break;
    }

  return name;
}				/* winName */


/*
   ** initializeStaticData
 */
void
initializeStaticData (void)
{
  initGenericPart (sourceExecInfoWinPtr ());
  initGenericPart (disassemExecInfoWinPtr ());
  initGenericPart (locatorWinInfoPtr ());

  return;
}				/* initializeStaticData */


/*
   ** allocGenericWinInfo().
 */
TuiGenWinInfoPtr
allocGenericWinInfo (void)
{
  TuiGenWinInfoPtr win;

  if ((win = (TuiGenWinInfoPtr) xmalloc (
		     sizeof (TuiGenWinInfoPtr))) != (TuiGenWinInfoPtr) NULL)
    initGenericPart (win);

  return win;
}				/* allocGenericWinInfo */


/*
   ** initGenericPart().
 */
void
initGenericPart (TuiGenWinInfoPtr win)
{
  win->width =
    win->height =
    win->origin.x =
    win->origin.y =
    win->viewportHeight =
    win->contentSize =
    win->lastVisibleLine = 0;
  win->handle = (WINDOW *) NULL;
  win->content = (OpaquePtr) NULL;
  win->contentInUse =
    win->isVisible = FALSE;

  return;
}				/* initGenericPart */


/*
   ** initContentElement().
 */
void
initContentElement (TuiWinElementPtr element, TuiWinType type)
{
  element->highlight = FALSE;
  switch (type)
    {
    case SRC_WIN:
    case DISASSEM_WIN:
      element->whichElement.source.line = (char *) NULL;
      element->whichElement.source.lineOrAddr.lineNo = 0;
      element->whichElement.source.isExecPoint = FALSE;
      element->whichElement.source.hasBreak = FALSE;
      break;
    case DATA_WIN:
      initGenericPart (&element->whichElement.dataWindow);
      element->whichElement.dataWindow.type = DATA_ITEM_WIN;
      ((TuiGenWinInfoPtr) & element->whichElement.dataWindow)->content =
	(OpaquePtr) allocContent (1, DATA_ITEM_WIN);
      ((TuiGenWinInfoPtr)
       & element->whichElement.dataWindow)->contentSize = 1;
      break;
    case CMD_WIN:
      element->whichElement.command.line = (char *) NULL;
      break;
    case DATA_ITEM_WIN:
      element->whichElement.data.name = (char *) NULL;
      element->whichElement.data.type = TUI_REGISTER;
      element->whichElement.data.itemNo = UNDEFINED_ITEM;
      element->whichElement.data.value = (Opaque) NULL;
      element->whichElement.data.highlight = FALSE;
      break;
    case LOCATOR_WIN:
      element->whichElement.locator.fileName[0] =
	element->whichElement.locator.procName[0] = (char) 0;
      element->whichElement.locator.lineNo = 0;
      element->whichElement.locator.addr = 0;
      break;
    case EXEC_INFO_WIN:
      element->whichElement.simpleString = blankStr ();
      break;
    default:
      break;
    }
  return;
}				/* initContentElement */

/*
   ** initWinInfo().
 */
void
initWinInfo (TuiWinInfoPtr winInfo)
{
  initGenericPart (&winInfo->generic);
  winInfo->canHighlight =
    winInfo->isHighlighted = FALSE;
  switch (winInfo->generic.type)
    {
    case SRC_WIN:
    case DISASSEM_WIN:
      winInfo->detail.sourceInfo.executionInfo = (TuiGenWinInfoPtr) NULL;
      winInfo->detail.sourceInfo.hasLocator = FALSE;
      winInfo->detail.sourceInfo.horizontalOffset = 0;
      winInfo->detail.sourceInfo.startLineOrAddr.addr = 0;
      break;
    case DATA_WIN:
      winInfo->detail.dataDisplayInfo.dataContent = (TuiWinContent) NULL;
      winInfo->detail.dataDisplayInfo.dataContentCount = 0;
      winInfo->detail.dataDisplayInfo.regsContent = (TuiWinContent) NULL;
      winInfo->detail.dataDisplayInfo.regsContentCount = 0;
      winInfo->detail.dataDisplayInfo.regsDisplayType =
	TUI_UNDEFINED_REGS;
      winInfo->detail.dataDisplayInfo.regsColumnCount = 1;
      winInfo->detail.dataDisplayInfo.displayRegs = FALSE;
      break;
    case CMD_WIN:
      winInfo->detail.commandInfo.curLine = 0;
      winInfo->detail.commandInfo.curch = 0;
      break;
    default:
      winInfo->detail.opaque = (Opaque) NULL;
      break;
    }

  return;
}				/* initWinInfo */


/*
   ** allocWinInfo().
 */
TuiWinInfoPtr
allocWinInfo (TuiWinType type)
{
  TuiWinInfoPtr winInfo = (TuiWinInfoPtr) NULL;

  winInfo = (TuiWinInfoPtr) xmalloc (sizeof (TuiWinInfo));
  if (m_winPtrNotNull (winInfo))
    {
      winInfo->generic.type = type;
      initWinInfo (winInfo);
    }

  return winInfo;
}				/* allocWinInfo */


/*
   ** allocContent().
   **        Allocates the content and elements in a block.
 */
TuiWinContent
allocContent (int numElements, TuiWinType type)
{
  TuiWinContent content = (TuiWinContent) NULL;
  char *elementBlockPtr = (char *) NULL;
  int i;

  if ((content = (TuiWinContent)
  xmalloc (sizeof (TuiWinElementPtr) * numElements)) != (TuiWinContent) NULL)
    {				/*
				   ** All windows, except the data window, can allocate the elements
				   ** in a chunk.  The data window cannot because items can be
				   ** added/removed from the data display by the user at any time.
				 */
      if (type != DATA_WIN)
	{
	  if ((elementBlockPtr = (char *)
	   xmalloc (sizeof (TuiWinElement) * numElements)) != (char *) NULL)
	    {
	      for (i = 0; i < numElements; i++)
		{
		  content[i] = (TuiWinElementPtr) elementBlockPtr;
		  initContentElement (content[i], type);
		  elementBlockPtr += sizeof (TuiWinElement);
		}
	    }
	  else
	    {
	      tuiFree ((char *) content);
	      content = (TuiWinContent) NULL;
	    }
	}
    }

  return content;
}				/* allocContent */


/*
   ** addContentElements().
   **        Adds the input number of elements to the windows's content.  If
   **        no content has been allocated yet, allocContent() is called to
   **        do this.  The index of the first element added is returned,
   **        unless there is a memory allocation error, in which case, (-1)
   **        is returned.
 */
int
addContentElements (TuiGenWinInfoPtr winInfo, int numElements)
{
  TuiWinElementPtr elementPtr;
  int i, indexStart;

  if (winInfo->content == (OpaquePtr) NULL)
    {
      winInfo->content = (OpaquePtr) allocContent (numElements, winInfo->type);
      indexStart = 0;
    }
  else
    indexStart = winInfo->contentSize;
  if (winInfo->content != (OpaquePtr) NULL)
    {
      for (i = indexStart; (i < numElements + indexStart); i++)
	{
	  if ((elementPtr = (TuiWinElementPtr)
	       xmalloc (sizeof (TuiWinElement))) != (TuiWinElementPtr) NULL)
	    {
	      winInfo->content[i] = (Opaque) elementPtr;
	      initContentElement (elementPtr, winInfo->type);
	      winInfo->contentSize++;
	    }
	  else			/* things must be really hosed now! We ran out of memory!? */
	    return (-1);
	}
    }

  return indexStart;
}				/* addContentElements */


/*
   **  tuiDelWindow().
   **     Delete all curses windows associated with winInfo, leaving everything
   **     else in tact.
 */
void
tuiDelWindow (TuiWinInfoPtr winInfo)
{
  Opaque detail;
  int i;
  TuiGenWinInfoPtr genericWin;


  switch (winInfo->generic.type)
    {
    case SRC_WIN:
    case DISASSEM_WIN:
      genericWin = locatorWinInfoPtr ();
      if (genericWin != (TuiGenWinInfoPtr) NULL)
	{
	  tuiDelwin (genericWin->handle);
	  genericWin->handle = (WINDOW *) NULL;
	  genericWin->isVisible = FALSE;
	}
      genericWin = winInfo->detail.sourceInfo.executionInfo;
      if (genericWin != (TuiGenWinInfoPtr) NULL)
	{
	  tuiDelwin (genericWin->handle);
	  genericWin->handle = (WINDOW *) NULL;
	  genericWin->isVisible = FALSE;
	}
      break;
    case DATA_WIN:
      if (winInfo->generic.content != (OpaquePtr) NULL)
	{
	  int i;

	  tuiDelDataWindows (
			      winInfo->detail.dataDisplayInfo.regsContent,
			  winInfo->detail.dataDisplayInfo.regsContentCount);
	  tuiDelDataWindows (
			      winInfo->detail.dataDisplayInfo.dataContent,
			  winInfo->detail.dataDisplayInfo.dataContentCount);
	}
      break;
    default:
      break;
    }
  if (winInfo->generic.handle != (WINDOW *) NULL)
    {
      tuiDelwin (winInfo->generic.handle);
      winInfo->generic.handle = (WINDOW *) NULL;
      winInfo->generic.isVisible = FALSE;
    }

  return;
}				/* tuiDelWindow */


/*
   **  freeWindow().
 */
void
freeWindow (TuiWinInfoPtr winInfo)
{
  Opaque detail;
  int i;
  TuiGenWinInfoPtr genericWin;


  switch (winInfo->generic.type)
    {
    case SRC_WIN:
    case DISASSEM_WIN:
      genericWin = locatorWinInfoPtr ();
      if (genericWin != (TuiGenWinInfoPtr) NULL)
	{
	  tuiDelwin (genericWin->handle);
	  genericWin->handle = (WINDOW *) NULL;
	}
      freeWinContent (genericWin);
      genericWin = winInfo->detail.sourceInfo.executionInfo;
      if (genericWin != (TuiGenWinInfoPtr) NULL)
	{
	  tuiDelwin (genericWin->handle);
	  genericWin->handle = (WINDOW *) NULL;
	  freeWinContent (genericWin);
	}
      break;
    case DATA_WIN:
      if (winInfo->generic.content != (OpaquePtr) NULL)
	{
	  freeDataContent (
			    winInfo->detail.dataDisplayInfo.regsContent,
			  winInfo->detail.dataDisplayInfo.regsContentCount);
	  winInfo->detail.dataDisplayInfo.regsContent =
	    (TuiWinContent) NULL;
	  winInfo->detail.dataDisplayInfo.regsContentCount = 0;
	  freeDataContent (
			    winInfo->detail.dataDisplayInfo.dataContent,
			  winInfo->detail.dataDisplayInfo.dataContentCount);
	  winInfo->detail.dataDisplayInfo.dataContent =
	    (TuiWinContent) NULL;
	  winInfo->detail.dataDisplayInfo.dataContentCount = 0;
	  winInfo->detail.dataDisplayInfo.regsDisplayType =
	    TUI_UNDEFINED_REGS;
	  winInfo->detail.dataDisplayInfo.regsColumnCount = 1;
	  winInfo->detail.dataDisplayInfo.displayRegs = FALSE;
	  winInfo->generic.content = (OpaquePtr) NULL;
	  winInfo->generic.contentSize = 0;
	}
      break;
    default:
      break;
    }
  if (winInfo->generic.handle != (WINDOW *) NULL)
    {
      tuiDelwin (winInfo->generic.handle);
      winInfo->generic.handle = (WINDOW *) NULL;
      freeWinContent (&winInfo->generic);
    }
  xfree (winInfo);

  return;
}				/* freeWindow */


/*
   ** freeAllSourceWinsContent().
 */
void
freeAllSourceWinsContent (void)
{
  int i;

  for (i = 0; i < (sourceWindows ())->count; i++)
    {
      TuiWinInfoPtr winInfo = (TuiWinInfoPtr) (sourceWindows ())->list[i];

      if (m_winPtrNotNull (winInfo))
	{
	  freeWinContent (&(winInfo->generic));
	  freeWinContent (winInfo->detail.sourceInfo.executionInfo);
	}
    }

  return;
}				/* freeAllSourceWinsContent */


/*
   ** freeWinContent().
 */
void
freeWinContent (TuiGenWinInfoPtr winInfo)
{
  if (winInfo->content != (OpaquePtr) NULL)
    {
      freeContent ((TuiWinContent) winInfo->content,
		   winInfo->contentSize,
		   winInfo->type);
      winInfo->content = (OpaquePtr) NULL;
    }
  winInfo->contentSize = 0;

  return;
}				/* freeWinContent */


/*
   ** freeAllWindows().
 */
void
freeAllWindows (void)
{
  TuiWinType type = SRC_WIN;

  for (; type < MAX_MAJOR_WINDOWS; type++)
    if (m_winPtrNotNull (winList[type]) &&
	winList[type]->generic.type != UNDEFINED_WIN)
      freeWindow (winList[type]);
  return;
}				/* freeAllWindows */


void
tuiDelDataWindows (TuiWinContent content, int contentSize)
{
  int i;

  /*
     ** Remember that data window content elements are of type TuiGenWinInfoPtr,
     ** each of which whose single element is a data element.
   */
  for (i = 0; i < contentSize; i++)
    {
      TuiGenWinInfoPtr genericWin = &content[i]->whichElement.dataWindow;

      if (genericWin != (TuiGenWinInfoPtr) NULL)
	{
	  tuiDelwin (genericWin->handle);
	  genericWin->handle = (WINDOW *) NULL;
	  genericWin->isVisible = FALSE;
	}
    }

  return;
}				/* tuiDelDataWindows */


void
freeDataContent (TuiWinContent content, int contentSize)
{
  int i;

  /*
     ** Remember that data window content elements are of type TuiGenWinInfoPtr,
     ** each of which whose single element is a data element.
   */
  for (i = 0; i < contentSize; i++)
    {
      TuiGenWinInfoPtr genericWin = &content[i]->whichElement.dataWindow;

      if (genericWin != (TuiGenWinInfoPtr) NULL)
	{
	  tuiDelwin (genericWin->handle);
	  genericWin->handle = (WINDOW *) NULL;
	  freeWinContent (genericWin);
	}
    }
  freeContent (content,
	       contentSize,
	       DATA_WIN);

  return;
}				/* freeDataContent */


/**********************************
** LOCAL STATIC FUNCTIONS        **
**********************************/


/*
   ** freeContent().
 */
static void
freeContent (TuiWinContent content, int contentSize, TuiWinType winType)
{
  if (content != (TuiWinContent) NULL)
    {
      freeContentElements (content, contentSize, winType);
      tuiFree ((char *) content);
    }

  return;
}				/* freeContent */


/*
   ** freeContentElements().
 */
static void
freeContentElements (TuiWinContent content, int contentSize, TuiWinType type)
{
  if (content != (TuiWinContent) NULL)
    {
      int i;

      if (type == SRC_WIN || type == DISASSEM_WIN)
	{
	  /* free whole source block */
	  if (content[0]->whichElement.source.line != (char *) NULL)
	    tuiFree (content[0]->whichElement.source.line);
	}
      else
	{
	  for (i = 0; i < contentSize; i++)
	    {
	      TuiWinElementPtr element;

	      element = content[i];
	      if (element != (TuiWinElementPtr) NULL)
		{
		  switch (type)
		    {
		    case DATA_WIN:
		      tuiFree ((char *) element);
		      break;
		    case DATA_ITEM_WIN:
		      /*
		         ** Note that data elements are not allocated
		         ** in a single block, but individually, as needed.
		       */
		      if (element->whichElement.data.type != TUI_REGISTER)
			tuiFree ((char *)
				 element->whichElement.data.name);
		      tuiFree ((char *) element->whichElement.data.value);
		      tuiFree ((char *) element);
		      break;
		    case CMD_WIN:
		      tuiFree ((char *) element->whichElement.command.line);
		      break;
		    default:
		      break;
		    }
		}
	    }
	}
      if (type != DATA_WIN && type != DATA_ITEM_WIN)
	tuiFree ((char *) content[0]);	/* free the element block */
    }

  return;
}				/* freeContentElements */
