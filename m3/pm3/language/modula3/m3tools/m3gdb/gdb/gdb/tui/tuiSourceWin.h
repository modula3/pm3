/* TUI display source/assembly window.
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

#ifndef _TUI_SOURCEWIN_H
#define _TUI_SOURCEWIN_H

extern void tuiDisplayMainFunction (void);
extern void tuiUpdateSourceWindow (TuiWinInfoPtr, struct symtab *, TuiLineOrAddress,
				   int);
extern void tuiUpdateSourceWindowAsIs (TuiWinInfoPtr, struct symtab *, TuiLineOrAddress,
				       int);
extern void tuiUpdateSourceWindowsWithAddr (CORE_ADDR);
extern void tuiUpdateSourceWindowsWithLine (struct symtab *, int);
extern void tuiUpdateSourceWindowsFromLocator (void);
extern void tuiClearSourceContent (TuiWinInfoPtr, int);
extern void tuiClearAllSourceWinsContent (int);
extern void tuiEraseSourceContent (TuiWinInfoPtr, int);
extern void tuiEraseAllSourceWinsContent (int);
extern void tuiSetSourceContentNil (TuiWinInfoPtr, char *);
extern void tuiShowSourceContent (TuiWinInfoPtr);
extern void tuiShowAllSourceWinsContent (void);
extern void tuiHorizontalSourceScroll (TuiWinInfoPtr, TuiScrollDirection,
				       int);
extern void tuiUpdateOnEnd (void);

extern TuiStatus tuiSetExecInfoContent (TuiWinInfoPtr);
extern void tuiShowExecInfoContent (TuiWinInfoPtr);
extern void tuiShowAllExecInfosContent (void);
extern void tuiEraseExecInfoContent (TuiWinInfoPtr);
extern void tuiEraseAllExecInfosContent (void);
extern void tuiClearExecInfoContent (TuiWinInfoPtr);
extern void tuiClearAllExecInfosContent (void);
extern void tuiUpdateExecInfo (TuiWinInfoPtr);
extern void tuiUpdateAllExecInfos (void);

extern void tuiSetIsExecPointAt (TuiLineOrAddress, TuiWinInfoPtr);
extern void tuiSetHasBreakAt (struct breakpoint *, TuiWinInfoPtr, int);
extern void tuiAllSetHasBreakAt (struct breakpoint *, int);
extern TuiStatus tuiAllocSourceBuffer (TuiWinInfoPtr);
extern int tuiLineIsDisplayed (int, TuiWinInfoPtr, int);
extern int tuiAddrIsDisplayed (CORE_ADDR, TuiWinInfoPtr, int);


/*
   ** Constant definitions
 */
#define        SCROLL_THRESHOLD            2	/* threshold for lazy scroll */

#endif
/*_TUI_SOURCEWIN_H */
