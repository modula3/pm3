/* TUI layout window management.
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

#ifndef TUI_LAYOUT_H
#define TUI_LAYOUT_H

extern void showLayout (TuiLayoutType);
extern void tuiAddWinToLayout (TuiWinType);
extern int tuiDefaultWinHeight (TuiWinType, TuiLayoutType);
extern int tuiDefaultWinViewportHeight (TuiWinType, TuiLayoutType);
extern TuiStatus tui_set_layout (const char *);
extern TuiStatus tuiSetLayout (TuiLayoutType, TuiRegisterDisplayType);

#endif /*TUI_LAYOUT_H */
