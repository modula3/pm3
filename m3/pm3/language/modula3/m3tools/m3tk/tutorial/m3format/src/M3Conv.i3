(************************************************************************
!		                                                        *
!*                                                                      *
!*         Copyright 1994 Sun Microsystems, Inc. All Rights Reserved.   *
!*                                                                      *
!*      Permission to use, copy, modify, and distribute this software   *
!*      and its documentation for any purpose and without fee is hereby *
!*      granted, provided that the above copyright notice appear in all *
!*      copies and that both that copyright notice and this permission  *
!*      notice appear in supporting documentation, and that the name of *
!*      Sun Microsystems, Inc. (SMI) not be used in advertising or      *
!*      publicity pertaining to distribution of the software without    *
!*      specific, written prior permission.                             *
!*                                                                      *
!*                                                                      *
!*      SMI DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,      *
!*      INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY,	        *
!*      FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT.           *
!*      IN NO EVENT SHALL SMI BE LIABLE FOR ANY SPECIAL, INCIDENTAL,    *
!*	INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER     *
!*      RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN      *
!*      ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,        *
!*      ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE     *
!*      OF THIS SOFTWARE.                                               *
!*                                                                      *
!***********************************************************************)

(* Modified by Michel Dagenais, 15 April 1997. *)

INTERFACE M3Conv;

IMPORT M3AST_AS;

PROCEDURE Set(n: M3AST_AS.SRC_NODE_C; indent := 0; checkFormat := FALSE;
    underscore := FALSE);

(* The Set procedure adds to the specified Modula-3 unit all the syntaxic
   tokens needed when printing out a program. Indeed, the ast contains
   all the information about the identifiers but does not store explicitly
   the keywords (IF WHILE TRY REVEAL...), whitespace (space, newline), and
   separators (coma, semicolon, bar...). Thus, Set attaches this information
   to each token in preparation for printing out. 

   When "check" is true, Set also verifies the upper/lower cases 
   used in each identifier as well as the original
   level of indentation used in the source code and reports any deviation
   to the usual programming conventions (See Modula-3, Sam Harbison, 
   Prentice Hall). Underscore characters are not recommended in identifiers
   and are reported unless "underscore" is true. *)

END M3Conv.
