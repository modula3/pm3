/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 This module implements a simple set

 Creation date: 25.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 00.00.00   XXXXXXXXXX   XXXXXXXXX
 ***************************************************************************/

#ifndef _smallset_h
#define _smallset_h

typedef unsigned long int smallset;	       /* the smallset type is by */

				      /* definition a set capable of */
				      /* holding 32 members - the */
				      /* numbers 0-31 */
typedef short int membertype;		       /* the type of the members of */

				      /* the smallset - actually 0..31 */


extern void empty_set(smallset * set);

/*
 * IN/OUT set
 *
 * Make sure that set is empty.
 */


extern void add_to_set(smallset * set, membertype value);

/*
 * IN/OUT set
 * IN     value
 *
 * Smallset is a set capable of holding the numbers 0 to 31.
 */


extern int is_member(smallset set, membertype value);

/*
 * IN  set
 * IN  value
 * OUT boolean (TRUE/FALSE)
 */


extern int is_empty(smallset set);

/*
 * IN  set
 * RETURN boolean (TRUE/FALSE);
 *
 * Is the set empty?
 */


#endif
