





/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 The build-up module creates an initial benchmark database.

 See also bmglobal.h for type descriptions.

 Creation date: 19.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 00.00.00   XXXXXXXXXX   XXXXXXXXX
 ***************************************************************************/

#ifndef _bm_buildDB_h
#define _bm_buildDB_h


extern void BMBuildDatabase(char *name, int no_of_levels);

/*
 * IN  name
 * IN  no_of_levels
 *
 * This function constitues the initial phase of the Software Engineering
 * Database Benchmark. It builds up a test database consisting of nodes of
 * type 'Node' and 'TextNode', connected by the composition relation
 * father/children and the reference relation refTo/refFrom. The 'name'
 * parameter gives the name of the database. Different databases may have
 * different opinions about what this name should represent. It may be an
 * absolute path, a file name or some other name. Thus, the semantics of this
 * attribute is actually database dependent. E.g. in GRAS it would be a
 * filename, as the rest of the path is given by an environment variable
 * $GRAS.  The no_of_levels parameter determines the size of the test
 * database, more specifically the height of the tree built by the
 * parent/children relation.
 */


#endif
