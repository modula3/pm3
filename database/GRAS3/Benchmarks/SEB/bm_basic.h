/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 The Basic Benchmark Operations module contains the basic operations of
 the benchmark.  Some of them are only used in phase 1 wheras others
 also are used in phase 2 and 3.  Others are only used during the
 build-up phase.

 It may seem strange that some of the functions do not return the values
 they compute.  The reason for this is that we are not really
 interested in the values (of e.g. the million attribute).  We only
 want to be sure that the million attribute is really read from the
 database.  If we wanted to return the values, this would imply
 extra assignments and memory allocation, and since we want the
 times reported to be as pure as possible, we have chosen to leave
 out what is not strictly necessary.

 See also bm_global.h for type descriptions.

 Creation date: 16.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 22.03.93   F.H.         Changed BBGetSizeDB()s 3rd parameter from
 int* to long*
 ***************************************************************************/

#ifndef _bm_basic_h
#define _bm_basic_h

#include "bm_global.h"

/***************************************************************************
 Database administration
 ***************************************************************************/

extern void BBLoginDB(void);

/*
 * This function establishes the connection with the database application
 * process (if it is necessary) and does other initialization.
 */


extern void BBLogoutDB(void);

/*
 * This function closes down the connection with the database application
 * and does necessary cleaning up (wrt the database).
 */


extern void BBOpenDB(char *name, dbreftype * dbrefp);

/*
 * IN  name
 * OUT dbrefp
 *
 * Example use:
 *
 * dbreftype dbref;
 * BBOpenDB("myDB",&dbref);
 *
 * This function opens a database identified by 'name' for use.  Different
 * databases may have different opinions about what this name should
 * represent.  It may be an absolute path, a file name or some other name.
 * Thus, the semantics of this attribute is actually database dependent.
 * E.g. in GRAS it would be a filename, as the rest of the path is given by
 * an environment variable $GRAS.
 *
 * The dbrefp parameter is returned by the database, and will be used by
 * subsequent database calls to identify the open database.  The type of this
 * parameter is of no interest to the benchmark program and is database
 * dependent.  We therefore want to handle it as an opaque type.  The type
 * dbreftype is really a (void *).  In the implementation of this module,
 * appropriate type casting will have to be carried out between dbreftype and
 * whatever type is being used by the database (this may be a pointer to a
 * struct containg file pointers etc.).
 */


extern void BBCloseDB(dbreftype dbref);

/*
 * IN  dbref
 *
 * This function closes the database identified by dbref (see BBOpenDB).
 */


extern void BBCreateDB(char *name, dbreftype * dbrefp);

/*
 * IN  name
 * OUT dbrefp
 *
 * This function creates a new database with the name given by the
 * parameter.  This name will later be used by BBOpenDB to identify which
 * database to open (see comment of BBOpenDB for an explanation of both
 * parameters).  The function should leave the newly created database open,
 * so that a subsequent call to BBOpenDB will not be necessary.
 *
 * Creating the database may imply creating files, schema information etc.
 */


extern void BBBackupDB(char *name);

/*
 * IN  name
 *
 * This function makes a copy of the database with name 'name'.  This
 * copy can later be retrieved by BBRestoreDB().  The meaning of the
 * name parameter is the same as for BBOpenDB.
 */


extern void BBRestoreDB(char *name);

/*
 * IN  name
 *
 * This function restores the database with name 'name' from the copy
 * previously made with BBBackupDB().
 */



extern void BBDelete(char *name);

/*
 * IN  name
 *
 * This function deletes the named database.  The meaning of the name
 * parameter is the same as for BBOpenDB.
 */


extern void BBEmptyCacheDB(char *name, dbreftype * dbref);

/*
 * IN      name
 * IN/OUT  dbref
 *
 * The purpose of this function is to empty the database cache so that new
 * information will have to be read from disc.  In most cases, the database
 * system will probably not offer control over this.  In any case a
 * program like chill is called (system_empty_cache() in bm_system)
 * to cool down the system cache.  It may be useful to close and open
 * the database in order to empty any internal buffers, and therefore
 * both 'name' and 'dbref' is given as parameters.  In that case the
 * new database reference will be returned in 'dbref'.
 */


extern void BBGetHeightDB(char *name, int *height);

/*
 * IN  name
 * OUT height
 *
 * Return the height of the benchmark testdatabase named 'name'.  The
 * height is stored in the million attribute of the root node in the
 * tree built by the parent/children relation at database creation time.
 */



extern void BBGetSizeDB(char *name, dbreftype * dbref, long *size);

/*
 * IN  name
 * IN/OUT dbref
 * OUT size
 *
 * Return the size of the database.  If 'size' is negative, something
 * went wrong...  To get the size of the database it may have to be
 * closed, therefore 'name' and 'dbref' is given as parameters.
 */




/***************************************************************************
 Transactions
 ***************************************************************************/

extern void BBTransactionStartDB(char * dbref);

/*
 * IN  dbref
 *
 * Start a new transaction for the open database identified by 'dbref'.
 */


extern void BBTransactionCommitDB(char * dbref);

/*
 * IN  dbref
 *
 * Commit a transaction of the open database identified by 'dbref'.
 */


extern void BBTransactionAbortDB(char * dbref);

/*
 * IN  dbref
 *
 * Abort (Rollback) a transaction of the open database identified by
 * 'dbref'.
 */

/***************************************************************************
 Basic benchmark operations
 ***************************************************************************/

extern void BBCreateTree(dbreftype dbref, int no_of_levels);

/*
 * IN  dbref
 * IN  no_of_levels
 *
 * This function creates the tree structure of the benchmark test
 * database of height 'no_of_levels' in the database 'dbref'.  Only
 * the nodes and the parent/children relations are created.  The
 * refTo/refFrom relations are created in a separate call (see
 * CreateRefs).  The database should be clustered along the
 * parent/children relation and an index on the externalID attribute
 * of the nodes should also be built.
 *
 * This function is used in the build-up phase.
 */


extern void BBCreateRefs(dbreftype dbref, int no_of_levels);

/*
 * IN  dbref
 * IN  no_of_levels
 *
 * This function creates the refTo/refFrom relations of the benchmark
 * test database of height 'no_of_levels' in the database 'dbref'.
 * (See also CreateTree).  This function is used in the build-up phase.
 */


extern intIDtype BBInternalIDLookup(dbreftype dbref, extIDtype extID);

/*
 * IN  dbref
 * IN  extID
 *
 * This function returns the internalID of a node identified by its
 * externalID 'extID' in the database 'dbref'.  The function is used in
 * the first test of phase 1.
 */


extern void BBAttributeRead(dbreftype dbref, extIDtype extID);

/*
 * IN  dbref
 * IN  extID
 *
 * This function reads the value of the million attribute of the node
 * identified by its externalID 'extID' in the database 'dbref'. The node may
 * be of either type 'Node' or 'TextNode'.  The function is used in
 * the second test of phase 1.
 */


extern void BBAttributeWrite(dbreftype dbref, extIDtype extID, int million);

/*
 * IN  dbref
 * IN  extID
 * IN  million
 *
 * This function writes a new value 'million' to the million attribute of
 * a node identified by its externalID 'extID' in the database 'dbref'.
 * The node may be of either type 'Node' or 'TextNode'.  This function is
 * used in the third test of phase 1.
 */


extern void BBTextAttributeRead(dbreftype dbref, extIDtype extID);

/*
 * IN  dbref
 * IN  extID
 *
 * This function reads the text attribute of a node (type 'TextNode')
 * identified by its externalID 'extID' in the database 'dbref'.  This
 * function is used in the fourth test of phase 1.
 */


extern void BBTextAttributeWrite(dbreftype dbref, extIDtype extID,
				 char *text, int textlen);

/*
 * IN  dbref
 * IN  extID
 * IN  text
 * IN  textlen
 *
 * This function writes a new value 'text' to the text attribute to the
 * node identified by its externalID 'extID' in the database 'dbref'.
 * The length of the text string is given in 'textlen'.  The node must
 * be of type 'TextNode'.  This function is used in the fifth test of phase 1.
 */


extern void BBClosureRead1N(dbreftype dbref, intIDtype node, int ordered,
			    int *size_of_closure);

/*
 * IN  dbref
 * IN  node
 * IN  ordered
 * OUT size_of_closure
 *
 * This function computes the transitive closure of the
 * parent/children relation when this is traversed in the forward
 * direction, starting from the node 'node'.  The number of nodes in the
 * closure (not counting the input node) is reported in
 * 'size_of_closure'.  The closure is traversed depth-first.  If
 * 'ordered' is true, the children are traversed in order with respect to
 * the order attribute of the parent/children relation.  If not, the
 * database system itself will choose an appropriate order.  As the
 * closure is traversed, also the million attribute of the
 * parent/children relation is read.  This function is used in the
 * sixth test of phase 1 and in phases 2 and 3.
 */

extern void BBClosureReadMN(dbreftype dbref, intIDtype node, int ordered,
			    int *size_of_closure);

/*
 * IN  dbref
 * IN  node
 * IN  no_of_levels
 * IN  ordered
 * OUT size_of_closure
 *
 * This function computes the transitive closure of the refTo/refFrom
 * relation when this is traversed in the forward direction, starting
 * from the node 'node'.  The number of nodes in the closure (not
 * counting the input node) is reported in 'size_of_closure'.  The
 * closure is traversed depth-first.  If 'ordered' is true, the children
 * are traversed in order with respect to the order attribute of the
 * refTo/refFrom relation.  If not, the database system itself will
 * choose an appropriate order.  As the closure is traversed, also the
 * million attribute of the refTo/refFrom relation is read.  Since the
 * closure is possibly infinite (when not marking previously visited
 * nodes) the depth of the traversed closure is proportional to the number of
 * levels in the database (see benchmark description).  This function
 * is used in the seventh test of phase 1 and in phase 2.
 */


extern void BBClosureReverseRead1N(dbreftype dbref, intIDtype node,
				   int *size_of_closure);

/*
 * IN  dbref
 * IN  node
 * OUT size_of_closure
 *
 * This function computes the transitive closure of the
 * parent/children relation when this is traversed in the reverse
 * direction, starting from the node 'node'.  The number of the nodes in
 * the closure (not counting the input node) is reported in
 * 'size_of_closure'.  This function is used in the eigth test of phase 1.
 */


extern void BBClosureReverseReadMN(dbreftype dbref, intIDtype node,
				   int *size_of_closure);

/*
 * IN  dbref
 * IN  node
 * OUT size_of_closure
 *
 * The function computes the transitive closure of the refTo/refFrom
 * relation when this is traversed in the reverse direction, starting
 * from the node 'node'.  The number of nodes in the closure (not
 * counting the input node) is reported in 'size_of_closure'.  The
 * closure is traversed depth-first.  Since the closure is possibly
 * infinite (when not marking previously visited nodes) the depth of
 * the traversed closure is proportional to the number of levels in
 * the database (see benchmark description).  This function is used in
 * the ninth test of phase 1.
 */


extern void BBSubTreeDelete(dbreftype dbref, extIDtype extID,
			    int *size_of_tree);

/*
 * IN  dbref
 * IN  extID
 * OUT size_of_tree
 *
 * This function deletes the subtree of the node in the database
 * 'dbref' with external identifier 'extID'.  All nodes in the closure
 * of the parent/children relation are deleted as well as any
 * relations connected to them.  The number of nodes deleted is
 * returned in 'size_of_tree'.  This function is used in the tenth
 * test of phase 1 and phases 2 and 3.
 */


extern void BBSubTreeCreate(dbreftype dbref, extIDtype extID,
			    int no_of_children, int *size_of_tree);

/*
 * IN  dbref
 * IN  extID
 * IN  no_of_children
 * OUT size_of_tree
 *
 * A new subtree of nodes connected by the parent/children relation is
 * created in the database 'dbref' with the node with externalID
 * 'extID' as root.  The number of children of the root node is given in
 * 'no_of_children'.  The max height of the tree is given by the
 * original height as stored in the million attribute of the root
 * node.  'size_of_tree' returns the number of new nodes created.
 * This function is used in the eleventh test of phase 1 and phases 2
 * and 3 together with SubTreeRefsCreate.
 */


extern void BBSubTreeRefsCreate(dbreftype dbref, extIDtype extID);

/*
 * IN  dbref
 * IN  extID
 *
 * This function creates new refTo/refFrom relations for the nodes in
 * the subtree where 'extID' is root.  This function is used in the
 * eleventh test of phase 1 and phases 2 and 3 together with
 * SubTreeCreate.
 */

#endif
