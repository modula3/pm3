/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 The bm_database.h and bm_database.c files implement the "General Database
 Operations" module.  The purpose of this module is to isolate the part
 of the benchmark which is dependent on the database system being tested.
 Thus, even though the implementation of this module will vary, it's
 interface must not be altered.

 Some of the functions described may not be applicable to all database
 systems (e.g. DBLogin and DBEmptyCache).  In this case, the functions
 should be implemented as NULL functions.

 The functions do not return any error codes.  Errors occuring are handled
 by calling bm_error() defined in bm_global.h, giving an appropriate
 error message.

 See also bm_global.h for type descriptions.

 Creation date: 14.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 03.02.93   F.H.         The interface has been changed to
 accomodate for changed specifications of
 the benchmark.  MN relations now go from
 level n to level n+1.  DBRestoreRef() has
 changed both with regard to arguments and
 semantic.  A function DBGetStoredRef() has
 been added.


 00.00.00   XXXXXXXXXX   XXXXXXXXX
 ***************************************************************************/

#ifndef _bm_database_h
#define _bm_database_h

#include "bm_global.h"


/***************************************************************************
 Operations on entire database
 ***************************************************************************/

extern void DBLogin(void);

/*
 * This function establishes the connection with the database application
 * process (if it is necessary) and does other initialization.
 */


extern void DBLogout(void);

/*
 * This function closes down the connection with the database application
 * and does necessary cleaning up (wrt the database).
 */


extern void DBOpen(char *name, dbreftype * dbrefp);

/*
 * IN  name
 * OUT dbrefp
 *
 * Example use:
 *
 * dbreftype db;
 * DBOpen("myDB",&db);
 *
 * This function opens a database identified by 'name' for use.  Different
 * databases may have different opinions about what this name should
 * represent.  It may be an absolute path, a file name or some other name.
 * Thus, the semantics of this attribute is actually database dependent.
 * E.g. in GRAS it would be a filename, as the rest of the path is given by
 * an environment variable $GRAS.
 *
 * The 'dbrefp' parameter is returned by the database, and will be used by
 * subsequent database calls to identify the open database.  The type of this
 * parameter is of no interest to the benchmark program and is database
 * dependent.  We therefore want to handle it as an opaque type.  The type
 * dbreftype is really a (void *).  In the implementation of this module,
 * appropriate type casting will have to be carried out between dbreftype and
 * whatever type is being used by the database (this may be a pointer to a
 * struct containg file pointers etc.).
 */


extern void DBClose(dbreftype dbref);

/*
 * IN  dbref
 *
 * This function closes the database identified by dbref (see DBOpen).
 */


extern void DBCreate(char *name, dbreftype * dbrefp);

/*
 * IN  name
 * OUT dbrefp
 *
 * This function creates a new database with the name given by the
 * parameter.  This name will later be used by DBOpen to identify which
 * database to open (see comment of DBOpen for an explanation of both
 * parameters).  The function should leave the newly created database open,
 * so that a subsequent call to DBOpen will not be necessary.
 *
 * Creating the database may imply creating files, schema information etc.
 */


extern void DBBackup(char *name);

/*
 * IN  name
 *
 * This function makes a copy of the database with name 'name'.  This
 * copy can later be retrieved by DBRestore().  The meaning of the
 * name parameter is the same as for DBOpen.
 */


extern void DBRestore(char *name);

/*
 * IN  name
 *
 * This function restores the database with name 'name' from the copy
 * previously made with DBBackup().
 */






extern void DBDelete(char *name);

/*
 * IN  name
 *
 * This function deletes the named database.  The meaning of the name
 * parameter is the same as for DBOpen.
 */


extern void DBEmptyCache(char *name, dbreftype * dbref);

/*
 * IN  name
 * IN/OUT  dbref
 *
 * The purpose of this function is to empty the database cache so that new
 * information will have to be read from disc.  In most cases, the database
 * system will probably not offer control over this.   It may be
 * useful however to close and open the database in order to empty any
 * internal buffers, and therefore both 'name' and 'dbref' is given as
 * parameters.  In this case the new database reference will be
 * returned in 'dbref'.
 *
 */


extern void DBGetHeight(char *name, int *height);

/*
 * IN  name
 * OUT height
 *
 * Return the height of the benchmark testdatabase named 'name'.  The
 * height is stored in the million attribute of the root node in the
 * tree built by the parent/children relation at database creation time.
 */


extern void DBGetSize(char *name, dbreftype * dbref, long *size);

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

 (Short) Transactions ensure that the database operations performed within
 the transaction are either fully completed or have no effect at all.
 Nested transactions are not used by the benchmark, and thus the database
 does not need to support it.
 ***************************************************************************/

extern void DBTransactionStart(char * dbref);

/*
 * IN  dbref
 *
 * Start a new transaction for the open database identified by 'dbref'.
 */


extern void DBTransactionCommit(char * dbref);

/*
 * IN  dbref
 *
 * Commit a transaction of the open database identified by 'dbref'.
 */


extern void DBTransactionAbort(char * dbref);

/*
 * IN  dbref
 *
 * Abort (Rollback) a transaction of the open database identified by
 * 'dbref'.
 */


/***************************************************************************
 Instance creation/deletion operations
 ***************************************************************************/


extern void DBCreateRootNode(dbreftype dbref, extIDtype extID, int million,
			     intIDtype * root_node);

/*
 * IN  dbref
 * IN  extID
 * IN  million
 * OUT root_node
 *
 * This function creates the root node (of type 'Node') of the benchmark
 * database.  The parameter 'dbref' identifies the database, 'extID' is the
 * value of the externalID attribute of the node (1), and 'million' is the
 * value of its million attribute.  The function returns the database's
 * internal identifier of the node in 'root_node'.  Different
 * databases may have different opinions of the type of the internal
 * identifiers.  Therefore, as dbreftype, this is considered an opaque type.
 * The type intIDtype is really a (void *).  In the implementation of
 * this module, appropriate type casting will have to be carried out
 * between intIDtype and whatever type is being used by the database.
 *
 * If the database allows it, an index should be built on the
 * externalID attribute.
 */


extern void DBCreateNode(dbreftype dbref, intIDtype parent_node,
			 extIDtype extID, int order, int rel_million,
			 int node_million, intIDtype * child_node);

/*
 * IN  dbref
 * IN  parent_node
 * IN  extID
 * IN  order
 * IN  rel_million
 * IN  node_million
 * OUT child_node
 *
 * This function creates an instance of the node type 'Node' in the
 * database identified by 'dbref'.  The node is created as the 'order'th
 * child of the node with the internal identifier 'parent_node' and given
 * the externalID 'extID'.  The million attribute of the parent/children
 * relation is initialized to 'rel_million', and that of the newly
 * created node to 'node_million'.  The internal identifier of the node
 * is returned in 'child_node'.
 *
 * The database should be clustered along the parent/children
 * relation.  If the database allows it, an index should be built on
 * the externalID attribute.
 */


extern void DBCreateTextNode(dbreftype dbref, intIDtype parent_node,
			     extIDtype extID, int order, int rel_million,
			     int node_million, char *text,
			     int textlen,
			     intIDtype * child_node);

/*
 * IN  dbref
 * IN  parent_node
 * IN  extID
 * IN  order
 * IN  rel_million
 * IN  node_million
 * IN  text
 * IN  textlen
 * OUT child_node
 *
 * This function creates an instance of the node type 'TextNode' in
 * the database identified by 'dbref'.  The node is created as the
 * 'order'th child of the node with the internal identifier 'parent_node'
 * and given the externalID 'extID'.  The million attribute of the
 * parent/children relation is initialized to 'rel_million', and that of
 * the newly created node to 'node_million'.  Likewise, the text
 * attribute of the node is initialized to 'text'.  The internal
 * identifier of the node is returned in 'child_node'.
 *
 * Although the text string is NULL terminated, 'textlen' is also
 * given as a parameter (for optimisation).
 *
 * See DBCreateNode for comment on index and clustering.
 */


extern void DBCreateRef(dbreftype dbref, intIDtype source_node,
			intIDtype sink_node, int reforder, int million);

/*
 * IN  dbref
 * IN  source_node
 * IN  sink_node
 * IN  reforder
 * IN  million
 *
 * This function creates an instance of the refTo/refFrom relation from the
 * 'source_node' to the 'sink_node' in the database 'dbref'.
 * The reforder and million attributes of the relation are initialized to
 * 'reforder' and 'million' respectively.
 */


extern int DBGetStoredRef(dbreftype dbref, intIDtype root_node,
			  intIDtype * source_node, int *source_level);

/*
 * IN  dbref
 * IN  root_node
 * OUT source_node
 * OUT source_level
 * RETURN found
 *
 * This function searches for refTo/refFrom relations having
 * 'root_node' as goal and whose 'source_node' is on the same level as
 * or on a higher level than 'root_node'.  If such an edge is found, the
 * source_node and the level at which the source_node resides are
 * returned.  The return value of the function will then be TRUE.  If
 * no such edge is found, FALSE is returned.  See also DBRestoreRef().
 * 'root_node' must be on CLOSURE_LEVEL.
 */


extern void DBRestoreRef(dbreftype dbref, intIDtype root_node,
			 intIDtype source_node, intIDtype target_node);

/*
 * IN  dbref
 * IN  root_node
 * IN  source_node
 * IN  target_node
 *
 * This function redirects a refTo/refFrom edge going from 'source_node' to
 * 'root_node' to a new target 'target_node'.  If no edge exists, an
 * error message is printed, and the program exits.  The function
 * should be used together with DBGetStoredRef().
 */


extern void DBDeleteNode(dbreftype dbref, intIDtype node, intIDtype ref_node);

/*
 * IN  dbref
 * IN  ref_node
 * IN  node
 *
 * This function deletes the node 'node' in the database 'dbref'.  Also all
 * refTo/refFrom relations originating from is deleted as well as the
 * parent/children relation connecting the node to its parent.  In
 * this way referential integrity is ensured.  Incoming refTo/refFrom
 * relations are redirected to the node 'ref_node'.
 *
 * A node having children cannot be deleted.  The root node cannot be deleted.
 */



/***************************************************************************
 Query/update operations
 ***************************************************************************/

extern intIDtype DBGetInternalID(dbreftype dbref, extIDtype extID);

/*
 * IN  dbref
 * IN  extID
 * RETURN node
 *
 * This function returns the internalID 'node' of the node with
 * externalID 'extID' in the database 'dbref'.
 */


extern extIDtype DBGetExternalID(dbreftype dbref, intIDtype node);

/*
 * IN  dbref
 * IN  node
 * RETURN extID
 *
 * This function returns in 'extID' the value of the externalID attribute of
 * the node 'node' stored in the database 'dbref'.
 */


extern void DBReadNodeMillionAttr(dbreftype dbref, intIDtype node,
				  int *million);

/*
 * IN  dbref
 * IN  node
 * OUT million
 *
 * This function reads the million attribute of the node 'node'
 * stored in the database 'dbref'.  The node may either be of type 'Node'
 * or 'TextNode'.  The value of the attribute is returned in 'million'.
 */


extern void DBReadNodeTextAttr(dbreftype dbref, intIDtype node,
			       char text[TEXT_MAX_LEN + 1], int *textlen);

/*
 * IN  dbref
 * IN  node
 * OUT text
 * OUT textlen
 *
 * This function reads text attribute of the node 'node' stored in the
 * database 'dbref'.  The node must be of type 'TextNode'.  The text
 * must be copied to the 'text' buffer.  The length of the text string
 * is returned in 'textlen'.
 */


extern void DBWriteNodeMillionAttr(dbreftype dbref, intIDtype node,
				   int million);

/*
 * IN  dbref
 * IN  node
 * IN  million
 *
 * This function sets the value of the million attribute of the node 'node'
 * in the database 'dbref' to that of the 'million' parameter.
 */


extern void DBWriteNodeTextAttr(dbreftype dbref, intIDtype node,
				char *text, int textlen);

/*
 * IN  dbref
 * IN  node
 * IN  text
 * IN  textlen
 *
 * This function sets the value of the text attribute of the 'node' node in
 * the database 'dbref' to that of the 'text' parameter.  The length
 * of the text string is given in 'textlen'.
 */


extern void DBGetChildren(dbreftype dbref, int ordered,
			  intIDtype parent_node, int *no_of_children,
			  intIDtype children_nodes[MAX_CHILDREN]);

/*
 * IN  dbref
 * IN  ordered
 * IN  parent_node
 * OUT no_of_children
 * OUT children_nodes
 *
 * This function returns the internal identifiers of the children of a node
 * 'parent_node' in the database 'dbref'.  The internalIDs of the children
 * are returned in the array 'children_nodes'.  The actual number of children
 * is returned in 'no_of_children'.  If 'ordered' is true, the internalID of
 * the children must be *retrieved* from the database in order, with respect
 * to the order attribute of the parent/children relation, returning them so
 * that children_nodes[0] holds the internalID of the first child,
 * children_nodes[1] is the second child etc.  If 'ordered' is false, the
 * database may itself choose in which order to retrieve the IDs, perhaps
 * doing this more optimal.  Also in this case, the array should be filled up
 * from the start, reflecting the sequence in which the internalIDs are
 * actually retrieved.
 */


extern void DBGetChildrenWithAttr(dbreftype dbref,
				  int ordered,
				  intIDtype parent_node,
				  int *no_of_children,
				  int million_attrs[MAX_CHILDREN],
				  intIDtype children_nodes[MAX_CHILDREN]);

/*
 * IN  dbref
 * IN  ordered
 * IN  parent_node
 * OUT no_of_children
 * OUT million_attrs
 * OUT children_nodes
 *
 * This function is basically the same as DBGetChildren.  However, in
 * addition to returning the set of nodes reachable by the parent/children
 * relation, the million attribute of the *relations* then traversed should be
 * read and returned in the 'million_attrs' parameter.  The internal
 * identifier of a node and the million attribute of the relation
 * connecting this node with 'parent_node' should be put at the same
 * position in the two arrays.
 */


extern int DBGetParent(dbreftype dbref, intIDtype child_node,
		       intIDtype * parent_node);

/*
 * IN  dbref
 * IN  child_node
 * OUT parent_node
 * RETURN found
 *
 * This function returns in 'parent_node' the internal identifier of the
 * father of a node 'child_node' in the database 'dbref'.  If the node
 * has no parent, FALSE is returned, otherwise TRUE is returned.
 */


extern int DBGetParentWithAttr(dbreftype dbref, intIDtype child_node,
			       int *million, intIDtype
			       * parent_node);

/*
 * IN  dbref
 * IN  child_node
 * OUT million
 * OUT parent_node
 *
 * This function is basically the same as DBGetParent().  In addition
 * it returns the million attribute of the *relation* traversed.
 */



extern void DBGetReferences(dbreftype dbref, int ordered, intIDtype node,
			    int *no_of_refs,
			    intIDtype ref_nodes[MAX_REFS]);

/*
 * IN  dbref
 * IN  ordered
 * IN  node
 * OUT no_of_refs
 * OUT ref_nodes
 *
 * This function returns the internal identifiers of the nodes connected to
 * a node 'node' by the refTo/refFrom relationship.  The database is
 * identified by the parameter 'dbref'.  The internalIDs of the referenced
 * nodes are returned in the array 'ref_nodes'.  The actual number of
 * references is returned in 'no_of_refs'.  If 'ordered' is true, the
 * internalID of the references must be *retrieved* from the database in
 * order, with respect to the reforder attribute of the refTo/refFrom
 * relation, returning them so that ref_nodes[0] holds the internalID of the
 * first reference, children_nodes[1] the second reference etc.  If
 * 'ordered' is false, the database may itself choose in which order to
 * retrieve the IDs, perhaps doing this more optimal.  Also in this case, the
 * array should be filled up from the start, reflecting the sequence in which
 * the internalIDs are actually retrieved.
 */


extern void DBGetRefsWithAttr(dbreftype dbref, int ordered, intIDtype node,
			      int *no_of_refs,
			      int million_attrs[MAX_REFS],
			      intIDtype ref_nodes[MAX_REFS]);

/*
 * IN  dbref
 * IN  ordered
 * IN  node
 * OUT no_of_refs
 * OUT million_attrs
 * OUT ref_nodes
 *
 * This function is basically the same as DBGetReferences.  However, in
 * addition to returning the set of nodes reachable by the refTo/refFrom
 * relation, the million attribute of the relations then traversed should be
 * read and returned in the 'million_attrs' parameter.  The internal
 * identifier of a node and the million attribute of the relation
 * connecting this node with 'node' should be put at the same position in the
 * two arrays.
 */


extern void DBGetRevRefsWithAttr(dbreftype dbref, intIDtype node,
				 int *no_of_revrefs,
				 int million_attrs[MAX_REVREFS],
				 intIDtype revref_nodes[MAX_REVREFS]);

/*
 * IN  dbref
 * IN  node
 * OUT no_of_revrefs
 * OUT million_attrs
 * OUT revref_nodes
 *
 * This function returns the internalIDs of those nodes referencing
 * the node 'node' in the database 'dbref'.  The number of nodes
 * referencing the node is returned in 'no_of_revrefs', and the
 * internalIDs in the array revref_nodes.  Also return the million
 * attribute of the relations traversed in 'million_attr'.  The
 * revref_nodes array is of size MAX_REVREFS (defined in bm_global.h).
 * Since theoretically the in-context of the node may be equal to the
 * number of nodes in the database, it is possible that we get an
 * overflow.  This must be checked for in the function!  If an overflow
 * occurs, the program should halt and display an appropriate error
 * message instructing the user to increase the value of the constant
 * MAX_REVREFS and recompile (using bm_error() in bm_global.h).  The
 * reason for doing it this way, is that we want to avoid having to
 * allocate memory dynamically (it takes time).
 */

#endif
