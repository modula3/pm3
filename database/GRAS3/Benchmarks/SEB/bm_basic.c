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

 Contrary to what was initially intended for the benchmark, some
 random values are actually generated within the time measure
 interval.  This is because it would be highly impractical to e.g.
 generate random million attributes for all new nodes before
 creating the database.  An array holding this information might not
 fit in main memory and consequently cause swapping.  Thus, in
 either case time would be lost.

 See also bm_global.h for type descriptions.

 Creation date: 30.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 22.03.93   F.H.         Changed BBGetSizeDB()s 3rd parameter from
 int* to long*, corrected bug in BBAttributeRead():
 million was type long - corrected to int.
 ----------------------------------------------------------------------
 03.02.93   F.H.         Made major changes to adapt to the new specification
 of the benchmark.  MN relations now go from level
 n to level n+1.  The changes affect
 BBCreateRef(),BBClosureReadMN(),
 BBClosureReverseReadMN(),
 RecursiveSubTreeRefsCreate(), and
 BBSubTreeRefsCreate().
 RecursiveClosureReadMN() and
 RecursiveClosureRevReadMN() has been deleted.
 ***************************************************************************/

#include "bm_global.h"
#include "bm_database.h"
#include "bm_system.h"
#include "bm_random.h"
#include "bm_basic.h"


char text[TEXT_MAX_LEN + 1];		       /* text buffer; used when reading */

				   /* or writing text attribute */
int textlen;				       /* length of text in 'text' */



/***************************************************************************
 Database administration
 ***************************************************************************/


void
BBLoginDB(void)
{
  DBLogin();
}


void
BBLogoutDB(void)
{
  DBLogout();
}


void
BBOpenDB(char *name, dbreftype * dbref)
{
  DBOpen(name, dbref);
}


void
BBCloseDB(dbreftype dbref)
{
  DBClose(dbref);
}


void
BBCreateDB(char *name, dbreftype * dbrefp)
{
  DBCreate(name, dbrefp);
}


void
BBBackupDB(char *name)
{
  DBBackup(name);
}


void
BBRestoreDB(char *name)
{
  DBRestore(name);
}


void
BBEmptyCacheDB(char *name, dbreftype * dbref)
{
  system_empty_cache();
  DBEmptyCache(name, dbref);
}


void
BBGetHeightDB(char *name, int *height)
{
  DBGetHeight(name, height);
}


void
BBGetSizeDB(char *name, dbreftype * dbref, long *size)
{
  DBGetSize(name, dbref, size);
}



/***************************************************************************
 Transactions
 ***************************************************************************/

void
BBTransactionStartDB(char * dbref)
{
  DBTransactionStart(dbref);
}


void
BBTransactionCommitDB(char * dbref)
{
  DBTransactionCommit(dbref);
}


void
BBTransactionAbortDB(char * dbref)
{
  DBTransactionAbort(dbref);
}



/***************************************************************************
 Basic benchmark operations
 ***************************************************************************/


static void
CreateSubTree(dbreftype dbref, intIDtype parent_node, extIDtype extID,
	      int order, int no_of_levels, int level,
	      int no_of_children)
/*
 * IN  dbref             -- reference to open database
 * IN  parent_node       -- internalID of parent node of root of subtree
 * IN  extID             -- externalID of root of subtree
 * IN  order             -- root is order'th child of parent -- value
 * -- of order attribute of parent/child relation
 * IN  no_of_levels      -- total number of levels in database
 * IN  level             -- the level of the subtree root node
 * IN  no_of_children    -- the number of children of the subtree root node
 *
 * This function recursively creates the tree structure of the
 * benchmark test database.
 */
{
  int node_million, rel_million;	       /* random attribute values */

  /* for Node and parent/children */
  intIDtype this_node;			       /* internalID of this node */
  extIDtype childExtID;			       /* externalID of first child */
  int ch_no_of_children;		       /* child's number of children */
  int child_no;				       /* looping through the children */

  node_million = GetRandomMillionValue();      /* get random million */
  rel_million = GetRandomMillionValue();       /* attribute values */

  if (level >= no_of_levels)
    {					       /* last level -> TextNode */
      /* end recursion */
      GetRandomText(text, &textlen);	       /* get random text attr value */
      DBCreateTextNode(dbref, parent_node, extID, order, rel_million,
		       node_million, text, textlen, &this_node);

    }
  else
    {					       /* inner node -> Node */

      DBCreateNode(dbref, parent_node, extID, order, rel_million,
		   node_million, &this_node);

      if (no_of_children > 0)
	{				       /* create children */

	  /* compute externalID of first of nodes children */
	  if (even(extID))		       /* this node is left node of pair */
	    childExtID = geometric(AVG_CHILDREN, level)
	      + ((extID - 1) - geometric(AVG_CHILDREN, level - 1)) * AVG_CHILDREN
	      + 1;
	  else				       /* this node is right node of pair */
	    childExtID = geometric(AVG_CHILDREN, level)
	      + ((extID - 2) - geometric(AVG_CHILDREN, level - 1)) * AVG_CHILDREN
	      + (MAX_CHILDREN - no_of_children)
	      + 1;


	  for (child_no = 1; child_no <= no_of_children; child_no++)
	    {

	      /* create children in pairs */

	      if (level < no_of_levels - 1)    /* get random number for first pair */
		ch_no_of_children = GetRandomNoOfChildren();
	      else			       /* next last level -> no children */
		ch_no_of_children = 0;

	      CreateSubTree(dbref, this_node, childExtID + child_no - 1,
			    child_no, no_of_levels, level + 1,
			    ch_no_of_children);
	      child_no++;
	      CreateSubTree(dbref, this_node, childExtID + child_no - 1,
			    child_no, no_of_levels, level + 1,
			    MAX_CHILDREN - ch_no_of_children);
	    }
	}				       /* no_of_children > 0 */
    }					       /* else */
}


void
BBCreateTree(dbreftype dbref, int no_of_levels)
{
  intIDtype root_node;
  int child_no;				       /* for iteration through the children */
  int ch_no_of_children;		       /* child's number of children */

  /* Create root node and store the number of levels of the benchmark */
  /* test database in the million attribute */
  DBCreateRootNode(dbref, 1, no_of_levels, &root_node);

  for (child_no = 1; child_no <= AVG_CHILDREN; child_no++)
    {
      ch_no_of_children = GetRandomNoOfChildren();
      CreateSubTree(dbref, root_node, child_no + 1, child_no, no_of_levels,
		    1, ch_no_of_children);
      child_no++;
      CreateSubTree(dbref, root_node, child_no + 1, child_no, no_of_levels,
		    1, MAX_CHILDREN - ch_no_of_children);
    }
}


void
BBCreateRefs(dbreftype dbref, int no_of_levels)
{
  int level;
  extIDtype this_level_low = 1;		       /* externalID of first node on current level */
  extIDtype this_level_high = 1;	       /* externalID of last node on current level */
  int million;
  int no_of_refs;
  int ref_order;
  extIDtype from;
  intIDtype from_node, to_node;

  for (level = 0; level < no_of_levels; level++)
    {
      for (from = this_level_low; from <= this_level_high; from++)
	{
	  no_of_refs = GetRandomNoOfRefs();
	  from_node = DBGetInternalID(dbref, from);
	  for (ref_order = 1; ref_order <= no_of_refs; ref_order++)
	    {
	      million = GetRandomMillionValue();
	      to_node = DBGetInternalID(dbref, GetRandomExtIDLevelN(level + 1));
	      DBCreateRef(dbref, from_node, to_node, ref_order, million);
	    }
	}
      this_level_low = geometric(AVG_CHILDREN, level) + 1;
      this_level_high = geometric(AVG_CHILDREN, level + 1);
    }
}



intIDtype
BBInternalIDLookup(dbreftype dbref, extIDtype extID)
{
  return (DBGetInternalID(dbref, extID));
}


void
BBAttributeRead(dbreftype dbref, extIDtype extID)
{
  int million;

  DBReadNodeMillionAttr(dbref, DBGetInternalID(dbref, extID), &million);
}


void
BBAttributeWrite(dbreftype dbref, extIDtype extID, int million)
{
  DBWriteNodeMillionAttr(dbref, DBGetInternalID(dbref, extID), million);
}


void
BBTextAttributeRead(dbreftype dbref, extIDtype extID)
{
  DBReadNodeTextAttr(dbref, DBGetInternalID(dbref, extID), text, &textlen);
}


void
BBTextAttributeWrite(dbreftype dbref, extIDtype extID,
		     char *txt, int txtlen)
{
  DBWriteNodeTextAttr(dbref, DBGetInternalID(dbref, extID), txt, txtlen);
}


void
BBClosureRead1N(dbreftype dbref, intIDtype node, int ordered,
		int *size_of_closure)
{
  int no_of_children;
  int million_attrs[MAX_CHILDREN];
  intIDtype children_nodes[MAX_CHILDREN];
  int i;
  int cl_size;

  *size_of_closure = 0;
  DBGetChildrenWithAttr(dbref, ordered, node, &no_of_children, million_attrs,
			children_nodes);
  if (no_of_children)
    {
      *size_of_closure = no_of_children;
      for (i = 0; i < no_of_children; i++)
	{
	  BBClosureRead1N(dbref, children_nodes[i], ordered, &cl_size);
	  *size_of_closure += cl_size;
	}
    }
}


void
BBClosureReadMN(dbreftype dbref, intIDtype node, int ordered,
		int *size_of_closure)
{
  int no_of_refs;
  int million_attrs[MAX_REFS];
  intIDtype ref_nodes[MAX_REFS];
  int i;
  int cl_size;

  *size_of_closure = 0;
  DBGetRefsWithAttr(dbref, ordered, node, &no_of_refs, million_attrs,
		    ref_nodes);
  if (no_of_refs)
    {
      *size_of_closure = no_of_refs;
      for (i = 0; i < no_of_refs; i++)
	{
	  BBClosureReadMN(dbref, ref_nodes[i], ordered, &cl_size);
	  *size_of_closure += cl_size;
	}
    }
}


void
BBClosureReverseRead1N(dbreftype dbref, intIDtype node,
		       int *size_of_closure)
{

  intIDtype parent;
  int million;
  int cl_size;

  *size_of_closure = 0;
  if (DBGetParentWithAttr(dbref, node, &million, &parent))
    {
      BBClosureReverseRead1N(dbref, parent, &cl_size);
      *size_of_closure = cl_size + 1;
    }
}


void
BBClosureReverseReadMN(dbreftype dbref, intIDtype node,
		       int *size_of_closure)
{
  int no_of_revrefs;
  int million_attrs[MAX_REVREFS];
  intIDtype revref_nodes[MAX_REVREFS];
  int i;
  int cl_size;

  *size_of_closure = 0;
  DBGetRevRefsWithAttr(dbref, node, &no_of_revrefs, million_attrs,
		       revref_nodes);
  if (no_of_revrefs)
    {					       /* recursively get revrefs of revreferences */
      *size_of_closure = no_of_revrefs;
      for (i = 0; i < no_of_revrefs; i++)
	{
	  BBClosureReverseReadMN(dbref, revref_nodes[i], &cl_size);
	  *size_of_closure += cl_size;
	}
    }					       /* end of recursion */
}


static void
RecursiveSubTreeDelete(dbreftype dbref, intIDtype subtreeroot,
		       intIDtype node,
		       int *size_of_tree)
/*
 * IN  dbref
 * IN  subtreeroot
 * IN  node
 * OUT size_of_tree
 *
 * This function recursively deletes the subtree of the node subtreeroot.
 * The subtree root node is given as argument to be able to redirect
 * any refTo/refFrom relations going *to* a node being deleted to the
 * subtree root node.
 */
{
  int no_of_children;
  intIDtype children_nodes[MAX_CHILDREN];
  int i;
  int cl_size;

  *size_of_tree = 0;
  DBGetChildren(dbref, FALSE, node, &no_of_children, children_nodes);
  if (no_of_children > 0)
    {
      *size_of_tree = no_of_children;
      for (i = 0; i < no_of_children; i++)
	{
	  RecursiveSubTreeDelete(dbref, subtreeroot, children_nodes[i],
				 &cl_size);
	  *size_of_tree += cl_size;
	}
    }
  DBDeleteNode(dbref, node, subtreeroot);
}


void
BBSubTreeDelete(dbreftype dbref, extIDtype extID, int *size_of_tree)
{
  int no_of_children;
  intIDtype subtreeroot;
  intIDtype children_nodes[MAX_CHILDREN];
  int i;
  int cl_size;

  *size_of_tree = 0;
  subtreeroot = DBGetInternalID(dbref, extID);
  DBGetChildren(dbref, FALSE, subtreeroot, &no_of_children, children_nodes);
  if (no_of_children > 0)
    {
      *size_of_tree = no_of_children;
      for (i = 0; i < no_of_children; i++)
	{
	  RecursiveSubTreeDelete(dbref, subtreeroot, children_nodes[i],
				 &cl_size);
	  *size_of_tree += cl_size;
	}
    }
}


void
BBSubTreeCreate(dbreftype dbref, extIDtype extID,
		int no_of_children, int *size_of_tree)
{
  intIDtype node;
  extIDtype childExtID;
  int child_no;
  int ch_no_of_children;
  int height;

  DBReadNodeMillionAttr(dbref, DBGetInternalID(dbref, 1), &height);
  node = DBGetInternalID(dbref, extID);

  /* compute externalID of first of nodes children */
  if (even(extID))			       /* this node is left node of pair */
    childExtID = geometric(AVG_CHILDREN, CLOSURE_LEVEL)
      + ((extID - 1) - geometric(AVG_CHILDREN, CLOSURE_LEVEL - 1)) * AVG_CHILDREN
      + 1;
  else					       /* this node is right node of pair */
    childExtID = geometric(AVG_CHILDREN, CLOSURE_LEVEL)
      + ((extID - 2) - geometric(AVG_CHILDREN, CLOSURE_LEVEL - 1)) * AVG_CHILDREN
      + (MAX_CHILDREN - no_of_children)
      + 1;

  for (child_no = 1; child_no <= no_of_children; child_no++)
    {
      ch_no_of_children = GetRandomNoOfChildren();
      CreateSubTree(dbref, node, childExtID + child_no - 1, child_no, height,
		    CLOSURE_LEVEL + 1, ch_no_of_children);
      child_no++;
      CreateSubTree(dbref, node, childExtID + child_no - 1, child_no, height,
		    CLOSURE_LEVEL + 1, MAX_CHILDREN - ch_no_of_children);
    }

  *size_of_tree = no_of_children * geometric(AVG_CHILDREN,
					     height - CLOSURE_LEVEL - 1);
}


static void
RecursiveSubTreeRefsCreate(dbreftype dbref, intIDtype from_node,
			   int no_of_levels, int level)
{
  int million;
  int no_of_refs;
  int ref_order;
  intIDtype to_node;
  int no_of_children;
  intIDtype children_nodes[MAX_CHILDREN];
  int i;

  if (level < no_of_levels)
    {
      no_of_refs = GetRandomNoOfRefs();
      for (ref_order = 1; ref_order <= no_of_refs; ref_order++)
	{
	  million = GetRandomMillionValue();
	  to_node = DBGetInternalID(dbref, GetRandomExtIDLevelN(level + 1));
	  DBCreateRef(dbref, from_node, to_node, ref_order, million);
	}
      DBGetChildren(dbref, FALSE, from_node, &no_of_children, children_nodes);
      for (i = 0; i < no_of_children; i++)
	RecursiveSubTreeRefsCreate(dbref, children_nodes[i],
				   no_of_levels, level + 1);
    }
}


void
BBSubTreeRefsCreate(dbreftype dbref, extIDtype extID)
{
  intIDtype node;
  int no_of_children;
  intIDtype children_nodes[MAX_CHILDREN];
  int i;
  int height;
  int found;
  intIDtype source_node;
  intIDtype target_node;
  int source_level;


  DBReadNodeMillionAttr(dbref, DBGetInternalID(dbref, 1), &height);
  node = DBGetInternalID(dbref, extID);
  DBGetChildren(dbref, FALSE, node, &no_of_children, children_nodes);
  for (i = 0; i < no_of_children; i++)
    {
      RecursiveSubTreeRefsCreate(dbref, children_nodes[i], height,
				 CLOSURE_LEVEL + 1);
    }
  found = DBGetStoredRef(dbref, node, &source_node, &source_level);
  while (found)
    {
      target_node = DBGetInternalID(dbref, GetRandomExtIDLevelN(source_level + 1));
      DBRestoreRef(dbref, node, source_node, target_node);
      found = DBGetStoredRef(dbref, node, &source_node, &source_level);
    }
}
