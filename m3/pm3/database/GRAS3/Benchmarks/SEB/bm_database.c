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

 Creation date: 01.02.93

 ***************************************************************************/

#include <stdio.h>
#include <string.h>
#include <memory.h>			       /* for memcpy */

#include <rgglobal.h>
#include <rgras.h>

#include "bm_database.h"
#include "bm_global.h"
#include "bm_system.h"

extern GroupType lockduration;

GraphType graphtype = 12;		       /* GraphType for AGCreateTypedGraph() */

TypeNumber OBJECTCLASS;	       /* A.S. of NODECLASS and LINKCLASS */
TypeNumber NODECLASS;	       /* A.S. of INNERCLASS */
TypeNumber INNERCLASS;	       /* A.S. of TEXTNODECLASS and LEAFCLASS */
TypeNumber LEAFCLASS;
TypeNumber TEXTNODECLASS;
TypeNumber LINKCLASS;	       /* A.S. of CHILDLINKCLASS and REFLINKCLASS */
TypeNumber CHILDLINKCLASS;
TypeNumber REFLINKCLASS;

TypeNumber INNERTYPE;	       /* Instance of INNERCLASS */
TypeNumber TEXTNODETYPE;       /* Instance of TEXTNODECLASS */
TypeNumber CHILDLINKTYPE;      /* Instance of CHILDLINKCLASS */
TypeNumber REFLINKTYPE;	       /* Instance of REFLINKCLASS */

AttributeNumber EXTERNALIDATTR;   /* NODECLASS attribute: EXTERNALIDATTR */
AttributeNumber OBJMILLIONATTR;   /* NODECLASS attribute: OBJMILLIONATTR */
AttributeNumber TEXTATTR;	  /* TEXTNODECLASS attribute: TEXTATTR */
AttributeNumber RELMILLIONATTR;   /* LINKCLASS attribute RELMILLIONATTR */
AttributeNumber ORDERATTR;	  /* CHILDLINKCLASS attribute: ORDERATTR */
AttributeNumber REFORDERATTR;     /* REFLINKCLASS attribute: REFORDERATTR */

TypeNumber FROMPARENTEDGE;     /* edge type */
TypeNumber TOCHILDEDGE;	       /* edge type */
TypeNumber FROMREFEDGE;	       /* edge type */
TypeNumber TOREFEDGE;	       /* edge type */

ExternNumber external_number;

/***************************************************************************
 Operations on entire database
 ***************************************************************************/

extern char DBNameServerAgent[NAME_LEN + 1];

void
DBLogin(void)
{
#ifdef DEBUG
  printf("DBLogin\n");
#endif /* DEBUG */
#if defined(LOCALGRAS) && defined(SUN_MODULA)
  m2_login();
#endif
  if (DBNameServerAgent[0] == 0)
    {
      AGLogin(NULL,500,NULL,NULL);
    }
  else
    {
      AGLogin(NULL,500,NULL,&DBNameServerAgent[0]);
    }
}


void
DBLogout(void)
{
#ifdef DEBUG
  printf("DBLogout\n");
#endif /* DEBUG */
  AGLogout();
}

static int IHaveSchema = 0; /* set when schema is declared/read */
void DBReadSchema(char *name);


void
DBOpen(char *name, dbreftype *dbrefp)
{
  TStatus status;
  PoolName poolname;
  GraphName graphname;
  ExternNumber extno;

#ifdef DEBUG
  printf("DBOpen\n");
#endif /* DEBUG */

  /*
  strcpy(poolname, name);
  strcpy(graphname, name);
  */

  if (!IHaveSchema) DBReadSchema(name);
  AGOpenGraph(name,name, &graphtype, OldGraph, "",
	      lockduration, NewGroup, TRUE, FALSE, &status, &extno, dbrefp, "");
  if (status != NoError)
    bm_error("DBOpen", "Unable to open graph", FALSE);
}


void
DBClose(dbreftype dbref)
{
#ifdef DEBUG
  printf("DBClose\n");
#endif /* DEBUG */
  AGCloseGraph(dbref);
}


/* names of node classes, types, attributes and edge types in schema */

static char *schemaname = "SEB";	    /* schema SEB */
static char *objectclass = "OBJECT";	    /* class OBJECT */
static char *nodeclass = "NODE";	    /* class NODE */
static char *innerclass = "INNER";	    /* class INNER is a NODE */
static char *leafclass = "LEAF";	    /* class LEAF is a NODE */
static char *textnodeclass = "TEXTNODE";    /* class TEXTNODE is a LEAF */
static char *linkclass = "LINK";	    /* class LINK */
static char *childlinkclass = "CHILDLINK";  /* class CHILDLINK is a LINK */
static char *reflinkclass = "REFLINK";      /* class REFLINK is a LINK */
static char *innertype = "Inner";	    /* nodetype Inner : INNER */
static char *textnodetype = "Textnode";     /* nodetype Textnode : TEXTNODE */
static char *childlinktype = "Childlink";   /* nodetype Childlink:CHILDLINK */
static char *reflinktype = "Reflink";	    /* nodetype Reflink : REFLINK */
static char *orderattr = "order";	    /* Intrinsic order : integer */
static char *reforderattr = "refOrder";     /* Intrinsic refOrder : integer */
static char *relmillionattr = "relmillion"; /* Intrinsic million : integer */
static char *externalidattr = "externalID"; /* Intrinsic key externalID : integer */
static char *objmillionattr = "objmillion"; /* Intrinsic million : integer */
static char *textattr = "text";	            /* Intrinsic text : string */
static char *fromparentedge = "FromParent"; /* edge type FromParent */
static char *tochildedge = "ToChild";	    /* edge type ToChild */
static char *fromrefedge = "FromRef";	    /* edge type FromRef */
static char *torefedge = "ToRef";	    /* edge type ToRef */

/*
 * Read the benchmark database schema
 */
void
DBReadSchema(char *name)
{
  TStatus status;
  SchemeNumber schema;
  int closepool = 1;
  BOOLEAN existent;

  AGOpenGraphPool(name, OldPool, &status);
  if (status == NotExistent)
    bm_error("DBReadSchema", "Graphpool not existent", FALSE);
  else if (status == AlreadyExistent)
    closepool = 0;
  else if (status != NoError)
    bm_error("DBCreate", "Unable to open graphpool", FALSE);

  /* SEB */
  AGDeclareScheme(schemaname, &schema, name);

  AGStartTransaction(name);

  /* OBJECT */
  AGTestAndShowNodeClassNumber(schema, objectclass, &existent,
			       &OBJECTCLASS, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find class", FALSE);

  /* NODE */
  AGTestAndShowNodeClassNumber(schema, nodeclass, &existent, &NODECLASS, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find class", FALSE);

  /* externalID */
  AGTestAndShowAttributeNumber(schema, externalidattr, NODECLASS,
		     &existent, &EXTERNALIDATTR, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find attribute", FALSE);

  /* million (objmillion) */
  AGTestAndShowAttributeNumber(schema, objmillionattr, NODECLASS,
		     &existent, &OBJMILLIONATTR, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find attribute", FALSE);

  /* INNER */
  AGTestAndShowNodeClassNumber(schema, innerclass, &existent, &INNERCLASS, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find class", FALSE);

  /* Inner */
  AGTestAndShowNodeTypeNumber(schema, innertype, &existent, &INNERTYPE, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find type", FALSE);

  /* LEAF */
  AGTestAndShowNodeClassNumber(schema, leafclass, &existent, &LEAFCLASS, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find class", FALSE);

  /* TEXTNODE */
  AGTestAndShowNodeClassNumber(schema, textnodeclass, &existent,
			       &TEXTNODECLASS, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find class", FALSE);

  /* text */
  AGTestAndShowAttributeNumber(schema, textattr, TEXTNODECLASS,
		     &existent, &TEXTATTR, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find attribute", FALSE);

  /* TextNode */
  AGTestAndShowNodeTypeNumber(schema, textnodetype, &existent,
			      &TEXTNODETYPE, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find type", FALSE);

  /* LINK */
  AGTestAndShowNodeClassNumber(schema, linkclass, &existent, &LINKCLASS, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find class", FALSE);

  /* relmillion */
  AGTestAndShowAttributeNumber(schema, relmillionattr, LINKCLASS,
		     &existent, &RELMILLIONATTR, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find attribute", FALSE);

  /* CHILDLINK */
  AGTestAndShowNodeClassNumber(schema, childlinkclass, &existent,
			       &CHILDLINKCLASS, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find class", FALSE);

  /* order */
  AGTestAndShowAttributeNumber(schema, orderattr, CHILDLINKCLASS,
		     &existent, &ORDERATTR, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find attribute", FALSE);

  /* ChildLink */
  AGTestAndShowNodeTypeNumber(schema, childlinktype, &existent,
			      &CHILDLINKTYPE, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find type", FALSE);

  /* REFLINK */
  AGTestAndShowNodeClassNumber(schema, reflinkclass, &existent,
			       &REFLINKCLASS, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find class", FALSE);

  /* reforder */
  AGTestAndShowAttributeNumber(schema, reforderattr, REFLINKCLASS,
		     &existent, &REFORDERATTR, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find attribute", FALSE);

  /* RefLink */
  AGTestAndShowNodeTypeNumber(schema, reflinktype, &existent, &REFLINKTYPE, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find type", FALSE);

  /* FromParentEdge */
  AGTestAndShowEdgeTypeNumber(schema, fromparentedge, &existent,
			      &FROMPARENTEDGE, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find edge type", FALSE);

  /* ToChildEdge */
  AGTestAndShowEdgeTypeNumber(schema, tochildedge, &existent,
			      &TOCHILDEDGE, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find edge type", FALSE);

  /* FromRefEdge */
  AGTestAndShowEdgeTypeNumber(schema, fromrefedge, &existent,
			      &FROMREFEDGE, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find edge type", FALSE);

  /* ToRefEdge */
  AGTestAndShowEdgeTypeNumber(schema, torefedge, &existent,
			      &TOREFEDGE, FALSE);
  if (!existent)
    bm_error("DBReadSchema", "Cannot find edge type", FALSE);

  AGCommitScheme(schema);

  AGCommitTransaction(name);

  if (closepool)
    AGCloseGraphPool(name);

  IHaveSchema = 1;
}

/*
 * Create benchmark test database in $GRAS/'name'/'name'.
 */
 void
DBCreate(char *name, dbreftype *dbrefp)
{
  TStatus status;
  GraphNumber graphnr;			       /* pointer to open graph */
  ExternNumber ext_no;			       /* of created graph */
  SchemeNumber schema;

#ifdef DEBUG
  printf("DBCreate: %s\n", name);
#endif

  AGDeleteGraphPool(name,&status,TRUE);
  
  AGOpenGraphPool(name, NewPool, &status);
  if (status == AlreadyExistent)
    bm_error("DBCreate", "Graphpool does already exist", FALSE);
  else if (status != NoError)
    bm_error("DBCreate", "Unable to create graphpool", FALSE);

      /* SEB */
      AGDeclareScheme(schemaname, &schema, name);

      AGStartTransaction(name);

      /* OBJECT */
      AGDeclareNodeClass(schema, objectclass, &OBJECTCLASS);

      /* NODE */
      AGDeclareNodeClass(schema, nodeclass, &NODECLASS);
      AGAppendNodeClass(schema, nodeclass, objectclass);

      /* externalID */
      AGDeclareAttribute(schema, nodeclass, externalidattr,
			 Intrinsic, Key, IntegerValue, OptUnique,
			 TRUE, sizeof(extIDtype), &EXTERNALIDATTR);

      /* million (objmillion) */
      AGDeclareAttribute(schema, nodeclass, objmillionattr,
			 Intrinsic, Normal, IntegerValue, OptUnique,
			 TRUE, sizeof(int), &OBJMILLIONATTR);

      /* INNER */
      AGDeclareNodeClass(schema, innerclass, &INNERCLASS);
      AGAppendNodeClass(schema, innerclass, nodeclass);

      /* Inner */
      AGDeclareNodeType(schema, innertype,
			&INNERTYPE);
      AGAppendNodeType(schema, innertype, innerclass);

      /* LEAF */
      AGDeclareNodeClass(schema, leafclass, &LEAFCLASS);
      AGAppendNodeClass(schema, leafclass, nodeclass);

      /* TEXTNODE */
      AGDeclareNodeClass(schema, textnodeclass,
			 &TEXTNODECLASS);
      AGAppendNodeClass(schema, textnodeclass, leafclass);

      /* text */
      AGDeclareAttribute(schema, textnodeclass, textattr,
			 Intrinsic, Normal, StringValue, OptUnique,
			 FALSE, 0, &TEXTATTR);

      /* TextNode */
      AGDeclareNodeType(schema, textnodetype,
			&TEXTNODETYPE);
      AGAppendNodeType(schema, textnodetype, textnodeclass);

      /* LINK */
      AGDeclareNodeClass(schema, linkclass, &LINKCLASS);
      AGAppendNodeClass(schema, linkclass, objectclass);

      /* relmillion */
      AGDeclareAttribute(schema, linkclass, relmillionattr,
			 Intrinsic, Normal, IntegerValue, OptUnique,
			 TRUE, sizeof(int), &RELMILLIONATTR);

      /* CHILDLINK */
      AGDeclareNodeClass(schema, childlinkclass,
			 &CHILDLINKCLASS);
      AGAppendNodeClass(schema, childlinkclass, linkclass);

      /* order */
      AGDeclareAttribute(schema, childlinkclass, orderattr,
			 Intrinsic, Normal, IntegerValue, OptUnique,
			 TRUE, sizeof(int), &ORDERATTR);

      /* ChildLink */
      AGDeclareNodeType(schema, childlinktype,
			&CHILDLINKTYPE);
      AGAppendNodeType(schema, childlinktype, childlinkclass);

      /* REFLINK */
      AGDeclareNodeClass(schema, reflinkclass, &REFLINKCLASS);
      AGAppendNodeClass(schema, reflinkclass, linkclass);

      /* reforder */
      AGDeclareAttribute(schema, reflinkclass, reforderattr,
			 Intrinsic, Normal, IntegerValue, OptUnique,
			 TRUE, sizeof(int), &REFORDERATTR);

      /* RefLink */
      AGDeclareNodeType(schema, reflinktype,
			&REFLINKTYPE);
      AGAppendNodeType(schema, reflinktype, reflinkclass);

      /* FromParentEdge */
      AGDeclareEdgeType(schema, fromparentedge, innerclass,
			OptUnique, childlinkclass, OptUnique, &FROMPARENTEDGE);

      /* ToChildEdge */
      AGDeclareEdgeType(schema, tochildedge, childlinkclass,
			OptUnique, nodeclass, OptUnique, &TOCHILDEDGE);

      /* FromRefEdge */
      AGDeclareEdgeType(schema, fromrefedge, innerclass, OptUnique,
			reflinkclass, OptUnique, &FROMREFEDGE);

      /* ToRefEdge */
      AGDeclareEdgeType(schema, torefedge, reflinkclass, OptUnique,
			nodeclass, OptUnique, &TOREFEDGE);

      AGCommitScheme(schema);

      AGCommitTransaction(name);
            
      AGOpenGraph(name, name, &graphtype, NewGraph, "",
	      lockduration, NewGroup, TRUE, FALSE, &status, &ext_no, &graphnr, schemaname);

      external_number = ext_no;
      *dbrefp = graphnr;		       /* pointer to open graph */
      
      if (status != NoError)
        bm_error("DBCreate", "Unable to create graph", FALSE);

      IHaveSchema = 1;
  
      /* Decreases efficiency */
      AGSetErrorCheckMode(graphnr, TRUE);
      
}


void
DBBackup(char *name)
{
#if 0
  char *GRASpath;
  char frompath[MAX_PATH + 1];
  char topath[MAX_PATH + 1];
#endif

#ifdef DEBUG
  printf("DBBackup\n");
#endif
/* Decided that this wasn't necessary as structural changes are non-destructive
 * GRASpath=get_env_var("GRAS");
 * if(GRASpath==NULL){
 * bm_error("DBBackup","Environment variable GRAS not set",FALSE);
 * }else{
 * sprintf(frompath,"%s/%s/%s.0001",GRASpath,name,name);
 * sprintf(topath,"%s/%s/%s.0001.bak",GRASpath,name,name);
 * if(file_copy(frompath,topath)!=FILE_OK)
 * bm_error("DBBackup","Couldn't make backup",FALSE);
 * sprintf(frompath,"%s/%s/.ManGraph",GRASpath,name);
 * sprintf(topath,"%s/%s/.ManGraph.bak",GRASpath,name);
 * if(file_copy(frompath,topath)!=FILE_OK)
 * bm_error("DBBackup","Couldn't make backup",FALSE);
 *
 * }
 */
}


void
DBRestore(char *name)
{
#if 0
  char *GRASpath;
  char frompath[MAX_PATH + 1];
  char topath[MAX_PATH + 1];
#endif

#ifdef DEBUG
  printf("DBBackup\n");
#endif
/* see DBBackup()
 * GRASpath=get_env_var("GRAS");
 * if(GRASpath==NULL){
 * bm_error("DBBackup","Environment variable GRAS not set",FALSE);
 * }else{
 * sprintf(frompath,"%s/%s/%s.0001.bak",GRASpath,name,name);
 * sprintf(topath,"%s/%s/%s.0001",GRASpath,name,name);
 * if(file_copy(frompath,topath)!=FILE_OK)
 * bm_error("DBBackup","Couldn't make backup",FALSE);
 * sprintf(frompath,"%s/%s/.ManGraph.bak",GRASpath,name);
 * sprintf(topath,"%s/%s/.ManGraph",GRASpath,name);
 * if(file_copy(frompath,topath)!=FILE_OK)
 * bm_error("DBBackup","Couldn't make backup",FALSE);
 * }
 */
}


void
DBEmptyCache(char *name, dbreftype *dbref)
{
#ifdef DEBUG
  printf("DBEmptyCache\n");
#endif /* DEBUG */
  DBClose(*dbref);
  DBLogout();
  DBLogin();
  DBOpen(name, dbref);
}


void
DBGetSize(char *name, dbreftype *dbref, long *size)
{
  char *GRASpath;
  char filepath[MAX_PATH + 1];
  long dbsize;

#ifdef DEBUG
  printf("DBGetSize\n");
#endif /* DEBUG */
  
  DBClose(*dbref);
  
  *size = AGFileSize(name,name);

  *size = *size * 8192;
  
  DBOpen(name, dbref);
  
}


void
DBGetHeight(char *name, int *height)
{
  dbreftype dbref;

  DBLogin();
  DBOpen(name, &dbref);
  DBReadNodeMillionAttr(dbref, DBGetInternalID(dbref, 1), height);
  DBClose(dbref);
  DBLogout();
}





/***************************************************************************
 Transactions

 (Short) Transactions ensure that the database operations performed within
 the transaction are either fully completed or have no effect at all.
 Nested transactions are not used by the benchmark, and thus the database
 does not need to support it.
 ***************************************************************************/


void
DBTransactionStart(char * dbref)
{
#ifdef DEBUG
  printf("DBTransactionStart\n");
#endif /* DEBUG */

  AGStartTransaction(dbref);
}


void
DBTransactionCommit(char * dbref)
{
#ifdef DEBUG
  printf("DBTransactionCommit\n");
#endif /* DEBUG */

  AGCommitTransaction(dbref);
}


void
DBTransactionAbort(char * dbref)
{
#ifdef DEBUG
  printf("DBTransactionAbort\n");
#endif /* DEBUG */

  AGAbortTransaction(dbref);
}




/***************************************************************************
 Instance creation/deletion operations
 ***************************************************************************/


void
DBCreateRootNode(dbreftype dbref, extIDtype extID, int million,
		 intIDtype *root_node)
{
  NodeNumber temp_node;
  
#ifdef DEBUG
  printf("DBCreateRootNode: extID=%i million=%i\n", extID, million);
#endif /* DEBUG */
  
  temp_node.entity = 1000;
  temp_node.graph = external_number;

  /* create node */
  AGCreateNode(dbref, INNERTYPE, temp_node, root_node);

  AGPutAttributeSubstr(dbref, *root_node, EXTERNALIDATTR,
		       0, sizeof(extID), (TEXT) &extID, TRUE);

  /* init million attribute */
  AGPutAttributeSubstr(dbref, *root_node, OBJMILLIONATTR,
		       0, sizeof(million), (TEXT) &million,
		       FALSE);
}


void
DBCreateNode(dbreftype dbref, intIDtype parent_node,
	     extIDtype extID, int order, int rel_million,
	     int node_million, intIDtype *child_node)
{
  intIDtype link_node;

#ifdef DEBUG
  printf("DBCreateNode: parent_node=%i extID=%i order=%i\n ", parent_node,
	 extID, order);
  printf("              node_million=%i rel_million=%i\n", node_million,
	 rel_million);
#endif /* DEBUG */

  /* create Childlink node and Inner node */
  AGCreateEdgeAndNode(dbref, parent_node, FROMPARENTEDGE,
		      CHILDLINKTYPE, &link_node);
  AGCreateEdgeAndNode(dbref, link_node, TOCHILDEDGE,
		      INNERTYPE, child_node);

  AGPutAttributeSubstr(dbref, *child_node, EXTERNALIDATTR,
		       0, sizeof(extID), (TEXT) &extID, TRUE);

  /* init million attr of Inner */
  /* and million and order attr */
  /* of Childlink */
  AGPutAttributeSubstr(dbref, *child_node, OBJMILLIONATTR,
		       0, sizeof(node_million),
		       (TEXT) &node_million,
		       FALSE);
  AGPutAttributeSubstr(dbref, link_node, ORDERATTR,
		       0, sizeof(order),
		       (TEXT) &order,
		       FALSE);
  AGPutAttributeSubstr(dbref, link_node, RELMILLIONATTR,
		       0, sizeof(rel_million),
		       (TEXT) &rel_million,
		       FALSE);
}


void
DBCreateTextNode(dbreftype dbref, intIDtype parent_node,
		 extIDtype extID, int order, int rel_million,
		 int node_million, char *text,
		 int textlen,
		 intIDtype *child_node)
{
  intIDtype link_node;

#ifdef DEBUG
  printf("DBCreateTextNode: parent_node=%i extID=%i order=%i\n", parent_node,
	 extID, order);
  printf("                  node_million=%i rel_million=%i\n", node_million,
	 rel_million);
  printf("                  textlen=%i text=%s\n", textlen, text);
#endif /* DEBUG */

  /* create Childlink node and Textnode node */
  AGCreateEdgeAndNode(dbref, parent_node, FROMPARENTEDGE,
		      CHILDLINKTYPE,
		      &link_node);
  AGCreateEdgeAndNode(dbref, link_node, TOCHILDEDGE,
		      TEXTNODETYPE,
		      child_node);

  AGPutAttributeSubstr(dbref, *child_node, EXTERNALIDATTR,
		       0, sizeof(extID), (TEXT) &extID, TRUE);

  /* init million and text */
  /* attribute of Textnode node and */
  /* million and order of */
  /* Childlink node */
  AGPutAttributeSubstr(dbref, *child_node, OBJMILLIONATTR,
		       0, sizeof(node_million),
		       (TEXT) &node_million,
		       FALSE);
  AGPutAttributeSubstr(dbref, *child_node, TEXTATTR,
		       0, textlen + 1,
		       text, TRUE);
  AGPutAttributeSubstr(dbref, link_node, ORDERATTR,
		       0, sizeof(order),
		       (TEXT) &order, FALSE);
  AGPutAttributeSubstr(dbref, link_node, RELMILLIONATTR,
		       0, sizeof(rel_million),
		       (TEXT) &rel_million,
		       FALSE);
}


void
DBCreateRef(dbreftype dbref, intIDtype source_node,
	    intIDtype sink_node, int reforder, int million)
{
  intIDtype link_node;

#ifdef DEBUG
  printf("DBCreateRef: source_node=%i sink_node=%i reforder=%i million=%i\n",
	 source_node, sink_node, reforder, million);
#endif /* DEBUG */

  /* create Reflink node */
  AGCreateEdgeAndNode(dbref, source_node, FROMREFEDGE,
		      REFLINKTYPE,
		      &link_node);
  AGCreateEdge(dbref, link_node, sink_node, TOREFEDGE);

  /* init million and order */
  /* attribute of Reflink */
  /* node */
  
  AGPutAttributeSubstr(dbref, link_node, REFORDERATTR,
		       0, sizeof(reforder),
		       (TEXT) &reforder,
		       FALSE);
  AGPutAttributeSubstr(dbref, link_node, RELMILLIONATTR,
		       0, sizeof(million),
		       (TEXT) &million, FALSE);
}


int
DBGetStoredRef(dbreftype dbref, intIDtype root_node,
	       intIDtype *source_node, int *source_level)
{
/*
 * In order for the restoration of refTo/refFrom relations not to be
 * too inefficient for GRAS, we keep the set of refTo/refFrom
 * relations between calls.  We expect all of the set to be processed.
 * If not, an error occurs.
 */

  static int valid_set = FALSE;		       /* TRUE at entrance if current set */

  /* is valid */
  static SimpleSet from_refs;		       /* Set kept between calls */
  static dbreftype last_db;		       /* 'dbref' in last call */
  static intIDtype last_node;		       /* 'root_node' in last call */

  BOOLEAN found;
  intIDtype ref_node;			       /* source of refTo/refFrom */

  /* relation */
  extIDtype extID;			       /* extID of ref_node */
  intIDtype link_node;			       /* link node of refTo/refFrom */


#ifdef DEBUG
  printf("DBRestoreRef: root_node=%i", root_node);
#endif /* DEBUG */

  if (!valid_set)
    {
      valid_set = TRUE;
      last_db = dbref;
      last_node = root_node;
      AGSetCreate(dbref, &from_refs);
      AGShowAllSourceNodes(dbref, root_node, TOREFEDGE,
			   &from_refs);
    }
  else if (last_db != dbref || ! ((last_node.graph == root_node.graph) && (last_node.entity == root_node.entity)))
    bm_error("DBGetStoredRef",
	     "All found edges of last set were not processed", FALSE);

  AGSetRemoveAnyElement(dbref, &found, &link_node, from_refs);
  while (found)
    {
      AGShowSourceNode(dbref, link_node, FROMREFEDGE, &ref_node);
      extID = DBGetExternalID(dbref, ref_node);
      if (extID >= geometric(AVG_CHILDREN, CLOSURE_LEVEL - 1) + 1)
	{
	  *source_node = link_node;	       /* A GRAS hack - since in GRAS we */
	  /* have edge nodes, what is */
	  /* changed is the edge between the */
	  /* link node and the target node */
	  *source_level = level_of(extID);
	  break;			       /* exit loop */
	}
      AGSetRemoveAnyElement(dbref, &found, &link_node, from_refs);
    }
  if (!found)
    {
      valid_set = FALSE;
      AGSetKill(dbref, &from_refs);
    }
  return (found);
}


void
DBRestoreRef(dbreftype dbref, intIDtype root_node,
	     intIDtype source_node, intIDtype target_node)
{
  AGDeleteEdge(dbref, source_node, root_node, TOREFEDGE);
  AGCreateEdge(dbref, source_node, target_node, TOREFEDGE);
}


void
DBDeleteNode(dbreftype dbref, intIDtype node, intIDtype ref_node)
{
  RelSet context;
  BOOLEAN found;
  NodeNumber edge, targetnode, sourcenode;
  NodeNumber parent_link;
  SimpleSet from_refs;

#ifdef DEBUG
  printf("DBDeleteNode: node=%i ref_node=%i\n", node, ref_node);
#endif /* DEBUG */

  /* first check if the node has any children -> if yes, abort */
  if (AGTestOutgoingEdge(dbref, node, FROMPARENTEDGE))
    bm_error("DBDeleteNode", "Attempt to delete node with children", FALSE);

  /* If not, delete ChildLink nodes and RefLink nodes of outgoing */
  /* refTo/refFrom relations with edges.  Incoming refTo/refFrom */
  /* relations are redirected to ref_node */
  /* finally delete node itself */

  AGRelCreate(dbref, &context);
  AGSetCreate(dbref, &from_refs);

  AGGetOutContextOfNode(dbref, node, &context);		/* references to other nodes */
  AGRelRemoveAnyTuple(dbref, &found, &targetnode, &edge, context);
  while (found)
    {
      AGDeleteNodeAndEdges(dbref, targetnode);
      AGRelRemoveAnyTuple(dbref, &found, &targetnode,&edge ,context);
    }

  /*incoming references */
  AGShowAllSourceNodes(dbref, node, TOREFEDGE, &from_refs);
  AGSetRemoveAnyElement(dbref, &found, &sourcenode, from_refs);
  while (found)
    {
      AGDeleteEdge(dbref, sourcenode, node, TOREFEDGE);
      AGCreateEdge(dbref, sourcenode, ref_node, TOREFEDGE);
      AGSetRemoveAnyElement(dbref, &found, &sourcenode, from_refs);
    }

  AGShowSourceNode(dbref, node, TOCHILDEDGE,   /* parent */
		   &parent_link);

  AGDeleteNodeAndEdges(dbref, node);	       /* delete node itself */
  AGDeleteNodeAndEdges(dbref, parent_link);    /* delete link to parent */

  AGRelKill(dbref, &context);
  AGSetKill(dbref, &from_refs);
}




/***************************************************************************
 Query/update operations
 ***************************************************************************/


intIDtype
DBGetInternalID(dbreftype dbref, extIDtype extID)
{
  BOOLEAN exist;
  intIDtype node;

#ifdef DEBUG
  printf("DBGetInternalID: extID=%i\n", extID);
#endif /* DEBUG */

  AGShowNodeWithKey(dbref, EXTERNALIDATTR, sizeof(extID),
		    (TEXT) &extID, &exist, &node);
  if (exist)
    return (node);
  else
    {      
      bm_error("DBGetInternalID", "Node not found", FALSE);
    }
}


extIDtype
DBGetExternalID(dbreftype dbref, intIDtype node)
{
  CARDINAL extlen;
  extIDtype extID;

#ifdef DEBUG
  printf("DBGetExternalID: node=%i\n", node);
#endif /* DEBUG */

  AGGetAttributeSubstr(dbref, node, EXTERNALIDATTR, 0,
		       sizeof(extID), (TEXT) &extID, &extlen);
  if (extlen == sizeof(extID))
    {
      return (extID);
    }
  else
    {
      bm_error("DBGetExternalID", "Node not found", FALSE);
    }
}


void
DBReadNodeMillionAttr(dbreftype dbref, intIDtype node, int *million)
{
  CARDINAL len;

#ifdef DEBUG
  printf("DBReadNodeMillionAttr: node=%i ", node);
#endif /* DEBUG */

  AGGetAttributeSubstr(dbref, node, OBJMILLIONATTR,
		       0, sizeof(*million),
		       (TEXT) million, &len);
  if (len != sizeof(*million))
    bm_error("DBReadNodeMillionAttr", "0 length attribute returned", FALSE);

#ifdef DEBUG
  printf("million=%i\n", *million);
#endif /* DEBUG */
}


void
DBReadNodeTextAttr(dbreftype dbref, intIDtype node,
		   char text[TEXT_MAX_LEN + 1], int *textlen)
{
  CARDINAL len;
  
#ifdef DEBUG
  printf("DBReadNodeTextAttr: node=%i ", node);
#endif /* DEBUG */

  AGGetAttributeSubstr(dbref, node, TEXTATTR, 0,
		       (sizeof(char) *(TEXT_MAX_LEN + 1)),
		       text,
		       &len);

  if (len == 0)
    bm_error("DBReadNodeTextAttr", "0 length attribute returned", FALSE);

  *textlen = (int) len - 1;		       /* do not count \0 character */
#ifdef DEBUG
  printf("textlen=%i text=%s\n", *textlen, text);
#endif /* DEBUG */
}



void
DBWriteNodeMillionAttr(dbreftype dbref, intIDtype node, int million)
{
#ifdef DEBUG
  printf("DBWriteNodeMillionAttr: node=%i million=%i\n", node, million);
#endif /* DEBUG */
  
  AGPutAttributeSubstr(dbref, node, OBJMILLIONATTR,
		       0, sizeof(million),
		       (TEXT) &million,
		       FALSE);
}


void
DBWriteNodeTextAttr(dbreftype dbref, intIDtype node,
		    char *text, int textlen)
{
#ifdef DEBUG
  printf("DBWriteNodeTextAttr: node=%i textlen=%i text=%s\n",
	 node, textlen, text);
#endif /* DEBUG */

  AGPutAttributeSubstr(dbref, node, TEXTATTR, 0,
		       textlen + 1,
		       text,
		       TRUE);
}


void
DBGetChildren(dbreftype dbref, int ordered,
	      intIDtype parent_node, int *no_of_children,
	      intIDtype children_nodes[MAX_CHILDREN])
{
  SimpleSet context;
  BOOLEAN found;
  NodeNumber linknode;
  intIDtype targetnode;
  int order;
  CARDINAL len;
  int childno = 0;

#ifdef DEBUG
  printf("DBGetChildren: node=%i\n", parent_node);
#endif /* DEBUG */

  AGSetCreate(dbref, &context);
  AGShowAllTargetNodes(dbref, parent_node, FROMPARENTEDGE, &context);
  AGSetRemoveAnyElement(dbref, &found, &linknode, context);
  while (found)
    {
      AGShowTargetNode(dbref, linknode, TOCHILDEDGE, &targetnode);

      AGGetAttributeSubstr(dbref, linknode, ORDERATTR,
			   0, sizeof(order),
			   (TEXT) &order, &len);
      if (len == 0)
	bm_error("DBGetChildren", "0 length attribute returned", FALSE);

      if (ordered)			       /* order children found */
	children_nodes[order - 1] = targetnode;
      else				       /* don't order */
	children_nodes[childno] = targetnode;

      childno++;
      AGSetRemoveAnyElement(dbref, &found, &linknode, context);

#ifdef DEBUG
      printf("\tchild_node=%i ", targetnode);
      if (ordered)
	printf("ordered=%i", ordered);
      printf("\n");
#endif /* DEBUG */

    }					       /* while */

  *no_of_children = childno;
  AGSetKill(dbref, &context);

}


void
DBGetChildrenWithAttr(dbreftype dbref, int ordered,
		      intIDtype parent_node,
		      int *no_of_children,
		      int million_attrs[MAX_CHILDREN],
		      intIDtype children_nodes[MAX_CHILDREN])
{
  SimpleSet context;
  BOOLEAN found;
  NodeNumber linknode;
  intIDtype targetnode;
  int order;
  int million;
  CARDINAL len;
  int childno = 0;

#ifdef DEBUG
  printf("DBGetChildrenWithAttr: node=%i\n", parent_node);
#endif /* DEBUG */
  AGSetCreate(dbref, &context);
  AGShowAllTargetNodes(dbref, parent_node, FROMPARENTEDGE, &context);
  AGSetRemoveAnyElement(dbref, &found, &linknode, context);
  while (found)
    {
      AGGetAttributeSubstr(dbref, linknode, RELMILLIONATTR,
			   0, sizeof(million),
			   (TEXT) &million, &len);
      if (len == 0)
	bm_error("DBGetChildrenWithAttr", "0 length attribute returned", FALSE);

      AGGetAttributeSubstr(dbref, linknode, ORDERATTR,
			   0, sizeof(order),
			   (TEXT) &order, &len);
      if (len == 0)
	bm_error("DBGetChildrenWithAttr",
		 "0 length attribute returned", FALSE);

      AGShowTargetNode(dbref, linknode, TOCHILDEDGE, &targetnode);

      if (ordered)
	{				       /* order children found */
	  children_nodes[order - 1] = targetnode;
	  million_attrs[order - 1] = million;
	}
      else
	{				       /* don't order */
	  children_nodes[childno] = targetnode;
	  million_attrs[childno] = million;
	}

      childno++;
      AGSetRemoveAnyElement(dbref, &found, &linknode, context);

#ifdef DEBUG
      printf("\tchild_node=%i relmillion=%i ", targetnode, million);
      if (ordered)
	printf("order=%i", order);
      printf("\n");
#endif /* DEBUG */

    }					       /* while */

  *no_of_children = childno;
  AGSetKill(dbref, &context);
}


int
DBGetParent(dbreftype dbref, intIDtype child_node,
	    intIDtype *parent_node)
{
  NodeNumber linknode;
  CARDINAL SourceNrs;

#ifdef DEBUG
  printf("DBGetParent: node=%i ", child_node);
#endif /* DEBUG */
  AGTestAndShowSourceNode(dbref, child_node, TOCHILDEDGE, &SourceNrs,
			  &linknode);
  if (SourceNrs == 0)
    {
#ifdef DEBUG
      printf("(NO PARENT)\n");
#endif /* DEBUG */
      return FALSE;
    }
  else if (SourceNrs == 1)
    {
      AGShowSourceNode(dbref, linknode, FROMPARENTEDGE, parent_node);
#ifdef DEBUG
      printf("parent_node=%i\n", *parent_node);
#endif /* DEBUG */
      return TRUE;
    }
  else
    {
      bm_error("DBGetParent", "Node has more than one parent", FALSE);
    }
}


int
DBGetParentWithAttr(dbreftype dbref, intIDtype child_node,
		    int *million, intIDtype *parent_node)
{
  NodeNumber linknode;
  CARDINAL len;
  CARDINAL SourceNrs;

#ifdef DEBUG
  printf("DBGetParentWithAttr: node=%i ", child_node);
#endif /* DEBUG */
  AGTestAndShowSourceNode(dbref, child_node, TOCHILDEDGE,
			  &SourceNrs, &linknode);
  if (SourceNrs == 0)
    {
#ifdef DEBUG
      printf("(NO PARENT)\n");
#endif /* DEBUG */
      return FALSE;
    }
  else if (SourceNrs == 1)
    {
      AGGetAttributeSubstr(dbref, linknode, RELMILLIONATTR, 0,
			   sizeof(*million), (TEXT) million, &len);
      if (len == 0)
	bm_error("DBGetParentWithAttr", "0 length attribute returned", FALSE);
      AGShowSourceNode(dbref, linknode, FROMPARENTEDGE, parent_node);
#ifdef DEBUG
      printf(" parent_node=%i relmillion=%i\n", *parent_node, *million);
#endif /* DEBUG */
      return TRUE;
    }
  else
    bm_error("DBGetParentWithAttr", "Node has more than one parent", FALSE);
}


void
DBGetReferences(dbreftype dbref, int ordered, intIDtype node,
		int *no_of_refs, intIDtype ref_nodes[MAX_REFS])
{
  SimpleSet context;
  BOOLEAN found;
  NodeNumber linknode;
  intIDtype targetnode;
  int reforder;
  CARDINAL len;
  int refno = 0;

#ifdef DEBUG
  printf("DBGetReferences: node=%i ", node);
#endif /* DEBUG */
  AGSetCreate(dbref, &context);
  AGShowAllTargetNodes(dbref, node, FROMREFEDGE, &context);
  AGSetRemoveAnyElement(dbref, &found, &linknode, context);
  while (found)
    {
      AGShowTargetNode(dbref, linknode, TOREFEDGE, &targetnode);

      AGGetAttributeSubstr(dbref, linknode, REFORDERATTR,
			   0, sizeof(reforder),
			   (TEXT) &reforder, &len);
      if (len == 0)
	bm_error("DBGetReferences", "0 length attribute returned", FALSE);

      if (ordered)			       /* order references found */
	ref_nodes[reforder - 1] = targetnode;
      else				       /* don't order */
	ref_nodes[refno] = targetnode;

      refno++;
      AGSetRemoveAnyElement(dbref, &found, &linknode, context);

#ifdef DEBUG
      printf("targetnode=%i ", targetnode);
      if (ordered)
	printf("ordered=%i", ordered);
      printf("\n");
#endif /* DEBUG */

    }

  *no_of_refs = refno;
  AGSetKill(dbref, &context);
}


void
DBGetRefsWithAttr(dbreftype dbref, int ordered, intIDtype node,
		  int *no_of_refs,
		  int million_attrs[MAX_REFS],
		  intIDtype ref_nodes[MAX_REFS])
{
  SimpleSet context;
  BOOLEAN found;
  NodeNumber linknode;
  intIDtype targetnode;
  int reforder;
  int million;
  CARDINAL len;
  int refno = 0;

#ifdef DEBUG
  printf("DBGetRefsWithAttr: node=%i\n", node);
#endif /* DEBUG */
  AGSetCreate(dbref, &context);
  AGShowAllTargetNodes(dbref, node, FROMREFEDGE, &context);
  AGSetRemoveAnyElement(dbref, &found, &linknode, context);
  while (found)
    {
      AGGetAttributeSubstr(dbref, linknode, RELMILLIONATTR,
			   0, sizeof(million),
			   (TEXT) &million, &len);
      if (len == 0)
	bm_error("DBGetRefsWithAttr", "0 length attribute returned", FALSE);

      AGGetAttributeSubstr(dbref, linknode, REFORDERATTR,
			   0, sizeof(reforder),
			   (TEXT) &reforder, &len);
      if (len == 0)
	bm_error("DBGetRefsWithAttr", "0 length attribute returned", FALSE);

      AGShowTargetNode(dbref, linknode, TOREFEDGE, &targetnode);

      if (ordered)
	{				       /* order references found */
	  ref_nodes[reforder - 1] = targetnode;
	  million_attrs[reforder - 1] = million;
	}
      else
	{				       /* don't order */
	  ref_nodes[refno] = targetnode;
	  million_attrs[refno] = million;
	}

      refno++;
      AGSetRemoveAnyElement(dbref, &found, &linknode, context);

#ifdef DEBUG
      printf("\ttargetnode=%i relmillion=%i", targetnode, million);
      if (ordered)
	printf(" order=%i", reforder);
      printf("\n");
#endif /* DEBUG */

    }					       /* while */

  *no_of_refs = refno;
  AGSetKill(dbref, &context);
}


void
DBGetRevRefsWithAttr(dbreftype dbref, intIDtype node,
		     int *no_of_revrefs,
		     int million_attrs[MAX_REVREFS],
		     intIDtype revref_nodes[MAX_REVREFS])
{
  SimpleSet context;
  BOOLEAN found;
  NodeNumber linknode;
  intIDtype sourcenode;
  int million;
  CARDINAL len;
  int refno = 0;

#ifdef DEBUG
  printf("DBGetRevRefsWithAttr: node=%i\n", node);
#endif /* DEBUG */
  AGSetCreate(dbref, &context);
  AGShowAllSourceNodes(dbref, node, TOREFEDGE, &context);
  AGSetRemoveAnyElement(dbref, &found, &linknode, context);
  while (found)
    {
      AGGetAttributeSubstr(dbref, linknode, RELMILLIONATTR,
			   0, sizeof(million),
			   (TEXT) &million, &len);
      if (len == 0)
	bm_error("DBGetRevRefsWithAttr", "0 length attribute returned", FALSE);
      million_attrs[refno] = million;
      AGShowSourceNode(dbref, linknode, FROMREFEDGE, &sourcenode);
      revref_nodes[refno] = sourcenode;
#ifdef DEBUG
      printf("\tsourcenode=%i relmillion=%i\n", sourcenode, million);
#endif /* DEBUG */
      refno++;
      AGSetRemoveAnyElement(dbref, &found, &linknode, context);
    }
  AGSetKill(dbref, &context);
  *no_of_revrefs = refno;
}

