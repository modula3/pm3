/* Headerdatei fuer RGRASGraph. */
/* Automatisch generiert */
/* Typedeklarationen stehen in C_Global.h in Abschnitt RGRASGraph */

#if !defined (C_RGRASGraph_h_)
#define C_RGRASGraph_h_

#ifdef __cplusplus
extern "C" {
#endif

#include <string.h>
#include <stdio.h>
#include <CTypesForM3.h>
#include <rgglobal.h>

/**************************************************************************
 * Created by:  Roland Baumann						   *
 * $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:46  hosking
    Initial revision

    Revision 1.5  1998/07/29 09:29:11  roland
    C interface and jump-tables are now generated in a compiler dependent
    fashion, so that sub-directories for compiler/platform are not needed
    anymore.

    Revision 1.2  1998/05/28 13:31:19  roland
    CM3 does not support Platforms variable. Changed to TARGET.

    Revision 1.1  1998/05/27 16:58:02  roland
    Platform and compiler specific code is now separated by directory names.

    Revision 1.2  1998/03/14 09:21:54  renehuel
    Removed unused parameter from procedure CollectGarbage.

    Revision 1.1  1998/03/10 15:52:19  renehuel
    These files implement the c-interface to the rgras-interface.

    Revision 1.3  1997/10/24 14:39:05  renehuel
    These files implement the new RGRASGraph interface.

    Revision 1.2  1997/05/13 09:30:02  roland
    Interface cleaned up a little.
    Implementation uses current modul names.

    Revision 1.1  1996/09/13 15:44:33  walkmark
    Interface to use old GRAS Implementations with new GRAS (basic
    procedures) (hope it will work)

*
 **************************************************************************/
 
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Initialization of the GRAS system.                 *
 *                                                      *
 *   ClientRoot is the directory in which private       *
 *   client data is stored. IF not given, RGRAS tries   *
 *   to read the environment variable GRAS3 instead.    *
 *                                                      *
 *   CacheSize determines the size of the GRAS client   *
 *   cache in 8 kByte pages (default is 100).           *
 *                                                      *
 *   ServerId is the name of the GRAS page server to    *
 *   which this client should try to connect. Default   *
 *   is GRAS-3.<uid>, with <uid> being the numerical    *
 *   user id of the user running the program.           *
 *                                                      *
 *   Agent is a host name on which the netobjd process  *
 *   is running. Default is given in Config.i3          *
 *                                                      *
 *                                                      *
 ********************************************************/

void
AGLogin(TEXT ClientRoot, CARDINAL CacheSize, TEXT ServerId, TEXT Agent);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Logging out the GRAS system.                       *
 *   All opened schemes, graphs and pools are closed.   *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   RessourceTriggerStorage.TriggerDelayed             *
 *   GraphPool.InternalError                            *
 *   Graph.InternalError                                *
 *   Scheme.InternalError                               *
 *                                                      *
 ********************************************************/

void
AGLogout();

/************************************************************************
 *                                                                      *
 *                Operations for managing graph pools                   *
 *                                                                      *
 ************************************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Creation of a graph pool which can hold several    *
 *   graphs.                                            *
 *                                                      *
 * Errors Returned:                                     *
 *                                                      *
 *   AlreadyExistent: pool already exists               *
 *   AlreadyOpen : pool already open                    *
 *   NotExistent : pool does not exist                  *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Denied                                      *
 *   Access.Locked                                      *
 *   PageFile.NoAccess                                  *
 *   GraphPool.InternalError                            *
 *   RGRASNames.InternalError                           *
 *   GraphPool.CardinalityError                         *
 *   GraphPool.NotInTransaction                         *
 *   GraphSystem.InternalError                          *
 *                                                      *
 ********************************************************/

void
AGOpenGraphPool(PoolName poolName, GraphPoolMode mode, TStatus *status);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Closes the graph pool.                             *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   ResourceTriggerStorage.TriggerDelayed              *
 *   GraphPool.InternalError                            *
 *   The pool is not open                               *
 *                                                      *
 ********************************************************/

void
AGCloseGraphPool(PoolName poolName);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Deletes a pool if there are no graphs  or schemes  *
 *   left in it, or force = TRUE.                       *
 *                                                      *
 * Errors Returned:                                     *
 *                                                      *
 *   NotExistent: pool does not exist                   *
 *   NotEmpty   : there is at least one graph or        *
 *                scheme left in the pool.              *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   PageFile.NoAccess                                  *
 *   GraphSystem.InternalError                          *
 *   ResourceTriggerStorage.TriggerDelayed              *
 *   GraphPool.InternalError                            *
 *   Access.Denied                                      *
 *   Access.Locked                                      *
 *   GraphPool.CardinalityError                         *
 *   GraphPool.NotInTransaction                         *
 *   The pool is not open                               *
 *                                                      *
 ********************************************************/

void
AGDeleteGraphPool(PoolName poolName, TStatus *status, BOOLEAN force);

 /********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Reorganizes the graph pool by removing all unused  *
 *   information from the management graph. Besides,    *
 *   all unnecessary GRAS files are deleted. Deltas are *
 *   packed and combined if possible. Furthermore all   *
 *   existing locks on graphs are released (which are   *
 *   due to application or system crashes) and recovery *
 *   is initiated for all previously locked graphs      *
 *   based on information in TMPGRAS (see also AGCheck- *
 *   Graph).                                            *
 *                                                      *
 * Warning:                                             *
 *                                                      *
 *   This PROCEDURE may only be called by privileged    *
 *   application modules, because any access to the     *
 *   graph pool is locked until AGReorganizeGraphPool   *
 *   is terminated.                                     *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   AGReorganizeGraphPool may take a rather long time  *
 *   to do its job!                                     *
 *                                                      *
 * Errors Returned:                                     *
 *                                                      *
 *   AlreadyExistent: pool already exists               *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *                                                      *
 ********************************************************/

 /********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   are due to application or system crashes) and      *
 *   recovery is initiated for all previously locked    *
 *   graphs based on information in TMPGRAS.            *
 *                                                      *
 * Warning:                                             *
 *                                                      *
 * Note:                                                *
 *                                                      *
 * Errors Returned:                                     *
 *                                                      *
 *   AlreadyExistent: pool already exists               *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *                                                      *
 ********************************************************/

 /********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   All existing locks on graphs are released (which   *
 *   are due to application or system crashes).         *
 * Warning:                                             *
 *                                                      *
 * Note:                                                *
 *                                                      *
 * Errors Returned:                                     *
 *                                                      *
 *   AlreadyExistent: pool already exists               *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *                                                      *
 ********************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Tests, whether the pool exists. If so, a list of   *
 *   all external graph numbers of the graphs in this   *
 *   pool is created. It can be referred to with the    *
 *   PROCEDURE "AGGiveNextGraphInGraphPool".            *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   GraphSystem.InternalError                          *
 *   Access.Denied                                      *
 *   Access.Locked                                      *
 *   PageFile.NoAccess                                  *
 *   GraphPool.InternalError                            *
 *   ResourceTriggerStorage.TriggerDelayed              *
 *   ChgMgmtGraphPool.InternalError                     *
 *   ChgMgmtGraphPool.NotExistent                       *
 *   GraphPool.CardinalityError                         *
 *   GraphPool.NotInTransaction                         *
 *                                                      *
 ********************************************************/

void
AGGetGraphsInGraphPool(PoolName poolName, BOOLEAN *existent);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   The result of this function is the next external   *
 *   graph number in the list created by the PROCEDURE  *
 *   "AGGetGraphsInGraphPool".                          *
 *                                                      *
 ********************************************************/

void
AGGiveNextGraphInGraphPool(ExternNumber *extNumber, BOOLEAN *existent);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Creates a copy of an open graph and places it in   *
 *   pool 'targetPoolName'. 'targetGraph' can be used   *
 *   directly and doesn't need to be reconstructed with *
 *   a delta.                                           *
 *                                                      *
 * Errors Returned:                                     *
 *                                                      *
 *   NotExistent    : TargetPool doesn't exist          *
 *   AlreadyExistent: graph already exists              *
 *   StillPendingTransactions : The targetpool is still *
 *   in transaction                                     *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Access.Denied                                      *
 *   PageFile.NoAccess                                  *
 *   Graph.InternalError                                *
 *   GraphPool.CardinalityError                         *
 *   GraphPool.InternalError                            *
 *   GraphPool.Existent                                 *
 *   GraphPool.NotExistent                              *
 *   GraphPool.NotInTransaction                         *
 *   GraphPool.InTransaction                            *
 *   GraphPool.InUse                                    *
 *   GraphSystem.InternalError                          *
 *   ResourceTriggerStorage.TriggerDelayed              *
 *   TypedNames.InternalError                           *
 *   TypedNames.Unknown                                 *
 *   GraphPool.NoGraph                                  *
 *   GraphPool.NoScheme                                 *
 *   GraphPool.NotExistent                              *
 *                                                      *
 ********************************************************/

void
AGCopyGraph(GraphNumber sourceGraph, PoolName targetPoolName, GraphName targetGrName, TStatus *status);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Copies the closed graph "sourceGrName" from the    *
 *   pool "poolName" logically to "targetGrName" in the *
 *   same pool. If "Forward" is set to TRUE, the copy is*
 *   performed with a forward delta, else with a back-  *
 *   ward delta.                                        *
 *                                                      *
 * Note:                                                *
 *   Take care of the fact, that AGDeltaCopyGraph needs *
 *   a closed source graph whereas AGCopyGraph needs an *
 *   open one!                                          *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   ChgMgmtGraphPool.Existent                          *
 *   ChgMgmtGraphPool.InUse                             *
 *   ChgMgmtGraphPool.NotExistent                       *
 *   ChgMgmtGraphPool.InternalError                     *
 *   GraphPool.InternalError                            *
 *   GraphPool.CardinalityError                         *
 *   GraphPool.NotInTransaction                         *
 *                                                      *
 ********************************************************/

void
AGDeltaCopyGraph(PoolName poolName, GraphName sourceGrName, GraphName targetGrName, BOOLEAN forward, TStatus *Status);

 /********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Tests wether the graph is opened AND an access     *
 *   group exists. Then the parameter OneGroup is       *
 *   TRUE, if the graph might be accessed only          *
 *   by members in exactly this group (i.e. no other    *
 *   groups can exist at the same time). The identi-    *
 *   fication of the group is returned in Group.        *
 *   The lock duration to the graph for all group       *
 *   members is returned in LockMode.                   *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   If the parameter Existent is FALSE just as it was  *
 *   in the call to AGGetWriterGroupInfo then the graph *
 *   is not opened.                                     *
 *   If the parameter OneGroup is TRUE, a subsequent    *
 *   call to AGGetNextReaderGroupInfo always yields     *
 *   Existent = FALSE.                                  *
 *                                                      *
 ********************************************************/

 /********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Returns information on the next group if existent. *
 *                                                      *
 ********************************************************/

 /********************************************************
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *                                                      *
 ********************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Checks, whether a graph with the external number   *
 *   "extNumber" exists in the pool. If so, the         *
 *   following information is returned:                 *
 *   - its name                                         *
 *   - whether the graph is opened by the current       *
 *     application and if so,                           *
 *     - its temporary graph number                     *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   GraphSystem.InternalError                          *
 *   Access.Denied                                      *
 *   Access.Locked                                      *
 *   PageFile.NoAccess                                  *
 *   GraphPool.InternalError                            *
 *   ResourceTriggerStorage.TriggerDelayed              *
 *   ChgMgmtGraphPool.InternalError                     *
 *   ChgMgmtGraphPool.NotExistent                       *
 *   GraphPool.CardinalityError                         *
 *   GraphPool.NotInTransaction                         *
 *                                                      *
 ********************************************************/

void
AGGetInfoByExtNo(PoolName poolName, ExternNumber extNumber, BOOLEAN *existent, GraphName grName, GraphType *grType, BOOLEAN *isOpen, GraphNumber *graph);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Checks, whether a graph with the specified name    *
 *   "grName" exists in the pool. If so, the following  *
 *   information is returned:                           *
 *   - its external number                              *
 *   - whether the graph is opened by the current       *
 *     application and if so,                           *
 *     - its temporary graph number                     *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   GraphSystem.InternalError                          *
 *   Access.Denied                                      *
 *   Access.Locked                                      *
 *   PageFile.NoAccess                                  *
 *   GraphPool.InternalError                            *
 *   ChgMgmtGraphPool.InternalError                     *
 *   ChgMgmtGraphPool.NotExistent                       *
 *   ResourceTriggerStorage.TriggerDelayed              *
 *   GraphPool.CardinalityError                         *
 *   GraphPool.NotInTransaction                         *
 *                                                      *
 ********************************************************/

void
AGGetInfoByName(PoolName poolName, GraphName grName, BOOLEAN *existent, GraphType *grType, ExternNumber *extNumber, BOOLEAN *isOpen, GraphNumber *graphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *   Creates or opens a graph. If already open, then    *
 *   closes the graph and uses the appropriate          *
 *   parameters for opening the graph again.            *
 *                                                      *
 * Functionality:                                       *
 *                                                      *
 *   Opens a graph referred to by poolName and grName   *
 *                                                      *
 * Errors Returned:                                     *
 *                                                      *
 *   NotExistent          : the graph or the pool does  *
 *                          not exist                   *
 *   NoScheme             : the scheme does not exist   *
 *   AlreadyExistent      : the graph does already      *
 *                          exist                       *
 *   StillPendingTransaction : The pool is still in     *
 *                             transaction              *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   In addition to the error codes mentioned above     *
 *   errors are returned which occur due to the use     *
 *   of the standard open function AGOpenGraph.         *
 *                                                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Graph.InUse                                        *
 *   Graph.InternalError                                *
 *   Graph.NoScheme                                     *
 *   Graph.NotExistent                                  *
 *   ChgMgmtGraphPool.InternalError                     *
 *   ChgMgmtGraphPool.NotExistent                       *
 *   GraphPool.InternalError                            *
 *   GraphPool.CardinalityError                         *
 *   GraphPool.NotInTransaction                         *
 *   GraphSystem.InternalError                          *
 *   Access.Denied                                      *
 *   Access.Locked                                      *
 *   PageFile.NoAccess                                  *
 *   RGRASNames.InternalError                           *
 *   GraphPool.NoGraph                                  *
 *   GraphPool.NoScheme                                 *
 *   GraphPool.NotExistent                              *
 *                                                      *
 ********************************************************/

void
AGOpenGraph(PoolName poolName, GraphName grName, GraphType *grType, GraphMode grMode, GroupName gpName, GroupType gpType, GroupMode gpMode, BOOLEAN oneGroup, BOOLEAN noWait, TStatus *status, ExternNumber *extNumber, GraphNumber *grNumber, SchemeName schemeName);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Closes a graph and resets the corresponding access *
 *   locks. This PROCEDURE may only be called at a time *
 *   when all transactions concerning the graph are     *
 *   committed.                                         *
 *                                                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Graph.InternalError                                *
 *   The graph is not open                              *
 *   Pending transactions                               *
 *                                                      *
 ********************************************************/

void
AGCloseGraph(GraphNumber graphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   The closed graph "grName" in the GraphPool         *
 *   "poolName" will be deleeted.                       *
 *                                                      *
 *   If there is a lock on the management graph, one    *
 *   should try to copy all graphs from the pool into   *
 *   a new GraphPool (...if this is still possible with *
 *   the corrupted management graph). Therefore one     *
 *   needs to reset all locks with the help of the      *
 *   operating system.                                  *
 *                                                      *
 * Errors Returned:                                     *
 *                                                      *
 *   NotExistent: graph or pool does not exist          *
 *   StillOpen  : the graph is still opened by          *
 *                an application program                *
 *   StillPendingTransactions : The pool is still in    *
 *                              transaction             *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   GraphSystem.InternalError                          *
 *   Access.Locked                                      *
 *   Access.Denied                                      *
 *   PageFile.NoAccess                                  *
 *   GraphPool.InternalError                            *
 *   GraphPool.InUse                                    *
 *   GraphPool.NoGraph                                  *
 *   GraphPool.NotExistent                              *
 *   GraphPool.CardinalityError                         *
 *   GraphPool.NotInTransaction                         *
 *   ResourceTriggerStorage.TriggerDelayed              *
 *                                                      *
 ********************************************************/

void
AGDeleteGraph(PoolName poolName, GraphName grName, BOOLEAN force, TStatus *status);
/************************************************************************
 *                                                                      *
 *            Operations for Primitive Recovery Mechanisms              *
 *                                                                      *
 ************************************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Starts a new transaction. This transaction can     *
 *   either be committed normally (then it has no       *
 *   effects on the state of the graph) or be aborted   *
 *   (that means, that the graph is reset to the state  *
 *   when AGStartTransaction was called).               *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   All restrictions and effects of this PROCEDURE are *
 *   mentioned in the module comments.                  *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   GraphPool.InternalError                            *
 *   The pool is not open                               *
 *                                                      *
 ********************************************************/

void
AGStartTransaction(PoolName poolName);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Commits all transactions that were started on the  *
 *   specified graph.                                   *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   GraphPool.InternalError                            *
 *   GraphPool.CardinalityError                         *
 *   GraphPool.NotInTransaction                         *
 *   The pool is not open                               *
 *                                                      *
 ********************************************************/

void
AGCommitToplevelTransaction(PoolName poolName);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Commits a transaction that was started by calling  *
 *   AGStartTransaction in the normal way. That means,  *
 *   that all graph changing operations performed       *
 *   during the transaction take place in the graph.    *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   GraphPool.InternalError                            *
 *   GraphPool.CardinalityError                         *
 *   GraphPool.NotInTransaction                         *
 *   The pool is not open                               *
 *                                                      *
 ********************************************************/

void
AGCommitTransaction(PoolName poolName);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Resets the graph to the state right before         *
 *   initiating the first transaction.                  *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   GraphPool.InternalError                            *
 *   GraphPool.NotInTransaction                         *
 *   The pool is not open                               *
 *                                                      *
 ********************************************************/

void
AGAbortToplevelTransaction(PoolName poolName);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Aborts a transaction that was started by calling   *
 *   AGStartTransaction. The effect is, that the graph  *
 *   is reset into the state at the time the trans-     *
 *   action was started.                                *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   GraphPool.InternalError                            *
 *   GraphPool.NotInTransaction                         *
 *   The pool is not open                               *
 *                                                      *
 ********************************************************/

void
AGAbortTransaction(PoolName poolName);
/************************************************************************
 *                                                                      *
 *      Operations for Requests and Modifications on Single Graphs      *
 *                                                                      *
 * Note:                                                                *
 *                                                                      *
 *   The effects of these operations can be reset by aborting trans-    *
 *   actions (but this does not change the state of sets!)              *
 *                                                                      *
 ************************************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Creates a new node in graph "graphNumber".         *
 *   This node gets the type "nType".                   *
 *   The parameter "environment" may                    *
 *   specify another node, which represents a logical   *
 *   neighbor to the current one. "nodeNr" passes the   *
 *   number of the new node to the application. It is   *
 *   set to "environment", if that number is not yet    *
 *   used.                                              *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   The parameter "environment" is internally used to  *
 *   place the nodes onto pages in order to minimize    *
 *   disk accesses. Therefore logically neighbored      *
 *   nodes should always be stored as physically neigh- *
 *   bored ones.                                        *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.NotOwner                                     *
 *   Graph.LogError                                     *
 *   Graph.Unknown                                      *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGCreateNode(GraphNumber graphNumber, TypeNumber nType, NodeNumber environment, NodeNumber *nodeNr);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Inserts a new edge from node "sourceNode" to a     *
 *   node of type "nType". The edge gets the type       *
 *   "eType". "nodeNr" returns the number of the new    *
 *   node. The new node is placed in physical neigh-    *
 *   borhood to the existing one.                       *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.NotOwner                                     *
 *   Graph.LogError                                     *
 *   Graph.Unknown                                      *
 *   Graph.CardinalityError                             *
 *   Graph.NodeNotFound                                 *
 *   Graph.WrongType                                    *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGCreateEdgeAndNode(GraphNumber graphNumber, NodeNumber sourceNode, TypeNumber eType, TypeNumber nType, NodeNumber *nodeNr);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Deletes the specified node with all its incoming   *
 *   and outgoing edges from the graph.                 *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.NotOwner                                     *
 *   graph.LogError                                     *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGDeleteNodeAndEdges(GraphNumber graphNumber, NodeNumber node);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   This PROCEDURE computes a number which is unique   *
 *   for the whole graph pool.                          *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   RGRASNames.InternalError                           *
 *   Access.Locked                                      *
 *   Names.Undeclared                                   *
 *   Names.Unknown                                      *
 *   GraphPool.InernalError                             *
 *   GraphPool.CardinalityError                         *
 *   GraphPool.NotInTransaction                         *
 *   The pool could not be found                        *
 *                                                      *
 ********************************************************/

void
AGGetUniqueNodeId(GraphNumber graphNumber, CARDINAL *uniqueID);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Returns a set of nodes the given attribute value   *
 *   is associated with.                                *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   This PROCEDURE may not be used within evaluation-  *
 *   functions of attributes.                           *
 *                                                      *
 *   Furthermore, in the case of a (boolean) constraint *
 *   attribute this query is only supported for "value" *
 *   equal to "Violated" = FALSE (with "Length" = 1) in *
 *   order to avoid creation of a large useless index   *
 *   for all nodes which respect a certain constraint.  *
 *                                                      *
 *   Use "AGSetRemoveAnyElement" to access all nodes    *
 *   stored within the temporarily existing "SimpleSet" *
 *   and use "AGSetKill" afterwards to release all      *
 *   storage occupied by this set.                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.CyclicEvaluation                             *
 *   Graph.LogError                                     *
 *   Graph.NoIndex                                      *
 *   Graph.Unknown                                      *
 *   Graph.InternalError                                *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGShowAllNodesWithIndex(GraphNumber graphNumber, TypeNumber attNo, CARDINAL attLength, TEXT value, SimpleSet *setOfNodes);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Returns the number of the node, the given attribute*
 *   value is associated with.                          *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   This PROCEDURE may not be used within evaluation-  *
 *   functions of attributes.                           *
 *                                                      *
 *   Furthermore, in the case of a (boolean) constraint *
 *   attribute this query is only supported for "value" *
 *   equal to "Violated" = FALSE (with "Length" = 1) in *
 *   order to avoid creation of a large useless index   *
 *   for all nodes which respect a certain constraint.  *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.CyclicEvaluation                             *
 *   Graph.LogError                                     *
 *   Graph.NoIndex                                      *
 *   Graph.Unknown                                      *
 *   Graph.InternalError                                *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGShowNodeWithKey(GraphNumber graphNumber, TypeNumber attNo, CARDINAL attLength, TEXT value, BOOLEAN *exist, NodeNumber *node);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Returns the type of a given node. If the node does *
 *   not exist, nType.entity is set to                  *
 *   UndefinedTypeNumber=0.                             *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.NodeNotFound                                 *
 *   Graph.NotOwner                                     *
 *   Graph.Unknown                                      *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGShowTypeOfNode(GraphNumber graphNumber, NodeNumber node, TypeNumber *nType);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Inserts a new directed edge from node "sourceNode" *
 *   to node "targetNode". This edge gets the type      *
 *   "eType".                                           *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   Edges of the composite category can only be        *
 *   created as incoming ones, when the node is created.*
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.CardinalityError                             *
 *   Graph.LogError                                     *
 *   Graph.NodeNotFound                                 *
 *   Graph.NotOwner                                     *
 *   Graph.Unknown                                      *
 *   Graph.WrongType                                    *
 *   Graph.InternalError                                *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGCreateEdge(GraphNumber graphNumber, NodeNumber sourceNode, NodeNumber targetNode, TypeNumber eType);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Deletes the edge of type "eType" between the       *
 *   nodes "sourceNode" and "targetNode".               *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   Edges of the composite category can only be        *
 *   deleted together with their corresponding nodes.   *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.CardinalityError                             *
 *   Graph.NotOwner                                     *
 *   Graph.NodeNotFound                                 *
 *   Graph.Unknown                                      *
 *   Graph.WrongType                                    *
 *   Graph.LogError                                     *
 *   Graph.InternalError                                *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGDeleteEdge(GraphNumber graphNumber, NodeNumber sourceNode, NodeNumber targetNode, TypeNumber eType);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Tests, whether there is a directed edge between    *
 *   the nodes.                                         *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Graph.InternalError                                *
 *   Access.Locked                                      *
 *   Graph.NodeNotFound                                 *
 *   Graph.NotOwner                                     *
 *   Graph.Unknown                                      *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

BOOLEAN
AGEdgeExists(GraphNumber graphNumber, NodeNumber sourceNode, NodeNumber targetNode, TypeNumber eType);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Returns the source node to a given edge and target *
 *   node.                                              *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   There must be one incoming edge of type "eType"    *
 *   at the node "targetNode".                          *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.NodeNotFound                                 *
 *   Graph.Unknown                                      *
 *   Graph.NotOwner                                     *
 *   The graph is not open                              *
 *   More than one source node                          *
 *                                                      *
 ********************************************************/

void
AGShowSourceNode(GraphNumber graphNumber, NodeNumber targetNode, TypeNumber eType, NodeNumber *sourceNode);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   "sourceNrs" specifies, how many edges of type      *
 *   "eType" come into "targetNode". If this count      *
 *   is exactly 1, the number of the source node is     *
 *   returned.                                          *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.NodeNotFound                                 *
 *   Graph.Unknown                                      *
 *   Graph.NotOwner                                     *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGTestAndShowSourceNode(GraphNumber graphNumber, NodeNumber targetNode, TypeNumber eType, CARDINAL *sourceNrs, NodeNumber *sourceNode);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Returns all numbers of source nodes to a given     *
 *   pair (edge,target node).                           *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   Use "AGSetRemoveAnyElement" to access all nodes    *
 *   stored within the temporarily existing "SimpleSet" *
 *   and use "AGSetKill" afterwards to release all      *
 *   storage occupied by this set.                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.NodeNotFound                                 *
 *   Graph.Unknown                                      *
 *   Graph.NotOwner                                     *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGShowAllSourceNodes(GraphNumber graphNumber, NodeNumber targetNode, TypeNumber eType, SimpleSet *sourceNodeSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Returns the target node to a given edge and source *
 *   node.                                              *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   There must be one outgoing edge of type "eType"    *
 *   at the node "sourceNode".                          *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.NodeNotFound                                 *
 *   Graph.Unknown                                      *
 *   Graph.NotOwner                                     *
 *   The graph is not open                              *
 *   More than one target node                          *
 *                                                      *
 ********************************************************/

void
AGShowTargetNode(GraphNumber graphNumber, NodeNumber sourceNode, TypeNumber eType, NodeNumber *targetNode);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Returns all numbers of target nodes to a given     *
 *   pair (source node,edge).                           *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   Use "AGSetRemoveAnyElement" to access all nodes    *
 *   stored within the temporarily existing "SimpleSet" *
 *   and use "AGSetKill" afterwards to release all      *
 *   storage occupied by this set.                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.NodeNotFound                                 *
 *   Graph.Unknown                                      *
 *   Graph.NotOwner                                     *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGShowAllTargetNodes(GraphNumber graphNumber, NodeNumber sourceNode, TypeNumber eType, SimpleSet *targetNodeSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   "targetNrs" specifies, how many edges of type      *
 *   "eType" go out of "sourceNode". If this count      *
 *   is exactly 1, the number of the target node is     *
 *   returned.                                          *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.NodeNotFound                                 *
 *   Graph.Unknown                                      *
 *   Graph.NotOwner                                     *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGTestAndShowTargetNode(GraphNumber graphNumber, NodeNumber sourceNode, TypeNumber eType, CARDINAL *targetNrs, NodeNumber *targetNode);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Tests, whether there is at least one edge of type  *
 *   "eType" coming into "targetNode".                  *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.NodeNotFound                                 *
 *   Graph.NotOwner                                     *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

BOOLEAN
AGTestIncomingEdge(GraphNumber graphNumber, NodeNumber targetNode, TypeNumber eType);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Tests, whether there is at least one edge of type  *
 *   "eType" going out of "sourceNode".                 *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.NodeNotFound                                 *
 *   Graph.NotOwner                                     *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

BOOLEAN
AGTestOutgoingEdge(GraphNumber graphNumber, NodeNumber sourceNode, TypeNumber eType);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   This PROCEDURE calculates a set of tuples of the   *
 *   form (eType,sourceNode). Each element of this      *
 *   set fulfills the condition, that the edge of type  *
 *   edgeType runs from sourceNode to the specified     *
 *   targetNode.                                        *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   Use "AGRelRemoveAnyTuple" to access all tuples     *
 *   stored within the temporarily existing "RelSet"    *
 *   and use "AGRelKill" afterwards to release all      *
 *   storage occupied by this relation.                 *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.NodeNotFound                                 *
 *   Graph.NotOwner                                     *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGGetInContextOfNode(GraphNumber graphNumber, NodeNumber targetNode, RelSet *tupleSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   This PROCEDURE calculates a set of tuples of the   *
 *   form (edgeType,targetNode). Each element of this   *
 *   set fulfills the condition, that the edge of type  *
 *   edgeType runs from the specified sourceNode to     *
 *   targetNode.                                        *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   Use "AGRelRemoveAnyTuple" to access all tuples     *
 *   stored within the temporarily existing "RelSet"    *
 *   and use "AGRelKill" afterwards to release all      *
 *    storage occupied by this relation.                *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.NodeNotFound                                 *
 *   Graph.NotOwner                                     *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGGetOutContextOfNode(GraphNumber graphNumber, NodeNumber sourceNode, RelSet *tupleSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Replaces a substring of the attNo'th (intrin-      *
 *   sic) attribute of a node by a new value. The sub-  *
 *   string begins at position "attBegin" and has a     *
 *   length of "attLength" characters. The truncate     *
 *   argument specifies whether the attribute length    *
 *   will be set to attBegin+attLength-1.               *
 *                                                      *
 * Remarks:                                             *
 *                                                      *
 * - A typical call of AGPutAttributeSubstr might look  *
 *   like "AGPutAttributeSubstr(g,n,0,0,SIZE(I),I,      *
 *   FALSE)", making use of SYSTEM's PROCEDURE SIZE.    *
 * - Writing a range starting at a position > 0 has the *
 *   effect that reading below this position will not   *
 *   result in error messages or strings of length 0!   *
 *   The contents however may be undefined!             *
 * - The call "AGPutAttributeSubstr(g,n,X,0,0,I,TRUE)"  *
 *   will release the internal storage of the X'th      *
 *   attribute by setting its physical length to zero.  *
 *   It's easier to use AGDeleteAttribute(g,n,X).       *
 * - index and key attributes must have a length up to  *
 *   250 bytes, normal attributes must be shorter       *
 *   than 16449286 bytes (see constants MaxNormalAttri- *
 *   buteLength and MaxIndexAttributeLength in module   *
 *   AGGlobal).                                         *
 * - The first about 250 bytes of the 0th normal attri- *
 *   bute should be preferred by any application because*
 *   GRAS' internal storage mechanisms handle them in a *
 *   more efficient way.                                *
 *   If efficient storage of attribute values is essen- *
 *   tial then please store all short attribute values  *
 *   in UNDECLARED 0 attributes (the first attribute    *
 *   value at position 0, the second at position 0 +    *
 *   size of first attribute value etc.). But not that  *
 *   maintaining indexes or incremental attribute eva-  *
 *   luation doesn't work for these effficiently stored *
 *   attributes.                                        *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.LogError                                     *
 *   Graph.NodeNotFound                                 *
 *   Graph.NotOwner                                     *
 *   Graph.Unknown                                      *
 *   Graph.WrongType                                    *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGPutAttributeSubstr(GraphNumber graphNumber, NodeNumber node, TypeNumber attNo, CARDINAL attBegin, CARDINAL attLength, TEXT attValue, BOOLEAN truncate);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Deletes the attribute attNo from the node.         *
 *                                                      *
 * Remarks:                                             *
 *                                                      *
 *   A call to AGDeleteAttribute is basically a call    *
 *   AGPutAttributeSubstr(graphNumber,node,attNo,0,0,i, *
 *   TRUE)                                              *
 *   (with a dummy parameter i - see AGPutAttribute-    *
 *   Substr).                                           *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.WrongType                                    *
 *   Graph.Unknown                                      *
 *   Graph.NotOwner                                     *
 *   Graph.NodeNotFound                                 *
 *   Graph.LogError                                     *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGDeleteAttribute(GraphNumber graphNumber, NodeNumber node, AttributeNumber attNo);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   This PROCEDURE performs the inverse action of      *
 *   AGPutAttributeSubstr. The value of the attribute   *
 *   substring is returned in the "generic" parameter   *
 *   "attValue". ReturnedLength returns the number of   *
 *   bytes that could actually be read. If the value of *
 *   the attribute is undefined (due to the fact that   *
 *   the attribute didn't receive an initial value or   *
 *   previous assignments - to a derived attribute -    *
 *   became invalid due to certain graph changes), then *
 *   ReturnedLength will be 0!                          *
 *   In the case of an invalid derived attribute, the   *
 *   application may reevaluate this attribute and      *
 *   assign a new value (using 'AGPutAttributeS.')      *
 *   It is, however, possible to let GRAS do this       *
 *   automatically. In order to do this, information    *
 *   about the function name must be stored in the graph*
 *   scheme using "AGSetEvaluationFunction".            *
 *   Additionally, the application must specify the     *
 *   current address of the function code at runtime    *
 *   using "AGBindEvaluationFunction".                  *
 *   In this case any attempt to read an invalid attri- *
 *   bute will cause GRAS to reevaluate it and yield    *
 *   the new value.                                     *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.LogError                                     *
 *   Graph.NodeNotFound                                 *
 *   Graph.NotOwner                                     *
 *   Graph.Unknown                                      *
 *   Graph.WrongType                                    *
 *   Graph.CyclicEvaluation                             *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGGetAttributeSubstr(GraphNumber graphNumber, NodeNumber node, TypeNumber attNo, CARDINAL attBegin, CARDINAL attLength, TEXT attValue, CARDINAL *returnedLength);
/************************************************************************
 *                                                                      *
 *                    Operations for Event Handling                     *
 *                                                                      *
 ************************************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *  Activates a daemon. If "Event" occurs in the graph  *
 *  for which "Action" was defined, "Action" will be	 *
 *  called with parameters as listed above. For every   *
 *  tuple (Action,Event) there will be max. one daemon  *
 *  started.					         *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   GraphList.EntryNotInList                           *
 *   EventType.Mismatch                                 *
 *   EventType.Unknown                                  *
 *   EventTypes.Unknown                                 *
 *                                                      *
 ********************************************************/

void
AGActivateDaemon(GraphNumber Graph, GraphEvent Event, ActionPriority Priority, ActionProc Action);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Deactivates a daemon that has be activated by a    *
 *   call of AGActivateDaemon. If a graph is closed, all*
 *   its active daemons will be killed, so the next time*
 *   it is opened, there will be no initially active    *
 *   daemons.                                           *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   The execution of activated actions may not be      *
 *   locked for the specified graph!                    *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   DaemonList.EntryNotInList                          *
 *                                                      *
 ********************************************************/

void
AGKillDaemon(GraphNumber Graph, GraphEvent Event, ActionProc Action);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 * All daemons, that wait for UserDefined events	 *
 * (with matching parameters) will invoke their 	 *
 * actions. This can be used to communicate with	 *
 * other clients. But be careful: the meaning of	 *
 * p1 and p2 is totally undefined from GRAS and can	 *
 * be different from client to client.			 *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   RaiseEvent can also be called from a client        *
 *   different from the owner of the graph who may have *
 *   defined the EventPattern and activated the daemon, *
 *   provided that he has the right to work on the      *
 *   graph under consideration.                         *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   GraphList.EntryNotInList                           *
 *   EventTypes.Unknown                                 *
 *   EventType.Unknown                                  *
 *   EventType.Mismatch                                 *
 *                                                      *
 ********************************************************/

void
AGRaiseEvent(GraphNumber Graph, CARDINAL p1, CARDINAL p2, CARDINAL p3);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Locks execution of all actions that were activated *
 *   for the pool. Locking may be nested, which means   *
 *   that a certain number of AGDelayActionExecution    *
 *   calls must be followed be the same number of calls *
 *   to AGReleaseActionExecution to release the lock.   *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The pool is not open                               *
 *                                                      *
 ********************************************************/

void
AGDelayActionExecution(PoolName poolName);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Unlocks execution of activated actions for a pool. *
 *   Locked active actions are executed. Locking may be *
 *   nested, which means that a certain number of       *
 *   AGDelayActionExecution calls must be followed be   *
 *   the same number of calls to AGReleaseAction-       *
 *   Execution to release the lock.                     *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The pool is not open                               *
 *                                                      *
 ********************************************************/

void
AGReleaseActionExecution(PoolName poolName);
/************************************************************************
 *                                                                      *
 *                      Operations on Local Sets                        *
 *                                                                      *
 * Note:                                                                *
 *                                                                      *
 *   Aborting transactions does not effect local sets! They are not     *
 *   reset into a previous state.                                       *
 *                                                                      *
 ************************************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Creates an empty temporary set.                    *
 *   A set is a structure with                          *
 *   the properties of mathematical sets. It can store  *
 *   elements which are present at most once. Several   *
 *   operations are presented for these sets. SimpleSet *
 *   structures contain NodeNumbers      .              *
 *                                                      *
 ********************************************************/

void
AGSetCreate(GraphNumber graphNumber, SimpleSet *emptySet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Clears an temporary set.                           *
 *   A set is a structure with the properties           *
 *   of mathematical sets. It can store elements which  *
 *   are present at most once. Several operations are   *
 *   presented for these sets. SimpleSet structures con-*
 *   tain NodeNumbers.                                  *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

void
AGSetClear(GraphNumber graphNumber, SimpleSet *clearSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Releases the amount of memory used by "set". It    *
 *   may then be reused by AGSetCreate.                 *
 *                                                      *
 ********************************************************/

void
AGSetKill(GraphNumber graphNumber, SimpleSet *set);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Copies "sourceSet" to "targetSet".                 *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The sourceset has not been created                 *
 *                                                      *
 ********************************************************/

void
AGSetCopy(GraphNumber graphNumber, SimpleSet sourceSet, SimpleSet *targetSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Inserts an element into a set.                     *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

void
AGSetInsertElement(GraphNumber graphNumber, SimpleElement element, SimpleSet set);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Deletes an element from a set.                     *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

void
AGSetDeleteElement(GraphNumber graphNumber, SimpleElement element, SimpleSet set);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Deletes an arbitrary element from the set and      *
 *   returns it to the calling module. If the set is    *
 *   empty, the result of "found" is FALSE.             *
 *   "Arbitrary" means that the resulting sequence of   *
 *   returned elements is the most efficient one with   *
 *   respect to the system's internal storage layout.   *
 *   Use "AGSetRemoveRndElement" if real random results *
 *   are required.                                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

void
AGSetRemoveAnyElement(GraphNumber graphNumber, BOOLEAN *found, SimpleElement *element, SimpleSet set);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Deletes a real random element from the set and     *
 *   returns it to the calling module. If the set is    *
 *   empty, the result of "found" is FALSE.             *
 *   This PROCEDURE is considerably less efficient than *
 *   "AGSetRemoveAnyElement" and should only be used in *
 *   cases where "random" access is really important.   *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

void
AGSetRemoveRndElement(GraphNumber graphNumber, BOOLEAN *found, SimpleElement *element, SimpleSet set);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Returns whether "element" is in "set".             *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

BOOLEAN
AGSetIsElement(GraphNumber graphNumber, SimpleElement element, SimpleSet set);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Computes the cardinal number of "set".             *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

CARDINAL
AGSetSize(GraphNumber graphNumber, SimpleSet set);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Computes the union of targetSet and sourceSet and  *
 *   places the result in targetSet.                    *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The target or sourceset has not been created       *
 *                                                      *
 ********************************************************/

void
AGSetUnion(GraphNumber graphNumber, SimpleSet sourceSet, SimpleSet targetSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Computes targetSet := targetSet intersected with   *
 *                         sourceSet                    *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The target or sourceset has not been created       *
 *                                                      *
 ********************************************************/

void
AGSetIntersection(GraphNumber graphNumber, SimpleSet sourceSet, SimpleSet targetSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Computes targetSet := targetSet without sourceSet. *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The target or sourceset has not been created       *
 *                                                      *
 ********************************************************/

void
AGSetDifference(GraphNumber graphNumber, SimpleSet sourceSet, SimpleSet targetSet);
/************************************************************************
 *                                                                      *
 *                   Operations on Local Relations                      *
 *                                                                      *
 * Note:                                                                *
 *                                                                      *
 *   Aborting transactions does not effect local relations! They are    *
 *   not reset into a previous state.                                   *
 *                                                                      *
 ************************************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Creates an empty temporary tuple set               *
 *   A relation set is a                                *
 *   structure with the properties of mathematical sets.*
 *   It can store elements which are present at most    *
 *   once. Several operations are presented for these   *
 *   sets. RelSet structures contain elements which are *
 *   each a pair of NodeNumbers.                        *
 *                                                      *
 ********************************************************/

void
AGRelCreate(GraphNumber graphNumber, RelSet *emptySet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Clears an temporary tuple set.                     *
 *   A relation set is a                                *
 *   structure with the properties of mathematical sets.*
 *   It can store elements which are present at most    *
 *   once. Several operations are presented for these   *
 *   sets. RelSet structures contain elements which are *
 *   each a pair of NodeNumbers.     .                  *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

void
AGRelClear(GraphNumber graphNumber, RelSet *clearSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Releases the amount of memory used by "set". It    *
 *   may then be re-used be AGRelCreate.                *
 *                                                      *
 ********************************************************/

void
AGRelKill(GraphNumber graphNumber, RelSet *set);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Copies "sourceSet" to "targetSet".                 *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The source set has not been created                *
 *                                                      *
 ********************************************************/

void
AGRelCopy(GraphNumber graphNumber, RelSet sourceSet, RelSet *targetSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Inserts the Tuple ("surr1","surr2") into a set.    *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

void
AGRelInsertTuple(GraphNumber graphNumber, TypeNumber surr1, TypeNumber surr2, RelSet set);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Deletes the tuple ("surr1","surr2") from the set.  *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

void
AGRelDeleteTuple(GraphNumber graphNumber, TypeNumber surr1, TypeNumber surr2, RelSet set);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Deletes an arbitrary tuple from the set and        *
 *   returns its components "surr1" and "surr2" to the  *
 *   calling module. If the set is empty, the result of *
 *   "found" is FALSE.                                  *
 *   "Arbitrary" means that the resulting sequence of   *
 *   returned elements is the most efficient one with   *
 *   respect to the system's internal storage layout.   *
 *   Use "AGRelRemoveRndElement" if real random results *
 *   are required.                                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

void
AGRelRemoveAnyTuple(GraphNumber graphNumber, BOOLEAN *found, TypeNumber *surr1, TypeNumber *surr2, RelSet set);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Deletes an arbitrary tuple from the set and        *
 *   returns its components "surr1" and "surr2" to the  *
 *   calling module. If the set is empty, the result of *
 *   "found" is FALSE.                                  *
 *   This PROCEDURE is considerably less efficient than *
 *   "AGRelRemoveAnyElement" and should only be used in *
 *   cases where "random" access is really important.   *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

void
AGRelRemoveRndTuple(GraphNumber graphNumber, BOOLEAN *found, TypeNumber *surr1, TypeNumber *surr2, RelSet set);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   The result of this function is true, if the pair   *
 *   ("surr1","surr2") is in "set".                     *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

BOOLEAN
AGRelIsElement(GraphNumber graphNumber, TypeNumber surr1, TypeNumber surr2, RelSet set);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Computes the cardinal number of "set".             *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

CARDINAL
AGRelSize(GraphNumber graphNumber, RelSet set);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Computes the union of targetSet and sourceSet and  *
 *   places the result in targetSet.                    *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The source or targetset has not been created       *
 *                                                      *
 ********************************************************/

void
AGRelUnion(GraphNumber graphNumber, RelSet sourceSet, RelSet targetSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Computes targetSet := targetSet intersected with   *
 *                         sourceSet                    *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The source or targetset has not been created       *
 *                                                      *
 ********************************************************/

void
AGRelIntersection(GraphNumber graphNumber, RelSet sourceSet, RelSet targetSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Computes targetSet := targetSet without sourceSet. *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The source or targetset has not been created       *
 *                                                      *
 ********************************************************/

void
AGRelDifference(GraphNumber graphNumber, RelSet sourceSet, RelSet targetSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Computes the cartesian product of the two source   *
 *   sets and places the result in the tuple set        *
 *   "targetSet".                                       *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The source or targetset has not been created       *
 *                                                      *
 ********************************************************/

void
AGCartesian(GraphNumber graphNumber, SimpleSet sourceSet1, SimpleSet sourceSet2, RelSet *targetSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Projection of the first component of "sourceSet"   *
 *   into "targetSet".                                  *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The source set has not been created                *
 *                                                      *
 ********************************************************/

void
AGProjectionFirst(GraphNumber graphNumber, RelSet sourceSet, SimpleSet *targetSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Projection of the second component of "sourceSet"  *
 *   into "targetSet", which must have been created     *
 *   before.                                            *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The source set has not been created                *
 *                                                      *
 ********************************************************/

void
AGProjectionSecond(GraphNumber graphNumber, RelSet sourceSet, SimpleSet *targetSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Projection of the second component of "sourceSet"  *
 *   into "targetSet", if the first component matches   *
 *   "surr". "targetSet" must have been created before. *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The source set has not been created                *
 *                                                      *
 ********************************************************/

void
AGQueryProjection(GraphNumber graphNumber, RelSet sourceSet, TypeNumber surr, SimpleSet *targetSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   AGSingleQueryProjection calculates *one* second    *
 *   component surr2 for a given first component surr1. *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

void
AGSingleQueryProjection(GraphNumber graphNumber, RelSet set, TypeNumber surr1, TypeNumber *surr2, BOOLEAN *found);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Exchanges the components of a local tuple set.     *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The set has not been created                       *
 *                                                      *
 ********************************************************/

void
AGChange(GraphNumber graphNumber, RelSet *set);
/************************************************************************
 *                                                                      *
 *                    PROCEDUREs to work with Logs                      *
 *                                                                      *
 ************************************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Sets a checkpoint.                                 *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   ChgMgmtGraph.InternalError                         *
 *   ChgMgmtGraph.LogError                              *
 *   ChgMgmtGraph.NoLog                                 *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGSetCheckpoint(GraphNumber graphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   AGUndo performs an UNDO back for n check points.   *
 *   If there are less than n check points set, the     *
 *   number of successful UNDOs is returned in n. This  *
 *   PROCEDURE should only be called, if there were no  *
 *   graph changing operations since the last setting   *
 *   of a check point. Otherwise, a new checkpoint is   *
 *   created internally before executing AGUndo itself. *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   ChgMgmtGraph.InternalError                         *
 *   ChgMgmtGraph.LogError                              *
 *   ChgMgmtGraph.NoLog                                 *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGUndo(GraphNumber graphNumber, CARDINAL *n);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   AGRedo performs a REDO of all graph changing       *
 *   operations that were performed since n check       *
 *   points before. This PROCEDURE may only be called,  *
 *   if there were no graph changing operations since   *
 *   the last setting of a check point. The number of   *
 *   successful REDOs is returned as parameter n.       *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   ChgMgmtGraph.InternalError                         *
 *   ChgMgmtGraph.LogError                              *
 *   ChgMgmtGraph.NoLog                                 *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGRedo(GraphNumber graphNumber, CARDINAL *n);
/************************************************************************
 *                                                                      *
 *                           Scheme Management                          *
 *                                                                      *
 *  N O T E :  The following PROCEDUREs are not necessary for the mani- *
 *             pulation of attributed graphs (if the error check mode   *
 *             is deactivated and incremental attribute evaluation as   *
 *             well as index or key attributes are not used).           *
 *                                                                      *
 ************************************************************************/
/************************************************************************
 *                                                                      *
 *         Operations for managing schemes                              *
 *                                                                      *
 * Every component of the graph scheme has two identifiers. One is a    *
 * (human-readable) name, the other one an arbitrary internally used    *
 * number which must be unique across all types of declared graph       *
 * compontents.                                                         *
 *                                                                      *
 * ATTENTION: Even components belonging to different categories of data *
 *            - like node classes and edge types - must have different  *
 *            names and internally used type numbers.                   *
 *                                                                      *
 * This part of the GRAS interface is not yet complete. Explanations of *
 * error messages must be added and a number of PROCEDUREs for querying *
 * an already existing graph are missing.                               *
 *                                                                      *
 ************************************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Creation of a scheme with the name "Name" that     *
 *   will get the number "Scheme". This number can be   *
 *   used to reference it. It is possible to have more  *
 *   than one (sub-)scheme in one graph.                *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   AGDeclareScheme may be used to extend already      *
 *   existing graph schemes and to redefine the         *
 *   evaluation functions and attribute dependencies    *
 *   of previously declared attributes, i.e. AGDeclare- *
 *   Scheme for an already existing graph scheme starts *
 *   the extension of this graph scheme.                *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Denied                                      *
 *   Access.Locked                                      *
 *   Scheme.Existent                                    *
 *   Scheme.InUse                                       *
 *   Scheme.InternalError                               *
 *   GraphPool.InternalError                            *
 *   Scheme.NoValidScheme                               *
 *   Scheme.NotExistent                                 *
 *   GraphPool.CardinalityError                         *
 *   GraphPool.NotInTransaction                         *
 *   The pool is not open                               *
 *   The scheme is already open                         *
 *                                                      *
 ********************************************************/

void
AGDeclareScheme(SchemeName schemeName, SchemeNumber *schemeNumber, PoolName poolName);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Commit all declarations that were made for this    *
 *   scheme. This PROCEDURE is used to tell the GRAS    *
 *   system that there are no more declarations. There- *
 *   fore it must be called AFTER all other scheme      *
 *   PROCEDUREs. All declarations are then carried out  *
 *   in the proper order. That means, that you can call *
 *   all declarations in arbitrary order and then simply*
 *   commit the scheme.                                 *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   AGCommitScheme may be used to extend already       *
 *   existing graph schemes and to redefine the         *
 *   evaluation functions and attribute dependencies    *
 *   of previously declared attributes.                 *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGCommitScheme(SchemeNumber schemeNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Look for the scheme "schemeName".                  *
 *   If it is existent, TRUE is returned .              *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   GraphPool.InternalError                            *
 *   Access.Locked                                      *
 *   The pool is not open                               *
 *                                                      *
 ********************************************************/

BOOLEAN
AGExistsScheme(PoolName poolName, SchemeName schemeName);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get the name of the scheme "schemeNumber"          *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGShowSchemeName(SchemeNumber schemeNumber, SchemeName name);

 
 /********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Set the version of the scheme "Scheme".            *
 *                                                      *
 ********************************************************/

 /********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get the version of the scheme "Scheme".            *
 *                                                      *
 ********************************************************/
/*****************************************************************************
 *                                                                           *
 *  PROCEDUREs for managing node classes                                     *
 *                                                                           *
 *****************************************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Declare the node class "name" for the scheme       *
 *   "schemeNumber". This node class will get the number*
 *   "nodeClass". The name and the number of the node   *
 *   class must be unique for this graph.               *
 *   Node class no. 0 is reserved.                      *
 *                                                      *
 * Note:                                                *
 *   This action will not take place before the scheme  *
 *   is committed.                                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.AlreadyDeclared                             *
 *   Scheme.InternalError                               *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGDeclareNodeClass(SchemeNumber schemeNumber, TEXT name, TypeNumber *nodeClass);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Look for the node class "className".               *
 *   If it is existent, its number is returned in       *
 *   "nodeClass".                                       *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGTestAndShowNodeClassNumber(GraphNumber graphOrSchemeNumber, TEXT className, BOOLEAN *existent, TypeNumber *nodeClass, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get the name of the node class "nodeClass". If the *
 *   node class is declared, the name is returned in    *
 *   "className".                                       *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGShowNodeClassName(GraphNumber graphOrSchemeNumber, TypeNumber nodeClass, TEXT className, BOOLEAN *existent, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *  Get all node types of a given class and all of its  *
 *  subclasses in "setOfTypes"                          *
 *                                                      *
 *   Note:                                              *
 *                                                      *
 *   Use "AGSetRemoveAnyElement" to access all types    *
 *   stored within the temporarily existing "SimpleSet" *
 *   and use "AGSetKill" afterwards to release all      *
 *    storage occupied by this set.                     *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGShowAllNodeTypes(GraphNumber graphOrSchemeNumber, TypeNumber nodeClass, SimpleSet *setOfTypes, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Add a specialization/generalization relation to    *
 *   the scheme. The node class "nodeClassName" will    *
 *   then be a subclass of "superClassName" and inherits*
  all attributes and attribute evaluation rules. *
 *                                                      *
 * Note:                                                *
 *   This action will not take place before the scheme  *
 *   is committed.                                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.NotDeclared                                 *
 *   Scheme.AttributeNameClash                          *
 *   Scheme.AttributePlacementClash                     *
 *   Scheme.Cyclic                                      *
 *   Scheme.InternalError                               *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGAppendNodeClass(SchemeNumber schemeNumber, TEXT nodeClassName, TEXT superClassName);
/*****************************************************************************
 *                                                                           *
 * PROCEDUREs for managing node types                                        *
 *                                                                           *
 *****************************************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Declare node type "name" for scheme "schemeNumber".*
 *   This node type will get  number "nType".           *
 *   The name and the number of the node type must be   *
 *   unique for this graph. Node type no. 0 is reserved.*
 *   If the "UseMembership" flag is set, GRAS will be   *
 *   able to find all nodes of that type using the      *
 *   PROCEDURE "AGGetAllNodesOfNodeType".               *
 *   The "UseMembership" flag may not be used if more   *
 *   than 5000 nodes of this type will be created.      *
 *                                                      *
 * Notes:                                               *
 *   1) This action will not take place before the      *
 *      scheme is committed.                            *
 *   2) Creating and deleting nodes of this type        *
 *      will be more efficient if the                   *
 *      "UseMembership" option is NOT set.              *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.AlreadyDeclared                             *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGDeclareNodeType(SchemeNumber schemeNumber, TEXT name, TypeNumber *nType);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Look for the node type "name". If it is existent,  *
 *   its number is returned in "nType".                 *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGTestAndShowNodeTypeNumber(CARDINAL graphOrSchemeNumber, TEXT name, BOOLEAN *existent, TypeNumber *nType, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get the name of the node type "nType". If the      *
 *   node type was declared, the name is returned in    *
 *   "name".                                            *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGShowNodeTypeName(CARDINAL graphOrSchemeNumber, TypeNumber nType, TEXT name, BOOLEAN *existent, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Add a specialization/generalization relation to    *
 *   the scheme. The node type "nType" will then        *
 *   belong to the class "nodeClass" and inherits all   *
 *   attributes and attribute evaluation rules.         *
 *                                                      *
 * Note:                                                *
 *   This action will not take place before the scheme  *
 *   is committed.                                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   Scheme.AlreadyDeclared                             *
 *   Scheme.AttributePlacementClash                     *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGAppendNodeType(CARDINAL graphOrSchemeNumber, TEXT nType, TEXT nodeClass);
/*****************************************************************************
 *                                                                           *
 * PROCEDUREs for managing edge types                                        *
 *                                                                           *
 *****************************************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Declare the edge type "name" for                   *
 *   the scheme "schemeNumber". The edge type will get  *
 *   the number "edge". Edge type no. 0 is reserved.    *
 *   All nodes that belong to "sourceNodeClassOrType"   *
 *   are allowed as source nodes for edges of this type.*
 *   All nodes that belong to "targetNodeClassOrType"   *
 *   are allowed as target nodes for edges of this type.*
 *   "sourceCardinality" and "targetCardinality" are    *
 *   used for runtime checks. "targetCardinality" =     *
 *   "ObligateSet" means e.g. that any node of class    *
 *   "sourceNodeClassOrType" is expected to have at     *
 *   least one emanating edge of this type.             *
 *                                                      *
 * Note:                                                *
 *   This action will not take place before the scheme  *
 *   is committed.                                      *
 *                                                      *
 * ATTENTION:                                           *
 *   Up to now, cardinality checks in "AGCreateEdge"    *
 *   etc. are not yet implemented.                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   Scheme.AlreadyDeclared                             *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGDeclareEdgeType(SchemeNumber schemeNumber, TEXT name, TEXT sourceNodeClassOrType, Cardinality sourceCardinality, TEXT targetNodeClassOrType, Cardinality targetCardinality, TypeNumber *edge);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Look for the edge type "name". If it is existent,  *
 *   its number is returned in "edge".                  *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGTestAndShowEdgeTypeNumber(CARDINAL graphOrSchemeNumber, TEXT name, BOOLEAN *existent, TypeNumber *edge, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get the name of the edge type "edge". If the       *
 *   edge type was declared, the name is returned in    *
 *   "name".                                            *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGShowEdgeTypeName(CARDINAL graphOrSchemeNumber, TypeNumber edge, TEXT name, BOOLEAN *existent, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get the properties of the edge type "edge". If the *
 *   edge type was declared, its cardinalities and its  *
 *   source and target types/classes are returned.      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGShowEdgeTypeProps(CARDINAL graphOrSchemeNumber, TypeNumber edge, TypeNumber *sourceCT, Cardinality *sourceCardinality, TypeNumber *targetCT, Cardinality *targetCardinality, BOOLEAN *existent, BOOLEAN isGraphNumber);
/*****************************************************************************
 *                                                                           *
 * PROCEDUREs for managing attributes                                        *
 *                                                                           *
 *****************************************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Declare the Attribute "name" for the scheme        *
 *   "schemeNumber". All nodes that belong to the       *
 *   class/type "classOrType" will carry that attribute.*
 *   The attribute will get the number "attNo". The     *
 *   name and the number of the attribute must be       *
 *   unique.                                            *
 *                                                      *
 *   "kind" specifies whether the attribute is derived  *
 *   (with its subcase dummy), i.e. depends on other    *
 *   attributes, or whether it is an intrinsic attribute*
 *   (with its subcase meta attribute which has the same*
 *   value for all nodes of a certain type, i.e. is an  *
 *   attribute of the node type itself).                *
 *                                                      *
 *   If constant length flag is set, "length" specifies *
 *   the size of the values that will be stored in      *
 *   that attribute (in Bytes).                         *
 *                                                      *
 *   "props" contains information about the properties  *
 *   of the attribute, i.e. whether it is a normal, an  *
 *   index or a key attribute.                          *
 *                                                      *
 *   "valueType" is an almost uninterpreted number which*
 *   gives applications the chance to store information *
 *   about the type of an attribute's value (which is   *
 *   normally considered to be an uninterpreted byte    *
 *   stream. The numbers for standard types like integer*
 *   boolean etc. are fixed! All remaining numbers may  *
 *   be used as "pointers" to application defined types.*
 *                                                      *
 *   "valueCardinality" specifies whether a value for   *
 *   this attribute is optional ( -> "Opt..." ) or      *
 *   obligate ( -> "Obl..." ) and whether the attribute *
 *   contains a set of values ( -> "...Set" ) or only   *
 *   a single element ( -> "...Unique" )                *
 *   Note that "ValueCardinality" is an attribute prop. *
 *   which has no meaning for GRAS (like "ValueType")   *
 *   because (up to now) all (sets of) attribute values *
 *   are uninterpreted arrays of byte for GRAS.         *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   1) The attribute number 0 should be used for the   *
 *      most frequently used NON-INDEX (and NON-KEY)    *
 *      attribute. If time and space consumption are    *
 *      very important, then you might even store many  *
 *      short attribute values within the 0 attribute   *
 *      (or within another attribute) without declaring *
 *  	   this attribute. This trick deactivates a number *
 *      of checks (for keeping derived attributes and   *
 *      attribute indexes consistent).                  *
 *   2) Meta and dummy index or key attributes are not  *
 *      supported.                                      *
 *   3) Dummy attributes may be used for propagating    *
 *      graph changes along arbitrarily long paths in   *
 *      a graph (by translating an n-context dependency *
 *      between two attributes into n 1-context depen-  *
 *      dencies between dummy attributes).              *
 *   4) Dummy attributes only are allowed to have a     *
 *      constant length of size 0.                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.AlreadyDeclared                             *
 *   Scheme.AttributeNameClash                          *
 *   Scheme.AttributePlacementClash                     *
 *   Scheme.NotDeclared                                 *
 *   Scheme.InternalError                               *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGDeclareAttribute(SchemeNumber schemeNumber, TEXT classOrType, TEXT name, AttributeKind kind, IndexProperties props, ValueTypeNumber valueType, Cardinality valueCardinality, BOOLEAN constantLength, CARDINAL length, AttributeNumber *attNo);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Look for the attribute "name". If it is existent,  *
 *   its number is returned in "attNo".                 *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGTestAndShowAttributeNumber(GraphNumber graphOrSchemeNumber, TEXT name, TypeNumber classOrType, BOOLEAN *existent, AttributeNumber *attNo, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get the name of the attribute "attNo". If the      *
 *   attribute was declared, the name is returned in    *
 *   "name".                                            *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGShowAttributeName(GraphNumber graphOrSchemeNumber, AttributeNumber attNo, TEXT name, BOOLEAN *existent, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get the properties of the attribute "attNo".       *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGShowAttributeProps(GraphNumber graphOrSchemeNumber, AttributeNumber attNo, AttributeKind *kind, IndexProperties *props, ValueTypeNumber *valueType, Cardinality *attCard, BOOLEAN *constLength, CARDINAL *length, BOOLEAN *existent, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Basically works like 'AGPutAttributeSubstr' but is *
 *   used to define the "meta" attribute value of       *
 *   'attNo' at a node type object (and not at a node). *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGPutMetaAttribute(GraphNumber schemeNumber, TypeNumber nType, AttributeNumber attNo, CARDINAL attBegin, CARDINAL attLength, TEXT attValue, BOOLEAN truncate);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Basically works like 'AGGetAttributeSubstr' but is *
 *   used to access the "meta" attribute value of       *
 *   'attNo' at a node type object (and not at a node)  *
 *   and is therefore the inverse function to "AGPut-   *
 *   MetaAttribute".                                    *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGGetMetaAttribute(GraphNumber graphOrSchemeNumber, TypeNumber nType, AttributeNumber attNo, CARDINAL attBegin, CARDINAL attLength, TEXT attValue, CARDINAL *returnedLength, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Set the name of the evaluation function for the    *
 *   attribute "attribute". This name will be           *
 *   valid for all nodes that belong to the class/type  *
 *   "nodeClassOrType" with the exception of those      *
 *   attributes which belong to a subclass (type) with  *
 *   a redefinition of the evaluation function.         *
 *   To get the valid evaluation function for a specific*
 *   class or type use "AGShowEvaluationFunction".      *
 *   The purpose of an evaluation function depends on   *
 *   the attribute's kind:                              *
 *      derived:   Recompute an attribute's value.      *
 *      intrinsic: Initialize new attribute instances   *
 *                 (not for intrinsic KEY attribute).   *
 *      meta:      Compute constant value for all nodes *
 *                 of given node type (i.e. the 'Name'  *
 *                 function has a node type instead of  *
 *                 a node as one of its parameters).    *
 *      dummy:     Not useful!                          *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   1) This action will not take place before the      *
 *      scheme is committed.                            *
 *   2) The name of the evaluation function may be      *
 *      redefined in subclasses/subtypes.               *
 *   3) You have to use "AGBindEvaluationFunction" to   *
 *      provide GRAS at runtime with the code addresses *
 *      of these functions.                             *
 *                                                      *
 * Feature:                                             *
 *                                                      *
 *   The main purpose of an evaluation function is to   *
 *   compute a new value for an invalid attribute and   *
 *   to store this value within the proper attribute    *
 *   itself by using 'AGPutAttributeSubstr'.            *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGSetEvaluationFunction(SchemeNumber schemeNumber, TEXT nodeClassOrType, TEXT attribute, TEXT name);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get the name of the evaluation function for the    *
 *   attribute "attNo" and the node class/type          *
 *   "nodeClassOrType". If the evaluation               *
 *   function was set, the name is returned in          *
 *   "name".                                            *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGShowEvaluationFunction(GraphNumber graphOrSchemeNumber, AttributeNumber attNo, TypeNumber nodeClassOrType, TEXT name, BOOLEAN *existent, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Specify the current address of the evaluation      *
 *   function "name" at runtime.                        *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   This function must be used after each "AGLogin" to *
 *   provide GRAS at runtime with the code addresses    *
 *   of the evaluation functions.                       *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Graph.AlreadyBound                                 *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGBindEvaluationFunction(GraphNumber graphNumber, TEXT name, EvalFunction function);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Statically declare a dependency of the attribute   *
 *   "dependent" from the attribute "dependsOn". This   *
 *   dependency will be valid for all nodes that belong *
 *   to the class/type "nodeClassOrType".               *
 *   "kindOfDependency" specifies the kind of dependency*
 *   of the attribute (see AGGlobal for details).       *
 *   GRAS will delete all attributes that are affected  *
 *   by attribute writing or structural changes of the  *
 *   graph. This values must be reevaluated later on.   *
 *                                                      *
 * Note:                                                *
 *   This action will not take place before the scheme  *
 *   is committed.                                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGDeclareDependency(SchemeNumber schemeNumber, TEXT nodeClassOrType, TEXT dependent, TEXT dependsOn, TEXT nameOfEdge, DependencyKind kindOfDependency);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Delete all dependencies of the attribute           *
 *   "dependent" for the node class/type                *
 *   "nodeClassOrType". This will also delete           *
 *   any evaluation functions that were declared for    *
 *   this attribute/class combination.                  *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGDeleteDependencies(GraphNumber graphOrSchemeNumber, TEXT nodeClassOrType, TEXT dependent, BOOLEAN restoreInheritance, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get all attributes that were declared for the node *
 *   type "nType".                                      *
 *                                                      *
 *   Note:                                              *
 *                                                      *
 *   Use "AGSetRemoveAnyElement" to access all attnos.  *
 *   stored within the temporarily existing "SimpleSet" *
 *   and use "AGSetKill" afterwards to release all      *
 *   storage occupied by this set.                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGShowAllAttributesOfNodeType(GraphNumber graphOrSchemeNumber, TypeNumber nType, SimpleSet *attributes, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get all nodes in the graph. All node types must use*
 *   the membership relation.                           *
 *                                                      *
 *   Note:                                              *
 *                                                      *
 *   Use "AGSetRemoveAnyElement" to access all nodes    *
 *   stored within the temporarily existing "SimpleSet" *
 *   and "AGSetKill" afterwards to release all storage  *
 *   occupied by this set.                              *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGGetAllNodesOfGraph(GraphNumber graphNumber, SimpleSet *setOfNodes);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get all declared classes in the graph.             *
 *                                                      *
 *   Note:                                              *
 *                                                      *
 *   Use "AGSetRemoveAnyElement" to access all classes  *
 *   stored within the temporarily existing "SimpleSet" *
 *   and use "AGSetKill" afterwards to release all      *
 *   storage occupied by this set.                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGGetAllNodeclasses(GraphNumber graphOrSchemeNumber, SimpleSet *setOfClasses, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get all declared edge types in the graph.          *
 *                                                      *
 *   Note:                                              *
 *                                                      *
 *   Use "AGSetRemoveAnyElement" to access all types    *
 *   stored within the temporarily existing "SimpleSet" *
 *   and use "AGSetKill" afterwards to release all      *
 *   storage occupied by this set.                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   The graph is not open                              *
 *   The scheme is not open                             *
 *                                                      *
 ********************************************************/

void
AGGetAllEdgetypes(GraphNumber graphOrSchemeNumber, SimpleSet *setOfEdgetypes, BOOLEAN isGraphNumber);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get all nodes of the class "nodeClass". The set    *
 *   must have been created. All node types must use    *
 *   the membership relation.                           *
 *                                                      *
 *   Note:                                              *
 *                                                      *
 *   Use "AGSetRemoveAnyElement" to access all nodes    *
 *   stored within the temporarily existing "SimpleSet" *
 *   and use "AGSetKill" afterwards to release all      *
 *   storage occupied by this set.                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGGetAllNodesOfNodeClass(GraphNumber graphOrSchemeNumber, TypeNumber nodeClass, SimpleSet *setOfNodes);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get all nodes of type "nType". All subtypes must   *
 *   use the membership relation.                       *
 *                                                      *
 *   Note:                                              *
 *                                                      *
 *   Use "AGSetRemoveAnyElement" to access all nodes    *
 *   stored within the temporarily existing "SimpleSet" *
 *   and use "AGSetKill" afterwards to release all      *
 *   storage occupied by this set.                      *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGGetAllNodesOfNodeType(GraphNumber graphNumber, TypeNumber nType, SimpleSet *setOfNodes);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Get all nodes of the graph that do not belong to   *
 *   "setOfNodes". All node types must use the          *
 *   membership relation.                               *
 *                                                      *
 *   Note:                                              *
 *                                                      *
 *   Use "AGSetRemoveAnyElement" to access all nodes    *
 *   stored within the temporarily existing "SimpleSet" *
 *   and use "AGSetCreate" to initialize the set before *
 *   calling this PROCEDURE and use "AGSetKill" after-  *
 *   wards to release all storage occupied by this set. *
 *                                                      *
 ********************************************************/

void
AGGetComplementOfNodeSet(GraphNumber graphNumber, SimpleSet *setOfNodes);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Self explanatory.                                  *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   Graph.InternalError                                *
 *   Graph.NodeNotFound                                 *
 *   Graph.NotOwner                                     *
 *   Graph.Unknown                                      *
 *   Scheme.InternalError                               *
 *   Scheme.NotDeclared                                 *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

BOOLEAN
AGTestIsNodeOfNodeClass(GraphNumber graphNumber, TypeNumber nodeClass, TypeNumber node);
/*****************************************************************************
 *                                                                           *
 * PROCEDUREs for setting the error check mode                               *
 *                                                                           *
 *****************************************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Switch on/off the runtime legality tests. The      *
 *   default setting is OFF.                            *
 *   This affects only expensive checks against graph   *
 *   schemes, i.e. even with error check "OFF"  many    *
 *   consistency checks are active in order to avoid    *
 *   "serious" corruptions of graphs. Set this switch   *
 *   to "ON" (TRUE) for debugging purposes and to "OFF" *
 *   (FALSE) in your production version.                *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Graph.InternalError                                *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGSetErrorCheckMode(GraphNumber graphNumber, BOOLEAN errorcheckOn);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Returns the filesize of the file 'fileName'        *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   The pool is not open                               *
 *   PageFile.NoAccess                                  *
 *   VirtualResource.FatalError                         *
 *                                                      *
 ********************************************************/
  
CARDINAL
AGFileSize(PoolName poolName, TEXT fileName);
/**********************************************************************
 *                                                                    *
 * Here starts the 'hack section'! The PROCEDUREs below are to be     *
 * handled with care because they may lead to inconsistent states     *
 * (if not carefully used) which may result in loss of data!          *
 *                                                                    *
 * The reasons for still offering these PROCEDUREs are twofold:       *
 *    (1) Compatibility with older GRAS versions                      *
 *    (2) Efficient usage of GRAS resources in the absence of a graph *
 *        scheme                                                      *
 *                                                                    *
 *                               *****                                *
 *                               *   *                                *
 *                               *   *                                *
 *                               *   *                                *
 *                               *   *                                *
 *                            ****   ****                             *
 *                             *       *                              *
 *                              *     *                               *
 *                               *   *                                *
 *                                * *                                 *
 *                                 *                                  *
 *                                                                    *
 **********************************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Without scheme                                     *
 *   --------------                                     *
 *   Assigns a new value to an index attribute and over-*
 *   rides the old value, if existent.                  *
 *                                                      *
 *   With scheme                                        *
 *   -----------                                        *
 *   Creates an index attribute for "node". If the      *
 *   scheme is used, we recommend to use AGPutAttribute-*
 *   Substr rather than this PROCEDURE!                 *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   ChgMgmtGraph.InternalError                         *
 *   PersistentGraph.NodeNotFound                       *
 *   ChgMgmtGraph.LogError                              *
 *   PersistentGraph.IndexUsed                          *
 *   PersistentGraph.NotOwner                           *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGPutIndexAttribute(GraphNumber graphNumber, NodeNumber node, AttributeNumber attNo, CARDINAL attLength, TEXT attValue);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Without scheme                                     *
 *   --------------                                     *
 *   Returns the value of the attribute "AttNo".        *
 *                                                      *
 *   With scheme                                        *
 *   -----------                                        *
 *   Returns the value of an index attribute for Node.  *
 *   If the scheme is used, we recommend to use AGGet-  *
 *   AttributeSubstr rather than this PROCEDURE!        *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   PersistentGraph.InternalError                      *
 *   PersistentGraph.NodeNotFound                       *
 *   PersistentGraph.NotOwner                           *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGGetIndexAttribute(GraphNumber graphNumber, NodeNumber node, AttributeNumber attNo, TEXT attValue, CARDINAL *attLength);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Without scheme                                     *
 *   --------------                                     *
 *   Deletes an index attribute from a node.            *
 *                                                      *
 *   With scheme                                        *
 *   -----------                                        *
 *   Deletes an index attribute from a node. If the     *
 *   scheme is used, we recommend to use AGPutAttribute-*
 *   Substr rather than this PROCEDURE!                 *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   ChgMgmtGraph.InternalError                         *
 *   ChgMgmtGraph.LogError                              *
 *   PersistentGraph.IndexUnused                        *
 *   PersistentGraph.NodeNotFound                       *
 *   PersistentGraph.NotOwner                           *
 *   Graph.CyclicEvaluation                             *
 *   Graph.InternalError                                *
 *   Graph.NodeNotFound                                 *
 *   Graph.LogError                                     *
 *   Graph.Unknown                                      *
 *   Graph.WrongType                                    *
 *   Graph.NotOwner                                     *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGDeleteIndexAttribute(GraphNumber graphNumber, NodeNumber node, AttributeNumber attNo);

 
 /********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Returns all nodes which have currently invalid     *
 *   index/key attribute "AttNo". This PROCEDURE is for *
 *   applications which do not use the automatic evalu- *
 *   facility of GRAS but have their own method to re-  *
 *   compute invalid derived attributes. These appli-   *
 *   cations need knowledge about invalid index attri-  *
 *   butes in order to be able to compute all these     *
 *   attributes before starting a corresponding index   *
 *   query.                                             *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *                                                      *
 ********************************************************/
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Without scheme                                     *
 *   --------------                                     *
 *   Returns all attribute numbers of index attributes  *
 *   corresponding to node whose value is currently     *
 *   defined.                                           *
 *                                                      *
 *   With scheme                                        *
 *   -----------                                        *
 *   Returns all index attribute numbers corresponding  *
 *   to Node which are NOT declared in the graph scheme.*
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   PersistentGraph.InternalError                      *
 *   PersistentGraph.NodeNotFound                       *
 *   PersistentGraph,NotOwner                           *
 *   The graph is not open                              *
 *   Access.Denied                                      *
 *   PageFile.NoAccess                                  *
 *   GraphPool.InternalError                            *
 *   ChgMgmtGraphPool.InternalError                     *
 *   ChgMgmtGraphPool.NotExistent                       *
 *   ResourceTriggerStorage.TriggerDelayed              *
 *                                                      *
 ********************************************************/

void
AGGetAllIndexAttributes(GraphNumber graphNumber, NodeNumber node, SimpleSet *indexSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Returns the numbers of all attributes of a node    *
 *   which have currently a defined value. The result of*
 *   this PROCEDURE is unpredictable if there are some  *
 *   derived attributes whose value is not up to date.  *
 *   (the evaluation of these attributes will not be    *
 *   triggered automatically).                          *
 *   For index attributes, AGGetAllDefinedAttributes    *
 *   returns all those, which are declared in the scheme*
 *   and whose value is currently defined. You can use  *
 *   AGGetAllIndexAttributes to get the attributes      *
 *   which have not been defined.                       *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Denied                                      *
 *   Access.Locked                                      *
 *   PersistentGraph.InternalError                      *
 *   PersistentGraph.NodeNotFound                       *
 *   PersistentGraph.NotOwner                           *
 *   The graph is not open                              *
 *   PageFile.NoAccess                                  *
 *   GraphPool.InternalError                            *
 *   ChgMgmtGraphPool.InternalError                     *
 *   ChgMgmtGraphPool.NotExistent                       *
 *   ResourceTriggerStorage.TriggerDelayed              *
 *                                                      *
 ********************************************************/

void
AGGetAllDefinedAttributes(GraphNumber graphNumber, NodeNumber node, SimpleSet *attSet);
/********************************************************
 *                                                      *
 * Purpose:                                             *
 *                                                      *
 *   Replaces the type of the given node with "newType".*
 *   Attributes, names and edges of the node are not    *
 *   affected.                                          *
 *                                                      *
 *   DON'T USE this PROCEDURE within new applications!  *
 *                                                      *
 * Note:                                                *
 *                                                      *
 *   This operation is only admissible if the new and   *
 *   the old node type have the same attributes and     *
 *   evaluation functions (this property won't be       *
 *   assured or even checked)! Furthermore, instances   *
 *   of the new type must be allowed to have the same   *
 *   edge context as instances of the old type.         *
 *   Last but not least this function does not work for *
 *   node types which are declared within the graph     *
 *   scheme with "UseMembership"=YES (for efficiency    *
 *   reasons only).                                     *
 *                                                      *
 * Error Messages:                                      *
 *                                                      *
 *   Access.Locked                                      *
 *   ChgMgmtGraph.InternalError                         *
 *   ChgMgmtGraph.LogError                              *
 *   PersistentGraph.NodeNotFound                       *
 *   PersistentGraph.NotOwner                           *
 *   PersistentGraph.IndexUnused                        *
 *   PersistentGraph.InternalError                      *
 *   PersistentGraph.IndexUsed                          *
 *   The graph is not open                              *
 *                                                      *
 ********************************************************/

void
AGChangeNodeType(GraphNumber graphNumber, NodeNumber node, TypeNumber newType);

void
RGRASGraph_create_proc_address_table(_LINK_INFO *l_info);

void
CollectGarbage();

#endif
