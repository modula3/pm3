#if !defined (_grastypes_)
#define _grastypes_


/************************************************************************/
/*                    Modula-3 standard data types			*/
/************************************************************************/

typedef unsigned int    CARDINAL;  /* Range 0..(2^32)-1 */
typedef unsigned short  SHORTCARD; /* Range 0..(2^16)-1 */

typedef unsigned char   BOOLEAN;   /* Range 0..(2^8)-1  */

#undef  TRUE
#undef  FALSE

#define FALSE           ((BOOLEAN) 0)
#define TRUE            ((BOOLEAN) 1)


/************************************************************************/
/*          Here starts the correspondance with AGGlobal.def		*/
/************************************************************************/

typedef char * TEXT;

typedef CARDINAL  GraphNumber;

typedef TEXT   ApplicationName;

typedef TEXT GroupName;

typedef TEXT PoolName;

typedef TEXT GraphName;

typedef TEXT SchemeName;

typedef struct _Node {
  CARDINAL graph,entity;
  } Node;

typedef Node TypeNumber;

typedef Node SimpleElement;

typedef CARDINAL SchemeNumber;

typedef CARDINAL ValueTypeNumber;

typedef CARDINAL        ExternNumber;
typedef CARDINAL        GraphType;
typedef Node            NodeNumber;
typedef Node            AttributeNumber;

typedef CARDINAL        SimpleSet;
typedef CARDINAL        RelSet;

typedef unsigned char DependencyKind;

#define SelfDependent               0
#define IncomingDependent           1
#define OutgoingDependent           2

typedef unsigned char IndexProperties;

#define Normal                      0
#define Index                       1
#define Key                         2

typedef unsigned char AttributeKind;

#define Derived                     0
#define Intrinsic                   1
#define Dummy                       2
#define Meta                        3

typedef unsigned char Cardinality;

#define OptUnique                   0
#define OblUnique                   1
#define OptSet                      2
#define OblSet                      3

typedef unsigned char GroupType;

#define Session                     0
#define Transaction                 1
#define Operation                   2

typedef unsigned char GroupMode;

#define NewGroup                    0
#define OldGroup                    1
#define GetGroup                    2

typedef unsigned char GraphPoolMode;

#define NewPool                     0
#define OldPool                     1
#define Getpool                     2

typedef unsigned char GraphMode;

#define NewGraph                    0
#define OldGraph                    1
#define GetGraph                    2

typedef unsigned char   TStatus;

#define NoError                     0
#define AlreadyExistent             1
#define AlreadyOpen                 2
#define NotExistent                 3
#define Overflow                    4
#define NotEmpty                    5
#define Locked                      6
#define NoDump                      7
#define NoOriginal                  8
#define NoDumpNoOriginal            9
#define TagfieldExistent           10
#define UnknownGraph               11
#define NoNewGraph                 12
#define UnprocessableGraph         13
#define NotSaved                   14
#define IncompatibleGraph          15
#define AttributeSizeError         16
#define NoEvalFunction             17
#define StillPendingTransaction    18
#define NoPendingTransaction       19
#define ActionsBlocked             20
#define ActionsNotBlocked          21
#define NoActionperformer          22
#define RGInternalError            23

typedef unsigned EvalFunction;

#define SaveGroup ""

#define DummyValue          0
#define BooleanValue        1
#define CardinalValue       2
#define IntegerValue        3
#define RealValue           4
#define CharValue           5
#define StringValue         6
#define SurrogateValue      7
#define SurrogatePairValue  8
#define UnknownValue        9
#define ConstraintValue    10

enum EventKind {
	EVNewNode = 0,
	EVDeleteNode = 1,
	EVNewEdge = 2,
	EVDeleteEdge = 3,
	EVModifyAttribute = 4,
	EVIndexKeyModified = 5,
	EVTransactionStart = 6,
	EVTransactionEnd = 7,
	EVSetCheckpoint = 8,
	EVUndo = 9,
	EVRedo = 10,
	EVOpenGraph = 11,
	EVCloseGraph = 12,
	EVUserDefined = 13,
};
typedef enum EventKind EventKind;

struct GraphEvent {
	EventKind Kind;
	NodeNumber SNode;
	BOOLEAN SNodeExists;
	NodeNumber TNode;
	BOOLEAN TNodeExists;
	TypeNumber TNo;
	BOOLEAN Self;
};
typedef struct GraphEvent GraphEvent;

typedef void (*ActionProc)(GraphNumber, GraphEvent);

typedef CARDINAL ActionPriority;

#endif /* _grastypes_ */
