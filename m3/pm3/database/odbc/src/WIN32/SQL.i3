
(* SQL defines the C-level interface to the ODBC system.  The contents
   of this interface were derived from the Microsoft version of "sql.h"
   dated 4/10/95.

   See RDB.i3 for a safe Modula-3 interface to the same functionality.  *)

UNSAFE INTERFACE SQL;

FROM SQLtypes IMPORT SQLRETURN, SQLHENV, SQLHENV_star, SQLHDBC, SQLHDBC_star,
   SQLHSTMT, SQLHSTMT_star, SQLUSMALLINT, SQLUSMALLINT_star, SQLSMALLINT,
   SQLSMALLINT_star, SQLPOINTER, SQLPOINTER_star, SQLINTEGER, SQLINTEGER_star,
   SQLUINTEGER, SQLUINTEGER_star, SQLCHAR_star;

CONST
  ODBCVER         = 16_0250;    (* ODBC version number *)
  SQL_SPEC_MAJOR  = 2;          (* Major version of specification  *)
  SQL_SPEC_MINOR  = 50;         (* Minor version of specification  *)
  SQL_SPEC_STRING = "02.50";    (* String constant for version     *)


  SQL_SQLSTATE_SIZE      = 5;   (* size of SQL state *)
  SQL_MAX_MESSAGE_LENGTH = 512; (* message buffer size *)
  SQL_MAX_DSN_LENGTH     = 32 ; (* maximum data source name size *)

  (* RETCODEs *)  
  SQL_INVALID_HANDLE     = -2;
  SQL_ERROR              = -1;
  SQL_SUCCESS            = 0;
  SQL_SUCCESS_WITH_INFO  = 1;
  SQL_NO_DATA_FOUND      = 100;

  (* Standard SQL datatypes, using ANSI numbering *)
  SQL_CHAR               = 1;
  SQL_NUMERIC            = 2;
  SQL_DECIMAL            = 3;
  SQL_INTEGER            = 4;
  SQL_SMALLINT           = 5;
  SQL_FLOAT              = 6;
  SQL_REAL               = 7;
  SQL_DOUBLE             = 8;
  SQL_VARCHAR            = 12;

  SQL_TYPE_NULL          = 0;
  SQL_TYPE_MIN           = SQL_BIT;
  SQL_TYPE_MAX           = SQL_VARCHAR;
  SQL_ALL_TYPES          = 0;

  (* C datatype to SQL datatype mapping *)     (* SQL types                       *)
  SQL_C_CHAR             = SQL_CHAR;           (* CHAR, VARCHAR, DECIMAL, NUMERIC *)
  SQL_C_LONG             = SQL_INTEGER;        (* INTEGER                         *)
  SQL_C_SHORT            = SQL_SMALLINT;       (* SMALLINT                        *)
  SQL_C_FLOAT            = SQL_REAL;           (* REAL                            *)
  SQL_C_DOUBLE           = SQL_DOUBLE;         (* FLOAT, DOUBLE                   *)
  SQL_C_DEFAULT          = 99;

  (* NULL status constants.  These are used in SQLColumns, SQLColAttributes,
     SQLDescribeCol, SQLDescribeParam, and SQLSpecialColumns to describe the
     nullability of a column in a table. *)
  SQL_NO_NULLS           = 0;
  SQL_NULLABLE           = 1;
  SQL_NULLABLE_UNKNOWN   = 2;

  (* Special length values *)
  SQL_NULL_DATA          = -1;
  SQL_DATA_AT_EXEC       = -2;
  SQL_NTS                = -3;

  (* SQLFreeStmt defines *)
  SQL_CLOSE              = 0;
  SQL_DROP               = 1;
  SQL_UNBIND             = 2;
  SQL_RESET_PARAMS       = 3;

  (* SQLTransact defines *)
  SQL_COMMIT             = 0;
  SQL_ROLLBACK           = 1;

  (* SQLColAttributes defines *)
  SQL_COLUMN_COUNT          = 0;
  SQL_COLUMN_NAME           = 1;
  SQL_COLUMN_TYPE           = 2;
  SQL_COLUMN_LENGTH         = 3;
  SQL_COLUMN_PRECISION      = 4;
  SQL_COLUMN_SCALE          = 5;
  SQL_COLUMN_DISPLAY_SIZE   = 6;
  SQL_COLUMN_NULLABLE       = 7;
  SQL_COLUMN_UNSIGNED       = 8;
  SQL_COLUMN_MONEY          = 9;
  SQL_COLUMN_UPDATEABLE     = 10;
  SQL_COLUMN_AUTO_INCREMENT = 11;
  SQL_COLUMN_CASE_SENSITIVE = 12;
  SQL_COLUMN_SEARCHABLE     = 13;
  SQL_COLUMN_TYPE_NAME      = 14;
  SQL_COLUMN_TABLE_NAME     = 15;
  SQL_COLUMN_OWNER_NAME     = 16;
  SQL_COLUMN_QUALIFER_NAME  = 17;
  SQL_COLUMN_LABEL          = 18;
  SQL_COLATT_OPT_MAX        = SQL_COLUMN_LABEL;
  SQL_COLUMN_DRIVER_START   = 1000;
  SQL_COLLATT_OPT_MIN       = SQL_COLUMN_COUNT;
  
  (* SQLCollAttributes subdefines for SQL_COLUMN_UPDATEABLE *)
  SQL_ATTR_READONLY         = 0;
  SQL_ATTR_WRITE            = 1;
  SQL_ATTR_READWITE_UNKNOWN = 2;
  
  (* SQLColAttributes subdefines for SQL_COLUMN_SEARCHABLE *)
  (* These are also used by SQLGetInfo. *)
  SQL_UNSEARCHABLE          = 0;
  SQL_LIKE_ONLY             = 1;
  SQL_ALL_EXCEPT_LIKE       = 2;
  SQL_SEARCHABLE            = 3;
  
  (* SQLError defines *)
  SQL_NULL_HENV  : SQLHENV  = NIL;
  SQL_NULL_HDBC  : SQLHDBC  = NIL; 
  SQL_NULL_HSTMT : SQLHSTMT = NIL;
  
(* Core Function Prototypes *)

<*EXTERNAL SQLAllocConnect:WINAPI *>
PROCEDURE SQLAllocConnect (henv: SQLHENV;  phdbc: SQLHDBC_star): SQLRETURN;

<*EXTERNAL SQLAllocEnv:WINAPI *>
PROCEDURE SQLAllocEnv (phenv: SQLHENV_star): SQLRETURN;

<*EXTERNAL SQLAllocStmt:WINAPI *>
PROCEDURE SQLAllocStmt (hdbc:  SQLHDBC;  phstmt: SQLHSTMT_star): SQLRETURN;

<*EXTERNAL SQLBindCol:WINAPI *>
PROCEDURE SQLBindCol (
            hstmt:      SQLHSTMT;
            icol:       SQLUSMALLINT;
            fCtype:     SQLSMALLINT;
            rgbValue:   SQLPOINTER;
            cbValueMax: SQLINTEGER;
            pcbValue:   SQLINTEGER_star): SQLRETURN;
        
<*EXTERNAL SQLCancel:WINAPI *>
PROCEDURE SQLCancel (hstmt: SQLHSTMT): SQLRETURN;
        
<*EXTERNAL SQLColAttributes:WINAPI *>
PROCEDURE SQLColAttributes (
            hstmt:     SQLHSTMT;
            icol:      SQLUSMALLINT;
            fDescType: SQLUSMALLINT;
            rgbDesc:   SQLPOINTER;
            cbDescMax: SQLSMALLINT;
            pcbDesc:   SQLSMALLINT_star;
            pfDesc:    SQLINTEGER_star): SQLRETURN;
        
<*EXTERNAL SQLConnect:WINAPI *>
PROCEDURE SQLConnect (
            hdbc:      SQLHDBC;
            szDSN:     SQLCHAR_star;
            cbDSN:     SQLSMALLINT;
            szUID:     SQLCHAR_star;
            cbUID:     SQLSMALLINT;
            szAuthStr: SQLCHAR_star;
            cbAuthStr: SQLSMALLINT): SQLRETURN;
        
<*EXTERNAL SQLDescribeCol:WINAPI *>
PROCEDURE SQLDescribeCol (
            hstmt:        SQLHSTMT;
            icol:         SQLUSMALLINT;
            szColName:    SQLCHAR_star;
            cbColNameMax: SQLSMALLINT;
            pcbColName:   SQLSMALLINT_star;
            pfSqlType:    SQLSMALLINT_star;
            pcbColDef:    SQLUINTEGER_star;
            pibScale:     SQLSMALLINT_star;
            pfNullable:   SQLSMALLINT_star): SQLRETURN;
        
<*EXTERNAL SQLDisconnect:WINAPI *>
PROCEDURE SQLDisconnect (hdbc: SQLHDBC): SQLRETURN;
        
<*EXTERNAL SQLError:WINAPI *>
PROCEDURE SQLError (
            henv:          SQLHENV;
            hdbc:          SQLHDBC;
            hstmt:         SQLHSTMT;
            szSqlState:    SQLCHAR_star;
            pfNativeError: SQLINTEGER_star;
            szErrorMsg:    SQLCHAR_star;
            cbErrorMsgMax: SQLSMALLINT;
            pcbErrorMsg:   SQLSMALLINT_star): SQLRETURN;        

<*EXTERNAL SQLExecDirect:WINAPI *>
PROCEDURE SQLExecDirect (
            hstmt:     SQLHSTMT;
            szSqlStr:  SQLCHAR_star;
            cbSqlStr:  SQLINTEGER): SQLRETURN;

<*EXTERNAL SQLExecute:WINAPI *>
PROCEDURE SQLExecute (hstmt: SQLHSTMT): SQLRETURN;
        
<*EXTERNAL SQLFetch:WINAPI *>
PROCEDURE SQLFetch (hstmt: SQLHSTMT): SQLRETURN;
        
<*EXTERNAL SQLFreeConnect:WINAPI *>
PROCEDURE SQLFreeConnect (hdbc: SQLHDBC): SQLRETURN;

<*EXTERNAL SQLFreeEnv:WINAPI *>
PROCEDURE SQLFreeEnv (henv: SQLHENV): SQLRETURN;
        
<*EXTERNAL SQLFreeStmt:WINAPI *>
PROCEDURE SQLFreeStmt (hstmt: SQLHSTMT;  fOption: SQLUSMALLINT): SQLRETURN;
        
<*EXTERNAL SQLGetCursorName:WINAPI *>
PROCEDURE SQLGetCursorName (
            hstmt:       SQLHSTMT;
            szCursor:    SQLCHAR_star;
            cbCursorMax: SQLSMALLINT;
            pcbCursor:   SQLSMALLINT_star): SQLRETURN;
        
<*EXTERNAL SQLNumResultCols:WINAPI *>
PROCEDURE SQLNumResultCols (
            hstmt: SQLHSTMT;
            pccol: SQLSMALLINT_star): SQLRETURN;
        
<*EXTERNAL SQLPrepare:WINAPI *>
PROCEDURE SQLPrepare (
            hstmt:    SQLHSTMT;
            szSqlStr: SQLCHAR_star;
            cbSqlStr: SQLINTEGER): SQLRETURN;
        
<*EXTERNAL SQLRowCount:WINAPI *>
PROCEDURE SQLRowCount (
            hstmt: SQLHSTMT;
            pcrow: SQLINTEGER_star): SQLRETURN;

<*EXTERNAL SQLSetCursorName:WINAPI *>
PROCEDURE SQLSetCursorName (
            hstmt:    SQLHSTMT;
            szCursor: SQLCHAR_star;
            cbCursor: SQLSMALLINT): SQLRETURN;
                
<*EXTERNAL SQLTransact:WINAPI *>
PROCEDURE SQLTransact (
            henv:  SQLHENV;
            hdbc:  SQLHDBC;
            fType: SQLUSMALLINT): SQLRETURN;
        
(* Deprecated functions from prior versions of ODBC *)

<*EXTERNAL SQLSetParam:WINAPI *>
PROCEDURE SQLSetParam (
            hstmt:      SQLHSTMT;
            ipar:       SQLUSMALLINT;
            fCType:     SQLSMALLINT;
            fSqlType:   SQLSMALLINT;
            cbParamDef: SQLUINTEGER;
            ibScale:    SQLSMALLINT;
            rgbValue:   SQLPOINTER;
            pcbValue:   SQLINTEGER_star): SQLRETURN;
                        
(* Defines used by both Level 1 and Level 2 functions *)

(* generally useful constants *)
CONST 
  SQL_MAX_OPTION_STRING_LENGTH = 256;

(* Additional return codes *)
CONST 
  SQL_STILL_EXECUTING     = 2;
  SQL_NEED_DATA           = 99;
  
(* SQL extended datatypes *)
CONST
   SQL_DATE                      = 9;
   SQL_TIME                      = 10;
   SQL_TIMESTAMP                 = 11;
   SQL_LONGVARCHAR               = -1;
   SQL_BINARY                    = -2;
   SQL_VARBINARY                 = -3;
   SQL_LONGVARBINARY             = -4;
   SQL_BIGINT                    = -5;
   SQL_TINYINT                   = -6;
   SQL_BIT                       = -7;
   
   SQL_INTERVAL_YEAR             = -80;
   SQL_INTERVAL_MONTH            = -81;
   SQL_INTERVAL_YEAR_TO_MONTH    = -82;
   SQL_INTERVAL_DAY              = -83;
   SQL_INTERVAL_HOUR             = -84;
   SQL_INTERVAL_MINUTE           = -85;
   SQL_INTERVAL_SECOND           = -86;
   SQL_INTERVAL_DAY_TO_HOUR      = -87;
   SQL_INTERVAL_DAY_TO_MINUTE    = -88;
   SQL_INTERVAL_DAY_TO_SECOND    = -89;
   SQL_INTERVAL_HOUR_TO_MINUTE   = -90;
   SQL_INTERVAL_HOUR_TO_SECOND   = -91;
   SQL_INTERVAL_MINUTE_TO_SECOND = -92;
   SQL_UNICODE                   = -95;
   SQL_UNICODE_VARCHAR           = -96;
   SQL_UNICODE_LONGVARCHAR       = -97;
   SQL_UNICODE_CHAR              = SQL_UNICODE;
   
   SQL_TYPE_DRIVER_START         = SQL_INTERVAL_YEAR;
   SQL_TYPE_DRIVER_END           = SQL_UNICODE_LONGVARCHAR;
   
   SQL_SIGNED_OFFSET             = -20;
   SQL_UNSIGNED_OFFSET           = -22;
   
(* C datatype to SQL datatype mapping *)
CONST
   SQL_C_DATE                    = SQL_DATE;
   SQL_C_TIME                    = SQL_TIME;
   SQL_C_TIMESTAMP               = SQL_TIMESTAMP;
   SQL_C_BINARY                  = SQL_BINARY;
   SQL_C_BIT                     = SQL_BIT;
   SQL_C_TINYINT                 = SQL_TINYINT;
   SQL_C_SLONG                   = SQL_C_LONG  + SQL_SIGNED_OFFSET;
   SQL_C_SSHORT                  = SQL_C_SHORT + SQL_SIGNED_OFFSET;
   SQL_C_STINYINT                = SQL_TINYINT + SQL_SIGNED_OFFSET;
   SQL_C_ULONG                   = SQL_C_LONG  + SQL_UNSIGNED_OFFSET;
   SQL_C_USHORT                  = SQL_C_SHORT + SQL_UNSIGNED_OFFSET;
   SQL_U_TINYINT                 = SQL_TINYINT + SQL_UNSIGNED_OFFSET;
   SQL_C_BOOKMARK                = SQL_C_ULONG;
   
(* Level 1 Functions *)

(* Special return values for SQLGetData *)
CONST
   SQL_NO_TOTAL                  = -4;
   
(* Defines for SQLGetFunctions *)
CONST
   SQL_API_SQLALLOCCONNECT       = 1; (* Core Functions                *)
   SQL_API_SQLALLOCENV           = 2;
   SQL_API_SQLALLOCSTMT          = 3;
   SQL_API_SQLBINDCOL            = 4;
   SQL_API_SQLCANCEL             = 5;
   SQL_API_SQLCOLATTRIBUTES      = 6;
   SQL_API_SQLCONNECT            = 7;
   SQL_API_SQLDESCRIBECOL        = 8;
   SQL_API_SQLDISCONNECT         = 9;
   SQL_API_SQLERROR              = 10;
   SQL_API_SQLEXECDIRECT         = 11;
   SQL_API_SQLEXECUTE            = 12;
   SQL_API_SQLFETCH              = 13;
   SQL_API_SQLFREECONNECT        = 14;
   SQL_API_SQLFREEENV            = 15;
   SQL_API_SQLFREESTMT           = 16;
   SQL_API_SQLGETCURSORNAME      = 17;
   SQL_API_NUMRESULTCOLS         = 18;
   SQL_API_SQLPREPARE            = 19;
   SQL_API_SQLROWCOUNT           = 20;
   SQL_API_SQLSETCURSORNAME      = 21;
   SQL_API_SQLSETPARAM           = 22;
   SQL_API_SQLTRANSACT           = 23;
   
   SQL_NUM_FUNCTIONS             = 23;
   
   SQL_EXT_API_START             = 40;
   
   SQL_API_SQLCOLUMNS            = 40;  (* Level 1 Functions                *)
   SQL_API_SQLDRIVERCONNECT      = 41;
   SQL_API_SQLGETCONNECTOPTION   = 42;
   SQL_API_SQLGETDATA            = 43;
   SQL_API_SQLGETFUNCTIONS       = 44;
   SQL_API_SQLGETINFO            = 45;
   SQL_API_SQLGETSTMTOPTION      = 46;
   SQL_API_SQLGETTYPEINFO        = 47;
   SQL_API_SQLPARAMDATA          = 48;
   SQL_API_SQLPUTDATA            = 49;
   SQL_API_SQLSETCONNECTOPTION   = 50;
   SQL_API_SQLSTMTOPTION         = 51;
   SQL_API_SQLSPECIALCOLUMNS     = 52;
   SQL_API_SQLSTATISTICS         = 53;
   SQL_API_SQLTABLES             = 54;

   SQL_API_SQLBROWSECONNECT      = 55; (* Level 2 Functions                *)
   SQL_API_SQLCOLUMNPRIVILEGES   = 56;
   SQL_API_SQLDATASOURCES        = 57;
   SQL_API_SQLDESCRIBEPARAM      = 58;
   SQL_API_SQLEXTENDEDFETCH      = 59;
   SQL_API_SQLFOREIGNKEYS        = 60;
   SQL_API_SQLMORERESULTS        = 61;
   SQL_API_SQLNATIVESQL          = 62;
   SQL_API_SQLNUMPARAMS          = 63;
   SQL_API_SQLPARAMOPTIONS       = 64;
   SQL_API_SQLPRIMARYKEYS        = 65;
   SQL_API_SQLPROCEDURECOLUMNS   = 66;
   SQL_API_SQLPROCEDURES         = 67;
   SQL_API_SQLSETPOS             = 68;
   SQL_API_SQLSETSCROLLOPTIONS   = 69;
   SQL_API_SQLTABLEPRIVILEGES    = 70;
   
   (* SDK 2.0 Additions *)
   SQL_API_SQLDRIVERS            = 71;
   SQL_API_SQLBINDPARAMETER      = 72;
   
   SQL_EXT_API_LAST              = SQL_API_SQLBINDPARAMETER;
   
   SQL_API_ALL_FUNCTIONS         = 0;
   
   SQL_NUM_EXTENSIONS            = SQL_EXT_API_LAST - SQL_EXT_API_START + 1;
   
   SQL_API_LOADBYORDINAL         = 199;
   
(* Defines for SQLGetInfo *)
CONST
   SQL_INFO_FIRST                = 0;
   SQL_ACTIVE_CONNECTIONS        = 0;
   SQL_ACTIVE_STATEMENTS         = 1;
   SQL_DATA_SOURCE_NAME          = 2;
   SQL_DRIVER_HDBC               = 3;
   SQL_DRIVER_HENV               = 4;
   SQL_DRIVER_HSTMT              = 5;
   SQL_DRIVER_NAME               = 6;
   SQL_DRIVER_VER                = 7;
   SQL_FETCH_DIRECTION           = 8;
   SQL_ODBC_API_CONFORMANCE      = 9;
   SQL_ODBC_VER                  = 10;
   SQL_ROW_UPDATES               = 11;
   SQL_ODBC_SAG_CLI_CONFORMANCE  = 12;
   SQL_SERVER_NAME               = 13;
   SQL_SEARCH_PATTERN_ESCAPE     = 14;
   SQL_ODBC_SQL_CONFORMANCE      = 15;

   SQL_DBMS_NAME                 = 17;
   SQL_DBMS_VER                  = 18;

   SQL_ACCESSIBLE_TABLES         = 19;
   SQL_ACCESSIBLE_PROCEDURES     = 20;
   SQL_PROCEDURES                = 21;
   SQL_CONCAT_NULL_BEHAVIOR      = 22;
   SQL_CURSOR_COMMIT_BEHAVIOR    = 23;
   SQL_CURSOR_ROLLBACK_BEHAVIOR  = 24;
   SQL_DATA_SOURCE_READ_ONLY     = 25;
   SQL_DEFAULT_TXN_ISOLATION     = 26;
   SQL_EXPRESSIONS_IN_ORDERBY    = 27;
   SQL_IDENTIFIER_CASE           = 28;
   SQL_IDENTIFIER_QUOTE_CHAR     = 29;
   SQL_MAX_COLUMN_NAME_LEN       = 30;
   SQL_MAX_CURSOR_NAME_LEN       = 31;
   SQL_MAX_OWNER_NAME_LEN        = 32;
   SQL_MAX_PROCEDURE_NAME_LEN    = 33;
   SQL_MAX_QUALIFIER_NAME_LEN    = 34;
   SQL_MAX_TABLE_NAME_LEN        = 35;
   SQL_MULT_RESULT_SETS          = 36;
   SQL_MULTIPLE_ACTIVE_TXN       = 37;
   SQL_OUTER_JOINS               = 38;
   SQL_OWNER_TERM                = 39;
   SQL_PROCEDURE_TERM            = 40;
   SQL_QUALIFIER_NAME_SEPARATOR  = 41;
   SQL_QUALIFIER_TERM            = 42;
   SQL_SCROLL_CONCURRENCY        = 43;
   SQL_SCROLL_OPTIONS            = 44;
   SQL_TABLE_TERM                = 45;
   SQL_TXN_CAPABLE               = 46;
   SQL_USER_NAME                 = 47;
   
   SQL_CONVERT_FUNCTIONS         = 48;
   SQL_NUMERIC_FUNCTIONS         = 49;
   SQL_STRING_FUNCTIONS          = 50;
   SQL_SYSTEM_FUNCTIONS          = 51;
   SQL_TIMEDATE_FUNCTIONS        = 52;

   SQL_CONVERT_BIGINT            = 53;
   SQL_CONVERT_BINARY            = 54;
   SQL_CONVERT_BIT               = 55;
   SQL_CONVERT_CHAR              = 56;
   SQL_CONVERT_DATE              = 57;
   SQL_CONVERT_DECIMAL           = 58;
   SQL_CONVERT_DOUBLE            = 59;
   SQL_CONVERT_FLOAT             = 60;
   SQL_CONVERT_INTEGER           = 61;
   SQL_CONVERT_LONGVARCHAR       = 62;
   SQL_CONVERT_NUMERIC           = 63;
   SQL_CONVERT_REAL              = 64;
   SQL_CONVERT_SMALLINT          = 65;
   SQL_CONVERT_TIME              = 66;
   SQL_CONVERT_TIMESTAMP         = 67;
   SQL_CONVERT_TINYINT           = 68;
   SQL_CONVERT_VARBINARY         = 69;
   SQL_CONVERT_VARCHAR           = 70;
   SQL_CONVERT_LONGVARBINARY     = 71;

   SQL_TXN_ISOLATION_OPTION      = 72;
   SQL_ODBC_SQL_OPT_IEF          = 73;

   (* ODBC SDK 1.0 Additions *)
   SQL_CORRELATION_NAME          = 74;
   SQL_NON_NULLABLE_COLUMNS      = 75;

   (* ODBC SDK 2.0 Additions *)
   SQL_DRIVER_HLIB               = 76;
   SQL_DRIVER_ODBC_VER           = 77;
   SQL_LOCK_TYPES                = 78;
   SQL_POS_OPERATIONS            = 79;
   SQL_POSITIONED_STATEMENTS     = 80;
   SQL_GETDATA_EXTENSIONS        = 81;
   SQL_BOOKMARK_PERSISTENCE      = 82;
   SQL_STATIC_SENSITIVITY        = 83;
   SQL_FILE_USAGE                = 84;
   SQL_NULL_COLLATION            = 85;
   SQL_ALTER_TABLE               = 86;
   SQL_COLUMN_ALIAS              = 87;
   SQL_GROUP_BY                  = 88;
   SQL_KEYWORDS                  = 89;
   SQL_ORDER_BY_COLUMNS_IN_SELECT= 90;
   SQL_OWNER_USAGE               = 91;
   SQL_QUALIFIER_USAGE           = 92;
   SQL_QUOTED_IDENTIFIER_CASE    = 93;
   SQL_SPECIAL_CHARACTERS        = 94;
   SQL_SUBQUERIES                = 95;
   SQL_UNION                     = 96;
   SQL_MAX_COLUMNS_IN_GROUP_BY   = 97;
   SQL_MAX_COLUMNS_IN_INDEX      = 98;
   SQL_MAX_COLUMNS_IN_ORDER_BY   = 99;
   SQL_MAX_COLUMNS_IN_SELECT     = 100;
   SQL_MAX_COLUMNS_IN_TABLE      = 101;
   SQL_MAX_INDEX_SIZE            = 102;
   SQL_MAX_ROW_SIZE_INCLUDES_LONG= 103;
   SQL_MAX_ROW_SIZE              = 104;
   SQL_MAX_STATEMENT_LEN         = 105;
   SQL_MAX_TABLES_IN_SELECT      = 106;
   SQL_MAX_USER_NAME_LEN         = 107;
   SQL_MAX_CHAR_LITERAL_LEN      = 108;
   SQL_TIMEDATE_ADD_INTERVALS    = 109;
   SQL_TIMEDATE_DIFF_INTERVALS   = 110;
   SQL_NEED_LONG_DATA_LEN        = 111;
   SQL_MAX_BINARY_LITERAL_LEN    = 112;
   SQL_LIKE_ESCAPE_CLAUSE        = 113;
   SQL_QUALIFIER_LOCATION        = 114;

   (* ODBC SDK 2.01 Additions *)
   SQL_OJ_CAPABILITIES           = 65003;  (* Temporary value until ODBC 3.0 *)
   
   SQL_INFO_LAST                 = SQL_QUALIFIER_LOCATION;
   SQL_INFO_DRIVER_START         = 1000;

(* SQL_CONVERT_* return value bitmasks *)
CONST
   SQL_CVT_CHAR                  = 16_00000001;
   SQL_CVT_NUMERIC               = 16_00000002;
   SQL_CVT_DECIMAL               = 16_00000004;
   SQL_CVT_INTEGER               = 16_00000008;
   SQL_CVT_SMALLINT              = 16_00000010;
   SQL_CVT_FLOAT                 = 16_00000020;
   SQL_CVT_REAL                  = 16_00000040;
   SQL_CVT_DOUBLE                = 16_00000080;
   SQL_CVT_VARCHAR               = 16_00000100;
   SQL_CVT_LONGVARCHAR           = 16_00000200;
   SQL_CVT_BINARY                = 16_00000400;
   SQL_CVT_VARBINARY             = 16_00000800;
   SQL_CVT_BIT                   = 16_00001000;
   SQL_CVT_TINYINT               = 16_00002000;
   SQL_CVT_BIGINT                = 16_00004000;
   SQL_CVT_DATE                  = 16_00008000;
   SQL_CVT_TIME                  = 16_00010000;
   SQL_CVT_TIMESTAMP             = 16_00020000;
   SQL_CVT_LONGVARBINARY         = 16_00040000;

(* SQL_CONVERT_FUNCTIONS functions *)
CONST
   SQL_FN_CVT_CONVERT            = 16_00000001;
   
(* SQL_STRING_FUNCTIONS functions *)
CONST
   SQL_FN_STR_CONCAT             = 16_00000001;
   SQL_FN_STR_INSERT             = 16_00000002;
   SQL_FN_STR_LEFT               = 16_00000004;
   SQL_FN_STR_LTRIM              = 16_00000008;
   SQL_FN_STR_LENGTH             = 16_00000010;
   SQL_FN_STR_LOCATE             = 16_00000020;
   SQL_FN_STR_LCASE              = 16_00000040;
   SQL_FN_STR_REPEAT             = 16_00000080;
   SQL_FN_STR_REPLACE            = 16_00000100;
   SQL_FN_STR_RIGHT              = 16_00000200;
   SQL_FN_STR_RTRIM              = 16_00000400;
   SQL_FN_STR_SUBSTRING          = 16_00000800;
   SQL_FN_STR_UCASE              = 16_00001000;
   SQL_FN_STR_ASCII              = 16_00002000;
   SQL_FN_STR_CHAR               = 16_00004000;
   SQL_FN_STR_DIFFERENCE         = 16_00008000;
   SQL_FN_STR_LOCATE_2           = 16_00010000;
   SQL_FN_STR_SOUNDEX            = 16_00020000;
   SQL_FN_STR_SPACE              = 16_00040000;
   
(* SQL_NUMERIC_FUNCTIONS functions *)
CONST
   SQL_FN_NUM_ABS                = 16_00000001;
   SQL_FN_NUM_ACOS               = 16_00000002;
   SQL_FN_NUM_ASIN               = 16_00000004;
   SQL_FN_NUM_ATAN               = 16_00000008;
   SQL_FN_NUM_ATAN2              = 16_00000010;
   SQL_FN_NUM_CEILING            = 16_00000020;
   SQL_FN_NUM_COS                = 16_00000040;
   SQL_FN_NUM_COT                = 16_00000080;
   SQL_FN_NUM_EXP                = 16_00000100;
   SQL_FN_NUM_FLOOR              = 16_00000200;
   SQL_FN_NUM_LOG                = 16_00000400;
   SQL_FN_NUM_MOD                = 16_00000800;
   SQL_FN_NUM_SIGN               = 16_00001000;
   SQL_FN_NUM_SIN                = 16_00002000;
   SQL_FN_NUM_SQRT               = 16_00004000;
   SQL_FN_NUM_TAN                = 16_00008000;
   SQL_FN_NUM_PI                 = 16_00010000;
   SQL_FN_NUM_RAND               = 16_00020000;
   SQL_FN_NUM_DEGREES            = 16_00040000;
   SQL_FN_NUM_LOG10              = 16_00080000;
   SQL_FN_NUM_POWER              = 16_00100000;
   SQL_FN_NUM_RADIANS            = 16_00200000;
   SQL_FN_NUM_ROUND              = 16_00400000;
   SQL_FN_NUM_TRUNCATE           = 16_00800000;

(* SQL_TIMEDATE_FUNCTIONS functions *)
CONST
   SQL_FN_TD_NOW                 = 16_00000001;
   SQL_FN_TD_CURDATE             = 16_00000002;
   SQL_FN_TD_DAYOFMONTH          = 16_00000004;
   SQL_FN_TD_DAYOFWEEK           = 16_00000008;
   SQL_FN_TD_DAYOFYEAR           = 16_00000010;
   SQL_FN_TD_MONTH               = 16_00000020;
   SQL_FN_TD_QUARTER             = 16_00000040;
   SQL_FN_TD_WEEK                = 16_00000080;
   SQL_FN_TD_YEAR                = 16_00000100;
   SQL_FN_TD_CURTIME             = 16_00000200;
   SQL_FN_TD_HOUR                = 16_00000400;
   SQL_FN_TD_MINUTE              = 16_00000800;
   SQL_FN_TD_SECOND              = 16_00001000;
   SQL_FN_TD_TIMESTAMPADD        = 16_00002000;
   SQL_FN_TD_TIMESTAMPDIFF       = 16_00004000;
   SQL_FN_TD_DAYNAME             = 16_00008000;
   SQL_FN_TD_MONTHNAME           = 16_00010000;
  
(* SQL_SYSTEM_FUNCTIONS functions *) 
CONST
   SQL_FN_SYS_USERNAME           = 16_00000001;
   SQL_FN_SYS_DBNAME             = 16_00000002;
   SQL_FN_SYS_IFNULL             = 16_00000004;
   
(* SQL_TIMEDATE_ADD_INTERVALS and SQL_TIMEDATE_DIFF_INTERVALS functions *)
CONST
   SQL_FN_TSI_FRAC_SECOND        = 16_00000001;
   SQL_FN_TSI_SECOND             = 16_00000002;
   SQL_FN_TSI_MINUTE             = 16_00000004;
   SQL_FN_TSI_HOUR               = 16_00000008;
   SQL_FN_TSI_DAY                = 16_00000010;
   SQL_FN_TSI_WEEK               = 16_00000020;
   SQL_FN_TSI_MONTH              = 16_00000040;
   SQL_FN_TSI_QUARTER            = 16_00000080;
   SQL_FN_TSI_YEAR               = 16_00000100;

(* SQL_ODBC_API_CONFORMANCE values *)
CONST
   SQL_OAC_NONE                  = 16_0000;
   SQL_OAC_LEVEL1                = 16_0001;
   SQL_OAC_LEVEL2                = 16_0002;

(* SQL_ODBC_SAG_CLI_CONFORMANCE values *)
CONST
   SQL_OSCC_NOT_COMPLIANT        = 16_0000;
   SQL_OSCC_COMPLIANT            = 16_0001;

(* SQL_ODBC_SQL_CONFORMANCE values *)

   SQL_OSC_MINIMUM               = 16_0000;
   SQL_OSC_CORE                  = 16_0001;
   SQL_OSC_EXTENDED              = 16_0002;

(* SQL_CONCAT_NULL_BEHAVIOR values *)

   SQL_CB_NULL                   = 16_0000;
   SQL_CB_NON_NULL               = 16_0001;

(* SQL_CURSOR_COMMIT_BEHAVIOR and SQL_CURSOR_ROLLBACK_BEHAVIOR values *)

   SQL_CB_DELETE                 = 16_0000;
   SQL_CB_CLOSE                  = 16_0001;
   SQL_CB_PRESERVE               = 16_0002;

(* SQL_IDENTIFIER_CASE values *)

   SQL_IC_UPPER                  = 16_0001;
   SQL_IC_LOWER                  = 16_0002;
   SQL_IC_SENSITIVE              = 16_0003;
   SQL_IC_MIXED                  = 16_0004;

(* SQL_TXN_CAPABLE values *)

   SQL_TC_NONE                   = 16_0000;
   SQL_TC_DML                    = 16_0001;
   SQL_TC_ALL                    = 16_0002;
   SQL_TC_DDL_COMMIT             = 16_0003;
   SQL_TC_DDL_IGNORE             = 16_0004;

(* SQL_SCROLL_OPTIONS masks *)

   SQL_SO_FORWARD_ONLY           = 16_00000001;
   SQL_SO_KEYSET_DRIVEN          = 16_00000002;
   SQL_SO_DYNAMIC                = 16_00000004;
   SQL_SO_MIXED                  = 16_00000008;
   SQL_SO_STATIC                 = 16_00000010;

(* SQL_SCROLL_CONCURRENCY masks *)

   SQL_SCCO_READ_ONLY            = 16_00000001;
   SQL_SCCO_LOCK                 = 16_00000002;
   SQL_SCCO_OPT_ROWVER           = 16_00000004;
   SQL_SCCO_OPT_VALUES           = 16_00000008;
 
(* SQL_FETCH_DIRECTION masks *)

   SQL_FD_FETCH_NEXT             = 16_00000001;
   SQL_FD_FETCH_FIRST            = 16_00000002;
   SQL_FD_FETCH_LAST             = 16_00000004;
   SQL_FD_FETCH_PRIOR            = 16_00000008;
   SQL_FD_FETCH_ABSOLUTE         = 16_00000010;
   SQL_FD_FETCH_RELATIVE         = 16_00000020;
   SQL_FD_FETCH_RESUME           = 16_00000040;
   SQL_FD_FETCH_BOOKMARK         = 16_00000080;

(* SQL_TXN_ISOLATION_OPTION masks *)

   SQL_TXN_READ_UNCOMMITTED      = 16_00000001;
   SQL_TXN_READ_COMMITTED        = 16_00000002;
   SQL_TXN_REPEATABLE_READ       = 16_00000004;
   SQL_TXN_SERIALIZABLE          = 16_00000008;
   SQL_TXN_VERSIONING            = 16_00000010;

(* SQL_CORRELATION_NAME values *)

   SQL_CN_NONE                   = 16_0000;
   SQL_CN_DIFFERENT              = 16_0001;
   SQL_CN_ANY                    = 16_0002;

(* SQL_NON_NULLABLE_COLUMNS values *)

   SQL_NNC_NULL                  = 16_0000;
   SQL_NNC_NON_NULL              = 16_0001;

(* SQL_NULL_COLLATION values *)

   SQL_NC_HIGH                   = 16_0000;
   SQL_NC_LOW                    = 16_0001;
   SQL_NC_START                  = 16_0002;
   SQL_NC_END                    = 16_0004;

(* SQL_FILE_USAGE values *)

   SQL_FILE_NOT_SUPPORTED        = 16_0000;
   SQL_FILE_TABLE                = 16_0001;
   SQL_FILE_QUALIFIER            = 16_0002;

(* SQL_GETDATA_EXTENSIONS values *)

   SQL_GD_ANY_COLUMN             = 16_00000001;
   SQL_GD_ANY_ORDER              = 16_00000002;
   SQL_GD_BLOCK                  = 16_00000004;
   SQL_GD_BOUND                  = 16_00000008;

(* SQL_ALTER_TABLE values *)

   SQL_AT_ADD_COLUMN             = 16_00000001;
   SQL_AT_DROP_COLUMN            = 16_00000002;
   
(* SQL_POSITIONED_STATEMENTS masks *)

   SQL_PS_POSITIONED_DELETE      = 16_00000001;
   SQL_PS_POSITIONED_UPDATE      = 16_00000002;
   SQL_PS_SELECT_FOR_UPDATE      = 16_00000004;

(* SQL_GROUP_BY values *)

   SQL_GB_NOT_SUPPORTED          = 16_0000;
   SQL_GB_GROUP_BY_EQUALS_SELECT = 16_0001;
   SQL_GB_GROUP_BY_CONTAINS_SELECT = 16_0002;
   SQL_GB_NO_RELATION            = 16_0003;

(* SQL_OWNER_USAGE masks *)

   SQL_OU_DML_STATEMENTS         = 16_00000001;
   SQL_OU_PROCEDURE_INVOCATION   = 16_00000002;
   SQL_OU_TABLE_DEFINITION       = 16_00000004;
   SQL_OU_INDEX_DEFINITION       = 16_00000008;
   SQL_OU_PRIVILEGE_DEFINITION   = 16_00000010;

(* SQL_QUALIFIER_USAGE masks *)

   SQL_QU_DML_STATEMENTS         = 16_00000001;
   SQL_QU_PROCEDURE_INVOCATION   = 16_00000002;
   SQL_QU_TABLE_DEFINITION       = 16_00000004;
   SQL_QU_INDEX_DEFINITION       = 16_00000008;
   SQL_QU_PRIVILEGE_DEFINITION   = 16_00000010;

(* SQL_SUBQUERIES masks *)

   SQL_SQ_COMPARISON             = 16_00000001;
   SQL_SQ_EXISTS                 = 16_00000002;
   SQL_SQ_IN                     = 16_00000004;
   SQL_SQ_QUANTIFIED             = 16_00000008;
   SQL_SQ_CORRELATED_SUBQUERIES  = 16_00000010;

(* SQL_UNION masks *)

   SQL_U_UNION                   = 16_00000001;
   SQL_U_UNION_ALL               = 16_00000002;

(* SQL_BOOKMARK_PERSISTENCE values *)

   SQL_BP_CLOSE                  = 16_00000001;
   SQL_BP_DELETE                 = 16_00000002;
   SQL_BP_DROP                   = 16_00000004;
   SQL_BP_TRANSACTION            = 16_00000008;
   SQL_BP_UPDATE                 = 16_00000010;
   SQL_BP_OTHER_HSTMT            = 16_00000020;
   SQL_BP_SCROLL                 = 16_00000040;

(* SQL_STATIC_SENSITIVITY values *)

   SQL_SS_ADDITIONS              = 16_00000001;
   SQL_SS_DELETIONS              = 16_00000002;
   SQL_SS_UPDATES                = 16_00000004;

(* SQL_LOCK_TYPESL masks *)

   SQL_LCK_NO_CHANGE             = 16_00000001;
   SQL_LCK_EXCLUSIVE             = 16_00000002;
   SQL_LCK_UNLOCK                = 16_00000004;

(* SQL_POS_OPERATIONS masks *)

   SQL_POS_POSITION              = 16_00000001;
   SQL_POS_REFRESH               = 16_00000002;
   SQL_POS_UPDATE                = 16_00000004;
   SQL_POS_DELETE                = 16_00000008;
   SQL_POS_ADD                   = 16_00000010;

(* SQL_QUALIFIER_LOCATION values *)

   SQL_QL_START                  = 16_0001;
   SQL_QL_END                    = 16_0002;

(* SQL_OJ_CAPABILITIES values *)

   SQL_OJ_LEFT                   = 16_00000001;
   SQL_OJ_RIGHT                  = 16_00000002;
   SQL_OJ_FULL                   = 16_00000004;
   SQL_OJ_NESTED                 = 16_00000008;
   SQL_OJ_NOT_ORDERED            = 16_00000010;
   SQL_OJ_INNER                  = 16_00000020;
   SQL_OJ_ALL_COMPARISON_OPS     = 16_00000040;

(* options for SQLGetStmtOption/SQLSetStmtOption *)
   SQL_QUERY_TIMEOUT             = 0;
   SQL_MAX_ROWS                  = 1;
   SQL_NOSCAN                    = 2;
   SQL_MAX_LENGTH                = 3;
   SQL_ASYNC_ENABLE              = 4;
   SQL_BIND_TYPE                 = 5;
   SQL_CURSOR_TYPE               = 6;
   SQL_CONCURRENCY               = 7;
   SQL_KEYSET_SIZE               = 8;
   SQL_ROWSET_SIZE               = 9;
   SQL_SIMULATE_CURSOR           = 10;
   SQL_RETRIEVE_DATA             = 11;
   SQL_USE_BOOKMARKS             = 12;
   SQL_GET_BOOKMARK              = 13;      (* GetStmtOption Only *)
   SQL_ROW_NUMBER                = 14;      (* GetStmtOption Only *)
   SQL_STMT_OPT_MAX              = SQL_ROW_NUMBER;
   SQL_STMT_OPT_MIN              = SQL_QUERY_TIMEOUT;

(* SQL_QUERY_TIMEOUT options *)
   SQL_QUERY_TIMEOUT_DEFAULT     = 0;

(* SQL_MAX_ROWS options *)
   SQL_MAX_ROWS_DEFAULT          = 0;

(* SQL_NOSCAN options *)
   SQL_NOSCAN_OFF                = 0;     (* 1.0 FALSE *)
   SQL_NOSCAN_ON                 = 1;     (* 1.0 TRUE *)
   SQL_NOSCAN_DEFAULT            = SQL_NOSCAN_OFF;

(* SQL_MAX_LENGTH options *)
   SQL_MAX_LENGTH_DEFAULT        = 0;

(* SQL_ASYNC_ENABLE options *)
   SQL_ASYNC_ENABLE_OFF          = 0;
   SQL_ASYNC_ENABLE_ON           = 1;
   SQL_ASYNC_ENABLE_DEFAULT      = SQL_ASYNC_ENABLE_OFF;

(* SQL_BIND_TYPE options *)
   SQL_BIND_BY_COLUMN            = 0;
   SQL_BIND_TYPE_DEFAULT         = SQL_BIND_BY_COLUMN;  (* Default value *)

(* SQL_CONCURRENCY options *)
   SQL_CONCUR_READ_ONLY          = 1;
   SQL_CONCUR_LOCK               = 2;
   SQL_CONCUR_ROWVER             = 3;
   SQL_CONCUR_VALUES             = 4;
   SQL_CONCUR_DEFAULT            = SQL_CONCUR_READ_ONLY; (* Default value *)

(* SQL_CURSOR_TYPE options *)
   SQL_CURSOR_FORWARD_ONLY       = 0;
   SQL_CURSOR_KEYSET_DRIVEN      = 1;
   SQL_CURSOR_DYNAMIC            = 2;
   SQL_CURSOR_STATIC             = 3;
   SQL_CURSOR_TYPE_DEFAULT       = SQL_CURSOR_FORWARD_ONLY; (* Default value *)

(* SQL_ROWSET_SIZE options *)
   SQL_ROWSET_SIZE_DEFAULT       = 1;

(* SQL_KEYSET_SIZE options *)
   SQL_KEYSET_SIZE_DEFAULT       = 0;

(* SQL_SIMULATE_CURSOR options *)
   SQL_SC_NON_UNIQUE             = 0;
   SQL_SC_TRY_UNIQUE             = 1;
   SQL_SC_UNIQUE                 = 2;

(* SQL_RETRIEVE_DATA options *)
   SQL_RD_OFF                    = 0;
   SQL_RD_ON                     = 1;
   SQL_RD_DEFAULT                = SQL_RD_ON;

(* SQL_USE_BOOKMARKS options *)
   SQL_UB_OFF                    = 0;
   SQL_UB_ON                     = 1;
   SQL_UB_DEFAULT                = SQL_UB_OFF;

(* options for SQLSetConnectOption/SQLGetConnectOption *)
   SQL_ACCESS_MODE               = 101;
   SQL_AUTOCOMMIT                = 102;
   SQL_LOGIN_TIMEOUT             = 103;
   SQL_OPT_TRACE                 = 104;
   SQL_OPT_TRACEFILE             = 105;
   SQL_TRANSLATE_DLL             = 106;
   SQL_TRANSLATE_OPTION          = 107;
   SQL_TXN_ISOLATION             = 108;
   SQL_CURRENT_QUALIFIER         = 109;
   SQL_ODBC_CURSORS              = 110;
   SQL_QUIET_MODE                = 111;
   SQL_PACKET_SIZE               = 112;
   SQL_CONN_OPT_MAX              = SQL_PACKET_SIZE;
   SQL_CONNECT_OPT_DRVR_START    = 1000;
   SQL_CONN_OPT_MIN              = SQL_ACCESS_MODE;

(* SQL_ACCESS_MODE options *)
   SQL_MODE_READ_WRITE           = 0;
   SQL_MODE_READ_ONLY            = 1;
   SQL_MODE_DEFAULT              = SQL_MODE_READ_WRITE;

(* SQL_AUTOCOMMIT options *)
   SQL_AUTOCOMMIT_OFF            = 0;
   SQL_AUTOCOMMIT_ON             = 1;
   SQL_AUTOCOMMIT_DEFAULT        = SQL_AUTOCOMMIT_ON;

(* SQL_LOGIN_TIMEOUT options *)
   SQL_LOGIN_TIMEOUT_DEFAULT     = 15;

(* SQL_OPT_TRACE options *)
   SQL_OPT_TRACE_OFF             = 0;
   SQL_OPT_TRACE_ON              = 1;
   SQL_OPT_TRACE_DEFAULT         = SQL_OPT_TRACE_OFF;
   SQL_OPT_TRACE_FILE_DEFAULT    = "\\SQL.LOG";

(* SQL_ODBC_CURSORS options *)
   SQL_CUR_USE_IF_NEEDED         = 0;
   SQL_CUR_USE_ODBC              = 1;
   SQL_CUR_USE_DRIVER            = 2;
   SQL_CUR_DEFAULT               = SQL_CUR_USE_DRIVER;

(* Column types and scopes in SQLSpecialColumns.  *)
   SQL_BEST_ROWID                = 1;
   SQL_ROWVER                    = 2;

   SQL_SCOPE_CURROW              = 0;
   SQL_SCOPE_TRANSACTION         = 1;
   SQL_SCOPE_SESSION             = 2;

(* Defines for SQLStatistics *)
   SQL_INDEX_UNIQUE              = 0;
   SQL_INDEX_ALL                 = 1;

   SQL_QUICK                     = 0;
   SQL_ENSURE                    = 1;

(* Defines for SQLStatistics (returned in the result set) *)
   SQL_TABLE_STAT                = 0;
   SQL_INDEX_CLUSTERED           = 1;
   SQL_INDEX_HASHED              = 2;
   SQL_INDEX_OTHER               = 3;

(* Defines for SQLSpecialColumns (returned in the result set) *)
   SQL_PC_UNKNOWN                = 0;
   SQL_PC_NOT_PSEUDO             = 1;
   SQL_PC_PSEUDO                 = 2;

(* SQLDataSources "fDirection" values, also used on SQLExtendedFetch() *)
   SQL_FETCH_NEXT                = 1;
   SQL_FETCH_FIRST               = 2;

(* Level 1 Prototypes *)
<*EXTERNAL SQLColumns:WINAPI *>
PROCEDURE SQLColumns (
            hstmt:         SQLHSTMT;
            szCatalogName: SQLCHAR_star;
            cbCatalogName: SQLSMALLINT;
            szSchemaName:  SQLCHAR_star;
            cbSchemaName:  SQLSMALLINT;
            szTableName:   SQLCHAR_star;
            cbTableName:   SQLSMALLINT;
            szColumnName:  SQLCHAR_star;
            cbColumnName:  SQLSMALLINT): SQLRETURN;
        
<*EXTERNAL SQLGetConnectOption:WINAPI *>
PROCEDURE SQLGetConnectOption (
            hdbc:    SQLHDBC;
            fOption: SQLUSMALLINT;
            pvParam: SQLPOINTER): SQLRETURN;
                     
<*EXTERNAL SQLGetData:WINAPI *>
PROCEDURE SQLGetData (
            hstmt:      SQLHSTMT;
            icol:       SQLUSMALLINT;
            fCtype:     SQLSMALLINT;
            rgbValue:   SQLPOINTER;
            cbValueMax: SQLINTEGER;
            pcbValue:   SQLINTEGER_star): SQLRETURN;
                     
<*EXTERNAL SQLGetFunctions:WINAPI *>
PROCEDURE SQLGetFunctions (
            hdbc:      SQLHDBC;
            fFunction: SQLUSMALLINT;
            pfExists:  SQLUSMALLINT_star): SQLRETURN;
        
<*EXTERNAL SQLGetInfo:WINAPI *>
PROCEDURE SQLGetInfo (
            hdbc:           SQLHDBC;
            fInfoType:      SQLUSMALLINT;
            rgbInfoValue:   SQLPOINTER;
            cbInfoValueMax: SQLSMALLINT;
            pcbInfoValue:   SQLSMALLINT_star): SQLRETURN;
                     
<*EXTERNAL SQLGetStmtOption:WINAPI *>
PROCEDURE SQLGetStmtOption (
            hstmt:   SQLHSTMT;
            fOption: SQLUSMALLINT;
            pvParam: SQLPOINTER): SQLRETURN;
                     
<*EXTERNAL SQLGetTypeInfo:WINAPI *>
PROCEDURE SQLGetTypeInfo (
            hstmt:    SQLHSTMT;
            fSqlType: SQLSMALLINT): SQLRETURN;                

<*EXTERNAL SQLParamData:WINAPI *>
PROCEDURE SQLParamData (
            hstmt:      SQLHSTMT;
            prgbValue:  SQLPOINTER_star): SQLRETURN;                

<*EXTERNAL SQLPutData:WINAPI *>
PROCEDURE SQLPutData (
            hstmt:    SQLHSTMT;
            rgbValue: SQLPOINTER;
            cbValue:  SQLINTEGER): SQLRETURN;
             
                     
<*EXTERNAL SQLSetConnectOption:WINAPI *>
PROCEDURE SQLSetConnectOption (
            hdbc:    SQLHDBC;
            fOption: SQLUSMALLINT;
            vParam:  SQLUINTEGER): SQLRETURN;
                     
<*EXTERNAL SQLSetStmtOption:WINAPI *>
PROCEDURE SQLSetStmtOption (
            hstmt:   SQLHSTMT;
            fOption: SQLUSMALLINT;
            vParam:  SQLUINTEGER): SQLRETURN;
             
                     
<*EXTERNAL SQLSpecialColumns:WINAPI *>
PROCEDURE SQLSpecialColumns (
            hstmt:         SQLHSTMT;
            fColType:      SQLUSMALLINT;
            szCatalogName: SQLCHAR_star;
            cbCatalogName: SQLSMALLINT;
            szSchemaName:  SQLCHAR_star;
            cbSchemaName:  SQLSMALLINT;
            szTableName:   SQLCHAR_star;
            cbTableName:   SQLSMALLINT;
            fScope:        SQLUSMALLINT;
            fNullable:     SQLUSMALLINT): SQLRETURN;
                                  
<*EXTERNAL SQLStatistics:WINAPI *>
PROCEDURE SQLStatistics (
            hstmt:         SQLHSTMT;
            szCatalogName: SQLCHAR_star;
            cbCatalogName: SQLSMALLINT;
            szSchemaName:  SQLCHAR_star;
            cbSchemaName:  SQLSMALLINT;
            szTableName:   SQLCHAR_star;
            cbTableName:   SQLSMALLINT;
            fUnique:       SQLUSMALLINT;
            fAccuracy:     SQLUSMALLINT): SQLRETURN;
           
<*EXTERNAL SQLTables:WINAPI *>
PROCEDURE SQLTables (
            hstmt:         SQLHSTMT;
            szCatalogName: SQLCHAR_star;
            cbCatalogName: SQLSMALLINT;
            szSchemaName:  SQLCHAR_star;
            cbSchemaName:  SQLSMALLINT;
            szTableName:   SQLCHAR_star;
            cbTableName:   SQLSMALLINT;
            szTableType:   SQLCHAR_star;
            cbTableType:   SQLSMALLINT): SQLRETURN;
             
(* Level 2 Functions *)             
                   
<*EXTERNAL SQLDataSources:WINAPI *>
PROCEDURE SQLDataSources (
            henv:             SQLHENV;
            fDirection:       SQLUSMALLINT;
            szDSN:            SQLCHAR_star;
            cbDSNMax:         SQLSMALLINT;
            pcbDSN:           SQLSMALLINT_star;
            szDescription:    SQLCHAR_star;
            cbDescriptionMax: SQLSMALLINT;
            pcbDescription:   SQLSMALLINT_star): SQLRETURN;

(* Deprecated defines from prior versions of ODBC *)
CONST
  SQL_DATABASE_NAME         = 16;   (* Use SQLGetConnectOption/SQL_CURRENT_QUALIFIER *)
  SQL_FD_FETCH_PREV         = SQL_FD_FETCH_PRIOR;
  SQL_FETCH_PREV            = 4; (* == SQLext.SQL_FETCH_PRIOR  *)
  SQL_CONCUR_TIMESTAMP      = SQL_CONCUR_ROWVER;
  SQL_SCCO_OPT_TIMESTAMP    = SQL_SCCO_OPT_ROWVER;
  SQL_CC_DELETE             = SQL_CB_DELETE;
  SQL_CR_DELETE             = SQL_CB_DELETE;
  SQL_CC_CLOSE              = SQL_CB_CLOSE;
  SQL_CR_CLOSE              = SQL_CB_CLOSE;
  SQL_CC_PRESERVE           = SQL_CB_PRESERVE;
  SQL_CR_PRESERVE           = SQL_CB_PRESERVE;
  SQL_FETCH_RESUME          = 7;  (* Not supported by 2.0 drivers *)
  SQL_SCROLL_FORWARD_ONLY   = 0;  (* -SQL_CURSOR_FORWARD_ONLY *)
  SQL_SCROLL_KEYSET_DRIVEN  = -1; (* -SQL_CURSOR_KEYSET_DRIVEN *)
  SQL_SCROLL_DYNAMIC        = -2; (* -SQL_CURSOR_DYNAMIC *)
  SQL_SCROLL_STATIC         = -3; (* -SQL_CURSOR_STATIC *)
  SQL_PC_NON_PSEUDO         = SQL_PC_NOT_PSEUDO;

(* Deprecated functions from prior versions of ODBC *)

<*EXTERNAL SQLSetScrollOptions:WINAPI *>
PROCEDURE SQLSetScrollOptions (   (* Use SQLSetStmtOptions instead *)
            hstmt:        SQLHSTMT;
            fConcurrency: SQLUSMALLINT;
            crowKeyset:   SQLINTEGER;
            crowRowset:   SQLUSMALLINT): SQLRETURN;

END SQL.

