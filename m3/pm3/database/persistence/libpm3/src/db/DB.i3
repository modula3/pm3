INTERFACE DB;

IMPORT BerkeleyDB;

VAR dbenv: BerkeleyDB.DB_ENV;
<*EXTERNAL*> VAR stderr: ADDRESS;

END DB.
