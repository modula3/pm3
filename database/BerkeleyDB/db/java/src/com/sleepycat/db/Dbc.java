/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2002
 *      Sleepycat Software.  All rights reserved.
 *
 * $Id$
 */

package com.sleepycat.db;

/**
 *
 * @author Donald D. Anderson
 */
public class Dbc
{
    // methods
    //
    public native void close()
         throws DbException;

    public native int count(int flags)
         throws DbException;

    // returns: 0, DB_KEYEMPTY, or throws error
    public native int del(int flags)
         throws DbException;

    public native Dbc dup(int flags)
         throws DbException;

    // returns: 0, DB_NOTFOUND, or throws error
    public native int get(Dbt key, Dbt data, int flags)
         throws DbException;

    // returns: 0, DB_NOTFOUND, or throws error
    public native int pget(Dbt key, Dbt pkey, Dbt data, int flags)
         throws DbException;

    // returns: 0, DB_KEYEXIST, or throws error
    public native int put(Dbt key, Dbt data, int flags)
         throws DbException;

    protected native void finalize()
         throws Throwable;

    // get/set methods
    //

    // private data
    //
    private long private_dbobj_ = 0;

    static {
        Db.load_db();
    }
}

// end of Dbc.java
