/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000-2002
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id$
 */

/*
 * Do some regression tests for simple get/set access methods
 * on DbEnv, DbTxn, Db.  We don't currently test that they have
 * the desired effect, only that they operate and return correctly.
 */
package com.sleepycat.test;

import com.sleepycat.db.*;
import java.io.FileNotFoundException;

public class TestGetSetMethods
{
    public void testMethods()
        throws DbException, FileNotFoundException
    {
        DbEnv dbenv = new DbEnv(0);
        DbTxn dbtxn;
        byte[][] conflicts = new byte[10][10];

        dbenv.set_timeout(0x90000000,
                          Db.DB_SET_LOCK_TIMEOUT);
        dbenv.set_lg_bsize(0x1000);
        dbenv.set_lg_dir(".");
        dbenv.set_lg_max(0x10000000);
        dbenv.set_lg_regionmax(0x100000);
        dbenv.set_lk_conflicts(conflicts);
        dbenv.set_lk_detect(Db.DB_LOCK_DEFAULT);
        // exists, but is deprecated:
        // dbenv.set_lk_max(0);
        dbenv.set_lk_max_lockers(100);
        dbenv.set_lk_max_locks(10);
        dbenv.set_lk_max_objects(1000);
        dbenv.set_mp_mmapsize(0x10000);
        dbenv.set_tas_spins(1000);

        // Need to open the environment so we
        // can get a transaction.
        //
        dbenv.open(".", Db.DB_CREATE | Db.DB_INIT_TXN |
                   Db.DB_INIT_LOCK | Db.DB_INIT_LOG |
                   Db.DB_INIT_MPOOL,
                   0644);

        dbtxn = dbenv.txn_begin(null, Db.DB_TXN_NOWAIT);
        dbtxn.set_timeout(0xA0000000, Db.DB_SET_TXN_TIMEOUT);
        dbtxn.abort();

        dbenv.close(0);

        // We get a db, one for each type.
        // That's because once we call (for instance)
        // set_bt_maxkey, DB 'knows' that this is a
        // Btree Db, and it cannot be used to try Hash
        // or Recno functions.
        //
        Db db_bt = new Db(null, 0);
        db_bt.set_bt_maxkey(10000);
        db_bt.set_bt_minkey(100);
        db_bt.set_cachesize(0, 0x100000, 0);
        db_bt.close(0);

        Db db_h = new Db(null, 0);
        db_h.set_h_ffactor(0x10);
        db_h.set_h_nelem(100);
        db_h.set_lorder(0);
        db_h.set_pagesize(0x10000);
        db_h.close(0);

        Db db_re = new Db(null, 0);
        db_re.set_re_delim('@');
        db_re.set_re_pad(10);
        db_re.set_re_source("re.in");
        db_re.close(0);

        Db db_q = new Db(null, 0);
        db_q.set_q_extentsize(200);
        db_q.close(0);
    }

    public static void main(String[] args)
    {
        try {
            TestGetSetMethods tester = new TestGetSetMethods();
            tester.testMethods();
        }
        catch (Exception e) {
            System.err.println("TestGetSetMethods: Exception: " + e);
        }
    }
}
