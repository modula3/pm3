/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2002
 *      Sleepycat Software.  All rights reserved.
 *
 * $Id$
 */

package com.sleepycat.db;
import java.io.OutputStream;
import java.io.IOException;

/**
 *
 * @author Donald D. Anderson
 *
 * This class is not public, as it is only used internally
 * by Db to implement a default error handler.
 */

/*package*/ class DbOutputStreamErrcall implements DbErrcall
{
    DbOutputStreamErrcall(OutputStream stream)
    {
        this.stream_ = stream;
    }

    // errcall implements DbErrcall
    //
    public void errcall(String prefix, String buffer)
    {
        try {
            if (prefix != null) {
                stream_.write(prefix.getBytes());
                stream_.write((new String(": ")).getBytes());
            }
            stream_.write(buffer.getBytes());
            stream_.write((new String("\n")).getBytes());
        }
        catch (IOException e) {

            // well, we tried.
            // Do our best to report the problem by other means.
            //
            System.err.println("DbOutputStreamErrcall Exception: " + e);
            if (prefix != null)
                System.err.print(prefix + ": ");
            System.err.println(buffer + "\n");
        }
    }

    // private data
    //
    private OutputStream stream_;
}

// end of DbOutputStreamErrcall.java
