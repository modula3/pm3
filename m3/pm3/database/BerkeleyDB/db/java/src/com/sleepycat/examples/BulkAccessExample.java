/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2002
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id$
 */

package com.sleepycat.examples;

import com.sleepycat.db.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.PrintStream;

class BulkAccessExample
{
    private static final String FileName = "access.db";

    public BulkAccessExample()
    {
    }

    public static void main(String argv[])
    {
        try
        {
            BulkAccessExample app = new BulkAccessExample();
            app.run();
        }
        catch (DbException dbe)
        {
            System.err.println("BulkAccessExample: " + dbe.toString());
            System.exit(1);
        }
        catch (FileNotFoundException fnfe)
        {
            System.err.println("BulkAccessExample: " + fnfe.toString());
            System.exit(1);
        }
        System.exit(0);
    }

    // Prompts for a line, and keeps prompting until a non blank
    // line is returned.  Returns null on error.
    //
    static public String askForLine(InputStreamReader reader,
                                    PrintStream out, String prompt)
    {
        String result = "";
        while (result != null && result.length() == 0) {
            out.print(prompt);
            out.flush();
            result = getLine(reader);
        }
        return result;
    }

    // Not terribly efficient, but does the job.
    // Works for reading a line from stdin or a file.
    // Returns null on EOF.  If EOF appears in the middle
    // of a line, returns that line, then null on next call.
    //
    static public String getLine(InputStreamReader reader)
    {
        StringBuffer b = new StringBuffer();
        int c;
        try {
            while ((c = reader.read()) != -1 && c != '\n') {
                if (c != '\r')
                    b.append((char)c);
            }
        }
        catch (IOException ioe) {
            c = -1;
        }

        if (c == -1 && b.length() == 0)
            return null;
        else
            return b.toString();
    }

    public void run()
         throws DbException, FileNotFoundException
    {
        // Remove the previous database.
        new File(FileName).delete();

        // Create the database object.
        // There is no environment for this simple example.
        Db table = new Db(null, 0);
        table.set_error_stream(System.err);
        table.set_errpfx("BulkAccessExample");
        table.open(null, FileName, null, Db.DB_BTREE, Db.DB_CREATE, 0644);

        //
        // Insert records into the database, where the key is the user
        // input and the data is the user input in reverse order.
        //
        InputStreamReader reader = new InputStreamReader(System.in);

        for (;;) {
            String line = askForLine(reader, System.out, "input> ");
            if (line == null)
                break;

            String reversed = (new StringBuffer(line)).reverse().toString();

            // See definition of StringDbt below
            //
            StringDbt key = new StringDbt(line);
            StringDbt data = new StringDbt(reversed);

            try
            {
                int err;
                if ((err = table.put(null,
                    key, data, Db.DB_NOOVERWRITE)) == Db.DB_KEYEXIST) {
                        System.out.println("Key " + line + " already exists.");
                }
            }
            catch (DbException dbe)
            {
                System.out.println(dbe.toString());
            }
            System.out.println("");
        }

        // Acquire a cursor for the table and two Dbts.
        Dbc dbc = table.cursor(null, 0);
        Dbt foo = new Dbt();
        foo.set_flags(Db.DB_DBT_MALLOC);

        Dbt bulk_data = new Dbt();

        // Set Db.DB_DBT_USERMEM on the data Dbt;  Db.DB_MULTIPLE_KEY requires
        // it.  Then allocate a byte array of a reasonable size;  we'll
        // go through the database in chunks this big.
        bulk_data.set_flags(Db.DB_DBT_USERMEM);
        bulk_data.set_data(new byte[1000000]);
        bulk_data.set_ulen(1000000);


        // Walk through the table, printing the key/data pairs.
        //
        while (dbc.get(foo, bulk_data, Db.DB_NEXT | Db.DB_MULTIPLE_KEY) == 0)
        {
            DbMultipleKeyDataIterator iterator;
            iterator = new DbMultipleKeyDataIterator(bulk_data);

            StringDbt key, data;
            key = new StringDbt();
            data = new StringDbt();

            while (iterator.next(key, data)) {
                System.out.println(key.getString() + " : " + data.getString());
            }
        }
        dbc.close();
        table.close(0);
    }

    // Here's an example of how you can extend a Dbt in a straightforward
    // way to allow easy storage/retrieval of strings, or whatever
    // kind of data you wish.  We've declared it as a static inner
    // class, but it need not be.
    //
    static /*inner*/
    class StringDbt extends Dbt
    {
        StringDbt()
        {
            set_flags(Db.DB_DBT_MALLOC); // tell Db to allocate on retrieval
        }

        StringDbt(String value)
        {
            setString(value);
            set_flags(Db.DB_DBT_MALLOC); // tell Db to allocate on retrieval
        }

        void setString(String value)
        {
            byte[] data = value.getBytes();
            set_data(data);
            set_size(data.length);
        }

        String getString()
        {
            return new String(get_data(), get_offset(), get_size());
        }
    }
}
