INTERFACE BrowserConfiguration;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/06/06 14:24:36  roland
    Some runtime configuration support added. A file-name can be given as
    command line parameter with '-config'. From this file the collection
    name and representation types for names attributes will be read.

*)
(***************************************************************************)

IMPORT Rd, Wr;

TYPE AttributeFormat = {NonReadableText, ReadableText, Integer, Cardinal};

PROCEDURE ReadConfiguration(rd: Rd.T);
PROCEDURE WriteConfiguration(wr: Wr.T);

PROCEDURE GetCollectionName(): TEXT;
PROCEDURE SetCollectionName(name: TEXT);

PROCEDURE SetAttributeFormat(attr: TEXT; format: AttributeFormat);
PROCEDURE GetAttributeFormat(attr: TEXT): AttributeFormat;
PROCEDURE FormatAttribute(attr, val: TEXT): TEXT;
  
END BrowserConfiguration.
