(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Wed Aug 21 09:05:34 PDT 1996 by steveg *)

MODULE HTTP;

IMPORT
  App, Atom, Date, FastLex, FloatMode, Fmt, HTTPApp, Lex, Rd, RdCopy,
  Text, CIText, TextRd, TextWr, Thread, Time, UnsafeRd, Word, Wr;

VAR
  fieldMu: MUTEX := NEW(MUTEX);

VAR
  programInfo := ProgramInfo{ProgramType.Tunnel, "", AuthType.None, "", ""};
  defaultViaFieldValue: TEXT;

PROCEDURE VersionGE (READONLY v1, v2: Version): BOOLEAN =
  BEGIN
    IF v1.major > v2.major
         OR (v1.major = v2.major AND v1.minor >= v2.minor) THEN
      RETURN TRUE
    END;
    RETURN FALSE;
  END VersionGE;

PROCEDURE DefaultStyle(READONLY version: Version := CurrentVersion): Style =
  VAR
    res := NEW(Style);
  BEGIN
    res.version := version;
    res.absoluteURLs := FALSE;
    res.viaFieldValue := defaultViaFieldValue;
    IF VersionGE(version, Version1_0) THEN
      res.wrappedLines := TRUE;
    ELSE
      res.wrappedLines := FALSE;
    END;
    RETURN res;
  END DefaultStyle;

(* The "http0_9" is needed because we can't tell the start of an HTTP/0.9
   reply from a "real" reply until we have already read the characters.
   We will assume that any reply starting "HTTP/" is an HTTP/1.0 
   or later request.  If the result is "Version0_9", then the value of
   "http0_9" is the length of the substring of "HTTP/" that was found
   before it became apparent that it is an HTTP 0.9 reply.
*)
PROCEDURE ParseVersion (              rd     : Rd.T;
                        VAR (* OUT *) http0_9: [0 .. 4];
                                      log    : App.Log   ): Version
  RAISES {App.Error} =
  VAR res: Version;
  BEGIN
    TRY
      FOR i := 0 TO LAST(HTTPSlash) DO
        IF Rd.GetChar(rd) # HTTPSlash[i] THEN
          Rd.UnGetChar(rd);
          http0_9 := i;
          RETURN Version0_9;
        END;
      END;
      res.major := Lex.Int(rd);
      IF Rd.GetChar(rd) # '.' THEN
        log.log("Bad version string (missing '.')", App.LogStatus.Error);
      END;
      res.minor := Lex.Int(rd);
    EXCEPT
    | Rd.EndOfFile, Rd.Failure, Thread.Alerted, FloatMode.Trap,
          Lex.Error =>
        log.log("Bad version string", App.LogStatus.Error);
    END;
    RETURN res;
  END ParseVersion;

PROCEDURE WriteVersion(wr: Wr.T; READONLY version: Version) 
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    Wr.PutText(wr, "HTTP/");
    Wr.PutText(wr, Fmt.Int(version.major));
    Wr.PutChar(wr, '.');
    Wr.PutText(wr, Fmt.Int(version.minor));
  END WriteVersion;

PROCEDURE GetRestOfLine (rd: Rd.T; style: Style): TEXT
  RAISES {Rd.Failure, Rd.EndOfFile, Thread.Alerted} =
  VAR
    res: TEXT;
    ch : CHAR;
  BEGIN
    IF UnsafeRd.FastEOF(rd) THEN
      RETURN "";
    ELSE
      res := Rd.GetLine(rd);
      IF style.wrappedLines THEN
        LOOP
          ch := Rd.GetChar(rd);
          Rd.UnGetChar(rd);
          IF NOT ch IN Spaces THEN EXIT END;
          res := res & Rd.GetLine(rd);
        END;
      END;
      RETURN res;
    END;
  END GetRestOfLine;

REVEAL
  Field = FieldPublic BRANDED OBJECT
  OVERRIDES
    init := InitField;
  END;

PROCEDURE InitField(self: Field; name, value: TEXT): Field =
  BEGIN
    self.name := name;
    self.value := value;
    RETURN self;
  END InitField;

TYPE
  FieldList = REF RECORD
    field: Field;
    tail: FieldList; (* protected by fieldMu *)
  END;

REVEAL
  Header = HeaderPublic BRANDED OBJECT
             fields: FieldList;  (* protected by fieldMu *)
           OVERRIDES
             lookupField   := LookupHeaderField;
             addField      := AddHeaderField;
             removeField   := RemoveHeaderField;
             iterateFields := IterateHeaderFields;
             copyFields    := CopyHeaderFields;
           END;

PROCEDURE MatchField(field1, field2: Field): BOOLEAN =
  BEGIN
    RETURN field1.name = field2.name AND (field2.value = NIL OR
                              Text.Equal(field1.value, field2.value)); 
  END MatchField;

PROCEDURE LookupHeaderField(self: Header; 
                            name, value: TEXT): Field =
  VAR
    fields: FieldList;
  BEGIN
    LOCK fieldMu DO
      fields := self.fields;
      WHILE fields # NIL DO
        IF CIText.Equal(name, fields.field.name) THEN
          IF value = NIL OR Text.Equal(value, fields.field.value) THEN
            RETURN fields.field
          END;
        END;
        fields := fields.tail;
      END;
    END;
    RETURN NIL;
  END LookupHeaderField;

PROCEDURE AddHeaderField(self: Header; field, after: Field): Field =
  VAR
    fields: FieldList;
  BEGIN
    LOCK fieldMu DO
      fields := self.fields;
      IF after = NIL THEN
        self.fields := NEW(FieldList, field := field, tail := fields);
      ELSE
        WHILE fields # NIL AND fields.field # after DO
          fields := fields.tail
        END;
        IF fields = NIL THEN
          self.fields := NEW(FieldList, field := field, tail := self.fields);
        ELSE
          fields.tail := NEW(FieldList, field := field, tail := fields.tail);
        END;
      END;
    END;
    RETURN field;
  END AddHeaderField;

PROCEDURE RemoveHeaderField(self: Header; field: Field): BOOLEAN =
  VAR
    fields: FieldList;
    prev: FieldList := NIL;
  BEGIN
    LOCK fieldMu DO
      fields := self.fields;
      WHILE fields # NIL AND NOT MatchField(fields.field, field) DO
        prev := fields;
        fields := fields.tail;
      END;
      IF fields = NIL THEN
        RETURN FALSE;
      ELSE
        IF prev = NIL THEN
          self.fields := self.fields.tail;
        ELSE
          prev.tail := fields.tail;
        END;
        RETURN TRUE;
      END;
    END;
  END RemoveHeaderField;

REVEAL
  FieldIterator = FieldIteratorPublic BRANDED OBJECT
    list: FieldList;
  OVERRIDES
    next := NextFieldIterator;
  END;

PROCEDURE IterateHeaderFields(self: Header): FieldIterator =
  BEGIN
    LOCK fieldMu DO
      RETURN NEW(FieldIterator, list := self.fields);
    END;
  END IterateHeaderFields;

PROCEDURE NextFieldIterator(self: FieldIterator): Field =
  VAR 
    res: Field;
  BEGIN
    LOCK fieldMu DO
      IF self.list = NIL THEN RETURN NIL END;
      res := self.list.field;
      self.list := self.list.tail;
    END;
    RETURN res;
  END NextFieldIterator;

PROCEDURE CopyHeaderFields (self, to: Header) =
  VAR fields: FieldList;
  BEGIN
    LOCK fieldMu DO
      fields := self.fields;
      WHILE fields # NIL DO
        to.fields :=
          NEW(FieldList, field := fields.field, tail := to.fields);
        fields := fields.tail;
      END;
    END;
  END CopyHeaderFields;

REVEAL
  Request = RequestPublic BRANDED OBJECT
  OVERRIDES
    parse := ParseRequest;
    write := WriteRequest;
    toText := RequestToText;
  END;

REVEAL
  Reply = ReplyPublic BRANDED OBJECT
  OVERRIDES
    parse := ParseReply;
    write := WriteReply;
    toText := ReplyToText;
  END;

VAR (* CONST *)
  Methods: ARRAY Method OF Atom.T;

PROCEDURE LookupMethod (nameTxt: TEXT; log: App.Log): Method
  RAISES {App.Error} =
  VAR name := Atom.FromText(nameTxt);
  BEGIN
    FOR i := FIRST(Method) TO LAST(Method) DO
      IF Methods[i] = name THEN RETURN i END;
    END;
    log.log(
      Fmt.F("Unknown method found: %s", nameTxt), App.LogStatus.Error);
    <* ASSERT FALSE *>
  END LookupMethod;

CONST 
  Spaces = Lex.Blanks - SET OF CHAR {'\r', '\n'};

PROCEDURE ParseHeaderFields (self        : Header;
                             rd          : Rd.T;
                             style       : Style;
                             log         : App.Log  ) RAISES {App.Error} =
  VAR
    field, prev: Field   := NIL;
    name, value: TEXT;
    cur        : INTEGER;
    ch         : CHAR;
  BEGIN
    TRY
      LOOP
        cur := Rd.Index(rd);
        name := FastLex.Scan(rd, Token);
        IF Rd.Index(rd) = cur THEN
          ch := UnsafeRd.FastGetChar(rd);
          IF ch = '\n' OR (ch = '\r' AND UnsafeRd.FastGetChar(rd) = '\n') THEN
            (* empty line *)
            EXIT
          ELSE
            log.log(
              Fmt.F("Missing field name in header line: %s",
                    GetRestOfLine(rd, style)), App.LogStatus.Error);
          END;
        END;
        IF UnsafeRd.FastGetChar(rd) = ':' THEN
          FastLex.Skip(rd, Spaces);
          value := GetRestOfLine(rd, style);
          field := NEW(Field).init(name, value);
          prev := self.addField(field, prev);
        ELSE
          log.log(Fmt.F("Read failure parsing header line: %s", name),
                  App.LogStatus.Status);
          EXIT;
        END;
      END;
    EXCEPT
    | Rd.EndOfFile, Rd.Failure, Thread.Alerted =>
        log.log(
          Fmt.F("Read failure parsing header line: %s:%s", name, value),
          App.LogStatus.Error);
    END;
  END ParseHeaderFields;

PROCEDURE ParseRequest (self: Request; rd: Rd.T; log: App.Log): Request
  RAISES {App.Error} =
  VAR
    ch : CHAR;
    foo: [0 .. 4];
  BEGIN
    TRY
      self.method := LookupMethod(FastLex.Scan(rd), log);
      FastLex.Skip(rd, Spaces);
      self.url := NEW(URL).init(FastLex.Scan(rd), log);
      IF UnsafeRd.FastEOF(rd) THEN
        self.version := Version0_9
      ELSE
        FastLex.Skip(rd);
        self.version := ParseVersion(rd, foo, log);
      END;
      ch := UnsafeRd.FastGetChar(rd);
      IF NOT (ch = '\n' OR (ch = '\r' AND UnsafeRd.FastGetChar(rd) = '\n')) THEN
        log.log("Bad request header", App.LogStatus.Error);
      END;
      ParseHeaderFields(
        self, rd, DefaultStyle(self.version), log);
    EXCEPT
    | Rd.EndOfFile, Rd.Failure, Thread.Alerted =>
        log.log("Read failure in ParseRequest", App.LogStatus.Error);
    END;
    RETURN self;
  END ParseRequest;

CONST
  HTTPSlash = ARRAY OF CHAR{'H', 'T', 'T', 'P', '/'};

PROCEDURE ParseReply (self: Reply; rd: Rd.T; log: App.Log): Reply
  RAISES {App.Error} =
  VAR
    version: Version;
    style  : Style;
    http0_9: [0 .. 4];
  BEGIN
    TRY
      self.version := ParseVersion(rd, http0_9, log);
      style := DefaultStyle(version);
      IF VersionGE(self.version, Version1_0) THEN
        FastLex.Skip(rd, Spaces);
        self.code := Lex.Int(rd);
        FastLex.Skip(rd, Spaces);
        self.reason := GetRestOfLine(rd, style);
        ParseHeaderFields(self, rd, style, log);
      ELSE
        self.code := 0;
        self.reason := Text.FromChars(SUBARRAY(HTTPSlash, 0, http0_9));
      END;
    EXCEPT
    | FloatMode.Trap, Lex.Error, Rd.EndOfFile, Rd.Failure,
          Thread.Alerted =>
        log.log("Read failure in ParseReply", App.LogStatus.Error);
    END;
    RETURN self;
  END ParseReply;

PROCEDURE WriteHeaderFields (self: Header; wr: Wr.T; log: App.Log)
  RAISES {App.Error} =
  VAR
    iter  := self.iterateFields();
    field := iter.next();
  BEGIN
    TRY
      WHILE field # NIL DO
        Wr.PutText(wr, field.name);
        Wr.PutText(wr, ": ");
        Wr.PutText(wr, field.value);
        Wr.PutText(wr, "\r\n");
        field := iter.next();
      END;
      Wr.PutText(wr, "\r\n");
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log("Write failure writing header fields", App.LogStatus.Error);
    END;
  END WriteHeaderFields;

CONST
  ProgramTypeField = ARRAY ProgramType OF TEXT{"User-Agent", "Via", "Server", ""};

(* "request" is used for the "Via" field.  It should be the original request
   on the proxy that generates this request or reply *)
PROCEDURE WriteProgramInfo (wr: Wr.T; style: Style; log: App.Log)
  RAISES {App.Error, Wr.Failure, Thread.Alerted} =
  BEGIN
    (* tunnels should be totally transparent *)
    CASE programInfo.type OF
    | ProgramType.Client, ProgramType.Server =>
        Wr.PutText(wr, ProgramTypeField[programInfo.type]);
        Wr.PutText(wr, ": ");
        Wr.PutText(wr, programInfo.name);
        Wr.PutText(wr, "\r\n");
    | ProgramType.Tunnel =>      (* nothing *)
    | ProgramType.Proxy =>
        IF style.viaFieldValue = NIL THEN
          log.log("No \"viaFieldValue\" for Proxy", App.LogStatus.Error);
        END;
        Wr.PutText(wr, "Via: ");
        Wr.PutText(wr, style.viaFieldValue);
        Wr.PutText(wr, "\r\n");
    END;
  END WriteProgramInfo;

PROCEDURE WriteRequest (self        : Request;
                        wr          : Wr.T;
                        style       : Style;
                        proxyRequest: BOOLEAN;
                        log         : App.Log  ) RAISES {App.Error} =
  VAR
    url : TEXT;
  BEGIN
    IF proxyRequest THEN
      url := self.url.toText(URLFormat.Canonical);
    ELSE
      url := self.url.toText(URLFormat.BodyOnly);
    END;
    IF style = NIL THEN style := DefaultStyle() END;
    TRY
      Wr.PutText(wr, MethodText[self.method]);
      Wr.PutChar(wr, ' ');
      Wr.PutText(wr, url);
      Wr.PutChar(wr, ' ');
      WriteVersion(wr, self.version);
      Wr.PutText(wr, "\r\n");
      IF self.lookupField(FieldName[FieldType.Host]) = NIL THEN
        EVAL
          self.addField(
            NEW(Field).init(FieldName[FieldType.Host], self.url.host));
      END;
      WriteProgramInfo(wr, style, log);
      WriteHeaderFields(self, wr, log);
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log("Write failure in WriteRequest", App.LogStatus.Error);
    END;
  END WriteRequest;

PROCEDURE WriteSimpleReplyHeader (wr     : Wr.T;
                                  style  : Style;
                                  log    : App.Log;
                                  code: INTEGER := StatusCode[
                                                     StatusType.OK];
                                  reason: TEXT := StatusReason[
                                                    StatusType.OK])
  RAISES {App.Error} =
  BEGIN
    IF style = NIL THEN style := DefaultStyle() END;
    TRY
      WriteVersion(wr, style.version);
      Wr.PutChar(wr, ' ');
      IF code = 200 THEN
        Wr.PutText(wr, "200");
      ELSE
        Wr.PutText(wr, Fmt.Int(code));
      END;
      Wr.PutChar(wr, ' ');
      Wr.PutText(wr, reason);
      Wr.PutText(wr, "\r\n");
      WriteProgramInfo(wr, style, log);
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log("WriteM failure in WriteRequest", App.LogStatus.Error);
    END;
  END WriteSimpleReplyHeader;

PROCEDURE WriteReply (self: Reply; wr: Wr.T; style: Style; log: App.Log)
  RAISES {App.Error} =
  BEGIN
    IF style = NIL THEN style := DefaultStyle() END;
    TRY
      IF self.version = Version0_9 THEN
        Wr.PutText(wr, self.reason);
      ELSE
        WriteSimpleReplyHeader(
          wr, style, log, self.code, self.reason);
        WriteHeaderFields(self, wr, log);
      END;
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log("Write failure in WriteRequest", App.LogStatus.Error);
    END;
  END WriteReply;

PROCEDURE RequestToText(self: Request; style: Style; proxy: BOOLEAN;
                        log: App.Log): TEXT RAISES {App.Error} =
  VAR
    wr := TextWr.New();
  BEGIN
    self.write(wr, style, proxy, log);
    RETURN TextWr.ToText(wr);
  END RequestToText;

PROCEDURE ReplyToText (self: Reply; style: Style; log: App.Log): TEXT
  RAISES {App.Error} =
  VAR wr := TextWr.New();
  BEGIN
    self.write(wr, style, log);
    RETURN TextWr.ToText(wr);
  END ReplyToText;

REVEAL
  URL = URLPublic BRANDED OBJECT
  OVERRIDES
    init := InitURL;
    initFromRd := InitURLFromRd;
    toText := URLToText;
    equivalent := URLEquivalent;
    local := URLLocal;
    derelativize := URLDerelativize;
  END;

PROCEDURE URLDerelativize (self, base: URL): URL =
  VAR
    res   : URL;
    iSlash: INTEGER;
  BEGIN
    IF Text.Length(self.host) = 0 THEN
      RETURN self;
    ELSE
      res := NEW(URL);
      res.url := NIL;
      res.scheme := self.scheme;
      res.host := base.host;
      res.port := base.port;
      IF self.absPath THEN
        res.path := self.path;
      ELSE
        iSlash := Text.FindCharR(base.path, '/');
        IF iSlash = -1 THEN
          res.path := self.path;
        ELSE
          res.path := Text.Sub(base.path, 0, iSlash + 1) & self.path;
        END;
      END;
      res.absPath := TRUE;
      res.params := self.params;
      res.query := self.query;
      res.fragment := self.fragment;
      RETURN res;
    END;
  END URLDerelativize;

PROCEDURE URLLocal (self: URL): BOOLEAN =
  BEGIN
    RETURN Text.Length(self.host) = 0
             OR HTTPApp.ServerPort(self.port)
                  AND (Text.Equal(self.host, App.GetHostName(FALSE))
                         OR Text.Equal(self.host, App.GetHostName(TRUE)));
  END URLLocal;

PROCEDURE URLEquivalent (self, other: URL): BOOLEAN =
  BEGIN
    TRY
      RETURN
        self.absPath = other.absPath
          AND Text.Equal(self.scheme, other.scheme)
          AND Text.Equal(self.host, other.host) AND self.port = other.port
          AND Text.Equal(self.path, other.path)
          AND Text.Equal(self.params, other.params)
          AND Text.Equal(UnescapeURLEntry(self.query, App.nullLog),
                         UnescapeURLEntry(other.query, App.nullLog));
    EXCEPT
    | App.Error => RETURN FALSE;
    END;
  END URLEquivalent;

CONST
  NotColonOrSlash = SET OF CHAR{'\000'.. '\377'} - SET OF CHAR{':', '/'};
  NotSemiQuestionPoundOrBlanks = SET OF CHAR{'\000'.. '\377'}
                                   - SET OF CHAR{';', '?', '#'}
                                   - Lex.Blanks;
  NotQuestionPoundOrBlanks = SET OF CHAR{'\000'.. '\377'}
                               - SET OF CHAR{'?', '#'} - Lex.Blanks;
  NotPoundOrBlanks = SET OF CHAR{'\000'.. '\377'} - SET OF CHAR{'#'}
                       - Lex.Blanks;
  NotBlanks = SET OF CHAR{'\000'.. '\377'} - Lex.Blanks;

PROCEDURE ProtocolToPort(protocol: TEXT; log: App.Log): INTEGER RAISES {App.Error} =
  BEGIN
    IF Text.Equal(protocol, "http") THEN
      RETURN 80;
    ELSIF Text.Equal(protocol, "gopher") THEN
      RETURN 70;
    ELSIF Text.Equal(protocol, "https") THEN
      RETURN 443;
    END;
    log.log(Fmt.F("Unknown protocol: %s", protocol), App.LogStatus.Error);
    RETURN 0;
  END ProtocolToPort;

PROCEDURE InitURL(self: URL; url: TEXT; log: App.Log): URL RAISES {App.Error} =
  VAR
    trd: TextRd.T := TextRd.New(url);
  BEGIN
    self.url := url;
    RETURN InitURLFromRd1(self, trd, log);
  END InitURL;

(* New plan has us reading the URL into a text and then scanning that so
   that we have the initial text of the URL *)
PROCEDURE InitURLFromRd (self: URL; rd: Rd.T; log: App.Log): URL
  RAISES {App.Error} =
  VAR index := Rd.Index(rd);
  BEGIN
    TRY
      RETURN InitURL(self, Lex.Scan(rd), log);
    EXCEPT
    | Rd.Failure, Thread.Alerted =>
        TRY
          IF Rd.Seekable(rd) THEN Rd.Seek(rd, index) END;
          log.log(
            Fmt.F("Bad URL: %s", FastLex.Scan(rd)), App.LogStatus.Error);
        EXCEPT
          Rd.Failure, Thread.Alerted => 
        END;
    END;
    RETURN NIL;
  END InitURLFromRd;

PROCEDURE Lowercase (t: TEXT): TEXT =
  VAR
    rd := TextRd.New(t);
    wr := TextWr.New();
    ch: CHAR;
  <* FATAL Rd.Failure, Rd.EndOfFile, Thread.Alerted, Wr.Failure *>
  BEGIN
    WHILE NOT Rd.EOF(rd) DO
      ch := Rd.GetChar(rd);
      IF 'A' <= ch AND ch <= 'Z' THEN
        ch := VAL(ORD('a') + ORD(ch) - ORD('A'), CHAR);
      END;
      Wr.PutChar(wr, ch);
    END;
    RETURN TextWr.ToText(wr);
  END Lowercase;

PROCEDURE InitURLFromRd1 (self: URL; rd: Rd.T; log: App.Log): URL
  RAISES {App.Error} =
  VAR
    ch   : CHAR;
    index          := Rd.Index(rd);
  BEGIN
    self.scheme := "http";
    self.host := "";
    self.port := 80;
    self.absPath := FALSE;
    self.path := "";
    self.params := "";
    self.query := "";
    self.fragment := "";
    TRY
      self.scheme := Lowercase(Lex.Scan(rd, NotColonOrSlash));
      IF Text.Length(self.scheme) = 0 THEN self.scheme := "http" END;
      ch := Rd.GetChar(rd);
      IF ch = ':' THEN ch := Rd.GetChar(rd) END;
      IF ch = '/' THEN
        self.absPath := TRUE;
        ch := Rd.GetChar(rd);
        IF ch = '/' THEN
          (* ...[//host[:port]]... *)
          (* host[:port] *)
          self.host := Lowercase(Lex.Scan(rd, NotColonOrSlash));
          ch := Rd.GetChar(rd);
          IF ch = ':' THEN
            self.port := Lex.Int(rd)
          ELSE
            Rd.UnGetChar(rd);
          END;
        ELSE
          self.port := ProtocolToPort(self.scheme, log);
          Rd.UnGetChar(rd);
        END;
      END;
      (* ...[[/]path] ... *)
      ch := Rd.GetChar(rd);
      IF ch = '/' THEN self.absPath := TRUE; ELSE Rd.UnGetChar(rd); END;
      (* ...[path]... *)
      self.path :=
        UnescapeURLEntry(Lex.Scan(rd, NotSemiQuestionPoundOrBlanks), log);
      IF Text.Length(self.path) = 0 THEN self.absPath := TRUE; END;
      (* ...[;params]... *)
      ch := Rd.GetChar(rd);
      IF ch = ';' THEN
        self.params :=
          UnescapeURLEntry(Lex.Scan(rd, NotQuestionPoundOrBlanks), log);
      ELSE
        Rd.UnGetChar(rd);
      END;
      (* ...[?query]... *)
      ch := Rd.GetChar(rd);
      IF ch = '?' THEN
        self.query := Lex.Scan(rd, NotPoundOrBlanks);
      ELSE
        Rd.UnGetChar(rd);
      END;
      (* ...[#fragment]... *)
      ch := Rd.GetChar(rd);
      IF ch = '#' THEN
        self.params := UnescapeURLEntry(Lex.Scan(rd, NotBlanks), log);
      ELSE
        Rd.UnGetChar(rd);
      END;
    EXCEPT
    | Rd.EndOfFile =>            (* done *)
    | FloatMode.Trap, Lex.Error, Rd.Failure, Thread.Alerted =>
        TRY
          IF Rd.Seekable(rd) THEN Rd.Seek(rd, index) END;
          log.log(
            Fmt.F("Bad URL: %s", FastLex.Scan(rd)), App.LogStatus.Error);
        EXCEPT
          Rd.Failure, Thread.Alerted =>
        END;
    END;
    RETURN self;
  END InitURLFromRd1;

PROCEDURE URLToText (self: URL; format: URLFormat): TEXT =
  VAR wr := TextWr.New();
  <* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    IF format = URLFormat.Default AND self.url # NIL THEN
      RETURN self.url;
    END;
    IF format # URLFormat.BodyOnly THEN
      Wr.PutText(wr, self.scheme);
      Wr.PutText(wr, "://");
      Wr.PutText(wr, self.host);
      Wr.PutChar(wr, ':');
      Wr.PutText(wr, Fmt.Int(self.port));
    END;
    IF self.absPath THEN
      Wr.PutChar(wr, '/');
    END;
    Wr.PutText(wr, EscapeURLEntry(self.path));
    IF Text.Length(self.params) # 0 THEN
      Wr.PutChar(wr, ';');
      Wr.PutText(wr, EscapeURLEntry(self.params));
    END;
    IF Text.Length(self.query) # 0 THEN
      Wr.PutChar(wr, '?');
      Wr.PutText(wr, self.query);
    END;
    IF Text.Length(self.fragment) # 0 THEN
      Wr.PutChar(wr, '#');
      Wr.PutText(wr, EscapeURLEntry(self.fragment));
    END;
    RETURN TextWr.ToText(wr);
  END URLToText;

CONST
  UnescapedCharacters = SET OF CHAR{'\040'..'\176'} - 
                        SET OF CHAR{' ', '<', '>', '"', '#', '%'} -
                        SET OF CHAR{'{', '}', '|', '\\', '^', '~', '[', ']', '`'};
  (* see http://ds.internic.net/rfc/rfc1738.txt for a description of escaping... *)

  Hex = ARRAY [0..15] OF CHAR{'0', '1', '2', '3', '4', '5', '6', '7', 
                              '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};

PROCEDURE HexToInt(ch: CHAR): INTEGER RAISES {App.Error} =
  BEGIN
    IF ('0' <= ch AND ch <= '9') THEN
      RETURN ORD(ch) - ORD('0');
    ELSIF ('A' <= ch AND ch <= 'F') THEN
      RETURN ORD(ch) - ORD('A') + 10;
    ELSIF ('a' <= ch AND ch <= 'f') THEN
      RETURN ORD(ch) - ORD('a') + 10;
    ELSE
      RAISE App.Error(NIL);
    END;
  END HexToInt;

PROCEDURE EscapeURLEntry(body: TEXT): TEXT =
  VAR
    trd := TextRd.New(body);
    twr := TextWr.New();
    ch: CHAR;
    <* FATAL Wr.Failure, Rd.Failure, Rd.EndOfFile, Thread.Alerted *>
  BEGIN
    WHILE NOT UnsafeRd.FastEOF(trd) DO
      ch := UnsafeRd.FastGetChar(trd);
      IF ch = ' ' THEN
        Wr.PutChar(twr, '+');
      ELSIF ch IN UnescapedCharacters THEN
        Wr.PutChar(twr, ch);
      ELSE
        Wr.PutChar(twr, '%');
        Wr.PutChar(twr, Hex[ORD(ch) DIV 16]);
        Wr.PutChar(twr, Hex[ORD(ch) MOD 16]);
      END;
    END;
    RETURN TextWr.ToText(twr);
  END EscapeURLEntry;

PROCEDURE UnescapeURLEntry(body: TEXT; log: App.Log): TEXT RAISES {App.Error} =
  VAR
    trd := TextRd.New(body);
    twr := TextWr.New();
    ch: CHAR;
    <* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    TRY
      WHILE NOT UnsafeRd.FastEOF(trd) DO
        ch := UnsafeRd.FastGetChar(trd);
        IF ch = '%' THEN
          Wr.PutChar(twr, VAL(HexToInt(UnsafeRd.FastGetChar(trd)) * 16 + 
                              HexToInt(UnsafeRd.FastGetChar(trd)), CHAR));
        ELSIF ch = '+' THEN
          Wr.PutChar(twr, ' ');
        ELSE
          Wr.PutChar(twr, ch);
        END;
      END;
    EXCEPT
    | App.Error, Rd.Failure, Rd.EndOfFile =>
        log.log(Fmt.F("Badly escaped URL body: %s", body), App.LogStatus.Error);
    END;
    RETURN TextWr.ToText(twr);
  END UnescapeURLEntry;

PROCEDURE EncodeTextForHTML(text: TEXT): TEXT =
  VAR
    in := TextRd.New(text);
    out := TextWr.New();
    ch: CHAR;
  BEGIN
    TRY
      LOOP
        ch := UnsafeRd.FastGetChar(in);
        CASE ch OF
        | '"' => Wr.PutText(out,  "&quot;");
        | '<' => Wr.PutText(out,  "&lt;");
        | '>' => Wr.PutText(out,  "&gt;");
        | '&' => Wr.PutText(out,  "&amp;");
        ELSE
          Wr.PutChar(out, ch);
        END;
      END;
    EXCEPT
    | Rd.EndOfFile, Rd.Failure, Wr.Failure, Thread.Alerted =>
    END;
    RETURN TextWr.ToText(out);
  END EncodeTextForHTML;

CONST
  Encodings = SET OF CHAR{'a' .. 'z'};

PROCEDURE DecodeTextForHTML(text: TEXT; log: App.Log): TEXT RAISES {App.Error} =
  VAR
    in := TextRd.New(text);
    out := TextWr.New();
    ch: CHAR;
    sym: TEXT;
  BEGIN
    TRY
      LOOP
        ch := UnsafeRd.FastGetChar(in);
        IF ch = '&' THEN
          sym := FastLex.Scan(in, Encodings);
          ch := UnsafeRd.FastGetChar(in);
          IF ch = ';' THEN
            IF Text.Equal(sym, "amp") THEN
              Wr.PutChar(out, '&');
            ELSIF Text.Equal(sym, "gt") THEN
              Wr.PutChar(out, '>');
            ELSIF Text.Equal(sym, "lt") THEN
              Wr.PutChar(out, '<');
            ELSIF Text.Equal(sym, "quot") THEN
              Wr.PutChar(out, '"');
            ELSE
              IF App.Verbose() THEN
                log.log(Fmt.F("Bad HTML encoding in %s", text),
                      App.LogStatus.Verbose);
              END; 
              Wr.PutChar(out, '&');
              Wr.PutText(out, sym);
              Wr.PutChar(out, ';');
            END;
          ELSE
            Wr.PutChar(out, '&');
            Wr.PutText(out, sym);
            Wr.PutChar(out, ch);
          END;
        ELSE
          Wr.PutChar(out, ch);
        END;
      END;
    EXCEPT
    | Rd.EndOfFile, Rd.Failure, Wr.Failure, Thread.Alerted =>
    END;
    RETURN TextWr.ToText(out);
  END DecodeTextForHTML;

PROCEDURE ReadItem(rd: Rd.T; stop: CHAR; wr: TextWr.T): TEXT 
  RAISES {Rd.Failure, Thread.Alerted, App.Error} =
  VAR
    ch: CHAR;
    <* FATAL Wr.Failure *>
  BEGIN
    EVAL wr.init(); (* safe *)
    TRY
      LOOP
        ch := UnsafeRd.FastGetChar(rd);
        IF ch = stop THEN 
          UnsafeRd.FastUnGetChar(rd);
          EXIT;
        ELSIF ch = '%' THEN
          Wr.PutChar(wr, VAL(HexToInt(UnsafeRd.FastGetChar(rd)) * 16 + 
                             HexToInt(UnsafeRd.FastGetChar(rd)), CHAR));
        ELSE Wr.PutChar(wr, ch);
        END;
      END;
    EXCEPT
    | Rd.EndOfFile => 
    END;
    RETURN TextWr.ToText(wr);
  END ReadItem;

REVEAL
  FormQuery = FormQueryPublic BRANDED OBJECT
              OVERRIDES
                init       := InitFormQueryFromURL;
                initFromRd := InitFormQueryFromRd;
                write      := WriteFormQuery;
                toText     := FormQueryToText;
              END;

(* format of the form state is:
   field1=value1
   &field2=value2
   ...
   &fieldn=valuen

   A character sequence "%cc" is interpreted as the ASCII character with
   character "cc" as a base 16 integer.  i.e. %20 => space
 *)

PROCEDURE InitFormQueryFromURL (self: FormQuery; query: TEXT): FormQuery
  RAISES {BadFormQuery} =
  VAR rd := TextRd.New(query);
  BEGIN
    RETURN InitFormQueryFromRd(self, rd);
  END InitFormQueryFromURL;

PROCEDURE InitFormQueryFromRd(self: FormQuery;
                              rd: Rd.T): FormQuery RAISES {BadFormQuery} =
  VAR
    name, value: TEXT := NIL;
    last: Field := NIL;
    twr := NEW(TextWr.T);
  BEGIN
    TRY
      LOOP
        name := ReadItem(rd, '=', twr);
        EVAL UnsafeRd.FastGetChar(rd); (* '=' *)
        value := ReadItem(rd, '&', twr);
        last := self.addField(NEW(Field).init(name, value), last);
        name := NIL;
        IF UnsafeRd.FastEOF(rd) THEN EXIT END;
        EVAL UnsafeRd.FastGetChar(rd); (* '&' *)
      END;
    EXCEPT
    | Rd.EndOfFile => 
        (* partial entry? *)
        IF name # NIL AND Text.Length(name) # 0 THEN
          RAISE BadFormQuery;
        END;
        (* done *)
    | Rd.Failure, Thread.Alerted, App.Error =>
        RAISE BadFormQuery;
    END;
    RETURN self;
  END InitFormQueryFromRd;

PROCEDURE WriteFormQuery(self: FormQuery; 
                         wr: Wr.T; log: App.Log) RAISES {App.Error} =
  VAR
    iterator := self.iterateFields();
    field := iterator.next();
    amp := "";
  BEGIN
    TRY 
      WHILE field # NIL DO
        Wr.PutText(wr, amp);
        Wr.PutText(wr, field.name);
        Wr.PutChar(wr, '=');
        Wr.PutText(wr, field.value);
        amp := "&";
        field := iterator.next();
      END;
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log("failed writing form query", App.LogStatus.Error);
    END;
  END WriteFormQuery;

PROCEDURE FormQueryToText(self: FormQuery):TEXT =
  VAR
    wr := TextWr.New();
  BEGIN
    self.write(wr, NIL); <* NOWARN *>
    RETURN TextWr.ToText(wr);
  END FormQueryToText;

CONST
  UUNull = -1;

(*
PROCEDURE UUDecode(ch: CHAR): INTEGER =
  BEGIN
    CASE ch OF
    | 'A'..'Z' => RETURN ORD(ch) - ORD('A');
    | 'a'..'z' => RETURN 26 + ORD(ch) - ORD('a');
    | '0'..'9' => RETURN 52 + ORD(ch) - ORD('0');
    | '+' => RETURN 62;
    | '/' => RETURN 63;
    ELSE
      RETURN UUNull;
    END;
  END UUDecode;
*)

PROCEDURE DecodeAuthorization(scheme: TEXT; msg: TEXT; log: App.Log): TEXT
  RAISES {App.Error} =
  VAR
    e0, e1, e2, e3: Word.T;
    c0, c1, c2: CHAR;
    res := TextWr.New();
    <* FATAL Thread.Alerted, Wr.Failure *>
  BEGIN
    IF Text.Equal(scheme, "Basic") THEN
      (* "Basic" scheme is simply user:password UUencoded *)
      FOR i := 0 TO (Text.Length(msg) DIV 4) - 1 DO
         e0 := Base64Decode[Text.GetChar(msg, 4 * i)];
         e1 := Base64Decode[Text.GetChar(msg, 4 * i + 1)];
         e2 := Base64Decode[Text.GetChar(msg, 4 * i + 2)];
         e3 := Base64Decode[Text.GetChar(msg, 4 * i + 3)];
         c0 := VAL(Word.Plus(Word.Shift(e0, 2), Word.Shift(e1, -4)), CHAR);
         Wr.PutChar(res, c0);
         IF e2 # UUNull THEN
           c1 := VAL(Word.Plus(Word.Shift(Word.And(e1, 16_F), 4),
                               Word.Shift(e2, -2)), CHAR);
           Wr.PutChar(res, c1);
           IF e3 # UUNull THEN
             c2 := VAL(Word.Plus(Word.Shift(Word.And(e2, 16_3), 6), e3), CHAR);
             Wr.PutChar(res, c2);
           END;
         END;
      END;
      RETURN TextWr.ToText(res);
    ELSE
      log.log(Fmt.F("unknown authorization scheme: %s\n", scheme),
              App.LogStatus.Error);
      RETURN msg;
    END;
  END DecodeAuthorization;

PROCEDURE AuthorizedRequest (request: Request;
                             auth   : AuthType;
                             account: TEXT;
                             log    : App.Log   ): BOOLEAN
  RAISES {App.Error} =
  VAR
    rd         : Rd.T;
    scheme, msg: TEXT;
    field      : Field;
  BEGIN
    TRY
      IF auth = AuthType.None THEN
         RETURN TRUE;
      ELSIF auth = AuthType.Server THEN
        field := request.lookupField(FieldName[FieldType.Authorization]);
      ELSE
        field :=
          request.lookupField(FieldName[FieldType.Proxy_Authorization]);
      END;
      IF field = NIL THEN RETURN FALSE; END;

      rd := TextRd.New(field.value);
      FastLex.Skip(rd, Spaces);
      scheme := FastLex.Scan(rd);
      FastLex.Skip(rd, Spaces);
      msg := FastLex.Scan(rd);
      msg := DecodeAuthorization(scheme, msg, log);
      IF Text.Equal(account, msg) THEN
        IF App.Debug() THEN
          log.log(
            Fmt.F("GOOD authorization: %s", msg), App.LogStatus.Debug);
        END;
        RETURN TRUE;
      ELSE
        log.log(Fmt.F("BAD authorization: %s", msg), App.LogStatus.Status);
        RETURN FALSE;
      END;
    EXCEPT
    | Rd.Failure, Thread.Alerted =>
        log.log("Unexpected problem in HTTPControl.AuthorizedRequest",
                App.LogStatus.Error);
        RETURN FALSE
    END;
  END AuthorizedRequest;

PROCEDURE ReplyUnauthorized (wr   : Wr.T;
                             auth : AuthType;
                             realm: TEXT;
                             log  : App.Log   ) RAISES {App.Error} =
  VAR
    st   : StatusType;
    ft   : FieldType;
    reply: Reply;
  BEGIN
    IF auth = AuthType.Proxy THEN
      st := StatusType.Proxy_Authentication_Required;
      ft := FieldType.Proxy_Authenticate;
    ELSE
      st := StatusType.Unauthorized;
      ft := FieldType.WWW_Authenticate;
    END;
    reply :=
      NEW(Reply, code := StatusCode[st], reason := StatusReason[st]);
    EVAL reply.addField(
           NEW(Field).init(
             FieldName[ft], Fmt.F("Basic realm=\"%s\"", realm)));
    IF App.Verbose() THEN
      log.log(reply.toText(NIL, log), App.LogStatus.Verbose);
    END;
    reply.write(wr, NIL, log);
  END ReplyUnauthorized;

CONST
  ChNull = '\000';

(*
PROCEDURE UUEncode(i: INTEGER): CHAR =
  BEGIN
    CASE i OF
    | 0..25 => RETURN VAL(ORD('A') + i, CHAR);
    | 26..51 => RETURN VAL(ORD('a') + i - 26, CHAR);
    | 52..61 => RETURN VAL(ORD('0') + i - 52, CHAR);
    | 62 => RETURN '+';
    | 63 => RETURN '/';
    ELSE <* ASSERT FALSE *> END;
  END UUEncode;
 *)

PROCEDURE AddEncoding (wr: Wr.T; c0, c1, c2: CHAR)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    (* top 6 bits of c0 *)
    Wr.PutChar(wr, Base64Encode[Word.Shift(ORD(c0), -2)]);
    IF c1 = ChNull THEN
      (* bottome 2 bits of c0 *)
      Wr.PutChar(wr, Base64Encode[Word.Shift(Word.And(ORD(c0), 16_3), 4)]);
      Wr.PutChar(wr, '=');
      Wr.PutChar(wr, '=');
    ELSE
      (* bottom 2 bits of c0 and top 4 bits of c1 *)
      Wr.PutChar(
        wr, Base64Encode[Word.Plus(Word.Shift(Word.And(ORD(c0), 16_3), 4),
                               Word.Shift(ORD(c1), -4))]);
      IF c2 = ChNull THEN
        (* bottom 4 bits of c1 *)
        Wr.PutChar(wr, Base64Encode[Word.Shift(Word.And(ORD(c1), 16_F), 2)]);
        Wr.PutChar(wr, '=');
      ELSE
        (* bottom 4 bits of c1 and top 2 bits of c2 *)
        Wr.PutChar(
          wr, Base64Encode[Word.Plus(Word.Shift(Word.And(ORD(c1), 16_F), 2),
                                 Word.Shift(ORD(c2), -6))]);
        (* bottom 6 bits of c2 *)
        Wr.PutChar(wr, Base64Encode[Word.And(ORD(c2), 16_3F)]);
      END;
    END;
  END AddEncoding;

PROCEDURE EncodeBasicAuth (account: TEXT): TEXT =
  VAR
    res              := TextWr.New();
    rd               := TextRd.New(account);
    c0, c1, c2: CHAR := ChNull;
  BEGIN
    TRY
      Wr.PutText(res, "Basic ");
      FOR i := 0 TO (Rd.Length(rd) DIV 3) - 1 DO
        AddEncoding(res, UnsafeRd.FastGetChar(rd),
                    UnsafeRd.FastGetChar(rd), UnsafeRd.FastGetChar(rd))
      END;

      IF Rd.Length(rd) MOD 3 # 0 THEN
        TRY
          c0 := UnsafeRd.FastGetChar(rd);
          c1 := UnsafeRd.FastGetChar(rd);
          c2 := UnsafeRd.FastGetChar(rd);
        EXCEPT
        | Rd.EndOfFile =>
        END;
        AddEncoding(res, c0, c1, c2);
      END;
    EXCEPT
    | Rd.Failure, Rd.EndOfFile, Wr.Failure, Thread.Alerted =>
    END;
    RETURN TextWr.ToText(res);
  END EncodeBasicAuth;

PROCEDURE BasicAuthField(account: TEXT; auth: AuthType): Field =
  BEGIN
    IF auth = AuthType.Server THEN
      RETURN NEW(Field).init(FieldName[FieldType.Authorization],
                           EncodeBasicAuth(account));
    ELSE
      RETURN NEW(Field).init(FieldName[FieldType.Proxy_Authorization],
                           EncodeBasicAuth(account));
    END;
  END BasicAuthField;

REVEAL
  RdSrc = RdSrcPublic BRANDED OBJECT
    rd: Rd.T;
  OVERRIDES
    init := InitRdSrc;
    fill := FillRdSrc;
  END;

PROCEDURE InitRdSrc(self: RdSrc; rd: Rd.T): RdSrc =
  BEGIN
    self.rd := rd;
    RETURN self;
  END InitRdSrc;

PROCEDURE FillRdSrc (self: RdSrc; VAR a: ARRAY OF CHAR): CARDINAL
  RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN Rd.GetSub(self.rd, a);
  END FillRdSrc;

REVEAL
  WrDest = WrDestPublic BRANDED OBJECT
    wr: Wr.T;
  OVERRIDES
    init := InitWrDest;
    copy := CopyWrDest;
  END;

PROCEDURE InitWrDest(self: WrDest; wr: Wr.T): WrDest =
  BEGIN
    self.wr := wr;
    RETURN self;
  END InitWrDest;

PROCEDURE CopyWrDest (self: WrDest; READONLY a: ARRAY OF CHAR)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    Wr.PutString(self.wr, a);
  END CopyWrDest;

PROCEDURE WriteBody (header: Header; wr: Wr.T; src: Src; log: App.Log)
  RAISES {App.Error} =
  VAR
    chars : ARRAY [0 .. 4095] OF CHAR;
    cnt   : CARDINAL;
    field : Field;
    trd   : TextRd.T;
    length: CARDINAL                  := LAST(CARDINAL);

  PROCEDURE FromProc (VAR a: ARRAY OF CHAR): CARDINAL
    RAISES {Rd.Failure, Thread.Alerted, CopyError} =
    BEGIN
      RETURN src.fill(a);
    END FromProc;

  BEGIN
    field :=
      header.lookupField(FieldName[FieldType.Content_Encoding]);
    IF field # NIL THEN
      IF CIText.Equal(field.value, "chunked") THEN
        TRY
          LOOP
            cnt := src.fill(chars);
            IF cnt = 0 THEN EXIT END;
            Wr.PutText(wr, Fmt.Int(cnt));
            Wr.PutText(wr, "\r\n");
            Wr.PutString(wr, SUBARRAY(chars, 0, cnt));
            Wr.PutText(wr, "\r\n");
          END;
          Wr.PutText(wr, "0\r\n\r\n");
        EXCEPT
        | Rd.Failure, Wr.Failure, Thread.Alerted, CopyError =>
            log.log(
              "HTTP.WriteBody: Error writing body", App.LogStatus.Error);
        END;

      ELSE
        log.log(Fmt.F("HTTP.WriteBody: Unknown Content-Encoding: %s",
                      field.value), App.LogStatus.Error);
      END;
    ELSE
      field :=
        header.lookupField(FieldName[FieldType.Content_Length]);

      TRY
        IF field # NIL THEN
          TRY
            trd := TextRd.New(field.value);
            length := Lex.Int(trd);
            IF NOT Rd.EOF(trd) THEN RAISE Rd.Failure(NIL); END;
          EXCEPT
          | Rd.Failure, FloatMode.Trap, Lex.Error =>
              log.log(Fmt.F("HTTP.WriteBody: Bad Content-Length: %s",
                            field.value), App.LogStatus.Error);
          END;
        END;
        EVAL RdCopy.FromProc(wr, FromProc, length); <* NOWARN *>
      EXCEPT
      | Wr.Failure, Thread.Alerted, CopyError =>
          log.log("HTTP.WriteBody: Error reading chunks from src",
                  App.LogStatus.Error);
      END;
    END;
  END WriteBody;

PROCEDURE ReadChunk (rd: Rd.T; dest: Dest; log: App.Log): BOOLEAN
  RAISES {App.Error} =
  VAR
    chars: ARRAY [0 .. 4095] OF CHAR;
    cnt  : INTEGER;
  BEGIN
    TRY
      cnt := Lex.Int(rd);
      IF Rd.GetChar(rd) # '\r' OR Rd.GetChar(rd) # '\n' THEN
        log.log("HTTP.ReadChunk: bad chunk format", App.LogStatus.Error);
      END;
      IF cnt = 0 THEN RETURN FALSE; END;
      WHILE cnt > 0 DO
        WITH len = MIN(NUMBER(chars), cnt) DO
          IF Rd.GetSub(rd, SUBARRAY(chars, 0, len)) # len THEN
            log.log(
              "HTTP.ReadChunk: insufficient chars", App.LogStatus.Error);
          END;
          dest.copy(SUBARRAY(chars, 0, len));
          cnt := cnt - NUMBER(chars);
        END;
      END;
    EXCEPT
    | FloatMode.Trap, Lex.Error, Rd.Failure, Thread.Alerted, Rd.EndOfFile,
          Wr.Failure, CopyError =>
        log.log(
          "HTTP.ReadChunk: problem reading chunk", App.LogStatus.Error);
    END;
    RETURN TRUE;
  END ReadChunk;

PROCEDURE ReadBody (header: Header; rd: Rd.T; dest: Dest; log: App.Log)
  RAISES {App.Error} =
  VAR
    field : Field;
    length: CARDINAL := LAST(CARDINAL);
    trd   : TextRd.T;
  PROCEDURE ToProc (READONLY a: ARRAY OF CHAR)
    RAISES {Wr.Failure, Thread.Alerted, CopyError} =
    BEGIN
      dest.copy(a);
    END ToProc;
  BEGIN
    field :=
      header.lookupField(FieldName[FieldType.Content_Encoding]);
    IF field # NIL THEN
      IF CIText.Equal(field.value, "chunked") THEN
        WHILE ReadChunk(rd, dest, log) DO END;
      ELSE
        log.log(Fmt.F("HTTP.ReadBody: Unknown Content-Encoding: %s",
                      field.value), App.LogStatus.Error);
      END;
    ELSE
      field :=
        header.lookupField(FieldName[FieldType.Content_Length]);
      TRY
        IF field # NIL THEN
          TRY
            trd := TextRd.New(field.value);
            length := Lex.Int(trd);
            IF NOT Rd.EOF(trd) THEN RAISE Rd.Failure(NIL); END;
          EXCEPT
          | Rd.Failure, FloatMode.Trap, Lex.Error =>
              log.log(Fmt.F("HTTP.ReadBody: Bad Content-Length: %s",
                            field.value), App.LogStatus.Error);
          END;
        END;
        EVAL RdCopy.ToProc(rd, ToProc, length); <* NOWARN *>
      EXCEPT
      | Rd.Failure, Thread.Alerted, CopyError =>
          log.log("HTTP.ReadBody: Error reading body", App.LogStatus.Error);
      END;
    END;
  END ReadBody;

CONST
  WeekDay3 = ARRAY Date.WeekDay OF
               TEXT{"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
  WeekDayFull = ARRAY Date.WeekDay OF
                  TEXT{"Sunday", "Monday", "Tuesday", "Wednesday",
                       "Thursday", "Friday", "Saturday"};
  Month3 = ARRAY Date.Month OF
             TEXT{"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                  "Sep", "Oct", "Nov", "Dec"};

PROCEDURE WriteTime(wr: Wr.T; time: Time.T; log: App.Log) RAISES {App.Error} =
  VAR
    date := Date.FromTime(time, Date.UTC);
  BEGIN
    TRY
      Wr.PutText(wr, WeekDay3[date.weekDay]);
      Wr.PutText(wr, ", ");
      Wr.PutText(wr, Fmt.Pad(Fmt.Int(date.day), 2, '0'));
      Wr.PutChar(wr, ' ');
      Wr.PutText(wr, Month3[date.month]);
      Wr.PutChar(wr, ' ');
      Wr.PutText(wr, Fmt.Int(date.year));
      Wr.PutChar(wr, ' ');
      Wr.PutText(wr, Fmt.Pad(Fmt.Int(date.hour), 2, '0'));
      Wr.PutChar(wr, ':');
      Wr.PutText(wr, Fmt.Pad(Fmt.Int(date.minute), 2, '0'));
      Wr.PutChar(wr, ':');
      Wr.PutText(wr, Fmt.Pad(Fmt.Int(date.second), 2, '0'));
      Wr.PutText(wr, " GMT");
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        log.log("HTTP.WriteTime: error writing time", App.LogStatus.Error);
    END;
  END WriteTime;

PROCEDURE Match (READONLY texts: ARRAY OF TEXT; text: TEXT; log: App.Log):
  INTEGER RAISES {App.Error} =
  BEGIN
    FOR i := 0 TO LAST(texts) DO
      IF Text.Equal(texts[i], text) THEN RETURN i END;
    END;
    log.log(
      Fmt.F("HTTP.Match: no match for: %s", text), App.LogStatus.Error);
    RETURN -1;
  END Match;
  
CONST
  NotSpaceOrComma = SET OF CHAR{'\000' .. '\377'} - SET OF CHAR{' ', ','};
  NotSpaceOrDash = SET OF CHAR{'\000' .. '\377'} - SET OF CHAR{' ', '-'};

PROCEDURE ReadTime (rd: Rd.T; log: App.Log): Time.T RAISES {App.Error} =
  VAR
    tok : TEXT;
    date: Date.T;
    ch  : CHAR;
  BEGIN
    TRY
      tok := Lex.Scan(rd, NotSpaceOrComma);
      IF Text.Length(tok) = 3 THEN
        date.weekDay := VAL(Match(WeekDay3, tok, log), Date.WeekDay);
        ch := Rd.GetChar(rd);
        IF ch = ',' THEN
          (* RFC 822/RFC 1123 format *)
          IF Rd.GetChar(rd) # ' ' THEN
            log.log("HTTP.ReadTime: bad char1", App.LogStatus.Error)
          END;
          date.day := Lex.Int(rd);
          IF Rd.GetChar(rd) # ' ' THEN
            log.log("HTTP.ReadTime: bad char2", App.LogStatus.Error)
          END;
          tok := Lex.Scan(rd);
          date.month := VAL(Match(Month3, tok, log), Date.Month);
          IF Rd.GetChar(rd) # ' ' THEN
            log.log("HTTP.ReadTime: bad char3", App.LogStatus.Error)
          END;
          date.year := Lex.Int(rd);
          IF Rd.GetChar(rd) # ' ' THEN
            log.log("HTTP.ReadTime: bad char4", App.LogStatus.Error)
          END;
          date.hour := Lex.Int(rd);
          IF Rd.GetChar(rd) # ':' THEN
            log.log("HTTP.ReadTime: bad char5", App.LogStatus.Error)
          END;
          date.minute := Lex.Int(rd);
          IF Rd.GetChar(rd) # ':' THEN
            log.log("HTTP.ReadTime: bad char6", App.LogStatus.Error)
          END;
          date.second := Lex.Int(rd);
          IF Rd.GetChar(rd) # ' ' THEN
            log.log("HTTP.ReadTime: bad char7", App.LogStatus.Error)
          END;
          IF NOT Text.Equal(Lex.Scan(rd), "GMT") THEN
            log.log("HTTP.ReadTime: not GMT", App.LogStatus.Error)
          END;
        ELSIF ch = ' ' THEN
          (* ANSI C's asctime() format *)
          tok := Lex.Scan(rd);
          date.month := VAL(Match(Month3, tok, log), Date.Month);
          IF Rd.GetChar(rd) # ' ' THEN
            log.log("HTTP.ReadTime: bad char10", App.LogStatus.Error)
          END;
          Lex.Skip(rd);
          date.day := Lex.Int(rd);
          IF Rd.GetChar(rd) # ' ' THEN
            log.log("HTTP.ReadTime: bad char11", App.LogStatus.Error)
          END;
          date.hour := Lex.Int(rd);
          IF Rd.GetChar(rd) # ':' THEN
            log.log("HTTP.ReadTime: bad char12", App.LogStatus.Error)
          END;
          date.minute := Lex.Int(rd);
          IF Rd.GetChar(rd) # ':' THEN
            log.log("HTTP.ReadTime: bad char13", App.LogStatus.Error)
          END;
          date.second := Lex.Int(rd);
          IF Rd.GetChar(rd) # ' ' THEN
            log.log("HTTP.ReadTime: bad char14", App.LogStatus.Error)
          END;
          date.year := Lex.Int(rd);
        ELSE
          log.log(
            "HTTP.ReadTime: bad weekday separator", App.LogStatus.Error);
        END;
      ELSE
        (* RFC 850/RFC 1036 format *)
        date.weekDay := VAL(Match(WeekDayFull, tok, log), Date.WeekDay);
        IF Rd.GetChar(rd) # ',' THEN
          log.log("HTTP.ReadTime: bad char21", App.LogStatus.Error)
        END;
        IF Rd.GetChar(rd) # ' ' THEN
          log.log("HTTP.ReadTime: bad char22", App.LogStatus.Error)
        END;
        date.day := Lex.Int(rd);
        IF Rd.GetChar(rd) # '-' THEN
          log.log("HTTP.ReadTime: bad char23", App.LogStatus.Error)
        END;
        tok := Lex.Scan(rd, NotSpaceOrDash);
        date.month := VAL(Match(Month3, tok, log), Date.Month);
        IF Rd.GetChar(rd) # '-' THEN
          log.log("HTTP.ReadTime: bad char10", App.LogStatus.Error)
        END;
        date.year := Lex.Int(rd);
        IF date.year < 70 THEN
          date.year := 2000 + date.year;
        ELSE
          date.year := 1900 + date.year;
        END;
        IF Rd.GetChar(rd) # ' ' THEN
          log.log("HTTP.ReadTime: bad char23", App.LogStatus.Error)
        END;
        date.hour := Lex.Int(rd);
        IF Rd.GetChar(rd) # ':' THEN
          log.log("HTTP.ReadTime: bad char24", App.LogStatus.Error)
        END;
        date.minute := Lex.Int(rd);
        IF Rd.GetChar(rd) # ':' THEN
          log.log("HTTP.ReadTime: bad char25", App.LogStatus.Error)
        END;
        date.second := Lex.Int(rd);
        IF Rd.GetChar(rd) # ' ' THEN
          log.log("HTTP.ReadTime: bad char26", App.LogStatus.Error)
        END;
        IF NOT Text.Equal(Lex.Scan(rd), "GMT") THEN
          log.log("HTTP.ReadTime: not GMT", App.LogStatus.Error)
        END;
      END;
      date.offset := 0;
      date.zone := "GMT";
      RETURN Date.ToTime(date);
    EXCEPT
    | Date.Error, Rd.Failure, Rd.EndOfFile, Thread.Alerted, FloatMode.Trap,
          Lex.Error =>
        log.log("HTTP.ReadTime: error reading time", App.LogStatus.Error);
        RETURN 0.0D0;
    END;
  END ReadTime;

PROCEDURE SetProgramInfo(READONLY info: ProgramInfo) =
  BEGIN
    programInfo := info;
  END SetProgramInfo;

PROCEDURE GetProgramInfo(): ProgramInfo =
  BEGIN
    RETURN programInfo;
  END GetProgramInfo;

PROCEDURE SetDefaultViaFieldValue (READONLY version: Version;
                                   port   : INTEGER;
                                   alias  : TEXT      := NIL) =
  VAR
    wr := TextWr.New();
    <* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    IF alias # NIL THEN
      Wr.PutText(wr, alias);
    ELSE
      Wr.PutText(wr, App.GetHostName());
      Wr.PutChar(wr, ':');
      Wr.PutText(wr, Fmt.Int(port));
    END;
    Wr.PutChar(wr, ' ');
    Wr.PutText(wr, Fmt.Int(version.major));
    Wr.PutChar(wr, '.');
    Wr.PutText(wr, Fmt.Int(version.minor));
    Wr.PutChar(wr, ' ');
    Wr.PutText(wr, programInfo.name);
    defaultViaFieldValue := TextWr.ToText(wr);
  END SetDefaultViaFieldValue;

BEGIN
  FOR i := FIRST(Method) TO LAST(Method) DO
    Methods[i] := Atom.FromText(MethodText[i]);
  END;
END HTTP.
