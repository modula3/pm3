INTERFACE Parameter;

CONST
  Brand = "Parameter";

TYPE
  T = RECORD
        type      : TEXT;
        writeable : BOOLEAN;
        function  : BOOLEAN;
      END;

END Parameter.
