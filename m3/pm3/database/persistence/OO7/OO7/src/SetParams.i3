INTERFACE SetParams;

EXCEPTION Error;

PROCEDURE FromFile(configFileName: TEXT) RAISES { Error };

END SetParams.
