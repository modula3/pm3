INTERFACE Frame ;

IMPORT Pathname ;

PROCEDURE Put(name : TEXT ; content : TEXT) ;

PROCEDURE Get(name : TEXT) : TEXT ;

PROCEDURE Create(srcDir : Pathname.T ; src : TEXT ; altSrc : TEXT ; dest : TEXT) ;

END Frame.
