MODULE TasteSym ;

IMPORT Text ;

TYPE
  Scope = OBJECT
            locals  : Obj ;
            nextAdr : CARDINAL ;
            next    : Scope
          END ;

VAR
  topScope : Scope ;    (* topmost procedure scope *)

PROCEDURE EnterScope() =
VAR
  scope := NEW(Scope, locals := NIL, nextAdr := 3) ;
BEGIN
  scope.next := topScope ;
  topScope := scope ;
  INC(curLevel)
END EnterScope ;

PROCEDURE LeaveScope() =
BEGIN
  topScope := topScope.next ;
  DEC(curLevel)
END LeaveScope ;

PROCEDURE DataSpace() : CARDINAL =
BEGIN
  RETURN topScope.nextAdr - 3
END DataSpace ;

PROCEDURE New(name: TEXT; kind : ObjKind) : Obj =
VAR
  obj, p : Obj ;
BEGIN
  obj := NEW(Obj, name := name, level := curLevel, kind := kind,
                  vtype := VarType.Undef, next := NIL) ;
  p := topScope.locals ;
  WHILE (p # NIL) DO
    IF (Text.Equal(p.name, name)) THEN
      RETURN undefObj
    END ;
    p := p.next
  END ;
  obj.next := topScope.locals ;
  topScope.locals := obj ;
  IF (kind = ObjKind.Var) THEN
    obj.adr := topScope.nextAdr ;
    INC(topScope.nextAdr)
  END ;
  RETURN obj
END New ;

PROCEDURE Find(name: TEXT) : Obj =
VAR
  obj   : Obj ;
  scope : Scope ;
BEGIN
  scope := topScope;
  WHILE (scope # NIL) DO
    obj := scope.locals ;
    WHILE (obj # NIL) DO
      IF (Text.Equal(obj.name, name)) THEN
        RETURN obj
      END ;
      obj := obj.next
    END ;
    scope := scope.next
  END ;
  RETURN undefObj
END Find ;

BEGIN
  topScope := NIL ;
  curLevel := 0 ;
  undefObj := NEW(Obj, name := "*undef*", kind := ObjKind.Undef,
                       vtype := VarType.Undef, next := NIL)
END TasteSym.
