MODULE CrossRef ;
(* Create cross reference list of identifiers
   P.D. Terry, Rhodes University, January 1992
   Release for use with COCO/R system *)

(* Modula 3 version by Olly Stephens (Jan 1997) *)

IMPORT Text, Wr, Fmt, Thread, IntList ;

<* FATAL Wr.Failure, Thread.Alerted *>

TYPE
  Node = BRANDED OBJECT
           text      : TEXT ;
           leftTree  : Table ;
           rightTree : Table ;
           definedBy : CARDINAL ;
           okay      : BOOLEAN ;
           refList   : IntList.T
         END ;

REVEAL
  Table = Node ;

PROCEDURE Create(VAR table : Table) =
BEGIN
  table := NIL ;
END Create ;

PROCEDURE Add (VAR table : Table ; name : TEXT ;
               reference : CARDINAL ; defined : BOOLEAN) =

  PROCEDURE AddToTree (VAR root : Table) =

    PROCEDURE NewReference(leaf : Table) =
    BEGIN
      IF (NOT IntList.Member(leaf.refList, reference)) THEN
        leaf.refList := IntList.Cons(reference, leaf.refList)
      END
    END NewReference ;

  BEGIN
    IF (root = NIL) THEN (* at a leaf - name must now be inserted *)
      root := NEW(Table, text := name) ;
      CASE defined OF
        TRUE =>
          root.definedBy := reference ;
          root.okay      := TRUE
      | FALSE =>
          root.definedBy := 0 ;
          root.okay      := FALSE ;
          NewReference(root)
      END
    ELSIF (Text.Compare(name, root.text) > 0) THEN
      AddToTree(root.rightTree)
    ELSIF (Text.Compare(name, root.text) < 0) THEN
      AddToTree(root.leftTree)
    ELSE
      CASE defined OF
        TRUE =>
          IF (root.definedBy = 0) THEN
            root.definedBy := reference ;
            root.okay := TRUE
          ELSE
            root.okay := FALSE (*redefined*)
          END
      | FALSE =>
          NewReference(root)
      END
    END
  END AddToTree ;

BEGIN
  AddToTree(table)
END Add ;

PROCEDURE List(table : Table ; wr : Wr.T) =

  PROCEDURE OneEntry(thisNode : Table) =
  VAR list : IntList.T ;
      len  : CARDINAL ;
  BEGIN
    Wr.PutText(wr, thisNode.text) ;
    len := Text.Length(thisNode.text) ;
    IF (len < 16) THEN
      Wr.PutText(wr, Fmt.Pad(" ", (16 - len)))
    END ;
    IF (NOT thisNode.okay) THEN
      Wr.PutChar(wr, '?')
    ELSE
      Wr.PutChar(wr, ' ')
    END ;
    Wr.PutText(wr, Fmt.Pad(Fmt.Int(thisNode.definedBy), 4)) ;
    Wr.PutText(wr, " - ") ;
    len := 0 ;
    list := IntList.Reverse(thisNode.refList) ;
    WHILE (list # NIL) DO
      Wr.PutText(wr, Fmt.Pad(Fmt.Int(list.head), 4)) ;
      INC(len) ;
      IF ((len MOD 12) = 0) THEN
        Wr.PutText(wr, "\n" & Fmt.Pad(" ", 20))
      END ;
      list := list.tail
    END ;
    Wr.PutChar(wr, '\n') ;
  END OneEntry ;

BEGIN
  IF (table # NIL) THEN
    List(table.leftTree, wr) ;
    OneEntry(table) ;
    List(table.rightTree, wr)
  END
END List ;

BEGIN
END CrossRef.
