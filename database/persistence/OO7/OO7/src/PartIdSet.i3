INTERFACE PartIdSet;
TYPE
  Public = UNTRACED ROOT OBJECT
  METHODS
    dispose();
    insert(val: INTEGER);
    contains(val: INTEGER): BOOLEAN;
    empty(): BOOLEAN;
  END;
  T <: Public;
END PartIdSet.
