UNSAFE INTERFACE StringCopy;

IMPORT Ctypes;

PROCEDURE CopyAttributeStoT (s: Ctypes.char_star; len: CARDINAL): TEXT;
  (* Return a text with the same contents as the null-terminated C string
     "s".  This copies "s", so the result is valid even if "s" is later
     freed. *)

PROCEDURE CopyTtoS (t: TEXT; s: Ctypes.char_star);
  (* Return a null-terminated C string with the same contents as "t", but
     not sharing any storage with "t".  The result should be passed to
     "FreeCopiedS" after it is irrelevant. *)

PROCEDURE CopyStoT (s: Ctypes.char_star): TEXT;
  (* Return a text with the same contents as the null-terminated C string
     "s".  This copies "s", so the result is valid even if "s" is later
     freed. *)

END StringCopy.



