INTERFACE Uframe;

FROM Ctypes IMPORT int, char_star;

(* sys/frame.h *)

TYPE
  struct_frame = RECORD
    fr_local : ARRAY [0..7] OF int;	 (* saved locals *)
    fr_arg   : ARRAY [0..5] OF int;	 (* saved arguments [0 - 5] *)
    fr_savfp : ADDRESS;			 (* saved frame pointer *)
    fr_savpc : int;			 (* saved program counter *)
    fr_stret : char_star;		 (* struct return addr *)
    fr_argd  : ARRAY [0..5] OF int;	 (* arg dump area *)
    fr_argx  : ARRAY [0..0] OF int;	 (* array of args past the sixth *)
  END;
  struct_frame_star = UNTRACED REF struct_frame;
END Uframe.
