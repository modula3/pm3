INTERFACE PageLock;

(*
  | *************************************************************************
  | Created by:  Reiner Nix 
  |
  | $Author$
  | $Revision$
  | $Date$
  | $Log$
  | Revision 1.1  2003/03/27 15:25:27  hosking
  | Initial revision
  |
  | Revision 1.2  1996/03/01 12:56:41  rbnix
  | 	Format function(s) added.
  |
  | Revision 1.1  1996/02/09 16:03:03  rbnix
  | 	First version of common used scheduler parts.
  |
  | *************************************************************************
*)

(*
 | --- PageLock -----------------------------------------------------------
  This type collection module contains lock modes for pages.
 | ------------------------------------------------------------------------
 *)

TYPE
  Mode			= {X,			(* exclusive		*)
                           C,			(* cached		*)
                           P,			(* pending		*)
                           O,			(* out-of-date		*)
                           S};			(* shared		*)
  ModeSet		= SET OF Mode;

  ClientMode		=[Mode.X .. Mode.S];	(* modes of client	*)
  ServerMode		=[Mode.X .. Mode.O];	(* modes of server	*)
  CallbackMode		=[Mode.C .. Mode.O];	(* modes for callbacks	*)


PROCEDURE FmtMode	(         mode		:Mode) :TEXT;

  
END PageLock.
