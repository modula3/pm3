INTERFACE xRNG01;
(*Copyright (c) 1996, m3na project
  
Abstract: Collection of random number generators
Each returns numbers in range:
     xRand.Min..xRand.Max
which is 0..1, but not including the endpoints.

3/16/96  Harry George  Initial version
*)
IMPORT xRand;

(*==========================*)
TYPE
  DECSRC <: xRand.RandomGen;  (*wrapper for DEC SRC Random.Default*)
  ran0   <: xRand.RandomGen;  (*inspired by NR92 ran0*)
  ran1   <: xRand.RandomGen;  (*inspired by NR92 ran1*)

(*==========================*)
END xRNG01.
