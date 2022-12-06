(* Path module interface *)
(* LAP (AMD 2022) *)

(*
0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
	100 columns
*)

type point = int * int
type path = point list

val _NO_PATH : path

val makeSegment : point -> point -> path
val isContinuous : path -> bool
val intersections : path -> point list
val isSegment : path -> bool
val segments : path -> path list
val interval : path -> point -> point -> path
val best0 : path list -> point -> point -> path
val best1 : path list -> point list -> point -> point -> path
(*val best : path list -> point list -> point -> point -> path*)