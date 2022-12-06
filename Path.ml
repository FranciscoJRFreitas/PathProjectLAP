(* Path module body *)
(* LAP (AMD 2022) *)

(* 
Student 1: 60313 Francisco Freitas
Student 2: 60288 Guilherme Figueira

Comment:

?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????

*)

(*
0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
	100 columns
*)


(* COMPILATION - How Mooshak builds this module:
		ocamlc -c Path.mli Path.ml
*)



(* AUXILIARY GENERAL FUNCTIONS - you can add more *)

let rec clean l =
	match l with
	| [] -> []
	| [x] -> [x]
	| x::y::xs ->
		if x = y then clean (y::xs)
		else x::clean (y::xs)

let unique l = (* removes duplicates *)
	clean (List.sort compare l)

let length =
	List.length

let map =
	List.map

let filter =
	List.filter

let mem =
	List.mem

let flatMap f l =
	List.flatten (map f l)

let partition =
	List.partition

let exists =
	List.exists

let for_all =
	List.for_all

let union l1 l2 =
	clean (l1 @ l2)

let inter l1 l2 =
	filter (fun x -> mem x l2) l1

let diff l1 l2 =
	filter (fun a -> not (mem a l2)) l1



(* TYPES & CONSTANTS *)


type point = int * int
type path = point list

let _NO_PATH = []



(* SOME EXAMPLES - you can add more *)

let example1 = [
		(0,0); (0,1); (0,2);
		(1,2); (2,2); (2,1);
		(2,0); (1,0)
]

let example2 = [
		(0,0); (0,1); (0,2);
		(1,2); (2,2); (2,1);
		(2,0); (1,0); (0,0)
]

let example3 = [
		(2,2); (2,3); (2,4); (2,5);
		(3,5); (4,5); (5,5); (6,5);
		(5,4); (4,3); (3,2); (2,1);
		(1,0); (1,1); (1,2); (2,3);
		(3,4); (4,5); (5,6); (6,7)
]

let example4 = [
		(1,1); (2,1); (3,1); (4,1);
		(1,2); (2,2); (3,2); (4,2);
		(1,3); (2,3); (3,3); (4,3);
		(1,4); (2,4); (3,4); (4,4)
]

let example5 = [
		(0,4); (1,4); (2,4); (3,4); (4,4); (5,4); (6,4);
		(4,0); (4,1); (4,2); (4,3); (4,4); (4,5); (4,6);
		(0,0); (1,1); (2,2); (3,3); (4,4); (5,5); (6,6);
		(0,8); (1,8); (2,8); (2,9); (2,10); (1,10); (0,10); (0,9); (0,8)
]

let example6 = [
		(0,0); (0,1); (0,2); (0,3); (0,3); (0,4); (0,5); (0,7); (0,8)
]



(* BASIC PATH FUNCTIONS - you can add more *)

(* Adjacent points? *)
let areAdjacent (x1, y1) (x2, y2) =
	abs(x2 - x1) <= 1 && abs(y2 - y1) <= 1

(* Are two points the same? *)
let areSame a b =
	a = b
	
(* Adjacent distinct points? *)
let areAdjacentDistinct a b =
	areAdjacent a b && not (areSame a b)

let isFirst p l =
	match p with
	| [] -> false
	| x :: xs -> mem x l
;;

(* FUNCTION makeSegment *)

let rec makeSegment (x,y) (w,z) =
		if x > w then (x,y) :: makeSegment (x-1,y) (w,z)
			 else if x < w then (x,y) :: makeSegment (x+1, y) (w,z)
					else if y > z then (x,y) :: makeSegment (x,y-1) (w,z)
							else if y < z then (x,y) :: makeSegment (x, y+1) (w,z)
					else [(x,y)]
;;


(* FUNCTION isContinuous *)

let rec isContinuous p =
	match p with
	| [] -> false
	| [(x,y)] -> true
	| (x,y) :: (w,z) :: xs -> ((areAdjacent (x,y) (w,z)) && (isContinuous ((w,z) :: xs))) 
;;

let rec getDiscPoints p =
	if isContinuous p then _NO_PATH
	else
	match p with
	| [] -> _NO_PATH
	| [x] -> _NO_PATH
	| x :: y :: xs -> if not (areAdjacent x y) 
							then (y::(getDiscPoints (y:: xs))) 
							else getDiscPoints (y :: xs)
;;


(* FUNCTION intersections *)

let rec intersections p =
	match p with
	| [] -> _NO_PATH
	| x :: xs -> if mem x xs then unique (x :: (intersections xs)) else intersections xs
;;



(* FUNCTION isSegment *)

(*Pre: lenght p > 0*)
let getLastPoint p = List.nth p ((length p) - 1);;


let isClosedSegment p =
	(isContinuous p) && (length (intersections p) = 1) && 
	mem (getLastPoint p) (intersections p) && isFirst p (intersections p)
;;

let isSegment p =
	if (isContinuous p) && (length (intersections p) = 0)
		then true
	else
		isClosedSegment p
;;


(* FUNCTION interval *)

 let rec startInterval s b =
   match s with
   | [] -> _NO_PATH
   | x::xs -> if not (areSame x b) then x::startInterval xs b else b::_NO_PATH
 ;;

let rec interval p a b =
  if (mem a p) && (mem b p)
		then if areSame a b 
  			then if isClosedSegment p && isFirst p [a]
				then p
				else [a]
		else
	 	match p with
     	| [] -> _NO_PATH
     	| x::xs -> if (areSame x a) then a::(startInterval xs b) else interval xs a b
	else _NO_PATH
;;
 
(* FUNCTION segments *)
let rec getRestAt p i =
	match p with
	| [] -> _NO_PATH
	| x::xs -> if i <= 0 then p else getRestAt xs (i-1)
;; 

let rec getSegmentsDisc_aux p =
	match p with
	|[] -> _NO_PATH
	|[x] -> [x] 
	| x::y::xs -> if areAdjacent x y then x::(getSegmentsDisc_aux (y::xs)) else [x]
;;

let rec getSegmentsDisc p =
	match p with
	| [] -> _NO_PATH
	| x::xs -> let seg = getSegmentsDisc_aux p in seg :: getSegmentsDisc (getRestAt p (length seg))
				
;;

let pointIsFirst p l =
	match l with
	| [] -> false
	| x::xs -> areSame x p
;;

let rec sepInter_aux pdOriginal pd i = 
	match pd with
	| [] -> _NO_PATH
	| (x,y)::xs -> if (mem (x,y) i && not (pointIsFirst (x,y) pdOriginal)) then [(x,y)] 
				   else (x,y)::(sepInter_aux pdOriginal xs i)
;;

let rec sepInter pd i = 
	match pd with
	|[] -> _NO_PATH
	| x::xs -> let segi = sepInter_aux pd pd i in segi :: sepInter (getRestAt pd (length segi)) i
;;

let rec joinInters segs i=
	match segs with
	| [] -> _NO_PATH
	| x::xs -> (sepInter x i) @ (joinInters xs i)
;;

let rec dupeSamePoints l i=
	match l with
	| [] -> _NO_PATH
	| [x] -> [x]
	| x::y::xs -> if (mem (getLastPoint x) i) && not (areSame (getLastPoint x) (List.hd y)) 
								then x::(dupeSamePoints (((getLastPoint x)::y)::xs) i)
								else x::(dupeSamePoints (y::xs) i)
;;

let rec segments p = 
	let discSegs = getSegmentsDisc p in
	let auxRes = joinInters discSegs (intersections p) in 
	dupeSamePoints auxRes (intersections p) 
;;

(* FUNCTION best0 *)

let rec best0 pp a b =
   match pp with
   | [] -> _NO_PATH
   | x::xs -> if (length (interval x a b) <= length (best0 xs a b)) 
   			  then interval x a b else best0 xs a b
;; 


(* FUNCTION best1 *)

let rec getSegmentsWith sl p =
	match sl with
	| [] -> _NO_PATH
	| x::xs -> if mem p x then x::(getSegmentsWith xs p) else getSegmentsWith xs p
;;

let rec getOneSegmentWith sl p =
	match sl with
	| [] -> _NO_PATH
	| x::xs -> if mem p x then x else getOneSegmentWith xs p
;;

let intervalBest1 pa pb i a b=
	unique ((interval pa a i)@(interval pb i b))
;;

let rec best1_aux s pp ii a b =
	if best0 pp a b = _NO_PATH then
	match ii with
	| [] -> s
	| x::xs -> let ipps = (getSegmentsWith pp x) in
				let pa = (getOneSegmentWith ipps a) in
				let pb = getOneSegmentWith ipps b in
				 if length (intervalBest1 pa pb x a b) = 0 
					then best1_aux s pp xs a b
				 else (let newP = (intervalBest1  pa pb x a b) in
				 	if length newP <= length s then best1_aux newP pp xs a b
				 	else best1_aux s pp xs a b)
	else best0 pp a b
;;

let best1 pp ii a b =
	match pp with
	| [] -> _NO_PATH
	| x::xs -> best1_aux x pp ii a b
;;

(* FUNCTION best *)

let rec getFirstSegment pp a =
	match pp with
	| [] -> _NO_PATH
	| x::xs -> if mem a x then x else getFirstSegment xs a
;;

let rec getFirstInter p ii =
	match ii with
	| [] -> List.hd p
	| x::xs -> if mem x p && length (interval p (List.hd p) x) <= length (interval p (List.hd p) (getFirstInter p xs)) 
							then x 
						 else getFirstInter p xs
;;

let rec best pp ii a b =
	if best1 pp ii a b <> _NO_PATH then best1 pp ii a b
	else let firstSeg = getFirstSegment pp a in
				if mem b firstSeg then interval firstSeg a b else
					let firstInter = getFirstInter firstSeg ii in
				interval firstSeg a firstInter @ best pp (diff ii [firstInter]) firstInter b
	;;
