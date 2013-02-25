(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals(strings) =
  List.filter (fn x => Char.isUpper(String.sub(x, 0))) strings

fun longest_string1(strings) =
  foldl (fn(x,y) => if String.size x > String.size y then x else y) "" strings

fun longest_string2(strings) =
  foldl (fn(x,y) => if String.size x >= String.size y then x else y) "" strings

fun longest_string_helper f strings =
  foldl (fn(x,y) => if f(String.size x, String.size y) then x else y) "" strings

val longest_string3 =
  longest_string_helper (fn(x,y) => x > y)

val longest_string4 =
  longest_string_helper (fn(x,y) => x >= y)

val longest_capitalized =
  longest_string4 o only_capitals

val rev_string =
  implode o rev o explode

fun first_answer _ [] = raise NoAnswer
  | first_answer f (x::xs) =
    case f(x)
      of NONE => first_answer f xs
       | SOME v => v

fun all_answers _ [] = SOME []
  | all_answers f lst =
    let
      fun helper([], acc) = SOME acc
        | helper(x::xs, acc) =
          case x
            of NONE => NONE
             | SOME v => helper(xs, v @ acc)
    in
      helper(map f lst, [])
    end

val count_wildcards =
  g (fn x => 1) (fn x => 0)

val count_wild_and_variable_lengths =
  g (fn x => 1) (fn x => String.size x)

fun count_some_var(str, ptrn) =
  g (fn x => 0) (fn x => if x = str then 1 else 0) ptrn

val check_pat =
  let
    fun get_variables p =
      case p
        of Variable x => [x]
	 | TupleP ps => List.concat (map get_variables ps)
	 | ConstructorP(_,p) => get_variables p
	 | _ => []

    fun has_duplicates [] = false
      | has_duplicates (x::xs) =
        List.exists (fn y => x = y) xs orelse has_duplicates xs
  in
    not o has_duplicates o get_variables
  end

fun match valptrn =
  case valptrn
    of (_, Wildcard) => SOME []
     | (v, Variable s) => SOME [(s, v)]
     | (Unit, UnitP) => SOME []
     | (Const v, ConstP v') => if v = v' then SOME [] else NONE
     | (Tuple vs, TupleP ps) =>
         if length(vs) = length(ps)
         then all_answers match (ListPair.zip(vs, ps))
         else NONE
     | (Constructor(s2, v), ConstructorP(s1, p)) => 
         if s1 = s2
         then match(v, p)
         else NONE
     | _ => NONE

fun first_match v ptrnlist =
  SOME(first_answer (fn p => match(v, p)) ptrnlist)
  handle NoAnswer => NONE
