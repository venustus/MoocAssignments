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

(* Using curryied form of List.filter here *)
val only_capitals = List.filter (fn x => Char.isUpper(String.sub(x, 0)))

val longest_string1 =
    List.foldl (fn (x, acc) => if String.size(x) > String.size(acc) then x else acc) ""

val longest_string2 = 
    List.foldl (fn (x, acc) => if String.size(x) >= String.size(acc) then x else acc) ""

fun longest_string_helper f xs =
    List.foldl (fn (x, acc) => if f(String.size(x), String.size(acc)) then x else acc) "" xs

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals 

val rev_string  = (implode o List.rev o explode)

fun first_answer f xs =
    case xs of 
        [] => raise NoAnswer
     | x :: xs' => case f(x) of
                       SOME(v) => v
                     | NONE => first_answer f xs'

fun all_answers f xs =
    let 
        fun all_answers_helper (x, acc) =
            case acc of
                NONE => NONE
	      | SOME(lst) => (case f(x) of
		                  NONE => NONE
				| SOME(ys) => SOME(lst @ ys))
    in
        List.foldl all_answers_helper (SOME []) xs
    end
                
val count_wildcards = g (fn () => 1) (fn (x) => 0) 

val count_wild_and_variable_lengths = g (fn () => 1) (fn (x) => String.size(x))

fun count_some_var(s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p = 
    let
        fun get_vars pat =
            case pat of
                Variable var => [var]
              | TupleP ps => List.foldl (fn (p, acc) => (get_vars p) @ acc) [] ps
              | ConstructorP(_, p) => get_vars(p)
              | _ => []
        fun does_duplicates_exist xs =
            case xs of 
                [] => false
	      | x :: xs' => does_duplicates_exist xs' orelse List.exists (fn y => x = y) xs'
    in
        not ((does_duplicates_exist o get_vars) p)
    end

fun match (value, pat) =
    case pat of
        Wildcard => SOME []
      | Variable x => SOME [(x, value)]
      | UnitP => (case value of 
                     Unit => SOME []
		   | _ => NONE)
      | ConstP i => (case value of Const j => if i = j then SOME [] else NONE | _ => NONE)
      | TupleP ps => (case value of 
                          Tuple vs => if List.length(ps) = List.length(vs) then
					  let val ziplist = ListPair.zip(ps, vs)
					  in
					      (all_answers (fn (p, v) => match(v, p)) ziplist)
					  end
                                      else NONE
			| _ => NONE)
      | ConstructorP(s1, p) => (case value of
                                    Constructor(s2, v) => if s1 = s2 then match(v, p)
                                                          else NONE
				  | _ => NONE)

fun first_match value ps =
    SOME(first_answer (fn p => match(value, p)) ps) handle NoAnswer => NONE
