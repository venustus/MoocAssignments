(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s, sl) =
        case sl of 
            [] => NONE
          | x::xs => if same_string(s, x)
                     then SOME xs
                     else let val res = all_except_option(s, xs)
                          in
                              case res of
                                  NONE => NONE
                                | SOME ys => SOME (x :: ys)
                          end

fun get_substitutions1(names, s) = 
    case names of 
        [] => []
      | l :: ls => case all_except_option(s, l) of
                       SOME xs => xs @ get_substitutions1(ls, s)
                     | NONE => get_substitutions1(ls, s)

fun get_substitutions2(names, s) = 
    let fun get_substitutions2_helper(xs, acc) =
	case xs of 
	    [] => acc
	  | l :: ls => case all_except_option(s, l) of
			   SOME xs => get_substitutions2_helper(ls, acc @ xs)
			 | NONE => get_substitutions2_helper(ls, acc)
    in
        get_substitutions2_helper(names, [])
    end

fun similar_names(substitutions, {first = f, middle = m, last = l}) =
    let val first_names = get_substitutions2(substitutions, f)
        fun prepare_names(xs) = 
            case xs of 
                [] => []
	      | y :: ys => {first = y, middle = m, last = l} :: prepare_names(ys)
    in
        {first = f, middle = m, last = l} :: prepare_names(first_names)
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(s, r) =
    case s of 
        Spades => Black
      | Clubs => Black
      | _ => Red

fun card_value(s, r) =
    case r of 
        Num(x) => x
      | Ace => 11
      | _ => 10

fun remove_card(cs, c, e) =
    case cs of 
        [] => raise e
      | x :: xs => if x = c then xs else x :: remove_card(xs, c, e)

fun all_same_color(cs) = 
    case cs of 
        [] => true
     |  x :: [] => true
     |  x :: y :: xs => card_color(x) = card_color(y) andalso all_same_color(y :: xs)

fun sum_cards(cs) = 
    let fun sum_cards_helper(xs, acc) =
            case xs of 
                [] => acc
              | y :: ys => sum_cards_helper(ys, acc + card_value(y))
    in
        sum_cards_helper(cs, 0)
    end 

fun score(held_cards, goal) =
    let val sum = sum_cards(held_cards)
        val preliminary_score = if sum > goal 
                                then 3 * (sum - goal) 
                                else (goal - sum)
    in
         if all_same_color(held_cards) 
         then preliminary_score div 2 
         else preliminary_score
    end


fun officiate(cards, moves, goal) =
    let fun get_game_score(held_cards, card_list, moves_left) =
            case moves_left of
                [] => score(held_cards, goal)
	      | Discard(card) :: other_moves => get_game_score(remove_card(held_cards, card, IllegalMove), card_list, other_moves)
              | Draw :: other_moves => (case card_list of
			                   [] => score(held_cards, goal)
                                         | y :: ys => if sum_cards(y :: held_cards) > goal 
                                                      then score(y :: held_cards, goal)
                                                      else get_game_score(y :: held_cards, ys, other_moves))
    in
        get_game_score([], cards, moves)
    end
