(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1a. funtion all_except_option takes a string and a string list. 
       Return NONE if the string is not in the list, else return SOME lst 
       where lst is identical to the argument list except the string is not in it. *)

(* string * ( string list ) -> string list option*)
fun all_except_option (str, str_list) =
    let fun all_except(str_lst) = 
	case str_lst of [] => []
		      | x::xs  => if same_string (str, x)
				  then x :: all_except (xs)
				  else all_except(xs)
    in
	case all_except(str_list) of [] => NONE
				  | x' => SOME x' 
    end

(* 1b. function get_substitutions1 *)
(* string list * string -> string list *) 
fun get_substitutions1 (xss, s) = 
    case xss of [] => [] 
		   | xs''::xss'' => case all_except_option (s, xs'') of NONE => get_substitutions1(xss'', s)
								      | SOME x => x @ get_substitutions1(xss'', s)   

(* 1c.  function get_substitutions2 *)
(* string list list * string -> string list *)
fun get_substitutions2 (xss, s) = 
    let fun helper (x, acc) =
	    case x of [] => []
		    |  xs'::xss' => case all_except_option(s, xs') of NONE => helper (xss', acc)
								   | SOME x => helper (xss', acc @ x) 
    in
	case xss of [] => []
		 | _ => helper (xss,[])
    end
   
(* 1d.  function similar_names *)
fun similar_names (str, {first=f, last=l, middle=m}) =
    let fun builder (cs) =
	    case cs of [] => []
		    |  cs::css => {first=cs, last=l, middle=m} :: builder(css)
    in
	builder( get_substitutions1(str, f))
    end
															       
(* 2.  Solitaire card game - tracking the progress of a game *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* Solutions for problem 2 here *)

(* 2a. function card_color takes a card and returns its color (spades and clubs are black,
       diamonds and hearts are red).
 *)
fun card_color (card_suit, card_rank) = 
    case card_suit of Clubs => Black
		       | Spades => Black
		       | Hearts => Red 
		       | Diamonds => Red
    
(* 2b  function card_value takes a card and returns its value (numbered cards have their
number as the value, aces are 11, everything else is 10). *)
(* suit * rank -> int *)
fun card_value (card_suit, card_rank) = 
    case card_rank of Ace  => 11
		    | _    => 10 

(* 2c  function remove_card, which takes a list of cards cs, a card c, and an exception e. 
       It returns a list that has all the elements of cs except c. 

       If c is in the list more than once, remove only the 1st one.
       If c is not in the list, raise the exception e *)
(* (suit*rank) * (suit*rank) -> bool *)

(* card * card -> bool *)
fun same_card (c1: card, c2: card)= 
    let val (x1,y1) = c1
	val (x2,y2) = c2 
    in
	(x1 = x2) andalso (y1 = y2)
    end

(* card list * card -> card list *)
fun remove_card (c_lst: card list, c: card) =
    let fun remove_if_found (lst: card list) =
	case lst of [] => []
		  | x::xss => if same_card(x, c) 
			      then xss
			      else x:: remove_if_found (xss)
    in
	remove_if_found(c_lst)
    end

(* 2d  function all_same_color, which takes a list of cards and returns true if all the cards in the
       list are the same color
 *)

fun all_same_color (cs) =
    let fun color(s) =
	    case s of Clubs => Black
		    | Spades => Black
		    | Hearts => Red 
		    | Diamonds => Red 
    in
	case cs of [] => true
		 | (s,r)::[] => true
		 | (s1,r1)::(s2,r2)::[] => ( color(s1) = color(s2) )
		 | (s1',r1')::(s2',r2')::rest => if color(s1') = color(s2')
						 then all_same_color((s2',r2')::rest)
						 else false
    end

(* 2e  function sum_cards, which takes a list of cards and 
       returns the sum of their values. *)
fun sum_cards (cs: card list) =
    case cs of [] => 0
	     | cs'::css' => card_value(cs') + sum_cards(css') 
 
(* 2f  function score takes a card list (the held-cards) and an int (the goal) and computes
       the score. *)
fun score (card_list, goal) = 
    let val sum = sum_cards(card_list)
	val preliminary_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in
	case card_list of [] => 0
			| _ => if all_same_color(card_list)
			       then preliminary_score div 2
			       else preliminary_score
    end  

(* 2g. *)
(* definition *)
 
fun draw_a_card (a_card, held_cards) = 
    a_card :: held_cards

fun in_list (item, item_list) =
    case item_list of [] => false
		    | xs::xss => if ( xs = item) then true else in_list (item, xss) 

fun officiate ([], moves, goal) = score([], goal)
  | officiate (held_cards, [], goal ) = score(held_cards, goal)


  | officiate (held_cards, Draw :: next_moves, goal)  =
    let val card_list = [ (Clubs, Ace), (Clubs, King), (Clubs, Queen), (Diamonds, Num(10)), (Hearts, Num(8)), (Spades, Num(9))]
    in
	case card_list of [] => score (held_cards, goal)
			| xs::xss => if sum_cards (xs :: held_cards) > goal 
				     then score (xs :: held_cards, goal) 
				     else officiate (xs :: held_cards, next_moves, goal)	    
    end

  | officiate (held_cards, Discard(a_card) :: moves, goal) = 
    if not ( in_list (a_card, held_cards))
    then raise IllegalMove
    else let val held_cards_next_turn = remove_card(held_cards, a_card)
	 in
	     if sum_cards (held_cards)> goal 
	     then score(held_cards, goal) 
	     else officiate(held_cards_next_turn, moves, goal)
	end
(* 3. Challenge Problems *)

(* (a) score_challenge *)
fun score_challenge (card_list, goal) = 
    let fun card_value11 (card_suit, card_rank) = 
	    case card_rank of Ace  => 11
			    | _    => 10 
	fun card_value1 (card_suit, card_rank) = 
	    case card_rank of Ace  => 1
			    | _    => 10 
	fun sum_cards1 (cs: card list) =
	    case cs of [] => 0
		     | cs'::css' => card_value1(cs') + sum_cards(css') 

	fun sum_cards11 (cs: card list) =
	    case cs of [] => 0
		     | cs'::css' => card_value11(cs') + sum_cards(css') 

	val sum1 = sum_cards1(card_list)
	val preliminary_score1 = if sum1 > goal then 3 * (sum1 - goal) else (goal - sum1)
	val sum11 = sum_cards11(card_list)
	val preliminary_score11 = if sum11 > goal then 3 * (sum11 - goal) else (goal - sum11)

	val score1 = case card_list of [] => 0
				     | _ => if all_same_color(card_list)
					    then preliminary_score1 div 2
					    else preliminary_score1
	val score11 = case card_list of [] => 0
				      | _ => if all_same_color(card_list)
					     then preliminary_score11 div 2
					     else preliminary_score11
    in
	if score1 < score11 then score1 else score11
    end  

(* (a) officiate_challenge *)
fun officiate_challenge ([], moves, goal) = score_challenge([], goal)
  | officiate_challenge (held_cards, [], goal ) = score_challenge(held_cards, goal)


  | officiate_challenge (held_cards, Draw :: next_moves, goal)  =
    let val card_list = [ (Clubs, Ace), (Clubs, King), (Clubs, Queen), (Diamonds, Num(10)), (Hearts, Num(8)), (Spades, Num(9))]
    in
	case card_list of [] => score_challenge (held_cards, goal)
			| xs::xss => if sum_cards (xs :: held_cards) > goal 
				     then score_challenge (xs :: held_cards, goal) 
				     else officiate_challenge (xs :: held_cards, next_moves, goal)	    
    end

  | officiate_challenge (held_cards, Discard(a_card) :: moves, goal) = 
    if not ( in_list (a_card, held_cards))
    then raise IllegalMove
    else let val held_cards_next_turn = remove_card(held_cards, a_card)
	 in
	     if sum_cards (held_cards)> goal 
	     then score_challenge(held_cards, goal) 
	     else officiate_challenge(held_cards_next_turn, moves, goal)
	end

(* (b) careful_player *)
