(* Dan Grossman, CSE341 Winter 2013, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 9
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*==================================================*)

fun all_except_option(opt, strings) =
   let
     fun except_option_rec(acc, curr_strings) =
       case curr_strings of
            [] => NONE
            |x::tl => if opt=x
                     then SOME(rev(acc) @ tl)
                     else except_option_rec(x::acc, tl)
   in except_option_rec([], strings)
   end
   
fun get_substitutions1(subs, opt) =
  case subs of
       [] => []
     |x::tl => case all_except_option(opt, x) of
                    NONE => get_substitutions1(tl, opt) 
                  |SOME(l) => l @ get_substitutions1(tl, opt)

fun get_substitutions2(subs, opt) =
let
  fun get_substitutions2_rec(acc, curr_subs) =
   case curr_subs of
        [] => acc
      |x::tl => case all_except_option(opt, x) of
                     NONE => get_substitutions2_rec(acc,tl)
                   |SOME(l) => get_substitutions2_rec(acc @ l, tl)
in get_substitutions2_rec([], subs)
end

fun similar_names(subs, full_name) =
   let
     val {first=f, middle=m, last=l} = full_name
     fun aux(curr_firsts) =
         case curr_firsts of
              [] => []
            |hd::tl => {first=hd, middle=m, last=l}::aux(tl)
   in full_name::aux(get_substitutions2(subs, f))
   end

(*==================================================*)

fun card_color(card) =
  case card of
       (Spades, _) => Black
     |(Clubs, _) => Black
     |(Hearts, _) => Red
     |(Diamonds, _) => Red

fun card_value(card) =
  case card of
       (_, Num(v)) => v
     |(_, Ace) => 11
     | _ => 10

fun remove_card(cards, card, except) =
  case all_except_option(card, cards) of
       NONE => raise except
     |SOME(updated_cards) => updated_cards

fun all_same_color(cards) =
  case cards of
       [] => true
     | card::[] => true
     | card1::(card2::tl) => (card_color(card1)=card_color(card2)) andalso all_same_color(card2::tl)

fun sum_cards(cards) =
   let
     fun aux(curr_cards, acc) =
       case curr_cards of
            [] => acc
          |hd::tl => aux(tl, acc+card_value(hd))
   in aux(cards, 0)
   end

fun score(held_cards, goal) =
   let
     val diff = sum_cards(held_cards) - goal
     val pre_score = if (diff < 1) then ~diff else diff*3
   in
     if all_same_color(held_cards)
     then pre_score div 2
     else pre_score
   end

fun officiate(cards, moves, goal) =
   let
     fun play_rec(curr_cards, curr_moves, held_cards) =
       case (curr_cards, curr_moves) of
            ([],Draw::_) => score(held_cards, goal)
          | (_,[]) => score(held_cards, goal)
          | (cc_hd::cc_tl, Draw::cm_tl) =>
              if sum_cards(cc_hd::held_cards) > goal
              then score(cc_hd::held_cards, goal)
              else play_rec(cc_tl, cm_tl, cc_hd::held_cards)
          | (_, Discard(cm_hd)::cm_tl) => play_rec(curr_cards, cm_tl, remove_card(held_cards, cm_hd, IllegalMove))
   in
     play_rec(cards, moves, [])
   end

(*==================================================*)

fun count_aces(cards) = 
  case cards of
       [] => 0
     |(_,Ace)::cd_tl => 1 + count_aces(cd_tl)
     |_::cd_tl => 0 + count_aces(cd_tl)

fun lower_n_aces_value(cards, n_aces) = 
  if n_aces = 0
  then cards
  else
    case cards of
         (set,Ace)::cd_tl => (set, Num(1))::lower_n_aces_value(cd_tl, n_aces-1)
       |hd::cd_tl => hd::lower_n_aces_value(cd_tl, n_aces-1)
       | _ => []

fun score_challenge(held_cards, goal) =
   let
     val n_aces = count_aces(held_cards)
     fun search_score(curr_score, curr_n_aces) =
       if n_aces = curr_n_aces 
       then curr_score
       else if curr_score > score(lower_n_aces_value(held_cards, curr_n_aces+1), goal)
       then search_score(score(lower_n_aces_value(held_cards, curr_n_aces+1), goal), curr_n_aces+1)
       else curr_score
   in
     search_score(score(held_cards, goal), 0)
   end

fun officiate_challenge(cards, moves, goal) =
   let
     fun aux(curr_cards, curr_moves, held_cards) =
       case (curr_cards, curr_moves) of
            ([],Draw::_) => score_challenge(held_cards, goal)
          |(_,[]) => score_challenge(held_cards, goal)
          |(cc_hd::cc_tl, Draw::cm_tl) =>
              if sum_cards(cc_hd::held_cards) >= goal
              then score_challenge(cc_hd::held_cards, goal)
              else aux(cc_tl, cm_tl, cc_hd::held_cards)
          |(_, Discard(cm_hd)::cm_tl) => aux(curr_cards, cm_tl, remove_card(held_cards, cm_hd, IllegalMove))
   in
     aux(cards, moves, [])
   end

(*==================================================*)

fun play_moves(cards, moves) =
   let
      fun moves_rec(curr_cards, curr_hands, curr_moves) =
        case (curr_cards, curr_hands,curr_moves) of
            (_,_,[]) => rev(curr_hands)
          | (_,[],Discard(_)::_) => rev(curr_hands)
          | ([],_,Draw::_) => rev(curr_hands)
          | (hd_cd::tl_cd, _, Draw::tl_mv) => moves_rec(tl_cd, hd_cd::curr_hands, tl_mv)
          | (_, hd_hnd::tl_hnd, Discard(_)::tl_mv) => moves_rec(curr_cards, tl_hnd, tl_mv)
   in moves_rec(cards, [], moves)
   end


fun careful_player(cards, goal) =
   let
     fun select_moves(curr_cards, moves) =
       let
         val curr_play = play_moves(cards, rev(moves))
         val curr_score = score(curr_play, goal) 
         val diff = goal - sum_cards(curr_play)
       in
         (*print ("curr_score is: " ^ Int.toString(curr_score) ^ "\n");*)
         (*print ("diff is: " ^ Int.toString(diff) ^ "\n");*)
         if curr_score = 0
         then rev(moves)
         else if diff <= 11
         then
           case curr_cards of
                hd_cd0::hd_cd1::tl => 
                  let
                    val cd0_value = sum_cards([hd_cd0])
                    val discard_moves = Draw::Discard(hd_cd0)::Draw::moves
                    val discard_play = play_moves(cards, rev(discard_moves))
                    val discard_score =  score(discard_play, goal)
                  in
                    (*print ("discard_score is: " ^ Int.toString(discard_score) ^ "\n");*)
                    if diff = 11 andalso cd0_value = 11
                    then rev(Draw::moves)
                    else if discard_score = 0
                    then rev(discard_moves)
                    else if diff = 11
                    then select_moves(hd_cd1::tl, Draw::moves)
                    else rev(moves)
                  end
              | hd_cd0::[] => if diff = 11
                              then rev(Draw::moves)
                              else rev(moves)
         else
           case curr_cards of
                hd_cd::tl_cd => select_moves(tl_cd, Draw::moves)
              | _ => rev(moves)
       end
   in select_moves(cards, [])
   end



