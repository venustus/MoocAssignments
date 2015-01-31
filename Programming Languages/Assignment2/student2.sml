(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(e, lst) =
  let
    fun exclude(lst) =
      case lst of
           [] => []
         | h::lst' => if same_string(h,e) then exclude(lst') else h::exclude(lst')
  in
    let val filtered = exclude(lst)
    in
      if filtered = lst then NONE else SOME filtered
    end
  end

fun get_substitutions1(ls, e) =
  case ls of
       [] => []
     | h::ls' =>
         case all_except_option(e, h) of
             NONE => get_substitutions1(ls',e)
           | SOME lst => lst @ get_substitutions1(ls',e)

fun get_substitutions2(ls, e) =
  let
    fun aux(ls,acc) =
      case ls of
           [] => acc
         | h::ls' =>
             case all_except_option(e,h) of
                  NONE => aux(ls',acc)
                | SOME lst => aux(ls', acc @ lst)
  in
    aux(ls,[])
  end

fun similar_names(names, {first=f,middle=m,last=l}) =
  let
    fun helper(names) =
      case names of
           [] => []
         | first_name::names' => {first=first_name,last=l,middle=m}::helper(names')
  in
    {first=f,last=l,middle=m}::helper(get_substitutions1(names,f))
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

fun card_color(card) =
  case card of
       (Clubs,_) => Black
     | (Spades,_) => Black
     | (Diamonds,_) => Red
     | (Hearts,_) => Red

fun card_value(card) =
  case card of
       (_, Num n) => n
     | (_, Ace) => 11
     | (_,_) => 10

fun remove_card(cs, c, ex) =
  let
    fun aux(cs) =
      case cs of
           [] => raise ex
         | h::cs' => if h=c then cs' else h::aux(cs')
  in
    aux(cs)
  end

fun all_same_color(cs) =
  case cs of
       [] => true
     | h::[] => true
     | h::(n::cs') => if card_color(h) = card_color(n) then all_same_color(n::cs')
                    else false

fun sum_cards(cs) =
  let
    fun aux(cs,acc) =
      case cs of
           [] => acc
         | h::cs' => aux(cs',card_value(h) + acc)
  in
    aux(cs,0)
  end

fun score(cs,g) =
  let
    val p = sum_cards(cs)-g
    val s = if p>0 then p*3 else ~p
  in
    if all_same_color(cs) andalso s > 1 then s div 2 else s
  end

fun officiate(cardlist,moves,goal) =
  let
    fun helper(cardlist,player_cards,moves,current_score) =
      if current_score > goal
      then current_score
      else
        case cardlist of
             [] => current_score
           | top_card::cardlist' =>
                case moves of
                     [] => current_score
                   | (Discard card)::moves' =>
                              let val current_cards = remove_card(player_cards,card,IllegalMove)
                              in
                                 helper(cardlist,current_cards,moves',score(current_cards,goal))
                              end
                   | (DRAW)::moves' =>
                              let val current_cards = top_card::player_cards
                              in
                                helper(cardlist',current_cards,moves',score(current_cards,goal))
                              end
  in
    helper(cardlist,[],moves,score([],goal))
  end

(* 3. Challenge Problems *)

fun score_challenge(cs,g) =
  let
    fun repl_ace(cs) =
      case cs of
           [] => []
         | (color,suit)::cs' =>
             (case suit of
                  Ace => (color,Num(1))::cs'
                | _ => (color,suit)::repl_ace(cs'))
  in
    let
      val s = score(cs,g)
      val rs = repl_ace(cs)
      val srs = score(rs,g)
    in
      if s>0 andalso s>srs
      then score_challenge(rs,g)
      else s
    end
  end

fun officiate_challenge(cardlist,moves,goal) =
  let
    fun helper(cardlist,player_cards,moves,current_score) =
      if current_score > goal
      then current_score
      else
        case cardlist of
             [] => current_score
           | top_card::cardlist' =>
                case moves of
                     [] => current_score
                   | (Discard card)::moves' =>
                              let val current_cards = remove_card(player_cards,card,IllegalMove)
                              in
                                 helper(cardlist,current_cards,moves',score_challenge(current_cards,goal))
                              end
                   | (DRAW)::moves' =>
                              let val current_cards = top_card::player_cards
                              in
                                helper(cardlist',current_cards,moves',score_challenge(current_cards,goal))
                              end
  in
    helper(cardlist,[],moves,score_challenge([],goal))
  end

fun careful_player(cardlist,goal) =
  let
    fun helper(current_cards,hand) =
      let
        val hand_sum = sum_cards(hand)
      in
        case current_cards of
             [] => if 11-hand_sum>=0 then Draw::[] else []
           | last::[] => if sum_cards(last::hand) > goal then (Discard last)::[]
                         else Draw::[]
           | first::second::cards' =>
                   if sum_cards(second::hand) = 0 orelse sum_cards(first::hand) > goal
                   then (Discard first)::helper(second::cards',hand)
                   else Draw::helper(second::cards',first::hand)
      end
  in
    helper(cardlist,[])
  end
