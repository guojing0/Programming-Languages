fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* all_except_option("hello", ["fuck", "sleep", "hello", "mute", "sick"]); *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color card =
    case card of
        (x, _) => if (x = Clubs orelse x = Spades) then Black else Red

fun card_value card =
    case card of
        (_, Num i) => i
      | (_, Ace) => 11
      | _ => 10

(* TO BE THOUGHT *)
fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
      | card::cs' => if card = c
                     then cs'
                     else card :: remove_card(cs', c, e)

fun all_same_color cards =
    case cards of
        [] => true
      | _::[] => true
      | c1::c2::rest => card_color c1 = card_color c2
                        andalso all_same_color rest

fun sum_cards cards =
    let fun helper (c, sum) =
        case c of
            [] => sum
          | v::c' => helper(c', card_value v + sum)
    in
        helper(cards, 0)
    end

fun score (held_cards, goal) =
    let val sum = sum_cards held_cards
        val pre_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in
        if all_same_color held_cards then pre_score div 2 else pre_score
    end

fun officiate (clist, mvlist, goal) =
    let fun helper (clist, mvlist, goal) =
        case (clist, mvlist, goal) of
            ([], _, _) => score
            (_, Discard c, _) => XXX
            (_, Draw, _) =>

