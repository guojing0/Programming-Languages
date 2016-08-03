(* Problem 1 *)

fun same_string (s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (str, strlist) =
    case strlist of
        [] => NONE
      | s::strlist' => if same_string(s, str)
                       then SOME strlist'
                       else case all_except_option(str, strlist') of
                           NONE => NONE
                         | SOME lst => SOME (s :: lst)

fun get_substitutions1 (nlist, str) =
    case nlist of
        [] => []
      | n::nlist' => case all_except_option(str, n) of
                         NONE => [] @ get_substitutions1(nlist', str)
                       | SOME lst => lst @ get_substitutions1(nlist', str)

fun get_substitutions2 (nlist, str) =
    let fun helper (nlst, s, res) =
        case nlst of
            [] => res
          | n::nlst' => case all_except_option(s, n) of
                            NONE => helper(nlst', s, res)
                          | SOME lst => helper(nlst', s, res @ lst)
    in
        helper(nlist, str, [])
    end

fun similar_names (nlist, {first=x, last=z, middle=y}) =
    let fun f (nlst, s) =
        case nlst of
            [] => []
          | n::nlst' => {first=n, last=z, middle=y} :: f(nlst', s)
    in
        case nlist of
            [] => {first=x, last=z, middle=y} :: []
          | _ => {first=x, last=z, middle=y}
                 :: f(get_substitutions2(nlist, x), x)
    end

(* Problem 2 *)

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
                        andalso all_same_color (c2::rest)

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
    let fun helper (held_cards, clist, mvlist) =
        case mvlist of
            [] => score(held_cards, goal)
          | (Discard c)::rst  => helper(remove_card(held_cards, c, IllegalMove), clist, rst)
          | Draw::rst => case clist of
                             [] => score(held_cards, goal)
                           | cd::clist' => let val res = cd :: held_cards
                                           in
                                               if sum_cards(res) > goal
                                               then score(res, goal)
                                               else helper(res, clist', rst)
                                           end
    in
        helper([], clist, mvlist)
    end

