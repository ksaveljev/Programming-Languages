(* Dan Grossman, Coursera PL, HW2 Provided Tests *)

val all_except_option_test1 = all_except_option("1", []) = NONE
val all_except_option_test2 = all_except_option("1", ["1", "2", "3"]) = SOME(["2", "3"])
val all_except_option_test3 = all_except_option("2", ["1", "2", "3"]) = SOME(["1", "3"])
val all_except_option_test4 = all_except_option("3", ["1", "2", "3"]) = SOME(["1", "2"])
val all_except_option_test5 = all_except_option("4", ["1", "2", "3"]) = NONE

val get_substitutions1_test1 = get_substitutions1([["Fred","Frederick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Frederick","Freddie","F"]
val get_substitutions1_test2 = get_substitutions1([["Fred","Frederick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val get_substitutions2_test1 = get_substitutions2([["Fred","Frederick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Frederick","Freddie","F"]
val get_substitutions2_test2 = get_substitutions2([["Fred","Frederick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val similar_names_test1 = similar_names([["Fred","Frederick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}, {first="Frederick", last = "Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
val similar_names_test2 = similar_names([], {first="test", middle="test", last="test"}) = [{first="test", middle="test", last="test"}]


(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)
val card_color_test1 = card_color((Spades, Num 8)) = Black
val card_color_test2 = card_color((Clubs, Ace)) = Black
val card_color_test3 = card_color((Hearts, Num 3)) = Red
val card_color_test4 = card_color((Diamonds, Jack)) = Red

val card_value_test1 = card_value((Spades, Num 8)) = 8
val card_value_test2 = card_value((Clubs, Ace)) = 11
val card_value_test3 = card_value((Diamonds, Jack)) = 10

val remove_card_test1 = remove_card([(Spades, Ace),(Diamonds,Jack)], (Spades, Ace), IllegalMove) = [(Diamonds,Jack)]

val all_same_color_test1 = all_same_color([(Spades, Ace),(Spades, Num 3)]) = true
val all_same_color_test2 = all_same_color([(Spades, Ace),(Spades, Num 3),(Hearts, Jack)]) = false

val sum_cards_test1 = sum_cards([(Spades, Ace),(Spades, Num 3),(Hearts, Jack)]) = 24

fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end

fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end
