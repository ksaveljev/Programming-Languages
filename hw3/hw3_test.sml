val only_capitals_test1 = only_capitals([]) = []
val only_capitals_test2 = only_capitals(["hi","ho","hu"]) = []
val only_capitals_test3 = only_capitals(["Hi","Ho","Hu"]) = ["Hi","Ho","Hu"]
val only_capitals_test4 = only_capitals(["Hi","ho","Hu"]) = ["Hi","Hu"]

val longest_string1_test1 = longest_string1([]) = ""
val longest_string1_test2 = longest_string1([""]) = ""
val longest_string1_test3 = longest_string1(["a"]) = "a"
val longest_string1_test4 = longest_string1(["a","bb","ccc","dddd","eee"]) = "dddd"
val longest_string1_test5 = longest_string1(["a","bb","cc","dd"]) = "bb"

val longest_string2_test1 = longest_string2([]) = ""
val longest_string2_test2 = longest_string2([""]) = ""
val longest_string2_test3 = longest_string2(["a"]) = "a"
val longest_string2_test4 = longest_string2(["a","bb","ccc","dddd","eee"]) = "dddd"
val longest_string2_test5 = longest_string2(["a","bb","cc","dd"]) = "dd"

val longest_string3_test1 = longest_string3([]) = ""
val longest_string3_test2 = longest_string3([""]) = ""
val longest_string3_test3 = longest_string3(["a"]) = "a"
val longest_string3_test4 = longest_string3(["a","bb","ccc","dddd","eee"]) = "dddd"
val longest_string3_test5 = longest_string3(["a","bb","cc","dd"]) = "bb"

val longest_string4_test1 = longest_string4([]) = ""
val longest_string4_test2 = longest_string4([""]) = ""
val longest_string4_test3 = longest_string4(["a"]) = "a"
val longest_string4_test4 = longest_string4(["a","bb","ccc","dddd","eee"]) = "dddd"
val longest_string4_test5 = longest_string4(["a","bb","cc","dd"]) = "dd"

val longest_capitalized_test1 = longest_capitalized([]) = ""
val longest_capitalized_test2 = longest_capitalized(["a","bb","ccc"]) = ""
val longest_capitalized_test3 = longest_capitalized(["a","Bb","Ccc","DdDD","eEEE","ffffff"]) = "DdDD"
val longest_capitalized_test4 = longest_capitalized(["aaa","Bbb","CCC","Ddd","eeE"]) = "Ddd"

val rev_string_test1 = rev_string("") = ""
val rev_string_test2 = rev_string("abdef") = "fedba"
val rev_string_test3 = rev_string("AbCdEfG") = "GfEdCbA"

fun f n = if n = 6 then NONE else SOME n
val first_answer_test1 = (first_answer f []; false) handle NoAnswer => true
val first_answer_test2 = first_answer f [1] = 1
val first_answer_test3 = first_answer f [6,6,3,6] = 3
val first_answer_test4 = (first_answer f [6,6,6,6]; false) handle NoAnswer => true
val first_answer_test5 = first_answer f [3,4,5,6] = 3

fun f n = if n = 6 then NONE else SOME [n]
val all_answers_test1 = all_answers f [] = SOME []
val all_answers_test2 = all_answers f [1] = SOME [1]
val all_answers_test3 = all_answers f [1,2,3] = SOME[3,2,1]
val all_answers_test4 = all_answers f [1,2,6,3] = NONE

val count_wildcards_test1 = count_wildcards Wildcard = 1
val count_wildcards_test2 = count_wildcards (Variable "hi") = 0
val count_wildcards_test3 = count_wildcards (TupleP [Variable "hi", Wildcard, Variable "ha", Wildcard]) = 2
val count_wildcards_test4 = count_wildcards (ConstructorP("hi", TupleP[Wildcard, Variable "ha"])) = 1

val count_wild_and_variable_lengths_test1 = count_wild_and_variable_lengths Wildcard = 1
val count_wild_and_variable_lengths_test2 = count_wild_and_variable_lengths (Variable "hi") = 2
val count_wild_and_variable_lengths_test3 = count_wild_and_variable_lengths UnitP = 0
val count_wild_and_variable_lengths_test4 = count_wild_and_variable_lengths (ConstructorP("hi", TupleP[Wildcard, Variable "ha"])) = 3

val count_some_var_test1 = count_some_var("hi", Wildcard) = 0
val count_some_var_test2 = count_some_var("hi", (Variable "hi")) = 1
val count_some_var_test3 = count_some_var("hi", UnitP) = 0
val count_some_var_test4 = count_some_var("hi", TupleP[Wildcard, Variable "hi", Variable "ha"]) = 1
val count_some_var_test5 = count_some_var("hi", ConstructorP("hi", TupleP[Wildcard, Variable "hi", TupleP[Variable "hi", Variable "ho"]])) = 2


val check_pat_test1 = check_pat Wildcard = true
val check_pat_test2 = check_pat (Variable "hi") = true
val check_pat_test3 = check_pat (TupleP[Wildcard, Variable "hi", ConstructorP("hi", Variable "ho")]) = true
val check_pat_test4 = check_pat (TupleP[Wildcard, Variable "hi", ConstructorP("ho", Variable "hi")]) = false

val match_test1 = match(Const 1, Wildcard) = SOME []
val match_test2 = match(Unit, Wildcard) = SOME []
val match_test3 = match(Tuple[Const 1, Unit], Wildcard) = SOME []
val match_test4 = match(Constructor("hi", Const 3), Wildcard) = SOME []
val match_test5 = match(Const 1, Variable "hi") = SOME [("hi",Const 1)]
val match_test6 = match(Unit, Variable "hi") = SOME [("hi", Unit)]
val match_test7 = match(Tuple[Const 1, Unit], Variable "hi") = SOME [("hi",Tuple[Const 1, Unit])]
val match_test8 = match(Constructor("hi", Const 1), Variable "hi") = SOME [("hi",Constructor("hi", Const 1))]
val match_test9 = match(Const 1, UnitP) = NONE
val match_test10 = match(Unit, UnitP) = SOME []
val match_test11 = match(Tuple[Const 1, Unit], UnitP) = NONE
val match_test12 = match(Constructor("hi", Unit), UnitP) = NONE
val match_test13 = match(Const 1, ConstP 1) = SOME []
val match_test14 = match(Const 1, ConstP 2) = NONE
val match_test15 = match(Unit, ConstP 1) = NONE
val match_test16 = match(Tuple[Const 1, Unit], ConstP 1) = NONE
val match_test17 = match(Constructor("hi", Const 1), ConstP 1) = NONE
val match_test18 = match(Const 1, TupleP[ConstP 1]) = NONE
val match_test19 = match(Unit, TupleP[UnitP]) = NONE
val match_test20 = match(Constructor("hi", Const 1), TupleP[ConstP 1]) = NONE
val match_test21 = match(Tuple[Unit], TupleP[UnitP, UnitP]) = NONE
val match_test22 = match(Tuple[Unit, Unit], TupleP[UnitP]) = NONE
val match_test23 = match(Tuple[Const 1], TupleP[UnitP]) = NONE
val match_test24 = match(Tuple[Const 1], TupleP[ConstP 2]) = NONE
val match_test25 = match(Tuple[Const 1], TupleP[ConstP 1]) = SOME []
val match_test26 = match(Tuple[Unit], TupleP[Variable "hi"]) = SOME [("hi", Unit)]
val match_test27 = match(Tuple[Constructor("hi", Const 1)], TupleP[ConstructorP("hi", ConstP 1)]) = SOME []
val match_test28 = match(Const 1, ConstructorP("hi", ConstP 1)) = NONE
val match_test29 = match(Unit, ConstructorP("hi", UnitP)) = NONE
val match_test30 = match(Tuple[Unit], ConstructorP("hi", TupleP[UnitP])) = NONE
val match_test31 = match(Constructor("hi", Unit), ConstructorP("ho", UnitP)) = NONE
val match_test32 = match(Constructor("hi", Unit), ConstructorP("hi", UnitP)) = SOME []

val first_match_test1 = first_match (Const 1) [ConstP 1] = SOME []
val first_match_test2 = first_match (Const 1) [ConstP 2] = NONE
val first_match_test3 = first_match (Const 1) [ConstP 2, Variable "hi", ConstP 1] = SOME [("hi", Const 1)]
