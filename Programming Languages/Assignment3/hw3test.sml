(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3provided.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_1 = only_capitals ["Hi", "venkat", "Here"] = ["Hi", "Here"]
val test1_2 = only_capitals ["hi", "venkat", "here"] = []


val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_1 = longest_string1 ["A", "bc", "C", "cd"] = "bc"
val test2_2 = longest_string1 ["A", "bc", "C", "cde"] = "cde"
val test2_3 = longest_string1 [] = ""

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_1 = longest_string2 ["A", "bc", "C", "cd"] = "cd"
val test3_2 = longest_string2 ["A", "bc", "C", "cde"] = "cde"
val test3_3 = longest_string2 [] = ""

val test4a= longest_string3 ["A","bc","C"] = "bc"
val test4a_1 = longest_string3 ["A", "bc", "C", "cd"] = "bc"
val test4a_2 = longest_string3 ["A", "bc", "C", "cde"] = "cde"
val test4a_3 = longest_string3 [] = ""

val test4b= longest_string4 ["A","B","C"] = "C"
val test4b_1 = longest_string4 ["A", "bc", "C", "cd"] = "cd"
val test4b_2 = longest_string4 ["A", "bc", "C", "cde"] = "cde"
val test4b_3 = longest_string4 [] = ""


val test5 = longest_capitalized ["A","bc","C"] = "A";
val test5_1 = longest_capitalized ["A", "Bc", "C", "Cd"] = "Bc"
val test5_2 = longest_capitalized ["a", "bc", "c", "cd"] = ""
val test5_3 = longest_capitalized [] = ""


val test6 = rev_string "abc" = "cba";
val test6_1 = rev_string "" = "";
val test6_2 = rev_string "a" = "a";


val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7_1 = (first_answer (fn x => NONE) [1, 2, 3, 4, 5] handle NoAnswer => 0) = 0

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val test8_2 = all_answers (fn x => if x < 4 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_3 = all_answers (fn x => if x > 10 then NONE else SOME([x + 1, x + 2])) [2,3,4,5,6,7] = SOME [3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9]


val test9a = count_wildcards Wildcard = 1
val test9a_1 = (count_wildcards (TupleP [Wildcard, TupleP [Variable "t", Wildcard]])) = 2

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_1 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable("a"), TupleP [Variable("ab"), Variable("cde"), Wildcard]]) = 8

val test9c = count_some_var ("x", Variable("x")) = 1;
val test9c_1 = count_some_var ("x", Variable("y")) = 0;
val test9c_2 = count_some_var("ab", (TupleP [Wildcard, Variable("ab"), TupleP [Variable("ab"), Variable("ab"), Wildcard]])) = 3

val test10 = check_pat (Variable("x")) = true
val test10_1 = check_pat(TupleP [Variable("x"), Variable("x")]) = false
val test10_2 = check_pat((TupleP [Wildcard, Variable("a"), TupleP [Variable("ab"), Variable("cde"), ConstructorP("cdef", Variable("efg"))]])) = true
val test10_3 = check_pat((TupleP [Wildcard, Variable("ab"), TupleP [Variable("ab"), Variable("cde"), ConstructorP("cdef", Variable("efg"))]])) = false


val test11 = match (Const(1), UnitP) = NONE
val test11_1 = match(Const(1), ConstP(1)) = SOME []
val test11_2 = match(Const(1), ConstP(2)) = NONE
val test11_3 = match(Const(1), Wildcard) = SOME []
val test11_4 = match(Const(1), Variable("a")) = SOME [("a", Const(1))]
val test11_5 = match(Tuple [Const(1), Const(5), Tuple [Unit, Const(6), Constructor("cdef", Const(7))]], TupleP [Wildcard, Variable("ab"), TupleP [Variable("cd"), Variable("cde"), ConstructorP("cdef", Variable("efg"))]]) = SOME [("ab", Const(5)), ("cd", Unit), ("cde", Const(6)), ("efg", Const(7))]
val test11_6 = match(Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],TupleP[Wildcard,Wildcard]) = NONE

val test12 = first_match Unit [UnitP] = SOME []
val test12_1 = first_match Unit [Variable("a")] = SOME [("a", Unit)]


