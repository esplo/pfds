exception Foo;

fun update([], i, y) = raise Subscript
    | update(x::xs, 0, y) = y :: xs
    | update(x::xs, i, y) = x :: update(xs, i-1, y);

fun suffixes([]) = [[]]
    | suffixes(u as x::xs) = [u] @ suffixes(xs)

(* test *)
val case1 = [1, 2, 3, 4];

val res1 = suffixes(case1);
val ans1 = [[1,2,3,4], [2,3,4], [3,4], [4], []];

print("== test 1\n");
if res1 <> ans1 then raise Foo else "AC";

(* info *)
(*

Q. O(n)で生成できるのはなぜ？
A. suffixesは必ずtlを取るため、引数のリストの長さは1ずつ減っていくため。

Q. O(n)の空間になるのはなぜ？
A. suffixesは「与えられたリスト」の各要素に対する参照を持つため
*)
