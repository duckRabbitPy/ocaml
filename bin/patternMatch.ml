type my_type = String of string | Int of int

let isOdd num = num mod 2 = 1

let isValueOdd (x : my_type) : bool =
  match x with String s -> isOdd (int_of_string s) | Int i -> isOdd i
