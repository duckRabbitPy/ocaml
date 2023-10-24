(* cd learningOcaml  *)
(*  dune exec learningOcaml *)

type coordinate = { x : int; y : int }
type directions = UP | DOWN | LEFT | RIGHT
type vector = { direction : directions; magnitude : int }

let incrementXaxis c (increment : int) = { c with x = c.x + increment }
let incrementYaxis c (increment : int) = { c with y = c.y + increment }
let decrementXaxis c (decrement : int) = { c with x = c.x - decrement }
let decrementYaxis c (decrement : int) = { c with y = c.y - decrement }

let string_of_coordinate c =
  "x = " ^ string_of_int c.x ^ "; y = " ^ string_of_int c.y

let initialPos = { x = 0; y = 0 }

let moveInDirection c v =
  match v with
  | { direction = UP; magnitude } -> incrementYaxis c magnitude
  | { direction = DOWN; magnitude } -> decrementYaxis c magnitude
  | { direction = LEFT; magnitude } -> decrementXaxis c magnitude
  | { direction = RIGHT; magnitude } -> incrementXaxis c magnitude

let applyDirections c (vector : vector list) =
  List.fold_left moveInDirection c vector

let direction_of_string s =
  match s with
  | "U" -> UP
  | "D" -> DOWN
  | "L" -> LEFT
  | "R" -> RIGHT
  | _ -> failwith "Invalid direction"

let directions =
  [
    "U2";
    "D100";
    "L4";
    "R5";
    "U10";
    "D88";
    "L12";
    "R13";
    "U5";
    "D15";
    "L1";
    "R99";
  ]

let parseDirections =
  List.map (fun s ->
      {
        direction = direction_of_string (String.sub s 0 1);
        magnitude = int_of_string (String.sub s 1 (String.length s - 1));
      })

let () =
  applyDirections initialPos (parseDirections directions)
  |> string_of_coordinate |> print_endline
