type colour =
  | Red
  | Blue
  | Green
  | Orange
  | Yellow
  | Black

type position =
  { x : int
  ; y : int
  }

type square =
  { colour : colour
  ; pos : position
  ; queen_present : bool
  }

type q_board =
  { x_lower : int
  ; x_upper : int
  ; y_lower : int
  ; y_upper : int
  ; squares : square list
  }

let colour_to_string (c : colour) : string =
  match c with
  | Red -> "red"
  | Black -> "black"
  | Blue -> "blue"
  | Green -> "green"
  | Yellow -> "yellow"
  | Orange -> "orange"
;;

let print_q_board (b : q_board) =
  Printf.printf "x_lower: %d\n" b.x_lower;
  Printf.printf "x_upper: %d\n" b.x_upper;
  Printf.printf "y_lower: %d\n" b.y_lower;
  Printf.printf "y_upper: %d\n" b.y_upper;
  let rec print_square (sq : square list) =
    match sq with
    | [] -> ()
    | head :: tail ->
      Printf.printf "x: %d ; y %d\n" head.pos.x head.pos.y;
      print_endline (colour_to_string head.colour);
      if head.queen_present then print_endline "queen_present" else print_endline "";
      print_square tail
  in
  print_square b.squares
;;
