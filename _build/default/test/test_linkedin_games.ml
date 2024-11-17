open Queens__.Types

let%test_unit "Test 5x5 queen board" =
  let ( => ) = [%test_eq: Base.bool] in
  Queens.valid_board [ { x = 0; y = 0 }; { x = 0; y = 0 } ] => false;
  Queens.valid_board [ { x = 0; y = 0 }; { x = 1; y = 0 } ] => false;
  Queens.valid_board [ { x = 0; y = 0 }; { x = 2; y = 0 } ] => true;
  Queens.valid_board [ { x = 0; y = 0 }; { x = 1; y = 1 } ] => false;
  Queens.valid_board [ { x = 2; y = 2 }; { x = 3; y = 3 } ] => false;
  Queens.valid_board [ { x = 0; y = 0 }; { x = 3; y = 0 }; { x = 0; y = 3 } ] => true
;;

let four_by_four : square list =
  [ { colour = Red; pos = { x = 0; y = 0 }; queen_present = false }
  ; { colour = Red; pos = { x = 1; y = 0 }; queen_present = false }
  ; { colour = Green; pos = { x = 2; y = 0 }; queen_present = true }
  ; { colour = Red; pos = { x = 3; y = 0 }; queen_present = false }
  ; { colour = Yellow; pos = { x = 0; y = 1 }; queen_present = true }
  ; { colour = Red; pos = { x = 1; y = 1 }; queen_present = false }
  ; { colour = Red; pos = { x = 2; y = 1 }; queen_present = false }
  ; { colour = Red; pos = { x = 3; y = 1 }; queen_present = false }
  ; { colour = Red; pos = { x = 0; y = 2 }; queen_present = false }
  ; { colour = Red; pos = { x = 1; y = 2 }; queen_present = false }
  ; { colour = Red; pos = { x = 2; y = 2 }; queen_present = false }
  ; { colour = Blue; pos = { x = 3; y = 2 }; queen_present = true }
  ; { colour = Red; pos = { x = 0; y = 3 }; queen_present = false }
  ; { colour = Red; pos = { x = 1; y = 3 }; queen_present = true }
  ; { colour = Red; pos = { x = 2; y = 3 }; queen_present = false }
  ; { colour = Red; pos = { x = 3; y = 3 }; queen_present = false }
  ]
;;

let bad_axis_four_by_four : square list =
  [ { colour = Red; pos = { x = 0; y = 0 }; queen_present = false }
  ; { colour = Red; pos = { x = 1; y = 0 }; queen_present = false }
  ; { colour = Green; pos = { x = 2; y = 0 }; queen_present = true }
  ; { colour = Red; pos = { x = 3; y = 0 }; queen_present = false }
  ; { colour = Yellow; pos = { x = 0; y = 1 }; queen_present = true }
  ; { colour = Red; pos = { x = 1; y = 1 }; queen_present = false }
  ; { colour = Red; pos = { x = 2; y = 1 }; queen_present = false }
  ; { colour = Red; pos = { x = 3; y = 1 }; queen_present = false }
  ; { colour = Red; pos = { x = 0; y = 2 }; queen_present = false }
  ; { colour = Blue; pos = { x = 1; y = 2 }; queen_present = true }
  ; { colour = Red; pos = { x = 2; y = 2 }; queen_present = false }
  ; { colour = Red; pos = { x = 3; y = 2 }; queen_present = false }
  ; { colour = Red; pos = { x = 0; y = 3 }; queen_present = false }
  ; { colour = Red; pos = { x = 1; y = 3 }; queen_present = true }
  ; { colour = Red; pos = { x = 2; y = 3 }; queen_present = false }
  ; { colour = Red; pos = { x = 3; y = 3 }; queen_present = false }
  ]
;;

let too_many_colour_four_by_four : square list =
  [ { colour = Red; pos = { x = 0; y = 0 }; queen_present = false }
  ; { colour = Red; pos = { x = 1; y = 0 }; queen_present = false }
  ; { colour = Green; pos = { x = 2; y = 0 }; queen_present = true }
  ; { colour = Red; pos = { x = 3; y = 0 }; queen_present = false }
  ; { colour = Yellow; pos = { x = 0; y = 1 }; queen_present = true }
  ; { colour = Red; pos = { x = 1; y = 1 }; queen_present = false }
  ; { colour = Red; pos = { x = 2; y = 1 }; queen_present = false }
  ; { colour = Red; pos = { x = 3; y = 1 }; queen_present = false }
  ; { colour = Red; pos = { x = 0; y = 2 }; queen_present = false }
  ; { colour = Red; pos = { x = 1; y = 2 }; queen_present = false }
  ; { colour = Red; pos = { x = 2; y = 2 }; queen_present = false }
  ; { colour = Red; pos = { x = 3; y = 2 }; queen_present = true }
  ; { colour = Red; pos = { x = 0; y = 3 }; queen_present = false }
  ; { colour = Red; pos = { x = 1; y = 3 }; queen_present = true }
  ; { colour = Red; pos = { x = 2; y = 3 }; queen_present = false }
  ; { colour = Red; pos = { x = 3; y = 3 }; queen_present = false }
  ]
;;

let not_all_colour_four_by_four : square list =
  [ { colour = Red; pos = { x = 0; y = 0 }; queen_present = false }
  ; { colour = Red; pos = { x = 1; y = 0 }; queen_present = false }
  ; { colour = Green; pos = { x = 2; y = 0 }; queen_present = true }
  ; { colour = Red; pos = { x = 3; y = 0 }; queen_present = false }
  ; { colour = Yellow; pos = { x = 0; y = 1 }; queen_present = true }
  ; { colour = Red; pos = { x = 1; y = 1 }; queen_present = false }
  ; { colour = Red; pos = { x = 2; y = 1 }; queen_present = false }
  ; { colour = Red; pos = { x = 3; y = 1 }; queen_present = false }
  ; { colour = Red; pos = { x = 0; y = 2 }; queen_present = false }
  ; { colour = Red; pos = { x = 1; y = 2 }; queen_present = false }
  ; { colour = Red; pos = { x = 2; y = 2 }; queen_present = false }
  ; { colour = Blue; pos = { x = 3; y = 2 }; queen_present = false }
  ; { colour = Red; pos = { x = 0; y = 3 }; queen_present = false }
  ; { colour = Red; pos = { x = 1; y = 3 }; queen_present = true }
  ; { colour = Red; pos = { x = 2; y = 3 }; queen_present = false }
  ; { colour = Red; pos = { x = 3; y = 3 }; queen_present = false }
  ]
;;

let get_queen_positions (squares : square list) : position list =
  let rec get_queen_positions_rec (squares : square list) (ls : position list)
    : position list
    =
    match squares with
    | [] -> ls
    | head :: tail ->
      if head.queen_present
      then get_queen_positions_rec tail (ls @ [ head.pos ])
      else get_queen_positions_rec tail ls
  in
  get_queen_positions_rec squares []
;;

(* let%test_unit "Test valid board" =
   let ( => ) = [%test_eq: Base.bool] in
   Queens.valid_board (get_queen_positions four_by_four) => true
   ;; *)

let%test_unit "Test is_solved" =
  let ( => ) = [%test_eq: Base.bool] in
  (* valid *)
  Queens.is_solved
    { x_lower = 0; x_upper = 3; y_lower = 0; y_upper = 3; squares = four_by_four }
  => true;
  (* bad colour *)
  Queens.is_solved
    { x_lower = 0
    ; x_upper = 3
    ; y_lower = 0
    ; y_upper = 3
    ; squares = too_many_colour_four_by_four
    }
  => false;
  Queens.is_solved
    { x_lower = 0
    ; x_upper = 3
    ; y_lower = 0
    ; y_upper = 3
    ; squares = not_all_colour_four_by_four
    }
  => false;
  (* bad axis *)
  Queens.is_solved
    { x_lower = 0
    ; x_upper = 3
    ; y_lower = 0
    ; y_upper = 3
    ; squares = bad_axis_four_by_four
    }
  => false
;;
