open Types

let get_squares_x (squares : square list) : int list =
  let rec get_squares_x_rec (squares : square list) (ls : int list) =
    match squares with
    | [] -> ls
    | head :: tail ->
      if List.mem head.pos.x ls
      then get_squares_x_rec tail ls
      else get_squares_x_rec tail (ls @ [ head.pos.x ])
  in
  get_squares_x_rec squares []
;;

let get_squares_y (squares : square list) : int list =
  let rec get_squares_y_rec (squares : square list) (ls : int list) =
    match squares with
    | [] -> ls
    | head :: tail ->
      if List.mem head.pos.y ls
      then get_squares_y_rec tail ls
      else get_squares_y_rec tail (ls @ [ head.pos.y ])
  in
  get_squares_y_rec squares []
;;

(* Checks if queen positions are valid *)
let rec valid_board (positions : position list) : bool =
  match positions with
  | [] -> true
  | head :: tail ->
    let rec valid_position (pos : position) (positions : position list) : bool =
      match positions with
      | [] -> true
      | head :: tail ->
        let invalid_pos (p1 : int) (p2 : int) : bool =
          let p = p1 - p2 in
          if p = 1 then true else if p = -1 then true else false
        in
        if invalid_pos pos.x head.x
           || invalid_pos pos.y head.y
           || (pos.x == head.x && pos.y == head.y)
        then false
        else valid_position pos tail
    in
    if valid_position head tail then valid_board tail else false
;;

(* assumes valid board *)
let is_solved (board : q_board) : bool =
  (* returns false if valid *)
  let rec check_colours (s : square list) (cl : colour list) (ocl : colour list) : bool =
    match s with
    | [] -> if List.length ocl = 0 then false else true
    | head :: tail ->
      (match head.queen_present with
       | true ->
         (match List.mem head.colour cl, List.mem head.colour ocl with
          | true, _ -> true
          | _, true ->
            check_colours
              tail
              (cl @ [ head.colour ])
              (List.filter (fun x -> x != head.colour) ocl)
          | _ -> check_colours tail (cl @ [ head.colour ]) ocl)
       | false ->
         if List.mem head.colour cl
         then check_colours tail cl (List.filter (fun x -> x != head.colour) ocl)
         else check_colours tail cl (ocl @ [ head.colour ]))
  in
  (* returns false if valid *)
  let rec check_axis
    (sq : square list)
    (x_axis : int list)
    (y_axis : int list)
    (x_ls : int list)
    (y_ls : int list)
    : bool
    =
    match sq with
    | [] ->
      if List.for_all (fun x -> List.mem x x_ls) x_axis
         && List.for_all (fun x -> List.mem x y_ls) y_axis
      then false
      else true
    | hd :: tl ->
      (match
         ( hd.queen_present && not (List.mem hd.pos.x x_ls)
         , hd.queen_present && not (List.mem hd.pos.y y_ls) )
       with
       | true, true ->
         check_axis tl x_axis y_axis (x_ls @ [ hd.pos.x ]) (y_ls @ [ hd.pos.y ])
       | false, true -> check_axis tl x_axis y_axis x_ls (y_ls @ [ hd.pos.y ])
       | true, false -> check_axis tl x_axis y_axis (x_ls @ [ hd.pos.x ]) y_ls
       | false, false -> check_axis tl x_axis y_axis x_ls y_ls)
  in
  if check_colours board.squares [] []
  then false
  else if check_axis
            board.squares
            (get_squares_x board.squares)
            (get_squares_y board.squares)
            []
            []
  then false
  else true
;;

(* Calculates adds all positions a queen's position makes invalid *)
let invalidate_squares
  (pos : position)
  (x_lower : int)
  (x_upper : int)
  (y_lower : int)
  (y_upper : int)
  (invalid_squares : position list)
  : position list
  =
  let rec add_x_y
    (inc : int)
    (no_inc : int)
    (upper : int)
    (x_pos : bool)
    (invalid_squares : position list)
    : position list
    =
    if inc <= upper
    then
      add_x_y
        (inc + 1)
        no_inc
        upper
        x_pos
        (invalid_squares
         @
         match x_pos with
         | true -> [ { x = inc; y = no_inc } ]
         | false -> [ { x = no_inc; y = inc } ])
    else invalid_squares
  in
  add_x_y
    y_lower
    pos.x
    y_upper
    false
    (add_x_y x_lower pos.y x_upper true (invalid_squares @ [ pos ]))
;;
