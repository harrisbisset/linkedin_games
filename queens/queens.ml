type colour =
  | Red
  | Blue

type position =
  { x : int
  ; y : int
  }

type axis =
  | XAxis
  | YAxis

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

let add_x_y
  (x : int)
  (y : int)
  (x_lower : int)
  (x_upper : int)
  (y_lower : int)
  (y_upper : int)
  (invalid_squares : position list)
  : position list
  =
  let rec inc
    (move : int)
    (stationary : int)
    (upper : int)
    (ax : axis)
    (invalid_squares : position list)
    : position list
    =
    if move <= upper
    then (
      match ax with
      | XAxis ->
        inc
          (move + 1)
          stationary
          upper
          ax
          (invalid_squares @ [ { x = move; y = stationary } ])
      | YAxis ->
        inc
          (move + 1)
          stationary
          upper
          ax
          (invalid_squares @ [ { x = stationary; y = move } ]))
    else invalid_squares
  in
  inc y_lower x y_upper YAxis (inc x_lower y x_upper XAxis invalid_squares)
;;

let invalidate_squares
  (x_lower : int)
  (x_upper : int)
  (y_lower : int)
  (y_upper : int)
  (pos : position)
  (invalid_squares : position list)
  : position list
  =
  let add_original (pos : position) (invalid_squares : position list) : position list =
    invalid_squares @ [ pos ]
  in
  add_x_y pos.x pos.y x_lower x_upper y_lower y_upper (add_original pos invalid_squares)
;;
