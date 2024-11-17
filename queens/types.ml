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
