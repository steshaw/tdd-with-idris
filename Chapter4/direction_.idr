data Direction
  = North
  | East
  | South
  | West

turn_clockwise : Direction -> Direction
turn_clockwise North = East
turn_clockwise East = South
turn_clockwise South = West
turn_clockwise West = North
