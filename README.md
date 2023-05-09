# Conway-game-of-life

This is an implementation of Conway's Game of Life in Haskell using the CodeWorld library. For more information about Conway's Game of Life, please refer to this [wiki page](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).

## Features

* Maps are implemented as spaces of cells. 

* Cell is a basic object that can have one of two values: dead (represented by a white rectangle) or alive (represented by a black rectangle).

* Each line consists of a point in center (focus) and a list of points to the left and right of it.

* Space is implemented as a line of lines, where the line in the middle represents the center of the space. The left list represents the lines above it, with the head of this list being the first line above the center. The right list represents the lines below it, with the head of this list being the first line below the center.

* You can check how the predefined maps were created:

  - Block:
  ```haskell
    block :: Space Cell
    block = Space(
      Line
        [
          Line [Alive, Dead] Alive [Dead],
          Line [Dead, Dead] Dead [Dead]
        ]
        ( Line [Alive, Dead] Alive [Dead])
        [
          Line [Dead, Dead] Dead [Dead]
        ])
  ```
  - Blinker:
  ```haskell
  blinker = (Space (Line blinker' (Line [Alive, Dead, Dead] Alive [Alive, Dead, Dead]) blinker')) 
    where
      blinker' = replicate 3 (Line (replicate 3 Dead) Dead (replicate 3 Dead))
  ```
  - Glider:
  ```haskell
  glider :: Space Cell
  glider =  Space(
    Line 
      [
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead]
      ]
      ( Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead])
      [
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Alive, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Alive [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead]
      ])
  ```

  - Penta Decathlon:
  ```haskell
  pentaDecathlon :: Space Cell 
  pentaDecathlon = Space(
    Line 
      [
        Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
        Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
        Line [Alive, Dead, Dead, Dead, Dead] Dead  [Alive, Dead, Dead, Dead, Dead],
        Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead]
      ]
      ( Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead])
      [
        Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
        Line [Alive, Dead, Dead, Dead, Dead] Dead  [Alive, Dead, Dead, Dead, Dead],
        Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
        Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead]
      ])
  ```

## How to Run

The simplest way to run the code is by copying it and pasting it into the [CodeWorld website](https://code.world/haskell#). To change the printed map just change the `shape` variable in `main :: IO()` to the map you want.





