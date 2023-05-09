import CodeWorld

-----------------------------------Lines-----------------------------------
data Line a = Line [a] a [a] deriving (Show)
integers :: Line Integer
integers = Line [-1, -2..] 0 [1, 2..]

-- return first n element of the given list
cutList :: Int -> [a] -> [a]
cutList 0 _ = []
cutList n (x:xs) = x : (cutList (n-1) xs)

cutLine :: Int -> Line a -> Line a
cutLine n (Line xs y zs) = Line (cutList n xs) y (cutList n zs)

-- return a line where the first list of the line is obtained from applying f,
-- the focus is x
-- the second list of the line is obtained from applying g.
genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine f x g = Line (apply f x) x (apply g x)
  where
    apply :: (a -> Maybe a) -> a -> [a]
    apply f x = case (f x) of
      Nothing -> []
      Just y -> y : apply f y

-- return Just (f x) if the rule p is True.
applyIf :: (a -> Bool) -> (a -> b) -> a -> Maybe b
applyIf p f x
  | p x       = Just (f x)
  | otherwise = Nothing

-- apply f to all line elements
mapLine :: (a -> b) -> Line a -> Line b
mapLine f (Line xs y zs) = Line (map f xs) (f y) (map f zs)

-- return a line of zipping two lines
zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line axs ay azs) (Line bxs by bzs) = Line (zip axs bxs) (ay,by) (zip azs bzs)

-- return a line of zipping two lines with a function
zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith f (Line axs ay azs) (Line bxs by bzs)  = 
  Line (zipWith f axs bxs) (f ay by) (zipWith f azs bzs)

----------------------------------Rule 30----------------------------------
data Cell = Alive | Dead deriving (Show)

-- return Rule30 of the 3 cells
encodeRule30 :: (Cell, Cell, Cell) -> Cell
encodeRule30 (Alive, Alive, Alive) = Dead
encodeRule30 (Alive, Alive, Dead) = Dead
encodeRule30 (Alive, Dead, Alive) = Dead
encodeRule30 (Alive, Dead, Dead) = Alive
encodeRule30 (Dead, Alive, Alive) = Alive
encodeRule30 (Dead, Alive, Dead) = Alive
encodeRule30 (Dead, Dead, Alive) = Alive
encodeRule30 (Dead, Dead, Dead) = Dead

-- Return Rule30 to the focus of the line
rule30 :: Line Cell -> Cell
rule30 (Line [] y (z:zs)) = encodeRule30 (Dead,y,z)
rule30 (Line (x:xs) y []) = encodeRule30 (x,y,Dead)
rule30 (Line (x:xs) y (z:zs)) =  encodeRule30 (x,y,z)

-- Return a Just Line where the focus is shifted to the left 
-- or Nothing if there is no left element
shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line [] y zs) = Nothing
shiftLeft (Line (x:xs) y zs) = Just (Line xs x (y:zs))

-- |Return a Just Line where the focus is shifted to the right 
-- or Nothing if there is no right element
shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line xs y []) = Nothing
shiftRight (Line xs y (z:zs)) = Just (Line (y:xs) z zs)

-- maps every cell in a line into a
-- version of the original line where that cell is in focus. 
lineShifts :: Line a -> Line (Line a)
lineShifts ln = Line (allShiftLeft ln) ln (allShiftRight ln)
    where
      allShiftLeft :: Line a -> [Line a]
      allShiftLeft ln = case (shiftLeft ln) of
        Just x -> x : allShiftLeft (x)
        Nothing -> []
      allShiftRight :: Line a -> [Line a]
      allShiftRight ln = case (shiftRight ln) of
        Just x -> x : allShiftRight (x)
        Nothing -> []

-- apply rule30 to each shifted version of the line to get the new state for each cell
applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)


sampleLine :: Line Picture
sampleLine = Line [a,b,c,d,e,f,g] c [g,d,b,c,a,f]
  where
    a = colored red (solidCircle 0.5)
    b = colored green (solidCircle 0.5)
    c = colored blue (solidCircle 0.5)
    d = colored yellow (solidCircle 0.5)
    e = colored purple (solidCircle 0.5)
    f = colored brown (solidCircle 0.5)
    g = colored pink (solidCircle 0.5)

-- Render a line of 1x1 pictures.
renderLine:: Line Picture -> Picture
renderLine(Line xs y zs) = (translated (-1) 0 (renderList (-1) xs)) <> 
  y <> (translated 1 0 (renderList 1 zs))
  where
    renderList :: Double -> [Picture] -> Picture
    renderList _ [] = blank
    renderList sft (x:xs) = x <> translated sft 0 (renderList sft xs)

-- render a cell
renderCell :: Cell -> Picture
renderCell Dead = rectangle 1 1
renderCell Alive = solidRectangle 1 1

-- | Render the fist N steps of Rule 30,
-- applied to a given starting line.
renderRule30 :: Int -> Line Cell -> Picture
renderRule30 0 _ = blank
renderRule30 n ln = (renderRow ln) <> translated 0 1 (renderRule30 (n-1) (applyRule30 ln))
  where
    renderRow :: Line Cell -> Picture
    renderRow ln = renderLine (mapLine renderCell ln)

------------------------------Discrete spaces------------------------------
data Space a = Space (Line (Line a))

-- |Ex 1.10:
-- apply f to all space elements
mapSpace :: (a -> b) -> Space a -> Space b
mapSpace f (Space (Line xs y zs)) = Space (Line (map (mapLine f) xs) (mapLine f y) (map (mapLine f) zs))

-- return a line of zipping two spaces
zipSpaces :: Space a -> Space b -> Space (a, b)
zipSpaces (Space (Line axs ay azs)) (Space (Line bxs by bzs)) = 
   Space (Line (zipWith zipLines axs bxs) (zipLines ay by) (zipWith zipLines azs bzs))

-- return a line of zipping two spaces with a function
zipSpacesWith :: (a -> b -> c) -> Space a -> Space b -> Space c
zipSpacesWith f (Space (Line axs ay azs)) (Space (Line bxs by bzs)) = Space (
      Line (zipWith (zipLinesWith f) axs bxs) 
      (zipLinesWith f ay by) 
      (zipWith (zipLinesWith f) azs bzs) 
    )

---------------------------Conway’s Game of Life---------------------------

-- apply conway rule to the focus of the space
conwayRule :: Space Cell -> Cell
conwayRule (Space (Line [] y zs)) = conwayRule (Space (Line [(Line [Dead] Dead [Dead])] y zs))
conwayRule (Space (Line xs y [])) = conwayRule (Space (Line xs y [(Line [Dead] Dead [Dead])]))
conwayRule (Space (Line ((Line [] a0 a1):b) c0 c1)) = conwayRule (Space (Line ((Line [Dead] a0 a1):b) c0 c1))
conwayRule (Space (Line ((Line a0 a1 []):b) c0 c1)) = conwayRule (Space (Line ((Line a0 a1 [Dead]):b) c0 c1))
conwayRule (Space (Line a1 a2 ((Line [] b1 b2):c))) = conwayRule (Space (Line a1 a2 ((Line [Dead] b1 b2):c)))
conwayRule (Space (Line a1 a2 ((Line b1 b2 []):c))) = conwayRule (Space (Line a1 a2 ((Line b1 b2 [Dead]):c)))
conwayRule (Space (Line a1 (Line [] b1 b2) a2 )) = conwayRule (Space (Line a1 (Line [Dead] b1 b2) a2 ))
conwayRule (Space (Line a1 (Line b1 b2 []) a2)) = conwayRule (Space (Line a1 (Line b1 b2 [Dead]) a2))
conwayRule (Space (Line ((Line (d1:_) d2 (d3:_)):_) (Line (m1:_) m2 (m3:_))
  ((Line (u1:_) u2 (u3:_)):_))) = 
  case (m2, (sum (map isAlive arr))) of
    (Dead,3) -> Alive
    (Alive,2) -> Alive
    (Alive,3) -> Alive
    _ -> Dead
  where
    arr :: [Cell]
    arr = [d1,d2,d3,m1,m3,u1,u2,u3]
    isAlive :: Cell -> Int
    isAlive Alive = 1
    isAlive Dead = 0
    
-- shift space focus to right
spaceRightShift :: Space a -> Space a
spaceRightShift (Space ln) = (Space (mapLine (\x -> case (shiftRight x) of
                                                        Nothing -> x
                                                        Just newX -> newX) ln))
                                                        
-- shift space focus to left
spaceLeftShift :: Space a -> Space a
spaceLeftShift (Space ln) = (Space (mapLine (\x -> case (shiftLeft x) of
                                                        Nothing -> x
                                                        Just newX -> newX) ln))

-- shift space focus up
spaceUpShift :: Space a -> Space a
spaceUpShift (Space (Line [] y zs)) = (Space (Line [] y zs))
spaceUpShift (Space (Line (x:xs) y zs)) = (Space (Line xs x (y:zs)))

-- shift space focus down
spaceDownShift :: Space a -> Space a
spaceDownShift (Space (Line xs y [])) = (Space (Line xs y []))
spaceDownShift (Space (Line xs y (z:zs))) = (Space (Line (y:xs) z zs))

-- return a list of all space with focus shifted to right
spaceAllRightShifts :: Space a -> [Space a]
spaceAllRightShifts (Space (Line _ (Line _ _ []) _)) = []
spaceAllRightShifts sp = (spaceRightShift sp) : spaceAllRightShifts (spaceRightShift sp)

-- return a list of all space with focus shifted to left
spaceAllLeftShifts :: Space a -> [Space a]
spaceAllLeftShifts (Space (Line _ (Line [] _ _) _)) = []
spaceAllLeftShifts sp = (spaceLeftShift sp) : spaceAllLeftShifts (spaceLeftShift sp)

-- return a line of spaces with focus shifted left and right
spaceAllLine :: Space a -> Line(Space a)
spaceAllLine sp = (Line (spaceAllLeftShifts sp) sp (spaceAllRightShifts sp))

-- return a list of lines of spaces with focus shifted up
spaceAllUpShifts :: Space a -> [Line (Space a)]
spaceAllUpShifts (Space (Line [] y zs) ) = [(spaceAllLine (Space (Line [] y zs) ))]
spaceAllUpShifts sp = (spaceAllLine sp) : spaceAllUpShifts (spaceUpShift sp)

-- return a list of lines of spaces with focus shifted down
spaceAllDownShifts :: Space a -> [Line (Space a)]
spaceAllDownShifts (Space (Line xs y []) ) = [(spaceAllLine (Space (Line xs y []) ))]
spaceAllDownShifts sp = (spaceAllLine sp) : spaceAllDownShifts (spaceDownShift sp)

-- return a space of space with all possible focus shift in correct order
spaceShifts :: Space a -> Space (Space a)
spaceShifts sp = (Space (Line (spaceAllUpShifts (spaceUpShift sp)) (spaceAllLine sp)
  (spaceAllDownShifts (spaceDownShift sp))))

-- apply ConwayRule on every cell in a space 
applyConwayRule :: Space Cell -> Space Cell
applyConwayRule space = mapSpace conwayRule (spaceShifts space)

-- Render a space of 1x1 pictures.
renderSpace :: Space Picture -> Picture
renderSpace (Space (Line xs y zs)) = renderLines (Line (map renderLine xs) 
  (renderLine y) (map renderLine zs))
    where
      renderLines :: Line Picture -> Picture
      renderLines (Line xs y zs) = y <> translated 0 1 (renderList 1 xs) <> 
        translated 0 (-1) (renderList (-1) zs)
        where
          renderList :: Double -> [Picture] -> Picture
          renderList _ [] = blank
          renderList sft (x:xs) = x <> translated 0 sft (renderList sft xs)

-- Animate Conway's Game of Life,
-- starting with a given space
-- updating it every 0.6 second.
animateConway :: Space Cell -> IO ()
animateConway sp = activityOf (0, sp) update renderer
  where
   renderer :: (Double , Space Cell) -> Picture
   renderer (_,sp) = renderSpace (mapSpace renderCell sp)
   update (TimePassing delta) (time, space)
    | time >= 0.6 = (0, (applyConwayRule space))
    |  otherwise = (time + delta, space)
   update _ x = x

block :: Space Cell
block = Space(
    Line
        [
            Line [Alive, Dead] Alive [Dead],
            Line [Dead, Dead] Dead [Dead]
        ]
        (   Line [Alive, Dead] Alive [Dead])
        [
            Line [Dead, Dead] Dead [Dead]
        ])

blinker = (Space (Line blinker' (Line [Alive, Dead, Dead] Alive [Alive, Dead, Dead]) blinker')) 
  where
    blinker' = replicate 3 (Line (replicate 3 Dead) Dead (replicate 3 Dead))

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
      (   Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead])
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
        (   Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead])
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


main :: IO()
main = do
  let shape = pentaDecathlon
  animateConway shape

  