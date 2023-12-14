{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GHC.IO (unsafePerformIO)
import System.Random

type Grid = [[Int]]
type IPoint = (Int, Int)

data State = Start | Stage1 Grid Int Bool | Stage2 Grid Grid Grid Grid | Final Bool
data Buttons = Ship Int | InGrid IPoint | StartButton | ReverseButton

-- | Returns second background image
backgroundImage :: Picture
backgroundImage = color (makeColor 1 0 0 0) (scale 1.6 1.6 (unsafePerformIO (loadBMP "./images/battleship2.bmp")))

-- | Returns first background image
backgroundImageMenu :: Picture
backgroundImageMenu = color (makeColor 1 0 0 0) (scale 1.2 1.2 (unsafePerformIO (loadBMP "./images/battleship1.bmp")))

-- | Returns a semi-transparent black rectangle
tableGame :: Picture
tableGame = color (makeColor 0 0 0 0.7) (rectangleSolid 1400 900)

-- | Render user's empty grid
myBoard :: Picture
myBoard = translate (-300) 0 (color (makeColor 1 1 1 1) (rectangleSolid 500 500) <>
            pictures [line [(-250, -250 + x), (250, -250 + x)] | x <- list] <>
            pictures [line [(-250 + x, -250), (-250 + x, 250)] | x <- list])
    where
        list = [0, 50..500]

-- | Renders enemy empty grid
enemyBoardPicture :: Picture
enemyBoardPicture = translate 600 0 myBoard

-- | Convert type of ship to its colour
getColour :: Int -> Color
getColour 1 = magenta
getColour 2 = blue
getColour 3 = green
getColour 4 = azure
getColour _ = white

-- | Return Picture of X mark
xMark :: Picture
xMark = color red (pictures [rotate 45 (rectangleSolid 40 4), rotate (-45) (rectangleSolid 40 4)])

-- | Return picture of ship with given type
getShip :: Int -> Picture
getShip val = color (getColour val) (rectangleSolid (fromIntegral (val*50)) 50) <> pictures [line [(x - 25 * fromIntegral val, -25), (x - 25 * fromIntegral val, 25)] | x <- list]
    where list = [0, 50..fromIntegral (val*50)]

-- | Helper function that return Picture of given ship during placement stage
setUp :: Int -> Int -> Picture
setUp pos num = translate 200 (450 - 150 * fromIntegral pos) (getShip pos <> translate (50 + 25 * fromIntegral pos) 0 (xMark <> translate 50 (-25) (scale 0.65 0.65 (color red (sizedText (show num) 3)))))

-- | Helper fucntion that returns list of all cells of 10x10 grid
allMoves :: [IPoint]
allMoves = combineList [0..9] [0..9]

-- | Return Picture of ships that should be placed to board
showAvailable :: [Int] -> Picture
showAvailable list = pictures [setUp x y | (x, y) <- idxList]
    where
        idxList = zip [1..] list

-- | Return Picture of given string with given thickness
sizedText :: String -> Float -> Picture
sizedText s sz = pictures [translate x y (text s) | x <- myShift, y <- myShift]
    where
        myShift = [0..sz]

-- | Helper function to get Picture of inscription
spaceToMenu :: Picture
spaceToMenu = color (greyN 0.4) (scale 0.21 0.21 (sizedText "Press [Enter] to return to Start Menu" 1))

-- | Picture of start menu
startMenu :: Picture
startMenu = translate 0 50 (color white (rectangleSolid 900 400)) 
            <> translate (-250) 150 (scale 0.6 0.6 (sizedText "Start Menu" 2))
            <> translate (-300) 65 (scale 0.4 0.4 (sizedText "Welcome to BattleShip!" 1))
            <> translate (-300) (-100) (color (greyN 0.4) (scale 0.23 0.23 (sizedText "Press [Space] to start the game" 1)))

-- | Picture of announcement that you won
winAnnounce :: Picture
winAnnounce = translate 0 50 (color white (rectangleSolid 900 400)) 
                <> translate (-300) 150 (scale 0.6 0.6 (sizedText "Congratulations!" 2))
                <> translate (-400) 65 (scale 0.25 0.25 (sizedText "You have successfully destroyed all enemy ships" 1))
                <> translate (-200) 5 (scale 0.25 0.25 (sizedText "and won this battle!" 1))
                <> translate (-300) (-100) spaceToMenu

-- | Picture of announcement that you lost
loseAnnounce :: Picture
loseAnnounce = translate 0 50 (color white (rectangleSolid 900 400)) 
                <> translate (-250) 150 (scale 0.6 0.6 (sizedText "Game over" 2)) 
                <> translate (-400) 65 (scale 0.27 0.27 (sizedText "Unfortunately, all your ships were destroyed." 1))
                <> translate (-300) (-50) spaceToMenu

-- | Picture of change mode button
reverseButton :: Bool -> Picture
reverseButton active = translate 225 (-270) (color (greyN 0.5) (rectangleSolid 250 60) <> translate (-115) (-20) (scale 0.22 0.22 (sizedText label 1)))
    where
        label
            | active = "Mode: Vertical"
            | otherwise = "Mode: Horizontal"

-- | Picture of start game button
startButton :: Bool -> Picture
startButton active = translate 225 (-360) (color col (rectangleSolid 250 60) <> translate (-110) (-20) (scale 0.3 0.3 (sizedText "Start Game" 1)))
    where
        col
            | active = greyN 0.5
            | otherwise = greyN 0.3

-- | Empty grid
grid :: Grid
grid = replicate 10 (replicate 10 0)

-- | Return list of ships that have not been placed yet
getCount :: Grid -> [Int]
getCount list = [5 - need - (length [element | row <- list, element <- row, element == need]) `div` need | need <- needList]
    where
        needList = [1..4]

-- | Helper function to convert point of mouse click to corresponding cell for first board
pointsToPosition :: (Float, Float) -> IPoint
pointsToPosition (x, y) = (-floor (y/50) + 4, truncate (x/50) + 10)

-- | Returns button that corresponds to given point
pointToButton :: Point -> Maybe Buttons
pointToButton (x, y)
    | x <= 350 && x >= 100 && y <= -330 && y >= -390 = Just StartButton
    | x <= 350 && x >= 100 && y <= -240 && y >= -300 = Just ReverseButton
    | x <= 225 && x >= 175 && y <= 325 && y >= 275 = Just (Ship 1)
    | x <= 250 && x >= 150 && y <= 175 && y >= 125 = Just (Ship 2)
    | x <= 275 && x >= 125 && y <= 25 && y >= -25 = Just (Ship 3)
    | x <= 300 && x >= 100 && y <= -125 && y >= -175 = Just (Ship 4)
    | x < -50 && x > -550 && y < 250 && y > -250 = Just (InGrid (pointsToPosition (x, y)))
    | otherwise = Nothing

-- | Helper function to convert point of mouse click to corresponding cell for second board
convertPoint :: (Float, Float) -> IPoint
convertPoint (x, y) = (-floor (y/50) + 4, truncate ((x - 600)/50) + 10)

-- | Helper function to determing if given point inside the board
inSquare :: (Float, Float) -> Bool
inSquare (x, y) = x > 50 && x < 550 && y > -250 && y < 250

-- | Returns new State after clicking on given cell
makeMove :: (Float, Float) -> State -> State
makeMove (x, y) (Stage2 myCurBoard myHits enemyBoard enemyHits)
    | inSquare (x, y) && (myHits !! a) !! b > 0 = Stage2 myCurBoard myHits enemyBoard enemyHits
    | inSquare (x, y) && (enemyBoard !! a) !! b == 0 = makeEnemyMove (Stage2 myCurBoard (updateGrid myHits (convertPoint (x, y)) 1) enemyBoard enemyHits)
    | inSquare (x, y) && (enemyBoard !! a) !! b > 0 && winCondition tempState = Final True
    | inSquare (x, y) && (enemyBoard !! a) !! b > 0 = tempState
    | otherwise = Stage2 myCurBoard myHits enemyBoard enemyHits
    where
        (a, b) = convertPoint (x, y)
        tempState = Stage2 myCurBoard (updateGrid myHits (convertPoint (x, y)) 1) enemyBoard enemyHits
makeMove (_, _) state = state 

-- | Event handler for battle ship stage
handleSecondStage :: Event -> State -> State
handleSecondStage (EventKey (MouseButton LeftButton) Down _ (x, y)) state = makeMove (x, y) state
handleSecondStage _ state = state

-- | Return some random depending on grid
getSeed :: Grid -> Int -> Int
getSeed ((x : xs) : zs) col = (x * col + getSeed (xs : zs) col) `mod` 51
getSeed ([] : zs) col = getSeed zs (col + 1) `mod` 51
getSeed [] _ = 0

-- Pure function to generate a random Int in a given range
getRandomInt :: StdGen -> Int -> Int -> Int
getRandomInt gen low high =
  let (randomValue, _) = randomR (low, high) gen
  in randomValue

-- | Returns list of all cells where given ship can be placed
getPossibleMoves :: Grid -> Int -> Bool -> [IPoint]
getPossibleMoves givenGrid ship mode = [(x, y) | (x, y) <- allMoves, canPlace givenGrid ship (x, y) mode]

-- | Helper function to generate one ship
generateShip :: Grid -> Int -> StdGen -> Grid
generateShip givenGrid ship seed
    | canPlace givenGrid ship randomMove mode = placeShip givenGrid randomMove ship mode
    | otherwise = placeShip givenGrid randomMove ship (not mode)
    where
        mode
            | getRandomInt seed 0 10 > 4 = False
            | otherwise = True
        steps
            | null (getPossibleMoves givenGrid ship mode) = getPossibleMoves givenGrid ship (not mode)
            | otherwise = getPossibleMoves givenGrid ship mode
        randomMove = steps !! getRandomInt seed 0 (length steps - 1) 

-- | Generates random grid with ships for enemy
generateEnemyBoard :: Grid -> Grid
generateEnemyBoard givenGrid = foldl (\acc (ship, idx) -> generateShip acc ship (gens !! idx)) grid list
    where
        list = zip [4, 3, 3, 2, 2, 2, 1, 1, 1, 1] [0..]
        seed = 42 + getSeed givenGrid 1
        gen = mkStdGen seed
        gens = [foldl (\acc _ -> snd (split acc)) gen [0..x] | x <- [0..10]]

-- | Helper function to render enemy hits
hitPicture :: Int -> Int -> Picture 
hitPicture _ 0 = color (makeColor 0 0 0 0) (rectangleSolid 49 49)
hitPicture 0 _ = color (makeColor 0 0 0 0.4) (rectangleSolid 49 49) 
hitPicture _ _ = color red (rotate 45 (rectangleSolid 60 5) <> rotate (-45) (rectangleSolid 60 5))

-- | Render enemy hits
drawEnemyHits :: Grid -> Grid -> Picture
drawEnemyHits givenGrid hits = pictures[translate (-525 + 50 * fromIntegral y) (225 - 50 * fromIntegral x) 
                                (hitPicture ((givenGrid !! x) !! y) ((hits !! x) !! y)) | (x, y) <- allMoves]

-- | Returns 1 if this cell belongs to a destroyed ship, 2 - hitted ship, 0 - no ship
isKilled :: Grid -> Grid -> IPoint -> Int
isKilled givenGrid hits (x, y)
        | hits !! x !! y == 0 = 0
        | null ships = 0
        | allMatch = 1
        | otherwise = 2
    where
        ships = collectShips givenGrid (x, y) 5
        match = [(hits !! a) !! b == 1 | (a, b) <- ships]
        allMatch = allTrue match

-- | Returns all cells containing a ship that touches a given cell
collectShips :: Grid -> IPoint -> Int -> [IPoint]
collectShips givenGrid (x, y) lastMove
        | x < 0 || x > 9 || y < 0 || y > 9 || val == 0 = []
        | otherwise = [(x, y)] ++ down ++ up ++ left ++ right
    where
        val = (givenGrid !! x) !! y
        down 
            | lastMove == 2 = []
            | otherwise = collectShips givenGrid (x + 1, y) 4
        up 
            | lastMove == 4 = []
            | otherwise = collectShips givenGrid (x - 1, y) 2
        right 
            | lastMove == 1 = []
            | otherwise = collectShips givenGrid (x, y + 1) 3
        left   
            | lastMove == 3 = []
            | otherwise = collectShips givenGrid (x, y - 1) 1

-- | Helper function that determine colour of enemy cell
hitColour :: Grid -> Grid -> IPoint -> Color
hitColour givenGrid hits (x, y)
    | (hits !! x) !! y == 0 = makeColor 0 0 0 0
    | (hits !! x) !! y == 1 && (givenGrid !! x) !! y == 0 = makeColor 0 0 0 0.3
    | isKilled givenGrid hits (x, y) == 1 = makeColor 1 0 0 0.5
    | otherwise = makeColor 1 1 0 0.4

-- | Return picture of grid where each cell colored with grey (miss), orange(hit), red(full destroy)
drawMyHits :: Grid -> Grid -> Picture
drawMyHits givenGrid hits = pictures[translate (75 + 50 * fromIntegral y) (225 - 50 * fromIntegral x) 
                        (color (hitColour givenGrid hits (x, y)) (rectangleSolid 49 49)) | (x, y) <- allMoves]

-- | Return random cell where theoretically could be a ship
randomHit :: Grid -> Grid -> Int -> IPoint
randomHit myHits deaths seed = move
    where
        gen = mkStdGen seed
        allPossible = [(x, y) | (x, y) <- allMoves, canPlace deaths 1 (x, y) True && (myHits !! x) !! y == 0]
        idx = getRandomInt gen 0 (length allPossible - 1)
        move = allPossible !! idx

-- | Returns given cell if theoretically there could be a ship there
tryMove :: Grid -> Grid -> IPoint -> IPoint
tryMove myHits deaths (x, y)
    | x < 0 || x > 9 || y < 0 || y > 9 || (myHits !! x) !! y == 1 || canPlace deaths 1 (x, y) True == False = (-1, -1)
    | otherwise = (x, y)

-- | Returns first value not equal to (-1, -1)
getConsistent :: [IPoint] -> IPoint
getConsistent (x : xs)
    | x /= (-1, -1) = x
    | otherwise = getConsistent xs

-- | Returns the optimal move knowing two hits (non-fully destroyed) on the same ship
twoPoint :: Grid -> Grid -> IPoint -> IPoint -> IPoint
twoPoint myHits deaths (x1, y1) (x2, y2)
    | x1 == x2 = getConsistent [tryMove myHits deaths (x1, max y1 y2 + 1), tryMove myHits deaths (x1, min y1 y2 - 1)]
    | otherwise = getConsistent [tryMove myHits deaths (min x1 x2 - 1, y1), tryMove myHits deaths (max x1 x2 + 1, y2)]

-- | Determines what move the enemy will make given the current state of the game
makeEnemyMove :: State -> State
makeEnemyMove (Stage2 myCurBoard myHits enemyBoard enemyHits)
    | (myCurBoard !! ansx) !! ansy == 0 = Stage2 myCurBoard myHits enemyBoard (updateGrid enemyHits (ansx, ansy) 1)
    | loseCondition (Stage2 myCurBoard myHits enemyBoard (updateGrid enemyHits (ansx, ansy) 1)) = Final False
    | otherwise = makeEnemyMove (Stage2 myCurBoard myHits enemyBoard (updateGrid enemyHits (ansx, ansy) 1))
    where   
        allHits = [(x, y) | (x, y) <- allMoves, isKilled myCurBoard enemyHits (x, y) == 2]
        allDeaths = [(x, y) | (x, y) <- allMoves, isKilled myCurBoard enemyHits (x, y) == 1]
        deathMatrix = updateGridList grid allDeaths 1
        (a, b) 
            | length allHits > 0 = head allHits
            | otherwise = (-1, -1)
        (ansx, ansy) 
            | null allHits = randomHit enemyHits deathMatrix (getSeed myCurBoard 1)
            | length allHits == 1 = getConsistent [tryMove enemyHits deathMatrix (a - 1, b),
                                                   tryMove enemyHits deathMatrix (a + 1, b), 
                                                   tryMove enemyHits deathMatrix (a, b + 1), 
                                                   tryMove enemyHits deathMatrix (a, b - 1)]
            | otherwise = twoPoint enemyHits deathMatrix (head allHits) (last allHits)
makeEnemyMove state = state

-- | Changes State after Left Click Event
handleClick :: Maybe Buttons -> State -> State
handleClick Nothing state = state
handleClick (Just StartButton) (Stage1 list active mode)
    | isStartActive list = Stage2 list grid (generateEnemyBoard list) grid
    | otherwise = Stage1 list active mode
handleClick (Just ReverseButton) (Stage1 list active mode) = Stage1 list active (not mode)
handleClick (Just (Ship n)) (Stage1 list active mode)
    | getCount list !! (n - 1) > 0 = Stage1 list n mode
    | otherwise = Stage1 list active mode
handleClick (Just (InGrid (x, y))) (Stage1 list active mode)
    | active == 0 || not (canPlace list active (x, y) mode) = Stage1 list active mode
    | otherwise = Stage1 (placeShip list (x, y) active mode) 0 mode
handleClick _ state = state

-- | Changes State after Right Click Event
handleRightClick :: Maybe Buttons -> State -> State
handleRightClick (Just (InGrid (x, y))) (Stage1 list active mode) 
    | list !! x !! y == 0 = Stage1 list active mode 
    | otherwise = Stage1 (updateGridList list killList 0) 0 mode
    where
        killList = collectShips list (x, y) 5
handleRightClick _ state = state

-- | Changes State depending on given Event
handleGame :: Event -> State -> State
handleGame (EventKey (SpecialKey KeyEnter) Down _ (_, _)) (Final _) = Start
handleGame (EventKey (SpecialKey KeySpace) Down _ (_, _)) Start = Stage1 grid 0 False  
handleGame event (Stage2 myCurBoard myHits enemyBoard enemyHits) = handleSecondStage event (Stage2 myCurBoard myHits enemyBoard enemyHits)
handleGame (EventKey (MouseButton LeftButton) Down _ (x, y)) state = handleClick (pointToButton (x, y)) state
handleGame (EventKey (MouseButton RightButton) Down _ (x, y)) state = handleRightClick (pointToButton (x, y)) state
handleGame _ state = state

-- | Returns True if all ships are placed to the Grid
isStartActive :: Grid -> Bool
isStartActive givenGrid = sum list == 0
    where
        list = getCount givenGrid

-- | Render current State
drawState :: State -> Picture
drawState (Stage1 list active mode) = defaultPicture <> posMove
    where
        defaultPicture = pictures [backgroundImage, tableGame, myBoard, showAvailable (getCount list), startButton startButtonActive, reverseButton mode, drawShips list]
        posMove
            | active == 0 = pictures []
            | otherwise = possibleMoves list active mode
        startButtonActive = isStartActive list
drawState (Stage2 myCurBoard myHits enemyBoard enemyHits) = defaultPicture
    where
        defaultPicture = pictures [backgroundImage, tableGame, myBoard, drawShips myCurBoard, enemyBoardPicture, drawEnemyHits myCurBoard enemyHits, drawMyHits enemyBoard myHits]
drawState (Final win) 
    | win = backgroundImageMenu <> tableGame <> winAnnounce
    | otherwise = backgroundImageMenu <> tableGame <> loseAnnounce
drawState Start = backgroundImageMenu <> tableGame <> startMenu

-- | Add number of row and coloumn to each element of Grid
addIndices :: Grid -> [[(Int, Int, Int)]]
addIndices givenGrid =
    [ [(element, row, col) | (col, element) <- indexedRow] | (row, indexedRow) <- indexedGrid ]
    where
        indexedGrid = zip [0..] (map (zip [0..]) givenGrid)

-- | Return Picture of given Grid with rendered ships
drawShips :: Grid -> Picture
drawShips list = pictures [translate (-525 + 50 * fromIntegral x) (225 - 50 * fromIntegral y) (color (getColour val) (rectangleSolid 49 49)) | row <- idxList, (val, y, x) <- row]
  where
    idxList = addIndices list

-- | Returns True if there is no ship at this point
isFree :: Grid -> IPoint -> Bool
isFree [] _ = False
isFree ([] : _) _ = False
isFree ((x : _) : _) (0, 0) = x == 0
isFree ((_ : zx) : _) (0, y) = isFree [zx] (0, y - 1)
isFree (_ : xs) (x, y) = isFree xs (x - 1, y)

-- | Returns list of all possible pairs composed from two given lists
combineList :: [Int] -> [Int] -> [IPoint]
combineList list1 list2 = [(x, y) | x <- list1, y <- list2]

-- | Returns True if all values in list are True
allTrue :: [Bool] -> Bool
allTrue xs = foldr (&&) True xs

-- | Returns True if given ship can be placed on given cell
canPlace :: Grid -> Int -> IPoint -> Bool -> Bool
canPlace givenGrid ship (x, y) mode
    | mode && x + ship - 1 > 9 = False
    | not mode && y + ship - 1 > 9 = False
    | otherwise = allTrue [isFree givenGrid (a, b) | (a, b) <- area]
    where
        area
            | not mode = combineList [(max (x-1) 0)..(min (x+1) 9)] [(max (y-1) 0)..(min (y+ship) 9)]
            | otherwise = combineList [(max (x-1) 0)..(min (x+ship) 9)] [(max (y-1) 0)..(min (y+1) 9)]

-- | Returns picture of 10x10 grid where each cell colored with grey or yellow
--   depending on whether given ship can be placed on the current cell
possibleMoves :: Grid -> Int -> Bool -> Picture
possibleMoves givenGrid ship mode = pictures [translate (-525 + 50 * fromIntegral y) (225 - 50 * fromIntegral x)
                                        (color (myColour (x, y)) (rectangleSolid 49 49)) | (x, y) <- allMoves]
    where
        myColour :: IPoint -> Color
        myColour (a, b)
            | canPlace givenGrid ship (a, b) mode = makeColor 1 1 0 0.35
            | otherwise = makeColor 0 0 0 0.35


-- | Returns new grid after updating cell in given point with given value
updateGrid :: Grid -> IPoint -> Int -> Grid
updateGrid givenGrid (rowIndex, colIndex) ship =
  take rowIndex givenGrid ++
  [take colIndex (givenGrid !! rowIndex) ++ [ship] ++ drop (colIndex + 1) (givenGrid !! rowIndex)] ++
  drop (rowIndex + 1) givenGrid

-- | Returns new grid after updating cells in given points with given value
updateGridList :: Grid -> [IPoint] -> Int -> Grid
updateGridList givenGrid indices newValue =
  foldl (\acc (row, col) -> updateGrid acc (row, col) newValue) givenGrid indices

-- | Returns new grid after placement given ship 
placeShip :: Grid -> IPoint -> Int -> Bool -> Grid
placeShip givenGrid (x, y) ship mode
    | not mode = updateGridList givenGrid (combineList [x] [y..y+ship-1]) ship
    | otherwise = updateGridList givenGrid (combineList [x..x+ship-1] [y]) ship

controlGame :: Float -> State -> State
controlGame _ state = state

-- | Returns true if you have already won
winCondition :: State -> Bool
winCondition (Stage2 _ myHits enemyBoard _) = length [(x, y) | (x, y) <- allMoves, isKilled enemyBoard myHits (x, y) == 1] == 20
winCondition _ = False

-- | Returns true if you have already lost
loseCondition :: State -> Bool
loseCondition (Stage2 myCurBoard _ _ enemyHits) = length [(x, y) | (x, y) <- allMoves, isKilled myCurBoard enemyHits (x, y) == 1] == 20 
loseCondition _ = False

main :: IO()
main = play FullScreen white 60 Start drawState handleGame controlGame
