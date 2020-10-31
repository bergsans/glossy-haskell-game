{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.Pure.Game

data Direction
  = East
  | West
  | None
  deriving (Eq)

data Heading
  = FacingWest
  | FacingEast
  deriving (Eq)

data GameState =
  GameState
    { position :: Point
    , direction :: Direction
    , heading :: Heading
    , currentLevel :: Level
    , spriteCount :: Int
    , speedX :: Float
    , speedY :: Float
    }

type CellType = Char

type Cell = (Point, CellType)

type Level = [Cell]

tileSize :: Float
tileSize = 32.0

window :: Display
window = InWindow "Play w. Gloss" (1024, 768) (0, 0)

background :: Color
background = makeColor 0.2 0.1 0.1 1

fps :: Int
fps = 60

isHit :: Point -> Point -> Bool
isHit (b1x, b1y) (b2x, b2y) =
  (b1x - 10) < b2x + tileSize &&
  b1x + 50 - 10 > b2x && b1y < b2y + tileSize && b1y + 54 > b2y

makeRow :: String -> Int -> Level
makeRow row y =
  [ ( ( (fromIntegral x * tileSize) - ((1024 / 2) - (tileSize / 2))
      , (fromIntegral y * tileSize) - ((768 / 2) - (tileSize / 2)))
    , row !! x)
  | x <- [0 .. length row - 1]
  , row !! x == '*' || row !! x == '%'
  ]

prepareData :: [String] -> Level
prepareData rawData =
  concat [makeRow (rawData !! y) y | y <- [0 .. length rawData - 1]]

whatImg :: Cell -> Picture -> Picture -> Picture
whatImg (_, cellType) tile food =
  if cellType == '*'
    then tile
    else food

drawTile :: Cell -> Picture -> Picture -> Picture
drawTile cell tileImg foodImg =
  uncurry translate (fst cell) (whatImg cell tileImg foodImg)

render :: GameState -> [Picture] -> Picture
render gs imgs =
  pictures
    ([drawTile cell (head imgs) (imgs !! 1) | cell <- currentLevel gs] ++
     [ translate
         (fst (position gs))
         (snd (position gs) + 10)
         (imgs !! (spriteCount gs + 2 + isRight (heading gs)))
     ])

isRight :: Heading -> Int
isRight FacingEast = 6
isRight _ = 0

incSprite :: GameState -> Int
incSprite gs =
  if direction gs /= None
    then if spriteCount gs == 5
           then 0
           else spriteCount gs + 1
    else spriteCount gs

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gs =
  gs {direction = West, heading = FacingWest}
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gs =
  gs {direction = East, heading = FacingEast}
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gs =
  gs
    { speedY =
        if isCollision gs (fst (position gs), snd (position gs) + speedY gs) '*'
          then 6
          else (-6)
    }
handleKeys _ gs = gs {direction = None}

checkFood :: GameState -> Level
checkFood gs =
  filter
    (\cell -> not (isHit (fst cell) (position gs) && snd cell == '%'))
    (currentLevel gs)

checkSpeedY :: GameState -> Float
checkSpeedY gs
  | isCollision gs (fst (position gs), snd (position gs) + speedY gs) '*' = -3
  | speedY gs >= -6 = speedY gs - 0.1
  | otherwise = -6

checkSpeedX :: GameState -> Float
checkSpeedX gs
  | direction gs == West || direction gs == East =
    if speedX gs > 5.0
      then 5.0
      else speedX gs + 0.5
  | otherwise =
    if speedX gs <= 1
      then 1.0
      else speedX gs - 0.5

isCollision :: GameState -> Point -> CellType -> Bool
isCollision gs pnt checkType =
  any
    (\((x, y), tileType) -> tileType == checkType && isHit pnt (x, y))
    (currentLevel gs)

moveX :: Direction -> GameState -> Point
moveX East gs =
  if not (isCollision gs (fst (position gs) + speedX gs, snd (position gs)) '*')
    then (fst (position gs) + speedX gs, snd (position gs))
    else position gs
moveX West gs =
  if not
       (isCollision
          gs
          (fst (position gs) + speedX gs * (-1), snd (position gs))
          '*')
    then (fst (position gs) + speedX gs * (-1), snd (position gs))
    else position gs
moveX _ gs = position gs

moveY :: GameState -> Point -> Point
moveY gs pnt =
  if not (isCollision gs (fst pnt, snd pnt + speedY gs) '*')
    then (fst pnt, snd pnt + speedY gs)
    else pnt

update :: Float -> GameState -> GameState
update _ gs =
  gs
    { speedY = checkSpeedY gs
    , speedX = checkSpeedX gs
    , position = moveY gs $ moveX (direction gs) gs
    , spriteCount = incSprite gs
    , currentLevel = checkFood gs
    }

main :: IO ()
main = do
  tileImg <- loadBMP "assets/tile.bmp"
  foodImg <- loadBMP "assets/food.bmp"
  left1 <- loadBMP "assets/left1.bmp"
  left2 <- loadBMP "assets/left2.bmp"
  left3 <- loadBMP "assets/left3.bmp"
  left4 <- loadBMP "assets/left4.bmp"
  left5 <- loadBMP "assets/left5.bmp"
  left6 <- loadBMP "assets/left6.bmp"
  right1 <- loadBMP "assets/right1.bmp"
  right2 <- loadBMP "assets/right2.bmp"
  right3 <- loadBMP "assets/right3.bmp"
  right4 <- loadBMP "assets/right4.bmp"
  right5 <- loadBMP "assets/right5.bmp"
  right6 <- loadBMP "assets/right6.bmp"
  rawData <- readFile "assets/level"
  let level = prepareData $ reverse $ lines rawData
  let state =
        GameState
          { position = (0.0, 0.0)
          , direction = None
          , currentLevel = level
          , spriteCount = 0
          , heading = FacingWest
          , speedX = 2.0
          , speedY = (-6)
          }
  play
    window
    background
    fps
    state
    (`render` [ tileImg
              , foodImg
              , left1
              , left2
              , left3
              , left4
              , left5
              , left6
              , right1
              , right2
              , right3
              , right4
              , right5
              , right6
              ])
    handleKeys
    update
