module Keydoku where

import Data.Maybe (fromMaybe)
import Graphics.Vty
  ( Attr,
    Event (EvKey),
    Image,
    Key (KChar, KEsc),
    Vty,
    black,
    char,
    defAttr,
    horizCat,
    nextEvent,
    picForImage,
    shutdown,
    string,
    update,
    userConfig,
    vertCat,
    white,
    withBackColor,
    withForeColor,
    yellow,
  )
import Graphics.Vty.CrossPlatform (mkVty)

-- | 3x3 keypad position used for quadrant and cell selection.
data KeypadPos = KeypadPos
  { row :: Int,
    col :: Int
  }
  deriving (Eq, Show)

data SelectionPhase
  = SelectQuadrant
  | SelectCell
  deriving (Eq, Show)

data GameState = GameState
  { phase :: SelectionPhase,
    selectedQuadrant :: Maybe KeypadPos,
    selectedCell :: Maybe KeypadPos
  }
  deriving (Eq, Show)

initialState :: GameState
initialState =
  GameState
    { phase = SelectQuadrant,
      selectedQuadrant = Nothing,
      selectedCell = Nothing
    }

main :: IO ()
main = do
  vty <- userConfig >>= mkVty
  loop vty initialState

loop :: Vty -> GameState -> IO ()
loop vty state = do
  update vty (picForImage (render state))
  event <- nextEvent vty
  case event of
    EvKey KEsc [] -> shutdown vty
    EvKey (KChar 'q') [] -> shutdown vty
    EvKey (KChar key) [] ->
      loop vty (fromMaybe state (advanceWithKey key state))
    _ -> loop vty state

advanceWithKey :: Char -> GameState -> Maybe GameState
advanceWithKey key state = advanceSelection <$> keypadPosition key <*> pure state

advanceSelection :: KeypadPos -> GameState -> GameState
advanceSelection pos state =
  case state.phase of
    SelectQuadrant ->
      state
        { phase = SelectCell,
          selectedQuadrant = Just pos,
          selectedCell = Nothing
        }
    SelectCell ->
      case state.selectedQuadrant of
        Nothing -> state
        Just quadrant ->
          state
            { phase = SelectQuadrant,
              selectedCell = Just (absoluteCell quadrant pos)
            }

absoluteCell :: KeypadPos -> KeypadPos -> KeypadPos
absoluteCell quadrant cell =
  KeypadPos
    { row = quadrant.row * 3 + cell.row,
      col = quadrant.col * 3 + cell.col
    }

keypadPosition :: Char -> Maybe KeypadPos
keypadPosition key =
  case key of
    'u' -> Just (KeypadPos 0 0)
    'i' -> Just (KeypadPos 0 1)
    'o' -> Just (KeypadPos 0 2)
    'j' -> Just (KeypadPos 1 0)
    'k' -> Just (KeypadPos 1 1)
    'l' -> Just (KeypadPos 1 2)
    'm' -> Just (KeypadPos 2 0)
    ',' -> Just (KeypadPos 2 1)
    '.' -> Just (KeypadPos 2 2)
    _ -> Nothing

render :: GameState -> Image
render state =
  vertCat
    [ renderBoard state,
      string defAttr "",
      string defAttr (statusText state),
      string defAttr "Keys: u i p / j k l / m , .   (q or Esc to quit)"
    ]

statusText :: GameState -> String
statusText state =
  case state.phase of
    SelectQuadrant -> "Select quadrant (3x3 block)"
    SelectCell -> "Select cell inside highlighted quadrant"

renderBoard :: GameState -> Image
renderBoard state =
  vertCat
    [ renderLine state y line
      | (y, line) <- zip [0 ..] boardLines
    ]

renderLine :: GameState -> Int -> String -> Image
renderLine state y line =
  horizCat
    [ char (attrFor state x y c) c
      | (x, c) <- zip [0 ..] line
    ]

attrFor :: GameState -> Int -> Int -> Char -> Attr
attrFor state x y _c
  | inSelectedCell state x y = cellAttr
  | onQuadrantBorder state x y = borderAttr
  | otherwise = baseAttr

inSelectedCell :: GameState -> Int -> Int -> Bool
inSelectedCell state x y =
  case state.selectedCell of
    Nothing -> False
    Just cell ->
      let xStart = 1 + cell.col * 8
          yStart = 1 + cell.row * 4
       in x >= xStart && x <= xStart + 6 && y >= yStart && y <= yStart + 2

onQuadrantBorder :: GameState -> Int -> Int -> Bool
onQuadrantBorder state x y =
  case state.selectedQuadrant of
    Nothing -> False
    Just quadrant ->
      let xLeft = quadrant.col * 24
          xRight = (quadrant.col + 1) * 24
          yTop = quadrant.row * 12
          yBottom = (quadrant.row + 1) * 12
          onTopOrBottom = y == yTop || y == yBottom
          onLeftOrRight = x == xLeft || x == xRight
       in ((x >= xLeft && x <= xRight) && onTopOrBottom)
            || ((y >= yTop && y <= yBottom) && onLeftOrRight)

baseAttr :: Attr
baseAttr = defAttr `withForeColor` white

borderAttr :: Attr
borderAttr = baseAttr `withForeColor` yellow

cellAttr :: Attr
cellAttr = baseAttr `withForeColor` black `withBackColor` yellow

boardLines :: [String]
boardLines = topLine : concatMap rowGroup [0 .. 8] ++ [bottomLine]
  where
    rowGroup rowIndex =
      let rows = replicate 3 contentLine
          separator
            | rowIndex == 8 = []
            | rowIndex `mod` 3 == 2 = [majorSeparatorLine]
            | otherwise = [minorSeparatorLine]
       in rows ++ separator

topLine :: String
topLine = makeSeparator '╔' '╗' '╤' '╦' '═'

bottomLine :: String
bottomLine = makeSeparator '╚' '╝' '╧' '╩' '═'

majorSeparatorLine :: String
majorSeparatorLine = makeSeparator '╠' '╣' '╪' '╬' '═'

minorSeparatorLine :: String
minorSeparatorLine = makeSeparator '╟' '╢' '┼' '╫' '─'

contentLine :: String
contentLine =
  "║"
    ++ concat
      [ cellRow blockCol
          ++ if blockCol < 2 then "║" else ""
        | blockCol <- [0 .. 2]
      ]
    ++ "║"
  where
    cellRow _blockCol =
      concat
        [ " . . . " ++ if colInBlock < 2 then "│" else ""
          | colInBlock <- [0 .. 2]
        ]

makeSeparator :: Char -> Char -> Char -> Char -> Char -> String
makeSeparator left right minorCross majorCross fill =
  [left]
    ++ concat
      [ chunk blockCol
          ++ if blockCol < 2 then [majorCross] else ""
        | blockCol <- [0 .. 2]
      ]
    ++ [right]
  where
    chunk _blockCol =
      concat
        [ replicate 7 fill ++ if colInBlock < 2 then [minorCross] else ""
          | colInBlock <- [0 .. 2]
        ]
