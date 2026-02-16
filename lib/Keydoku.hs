module Keydoku where

import Data.Char (intToDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Graphics.Vty
  ( Attr,
    Event (EvKey),
    Image,
    Key (KChar, KEsc),
    Vty,
    black,
    brightCyan,
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
  deriving (Eq, Ord, Show)

data SelectionPhase
  = SelectQuadrant
  | SelectCell
  | SelectValue
  deriving (Eq, Show)

data GameState = GameState
  { phase :: SelectionPhase,
    selectedQuadrant :: Maybe KeypadPos,
    selectedCell :: Maybe KeypadPos,
    values :: Map KeypadPos Int
  }
  deriving (Eq, Show)

initialState :: GameState
initialState =
  GameState
    { phase = SelectQuadrant,
      selectedQuadrant = Nothing,
      selectedCell = Nothing,
      values = Map.empty
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
    EvKey (KChar key) [] -> loop vty (handleKey key state)
    _ -> loop vty state

handleKey :: Char -> GameState -> GameState
handleKey key state
  | key == 'h' = deselect state
  | key == 'n' = clearSelectedCellValue state
  | otherwise =
      case state.phase of
        SelectQuadrant -> maybe state (`selectQuadrant` state) (selectionKeypadPosition key)
        SelectCell -> maybe state (`selectCell` state) (selectionKeypadPosition key)
        SelectValue -> maybe state (`selectValue` state) (valueKeyToDigit key)

deselect :: GameState -> GameState
deselect state =
  state
    { phase = SelectQuadrant,
      selectedQuadrant = Nothing,
      selectedCell = Nothing
    }

selectQuadrant :: KeypadPos -> GameState -> GameState
selectQuadrant pos state =
  state
    { phase = SelectCell,
      selectedQuadrant = Just pos,
      selectedCell = Nothing
    }

selectCell :: KeypadPos -> GameState -> GameState
selectCell pos state =
  case state.selectedQuadrant of
    Nothing -> state
    Just quadrant ->
      state
        { phase = SelectValue,
          selectedCell = Just (absoluteCell quadrant pos)
        }

toggleSelectedValue :: Int -> GameState -> GameState
toggleSelectedValue digit state =
  case state.selectedCell of
    Nothing -> state
    Just cell ->
      state
        { values = toggleCellDigit cell digit state.values
        }

selectValue :: Int -> GameState -> GameState
selectValue digit state = deselect (toggleSelectedValue digit state)

clearSelectedCellValue :: GameState -> GameState
clearSelectedCellValue state =
  case state.selectedCell of
    Nothing -> state
    Just cell ->
      deselect
        state
          { values = Map.delete cell state.values
          }

toggleCellDigit :: KeypadPos -> Int -> Map KeypadPos Int -> Map KeypadPos Int
toggleCellDigit cell digit currentValues =
  case Map.lookup cell currentValues of
    Just existing | existing == digit -> Map.delete cell currentValues
    _ -> Map.insert cell digit currentValues

cellValueAt :: GameState -> KeypadPos -> Maybe Int
cellValueAt state pos = Map.lookup pos state.values

absoluteCell :: KeypadPos -> KeypadPos -> KeypadPos
absoluteCell quadrant cell =
  KeypadPos
    { row = quadrant.row * 3 + cell.row,
      col = quadrant.col * 3 + cell.col
    }

selectionKeypadPosition :: Char -> Maybe KeypadPos
selectionKeypadPosition key =
  case key of
    'u' -> Just (KeypadPos 0 0)
    'i' -> Just (KeypadPos 0 1)
    'p' -> Just (KeypadPos 0 2)
    'j' -> Just (KeypadPos 1 0)
    'k' -> Just (KeypadPos 1 1)
    'l' -> Just (KeypadPos 1 2)
    'm' -> Just (KeypadPos 2 0)
    ',' -> Just (KeypadPos 2 1)
    '.' -> Just (KeypadPos 2 2)
    _ -> Nothing

valueKeyToDigit :: Char -> Maybe Int
valueKeyToDigit key =
  case key of
    'm' -> Just 1
    ',' -> Just 2
    '.' -> Just 3
    'j' -> Just 4
    'k' -> Just 5
    'l' -> Just 6
    'u' -> Just 7
    'i' -> Just 8
    'o' -> Just 9
    _ -> Nothing

render :: GameState -> Image
render state =
  vertCat
    [ renderBoard state,
      string defAttr "",
      string defAttr (statusText state),
      string defAttr "Select: u i p / j k l / m , .",
      string defAttr "Value:  u i o / j k l / m , .    n=clear value, h=deselect, q/Esc=quit"
    ]

statusText :: GameState -> String
statusText state =
  case state.phase of
    SelectQuadrant -> "Step 1/3: select quadrant"
    SelectCell -> "Step 2/3: select cell inside highlighted quadrant"
    SelectValue -> "Step 3/3: select value key (toggle)"

renderBoard :: GameState -> Image
renderBoard state =
  vertCat
    [ renderLine state y line
      | (y, line) <- zip [0 ..] boardLines
    ]

renderLine :: GameState -> Int -> String -> Image
renderLine state y line =
  horizCat
    [ char (attrFor state x y) (charAt state x y baseChar)
      | (x, baseChar) <- zip [0 ..] line
    ]

charAt :: GameState -> Int -> Int -> Char -> Char
charAt state x y baseChar =
  case valueAtDisplayCoord state x y of
    Nothing -> baseChar
    Just value -> intToDigit value

attrFor :: GameState -> Int -> Int -> Attr
attrFor state x y
  | hasPlacedValueAt state x y = highlightedBase `withForeColor` brightCyan
  | otherwise = highlightedBase
  where
    highlightedBase
      | inSelectedCell state x y = cellAttr
      | onQuadrantBorder state x y = borderAttr
      | otherwise = baseAttr

hasPlacedValueAt :: GameState -> Int -> Int -> Bool
hasPlacedValueAt state x y =
  case valueAtDisplayCoord state x y of
    Nothing -> False
    Just _ -> True

valueAtDisplayCoord :: GameState -> Int -> Int -> Maybe Int
valueAtDisplayCoord state x y
  | x < 4 || y < 2 = Nothing
  | (x - 4) `mod` 8 /= 0 = Nothing
  | (y - 2) `mod` 4 /= 0 = Nothing
  | cellRow > 8 || cellCol > 8 = Nothing
  | otherwise = cellValueAt state (KeypadPos cellRow cellCol)
  where
    cellCol = (x - 4) `div` 8
    cellRow = (y - 2) `div` 4

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
