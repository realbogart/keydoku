module Keydoku where

import Control.Exception (IOException, try)
import Data.Char (digitToInt, intToDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Graphics.Vty
  ( Attr,
    Event (EvKey),
    Image,
    Key (KChar, KEsc, KFun),
    Vty,
    black,
    brightBlack,
    char,
    cyan,
    defAttr,
    green,
    horizCat,
    nextEvent,
    picForImage,
    red,
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
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

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
    values :: Map KeypadPos Int,
    fixedCells :: Set KeypadPos,
    undoHistory :: [BoardSnapshot],
    redoHistory :: [BoardSnapshot],
    statusMessage :: Maybe StatusMessage
  }
  deriving (Eq, Show)

data BoardSnapshot = BoardSnapshot
  { snapshotValues :: Map KeypadPos Int,
    snapshotFixedCells :: Set KeypadPos
  }
  deriving (Eq, Show)

data StatusMessage
  = StatusInfo String
  | StatusError String
  deriving (Eq, Show)

data RenderContext = RenderContext
  { solved :: Bool,
    conflicts :: Set KeypadPos
  }
  deriving (Eq, Show)

initialState :: GameState
initialState =
  GameState
    { phase = SelectQuadrant,
      selectedQuadrant = Nothing,
      selectedCell = Nothing,
      values = Map.empty,
      fixedCells = Set.empty,
      undoHistory = [],
      redoHistory = [],
      statusMessage = Nothing
    }

main :: IO ()
main = mainWithBoardFile Nothing

mainWithBoardFile :: Maybe FilePath -> IO ()
mainWithBoardFile maybeBoardFile = do
  initial <- loadInitialState maybeBoardFile
  vty <- userConfig >>= mkVty
  loop vty initial

loadInitialState :: Maybe FilePath -> IO GameState
loadInitialState maybeBoardFile =
  case maybeBoardFile of
    Nothing -> pure initialState
    Just boardFile -> loadStateFromBoardFile boardFile

loadStateFromBoardFile :: FilePath -> IO GameState
loadStateFromBoardFile boardFile = do
  fileContentResult <- readTextFile boardFile
  pure $
    case fileContentResult of
      Left err ->
        initialState
          { statusMessage =
              Just
                ( StatusError
                    ( "Failed to load board file "
                        ++ show boardFile
                        ++ ": "
                        ++ err
                    )
                )
          }
      Right fileContent ->
        case parseClipboardBoard fileContent of
          Left err ->
            initialState
              { statusMessage =
                  Just
                    ( StatusError
                        ( "Invalid board file "
                            ++ show boardFile
                            ++ ": "
                            ++ err
                        )
                    )
              }
          Right boardValues ->
            let fixed = Map.keysSet boardValues
             in initialState
                  { values = boardValues,
                    fixedCells = fixed,
                    statusMessage = Just (StatusInfo ("Loaded board file " ++ show boardFile ++ "."))
                  }

loop :: Vty -> GameState -> IO ()
loop vty state = do
  update vty (picForImage (render state))
  event <- nextEvent vty
  case event of
    EvKey KEsc [] -> shutdown vty
    EvKey (KChar 'q') [] -> shutdown vty
    EvKey (KFun 2) [] -> do
      updated <- loadBoardFromClipboard state
      loop vty updated
    EvKey (KChar key) [] -> loop vty (handleKey key state)
    _ -> loop vty state

handleKey :: Char -> GameState -> GameState
handleKey key state
  | key == 'h' = clearStatusMessage (deselect state)
  | key == 'y' = clearStatusMessage (undoLastBoardChange state)
  | key == 'p' = clearStatusMessage (redoLastBoardChange state)
  | key == 'n' = clearStatusMessage (clearSelectedCellValue state)
  | otherwise =
      case state.phase of
        SelectQuadrant -> maybe state (clearStatusMessage . (`selectQuadrant` state)) (selectionKeypadPosition key)
        SelectCell -> maybe state (clearStatusMessage . (`selectCell` state)) (selectionKeypadPosition key)
        SelectValue -> maybe state (clearStatusMessage . (`selectValue` state)) (valueKeyToDigit key)

clearStatusMessage :: GameState -> GameState
clearStatusMessage state = state {statusMessage = Nothing}

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
    Just cell | Set.member cell state.fixedCells -> state
    Just cell ->
      state
        { values = toggleCellDigit cell digit state.values
        }

selectValue :: Int -> GameState -> GameState
selectValue digit state =
  let updatedState = deselect (toggleSelectedValue digit state)
   in recordUndoIfBoardChanged state updatedState

clearSelectedCellValue :: GameState -> GameState
clearSelectedCellValue state =
  let updatedState =
        case state.selectedCell of
          Nothing -> state
          Just cell | Set.member cell state.fixedCells -> deselect state
          Just cell ->
            deselect
              state
                { values = Map.delete cell state.values
                }
   in recordUndoIfBoardChanged state updatedState

undoLastBoardChange :: GameState -> GameState
undoLastBoardChange state =
  case state.undoHistory of
    [] -> state
    latestSnapshot : remainingSnapshots ->
      let currentSnapshot = boardSnapshot state
       in deselect
            state
              { values = latestSnapshot.snapshotValues,
                fixedCells = latestSnapshot.snapshotFixedCells,
                undoHistory = remainingSnapshots,
                redoHistory = currentSnapshot : state.redoHistory
              }

redoLastBoardChange :: GameState -> GameState
redoLastBoardChange state =
  case state.redoHistory of
    [] -> state
    latestSnapshot : remainingSnapshots ->
      let currentSnapshot = boardSnapshot state
       in deselect
            state
              { values = latestSnapshot.snapshotValues,
                fixedCells = latestSnapshot.snapshotFixedCells,
                undoHistory = currentSnapshot : state.undoHistory,
                redoHistory = remainingSnapshots
              }

boardSnapshot :: GameState -> BoardSnapshot
boardSnapshot state =
  BoardSnapshot
    { snapshotValues = state.values,
      snapshotFixedCells = state.fixedCells
    }

recordUndoIfBoardChanged :: GameState -> GameState -> GameState
recordUndoIfBoardChanged previousState updatedState
  | boardSnapshot previousState == boardSnapshot updatedState = updatedState
  | otherwise =
      updatedState
        { undoHistory = boardSnapshot previousState : previousState.undoHistory,
          redoHistory = []
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
    'o' -> Just (KeypadPos 0 2)
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
  let context = renderContext state
   in vertCat
        [ renderBoard context state,
          string defAttr "",
          string (messageAttr context state) (statusTextWithContext context state),
          string defAttr "Select: u i o / j k l / m , .",
          string defAttr "Value:  u i o / j k l / m , .    y=undo, p=redo, n=clear value, h=deselect, F2=paste board, q/Esc=quit"
        ]

renderContext :: GameState -> RenderContext
renderContext state =
  let currentConflicts = conflictingCells state
   in RenderContext
        { solved = isSolvedFromConflicts state currentConflicts,
          conflicts = currentConflicts
        }

statusText :: GameState -> String
statusText state =
  statusTextWithContext (renderContext state) state

statusTextWithContext :: RenderContext -> GameState -> String
statusTextWithContext context state =
  if context.solved
    then "Solved!"
    else case state.statusMessage of
      Just (StatusInfo message) -> message
      Just (StatusError message) -> message
      Nothing ->
        case state.phase of
          SelectQuadrant -> "Step 1/3: select quadrant"
          SelectCell -> "Step 2/3: select cell inside highlighted quadrant"
          SelectValue -> "Step 3/3: select value key (toggle)"

messageAttr :: RenderContext -> GameState -> Attr
messageAttr context state =
  if context.solved
    then defAttr `withForeColor` green
    else case state.statusMessage of
      Nothing -> defAttr
      Just (StatusInfo _) -> defAttr `withForeColor` green
      Just (StatusError _) -> defAttr `withForeColor` red

loadBoardFromClipboard :: GameState -> IO GameState
loadBoardFromClipboard state = do
  clipboardResult <- readClipboard
  pure $
    case clipboardResult >>= parseClipboardBoard of
      Left err ->
        state
          { statusMessage = Just (StatusError ("Clipboard import failed: " ++ err))
          }
      Right boardValues ->
        let fixed = Map.keysSet boardValues
            updatedState =
              state
                { phase = SelectQuadrant,
                  selectedQuadrant = Nothing,
                  selectedCell = Nothing,
                  values = boardValues,
                  fixedCells = fixed,
                  statusMessage = Just (StatusInfo "Loaded board from clipboard.")
                }
         in recordUndoIfBoardChanged state updatedState

readClipboard :: IO (Either String String)
readClipboard = do
  available <- findFirstAvailable ["wl-paste", "xclip", "pbpaste"]
  case available of
    Nothing -> pure (Left "no supported clipboard command found (wl-paste, xclip, pbpaste)")
    Just command ->
      case command of
        "wl-paste" -> runClipboardCommand command ["-n"]
        "xclip" -> runClipboardCommand command ["-selection", "clipboard", "-o"]
        "pbpaste" -> runClipboardCommand command []
        _ -> pure (Left "unsupported clipboard command")

findFirstAvailable :: [FilePath] -> IO (Maybe FilePath)
findFirstAvailable = go
  where
    go [] = pure Nothing
    go (command : rest) = do
      executable <- findExecutable command
      case executable of
        Just _ -> pure (Just command)
        Nothing -> go rest

runClipboardCommand :: FilePath -> [String] -> IO (Either String String)
runClipboardCommand command args = do
  (exitCode, stdoutText, stderrText) <- readProcessWithExitCode command args ""
  pure $
    case exitCode of
      ExitSuccess -> Right (normalizeClipboardText stdoutText)
      _ ->
        Left
          ( "failed to read clipboard via "
              ++ command
              ++ ": "
              ++ firstNonEmpty stderrText stdoutText
          )

normalizeClipboardText :: String -> String
normalizeClipboardText = filter (/= '\r')

readTextFile :: FilePath -> IO (Either String String)
readTextFile path = do
  readResult <- try (readFile path) :: IO (Either IOException String)
  pure $
    case readResult of
      Left err -> Left (show err)
      Right fileContent -> Right (normalizeClipboardText fileContent)

firstNonEmpty :: String -> String -> String
firstNonEmpty first second =
  if null first
    then second
    else first

parseClipboardBoard :: String -> Either String (Map KeypadPos Int)
parseClipboardBoard clipboardText = do
  let boardRows = lines clipboardText
  if length boardRows /= 9
    then Left "expected exactly 9 lines"
    else
      if any ((/= 9) . length) boardRows
        then Left "each line must contain exactly 9 characters"
        else parseRows boardRows

parseRows :: [String] -> Either String (Map KeypadPos Int)
parseRows rows =
  fmap (Map.fromList . concat) (traverse parseRow (zip [0 ..] rows))

parseRow :: (Int, String) -> Either String [(KeypadPos, Int)]
parseRow (rowIndex, rowText) =
  fmap concat (traverse (parseCell rowIndex) (zip [0 ..] rowText))

allCells :: [String] -> [(Int, Int, Char)]
allCells rows =
  [ (rowIndex, colIndex, cellChar)
    | (rowIndex, rowText) <- zip [0 ..] rows,
      (colIndex, cellChar) <- zip [0 ..] rowText
  ]

parseCell :: Int -> (Int, Char) -> Either String [(KeypadPos, Int)]
parseCell rowIndex (colIndex, cellChar)
  | cellChar == ' ' = Right []
  | cellChar >= '1' && cellChar <= '9' =
      Right [(KeypadPos rowIndex colIndex, digitToInt cellChar)]
  | otherwise =
      Left
        ( "invalid character at row "
            ++ show (rowIndex + 1)
            ++ ", col "
            ++ show (colIndex + 1)
            ++ " (use digits 1-9 or spaces)"
        )

renderBoard :: RenderContext -> GameState -> Image
renderBoard context state =
  vertCat
    [ renderLine context state y line
      | (y, line) <- zip [0 ..] boardLines
    ]

renderLine :: RenderContext -> GameState -> Int -> String -> Image
renderLine context state y line =
  horizCat
    [ char (attrFor context state x y baseChar) (charAt state x y baseChar)
      | (x, baseChar) <- zip [0 ..] line
    ]

charAt :: GameState -> Int -> Int -> Char -> Char
charAt state x y baseChar =
  case valueAtDisplayCoord state x y of
    Just value -> intToDigit value
    Nothing ->
      case candidateAtDisplayCoord state x y of
        Just candidate -> intToDigit candidate
        Nothing ->
          if baseChar == '.'
            then ' '
            else baseChar

attrFor :: RenderContext -> GameState -> Int -> Int -> Char -> Attr
attrFor context state x y baseChar
  | isConflictAt context.conflicts x y = highlightedBase `withForeColor` red
  | isSelectedValueMatchAt state x y = highlightedBase `withForeColor` black `withBackColor` cyan
  | isFixedCellAt state x y = highlightedBase `withForeColor` white
  | hasPlacedValueAt state x y = highlightedBase `withForeColor` green
  | hasCandidateAt state x y = highlightedBase `withForeColor` brightBlack
  | otherwise = highlightedBase
  where
    highlightedBase
      | context.solved && isBorderChar baseChar = baseAttr `withForeColor` green
      | inSelectedCell state x y = cellAttr
      | onQuadrantBorder state x y = borderAttr
      | otherwise = baseAttr

isBorderChar :: Char -> Bool
isBorderChar c = c /= '.' && c /= ' '

hasPlacedValueAt :: GameState -> Int -> Int -> Bool
hasPlacedValueAt state x y =
  case valueAtDisplayCoord state x y of
    Nothing -> False
    Just _ -> not (isFixedCellAt state x y)

isFixedCellAt :: GameState -> Int -> Int -> Bool
isFixedCellAt state x y =
  case cellAtDisplayCoord x y of
    Nothing -> False
    Just cell -> Set.member cell state.fixedCells

isConflictAt :: Set KeypadPos -> Int -> Int -> Bool
isConflictAt conflictsAtFrame x y =
  case cellAtDisplayCoord x y of
    Nothing -> False
    Just cell -> Set.member cell conflictsAtFrame

hasCandidateAt :: GameState -> Int -> Int -> Bool
hasCandidateAt state x y =
  case candidateAtDisplayCoord state x y of
    Nothing -> False
    Just _ -> True

selectedFilledValue :: GameState -> Maybe Int
selectedFilledValue state = do
  selected <- state.selectedCell
  cellValueAt state selected

isSelectedValueMatchAt :: GameState -> Int -> Int -> Bool
isSelectedValueMatchAt state x y =
  case (selectedFilledValue state, valueAtDisplayCoord state x y) of
    (Just selectedValue, Just value) -> selectedValue == value
    _ -> False

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

cellAtDisplayCoord :: Int -> Int -> Maybe KeypadPos
cellAtDisplayCoord x y
  | x < 1 || y < 1 = Nothing
  | xRel >= 7 || yRel >= 3 = Nothing
  | cellRow > 8 || cellCol > 8 = Nothing
  | otherwise = Just (KeypadPos cellRow cellCol)
  where
    xOffset = x - 1
    yOffset = y - 1
    xRel = xOffset `mod` 8
    yRel = yOffset `mod` 4
    cellCol = xOffset `div` 8
    cellRow = yOffset `div` 4

candidateAtDisplayCoord :: GameState -> Int -> Int -> Maybe Int
candidateAtDisplayCoord state x y = do
  (cellPos, candidateDigit) <- candidateSlotAtDisplayCoord x y
  if hasValueAt state cellPos
    then Nothing
    else
      if candidateDigit `elem` allowedDigitsAt state cellPos
        then Just candidateDigit
        else Nothing

candidateSlotAtDisplayCoord :: Int -> Int -> Maybe (KeypadPos, Int)
candidateSlotAtDisplayCoord x y
  | x < 2 || y < 1 = Nothing
  | xRel == 6 || xRel == 7 = Nothing
  | yRel == 3 = Nothing
  | xRel `mod` 2 /= 0 = Nothing
  | cellRow > 8 || cellCol > 8 = Nothing
  | otherwise =
      Just
        ( KeypadPos cellRow cellCol,
          digitAtCandidatePos candRow candCol
        )
  where
    xOffset = x - 2
    yOffset = y - 1
    xRel = xOffset `mod` 8
    yRel = yOffset `mod` 4
    candCol = xRel `div` 2
    candRow = yRel
    cellCol = xOffset `div` 8
    cellRow = yOffset `div` 4

digitAtCandidatePos :: Int -> Int -> Int
digitAtCandidatePos candRow candCol =
  case candRow of
    0 -> 7 + candCol
    1 -> 4 + candCol
    2 -> 1 + candCol
    _ -> 0

hasValueAt :: GameState -> KeypadPos -> Bool
hasValueAt state cell =
  case cellValueAt state cell of
    Nothing -> False
    Just _ -> True

allowedDigitsAt :: GameState -> KeypadPos -> [Int]
allowedDigitsAt state cell
  | hasValueAt state cell = []
  | otherwise =
      [ digit
        | digit <- [1 .. 9],
          not (digit `elem` usedDigits)
      ]
  where
    usedDigits = rowDigits state cell ++ colDigits state cell ++ boxDigits state cell

rowDigits :: GameState -> KeypadPos -> [Int]
rowDigits state cell =
  [ value
    | (KeypadPos valueRow _valueCol, value) <- Map.toList state.values,
      valueRow == cell.row
  ]

colDigits :: GameState -> KeypadPos -> [Int]
colDigits state cell =
  [ value
    | (KeypadPos _valueRow valueCol, value) <- Map.toList state.values,
      valueCol == cell.col
  ]

boxDigits :: GameState -> KeypadPos -> [Int]
boxDigits state cell =
  [ value
    | (KeypadPos valueRow valueCol, value) <- Map.toList state.values,
      valueRow `div` 3 == cell.row `div` 3,
      valueCol `div` 3 == cell.col `div` 3
  ]

conflictingCells :: GameState -> Set KeypadPos
conflictingCells state =
  Set.fromList
    [ cell
      | (cell, value) <- Map.toList state.values,
        hasConflict cell value
    ]
  where
    hasConflict cell value =
      any
        (\(otherCell, otherValue) -> cell /= otherCell && value == otherValue && conflictsWith cell otherCell)
        (Map.toList state.values)

    conflictsWith left right =
      sameRow left right || sameCol left right || sameBox left right

    sameRow left right = left.row == right.row
    sameCol left right = left.col == right.col
    sameBox left right = left.row `div` 3 == right.row `div` 3 && left.col `div` 3 == right.col `div` 3

isSolved :: GameState -> Bool
isSolved state = isSolvedFromConflicts state (conflictingCells state)

isSolvedFromConflicts :: GameState -> Set KeypadPos -> Bool
isSolvedFromConflicts state currentConflicts =
  Map.size state.values == 81
    && Set.null currentConflicts

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
