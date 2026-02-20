module Keydoku where

import Control.Exception (IOException, try)
import Data.Char (digitToInt, intToDigit)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.Vty
  ( Attr,
    Event (EvKey),
    Image,
    Key (KChar, KDel, KEsc, KFun),
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
import System.Timeout (timeout)

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

data InsertionMode
  = InsertValues
  | RemoveCandidates
  deriving (Eq, Show)

data GameState = GameState
  { phase :: SelectionPhase,
    selectedQuadrant :: Maybe KeypadPos,
    selectedCell :: Maybe KeypadPos,
    highlightedDigit :: Maybe Int,
    insertionMode :: InsertionMode,
    values :: Map KeypadPos Int,
    removedCandidates :: Map KeypadPos (Set Int),
    fixedCells :: Set KeypadPos,
    undoHistory :: [BoardSnapshot],
    redoHistory :: [BoardSnapshot],
    statusMessage :: Maybe StatusMessage
  }
  deriving (Eq, Show)

data BoardSnapshot = BoardSnapshot
  { snapshotValues :: Map KeypadPos Int,
    snapshotRemovedCandidates :: Map KeypadPos (Set Int),
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

data TimerState = TimerState
  { startedAt :: UTCTime,
    frozenElapsedSeconds :: Maybe Int
  }
  deriving (Eq, Show)

initialState :: GameState
initialState =
  GameState
    { phase = SelectQuadrant,
      selectedQuadrant = Nothing,
      selectedCell = Nothing,
      highlightedDigit = Nothing,
      insertionMode = InsertValues,
      values = Map.empty,
      removedCandidates = Map.empty,
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
  timerStarted <- getCurrentTime
  vty <- userConfig >>= mkVty
  loop vty (TimerState timerStarted Nothing) initial

loadInitialState :: Maybe FilePath -> IO GameState
loadInitialState maybeBoardFile =
  case maybeBoardFile of
    Nothing -> startNewHardGame
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
                    removedCandidates = Map.empty,
                    fixedCells = fixed,
                    statusMessage = Just (StatusInfo ("Loaded board file " ++ show boardFile ++ "."))
                  }

loop :: Vty -> TimerState -> GameState -> IO ()
loop vty timer state = do
  now <- getCurrentTime
  let timerAtFrame = updateTimer now state timer
      elapsed = elapsedSeconds now timerAtFrame
  update vty (picForImage (render elapsed state))
  maybeEvent <- timeout 200000 (nextEvent vty)
  case maybeEvent of
    Nothing -> loop vty timerAtFrame state
    Just (EvKey KEsc []) -> shutdown vty
    Just (EvKey (KChar 'q') []) -> shutdown vty
    Just (EvKey (KFun 2) []) -> do
      updated <- startNewHardGame
      timerStarted <- getCurrentTime
      loop vty (TimerState timerStarted Nothing) updated
    Just (EvKey KDel []) -> loop vty timerAtFrame (clearStatusMessage (clearSelectedCellValue state))
    Just (EvKey (KChar key) []) -> loop vty timerAtFrame (handleKey key state)
    Just _ -> loop vty timerAtFrame state

updateTimer :: UTCTime -> GameState -> TimerState -> TimerState
updateTimer now state timer =
  case (isSolved state, timer.frozenElapsedSeconds) of
    (True, Nothing) -> timer {frozenElapsedSeconds = Just (elapsedSeconds now timer)}
    (False, Just frozenElapsed) ->
      timer
        { startedAt = addUTCTime (negate (fromIntegral frozenElapsed)) now,
          frozenElapsedSeconds = Nothing
        }
    _ -> timer

elapsedSeconds :: UTCTime -> TimerState -> Int
elapsedSeconds now timer =
  case timer.frozenElapsedSeconds of
    Just frozenElapsed -> frozenElapsed
    Nothing -> max 0 (floor (diffUTCTime now timer.startedAt))

formatElapsed :: Int -> String
formatElapsed elapsed =
  let (minutesElapsed, secondsElapsed) = elapsed `divMod` 60
   in pad2 minutesElapsed ++ ":" ++ pad2 secondsElapsed

pad2 :: Int -> String
pad2 n
  | n < 10 = '0' : show n
  | otherwise = show n

handleKey :: Char -> GameState -> GameState
handleKey key state
  | key == 'h' || key == '0' = clearStatusMessage (deselect state)
  | key == 'y' || key == '-' = clearStatusMessage (undoLastBoardChange state)
  | key == 'p' || key == '*' = clearStatusMessage (redoLastBoardChange state)
  | key == 'n' = clearStatusMessage (clearSelectedCellValue state)
  | key == '+' || key == ':' || key == 'ö' = clearStatusMessage (toggleInsertionMode state)
  | otherwise =
      case state.phase of
        SelectQuadrant -> maybe state (clearStatusMessage . (`selectQuadrant` state)) (selectionKeypadPosition key)
        SelectCell -> maybe state (clearStatusMessage . (`selectCell` state)) (selectionKeypadPosition key)
        SelectValue -> maybe state (clearStatusMessage . (`selectValue` state)) (valueKeyToDigit key)

clearStatusMessage :: GameState -> GameState
clearStatusMessage state = state {statusMessage = Nothing}

clearSelection :: GameState -> GameState
clearSelection state =
  state
    { phase = SelectQuadrant,
      selectedQuadrant = Nothing,
      selectedCell = Nothing
    }

deselect :: GameState -> GameState
deselect state =
  (clearSelection state) {highlightedDigit = Nothing}

selectQuadrant :: KeypadPos -> GameState -> GameState
selectQuadrant pos state =
  state
    { phase = SelectCell,
      selectedQuadrant = Just pos,
      selectedCell = Nothing,
      highlightedDigit = Nothing
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
  let selectedCellIsFixed =
        case state.selectedCell of
          Nothing -> False
          Just cell -> Set.member cell state.fixedCells
      updatedState =
        case state.insertionMode of
          InsertValues
            | selectedCellIsFixed -> deselect state
            | otherwise ->
                let stateAfterInsert = clearSelection (toggleSelectedValue digit state)
                    persistentHighlight =
                      if isSolved stateAfterInsert
                        then Nothing
                        else Just digit
                 in stateAfterInsert
                      { highlightedDigit = persistentHighlight
                      }
          RemoveCandidates ->
            (clearSelection (toggleSelectedCandidateRemoval digit state))
              { highlightedDigit = state.highlightedDigit
              }
   in recordUndoIfBoardChanged state updatedState

toggleInsertionMode :: GameState -> GameState
toggleInsertionMode state =
  state
    { insertionMode =
        case state.insertionMode of
          InsertValues -> RemoveCandidates
          RemoveCandidates -> InsertValues
    }

toggleSelectedCandidateRemoval :: Int -> GameState -> GameState
toggleSelectedCandidateRemoval digit state =
  case state.selectedCell of
    Nothing -> state
    Just cell | Set.member cell state.fixedCells -> state
    Just cell | hasValueAt state cell -> state
    Just cell ->
      state
        { removedCandidates = toggleRemovedCandidate cell digit state.removedCandidates
        }

toggleRemovedCandidate :: KeypadPos -> Int -> Map KeypadPos (Set Int) -> Map KeypadPos (Set Int)
toggleRemovedCandidate cell digit currentRemoved =
  let existing = Map.findWithDefault Set.empty cell currentRemoved
      updated =
        if Set.member digit existing
          then Set.delete digit existing
          else Set.insert digit existing
   in if Set.null updated
        then Map.delete cell currentRemoved
        else Map.insert cell updated currentRemoved

clearSelectedCellValue :: GameState -> GameState
clearSelectedCellValue state =
  let updatedState =
        case state.selectedCell of
          Nothing -> state
          Just cell | Set.member cell state.fixedCells -> clearSelection state
          Just cell ->
            clearSelection
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
                removedCandidates = latestSnapshot.snapshotRemovedCandidates,
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
                removedCandidates = latestSnapshot.snapshotRemovedCandidates,
                fixedCells = latestSnapshot.snapshotFixedCells,
                undoHistory = currentSnapshot : state.undoHistory,
                redoHistory = remainingSnapshots
              }

boardSnapshot :: GameState -> BoardSnapshot
boardSnapshot state =
  BoardSnapshot
    { snapshotValues = state.values,
      snapshotRemovedCandidates = state.removedCandidates,
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
    '7' -> Just (KeypadPos 0 0)
    '8' -> Just (KeypadPos 0 1)
    '9' -> Just (KeypadPos 0 2)
    '4' -> Just (KeypadPos 1 0)
    '5' -> Just (KeypadPos 1 1)
    '6' -> Just (KeypadPos 1 2)
    '1' -> Just (KeypadPos 2 0)
    '2' -> Just (KeypadPos 2 1)
    '3' -> Just (KeypadPos 2 2)
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
    '1' -> Just 1
    '2' -> Just 2
    '3' -> Just 3
    '4' -> Just 4
    '5' -> Just 5
    '6' -> Just 6
    '7' -> Just 7
    '8' -> Just 8
    '9' -> Just 9
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

render :: Int -> GameState -> Image
render elapsed state =
  let context = renderContext state
   in vertCat
        [ renderBoard context state,
          string defAttr "",
          string (messageAttr context state) (statusTextWithContext context state),
          string defAttr ("Time: " ++ formatElapsed elapsed),
          renderModeToggle state,
          string defAttr "Select: 7 8 9 / 4 5 6 / 1 2 3",
          string defAttr "Value:  7 8 9 / 4 5 6 / 1 2 3  Del=clear, -=undo, *=redo, +=toggle mode, 0=deselect, F2=new hard game, q/Esc=quit"
        ]

renderModeToggle :: GameState -> Image
renderModeToggle state =
  horizCat
    [ string defAttr "Mode: ",
      modeChip (state.insertionMode == InsertValues) (defAttr `withForeColor` black `withBackColor` green) " INSERT NUMBERS ",
      string defAttr " ",
      modeChip (state.insertionMode == RemoveCandidates) (defAttr `withForeColor` white `withBackColor` red) " REMOVE CANDIDATES ",
      string defAttr "  (+ to toggle)"
    ]

modeChip :: Bool -> Attr -> String -> Image
modeChip isActive activeAttr label =
  if isActive
    then string activeAttr label
    else string (defAttr `withForeColor` brightBlack) label

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
          SelectValue ->
            case state.insertionMode of
              InsertValues -> "Step 3/3: select value key (toggle value)"
              RemoveCandidates -> "Step 3/3: select value key (toggle candidate removal)"

messageAttr :: RenderContext -> GameState -> Attr
messageAttr context state =
  if context.solved
    then defAttr `withForeColor` green
    else case state.statusMessage of
      Nothing -> defAttr
      Just (StatusInfo _) -> defAttr `withForeColor` green
      Just (StatusError _) -> defAttr `withForeColor` red

startNewHardGame :: IO GameState
startNewHardGame = do
  seed <- freshSeed
  let boardValues = generateHardPuzzle seed
      fixed = Map.keysSet boardValues
  pure
    initialState
      { values = boardValues,
        fixedCells = fixed,
        statusMessage = Just (StatusInfo "Started a new hard Sudoku.")
      }

freshSeed :: IO Int
freshSeed = do
  now <- getPOSIXTime
  pure (floor (now * 1000000))

generateHardPuzzle :: Int -> Map KeypadPos Int
generateHardPuzzle seed =
  carveBoardToUniquePuzzle 25 (shuffleFromSeed (advanceSeed seed 97) allBoardCells) (generateSolvedBoard seed)

generateSolvedBoard :: Int -> Map KeypadPos Int
generateSolvedBoard seed =
  Map.fromList
    [ (KeypadPos row col, digitOrder !! (baseDigit - 1))
      | (row, sourceRow) <- zip [0 ..] rowOrder,
        (col, sourceCol) <- zip [0 ..] colOrder,
        let baseDigit = ((sourceRow * 3 + sourceRow `div` 3 + sourceCol) `mod` 9) + 1
    ]
  where
    digitOrder = shuffleFromSeed seed [1 .. 9]
    rowOrder = permuteIndicesByGroups (advanceSeed seed 11)
    colOrder = permuteIndicesByGroups (advanceSeed seed 23)

permuteIndicesByGroups :: Int -> [Int]
permuteIndicesByGroups seed =
  concatMap expandedBand orderedBands
  where
    orderedBands = shuffleFromSeed seed [0 .. 2]
    expandedBand band =
      [band * 3 + offset | offset <- shuffleFromSeed (advanceSeed seed (band + 1)) [0 .. 2]]

carveBoardToUniquePuzzle :: Int -> [KeypadPos] -> Map KeypadPos Int -> Map KeypadPos Int
carveBoardToUniquePuzzle targetClues cells solvedBoard = go cells solvedBoard
  where
    go [] board = board
    go (cell : rest) board
      | Map.size board <= targetClues = board
      | otherwise =
          let boardWithoutCell = Map.delete cell board
           in if countSolutionsUpTo 2 boardWithoutCell == 1
                then go rest boardWithoutCell
                else go rest board

countSolutionsUpTo :: Int -> Map KeypadPos Int -> Int
countSolutionsUpTo limit board = search limit board
  where
    search remaining current
      | remaining <= 0 = 0
      | otherwise =
          case bestEmptyCellWithCandidates current of
            Nothing -> 1
            Just (_cell, []) -> 0
            Just (cell, candidates) -> searchCandidates remaining cell candidates current

    searchCandidates _remaining _cell [] _current = 0
    searchCandidates remaining cell (candidate : otherCandidates) current =
      let foundWithCandidate = search remaining (Map.insert cell candidate current)
       in if foundWithCandidate >= remaining
            then remaining
            else
              foundWithCandidate
                + searchCandidates
                  (remaining - foundWithCandidate)
                  cell
                  otherCandidates
                  current

bestEmptyCellWithCandidates :: Map KeypadPos Int -> Maybe (KeypadPos, [Int])
bestEmptyCellWithCandidates board =
  case sortOn (length . snd) cellsWithCandidates of
    [] -> Nothing
    best : _ -> Just best
  where
    cellsWithCandidates =
      [ (cell, allowedDigitsAtRaw board cell)
        | cell <- allBoardCells,
          Map.notMember cell board
      ]

allowedDigitsAtRaw :: Map KeypadPos Int -> KeypadPos -> [Int]
allowedDigitsAtRaw board cell =
  [ digit
    | digit <- [1 .. 9],
      notElem digit usedDigits
  ]
  where
    usedDigits =
      [ value
        | (otherCell, value) <- Map.toList board,
          otherCell.row == cell.row
            || otherCell.col == cell.col
            || (otherCell.row `div` 3 == cell.row `div` 3 && otherCell.col `div` 3 == cell.col `div` 3)
      ]

advanceSeed :: Int -> Int -> Int
advanceSeed seed salt = (seed * 1103515245 + salt * 12345 + 67890) `mod` 2147483647

shuffleFromSeed :: Int -> [a] -> [a]
shuffleFromSeed seed items = go (normalizeSeed seed) items []
  where
    go _ [] acc = reverse acc
    go currentSeed remaining acc =
      let nextSeed = advanceSeed currentSeed 1
          pickIndex = nextSeed `mod` length remaining
          (picked, rest) = removeAt pickIndex remaining
       in go nextSeed rest (picked : acc)

normalizeSeed :: Int -> Int
normalizeSeed seed =
  let normalized = seed `mod` 2147483647
   in if normalized <= 0
        then normalized + 2147483646
        else normalized

removeAt :: Int -> [a] -> (a, [a])
removeAt targetIndex items =
  case splitAt targetIndex items of
    (prefix, picked : suffix) -> (picked, prefix ++ suffix)
    _ -> error "removeAt: index out of bounds"

allBoardCells :: [KeypadPos]
allBoardCells =
  [ KeypadPos row col
    | row <- [0 .. 8],
      col <- [0 .. 8]
  ]

normalizeClipboardText :: String -> String
normalizeClipboardText = filter (/= '\r')

readTextFile :: FilePath -> IO (Either String String)
readTextFile path = do
  readResult <- try (readFile path) :: IO (Either IOException String)
  pure $
    case readResult of
      Left err -> Left (show err)
      Right fileContent -> Right (normalizeClipboardText fileContent)

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
  case selectedValueFrameCharAt state x y of
    Just frameChar -> frameChar
    Nothing ->
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
  | hasSelectedValueFrameAt state x y = highlightedBase `withForeColor` cyan
  | isConflictAt context.conflicts x y = highlightedBase `withForeColor` red
  | isFixedCellAt state x y = highlightedBase `withForeColor` yellow
  | hasPlacedValueAt state x y = highlightedBase `withForeColor` green
  | hasCandidateAt state x y = highlightedBase `withForeColor` brightBlack
  | otherwise = highlightedBase
  where
    unhighlightedBaseAttr
      | isMinorBorderChar baseChar = fillAttr (baseAttr `withForeColor` brightBlack)
      | otherwise = fillAttr baseAttr
    highlightedBase
      | context.solved && isBorderChar baseChar = unhighlightedBaseAttr `withForeColor` green
      | onSelectedCellBorder state x y = cellBorderAttr
      | onQuadrantBorder state x y = borderAttr
      | otherwise = unhighlightedBaseAttr
    fillAttr attr
      | filledCellAt state x y = attr `withBackColor` brightBlack
      | otherwise = attr

isBorderChar :: Char -> Bool
isBorderChar c = c /= '.' && c /= ' '

isMinorBorderChar :: Char -> Bool
isMinorBorderChar c = c == '│' || c == '─' || c == '┼'

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

selectedValueMatchCells :: GameState -> Set KeypadPos
selectedValueMatchCells state =
  case activeHighlightedDigit state of
    Nothing -> Set.empty
    Just highlighted ->
      Set.fromList
        [ cell
          | (cell, value) <- Map.toList state.values,
            value == highlighted
        ]

activeHighlightedDigit :: GameState -> Maybe Int
activeHighlightedDigit state =
  case state.highlightedDigit of
    Just digit -> Just digit
    Nothing -> selectedFilledValue state

isSelectedValueMatchAt :: GameState -> Int -> Int -> Bool
isSelectedValueMatchAt state x y =
  case (selectedFilledValue state, valueAtDisplayCoord state x y) of
    (Just selectedValue, Just value) -> selectedValue == value
    _ -> False

selectedValueFrameCharAt :: GameState -> Int -> Int -> Maybe Char
selectedValueFrameCharAt state x y = do
  cell <- cellAtDisplayCoord x y
  if Set.member cell (selectedValueMatchCells state)
    then frameCharAtCell cell x y
    else Nothing

hasSelectedValueFrameAt :: GameState -> Int -> Int -> Bool
hasSelectedValueFrameAt state x y =
  case selectedValueFrameCharAt state x y of
    Just _ -> True
    Nothing -> False

frameCharAtCell :: KeypadPos -> Int -> Int -> Maybe Char
frameCharAtCell cell x y =
  case (x - centerX, y - centerY) of
    (-2, -1) -> Just '┌'
    (-1, -1) -> Just '─'
    (0, -1) -> Just '─'
    (1, -1) -> Just '─'
    (2, -1) -> Just '┐'
    (-2, 0) -> Just '│'
    (2, 0) -> Just '│'
    (-2, 1) -> Just '└'
    (-1, 1) -> Just '─'
    (0, 1) -> Just '─'
    (1, 1) -> Just '─'
    (2, 1) -> Just '┘'
    _ -> Nothing
  where
    centerX = cell.col * 8 + 4
    centerY = cell.row * 4 + 2

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

filledCellAt :: GameState -> Int -> Int -> Bool
filledCellAt state x y =
  case cellAtDisplayCoord x y of
    Nothing -> False
    Just cell -> hasValueAt state cell

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
          not (digit `elem` usedDigits),
          not (Set.member digit (removedCandidatesAt state cell))
      ]
  where
    usedDigits = rowDigits state cell ++ colDigits state cell ++ boxDigits state cell

removedCandidatesAt :: GameState -> KeypadPos -> Set Int
removedCandidatesAt state cell = Map.findWithDefault Set.empty cell state.removedCandidates

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

onSelectedCellBorder :: GameState -> Int -> Int -> Bool
onSelectedCellBorder state x y =
  state.phase == SelectValue
    && case state.selectedCell of
      Nothing -> False
      Just cell ->
        let xLeft = cell.col * 8
            xRight = (cell.col + 1) * 8
            yTop = cell.row * 4
            yBottom = (cell.row + 1) * 4
            onTopOrBottom = y == yTop || y == yBottom
            onLeftOrRight = x == xLeft || x == xRight
         in ((x >= xLeft && x <= xRight) && onTopOrBottom)
              || ((y >= yTop && y <= yBottom) && onLeftOrRight)

onQuadrantBorder :: GameState -> Int -> Int -> Bool
onQuadrantBorder state x y =
  state.phase == SelectCell
    && case state.selectedQuadrant of
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

cellBorderAttr :: Attr
cellBorderAttr = baseAttr `withForeColor` yellow

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
topLine = makeSeparator '╔' '╗' '═' '╦' '═'

bottomLine :: String
bottomLine = makeSeparator '╚' '╝' '═' '╩' '═'

majorSeparatorLine :: String
majorSeparatorLine = makeSeparator '╠' '╣' '═' '╬' '═'

minorSeparatorLine :: String
minorSeparatorLine = makeSeparator '║' '║' '┼' '║' '─'

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
