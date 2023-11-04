module Main where

-- 1 is Alive
-- 2 is Dead
-- 3 is Zombie

data Coord = Coord Int Int deriving (Eq, Show)
data Inspection = Inspection Int Int Int deriving (Eq, Show)
data Answer = Answer Grid Int deriving (Eq, Show)

type Cell = Int
type Row = [Int]
type Grid = [[Int]]
type NumCols = Int
type NumRows = Int

-- Função para pegar a matriz final do dado Answer
getGrid :: Answer -> Grid
getGrid (Answer grid num) = grid

-- Função para pergar o número de interações do dado Answer
getN :: Answer -> Int
getN (Answer grid num) = num

-- Função para pergar a coordernado x do dado Coord
getCoordX :: Coord -> Int
getCoordX (Coord x y) = x

-- Função para pegar a coordernada y do dado Coord
getCoordY :: Coord -> Int
getCoordY (Coord x y) = y

-- Adiciona linha na matrix
addRow :: Grid -> Row -> IO Grid
addRow grid row = return (grid ++ [row])

-- Ler uma linha de numeros do terminal
getRow :: NumCols -> IO Row
getRow numCols = do
  rowInput <- getLine
  let row = map read (words rowInput) :: Row
  if length row == numCols
    && all (< 4) row && all (> 0) row
      then return row
      else if length row > numCols &&
        all (< 4) row && all (> 0) row
      then do
        let newRow = take numCols row :: Row
        return newRow
      else if length row < numCols ||
        any (> 3) row || any (< 1) row
          then do
            putStrLn "Invalid Input. Try again."
            getRow numCols
          else return row

-- Função para construir a matriz
createGrid :: NumRows -> NumCols -> Grid -> IO Grid
createGrid numRows numCols grid = do
    if numRows == 0
    then return grid
    else do
        row <- getRow numCols
        newGrid <- addRow grid row
        createGrid (numRows - 1) numCols newGrid

-- Função para calcular os index dos vizinhos de uma célula
neighbors :: Coord -> [Coord]
neighbors (Coord x y) =
  [ Coord x (y + 1)
  , Coord x (y - 1)
  , Coord (x + 1) y
  , Coord (x - 1) y
  , Coord (x - 1) (y - 1)
  , Coord (x + 1) (y - 1)
  , Coord (x - 1) (y + 1)
  , Coord (x + 1) (y + 1)
  ]

-- Função para testar de a célula está viva
isAlive :: Cell -> Bool
isAlive x
  | x == 1 = True
  | x == 2 = False
  | x == 3 = False
  | otherwise = False

-- Função para verificar se a celula é um zumbi
isZombie :: Cell -> Bool
isZombie x
  | x == 1 = False
  | x == 2 = False
  | x == 3 = True
  | otherwise = False

-- Função para verificar se a célula está morta
isDead :: Cell -> Bool
isDead x
  | x == 1 = False
  | x == 2 = True
  | x == 3 = False
  | otherwise = False

-- Função para pegar o vizinho
getNeighbor :: Grid -> Coord -> NumRows -> NumCols -> IO Int
getNeighbor grid (Coord x y) numRows numCols = do
  if (x < numRows) && (y < numCols) && (x >= 0) && (y >= 0)
    then do
      return ((grid !! x) !! y)
    else return (-1)

-- Função para checar se o vizinho esta vivo, morto ou é zumbi
checkNeighbor :: Grid -> Coord -> NumRows -> NumCols -> Inspection -> IO Inspection
checkNeighbor grid (Coord x y) numRows numCols (Inspection alive dead zombie) = do
  cell <- getNeighbor grid (Coord x y) numRows numCols
  if isAlive cell
    then return (Inspection (alive + 1) dead zombie)
    else if isDead cell
      then return (Inspection alive (dead + 1) zombie)
      else if isZombie cell
        then return (Inspection alive dead (zombie + 1))
        else return (Inspection alive dead zombie)

-- Função para verificar a regra e descobrir qual será o estado da celula na resposta
checkInspection :: Cell -> Row -> Inspection -> IO Row
checkInspection cell row (Inspection alive dead zombie)
  | isAlive cell = if zombie >= 1
      then return (row ++ [3])
      else (if ((alive < 2) && (zombie <= 0)) || ((alive > 3) && (zombie <= 0))
        then return (row ++ [2])
        else return (row ++ [1]))
  | isDead cell = if alive == 3
        then return (row ++ [1])
        else return (row ++ [2])
  | isZombie cell = if alive <= 0
          then return (row ++ [2])
          else return (row ++ [3])
  | otherwise = return row

-- Função para dar inicio a verificações dos vizinhos
checkNeighbors :: Grid -> [Coord] -> NumRows -> NumCols -> Inspection -> Int -> IO Inspection
checkNeighbors grid coordsNeighbors numRows numCols inspection neighbor = do
  if neighbor < 8
    then do
      newInspection <- checkNeighbor grid (coordsNeighbors !! neighbor) numRows numCols inspection
      checkNeighbors grid coordsNeighbors numRows numCols newInspection (neighbor + 1)
    else return inspection

-- Função que verifica cada célula e inicia a verificação dos vizinhos
checkCell :: Grid -> Row -> Coord -> NumRows -> NumCols -> IO Row
checkCell grid row (Coord x y) numRows numCols = do
  if y < numCols
    then do
      inspection <- checkNeighbors grid (neighbors (Coord x y)) numRows numCols (Inspection 0 0 0) 0
      newRow <- checkInspection ((grid !! x) !! y) row inspection
      checkCell grid newRow (Coord x (y+1)) numRows numCols
    else return row

-- Função que verifica a matriz
checkGrid :: Grid -> Grid -> Coord -> NumRows -> NumCols -> IO Grid
checkGrid grid newGrid (Coord x y) numRows numCols = do
  if x < numRows
    then do
      row <- checkCell grid [] (Coord x y) numRows numCols
      checkGrid grid (newGrid ++ [row]) (Coord (x + 1) y) numRows numCols
    else return newGrid

-- Função que inicia o jogo
startGame :: Grid -> Int -> Int -> NumRows -> NumCols -> IO Answer
startGame grid numInteractions counter numRows numCols = do
  if counter <= numInteractions
    then do
      answerGrid <- checkGrid grid [] (Coord 0 0) numRows numCols
      if answerGrid == grid
        then return (Answer grid counter)
        else startGame answerGrid numInteractions (counter + 1) numRows numCols
    else do
      return (Answer grid numInteractions)

main :: IO ()
main = do

  putStrLn " "
  putStrLn " "

  putStrLn "Number of interactions: "
  numInteractions <- readLn

  putStrLn "\nNumber of rows in the matrix: "
  numRows <- readLn

  putStrLn "Number of columns in the matrix: "
  numCols <- readLn

  putStrLn " "

  putStrLn "Observation: Insert each line with numbers separated by spaces, \nas shown in the example."
  putStrLn "Example: 3x3 Matrix\n1 2 1\n2 3 1\n1 1 2\n"

  putStrLn "Start inserting the matrix: "
  grid <- createGrid numRows numCols []

  putStrLn " "

  putStrLn (show numRows ++ "x" ++ show numCols ++" initial matrix:")
  mapM_ print grid

  putStrLn " "

  answerGrid <- startGame grid numInteractions 0 numRows numCols

  putStrLn ("Response Matrix after " ++ show (getN answerGrid) ++ " interactions:")
  mapM_ print (getGrid answerGrid)

  putStrLn " "

  putStrLn " "