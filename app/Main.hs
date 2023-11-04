module Main where
import GHC.Generics (prec)

-- 1 is alive, 2 is Dead and 3 is Zombie
data XY = X | Y deriving (Eq, Show)
data Coord = Coord Int Int deriving (Eq, Show)
data V = V Int Int Int deriving (Eq, Show)
data RespostaFinal = R Grid Int

type Line = [Int]
type Grid = [[Int]]

getGrid :: RespostaFinal -> Grid
getGrid (R m n) = m

getN :: RespostaFinal -> Int
getN (R m n) = n

-- Adicionar linha na matrix
addLine :: Grid -> Line -> IO Grid
addLine m linha = return (m ++ [linha])

-- Ler linha de numeros do terminal
getLinha :: Int -> Int -> IO Line
getLinha numCols n = do
    --putStrLn $ "Linha " ++ show n ++ ": "
    line <- getLine
    let nums = map read (words line) :: Line
    if length nums == numCols
        then return nums
        else if length nums > numCols
        then do
            let newNums = take numCols nums :: Line
            return newNums
        else if length nums < 3
            then do
                putStrLn "Entrada Invalida, digite novamentea."
                getLinha numCols n
            else return nums

-- Construir Matriz
creatMatrix :: Int -> Int -> Grid -> Int -> IO Grid
creatMatrix numRows numCols matriz n = do
    if numRows <= 0
    then return matriz
    else do
        line <- getLinha numCols n
        matriz <- addLine matriz line
        creatMatrix (numRows - 1) numCols matriz (n + 1)

vizinhos :: Coord -> [Coord]
vizinhos (Coord x y) =
  [ Coord x (y + 1)
  , Coord x (y - 1)
  , Coord (x + 1) y
  , Coord (x - 1) y
  , Coord (x - 1) (y - 1)
  , Coord (x + 1) (y - 1)
  , Coord (x - 1) (y + 1)
  , Coord (x + 1) (y + 1)
  ]

isAlive :: Int -> Bool
isAlive x
  | x == 1 = True
  | x == 2 = False
  | x == 3 = False
  | otherwise = False

isZombie :: Int -> Bool
isZombie x
  | x == 1 = False
  | x == 2 = False
  | x == 3 = True
  | otherwise = False

isDead :: Int -> Bool
isDead x
  | x == 1 = False
  | x == 2 = True
  | x == 3 = False
  | otherwise = False

getVizinho :: Grid -> Coord -> Int -> Int -> IO Int
getVizinho m (Coord x y) numRows numCols = do
  if (x < numRows) && (y < numCols) && (x >= 0) && (y >= 0)
    then do
      return ((m !! x) !! y)
    else return (-1)

verificaVizinho :: Grid -> Coord -> Int -> Int -> V -> IO V
verificaVizinho m (Coord x y) numRows numCols (V a d z) = do
  r <- getVizinho m (Coord x y) numRows numCols
  if isAlive r
    then return (V (a+1) d z)
    else if isDead r
      then return (V a (d + 1) z)
      else if isZombie r
        then return (V a d (z + 1))
        else return (V a d z)

verificaResultado :: Int -> Line -> V -> IO Line
verificaResultado c rowResp (V a d z)
  | isAlive c = if z >= 1
      then return (rowResp ++ [3])
      else (if ((a < 2) && (z <= 0)) || ((a > 3) && (z <= 0))
        then return (rowResp ++ [2])
        else return (rowResp ++ [1]))
  | isDead c = if a == 3
        then return (rowResp ++ [1])
        else return (rowResp ++ [2])
  | isZombie c = if a <= 0
          then return (rowResp ++ [2])
          else return (rowResp ++ [3])
  | otherwise = return rowResp

verificaVizinhos :: Grid -> [Coord] -> Int -> Int -> V -> Int -> IO V
verificaVizinhos m coords numRows numCols v numV = do
  if numV < 8
    then do
      p <- verificaVizinho m (coords !! numV) numRows numCols v
      verificaVizinhos m coords numRows numCols p (numV + 1)
    else return v

verificaColuna :: Grid -> Line -> Coord -> Int -> Int -> IO Line
verificaColuna m rowResp (Coord x y) numRows numCols = do
  if y < numCols
    then do
      v <- verificaVizinhos m (vizinhos (Coord x y)) numRows numCols (V 0 0 0) 0
      resp <- verificaResultado ((m !! x) !! y) rowResp v
      verificaColuna m resp (Coord x (y+1)) numRows numCols
    else return rowResp

verificaMatriz :: Grid -> Grid -> Coord -> Int -> Int -> IO Grid
verificaMatriz m mSaida (Coord x y) numRows numCols = do
  if x < numRows
    then do
      l <- verificaColuna m [] (Coord x y) numRows numCols
      verificaMatriz m (mSaida ++ [l]) (Coord (x + 1) y) numRows numCols
    else return mSaida

startGame :: Grid -> Grid -> Int -> Int -> Int -> IO RespostaFinal
startGame m mResp n numRows numCols = do
  if (n > 0) && (m /= mResp)
    then do
      resp <- verificaMatriz m [] (Coord 0 0) numRows numCols
      startGame resp m (n-1) numRows numCols
    else do
      return (R m n)

m :: Grid
m =
  [
  [1, 1, 2, 1, 3, 2],
  [2, 1, 2, 1, 1, 1],
  [1, 1, 1, 3, 1, 2],
  [2, 1, 1, 2, 1, 1],
  [1, 1, 1, 1, 1, 2],
  [1, 2, 2, 1, 3, 1]
  ]

main :: IO ()
main = do

  putStrLn " "

  --putStrLn "Numero de linhas: "
  --numRows <- readLn

  --putStrLn "Numero de colunas: "
  --numCols <- readLn

  --putStrLn "Numero de interacoes: "
  --numInt <- readLn

  putStrLn " "

  --putStrLn "OBS: Insera cada linha de forma verifical\ne os numeros separados por espaço."
  --putStrLn "Ex:\n1 2 1\n2 3 1\n1 1 2\n"

  --putStrLn "Insira os dados da matriz: "
  --m <- creatMatrix numRows numCols [] 1

  putStrLn " "

  putStrLn "Matriz de Entrada:"
  mapM_ print m

  putStrLn " "

  mResposta <- startGame m [] 1 6 6

  putStrLn "Matriz de Saida - 1 Interacao:"
  print "Interacaoes: " ++ show (1 - (getN mResposta))
  mapM_ print (getGrid mResposta)

  putStrLn " "

  putStrLn " "

  mResposta2 <- startGame m [] 20 6 6

  putStrLn "Matriz de Saida -2 Interacao:"
  mapM_ print (getGrid mResposta2)

  putStrLn " "