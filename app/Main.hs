module Main where
import GHC.Generics (prec)

type Line = [Int]
type Matrix = [[Int]]

-- Adicionar linha na matrix
addLine :: Matrix -> Line -> IO Matrix
addLine m line = return (m ++ [line])

-- Ler linha de numeros do terminal
getLinha :: IO Line
getLinha = do
  line <- getLine
  let nums = map read (words line) :: [Int]
  return nums

-- Subtração
calSubtr :: Int -> IO Int
calSubtr n = return (n - 1)

-- Criar matriz
creatMatrix :: Int -> Matrix -> IO Matrix
creatMatrix numRows matriz = do
  if numRows <= 0
    then return matriz
    else do
      line <- getLinha
      matriz <- addLine matriz line
      numRows <- calSubtr numRows
      creatMatrix numRows matriz

main :: IO ()
main = do
  putStrLn "Enter the number of rows: "
  numRows <- readLn
  -- putStrLn "Enter the number of columns: "
--  numCols <- readLn
  m <- creatMatrix numRows []
  mapM_ print m




