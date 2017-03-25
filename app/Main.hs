module Main (main) where


import System.Environment
import LShapeSolver




-- | Główna funkcja, przyjmuje n i zwraca wynik przygotowany dla gnuplota
main :: IO Integer
main = do
  n <- getIntArg
  putStr "set dgrid3d "
  putStr . show $ (n*2+1)
  putStr ", "
  print (n*2+1)
  putStrLn "set grid"
  putStrLn "set hidden3d"
  putStrLn "$grid << EOD"
  printArray (generateTuples n)
  putStrLn "EOD"
  putStrLn "splot '$grid' u 1:2:3 with lines"
  return 0


getIntArg :: IO Int
getIntArg = fmap (read . head) getArgs


printArray [] = return ()
printArray ((x,y,z):xs) = do
  putStr . show $ x
  putStr " "
  putStr . show $ y
  putStr " "
  putStr . show $ z
  putStrLn " "
  printArray xs
  return ()
