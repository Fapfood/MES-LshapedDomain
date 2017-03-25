-----------------------------------------------------------------------------
-- |
-- Module      : LShapeSolver
-- Copyright   : Fapfood
-----------------------------------------------------------------------------
module OldForTestSolver(generuj,
                       uzupelnij_pomoc_f,
                       grzanie,
                       gInCartesian) where



import Matrix.Matrix
import Matrix.LU
import Data.Array
import LShapeSolver(roundFraction, fill_A)



-- | Generuje krotki (x,y,wartość), przyjmuje liczbę podziałów
generuj n = k
  where
  m' = n*2+1
  n' = fromIntegral n
  k = [(((fromIntegral x)-n'-1)/n',-((fromIntegral y)-n'-1)/n', roundFraction 5 (wynik!((y-1)*m'+x))) | y <- [1..m'], x <- [1..m']]
  wynik = oblicz n
  test_za_wynik = array (1,m'^2) [(x,x) | x <- [1..m'^2]]



oblicz n = solve (fill_A n) (uzupelnij_f n)



uzupelnij_f n = array (1,m) [(i, (uzupelnij_pomoc_f n i)) | i <- [1..m]]
  where
  m = (n*2+1)^2

--uzupelnij_pomoc_f :: (Integral a, Fractional b) => a -> a -> b
-- | Oblicza wartość spod indeksu prawej strony f, przyjmuje liczbę podziałów i numer indeksu
uzupelnij_pomoc_f n' ind'
  | ind' == 1 = k*grzanie(-(2*n-1)/(2*n),1) + k*grzanie(-1,(2*n-1)/(2*n))
  | ind' == m' = k*grzanie((2*n-1)/(2*n),1) + k*grzanie(1,(2*n-1)/(2*n))
--  | ind' == m'*(m'-1)+1 = k*grzanie(-(2*n-1)/(2*n),-1) + k*grzanie(-1,-(2*n-1)/(2*n))
  | ind' == m'^2 = k*grzanie((2*n-1)/(2*n),-1) + k*grzanie(1,-(2*n-1)/(2*n))
  | ind' > 1 && ind' < m' = k*grzanie(-(2*n-1)/(2*n) + (ind-2)/n,1) + k*grzanie(-(2*n-1)/(2*n) + (ind-1)/n,1)
  | ind' > m'*(m'-1)+n'+1 && ind' < m'^2 = k*grzanie(-(2*n-1)/(2*n) + (indmodm-2)/n,-1) + k*grzanie(-(2*n-1)/(2*n) + (indmodm-1)/n,-1)
  | ind' `mod` m' == 1 && ind' < m'*n' = k*grzanie(-1,(2*n-1)/(2*n) - (inddivm-1)/n) + k*grzanie(-1,(2*n-1)/(2*n) - (inddivm)/n)
  | ind' `mod` m' == 0 = k*grzanie(1,(2*n-1)/(2*n) - (inddivm-2)/n) + k*grzanie(1,(2*n-1)/(2*n) - (inddivm-1)/n)
  | otherwise = 0
  where
    n = fromIntegral n'
    ind = fromIntegral ind'
    m' = n'*2+1
    indmodm = fromIntegral (ind' `mod` m')
    inddivm = fromIntegral (ind' `div` m')
    k = 1/(2*n)


-- | Zadana funkcja g na brzegu we współrzędnych karteziańskich
grzanie (x,y) = res
  where
    res = if (x==0 && y == 0)
      then 0.0
      else (r^(2)*sin(psi+pi/2)^(2))**(1/3)
    r = (x^2+y^2)**(1/2)
    psi = acos(x/r)*signum(y)


-- | Zadana funkcja g na brzegu we współrzędnych karteziańskich
gInCartesian (x,y) = (x^2)**(1/3)

{-
grzanie (x,y) -- = x*y
  | y == -1 = -x
  | y == 1 = x
  | x == 1 = y
  | x == -1 = -y

-}
