-----------------------------------------------------------------------------
-- |
-- Module      : LShapeSolver
-- Copyright   : Fapfood
-----------------------------------------------------------------------------

module LShapeSolver
    (generateTuples,
    calculate,
    fill_A,
    fill_f,
    roundFraction,
    fill_f_aux,
    toPolar,
    gInPolar
    ) where


import Matrix.Matrix
import Matrix.LU
import Data.Array





-- | Generuje krotki (x,y,wartość), przyjmuje liczbę podziałów
generateTuples n = k
  where
  indPerSide = n*2+1
  k = [(x, y, roundFraction 5 (wynik!ind)) | ind <- [1..indPerSide^2], let (x,y) = (coordOfInd n ind)]
  wynik = calculate n


-- | Zaokrągla val do decPlace miejsc po przecinku
roundFraction decPlace val = (fromIntegral $ round $ val * (10^decPlace)) / (10.0^^decPlace)

unpackX (x,_) = x

unpackY (_,y) = y


-- | Wylicza macierz rozwiązań, przyjmuje liczbę podziałów
calculate n = solve (fill_A n) (fill_f n)


-- | Uzupełnia macierz lewej strony A, przyjmuje liczbę podziałów
fill_A n = dirichlet n (fill_A_aux n 1 a)
  where
  a = zeros_A n


zeros_A n = array ((1,1),(m,m)) [((i, j), 0) | i <- [1..m], j <- [1..m]]
  where
  m = (n*2+1)^2


dirichlet n a = a//k
  where
    m' = n*2+1
    m = m'^2
    listX = [x | x <- [m'*n+1..m], x `mod` m' <= n+1 && x `mod` m' /= 0]
    k = [((x,y),0)| y<-[1..m], x <- listX, y /= x] ++ [((x,x),1)| x<-listX]


fill_A_aux :: (Ix a, Integral a, Fractional b) => a -> a -> Array (a,a) b -> Array (a,a) b
fill_A_aux n x a
  | x >= m'*(m'-1) = a
  | x `mod` m' == 0 = fill_A_aux n (x+1) a
  | otherwise = fill_A_aux n (x+1) (a//k)
    where
      m' = n*2+1
      fi1 = x+m'
      fi2 = x+m'+1
      fi3 = x+1
      fi4 = x
      k = [((fi4,fi4), a!(fi4,fi4)+(takeCorresp (4,4))), ((fi4,fi3), a!(fi4,fi3)+(takeCorresp (4,3))), ((fi4,fi1), a!(fi4,fi1)+(takeCorresp (4,1))), ((fi4,fi2), a!(fi4,fi2)+(takeCorresp (4,2))),
           ((fi3,fi4), a!(fi3,fi4)+(takeCorresp (3,4))), ((fi3,fi3), a!(fi3,fi3)+(takeCorresp (3,3))), ((fi3,fi1), a!(fi3,fi1)+(takeCorresp (3,1))), ((fi3,fi2), a!(fi3,fi2)+(takeCorresp (3,2))),
           ((fi1,fi4), a!(fi1,fi4)+(takeCorresp (1,4))), ((fi1,fi3), a!(fi1,fi3)+(takeCorresp (1,3))), ((fi1,fi1), a!(fi1,fi1)+(takeCorresp (1,1))), ((fi1,fi2), a!(fi1,fi2)+(takeCorresp (1,2))),
           ((fi2,fi4), a!(fi2,fi4)+(takeCorresp (2,4))), ((fi2,fi3), a!(fi2,fi3)+(takeCorresp (2,3))), ((fi2,fi1), a!(fi2,fi1)+(takeCorresp (2,1))), ((fi2,fi2), a!(fi2,fi2)+(takeCorresp (2,2)))]



takeCorresp (i,j) = fi!(i,j)
  where
  b1 = 2/3
  b2 = -1/6
  b3 = -1/3
  fi = array ((1,1),(4,4)) [((1,1), b1), ((1,2), b2), ((1,3), b3), ((1,4), b2),
                            ((2,1), b2), ((2,2), b1), ((2,3), b2), ((2,4), b3),
                            ((3,1), b3), ((3,2), b2), ((3,3), b1), ((3,4), b2),
                            ((4,1), b2), ((4,2), b3), ((4,3), b2), ((4,4), b1)]




-- | Uzupełnia macierz prawej strony f, przyjmuje liczbę podziałów
fill_f n = array (1,m) [(i, (fill_f_aux n i)) | i <- [1..m]]
  where
  m = (n*2+1)^2


-- | Oblicza wartość spod indeksu prawej strony f, przyjmuje liczbę podziałów i numer indeksu
fill_f_aux n ind
  | ind == 1                               = k * (edgeAvgVal n ind (ind + 1) + (edgeAvgVal n ind (ind + indPerSide)))
  | ind == iPS                             = k * (edgeAvgVal n ind (ind - 1) + (edgeAvgVal n ind (ind + indPerSide)))
  | ind == iPS^2                           = k * (edgeAvgVal n ind (ind - 1) + (edgeAvgVal n ind (ind - indPerSide)))
  | ind > 1 && ind < iPS                   = k * (edgeAvgVal n ind (ind - 1) + (edgeAvgVal n ind (ind + 1)))
  | ind > iPS*(iPS-1)+n+1 && ind < iPS^2   = k * (edgeAvgVal n ind (ind - 1) + (edgeAvgVal n ind (ind + 1)))
  | indModIndPerSide == 1 && ind < iPS*n   = k * (edgeAvgVal n ind (ind - indPerSide) + (edgeAvgVal n ind (ind + indPerSide)))
  | indModIndPerSide == 0                  = k * (edgeAvgVal n ind (ind - indPerSide) + (edgeAvgVal n ind (ind + indPerSide)))
  | otherwise                              = 0
  where
    fractN = fromIntegral n
    indModIndPerSide = fromIntegral (ind `mod` indPerSide)
    indDivIndPerSide = fromIntegral (ind `div` indPerSide)
    indPerSide = n*2+1
    iPS = indPerSide
    k = 1/(2*fractN)


edgeAvgVal n ind1 ind2 = gInPolar $ toPolar $ avgCoord n ind1 ind2


avgCoord :: (Integral a, Floating b, Eq b) => a -> a -> a -> (b,b)
avgCoord n ind1 ind2 = (x,y)
  where
    (x1,y1) = coordOfInd n ind1
    (x2,y2) = coordOfInd n ind2
    x = (x1+x2)/2
    y = (y1+y2)/2


coordOfInd n ind = (x,y)
  where
  x | indModIndPerSide == 0 = 1
    | otherwise =  (indModIndPerSide - fractN - 1)/fractN
  y | indModIndPerSide == 0 = (-indDivIndPerSide + fractN + 1)/fractN
    | otherwise = (-indDivIndPerSide + fractN)/fractN
  fractN = fromIntegral n
  indModIndPerSide = fromIntegral (ind `mod` indPerSide)
  indDivIndPerSide = fromIntegral (ind `div` indPerSide)
  indPerSide = n*2+1


-- | Zamienia ze współrzędnych karteziańskich na biegunowe
toPolar :: (Floating a, Eq a) => (a,a) -> (a,a)
toPolar (x,y) = (r,psi)
  where
    r = (x^2+y^2)**(1/2)
    psi = if (r==0) then 0
      else acos(x/r)*signum(y)

-- | Zadana funkcja g na brzegu we współrzędnych biegunowych
-- TODO **(2/3)
gInPolar (r,psi) = ((r*sin(psi+pi/2))^2)**(1/3)
