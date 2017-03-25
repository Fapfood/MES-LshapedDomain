import Test.HUnit
import Test.QuickCheck
import LShapeSolver
import OldForTestSolver


main :: IO ()
main = runTestTT uTests >> qTests


qTests = do
  quickCheck ((\x y -> roundFraction 5 (gInCartesian (x,y)) == roundFraction 5 (grzanie (x,y)))::Double -> Double -> Bool)
  quickCheck ((\x y -> roundFraction 5 (gInCartesian (x,y)) == roundFraction 5 (gInPolar $ toPolar (x,y)))::Double -> Double -> Bool)
  let n = 2
  quickCheck ((\ind -> roundFraction 5 (fill_f_aux n ind) == roundFraction 5 (uzupelnij_pomoc_f n ind))::Integer -> Bool)


uTests :: Test
uTests = TestList [TestLabel "testGenerateTuples n=1" uTest1,
                   TestLabel "testGenerateTuples n=2" uTest2,
                   TestLabel "testGenerateTuples n=3" uTest3,
                   TestLabel "testGenerateTuples n=4" uTest4,
                   TestLabel "testGenerateTuples n=5" uTest5]


uTest1 = TestCase (assertEqual "porażka przy n=1" (generateTuples 1) (generuj 1))

uTest2 = TestCase (assertEqual "porażka przy n=2" (generateTuples 2) (generuj 2))

uTest3 = TestCase (assertEqual "porażka przy n=3" (generateTuples 3) (generuj 3))

uTest4 = TestCase (assertEqual "porażka przy n=4" (generateTuples 4) (generuj 4))

uTest5 = TestCase (assertEqual "porażka przy n=5" (generateTuples 5) (generuj 5))
