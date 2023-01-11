import Test.HUnit
import Utils
import Board

testAdd1 :: Test
testAdd1 = TestCase (assertEqual "for (add 1 2)," 3 (add 1 2))

testAdd2 :: Test
testAdd2 = TestCase (assertEqual "for (add -1 3)," 2 (add (-1) 3))

testCreateBoard :: Test
testCreateBoard = TestCase (assertEqual "for (createBoard 3 3 3)," (listArray ((0,0),(2,2)) [Cell False False False,Cell False False False,Cell False False False,Cell False False False,Cell False False False,Cell False False False,Cell False False False,Cell False False False,Cell False False False]) (createBoard 3 3 3))

tests :: Test
tests = TestList [TestLabel "testAdd1" testAdd1, TestLabel "testAdd2" testAdd2]

main :: IO ()
main = do 
    _ <- runTestTT tests
    _ <- runTestTT testCreateBoard
    return ()