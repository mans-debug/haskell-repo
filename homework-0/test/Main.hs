{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Main (main) where

import Data.List (foldl', sort)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Range (lowerBound)
import Hedgehog.Internal.Show (Value (Char))
import qualified Hedgehog.Range as Range
import MyLib
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain tests

prop_rev :: Property
prop_rev = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
  rev (rev xs) === xs

-- prop_opaque :: Property
-- prop_opaque = property $ do
--   xs <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
--   let op = Opaque xs
--   op /== op

prop_div10000 :: Property
prop_div10000 = property $ do
  x <- forAll $ Gen.int (Range.linear 1 100000)
  x `mod` 10000 /== 0

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ testGroup
        "Unit Tests"
        [ testCase "3*5 == 15" $ 3 * 5 @?= 15,
          testCase "2*2 == 4" $ 4 @=? 2 * 2,
          testCase "rev []" $ rev [] @?= ([] :: [Int]),
          testCase "rev [1,2,3]" $
            rev [1, 2, 3] @?= [3, 2, 1]
        ],
      testProperty "reverse works" $ prop_rev,
      -- , testProperty "strange opaque value" prop_opaque
      testProperty "all numbers do not divide 10000" prop_div10000,
      treeTests,
      homework_1_test
    ]

homework_1_test :: TestTree
homework_1_test =
  testGroup
    "ZipLong tests"
    [ testCase "Simple zipLong eq length" $
        zipLong [1, 2, 3] "abc" @?= [(1, 'a'), (2, 'b'), (3, 'c')],
      testCase "zipLong different length" $
        zipLong [1, 2] "abcd" @?= [(1, 'a'), (2, 'b'), (1, 'c'), (2, 'd')],
      testCase "zipLong one empty list" $ zipLong [] "abc" @?= ([] :: [(Int, Char)]),
      testCase "zipLong two empty lists" $ zipLong [] [] @?= ([] :: [(Int, Char)])
    ]

treeTests :: TestTree
treeTests =
  testGroup
    "Tree Tests"
    [ traversalTests,
      insertTests,
      rotationTest,
      treeGenerationTests
    ]

traversalTests :: TestTree
traversalTests =
  testGroup
    "traversal"
    [ testCase "empty" $ traversal empty @?= ([] :: [Int]),
      testCase "single elt" $ traversal (Node Nothing 1 Nothing) @?= [1],
      testCase "three elts" $
        traversal (Node (Just $ leaf 1) 2 (Just $ leaf 3)) @?= [1, 2, 3]
    ]

rotationTest :: TestTree
rotationTest =
  testGroup
    "Rotation tests"
    [ 
      testCase "empty" $ rotateLeft empty @?= (empty :: Tree Int),
      testProperty "test left rotation" rotation_prop_left,
      testProperty "test right rotation" rotation_prop_right
    ]

rotation_prop_left :: Property
rotation_prop_left = 
  property $ do
    tree <- forAll $ arbitraryTree 5 100 10000
    isCorrect (rotateLeft tree) === True

rotation_prop_right :: Property
rotation_prop_right = 
  property $ do
    tree <- forAll $ arbitraryTree 5 100 10000
    isCorrect (rotateRight tree) === True

treeGenerationTests :: TestTree
treeGenerationTests =
  testGroup
    "Tree generation tests"
    [
      testProperty "Test generated tree is correct" genTree_prop
    ]

genTree_prop :: Property
genTree_prop = property $ do
  tree <- forAll $ arbitraryTree 5 100 10000
  isCorrect tree === True

isCorrect :: (Ord a) => Tree a -> Bool
isCorrect Empty = True
isCorrect (Node ml v mr) =
  maybe True isCorrect ml
    && maybe True isCorrect mr
    && all (< v) (maybe [] traversal ml)
    && all (>= v) (maybe [] traversal mr)

arbitraryTree :: Size -> Int -> Int -> Gen (Tree Int)
arbitraryTree 0 _ _ = pure empty
arbitraryTree size lower upper = do
  leftSize <- Size <$> Gen.int (Range.linear 0 $ unSize size - 1)
  let rightSize = size - leftSize - 1
  v <- Gen.int $ Range.linear lower upper
  l <-
    if leftSize == 0
      then pure Nothing
      else Just <$> arbitraryTree leftSize lower (v - 1)
  r <-
    if rightSize == 0
      then pure Nothing
      else Just <$> arbitraryTree rightSize (v + 1) upper
  pure $ Node l v r


prop_bst :: Property
prop_bst = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 30) $ Gen.int (Range.linear 0 1000)
  let t = foldl' (\tree elt -> insert elt tree) empty xs
  assert $ isCorrect t

prop_inserts :: Property
prop_inserts = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 30) $ Gen.int (Range.linear 0 1000)
  let t = foldl' (\tree elt -> insert elt tree) empty xs
  sort xs === traversal t

insertTests :: TestTree
insertTests =
  testGroup
    "insert"
    [ testProperty "BST is correct" prop_bst,
      testProperty "Insert really inserts" prop_inserts,
      testCase "1" $ isCorrect (leaf 1) @? "leaf 1",
      testCase "3" $
        isCorrect (Node (Just $ leaf 1) 2 (Just $ leaf 2)) @? "tree 3"
    ]
