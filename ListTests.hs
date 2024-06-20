module ListTests where

import Test.HUnit
import Seq
import ListSeq

fromTreeView :: Seq s => TreeView a (s a) -> (s a, s a)
fromTreeView EMPTY = (emptyS, emptyS)
fromTreeView (ELT x) = (singletonS x, emptyS)
fromTreeView (NODE l r) = (l,r)

s0, s1, s2, s3, s4, s5 :: [Int]
s0 = fromList []
s1 = fromList [4]
s2 = fromList [5,1]
s3 = fromList [6,3,4]
s4 = fromList [1..10000]
s5 = fromList [1..4]

testLengthEmptySeq :: Test
testLengthEmptySeq = 
  TestCase $ assertEqual "Error on empty sequence length"
                         0 (lengthS s0)

testLengthNonEmptySeq :: Test
testLengthNonEmptySeq = 
  TestCase $ assertEqual "Error on non-empty sequence length"
                         2 (lengthS s2)

testMapEmptySeq :: Test
testMapEmptySeq = 
  TestCase $ assertEqual "Error on empty sequence map"
                         s0 (mapS (+1) s0)

testMapNonEmptySeq :: Test
testMapNonEmptySeq = 
  TestCase $ assertEqual "Error on non-empty sequence map"
                         (fromList [7,4,5]) (mapS (+1) s3)

testReduceSumSeq0 :: Test
testReduceSumSeq0 = 
  TestCase $ assertEqual "Error reducing empty sequence"
                         0 (reduceS (+) 0 s0)

testReduceSumSeq3 :: Test
testReduceSumSeq3 = 
  TestCase $ assertEqual "Error reducing sequence of length 3"
                         13 (reduceS (+) 0 s3)

testScanSumSeq0 :: Test
testScanSumSeq0 = 
  TestCase $ assertEqual "Error on empty sequence scan"
                         (emptyS, 0) (scanS (+) 0 s0)

testScanSumSeq3 :: Test
testScanSumSeq3 = 
  TestCase $ assertEqual "Error on scan for sequence of length 3"
                         (fromList[0,6,9], 13) (scanS (+) 0 s3)

testReduceSumBigSeq :: Test
testReduceSumBigSeq =
  TestCase $ assertEqual "Error on scan for big sequence"
                         50005000 (reduceS (+) 0 s4)

testReduceReductionOrder :: Test
testReduceReductionOrder =
  TestCase $ assertEqual "Error on reduce reduction order for list from 1 to 4"
                         777 (reduceS (-) 777 s5)

testScanBigList :: Test
testScanBigList =
  TestCase $ assertEqual "Error on scan for big sequence"
                         (takeS s4 (lengthS s4 - 1)) (fst $ scanS max (nthS s4 0) (dropS s4 1))

testScanFibonacci :: Test
testScanFibonacci :: Test = let
                              matMult:: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
                              matMult (a0,a1,a2,a3) (b0,b1,b2,b3) = (a0*b0+a1*b2, a0*b1+a1*b3, a2*b0+a3*b2, a2*b1+a3*b3)
                              matList:: [(Int,Int,Int,Int)]
                              matList = tabulateS (const (1,1,1,0)) 17
                              fibo = mapS (\(a,_,_,_) -> a) $ fst $ scanS matMult (1,0,0,1) matList
                            in
                              TestCase $ assertEqual "Error on scan for fibonacci sequence" (fromList [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597]) fibo

testsLists :: [Test]
testsLists = 
  [
    testMapEmptySeq,
    testMapNonEmptySeq,
    testLengthEmptySeq,
    testLengthNonEmptySeq,
    testReduceSumSeq0,
    testReduceSumSeq3,
    testScanSumSeq0,
    testScanSumSeq3,
    testReduceSumBigSeq,
    testReduceReductionOrder,
    testScanBigList,
    testScanFibonacci
  ]


main :: IO Counts
main = runTestTT $ TestList testsLists
