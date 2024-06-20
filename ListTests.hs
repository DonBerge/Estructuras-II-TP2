module ListTests where

import Test.HUnit
import Seq
import ListSeq

s0, s1, s2, s3 :: [Int]
s0 = fromList []
s1 = fromList [4]
s2 = fromList [5,1]
s3 = fromList [6,3,4]

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

--- Test de tabulate ---

testTabulateEmpty :: Test
testTabulateEmpty =
  TestCase $ assertEqual "Error on tabulate for n = 0"
                        [] (tabulateS id 0)

testTabulateCube :: Test
testTabulateCube =
  TestCase $ assertEqual "Error on tabulate test"
                         (tabulateS (^3) 6) [0,1,8,27,64,125]
testTabulateZero :: Test
testTabulateZero =
  TestCase $ assertEqual "Error on tabulate test"
                         (tabulateS (0*) 100) [0 | i <- [1..100]]

--- Test de map ---

testMapInStrings :: Test
testMapInStrings =
  TestCase $ assertEqual "Error on map for a sequence of strings"
                        [5,3,1] (mapS length $ fromList ["abcde","xyz","T"])

testMapInvertibleFun :: Test
testMapInvertibleFun =
  TestCase $ assertEqual "Error on map for invertible functions"
                        [1..100] (mapS round $ mapS tan $ mapS atan $ fromList [1..100])

--- Test filter ---

testFilterEmpty :: Test
testFilterEmpty =
  TestCase $ assertEqual "Error on filter for empty list"
                        [] (filterS (==202) [])

testFilterEven :: Test
testFilterEven =
  TestCase $ assertEqual "Error on filter for even"
                        [2,4,6,8,10] (filterS even $ fromList [1..10])

testFilterEmptyResult :: Test
testFilterEmptyResult =
  TestCase $ assertEqual "Error on filter test 3, result should be empty"
                        [] (filterS (\i -> i == i+1) $ fromList [1..1000])

--- Test reduce ---
testReduceEmptySeq :: Test
testReduceEmptySeq =
  TestCase $ assertEqual "Error on reduce for empty sequence"
                        "vacio" (reduceS (++) "vacio" [])

testReduceReductionOrder :: Test
testReduceReductionOrder =
  TestCase $ assertEqual "Error on reduction with non asociative operation(substraction)"
                       59 (reduceS (-) 59 [1..32])

testReduceConcatenation :: Test
testReduceConcatenation =
  TestCase $ assertEqual "Error on reduce for concatenation of strings"
                      "Habia una vez, un circo que alegraba siempre el corazon"
                      (reduceS (++) "" ["Habia", " una", " vez",",", " un",
                      " circo", " que ", "alegraba", " siempre ", "el ", "corazon"])

--- Test scan ---
testScanEmptySeq :: Test
testScanEmptySeq =
  TestCase $ assertEqual "Error on scan for empty sequence"
                      ([], "vacio") (scanS (++) "vacio" [])

testScanSingleton :: Test
testScanSingleton =
  TestCase $ assertEqual "Error on scan for singleton sequence"
                      (["vacio"], "vaciouno") (scanS (++) "vacio" $ fromList ["uno"])

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

    testTabulateEmpty,
    testTabulateCube,
    testTabulateZero,

    testMapInStrings,
    testMapInvertibleFun,

    testFilterEmpty,
    testFilterEven,
    testFilterEmptyResult,

    testReduceEmptySeq,
    testReduceReductionOrder,
    testReduceConcatenation,

    testScanEmptySeq,
    testScanSingleton,
    testScanFibonacci
  ]


main :: IO Counts
main = runTestTT $ TestList testsLists
