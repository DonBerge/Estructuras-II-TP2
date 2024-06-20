module ListSeq where

import Par
import Seq

instance Seq [] where
    emptyS :: [a]
    emptyS = []

    singletonS :: a -> [a]
    singletonS = (: [])

    lengthS :: [a] -> Int
    lengthS = length

    nthS :: [a] -> Int -> a
    nthS = (!!)

    tabulateS :: (Int -> a) -> Int -> [a]
    tabulateS f n = aux f 0 n
      where
        aux f m n
            | m >= n = []
            | m == n - 1 = [f m]
            | m < n =
                let (x, xs) = f m ||| aux f (m + 1) n
                 in x : xs

    mapS :: (a -> b) -> [a] -> [b]
    mapS f [] = []
    mapS f (x : xs) =
        let (x', xs') = f x ||| mapS f xs
         in x' : xs'

    filterS :: (a -> Bool) -> [a] -> [a]
    filterS p [] = []
    filterS p (x : xs) =
        let (bool, xs') = p x ||| filter p xs
         in if bool then x : xs' else xs'

    appendS :: [a] -> [a] -> [a]
    appendS = (++)

    takeS :: [a] -> Int -> [a]
    takeS = flip take

    dropS :: [a] -> Int -> [a]
    dropS = flip drop

    showtS :: [a] -> TreeView a [a]
    showtS [] = EMPTY
    showtS [x] = ELT x
    showtS s =
        let
            m = length s `div` 2
         in
            uncurry NODE $ take m s ||| drop m s

    showlS :: [a] -> ListView a [a]
    showlS [] = NIL
    showlS (x : xs) = CONS x xs

    joinS :: [[a]] -> [a]
    joinS = concat

    reduceS :: (a -> a -> a) -> a -> [a] -> a
    reduceS f e [] = e
    reduceS f e s = f e $ head $ reduce' f s
      where
        reduce' :: (a -> a -> a) -> [a] -> [a]
        reduce' f [] = []
        reduce' f [x] = [x]
        reduce' f xs = reduce' f $ compact f xs

        compact :: (a -> a -> a) -> [a] -> [a]
        compact f [] = []
        compact f [x] = [x]
        compact f (x : y : xs) = uncurry (:) $ f x y ||| compact f xs

    scanS :: (a -> a -> a) -> a -> [a] -> ([a], a)
    scanS _ b [] = ([], b)
    scanS f b [x] = ([b], f b x)
    scanS f b s =
        let
            (s', t) = scanS f b $ compact s
            r = combine s' s
         in
            (r, t)
      where
        compact [] = []
        compact [x] = [x]
        compact (x : y : xs) = uncurry (:) $ f x y ||| compact xs

        combine _ [] = []
        combine (x : _) [_] = [x]
        combine (x : xs) (y : _ : ys) =
            let
                (x', xs') = f x y ||| combine xs ys
             in
                x : x' : xs'

    fromList :: [a] -> [a]
    fromList = id
