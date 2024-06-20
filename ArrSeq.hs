module ArrSeq where
import Par
import Seq
import Arr (Arr, (!))
import qualified Arr as A

instance Seq Arr where
    emptyS :: Arr a
    emptyS = A.empty
    
    singletonS :: a -> Arr a
    singletonS = A.fromList . (:[])

    lengthS :: Arr a -> Int
    lengthS = A.length

    nthS :: Arr a -> Int -> a
    nthS = (!)

    tabulateS :: (Int -> a) -> Int -> Arr a
    tabulateS = A.tabulate

    mapS :: (a -> b) -> Arr a -> Arr b
    mapS f s = A.tabulate (f . (s !)) $ A.length s

    filterS :: (a -> Bool) -> Arr a -> Arr a
    filterS f s = let
                    f' x
                        | f x = singletonS x
                        | otherwise = emptyS
                  in
                    A.flatten $ mapS f' s

    appendS :: Arr a -> Arr a -> Arr a
    appendS x y = A.flatten $ A.fromList [x,y]

    takeS :: Arr a -> Int -> Arr a
    takeS = flip (A.subArray 0)

    dropS :: Arr a -> Int -> Arr a
    dropS s i = A.subArray i (lengthS s - i) s

    showtS :: Arr a -> TreeView a (Arr a)
    showtS s = case A.length s of
                0 -> EMPTY
                1 -> ELT $ s ! 0
                n -> NODE (takeS s n) (dropS s n)

    showlS :: Arr a -> ListView a (Arr a)
    showlS s = case A.length s of
                0 -> NIL
                _ -> CONS (s ! 0) (dropS s 1)

    joinS :: Arr (Arr a) -> Arr a
    joinS = A.flatten 

    reduceS :: (a -> a -> a) -> a -> Arr a -> a
    reduceS f e s = case A.length s of
                      0 -> e
                      _ -> f e $ reduce' f s
      where
        reduce' f s = case A.length s of
                        1 -> s ! 0
                        _ -> reduce' f $ compact f s
        compact f s = let
                        n = A.length s
                        apply i
                              | 2*i+1 == n = s ! (2*i)
                              | otherwise = f (s ! (2*i)) (s ! (2*i+1))
                      in
                        -- Obtengo el techo de la division
                        A.tabulate apply (div (n + 1) 2)

    scanS :: (a -> a -> a) -> a -> Arr a -> (Arr a, a)
    scanS f b s
                | lengthS s == 0 = (emptyS, b)
                | lengthS s == 1 = (singletonS b, f b $ s ! 0)
                | otherwise = let
                                (s',t) = scanS f b $ compact f s
                              in
                                (combine f s' s,t)
        where
            compact f s = let
                            n = A.length s
                            apply i
                                    | 2*i+1 == n = s ! (2*i)
                                    | otherwise = f (s ! (2*i)) (s ! (2*i+1))
                          in
                            -- Obtengo el techo de la division
                            A.tabulate apply (div (n + 1) 2)

            combine f s' s =  let
                                apply i
                                    | even i = s' ! div i 2
                                    | otherwise = f (s' ! div i 2) (s ! (i-1))
                              in
                                A.tabulate apply $ A.length s

    fromList :: [a] -> Arr a
    fromList = A.fromList