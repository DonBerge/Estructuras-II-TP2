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
    mapS f s = tabulateS (f . (s !)) $ lengthS s

    appendS :: Arr a -> Arr a -> Arr a
    appendS x y = A.flatten $ A.fromList [x,y] 

    filterS :: (a -> Bool) -> Arr a -> Arr a
    filterS f s
                | A.length s == 0 = A.empty
                | A.length s == 1 = if f (s ! 0) 
                                        then s
                                        else A.empty
                | otherwise = 
                    let
                        n = A.length s
                        m = div n 2
                        (l,r) = A.subArray 0 m s ||| A.subArray m n s
                        (fl,fr) = filterS f l ||| filterS f r
                    in
                        appendS fl fr

    takeS :: Arr a -> Int -> Arr a
    takeS = flip (A.subArray 0)

    dropS :: Arr a -> Int -> Arr a
    dropS s i = A.subArray i (A.length s - i) s

    showtS :: Arr a -> TreeView a (Arr a)
    showtS s
            | A.length s  == 0 = EMPTY
            | A.length s == 1 = ELT $ s ! 0
            | otherwise = let
                            m = div (A.length s) 2
                          in
                            NODE (takeS s m) (dropS s m)
    
    showlS :: Arr a -> ListView a (Arr a)
    showlS s
            | A.length s == 0 = NIL
            | otherwise = CONS (s ! 0) (dropS s 1)

    joinS :: Arr (Arr a) -> Arr a
    joinS = A.flatten

    reduceS :: (a -> a -> a) -> a -> Arr a -> a
    reduceS f e s = case showtS s of
                        EMPTY -> e
                        ELT x -> x
                        NODE l r -> let
                                    (fl,fr) = reduceS f e l ||| reduceS f e r
                                   in
                                    f fl fr

    scanS :: (a -> a -> a) -> a -> Arr a -> (Arr a, a)
    scanS f b s
                | lengthS s == 0 = (emptyS, b)
                | lengthS s == 1 = (singletonS b, f b $ s ! 0)
                | otherwise = let
                                (s',t) = (1,1)
                              in
                                (compact f s,reduceS f b s)
        where
            compact f s
                    | lengthS s <= 1 = s
                    | otherwise = let
                                    apply i
                                            | 2*i+1 == lengthS s = s ! (2*i)
                                            | otherwise = f (s ! (2*i)) (s ! (2*i+1))
                                  in
                                    -- Obtengo el techo de la division
                                    tabulateS apply (div (lengthS s + 1) 2)
            
    
    fromList :: [a] -> Arr a
    fromList = A.fromList