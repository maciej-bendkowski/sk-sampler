{-|
 - Module       : RemySampler
 - Description  : Binary tree sampler based on 
 -                the ideas of [1] and [2].
 - Copyright    : (c) Maciej Bendkowski, 2016
 - Maintainer   : maciej.bendkowski@tcs.uj.edu.pl
 - Stability    : experimental
 -
 - References:
 -      [1] Remy - Un procede iteratif de denombrement d'arbres
 -      binaires et son application a leur generation aleatoire.
 -
 -      [2] Knuth - The Art of Computer Programming Vol 4.
 -      Generating All Trees: History of Combinatorial Generation.
 -}
module RemySampler (
    algR,
    algRIO
) where
  
    import Control.Monad.ST
    import Data.Array.ST
    
    import System.Random
    import CL
    
    -- | Samples a uniformly random SK-combinator.
    algR :: Int -> StdGen -> (CL, StdGen)
    algR 0 g = let (isS,g') = random g in
        if isS then (S,g') else (K,g')
    algR n g = runST $ do
        arrL <- newArray (0, 2 * n) 0
        (arrL', g') <- algR' arrL g n 0
        r <- readArray arrL' 0
        (t,g'') <- buildCL arrL' r g'
        return (t, g'')
    
    -- | Samples a uniformly random SK-combinator using the IO monad.
    algRIO :: Int -> IO CL
    algRIO n = getStdRandom (algR n)
  
    algR' :: STUArray s Int Int
          -> StdGen -> Int -> Int
          -> ST s (STUArray s Int Int, StdGen)
    algR' arrL g n i
        | n == i = return (arrL, g)
        | otherwise = do
            let (x,g') = randomR (0, 4 * i + 1) g
            
            let (k,b) = x `divMod` 2
            let i' = i + 1

            writeArray arrL (2 * i' - b) (2 * i')

            z <- readArray arrL k
            writeArray arrL (2 * i' - 1 + b) z

            writeArray arrL k (2 * i' - 1)
            algR' arrL g' n i'

    -- | Build a uniformly random SK-combinator out of 
    -- the given binary tree structure.
    buildCL :: STUArray s Int Int
            -> Int -> StdGen -> ST s (CL, StdGen)
    buildCL arrL n g = do
        lc <- readArray arrL n
        (nodeL, g') <- if even lc then do
                let (isS, g') = random g
                return $ if isS then (S,g') else (K,g')
            else buildCL arrL lc g

        rc <- readArray arrL (n + 1)
        (nodeR, g'') <- if even rc then do
                let (isS, g'') = random g'
                return $ if isS then (S,g'') else (K,g'')
            else buildCL arrL rc g'

        return (App nodeL nodeR, g'')
