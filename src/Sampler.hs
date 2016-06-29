{-|
 - Module       : Sampler 
 - Description  : Generic SK-combinator sampler utilities.
 - Copyright    : (c) Maciej Bendkowski, 2016
 - Maintainer   : maciej.bendkowski@tcs.uj.edu.pl
 - Stability    : experimental
 -}
module Sampler (
    trackableExperiment
) where
 
    import Control.Concurrent.ParallelIO.Global

    import CL
    import RemySampler

    -- | Given an experiment function and a number n,
    -- samples a uniformly random combinator t of size n
    -- and tracks the experiment result on t.
    trackableExperiment :: (Show a, Num a, Integral b)
                         => (CL -> (Bool, a)) -> b -> IO ()
    
    trackableExperiment f n = do
        t <- algRIO (fromIntegral n)
        let (b, m) = f t
        let x = if b then m else (-1)
        print x
