{-|
 - Module       : Main 
 - Description  : SK-sampler executable module.
 - Copyright    : (c) Maciej Bendkowski, 2016
 - Maintainer   : maciej.bendkowski@tcs.uj.edu.pl
 - Stability    : experimental
 -}
module Main (main) where
    
    import System.IO
    import System.Exit
    import System.Console.GetOpt
    import System.Environment
    
    import Data.List (nub)

    import Control.Monad
    import Control.Concurrent.ParallelIO.Global

    import CL
    import RemySampler
    import Sampler

    data Flag = Samples String
              | Reductions String
              | Size String
              | Version
              | Help
                deriving (Eq)

    options :: [OptDescr Flag]
    options = [Option "s" ["samples"] (ReqArg Samples "s")
        "The number of uniformly random samples.",
    
        Option "r" ["reductions"] (ReqArg Reductions "r")
            "Uses up to r reductions while asserting normalization.",
        
        Option "n" ["size"] (ReqArg Size "n")
            "Samples SK-combinators of size n.",
        
        Option "v" ["version"] (NoArg Version)
            "Prints the program version number.",
        
        Option "h?" ["help"] (NoArg Help)
            "Prints this help message."]

    getSamples :: [Flag] -> Int
    getSamples (Samples sr : _) = read sr
    getSamples (_:fs) = getSamples fs
    getSamples [] = -1

    getReductions :: [Flag] -> Int
    getReductions (Reductions n : _) = read n
    getReductions (_:fs) = getReductions fs
    getReductions [] = -1

    getSize :: [Flag] -> Int
    getSize (Size n : _) = read n
    getSize (_:fs) = getSize fs
    getSize [] = -1
    
    usageHeader :: String
    usageHeader = "Usage: sk-sampler [OPTIONS...]"

    versionHeader :: String
    versionHeader = "SK-sampler version 1.0, (c) Maciej Bendkowski 2016"

    parse :: [String] -> IO ([Flag], [String])
    parse argv = case getOpt Permute options argv of
        (ops, nonops, [])
            | Help `elem` ops -> do
                putStrLn $ usageInfo usageHeader options
                exitSuccess
            | Version `elem` ops -> do
                putStrLn versionHeader
                exitSuccess
            | otherwise -> return (nub (concatMap mkset ops), fs)
                where
                    fs = if null nonops then ["-"] else nonops
                    mkset x = [x]
        (_, _, errs) -> do
            hPutStr stderr (concat errs ++ usageInfo usageHeader options)
            exitWith (ExitFailure 1)

    run :: [Flag] -> String -> IO ()
    run fgs _
        | r < 0 || s < 0 || n < 0 = do
            hPutStr stderr $ usageInfo usageHeader options
            exitWith (ExitFailure 1)
        | otherwise = do
            let exs = replicate s (trackableExperiment (trackNormalizationN r) n)
            parallel_ exs
            stopGlobalPool
        where
            r = getReductions fgs
            s = getSamples fgs
            n = getSize fgs

    main :: IO ()
    main = do
        (ops, fs) <- getArgs >>= parse
        mapM_ (run ops) fs
        exitSuccess
