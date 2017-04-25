module Utilities where

import           Control.Monad
import           Data.List (sort)
import           Text.Read (readMaybe)
import           System.IO (hPutStrLn, stderr)
import           System.Exit (exitWith, ExitCode(ExitFailure))

-- Convert list to list of pairs
pairs :: Ord a => [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs' (tail xs')
  where xs' = sort xs

-- Read a list of strings and return a Maybe List of
-- the parsed strings
listReadM :: (Read a) => [String] -> Maybe [a]
listReadM = mapM readMaybe

-- Take a function from list of a to b and a list of a and return either mzero or the lifted result
-- of applying the function
zeroMap :: (MonadPlus m) => ([a] -> b) -> [a] -> m b
zeroMap f [] = mzero
zeroMap f b = return . f $ b

-- Take list of lists and strip out empty ones
filterEmptyList :: Eq a => [[a]] -> [[a]]
filterEmptyList = filter (\x -> x /= [])

-- Exit the app with a given error code
dieWithMessage :: String -> IO ()
dieWithMessage a = do
  hPutStrLn stderr a
  exitWith (ExitFailure 1)