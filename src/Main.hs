{-# LANGUAGE FlexibleInstances #-}

import           BuildCommand
import           Data.Maybe
import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main = do
    args <- getArgs
    if length args /= 4
        then dieWithMessage "Invalid arguments list"
        else generateCommand args

generateCommand :: [String] -> IO ()
generateCommand args = do
            let [fileName,extension,commonParams,times] = args
            let command = buildCommand fileName extension commonParams (words times)
            maybe (dieWithMessage "Invalid time list") putStrLn command

dieWithMessage :: String -> IO ()
dieWithMessage a = do
    hPutStrLn stderr a
    exitWith (ExitFailure 1)
