{-# LANGUAGE FlexibleInstances #-}

import           Data.List
import           Data.Monoid
import           FfmpegGen
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    let hmsList = tail args
    putStr $ buildFfmpegSplitCmd fileName hmsList
    return ()

buildFfmpegSplitCmd :: String -> [String] -> String
buildFfmpegSplitCmd fileName hmsList = commandHead fileName ++ mkBody hmsList

mkBody :: [String] -> String
mkBody hmsList = foldr commandBody "" (zip [0..] $ mkStartStopList hmsList)

commandHead :: String -> String
commandHead a = "ffmpeg -i \"" ++ a ++ "\""

commandBody :: Show a => (a, Either String (String, String)) -> String -> String
commandBody (_, Left a) _ = error a
commandBody (i, Right (a,_)) "" = " -c:v libx264 -c:a libfaac -ss " ++ a ++ " " ++ show i ++ ".mp4"
commandBody (i, Right (a,b)) acc = " -c:v libx264 -c:a libfaac -ss " ++ a ++ " -t " ++ b ++ " " ++ show i ++ ".mp4" ++ acc

