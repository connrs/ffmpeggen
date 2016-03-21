module BuildCommand ( generateCommand
                    ) where

import           Data.List
import           Data.Maybe
import           HMSTime
import           Text.Read

-- Common aliases
type StartTime    = HMSTime
type EndTime      = HMSTime
type HMSTuple     = (StartTime,EndTime)
type Extension    = String
type InFile       = String
type CommonParams = String
type Index        = Int

-- Build the entire command string
generateCommand :: InFile -> Extension -> CommonParams -> [String] -> Maybe String
generateCommand i e c s = parseTimes s >>= toStartStopPairs >>= constructCommand'
    where constructCommand' = Just . constructCommand i e c

-- Build command for a single HMSTuple
generateComponent :: InFile -> Extension -> CommonParams -> Index -> HMSTuple -> String
generateComponent inFile extension commonParams index (start,stop)
    | toDouble stop == 0 = lastParams
    | otherwise          = fullParams
    where
        inParam     = "-i " ++ inFile
        outputParam = show index ++ "." ++ extension
        startParam  = "-ss " ++ show start
        stopParam   = "-t " ++ show stop
        fullParams  = unwords $ stripBlank [inParam, commonParams, startParam, stopParam, outputParam]
        lastParams  = unwords $ stripBlank [inParam, commonParams, startParam, outputParam]
        stripBlank  = filter (not . null)

-- Get list of strings and convert to HMSTime
parseTimes :: [String] -> Maybe [HMSTime]
parseTimes = mapM readMaybe

-- Get a list of HMSTime and convert to pairs of start-stop
toStartStopPairs :: [HMSTime] -> Maybe [HMSTuple]
toStartStopPairs [] = Nothing
toStartStopPairs a  =
    let a'    = [fromDouble 0] ++ a ++ [fromDouble 0] -- Add 00:00:00.000 to the beginning of the list
        start = init a'
        end   = tail a'
    in Just (zip start end)

-- Construct a full command
constructCommand :: InFile -> Extension -> CommonParams -> [HMSTuple] -> String
constructCommand inputFile extension commonParams = unwords . prependFfmpeg . zipCommands
    where build = generateComponent inputFile extension commonParams
          prependFfmpeg = ("ffmpeg " :)
          zipCommands = zipWith build [0..]
