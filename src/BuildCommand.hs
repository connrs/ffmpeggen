module BuildCommand where

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

-- Take a list of strings and read them in as HMSTimes. Finally convert the list to a tuple list
-- This function is very naive and I expect it to exit with an error initially if the input is bad
mkTupleList :: [String] -> Maybe [HMSTuple]
mkTupleList = toTupleList . mapM readMaybe

toTupleList :: Maybe [HMSTime] -> Maybe [HMSTuple]
toTupleList Nothing   = Nothing
toTupleList (Just []) = Nothing
toTupleList (Just a)  =
    let a'    = [fromDouble 0] ++ a ++ [fromDouble 0] -- Add 00:00:00.000 to the beginning of the list
        start = init a'
        end   = tail a'
    in Just (zip start end)

-- Build the entire command string
buildCommand :: InFile -> Extension -> CommonParams -> [String] -> Maybe String
buildCommand i e c s
    | isNothing tuples = Nothing
    | otherwise        = Just $ unwords commands
    where
        tuples   = mkTupleList s
        commands = "ffmpeg" : zipWith build [0..] (fromJust tuples)
        build    = buildSingleCommand i e c

-- Build command for a single HMSTuple
buildSingleCommand :: InFile -> Extension -> CommonParams -> Index -> HMSTuple -> String
buildSingleCommand inFile extension commonParams index (start,stop)
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
