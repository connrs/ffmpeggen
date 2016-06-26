module BuildCommand (generateCommand) where

import Control.Monad
import Data.List
import Data.Maybe
import HMSTime
import Text.Read
import Utilities

-- Common aliases
type StartTime = HMSTime

type EndTime = HMSTime

type HMSTuple = (StartTime,EndTime)

type Extension = String

type InFile = String

type CommonParams = String

type Index = Int

-- Build the entire command string
generateCommand :: InFile -> Extension -> CommonParams -> [String] -> Maybe String
generateCommand i e c s = parseTimes s >>= hmsPairs >>= constructCommand'
  where constructCommand' = return . constructCommand i e c

-- Build command for a single HMSTuple
generateComponent :: InFile -> Extension -> CommonParams -> Index -> HMSTuple -> String
generateComponent inFile extension commonParams index (start,stop)
  | stop == EndHMSTime = lastParams
  | otherwise = fullParams
  where inParam = "-i " ++ inFile
        outputParam = show index ++ "." ++ extension
        startParam = "-ss " ++ show start
        stopParam = "-t " ++ show stop
        combineNonnull = unwords . filter (not . null)
        fullParams = combineNonnull [inParam,commonParams,startParam,stopParam,outputParam]
        lastParams = combineNonnull [inParam,commonParams,startParam,outputParam]

-- Get list of strings and convert to HMSTime
parseTimes :: [String] -> Maybe [HMSTime]
parseTimes = mapM readMaybe

-- Wrap list of HMSTime with Start and End
wrapHMSTimes a = StartHMSTime : a ++ [EndHMSTime]

-- Get a list of HMSTime and convert to pairs of start-stop
hmsPairs :: (MonadPlus m) => [HMSTime] -> m [HMSTuple]
hmsPairs [] = mzero
hmsPairs a = return . pairs . wrapHMSTimes $ a

-- Construct a full command
constructCommand :: InFile -> Extension -> CommonParams -> [HMSTuple] -> String
constructCommand inputFile extension commonParams = unwords . prependFfmpeg . zipCommands
  where build = generateComponent inputFile extension commonParams
        prependFfmpeg = ("ffmpeg " :)
        zipCommands = zipWith build [0 ..]