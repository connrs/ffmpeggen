module FFMpegCommand (generateCommand) where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           HMSTime
import           Utilities
import           Command

-- Common aliases
type StartTime = HMSTime

type EndTime = HMSTime

type HMSTuple = (StartTime, EndTime)

type Extension = String

type InFile = String

type CommonParams = String

type Index = Int

-- Build the entire command string
generateCommand :: InFile -> Extension -> CommonParams -> [String] -> Maybe String
generateCommand i e c s = listReadM s >>= hmsPairs >>= buildCommand'
  where
    hmsPairs = zeroMap (pairs . wrapHMSTimes)
    buildCommand' = return . buildCommand . mkFfmpegCommand i e c

-- Construct a full command
mkFfmpegCommand :: InFile -> Extension -> CommonParams -> [HMSTuple] -> CommandTemplate
mkFfmpegCommand i e c h =
  CommandTemplate { prefix = Just "ffmpeg"
                  , commandComponents = zipWith (ffmpegCommandSegment i e c) [0 ..] h
                  , suffix = Nothing }

-- Build command for a single HMSTuple
ffmpegCommandSegment :: InFile -> Extension -> CommonParams -> Index -> HMSTuple -> CommandComponent
ffmpegCommandSegment inFile extension commonParams index (start, stop)
  | stop == EndHMSTime = CommandComponent $ filterEmptyList [inParam, commonParams, startParam, outputParam]
  | otherwise = CommandComponent $ filterEmptyList [inParam, commonParams, startParam, stopParam, outputParam]
  where
    inParam = "-i " ++ inFile
    outputParam = show index ++ "." ++ extension
    startParam = "-ss " ++ show start
    stopParam = "-t " ++ show stop

-- Wrap list of HMSTime with Start and End
wrapHMSTimes a = concat [[StartHMSTime], a, [EndHMSTime]]