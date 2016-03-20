module HMSTime ( mkHMSTime
               , fromDouble
               , toDouble
               , HMSTime
               ) where

import           Data.List
import qualified Data.Text.Format as F

-- Constants
maxSeconds = 59
maxMinutes = 59
maxMilliseconds = 999

-- Data type
data HMSTime = HMSTime { hour        :: Int
                       , minute      :: Int
                       , second      :: Int
                       , millisecond :: Int
                       }

instance Read HMSTime where
    readsPrec _ (h1:h2:':':m1:m2:':':s1:s2:'.':ms1:ms2:ms3:excess) =
        let h  =  read [h1,h2] :: Int
            m  =  read [m1,m2] :: Int
            s  =  read [s1,s2] :: Int
            ms = read [ms1,ms2,ms3] :: Int
        in [(mkHMSTime h m s ms,excess)]
    readsPrec _ (h1:h2:':':m1:m2:':':s1:s2:excess) =
        let h  =  read [h1,h2] :: Int
            m  =  read [m1,m2] :: Int
            s  =  read [s1,s2] :: Int
            ms = 0
        in [(mkHMSTime h m s ms,excess)]
    readsPrec _ _ = []

instance Show HMSTime where
    show a = intercalate ":" [h,m,s] ++ "." ++ ms
        where pad3 = leftPad 3 '0' . show
              pad2 = leftPad 2 '0' . show
              h  = pad2 $ hour a
              m  = pad2 $ minute a
              s  = pad2 $ second a
              ms = pad3 $ millisecond a

-- Construct an HMSTime
mkHMSTime h m s ms
    | validComponents m s ms = HMSTime { hour=h, minute=m, second=s, millisecond=ms }
    | otherwise              = error "Bad HMSTime"
    where
        validComponents m s ms = (m <= maxMinutes) && (s <= maxSeconds)

-- Create a HMSTime value from a Double
fromDouble :: Double -> HMSTime
fromDouble seconds =
    let
        absSeconds    = abs seconds
        absSecondsInt = round absSeconds :: Int
        ms            = round ((seconds - absSeconds) * 1000.0) :: Int
        h             = absSecondsInt `div` 3600
        m             = (absSecondsInt `div` 60) `mod` 60
        s             = absSecondsInt `mod` 60
    in mkHMSTime h m s ms

-- Convert HMSTime to a Double for use in calculations
toDouble :: HMSTime -> Double
toDouble a =
    let hoursInSeconds   = fromIntegral . (* 3600) . hour
        minutesInSeconds = fromIntegral . (* 60) . minute
        seconds          = fromIntegral $ second a
        msInSeconds      = (/1000) . fromIntegral . millisecond
    in hoursInSeconds a + minutesInSeconds a + seconds + msInSeconds a

-- Pad a list of a with a where length is less than n
leftPad :: Int -> a -> [a] -> [a]
leftPad n c s
    | excess <= 0 = s
    | otherwise   = replicate excess c ++ s
  where
    excess = n - length s

