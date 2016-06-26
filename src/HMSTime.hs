module HMSTime (mkHMSTime, HMSTime(..), fromDouble, toDouble) where

import Data.List
import qualified Data.Text.Format as F
import Data.Ord
import Data.Char
import Control.Applicative

-- Constants
maxSeconds = 59

maxMinutes = 59

maxMilliseconds = 999

-- Data type
data HMSTime
  = MkHMSTime {hour :: Int
              ,minute :: Int
              ,second :: Int
              ,millisecond :: Int}
  | EndHMSTime 
  | StartHMSTime 

instance Read HMSTime where
  readsPrec _ (h1:h2:':':m1:m2:':':s1:s2:'.':excess) = 
    let h = read [h1,h2]
        m = read [m1,m2]
        s = read [s1,s2]
        (msParts,rest) = span isDigit excess
        ms = 
          case msParts of
            [] -> 0
            _ -> read msParts
    in [(mkHMSTime h m s ms,rest)]
  readsPrec _ (h1:h2:':':m1:m2:':':s1:s2:excess) = 
    let h = read [h1,h2]
        m = read [m1,m2]
        s = read [s1,s2]
        ms = 0
    in [(mkHMSTime h m s ms,excess)]
  readsPrec _ _ = []

instance Show HMSTime where
  show EndHMSTime = error "It is not possible to display the end time"
  show StartHMSTime = "00:00:00.000"
  show a = 
    intercalate ":"
                [h,m,s] ++
    "." ++ ms
    where pad3 = leftPad 3 '0' . show
          pad2 = leftPad 2 '0' . show
          h = pad2 $ hour a
          m = pad2 $ minute a
          s = pad2 $ second a
          ms = pad3 $ millisecond a

instance Ord HMSTime where
  compare StartHMSTime StartHMSTime = EQ
  compare StartHMSTime _ = LT
  compare EndHMSTime EndHMSTime = EQ
  compare EndHMSTime _ = GT
  compare MkHMSTime{} StartHMSTime = GT
  compare MkHMSTime{} EndHMSTime = LT
  compare a b = 
    compare (toDouble a)
            (toDouble b)

instance Eq HMSTime where
  StartHMSTime == StartHMSTime = True
  EndHMSTime == EndHMSTime = True
  a@MkHMSTime{} == b@MkHMSTime{} = 
    and [hour a == hour b
        ,minute a == minute b
        ,second a == second b
        ,millisecond a == millisecond b]
  _ == _ = False

-- Construct an HMSTime
mkHMSTime h m s ms
  | h == 0 && m == 0 && s == 0 && ms == 0 = StartHMSTime
  | validComponents m s ms = 
    MkHMSTime {hour = h
              ,minute = m
              ,second = s
              ,millisecond = ms}
  | otherwise = error "Bad HMSTime"
  where validComponents m s ms = (m <= maxMinutes) && (s <= maxSeconds)

-- Create a HMSTime value from a Double
fromDouble :: Double -> HMSTime
fromDouble seconds = 
  let absSeconds = abs seconds
      roundAbsSeconds = fromIntegral . floor $ absSeconds
      absSecondsInt = floor absSeconds :: Int
      ms = round ((seconds - roundAbsSeconds) * 1000.0) :: Int
      h = absSecondsInt `div` 3600
      m = (absSecondsInt `div` 60) `mod` 60
      s = absSecondsInt `mod` 60
  in mkHMSTime h m s ms

-- Convert HMSTime to a Double for use in calculations
toDouble :: HMSTime -> Double
toDouble StartHMSTime = 0.0
toDouble a = 
  let hoursInMs = (* 1000) . fromIntegral . (* 3600) . hour
      minutesInMs = (* 1000) . fromIntegral . (* 60) . minute
      secondsInMs = (* 1000) . fromIntegral . second
      ms = fromIntegral . millisecond
  in (/ 1000) $ sum $ [hoursInMs,minutesInMs,secondsInMs,ms] <*> [a]

-- Pad a list of a with a where length is less than n
leftPad :: Int -> a -> [a] -> [a]
leftPad n c s
  | excess <= 0 = s
  | otherwise = replicate excess c ++ s
  where excess = n - length s