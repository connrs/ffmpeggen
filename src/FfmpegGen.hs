module FfmpegGen (
  mkStartStopList
) where

import           Control.Applicative
import           Control.Arrow
import           Data.Either
import           Data.List
import qualified Data.Text           as T
import           Text.Read

type HmsValue     = Either String Double
type HmsString    = Either String String
type StartDurPair = Either String (String,String)

-- Generate a list of pairs from a list of hms strings
mkStartStopList :: [String] -> [(HmsString, HmsString)]
mkStartStopList = toHms . stopsToDurations . reverse . init . pairUpNums . mapHmsToSec
  where
    toHms            = map (secToHms *** secToHms)
    stopsToDurations = map tupleABDiff
    pairUpNums       = foldl (\acc time -> (snd $ head acc,time):acc) startingPair
    startingPair     = [(Right 0.0, Right 0.0)]
    mapHmsToSec      = map hmsToSec

hmsToSec :: String -> HmsValue
hmsToSec a = listToSeconds components
  where
    components     = (map textToHmsValue . reverse) hmsList
    hmsList        = splitByColon a
    splitByColon b = T.split (== ':') $ T.pack b

secToHms :: HmsValue -> HmsString
secToHms = fmap secToHms'

secToHms' :: RealFrac a => a -> String
secToHms' a = hms ++ decimals
  where
    seconds  = floor a
    decimals = getDecimals a
    h        = seconds `div` 3600
    m        = (seconds `div` 60) `mod` 60
    s        = seconds `mod` 60
    hms      = intercalate ":" (map (lp2 . show) [h,m,s])
    lp2      = leftPad 2 '0'

-- Take a number and pull out a string that contains 3dp, eg: ".123"
getDecimals :: RealFrac s => s -> String
getDecimals a = "." ++ decimals
  where
    decimals = take 3 $ lp3 $ show rounded
    rounded  = round $ (a - floored) * 1000
    floored  = fromIntegral $ floor a
    lp3      = leftPad 3 '0'

listToSeconds :: [HmsValue] -> HmsValue
listToSeconds a
    | any isLeft a = Left "Invalid hms string"
    | otherwise    = Right b
  where
    b = listToSeconds' a

listToSeconds' :: [HmsValue] -> Double
listToSeconds' a = sum $ zipWith (curry pairEitherMult) a powersOf60

pairEitherMult :: Num a => (Either t a, a) -> a
pairEitherMult (Left _, _)  = 0
pairEitherMult (Right a, b) = a * b

textToHmsValue :: T.Text -> HmsValue
textToHmsValue t = readEither s :: HmsValue
  where
    s = T.unpack t

leftPad :: Int -> a -> [a] -> [a]
leftPad n c s
    | excess <= 0 = s
    | otherwise   = replicate excess c ++ s
  where
    excess = n - length s

-- Infinite array of the powers of 60
powersOf60 = map (\x -> 60^x) [0..]

-- Take a (start,stop) pair and turn stop in to the difference of stop - start
tupleABDiff :: (Applicative f, Num b) => (f b, f b) -> (f b, f b)
tupleABDiff (a,b) = (a, subtract <$> a <*> b)

