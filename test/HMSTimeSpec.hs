{-# LANGUAGE TemplateHaskell #-}

module HMSTimeSpec where

import HMSTime
import Test.QuickCheck.All

tdp :: (Integral a)
    => a -> Double -> Double
tdp n f = fromInteger (round $ f * (10 ^ n)) / (10.0 ^^ n)

-- Going from Double to HMSTime and back again produces the same number
-- (if we round to 3dp first)
prop_fromDouble_toDouble a = a' == (toDouble . fromDouble) a'
  where a' = tdp 3 a

-- If we compare 2 doubles (so that we get back greater than, less than or equal)
-- and compare the same 2 doubles converted to HMSTime, we get the same comparison
prop_gtDouble_eq_gtHMSTime a b = compare a' b' == compare a'' b''
  where a' = tdp 3 a
        b' = tdp 3 b
        a'' = fromDouble a'
        b'' = fromDouble b'

return []

runHMSTimeSpecTests = $(quickCheckAll)