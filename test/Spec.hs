{-# LANGUAGE TemplateHaskell #-}

import HMSTimeSpec

main :: IO ()
main = 
  do runHMSTimeSpecTests
     return ()