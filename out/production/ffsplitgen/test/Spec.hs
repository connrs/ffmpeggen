{-# LANGUAGE TemplateHaskell #-}

import HMSTimeSpec
import FFMpegCommandSpec

main :: IO ()
main = 
  do runHMSTimeSpecTests
     runFFMpegCommandTests
     return ()