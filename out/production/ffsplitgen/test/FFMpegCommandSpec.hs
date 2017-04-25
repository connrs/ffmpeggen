module FFMpegCommandSpec where

import Test.HUnit
import FFMpegCommand

runFFMpegCommandTests = runTestTT tests

tests = TestList [ TestLabel "Test Single Time" testSingleTime
                 ]
testSingleTime = TestCase (assertEqual "test.mkv 00:00:01" (Just "ffmpeg -i test.mkv -ss 00:00:00.000 -t 00:00:01.000 0.mp4 -i test.mkv -ss 00:00:01.000 1.mp4") (generateCommand "test.mkv" "mp4" "" ["00:00:01"]))
