module FFMpegCommandSpec where

import Test.HUnit
import FFMpegCommand

runFFMpegCommandTests = runTestTT tests

tests = TestList [ TestLabel "Test Single Time" testSingleTime
                 , TestLabel "Test Two Times" testTwoTimes ]

testSingleTime = TestCase (
  assertEqual "test.mkv 00:00:01" (Just "ffmpeg -i test.mkv -ss 00:00:00.000 -t 00:00:01.000 0.mp4 -i test.mkv -ss 00:00:01.000 1.mp4") (generateCommand "test.mkv" "mp4" "" ["00:00:01"]) )

testTwoTimes = TestCase (
  assertEqual "test.mkv 00:00:01 00:00:02" (Just "ffmpeg -i test.mkv -ss 00:00:00.000 -t 00:00:01.000 0.mp4 -i test.mkv -ss 00:00:01.000 -t 00:00:02.000 1.mp4 -i test.mkv -ss 00:00:02.000 2.mp4") (generateCommand "test.mkv" "mp4" "" ["00:00:01", "00:00:02"]) )

testExtraCommands = TestCase (
  assertEqual "test.mkv 00:00:01 00:00:02" (Just "ffmpeg -i test.mkv -c:v libx264 -ss 00:00:00.000 -t 00:00:01.000 0.mp4 -i test.mkv -c:v libx264 -ss 00:00:01.000 -t 00:00:02.000 1.mp4 -i test.mkv -c:v libx264 -ss 00:00:02.000 2.mp4") (generateCommand "test.mkv" "mp4" "-c:v libx264" ["00:00:01", "00:00:02"]) )
