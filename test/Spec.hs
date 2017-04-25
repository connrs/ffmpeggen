import           Control.Monad
import           FFMpegCommandSpec
import           HMSTimeSpec
import           UtilitiesSpec
import           Test.HUnit
import           Utilities

main :: IO ()
main = do
  hmsTimeSpecSuccess <- runHMSTimeSpecTests
  ffmpegCommandTestsCount <- runFFMpegCommandTests
  utilitiesTestsCount <- runUtilitiesTests
  let ffmpegCommandTestsErrors = errors ffmpegCommandTestsCount
  let ffmpegCommandTestsFailures = failures ffmpegCommandTestsCount
  let utilitiesTestsFailures = failures utilitiesTestsCount
  let utilitiesTestsErrors = errors utilitiesTestsCount
  let anyErrors = ffmpegCommandTestsErrors > 0
                  || ffmpegCommandTestsFailures > 0
                  || utilitiesTestsErrors > 0
                  || utilitiesTestsFailures > 0
                  || not hmsTimeSpecSuccess
  when anyErrors $ dieWithMessage "Errors found"
