module Main where

import System.IO
import Text.Regex.Base
import Text.Regex.TDFA

computeWinningChargeTimesWithSpeedToComplete :: (Int, Int) -> [(Int, Int)]
computeWinningChargeTimesWithSpeedToComplete (duration, distance) = do
  let possibleChargeTimes = [1 .. duration - 1] -- No point to charge 0ms or to charge full amount of time
  let remainingTimes = map (\chargeTime -> duration - chargeTime) possibleChargeTimes
  let chargeTimesWithRemainingTimes = zip possibleChargeTimes remainingTimes
  let allChargeTimes = map computeSpeedToComplete chargeTimesWithRemainingTimes
  filter (isWinningChargeTime distance) allChargeTimes

computeSpeedToComplete :: (Int, Int) -> (Int, Int)
computeSpeedToComplete (chargeTime, remainingTime) = do
  (chargeTime, (chargeTime * remainingTime))

countWinningWaysPerDay :: [(Int, Int)] -> Int
countWinningWaysPerDay winningChargeTimes = length winningChargeTimes

isWinningChargeTime :: Int -> (Int, Int) -> Bool
isWinningChargeTime distance (chargeTime, distanceCompleted) = do
  distance < distanceCompleted

parseIntegersFromLine :: String -> [Int]
parseIntegersFromLine line = do
  let pattern = "[0-9]+"
  let matches = getAllTextMatches (line =~ pattern) :: [String]
  map read matches

main :: IO ()
main = do
  timesLine <- getLine
  distancesLine <- getLine
  let times = parseIntegersFromLine timesLine
  let distances = parseIntegersFromLine distancesLine
  let timesWithDistances = zip times distances
  let winningChargeTimes = map computeWinningChargeTimesWithSpeedToComplete timesWithDistances
  let counts = map countWinningWaysPerDay winningChargeTimes
  print (product counts)
