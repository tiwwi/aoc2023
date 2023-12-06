module Day06 (solveFrom) where

data Race = Race Int Int

solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . readFile

solve :: String -> (String, String)
solve txt = (show $ part1 races, show $ part2 bigRace)
    where races = parseRaces txt
          bigRace = parseBigRace txt

parseRaces :: String -> [Race]
parseRaces txt = zipWith Race times dists
    where (times:dists:_) = map read . tail . words <$> lines txt

parseBigRace :: String -> Race
parseBigRace txt = Race time dist
    where (time:dist:_) = read . concat . tail . words <$> lines txt

numberStrategies :: Race -> Int
numberStrategies (Race time distance) = solhi - sollo + 1
    where p = fromIntegral $ -time :: Double
          q = fromIntegral distance :: Double
          disk = sqrt $ (p/2)^2 - q
          solhi = ceiling $ (-p/2) + disk - 1
          sollo = floor   $ (-p/2) - disk + 1

part1 :: [Race] -> Int
part1 = product . map numberStrategies

part2 :: Race -> Int
part2 = numberStrategies
