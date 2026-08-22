module Main where

data LegacyFahrenheitSensor = LegacyFahrenheitSensor

readFahrenheit :: LegacyFahrenheitSensor -> Int
readFahrenheit _ = 86

newtype TemperatureReader = TemperatureReader { readCelsius :: Int }

adaptTemperature :: LegacyFahrenheitSensor -> TemperatureReader
adaptTemperature sensor =
  let fahrenheit = readFahrenheit sensor
      celsius = round (fromIntegral (fahrenheit - 32) * 5 / 9 :: Double)
  in TemperatureReader celsius

main :: IO ()
main = do
  let legacy = LegacyFahrenheitSensor
      reader = adaptTemperature legacy
  putStrLn $ "legacy=" ++ show (readFahrenheit legacy) ++ "F"
  putStrLn $ "adapted=" ++ show (readCelsius reader) ++ "C"
