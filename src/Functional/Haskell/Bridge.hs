module Main where

data Device = Device { turnOn :: String, mute :: String }

tv, radio :: Device
tv = Device "TV:on" "TV:muted"
radio = Device "Radio:on" "Radio:muted"

type Remote = Device -> String

basicRemote, muteRemote :: Remote
basicRemote = turnOn
muteRemote = mute

main :: IO ()
main = do
  putStrLn $ "basic-tv=" ++ basicRemote tv
  putStrLn $ "basic-radio=" ++ basicRemote radio
  putStrLn $ "mute-tv=" ++ muteRemote tv
  putStrLn $ "mute-radio=" ++ muteRemote radio
