module Main

import Effects
import Effect.StdIO
import Effect.Default

import System.Protocol

import Protocol.Echo
import Protocol.Echo.Process

import Protocol.CharGen
import Protocol.CharGen.Process

import Protocol.Time
import Protocol.Time.Process

import Protocol.Daytime
import Protocol.Daytime.Process

data Example = Echo | Time | DayTime | CharGen

processArgs : (List String) -> Maybe Example
processArgs (x::y::xs) = case y of
    "echo"    => Just Echo
    "time"    => Just Time
    "daytime" => Just DayTime
    "chargen" => Just CharGen
    _         => Nothing

main : IO ()
main = do
    args <- getArgs
    case processArgs args of
      Just eg  => case eg of
        Echo    => doEchoProcess
        CharGen => doChargenProcess
        Time    => doTimeProcess
        DayTime => doDaytimeProcess
      Nothing => putStrLn "Example program not there"

-- --------------------------------------------------------------- [ EOF ]
