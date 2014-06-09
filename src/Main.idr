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

import Protocol.Greeter.Example

data Example = Echo | Time | DayTime | CharGen | Greeter

processArgs : (List String) -> Maybe Example
processArgs [x] = Nothing
processArgs (x::y::xs) = case y of
    "echo"    => Just Echo
    "time"    => Just Time
    "daytime" => Just DayTime
    "chargen" => Just CharGen
    "greeter" => Just Greeter
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
        Greeter => doGreeterProcess 3
      Nothing => putStrLn "Example program not there"

-- --------------------------------------------------------------- [ EOF ]
