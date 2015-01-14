module Main

import Effects
import Effect.StdIO
import Effect.Default
import Effect.Msg

import System.Protocol

import Greeter.Example

usage : String
usage = unwords $ intersperse "\n" ["Usage:",
          "\t greeter-app <num>"]

processArgs : (List String) -> Maybe Int
processArgs [x]         = Nothing
processArgs (x::y::Nil) = Just $ cast y
processArgs _           = Nothing

main : IO ()
main = do
    args <- getArgs
    case processArgs args of
      Just i  => doGreeterProcess i
      Nothing => do
        putStrLn "Greeter takes a number"
        putStrLn usage

-- --------------------------------------------------------------- [ EOF ]
