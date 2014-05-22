module Main

import Effects
import Effect.StdIO
import Effect.Default

import System.Protocol

import Protocol.Echo
import Protocol.Echo.Process
import Protocol.Echo.IPC

import Protocol.Chargen
import Protocol.Chargen.Process

import Protocol.Time
import Protocol.Daytime




data Example = Echo | Time | DayTime | CharGen
data Context = IPC | Process | Network

Launch : Type
Launch = (Example, Context)

processArgs : (List String) -> Maybe (Launch)
processArgs (x::y::z::xs) = case y of
    "echo"    => Just $ (Echo    , (getContext z) )
    "time"    => Just $ (Time    , (getContext z) )
    "daytime" => Just $ (DayTime , (getContext z) )
    "chargen" => Just $ (CharGen , (getContext z) )
    _         => Nothing
  where
    getContext : String -> Context
    getContext str = case str of
      "ipc"  => IPC
      "proc" => Process
      "net"  => Network

main : IO ()
main = do
    args <- getArgs
    case processArgs args of
      Just (l,c)  => case l of
        Echo    => doLaunch c (doEchoIPC) (doEchoProcess) (doEchoIPC)
        CharGen => doLaunch c (putStrLn "as") (doChargenProcess) (putStrLn "asas")
      Nothing => putStrLn "Example program not there"
  where
    doLaunch : Context -> IO () -> IO () -> IO () -> IO ()
    doLaunch (ctxt) i p n = case ctxt of
      IPC     => i
      Process => p
      Network => n


-- --------------------------------------------------------------- [ EOF ]
