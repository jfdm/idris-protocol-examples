module Main

import Effects
import Effect.StdIO
import Effect.Default

import System.Protocol

import Protocol.Echo
import Protocol.Echo.Process
import Protocol.Echo.IPC

import Protocol.CharGen
import Protocol.CharGen.Process

import Protocol.Time
import Protocol.Time.Process

import Protocol.Daytime
import Protocol.Daytime.Process

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

doLaunch : Context -> IO () -> IO () -> IO () -> IO ()
doLaunch (ctxt) i p n = case ctxt of
  IPC     => i
  Process => p
  Network => n

main : IO ()
main = do
    args <- getArgs
    case processArgs args of
      Just (l,c)  => case l of
        Echo    => doLaunch c (doEchoIPC)
                              (doEchoProcess)
                              (nout)
        CharGen => doLaunch c (nout)
                              (doChargenProcess)
                              (nout)
        Time    => doLaunch c (nout)
                              (doTimeProcess)
                              (nout)
        DayTime => doLaunch c (nout)
                              (doDaytimeProcess)
                              (nout)
      Nothing => putStrLn "Example program not there"
  where
     nout : IO ()
     nout = putStrLn "Nothing just yet"

-- --------------------------------------------------------------- [ EOF ]
