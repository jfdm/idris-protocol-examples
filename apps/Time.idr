-- ---------------------------------------------------------------- [ Time.idr ]
--
-- Implementation of the Time Protocol for programs
--
-- --------------------------------------------------------------------- [ EOH ]
module Time

import Effects

import Effect.Default
import Effect.StdIO
import Effect.Msg

import System
import System.Protocol

import RFC.Time

-- ---------------------------------------------------------- [ Server Process ]
||| Implementation of the Time Protocol from the Server's Perspective
covering
timeServer : (proc : PID)
                  -> Process (timeProtocol) 'Server ['Client := proc] [STDIO] ()
timeServer proc = do
    req <- recvFrom 'Client
    let t = systime
    let resp = if t /= 0
                 then Right t
                 else Left "No time"
    sendTo 'Client resp
    return ()

  where
    systime : Int
    systime = unsafePerformIO time


||| Implementation of the Time Protocol from the Clients Perspective.
covering
timeClient : (proc : PID)
                  -> Process (timeProtocol) 'Client ['Server := proc] [STDIO] ()
timeClient proc = do
    sendTo 'Server Nothing
    t <- recvFrom 'Server
    case t of
      Left err => do
        putStrLn $ "Time not returned: " ++ err
        return ()
      Right res => do
        putStrLn $ "The unix time is: " ++ show res
        return ()


-- ------------------------------------------------------ [ Sample Innvocation ]
||| Sample innvocation
covering
doTimeProcess : IO ()
doTimeProcess = runConc [()] doTime'
  where
    doTime' : Process (timeProtocol) 'Client [] [STDIO] ()
    doTime' = do
        server <- spawn timeServer [()]
        setChan 'Server server
        timeClient server
        dropChan 'Server

-- -------------------------------------------------------------------- [ Main ]
namespace Main
  main : IO ()
  main = doTimeProcess
-- --------------------------------------------------------------------- [ EOF ]
