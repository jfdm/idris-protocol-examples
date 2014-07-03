-- ---------------------------------------------------------------- [ Time.idr ]
--
-- Implementation of the Time Protocol for programs
--
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.Time.Process

import Effects

import Effect.Default
import Effect.StdIO

import System.Protocol

import Protocol.Time

-- ---------------------------------------------------------- [ Server Process ]
||| Implementation of the Time Protocol from the Server's Perspective
covering
timeProcessServer : (proc : PID)
                  -> Process (timeProtocol) 'Server ['Client := proc] [STDIO] ()
timeProcessServer proc = do
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
timeProcessClient : (proc : PID)
                  -> Process (timeProtocol) 'Client ['Server := proc] [STDIO] ()
timeProcessClient proc = do
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
        server <- spawn timeProcessServer [()]
        setChan 'Server server
        timeProcessClient server
        dropChan 'Server

-- --------------------------------------------------------------------- [ EOF ]
