-- ------------------------------------------------------------- [ Process.idr ]
--
-- Implementation of the Daytime protocol
--
-- --------------------------------------------------------------------- [ EOH ]
module Daytime

import Effects
import Effect.StdIO
import Effect.Default

import System.Protocol

import RFC.Daytime
import RFC.Utils
import Utils

-- ---------------------------------------------------------- [ Server Process ]

||| Implementation of the Daytime Protocol from the Server's perspective.
|||
||| @proc The PID of the client process.
covering
daytimeServer : (proc : PID)
              -> Process (daytime) 'Server ['Client := proc] [STDIO] ()
daytimeServer proc = do
    msg <- recvFrom 'Client
    sendTo 'Client (getDayTime)
    return ()

-- ---------------------------------------------------------- [ Client Process ]

||| Implementation of the Daytime Protocol from the Client's perspective.
|||
||| @proc The PID of the server process.
covering
daytimeClient : (proc : PID)
              -> Process (daytime) 'Client ['Server := proc] [STDIO] ()
daytimeClient proc = do
    sendTo 'Server Nothing
    dt <- recvFrom 'Server
    case dt of
      Left err => do
        putStrLn $ "Time not returned: " ++ err
        return ()
      Right res => do
        putStrLn $ "The time is: " ++ show res
        return ()

-- ------------------------------------------------------ [ Sample Innvocation ]

||| Sample Innvocation of the Echo protocols between the client and
||| server functions.
covering
doDaytimeProcess : IO ()
doDaytimeProcess = runConc [()] doDaytime'
  where
    doDaytime' : Process (daytime) 'Client [] [STDIO] ()
    doDaytime' = do
       server <- spawn (daytimeServer) [()]
       setChan 'Server server
       daytimeClient server
       dropChan 'Server

-- -------------------------------------------------------------------- [ Main ]
namespace Main
  main : IO ()
  main = doDaytimeProcess

-- --------------------------------------------------------------------- [ EOF ]
