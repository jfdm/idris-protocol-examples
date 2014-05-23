-- ------------------------------------------------------------- [ Process.idr ]
--
-- Implementation of the Daytime protocol
--
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.Daytime.Process

import Effects
import Effect.StdIO
import Effect.Default

import System.Protocol

import Protocol.Daytime
import Protocol.Daytime.Utils

-- ---------------------------------------------------------- [ Server Process ]

||| Implementation of the Daytime Protocol from the Server's perspective.
|||
||| @proc The PID of the client process.
-- covering
daytimeProcessServer : (proc : PID)
                  -> Process IO (daytime) 'Server ['Client := proc] [STDIO] ()
daytimeProcessServer proc = do
    msg <- recvFrom 'Client
    sendTo 'Client (getDayTime)
    return ()

-- ---------------------------------------------------------- [ Client Process ]

||| Implementation of the Daytime Protocol from the Client's perspective.
|||
||| @proc The PID of the server process.
-- covering
daytimeProcessClient : (proc : PID)
                   -> Process IO (daytime) 'Client ['Server := proc] [STDIO] ()
daytimeProcessClient proc = do
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
-- covering
doDaytimeProcess : IO ()
doDaytimeProcess = runConc [()] doDaytime'
  where
    doDaytime' : Process IO (daytime) 'Client [] [STDIO] ()
    doDaytime' = do
       server <- spawn (daytimeProcessServer) [()]
       setChan 'Server server
       daytimeProcessClient server
       dropChan 'Server

-- --------------------------------------------------------------------- [ EOF ]
