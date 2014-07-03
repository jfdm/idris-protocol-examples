-- ------------------------------------------------------------- [ Process.idr ]
--
-- Implementation of the echo protocol for process communication.
--
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.Chargen.Process

import Effects
import Effect.StdIO
import Effect.Default

import System.Protocol

import Protocol.CharGen
import Protocol.CharGen.Utils

-- ---------------------------------------------------------- [ Server Process ]

||| Implementation of the Echo Protocol from the Server's perspective.
|||
||| @client The PID of the client process.
covering
chargenProcessServer : (str : String)
                  -> (client : PID)
                  -> Process (chargen) 'Server ['Client := client] [STDIO] ()
chargenProcessServer str client = do
    msg <- recvFrom 'Client
    case msg of
      Just m => do
        sendTo 'Client str
        rec (chargenProcessServer (strShift str) client)

      Nothing => return ()

-- ---------------------------------------------------------- [ Client Process ]

||| Implementation of the Echo Protocol from the Client's perspective.
|||
||| @server The PID of the server process.
-- covering
chargenProcessClient : (server : PID)
                   -> Process (chargen) 'Client ['Server := server] [STDIO] ()
chargenProcessClient server = do
    putStrLn "To stop enter 'q':"
    msg_raw <- getStr
    case processMsg (trim msg_raw) of
      Nothing => do
        sendTo 'Server Nothing
      Just m => do
        sendTo 'Server (Just m)
        putStrLn !(recvFrom 'Server)
        rec (chargenProcessClient server)
  where
    processMsg : String -> Maybe String
    processMsg msg = case msg == "q" of
                       True => Nothing
                       False => Just msg

-- ------------------------------------------------------ [ Sample Innvocation ]

||| Sample Innvocation of the Echo protocols between the client and
||| server functions.
-- covering
doChargenProcess : IO ()
doChargenProcess = runConc [()] doEcho'
  where
    doEcho' : Process (chargen) 'Client [] [STDIO] ()
    doEcho' = do
       server <- spawn (chargenProcessServer dummyText) [()]
       setChan 'Server server
       chargenProcessClient server
       dropChan 'Server

-- --------------------------------------------------------------------- [ EOF ]
