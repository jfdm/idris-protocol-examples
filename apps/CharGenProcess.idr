-- ------------------------------------------------------------- [ Process.idr ]
--
-- Implementation of the echo protocol for process communication.
--
-- --------------------------------------------------------------------- [ EOH ]
module Chargen

import Effects
import Effect.StdIO
import Effect.Default

import System.Protocol

import RFC.CharGen
import Utils

-- ---------------------------------------------------------- [ Server Process ]

||| Implementation of the Echo Protocol from the Server's perspective.
|||
||| @client The PID of the client process.
covering
chargenServer : (str : Stream Char)
              -> Nat
              -> (client : PID)
              -> Process (chargen) 'Server ['Client := client] [STDIO] ()
chargenServer str pos client = do
    msg <- recvFrom 'Client
    case msg of
      Just m => do
        let pos' = mod (pos + 1) 92
        let str' = take 92 $ drop pos' str
        sendTo 'Client (pack str')
        rec (chargenServer str (pos + 1) client)

      Nothing => return ()

-- ---------------------------------------------------------- [ Client Process ]

||| Implementation of the Echo Protocol from the Client's perspective.
|||
||| @server The PID of the server process.
-- covering
chargenClient : (server : PID)
                   -> Process (chargen) 'Client ['Server := server] [STDIO] ()
chargenClient server = do
    putStrLn "To stop enter 'q':"
    msg_raw <- getStr
    case processMsg (trim msg_raw) of
      Nothing => do
        sendTo 'Server Nothing
      Just m => do
        sendTo 'Server (Just m)
        putStrLn !(recvFrom 'Server)
        rec (chargenClient server)
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
       server <- spawn (chargenServer dummyTextStream 0) [()]
       setChan 'Server server
       chargenClient server
       dropChan 'Server

-- -------------------------------------------------------------------- [ Main ]
namespace Main
  main : IO ()
  main = doChargenProcess

-- --------------------------------------------------------------------- [ EOF ]
