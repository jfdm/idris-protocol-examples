-- ------------------------------------------------------------- [ Process.idr ]
--
-- Implementation of the echo protocol for process communication.
--
-- --------------------------------------------------------------------- [ EOH ]
module EchoProtocol

import Effects
import Effect.StdIO
import Effect.Default

import System.Protocol

import RFC.Echo

-- ---------------------------------------------------------- [ Server Process ]

||| Implementation of the Echo Protocol from the Server's perspective.
|||
||| @client The PID of the client process.
covering
echoServer : (client : PID)
           -> Process (echo) 'Server ['Client := client] [STDIO] ()
echoServer client = do
    msg <- recvFrom 'Client
    case msg of
      Just m => do
        sendTo 'Client (m ** Refl)
        rec (echoServer client)

      Nothing => return ()

-- ---------------------------------------------------------- [ Client Process ]

||| Implementation of the Echo Protocol from the Client's perspective.
|||
||| @server The PID of the server process.
covering
echoClient : (server : PID)
           -> Process (echo) 'Client ['Server := server] [STDIO] ()
echoClient server = do
    putStrLn "Enter some text ('q' to quit):"
    msg_raw <- getStr
    case (processMsg (trim msg_raw)) of
      Nothing => do
        sendTo 'Server Nothing
        return ()

      Just m => do
        sendTo 'Server (Just m)
        (resp ** _ ) <- recvFrom 'Server
        putStrLn $ show resp
        rec (echoClient server)

  where
    processMsg : String -> Maybe String
    processMsg msg = case msg == "q" of
                       True => Nothing
                       False => Just msg

-- ------------------------------------------------------ [ Sample Innvocation ]

||| Sample Innvocation of the Echo protocols between the client and
||| server functions.
covering
doEchoProcess : IO ()
doEchoProcess = runConc [()] doEcho
  where
    doEcho : Process (echo) 'Client [] [STDIO] ()
    doEcho = do
       server <- spawn (echoServer) [()]
       setChan 'Server server
       echoClient server
       dropChan 'Server

-- -------------------------------------------------------------------- [ Main ]
namespace Main
  main : IO ()
  main = doEchoProcess

-- --------------------------------------------------------------------- [ EOF ]
