-- ------------------------------------------------------------- [ Process.idr ]
--
-- Implementation of the echo protocol for process communication.
--
-- --------------------------------------------------------------------- [ EOH ]
module EchoProcess

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
echoClient : Maybe String ->
                  (server : PID) ->
                  Process (echo) 'Client ['Server := server] [STDIO] ()
echoClient msg server = do
    case msg of
      Nothing => do
        sendTo 'Server Nothing
      Just str => do
        sendTo 'Server (Just str)
        (resp ** _ ) <- recvFrom 'Server
        putStrLn $ show resp
        rec (echoClient Nothing server)


-- ------------------------------------------------------ [ Sample Innvocation ]

||| Sample Innvocation of the Echo protocols between the client and
||| server functions.
covering
doEchoProcess : String -> IO ()
doEchoProcess s = runConc [()] (doEcho s)
  where
    doEcho : String -> Process (echo) 'Client [] [STDIO] ()
    doEcho s = do
       server <- spawn (echoServer) [()]
       setChan 'Server server
       echoClient (Just s) server
       dropChan 'Server


processArgs : (List String) -> Maybe String
processArgs [x] = Nothing
processArgs (x::xs) = Just $ unwords xs

-- -------------------------------------------------------------------- [ Main ]
namespace Main
  main : IO ()
  main = do
    args <- getArgs
    case processArgs args of
      Just str => doEchoProcess str
      Nothing => doEchoProcess "\n"


-- --------------------------------------------------------------------- [ EOF ]
