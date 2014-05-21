-- ------------------------------------------------------------- [ Process.idr ]
--
-- Implementation of the echo protocol for process communication.
--
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.Echo.Process

import Effects
import Effect.StdIO
import Effect.Default

import System.Protocol

import Protocol.Echo

-- ---------------------------------------------------------- [ Server Process ]

||| Implementation of the Echo Protocol from the Server end of the
||| Protocol.
||| 
||| @client The PID of the client process.
covering
echoProcessServer : (client : PID)
                  -> Process IO (echo) 'Server ['Client := client] [STDIO] ()
echoProcessServer client = do
    msg <- recvFrom 'Client
    case msg of
      Just m => do
        sendTo 'Client (m ** refl)
        rec (echoProcessServer client)
      Nothing => return ()

-- ---------------------------------------------------------- [ Client Process ]

||| Implementation of the Echo Protocol from the Client end of the
||| Protocol.
|||
||| @server The PID of the server process.
covering
echoProcessClient : (server : PID)
                   -> Process IO (echo) 'Client ['Server := server] [STDIO] ()
echoProcessClient server = do
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
        rec (echoProcessClient server)


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
doEchoProcess = runConc [()] doEcho'
  where
    doEcho' : Process IO (echo) 'Client [] [STDIO] ()
    doEcho' = do
       server <- spawn (echoProcessServer) [()]
       setChan 'Server server
       echoProcessClient server
       dropChan 'Server

-- ---------------------------------------------------- [ Naive Echo Processes ]

||| Implementation of the Echo Protocol from the Server end of the
||| Protocol.
||| 
||| @client The PID of the client process.
covering
echoProcessServer' : (client : PID)
           -> Process IO (echo') 'Server ['Client := client] [STDIO] ()
echoProcessServer' client = do
    msg <- recvFrom 'Client
    sendTo 'Client msg
    return ()

||| Implementation of the Echo Protocol from the Client end of the
||| Protocol.
|||
||| @server The PID of the server process.
covering
echoProcessClient' : (server : PID)
           -> Process IO (echo') 'Client ['Server := server] [STDIO] ()
echoProcessClient' server = do
    putStrLn "Enter some text:"
    msg <- getStr    
    sendTo 'Server msg
    resp <- recvFrom 'Server
    putStrLn $ show (trim resp)
    return ()
    

||| Sample Innvocation of the Echo protocols between the client and
||| server functions.
doEchoProcess' : IO ()
doEchoProcess' = forever $ runConc [()] doEcho''
  where
    forever : IO () -> IO ()
    forever proc = do proc; forever proc

    doEcho'' : Process IO (echo') 'Client [] [STDIO] ()
    doEcho'' = do
       server <- spawn (echoProcessServer') [()]
       setChan 'Server server
       echoProcessClient' server
       dropChan 'Server

-- --------------------------------------------------------------------- [ EOF ]
