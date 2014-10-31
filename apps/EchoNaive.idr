-- ------------------------------------------------------------- [ Process.idr ]
--
-- Implementation of the echo protocol for process communication.
--
-- --------------------------------------------------------------------- [ EOH ]
module EchoNaive

import Effects
import Effect.StdIO
import Effect.Default

import System.Protocol

import RFC.Echo

-- ---------------------------------------------------- [ Naive Echo Processes ]

||| Implementation of the Echo Protocol from the Server end of the
||| Protocol.
|||
||| @client The PID of the client process.
covering
echoProcessServer' : (client : PID)
           -> Process (echo') 'Server ['Client := client] [STDIO] ()
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
           -> Process (echo') 'Client ['Server := server] [STDIO] ()
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

    doEcho'' : Process (echo') 'Client [] [STDIO] ()
    doEcho'' = do
       server <- spawn (echoProcessServer') [()]
       setChan 'Server server
       echoProcessClient' server
       dropChan 'Server

-- --------------------------------------------------------------------- [ EOF ]
