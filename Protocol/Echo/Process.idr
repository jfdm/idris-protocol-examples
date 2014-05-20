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

echoServer : (client : PID)
           -> Process IO (echoProtocol') 'Server ['Client := client] [STDIO] ()
echoServer client = do
    msg <- recvFrom 'Client
    sendTo 'Client msg
    return ()

echoClient : (server : PID)
           -> Process IO (echoProtocol') 'Client ['Server := server] [STDIO] ()
echoClient server = do
    putStrLn "Enter some text:"
    msg <- getStr
    sendTo 'Server msg
    resp <- recvFrom 'Server
    putStrLn $ show (trim resp)
    return ()
    
doEcho : IO ()
doEcho = forever $ runConc [()] doEcho'
  where
    forever : IO () -> IO ()
    forever proc = do proc; forever proc

    doEcho' : Process IO (echoProtocol') 'Client [] [STDIO] ()
    doEcho' = do
       server <- spawn (echoServer) [()]
       setChan 'Server server
       echoClient server
       dropChan 'Server


-- --------------------------------------------------------------------- [ EOF ]
