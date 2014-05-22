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

import Protocol.Chargen

text : String
text = pack $ map (\x => chr x) [33..125]

bump : String -> String
bump s = reverse (strCons h (reverse t))
  where
    h : Char
    h = strHead s
    t : String
    t = strTail s

-- ---------------------------------------------------------- [ Server Process ]

||| Implementation of the Echo Protocol from the Server's perspective.
|||
||| @client The PID of the client process.
-- covering
chargenProcessServer : (str : String)
                  -> (client : PID)
                  -> Process IO (chargen) 'Server ['Client := client] [STDIO] ()
chargenProcessServer str client = do
    msg <- recvFrom 'Client
    case msg of
      Just m => do
        sendTo 'Client str
        rec (chargenProcessServer (bump str) client)

      Nothing => return ()

-- ---------------------------------------------------------- [ Client Process ]

||| Implementation of the Echo Protocol from the Client's perspective.
|||
||| @server The PID of the server process.
-- covering
chargenProcessClient : (server : PID)
                   -> Process IO (chargen) 'Client ['Server := server] [STDIO] ()
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
    doEcho' : Process IO (chargen) 'Client [] [STDIO] ()
    doEcho' = do
       server <- spawn (chargenProcessServer text) [()]
       setChan 'Server server
       chargenProcessClient server
       dropChan 'Server

-- --------------------------------------------------------------------- [ EOF ]
