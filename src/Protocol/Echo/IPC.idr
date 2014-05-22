-- ----------------------------------------------------------------- [ IPC.idr ]
--
-- Implementation of the echo protocols for Interprocess Communication.
--
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.Echo.IPC

import Effects
import Effect.StdIO
import Effect.Default

import System.Protocol

import Protocol.Echo

-- ---------------------------------------------------- [ Marshalling Commands ]

using (m : String)
  instance IPCValue (resp : String ** (resp = m)) where
    toIPCString (resp ** _) = show resp ++ "\n"

    fromIPCString {m} str with (decEq str m)
                  | Yes eq = Just (str ** eq)
                  | No  _  = Nothing

instance IPCValue (Maybe String) where
  toIPCString (Just str) = show str ++ "\n"
  toIPCString Nothing    = show "" ++ "\n"

  fromIPCString str = case (str /= "") of
    True  => Just (Just str)
    False => Nothing
-- ---------------------------------------------------------- [ Server Process ]

||| Implementation of the Echo Protocol from the Server's perspective.
|||
||| @proc The process we are interacting with.
-- covering
echoIPCServer : (proc : File)
              -> IPC (echo) 'Server ['Client := proc] [STDIO] ()
echoIPCServer proc = do
    msg <- recvFrom 'Client
    case msg of
      Just m => do
        sendTo 'Client (m ** refl)
        rec (echoIPCServer proc)

      Nothing => return ()

||| Implementation of the Echo Protocol from the Client's perspective.
|||
||| @proc The process we are interacting with.
-- covering
echoIPCClient : (proc : File)
              -> IPC (echo) 'Client ['Server := proc] [STDIO] ()
echoIPCClient proc = do
    putStrLn "Enter some text ('q' to quit):"
    msg_raw <- getStr
    case (processMsg (trim msg_raw)) of

      Just m => do
        sendTo 'Server (Just m)
        (resp ** _ ) <- recvFrom 'Server
        putStrLn $ show resp
        rec (echoIPCClient proc)

      Nothing => do
        sendTo 'Server Nothing
        return ()

  where
    processMsg : String -> Maybe String
    processMsg msg = case msg == "q" of
                       True => Nothing
                       False => Just msg

doEchoIPC : IO ()
doEchoIPC = do
    proc <- popen "echo" ReadWrite
    ioe_run (runInit [Proto, ()] (echoIPCClient proc))
      (\err => putStrLn $ "It went wrong " ++ err)
      (\ok => putStrLn "Success")
    pclose proc


-- --------------------------------------------------------------------- [ EOF ]
