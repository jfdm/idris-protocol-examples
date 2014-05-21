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

instance IPCValue (resp : String ** resp = m) where
  toIPCString (resp ** _) = show resp ++ "\n"
  fromIPCString {m} str = case choose (str == m) of
    (Left  p) => Just (str ** p)
    (Right p) => Nothing
    
instance IPCValue (resp : String ** so (resp == m)) where
  toIPCString (resp ** _) = show resp ++ "\n"
  fromIPCString {m} str = case choose (str == m) of
    (Left  p) => Just (str ** p)
    (Right p) => Nothing

instance IPCValue (Maybe String) where
  toIPCString (Just str) = show str ++ "\n"
  toIPCString Nothing    = show "" ++ "\n"
  
  fromIPCString str = case (str /= "") of
    True  => Just (Just str)
    False => Nothing
-- ---------------------------------------------------------- [ Server Process ]

covering
echoIPCServer : (proc : File)
              -> IPC (echo) 'Server ['Client := proc] [STDIO] ()
echoIPCServer proc = do
    msg <- recvFrom 'Client
    case msg of
      Just m => do
        sendTo 'Client (m ** m = m)
        rec (echoIPCServer proc)
        
      Nothing => return ()
              
              



-- --------------------------------------------------------------------- [ EOF ]
