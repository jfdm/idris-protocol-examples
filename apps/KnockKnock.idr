-- ---------------------------------------------------------- [ KnockKnock.idr ]
--
-- Implementation of the knock protocol for process communication.
--
-- --------------------------------------------------------------------- [ EOH ]
module KnockKnock

import Effects
import Effect.StdIO
import Effect.Default
import Effect.Msg

import System
import System.Protocol

import RFC.Knock

-- ---------------------------------------------------------- [ Server Process ]
covering
knockServer : (client : PID)
            -> Process (knockknock) 'Server ['Client := client] [STDIO] ()
knockServer client = do
    (kk ** _ ) <- recvFrom 'Client
    putStrLn $ "From Teller: " ++ kk

    sendTo 'Client ("Who's there?" ** Refl)

    res <- recvFrom 'Client
    putStrLn $ "From Teller: " ++ res

    sendTo 'Client ((res ++ " who?") ** Refl)

    putStrLn $ "From Teller: " ++ !(recvFrom 'Client)

-- ---------------------------------------------------------- [ Client Process ]
covering
knockClient : String -> String
            -> (server : PID) -> Process (knockknock) 'Client ['Server := server] [STDIO] ()
knockClient setup reveal svr = do
    sendTo 'Server ("Knock Knock" ** Refl)

    (res ** _) <- recvFrom 'Server
    putStrLn $ "From Mark: " ++ res

    sendTo 'Server setup

    (res' ** _) <- recvFrom 'Server
    putStrLn $ "From Mark: " ++ res'

    sendTo 'Server reveal

-- ------------------------------------------------------ [ Sample Innvocation ]

||| Sample Innvocation of the Echo protocols between the client and
||| server functions.
covering
doKnockKnock : String -> String -> IO ()
doKnockKnock setup reveal = runConc [()] (doKnock setup reveal)
  where
    doKnock : String -> String -> Process (knockknock) 'Client [] [STDIO] ()
    doKnock s r = do
       server <- spawn (knockServer) [()]
       setChan 'Server server
       knockClient s r server
       dropChan 'Server
       pure ()


processArgs : (List String) -> Maybe (String, String)
processArgs [x] = Nothing
processArgs (x::y::z) = Just (y, unwords z)

-- -------------------------------------------------------------------- [ Main ]
usage : String
usage = unwords ["Usage: ./knockknock <setup> <reveal>"]

namespace Main
  main : IO ()
  main = do
    args <- getArgs
    case processArgs args of
      Just (x,y) => doKnockKnock x y
      Nothing => putStrLn usage


-- --------------------------------------------------------------------- [ EOF ]
