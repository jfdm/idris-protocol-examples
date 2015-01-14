-- ---------------------------------------------------------- [ KnockKnock.idr ]
--
-- Implementation of the knock protocol for process communication.
--
-- --------------------------------------------------------------------- [ EOH ]
module KnocKnockArgs

import Effects
import Effect.StdIO
import Effect.Default
import Effect.Msg

import System
import System.Protocol

import RFC.Knock

-- ---------------------------------------------------------- [ Server Process ]
covering
knockee : (client : PID)
            -> Process (knockknock) 'Server ['Client := client] [STDIO] ()
knockee client = do
    (kk ** _ ) <- recvFrom 'Client
    putStrLn $ "From Knocker: " ++ kk

    sendTo 'Client ("Who's there?" ** Refl)

    res <- recvFrom 'Client
    putStrLn $ "From Knocker: " ++ res

    sendTo 'Client ((res ++ " who?") ** Refl)

    putStrLn $ "From Knocker: " ++ !(recvFrom 'Client)

-- ---------------------------------------------------------- [ Client Process ]
covering
knocker : (server : PID)
            -> Process (knockknock) 'Client ['Server := server] [STDIO] ()
knocker server = do
    sendTo 'Server ("Knock Knock" ** Refl)

    (res ** _) <- recvFrom 'Server
    putStrLn $ "From Knockee: " ++ res

    putStr "Enter the msg: "
    setup <- getStr
    sendTo 'Server $ trim setup

    (res' ** _) <- recvFrom 'Server
    putStrLn $ "From Knockee: " ++ res'

    putStr "Enter the res: "
    reveal <- getStr
    sendTo 'Server $ trim reveal

-- ------------------------------------------------------ [ Sample Innvocation ]

doKnockKnock : IO ()
doKnockKnock = runConc [()] doKnock
  where
    doKnock : Process (knockknock) 'Client [] [STDIO] ()
    doKnock = do
       server <- spawn (knockee) [()]
       setChan 'Server server
       knocker server
       dropChan 'Server

-- -------------------------------------------------------------------- [ Main ]
namespace Main
  main : IO ()
  main = doKnockKnock

-- --------------------------------------------------------------------- [ EOF ]
