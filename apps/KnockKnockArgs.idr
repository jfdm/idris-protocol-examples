-- ---------------------------------------------------------- [ KnockKnock.idr ]
--
-- Implementation of the knock protocol for process communication.
--
-- --------------------------------------------------------------------- [ EOH ]
module KnocKnockArgs

import Effects
import Effect.StdIO
import Effect.Default

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
knockClient : (server : PID)
            -> Process (knockknock) 'Client ['Server := server] [STDIO] ()
knockClient server = do
    sendTo 'Server ("Knock Knock" ** Refl)

    (res ** _) <- recvFrom 'Server
    putStrLn $ "From Mark: " ++ res

    putStr "Enter the setup: "
    setup <- getStr
    sendTo 'Server $ trim setup

    (res' ** _) <- recvFrom 'Server
    putStrLn $ "From Mark: " ++ res'

    putStr "Enter the reveal: "
    reveal <- getStr
    sendTo 'Server $ trim reveal

-- ------------------------------------------------------ [ Sample Innvocation ]

doKnockKnock : IO ()
doKnockKnock = runConc [()] doKnock
  where
    doKnock : Process (knockknock) 'Client [] [STDIO] ()
    doKnock = do
       server <- spawn (knockServer) [()]
       setChan 'Server server
       knockClient server
       dropChan 'Server

-- -------------------------------------------------------------------- [ Main ]
namespace Main
  main : IO ()
  main = doKnockKnock

-- --------------------------------------------------------------------- [ EOF ]
