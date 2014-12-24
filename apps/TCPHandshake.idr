-- ------------------------------------------------------------- [ Process.idr ]
--
-- Implementation of the echo protocol for process communication.
--
-- --------------------------------------------------------------------- [ EOH ]
module TCPHandshake

import Effects
import Effect.StdIO
import Effect.Default

import System.Protocol

import RFC.Handshake

covering
bob : Nat -> (client : PID)
      -> Process (handshake) 'Bob ['Alice := client] [STDIO] ()
bob seqno client = do
    (msg, x) <- recvFrom 'Alice
    let x' = S x
    putStrLn $ "Bob -> Alice: SynAck, " ++ show seqno ++ ", " ++ show x'
    sendTo 'Alice (SynAck, seqno, (x'** Refl))
    (msg1, no') <- recvFrom 'Alice
    putStrLn $ "Received from Alice: " ++ show (getWitness no')
    pure ()

covering
alice : Nat -> (server : PID)
      -> Process (handshake) 'Alice ['Bob := server] [STDIO] ()
alice seqno server = do
    putStrLn $ "Alice -> Bob: Syn, " ++ show seqno
    sendTo 'Bob (Syn, seqno)
    (msg, y, x) <- recvFrom 'Bob
    let y' = S y
    putStrLn $ "Alice -> Bob: Ack " ++ show y'
    sendTo 'Bob (Ack, (y' ** Refl))
    pure ()

covering
doHandshake : IO ()
doHandshake = runConc [()] doShake
  where
    doShake : Process (handshake) 'Alice [] [STDIO] ()
    doShake = do
       server <- spawn (bob 2) [()]
       setChan 'Bob server
       alice 100 server
       dropChan 'Bob
       pure ()

-- -------------------------------------------------------------------- [ Main ]
namespace Main
  main : IO ()
  main = doHandshake

-- --------------------------------------------------------------------- [ EOF ]
