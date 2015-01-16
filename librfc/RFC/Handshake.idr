module RFC.Handshake

import Effects
import Effect.Default
import Effect.StdIO
import Effect.Msg

import System.Protocol

import RFC.Utils

data TCPMsg = Syn | SynAck | Ack

instance Default TCPMsg where
  default = Syn

data MsgTy = SYN | SYNACK | ACK

data TCPMsg' : MsgTy -> Type where
  Syn' : TCPMsg' SYN
  Ack' : TCPMsg' ACK
  Synack' :  TCPMsg' SYNACK

instance Default (TCPMsg' SYN) where
  default = Syn'


total
handshake' : Protocol ['Alice, 'Bob] ()
handshake' = do
  'Alice ==> 'Bob   | (TCPMsg, Nat)
  'Bob   ==> 'Alice | (TCPMsg, Nat, Nat)
  'Alice ==> 'Bob   | (TCPMsg, Nat)
  Done

total
handshake'' : Protocol ['Alice, 'Bob] ()
handshake'' = do
  (_, x) <- 'Alice ==> 'Bob | (TCPMsg' SYN, Nat)
  (_, y, _) <- 'Bob ==> 'Alice | (TCPMsg' SYNACK, Nat, (x' ** x' = S x))
  'Alice ==> 'Bob | (TCPMsg' ACK, (y' ** y' = S y))
  Done

total
handshake : Protocol ['Alice, 'Bob] ()
handshake = do
  (_, x) <- 'Alice ==> 'Bob | (TCPMsg, Nat)
  (_, y, _) <- 'Bob ==> 'Alice | (TCPMsg, Nat, (x' ** x' = S x))
  'Alice ==> 'Bob | (TCPMsg, (y' ** y' = S y))
  Done
