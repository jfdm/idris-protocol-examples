module RFC.TCPHandshake

import Effects

import Effect.Default
import Effect.StdIO

import System.Protocol

import RFC.Utils

total
handshake' : Protocol ['Alice, 'Bob] ()
handshake' = do
  'Alice ==> 'Bob   | (TCPMsg, Nat)
  'Bob   ==> 'Alice | (TCPMsg, Nat, Nat)
  'Alice ==> 'Bob   | (TCPMsg, Nat)
  Done

total
handshake : Protocol ['Alice, 'Bob] ()
handshake = do
  (_, x) <- 'Alice ==> 'Bob | (TCPMsg, Nat)
  (_, y, _) <- 'Bob ==> 'Alice | (TCPMsg, Nat, (x' ** x' = S x))
  'Alice ==> 'Bob | (TCPMsg, (y' ** y' = S y))
  Done
