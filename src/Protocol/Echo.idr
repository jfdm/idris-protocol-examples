-- ---------------------------------------------------------------- [ Echo.idr ]
--
-- This RFC specifies a standard for the ARPA Internet community.  Hosts on
-- the ARPA Internet that choose to implement an Echo Protocol are expected
-- to adopt and implement this standard.
--
-- A very useful debugging and measurement tool is an echo service.  An
-- echo service simply sends back to the originating source any data it
-- receives.
--
-- http://tools.ietf.org/html/rfc862
--
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.Echo

import Effects

import Effect.Default
import Effect.StdIO

import System.Protocol

import Protocol.Utils

||| An echo service simply sends back to the originating source any
||| data it receives.
|||
||| This is an enhanced version of the protocol in which an invariant
||| is provide that stipulates that the message echoed back is the
||| original message sent.
total
echo : Protocol ['Client, 'Server] ()
echo = do
    msg <- 'Client ==> 'Server | Maybe String
    case msg of
      Just m  => do
        'Server ==> 'Client | Literal m
        Rec echo
      Nothing => Done

||| An echo service simply sends back to the originating source any
||| data it receives.
|||
||| A naive version of the protocol. This version does not provide
||| guarantees over the return message nor provides the recursive step.
total
echo' : Protocol ['Client, 'Server] ()
echo' = do
    'Client ==> 'Server | String
    'Server ==> 'Client | String
    Done

-- --------------------------------------------------------------------- [ EOF ]
