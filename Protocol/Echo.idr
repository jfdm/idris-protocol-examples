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

||| One echo service is defined as a connection based application on TCP.
||| 
||| A server listens for TCP connections on TCP port 7. Once a
||| connection is established any data received is sent back. This
||| continues until the calling user terminates the connection.
echoProtocol : Protocol ['Client, 'Server] ()
echoProtocol = do
    msg <- 'Client ==> 'Server | String
    'Server ==> 'Client | (resp ** so (resp == msg))
    Done
    
echoProtocol' : Protocol ['Client, 'Server] ()
echoProtocol' = do
    'Client ==> 'Server | String
    'Server ==> 'Client | String
    Done
        
-- --------------------------------------------------------------------- [ EOF ]
