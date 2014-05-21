-- ---------------------------------------------------------------- [ Time.idr ]
--
-- This RFC specifies a standard for the ARPA Internet community.
-- Hosts on the ARPA Internet that choose to implement a Time Protocol
-- are expected to adopt and implement this standard.
--
-- This protocol provides a site-independent, machine readable date
-- and time.  The Time service sends back to the originating source
-- the time in seconds since midnight on January first 1900.
--
-- One motivation arises from the fact that not all systems have a
-- date/time clock, and all are subject to occasional human or machine
-- error.  The use of time-servers makes it possible to quickly
-- confirm or correct a system's idea of the time, by making a brief
-- poll of several independent sites on the network.
--
-- http://tools.ietf.org/html/rfc868
--
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.Time

import Effects

import Effect.Default
import Effect.StdIO

import System.Protocol


||| This protocol provides a site-independent, machine readable date
||| and time.  The Time service sends back to the originating source
||| the time in seconds since midnight on January first 1900.
|||
||| Enhanced version of the protocol.
time : Protocol ['Client, 'Server] ()
time = do
    'Server ==> 'Client | Either String Int
    Done

||| This protocol provides a site-independent, machine readable date
||| and time.  The Time service sends back to the originating source
||| the time in seconds since midnight on January first 1900.
|||
||| A naive version of the protocol. 
time' : Protocol ['Client, 'Server] ()
time' = do
    'Server ==> 'Client | Int
    Done

-- --------------------------------------------------------------------- [ EOF ]
