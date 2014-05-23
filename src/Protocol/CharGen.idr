-- ------------------------------------------------------------- [ Chargen.idr ]
--
-- This RFC specifies a standard for the ARPA Internet community.
-- Hosts on the ARPA Internet that choose to implement a Character
-- Generator Protocol are expected to adopt and implement this
-- standard.

-- A useful debugging and measurement tool is a character generator
-- service. A character generator service simply sends data without
-- regard to the input.
--
-- The data may be anything.  It is recommended that a recognizable
-- pattern be used in tha data.
--
-- One popular pattern is 72 chraracter lines of the ASCII printing
-- characters.  There are 95 printing characters in the ASCII
-- character set.  Sort the characters into an ordered sequence and
-- number the characters from 0 through 94.  Think of the sequence as
-- a ring so that character number 0 follows character number 94.  On
-- the first line (line 0) put the characters numbered 0 through
-- 71. On the next line (line 1) put the characters numbered 1 through
-- 72.  And so on.  On line N, put characters (0+N mod 95) through
-- (71+N mod 95).  End each line with carriage return and line feed.
--
-- http://tools.ietf.org/html/rfc864
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.CharGen

import Effects

import Effect.Default
import Effect.StdIO

import System.Protocol

||| A useful debugging and measurement tool is a character generator
||| service. A character generator service simply sends data without
||| regard to the input. The data may be anything.  It is recommended
||| that a recognizable pattern be used in tha data.
total
chargen : Protocol ['Client, 'Server] ()
chargen = do
    msg <- 'Client ==> 'Server | Maybe String
    case msg of
      Just m => do
        'Server ==> 'Client | String
        Rec chargen
      Nothing => Done

-- --------------------------------------------------------------------- [ EOF ]
