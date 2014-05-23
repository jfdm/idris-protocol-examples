-- ------------------------------------------------------------- [ Daytime.idr ]
--
-- This RFC specifies a standard for the ARPA Internet community.
-- Hosts on the ARPA Internet that choose to implement a Daytime
-- Protocol are expected to adopt and implement this standard.
--
-- A useful debugging and measurement tool is a daytime service. A
-- daytime service simply sends a the current date and time as a
-- character string without regard to the input.
--
-- There is no specific syntax for the daytime.  It is recommended
-- that it be limited to the ASCII printing characters, space,
-- carriage return, and line feed.  The daytime should be just one
-- line. One popular syntax is:
--
-- ```
-- Weekday, Month Day, Year Time-Zone
-- ```
--
-- Example:
---
--- ```
--- Tuesday, February 22, 1982 17:37:43-PST
--- ```
--
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.Daytime

import Effects

import Effect.Default
import Effect.StdIO

import System.Protocol

import Protocol.Daytime.Utils

||| A daytime service simply sends a the current date and time as a
||| character string without regard to the input.
daytime : Protocol ['Client, 'Server] ()
daytime = do
    'Client ==> 'Server | Maybe String
    'Server ==> 'Client | Either String DayTime
    Done


-- --------------------------------------------------------------------- [ EOF ]
