module RFC.Knock

import Effects
import Effect.StdIO
import Effect.Default
import Effect.State

import System.Protocol
import RFC.Utils

%access public

knockknock : Protocol ['Client, 'Server] ()
knockknock = do
  'Client ==> 'Server | Literal "Knock Knock"
  'Server ==> 'Client | Literal "Who's there?"
  res <- 'Client ==> 'Server | String
  'Server ==> 'Client | Literal (res ++ " who?")
  'Client ==> 'Server | String
  Done

-- --------------------------------------------------------------------- [ EOF ]
