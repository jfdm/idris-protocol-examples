module RFC.Knock

import Effects
import Effect.StdIO
import Effect.Default
import Effect.State

import System.Protocol
import RFC.Utils

%access public

knock : String -> String -> Protocol ['Client, 'Server] ()
knock req res = do
  'Client ==> 'Server | Literal "Knock Knock"
  'Server ==> 'Client | Literal "Who's there?"
  req_act <- 'Client ==> 'Server | Literal req
  case (req_act) of
    (x ** _) => do
      'Server ==> 'Client | Literal (x ++ " who?")
      'Client ==> 'Server | Literal (x ++ res)
      Done

knock' : String -> String -> Protocol ['Client, 'Server] ()
knock' req res = do
  'Client ==> 'Server | Literal "Knock Knock"
  'Server ==> 'Client | Literal "Who's there?"
  'Client ==> 'Server | Literal req
  'Server ==> 'Client | Literal (req ++ " who?")
  'Client ==> 'Server | Literal (req ++ res)
  Done

knock'' : Protocol ['Client, 'Server] ()
knock'' = do
  'Client ==> 'Server | Literal "Knock Knock"
  'Server ==> 'Client | Literal "Who's there?"
  res <- 'Client ==> 'Server | String
  'Server ==> 'Client | Literal (res ++ " who?")
  'Client ==> 'Server | String
  Done

-- --------------------------------------------------------------------- [ EOF ]
