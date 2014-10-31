-- ------------------------------------------------------------------------ [  ]
-- Example innvocations of greeters
-- --------------------------------------------------------------------- [ EOH ]
module Greeter.Example

import Effects
import Effect.StdIO
import Effect.Default
import Effect.State

import System.Protocol

import RFC.Greeter
import Greeter.Common
import Greeter.Frontend
import Greeter.Backend


%access public
-- ------------------------------------------------------------------------- [  ]

||| A Greeter Backend.
covering
exampleGreeterBackend : PID -> GreeterBackend (greeter gBody) ()
exampleGreeterBackend proc = greeterBackend proc (backendBody')

||| Run the Greeter Application.
|||
||| @max The max number of greets to be given.
doGreeterProcess : (max : Int) -> IO ()
doGreeterProcess max = runConc [(),0] (doGreet' max)
  where
    doGreet' : Int -> GreeterFrontend (greeter gBody) ()
    doGreet' max = do
      server <- spawn (exampleGreeterBackend) [(), max]
      greeterClient server

-- --------------------------------------------------------------------- [ EOF ]
