-- ------------------------------------------------------------------------ [  ]
-- Example innvocations of greeters
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.Greeter.Example

import Effects
import Effect.StdIO
import Effect.Default
import Effect.State

import System.Protocol

import Protocol.Greeter
import Protocol.Greeter.Common
import Protocol.Greeter.Frontend
import Protocol.Greeter.Backend


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
