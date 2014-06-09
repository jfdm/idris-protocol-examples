module Protocol.Greeter.Example

import Effects
import Effect.StdIO
import Effect.Default
import Effect.State

import System.Protocol

import Protocol.Greeter
import Protocol.Greeter.Frontend
import Protocol.Greeter.Backend


%access public

covering
exampleGreeterBackend : PID -> GreeterBackEndProcess (greeter gBody) ()
--Process IO (greeter gBody) 'Bob [] GreeterBackendES ()
exampleGreeterBackend proc = greeterBackend proc (backendBody')

-- --------------------------------------------------------------- [ doGreeter ]
doGreeterProcess : Int -> IO ()
doGreeterProcess max = runConc [(),0] (doGreet' max)
  where
    doGreet' : Int -> Process IO (greeter gBody) 'Alice [] [STDIO, STATE Int] ()
    doGreet' max = do
      server <- spawn (exampleGreeterBackend) [(), max]
      setChan 'Bob server
      greeterClient server
      dropChan 'Bob
