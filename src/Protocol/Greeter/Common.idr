-- ------------------------------------------------------------------------ [  ]
-- Common things
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.Greeter.Common

import Effects
import Effect.StdIO
import Effect.Default
import Effect.State

import System.Protocol

import Protocol.Greeter

%access public

GreeterES : List EFFECT
GreeterES = [STDIO, STATE Int]

GreeterFrontend : GreeterProto () -> Type -> Type
GreeterFrontend p t = Process IO p 'Alice [] GreeterES t

GreeterBackend : GreeterProto () -> Type -> Type
GreeterBackend p t = Process IO p 'Bob [] GreeterES t


-- --------------------------------------------------------------------- [ EOF ]
