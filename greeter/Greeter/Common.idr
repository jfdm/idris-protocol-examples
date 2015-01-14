-- ------------------------------------------------------------------------ [  ]
-- Common things
-- --------------------------------------------------------------------- [ EOH ]
module Greeter.Common

import Effects
import Effect.StdIO
import Effect.Default
import Effect.State
import Effect.Msg

import System.Protocol

import RFC.Greeter

%access public

GreeterES : List EFFECT
GreeterES = [STDIO, STATE Int]

GreeterFrontend : GreeterProto () -> Type -> Type
GreeterFrontend p t = Process p 'Alice [] GreeterES t

GreeterBackend : GreeterProto () -> Type -> Type
GreeterBackend p t = Process p 'Bob [] GreeterES t


-- --------------------------------------------------------------------- [ EOF ]
