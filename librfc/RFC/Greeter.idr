module RFC.Greeter

import Effects
import Effect.StdIO
import Effect.Default
import Effect.State

import System.Protocol

%access public

-- ------------------------------------------------------------------- [ Model ]

-- Internal representations
data Command = GreetMe String | Help | Cheat | Quit

-- How we display the internal representation for Command.
instance Show Command where
    show (GreetMe str) = ":greet " ++ show str
    show Quit = ":q"
    show Help = ":?"
    show Cheat = ":cheat"

-- -------------------------------------------------- [ Protocol Specification ]

GreeterProto : Type -> Type
GreeterProto t = Protocol ['Alice, 'Bob] ()

total
gBody : GreeterProto ()
gBody = do
    'Bob ==> 'Alice | String
    cmd <- 'Alice ==> 'Bob | Command
    case cmd of
      GreetMe str => do
        'Bob ==> 'Alice | String
        Rec gBody

      Help => do
        'Bob ==> 'Alice | String
        Rec gBody

      Cheat => do
        Rec gBody

      Quit => Done

total
greeter : GreeterProto () -> GreeterProto ()
greeter body = do
    'Alice ==> 'Bob | String
    body
    Done

-- --------------------------------------------------------------------- [ EOF ]
