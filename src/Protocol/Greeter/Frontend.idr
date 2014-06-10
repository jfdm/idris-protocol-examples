-- ------------------------------------------------------------------------ [  ]
-- Client Process
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.Greeter.Frontend

import Effects
import Effect.StdIO
import Effect.Default
import Effect.State

import System.Protocol

import Protocol.Greeter
import Protocol.Greeter.Common

%access public

-- ---------------------------------------------------------- [ Greeter Client ]

||| Get commands from the user.
private
readCmd : { [STDIO] } Eff IO Command
readCmd = case (process !getStr) of
    Just cmd => return cmd
    Nothing => do
      putStrLn "Bad command"
      putStr "Try again: "
      readCmd
  where
    process : String -> Maybe Command
    process str = case words (trim str) of
      [":greet", str] => Just (GreetMe str)
      [":?"]          => Just Help
      [":help"]       => Just Help
      [":q"]          => Just Quit
      [":cheat"]      => Just Cheat
      _ => Nothing


||| Process commands and interact with the backend.
private covering
clientBody : (proc : PID) -> GreeterFrontend (gBody) ()
clientBody proc = do
     setChan 'Bob proc
     putStrLn !(recvFrom 'Bob)
     cmd <- readCmd
     sendTo 'Bob cmd
     case cmd of
       GreetMe str => do
         putStrLn !(recvFrom 'Bob)
         dropChan 'Bob
         rec (clientBody proc)

       Cheat => do
         dropChan 'Bob
         rec (clientBody proc)

       Help => do
         putStrLn !(recvFrom 'Bob)
         dropChan 'Bob
         rec (clientBody proc)

       Quit => do
         dropChan 'Bob
         return ()

||| A Client process to perform the client greeter interactions.
covering
greeterClient : (proc : PID) -> GreeterFrontend (greeter gBody) ()
greeterClient proc = do
    setChan 'Bob proc
    sendTo 'Bob "I am a nasty hack"
    dropChan 'Bob
    (clientBody proc)
    return ()

-- --------------------------------------------------------------------- [ EOF ]
