module Protocol.Greeter.Frontend

import Effects
import Effect.StdIO
import Effect.Default
import Effect.State

import System.Protocol

import Protocol.Greeter

%access public

-- ---------------------------------------------------------- [ Greeter Client ]

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


private covering
clientBody : (proc : PID)
           -> Process IO (gBody) 'Alice ['Bob := proc] [STDIO] ()
clientBody proc = do
     putStrLn !(recvFrom 'Bob)
     cmd <- readCmd
     sendTo 'Bob cmd
     case cmd of
       GreetMe str => do
         putStrLn !(recvFrom 'Bob)
         rec (clientBody proc)

       Cheat => do
         rec (clientBody proc)

       Help => do
         putStrLn !(recvFrom 'Bob)
         rec (clientBody proc)

       Quit => return ()

covering
greeterClient : (proc : PID)
              -> Process IO (greeter gBody) 'Alice ['Bob := proc] [STDIO] ()
greeterClient proc = do
    sendTo 'Bob "I am a nasty hack"
    (clientBody proc)
    return ()

-- --------------------------------------------------------------------- [ EOF ]
