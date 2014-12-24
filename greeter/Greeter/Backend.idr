module Greeter.Backend

import Effects
import Effect.StdIO
import Effect.Default
import Effect.State

import System.Protocol

import RFC.Greeter
import Greeter.Common

%access public

-- --------------------------------------------------------- [ Greeter Backend ]

private
obtainHelp : String
obtainHelp = unwords ["List 'o Commands\n" ++
                      "\t :greet <name>\n" ++
                      "\t :q\n" ++
                      "\t :cheat\n" ++
                      "\t :help,:?\n"]

private
constructGreeting : Int -> String
constructGreeting i = "Enter Command (" ++ func i ++ ", :? for help) :"
  where
    func : Int -> String
    func b = case b > 0 of
               True  => show i ++ " greetings remaining"
               False =>  "no more greetings"

mutual
  covering
  backendBody : PID -> GreeterBackend gBody ()
  backendBody proc = do
      setChan 'Alice proc
      sendTo 'Alice "Enter Command:"
      resp <- recvFrom 'Alice
      case resp of

        GreetMe str => do
          sendTo 'Alice ("Hello: " ++ str)
          dropChan 'Alice
          rec (backendBody proc)

        Help => do
          sendTo 'Alice obtainHelp
          dropChan 'Alice
          rec (backendBody proc)

        Quit => do
          dropChan 'Alice
          return ()

  covering
  backendBody' : PID -> GreeterBackend gBody ()
  backendBody' proc = do
      setChan 'Alice proc
      sendTo 'Alice (constructGreeting !get)

      case !(recvFrom 'Alice) of
        GreetMe str => do

          let resp = if !get > 0
                       then ("Hello: " ++ str)
                       else "Sorry ran out of greetings."
          update (\x => x - 1)
          sendTo 'Alice resp
          dropChan 'Alice
          rec (backendBody' proc)

        Help => do
          sendTo 'Alice obtainHelp
          dropChan 'Alice
          rec (backendBody proc)

        Quit => do
          dropChan 'Alice
          return ()

covering
greeterBackend : PID
               -> (PID -> GreeterBackend gBody ())
               -> GreeterBackend (greeter gBody) ()
greeterBackend proc body = do
    setChan 'Alice proc
    foo <- recvFrom 'Alice
    dropChan 'Alice
    (body proc)
    return ()

-- --------------------------------------------------------------------- [ EOF ]
