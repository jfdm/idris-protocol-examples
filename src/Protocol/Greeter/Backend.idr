module Protocol.Greeter.Backend

import Effects
import Effect.StdIO
import Effect.Default
import Effect.State

import System.Protocol

import Protocol.Greeter

%access public

GreeterBackendES : List EFFECT
GreeterBackendES = [STDIO, STATE Int]

GreeterBackEndProcess : GreeterProto () -> Type -> Type
GreeterBackEndProcess p t = Process IO p 'Bob [] GreeterBackendES t

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
  backendBody : PID
              -> GreeterBackEndProcess gBody ()
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

        Cheat => do
          dropChan 'Alice
          rec (backendBody' proc)

        Quit => do
          dropChan 'Alice
          return ()

  covering
  backendBody' : PID
              -> GreeterBackEndProcess gBody ()
  backendBody' proc = do
      setChan 'Alice proc
      sendTo 'Alice (constructGreeting !get)
      resp <- recvFrom 'Alice

      case resp of
        GreetMe str => do
          c <- get
          let resp = if c > 0
                       then ("Hello: " ++ str)
                       else "Sorry ran out of greetings."
          update (\x => x - 1)
          sendTo 'Alice resp
          dropChan 'Alice
          rec (backendBody' proc)

        Help => do
          sendTo 'Alice obtainHelp
          dropChan 'Alice
          rec (backendBody' proc)

        Cheat => do
          dropChan 'Alice
          rec (backendBody proc)

        Quit => do
          dropChan 'Alice
          return ()

covering
greeterBackend : PID
               -> (PID -> GreeterBackEndProcess gBody ())
               -> GreeterBackEndProcess (greeter gBody) ()
greeterBackend proc body = do
    setChan 'Alice proc
    foo <- recvFrom 'Alice
    dropChan 'Alice
    (body proc)
    return ()

-- --------------------------------------------------------------------- [ EOF ]
