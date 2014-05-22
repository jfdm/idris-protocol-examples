-- ----------------------------------------------------------------- [ IPC.idr ]
--
-- Implementation of the echo protocols for Interprocess Communication.
--
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.Echo.Net

import Effects
import Effect.StdIO
import Effect.Default

import System.Protocol

import Protocol.Echo


-- ---------------------------------------------------- [ Marshalling Commands ]

using (m : String)
  instance Netvalue (resp : String ** (resp = m)) where
    toNetString (resp ** _) = show resp ++ "\n"

    fromNetString {m} str with (decEq str m)
                  | Yes eq = Just (str ** eq)
                  | No  _  = Nothing

instance Netvalue (Maybe String) where
  toNetString (Just str) = show str ++ "\n"
  toNetString Nothing    = show "" ++ "\n"

  fromNetString str = case (str /= "") of
    True  => Just (Just str)
    False => Nothing



-- --------------------------------------------------------------------- [ EOF ]
