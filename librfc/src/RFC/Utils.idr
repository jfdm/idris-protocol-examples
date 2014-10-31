-- --------------------------------------------------------------- [ Utils.idr ]
--
-- Utilities
--
-- --------------------------------------------------------------------- [ EOH ]
module RFC.Utils

import System

%access public

||| Representation of String literals that are to be sent.
Literal : String -> Type
Literal str = (str' : String ** str' = str)


-- ----------------------------------------------------------------- [ DayTime ]
||| Simple Record for DayTime
record DayTime : Type where
  MkDaytime : (y : Int)
            -> (m : Int)
            -> (d : Int)
            -> (hh : Int)
            -> (mm : Int)
            -> (ss : Int)
            -> DayTime

instance Show DayTime where
    show date = unwords [show (y  date), "-",
                         show (m  date), "-",
                         show (d  date), " ",
                         show (hh date), ":",
                         show (mm date), ":",
                         show (ss date) ]

-- --------------------------------------------------------------------- [ EOF ]
