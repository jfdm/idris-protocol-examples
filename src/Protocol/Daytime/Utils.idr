-- --------------------------------------------------------------- [ Utils.idr ]
--
-- Utility functions
--
-- Given the absence of a true daytime module for Idris we simulate
-- the utility here.
--
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.Daytime.Utils

import System

%access public

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

||| Embarrassingly bad function to do conversion between unix time and date time
private
convert : Int -> DayTime
convert i = MkDaytime
              (getYears + 1970)
              getMonths
              getDays
              getHours
              getMins
              getSecs
  where
    getYears : Int
    getYears = div i 31556926

    epoM : Int
    epoM = mod i 31556926

    getMonths : Int
    getMonths = div epoM 2629743

    epoD : Int
    epoD = mod epoM 2629743

    getDays : Int
    getDays = div epoD 86400

    epoH : Int
    epoH = mod epoD 86400

    getHours : Int
    getHours = div epoH 3600

    epoMin : Int
    epoMin = mod epoH 3600

    getMins : Int
    getMins = div epoMin 60

    epoSec : Int
    epoSec = mod epoMin 60

    getSecs : Int
    getSecs = epoSec


||| Return the daytime if possible.
getDayTime : Either String DayTime
getDayTime = let t = systime in
    if t /= 0
      then Right (convert t)
      else Left "No time"

  where
    systime : Int
    systime = unsafePerformIO time



-- --------------------------------------------------------------------- [ EOF ]
