-- --------------------------------------------------------------- [ Utils.idr ]
--
-- Utility functions for Chargen
--
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.CharGen.Utils

||| Rotate a String by one character
%assert_total
strShift : String -> String
strShift s = reverse (strCons h (reverse t))
  where
    h : Char
    h = strHead s
    t : String
    t = strTail s

||| Dummy text for CharGen
dummyText : String
dummyText = pack $ map (\x => chr x) [33..125]

-- --------------------------------------------------------------------- [ EOF ]
