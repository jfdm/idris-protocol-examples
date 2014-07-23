-- --------------------------------------------------------------- [ Utils.idr ]
--
-- Utilities
--
-- --------------------------------------------------------------------- [ EOH ]
module Protocol.Utils


Literal : String -> Type
Literal str = (str' : String ** str' = str)
