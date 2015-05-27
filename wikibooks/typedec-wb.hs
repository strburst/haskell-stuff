data Date = Date Int Int Int -- year month day
type Name = String

data Anniversary = Birthday Name Date
                 | Wedding Name Name Date

showAnniversary (Birthday name date) = show name ++ " was born on " ++ showDate date
        where showDate (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

showAnniversary (Wedding name1 name2 date) = show name1 ++ " married " ++ show name2 ++ " on " ++ showDate date
        where showDate (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d
