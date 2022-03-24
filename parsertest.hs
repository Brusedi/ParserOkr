import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Bool
import Data.Tuple
import Control.Monad

-- Parser alternative values from [String]
strsAltN :: [(String, Int )] ->  GenParser Char st Int
strsAltN []      =  return (-1)   
strsAltN ((s,n):[])  = liftM ( \_ -> n ) $ string s  
strsAltN (s:sx)  =  try (strsAltN [s] ) <|> strsAltN sx

-- data WeekDays = WeekDays [ Bool ]
-- instance Show WeekDays where
--     show ( WeekDays ds ) =  foldl (\ac (i,s) -> ac ++ if ( length ds > i && ds!!i)  then [s] else "_" ) "" ( zip [0..] "MTWTFSS" )

-- weekDay::  GenParser Char st WeekDays
-- weekDay = liftM toWd ( char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|> char '6' <|> char '7' )  where 
--             toWd d = WeekDays [ toN d == x | x <- [0..(toN d) ] ] where toN n = (read [n] ::Int ) - 1            

-- weekDays = do x -> many weekDay
--              return x
-- parse week days set 
-- weekDays::  GenParser Char st WeekDays
-- weekDays = manyFoldr1 spl weekDay             

-- manyFoldr1 f p = liftM  ( foldr1 f x)   many p     

data Day    = Monday  | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday  deriving ( Show, Eq)
instance Enum Day where
   fromEnum = fromJust . flip lookup dayIndexes
   toEnum = fromJust . flip lookup (map swap dayIndexes)
dayIndexes = zip [ Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday ] [1..]

data WeekDays = WeekDays [ Day ] deriving Show

weekDay::  GenParser Char st Day
weekDay = liftM (\x -> toEnum . read $ return x )  ( oneOf "1234567") 

weekDays:: GenParser Char st WeekDays
weekDays = liftM WeekDays ( many1 weekDay )
 

                    