module OkrParser where

import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Bool
import Control.Monad
import Data.Tuple

-------------------------------------------------------------------------------------------
--  CONSTANTS

tagMonths = ["ЯНВ","ФЕВ","МАР","АПР","МАЙ","ИЮН","ИЮЛ","АВГ","СЕН","ОКТ","НОЯ","ДЕК"] ;
tagActNew = [ "NOV", "НОВ" ]
tagActEdt = [ "IZM", "ИЗМ" ]
tagActDel = [ "OTM", "ОТМ" ]
tagPdk    = [ "ПДК", "PDK" ]
tagOkr    = ["ОКР","OKR"]  
tagUtc    = [ "УТЦ", "UTC" ] 
tagLoc    = [ "ЛТ", "LT" ]
---------------------------------------------------
--- STRUCTS 
--- ОКР 
data Okr = Okr OkrRcps OkrHeader OkrAction  deriving Show         -- TODO

-- Recipient List
data OkrRcps = OkrRcps [String] deriving Show 

--  Header of OKR
data OkrHeader = OkrHeader OkrCaption TimeZone OkrId deriving Show  --  TimeZone OkrId deriving Show   // TODO

-- Caption of OKR
data OkrCaption = OkrCaption OkrType Season Year deriving Show 

-- Type of message
data OkrType = Osc | Sdl | Inf 
instance Show OkrType where
   show Osc = "Correction"
   show Sdl  ="Shedule "
   show Inf  ="Information"  

-- Season 
data Season = Summer | Winter deriving Show

-- Year
data Year = Year Int deriving Show   

-- TimeZone UTC or Local
data TimeZone  = Utc | Loc  deriving Show 

-- Okr Id [ NUM / NUM OWN / CUST ]
data OkrId = OkrId Cust OkrNum ( Maybe OkrNum )
instance Show OkrId where show  ( OkrId c  n1  n2 )  =  "Id:{" ++ show n1  ++ " / "++ show n2 ++ " / " ++ show c ++" }" 

-- Number of Okr [DDMthNNNNNN]
data OkrNum = OkrN DayOfMth MthOfYear Int  -- deriving Show
instance Show OkrNum where show  ( OkrN  (DayMth d) (Mth m)  n )  =  (show d) ++" "++ tagMonths!!(m-1)  ++" №"  ++( show n) 

data MthOfYear = Mth Int deriving Show
data DayOfMth  = DayMth Int deriving ( Show , Eq )  
data Cust      = Cust String deriving Show  

-- BODY 
data OkrAction = ActNew | ActEdt Edt  | ActDel  deriving Show

-- ОКР изменение расписания
data Edt = Edt [Pdk] Flight deriving Show

-- ПДК период действия корректировки
data Pdk = Pdk OkrPeriod deriving Show

-- Период дни недели либо дни месяца
data OkrPeriod =   PerMd  MthWithDays |  PerWd  WeekDaysInterval  deriving Show

 -- Month with dates  [MMM DD DD DD ...] 
data MthWithDays = MthWithDays  MthOfYear MthDays deriving Show

-- Days of month 
data MthDays = MthDays [DayOfMth] deriving Show

-- Month day interval with Week days  [DDMMM  DDMMM NNNNNNNN ]   (23МАР 13АВГ 1234567)
data WeekDaysInterval = WeekDaysInterval MthDayInterval  WeekDays deriving Show

--  Month day interval 23МАР 13АВГ 
data MthDayInterval = MthDayInterval MthDay MthDay deriving Show

-- [ DDMMM ]  01МАР, 31АВГ ....         
data MthDay = MthDay DayOfMth  MthOfYear deriving Show

-- day of week
data Day    = Monday  | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday  deriving ( Show, Eq)
instance Enum Day where
   fromEnum = fromJust . flip lookup dayIndexes
   toEnum = fromJust . flip lookup (map swap dayIndexes)
dayIndexes = zip [ Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday ] [1..]

-- days of week
data WeekDays = WeekDays [ Day ] deriving Show

-- Flight
data Flight = Flight Cust Int deriving Show
-------------------------------------------------
--- combinators helpers  

id2f3 _ x _= x 
id1f2 x _  = x 

-- Parser alternative values from [String]
strsAlt :: [String] ->  GenParser Char st String
strsAlt []      =  return []   
strsAlt (s:[])  =  string s 
strsAlt (s:sx)  =  try (string s) <|> strsAlt sx

-- Parser alternative values from [String]
strsAltN :: [(String, Int )] ->  GenParser Char st Int
strsAltN []      =  return (-1)   
strsAltN ((s,n):[])  = liftM ( \_ -> n ) $ string s 
strsAltN (s:sx)  =  try (strsAltN [s] ) <|> strsAltN sx

--------------------------------------------------
--- Parsers

-- End row
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> try (string "\n\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

--spaces = (many space) 
spacesEol = spaces *> eol



-- Row of telegram
row = sepBy ( many (noneOf "\r\n")) eol 

-------------------------------------------
--- Typed Parsers

-- Main Okr parser 
okr :: GenParser Char st Okr
okr = liftM3 Okr (okrRcps) (okrHeader <* spaces )  okrAction   -- <* eol 

--okrRcps2 = liftM2 (,) okrRcps ( many $ noneOf "@" )

-- Recipient List [== SSSSSSSSSSSSSSSSSSSSSSSS ....  ]  "==eeee\r\n==eeee \r\n==eeee\r\n"
okrRcps :: GenParser Char st OkrRcps
okrRcps = liftM OkrRcps $ many1 $ liftM3 id2f3  (string "==")  ( many $ noneOf "\r\n" ) ( eol )  

-- Okr header parser
okrHeader :: GenParser Char st OkrHeader
okrHeader = liftM3  OkrHeader  ( okrCaption <* spaces)  ( timeZone <* spaces ) okrId

-- Caption section 
okrCaption :: GenParser Char st OkrCaption
okrCaption = liftM3 OkrCaption (okrType <* spaces) season year  -- todo

-- Okr Type 
okrType :: GenParser Char st OkrType
okrType  = try okrTypeOsc <|>  try okrTypeSdl <|>  okrTypeInf          
okrTypeOsc = liftM (\_ -> Osc) $ strsAlt tagOkr  
okrTypeSdl = liftM (\_ -> Sdl) $ string "РСП"  
okrTypeInf = liftM (\_ -> Inf) $ string "ИНФ"  

-- Season parser
season :: GenParser Char st Season
season =  seassonW  <|>  seassonS 
            where   seassonW = liftM (\_ -> Winter) $  ( char 'Л' <|> char 'S' ) 
                    seassonS = liftM (\_ -> Summer) $  ( char 'З' <|> char 'W' )

-- 2 digit chars to int
year :: GenParser Char st Year
year = liftM (\v -> Year (read v) ) (count 2 digit )

-- UTC or Local parser               
timeZone :: GenParser Char st TimeZone
timeZone =  ( liftM (\_ -> Utc ) $ strsAlt tagUtc )  <|>  ( liftM (\_ -> Loc ) $ strsAlt tagLoc )


-- parsers
okrId:: GenParser Char st OkrId 
okrId = try okrIdFull <|> okrIdShort where
    okrIdFull  = liftM3 (\n1 n2 cst ->  OkrId cst n1 (Just n2) ) okrNum ( char '/' >>  okrNum ) ( char '/' >> cust )
    okrIdShort = liftM2 (\n1 cst ->  OkrId cst n1 (Nothing) ) okrNum ( char '/' >> cust )

-- Number of Okr Parser
okrNum :: GenParser Char st OkrNum
okrNum = liftM3 OkrN day monthsOkr num

-- 2 digit chars to int
day :: GenParser Char st DayOfMth
day = liftM ( DayMth . read )  (count 2 digit)

-- month number
monthsOkr :: GenParser Char st MthOfYear
monthsOkr = liftM Mth ( strsAltN $ zip tagMonths [1..] ) 

-- many digit chars to int
num :: GenParser Char st Int
num =  liftM read (many1 digit) 

--            
cust :: GenParser Char st Cust
cust = liftM Cust ( count 2 ( noneOf "/ \n" ) )    
            
okrAction :: GenParser Char st OkrAction            
okrAction =  anew <|> aedt <|>  adel where  
                anew = liftM (const ActNew) $ strsAlt  tagActNew          
                aedt = liftM2 (\_ x -> ActEdt x)  ( strsAlt tagActEdt <* spaces )  edt
                adel = liftM (const ActDel) $ strsAlt  tagActDel           
  
weekDay::  GenParser Char st Day
weekDay = liftM (\x -> toEnum . read $ return x )  ( oneOf "1234567") 

weekDays:: GenParser Char st WeekDays
weekDays = liftM WeekDays ( many1 weekDay )

edt:: GenParser Char st Edt
edt = liftM2 Edt ( many1 (pdk <* eol) )  flight

pdk:: GenParser Char st Pdk
pdk =  liftM3 (\_ _ x -> Pdk x)  (strsAlt tagPdk) ( skipMany space)  okrPeriod 

okrPeriod:: GenParser Char st OkrPeriod
okrPeriod =  ( try $ liftM PerMd mthWithDays )  <|>  liftM  PerWd  weekDaysInterval 

mthDayInterval :: GenParser Char st MthDayInterval
mthDayInterval = liftM2 MthDayInterval mthDay ( many1 space >> mthDay)

weekDaysInterval::GenParser Char st WeekDaysInterval
weekDaysInterval = liftM2 WeekDaysInterval mthDayInterval ( many1 space >> weekDays)


mthDay :: GenParser Char st MthDay
mthDay = liftM2 MthDay day monthsOkr 

mthWithDays :: GenParser Char st MthWithDays
mthWithDays = liftM3 (\x _ y -> MthWithDays x y)  monthsOkr space  mthDays

mthDays :: GenParser Char st MthDays
mthDays =  liftM MthDays ( spaces *> many ( day <* spaces) ) --( sepBy day space ) 

flight :: GenParser Char st Flight
flight =  liftM2 (\c n ->  Flight c $ read n ) cust (many1 digit)  

---------------------------------------------------------------
okrPeriod1T = "МАР 01 22 23 10 05"
okrPeriod2T = "25ФЕВ 26ФЕВ 271"
    
pdkT1 = "ПДК МАР 01 22 23 10 05"
pdkT2 = "PDK 25ФЕВ 26ФЕВ 271"

pdkT3 = "PDK 25ФЕВ 26ФЕВ 271\r\nPDK 25ФЕВ 26ФЕВ 271\r\nPDK 25ФЕВ 26ФЕВ 271\r\nПДК МАР 01 22 23 10 05\r\nПДК МАР 01 22 23 10 05 \r\n"         
