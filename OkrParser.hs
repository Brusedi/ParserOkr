module OkrParser where

import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Bool
import Control.Monad

-------------------------------------------------------------------------------------------
--  CONSTANTS

monthsOkrNames = ["ЯНВ","ФЕВ","МАР","АПР","МАЙ","ИЮН","ИЮЛ","АВГ","СЕН","ОКТ","НОЯ","ДЕК"] ;



---------------------------------------------------
--- STRUCTS 
--- ОКР 
data Okr = Okr OkrRcps OkrHeader deriving Show            --OkrHeader OkrAction  deriving Show // TODO

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
instance Show OkrNum where show  ( OkrN  (DayMth d) (Mth m)  n )  =  (show d) ++" "++ monthsOkrNames!!(m-1)  ++" №"  ++( show n) 

data MthOfYear = Mth Int deriving Show
data DayOfMth  = DayMth Int deriving ( Show , Eq )  
data Cust      = Cust String deriving Show  


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

-- Row of telegram
row = sepBy ( many (noneOf "\r\n")) eol 

-------------------------------------------
--- Typed Parsers

-- Main Okr parser 
okr :: GenParser Char st Okr
okr = liftM2 Okr (okrRcps <* eol )  okrHeader 


-- Recipient List [== SSSSSSSSSSSSSSSSSSSSSSSS ....  ]  "==eeee\r\n==eeee \r\n==eeee\r\n"
okrRcps :: GenParser Char st OkrRcps
okrRcps = liftM OkrRcps $ many1 $ liftM3 id2f3  (string "==")  ( many $ noneOf "\r\n" ) ( eol )  

-- Okr header parser
okrHeader :: GenParser Char st OkrHeader
okrHeader = liftM3 (\cap tz id -> OkrHeader cap tz id )  okrCaption timeZone okrId

-- Caption section 
okrCaption :: GenParser Char st OkrCaption
okrCaption = liftM3 OkrCaption (okrType <* skipMany space) season year  -- todo

-- Okr Type 
okrType :: GenParser Char st OkrType
okrType  = try okrTypeOsc <|>  try okrTypeSdl <|>  okrTypeInf          
okrTypeOsc = liftM (\_ -> Osc) $ strsAlt ["ОКР","OKR"]  
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
timeZone =  ( liftM (\_ -> Utc ) $ strsAlt  [ "УТЦ", "UTC" ] )  <|>  ( liftM (\_ -> Utc ) $ strsAlt  [ "ЛТ", "LT" ] )


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
monthsOkr = liftM Mth ( strsAltN $ zip monthsOkrNames [1..] ) 

-- many digit chars to int
num :: GenParser Char st Int
num =  liftM read (many1 digit) 
           
cust :: GenParser Char st Cust
cust = liftM Cust ( count 2 ( noneOf "/ \n" ) )    
            

