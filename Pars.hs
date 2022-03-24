module Pars where

import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Bool
--import Text.ParserCombinators.Parsec.Prim
--import Text.Parsec.Prim



-- csvFile :: GenParser Char st String
-- csvFile = many digit

-- digits :: GenParser Char st String
-- digits = many digit

-- chars :: GenParser Char st String
-- chars = do  c <- char 'A'
--             d <- char 'B'
--             return [c,d]

-------------------------------------------------
-- COMON PARSERS
--------------------------------------------------
--  Parser for string value
str :: String ->  GenParser Char st String
str [] = do return []    
str (c:cx) = do c' <- char c 
                cx' <- str cx
                return  (c':cx')

-- Parser values from [String]
anyStrs :: [String] ->  GenParser Char st String
anyStrs []      =  return []   
anyStrs (s:[])  =  str s 
anyStrs (s:sx)  =  try (str s) <|> anyStrs sx


strN :: (String , Int ) ->  GenParser Char st (String , Int) 
strN (s,i) = do s' <- str s
                return (s', i)

-- Parser values from [(String,Int)] to with int
anyStrsN' :: [(String,Int)] ->  GenParser Char st (String , Int) 
anyStrsN' []      =  return ([],-1)   
anyStrsN' (s:[])  =  strN s 
anyStrsN' (s:sx)  =  try ( strN s ) <|> anyStrsN' sx

-- Parser values from [String] to index
anyStrsN :: [String] ->  GenParser Char st (String , Int) 
anyStrsN xs = anyStrsN' $ zip xs [0..] 

--manyFoldr1::(a -> a -> a) -> Text.Parsec.Prim.ParsecT s u m a -> ParsecT s u m [a]
manyFoldr1 f p = do xs <- many p     
                    return $  foldr1 f xs 


-----------------------------------------------------
-- OKR parsers & structs
------------------------------------------------------
-- Month names mnemonic
monthsOkrNames = ["ЯНВ","ФЕВ","МАР","АПР","МАЙ","ИЮН","ИЮЛ","АВГ","СЕН","ОКТ","НОЯ","ДЕК"] ;

------------------------------------------------------
data MthOfYear = Mth Int                        deriving Show
data DayOfMth  = DayMth Int                     deriving ( Show , Eq )  
data Cust      = Cust String                    deriving Show   
data Year      = Year Int                       deriving Show   
data TimeZone  = Utc | Loc                      deriving Show   
data Flight    = Flight Cust Int                deriving Show   

-----------------------------------------------------------------------------------------
-- Common parsers
--
-- Month mnemonic parser
monthsOkr :: GenParser Char st String
monthsOkr = anyStrs monthsOkrNames; 

-- Month mnemonic parser with index
monthsOkrN :: GenParser Char st (String, Int)
monthsOkrN = do (s,i) <- anyStrsN monthsOkrNames
                return (s,i+1)

monthsOkrN' :: GenParser Char st MthOfYear
monthsOkrN' = do (_,i) <- anyStrsN monthsOkrNames
                 return $ Mth $ i+1 


-- 2 digit chars to int
day :: GenParser Char st (String, Int)
day =  do v <- (count 2 digit)
          return (v, read v ) 

dayN :: GenParser Char st  DayOfMth
dayN = do  v <- (count 2 digit)
           return $ DayMth $ read v 

-- 2 digit chars to int
year :: GenParser Char st Year
year = do v <- count 2 digit 
          return $ Year (read v)              

-- 2 digit chars to int
nums :: GenParser Char st (String, Int)
nums =  do v <-  many1 digit 
           return (v, read v )  

cust :: GenParser Char st Cust
cust =  do  c <- count 2 (noneOf "/ \n")        
            return $ Cust c

timeZone :: GenParser Char st TimeZone
timeZone =   do anyStrs [ "УТЦ", "UTC" ]
                return Utc  
             <|>
             do anyStrs [ "ЛТ", "LT" ]
                return Loc  

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

endLine =  do  many (char ' ')
               eol
               return ()

-- Номер рейса[ SSNNNNN ]
flight = do cst <- cust
            num <- many digit
            return $ Flight cst (read num)


----------------------------------------------------------------------------------
-- Number of Okr [DDMthNNNNNN]
--
data OkrNum = OkrN MthOfYear  DayOfMth  Int  -- deriving Show
instance Show OkrNum where
   show  ( OkrN (Mth m)  (DayMth d)  n )  =  (show d) ++" "++ monthsOkrNames!!(m-1)  ++" №"  ++( show n) 

-- Number of Okr Parser
okrNum :: GenParser Char st OkrNum
okrNum = do (_, d) <- day 
            (_, m) <- monthsOkrN
            (_, n) <- nums
            return $ OkrN  (Mth m)  (DayMth d)  n

---------------------------------------------------------------------------------                
-- Okr Id [ NUM / NUM OWN / CUST ]
--
data OkrId = OkrId Cust OkrNum ( Maybe OkrNum )
instance Show OkrId where
   show  ( OkrId c  n1  n2 )  =  "Id:{" ++ show n1  ++ " / "++ show n2 ++ " / " ++ show c ++" }" 

-- parsers
okrIdFull :: GenParser Char st OkrId
okrIdFull = do n1 <- okrNum
               char '/' 
               n2 <- okrNum 
               char '/'
               cs <- cust
               return $ OkrId cs n1 (Just n2)  

okrIdShort :: GenParser Char st OkrId
okrIdShort = do n1 <- okrNum
                char '/' 
                cs <- cust
                return $ OkrId cs n1 (Nothing)  
-- main parser
okrId :: GenParser Char st OkrId
okrId =  try ( okrIdFull ) <|> okrIdShort
-------------------------------------------------------------------------
-- [ DDMMM ]  01МАР, 31АВГ ....         
data MthDay = MthDay MthOfYear DayOfMth deriving Show
mthDay :: GenParser Char st MthDay
mthDay = do (_, d) <- day 
            (_, m) <- monthsOkrN
            return $ MthDay (Mth m) (DayMth d)

----------------------------------------------------------------------
--- Week days [ NNNNNNN ]   123, 45634, 1234567  
data WeekDay = Mo | Tu | We | Th | Fr | St | Su deriving Show
toWeekDays:: WeekDay -> WeekDays 
toWeekDays Mo = WeekDays True False False False False False False
toWeekDays Tu = WeekDays False True False False False False False
toWeekDays We = WeekDays False False True False False False False
toWeekDays Th = WeekDays False False False True False False False
toWeekDays Fr = WeekDays False False False False True False False
toWeekDays St = WeekDays False False False False False True False
toWeekDays Su = WeekDays False False False False False False True

weekDayByNum :: Int -> WeekDays
weekDayByNum n | n == 1 = toWeekDays Mo  
               | n == 2 = toWeekDays Tu  
               | n == 3 = toWeekDays We  
               | n == 4 = toWeekDays Th  
               | n == 5 = toWeekDays Fr  
               | n == 6 = toWeekDays St  
               | n == 7 = toWeekDays Su  
               | otherwise = weekDaysEmpty 


weekDaysNames = [ "Mo","Tu","We","Th","Fr","St","Su"] ;
data WeekDays = WeekDays Bool Bool Bool Bool Bool Bool Bool 
instance Show WeekDays where
    show (WeekDays mo tu we th fr st su ) =  
        (if mo then weekDaysNames!!0 else "__")  ++ " " ++
        (if tu then weekDaysNames!!1 else "__")  ++ " " ++
        (if we then weekDaysNames!!2 else "__")  ++ " " ++
        (if th then weekDaysNames!!3 else "__")  ++ " " ++
        (if fr then weekDaysNames!!4 else "__")  ++ " " ++
        (if st then weekDaysNames!!5 else "__")  ++ " " ++
        (if su then weekDaysNames!!6 else "__")  

weekDaysEmpty = WeekDays False False False False False False False

spl :: WeekDays ->  WeekDays ->  WeekDays     
spl (WeekDays mo tu we th fr st su ) (WeekDays mo' tu' we' th' fr' st' su' ) = WeekDays (mo||mo') (tu||tu') (we||we') (th||th') (fr||fr') (st||st') (su||su')
(+++) = spl

setWeekDay::WeekDays ->  WeekDay ->  WeekDays     
setWeekDay wds wd =  wds +++ (toWeekDays  wd)

-- parse digit char to week day
weekDay::  GenParser Char st WeekDays
weekDay =  do d <- ( char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|> char '6' <|> char '7' ) 
              return $ weekDayByNum $ ( read [d] )

-- parse week days set 
weekDays::  GenParser Char st WeekDays
weekDays = manyFoldr1 spl weekDay
------------------------------------------------------------------------
-- Days of month 
data MthDays = MthDays [DayOfMth] deriving Show
empty = MthDays []
add :: MthDays -> DayOfMth -> MthDays
add (MthDays ds) day = MthDays $  if (  foldr (\e a ->  a || (e == day) ) False ds ) then ds else ( ds ++ [day]) 

mthDays =  do ds <- sepBy dayN space
              return $ MthDays ds   

mthDaysTest = "01 22 23 10 05"
-----------------------------------------------------------------------
-- Month with dates  [MMM DD DD DD ...] 
data MthWithDays = MthWithDays  MthOfYear MthDays deriving Show
mthWithDays = do m <- monthsOkrN'
                 many1 space
                 ds <- mthDays
                 return $ MthWithDays m ds 

mthWithDaysTest = "МАР 01 22 23 10 05"
------------------------------------------------------------------------
--  Month day interval 23МАР 13АВГ 
data MthDayInterval = MthDayInterval MthDay MthDay deriving Show
mthDayInterval :: GenParser Char st MthDayInterval
mthDayInterval = do d1 <- mthDay
                    many1 space
                    d2 <- mthDay
                    return $ MthDayInterval d1 d2
------------
-- Month day interval with Week days  [DDMMM  DDMMM NNNNNNNN ]   (23МАР 13АВГ 1234567)
data WeekDaysInterval = WeekDaysInterval MthDayInterval  WeekDays deriving Show
weekDaysInterval::GenParser Char st WeekDaysInterval
weekDaysInterval = do i <- mthDayInterval
                      many1 space
                      w <- weekDays
                      return $ WeekDaysInterval i w
-------------------------------------------------------------------------
-- tests
okrIdFullTestData = "12ЯНВ123/01МАЙ123234/SA" 
okrIdFullTestData2 = "12ЯНВ123/DE" 
okrIdFullTest =  parse  okrId "(unknown)" okrIdFullTestData  
okrIdFullTest2 =  parse  okrId "(unknown)" okrIdFullTestData2  

-----------------------------------------------------------------------------
-- Okr Header
-- 
-- Type of message
data OkrType = Osc | Sdl | Inf 
instance Show OkrType where
   show Osc = "Correction"
   show Sdl  ="Shedule "
   show Inf  ="Information"  

okrTypeOsc = do ( str "ОКР" ) <|>  ( str "OKR" )  
                return Osc
okrTypeSdl = do str "РСП"  
                return Sdl
okrTypeInf = do str "ИНФ"  
                return Inf
-- Type parser
okrType  = try okrTypeOsc <|>  try okrTypeSdl <|>  okrTypeInf            

data Season = Summer | Winter deriving Show

seassonW = do char 'Л' <|> char 'S'
              return  Winter
seassonS = do char 'З' <|> char 'W'
              return  Summer
season =  seassonW <|> seassonS

-- Okr season [ОКР SNN]
data OkrCaption = OkrCaption OkrType Season Year deriving Show
okrCaption = do t <- okrType
                skipMany space
                s <- season
                y <- year
                return $ OkrCaption t s y

---------------------------------------------------------
--  Header of OKR
data OkrHeader = Header OkrCaption TimeZone OkrId deriving Show
okrHeader :: GenParser Char st OkrHeader
okrHeader = do cap <- okrCaption 
               endLine 
               tz <- timeZone
               endLine
               id <- okrId
               endLine
               return $ Header cap tz id 

okrHeaderTest = "ОКР  Л21 \r\nЛТ \r\n25ФЕВ4272/24ФЕВ271/ЮТ \r\n ИЗМ \r\n"
-----------------------------------------------------------------------------
-- BODY 
------------------------------------------------------------------------------
--data OkrBody = Body OkrAction deriving Show
--OkrBody

data Edt = Edt Pdk deriving Show
edt = do p <- pdk 
         return $ Edt p    


data OkrAction = ActNew | ActEdt Edt  | ActDel  deriving Show
okrAction =   do str "NOV" <|> str "НОВ"
                 return  ActNew
              <|>                    
              do str "IZM" <|> str "ИЗМ"
                 endLine
                 a <- edt 
                 return $ ActEdt a
              <|> 
              do str "OTM" <|> str "ОТМ"
                 return  ActDel   




------------------------------------------------------
-- Период дни недели либо дни месяца
data OkrPeriod =   PerMd  MthWithDays |  PerWd  WeekDaysInterval  deriving Show

-- okrPeriod' = try (mthWithDays >>= (\i -> return $ PerMd i) ) <|> ( weekDaysInterval >>= (\i -> return $ PerWd i) )   -- wo do example
okrPeriod =     try ( do i <- mthWithDays 
                         return $ PerMd i 
                    ) 
                <|>
                do i <- weekDaysInterval
                   return $ PerWd i

okrPeriod1T = "МАР 01 22 23 10 05"
okrPeriod2T = "25ФЕВ 26ФЕВ 271"
      
------------------------------------------------------
-- ПДК период действия корректировки
data Pdk = Pdk OkrPeriod deriving Show
pdk = do str "ПДК" <|> str "PDK"
         skipMany space
         p <- okrPeriod
         return $ Pdk p

pdkT1 = "ПДК МАР 01 22 23 10 05"
pdkT2 = "PDK 25ФЕВ 26ФЕВ 271"
---------------------------------------------------------
--- ОКР 
data Okr = Okr OkrHeader OkrAction  deriving Show

okr :: GenParser Char st Okr
okr = do h <- okrHeader 
         --endLine 
         a <- okrAction  
         return $ Okr h a 

editT = "ОКР  Л21\r\nЛТ \r\n25ФЕВ4272/24ФЕВ271/ЮТ \r\nИЗМ \r\nПДК  02МАЙ 28ОКТ 47\r\n"


--parseMthOkr input = parse dayMthOkrN "(unknown)" input    

--parseOKR :: String -> Either ParseError String
--parseOKR input = parse monthsOkr "(unknown)" input
--parseOKR input = parse (str2 "ASD" "ASE" )"(unknown)" input

--parseOKR input = parse (anyStrsN' [ ("ASw",1 ) ,("ASD",2 ) ] ) "(unknown)" input


