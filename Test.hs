import System.IO  
import Control.Monad
import Text.ParserCombinators.Parsec
--import Pars
import OkrParser

-- 
fileContents = do  contents <- readFile "okr6.rpt"
                   return contents

okrContents :: GenParser Char st [String]
okrContents =  sepBy okrContent $ string "@\n"

okrContent :: GenParser Char st String
okrContent =  many (noneOf "@")    

okrContentsTest = do fcnt <- fileContents
                     return ( parse okrContents "(unknown)" fcnt )

okrTop :: Int -> GenParser Char st [String]
okrTop n = do okrs <- okrContents 
              return $ first n okrs  

first n xs = map ( \(i,_) -> i) $ zip xs [1..n]  

okrTopTest::Int -> IO (Either ParseError [String])
okrTopTest n = do fcnt <- fileContents
                  return $ parse ( okrTop n ) "(unknown)" fcnt 
-------------------------------------------------                

--okr::n -> IO (Either ParseError [Either ParseError Okr])
okrTest n = do fParsRes <- okrTopTest  n
               return $ do x <- fParsRes
                           return $ map ( \i -> parse okr ":(" i ) x              ---okrRcps2




tst = "==НЖВ_10=10341 \r\n==РЩН_06=20061 \r\n==ЮТ_04=35458  \r\nOKR  Л22 \r\nЛТ \r\n16МАР8176/16МАР354/ЮТ \r\nИЗМ \r\nПДК  27МАР 26ОКТ 37 \r\n"

