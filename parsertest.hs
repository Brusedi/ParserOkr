import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Bool
import Control.Monad

-- Parser alternative values from [String]
strsAltN :: [(String, Int )] ->  GenParser Char st Int
strsAltN []      =  return (-1)   
strsAltN ((s,n):[])  = liftM ( \_ -> n ) $ string s  
strsAltN (s:sx)  =  try (strsAltN [s] ) <|> strsAltN sx
