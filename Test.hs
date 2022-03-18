import System.IO  
import Control.Monad
import Text.ParserCombinators.Parsec
import Pars

main = do  contents <- readFile "okr2.txt"
           return contents

okrs :: GenParser Char st [String]
okrs = sepBy allC (string "\r\n")

allC :: GenParser Char st String
allC = do many (noneOf "")

test = do a <-  main  
          return ( parse okrs "(unknown)" a )  



     