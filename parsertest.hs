import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Bool
import Data.Tuple
import Control.Monad

import OkrParser
 
p =   spaces *> many ( day <* spaces  ))

--p1 sepBy

--p1 =  many1 (p <* eol ) 

p1 =  sepBy p ( string "R")

n = string "NNN"

n1 = liftM2 (\a x -> (a,x))  p1  n

t = parse  p "(unknown)" " 01 22 23 10 05"                    
t1 = parse p "(unknown)" " 01 22 23 10 05"                    
t2 = parse p "(unknown)" " 01 22 23 10 05 "                    
t3 = parse p "(unknown)" " 01 22 23 10 05\n\r"                    

a0 = "R 01 22 23 10 05"                    
a1 = "R 01 22 23 10 05"                    
a2 = "R 01 22 23 10 05 "                    
a3 = "R 01 22 23 10 05 "  

b = "NNNe"

a = a1++"\n"++a2++"\n"++a3 ++"\n"++a0++"\n"++b++"\n"   


--ta = t . t1 . t2 . t3 