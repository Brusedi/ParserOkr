import Data.List

newtype Parser a = Parser { unParser :: String -> [(String, a)] }

parseString :: String -> Parser a -> Maybe a
parseString s (Parser p) = case (p s) of
    [("", val)] -> Just val
    _           -> Nothing




predP :: (Char -> Bool) -> Parser Char
predP p = Parser f
  where
    f "" = []
    f (c : cs) | p c = [(cs, c)]
               | otherwise = []


stringP :: String -> Parser String
stringP s = Parser f
  where
    f s' | s == s' = [("", s)]
         | otherwise = []

skip :: (Char -> Bool) -> Parser ()
skip p = Parser (\s -> [(dropWhile p s, ())])

prefixP :: String -> Parser String
prefixP s = Parser f
  where
    f input = if s `isPrefixOf` input
                then [(drop (length s) input, s)]
                else []

skipString :: String -> Parser ()
skipString s = Parser f
  where
    f input = if s `isPrefixOf` input
                then [(drop (length s) input, ())]
                else []



instance Functor Parser where
fmap :: (a -> b) -> Parser a -> Parser b
fmap f (Parser p1) = Parser (\s ->  map ( \(s1,a1) -> (s1, f a1 ) ) (p1 s) )


--
applyP :: Parser (a -> b) -> Parser a -> Parser b
-- f::  String -> [(String, (a->b))]
-- p::  String -> [(String, a)]
applyP (Parser f ) (Parser p) = Parser ( \s ->  applyP        ) 



-- import Text.Parsec

-- getList :: Parsec String u [String]
-- getList =  many1 digit `sepBy` char ';'

--2*2

-- data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

-- concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
-- concat3OC (Un x) (Un y) z = Bi x y z
-- concat3OC (Un x) (Bi y1 y2 ys) z = concat3OC  (concat3OC (Un x) (Un y1) (Un y2) ) ys z
-- concat3OC (Bi x1 x2 xs) y z  = Bi x1 x2 ( concat3OC xs y z ) 

-- concatOC :: OddC (OddC a) -> OddC a
-- concatOC (Un ox) = ox
-- concatOC (Bi ox1 ox2 oxs) = concat3OC ox1 ox2 ( concatOC oxs)

-- instance Functor OddC where 
--   --fmap:: (a -> b) -> OddC a -> OddC b
--   fmap f (Un x) = Un $ f x
--   fmap f (Bi x1 x2 xs)  = Bi (f x1) (f x2) (fmap f xs)

-- instance Applicative OddC where 
--   --fmap:: (a -> b) -> OddC a -> OddC b
--   pure x = Un x
--   (<*>) (Un f) ox = fmap f ox  
--   (<*>) (Bi f1 f2 fs) ox = concat3OC (f1 <$> ox) (f2 <$> ox) (fs <*> ox)  

-- instance Monad OddC where
--    (>>=) (Un x) pf = pf x 
--    (>>=) (Bi x1 x2 xs) pf = concat3OC (pf x1) (pf x2) ( xs >>= pf ) 

-- tst1 = Bi 10 20 (Un 30)
-- tst2 = Bi 1 2 (Bi 3 4 (Un 5))



-- foo :: OddC a -> OddC a
-- foo (Un x)      = Un x
-- foo (Bi x y z)  = Bi y x z 

-- foo2 :: OddC a -> OddC a -> OddC a
-- foo2 (Un x) y = Bi x x y
-- foo2 (Bi x1 x2 xs) y  = Bi x1 x2 ( foo2 xs y ) 


-- foo3 :: OddC a -> OddC a -> OddC a -> OddC a

-- foo3 (Un x) (Un y) z = Bi x y z
-- foo3 (Un x) (Bi y1 y2 ys) z = foo3  (foo3 (Un x) (Un y1) (Un y2) ) ys z
-- foo3 (Bi x1 x2 xs) y z  = Bi x1 x2 ( foo3 xs y z ) 

--concatOC :: OddC (OddC a) -> OddC a
--concatOC = undefined



-- tst1 = Bi 'a' 'b' (Un 'c')
-- tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
-- tst3 = Bi 'i' 'j' (Un 'k')

-- t = Bi tst1 tst2 (Un tst3)


--Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
--Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))

-- --import Language.Haskell.Exts.Build

-- satisfyE :: (Char -> Bool) -> PrsE Char
-- satisfyE f = PrsE fn where 
--     fn "" = Left "unexpected end of input"
--     fn (c:cs) | f c =  Right (c, cs)
--               | otherwise = Left $ "unexpected "++[c]

-- charE :: Char -> PrsE Char
-- charE c = satisfyE (== c)

-- newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

-- instance Functor  PrsE where 
--   -- :: (a -> b)-> Prse  a -> Prse  b
--   fmap fn rpf  = PrsE ( \s -> do (x,s) <- runPrsE rpf s
--                                  return (fn x,s) )    

-- instance Applicative PrsE where 
--   pure x = PrsE (\s -> return (x , s) )
--   (<*>) pf pv = PrsE fun where 
--                      fun s = do (f', s') <- runPrsE pf s 
--                                 runPrsE (f' <$> pv ) s'


-- -- instance Monad PrsE where
-- --   (>>=) pv pf = PrsE (\s -> foo $ ( ( runPrsE pv ) s )  ) where 
-- --           foo (Right (x',s'))   =  ( runPrsE ( pf x') s' )   
-- --           foo (Left e )         =  Left e    
          
-- instance Monad PrsE where
--  (>>=) pv pf = PrsE (\s -> foo $ ( ( runPrsE pv ) s )  ) where 
--              foo ex  =  do (x',s') <- ex
--                            (x'', s'') <- (runPrsE ( pf x')) s'  
--                            return  (x'', s'')

--                            --return (x'',  ( if (x' == x'') then s'' else s') )
--                            --return (x'',  s' ++ "!" ++ s'' ++"$" )

-- --           (x'', s'') <- (runPrsE ( pf x')) s'  
--                           --  --return (x'',  ( if (x' == x'') then s'' else s') )
--                           --  return (x'',  s' ++ "!" ++ s'' ++"$" )



--             --  foo (Right (x,s')) = runPrsE (pf x) s'  
--             --  foo (Left l)       = Left l       
               
               
--                -- do (x',s') <- ex
--                 --           return  (x',s') 
--                            --(x'',s'') <- runPrsE ( pf x') s'  
--                            --return (x'',s'')


--                           --  (x'', s'') <- (runPrsE ( pf x')) s'  
--                           --  --return (x'',  ( if (x' == x'') then s'' else s') )
--                           --  return (x'',  s' ++ "!" ++ s'' ++"$" )

--   --    PrsE a   a -> PrsE b                         Either String (a, String)     
-- --   (>>=) pv       pf = PrsE            (\s -> foo $ (       ( runPrsE pv ) s     )  ) where 
-- -- --             Either String (a, String)         a  String 
-- --            foo ex                         =  do (x',s') <- ex


-- --                                                 (x'', s'') <- (runPrsE ( pf x')) s'   

-- --   --() 
--   -- (>>=) pv pf = PrsE (\s -> foo $ ( ( runPrsE pv ) s )  ) where 
--   --            foo ex  =  do (x',s') <- ex
--   --                          (x'', s'') <- (runPrsE ( pf x')) s'  
--   --                          --return (x'',  ( if (x' == x'') then s'' else s') )
--   --                          return (x'',  s' ++ "!" ++ s'' ++"$" )


-- --   (>>=) pv pf = PrsE (\s -> foo $ ( ( runPrsE pv ) s )  ) where 
-- --           foo ex  =  do (x,s) <- ex
-- --                         return (x,s)

-- f11 pv pf = PrsE (\s -> foo $ ( ( runPrsE pv ) s )  ) where 
--              foo ex  =  do (x',s') <- ex
--                            (x'',s'') <- runPrsE ( pf x') s'  
--                            return (x'',s'')


-- f0 ::  (Either String (a,String)) -> ( a -> ( Either String (a,String))  ) -> ( Either String (a,String))
-- f0 pv pf = do  (x,s) <- pv 
--                 (x',s') <- pf x 
--                 return (x', s' ++ s ) 


-- (>>=) (,) :: (a -> (b, str) ) -> (a,str) -> (b, str) 

-- (>>=) Eith :: (a -> Eth b ) -> Eth a -> Eth b  

 

--ff:: (PrsE a) -> ( a -> PrsE b) -> PrsE a
--ff:: Either String (a, String)  -> f -> Either String (a, String)  

-- ff :: (Monad m2) => Either String (m2 b) -> p -> Either String (m2 b)
-- ff pv pf = do (x) <- pv 
--               return (fff x)  where
--                       fff x' =  do x''<- x' 
--                                    return x''  

-- fff ::  (Either String (a,String)) -> ( a -> ( Either String (a,String))  ) -> ( Either String (a,String))
-- fff pv pf = do  (x,s) <- pv 
--                 (x',s') <- pf x 
--                 return (x', s' ++ s ) 

-- fff :: (Monad m2) => ( (m2 b), String)  -> p -> ( (m2 b), String)
-- fff pv pf = do (x) <- pv 
--                return (fff x)  where
--                        fff x' =  do x''<- x' 
--                                     return x''  


-- fff ::  Either String ( (b,String) ) -> p -> Either String ((b,String) )
-- fff pv pf = do (x) <- pv 
--               return (fff x)  where
--                       fff x' =  do x''<- x' 
--                                    return x''  



-- ffff pv pf = PrsE (\s -> foo $ ( ( runPrsE pv ) s )  ) where 
--           foo (Right (x',s'))   =  ( runPrsE ( pf x') s' )   
--           foo (Left e )         =  Left e             

  -- :: (a -> mb) -> ma -> mb
  -- :: (str->Eth str (a, str)) ->  (PrsE str->Eth str (a, str)) ->  (PrsE str->Eth str (b, str)) 
--  a -> Prse a => a -> Either 
--(  a -> Prse ( str ->  ( str->Eth str (a, str))  )   

-- \ a -> 
--                      Ether s (a,s)  
--   (>>=) pv pf = \s ->  ( ( runPrsE pv ) s )


                    


                      -- fun s = foo ( (runPrsE pv) s) where
                      --             foo ( Right (x', s')) = (pf x') 
                      --             foo (Left e) = 


--                             -- ( \s -> foo fn (fx s) ) where    
--                             --foo _ (Left e)  = Left e
                            --foo f ( Right (x , s') ) = Right  (f x, s')

--   (<*>) pf pv = PrsE fun where 
--                     fun s = do (f', s') <- runPrsE pf s 
--                                runPrsE (f' <$> pv) s'

-- instance Monad PrsE where
--   -- :: (a -> PrsE b) -> PrsE a -> PrsE b
--   (>>=) = undefined



-- newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

    --- (pure Branch) <*> (traverse  fn t1) <*> (fn x1) <*> (traverse fn Nil)


  --traverse fn (Branch t1 x1 (Nil)) = (pure Branch) <*> (traverse  fn t1) <*> (fn x1) <*> (traverse fn Nil)
  --traverse fn (Branch tl x  (Branch trl trx trr )) = (pure Branch) <*> (traverse  fn tl) <*> (fn trx) <*> (traverse  fn  ( Branch trl x trr  ) )


--sequenceA . fmap pure = pure


  -- traverse fn (Branch tl x tr) = rExch ( Branch <$> (traverse  fn tl) )   
  --      rExch af x' (Nil) =  af <*> (fn x') <*> (traverse fn Nil)
  --      rExch af x' (Branch tr' tx tl') = af <*> (fn tx) <*> (traverse fn ( Branch tr' x' tl' ))



  -- traverse fn (Branch tl x  (Branch trl trx trr )) = (pure Branch) <*> (traverse  fn tl) <*> (fn trx) <*> (traverse  fn  ( Branch trl x trr  ) )


  --traverse fn (Branch t1 x1 (Nil)) = (pure Branch) <*> (traverse  fn t1) <*> (fn x1) <*> (traverse fn Nil)
  --traverse fn (Branch tl x  (Branch trl trx trr )) = (pure Branch) <*> (traverse  fn tl) <*> (fn trx) <*> (traverse  fn  ( Branch trl x trr  ) )

--instance Foldable Postorder  where
--    foldr f ini (PostO (Nil)) = ini 
--    foldr f ini (PostO (Branch (t1) i (t2))) =   foldr f (  foldr f  (f i ini) (PostO t2) ) (PostO t1) 


-- data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)


-- instance  Functor OddC where
--   fmap h (Un a) = Un (h a)
--   fmap h (Bi x y z) = Bi (h x) (h y) (fmap h z)

-- instance Foldable OddC where 
--    foldr f ini (Un a)  = f a ini 
--    foldr f ini (Bi x y z)  = f x (f y (foldr f ini z))

-- instance  Traversable OddC where 
--   traverse f (Un a)  = (pure Un) <*> f a  
--   traverse f (Bi x y z) =  (pure Bi) <*> f x <*> f y <*> (traverse f z)

-- cnt1 = Un 42
-- cnt3 = Bi 1 2 cnt1
-- cnt5 = Bi 3 4 cnt3
  

--   -- :: (a - h b) ->  Cmps a -> h Cmps b  
--   -- :: f ( ga -> h gb ) ->  f g a -> h (f (gb))
--   -- :: g ( a -> h b ) -> g a -> h g b 
--   --                         ga -> hgb          
--   traverse fn ( Cmps x ) =  (pure Cmps) <*> ( traverse (traverse fn) x )  



--data Triple a = Tr a a a  deriving (Eq,Show)
--data Result a = Ok a | Error String deriving (Eq,Show)

-- {-# LANGUAGE TypeOperators #-}

-- infixr 9 |.|
-- newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

-- instance (Functor f, Functor g) => Functor (f |.| g) where
--   fmap h (Cmps x) = Cmps $ fmap (fmap h) x

-- instance (Foldable f, Foldable g) => Foldable (f |.| g) where 
--   foldr f ini ( Cmps x ) = foldr (\ga b ->  foldr f b ga  ) ini x 
--  -- :: (a -> b -> b ) -> b -> cmps f (g a) -> b
--  -- f : ( g a -> b -> b) -> b -> f g a -> b
--  -- g : ( a -> b -> b ) -> b -> g a -> b   

-- --Applicative f =>
-- instance (Traversable f, Traversable g) => Traversable (f |.| g) where 
--   -- :: (a - h b) ->  Cmps a -> h Cmps b  
--   -- :: f ( ga -> h gb ) ->  f g a -> h (f (gb))
--   -- :: g ( a -> h b ) -> g a -> h g b 
--   --                         ga -> hgb          
--   traverse fn ( Cmps x ) =  (pure Cmps) <*> ( traverse (traverse fn) x )
 
-- h (f(gb)) ->  h Cmps f g b 


-- instance  (Functor f, Functor g ) => Functor (Cmps f g) where
--   fmap f (Cmps x) = Cmps $ fmap (fmap f) x 
  -- :: (Functor f, Functor g)=> (a -> b) -> cmps f (g a) -> cmps f (g b ) 
  -- (f) (ga -> gb )-> f (g a) -> f (g b ) 
  -- (g) (a -> b ) -> g a -> g b  
    
  


-- data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

-- instance Functor Tree where
--   fmap f (Branch (t1) x (t2)) = Branch (fmap f t1) (f x) (fmap f t2) 
--   fmap f (Nil) = Nil

-- instance Foldable Tree  where
--   foldr f ini ( Branch (t1) x (t2) ) =   f x $ foldr f ( foldr f ini t2 )  t1     
--   foldr f ini (Nil) = ini    

-- instance Traversable Tree where
--   traverse fn ( Branch (t1) x (t2) )  =  Branch <$> ( traverse fn t1 ) <*> (fn x) <*> ( traverse fn t2 )
--   traverse fn (Nil)  =  pure (Nil)



-- instance Functor Result where
--   fmap f (Ok x) = Ok $ f x
--   fmap f (Error xs) = Error xs

-- instance Foldable Result  where
--   foldr f ini (Ok x) = f x ini    
--   foldr f ini (Error xs) = ini    

-- instance Traversable Result where
--     traverse fn (Ok x)      = (pure Ok) <*> (fn x) 
--     traverse fn (Error xs)  = pure (Error xs)


--traverse:: Applicative f => (a -> f b) -> Triple a -> f Triple b
--  traverse fn (Tr x y z) = (pure Tr) <*>  (fn x) <*>  (fn y) <*>  (fn z)
--    ( (pure fn) <*> (Tr x y z))    -- (fn <$> pure x)  (fn <$> pure y) (fn <$> pure z)
--traverse' fn (Tr x y z) =  (pure Tr) <*>  (fn x) <*>  (fn y) <*>  (fn z)  --    ( (pure fn) <*> (Tr x y z))    --   (fn <$> pure y) (fn <$> pure z)
--data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

--tree = Branch (Branch Nil 1 Nil) 2 (Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil))

--t= (\x -> (show x,x)) <$> tree
--traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
--traverse2list fn =  foldr (\x ac -> (pure (:)) <*> (fn x) <*> ac   ) (pure [])  

-- infixr 9 |.|
-- newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 


-- instance (Foldable f, Foldable g)  => Foldable (f |.| g) where
--   foldr fn ini (Cmps x) =  foldr (flip $ foldr fn )  ini  x
  --foldr fn ini (Cmps x) =  foldr (\ga i -> foldr fn i ga) ini  x
--( a -> b ) -> b -> ( (|.|) f g a ) -> b
-- foldr fn ini (Cmps x) 
--    show
  -- fn:: (a -> d) 
  
-- fol fn ini (Cmps x) =  Cmps $ foldr ( foldr fn init ) mempty  x

--   f.g fold:: (a -> b -> b) -> b -> f g a -> b

--   f:fold :: ( g a -> g b -> g b ) -> g b ->  f g a  ->  g b  ::  ga -> gb -> gb -> gb
--                     f?               mem       x

--   f:fold :: ( g a -> b ->  b ) -> b ->  f g a  ->   b  ::  ga -> gb -> gb -> gb
--   \ga ->                                     

--   g:fold ::  (a -> b -> b ) -> b ->  (g a -> b)    ; foldr fn ini :: g a -> b

--                 fn             ini    

--   (a -> b -> b ) ?  (g a -> g b -> g b)

--   (g a -> b ) ? ( g a -> g b -> g b )

--fol fn ini (Cmps x) =  Cmps $ foldr ( foldr fn ini ) mempty  x



-- f foldr foo ini x


--(a -> b -> b) -> b 
--foldr fn init 



--fold foo ini x    
--foo: ( g a -> g b  )

    -- g:: (a -> b)
    -- f:: (b -> c) 
    -- f . g :: (a -> c )
    --( c -> d ) -> d -> 

    -- fold f'   i g :: ( b -> b' ) -> b' -> ( a -> b )  -> b' 
    -- fold f''  i f :: ( c -> c' ) -> c' -> ( b -> c )  -> c'   


   --   foldr f mempty g :: ( b -> c) -> mempty::c -> (a -> b )  -> c 

  




--{-# LANGUAGE TypeOperators #-}

-- import Data.Monoid
-- import Data.Foldable

-- mkEndo :: Foldable t => t (a -> a) ->  Endo a
-- mkEndo x = Endo $ foldr (.) id x    
  
  
  
  -- import Data.Monoid
-- import Data.Char

-- data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)



-- newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show) 
-- newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
-- newtype Leveclorder a = LevelO (Tree a)    deriving (Eq, Show)

-- instance Foldable Preorder  where
--    foldr f ini (PreO (Nil)) = ini 
--    foldr f ini (PreO (Branch (t1) i (t2))) =  f i $ foldr f ( foldr f ini (PreO t2) ) (PreO t1)   

-- instance Foldable Postorder  where
--    foldr f ini (PostO (Nil)) = ini 
--    foldr f ini (PostO (Branch (t1) i (t2))) =   foldr f (  foldr f  (f i ini) (PostO t2) ) (PostO t1) 

-- instance Foldable Leveclorder  where
--   foldr f ini (LevelO t ) = foldrs f ini [t] where 
--                         foldrs f ini [] = ini
--                         foldrs f ini xs = foldr (foo f) ( foldrs f ini (chld xs) ) xs where 
--                           foo f (Nil) a  =  a
--                           foo f (Branch (t1) i (t2)) a  = f i a
--                           chld [] = []
--                           chld ((Nil):xs) = child xs
--                           chld ((Branch (t1) i (t2)):xs)  =  t1:t2:child xs




-- -- child:: Tree a -> [Tree a]  
-- -- child (Nil) = []
-- -- child (Branch (t1) i (t2))  =  t1:t2:child xs

 

-- child:: [Tree a] -> [Tree a]  
-- child [] = []
-- child ((Nil):xs) = child xs
-- child ((Branch (t1) i (t2)):xs)  =  t1:t2:child xs

-- --instance Foldable Leveclorder  where
-- --  foldr f ini (LevelO (Nil)) = [] -- ini 
-- --foldr f ini (LevelO (Branch (t1) i (t2))) =  child [(Branch (t1) i (t2))]
-- fu f (Branch (t1) i (t2)) a  = f i a
-- fol f ini (LevelO (Branch (t1) i (t2))) = ( foldr (fu f) ini (child [(Branch (t1) i (t2))]) ) 

-- fuu:: (a -> b -> b) -> (Tree a) -> b -> b  
-- fuu f (Nil) a  =  a
-- fuu f (Branch (t1) i (t2)) a  = f i a

-- foldrs:: (a -> b -> b ) -> b -> [Tree a] -> b
-- foldrs f ini [] = ini
-- foldrs f ini xs = foldr (foo f) ( foldrs f ini (chld xs) ) xs where 
--                         foo f (Nil) a  =  a
--                         foo f (Branch (t1) i (t2)) a  = f i a
--                         chld [] = []
--                         chld ((Nil):xs) = child xs
--                         chld ((Branch (t1) i (t2)):xs)  =  t1:t2:child xs




-- childs:: [Tree a] -> [Tree a]  
-- childs [] = []
-- childs xs = xs ++ (childs (child xs))

-- folll:: (a -> b -> b ) -> b -> [Tree a] -> b
-- folll f ini [] = ini
-- folll f ini xs = foldr (fuu f) ini (childs xs)
 

-- chil:: [Tree a] -> [Tree a]  
-- chil x = foldr (\a  ->   ) [] x  


-- instance Foldable Leveclorder  where
--    child (LevelO (Nil)) = []
--    child (LevelO (Branch (t1) i (t2))) =   f i $

--    foldr f ini (LevelO (Nil)) = ini 
--    foldr f ini (LevelO (Branch (t1) i (t2))) =   f i $

    
   

-- tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)

-- tre2 = Branch (Branch Nil 2  Nil) 1 (Branch Nil 3 Nil)

-- infixr 9 |.|

-- newtype (|.|) f g a = Cmps {getCmps :: f (g a)} deriving (Eq, Show)

-- instance (Functor f, Functor g) => Functor (f |.| g) where
--   fmap h (Cmps x) = Cmps $ fmap (fmap h) x

-- instance (Applicative f, Applicative g) => Applicative (f |.| g) where
--   pure = Cmps . pure . pure
--   (<*>) = undefined


-- unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
-- unCmps3 (Cmps x) = fmap (getCmps) x

-- unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
-- unCmps4  (Cmps x) = fmap unCmps3 x

-- fmap h (Cmps x) = Cmps $ fmap (fmap h) x

-- fmap h2 (fmap h1 (Cmps x)) = %def fmap (Cmps) (h1)

-- fmap h2 (Cmps $ fmap (fmap h1) x) = %def fmap (Cmps) (h2)

-- Cmps $ fmap (fmap h2) (fmap (fmap h1) x) = % def (.)

-- Cmps $ (fmap (fmap h2) . fmap (fmap h1)) x = % (2) fmap

-- Cmps $ fmap (fmap h2 . fmap h1) x = % (2) fmap

-- Cmps $ fmap (fmap (h2 . h1)) x = %def fmap (Cmps)

-- fmap (h2 .  h1) (Cmps x)


-- fmap (f . g) = fmap f . fmap g
-- fmap (f . g) x = fmap f (fmap g x)

-- ( h1 : a -> b )
-- (fmap h1) x = 

-- newtype Cmps f g a = Cmps { getCmps :: f (g a) }                    -- def Cmps

-- instance (Functor f, Functor g ) => Functor (Cmps f g ) where
--    fmap fn (Cmps x) = Cmps $ fmap (fmap fn) x                       -- (1)  def Cmps Functor

-- fmap (f . g) x = fmap f (fmap g x) = ( (fmap f) . (fmap g) ) x      -- (2)  Functor's second law 

-- -- докажем что : fmap h2 (fmap h1 (Cmps x)) = fmap (h2 . h1) (Cmps x) :

-- fmap h2 (fmap h1 (Cmps x))                          -- (1)     => fmap h1 (Cmps x) = Cmps $ fmap (fmap h1) x
-- == fmap h2 ( Cmps $ fmap (fmap h1) x )              -- (1)     => fmap h2 (Cmps x) = Cmps $ fmap (fmap h2) x
-- == Cmps $ fmap (fmap h2)  (fmap (fmap h1) x)        -- def (.) => f(g x) = (f . g) x
-- == Cmps $ (fmap (fmap h2)) . (fmap (fmap h1)) x     -- in (1) f - Functor & (2) ( (fmap f) . (fmap g) ) x = fmap (f . g) x =>
-- == Cmps $ fmap ( (fmap h2) . (fmap h1) ) x          -- in (1) g - Functor & (2) ( (fmap f) . (fmap g) ) x = fmap (f . g) x =>
-- == Cmps $ fmap ( fmap (h1 . h2) ) x                 -- (1) => 
-- == fmap (h2 . h1) (Cmps x)                          -- Q.E.D.


-- fmap h2 (fmap h1 (Cmps x))                          
-- -- (1)     => fmap h1 (Cmps x) = Cmps $ fmap (fmap h1) x
-- == fmap h2 ( Cmps $ fmap (fmap h1) x )              
-- -- (1)     => fmap h2 (Cmps x) = Cmps $ fmap (fmap h2) x
-- == Cmps $ fmap (fmap h2)  (fmap (fmap h1) x)        
-- -- def (.) => f(g x) = (f . g) x
-- == Cmps $ (fmap (fmap h2)) . (fmap (fmap h1)) x     
-- -- in (1) f - Functor & (2) ( (fmap f) . (fmap g) ) x = fmap (f . g) x =>
-- == Cmps $ fmap ( (fmap h2) . (fmap h1) ) x          
-- -- in (1) g - Functor & (2) ( (fmap f) . (fmap g) ) x = fmap (f . g) x =>
-- == Cmps $ fmap ( fmap (h1 . h2) ) x                 
-- -- (1) => 
-- == fmap (h2 . h1) (Cmps x)                          
-- -- Q.E.D.
 
--  fmap (h2 . h1) (Cmps x) 
--   == fmap f (fmap g (Cmps x) )
--   == 

-- fmap h2 (fmap h1 (Cmps x))
-- == fmap h2 (   Cmps $ fmap (fmap h1) x  )   -- (1)
-- == fmap h2 (   )
-- == Cmps $ fmap (fmap h2) (fmap (fmap h1) x )




-- fmap (h2 . h1) (Cmps x)
--  == Cmps $ fmap ( fmap (h2 . h1) ) x 
--  == Cmps $ fmap ( fmap h2 (fmap h1 ) ) x 
 


-- newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) } 
--   deriving (Eq,Show) 

-- instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
--   --fmap ( a -> b); x  f (g (h a)) fx:: g(h a) -> g(h b)
--   fmap fn (Cmps3 x) = Cmps3 $ fmap (fmap (fmap fn)) x 


-- {-# LANGUAGE TypeOperators #-}

-- infixr 9 |.|
-- newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq,Show)

-- type A   = ((,) Integer |.| (,) Char) Bool
-- type B t = ((,,) Bool (t -> t) |.| Either String) Int
-- type C   = (|.|) ((->) Bool) ((->) Integer) Integer

-- a :: A
-- a = Cmps (0::Integer,('A', True) )

-- b :: B t
-- b = Cmps (True, \t->t, Left "")

-- c :: C
-- c  = Cmps (\ b n -> n)



-- import Data.Char
-- import Control.Applicative
-- --import Text.Parsec

-- newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

-- satisfyE :: (Char -> Bool) -> PrsE Char
-- satisfyE f = PrsE fn where 
--     fn "" = Left "unexpected end of input"
--     fn (c:cs) | f c =  Right (c, cs)
--               | otherwise = Left $ "unexpecd "++[c]
   

-- charE :: Char -> PrsE Char
-- charE c = satisfyE (== c)

-- instance Functor PrsE where
--   fmap f px = PrsE fun where 
--        fun s = do (x', s') <- runPrsE px s
--                   return ( f x', s') 
           
           
           
--         --    case runPrsE px s of  
--         --             Left e -> Left e
--         --             Right (a, s') -> Right (f a, s)

-- instance Applicative PrsE where
--   pure x =  PrsE (\s -> return (x,s) )  
--   (<*>) pf pv = PrsE fun where 
--                     fun s = do (f', s') <- runPrsE pf s 
--                                runPrsE (f' <$> pv) s'   

-- newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

-- instance Functor Prs where
--    fmap f p = Prs fun where 
--       fun s = case runPrs p s of
--                   Just (a',s') -> Just (f a', s' )
--                   Nothing -> Nothing

-- instance Applicative Prs where
--   pure x = Prs (\s -> Just (x,s) )
--   (<*>) pf pv = Prs fun where 
--       fun s = do (f', s') <- runPrs pf s  
--                  (runPrs (fmap f' pv) s')   

-- satisfy :: (Char -> Bool) -> Prs Char
-- satisfy f = Prs fn where 
--     fn "" = Nothing
--     fn (c:cs) | f c =  Just (c, cs)
--               | otherwise = Nothing

-- char :: Char -> Prs Char
-- char c = satisfy (== c)

-- dig :: Prs Char
-- dig = satisfy (isDigit)



-- -- char :: Prs Char
-- -- char = Prs f where 
-- --   f ""   = Nothing
-- --   f (c:cs) = Just (c,cs)                               

-- instance Alternative Prs where
--   empty = Prs $ \s -> Nothing
--   (<|>) px py = Prs fun where 
--          fun s = case runPrs px s of 
--                      Nothing -> runPrs py s 
--                      Just(a', s') -> Just(a', s')

-- --end ::  Prs a -> Prs [a]
-- --end px = Prs fun where 
--             -- fun s = case runPrs px s of 
--             --     Just(a', s') -> if [a'] == "" then Nothing else Just([a'], s')

-- --end :: Char -> String -> String
-- --end s sx =

-- --addCharToInt :: Char -> Int -> Int
-- --addCharToInt c i = i * 10 + digitToInt c

-- addCharToInt :: Char -> Int -> Int
-- addCharToInt c i =   if i < 0 then  digitToInt c  else digitToInt c * (10 ^ length ((show i))) + i   



-- manyI :: Prs Char -> Prs Int
-- manyI px =   addCharToInt <$> (px) <*> (manyI px <|> (pure (-1)) ) 

-- manyI2 :: Prs Int
-- manyI2 = addCharToInt <$> ( satisfy (isDigit) ) <*> (manyI2 <|> (pure (-1)) ) 

-- manyI3 :: Prs Int
-- manyI3 = (\ c i ->  if i < 0 then  digitToInt c  else digitToInt c * (10 ^ length ((show i))) + i ) <$> ( satisfy (isDigit) ) <*> (manyI2 <|> (pure (-1)) ) 


-- --nat :: Prs Int
-- --nat = (\ c i ->  if i < 0 then  digitToInt c  else digitToInt c * (10 ^ length ((show i))) + i ) <$> ( digit ) <*> (nat <|> (pure (-1)) ) 


-- digit :: Prs Char
-- digit = Prs fn where 
--     fn "" = Nothing
--     fn (c:cs) | isDigit c =  Just (c, cs)
--               | otherwise = Nothing

-- int :: Prs Int
-- int = Prs fn where 
--     fn "" = Nothing
--     fn cs = Just (read cs,"")

-- many1 :: Prs a -> Prs [a]
-- many1 px =  (:) <$> (px ) <*> (many1 px <|> (pure [])) 

-- manyStr::  Prs String
-- manyStr  =  (:) <$> (digit) <*> ( manyStr <|> (pure []))    

-- nutF :: Prs String -> Prs Int 
-- nutF px =  Prs fun where 
--      fun s = case runPrs px s of 
--                      Nothing -> Nothing
--                      Just(a', s') -> Just( read a' , s')


-- nutF2 :: Prs Int 
-- nutF2 = Prs fun where 
--        fun s = case runPrs manyStr s of 
--                       Nothing -> Nothing
--                       Just(a', s') -> Just( read a' , s')


-- nutF3 :: Prs Int 
-- nutF3 = Prs fun where 
--         fun s = case runPrs ((:) <$> (digit) <*> ( manyStr <|> (pure []))) s of 
--                       Nothing -> Nothing
--                       Just(a', s') -> Just( read a' , s')


-- nat :: Prs Int
-- nat =  nutF3 
-- --nat =  nutF $ many1 digit

-- mult :: Prs Int
-- mult = (*) <$> nat <* char '*' <*> nat


-- --


--many1 px =  ( : "" ) <$> (px) <|> (pure [])


-- <*> (many1 px) -- <|> (pure [])                    
       


    -- <*> many1 px 
    -- ((>>=) .(:) )
    -- <|> empty
    --Prs fun where 
    --fun s = case runPrs px s of 

    
    
    --(:) <$> (char x) <*> many1 x 

--import Text.Char

-- newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

-- instance Functor Prs where
--    fmap f p = Prs fun where 
--       fun s = case runPrs p s of
--                   Just (a',s') -> Just (f a', s' )
--                   Nothing -> Nothing

-- anyChr :: Prs Char
-- anyChr = Prs f where 
--   f ""   = Nothing
--   f (c:cs) = Just (c,cs)

-- instance Applicative Prs where
--   pure x = Prs (\s -> Just (x,s) )
--   (<*>) pf pv = Prs fun where 
--       fun s = do (f', s') <- runPrs pf s  
--                  (runPrs (fmap f' pv) s')   
          
          
        --   case runPrs pf s of
        --             Nothing -> Nothing
        --             Just (f',s') -> runPrs (fmap f' pv) s' 
                        
                        
                        -- fmap f' pv 
                                                                



-- import Text.Parsec

-- ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
-- ignoreBraces p1 p2 p3 = p1 *> p3 <* p2

-- test = ignoreBraces (string "[[") (string "]]") (many1 letter)


-- import Text.Parsec

-- getList :: Parsec String u [String]
-- getList =  many1 digit `sepBy` char ';' 


-- import Control.Applicative ((<**>),ZipList(..), liftA, liftA2)
-- import Data.Maybe


-- --(<**>) :: Applicative f => f a -> f (a -> b) -> f b
-- --(<**>) = liftA2 (flip ($))

-- infixl 4 <*?>
-- (<*?>) :: Applicative f => f a -> f (a -> b) -> f b
-- (<*?>) = flip (<*>)

-- infixl 4 <*?>
-- (<*?>) :: Applicative f => f a -> f (a -> b) -> f b
-- (<*?>) x f  = liftA2 (flip ($)) x f 


--exprMaybe :: (forall a b . Maybe a -> Maybe (a -> b) -> Maybe b) -> Maybe Int
-- exprMaybe op = 
--   let (<??>) = op 
--       infixl 4 <??> 
--   in Just 5 <??> Just (+2) -- place for counterexample

-- exprList :: (forall a b . [a] -> [a -> b] -> [b]) -> [Int]
-- exprList op = 
--    let (<??>) = op 
--       infixl 4 <??> 
--    in [1,2] <??> [(+3),(*4)] -- place for counterexample

-- exprZipList :: (forall a b . ZipList a -> ZipList (a -> b) -> ZipList b) -> ZipList Int
-- exprZipList op = 
--   let (<??>) = op 
--       infixl 4 <??> 
--   in ZipList [1,2] <??> ZipList [(+3),(+4)]  -- place for counterexample

-- exprEither :: (forall a b . Either String a -> Either String (a -> b) -> Either String b) -> Either String Int
-- exprEither op = 
--   let (<??>) = op 
--       infixl 4 <??> 
--   in Left "AA" <??> Right (+1)  -- place for counterexample

-- exprPair :: (forall a b . (String,a) -> (String,a -> b) -> (String,b)) -> (String,Int)
-- exprPair op = 
--   let (<??>) = op 
--       infixl 4 <??> 
--  in ("AA", 3) <??> ("B",(+1))  -- place for counterexample

-- exprEnv :: (forall a b . (String -> a) -> (String -> (a -> b)) -> (String -> b)) -> (String -> Int)
-- exprEnv op = 
--   let (<??>) = op 
--       infixl 4 <??> 
--   in length <??> (\_ -> (+5))  -- place for counterexample


-- divideList :: Fractional a => [a] -> a
-- divideList []     = 1
-- divideList (x:xs) = (/) x (divideList xs)

-- --newType Log 

-- divideList' :: (Show a, Fractional a) => [a] -> (String,a)
-- --divideList' []     = ("1.0" ,1):t 
-- --divideList' (x:xs) = (/) <$> ("<-" ++(show x) ++"/" , x ) <*> divideList' xs

-- divideList' []     = (mempty ,1)
-- divideList' (x:xs) = (/) <$> ( ( mappend (show x)), x ) <*> divideList' xs


--divideList' [3,4,5]
--("<-3.0/<-4.0/<-5.0/1.0",3.75)

-- import Data.List

-- x1s = [1,2,3]
-- x2s = [4,5,6]
-- x3s = [7,8,9]
-- x4s = [10,11,12]

-- newtype ZipList a = ZipList { getZipList::[a] }  
--     deriving Show

-- instance Functor ZipList  where
--     fmap f (ZipList xs)  = ZipList ( map f xs )
    

-- instance Applicative ZipList where 
--     pure xs = ZipList [xs]
--     ZipList gs <*> ZipList xs = ZipList ( zipWith ($) gs xs )


-- (>*<) fs xs = getZipList $ (ZipList fs) <*> (ZipList xs) 
-- (>$<) f xs  = getZipList $ fmap f (ZipList xs) 


--(\a b -> 2*a+3*b) >$< x1s >*< x2s

--getZipList $ (\a b -> 2*a+3*b) <$> ZipList x1s <*> ZipList x2s




--data Triple a = Tr a a a  deriving (Eq,Show)

--[]]


-- newtype Arr  a = Arr {getArr :: a}
-- newtype Arr1 e1 a = Arr1 {getArr1 :: e1 -> a}
-- newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
-- newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

-- fo f (Arr1 f2) = Arr1 $ (f . f2)

-- instance Functor (Arr) where
--    --fmap:: (a -> b) -> Arr a -> Arr b
--    fmap f (Arr x ) =  Arr $ (f x)


-- instance Functor (Arr1 e1) where
--    --fmap:: (a -> b) -> Arr1 e1 a -> Arr1 e1 b
--    fmap f (Arr1 f2) =  Arr1 $ (f . f2)

-- fo2 f (Arr2 f2 ) =  Arr2 $ \e1 -> \e2 -> f $ f2 e1 e2    

-- instance Functor (Arr2 e1 e2) where
--    --fmap:: (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b  
--    fmap f (Arr2 f2 ) = Arr2 $ \e1 e2 -> f $ f2 e1 e2  

-- --getArr2 (+1) 

-- instance Functor (Arr3 e1 e2 e3) where
--    fmap f (Arr3 f2 ) = Arr3 $ \e1 e2 e3 -> f $ f2 e1 e2 e3  



-- import Control.Monad.Writer 
-- import Control.Monad.Reader 
-- import Control.Monad.State 
-- import Data.List (intercalate)

-- data Tree a = Leaf a | Fork (Tree a) a (Tree a)




-- data TreeDir = Troot | Tleft | Tright

-- instance Show a => Show (Tree a) where
--     show = showTree ("", Troot)
--         where
--             bi = replicate 8 ' '        -- Base Indent
--             bb = '|' : replicate 7 '-'  -- Base Branch
--             showVal i x = i ++ bb ++ "(" ++ show x ++ ")"

--             showTree (i,_)   (Leaf x)     = showVal i x
--             showTree (i,dir) (Fork l x r) =
--                 let (lSt,rSt) = case dir of
--                         Troot  -> ( (' ':bi, Tleft), (' ':bi, Tright) )
--                         Tleft  -> ( (i ++ ' ':bi, Tleft), (i ++ '|':bi, Tright) )
--                         Tright -> ( (i ++ '|':bi, Tleft), (i ++ ' ':bi, Tright) )
--                     lStr = showTree lSt l
--                     rStr = showTree rSt r
--                 in
--                     intercalate "\n" [lStr, showVal i x, rStr]

-- --numberTree :: Tree () -> Tree Integer
-- --numberTree tree = 


-- --treeHandle :: Tree ( () | Integer )  ->

-- tree = (Fork (Leaf ()) () (Leaf ()))

-- treei = (Fork (Leaf 0) 0 (Leaf 0))

-- trei =  Fork treei 0 treei 

-- --data UnInt =  () 

-- ff:: Integer ->  Tree Integer -> (Integer , (Tree Integer) )  
-- ff i (Leaf x) = ( i+1 , (Leaf i))   
-- ff i (Fork t1 _ t2 ) = (i+4,  (Fork (Leaf i ) i (Leaf i)))

-- -- fff:: Integer ->  Tree Integer -> State(Integer , (Tree Integer) )  
-- -- -- ffF i  (Leaf x) = ( i+1 , (Leaf i))   
-- -- -- ff i (Fork t1 _ t2 ) = (i+4,  (Fork (Leaf i ) i (Leaf i)))
-- -- fff i = state( \s -> s | (Leaf x) =  ( i+1 , (Leaf i))
-- --                        | (Fork t1 _ t2 ) = (i+4,  (Fork (Leaf i ) i (Leaf i)))
-- -- )


-- treeStep ::Integer ->  State (Tree Integer ) Integer
-- treeStep i = do
--     s  <- get
--     i' <- case s of (Leaf x) -> state (\s'-> (i+1 , Leaf $ i))   
--                     (Fork t1 x t2 ) ->  let (i1 ,t1') = ( runState (treeStep i ) ) t1 in 
--                                         let (i2',t2') = ( runState (treeStep (i1 + 1) ) ) t2 in    
--                                         state (\s'-> (i2' , Fork t1' i1 t2')) 
--     return i'                                    

-- stepTree:: Tree () -> State (Integer) (Tree Integer) 
-- stepTree t = do
--     i <- get
--     t' <- case t of (Leaf ()) -> state (\s'-> ( (Leaf i),i+1) )  
--                     (Fork t1 x t2 ) -> let (t1',i1 ) = ( runState (stepTree t1 ) ) i in 
--                                        let (t2',i2 ) = ( runState (stepTree t2 ) ) (i1 + 1) in    
--                                         state (\s'-> ((Fork t1' i1 t2'),i2)) 
--     return t'

-- numberTree :: Tree () -> Tree Integer
-- numberTree tree = fst $ (runState (stepTree tree)) 1

-- rn1::Tree () -> Tree Integer
-- rn1 t  =   fst $ (runState (stepTree t)) 1


-- -- treeStep ::Integer ->  State (Tree Integer) Integer
-- -- --treeStep i = state ( \s -> (i+1,s))
-- -- treeStep i = do
-- --     s  <- get
-- --     i' <- case s of (Leaf x) -> state (\s'-> (i+1 , Leaf $ i))   
-- --                     (Fork t1 x t2 ) ->  let (i1 ,t1') = ( runState (treeStep i ) ) t1 in 
-- --                                         let (i2',t2') = ( runState (treeStep (i1 + 1) ) ) t2 in    
-- --                                         state (\s'-> (i2' , Fork t1' i1 t2')) 
                        
--                         --treeStep 
--                         -- do 
--                         --                     i'' <- treeStep i
--                         --                     s'' <- get
--                         --                     -- i''' <- treeStep t2
--                         --                     -- s''' <- get
--                         --                     -- put $ Fork s'' i'' s'''    
--                         --                     return i''
--     --return $ i'
-- --treeStep i = state (\s -> ff i s ) 
-- --treeStep i = do
--     -- s <- get
--     -- -- put $ case s of (Leaf x) -> Leaf $ i+1  
--     -- --                 (Fork t1 _ t2 ) -> Leaf $ i+1 
--     -- (i,s1) <-  state (\s' -> (i,s') )
--     -- --case s of (Leaf x) -> Leaf $ i+1  
--     -- --                 (Fork t1 _ t2 ) -> Leaf $ i+1 


--     -- return i+1 
--     -- --case s of (Leaf x) -> i+1  
--     -- --                   (Fork t1 _ t2 ) -> i + 2 

                         
    
-- -- //treeStep (Leaf ()) =  

-- --     return 1  




-- rn =    (runState (treeStep 1)) 


-- --        r <- Tree Fork (Tree a) a (Tree a) = 3
-- --             Tree Leaf x = 2    
-- --        return (r) 

-- --exc = ( runState   treeStep )   tree

-- foo :: (Tree a) -> Integer
-- foo (Leaf x) = 2 
-- foo (Fork t1 x t2)  = 3  




-- fibStep :: State (Integer, Integer) ()
-- fibStep = do 
--     (x,y) <- get 
--     put (y, y+x )
--     return ()


-- execStateN :: Int -> State s a -> s -> s
-- execStateN n m = execState( replicateM n m) 


-- writerToState :: Monoid w => Writer w a -> State w a
-- --writerToState m = let (v,l) = runWriter $ m in state ( \s -> (v, s <> l ) ) -- Not in scope: ‘<>’
-- writerToState m = let (v,l) = runWriter $ m in state ( \s -> (v, ( mappend s l) ) ) -- Not in scope: ‘<>’
-- --writerToState m = let (v,l) = runWriter $ m in  state ( \s -> (v, s)) >>=  



-- --writerToState :: Monoid w => Writer w a -> w
-- --writerToState m = let (v,l) = runWriter $ m in  mappend( mempty,"e" )



        
--     -- do 
--     --     put l
--     --     return v


-- --     let f = do 
-- --         f <- runWriter m 
-- --         return f
-- --     in state $ \e -> (f e, e) 

-- wT :: Monoid w => Writer w a -> State String String
-- wT m = state $ \x -> ("3",x)

-- wT ::  a -> Writer String a 
-- wT a = do 
--     tell ("dddd")
--     tell ("wwwww")
--     return a

-- wT1 =  let (x,y) = runWriter $ wT 3 in (x,y)
         

-- wT2 ::  a -> State String a 
-- wT2 a = do 
--     return a    


-- greeter :: Reader String String
-- greeter = do
--     name <- ask
--     return ("hello, " ++ name ++ "!")

-- my :: Reader String String
-- my = return "rr"

-- myI :: Reader Int String
-- myI = return "int"

-- tu1 :: Reader [Int] Int
-- tu1 = do
--     x <- ask
--     return $ length x

-- br :: a -> Reader String a 
-- br x = reader $ \e -> x

-- readerToState :: Reader r a -> State r a
-- readerToState m = let f = runReader m in 
--     reader $ \e -> (f e, e) 
    
--readerToState :: Reader r a -> b
--readerToState m = runReader m  


-- readerToState :: Reader r a -> State r a
-- readerToState m = 
--     let f = do 
--         f <- runReader m 
--         return f
--     in state $ \e -> (f e, e)     

-- rSt :: a -> State r a
-- rSt a = return a


-- st =  runState (return 1)

-- greeter :: State String String
-- greeter = do
--     name <- get
--     put "tintin"
--     return ("hello, " ++ name ++ "!")


--runState greeter $ "adit"



--runReader greeter $ "adit"


--readerToState :: Reader r a -> State r a
--readerToState m = ?

--type Shopping = Writer (Sum Integer) ()
-- type Shopping = Writer ( Sum Integer, [String] ) ()

-- purchase :: String -> Integer -> Shopping
-- purchase item cost =  writer (() , (Sum cost, [item])  ) 

-- p1 = purchase "Jeans"  19200

-- p2 = purchase "Water"     180

-- -- -- p3 = p1 >>= 

-- a = do
--         x <- [1, 3, 4, 5]
--         y <- [ x + 1 ]
--         return y
        
-- a1 =  [1, 3, 4, 5] >>= (\x -> [x+1]  )
         
         

-- b = do 
--          Sum 1 
--          Sum 2

-- total :: Shopping -> Integer
-- total =  getSum . fst . snd . runWriter  

-- items :: Shopping -> [String]
-- items = snd . snd . runWriter
-- --s . snd . snd 
-- --items s = snd $ snd $ runWriter s  

-- shopping1 :: Shopping
-- shopping1 = do
--     purchase "Jeans"   19200
--     purchase "Water"     180
--     purchase "Lettuce"   328


-- p = do
--         Product 2
--         x <- Product 3
--         return x
        
    


-- type User = String
-- type Password = String
-- type UsersTable = [(User, Password)]

-- e = [("user", "123456"), ("x", "hi"), ("root", "123456")]

-- -- foo:: UsersTable -> [User]
-- -- foo z = do 
-- --     x <- z
-- --     True <- return $ (snd x) == "123456" 
-- --     return $ fst x 

-- --foo = head 

-- usersWithBadPasswords :: Reader UsersTable [User]
-- usersWithBadPasswords = do 
--         x <- ask
--         return $ do 
--                 y <- x
--                 True <- return $ (snd y) == "123456" 
--                 return $ fst y 



-- data Reader r a = Reader { runReader :: (r -> a) }

-- instance Monad (Reader r) where
--   return x = Reader $ \_ -> x
--   m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r


--local' :: (r -> r') -> Reader r' a -> Reader r a
-- local' f m = \e -> runReader m (f e)    
--local' f m = Reader $ \e -> runReader m (f e)


-- import System.Directory  

-- hasSbs2 x sl = fst $ foldr (\i (b,s) -> ( b || (take (length x) (i:s) ) == x , i:s) ) (False, "" ) sl 

-- dirs = getDirectoryContents "."
-- dir = ["thesis.txt", "kitten.jpg", "hello.world", "linux_in_nutshell.pdf"]
-- dr = ["Hello.hs","h2.hs","..","."]



-- foo sb ls = do
--     x <- ls
--     True <- return ( fst $ foldr (\i (b,s) -> ( b || (take (length sb) (i:s) ) == sb , i:s) ) (False, "" )  x)
--     return (x)




-- >> removeFile x 

-- fo f = putStrLn ("Removing file: " ++ f ) >> removeFile f 


-- --main' :: IO ()
-- main' = do
--     putStr "Substring: "
--     nm <- getLine 
--     if nm == "" then
--         putStrLn "Canceled"
--     else do
--         fls <- getDirectoryContents "." 
--         fl  <- return ( isSub nm fls )
--         --fl2 <- foldl (\a x -> a >> fo x ) (putStr "") fl  
--         fl2 <- foldl (\a x -> a >> putStrLn ("Removing file: " ++ x ) >> removeFile x  ) (putStr "") fl  
--         return fl2
--         where isSub sb ls = do
--                         x <- ls
--                         True <- return ( fst $ foldr (\i (b,s) -> ( b || (take (length sb) (i:s) ) == sb , i:s) ) (False, "" )  x)
--                         return (x)       
    
    --map fo fl  
    --return fl
    --fl2 <-  map (\x ->  putStrLn (" Removing file: " ++ x ) ) fl  
    --fl2 <- return (  foldl (\_ x ->  putStrLn ("Removing file: " ++ x)  ) (putStr "aaa") fl )
    

-- tst = do 
--    putStr "Substring: "
--    z <- return ["a","b","c"] 
   
--    return ( putStrLn z)



    -- if nm == "" then
    --     -- putStrLn "Canceled"
    --     return "Canceled"
    -- else do 
    --     fls <- getDirectoryContents "." 
    --     fl  <- return ( foo nm fls )
    --     return fl
        -- do        
        --fls <- getDirectoryContents "." 
        --fl  <- return ( foo nm fls )
        --return (fl)
        --foldl (\_ x ->  putStrLn ("Removing file: " ++ x)  ) (putStr "") fl
    

--hs x s | length s > length   =  

-- hs x s = (take (length x) s) == x      

--hs1 x c a = a >>= ( \j -> Just (c:j) ) >>= (\j -> if ((take (length x) j) == x ) then Nothing else (Just j) )

-- hasSbs x s  = foldr (foo x) (Just "") s 
--     where foo x c a = a >>= ( \j -> Just (c:j) ) >>= (\j -> if ((take (length x) j) == x ) then Nothing else (Just j) )

-- hasSbs2::  String -> String -> (Bool, String ) 



--(\i (b,s) -> ( ( b || (take (length x) i:s ) == x ), i:s ) )

--hs2 :: String -> Char -> Maybe String -> Maybe String
-- hs2 x c a =  do 
--     z <- a 
--     z <- (\j -> Just (c:j) ) z
--     z <- (\j -> if (hs x j) then Nothing else (Just j) ) z
--     return z

-- h = (Just 2) >>= (\x -> (Just x) ) >>= (\x -> (Just (x+x) ) ) 

-- hd = do 
--     --d <- Just 2
--     --l <- return 2
--     -- z <- (\x -> (Just x) ) d
--     d <- Just 2
--     z <- Just d
--     return z
    

    



-- hs2 x c a = do
--     t <- a
--     l <- ((return c):t)
--     return l

    -- r <- "rrrr" --(hs x l)  -- then "ee" else "ddd"
    --return l 

--     Just (c:j) 
--     z <- if (hs x j) then Nothing else (Just j)  
--     return z





-- f = do 
--    x <- [1..2]  
--    y <- [2..3]
--    return (x,y)

--[1..2]  >>= (\x -> [2..3])    

--getDirectoryContents "."

-- main' :: IO ()
-- main' = do 
--     name <- getName
--     putStrLn $ "Hi, "++ name ++"!"
--     where 
--         getName = do
--             putStrLn "What is your name?"
--             putStr "Name: "
--             nm <- getLine 
--             ret <- if nm /= "" then return nm else getName
--             return ret


-- pythagoreanTriple x = do 
--     c <- [1..x]
--     b <- [1..x]
--     a <- [1.. (b-1)]
--     True <- return ( (a ^ 2) + (b ^ 2) ==  c ^ 2 )
--     return (a,b,c)


-- --pythagoreanTriple :: Int -> [(Int, Int, Int)]
-- -- pythagoreanTriple x = do 
-- --     c <- [1..x]

-- --     b <- [1..c^2 ]
-- --     a <- [1.. (b-1)]
-- --     True <- return ( (a ^ 2) + (b ^ 2) ==  c ^ 2 )
-- --     return (a,b,c)

-- t1 = pythagoreanTriple 4



-- data Board = A | B | C | D deriving (Eq,Show)

-- nextPositions :: Board -> [Board]
-- nextPositions b =  [b,A,B,C,b]

-- --nPos::Board -> Int -> [Board] 

-- nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
-- nextPositionsN b n pred = do  
--     x <- nPos b n 
--     y <- if pred x then [x] else []
--     return y
--     where 
--         nPos b n = do 
--             x <- if n < 0 then  [] else ( if n == 0 then [b] else nextPositions b ) 
--             y <- if n > 1 then  ( nPos x (n-1) )  else [x]
--             return y


-- -- nPos::Board -> Int -> [Board] 
-- -- nPos b n = do 
-- --     x <- if n < 0 then  [] else ( if n == 0 then [b] else nextPositions b ) 
-- --     y <- if n > 1 then  ( nPos x (n-1))  else [x]
-- --     return y

-- d1 = nextPositionsN A 0 (\ x-> if x == A then False else True )
-- d2 = nextPositionsN A 1 (\ x-> if x == A then False else True )
-- d3 = nextPositionsN A 2 (\ x-> if x == A then False else True )
-- d4 = nextPositionsN A 3 (\ x-> if x == A then False else True )


-- -- t  = nPos A (-1)         
-- -- t0 = nPos A 0         
-- -- t1 = nPos A 1
-- -- t2 = nPos A 2
-- -- t3 = nPos A 3

-- l1 = nextPositions A
-- l2 = l1 >>= nextPositions 
-- l3 = l2 >>= nextPositions 


-- --        [1..n] >>= (\x -> nextPositions x ) 


-- nPos' b n = foldl ( \a _ -> a >>= nextPositions ) (return b) [1..n]

-- r0 = nPos' A 0        
-- r1 = nPos' A 1
-- r2 = nPos' A 2
-- r3 = nPos' A 3



-- nPos  b n  | n < 0         = [] 
--            | n == 0        = [b]           
--            | otherwise     = concatMap (\x -> nPos x (n-1) ) ( nextPositions b )

-- foldl >>= 

-- a =  B1 >=> nextPositions

-- >>= nextPositions >>= nextPositions >>= nextPositions



-- nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
-- nextPositionsN b n pred | n < 0         = [] 
--                         | n == 0        = [b]           
--                         | n == 1        = nextPositions b 
--                         | otherwise     = concatMap nextPositions (b-1)



-- p1 = (\x -> (x == B1) )
-- t1 = nextPositionsN B1 0 p1


--nextPositionsN b n pred = do 
        


-- import Data.Char

-- data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
--     deriving (Eq, Show)

-- asToken :: String -> Maybe Token
-- asToken n = foldl (\a x -> if (fst x) == n then Just (snd x) else a ) (if (all isDigit n) then Just (Number $ read n) else Nothing) [ ("+", Plus) , ("-", Minus) , ("(" , LeftBrace ) , ( ")" , RightBrace) ] 

-- a = Just 4 
-- b = Just [5,6] 
-- c = Nothing

-- f = a >>= (\x -> Just $ (:) x ) 

-- --f = a >>= ( Just $ (:) )

-- d = (<*>) f b

 

-- tokenize :: String -> Maybe [Token]
-- tokenize input = foldr (\x a ->  ( x >>= (\y -> Just $ (:) y )) <*> a ) ( return [] ) ( map asToken $ words input )  

--asToken n = foldl (\a x -> if (fst x) == n then Just (snd x) else a ) (if (all isDigit n) then Just (Number $ read n) else Nothing) [ ("+", Plus) , ("-", Minus) , ("(" , LeftBrace ) , ( ")" , RightBrace) ] 
-- import Control.Monad

-- data Log a = Log [String] a deriving Show

-- toLogger :: (a -> b) -> String -> (a -> Log b)
-- toLogger f msg = (\x -> Log [msg] (f x) )

-- foo:: (a -> Log b) -> Log a -> Log b
-- foo  f (Log ls a) = Log (ls ++ ls2) b  
--         where (Log ls2 b) = f a

-- execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
-- execLoggers x f g = foo g (f x) 

-- add1Log = toLogger (+1) "added one"
-- mult2Log = toLogger (* 2) "multiplied by 2"

-- returnLog :: a -> Log a
-- returnLog = Log []

-- bindLog :: Log a -> (a -> Log b) -> Log b
-- bindLog (Log ls a) f = Log (ls ++ ls1) b where (Log ls1 b) = f a

-- instance Applicative Log where 
--     pure a = Log [] a
--     (<*>) (Log ls1 f) (Log ls2 a) = Log (ls1++ls2) (f a)  

-- instance Functor Log where 
--     fmap f (Log ls a) = Log ls (f a)         

-- instance Monad Log where
--    return = returnLog
--    (>>=) = bindLog

-- execLoggersList :: a -> [a -> Log a] -> Log a
-- execLoggersList x  ls = foldl (\a x -> a >>= x ) (return (x))  ls     
 

-- data Entry k1 k2 v = Entry (k1, k2) v  deriving Show

-- data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

-- instance Functor (Entry k1 k2) where
--     fmap f ( Entry (k1,k2) v ) =  Entry (k1, k2) (f v)

-- instance Functor (Map k1 k2) where
--     fmap f (Map y) = Map ( map (\x -> fmap f x ) y )


-- data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

-- instance Functor Tree where
--     fmap f  (Leaf a) = Leaf (fmap f a)
--     fmap f  (Branch a b c) = Branch (fmap f a) (fmap f b) (fmap f c)  


--fmap (+3) Just 3


-- data Point3D a = Point3D a a a deriving Show

-- data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

-- instance Functor Point3D where
--     fmap f (Point3D x y z) =  Point3D (f x) (f y) (f z) 

-- instance Functor GeomPrimitive where
--     fmap  f (Point x) = Point ( fmap f x )
--     fmap  f (LineSegment x y) = LineSegment ( fmap f x ) ( fmap f y )


-- import Prelude hiding (lookup)
-- --import qualified Data.List as L

-- class MapLike m where
--     empty :: m k v
--     lookup :: Ord k => k -> m k v -> Maybe v
--     insert :: Ord k => k -> v -> m k v -> m k v
--     delete :: Ord k => k -> m k v -> m k v
--     fromList :: Ord k => [(k,v)] -> m k v
     


-- newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }



-- foo:: k -> v -> ArrowMap k v -> (k -> Maybe v) 
-- foo k v f = (getArrowMap f)  

-- ins:: k -> v -> ArrowMap k v -> (k -> Maybe v) 
-- ins k v f = (\x ->  Nothing ) 
        
--         -- ( \x ->  if x == k then (Just v) else Nothing ) 

-- -- .  (getArrowMap f) 
-- bar:: Ord k => (k -> Maybe v) -> k -> Maybe v -> (k -> Maybe v)
-- bar f k mv  = (\x -> if (x == k) then mv else (f x) )


-- instance  MapLike ArrowMap where
--         empty = ArrowMap  (\x -> Nothing)          
--         lookup k f =  getArrowMap f k 
--         insert k v f = ArrowMap $ bar (getArrowMap f) k (Just v) 
--         delete k f = ArrowMap $ bar (getArrowMap f) k Nothing
--         fromList [] = empty
--         fromList ((k,v):xs) = insert k v (fromList xs)


-- ls1 = empty::ArrowMap Int String 
-- ls2 = insert 3 "3333" ls1 
-- ls3 = insert 4 "4444" ls2 
-- ls4 = insert 3 "NNNN" ls3 
-- lsD = delete 4  ls3 



-- bar:: Ord k => (k -> Maybe v) -> k -> Maybe v -> (k -> Maybe v)
-- bar f k mv  = (\x -> if (x == k) then mv else (f x) )


        -- fromList 


-- import Prelude hiding (lookup)
-- import qualified Data.List as L

-- class MapLike m where
--     empty :: m k v
--     lookup :: Ord k => k -> m k v -> Maybe v
--     insert :: Ord k => k -> v -> m k v -> m k v
--     delete :: Ord k => k -> m k v -> m k v
--     fromList :: Ord k => [(k,v)] -> m k v
--     fromList [] = empty
--     fromList ((k,v):xs) = insert k v (fromList xs)

-- newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
--     deriving (Eq,Show)

-- instance  MapLike ListMap where
--         empty = ListMap []
--         lookup k ls =  L.find ( \x -> fst x == k ) (getListMap ls)   >>= (\x -> Just $ snd  x  )
--         delete k ls = ListMap  $  foldl (\a x -> if fst x == k then a else x:a ) [] (getListMap ls)
--         insert k v ls = ListMap $ (k,v):(getListMap $ case lookup k ls >>= (\x -> Just( delete k ls ) ) of
--                                                                 Just value -> value
--                                                                 Nothing    -> ls )
      
        -- insert k v ls = ListMap $ (k,v):( getListMap lst )  
        --         where lst = case lookup k ls >>= (\x -> Just( delete k ls ) )  of
        --                         Just value -> value
        --                         Nothing    -> ls


        -- empty = ListMap []
        -- --lookup :: Ord k => k -> ListMap k v -> Maybe v 
        -- lookup k ls = ( L.find ( \(kl,_) -> kl == k ) (getListMap ls) )  >>= (\(_,x) -> Just x )   
        -- --delete :: Ord k => k -> m k v -> m k v
        -- delete k ls = ListMap  $  foldl (\a x -> a ++  if (fst x) == k then [] else [x] ) [] (getListMap ls)
        -- --insert :: Ord k => k -> v -> m k v -> m k v
        -- insert k v ls = ListMap $ (k,v):( getListMap lst )  
        --         where lst = case lookup k ls >>= (\x -> Just( delete k ls ) )  of
        --                         Just value -> value
        --                         Nothing    -> ls


-- ls0 = ListMap $ [(3,"3"), (4,"3") ]

-- ls1 = empty::ListMap Int String 
-- ls2 = insert 3 "3333" ls1 
-- ls3 = insert 4 "4444" ls2 
-- lsD = delete 4  ls3 

-- ls4 = insert 3 "3NNN" ls3 

-- newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
--     deriving (Eq,Show)

-- --foo :: Maybe' a -> Maybe' a -> Bool
-- --foo x y = (x == y)

-- instance Monoid a => Semigroup (Maybe' a) where
--     (<>) ( Maybe' Nothing) ( Maybe' Nothing) = Maybe' Nothing
--     (<>) ( Maybe' Nothing) ( Maybe' ( Just mempty )) = Maybe' Nothing
--     (<>) ( Maybe' ( Just mempty )) ( Maybe' Nothing) = Maybe' Nothing
--     (<>) ( Maybe' ( Just x ))  ( Maybe' ( Just y )) = Maybe' (Just ( x `mappend` y ) )

--     -- x y = ept   where ept = Maybe'( Just mempty )
    
--     -- x y =  if ( x == Maybe'( Just mempty ) ) then x else y
            
--      --       ( ( if x == ( Maybe'( Just mempty )) then id else flip ) const ) x y

--     --(<>)  m (Maybe' ( Just x )) =  if (x::mempty) then m else Maybe' ( Just x )

-- instance Monoid a => Monoid (Maybe' a) where
--     mempty =  Maybe'( Just mempty )
--     mappend ( Maybe' Nothing) ( Maybe' Nothing) = Maybe' Nothing
--     mappend ( Maybe' Nothing) ( Maybe' ( Just mempty )) = Maybe' Nothing
--     mappend ( Maybe' ( Just mempty )) ( Maybe' Nothing) = Maybe' Nothing
--     mappend ( Maybe' ( Just x ))  ( Maybe' ( Just y )) = Maybe' (Just ( x `mappend` y ) )
    


-- Maybe' Nothing `mappend` mempty ≡ mempty `mappend` Maybe' Nothing ≡ Maybe' Nothing

-- Maybe' x  `mappend` mempty ≡ mempty `mappend` Maybe' x ≡ Maybe' x

-- newtype Xor = Xor { getXor :: Bool }
--     deriving (Eq,Show)

-- instance Semigroup Xor where
--       (<>)  =  (Xor . ) . ( /=  )

-- instance Monoid Xor where
--         mempty  = Xor False
--         mappend (Xor x) (Xor y) =  Xor ( x /= y )  

-- newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
--     deriving (Eq,Show)

-- instance Monoid a => Monoid (Maybe' a) where
--     mempty = Maybe ( Maybe mempty a   )   
    --mappend = undefined    

-- data A = One | Two deriving (Show)

-- instance Eq A where
--         (==) One One = True
--         (==) Two Two = True
--         (==) _ _     = False

-- instance Semigroup A where
--         (<>) x y = if (x == y) then x else y 

-- instance Monoid A where 



-- newtype Mbe' A = Mbe' { getMaybe :: Maybe A }  deriving (Eq,Show)

-- instance  Semigroup (Maybe' A) where
--        (<>) (Maybe' (Just x)) (Maybe' (Just y)) =  (Maybe' Just y)
--        (<>) _ _ = (Maybe' Nothing)


-- instance Semigroup a => Semigroup (Maybe' a) where
--     (<>) (Maybe' Nothing) _ =  (Maybe' Nothing)
--     (<>) _ (Maybe' Nothing) =  (Maybe' Nothing) 
--     (<>) (Maybe' Just x) (Maybe' Just y) =  Maybe' (Just (x + y)) 


-- instance Monoid a => Monoid (Maybe' a) where
--     mempty = Just 0 



-- newtype Xor = Xor { getXor :: Bool }
--     deriving (Eq,Show)

-- instance Semigroup Xor where
--      (<>)  =  (Xor . ) . ( /=  )

-- --instance Monoid Xor where        
-- --instance Monoid Xor where        

-- instance Monoid Xor where
--         mempty  = Xor False
        


-- infixl 6 :+:
-- infixl 7 :*:
-- data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
--     deriving (Show, Eq)

-- expand :: Expr -> Expr
-- expand ((e1 :+: e2) :*: e) = expand ( expand (expand e1 :*: expand e) :+: expand ( expand e2 :*: expand e) )
-- expand (e :*: (e1 :+: e2)) = expand ( expand (expand e :*: expand e1) :+: expand ( expand e :*: expand e2 ) )
-- expand (e :+: (e1 :+: e2)) =  expand e :+: expand e1 :+: expand e2 
-- expand (e1 :+: e2) = expand e1 :+: expand e2
-- expand (e1 :*: e2) = expand e1 :*: expand e2
-- expand e = e


-- infixl 6 :+:
-- infixl 7 :*:
-- data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
--     deriving (Show, Eq)


-- foo e f x y = if exy == f x y then exy else e exy where exy = f (e x) (e y)    
 
-- expand :: Expr -> Expr
-- expand ((e1 :+: e2) :*: e) = expand ( e1 :*: e :+: e2 :*: e )
-- expand (e :*: (e1 :+: e2)) = expand ( e :*: e1 :+: e :*: e2 )
-- expand (e1 :+: e2) = foo expand ( :+: ) e1 e2  
-- expand (e1 :*: e2) = foo expand ( :*: ) e1 e2  
-- expand e = e

-- if x == (e1 :+: e2)  then x else expand x where  x = (expand e1 :+: expand e2)  
-- if x == (e1 :*: e2)  then x else expand x where  x = (expand e1 :*: expand e2)  

--expand (e1 :+: e2) =  if x == (e1 :+: e2)  then x else expand x where  x = (expand e1 :+: expand e2)  
--expand (e1 :*: e2) =  if x == (e1 :*: e2)  then x else expand x where  x = (expand e1 :*: expand e2)  

--expand (e1 :+: (e2) ) = e1 :+: e2 
--expand (e1 :+: e2) = expand e1 :+: expand e2
-- expand ((e1 :+: e2) :*: e) = expand ( expand e1 :*: expand e :+: expand e2 :*: expand e )
-- expand (e :*: (e1 :+: e2)) = expand ( expand e :*: expand e1 :+: expand e :*: expand e2 )
-- expand (e1 :*: e2) = expand e1 :*: expand e2 


-- expand (e :*: (e1 :+: e2)) = expand ( (expand e) :*: e1 :+:  (expand e) :*: e2  )
-- expand ((e1 :+: e2) :*: e) = expand (e :*: (e1 :+: e2))
-- expand (e1 :+: e2) = expand e1 :+: expand e2 
-- expand (e1 :*: e2) = expand e1 :*: expand e2
-- expand e = e

-- expand (e1 :+: e2) = expand e1 :+: expand e2 
-- expand (e :*: (e1 :+: e2)) = expand ( expand e :*: expand e1  :+: expand e :*: expand e2  )
-- expand ((e1 :+: e2) :*: e) = expand ( expand e1 :*: expand e  :+: expand e2 :*: expand e  )
-- expand (e1 :*: e2) = expand e1 :*: expand e2
-- expand e = e

--expand (e :+: (e1 :+: e2)) =  expand e :+: expand e1 :+: expand e2 
--expand ((e1 :+: e2):+:e ) =  expand e :+: expand e1 :+: expand e2 



-- expand ((e1 :+: e2) :*: e) = expand ( expand ( expand e :*: expand e1 ) :+: expand ( expand e :*: expand e2 ) )
-- expand (e :*: (e1 :+: e2)) = expand ( expand ( expand e :*: expand e1 ) :+: expand ( expand e :*: expand e2 ) )
-- expand (e :+: (e1 :+: e2)) =  expand e :+: expand e1 :+: expand e2 
-- expand ((e1 :+: e2):+:e)   =  expand e1 :+: expand e2 :+: expand e 

-- expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
-- expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
-- expand (e1 :+: e2) = expand e1 :+: expand e2 
-- expand (e1 :*: e2) = expand e1 :*: expand e2 
-- expand e = e


-- expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
-- expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
-- expand (e1 :+: e2) = expand e1 :+: expand e2
-- expand (e1 :*: e2) = expand e1 :*: expand e2
-- expand e = e

-- expand ((e1 :+: e2) :*: e) = expand ( expand e1 :*: expand e :+:  expand e2 :*: expand e  )
-- expand (e :*: (e1 :+: e2)) = expand ( expand e :*: expand e1 :+:  expand e :*: expand e2  )
-- expand (e :+: (e1 :+: e2)) =  expand e :+: expand e1 :+: expand e2 
-- expand (e1 :+: e2) = expand e1 :+: expand e2
-- expand (e1 :*: e2) = expand e1 :*: expand e2
-- expand e = e


-- data Tree a = Leaf a | Node (Tree a) (Tree a)

-- avg :: Tree Int -> Int
-- avg t =
--     let (c,s) = go t
--     in s `div` c
--      where
--         go :: Tree Int -> (Int,Int)
--         go (Leaf x) = (1,x) 
--         go (Node x y) = 
--                 let (x1,x2) = go x 
--                     (y1,y2) = go y 
--                 in ( x1 + y1 , x2 + y2 )    

-- height :: Tree a -> Int
-- height (Leaf x) = 0
-- height (Node x y) = 1 + max (height x) (height y)


-- size :: Tree a -> Int
-- size (Leaf x) = 1 
-- size (Node x y) = 1 + (size x) + (size y)
 

-- a = Node (Leaf (31::Int)) (Leaf (2::Int))
-- b = Node (Leaf 3) (Leaf 4 )
-- c = Node a b
-- c1 = Node b a
-- d = Node c c1

-- data Nat = Zero | Suc Nat deriving Show

-- fromNat :: Nat -> Integer
-- fromNat Zero = 0
-- fromNat (Suc n) = fromNat n + 1

-- toNat :: Integer -> Nat
-- toNat 0 = Zero
-- toNat x = foldl (\a x -> Suc a) Zero [1..abs x] 

-- bFoo f x y = toNat $  f (fromNat x)  (fromNat y)
-- uFoo f x   = toNat $  f (fromNat x)  

-- add :: Nat -> Nat -> Nat
-- add =  bFoo (+)  

-- mul :: Nat -> Nat -> Nat
-- mul = bFoo (*) 

-- fac :: Nat -> Nat
-- fac = uFoo (\n -> foldl (*) 1 [1..n] )



-- data List a = Nil | Cons a (List a)  deriving Show

-- fromList :: List a -> [a]
-- fromList Nil = []
-- fromList (Cons x (l)) = x:( fromList l )

-- toList :: [a] -> List a
-- toList = foldr (\x a -> Cons x a ) Nil

-- ls = Cons 'e'  $ Cons 'd' Nil
-- l = "4444eewqew"



-- import Data.Char(isDigit)

-- data Error = ParsingError | IncompleteDataError | IncorrectDataError String    deriving Show
-- data Person = Person { firstName :: String, lastName :: String, age :: Int }   deriving Show



-- splt :: Char-> String -> [String] 
-- splt c = foldl (\a x -> if (x == c) then  a ++ [[]] else init a ++ [last a ++ [x]]) [[]] 

-- prsA s = ( Just $ splt '=' s )  
--      >>=  (\i -> if  length i == 2 && (last $ head i) == ' ' && (head $ last i) == ' ' then Just ((init $ head i), (tail $ last i) ) else Nothing    )


-- fil2 (f,l,a,e) (Just (n,v)) | n == "firstName" = (Just v ,l,a,e) 
--                             | n == "lastName" = (f,Just v,a,e)
--                             | n == "age" = if  (foldl (\a x -> a && isDigit x) True v )  then (f,l, Just ( (read v)::Int),e)  else (f,l,a, Just v ) 
--                             | otherwise = (f,l,a,e)

-- acc = (Nothing,Nothing,Nothing,Nothing)

-- foo (_,_,_, (Just n)) = Left (IncorrectDataError n)
-- foo (a,b,c, Nothing)  = if a == Nothing || b == Nothing || c == Nothing then Left IncompleteDataError else Right ((rn a),(rn b),(rn c) )
--                 where  rn::Maybe k -> k 
--                        rn (Just n) = n 

-- parsePerson :: String -> Either Error Person
-- parsePerson s = Right s 
--    >>= (\s -> Right $ splt '\n' s) 
--    >>= (\s -> Right $ ( map prsA s  ) )
--    >>= (\s -> if ( any ( == Nothing ) s ) then  Left ParsingError else Right s )
--    >>= (\s -> Right $  foldl fil2 acc s )     
--    >>= foo >>= (\(a,b,c) -> Right $  Person a b c )  


-- ret s = Right s 
--    >>= (\s -> Right $ splt '\n' s) 
--    >>= (\s -> Right $ ( map prsA s  ) )
--    >>= (\s -> if ( any ( == Nothing ) s ) then  Left ParsingError else Right s )
--    >>= (\s -> Right $  foldl fil2 a s )     
--    >>= foo >>= (\(a,b,c) -> Right $  Person a b c )     
    

  -- >>= (\s -> Right  ( foldl fil2 ((Nothing,Nothing,Nothing), Nothing ) s ) ) 
  --  >>= foo     

   -- >>= (\(s,v) -> foo s v )
   -- >>= (\(s,e) -> if e == Nothing then Right s else Left (IncorrectDataError "  ")  )
   -- >>= (\(s,e) -> foo s e )
           
    --       if e == Nothing then Right s else Left (IncorrectDataError e) )
   


-- fil2 (a1,vl) (Just (n,v)) =  if m == Nothing then (m, Just v ) else (m,vl) 
--         where m = a1 >>= (fil (n,v)) 

           
        
--fil :: Person -> (String,String) -> Mayby Person        
-- fil (f,l,a) (n,v) | n == "firstName" = Just $ p { firstName = v } 
--                   | n == "lastName" = Just $ p { lastName = v } 
--                   | n == "age" = if  (foldl (\a x -> a && isDigit x) True v )  then Just $ p { age = (read v)::Int } else  Nothing     
--                   | otherwise = Just p

-- fil (n,v) (f,l,a) | n == "firstName" = Just (v,l,a) 
--                   | n == "lastName" = Just (f,v,a)
--                   | n == "age" = if  (foldl (\a x -> a && isDigit x) True v )  then Just $ (f,l, (read v)::Int)  else  Nothing     
--                   | otherwise = Just (f,l,a)


           


           --then (Left IncorrectDataError) else (Right s)  )     

--p1 = Nothing

--p2 = [Just 2, Nothing, Just 3  ]  
--p3 = [Just 2, Just 1, Just 3  ]  

-- p1 = p {firstName = "4444" }

--     >>=  (\i -> if  length i == 2 && (last $ head i) == ' ' && (head $ last i) == ' ' then Just ((init $ head i), (tail $ last i) ) else Nothing    )



-- >>= (\i -> if  length i == 2 &&  ( last $ first i )  == ' '  then Just 333 else Nothing    )


--prs s = map (\x -> splt '=' x ) $ splt '\n' s 

--ret s = Right s 
--   >>= (\s -> Right $ splt '\n' s) 
--   >>= (\s -> Right $ map (\a -> splt '=' a ) s )         



-- s0 = "firstNamer\nage = 30\nasde=as11" 
-- s1 = "firstName = John Smith\nlastName = Connor\nage = 30\nasde=as11"
-- s2 = "firstName = John\nlastName = Connor\nage = 22w30"
-- s3 = "firstName = John Smith\nlastName = Connor\nage = 30\nasde = as11"





-- maybeToList :: Maybe a -> [a]
-- maybeToList x = case x of 
--                Just n  -> [n]
--                Nothing -> []

-- listToMaybe :: [a] -> Maybe a
-- listToMaybe [] = Nothing
-- listToMaybe n = Just $ head n


-- import Data.Char(isDigit)

-- findDigit :: [Char] -> Maybe Char
-- findDigit = foldl (\a x ->  if ( a == Nothing && isDigit x) then Just x else a ) Nothing 

-- findDigitOrX :: [Char] -> Char
-- findDigitOrX x = case findDigit x of 
--         Just n -> n 
--         _      -> 'X'

--fo:: Maybe Char -> Char -> Maybe Char
--fo = (\a x -> if( a == Nothing && isDigit x) then Nothing  else Nothing)

-- data Coord a = Coord a a deriving Show

-- foo:: ( Double -> a -> b ) -> Double -> Coord a -> Coord b
-- foo f sz (Coord x y) = Coord (f sz x ) (f sz y)

-- getCenter :: Double -> Coord Int -> Coord Double
-- getCenter = foo (\a n -> a * ( fromIntegral(n) + 0.5  )  ) 

-- getCell :: Double -> Coord Double -> Coord Int
-- getCell = foo (\a n -> floor $ n / a )

--fc:: Double -> Int -> Double  
--fc a n =  a * ( fromIntegral(n) + 0.5  )  
        
        
        
-- ( a * fromIntegral ( 2 * n + 1 ) ) * 0.5
       
--           + (a / 2) 

--t4 = fc 2.2 1

-- getCenter :: Double -> Coord Int -> Coord Double
-- getCenter  a (Coord x y) = Coord (fc a x) (fc a y) 
--           where fc:: Double -> Int -> Double  
--                 fc a n = ( a * fromIntegral ( 2 * n + 1 ) ) / 2


-- getCell :: Double -> Coord Double -> Coord Int
-- getCell a (Coord x y) = Coord (fc a x) (fc a y)          
--         where fc:: Double -> Double -> Int  
--               fc a n = truncate $ n / a


-- s1 = Coord (4::Double) (6::Double)
-- s2 = Coord (1::Double) (2::Double)
-- i1 = Coord (4::Int) (6::Int)
-- i2 = Coord (1::Int) (2::Int)
-- i0 = Coord (0::Int) (0::Int)


-- t1 = getCenter 10.0 i0 
-- t2 = getCell   10.0 s1 
-- t3 = getCell   1.1  s1

-- fc:: Double -> Int -> Double  
-- fc a n = a * fromIntegral $ 2 * n + 1 

--fc a n =  (2.0 * fromIntegral n )


--fc a n = a * fromIntegral(2 * n + 1)

--getCell :: Double -> Coord Double -> Coord Int
--getCell = 




-- data Coord a = Coord a a

-- distance :: Coord Double -> Coord Double -> Double
-- distance (Coord x1 y1) (Coord x2 y2)  =  sqrt $ lnSq x2 x1 + lnSq y2 y1
--         where lnSq a b = ( a - b ) ^ 2   

-- manhDistance :: Coord Int -> Coord Int -> Int
-- manhDistance (Coord x1 y1) (Coord x2 y2) = ln x2 x1 + ln y2  y1
--         where ln a b = abs ( a - b )   

-- s1 = Coord (4::Double) (6::Double)
-- s2 = Coord (1::Double) (2::Double)

-- t1 = distance s1 s2

-- i1 = Coord (4::Int) (6::Int)
-- i2 = Coord (1::Int) (2::Int)

-- t2 = manhDistance i1 i2

-- data Person = Person { firstName :: String, lastName :: String, age :: Int }  deriving Show
-- abbrFirstName p = if length (firstName p) > 2  then p else p { firstName  = (firstName p) !! 0:"." }

--abbrFirstName p = p { firstName = ( if length (firstName p) < 2  then (firstName p)  else  [(firstName p) !! 1] ) ++ "."  }

--abbrFirstName p = if length (firstName p) < 2  then p else p { firstName  = (firstName p) !! 0:"." }


--foo  ~(Person {firstName = fn}) = fn


--abbrFirstName :: Person -> Person
--abbrFirstName (Person {firstName = fn}) = fn | length fn < 2 = "444"
--                                             | _ = "333"   
--p { firstName = ( if length (firstName p) < 2  then (firstName p)  else  (firstName p) !! 1) :"."  }

--let s = Person "4444" "fff" 34

--abbrFirstName p = p { firstName = if length (firstName p) < 2  then (firstName p)  else  (firstName p) !! 0:"."  }

--let s = Person  { age = 34 }


-- import Data.Time.Clock
-- import Data.Time.Format
-- --import System.Locale

-- timeToString :: UTCTime -> String
-- timeToString = formatTime defaultTimeLocale "%a %d %T"



-- data LogLevel = Error | Warning | Info deriving Show

-- data LogEntry = LogEntry {timestamp :: UTCTime, logLevel::LogLevel, message:: String }  --deriving Show

-- logLevelToString :: LogLevel -> String
-- logLevelToString = show 

-- logEntryToString :: LogEntry -> String
-- logEntryToString a =  timeToString (timestamp a ) ++ ": " ++  logLevelToString ( logLevel a )     ++  ": " ++  message a 

-- test1 =
--   let ct = read "2019-02-24 18:28:52.607875 UTC"::UTCTime
--       le = LogEntry ct Info "Info Message"
--   in logEntryToString le

--sitemapLastMod = (read "2011-11-19")::UTCTime

--t2 = UTCTime (fromGregorian 2017 01 01)


--s = ( re "2011-11-19 18:28:r52.607875 UTC")::UTCTime

-- LogEntry{ getCurrentTime, Info, "ddddddddddddddddddd" }



-- import Data.Time.Clock
-- import Data.Time.Format
-- --import System.Locale

-- timeToString :: UTCTime -> String
-- timeToString = formatTime defaultTimeLocale "%a %d %T" 

-- data LogLevel = Error | Warning | Info deriving Show

-- data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }  deriving Show

-- --deriving Show

-- logLevelToString :: LogLevel -> String
-- logLevelToString = show 

-- logEntryToString :: LogEntry -> String
-- logEntryToString le = timeToString $ timestamp le

-- sitemapLastMod = (read "2011-11-19 18:28:r52.607875 UTC")::UTCTime

-- a = LogEntry sitemapLastMod Error "ddddddddddddddddddd" 
-- data Bit = Zero | One           deriving Show
-- data Sign = Minus | Plus        deriving Show
-- data Z = Z Sign [Bit]           deriving Show



-- zToInt (Z s bits) = foldl (\a b -> a . (toPow b) ) (toSign s) ( zip bits [0..] ) 0
--         where 
--                 toPow (One,i) =  ( + (2 ^ i)) 
--                 toPow _ = id  
--                 toSign Minus = (* (-1))
--                 toSign _ = id

-- intToZ n = Z ( toSign n) (map toBit $ dv (abs n))         
--         where 
--                 dv 0 = []
--                 dv n = [mod n 2] ++ dv( div n 2) 
--                 toBit x = if x == 0 then Zero else One
--                 toSign x = if x < 0 then Minus else  Plus

-- add :: Z -> Z -> Z
-- add a b = intToZ $ zToInt a + zToInt b 

-- mul :: Z -> Z -> Z
-- mul a b = intToZ $ zToInt a * zToInt b              


-- n1 = Z Minus [One, Zero, One, One]
-- n0 = Z Minus []

-- dv 0 = []
-- dv n = [mod n 2] ++ dv( div n 2) 
        
-- toBit x = if x == 0 then Zero else One
-- toSign x = if x < 0 then Minus else  Plus

-- intToBit :: Int -> [Bit]
-- intToBit n = map toBit $ dv n 
        
-- intToZ n = Z ( toSign n) (map toBit $ dv (abs n))         


-- toPow :: (Bit,Int) -> Int -> Int 
-- toPow (One,i) =  ( + (2 ^ i)) 
-- toPow _ = id   

-- toSign Minus = (* (-1))
-- toSign _ = id

--bitToInt::[Bit]->Int 
--bitToInt bits = foldl (\a b -> a + toPow b ) (+ 0) ( zip bits [0..] )

--zToInt::Z->Int
--zToFoInt (Z s bits)= foldl (\a b -> a . (toPow b) ) (toSign s) ( zip bits [0..] ) 



-- bitToInt::[Bit]->Int->Int
-- bitToFoInt bits = foldl (\a b -> a . (toPow b) ) (+ 0) ( zip bits [0..] )



-- toInt::Z -> Int
       
-- add :: Z -> Z -> Z
-- add = undefined

-- mul :: Z -> Z -> Z
-- mul = undefined

-- data Result = Fail | Success

-- doSomeWork :: Int -> (Result,Int) 
-- doSomeWork ival = (Fail, ival )


-- data Result' =  Succ | Result' Int

-- instance Show Result' where
--       show (Result' i) =  "Fail: "++show i
--       show Succ = "Success"   


-- doSomeWork' :: Int -> Result'
-- doSomeWork' sd = 
--   case  doSomeWork sd  of
--       (Fail,n) -> (Result' n)
--       _ -> Succ   



-- data Shape = Circle Double | Rectangle Double Double deriving Show

-- area :: Shape -> Double
-- area (Circle r)      = pi * r^2
-- area (Rectangle a b) = a * b
      


-- area (Circle r) = pi * r^2
-- area (Rectangle a b) = a * b
-- data Point = Point Double Double

-- origin :: Point
-- origin = Point 0.0 0.0

-- distanceToOrigin :: Point -> Double
-- distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

-- distance :: Point -> Point -> Double
-- distance (Point x1 y1) (Point x2 y2) = sqrt $ foo x1 x2 + foo y1 y2 where foo a b = (a-b)^2 


-- data Result = Fail | Success deriving Show

-- doSomeWork :: Int -> (Result,Int) 
-- doSomeWork ival = (Fail,ival )

-- processData :: SomeData -> String
-- processData ival =  
--    case  doSomeWork ival  of
--       (Fail,n) -> "Fail: " ++ show n
--       _ -> "Success"                                           




-- data LogLevel = Error | Warning | Info
-- cmp :: LogLevel -> LogLevel -> Ordering
-- cmp Error    Error      = EQ
-- cmp Warning  Warning    = EQ
-- cmp Warning  Info       = GT
-- cmp Warning  Error      = LT
-- cmp Info     Info       = Q
-- cmp Error _             = GT
-- cmp Info _              = LT



-- charToInt '0' = 0
-- charToInt '1' = 1
-- charToInt '2' = 2
-- charToInt '3' = 3
-- charToInt '4' = 4
-- charToInt '5' = 5
-- charToInt '6' = 6
-- charToInt '7' = 7
-- charToInt '8' = 8
-- charToInt '9' = 9

-- import Data.Char

-- charToInt

        --Red | "3"
        -- Green | "4"
        -- Blue  | "4"


-- import Data.List

-- revRange :: (Char,Char) -> [Char]
-- revRange = unfoldr g  
--     where g = (\ (a,b) -> if a>b then Nothing else Just (b, (a,pred b) ) )


--fl xs = foldl foo [[]] xs
--  where foo (a:xs) x = if xs == []  then [[2]] else [3] 

--f  =  ( (\a x -> x:a) `seq` ) 
--f1 = (\a x -> x:a)

--evenOnly xs =  (\(s,c) -> s ) . ( foo `seq` ( foldl foo ([],False) xs ))
--    where foo (s,c) x =  ( if c then s++[x] else s , not c) 

--enOnly xs =  foo `seq` ( foldl foo ([],False) xs )
--       where foo (s,c) x =  ( if c then s++[x] else s , not c) 
-- import Data.List


-- bar a x = a ++ [x]
-- ba  x a = x:a
-- fo xs = foldr ba [] xs
-- foo xs =   foldl' (($!)bar) []  xs 
-- --evenOnly xs =  (\(s,c) -> s ) . foldl' (\(s,c) x -> ( if c then s++[x] else s , not c) ) ([],False) $ xs


-- foor xs = foldr (\x a -> ( (x,2):a )) [] xs 
-- fool xs = foldl (\a x -> ( a++[(x,2)] )) [] xs 
-- fool' xs = foldl' (\a x :r-> ( a++[(x,2)] )) [] xs 

-- l = [ y | x <- [1..] , y <- [False,True] ]
-- fo3 xs = zip l xs
-- foo4 xs = foldr ( \(c,x) a -> if c then x:a else a ) []  (fo3 xs)
 

-- evenOnly xs = foldr (\(c,x) a->if c then x:a else a) [] (zip [y|x<-[1..],y<-[False,True] ] xs)


-- b = [1,2,3,4]

-- foo x = foldr:t 

--(\(s,c) -> s ) .
-- 

--meanList :: [Double] -> Double
--meanList xs = (\(s,c) -> s / c ) . foldr (\x (s,c) -> (s+x,c+1) ) (0,0) $ xs


-- b = [1,2,3,4]

-- foo x = foldr (\x s ->  (x `mod` 2) * x + s) 0 x 


-- b = [1,2,3,4]

-- -- foo [ ]

-- splt [] = ([],[])   
-- splt (x:xs) = ([x],xs)

 

-- foo [] = []
-- foo (x:xs) = [x] : [ x:y | y <- foo xs ]  

-- coins = [2, 3, 7]
-- coinsx = [[2], [3], [7]]

-- --change :: (Ord a, Num a) => a -> [[a]]



-- --change n = [  x:y  | x <- coins ,  y <- change n , sum (x:y) < n  ]

-- --sum x = foldr (+) 0 x 
-- mrg = foldr (++) []   
-- change :: (Ord a, Num a) => a -> [[a]]
-- change n = if n > 0 then mrg  [ ([ x : y | y <- change (n - x) , sum (x:y) == n ])   | x <- coins  ] else [[]]

-- --change n =  mrg  [ ([ x : y | y <- change (n - x) , sum (x:y) == n ])   | x <- coins  ] 


-- --change n = mrg $ [ [  x:y |  y <- [x] ] |  x <- coins  ]

-- foo  :: (Num a) => a -> a
-- foo x = x-1

-- foo [] = []
-- foo (x:xs) = [x]:foo xs ++ [ x:y | y <- foo xs , y /= [] ]  

-- --change :: (Ord a, Num a) => a -> [[a]]
-- change (xs,n) = [ [x] | x <- xs  ] 



--  sft (x:xs) = [ xs++[x] , 
--  b = [ x:a |  x <- a   ]
--  foo (x:xs) = ([x],xs)
--[ y | y <- xs ]



-- -- data Odd = Odd Integer 
-- --   deriving (Eq, Show)

-- -- addEven :: Odd -> Integer -> Odd
-- -- addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
-- --                   | otherwise      = error "addEven: second parameter cannot be odd"


-- -- lOdd = map Odd
-- -- isOdd x =  mod x 2 == 1                    

-- -- instance Enum Odd where 
-- --    succ (Odd n) =  Odd (n + 2)
-- --    pred (Odd n) =  Odd (n - 2)
-- --    toEnum n = Odd  ( ( 2 * toInteger(n)  + 1 )::Integer )
-- --    fromEnum (Odd n) = fromEnum ((div (n-1) 2)::Integer )

-- --    enumFrom (Odd x)                         = lOdd [ x , (x + 2) .. ]
-- --    enumFromThen (Odd x) (Odd y)             = lOdd  [ x , y .. ]
-- --    enumFromTo (Odd x) (Odd lim )            = lOdd  ( filter isOdd [ x .. lim ] )
-- --    enumFromThenTo (Odd x) (Odd y) (Odd lim) = lOdd  ( filter isOdd [ x , y .. lim ])


-- --    succ (Odd n) =  Odd ( (n + 2)::Integer )  
-- --    pred (Odd n) =  Odd  ( (n - 2)::Integer )  
-- --    toEnum n = Odd  ( ( 2 * toInteger(n)  + 1 )::Integer )
-- --    fromEnum (Odd n) = fromEnum ((div (n-1) 2)::Integer )
-- -- succ (Odd n) =  Odd (n + 2)
-- -- pred (Odd n) =  Odd (n - 2)
-- -- toEnum n = Odd $ toInteger(n) * 2  + 1  
-- -- fromEnum (Odd n) = fromEnum (//( div (n-1) 2 )::Integer) 


-- --instance Enum Odd where 
--     --succ n =  addEven n 2
--     --pred n =  addEven n (-2)

--     --toEnum n = 
--     --fromEnum n 

--     --toEnum :: Int -> Odd
--     --toEnum n = Odd ( toInteger n )
--     --fromEnum :: Odd -> Integer
--     --fromEnum (Odd n) = fromEnum ((div (n-1) 2)::Integer )

--     --succ (Odd n) =  Odd (n + 1)
--     --pred (Odd n) =  Odd (n - 1)

--     --toEnum n = Odd $ ( toEnum n :: Integer)
--     --fromEnum (Odd n) = fromEnum n


--     -- succ (Odd n) =  Odd ( (n + 2)::Integer )  
--     -- pred (Odd n) =  Odd  ( (n - 2)::Integer )  

--     -- toEnum n = Odd ( toEnum(n)::Integer )
--     -- fromEnum (Odd n) = fromEnum (n::Integer)


-- --fn :: Odd->Int 
-- --fn (Odd n) =  fromEnum $ div (n-1) 2
-- -- Большое число, которое не поместится в Int
-- baseVal = 9900000000000000000
-- baseValO = Odd baseVal

-- nt  = [Odd (baseVal).. Odd(baseVal+5) ]
-- nti = [baseVal .. baseVal+5 ]


-- -- Генератор значений для тестирования
-- testVal n = Odd $ baseVal + n
-- -- для проверки самих тестов. Тесты с 0..3 не должны выполняться
-- -- testVal = id

-- test0 = succ (testVal 1) == (testVal 3)
-- test1 = pred (testVal 3) == (testVal 1)
-- -- enumFrom
-- test2 = take 4 [testVal 1 ..] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- -- enumFromTo
-- -- -- По возрастанию
-- test3 = take 9 [testVal 1..testVal 7] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- -- -- По убыванию
-- test4 = take 3 [testVal 7..testVal 1] == []
-- -- enumFromThen
-- -- -- По возрастанию
-- test5 = take 4 [testVal 1, testVal 5 ..] == [testVal 1,testVal 5,testVal 9,testVal 13]
-- -- -- По убыванию
-- test6 = take 4 [testVal 5, testVal 3 ..] == [testVal 5,testVal 3,testVal 1,testVal (-1)]
-- -- enumFromThenTo
-- -- -- По возрастанию
-- test7 = [testVal 1, testVal 5 .. testVal 11] == [testVal 1,testVal 5,testVal 9]
-- -- -- По убыванию
-- test8 = [testVal 7, testVal 5 .. testVal 1] == [testVal 7,testVal 5,testVal 3,testVal 1]
-- -- -- x1 < x3 && x1 > x2
-- test9 = [testVal 7, testVal 5 .. testVal 11] == []
-- -- -- x1 > x3 && x1 < x2
-- test10 = [testVal 3, testVal 5 .. testVal 1] == []

-- test11 = take 4 [testVal 5, testVal 5 .. ] == replicate 4 (testVal 5)
-- test12 = take 4 [testVal 5, testVal 5 .. testVal 11] == replicate 4 (testVal 5)
-- test13 = take 4 [testVal 5, testVal 5 .. testVal 5] == replicate 4 (testVal 5)
-- test14 = [testVal 5, testVal 5 .. testVal 3] == []
-- test15 = [testVal 5, testVal 1 .. testVal 5] == [testVal 5]
-- test16 = toEnum (fromEnum (Odd 3)) == Odd 3
-- -- Это сомнительный тест. Скорее всего, его нет на stepik
-- test17 = fromEnum(Odd 3) + 1 == fromEnum(Odd 5)



-- testList = [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, 
--             test11, test12, test13, test14, test15, test16, test17]
-- allTests = zip [0..] testList
-- -- Список тестов с ошибками
-- badTests = map fst $ filter (not . snd) allTests




-- -- te:: Int -> Odd   
-- -- te n = Odd $ toInteger ( ( 2 * toInteger(n) ) + 1 ) 

-- -- x = Odd 33
-- -- x2 = Odd 9223372036854775817

-- -- fe (Odd n) = (fromIntegral n)::Int 
-- -- fe2 (Odd n) = (fromIntegral n) 


-- -- fe3 (Odd n) = fromInteger (n - 1) `div` 2
-- -- fe4 (Odd n) = fromInteger $ (n - 1) `div` 2
-- -- fe5 (Odd n) = (n - 1) `div` 2

-- -- sc  (Odd n) = (n + 2)
-- -- sc2 (Odd n) = toInteger (n + 2)
-- -- sc3 (Odd n) = ( fromInteger (n + 2) ) / 2
-- -- sc4 (Odd n) = (n + 2) * 2
-- -- sc5 (Odd n) = ( fromInteger (n + 2) ) * 2
-- -- fe6 (Odd n) = (n - 1) `div` 2




--  --toEnum n = Odd $ toInteger ( (2* n) +1 ) 
   


--     --fromEnum (Odd n) =   concatMap (\x -> [x,x]) [1..]
--    --fromEnum (Odd n) = fromInteger (n - 1) `div` 2
--     --if n > 0 then 1 else 0 

-- --fromEnum2 (Odd n) =  if n > 0 then take n $ concatMap (\x -> [x,x]) [1..] else 0    

  
-- --repeat2 = iterate repeatHelper 
-- --repeatHelper x = x  

-- --intg Num 
-- --fibStream :: [Integer]
-- --fibStream = zipWith (+) (0:fibStream) (0:1:fibStream)



-- -- max3 :: Ord a => [a] -> [a] -> [a] -> [a]
-- --max3 = zipWith3 (\x y z -> max (max x y) z )


-- -- perms :: [a] -> [[a]]
-- -- perms [] = [[]]
-- -- perms xs = concatMap (\n-> foo (xs!!n)  ( perms (  take (n) xs ++ drop (n+1) xs  ) ) ) [0..length xs - 1] where foo x ys = map (\y -> x:y ) ys 
-- {-
-- perms xs = concatMap (bar xs) [0..length xs - 1] 
--     where foo x xs = map (fooo x) xs 
--           fooo x xs = x:xs  
--           bar xs n = foo (xs!!n) (perms (drp xs n))     
--           drp ls n =  take n ls ++ drop (n+1) ls
-- -}

-- -- perms :: [a] -> [[a]]
-- -- perms [] = [[]]
-- -- perms xs = concatMap (\n-> foo (xs!!n)  ( perms (  take (n) xs ++ drop (n+1) xs  ) ) ) [0..length xs - 1] where foo x ys = map (\y -> x:y ) ys 
-- {-
-- perms xs = concatMap (bar xs) [0..length xs - 1] 
--     where foo x xs = map (fooo x) xs 
--           fooo x xs = x:xs  
--           bar xs n = foo (xs!!n) (perms (drp xs n))     
--           drp ls n =  take n ls ++ drop (n+1) ls
-- -}

-- {-
-- perms xs = concatMap (bar xs) [0..length xs - 1] 
--     where foo x xs = map (fooo x) xs 
--           fooo x xs = x:xs  
--           bar xs n = foo (xs!!n) (perms (drp xs n))     
--           drp ls n =  take n ls ++ drop (n+1) ls
-- -}
-- --


-- --perms xs = concatMap (\x-> foo x  ( perms (filter (/=x) xs))  ) xs where foo x ys = map (\y -> x:y ) ys 

-- --bar 


-- --foo ys =  map (bar ys) [0..length ys - 1]  
-- --bar xs n = drop n xs ++ take n xs 
-- --fooo (x:xs) =  map (\y -> x:y ) (foo xs)  


-- --perms :: Eq a => [a] -> [[a]]
-- --perms [] = [[]]
-- --perms xs = concatMap (\x-> foo x  ( perms (filter (/!x) xs))  ) xs where foo x xs = map (\y -> x:y ) xs 

-- --perm [] = []
-- --perm (x:xs) =   (perm xs) ++ [x] 
-- --perm xs = foldl (\x y -> [y] ++ x ) [] xs 

-- --foo xs =  map (\ x -> drop x xs ++ take x xs ) [0..length xs - 1]  
-- --bar x:xs = map (\ ) foo xs  
-- --foo  :: [a] -> [[a]]











-- --perms :: Num a => [a] -> [a]
-- --perms [] = [[]]
-- --perms [x] = [[x]] 

-- --perms xs = map (\x-> x: (filter (/=x) xs) ) xs where foo x xs = map (\y -> x:y ) xs 
-- --perms xs = concatMap (\x-> foo x  [(filter (/=x) xs)] ) xs where foo x xs = map (\y -> x:y ) xs

-- --perms xs = concatMap (\x-> foo x  ( perms (filter (/=x) xs))  ) xs where foo x xs = map (\y -> x:y ) xs 

-- --perms [x,y,z] = 

-- --perms (x) =  map (++) x 

-- --perms [x,y] = [[x,y],[y,x]]
-- --perms (x:xs) =  where foo l:ls map (\k -> l:k ) (perms xs)
-- --perms x concatMap () x where foo l f = f 

-- --bar (x:xs) = xs++[x] where foo l

-- -- squares'n'cubes :: Num a => [a] -> [a]
-- -- squares'n'cubes = concatMap (\x->[x^2,x^3] )

-- -- --qsort :: Ord a => [a] -> [a]
-- -- qsort [] = []
-- -- qsort (x:[]) = [x]
-- -- --qsort (x:xs) =  ( qsort ( filter (<=x) (xs) )) ++ [x] ++  ( qsort ( filter (>x) (x:xs) )) where foo f x = qsort ( filter f x )
-- -- qsort (x:xs) =  foo (<=x) xs ++ [x] ++ foo (>x) (x:xs)  where foo f x  = qsort.(filter f) $ x  


-- --qsort x = span (<6) x where splt (x,y) = x ++ y 
-- -- splt.(span (<5)) $ x where splt (x,y) = x ++ y
-- -- filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
-- -- --filterDisj x y = filter  ( (\a b c  -> (a c) || (b c) )  x y )  
-- -- filterDisj x y = filter  (     )

-- -- readDigits :: String -> (String, String)
-- -- readDigits = span isDigit



-- -- oddsOnly :: Integral a => [a] -> [a]
-- -- --oddsOnly = filter odd 
-- -- oddsOnly [] = []
-- -- oddsOnly (x:xs) = ( ++ oddsOnly xs) $ if odd x then [x] else []

   
--     --(fromInteger ( x + y + z ))::Double


-- -- class ( Eq a, Bounded a, Enum a) => SafeEnum a where
-- --     ssucc :: a -> a
-- --     ssucc x = if (maxBound) == x then  minBound else succ x 
        
-- --     spred :: a -> a
-- --     spred x =  if (minBound) == x then  maxBound else pred x 




-- -- class Printable a where 
-- --     toString :: a-> [Char]

-- -- instance Printable Bool where 
-- --     toString a = if a then "true" else  "false" 
                
-- -- instance Printable () where 
-- --     toString a = "unit type"
    
-- -- import GHC.Float

-- -- doItYourself = f . g . h
-- -- f = logBase 2
-- -- g = (^3)  
-- -- h = (max 42)



-- -- integration :: (Double -> Double) -> Double -> Double -> Double
-- -- integration f a b = h . sum . ((f b / 2):) . ((f a / 2):) . map ( f . xi ) $ [1..n-1] 
-- --                         where 
-- --                             n = 10000   
-- --                             h = (((b-a)/n)*)
-- --                             xi i = a + h i  

-- --     ( map ( (/2) . f ) [a,b] ::)

--  --   ((f a / 2):)
--  --x =  (/2) . sin $  

-- -- import Data.Char
-- -- sum'n'count :: Integer -> (Integer, Integer) 
-- -- sum'n'count x = let foo f1 f2 y = ( foldr f1 0 y, foldr f2 0 y )  
-- --                 in foo (\i a -> a + toInteger ( digitToInt i ))  (\i a -> a + 1 )  (filter isDigit (show x))



-- -- import Data.Char
-- -- --sum'n'count :: Integer -> (Integer, Integer) 
-- -- sum'n'count :: Integer -> [Integer]
-- -- sum'n'count x = map (\f -> (foldr f 0)) [ (\i a -> (toInteger (digitToInt i)) + a) , \i a -> a + 1 ] 
                    
                    
                    
-- --                     --foo (\i a -> (toInteger (digitToInt i)) + a)  (\i a -> (a + 1) )  (filter isDigit (show x))
    

-- --                (x) = map (\f -> fld f ) [ (\i a -> (toInteger (digitToInt i)) + a) , (\i a -> a + 1 ) ] [] 
                
                
-- ----sum'n'count :: Integer -> Integer
-- --sum'n'count x = let foo y = foldr (\i a -> (digitToInt i) + a) (filter isDigit y) 0
                
--     --{ let foS y = foldr (\i a -> (digitToInt i) + a) 0 y  ; let foC y = foldr (\i a -> a++ ) 0 y


--  --   in foo (filter isDigit (show x))

-- --foo y = foldr (\i a -> digitToInt i + a) 0 "33" 
-- --sum'n'count x = show x


-- -- fibonacci :: Integer -> Integer
-- -- fibonacci n = foo 0 1 2 n  

-- -- foo a0 a1 i n   | i == n  = if n > 0 then a0 + a1 else a1-a0 
-- --                 | n < 0  = foo ( a0 + a1 ) a0 (i - 1) n
-- --                 | otherwise = foo a1 (a0 + a1) (i + 1) n   

-- -- fibonacci :: Integer -> Integer
-- -- fibonacci 0    = 0
-- -- fibonacci 1    = 1 
-- -- --fibonacci (-1) = 1 
-- -- fibonacci n = foo (fibonacci 0) (fibonacci 1) (-1) n  

-- -- foo a0 a1 i n   | i == n  = if n > 0 then a0 + a1 else a1 - a0 
-- --                 | n < 0  = foo ( a0 + a1 ) a0 (i - 1) n
-- --                 | otherwise = foo a1 (a0 + a1) (i + 1) n   



-- -- fo o f = f 2 + ( o $ f 1 )
-- -- fo1 f o n n1 = f $ n - o n1 
-- -- fo2 f n o = fo o $ fo1 f o n

-- -- fibonacci :: Integer -> Integer
-- -- fibonacci 0 = 0
-- -- fibonacci 1 = 1
-- -- fibonacci n = fo2 fibonacci  n  (* if n > 0 then 1 else -1 )   


-- -- fo  f o = (f 2) + ( o (f 1) )
-- -- fo1 f o n n1 = f $ n - o n1 
-- -- fo2 f o n = fo (fo1 f o n) o  -- ;   (fo1 f o n 2 ) + ( o (fo1 f o n 1 ) )   --fo (fo1 f o n) o    

-- -- fibonacci :: Integer -> Integer
-- -- fibonacci 0 = 0
-- -- fibonacci 1 = 1
-- -- fibonacci n = fo2 fibonacci (if n > 0 then (*1) else ( *(-1)) ) n   




-- -- doubleFact :: Integer -> Integer
-- -- doubleFact 0 = 1
-- -- doubleFact 1 = 1
-- -- doubleFact n = n * doubleFact(n-2)



-- -- fo x y f f2 = f2 (f x) (f y)
-- -- foo x y = x * 10 + y
-- -- fooo f = if f isDigit (&&) then f digitToInt foo else 100
-- -- td :: Char -> Char -> Int 
-- -- td x y = fooo (fo x y)

-- -- twoDigits2Int :: Char -> Char -> Int

-- -- fo x y f = 
-- -- foo x y f1 f2 f3 f4 =  if f2 (f1 x) (f1 y) then 

-- -- twoDigits2Int x y = if isDigit x && isDigit y then digitToInt x * 10 + digitToInt y else 100

-- -- dist :: (Double, Double) -> (Double, Double) -> Double
-- -- foo p1 p2 f = (^2) $ f p1 - f p2  
-- -- fooo f2 f3 f  = sqrt $ f f2 + f f3   
-- -- dist x y   =  fooo fst snd $ foo x y 


