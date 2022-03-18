import System.Directory

hasSbs2 x sl = fst $ foldr (\i (b,s) -> ( b || (take (length x) (i:s) ) == x , i:s) ) (False, "" ) sl 

dirs = getDirectoryContents "."
dir = ["thesis.txt", "kitten.jpg", "hello.world", "linux_in_nutshell.pdf"]
dr = ["Hello.hs","h2.hs","..","."]



foo sb ls = do
    x <- ls
    --True <- return (hasSbs2 s x)
    True <- return ( fst $ foldr (\i (b,s) -> ( b || (take (length sb) (i:s) ) == sb , i:s) ) (False, "" )  x)
    return (x)


--main' :: IO ()
main' = do
    putStr "Substring: "
    nm <- getLine 
    fls <- getDirectoryContents "." 
    fl  <- return ( foo nm fls )
    return (nm,fls,fl)
    

--hs x s | length s > length   =  

-- hs x s = (take (length x) s) == x      

--hs1 x c a = a >>= ( \j -> Just (c:j) ) >>= (\j -> if ((take (length x) j) == x ) then Nothing else (Just j) )

hasSbs x s  = foldr (foo x) (Just "") s 
    where foo x c a = a >>= ( \j -> Just (c:j) ) >>= (\j -> if ((take (length x) j) == x ) then Nothing else (Just j) )

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


