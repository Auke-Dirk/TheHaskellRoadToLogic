{-# LANGUAGE ParallelListComp #-}

--EX 2.2 p33
-- P Q | P X Q
-- 0 0 |   0
-- 0 1 |   1
-- 1 0 |   1
-- 1 1 |   0

-- impl
(==>) :: Bool -> Bool -> Bool
x ==> y = not x || y

-- iff
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

--Ex 2.4 
(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y
--EX 2.2 p33
-- P Q | P <=> Q
-- 0 0 |   1
-- 0 1 |   0
-- 1 0 |   0
-- 1 1 |   1

-- Exactly flipped /negated from P X Q 2.2 

p = True
q = False

formula1 = (not p) && (p ==> q) <=> (q && (not p))

valid1 :: (Bool -> Bool) -> Bool
valid1 bf = (bf True) && (bf False)

-- can be nicely checked with a function that always return True
truef1 :: Bool -> Bool
truef1 x = True;

-- valid1 truef1 <=> True


valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf = (bf True True) &&
            (bf True False) &&
            (bf False True) &&
            (bf False False)
            
truef2 :: Bool -> Bool -> Bool
truef2 x y = True

truef3 :: Bool -> Bool -> Bool -> Bool
truef3 x y z = True
            
form1 :: Bool -> Bool -> Bool
form1 p q = p ==> (q ==> p)

form2 :: Bool -> Bool -> Bool
form2 p q = (p ==> q) ==> p


valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 bf = and [bf p q r | p <- [True,False]
                            | q <- [True,False]
                            | r <- [True,False]]

valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 bf = and [bf p q r s | p <- [True,False]
                            | q <- [True,False]
                            | r <- [True,False]
                            | s <- [True,False]]
                            
-- Ex 29 p43
-- trivial ;)


logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bfx bfy = (bfx True <=> bfy True) && (bfx False <=> bfy False)

logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 bfx bfy = and [(bfx p q) <=> (bfy p q) | p <- [True,False]
                                                 | q <- [True,False]]
-- quick test                                                
-- *Main> logEquiv2 truef2 truef2
-- True

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bfx bfy = and [(bfx p q r) <=> (bfy p q r) | p <- [True,False]
                                                     | q <- [True,False]
                                                     | r <- [True,False]]

-- Testing
formula3 p q = p
formula4 p q = (p <+> q) <+> q



