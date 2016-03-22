{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleInstances #-}

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


class TF p where
    valid :: p -> Bool
    lequiv :: p -> p -> Bool

instance TF Bool where
    valid = id
    lequiv f g = f == g 
    
instance TF p => TF (Bool -> p) where
   valid f = valid (f True) && valid (f False)
   lequiv f g = (f True) `lequiv` (g True) &&
                (f False) `lequiv` (g False)


-- Ex 2.13 p48
dml1 = valid (not True <=> False)
dml2 = valid (not False ==> True)
dml3 = valid (\ p -> p ==> False <=> not p)
dml3'= lequiv (\ p -> p ==> False) (\p -> not p)
dml4 = valid (\ p -> (p || True ) ==> True) 
dml5 = valid (\ p -> (p && False) ==> False) 

idl1 = valid (\ p -> (p || False) <=> p)
idl2 = valid (\ p -> (p && True) <=> p)

exm = valid (\ p -> (p || not p) <=> True)
cnt = valid (\ p -> (p && not p) <=> False)

-- ex 2.15 p48
contrad1 :: (Bool -> Bool) -> Bool
contrad1 f = (f True) == False && (f False) == False

contrad2 :: (Bool -> Bool -> Bool) -> Bool
contrad2 f = and [not (f x y)  | x <- [True,False],
                                 y <- [True,False]]                         

contrad3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
contrad3 f = and [not (f x y z)  | x <- [True,False],
                                   y <- [True,False],      
                                   z <- [True,False]]

-- Ex 2.16 p49
--   -Ex[x](x^2 + 1 = 0)
-- 

-- Ex 2.17 p49
-- x < y < z <=> (x < y) && (y < z) <=> not ((x < y) && ( y < z)) 

-- Ex 2.18.1 p49
-- A <=> B  ==
--          == (A => B) ^ (B => A)           comm
--          == (-B => -A) ^ (-A => -B)       contra
--          == (-A => -B) ^ (-B => -A)       A ^ B <=> B ^ A
--          == -A <=> -B                     comm

-- sub A, -A for 2.18.2

-- Ex 2.19 p49
-- No matter the truth values of the proposition letters inside A,B iff A <=> B is logically valid



-- Ex 2.20 
-- F lequiv (\ p q -> not p ==> q) (\ p q ->  p ==> not q)
-- F lequiv (\ p q -> not p ==> q) (\ p q ->  q ==> not p)
-- T lequiv (\ p q -> not p ==> q) (\ p q ->  not q ==> p)
-- F lequiv (\p q r -> p ==> (q ==> r)) (\p q r -> (q ==> p) ==> r)
-- F lequiv (\p q r -> p ==> (q ==> r)) (\p q r -> (p ==> q) ==> r)
-- T lequiv (\ p q -> (p ==> q) ==> p) (\ p q -> p)
-- F lequiv (\ p q r -> p || q ==> r) (\ p q r -> (p ==> r) && (q ==> r))

-- Ex 2,21
-- 1. q ==> p
-- 2. 2^4
-- 3. :)
-- 4 yes will always work
-- 5 rows = 2^3 = 8, 2^8 , expressions



-- Ex 2.22 Rl < Rr => Rm = (Rl + Rr) / 2

-- Vx(Ax => (Bx => Cx))
--    Ax => (Bx => Cx)
--        / \
--      Ax  Bx => Cx
--            / \              
--          Bx   Cx       
      
-- Ex(Ax ^ Bx)
--    Ax ^ Bx
--      / \
--     Ax  Bx  

-- ExAx ^ ExBx
--     / \
-- ExAx   ExBx
--   Ax     Bx


-- Ex 2.26 p53
-- 1. ExEy( x&Q ^ y&Q ^ x < y),  Ex&QEy&Q(x < y)
-- 2. Vx(x&R => Ey(y&R ^ x < y),  Vx&REy&R(x < Y)
-- 3.Vx(x&Z => Em,n(m&N ^ n&N ^ x = m - n)),  Vx&ZEn,m&N( x = m - n)

-- Ex 2.27 p53
-- 1. Vx&QEm,n&Z( n/=0 ^ x = m/n),  Vx(x&Q => Em,n( m&Z ^ n&Z ^ n/= 0 ^ x = m/n))
-- 2. Vx&FVy&D((Oxy => Bxy),  Vx(x&F => Vy(y&D ^ Oxy => Bxy)) // Vx(Fx => Vy(Dy => (Oxyu => Bxy))) // why?

-- Ex 2.31
-- 1. Ex(x^2 + 1 = 0)
-- 2. not En&NVm&N(m <= n)
-- 3. not Ex( x&N ^ x > 1 ^ x < 13 ^ x|13)
-- 4. n & N ^ ( not Ex x&N ^ x > 1 ^ x < n ^ x|n)
-- 5. Vm(m&N => En(n&M ^ n > m ^ o & N ^ ( not Ex x&N ^ x > 1 ^ x < o ^ x|o)))
-- Nice buildup from 3,4,5

-- Ex 2.32
-- 1. VxL(x,d)    VxLxd
-- 2. VxLdx
-- 3. Vx(Mx ==> M'x)
-- 4. Ex(Bx ^ not Fx)

-- Ex 2.33
-- 1. Vx(Dx ^ Bx => not B'y)
-- 2. Vx(Gx => not G'x)
-- 3. VxVy((Fxy ^ Fxd) ==> Fyd)
-- 4. Ve > 0 En&VVk&N( k>= n => 1/k < e)






