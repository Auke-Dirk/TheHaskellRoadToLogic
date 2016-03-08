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