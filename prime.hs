-- Ex 1.3 p5
divides d n = rem n d == 0

-- Ex 1.5 p8, 1.6 p9, 1.7 p9
ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

prime0 :: Integer -> Bool
prime0 n | n < 1          = error "not a positive integer"
         | n == 1         = False
         | otherwise      = ld n == n

-- Ex 1.9 p14
-- do not know yet how this type declaration goes 
-- lstf :: Func -> [Integer] -> Integer
lstf f []     = error "no elements in list"
lstf f [x]    = x
lstf f (x:xs) = f x (lstf f xs)

mnmInt l = lstf min l 
mxmInt l = lstf max l

-- Ex 1.10 p14
firstOn f x []                           = [] 
firstOn f x (y:ys) | (f x y) == True     = ys 
                   | otherwise           = y : (firstOn f x ys)

removeFst x l = firstOn (==) x l
