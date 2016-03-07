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
-- lstf :: (Ord a) => a -> a -> a -> [Integer] -> Integer
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

-- Ex 1.13 p16
count :: Char -> String -> Integer
count c []                 = 0
count c (x:xs) | x == c    = 1 + (count c xs)
               | otherwise = count c xs

-- Ex 1.14 p16
t_appendR :: String -> String -> Integer -> String
t_appendR c s i | i < 0     = error "impossible to append negative times" 
                | i == 0    = c            
                | otherwise = s ++ (t_appendR c c (i - 1))

t_blowupIdx :: Integer -> String -> String
t_blowupIdx i []     = ""
t_blowupIdx i (x:xs) = (t_appendR [x] [x] i) ++ (t_blowupIdx (i + 1) xs) 

t_blowup :: String -> String
t_blowup s = t_blowupIdx 1 s

-- Solution introduces nicely the copy of chars concept
-- already transforming Char -> String instead of x -> [x] as in t_blowupIdx
--- resulting in a cleaner solution
copy :: Int -> Char -> String
copy 0 c       = []
copy n c       = c : (copy (n - 1) c) 

-- the rest is based on same principle
blowup :: String -> String
blowup s = blowup' s 1

blowup' :: String -> Int -> String
blowup' [] n     = []
blowup' (x:xs) n = (copy n x) ++ (blowup' xs (n + 1))

-- Ex 1.15 p16
mnmChr :: String -> Char
mnmChr x = lstf min x

srtString :: String -> String
srtString [] = []
srtString x  = m : srtString((removeFst m x)) where m = mnmChr x

-- Ex 1.17
prefix :: String -> String -> Bool
prefix [] y          = True
prefix x []          = False
prefix (x:xs) (y:ys) = (x == y) && (prefix xs ys)
 
substring :: String -> String -> Bool
substring [] ys    = True -- i had forgotten the substring [] [] case
substring x []     = False
substring x (y:ys) | prefix x (y:ys) = True
                   | otherwise = substring x ys 

-- Thought the answer provided was a bit overdone
substring' :: String -> String -> Bool 
substring' [] ys         = True 
substring' (x:xs) []     = False
substring' (x:xs) (y:ys) = ((x==y) && (prefix xs ys)) || (substring' (x:xs) ys)

-- Ex 1.18 p18
-- too many :)

-- Ex 1.19
-- use interpreter

pfactors :: Integer -> [Integer]
pfactors n | n < 1     = error "argument not positive"
           | n == 1    = [] 
           | otherwise = p : pfactors (div n p) where p = ld n
           
-- Ex 1.20 p22
lengths ::  [[a]] -> [Int]
lengths x = map length x

-- Ex 1.21 p22
sumLengths :: [[a]] -> Int
sumLengths x = sum (lengths x) -- same as sum( map length x) see ex 1.20



