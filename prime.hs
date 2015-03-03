module GS
where

--divides :: Int -> Int -> Bool
divides d n = rem n d == 0

--ld :: Int -> Int
ld n = ldf 2 n

--ldf :: Int -> Int -> Int
ldf k n | divides k n = k
		| k^2 > n     = n
		| otherwise   = ldf (k + 1) n

prime0 :: Integer -> Bool
prime0 n | n < 1     = error "not a positive integer"
		 | n == 1    = False
		 | otherwise = ld n == n

--minInt :: [Int] -> Int
minInt []     = error "empty list"
minInt [x]    = x
minInt (x:xs) = min x (minInt xs)

maxInt :: [Int] -> Int
maxInt []     = error "empty list"
maxInt [x]    = x
maxInt (x:xs) = max x (maxInt xs)

-- removeFst :: Int -> [Int] -> [Int]
removeFst m [] = []
removeFst m (x:xs) | x == m    = xs
                   | otherwise = x : removeFst m xs

-- srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = minInt xs

average :: [Int] -> Rational
average [] = error "empty list"
average xs = toRational (sum xs) / toRational (length xs)

count :: Char -> String -> Int
count c [] = 0
count c (x:xs) | c == x = 1 + count c xs
			   | otherwise = count c xs

blowup :: String -> String
blowup s = bup 1 s

bup :: Int-> String -> String
bup n [] = []
bup n (c:cs) = (rep n c) ++ (bup (n + 1) cs)

rep :: Int -> Char -> String
rep n c | n == 0 = []
		| otherwise = [c] ++ (rep (n - 1) c)

--srtString :: [String] -> [String]
-- use srtInts

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

substring :: String -> String -> Bool
substring xs [] = False
substring xs ys | prefix xs ys = True
substring xs (y:ys) | substring xs ys = True
					| otherwise = False

factors :: Integer -> [Integer]
factors n | n < 1 = error "argument not positive"
		  | n == 1 = []
		  | otherwise = p : factors (div n p) where p = ld n

lengths :: [[a]] -> [Int]
lengths x = map length x

sumLengths :: [[a]] -> Int
sumLengths x = sum (lengths x)

primes0 :: [Integer]
primes0 = filter prime0 [2..]

ldp :: Integer -> Integer
ldp n = ldpf primes1 n

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0 = p
			  | p^2 > n      = n
			  | otherwise    = ldpf ps n

primes1 :: [Integer]
primes1 = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n | n < 1		= error "not a positive integer"
		| n == 1	= False
		| otherwise = ldp n == n

