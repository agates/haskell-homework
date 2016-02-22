doubleMe :: (Num a) => a -> a
doubleMe x = x + x

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Num a, Ord a) => a -> a
doubleSmallNumber x = if x > 100
                        then x
                        else doubleMe x

boomBangs :: (Integral a) => [a] -> [String]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs
--length' l = sum [1 | _ <- l]

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' xs = foldr (+) 0 xs
--sum' (x: xs) = x + sum' xs

removeNonUpperCase :: String -> String
removeNonUpperCase st = [ c | c <- st, c `elem` ['A'..'Z']]

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

circumference :: (Floating a) => a -> a
circumference r = 2 * pi * r

fst' :: (a, b) -> a
fst' (a, _) = a

fibonacci :: (Integral n) => n -> n
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs
