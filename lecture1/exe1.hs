-- Exercise 1

doubleMe :: Integer -> Integer 
doubleMe x = x + x

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
        | n < 0 = []
        | otherwise = toDigits (dropLastDigit n) ++ [lastDigit n]

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
        | n < 0 = []
        | otherwise = [lastDigit n] ++ toDigitsRev (dropLastDigit n)

--Exercise 2

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev (x:(y:zs)) = x : doubleMe y : doubleEveryOtherRev zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse( doubleEveryOtherRev (reverse n))

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = sum(toDigits x)
sumDigits (x:y) = sum(toDigits x) + sumDigits y

-- Exercise 4

validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther(toDigits n)) `mod` 10 == 0

-- Exercise 5 Hanoi

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 pa pb pc = []
hanoi n pa pb pc = (hanoi (n - 1) pa pc pb) 
    ++ [(pa, pb)]
    ++ (hanoi (n - 1) pc pb pa) 

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n pa pb pc pd
                | n < 4 = hanoi n pa pb pd
hanoi4                 
[(pa,pb),(pa,pc),(pb,pc),(pa,pb),(pa,pd),(pb,pd)(pc,pb),(pc,pd),(pb,pd)]
hanoi4 n pa pb pc pd = (hanoi (n - 1) pa pd pc pb) 
    ++ [(pa, pd)]
    ++ (hanoi (n - 1) pb pa pc pd) 


