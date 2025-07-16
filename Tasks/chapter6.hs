-- HC6T1: Factorial using recursion
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main1 :: IO ()
main1 = print $ factorial 5  -- Output: 120

-- HC6T2: Fibonacci using recursion
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main2 :: IO ()
main2 = print $ fibonacci 10  -- Output: 55

-- HC6T3: Sum of list using foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0

main3 :: IO ()
main3 = print $ sumList [1, 2, 3, 4, 5]  -- Output: 15

-- HC6T4: Product of list using foldl
productList :: [Int] -> Int
productList = foldl (*) 1

main4 :: IO ()
main4 = print $ productList [1, 2, 3, 4]  -- Output: 24

-- HC6T5: Reverse list using recursion
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

main5 :: IO ()
main5 = print $ reverseList [1, 2, 3, 4]  -- Output: [4,3,2,1]

-- HC6T6: Element exists in list
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists y (x:xs)
  | y == x    = True
  | otherwise = elementExists y xs

main6 :: IO ()
main6 = print $ elementExists 3 [1, 2, 3, 4]  -- Output: True

-- HC6T7: Length of list
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

main7 :: IO ()
main7 = print $ listLength [1, 2, 3, 4, 5]  -- Output: 5

-- HC6T8: Filter even numbers
filterEvens :: [Int] -> [Int]
filterEvens = filter even

main8 :: IO ()
main8 = print $ filterEvens [1, 2, 3, 4, 5, 6]  -- Output: [2,4,6]

-- HC6T9: Map function implementation
mapFunction :: (a -> b) -> [a] -> [b]
mapFunction _ [] = []
mapFunction f (x:xs) = f x : mapFunction f xs

main9 :: IO ()
main9 = print $ mapFunction (*2) [1, 2, 3, 4]  -- Output: [2,4,6,8]

-- HC6T10: Digits of a number using recursion
digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

main10 :: IO ()
main10 = print $ digits 12345  -- Output: [1,2,3,4,5]
