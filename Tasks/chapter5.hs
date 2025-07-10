-- HC5 Combined Tasks

-- HC5T1: Apply a function three times
applyThreeTimes :: (a -> a) -> a -> a
applyThreeTimes f x = f (f (f x))

-- HC5T2: Filtering odd numbers from 1 to 30
filterOddNumbers :: [Int]
filterOddNumbers = filter odd [1..30]

-- HC5T3: Check if any word starts with uppercase letter
hasUppercaseStart :: [String] -> Bool
hasUppercaseStart = any (\word -> not (null word) && head word `elem` ['A'..'Z'])

-- HC5T4: Rewrite with lambda
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

-- HC5T5: Partial application
multiplyByFive :: Int -> Int
multiplyByFive = (5 *)

-- HC5T6: Function composition for even squares
evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)

-- HC5T7: Using the $ operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

-- HC5T8: Point-free style
addFive :: Int -> Int
addFive = (+5)

-- HC5T9: Apply a function twice to every element in a list
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

-- HC5T10: Combine filter, map, and any to check squared > 50
anySquareGreaterThan50 :: [Int] -> Bool
anySquareGreaterThan50 = any (>50) . map (^2) . filter (>0)

-- Higher-order function: applyTwice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Main function to test everything
main :: IO ()
main = do
  putStrLn "--- HC5T1: applyThreeTimes ---"
  print $ applyThreeTimes (+1) 4       -- 7
  print $ applyThreeTimes (*2) 1       -- 8

  putStrLn "\n--- HC5T2: filterOddNumbers ---"
  print filterOddNumbers

  putStrLn "\n--- HC5T3: hasUppercaseStart ---"
  print $ hasUppercaseStart ["hello", "World", "haskell"] -- True
  print $ hasUppercaseStart ["one", "two", "three"]       -- False

  putStrLn "\n--- HC5T4: biggerThan10 (lambda) ---"
  print $ biggerThan10 9               -- False
  print $ biggerThan10 15              -- True

  putStrLn "\n--- HC5T5: multiplyByFive ---"
  print $ multiplyByFive 6            -- 30

  putStrLn "\n--- HC5T6: evenSquares ---"
  print $ evenSquares [1..10]         -- [4,16,36,64,100]

  putStrLn "\n--- HC5T7: $ operator ---"
  print result                         -- 92

  putStrLn "\n--- HC5T8: Point-free addFive ---"
  print $ addFive 10                  -- 15

  putStrLn "\n--- HC5T9: transformList ---"
  print $ transformList (+1) [1,2,3]  -- [3,4,5]
  print $ transformList (*2) [1,2,3]  -- [4,8,12]

  putStrLn "\n--- HC5T10: anySquareGreaterThan50 ---"
  print $ anySquareGreaterThan50 [2, 4, 6, 7]   -- True (7^2 = 49, 6^2 = 36, 4^2 = 16)
  print $ anySquareGreaterThan50 [1, 2, 3]      -- False

  putStrLn "\n--- applyTwice ---"
  print $ applyTwice (+10) 5          -- 25
  print $ applyTwice reverse "abc"    -- "abc"
