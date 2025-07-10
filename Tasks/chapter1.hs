-- HC1T1 - Task 1: Function Composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

-- HC1T2 - Task 2: Pure Function Example
circleArea :: Floating a => a -> a
circleArea r = pi * r * r

-- HC1T3 - Task 3: Checking if a Number is Greater than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

-- HC1T4 - Task 4: Composing a Function to Process Player Data
type Player = (String, Int)

extractPlayers :: [Player] -> [String]
extractPlayers players = map fst players

sortByScore :: [Player] -> [Player]
sortByScore = reverse . quicksort
  where
    quicksort [] = []
    quicksort (x:xs) = quicksort [y | y <- xs, snd y <= snd x]
                     ++ [x] ++
                     quicksort [y | y <- xs, snd y > snd x]

topThree :: [Player] -> [Player]
topThree = take 3

getTopThreePlayers :: [Player] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- HC1T5 - Task 5: Laziness in Haskell
infiniteNumbers :: [Integer]
infiniteNumbers = [1..]

takeFirstN :: Int -> [Integer]
takeFirstN n = take n infiniteNumbers

-- HC1T6 - Task 6: Using Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- HC1T7 - Task 7: Converting Fahrenheit to Celsius
fToC :: Floating a => a -> a
fToC f = (f - 32) * 5 / 9

-- HC1T8 - Task 8: Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Main function to test all tasks
main :: IO ()
main = do
    putStrLn "--- Task 1 ---"
    print $ doubleThenIncrement 4      -- Should print 9

    putStrLn "--- Task 2 ---"
    print $ circleArea 5               -- Should print 78.53981633974483

    putStrLn "--- Task 3 ---"
    print $ greaterThan18 20           -- Should print True
    print $ greaterThan18 10           -- Should print False

    putStrLn "--- Task 4 ---"
    let players = [("Alice", 10), ("Bob", 30), ("Charlie", 20), ("Diana", 25)]
    print $ getTopThreePlayers players -- Should print ["Bob","Diana","Charlie"]

    putStrLn "--- Task 5 ---"
    print $ takeFirstN 10              -- Should print [1,2,3,4,5,6,7,8,9,10]

    putStrLn "--- Task 6 ---"
    print $ addNumbers 3 5             -- Should print 8

    putStrLn "--- Task 7 ---"
    print $ fToC 98.6                  -- Should print ~37.0

    putStrLn "--- Task 8 ---"
    print $ applyTwice (+3) 4          -- Should print 10
    print $ applyTwice (*2) 5          -- Should print 20
