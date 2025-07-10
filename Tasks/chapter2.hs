-- HC2T1 - Task 1: Checking Types in GHCi
-- Expected types (before checking in GHCi):
-- 42                :: Integer (default, can also be Int)
-- 3.14              :: Fractional a => a (default: Double)
-- "Haskell"         :: [Char] or String
-- 'Z'               :: Char
-- True && False     :: Bool

-- HC2T2 - Task 2: Function Type Signatures and Implementations
add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings a b = a ++ b

-- HC2T3 - Task 3: Immutable Variables
myAge :: Int
myAge = 25

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

-- If you try: `myAge = 30` later, GHC will raise an error:
-- "Multiple declarations of ‘myAge’"

-- HC2T4 - Task 4: Converting Between Notations
-- Infix to Prefix:
-- 5 + 3         -> (+) 5 3
-- 10 * 4        -> (*) 10 4
-- True && False -> (&&) True False

-- Prefix to Infix:
-- (+) 7 2       -> 7 + 2
-- (*) 6 5       -> 6 * 5
-- (&&) True False -> True && False

-- HC2T5 - Task 5: Defining and Using Functions
circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c = max a (max b c)

-- HC2T6 - Task 6: Int vs Integer
smallNumber :: Int
smallNumber = 2^62

bigNumber :: Integer
bigNumber = 2^127

-- Evaluating `2^64 :: Int` in GHCi might overflow on 64-bit systems.
-- Try: `let overflowTest = 2^64 :: Int`

-- HC2T7 - Task 7: Boolean Expressions + Higher-Order Function
trueAnd :: Bool
trueAnd = True && True

falseOr :: Bool
falseOr = False || False

trueNot :: Bool
trueNot = not False

comparisonFalse :: Bool
comparisonFalse = 10 > 20

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Main function to test all tasks
main :: IO ()
main = do
    putStrLn "--- HC2T2: Function Tests ---"
    print $ add 5 7                    -- 12
    print $ isEven 4                  -- True
    print $ isEven 5                  -- False
    putStrLn $ concatStrings "Hello, " "World!" -- "Hello, World!"

    putStrLn "\n--- HC2T3: Immutable Variables ---"
    print myAge
    print piValue
    putStrLn greeting
    print isHaskellFun

    putStrLn "\n--- HC2T4: Notation Conversion ---"
    print $ (+) 5 3                    -- 8
    print $ (*) 10 4                   -- 40
    print $ (&&) True False           -- False
    print $ 7 + 2                     -- 9
    print $ 6 * 5                     -- 30
    print $ True && False            -- False

    putStrLn "\n--- HC2T5: Function Usage ---"
    print $ circleArea 5              -- 78.53982
    print $ maxOfThree 10 25 7        -- 25

    putStrLn "\n--- HC2T6: Int vs Integer ---"
    print smallNumber                 -- 4611686018427387904
    print bigNumber                   -- A very large number

    putStrLn "\n--- HC2T7: Boolean Expressions ---"
    print trueAnd                     -- True
    print falseOr                     -- False
    print trueNot                     -- True
    print comparisonFalse             -- False

    putStrLn "\n--- HC2T7: applyTwice Tests ---"
    print $ applyTwice (+2) 5         -- 9
    print $ applyTwice (*2) 3         -- 12
    print $ applyTwice reverse "abc"  -- "abc"
