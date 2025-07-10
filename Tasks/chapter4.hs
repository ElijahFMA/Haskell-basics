-- HC4 Combined Tasks

-- Task 1: Pattern matching on weather condition
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

-- Task 2: Day type using pattern matching
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType day
  | day `elem` ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"] = "It's a weekday."
  | otherwise = "Invalid day"

-- Task 3: Grade comment using guards
gradeComment :: Int -> String
gradeComment grade
  | grade >= 90 && grade <= 100 = "Excellent!"
  | grade >= 70 && grade <= 89  = "Good job!"
  | grade >= 50 && grade <= 69  = "You passed."
  | grade >= 0  && grade <= 49  = "Better luck next time."
  | otherwise = "Invalid grade"

-- Task 4: Rewrite specialBirthday using pattern matching
specialBirthday :: Int -> String
specialBirthday 1  = "Happy 1st Birthday!"
specialBirthday 18 = "You're an adult now!"
specialBirthday 100 = "A century old! Congratulations!"
-- Task 5: Add catch-all pattern
specialBirthday age = "Happy Birthday! You're " ++ show age ++ " years old."

-- Task 6: Identify list content using pattern matching
whatsInsideThisList :: [a] -> String
whatsInsideThisList []       = "The list is empty."
whatsInsideThisList [_]      = "The list has one item."
whatsInsideThisList [_, _]   = "The list has two items."
whatsInsideThisList _        = "The list has many items."

-- Task 7: Ignore elements and extract first and third
firstAndThird :: [a] -> Maybe (a, a)
firstAndThird (x:_:z:_) = Just (x, z)
firstAndThird _         = Nothing

-- Task 8: Extract values from tuple
describeTuple :: (String, Int, Bool) -> String
describeTuple (name, age, isStudent) =
  name ++ " is " ++ show age ++ " years old and " ++
  (if isStudent then "is a student." else "is not a student.")

-- Higher-Order Function: apply a function twice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Main to test all tasks
main :: IO ()
main = do
  putStrLn "--- Task 1: weatherReport ---"
  print $ weatherReport "sunny"
  print $ weatherReport "rainy"
  print $ weatherReport "cloudy"
  print $ weatherReport "foggy"

  putStrLn "\n--- Task 2: dayType ---"
  print $ dayType "Saturday"
  print $ dayType "Tuesday"
  print $ dayType "Funday"

  putStrLn "\n--- Task 3: gradeComment ---"
  print $ gradeComment 95
  print $ gradeComment 75
  print $ gradeComment 60
  print $ gradeComment 40
  print $ gradeComment 150

  putStrLn "\n--- Task 4 & 5: specialBirthday ---"
  print $ specialBirthday 1
  print $ specialBirthday 18
  print $ specialBirthday 100
  print $ specialBirthday 25

  putStrLn "\n--- Task 6: whatsInsideThisList ---"
  print $ whatsInsideThisList ([] :: [Int])
  print $ whatsInsideThisList [1]
  print $ whatsInsideThisList [1, 2]
  print $ whatsInsideThisList [1, 2, 3]

  putStrLn "\n--- Task 7: firstAndThird ---"
  print $ firstAndThird [1, 2, 3, 4]
  print $ firstAndThird [5, 6]
  print $ firstAndThird [7]

  putStrLn "\n--- Task 8: describeTuple ---"
  print $ describeTuple ("Alice", 21, True)
  print $ describeTuple ("Bob", 35, False)

  putStrLn "\n--- Higher Order: applyTwice ---"
  print $ applyTwice (+3) 4             -- 10
  print $ applyTwice (2*) 5             -- 20
  print $ applyTwice reverse "abc"      -- "abc"
