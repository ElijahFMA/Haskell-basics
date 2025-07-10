-- HC3 Combined Tasks

-- Task 1: Check if number is positive, negative, or zero
checkNumber :: Int -> String
checkNumber x =
  if x > 0 then "Positive"
  else if x < 0 then "Negative"
  else "Zero"

-- Task 2: Grade classification using guards
grade :: Int -> String
grade x
  | x >= 90 = "A"
  | x >= 80 = "B"
  | x >= 70 = "C"
  | x >= 60 = "D"
  | otherwise = "F"

-- Task 3: Convert RGB to hex string
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
  let toHex x = if x < 16 then '0' : showHex x "" else showHex x ""
  in '#' : toHex r ++ toHex g ++ toHex b
  where
    showHex x = (`showIntAtBase` intToDigit) 16 x ""

-- Task 4: Heron's formula for triangle area
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
  let s = (a + b + c) / 2
  in sqrt (s * (s - a) * (s - b) * (s - c))

-- Task 5: Triangle type using guards
triangleType :: Float -> Float -> Float -> String
triangleType a b c
  | a == b && b == c = "Equilateral"
  | a == b || b == c || a == c = "Isosceles"
  | otherwise = "Scalene"

-- Task 6: Check if a year is a leap year
isLeapYear :: Int -> Bool
isLeapYear year =
  if year `mod` 400 == 0 then True
  else if year `mod` 100 == 0 then False
  else if year `mod` 4 == 0 then True
  else False

-- Task 7: Determine season based on month
season :: Int -> String
season month
  | month == 12 || month == 1 || month == 2 = "Winter"
  | month >= 3 && month <= 5 = "Spring"
  | month >= 6 && month <= 8 = "Summer"
  | month >= 9 && month <= 11 = "Autumn"
  | otherwise = "Invalid month"

-- Task 8: BMI category using where
bmiCategory :: Float -> Float -> String
bmiCategory weight height
  | bmi < 18.5 = "Underweight"
  | bmi < 25.0 = "Normal"
  | bmi < 30.0 = "Overweight"
  | otherwise = "Obese"
  where bmi = weight / height ^ 2

-- Task 9: Max of three numbers using let
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c =
  let maxAB = max a b
      maxAll = max maxAB c
  in maxAll

-- Task 10: Check if a string is a palindrome
isPalindrome :: String -> Bool
isPalindrome s
  | length s <= 1 = True
  | head s == last s = isPalindrome (init (tail s))
  | otherwise = False

-- Higher-order function: Apply a function twice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Main function to test all tasks
main :: IO ()
main = do
  putStrLn "--- Task 1: checkNumber ---"
  print $ checkNumber 5
  print $ checkNumber (-3)
  print $ checkNumber 0

  putStrLn "\n--- Task 2: grade ---"
  print $ grade 95
  print $ grade 72
  print $ grade 50

  putStrLn "\n--- Task 3: rgbToHex ---"
  print $ rgbToHex (255, 0, 127)
  print $ rgbToHex (0, 255, 64)

  putStrLn "\n--- Task 4: triangleArea ---"
  print $ triangleArea 3 4 5
  print $ triangleArea 7 8 9

  putStrLn "\n--- Task 5: triangleType ---"
  print $ triangleType 3 3 3
  print $ triangleType 5 5 8
  print $ triangleType 6 7 8

  putStrLn "\n--- Task 6: isLeapYear ---"
  print $ isLeapYear 2000
  print $ isLeapYear 1900
  print $ isLeapYear 2024

  putStrLn "\n--- Task 7: season ---"
  print $ season 3
  print $ season 7
  print $ season 11

  putStrLn "\n--- Task 8: bmiCategory ---"
  print $ bmiCategory 70 1.75
  print $ bmiCategory 90 1.8

  putStrLn "\n--- Task 9: maxOfThree ---"
  print $ maxOfThree 10 20 15
  print $ maxOfThree 5 25 10

  putStrLn "\n--- Task 10: isPalindrome ---"
  print $ isPalindrome "racecar"
  print $ isPalindrome "haskell"
  print $ isPalindrome "madam"

  putStrLn "\n--- Higher Order: applyTwice ---"
  print $ applyTwice (+2) 3        -- 7
  print $ applyTwice reverse "abc" -- "abc"
