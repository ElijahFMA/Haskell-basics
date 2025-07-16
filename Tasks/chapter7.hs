-- HC7T1: Define Color and implement Eq
data Color = Red | Green | Blue deriving (Show, Read, Enum, Bounded)

instance Eq Color where
  Red   == Red   = True
  Green == Green = True
  Blue  == Blue  = True
  _     == _     = False

main1 :: IO ()
main1 = print (Red == Red, Green == Blue)  -- Output: (True,False)

-- HC7T2: Implement Ord for Color (Red < Green < Blue)
instance Ord Color where
  compare Red   Red   = EQ
  compare Red   _     = LT
  compare Green Red   = GT
  compare Green Green = EQ
  compare Green Blue  = LT
  compare Blue  Blue  = EQ
  compare Blue  _     = GT

main2 :: IO ()
main2 = print (Red < Green, Blue > Green)  -- Output: (True,True)

-- HC7T3: Function with Eq and Ord constraints
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues a b = if a >= b then a else b

main3 :: IO ()
main3 = print $ compareValues 42 27  -- Output: 42

-- HC7T4: Custom type Shape with Show and Read
data Shape = Circle Double | Rectangle Double Double

instance Show Shape where
  show (Circle r) = "Circle " ++ show r
  show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
  readsPrec _ input = 
    case words input of
      ("Circle":r:_) -> [(Circle (read r), "")]
      ("Rectangle":w:h:_) -> [(Rectangle (read w) (read h), "")]
      _ -> []

main4 :: IO ()
main4 = do
  let s = Circle 3.0
  print s
  print (read "Rectangle 4.0 5.0" :: Shape)

-- HC7T5: Function using Num
squareArea :: Num a => a -> a
squareArea side = side * side

main5 :: IO ()
main5 = print $ squareArea 4  -- Output: 16

-- HC7T6: Function using Integral and Floating
circleCircumference :: (Real a, Floating b) => a -> b
circleCircumference r = 2 * pi * realToFrac r

main6 :: IO ()
main6 = print $ circleCircumference (5 :: Int)  -- Output: 31.4159...

-- HC7T7: nextColor using Bounded and Enum
nextColor :: Color -> Color
nextColor c
  | c == maxBound = minBound
  | otherwise = succ c

main7 :: IO ()
main7 = print $ map nextColor [Red, Green, Blue]  -- Output: [Green,Blue,Red]

-- HC7T8: Parse Shape from String safely
parseShape :: String -> Maybe Shape
parseShape s = case reads s of
  [(shape, "")] -> Just shape
  _             -> Nothing

main8 :: IO ()
main8 = do
  print $ parseShape "Circle 2.5"  -- Just (Circle 2.5)
  print $ parseShape "Unknown"     -- Nothing

-- HC7T9: Describable type class for Bool and Shape
class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True  = "This is true."
  describe False = "This is false."

instance Describable Shape where
  describe (Circle r) = "A circle with radius " ++ show r
  describe (Rectangle w h) = "A rectangle with width " ++ show w ++ " and height " ++ show h

main9 :: IO ()
main9 = do
  print $ describe True
  print $ describe (Rectangle 3 5)

-- HC7T10: Function with multiple constraints
describeAndCompare :: (Describable a, Eq a, Ord a) => a -> a -> String
describeAndCompare a b = describe (compareValues a b)

main10 :: IO ()
main10 = do
  let s1 = Rectangle 2 3
      s2 = Rectangle 4 4
  putStrLn $ describeAndCompare s1 s2
