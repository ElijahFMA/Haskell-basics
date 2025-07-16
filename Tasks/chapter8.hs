-- HC8T1
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx from to amount = "From: " ++ from ++ ", To: " ++ to ++ ", Amount: " ++ show amount

main1 :: IO ()
main1 = putStrLn (generateTx "addr1" "addr2" 100)


-- HC8T2
data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person1 = Person1
  { name1 :: String
  , address1 :: (String, Int)
  , paymentMethod :: PaymentMethod
  } deriving Show

bob :: Person1
bob = Person1 "Bob" ("Main St", 123) Cash

main2 :: IO ()
main2 = print bob


-- HC8T3
data Shape = Circle Float | Rectangle Float Float deriving Show

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

main3 :: IO ()
main3 = do
  print (area (Circle 5))
  print (area (Rectangle 10 5))


-- HC8T4
data Employee = Employee
  { name :: String
  , experienceInYears :: Float
  } deriving Show

richard :: Employee
richard = Employee "Richard" 7.5

main4 :: IO ()
main4 = print richard


-- HC8T5
data Person2 = Person2
  { name2 :: String
  , age :: Int
  , isEmployed :: Bool
  } deriving Show

person1 :: Person2
person1 = Person2 "Alice" 30 True

person2 :: Person2
person2 = Person2 "Eve" 22 False

main5 :: IO ()
main5 = do
  print person1
  print person2


-- HC8T6
data Shape2
  = Circle2 { center :: (Float, Float), color :: String, radius :: Float }
  | Rectangle2 { width :: Float, height :: Float, color :: String }
  deriving Show

circleInstance :: Shape2
circleInstance = Circle2 (0, 0) "Red" 10

rectangleInstance :: Shape2
rectangleInstance = Rectangle2 20 15 "Blue"

main6 :: IO ()
main6 = do
  print circleInstance
  print rectangleInstance


-- HC8T7
data Animal = Dog String | Cat String deriving Show

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "A dog named " ++ name
describeAnimal (Cat name) = "A cat named " ++ name

main7 :: IO ()
main7 = do
  let dog = Dog "Rex"
  let cat = Cat "Whiskers"
  putStrLn (describeAnimal dog)
  putStrLn (describeAnimal cat)


-- HC8T8
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet name age = "Hello, " ++ name ++ "! You are " ++ show age ++ " years old."

main8 :: IO ()
main8 = putStrLn (greet "Charlie" 25)


-- HC8T9
data Transaction = Transaction
  { from :: Address
  , to :: Address
  , amount :: Value
  , transactionId :: String
  } deriving Show

createTransaction :: Address -> Address -> Value -> String
createTransaction fromAddr toAddr amt =
  let txId = "TX-" ++ take 6 (fromAddr ++ toAddr ++ show amt)
  in txId

main9 :: IO ()
main9 = putStrLn (createTransaction "addrX" "addrY" 150)


-- HC8T10
data Book = Book
  { title :: String
  , author :: String
  , year :: Int
  } deriving (Show)

myBook :: Book
myBook = Book "Haskell in Depth" "Vitaly Bragilevsky" 2021

main10 :: IO ()
main10 = print myBook


-- Combine all main functions (optional)
main :: IO ()
main = do
  putStrLn "\n--- HC8T1 ---"
  main1
  putStrLn "\n--- HC8T2 ---"
  main2
  putStrLn "\n--- HC8T3 ---"
  main3
  putStrLn "\n--- HC8T4 ---"
  main4
  putStrLn "\n--- HC8T5 ---"
  main5
  putStrLn "\n--- HC8T6 ---"
  main6
  putStrLn "\n--- HC8T7 ---"
  main7
  putStrLn "\n--- HC8T8 ---"
  main8
  putStrLn "\n--- HC8T9 ---"
  main9
  putStrLn "\n--- HC8T10 ---"
  main10
