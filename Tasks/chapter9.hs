-- HC9T1: Define a Parametric Type Synonym
type Entity a = (String, a)  -- e.g., name and address of any type

main1 :: IO ()
main1 = do
    let person :: Entity String
        person = ("John Doe", "123 Elm Street")
    print person

-- HC9T2: Implement a Parametric Data Type
data Box a = Empty | Has a deriving (Show)

main2 :: IO ()
main2 = do
    let box1 = Has 42
        box2 = Empty
    print box1
    print box2

-- HC9T3: Define a Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN n (Has x) = Has (n + x)
addN _ Empty   = Empty

main3 :: IO ()
main3 = do
    print $ addN 5 (Has 10)   -- Has 15
    print $ addN 5 Empty      -- Empty

-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract _ (Has x) = x
extract def Empty = def

main4 :: IO ()
main4 = do
    print $ extract 0 (Has 20)   -- 20
    print $ extract 0 Empty      -- 0

-- HC9T5: Create a Parametric Data Type with Record Syntax
data Shape a = Circle { color :: a }
             | Rectangle { color :: a }
             deriving (Show)

main5 :: IO ()
main5 = do
    let s1 = Circle "Red"
        s2 = Rectangle "Blue"
    print s1
    print s2
    print $ color s1

-- HC9T6: Define a Recursive Data Type
data Tweet = Tweet {
    content  :: String,
    likes    :: Int,
    comments :: [Tweet]
} deriving (Show)

main6 :: IO ()
main6 = do
    let t1 = Tweet "Hello" 5 []
        t2 = Tweet "Reply" 3 [t1]
    print t2

-- HC9T7: Function to Calculate Engagement
engagement :: Tweet -> Int
engagement (Tweet _ likes comments) = likes + sum (map engagement comments)

main7 :: IO ()
main7 = do
    let t1 = Tweet "First" 5 []
        t2 = Tweet "Second" 3 [t1]
        t3 = Tweet "Third" 2 [t2, t1]
    print $ engagement t3  -- Should be 5 + 3 + 5 + 2 = 15

-- HC9T8: Recursive Sequence Data Type
data Sequence a = End | Node a (Sequence a) deriving (Show)

main8 :: IO ()
main8 = do
    let seq1 = Node 1 (Node 2 (Node 3 End))
    print seq1

-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node y ys) = x == y || elemSeq x ys

main9 :: IO ()
main9 = do
    let seq = Node 1 (Node 2 (Node 3 End))
    print $ elemSeq 2 seq   -- True
    print $ elemSeq 4 seq   -- False

-- HC9T10: Binary Search Tree Data Type
data BST a = Leaf
           | Branch a (BST a) (BST a)
           deriving (Show)

main10 :: IO ()
main10 = do
    let tree = Branch 10
                (Branch 5 Leaf Leaf)
                (Branch 15 Leaf Leaf)
    print tree
