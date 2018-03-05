import Data.List (sortBy)  
-- Real World Haskell/ch02/Practise
-- Return the last second element in a list

--data Lbo = Just Int | TypeError
--lbo :: [a] -> Int -> Lbo
--lbo [] 2 = TypeError
--lbo [] 1 = TypeError
lbo xs 1 = last xs
lbo xs n = lbo (init xs) (n-1)
lastButOne xs = lbo xs 2
--lastButOne xs = Just (head (drop ((length xs) - 2) xs))



-- Real World Haskell/ch03/BookStore.hs
data BookInfo = Book Int String [String]
                deriving (Show)
myInfo = Book 9780135072455 "Algebra of Programming"
              ["Richard Bird", "Oege de Moor"]

data NN = T | F Int
        deriving (Show)



-- Real World Haskell/ch03/Practise 01, 02
-- Write a function that computes the number of elements in a list. To test it, ensure that it gives the same answers as the standard length function
mylen :: [a] -> Int
mylen [] = 0
mylen (x:xs) = 1 + mylen xs


-- Real World Haskell/ch03/Practise 03
-- Add a type signature for your function to your source file. To test it, load the source file into ghci again.
mysum :: [Float] -> Float
mysum [] = 0.0
mysum (x:xs) = x + mysum xs

mymean :: [Float] -> Float
mymean [] = 0.0
mymean a =  mysum a / fromIntegral (mylen a) 
-- or 
--mysum :: [Int] -> Int
--mysum [] = 0
--mysum (x:xs) = x + mysum xs
--mymean :: [Int] -> Float
--mymean [] = 0.0
--mymean a =  fromIntegral (mysum a) / fromIntegral (mylen a) 



-- Real World Haskell/ch03/Practise 04
-- Turn a list into a palindrome, i.e. it should read the same both backwards and forwards. For example, given the list [1,2,3], your function should return [1,2,3,3,2,1]
palin :: [a] -> [a]
palin [] = []
-- palin (x:[]) = [x]
palin (x:xs) = [x] ++ palin xs ++ [x]


-- Real World Haskell/ch03/Practise 05
-- Write a function that determines whether its input list is a palindrome
isPalin :: Eq a => [a] -> Bool
isPalin [] = True
isPalin (x:[]) = True
isPalin (x:xs) = if x == last xs 
                 then isPalin (init xs)
                 else False

-- Real World Haskell/ch03/Practise 06
-- Create a function that sorts a list of lists based on the length of each sublist. (You may want to look at the sortBy function from the Data.List module.)                 
-- ?
sortList :: [a] -> [a]
sortList xs = sortBy (\x y -> length x `compare` length y) xs 


-- Real World Haskell/ch03/Practise 07
-- Define a function that joins a list of lists together using a separator value.
intersperse :: a -> [[a]] -> [a]
intersperse a [] = []
intersperse a [x] = x
intersperse a (x:xs) = x++[a]++ intersperse a xs
-- intersperse 0 [[1,2,3],[6,7,8]]              >>  [1,2,3,0,6,7,8]
-- intersperse ',' []                           >>  ""
-- intersperse ',' ["foo"]                      >>  "foo"
-- intersperse ',' ["foo","bar","baz","quux"]   >>  "foo,bar,baz,quux"



