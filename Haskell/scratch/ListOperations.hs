{- 
	This file recreates some basic list functions from the ground up.  Each 
	function is the same name as the Prelude funtion with List appeneded
	to the end.
-}

-- Returns the first element of a list.
headList :: [a] -> a
headList (x:_) = x

-- Returns a list with the first element removed.
tailList :: [a] -> [a]
tailList (_:xs) = xs 

-- Returns the last element of list.
lastList :: [a] -> a
lastList (x:[]) = x
lastList (_:xs) = lastList xs

-- Returns a list with the last element removed.
initList :: [a] -> [a]
initList (x:[]) = []
initList (x:xs) = x : (initList xs)

-- Returns the number of elements in the list.
lengthList :: [a] -> Int
lengthList []     = 0
lengthList (_:xs) = 1 + lengthList xs

-- Returns the element at the given index.
-- TODO: index out of bounds exception
indexList :: [a] -> Int -> a
indexList (x:_) 0  = x
indexList (_:xs) n = indexList xs (n - 1)

-- Combines two lists.  The second list will be added to the end of the first 
-- list.
-- Note: the concat operator (++) in Haskell is also inefficient.  That is, the 
-- runtime of the algorithm is dependent on the size of the first list.
concatLists :: [a] -> [a] -> [a]
concatLists [] ys     = ys
concatLists (x:xs) ys = x : concatLists xs ys

-- Reverses the order of the elements of a list.
reverseList :: [a] -> [a]
reverseList [] = []
reverseList xs = lastList xs : reverseList (initList xs)

-- Returns True iff the list is empty
nullList :: [a] -> Bool
nullList [] = True
nullList xs = False

-- Returns the first n elements of a given list.
takeList :: Int -> [a] -> [a]
takeList 0 _      = []
takeList n []     = []
takeList n (x:xs) = x : takeList (n-1) xs

-- Drops the first n elements of a given list.
dropList :: Int -> [a] -> [a]
dropList 0 xs     = xs
dropList n []     = []
dropList n (_:xs) = dropList (n-1) xs

-- Returns the smallest number of an Int list
-- TODO: modify for any orderable type. Just modify the type signature?
minimumIntList :: [Int] -> Int
minimumIntList (x:[]) = x
minimumIntList (x:(y:zs))
  | x <= y    = minimumIntList (x : zs)
  | otherwise = minimumIntList (y : zs)
 
-- Returns the largest number of an Int list
-- TODO: modify for any orderable type. Just modify the type signature? 
maximumIntList :: [Int] -> Int
maximumIntList (x:[]) = x
maximumIntList (x:(y:zs))
  | x >= y    = maximumIntList (x : zs)
  | otherwise = maximumIntList (y : zs)
  
-- Returns the sum of a list of numbers
-- TODO: modify for any number type, not just Int.
sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList xs

-- Returns the product of a list of numbers
-- TODO: modify for any number type, not just Int.
productList :: [Int] -> Int
productList []     = 1
productList (x:xs) = x * productList xs

-- Returns True iff the specified INT element in is the list.
-- TODO: modify for any generic type.
elemList :: Int -> [Int] -> Bool
elemList _ [] = False
elemList x (y:ys)
  | (x == y) == True = True
  | otherwise        = elemList x ys
  
-- Takes a list and returns an infinite concatenation of itself
-- TODO: Lazy evaluation makes this impossible.
--cycleList :: [a] -> [a]
--cycleList xs = cycleList (concatLists xs xs)

-- Takes an element and produces an infinite list of it.
-- TODO: Depends on cycleList... Lazy evaluation makes this impossible now.
--repeatList :: a -> [a]
--repeatList a = cycleList (a:[])

-- Takes an element and makes a list out of it with the specified number of 
-- elements.
replicateList :: Int -> a -> [a]
replicateList 0 _ = []
replicateList n x = x : replicateList (n-1) x