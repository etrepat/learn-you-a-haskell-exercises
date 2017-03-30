-- 02-starting-out.hs
--
-- Once you've installed Haskell from http://www.haskell.org/platform/, load the interpreter with the command ghci.
--
-- You can load (and reload) this file in the interpreter with the command: ":l 2-starting-out.hs"
--
-- The first function has been completed as an example. All the other functions are undefined.
-- They can be implemented in one line using the material covered in http://learnyouahaskell.com/starting-out
--
-- All indices are zero based.


-- Find the penultimate element in list l
penultimate l = last (init l)

-- Find the element at index k in list l
-- For example: "findK 2 [0,0,1,0,0,0]" returns 1
findK k xs = xs !! k

-- Determine if list l is a palindrome
isPalindrome xs = xs == reverse xs

-- Duplicate the elements in list xs, for example "duplicate [1,2,3]" would give the list [1,1,2,2,3,3]
-- Hint: The "concat [l]" function flattens a list of lists into a single list.
-- (You can see the function definition by typing ":t concat" into the interpreter. Perhaps try this with other variables and functions)
--
-- For example: concat [[1,2,3],[3,4,5]] returns [1,2,3,3,4,5]
duplicate xs = concat [ [x, x] | x <- xs ]

-- Imitate the functinality of zip
-- The function "min x y" returns the lower of values x and y
-- For example "ziplike [1,2,3] ['a', 'b', 'c', 'd']" returns [(1,'a'), (2, 'b'), (3, 'c')]
ziplike xs ys = [ (xs !! i, ys !! i) | i <- [0..(min (length xs) (length ys) - 1)] ]
-- (It's better in my mind to think of this as a simple recursive iteration instead of a list
-- comprehension...)
ziplike' xs ys = reverse (ziptmp [] xs ys)
  where ziptmp os [] _ = os
        ziptmp os _ [] = os
        ziptmp os (x:xs') (y:ys') = ziptmp ((x,y) : os) xs' ys'

-- Split a list xs at element k into a tuple: The first part up to and including k, the second part after k
-- For example "splitAtIndex 3 [1,1,1,2,2,2]" returns ([1,1,1],[2,2,2])
splitAtIndex k xs = (take k xs, drop k xs)

-- Drop the element at index k in list xs
-- For example "dropK 3 [0,0,0,1,0,0,0]" returns [0,0,0,0,0,0]
dropK k xs = (take k xs) ++ (drop (k+1) xs)

-- Extract elements between ith and kth element in list xs. Including i, but not k
-- For example, "slice 3 6 [0,0,0,1,2,3,0,0,0]" returns [1,2,3]
slice i k xs = take (k-i) (drop i xs)

-- Insert element x in list xs at index k
-- For example, "insertElem 2 5 [0,0,0,0,0,0]" returns [0,0,0,0,0,2,0]
insertElem x k xs = take k xs ++ [x] ++ drop k xs

-- Rotate list l n places left.
-- For example, "rotate 2 [1,2,3,4,5]" gives [3,4,5,1,2]
rotate n xs = drop n xs ++ take n xs
