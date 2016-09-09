import Data.List.Split
import Data.List

main = putStrLn $ show (lotusSolver [5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0])

--input: Index x to examine
--output: The indices of the Petal associated with input index
getPetalIndices :: Int -> [Int]
--if x is an element of the list, return that same list
--based on constant petal indices
getPetalIndices x
   |(elem x [0..6])   = [0..6]
   |(elem x [7..13])  = [7..13]
   |(elem x [14..20]) = [14..20]
   |(elem x [21..27]) = [21..27]
   |(elem x [28..34]) = [28..34]
   |(elem x [34..41]) = [34..41]
   |(elem x [41..48]) = [41..48]
   
--input: Index x to examine
--output: The indices of the Right Arc associated with input index
getArcRIndices :: Int -> [Int]
--if x is an element of the list, return that same list
--based on constant Right Arc indices
getArcRIndices x
   |(elem x [0,7,15,22,30,37,45])  = [0,7,15,22,30,37,45]
   |(elem x [1,8,16,23,31,38,46])  = [1,8,16,23,31,38,46]
   |(elem x [2,9,17,24,32,39,47])  = [2,9,17,24,32,39,47]
   |(elem x [3,10,18,25,33,40,48]) = [3,10,18,25,33,40,48]
   |(elem x [4,11,19,26,34,41,42]) = [4,11,19,26,34,41,42]
   |(elem x [5,12,20,27,28,35,43]) = [5,12,20,27,28,35,43]
   |(elem x [6,13,14,21,29,36,44]) = [6,13,14,21,29,36,44]

--input: Index x to examine 
--output: The indices of the Left Arc associated with input index
getArcLIndices :: Int -> [Int]
--if x is an element of the list, return that same list
--based on constant Left Arc indices
getArcLIndices x
   |(elem x [0,13,20,26,33,39,46]) = [0,13,20,26,33,39,46]
   |(elem x [1,7,14,27,34,40,47])  = [1,7,14,27,34,40,47]
   |(elem x [2,8,15,21,28,41,48])  = [2,8,15,21,28,41,48]
   |(elem x [3,9,16,22,29,35,42])  = [3,9,16,22,29,35,42]
   |(elem x [4,10,17,23,30,36,43]) = [4,10,17,23,30,36,43]
   |(elem x [5,11,18,24,31,37,44]) = [5,11,18,24,31,37,44]
   |(elem x [6,12,19,25,32,38,45]) = [6,12,19,25,32,38,45]

--input: Indices to get from puzzle, the puzzle
--output: The 7 constants of the puzzle based on indices
getConstants :: [Int] -> [Int] -> [Int]
--Finds the xth element of ys and recurses until xs is empty
getConstants [] (ys) = []
getConstants (x:xs) (ys) = (ys!!x):(getConstants xs ys)

--removes the 0s from the list
removeZeros :: [Int] -> [Int]
removeZeros [] = []
removeZeros xs = filter (>0) xs

   
--input: a Petal, Right Arc, or Left Arc xs
--output: Valid possible solutions for 0s in the Petal, Right Arc, or Left Arc
getPossible ::[Int] -> [Int]
--compare lists
getPossible xs = [1..7] \\ xs

--input: an index x, the puzzle ys
--output: Possible solutions for index based on Petal Analysis
getPetalPossible :: Int -> [Int] -> [Int]
getPetalPossible x ys = (getPossible (getConstants (getPetalIndices x) ys))

--input: an index x, the puzzle ys
--output: Possible solutions for index based on Right Arc Analysis
getArcRPossible :: Int -> [Int] -> [Int]
getArcRPossible x ys = (getPossible (getConstants (getArcRIndices x) ys))

--input: an index x, the puzzle ys
--output: Possible solutions for index based on Left Arc Analysis
getArcLPossible :: Int -> [Int] -> [Int]
getArcLPossible x ys = (getPossible (getConstants (getArcLIndices x) ys))

--input: an index x, the puzzle ys
--output: overall possible solutions for the index
getOverallPossible :: Int -> [Int] -> [Int]
--finds matching possibles in petals and right and left arcs
getOverallPossible x ys = (((getPetalPossible x ys) `intersect` (getArcRPossible x ys)) `intersect` (getArcLPossible x ys))

--input: an index, the puzzle, a new value
--output: inserts a new value z at an index x in the puzzle ys
insertValue :: Int -> [Int] -> Int -> [Int]
insertValue x ys z = take x ys ++ [z] ++ drop (x + 1) ys

-- Find the next blank value starting from index x on board ys
-- 48 is the index of the last element in s
nextZero :: Int -> [Int] -> Int
nextZero x ys
   | x == 48           = 48
   | ys !! (x) == 0    =  x
   | otherwise         = nextZero (x + 1) ys
	
--Determines if petals are all valid
checkPetals :: [Int] -> Bool
checkPetals xs
   |(isRepeated(removeZeros (getConstants [0..6] xs)) == False) &&
    (isRepeated(removeZeros (getConstants [7..13] xs)) == False) &&
	(isRepeated(removeZeros (getConstants [14..20] xs)) == False) &&
	(isRepeated(removeZeros (getConstants [21..27] xs)) == False) &&
	(isRepeated(removeZeros (getConstants [28..34] xs)) == False) &&
	(isRepeated(removeZeros (getConstants [35..41] xs)) == False) &&
	(isRepeated(removeZeros (getConstants [42..48] xs)) == False) = True
   |otherwise                                       = False
   
--Determines all right arcs are valid
checkArcR :: [Int] -> Bool
checkArcR xs
   |(isRepeated(removeZeros (getConstants [0,7,15,22,30,37,45] xs)) == False) &&
    (isRepeated(removeZeros (getConstants [1,8,16,23,31,38,46] xs)) == False) &&
	(isRepeated(removeZeros (getConstants [2,9,17,24,32,39,47] xs)) == False) &&
	(isRepeated(removeZeros (getConstants [3,10,18,25,33,40,48] xs)) == False) &&
	(isRepeated(removeZeros (getConstants [4,11,19,26,34,41,42] xs)) == False) &&
	(isRepeated(removeZeros (getConstants [5,11,18,24,31,37,44] xs)) == False) &&
	(isRepeated(removeZeros (getConstants [6,12,19,25,32,38,45] xs)) == False) = True
   |otherwise                                       = False
	
--Determines all left arcs are valid
checkArcL :: [Int] -> Bool
checkArcL xs
   |(isRepeated(removeZeros (getConstants [0,13,20,26,33,39,46] xs)) == False) &&
    (isRepeated(removeZeros (getConstants [1,7,14,27,34,40,47] xs)) == False) &&
	(isRepeated(removeZeros (getConstants [2,8,15,21,28,41,48] xs)) == False) &&
	(isRepeated(removeZeros (getConstants [3,9,16,22,29,35,42] xs)) == False) &&
	(isRepeated(removeZeros (getConstants [4,10,17,23,30,36,43] xs)) == False) &&
	(isRepeated(removeZeros (getConstants [5,11,18,24,31,37,44] xs)) == False) &&
	(isRepeated(removeZeros (getConstants [6,12,19,25,32,38,45] xs)) == False) = True
   |otherwise                                       = False

--Determine if board is valid
isBoardValid :: [Int] -> Bool
isBoardValid xs = (checkPetals xs) && ((checkArcR xs) && (checkArcL xs))
   
--determines if any values are repeated in a list	
isRepeated :: [Int] -> Bool
isRepeated [] = False
isRepeated [_] = False
isRepeated (x:xs) = if elem x xs then True
                             else isRepeated xs

--determines if the placed move will be valid 
placeAndCheck :: Int -> [Int] -> Bool
placeAndCheck x ys = isBoardValid(insertValue (nextZero 0 ys) ys x)


--checkZeroes :: [int] -> Bool
--checkZeroes x:xs = if(x==0) then (false) else(if(checkZeroes xs == 0) then(false) else(true)) 


--checks if Solution is found
solutionFound :: [Int] -> Bool
solutionFound xs
   |((length (removeZeros xs)) == 49) && (isBoardValid xs) = True
   |otherwise                                              = False

      
isValid :: [Int] -> Bool-- checks for duplicates

SetVal:: [Int] -> Int -> Int  
   
solve :: [Int] -> Int -> [Int]
solve L 49 = L
Solve L i
    |isValid(solve(setVal))
   
   
   
--final solver using recursion
lotusSolver :: [Int] -> [Int]
lotusSolver xs 
   |(solutionFound xs)   = xs
   |(placeAndCheck 1 xs) && (isBoardValid(lotusSolver (insertValue (nextZero 0 xs) xs 1))) = xs
   |(placeAndCheck 2 xs) && (isBoardValid(lotusSolver (insertValue (nextZero 0 xs) xs 2))) = xs
   |(placeAndCheck 3 xs) && (isBoardValid(lotusSolver (insertValue (nextZero 0 xs) xs 3))) = xs
   |(placeAndCheck 4 xs) && (isBoardValid(lotusSolver (insertValue (nextZero 0 xs) xs 4))) = xs
   |(placeAndCheck 5 xs) && (isBoardValid(lotusSolver (insertValue (nextZero 0 xs) xs 5))) = xs
   |(placeAndCheck 6 xs) && (isBoardValid(lotusSolver (insertValue (nextZero 0 xs) xs 6))) = xs
   |(placeAndCheck 7 xs) && (isBoardValid(lotusSolver (insertValue (nextZero 0 xs) xs 7))) = xs
   |otherwise            = []
   where(solve(setVal L i 1) = solve(setval L i 1) i+1
   where(solve(setVal L i 1) = solve(setval L i 1) i+1
   where(solve(setVal L i 1) = solve(setval L i 1) i+1
   where(solve(setVal L i 1) = solve(setval L i 1) i+1
   where(solve(setVal L i 1) = solve(setval L i 1) i+1
   where(solve(setVal L i 1) = solve(setval L i 1) i+1
   where(solve(setVal L i 1) = solve(setval L i 1) i+1