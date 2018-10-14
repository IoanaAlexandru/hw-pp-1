module Tema1 (
        solveSimple,
        solveCosts
        ) where

import Data.Array

inf = 1000

src :: (Int, Int, Int) -> Int
src (s,_,_) = s

dest :: (Int, Int, Int) -> Int
dest (_,d,_) = d

dist :: (Int, Int, Int) -> Int
dist (_,_,d) = d


solveSimple :: (Int, [(Int, Int, Int)]) -> Maybe ([Int], Int)
solveSimple (n, l) = if snd (m 1 n) == inf
                     then Nothing
                     else Just (m 1 n) where

    m i j
        | min (snd (getDist (i, j))) (snd (getMin i j)) == 
            snd (getDist (i, j)) = getDist (i, j)
        | otherwise = getMin i j

    -- Get a tuple with a list with cities x and y, as well as the distance between them
    -- (if road between x and y is found in the input, or inf otherwise)
    getDist :: (Int, Int) -> ([Int], Int)
    getDist (x, y) = ([x, y], distOrInf (filter (\n->(src n == x) && (dest n == y)) l))

    distOrInf :: [(Int, Int, Int)] -> Int
    distOrInf [] = inf
    distOrInf c = dist (head c)

    -- Get minimum path
    getMin :: Int -> Int -> ([Int], Int)
    getMin i j = foldr sndMin ([], inf) 
                [( (init (fst (mat ! (i, k)))) ++ (fst (mat ! (k, j)))
                 , snd (mat ! (i, k)) + snd (mat ! (k, j))) | k <- [i + 1 .. j - 1]]

    -- Choose the path with minimum distance (distance = second element of tuple)
    sndMin :: ([Int], Int) -> ([Int], Int) -> ([Int], Int)
    sndMin t1 t2 = if (min (snd t1) (snd t2) == (snd t1))
                   then t1
                   else t2
                   
    -- DP matrix, where mat!(i,j) contains shortest way from i to j and the distance
    mat :: Array (Int, Int) ([Int], Int)
    mat = listArray bounds [m i j | (i, j) <- range bounds]

    bounds = ((1, 1), (n, n))
                            

solveCosts :: (Int, Int, [Int], [(Int, Int, Int)]) -> Maybe ([(Int, Int)], Int)
solveCosts (n, budget, tax, l) = if snd (m 1 n) == inf  || snd (last (fst (m 1 n))) < 0
                                 then Nothing
                                 else Just (m 1 n) where

    -- Converting the tax list into an array for easy index access
    t = listArray (1, n) tax

    m i j
        | min (snd (getDist (i, j) budget)) (snd (getMin i j)) == 
               snd (getDist (i, j) budget) = getDist (i, j) budget
        | otherwise = getMin i j

    -- Get a tuple with a list with cities x and y and the money owned in each city
    -- (money in city x is the budget b given as an argument and money in city y is
    -- b - cost of entrance in city y), as well as the distance between them (if road
    -- between x and y is found in the input, or inf otherwise)
    getDist :: (Int, Int) -> Int -> ([(Int, Int)], Int)
    getDist coords b = initPath (distOrInf (filter (\n -> (src n == fst coords) &&
                        (dest n == snd coords)) l)) coords b (t ! (snd coords))

    initPath :: Int -> (Int, Int) -> Int -> Int -> ([(Int, Int)], Int)
    initPath dist coords b cost = ([(fst coords, b), (snd coords, b - cost)], dist)

    distOrInf :: [(Int, Int, Int)] -> Int
    distOrInf [] = inf
    distOrInf c = dist (head c)

    -- Get minimum path
    getMin :: Int -> Int -> ([(Int, Int)], Int)
    getMin i j = foldr sndMin ([], inf) 
                [( getPath i k j
                 , snd (mat ! (i, k)) + snd (mat ! (k, j))) | k <- [i + 1 .. j - 1]]

    -- Choose path with minimum distance (distance = second element of tuple), and if
    -- the distances are the same, choose the path that costs less
    sndMin :: ([(Int, Int)], Int) -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
    sndMin t1 t2
        | (snd t1) < (snd t2) && snd (last (fst t1)) >= 0 = t1
        | (snd t1) == (snd t2) && lastBudget t1 >= lastBudget t2 = t1
        | otherwise = t2

    -- Obtain path starting in i, passing through k and ending in j
    getPath :: Int -> Int -> Int -> [(Int, Int)]
    getPath i k j = init (fst (mat ! (i, k))) ++ 
                    getBudget (fst (mat ! (k, j))) (lastBudget (mat ! (i, k)))
    
    -- Get budget from the second to last city in list
    lastBudget :: ([(Int, Int)], Int) -> Int
    lastBudget c = snd (last (init (fst c)))

    -- Get cities and money owned in each city by using an initial budget b and
    -- subtracting the tax of each city in the list
    getBudget :: [(Int, Int)] -> Int -> [(Int, Int)]
    getBudget c b = tail (scanl (\(x1, y1) (x2, y2)->(x2, y1 - y2)) (0, b) (getCosts c))

    -- Take a list of cities and return tuples of cities and their costs
    getCosts :: [(Int, Int)] -> [(Int, Int)]
    getCosts c = [(i, t ! i) | i <- (map (fst) c)]
                   
    -- DP matrix, where mat!(i,j) contains shortest way from i to j and the distance
    mat :: Array (Int, Int) ([(Int, Int)], Int)
    mat = listArray bounds [m i j | (i, j) <- range bounds]

    bounds = ((1, 1), (n, n))
