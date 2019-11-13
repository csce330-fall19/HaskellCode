
--recursive
allPos :: [Int]->Bool
allPos [] = True
allPos (n:ns) 
    | n > 0 = allPos ns
    | otherwise = False

--map or filter
allPos' :: [Int]->Bool
allPos' xs = and (map (>0) xs)

--foldr 
allPos'' :: [Int]->Bool
allPos'' xs = foldr (\ y ys -> (y>0) && ys ) True xs