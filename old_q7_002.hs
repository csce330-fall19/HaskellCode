--q1
nodups :: Eq a => [a] -> [a]
nodups [] = []
nodups (x:xs) 
    | elem x xs = nodups xs
    | otherwise = x: nodups xs


elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) 
    | a == x = True
    | otherwise = elem' a xs

evenSum :: [(Int,Int)]-> [Int]
evenSum xs = filter even (map (\p -> fst p + snd p ) xs )

evenSum' :: [(Int,Int)]-> [Int]
evenSum' xs = filter even (map (\(x,y) -> x+y ) xs )

evenSum'' xs= foldr (\(x,y) rest -> if  even $ x+y then x+y :rest else rest) [] xs