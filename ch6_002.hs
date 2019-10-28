
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs 

concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = [x] ++ replicate' (n-1) x

replicate'' :: Int -> a -> [a]
replicate'' 0 _ = []
replicate'' n x = x : replicate'' (n-1) x