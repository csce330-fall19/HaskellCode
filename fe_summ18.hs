--q17
revcat :: [String]->String
revcat xs = concat $ map reverse xs 

revcat' xs = foldr (\ a b -> a ++ b) [] (map reverse xs)

revcat'' xs = foldr (\ a b -> a ++ b) [] (map rev xs)
    where
        rev = foldr (\c cs -> cs ++ [c]) [] 