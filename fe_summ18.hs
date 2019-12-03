--Maybe Example

average :: [Float]->Float
average xs = sum xs / (fromIntegral $ length xs)

safeAvg :: [Float]-> Maybe Float
safeAvg [] = Nothing
safeAvg xs = Just( average xs)

--q12
noVowels cs = [ c| c<-cs,c/='a',c/='e',c/='i',c/='o',c/='u']

--q13
redact cs = [ 'X' | c<-cs ]

--clumsy, above cleaner
redact' cs = [ y | c<-cs, y<-['X'] ]

--q17
revcat :: [String]->String
revcat xs = concat $ map reverse xs 

revcat' xs = foldr (\ a b -> a ++ b) [] (map reverse xs)

revcat'' xs = foldr (\ a b -> a ++ b) [] (map rev xs)
    where
        rev = foldr (\c cs -> cs ++ [c]) [] 

