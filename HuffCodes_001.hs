import Data.List

data HTree = 
      CNode Double Char
    | INode Double HTree HTree deriving Show 


freq :: HTree -> Double
freq (CNode f _) = f
freq (INode f _ _) = f


merge :: HTree -> HTree -> HTree
merge l r = INode (freq l + freq r) l r


mergeAllTrees' :: [HTree]  -> HTree 
mergeAllTrees' [t] = t
mergeAllTrees' ts = mergeAllTrees' (t:rest)
    where
        (f:s:rest) = sortOn freq ts
        t = merge f s

{--
        
mergeAllTrees :: [HTree] -> HTree
--}


mini :: Ord a => (a->a->Bool) -> [a] -> Int
mini _ [_] = 0
mini compf (x:xs) 
    | compf x (xs !! taili) = 0
    | otherwise = 1 + taili
        where
            taili = mini compf xs




--list comprehension
removei :: Int -> [a] -> [a]
removei n xs = [ x | (x,i)<- zip xs [0..] , n /=i ]


--recursion
removei' :: Int -> [a] -> [a]
removei' 0 (_:xs) = xs
removei' n (x:xs) = x:(removei' (n-1) xs)


--other way?
--removei'' :: Int-> [a] ->[a]
removei'' n xs =  map (\ (x,y) -> x) ( filter (\(x,y)->y/=n ) (zip xs [0..]) )

removei''' n xs = take (n) xs ++ drop (n+1) xs

buildHTree :: [(Char,Double)] -> HTree 
buildHTree cfs = mergeAllTrees' ts
        where
            ts = map (\(c,f)->CNode f c ) cfs


decodeTree :: String -> HTree -> [(Char,String)]
decodeTree pre (CNode _ c) = [(c,pre)]
decodeTree pre (INode _ l r) = lefts ++ rights
    where
        lefts = decodeTree (pre ++ "0") l
        rights = decodeTree (pre ++ "1") r

--minor bug: what if there's only one char? who cares...
getCodes :: [(Char,Double)] -> [(Char,String)]
getCodes cfs = decodeTree "" (buildHTree cfs)



get350Slide38 = [('A',0.35),('B',0.1),('C',0.2),('D',0.2),('_',0.15)] 
        
