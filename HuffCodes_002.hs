import Data.List

data HTree =  CNode Double Char
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

        
mergeAllTrees :: [HTree] -> HTree
mergeAllTrees [t] = t
mergeAllTrees ts = mergeAllTrees (t:rest)
    where
        compf x y = freq x < freq y
        minia = mini compf ts
        tsa = removei minia ts
        minib = mini compf tsa
        rest = removei minib tsa
        t = merge (ts !! minia) (tsa!! minib)


mini :: (a->a->Bool) -> [a] -> Int
mini _ [x] = 0
mini compf (x:xs)
    | compf x (xs!!miniTail) = 0
    | otherwise = 1+ miniTail
    where
        miniTail = mini compf xs


--list comprehension
removei :: Int -> [a] -> [a]
removei n xs = [ x | (x,i)<- zip xs [0..] , i /= n ]


removei''' n xs = foldr (++) [] [ if (i==n) then [] else [x] | (x,i)<- zip xs [0..] ]

removei'''' n xs = concat [ if (i==n) then [] else [x] | (x,i)<- zip xs [0..] ]


--recursion
removei' :: Int -> [a] -> [a]
removei' 0 (_:xs) = xs
removei' n (x:xs) = [x]++ removei' (n-1) xs

removei''''' n (x:xs)
    |  n==0 = xs
    |  otherwise =  x: removei'''' (n-1) xs

removei'''''' n xs = map fst (filter ( \(x,i)-> i /= n ) xis)
    where
        xis = zip xs [0..]

--other way?
removei'' :: Int-> [a] ->[a]
removei'' n xs = take n xs ++ drop (n+1) xs


buildHTree :: [(Char,Double)] -> HTree 
buildHTree cfs = mergeAllTrees cnodes
    where
        cnodes = map (\(c,f)->CNode f c) cfs

decodeTree :: String -> HTree -> [(Char,String)]
decodeTree pre (CNode _ c) = [(c,pre)]
decodeTree pre (INode _ l r) = leftCodes ++ rightCodes
    where
        leftCodes = decodeTree (pre++"0") l
        rightCodes = decodeTree (pre++"1") r


--minor bug: what if there's only one char? who cares...
getCodes :: [(Char,Double)] -> [(Char,String)]
getCodes cfs = decodeTree "" (buildHTree cfs)



get350Slide38 = [('A',0.35),('B',0.1),('C',0.2),('D',0.2),('_',0.15)] 
        
