--Maybe Example

average :: [Float]->Float
average xs = sum xs / (fromIntegral $ length xs)

safeAvg :: [Float]-> Maybe Float
safeAvg [] = Nothing
safeAvg xs = Just( average xs)

--q6 run ":t f"on command line
f x y = [x,y]

--run ":t q7"on command line (String in place of [Char] is fine for your answers)
q7 = ( ( 2 , "legit") , "Kim" )

--q8
nor :: Bool -> Bool -> Bool
nor False False = True
nor _     _    = False

--q9
maxOr0 :: Float->Float-> Float
maxOr0 x y 
    | x<0.0 && y<0.0 = 0.0
    | x>y = x
    | otherwise = y

--q10
addFirst2 :: Num a => [a] ->[a]
addFirst2  = (\ (n0:n1:ns) -> (n0+n1):ns )

--q11
midItem :: [a] -> a
midItem = (\xs -> xs !! ((length xs -1) `div` 2) )


--q12
noVowels cs = [ c| c<-cs,c/='a',c/='e',c/='i',c/='o',c/='u']

--q13
redact cs = [ 'X' | c<-cs ]

--clumsy, above cleaner
redact' cs = [ y | c<-cs, y<-['X'] ]

--q14
isEvenLength :: [a] -> Bool
isEvenLength [] = True
isEvenLength [_] = False
isEvenLength (_:_:rest) = isEvenLength rest

--alternate solution
isEvenLength' [] = True
isEvenLength' [_] = False
isEvenLength' (_:xs) = isOddLength xs

isOddLength [] = False
isOddLength [_] = True
isOddLength (_:xs) = isEvenLength' xs

--rewriting length would be "legal", but a little clumsy, i.e.
isEvenLength'' xs = length' xs `mod` 2 == 0

length' [] = 0
length' (_:xs) = 1+ length' xs

--q15, could be ints... 
type Circle = (Float,Float,Float) 
--or perhaps
type Circle' = ( (Float,Float) ,Float)

--q16, probably will give type and have you use for Fall 19 exam
-- you won't have to do "deriving Show" part
data List a = Cons a (List a) | Nil deriving Show


--q17
revcat :: [String]->String
revcat xs = concat $ map reverse xs 

revcat' xs = foldr (\ a b -> a ++ b) [] (map reverse xs)

revcat'' xs = foldr (\ a b -> a ++ b) [] (map rev xs)
    where
        rev = foldr (\c cs -> cs ++ [c]) [] 

