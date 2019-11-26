import Data.Char 

--Maybe Example
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

message :: Maybe Int -> String
message Nothing = "Division by zero"
message (Just x) = "Result " ++ show x

--capFirstWord
capFirstWord :: String-> String
capFirstWord [] = []
capFirstWord (c:cs) = toUpper c : cfw cs

cfw [] = []
cfw [b] = [b]
cfw [b,c] = [b,c]
cfw (b:c:d:cs)
    | b == '.' && c == ' ' = b:c: toUpper d:cfw cs
    | otherwise = b:cfw (c:d: cs)

--see 002 for filter (best approach)
price :: String -> [(String, Float)] -> Float
price item prices = sum $ map (\ (n,p)->if n==item then p else 0) prices

price' item prices = foldr f 0 prices
    where
        f= \(n,p) rest -> (if n==item then p else 0)+rest 

priceRecipe prices iqs = sum $ map (\(i,q)->fromIntegral q * price i prices) iqs

priceRecipe' prices iqs = foldr (\(i,q) rest ->fromIntegral q * price i prices +rest) 0 iqs

price_table :: [(String,Float)]
price_table = [ ("nut",3.0), ("bolt",5.25), ("widget",9.99), ("screw",7.00), ("nail",2.05)]

recipe :: [(String,Integer)]
recipe = [ ("nut",2), ("bolt",2), ("widget",1) ] 


