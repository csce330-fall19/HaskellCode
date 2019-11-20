import Data.Char

--capFirstWord :: String->String 

--price :: String -> [(String, Float)] -> Float
price item pt = snd $ head ( filter (\pair-> fst pair == item) pt)

--priceRecipe :: [(String, Float)] -> [(String, Int)] -> Float

--priceRecipe’ :: [(String, Float)] -> [(String, Int)] -> Float

price_table = [ ("nut",3.0), ("bolt",5.25), ("widget",9.99), ("screw",7.00), ("nail",2.05)]   