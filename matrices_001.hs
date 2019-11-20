type Matrix = [ [ Double ] ]
type Vector = [ Double ]

ip :: Vector -> Vector -> Double
ip xs ys = sum $ map (\p-> fst p * snd p) (zip xs ys)

ip' xs ys = sum $ map (\(x,y)-> x*y) (zip xs ys)

ip'' xs ys = sum [ xs!!i * ys!!i | i<-[0..length xs - 1] ]

ip''' xs ys = foldr (\ (x,y) rest -> x*y + rest ) 0 (zip xs ys)

validip :: Vector -> Vector -> Bool
validip xs ys  = length xs == length ys

row :: Matrix -> Int -> Vector
row mat r = mat !! r

col :: Matrix -> Int -> Vector
col mat c = [ r !! c | r<-mat]

n_rows :: Matrix -> Int
n_rows mat = length mat

n_cols :: Matrix -> Int
n_cols mat = length (mat !! 0)

validmmul :: Matrix -> Matrix -> Bool
validmmul a b = n_cols a == n_rows b

mmul :: Matrix ->Matrix -> Matrix
mmul a b = [  [ ip (row a i) (col b j) | j<-[0..n_cols b-1] ] | 
                                         i<-[0..n_rows a-1] ] 




m3x3 :: Matrix
m3x3 = [ [1,2,3],[2,1,4],[5,2,1]]
