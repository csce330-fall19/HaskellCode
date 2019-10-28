pyths :: Int -> [(Int,Int,Int)]
pyths n = [ (x,y,z) | x<-[1..n],y<-[1..n],z<-[1..n], x^2+y^2 == z^2]

factors :: Int -> [Int]
factors n =
   [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x<-[2..n], x == sum ( init ( factors x) )]

prime :: Int -> Bool
prime n = factors n == [1,n]
