fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' 0 = 0
fib' 1 = 1
fib' n = fibh 0 1 2 n

fibh fibnm2 fibnm1 n s 
    | n==s = fibnm2 + fibnm1
    | otherwise = fibh fibnm1 (fibnm2 + fibnm1) (n+1) s 