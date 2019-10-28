

--q6
first5NotEven = take 10 [ 5*x | x<-[1..], odd x]

first5NotEven' = take 5 [ x | x<-[5,10..], odd x]

first5NotEven'' = take 5 [ x | x<-[5,15..]]