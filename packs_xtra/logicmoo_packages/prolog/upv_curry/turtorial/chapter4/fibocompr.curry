fibo = map fst fibopairs
fibopairs = (0,1):(1,2):[(y,x+y)|(x,y)<-fibopairs]

main1 = take 10 fibo     -- -> [0,1,1,2,1,3,2,5,3,8]
main2 = take 8 fibopairs -- -> [(0,1),(1,2),(1,1),(2,3),(1,2),(3,5),(2,3),(5,8)]

main = main2
