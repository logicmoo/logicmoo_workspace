from n = n : from (n+1)

nth n (x:xs) = if n==1 then x else nth (n-1) xs

fibolist x0 x1 = x0 : fibolist x1 (x0+x1)

-- should return 3
main1 = nth 3 (from 1)

-- should return 3
main2 = nth 5 (fibolist 0 1)


main = main2
