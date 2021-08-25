data task = cut | polish
data worker =  Alex | Bert | Chuck

assign cut = Alex
assign cut = Bert
assign polish = Bert
assign polish = Chuck

team | x /= y = (x,y)
  where x = assign cut 
        y = assign polish


main = team
