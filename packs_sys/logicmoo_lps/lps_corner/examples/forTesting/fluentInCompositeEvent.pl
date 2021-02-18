
:- expects_dialect(lps).

maxTime(5).

fluents hungry, foodAtHome, neighbourHome, haveMoney.
initially hungry, neighbourHome.
actions buyFood, borrowFood.
              
if  hungry at T then getFood from T to _.
getFood from T1 to T2 if not foodAtHome at T1, haveMoney at T1, buyFood from T1 to T2.
getFood from T1 to T2 if not foodAtHome at T1, neighbourHome at T1, borrowFood from T1 to T2.
