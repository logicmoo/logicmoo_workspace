:- include(test_header).

lives(englishman, red)
keep(swede, dogs)
drinks(dane, tea)
leftof(green, white)
?X lives(X, green) ? drinks(X, coffee)
?X smokes(X, pallmalls) ? keep(X, birds)
?X lives(X, yellow) ? smokes(X, dunhills)
?X position(X, 3) ? drinks(X, milk)
position(norwegian, 1)
?X,Y smokes(X, blend) ? neighbor(X, Y) ? smokes(Y, dunhill)
?X smokes(X, bluemasters) ? drinks(X, bier)
?X,Y keep(X, horses) ? neighbor(X, Y) ? smoke(Y, dunhill)
smokes(german, prince)
?X neighbor(norwegian, X) ? lives(X, blue)
?X,Y smokes(X, blends) ? neighbor(X,Y) ? drinks(Y, water)

