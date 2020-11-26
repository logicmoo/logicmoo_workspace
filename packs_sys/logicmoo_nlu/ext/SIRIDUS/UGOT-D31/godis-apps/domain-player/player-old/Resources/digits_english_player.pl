
number_phrase_eng([zero], 0).
number_phrase_eng([one], 1).
number_phrase_eng([two], 2).
number_phrase_eng([three], 3).
number_phrase_eng([four], 4).
number_phrase_eng([five], 5).
number_phrase_eng([six], 6).
number_phrase_eng([seven], 7).
number_phrase_eng([eight], 8).
number_phrase_eng([nine], 9).

number_phrase_eng([ten], 10).
number_phrase_eng([eleven], 11).
number_phrase_eng([twelve], 12).
number_phrase_eng([thirteen], 13).
number_phrase_eng([fourteen], 14).
number_phrase_eng([fifteen], 15).
number_phrase_eng([sixteen], 16).
number_phrase_eng([seventeen], 17).
number_phrase_eng([eighteen], 18).
number_phrase_eng([nineteen], 19).

number_phrase_eng([twenty], 20).
number_phrase_eng([twenty,one], 21).
number_phrase_eng([twenty,two], 22).
number_phrase_eng([twenty,three], 23).
number_phrase_eng([twenty,four], 24).
number_phrase_eng([twenty,five], 25).
number_phrase_eng([twenty,six], 26).
number_phrase_eng([twenty,seven], 27).
number_phrase_eng([twenty,eight], 28).
number_phrase_eng([twenty,nine], 29).

number_phrase_eng([thirty], 30).
number_phrase_eng([thirty,one], 31).
number_phrase_eng([thirty,two], 32).
number_phrase_eng([thirty,three], 33).
number_phrase_eng([thirty,four], 34).
number_phrase_eng([thirty,five], 35).
number_phrase_eng([thirty,six], 36).
number_phrase_eng([thirty,seven], 37).
number_phrase_eng([thirty,eight], 38).
number_phrase_eng([thirty,nine], 39).

number_phrase_eng([forty], 40).
number_phrase_eng([forty,one], 41).
number_phrase_eng([forty,two], 42).
number_phrase_eng([forty,three], 43).
number_phrase_eng([forty,four], 44).
number_phrase_eng([forty,five], 45).
number_phrase_eng([forty,six], 46).
number_phrase_eng([forty,seven], 47).
number_phrase_eng([forty,eight], 48).
number_phrase_eng([forty,nine], 49).

number_phrase_eng([fifty], 50).
number_phrase_eng([fifty,one], 51).
number_phrase_eng([fifty,two], 52).
number_phrase_eng([fifty,three], 53).
number_phrase_eng([fifty,four], 54).
number_phrase_eng([fifty,five], 55).
number_phrase_eng([fifty,six], 56).
number_phrase_eng([fifty,seven], 57).
number_phrase_eng([fifty,eight], 58).
number_phrase_eng([fifty,nine], 59).

number_phrase_eng([sixty], 60).
number_phrase_eng([sixty,one], 61).
number_phrase_eng([sixty,two], 62).
number_phrase_eng([sixty,three], 63).
number_phrase_eng([sixty,four], 64).
number_phrase_eng([sixty,five], 65).
number_phrase_eng([sixty,six], 66).
number_phrase_eng([sixty,seven], 67).
number_phrase_eng([sixty,eight], 68).
number_phrase_eng([sixty,nine], 69).

number_phrase_eng([seventy], 70).
number_phrase_eng([seventy,one], 71).
number_phrase_eng([seventy,two], 72).
number_phrase_eng([seventy,three], 73).
number_phrase_eng([seventy,four], 74).
number_phrase_eng([seventy,five], 75).
number_phrase_eng([seventy,six], 76).
number_phrase_eng([seventy,seven], 77).
number_phrase_eng([seventy,eight], 78).
number_phrase_eng([seventy,nine], 79).

number_phrase_eng([eighty], 80).
number_phrase_eng([eighty,one], 81).
number_phrase_eng([eighty,two], 82).
number_phrase_eng([eighty,three], 83).
number_phrase_eng([eighty,four], 84).
number_phrase_eng([eighty,five], 85).
number_phrase_eng([eighty,six], 86).
number_phrase_eng([eighty,seven], 87).
number_phrase_eng([eighty,eight], 88).
number_phrase_eng([eighty,nine], 89).

number_phrase_eng([ninety], 90).
number_phrase_eng([ninety,one], 91).
number_phrase_eng([ninety,two], 92).
number_phrase_eng([ninety,three], 93).
number_phrase_eng([ninety,four], 94).
number_phrase_eng([ninety,five], 95).
number_phrase_eng([ninety,six], 96).
number_phrase_eng([ninety,seven], 97).
number_phrase_eng([ninety,eight], 98).
number_phrase_eng([ninety,nine], 99).


number_phrase_eng([N1,hundred,and|N2], X):-
	nonvar(N1),
	nonvar(N2),
	number_phrase_eng([N1], X1),
	number_phrase_eng(N2, X2),
	X is X1*100 + X2.
