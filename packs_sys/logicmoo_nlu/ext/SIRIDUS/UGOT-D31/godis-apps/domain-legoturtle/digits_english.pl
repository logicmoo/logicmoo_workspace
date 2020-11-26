/*number_phrase([noll,0'0).
number_phrase([ett,0'1).
number_phrase([två,0'2).
number_phrase([tre,0'3).
number_phrase([fyra,0'4).
number_phrase([fem,0'5).
number_phrase([sex,0'6).
number_phrase([sju,0'7).
number_phrase([åtta,0'8).
number_phrase([nio,0'9).

%number_phrase([tio,0'10).
%number_phrase([femton,0'15).
%number_phrase([sjutton,0'17).
%number_phrase([arton,0'18).
%number_phrase([trettio,0'30).
*/

number_phrase([zero], 0).
number_phrase([one], 1).
number_phrase([two], 2).
number_phrase([three], 3).
number_phrase([four], 4).
number_phrase([five], 5).
number_phrase([six], 6).
number_phrase([seven], 7).
number_phrase([eight], 8).
number_phrase([nine], 9).

number_phrase([ten], 10).
number_phrase([eleven], 11).
number_phrase([twelve], 12).
number_phrase([thirteen], 13).
number_phrase([fourteen], 14).
number_phrase([fifteen], 15).
number_phrase([sixteen], 16).
number_phrase([seventeen], 17).
number_phrase([eighteen], 18).
number_phrase([nineteen], 19).

number_phrase([twenty], 20).
number_phrase([twenty,one], 21).
number_phrase([twenty,two], 22).
number_phrase([twenty,three], 23).
number_phrase([twenty,four], 24).
number_phrase([twenty,five], 25).
number_phrase([twenty,six], 26).
number_phrase([twenty,seven], 27).
number_phrase([twenty,eight], 28).
number_phrase([twenty,nine], 29).

number_phrase([thirty], 30).
number_phrase([thirty,one], 31).
number_phrase([thirty,two], 32).
number_phrase([thirty,three], 33).
number_phrase([thirty,four], 34).
number_phrase([thirty,five], 35).
number_phrase([thirty,six], 36).
number_phrase([thirty,seven], 37).
number_phrase([thirty,eight], 38).
number_phrase([thirty,nine], 39).

number_phrase([forty], 40).
number_phrase([forty,one], 41).
number_phrase([forty,two], 42).
number_phrase([forty,three], 43).
number_phrase([forty,four], 44).
number_phrase([forty,five], 45).
number_phrase([forty,six], 46).
number_phrase([forty,seven], 47).
number_phrase([forty,eight], 48).
number_phrase([forty,nine], 49).

number_phrase([fifty], 50).
number_phrase([fifty,one], 51).
number_phrase([fifty,two], 52).
number_phrase([fifty,three], 53).
number_phrase([fifty,four], 54).
number_phrase([fifty,five], 55).
number_phrase([fifty,six], 56).
number_phrase([fifty,seven], 57).
number_phrase([fifty,eight], 58).
number_phrase([fifty,nine], 59).

number_phrase([sixty], 60).
number_phrase([sixty,one], 61).
number_phrase([sixty,two], 62).
number_phrase([sixty,three], 63).
number_phrase([sixty,four], 64).
number_phrase([sixty,five], 65).
number_phrase([sixty,six], 66).
number_phrase([sixty,seven], 67).
number_phrase([sixty,eight], 68).
number_phrase([sixty,nine], 69).

number_phrase([seventy], 70).
number_phrase([seventy,one], 71).
number_phrase([seventy,two], 72).
number_phrase([seventy,three], 73).
number_phrase([seventy,four], 74).
number_phrase([seventy,five], 75).
number_phrase([seventy,six], 76).
number_phrase([seventy,seven], 77).
number_phrase([seventy,eight], 78).
number_phrase([seventy,nine], 79).

number_phrase([eighty], 80).
number_phrase([eighty,one], 81).
number_phrase([eighty,two], 82).
number_phrase([eighty,three], 83).
number_phrase([eighty,four], 84).
number_phrase([eighty,five], 85).
number_phrase([eighty,six], 86).
number_phrase([eighty,seven], 87).
number_phrase([eighty,eight], 88).
number_phrase([eighty,nine], 89).

number_phrase([ninety], 90).
number_phrase([ninety,one], 91).
number_phrase([ninety,two], 92).
number_phrase([ninety,three], 93).
number_phrase([ninety,four], 94).
number_phrase([ninety,five], 95).
number_phrase([ninety,six], 96).
number_phrase([ninety,seven], 97).
number_phrase([ninety,eight], 98).
number_phrase([ninety,nine], 99).


number_phrase([N1,hundred,and|N2], X):-
	number_phrase([N1], X1),
	number_phrase(N2, X2),
	X is X1*100 + X2.
