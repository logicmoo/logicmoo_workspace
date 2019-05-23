0.999940257964753::active(Fold,Model).
0.78899752857876::active(Fold,Model) :- ames(Fold,Model).
0.646035664682637::active(Fold,Model) :- has_property(Fold,Model,salmonella,p).
0.415886636704258::active(Fold,Model) :- ashby_alert(Fold,Model,amino,A), hidden_1(Fold,Model,A).
0.475674381593714::active(Fold,Model) :- ashby_alert(Fold,Model,di10,B).
0.699974861999099::active(Fold,Model) :- ashby_alert(Fold,Model,nitro,C), hidden_2(Fold,Model,C).
0.723077719914362::active(Fold,Model) :- ind(Fold,Model,amino,D), hidden_3(Fold,Model,D).
0.926538848873369::active(Fold,Model) :- atm(Fold,Model,E,c,22,F), hidden_4(Fold,Model,E,F).
0.410126067732641::active(Fold,Model) :- atm(Fold,Model,G,c,22,H), hidden_5(Fold,Model,G,H).
0.673429231230674::active(Fold,Model) :- nitro(Fold,Model,Y).
0.869382074167695::active(Fold,Model) :- amine(Fold,Model,Z).
0.438563643102909::active(Fold,Model) :- phenol(Fold,Model,A1).
0.09817862613744::hidden_1(Fold,Model,A) :- connected(Fold,Model,A,Z).
0.747288335634859::hidden_1(Fold,Model,A) :- connected(Fold,Model,Z,A), hidden_1_1(Fold,Model,Z,A).
0.372590124130032::hidden_2(Fold,Model,C) :- connected(Fold,Model,C,B1).
0.70499484779733::hidden_3(Fold,Model,D) :- ind(Fold,Model,di10,D).
0.353288006807505::hidden_4(Fold,Model,E,F) :- atm(Fold,Model,C1,c,22,F), hidden_4_1(Fold,Model,E,C1,F).
0.520975465125249::hidden_4(Fold,Model,E,F) :- atm(Fold,Model,D1,c,22,F), hidden_4_2(Fold,Model,E,D1,F).
0.408616194714965::hidden_4(Fold,Model,E,F) :- atm(Fold,Model,E1,c,22,F), hidden_4_3(Fold,Model,E,E1,F).
0.057996112673249::hidden_4(Fold,Model,E,F) :- symbond(Fold,Model,K,E,1).
0.413451343769544::hidden_4(Fold,Model,E,F) :- symbond(Fold,Model,E1,E,7), hidden_4_4(Fold,Model,F,E1,E).
0.53678142638137::hidden_5(Fold,Model,G,H) :- symbond(Fold,Model,O,G,1).
0.562187367517583::hidden_1_1(Fold,Model,Z,A) :- connected(Fold,Model,Z,B1), hidden_1_1_1(Fold,Model,A,Z,B1).
0.402825296004645::hidden_1_1(Fold,Model,Z,A) :- connected(Fold,Model,B1,Z), hidden_1_1_2(Fold,Model,A,B1,Z).
0.31364979974566::hidden_4_1(Fold,Model,E,C1,F) :- symbond(Fold,Model,Q,C1,1).
0.466577212724894::hidden_4_2(Fold,Model,E,D1,F) :- symbond(Fold,Model,F1,D1,1).
0.409777703013462::hidden_4_3(Fold,Model,E,E1,F) :- symbond(Fold,Model,I1,E1,1).
0.118983856108816::hidden_4_4(Fold,Model,F,E1,E) :- symbond(Fold,Model,E1,I1,1).
0.666808088230264::hidden_4_4(Fold,Model,F,E1,E) :- symbond(Fold,Model,I,E1,7), hidden_4_4_1(Fold,Model,F,E,I,E1).
0.854978867032281::hidden_1_1_1(Fold,Model,A,Z,B1) :- connected(Fold,Model,B1,Y), hidden_1_1_1_1(Fold,Model,A,Z,B1,Y).
0.158614742917993::hidden_1_1_2(Fold,Model,A,B1,Z) :- connected(Fold,Model,B1,C).
0.023450007557892::hidden_4_4_1(Fold,Model,F,E,I,E1) :- symbond(Fold,Model,I,M,1).
0.149781108257317::hidden_4_4_1(Fold,Model,F,E,I,E1) :- symbond(Fold,Model,G,I,7), hidden_4_4_1_1(Fold,Model,F,E,E1,G,I).
0.601271010745064::hidden_1_1_1_1(Fold,Model,A,Z,B1,Y) :- connected(Fold,Model,Y,C).
0.66875243592432::hidden_4_4_1_1(Fold,Model,F,E,E1,G,I) :- symbond(Fold,Model,G,O,1).
0.604227296106403::hidden_4_4_1_1(Fold,Model,F,E,E1,G,I) :- symbond(Fold,Model,D1,G,7), hidden_4_4_1_1_1(Fold,Model,F,E,E1,I,D1,G).
0.441452032284636::hidden_4_4_1_1_1(Fold,Model,F,E,E1,I,D1,G) :- symbond(Fold,Model,D1,F1,1).
0.772498504433373::hidden_4_4_1_1_1(Fold,Model,F,E,E1,I,D1,G) :- symbond(Fold,Model,C1,D1,7), hidden_4_4_1_1_1_1(Fold,Model,F,E,E1,I,G,C1,D1).
0.578899105727073::hidden_4_4_1_1_1_1(Fold,Model,F,E,E1,I,G,C1,D1) :- symbond(Fold,Model,C1,Q,1).
connected(_Fold,_Model,Ring1,Ring2) :- Ring1\=Ring2, member(A,Ring1), member(A,Ring2), !.
symbond(Fold,Model,A,B,T) :- bond(Fold,Model,A,B,T).
symbond(Fold,Model,A,B,T) :- bond(Fold,Model,B,A,T).
mutagenic(train,d2).
mutagenic(train,d13).
mutagenic(train,d18).
mutagenic(train,d19).
mutagenic(train,d27).
mutagenic(train,d28).
mutagenic(train,d35).
mutagenic(train,d36).
mutagenic(train,d37).
mutagenic(train,d38).
mutagenic(train,d41).
mutagenic(train,d42).
mutagenic(train,d48).
mutagenic(train,d50).
mutagenic(train,d51).
mutagenic(train,d54).
mutagenic(train,d58).
mutagenic(train,d61).
mutagenic(train,d62).
mutagenic(train,d63).
mutagenic(train,d66).
mutagenic(train,d69).
mutagenic(train,d72).
mutagenic(train,d76).
mutagenic(train,d77).
mutagenic(train,d78).
mutagenic(train,d84).
mutagenic(train,d86).
mutagenic(train,d89).
mutagenic(train,d92).
mutagenic(train,d96).
mutagenic(train,d101).
mutagenic(train,d104).
mutagenic(train,d106).
mutagenic(train,d107).
mutagenic(train,d112).
mutagenic(train,d113).
mutagenic(train,d117).
mutagenic(train,d121).
mutagenic(train,d123).
mutagenic(train,d126).
mutagenic(train,d128).
mutagenic(train,d135).
mutagenic(train,d137).
mutagenic(train,d139).
mutagenic(train,d140).
mutagenic(train,d143).
mutagenic(train,d144).
mutagenic(train,d145).
mutagenic(train,d146).
mutagenic(train,d147).
mutagenic(train,d152).
mutagenic(train,d153).
mutagenic(train,d154).
mutagenic(train,d155).
mutagenic(train,d156).
mutagenic(train,d159).
mutagenic(train,d160).
mutagenic(train,d161).
mutagenic(train,d163).
mutagenic(train,d164).
mutagenic(train,d166).
mutagenic(train,d168).
mutagenic(train,d171).
mutagenic(train,d173).
mutagenic(train,d174).
mutagenic(train,d177).
mutagenic(train,d179).
mutagenic(train,d180).
mutagenic(train,d182).
mutagenic(train,d183).
mutagenic(train,d185).
mutagenic(train,d186).
mutagenic(train,d187).
mutagenic(train,d188).
mutagenic(train,d189).
mutagenic(train,d191).
mutagenic(train,d192).
mutagenic(train,d193).
mutagenic(train,d195).
mutagenic(train,d197).
mutagenic(train,d201).
mutagenic(train,d202).
mutagenic(train,d205).
mutagenic(train,d206).
mutagenic(train,d207).
mutagenic(train,d211).
mutagenic(train,d214).
mutagenic(train,d215).
mutagenic(train,d216).
mutagenic(train,d224).
mutagenic(train,d225).
mutagenic(train,d227).
mutagenic(train,d228).
mutagenic(train,d229).
mutagenic(train,d231).
mutagenic(train,d235).
mutagenic(train,d237).
mutagenic(train,d239).
mutagenic(train,d242).
mutagenic(train,d245).
mutagenic(train,d246).
mutagenic(train,d249).
mutagenic(train,d251).
mutagenic(train,d254).
mutagenic(train,d257).
mutagenic(train,d258).
mutagenic(train,d261).
mutagenic(train,d264).
mutagenic(train,d266).
mutagenic(train,d269).
mutagenic(train,d270).
mutagenic(train,d271).
mutagenic(train,d288).
mutagenic(train,d292).
ames(train,d1).
ames(train,d2).
ames(train,d3).
ames(train,d4).
ames(train,d5).
ames(train,d6).
ames(train,d7).
ames(train,d8).
ames(train,d9).
ames(train,d10).
ames(train,d11).
ames(train,d12).
ames(train,d13).
ames(train,d14).
ames(train,d15).
ames(train,d16).
ames(train,d17).
ames(train,d18).
ames(train,d19).
ames(train,d20).
ames(train,d21).
ames(train,d22).
ames(train,d23_1).
ames(train,d23_2).
ames(train,d24).
ames(train,d25).
ames(train,d26).
ames(train,d27).
ames(train,d28).
ames(train,d29).
ames(train,d30).
ames(train,d31).
ames(train,d32).
ames(train,d34).
ames(train,d36).
ames(train,d37).
ames(train,d38).
ames(train,d39).
ames(train,d41).
ames(train,d43).
ames(train,d44).
ames(train,d45).
ames(train,d46).
ames(train,d47).
ames(train,d48).
ames(train,d49).
ames(train,d50).
ames(train,d51).
ames(train,d52).
ames(train,d53).
ames(train,d54).
ames(train,d55).
ames(train,d56).
ames(train,d57).
ames(train,d58).
ames(train,d59).
ames(train,d60).
ames(train,d61).
ames(train,d62).
ames(train,d63).
ames(train,d67).
ames(train,d68).
ames(train,d70).
ames(train,d71).
ames(train,d72).
ames(train,d73).
ames(train,d75).
ames(train,d78).
ames(train,d79).
ames(train,d80).
ames(train,d81).
ames(train,d83).
ames(train,d113).
ames(train,d128).
ames(train,d171).
ames(train,d194).
ames(train,d205).
ames(train,d216).
ames(train,d226).
ames(train,d227).
ames(train,d228).
ames(train,d229).
ames(train,d230).
ames(train,d232).
ames(train,d234).
ames(train,d235).
ames(train,d236).
ames(train,d237).
ames(train,d238).
ames(train,d239).
ames(train,d240).
ames(train,d241).
ames(train,d242).
ames(train,d243).
ames(train,d244).
ames(train,d245).
ames(train,d248).
ames(train,d249).
ames(train,d250).
ames(train,d252).
ames(train,d254).
ames(train,d255).
ames(train,d256).
ames(train,d257).
ames(train,d258).
ames(train,d261).
ames(train,d262).
ames(train,d263).
ames(train,d265).
ames(train,d268).
ames(train,d270).
ames(train,d272).
ames(train,d273).
ames(train,d274).
ames(train,d276).
ames(train,d278).
ames(train,d280).
ames(train,d281).
ames(train,d282).
ames(train,d286).
ames(train,d287).
ames(train,d288).
ames(train,d289).
ames(train,d290).
ames(train,d293).
ames(train,d294).
atm(train,d1,d1_1,c,22,-0.133).
atm(train,d1,d1_2,c,22,-0.133).
atm(train,d1,d1_3,c,22,-0.003).
atm(train,d1,d1_4,c,22,-0.003).
atm(train,d1,d1_5,c,22,-0.133).
atm(train,d1,d1_6,c,22,-0.133).
atm(train,d1,d1_7,h,3,0.127).
atm(train,d1,d1_8,h,3,0.127).
atm(train,d1,d1_9,h,3,0.127).
atm(train,d1,d1_10,h,3,0.127).
atm(train,d1,d1_11,c,14,0.547).
atm(train,d1,d1_12,c,22,-0.003).
atm(train,d1,d1_13,c,22,-0.003).
atm(train,d1,d1_14,c,14,0.547).
atm(train,d1,d1_15,c,22,-0.133).
atm(train,d1,d1_16,c,22,-0.133).
atm(train,d1,d1_17,c,22,0.197).
atm(train,d1,d1_18,c,22,-0.133).
atm(train,d1,d1_19,h,3,0.127).
atm(train,d1,d1_20,h,3,0.127).
atm(train,d1,d1_21,h,3,0.127).
atm(train,d1,d1_22,o,40,-0.553).
atm(train,d1,d1_23,o,40,-0.554).
atm(train,d1,d1_24,n,32,-0.784).
atm(train,d1,d1_25,h,1,0.327).
atm(train,d1,d1_26,h,1,0.327).
atm(train,d10,d10_1,c,22,-0.042).
atm(train,d10,d10_2,c,22,-0.042).
atm(train,d10,d10_3,c,22,-0.042).
atm(train,d10,d10_4,c,22,-0.042).
atm(train,d10,d10_5,c,22,0.287).
atm(train,d10,d10_6,c,22,-0.042).
atm(train,d10,d10_7,h,3,0.218).
atm(train,d10,d10_8,h,3,0.217).
atm(train,d10,d10_9,h,3,0.218).
atm(train,d10,d10_10,h,3,0.218).
atm(train,d10,d10_11,h,3,0.218).
atm(train,d10,d10_12,n,32,-0.313).
atm(train,d10,d10_13,n,32,-0.313).
atm(train,d10,d10_14,o,45,-0.563).
atm(train,d10,d10_15,h,1,0.487).
atm(train,d10,d10_16,o,40,-0.464).
atm(train,d100,d100_1,c,22,-0.105).
atm(train,d100,d100_2,c,22,-0.105).
atm(train,d100,d100_3,c,22,-0.105).
atm(train,d100,d100_4,c,22,0.125).
atm(train,d100,d100_5,c,22,-0.105).
atm(train,d100,d100_6,c,22,-0.105).
atm(train,d100,d100_7,h,3,0.156).
atm(train,d100,d100_8,h,3,0.155).
atm(train,d100,d100_9,h,3,0.155).
atm(train,d100,d100_10,h,3,0.156).
atm(train,d100,d100_11,c,22,-0.105).
atm(train,d100,d100_12,c,22,-0.105).
atm(train,d100,d100_13,c,22,-0.105).
atm(train,d100,d100_14,c,22,-0.105).
atm(train,d100,d100_15,c,22,-0.105).
atm(train,d100,d100_16,c,22,0.125).
atm(train,d100,d100_17,h,3,0.156).
atm(train,d100,d100_18,h,3,0.156).
atm(train,d100,d100_19,h,3,0.155).
atm(train,d100,d100_20,h,3,0.155).
atm(train,d100,d100_21,c,16,-0.174).
atm(train,d100,d100_22,c,16,0.426).
atm(train,d100,d100_23,cl,93,-0.174).
atm(train,d100,d100_24,cl,93,-0.174).
atm(train,d100,d100_25,cl,93,-0.174).
atm(train,d100,d100_26,cl,93,-0.174).
atm(train,d101,d101_1,c,10,0.177).
atm(train,d101,d101_2,c,10,-0.024).
atm(train,d101,d101_3,c,16,-0.124).
atm(train,d101,d101_4,c,16,-0.124).
atm(train,d101,d101_5,c,10,-0.024).
atm(train,d101,d101_6,c,10,-0.074).
atm(train,d101,d101_7,c,10,0.177).
atm(train,d101,d101_8,c,16,-0.124).
atm(train,d101,d101_9,c,16,-0.124).
atm(train,d101,d101_10,h,3,0.127).
atm(train,d101,d101_11,h,3,0.046).
atm(train,d101,d101_12,h,3,0.177).
atm(train,d101,d101_13,h,3,0.177).
atm(train,d101,d101_14,c,10,0.477).
atm(train,d101,d101_15,cl,93,-0.124).
atm(train,d101,d101_16,cl,93,-0.124).
atm(train,d101,d101_17,cl,93,-0.124).
atm(train,d101,d101_18,h,3,0.127).
atm(train,d101,d101_19,cl,93,-0.124).
atm(train,d101,d101_20,cl,93,-0.124).
atm(train,d101,d101_21,cl,93,-0.124).
atm(train,d101,d101_22,cl,93,-0.123).
atm(train,d102,d102_1,c,10,0.507).
atm(train,d102,d102_2,c,10,0.407).
atm(train,d102,d102_3,cl,93,-0.194).
atm(train,d102,d102_4,cl,93,-0.194).
atm(train,d102,d102_5,cl,93,-0.194).
atm(train,d102,d102_6,cl,93,-0.194).
atm(train,d102,d102_7,cl,93,-0.194).
atm(train,d102,d102_8,h,3,0.056).
atm(train,d103,d103_1,c,10,0.513).
atm(train,d103,d103_2,c,10,0.112).
atm(train,d103,d103_3,cl,93,-0.187).
atm(train,d103,d103_4,h,3,0.062).
atm(train,d103,d103_5,h,3,0.062).
atm(train,d103,d103_6,cl,93,-0.187).
atm(train,d103,d103_7,cl,93,-0.188).
atm(train,d103,d103_8,cl,93,-0.187).
atm(train,d104,d104_1,c,10,0.388).
atm(train,d104,d104_2,c,10,0.388).
atm(train,d104,d104_3,cl,93,-0.212).
atm(train,d104,d104_4,cl,93,-0.212).
atm(train,d104,d104_5,h,3,0.037).
atm(train,d104,d104_6,cl,93,-0.213).
atm(train,d104,d104_7,cl,93,-0.213).
atm(train,d104,d104_8,h,3,0.037).
atm(train,d105,d105_1,c,16,0.45).
atm(train,d105,d105_2,c,16,-0.15).
atm(train,d105,d105_3,cl,93,-0.15).
atm(train,d105,d105_4,cl,93,-0.15).
atm(train,d105,d105_5,cl,93,-0.15).
atm(train,d105,d105_6,h,3,0.15).
atm(train,d106,d106_1,c,10,-0.086).
atm(train,d106,d106_2,c,10,-0.036).
atm(train,d106,d106_3,c,16,-0.136).
atm(train,d106,d106_4,c,16,-0.136).
atm(train,d106,d106_5,c,10,-0.036).
atm(train,d106,d106_6,c,10,-0.086).
atm(train,d106,d106_7,c,10,0.163).
atm(train,d106,d106_8,c,16,-0.136).
atm(train,d106,d106_9,c,16,-0.136).
atm(train,d106,d106_10,c,10,0.164).
atm(train,d106,d106_11,h,3,0.034).
atm(train,d106,d106_12,h,3,0.034).
atm(train,d106,d106_13,h,3,0.163).
atm(train,d106,d106_14,h,3,0.163).
atm(train,d106,d106_15,c,10,0.463).
atm(train,d106,d106_16,c,10,-0.036).
atm(train,d106,d106_17,h,3,0.114).
atm(train,d106,d106_18,h,3,0.114).
atm(train,d106,d106_19,cl,93,-0.136).
atm(train,d106,d106_20,cl,93,-0.136).
atm(train,d106,d106_21,cl,93,-0.136).
atm(train,d106,d106_22,cl,93,-0.136).
atm(train,d106,d106_23,cl,93,-0.136).
atm(train,d106,d106_24,cl,93,-0.136).
atm(train,d106,d106_25,h,3,0.114).
atm(train,d106,d106_26,h,3,0.114).
atm(train,d107,d107c_1,c,10,0.1).
atm(train,d107,d107c_2,c,10,-0.1).
atm(train,d107,d107c_3,c,10,-0.1).
atm(train,d107,d107c_4,h,3,0.05).
atm(train,d107,d107c_5,h,3,0.05).
atm(train,d107,d107c_6,c,10,0.4).
atm(train,d107,d107c_7,h,3,0.05).
atm(train,d107,d107c_8,h,3,0.05).
atm(train,d107,d107c_9,c,10,-0.1).
atm(train,d107,d107c_10,c,10,-0.1).
atm(train,d107,d107c_11,h,3,0.05).
atm(train,d107,d107c_12,h,3,0.05).
atm(train,d107,d107c_13,c,10,-0.1).
atm(train,d107,d107c_14,h,3,0.05).
atm(train,d107,d107c_15,h,3,0.05).
atm(train,d107,d107c_16,c,10,0.15).
atm(train,d107,d107c_17,h,3,0.05).
atm(train,d107,d107c_18,h,3,0.05).
atm(train,d107,d107c_19,c,10,0.15).
atm(train,d107,d107c_20,c,10,-0.1).
atm(train,d107,d107c_21,c,10,0.15).
atm(train,d107,d107c_22,h,3,0.05).
atm(train,d107,d107c_23,h,3,0.05).
atm(train,d107,d107c_24,c,10,-0.1).
atm(train,d107,d107c_25,c,10,-0.1).
atm(train,d107,d107c_26,h,3,0.05).
atm(train,d107,d107c_27,h,3,0.05).
atm(train,d107,d107c_28,c,10,0.15).
atm(train,d107,d107c_29,h,3,0.05).
atm(train,d107,d107c_30,h,3,0.05).
atm(train,d107,d107c_31,c,10,-0.1).
atm(train,d107,d107c_32,c,10,0.15).
atm(train,d107,d107c_33,h,3,0.05).
atm(train,d107,d107c_34,h,3,0.05).
atm(train,d107,d107c_35,c,10,-0.1).
atm(train,d107,d107c_36,c,10,-0.1).
atm(train,d107,d107c_37,h,3,0.05).
atm(train,d107,d107c_38,h,3,0.05).
atm(train,d107,d107c_39,c,10,0.15).
atm(train,d107,d107c_40,h,3,0.05).
atm(train,d107,d107c_41,h,3,0.05).
atm(train,d107,d107c_42,c,10,0.15).
atm(train,d107,d107c_43,c,10,-0.1).
atm(train,d107,d107c_44,c,10,-0.1).
atm(train,d107,d107c_45,h,3,0.05).
atm(train,d107,d107c_46,h,3,0.05).
atm(train,d107,d107c_47,c,10,-0.15).
atm(train,d107,d107c_48,h,3,0.05).
atm(train,d107,d107c_49,h,3,0.05).
atm(train,d107,d107c_50,h,3,0.05).
atm(train,d107,d107c_51,h,3,0.05).
atm(train,d107,d107c_52,h,3,0.05).
atm(train,d107,d107c_53,cl,93,-0.2).
atm(train,d107,d107c_54,h,3,0.05).
atm(train,d107,d107c_55,h,3,0.05).
atm(train,d107,d107c_56,cl,93,-0.2).
atm(train,d107,d107c_57,cl,93,-0.2).
atm(train,d107,d107c_58,cl,93,-0.2).
atm(train,d107,d107c_59,h,3,0.05).
atm(train,d107,d107c_60,cl,93,-0.2).
atm(train,d107,d107c_61,h,3,0.05).
atm(train,d107,d107c_62,cl,93,-0.2).
atm(train,d107,d107c_63,h,3,0.05).
atm(train,d107,d107c_64,cl,93,-0.2).
atm(train,d107,d107c_65,h,3,0.05).
atm(train,d107,d107c_66,cl,93,-0.2).
atm(train,d107,d107c_67,h,3,0.05).
atm(train,d107,d107c_68,cl,93,-0.2).
atm(train,d107,d107c_69,h,3,0.05).
atm(train,d107,d107c_70,cl,93,-0.2).
atm(train,d107,d107c_71,h,3,0.05).
atm(train,d107,d107d_1,c,10,-0.277).
atm(train,d107,d107d_2,c,10,-0.276).
atm(train,d107,d107d_3,c,10,-0.276).
atm(train,d107,d107d_4,c,10,-0.277).
atm(train,d107,d107d_5,c,10,-0.276).
atm(train,d107,d107d_6,c,10,-0.076).
atm(train,d107,d107d_7,c,10,-0.276).
atm(train,d107,d107d_8,c,10,-0.126).
atm(train,d107,d107d_9,c,10,-0.066).
atm(train,d107,d107d_10,h,3,0.074).
atm(train,d107,d107d_11,h,3,0.074).
atm(train,d107,d107d_12,h,3,0.074).
atm(train,d107,d107d_13,c,10,-0.066).
atm(train,d107,d107d_14,h,3,0.074).
atm(train,d107,d107d_15,h,3,0.074).
atm(train,d107,d107d_16,h,3,0.074).
atm(train,d107,d107d_17,c,10,0.523).
atm(train,d107,d107d_18,c,10,-0.066).
atm(train,d107,d107d_19,h,3,0.074).
atm(train,d107,d107d_20,h,3,0.074).
atm(train,d107,d107d_21,h,3,0.074).
atm(train,d107,d107d_22,c,10,-0.066).
atm(train,d107,d107d_23,h,3,0.074).
atm(train,d107,d107d_24,h,3,0.074).
atm(train,d107,d107d_25,h,3,0.074).
atm(train,d107,d107d_26,c,10,-0.066).
atm(train,d107,d107d_27,h,3,0.074).
atm(train,d107,d107d_28,h,3,0.074).
atm(train,d107,d107d_29,h,3,0.074).
atm(train,d107,d107d_30,c,10,-0.066).
atm(train,d107,d107d_31,h,3,0.074).
atm(train,d107,d107d_32,h,3,0.074).
atm(train,d107,d107d_33,h,3,0.074).
atm(train,d107,d107d_34,c,10,0.523).
atm(train,d107,d107d_35,c,10,-0.066).
atm(train,d107,d107d_36,h,3,0.074).
atm(train,d107,d107d_37,h,3,0.074).
atm(train,d107,d107d_38,h,3,0.074).
atm(train,d107,d107d_39,c,10,-0.066).
atm(train,d107,d107d_40,h,3,0.074).
atm(train,d107,d107d_41,h,3,0.074).
atm(train,d107,d107d_42,h,3,0.074).
atm(train,d107,d107d_43,c,10,0.124).
atm(train,d107,d107d_44,c,10,0.124).
atm(train,d107,d107d_45,c,10,0.124).
atm(train,d107,d107d_46,c,10,-0.126).
atm(train,d107,d107d_47,h,3,-0.006).
atm(train,d107,d107d_48,h,3,0.074).
atm(train,d107,d107d_49,h,3,0.074).
atm(train,d107,d107d_50,h,3,0.074).
atm(train,d107,d107d_51,c,10,0.124).
atm(train,d107,d107d_52,h,3,0.074).
atm(train,d107,d107d_53,h,3,0.074).
atm(train,d107,d107d_54,cl,93,-0.176).
atm(train,d107,d107d_55,cl,93,-0.176).
atm(train,d107,d107d_56,cl,93,-0.176).
atm(train,d107,d107d_57,cl,93,-0.176).
atm(train,d107,d107d_58,cl,93,-0.176).
atm(train,d107,d107d_59,cl,93,-0.176).
atm(train,d107,d107d_60,cl,93,-0.176).
atm(train,d107,d107d_61,h,3,0.074).
atm(train,d107,d107d_62,h,3,0.074).
atm(train,d107,d107d_63,cl,93,-0.176).
atm(train,d107,d107d_64,h,3,0.074).
atm(train,d107,d107d_65,h,3,0.074).
atm(train,d107,d107d_66,cl,93,-0.176).
atm(train,d107,d107d_67,h,3,0.074).
atm(train,d107,d107d_68,h,3,0.074).
atm(train,d107,d107d_69,cl,93,-0.176).
atm(train,d107,d107d_70,h,3,0.074).
atm(train,d107,d107d_71,h,3,0.074).
atm(train,d108,d108_1,c,22,-0.086).
atm(train,d108,d108_2,c,22,-0.086).
atm(train,d108,d108_3,c,22,-0.086).
atm(train,d108,d108_4,c,22,-0.086).
atm(train,d108,d108_5,c,22,-0.086).
atm(train,d108,d108_6,c,22,-0.086).
atm(train,d108,d108_7,h,3,0.174).
atm(train,d108,d108_8,h,3,0.174).
atm(train,d108,d108_9,h,3,0.173).
atm(train,d108,d108_10,h,3,0.174).
atm(train,d108,d108_11,c,22,-0.086).
atm(train,d108,d108_12,c,22,-0.086).
atm(train,d108,d108_13,c,22,-0.086).
atm(train,d108,d108_14,c,22,-0.086).
atm(train,d108,d108_15,c,22,-0.086).
atm(train,d108,d108_16,c,22,-0.086).
atm(train,d108,d108_17,h,3,0.173).
atm(train,d108,d108_18,h,3,0.173).
atm(train,d108,d108_19,h,3,0.174).
atm(train,d108,d108_20,h,3,0.174).
atm(train,d108,d108_21,c,10,0.044).
atm(train,d108,d108_22,o,45,-0.607).
atm(train,d108,d108_23,c,10,0.543).
atm(train,d108,d108_24,cl,93,-0.156).
atm(train,d108,d108_25,cl,93,-0.156).
atm(train,d108,d108_26,cl,93,-0.156).
atm(train,d108,d108_27,cl,93,-0.156).
atm(train,d108,d108_28,cl,93,-0.156).
atm(train,d108,d108_29,h,8,0.443).
atm(train,d109,d109_1,c,21,0.022).
atm(train,d109,d109_2,c,21,-0.108).
atm(train,d109,d109_3,c,21,-0.108).
atm(train,d109,d109_4,c,21,-0.108).
atm(train,d109,d109_5,o,52,-0.008).
atm(train,d109,d109_6,c,22,-0.108).
atm(train,d109,d109_7,c,22,-0.108).
atm(train,d109,d109_8,c,22,-0.108).
atm(train,d109,d109_9,c,22,0.221).
atm(train,d109,d109_10,c,22,0.022).
atm(train,d109,d109_11,c,22,-0.108).
atm(train,d109,d109_12,c,14,0.801).
atm(train,d109,d109_13,o,45,-0.649).
atm(train,d109,d109_14,cl,93,-0.179).
atm(train,d109,d109_15,s,77,0.371).
atm(train,d109,d109_16,n,36,-0.279).
atm(train,d109,d109_17,o,51,-0.589).
atm(train,d109,d109_18,o,40,-0.229).
atm(train,d109,d109_19,o,40,-0.229).
atm(train,d109,d109_20,n,32,-0.379).
atm(train,d109,d109_21,c,10,0.022).
atm(train,d109,d109_22,h,3,0.152).
atm(train,d109,d109_23,h,3,0.152).
atm(train,d109,d109_24,h,1,0.271).
atm(train,d109,d109_25,h,1,0.171).
atm(train,d109,d109_26,h,1,0.171).
atm(train,d109,d109_27,h,1,0.321).
atm(train,d109,d109_28,h,3,0.072).
atm(train,d109,d109_29,h,3,0.072).
atm(train,d109,d109_30,h,3,0.152).
atm(train,d109,d109_31,h,3,0.152).
atm(train,d109,d109_32,h,3,0.152).
atm(train,d11,d11_1,c,22,-0.147).
atm(train,d11,d11_2,c,22,0.163).
atm(train,d11,d11_3,c,22,0.183).
atm(train,d11,d11_4,c,22,-0.147).
atm(train,d11,d11_5,c,22,0.183).
atm(train,d11,d11_6,c,22,-0.147).
atm(train,d11,d11_7,h,3,0.112).
atm(train,d11,d11_8,h,3,0.112).
atm(train,d11,d11_9,h,3,0.112).
atm(train,d11,d11_10,n,32,-0.798).
atm(train,d11,d11_11,n,32,-0.798).
atm(train,d11,d11_12,o,50,-0.248).
atm(train,d11,d11_13,c,10,0.073).
atm(train,d11,d11_14,h,3,0.032).
atm(train,d11,d11_15,h,3,0.032).
atm(train,d11,d11_16,h,3,0.032).
atm(train,d11,d11_17,h,1,0.313).
atm(train,d11,d11_18,h,1,0.313).
atm(train,d11,d11_19,h,1,0.312).
atm(train,d11,d11_20,h,1,0.313).
atm(train,d110,d110_1,c,22,-0.102).
atm(train,d110,d110_2,c,22,-0.102).
atm(train,d110,d110_3,c,22,-0.102).
atm(train,d110,d110_4,c,22,-0.102).
atm(train,d110,d110_5,c,22,-0.102).
atm(train,d110,d110_6,c,22,-0.102).
atm(train,d110,d110_7,h,3,0.157).
atm(train,d110,d110_8,h,3,0.157).
atm(train,d110,d110_9,h,3,0.157).
atm(train,d110,d110_10,h,3,0.157).
atm(train,d110,d110_11,h,3,0.157).
atm(train,d110,d110_12,cl,93,-0.173).
atm(train,d111,d111_1,c,22,-0.112).
atm(train,d111,d111_2,c,22,-0.112).
atm(train,d111,d111_3,c,22,0.198).
atm(train,d111,d111_4,c,22,0.199).
atm(train,d111,d111_5,c,22,-0.112).
atm(train,d111,d111_6,c,22,-0.112).
atm(train,d111,d111_7,h,3,0.148).
atm(train,d111,d111_8,h,3,0.148).
atm(train,d111,d111_9,h,3,0.148).
atm(train,d111,d111_10,c,22,0.199).
atm(train,d111,d111_11,c,22,-0.112).
atm(train,d111,d111_12,c,22,-0.112).
atm(train,d111,d111_13,c,22,-0.112).
atm(train,d111,d111_14,c,22,-0.112).
atm(train,d111,d111_15,c,22,0.198).
atm(train,d111,d111_16,h,3,0.148).
atm(train,d111,d111_17,h,3,0.148).
atm(train,d111,d111_18,h,3,0.148).
atm(train,d111,d111_19,o,50,-0.211).
atm(train,d111,d111_20,o,50,-0.211).
atm(train,d111,d111_21,cl,93,-0.182).
atm(train,d111,d111_22,cl,93,-0.182).
atm(train,d112,d112_1,c,191,-0.047).
atm(train,d112,d112_2,c,10,-0.047).
atm(train,d112,d112_3,c,10,-0.097).
atm(train,d112,d112_4,c,10,-0.097).
atm(train,d112,d112_5,c,10,-0.047).
atm(train,d112,d112_6,c,191,-0.047).
atm(train,d112,d112_7,c,10,-0.047).
atm(train,d112,d112_8,c,10,0.202).
atm(train,d112,d112_9,c,10,0.202).
atm(train,d112,d112_10,c,10,-0.047).
atm(train,d112,d112_11,h,3,0.023).
atm(train,d112,d112_12,h,3,0.023).
atm(train,d112,d112_13,o,50,-0.318).
atm(train,d112,d112_14,h,3,0.103).
atm(train,d112,d112_15,h,3,0.103).
atm(train,d112,d112_16,c,10,-0.047).
atm(train,d112,d112_17,h,3,0.103).
atm(train,d112,d112_18,h,3,0.103).
atm(train,d112,d112_19,c,10,0.452).
atm(train,d112,d112_20,cl,93,-0.148).
atm(train,d112,d112_21,cl,93,-0.147).
atm(train,d112,d112_22,cl,93,-0.147).
atm(train,d112,d112_23,cl,93,-0.148).
atm(train,d112,d112_24,h,3,0.103).
atm(train,d112,d112_25,cl,93,-0.147).
atm(train,d112,d112_26,h,3,0.103).
atm(train,d112,d112_27,cl,93,-0.148).
atm(train,d112,d112_28,h,3,0.103).
atm(train,d112,d112_29,h,3,0.103).
atm(train,d113,d113_1,c,22,0.0).
atm(train,d113,d113_2,c,22,-0.13).
atm(train,d113,d113_3,c,22,-0.13).
atm(train,d113,d113_4,c,22,-0.13).
atm(train,d113,d113_5,c,22,-0.129).
atm(train,d113,d113_6,c,22,-0.129).
atm(train,d113,d113_7,h,3,0.131).
atm(train,d113,d113_8,h,3,0.131).
atm(train,d113,d113_9,h,3,0.131).
atm(train,d113,d113_10,h,3,0.131).
atm(train,d113,d113_11,c,22,-0.129).
atm(train,d113,d113_12,c,22,-0.13).
atm(train,d113,d113_13,c,22,-0.13).
atm(train,d113,d113_14,c,22,0.0).
atm(train,d113,d113_15,c,22,-0.13).
atm(train,d113,d113_16,c,22,-0.13).
atm(train,d113,d113_17,h,3,0.13).
atm(train,d113,d113_18,h,3,0.13).
atm(train,d113,d113_19,h,3,0.13).
atm(train,d113,d113_20,h,3,0.13).
atm(train,d113,d113_21,c,10,0.0).
atm(train,d113,d113_22,c,10,-0.1).
atm(train,d113,d113_23,c,10,-0.1).
atm(train,d113,d113_24,h,3,0.05).
atm(train,d113,d113_25,h,3,0.05).
atm(train,d113,d113_26,h,3,0.05).
atm(train,d113,d113_27,h,3,0.05).
atm(train,d113,d113_28,h,3,0.05).
atm(train,d113,d113_29,c,10,-0.1).
atm(train,d113,d113_30,c,10,-0.1).
atm(train,d113,d113_31,h,3,0.05).
atm(train,d113,d113_32,h,3,0.05).
atm(train,d113,d113_33,h,3,0.05).
atm(train,d113,d113_34,h,3,0.05).
atm(train,d113,d113_35,h,3,0.05).
atm(train,d113,d113_36,c,10,0.401).
atm(train,d113,d113_37,h,3,0.1).
atm(train,d113,d113_38,cl,93,-0.199).
atm(train,d113,d113_39,cl,93,-0.199).
atm(train,d113,d113_40,h,3,0.05).
atm(train,d114,d114_1,c,22,-0.03).
atm(train,d114,d114_2,n,35,-0.44).
atm(train,d114,d114_3,c,22,0.2).
atm(train,d114,d114_4,c,22,-0.03).
atm(train,d114,d114_5,c,22,0.3).
atm(train,d114,d114_6,c,22,-0.03).
atm(train,d114,d114_7,cl,93,-0.1).
atm(train,d114,d114_8,n,32,-0.68).
atm(train,d114,d114_9,cl,93,-0.1).
atm(train,d114,d114_10,c,14,0.88).
atm(train,d114,d114_11,o,45,-0.57).
atm(train,d114,d114_12,cl,93,-0.1).
atm(train,d114,d114_13,h,1,0.43).
atm(train,d114,d114_14,h,1,0.43).
atm(train,d114,d114_15,o,51,-0.51).
atm(train,d114,d114_16,h,1,0.35).
atm(train,d115,d115_1,sn,113,0.8).
atm(train,d115,d115_2,cl,93,-0.4).
atm(train,d115,d115_3,cl,93,-0.4).
atm(train,d116,d116_1,c,22,-0.102).
atm(train,d116,d116_2,c,22,-0.102).
atm(train,d116,d116_3,c,22,-0.102).
atm(train,d116,d116_4,c,22,-0.102).
atm(train,d116,d116_5,c,22,-0.102).
atm(train,d116,d116_6,c,22,-0.102).
atm(train,d116,d116_7,h,3,0.157).
atm(train,d116,d116_8,h,3,0.157).
atm(train,d116,d116_9,h,3,0.157).
atm(train,d116,d116_10,h,3,0.157).
atm(train,d116,d116_11,c,22,-0.102).
atm(train,d116,d116_12,c,22,-0.102).
atm(train,d116,d116_13,c,22,-0.102).
atm(train,d116,d116_14,c,22,-0.102).
atm(train,d116,d116_15,c,22,-0.102).
atm(train,d116,d116_16,c,22,-0.102).
atm(train,d116,d116_17,h,3,0.157).
atm(train,d116,d116_18,h,3,0.157).
atm(train,d116,d116_19,h,3,0.157).
atm(train,d116,d116_20,h,3,0.157).
atm(train,d116,d116_21,c,10,0.027).
atm(train,d116,d116_22,c,10,0.427).
atm(train,d116,d116_23,h,3,0.127).
atm(train,d116,d116_24,cl,93,-0.173).
atm(train,d116,d116_25,cl,93,-0.172).
atm(train,d116,d116_26,h,3,0.078).
atm(train,d116,d116_27,cl,93,-0.173).
atm(train,d116,d116_28,cl,93,-0.173).
atm(train,d117,d117_1,c,17,0.204).
atm(train,d117,d117_2,c,17,-0.196).
atm(train,d117,d117_3,c,10,0.104).
atm(train,d117,d117_4,c,10,0.104).
atm(train,d117,d117_5,c,16,-0.196).
atm(train,d117,d117_6,c,16,0.004).
atm(train,d117,d117_7,n,36,-0.297).
atm(train,d117,d117_8,c,10,0.303).
atm(train,d117,d117_9,n,36,-0.297).
atm(train,d117,d117_10,s,77,0.353).
atm(train,d117,d117_11,h,3,0.054).
atm(train,d117,d117_12,h,3,0.054).
atm(train,d117,d117_13,h,3,0.054).
atm(train,d117,d117_14,h,3,0.054).
atm(train,d117,d117_15,h,3,0.104).
atm(train,d117,d117_16,h,3,0.104).
atm(train,d117,d117_17,s,77,0.353).
atm(train,d117,d117_18,cl,93,-0.196).
atm(train,d117,d117_19,o,40,-0.246).
atm(train,d117,d117_20,o,40,-0.247).
atm(train,d117,d117_21,h,1,0.154).
atm(train,d117,d117_22,h,1,0.154).
atm(train,d117,d117_23,n,36,-0.297).
atm(train,d117,d117_24,o,40,-0.247).
atm(train,d117,d117_25,o,40,-0.246).
atm(train,d117,d117_26,h,1,0.154).
atm(train,d117,d117_27,h,1,0.154).
atm(train,d118,d118_1,c,22,-0.019).
atm(train,d118,d118_2,n,35,-0.659).
atm(train,d118,d118_3,c,22,0.98).
atm(train,d118,d118_4,n,35,-0.659).
atm(train,d118,d118_5,c,22,-0.019).
atm(train,d118,d118_6,n,35,-0.659).
atm(train,d118,d118_7,c,22,-0.019).
atm(train,d118,d118_8,c,22,-0.019).
atm(train,d118,d118_9,c,22,-0.019).
atm(train,d118,d118_10,c,22,-0.019).
atm(train,d118,d118_11,c,22,-0.019).
atm(train,d118,d118_12,c,22,0.311).
atm(train,d118,d118_13,h,3,0.241).
atm(train,d118,d118_14,h,3,0.241).
atm(train,d118,d118_15,h,3,0.241).
atm(train,d118,d118_16,h,3,0.241).
atm(train,d118,d118_17,n,32,-0.289).
atm(train,d118,d118_18,cl,93,-0.089).
atm(train,d118,d118_19,cl,93,-0.089).
atm(train,d118,d118_20,cl,93,-0.089).
atm(train,d118,d118_21,h,1,0.411).
atm(train,d119,d119_1,n,36,-0.327).
atm(train,d119,d119_2,c,10,0.073).
atm(train,d119,d119_3,h,3,0.023).
atm(train,d119,d119_4,h,3,0.023).
atm(train,d119,d119_5,h,3,0.022).
atm(train,d119,d119_6,c,10,0.073).
atm(train,d119,d119_7,h,3,0.022).
atm(train,d119,d119_8,h,3,0.022).
atm(train,d119,d119_9,h,3,0.023).
atm(train,d119,d119_10,c,10,0.073).
atm(train,d119,d119_11,h,3,0.022).
atm(train,d119,d119_12,h,3,0.022).
atm(train,d119,d119_13,h,3,0.022).
atm(train,d119,d119_14,c,10,-0.027).
atm(train,d119,d119_15,c,10,0.073).
atm(train,d119,d119_16,h,3,0.022).
atm(train,d119,d119_17,h,3,0.022).
atm(train,d119,d119_18,cl,93,-0.227).
atm(train,d119,d119_19,h,3,0.022).
atm(train,d119,d119_20,h,3,0.022).
atm(train,d12,d12_1,c,22,-0.137).
atm(train,d12,d12_2,c,22,-0.187).
atm(train,d12,d12_3,c,22,0.193).
atm(train,d12,d12_4,c,22,-0.137).
atm(train,d12,d12_5,c,22,0.193).
atm(train,d12,d12_6,c,22,-0.137).
atm(train,d12,d12_7,h,3,0.123).
atm(train,d12,d12_8,h,3,0.123).
atm(train,d12,d12_9,h,3,0.123).
atm(train,d12,d12_10,n,32,-0.786).
atm(train,d12,d12_11,n,32,-0.786).
atm(train,d12,d12_12,h,1,0.323).
atm(train,d12,d12_13,h,1,0.323).
atm(train,d12,d12_14,h,1,0.323).
atm(train,d12,d12_15,h,1,0.324).
atm(train,d12,d12_16,c,10,-0.007).
atm(train,d12,d12_17,h,3,0.043).
atm(train,d12,d12_18,h,3,0.043).
atm(train,d12,d12_19,h,3,0.043).
atm(train,d120,d120_1,c,22,-0.075).
atm(train,d120,d120_2,c,22,-0.075).
atm(train,d120,d120_3,c,22,-0.075).
atm(train,d120,d120_4,c,22,-0.075).
atm(train,d120,d120_5,c,22,-0.075).
atm(train,d120,d120_6,c,22,-0.075).
atm(train,d120,d120_7,h,3,0.185).
atm(train,d120,d120_8,h,3,0.185).
atm(train,d120,d120_9,h,3,0.185).
atm(train,d120,d120_10,h,3,0.185).
atm(train,d120,d120_11,cl,93,-0.145).
atm(train,d120,d120_12,cl,93,-0.145).
atm(train,d121,d121_1,c,16,-0.114).
atm(train,d121,d121_2,c,10,-0.014).
atm(train,d121,d121_3,c,10,-0.064).
atm(train,d121,d121_4,c,10,-0.064).
atm(train,d121,d121_5,c,10,-0.014).
atm(train,d121,d121_6,c,16,-0.114).
atm(train,d121,d121_7,c,10,-0.014).
atm(train,d121,d121_8,c,191,-0.014).
atm(train,d121,d121_9,c,191,-0.014).
atm(train,d121,d121_10,c,10,-0.014).
atm(train,d121,d121_11,h,3,0.056).
atm(train,d121,d121_12,h,3,0.056).
atm(train,d121,d121_13,cl,93,-0.114).
atm(train,d121,d121_14,cl,93,-0.114).
atm(train,d121,d121_15,cl,93,-0.114).
atm(train,d121,d121_16,cl,93,-0.114).
atm(train,d121,d121_17,o,50,-0.283).
atm(train,d121,d121_18,h,3,0.137).
atm(train,d121,d121_19,h,3,0.137).
atm(train,d121,d121_20,c,10,-0.014).
atm(train,d121,d121_21,h,3,0.137).
atm(train,d121,d121_22,h,3,0.137).
atm(train,d121,d121_23,h,3,0.137).
atm(train,d121,d121_24,h,3,0.137).
atm(train,d121,d121_25,c,10,0.487).
atm(train,d121,d121_26,cl,93,-0.114).
atm(train,d121,d121_27,cl,93,-0.114).
atm(train,d122,d122_1,c,22,0.176).
atm(train,d122,d122_2,c,22,-0.133).
atm(train,d122,d122_3,c,22,-0.133).
atm(train,d122,d122_4,c,22,-0.133).
atm(train,d122,d122_5,c,22,-0.133).
atm(train,d122,d122_6,c,22,-0.134).
atm(train,d122,d122_7,h,3,0.127).
atm(train,d122,d122_8,h,3,0.127).
atm(train,d122,d122_9,h,3,0.127).
atm(train,d122,d122_10,h,3,0.127).
atm(train,d122,d122_11,c,22,-0.134).
atm(train,d122,d122_12,c,22,-0.134).
atm(train,d122,d122_13,c,22,0.176).
atm(train,d122,d122_14,c,22,-0.133).
atm(train,d122,d122_15,c,22,-0.133).
atm(train,d122,d122_16,c,22,-0.134).
atm(train,d122,d122_17,h,3,0.127).
atm(train,d122,d122_18,h,3,0.127).
atm(train,d122,d122_19,h,3,0.127).
atm(train,d122,d122_20,h,3,0.127).
atm(train,d122,d122_21,c,10,-0.003).
atm(train,d122,d122_22,c,10,0.496).
atm(train,d122,d122_23,h,3,0.097).
atm(train,d122,d122_24,cl,93,-0.204).
atm(train,d122,d122_25,cl,93,-0.204).
atm(train,d122,d122_26,cl,93,-0.204).
atm(train,d122,d122_27,o,50,-0.234).
atm(train,d122,d122_28,c,10,0.087).
atm(train,d122,d122_29,h,3,0.047).
atm(train,d122,d122_30,h,3,0.047).
atm(train,d122,d122_31,h,3,0.047).
atm(train,d122,d122_32,o,50,-0.234).
atm(train,d122,d122_33,c,10,0.087).
atm(train,d122,d122_34,h,3,0.047).
atm(train,d122,d122_35,h,3,0.047).
atm(train,d122,d122_36,h,3,0.047).
atm(train,d123,d123_1,c,10,-0.138).
atm(train,d123,d123_2,c,10,-0.088).
atm(train,d123,d123_3,h,3,0.062).
atm(train,d123,d123_4,h,3,0.062).
atm(train,d123,d123_5,h,3,0.062).
atm(train,d123,d123_6,c,10,-0.088).
atm(train,d123,d123_7,h,3,0.062).
atm(train,d123,d123_8,h,3,0.062).
atm(train,d123,d123_9,c,14,0.612).
atm(train,d123,d123_10,br,94,-0.138).
atm(train,d123,d123_11,c,10,-0.088).
atm(train,d123,d123_12,c,10,-0.138).
atm(train,d123,d123_13,h,3,0.062).
atm(train,d123,d123_14,h,3,0.062).
atm(train,d123,d123_15,h,3,0.062).
atm(train,d123,d123_16,h,3,0.062).
atm(train,d123,d123_17,h,3,0.062).
atm(train,d123,d123_18,n,32,-0.388).
atm(train,d123,d123_19,c,14,0.612).
atm(train,d123,d123_20,n,32,-0.588).
atm(train,d123,d123_21,o,40,-0.538).
atm(train,d123,d123_22,h,1,0.262).
atm(train,d123,d123_23,o,40,-0.538).
atm(train,d123,d123_24,h,1,0.312).
atm(train,d123,d123_25,h,1,0.312).
atm(train,d124,d124_1,n,32,-0.378).
atm(train,d124,d124_2,c,14,0.622).
atm(train,d124,d124_3,n,32,-0.378).
atm(train,d124,d124_4,o,40,-0.528).
atm(train,d124,d124_5,s,77,0.372).
atm(train,d124,d124_6,h,1,0.322).
atm(train,d124,d124_7,o,40,-0.228).
atm(train,d124,d124_8,o,40,-0.228).
atm(train,d124,d124_9,c,22,-0.108).
atm(train,d124,d124_10,c,22,-0.108).
atm(train,d124,d124_11,c,22,-0.108).
atm(train,d124,d124_12,c,22,-0.108).
atm(train,d124,d124_13,c,22,-0.108).
atm(train,d124,d124_14,c,22,-0.108).
atm(train,d124,d124_15,h,3,0.152).
atm(train,d124,d124_16,h,3,0.152).
atm(train,d124,d124_17,h,3,0.152).
atm(train,d124,d124_18,h,3,0.152).
atm(train,d124,d124_19,cl,93,-0.178).
atm(train,d124,d124_20,c,10,0.022).
atm(train,d124,d124_21,h,1,0.322).
atm(train,d124,d124_22,c,10,-0.078).
atm(train,d124,d124_23,h,3,0.072).
atm(train,d124,d124_24,h,3,0.072).
atm(train,d124,d124_25,c,10,-0.128).
atm(train,d124,d124_26,h,3,0.072).
atm(train,d124,d124_27,h,3,0.072).
atm(train,d124,d124_28,h,3,0.072).
atm(train,d124,d124_29,h,3,0.072).
atm(train,d124,d124_30,h,3,0.072).
atm(train,d125,d125_1,c,22,-0.097).
atm(train,d125,d125_2,c,22,-0.097).
atm(train,d125,d125_3,c,22,-0.097).
atm(train,d125,d125_4,c,22,-0.097).
atm(train,d125,d125_5,c,22,-0.097).
atm(train,d125,d125_6,c,22,-0.097).
atm(train,d125,d125_7,h,3,0.163).
atm(train,d125,d125_8,h,3,0.163).
atm(train,d125,d125_9,h,3,0.163).
atm(train,d125,d125_10,h,3,0.163).
atm(train,d125,d125_11,c,22,-0.097).
atm(train,d125,d125_12,c,22,-0.097).
atm(train,d125,d125_13,c,22,-0.097).
atm(train,d125,d125_14,c,22,-0.097).
atm(train,d125,d125_15,c,22,-0.097).
atm(train,d125,d125_16,c,22,-0.097).
atm(train,d125,d125_17,h,3,0.163).
atm(train,d125,d125_18,h,3,0.163).
atm(train,d125,d125_19,h,3,0.163).
atm(train,d125,d125_20,h,3,0.163).
atm(train,d125,d125_21,c,10,0.033).
atm(train,d125,d125_22,c,10,0.532).
atm(train,d125,d125_23,h,3,0.133).
atm(train,d125,d125_24,cl,93,-0.167).
atm(train,d125,d125_25,cl,93,-0.167).
atm(train,d125,d125_26,cl,93,-0.168).
atm(train,d125,d125_27,cl,93,-0.168).
atm(train,d125,d125_28,cl,93,-0.168).
atm(train,d126,d126_1,c,10,0.15).
atm(train,d126,d126_2,c,10,0.15).
atm(train,d126,d126_3,c,10,0.15).
atm(train,d126,d126_4,c,10,0.15).
atm(train,d126,d126_5,c,10,0.15).
atm(train,d126,d126_6,c,10,0.15).
atm(train,d126,d126_7,cl,93,-0.2).
atm(train,d126,d126_8,cl,93,-0.2).
atm(train,d126,d126_9,cl,93,-0.2).
atm(train,d126,d126_10,cl,93,-0.2).
atm(train,d126,d126_11,cl,93,-0.2).
atm(train,d126,d126_12,cl,93,-0.2).
atm(train,d126,d126_13,h,3,0.05).
atm(train,d126,d126_14,h,3,0.05).
atm(train,d126,d126_15,h,3,0.05).
atm(train,d126,d126_16,h,3,0.05).
atm(train,d126,d126_17,h,3,0.05).
atm(train,d126,d126_18,h,3,0.05).
atm(train,d127,d127_1,c,22,-0.003).
atm(train,d127,d127_2,c,22,-0.003).
atm(train,d127,d127_3,c,22,-0.003).
atm(train,d127,d127_4,c,22,-0.003).
atm(train,d127,d127_5,c,22,-0.003).
atm(train,d127,d127_6,c,22,-0.003).
atm(train,d127,d127_7,cl,93,-0.073).
atm(train,d127,d127_8,cl,93,-0.073).
atm(train,d127,d127_9,cl,93,-0.073).
atm(train,d127,d127_10,cl,93,-0.073).
atm(train,d127,d127_11,cl,93,-0.073).
atm(train,d127,d127_12,n,38,0.928).
atm(train,d127,d127_13,o,40,-0.272).
atm(train,d127,d127_14,o,40,-0.273).
atm(train,d128,d128_1,c,10,-0.026).
atm(train,d128,d128_2,c,10,-0.076).
atm(train,d128,d128_3,c,10,-0.026).
atm(train,d128,d128_4,c,10,0.225).
atm(train,d128,d128_5,cl,93,-0.125).
atm(train,d128,d128_6,c,10,-0.026).
atm(train,d128,d128_7,h,3,0.124).
atm(train,d128,d128_8,c,10,-0.026).
atm(train,d128,d128_9,h,3,0.044).
atm(train,d128,d128_10,c,10,-0.026).
atm(train,d128,d128_11,c,191,-0.026).
atm(train,d128,d128_12,c,191,-0.026).
atm(train,d128,d128_13,o,50,-0.295).
atm(train,d128,d128_14,h,3,0.124).
atm(train,d128,d128_15,h,3,0.125).
atm(train,d128,d128_16,c,10,-0.076).
atm(train,d128,d128_17,h,3,0.125).
atm(train,d128,d128_18,c,10,0.475).
atm(train,d128,d128_19,cl,93,-0.125).
atm(train,d128,d128_20,cl,93,-0.125).
atm(train,d128,d128_21,cl,93,-0.125).
atm(train,d128,d128_22,cl,93,-0.125).
atm(train,d128,d128_23,c,10,-0.076).
atm(train,d128,d128_24,h,3,0.044).
atm(train,d128,d128_25,h,3,0.125).
atm(train,d128,d128_26,h,3,0.044).
atm(train,d128,d128_27,cl,93,-0.125).
atm(train,d129,d129_1,cl,93,-0.224).
atm(train,d129,d129_2,c,16,0.376).
atm(train,d129,d129_3,cl,93,-0.224).
atm(train,d129,d129_4,c,16,-0.224).
atm(train,d129,d129_5,c,10,0.067).
atm(train,d129,d129_6,h,3,0.027).
atm(train,d129,d129_7,h,3,0.027).
atm(train,d129,d129_8,h,3,0.027).
atm(train,d129,d129_9,c,10,0.067).
atm(train,d129,d129_10,h,3,0.027).
atm(train,d129,d129_11,h,3,0.027).
atm(train,d129,d129_12,h,3,0.027).
atm(train,d13,d13_1,c,22,-0.133).
atm(train,d13,d13_2,c,22,0.196).
atm(train,d13,d13_3,c,22,-0.134).
atm(train,d13,d13_4,c,22,-0.133).
atm(train,d13,d13_5,c,22,0.196).
atm(train,d13,d13_6,c,22,-0.133).
atm(train,d13,d13_7,h,3,0.127).
atm(train,d13,d13_8,h,3,0.127).
atm(train,d13,d13_9,h,3,0.127).
atm(train,d13,d13_10,n,32,-0.404).
atm(train,d13,d13_11,n,38,0.796).
atm(train,d13,d13_12,n,32,-0.404).
atm(train,d13,d13_13,c,10,-0.003).
atm(train,d13,d13_14,h,3,0.047).
atm(train,d13,d13_15,h,3,0.047).
atm(train,d13,d13_16,h,3,0.047).
atm(train,d13,d13_17,h,1,0.296).
atm(train,d13,d13_18,o,40,-0.404).
atm(train,d13,d13_19,o,40,-0.404).
atm(train,d13,d13_20,c,10,-0.003).
atm(train,d13,d13_21,c,10,0.166).
atm(train,d13,d13_22,h,3,0.047).
atm(train,d13,d13_23,h,3,0.047).
atm(train,d13,d13_24,o,45,-0.654).
atm(train,d13,d13_25,h,3,0.037).
atm(train,d13,d13_26,h,3,0.037).
atm(train,d13,d13_27,c,10,-0.003).
atm(train,d13,d13_28,c,10,0.166).
atm(train,d13,d13_29,h,3,0.047).
atm(train,d13,d13_30,h,3,0.047).
atm(train,d13,d13_31,o,45,-0.654).
atm(train,d13,d13_32,h,3,0.037).
atm(train,d13,d13_33,h,3,0.037).
atm(train,d13,d13_34,h,8,0.396).
atm(train,d13,d13_35,h,8,0.396).
atm(train,d130,d130_1,c,22,-0.093).
atm(train,d130,d130_2,c,22,-0.093).
atm(train,d130,d130_3,c,22,-0.093).
atm(train,d130,d130_4,c,22,0.137).
atm(train,d130,d130_5,c,22,-0.093).
atm(train,d130,d130_6,c,22,-0.093).
atm(train,d130,d130_7,h,3,0.167).
atm(train,d130,d130_8,h,3,0.167).
atm(train,d130,d130_9,h,3,0.167).
atm(train,d130,d130_10,h,3,0.167).
atm(train,d130,d130_11,c,16,-0.163).
atm(train,d130,d130_12,cl,93,-0.163).
atm(train,d130,d130_13,c,16,-0.163).
atm(train,d130,d130_14,h,3,0.137).
atm(train,d130,d130_15,c,19,0.468).
atm(train,d130,d130_16,c,19,0.468).
atm(train,d130,d130_17,n,31,-0.462).
atm(train,d130,d130_18,n,31,-0.462).
atm(train,d131,d131_1,c,22,-0.06).
atm(train,d131,d131_2,c,22,-0.06).
atm(train,d131,d131_3,c,22,-0.06).
atm(train,d131,d131_4,c,22,-0.06).
atm(train,d131,d131_5,c,22,0.25).
atm(train,d131,d131_6,c,22,-0.06).
atm(train,d131,d131_7,cl,93,-0.13).
atm(train,d131,d131_8,cl,93,-0.13).
atm(train,d131,d131_9,o,50,-0.16).
atm(train,d131,d131_10,c,10,0.16).
atm(train,d131,d131_11,h,3,0.12).
atm(train,d131,d131_12,h,3,0.12).
atm(train,d131,d131_13,h,3,0.12).
atm(train,d131,d131_14,cl,93,-0.13).
atm(train,d131,d131_15,cl,93,-0.13).
atm(train,d131,d131_16,n,38,0.87).
atm(train,d131,d131_17,o,40,-0.33).
atm(train,d131,d131_18,o,40,-0.33).
atm(train,d132,d132_1,c,22,0.247).
atm(train,d132,d132_2,n,35,-0.543).
atm(train,d132,d132_3,c,22,0.497).
atm(train,d132,d132_4,c,22,-0.132).
atm(train,d132,d132_5,c,22,-0.132).
atm(train,d132,d132_6,c,22,-0.132).
atm(train,d132,d132_7,h,3,0.128).
atm(train,d132,d132_8,h,3,0.128).
atm(train,d132,d132_9,h,3,0.128).
atm(train,d132,d132_10,h,3,0.128).
atm(train,d132,d132_11,c,22,-0.132).
atm(train,d132,d132_12,c,22,-0.132).
atm(train,d132,d132_13,c,22,-0.132).
atm(train,d132,d132_14,c,22,-0.132).
atm(train,d132,d132_15,c,22,-0.132).
atm(train,d132,d132_16,c,22,-0.132).
atm(train,d132,d132_17,h,3,0.128).
atm(train,d132,d132_18,h,3,0.128).
atm(train,d132,d132_19,h,3,0.128).
atm(train,d132,d132_20,h,3,0.128).
atm(train,d132,d132_21,c,10,-0.002).
atm(train,d132,d132_22,c,10,-0.102).
atm(train,d132,d132_23,h,3,0.098).
atm(train,d132,d132_24,c,10,-0.002).
atm(train,d132,d132_25,h,3,0.048).
atm(train,d132,d132_26,h,3,0.048).
atm(train,d132,d132_27,n,36,-0.303).
atm(train,d132,d132_28,h,3,0.048).
atm(train,d132,d132_29,h,3,0.048).
atm(train,d132,d132_30,c,10,-0.002).
atm(train,d132,d132_31,h,3,0.048).
atm(train,d132,d132_32,h,3,0.048).
atm(train,d132,d132_33,h,3,0.048).
atm(train,d132,d132_34,c,10,-0.002).
atm(train,d132,d132_35,h,3,0.048).
atm(train,d132,d132_36,h,3,0.048).
atm(train,d132,d132_37,h,3,0.048).
atm(train,d132,d132_38,cl,93,-0.202).
atm(train,d133,d133_1,c,22,-0.079).
atm(train,d133,d133_2,c,22,-0.079).
atm(train,d133,d133_3,c,22,-0.079).
atm(train,d133,d133_4,c,22,-0.079).
atm(train,d133,d133_5,c,22,0.3).
atm(train,d133,d133_6,c,22,-0.079).
atm(train,d133,d133_7,h,3,0.181).
atm(train,d133,d133_8,h,3,0.181).
atm(train,d133,d133_9,h,3,0.181).
atm(train,d133,d133_10,o,45,-0.6).
atm(train,d133,d133_11,cl,93,-0.149).
atm(train,d133,d133_12,cl,93,-0.149).
atm(train,d133,d133_13,h,8,0.45).
atm(train,d134,d134_1,c,22,-0.13).
atm(train,d134,d134_2,c,22,-0.13).
atm(train,d134,d134_3,c,22,-0.13).
atm(train,d134,d134_4,c,22,-0.13).
atm(train,d134,d134_5,c,22,-0.13).
atm(train,d134,d134_6,c,22,-0.13).
atm(train,d134,d134_7,h,3,0.13).
atm(train,d134,d134_8,h,3,0.13).
atm(train,d134,d134_9,h,3,0.13).
atm(train,d134,d134_10,h,3,0.13).
atm(train,d134,d134_11,h,3,0.13).
atm(train,d134,d134_12,h,3,0.13).
atm(train,d135,d135_1,c,22,-0.123).
atm(train,d135,d135_2,c,22,-0.122).
atm(train,d135,d135_3,c,22,0.007).
atm(train,d135,d135_4,c,22,0.007).
atm(train,d135,d135_5,c,22,-0.123).
atm(train,d135,d135_6,c,22,-0.123).
atm(train,d135,d135_7,h,3,0.138).
atm(train,d135,d135_8,h,3,0.138).
atm(train,d135,d135_9,h,3,0.138).
atm(train,d135,d135_10,h,3,0.138).
atm(train,d135,d135_11,c,14,0.708).
atm(train,d135,d135_12,c,10,-0.142).
atm(train,d135,d135_13,o,51,-0.542).
atm(train,d135,d135_14,c,10,-0.093).
atm(train,d135,d135_15,c,10,-0.093).
atm(train,d135,d135_16,c,10,-0.142).
atm(train,d135,d135_17,h,3,0.057).
atm(train,d135,d135_18,h,3,0.057).
atm(train,d135,d135_19,h,3,0.057).
atm(train,d135,d135_20,h,3,0.057).
atm(train,d135,d135_21,h,3,0.057).
atm(train,d135,d135_22,c,10,-0.093).
atm(train,d135,d135_23,h,3,0.057).
atm(train,d135,d135_24,h,3,0.057).
atm(train,d135,d135_25,c,10,-0.093).
atm(train,d135,d135_26,h,3,0.057).
atm(train,d135,d135_27,h,3,0.057).
atm(train,d135,d135_28,c,10,-0.142).
atm(train,d135,d135_29,h,3,0.057).
atm(train,d135,d135_30,h,3,0.057).
atm(train,d135,d135_31,h,3,0.057).
atm(train,d135,d135_32,h,3,0.057).
atm(train,d135,d135_33,h,3,0.057).
atm(train,d135,d135_34,o,49,-0.642).
atm(train,d135,d135_35,c,10,0.258).
atm(train,d135,d135_36,h,3,0.107).
atm(train,d135,d135_37,h,3,0.107).
atm(train,d135,d135_38,h,3,-0.023).
atm(train,d135,d135_39,c,14,0.708).
atm(train,d135,d135_40,o,49,-0.642).
atm(train,d135,d135_41,c,10,0.258).
atm(train,d135,d135_42,c,10,-0.142).
atm(train,d135,d135_43,h,3,0.107).
atm(train,d135,d135_44,h,3,0.107).
atm(train,d135,d135_45,c,10,-0.142).
atm(train,d135,d135_46,h,3,0.057).
atm(train,d135,d135_47,h,3,0.057).
atm(train,d135,d135_48,h,3,0.057).
atm(train,d135,d135_49,c,10,-0.093).
atm(train,d135,d135_50,h,3,-0.023).
atm(train,d135,d135_51,c,10,-0.093).
atm(train,d135,d135_52,h,3,0.057).
atm(train,d135,d135_53,h,3,0.057).
atm(train,d135,d135_54,c,10,-0.093).
atm(train,d135,d135_55,h,3,0.057).
atm(train,d135,d135_56,h,3,0.057).
atm(train,d135,d135_57,c,10,-0.142).
atm(train,d135,d135_58,h,3,0.057).
atm(train,d135,d135_59,h,3,0.057).
atm(train,d135,d135_60,h,3,0.057).
atm(train,d135,d135_61,h,3,0.057).
atm(train,d135,d135_62,h,3,0.057).
atm(train,d135,d135_63,o,51,-0.542).
atm(train,d136,d136_1,n,36,-0.245).
atm(train,d136,d136_2,c,10,-0.095).
atm(train,d136,d136_3,c,14,0.834).
atm(train,d136,d136_4,h,3,0.105).
atm(train,d136,d136_5,h,3,0.105).
atm(train,d136,d136_6,o,51,-0.556).
atm(train,d136,d136_7,o,45,-0.616).
atm(train,d136,d136_8,c,10,-0.095).
atm(train,d136,d136_9,c,14,0.834).
atm(train,d136,d136_10,h,3,0.105).
atm(train,d136,d136_11,h,3,0.105).
atm(train,d136,d136_12,o,51,-0.556).
atm(train,d136,d136_13,o,45,-0.616).
atm(train,d136,d136_14,c,10,-0.095).
atm(train,d136,d136_15,c,14,0.834).
atm(train,d136,d136_16,h,3,0.105).
atm(train,d136,d136_17,h,3,0.105).
atm(train,d136,d136_18,o,45,-0.616).
atm(train,d136,d136_19,o,51,-0.556).
atm(train,d136,d136_20,h,1,0.305).
atm(train,d136,d136_21,h,1,0.304).
atm(train,d136,d136_22,h,1,0.305).
atm(train,d137,d137_1,c,22,0.177).
atm(train,d137,d137_2,c,22,-0.133).
atm(train,d137,d137_3,c,26,-0.043).
atm(train,d137,d137_4,c,26,0.097).
atm(train,d137,d137_5,c,22,-0.133).
atm(train,d137,d137_6,c,22,-0.133).
atm(train,d137,d137_7,h,3,0.127).
atm(train,d137,d137_8,h,3,0.097).
atm(train,d137,d137_9,h,3,0.127).
atm(train,d137,d137_10,c,21,-0.003).
atm(train,d137,d137_11,c,10,0.047).
atm(train,d137,d137_12,n,36,-0.302).
atm(train,d137,d137_13,c,10,-0.003).
atm(train,d137,d137_14,c,10,-0.103).
atm(train,d137,d137_15,c,21,-0.033).
atm(train,d137,d137_16,h,3,0.047).
atm(train,d137,d137_17,h,3,0.047).
atm(train,d137,d137_18,h,3,0.047).
atm(train,d137,d137_19,h,3,0.047).
atm(train,d137,d137_20,n,34,-0.362).
atm(train,d137,d137_21,o,50,-0.232).
atm(train,d137,d137_22,c,10,0.087).
atm(train,d137,d137_23,h,3,0.047).
atm(train,d137,d137_24,h,3,0.047).
atm(train,d137,d137_25,h,3,0.047).
atm(train,d137,d137_26,c,10,-0.103).
atm(train,d137,d137_27,c,10,-0.153).
atm(train,d137,d137_28,c,10,-0.153).
atm(train,d137,d137_29,c,10,-0.003).
atm(train,d137,d137_30,h,3,0.047).
atm(train,d137,d137_31,h,3,0.047).
atm(train,d137,d137_32,h,3,0.047).
atm(train,d137,d137_33,h,3,0.047).
atm(train,d137,d137_34,h,3,0.047).
atm(train,d137,d137_35,c,10,-0.103).
atm(train,d137,d137_36,c,10,0.062).
atm(train,d137,d137_37,c,10,0.248).
atm(train,d137,d137_38,c,10,-0.103).
atm(train,d137,d137_39,h,3,-0.033).
atm(train,d137,d137_40,h,3,-0.033).
atm(train,d137,d137_41,h,3,0.047).
atm(train,d137,d137_42,h,3,0.047).
atm(train,d137,d137_43,c,14,0.698).
atm(train,d137,d137_44,h,3,0.077).
atm(train,d137,d137_45,o,49,-0.652).
atm(train,d137,d137_46,c,10,0.238).
atm(train,d137,d137_47,h,3,0.097).
atm(train,d137,d137_48,h,3,0.097).
atm(train,d137,d137_49,h,3,0.097).
atm(train,d137,d137_50,o,51,-0.552).
atm(train,d137,d137_51,o,50,-0.372).
atm(train,d137,d137_52,h,3,0.047).
atm(train,d137,d137_53,c,10,0.017).
atm(train,d137,d137_54,h,3,0.057).
atm(train,d137,d137_55,h,3,0.057).
atm(train,d137,d137_56,h,3,0.057).
atm(train,d137,d137_57,o,49,-0.652).
atm(train,d137,d137_58,h,3,0.177).
atm(train,d137,d137_59,c,14,0.698).
atm(train,d137,d137_60,c,22,-0.003).
atm(train,d137,d137_61,c,22,-0.133).
atm(train,d137,d137_62,c,22,0.178).
atm(train,d137,d137_63,c,22,0.177).
atm(train,d137,d137_64,c,22,0.177).
atm(train,d137,d137_65,c,22,-0.133).
atm(train,d137,d137_66,h,3,0.127).
atm(train,d137,d137_67,h,3,0.127).
atm(train,d137,d137_68,o,51,-0.552).
atm(train,d137,d137_69,o,50,-0.232).
atm(train,d137,d137_70,c,10,0.087).
atm(train,d137,d137_71,h,3,0.047).
atm(train,d137,d137_72,h,3,0.047).
atm(train,d137,d137_73,h,3,0.047).
atm(train,d137,d137_74,o,50,-0.232).
atm(train,d137,d137_75,c,10,0.087).
atm(train,d137,d137_76,h,3,0.047).
atm(train,d137,d137_77,h,3,0.047).
atm(train,d137,d137_78,h,3,0.047).
atm(train,d137,d137_79,o,50,-0.232).
atm(train,d137,d137_80,c,10,0.087).
atm(train,d137,d137_81,h,3,0.047).
atm(train,d137,d137_82,h,3,0.047).
atm(train,d137,d137_83,h,3,0.047).
atm(train,d137,d137_84,h,1,0.298).
atm(train,d138,d138_1,c,22,-0.13).
atm(train,d138,d138_2,c,22,0.25).
atm(train,d138,d138_3,c,22,-0.13).
atm(train,d138,d138_4,c,22,-0.13).
atm(train,d138,d138_5,c,22,0.25).
atm(train,d138,d138_6,c,22,-0.13).
atm(train,d138,d138_7,h,3,0.13).
atm(train,d138,d138_8,h,3,0.13).
atm(train,d138,d138_9,h,3,0.13).
atm(train,d138,d138_10,h,3,0.13).
atm(train,d138,d138_11,o,45,-0.65).
atm(train,d138,d138_12,o,45,-0.65).
atm(train,d138,d138_13,h,8,0.4).
atm(train,d138,d138_14,h,8,0.4).
atm(train,d139,d139_1,c,10,-0.093).
atm(train,d139,d139_2,c,10,-0.093).
atm(train,d139,d139_3,c,10,-0.093).
atm(train,d139,d139_4,h,3,0.057).
atm(train,d139,d139_5,h,3,0.057).
atm(train,d139,d139_6,c,10,-0.094).
atm(train,d139,d139_7,h,3,0.057).
atm(train,d139,d139_8,h,3,0.057).
atm(train,d139,d139_9,c,10,-0.094).
atm(train,d139,d139_10,h,3,0.057).
atm(train,d139,d139_11,h,3,0.057).
atm(train,d139,d139_12,c,10,-0.093).
atm(train,d139,d139_13,h,3,0.057).
atm(train,d139,d139_14,h,3,0.057).
atm(train,d139,d139_15,c,10,-0.093).
atm(train,d139,d139_16,h,3,0.057).
atm(train,d139,d139_17,h,3,0.057).
atm(train,d139,d139_18,c,10,-0.093).
atm(train,d139,d139_19,h,3,0.057).
atm(train,d139,d139_20,h,3,0.057).
atm(train,d139,d139_21,c,10,-0.093).
atm(train,d139,d139_22,h,3,0.057).
atm(train,d139,d139_23,h,3,0.057).
atm(train,d139,d139_24,c,10,-0.093).
atm(train,d139,d139_25,h,3,0.057).
atm(train,d139,d139_26,h,3,0.057).
atm(train,d139,d139_27,c,14,0.786).
atm(train,d139,d139_28,h,3,0.057).
atm(train,d139,d139_29,h,3,0.057).
atm(train,d139,d139_30,o,45,-0.664).
atm(train,d139,d139_31,o,51,-0.604).
atm(train,d139,d139_32,h,1,0.256).
atm(train,d139,d139_33,n,36,-0.294).
atm(train,d139,d139_34,h,3,0.057).
atm(train,d139,d139_35,h,3,0.057).
atm(train,d139,d139_36,h,1,0.156).
atm(train,d139,d139_37,h,1,0.156).
atm(train,d14,d14_1,c,22,-0.137).
atm(train,d14,d14_2,c,22,-0.138).
atm(train,d14,d14_3,c,22,0.193).
atm(train,d14,d14_4,c,22,-0.138).
atm(train,d14,d14_5,c,22,-0.138).
atm(train,d14,d14_6,c,22,-0.138).
atm(train,d14,d14_7,h,3,0.122).
atm(train,d14,d14_8,h,3,0.122).
atm(train,d14,d14_9,h,3,0.122).
atm(train,d14,d14_10,h,3,0.122).
atm(train,d14,d14_11,h,3,0.122).
atm(train,d14,d14_12,n,32,-0.407).
atm(train,d14,d14_13,n,32,-0.407).
atm(train,d14,d14_14,c,22,0.193).
atm(train,d14,d14_15,c,22,-0.138).
atm(train,d14,d14_16,c,22,-0.138).
atm(train,d14,d14_17,c,22,-0.137).
atm(train,d14,d14_18,c,22,-0.138).
atm(train,d14,d14_19,c,22,-0.138).
atm(train,d14,d14_20,h,3,0.122).
atm(train,d14,d14_21,h,3,0.122).
atm(train,d14,d14_22,h,3,0.122).
atm(train,d14,d14_23,h,3,0.122).
atm(train,d14,d14_24,h,3,0.122).
atm(train,d14,d14_25,h,1,0.293).
atm(train,d14,d14_26,h,1,0.293).
atm(train,d140,d140_1,c,22,0.25).
atm(train,d140,d140_2,c,22,-0.131).
atm(train,d140,d140_3,c,22,0.099).
atm(train,d140,d140_4,c,22,-0.001).
atm(train,d140,d140_5,c,22,0.25).
atm(train,d140,d140_6,c,22,-0.131).
atm(train,d140,d140_7,h,3,0.129).
atm(train,d140,d140_8,h,3,0.129).
atm(train,d140,d140_9,c,16,-0.2).
atm(train,d140,d140_10,c,16,-0.2).
atm(train,d140,d140_11,c,10,0.099).
atm(train,d140,d140_12,c,10,-0.101).
atm(train,d140,d140_13,h,3,0.049).
atm(train,d140,d140_14,h,3,0.049).
atm(train,d140,d140_15,c,10,-0.101).
atm(train,d140,d140_16,h,3,0.049).
atm(train,d140,d140_17,h,3,0.049).
atm(train,d140,d140_18,c,14,0.58).
atm(train,d140,d140_19,h,3,0.049).
atm(train,d140,d140_20,h,3,0.049).
atm(train,d140,d140_21,o,42,-0.58).
atm(train,d140,d140_22,c,10,-0.101).
atm(train,d140,d140_23,c,10,-0.101).
atm(train,d140,d140_24,h,3,0.049).
atm(train,d140,d140_25,h,3,0.049).
atm(train,d140,d140_26,c,10,-0.101).
atm(train,d140,d140_27,h,3,0.049).
atm(train,d140,d140_28,h,3,0.049).
atm(train,d140,d140_29,c,10,0.25).
atm(train,d140,d140_30,h,3,0.049).
atm(train,d140,d140_31,h,3,0.049).
atm(train,d140,d140_32,c,10,-0.151).
atm(train,d140,d140_33,h,3,0.049).
atm(train,d140,d140_34,h,3,0.049).
atm(train,d140,d140_35,h,3,0.049).
atm(train,d140,d140_36,o,49,-0.65).
atm(train,d140,d140_37,h,3,0.18).
atm(train,d140,d140_38,c,14,0.7).
atm(train,d140,d140_39,o,45,-0.65).
atm(train,d140,d140_40,o,45,-0.65).
atm(train,d140,d140_41,h,8,0.4).
atm(train,d140,d140_42,h,8,0.4).
atm(train,d140,d140_43,o,51,-0.55).
atm(train,d140,d140_44,h,3,0.099).
atm(train,d140,d140_45,h,3,0.099).
atm(train,d141,d141_1,c,22,-0.137).
atm(train,d141,d141_2,c,22,-0.137).
atm(train,d141,d141_3,c,22,-0.137).
atm(train,d141,d141_4,c,22,-0.137).
atm(train,d141,d141_5,c,22,-0.137).
atm(train,d141,d141_6,c,22,-0.137).
atm(train,d141,d141_7,h,3,0.123).
atm(train,d141,d141_8,h,3,0.123).
atm(train,d141,d141_9,h,3,0.123).
atm(train,d141,d141_10,h,3,0.123).
atm(train,d141,d141_11,h,3,0.123).
atm(train,d141,d141_12,c,10,0.223).
atm(train,d141,d141_13,o,49,-0.458).
atm(train,d141,d141_14,h,3,0.093).
atm(train,d141,d141_15,h,3,0.093).
atm(train,d141,d141_16,c,14,0.692).
atm(train,d141,d141_17,c,10,-0.007).
atm(train,d141,d141_18,h,3,0.043).
atm(train,d141,d141_19,h,3,0.043).
atm(train,d141,d141_20,h,3,0.043).
atm(train,d141,d141_21,o,51,-0.558).
atm(train,d142,d142_1,c,22,-0.075).
atm(train,d142,d142_2,c,22,-0.075).
atm(train,d142,d142_3,c,26,-0.045).
atm(train,d142,d142_4,c,26,0.155).
atm(train,d142,d142_5,c,22,-0.075).
atm(train,d142,d142_6,c,22,-0.075).
atm(train,d142,d142_7,h,3,0.185).
atm(train,d142,d142_8,h,3,0.184).
atm(train,d142,d142_9,h,3,0.185).
atm(train,d142,d142_10,h,3,0.184).
atm(train,d142,d142_11,n,34,-0.486).
atm(train,d142,d142_12,c,21,0.055).
atm(train,d142,d142_13,s,72,-0.175).
atm(train,d142,d142_14,s,70,-0.246).
atm(train,d142,d142_15,h,1,0.304).
atm(train,d143,d143_1,c,22,-0.125).
atm(train,d143,d143_2,c,22,-0.125).
atm(train,d143,d143_3,c,22,0.005).
atm(train,d143,d143_4,c,22,0.776).
atm(train,d143,d143_5,n,35,-0.764).
atm(train,d143,d143_6,c,22,0.505).
atm(train,d143,d143_7,c,14,0.555).
atm(train,d143,d143_8,c,16,-0.195).
atm(train,d143,d143_9,c,16,-0.195).
atm(train,d143,d143_10,n,32,-0.395).
atm(train,d143,d143_11,h,3,0.135).
atm(train,d143,d143_12,h,3,0.135).
atm(train,d143,d143_13,o,40,-0.545).
atm(train,d143,d143_14,c,10,0.005).
atm(train,d143,d143_15,c,10,-0.095).
atm(train,d143,d143_16,h,3,0.055).
atm(train,d143,d143_17,h,3,0.055).
atm(train,d143,d143_18,h,3,0.055).
atm(train,d143,d143_19,h,3,0.055).
atm(train,d143,d143_20,h,3,0.055).
atm(train,d143,d143_21,c,14,0.786).
atm(train,d143,d143_22,o,45,-0.664).
atm(train,d143,d143_23,o,51,-0.604).
atm(train,d143,d143_24,h,1,0.255).
atm(train,d143,d143_25,c,10,0.005).
atm(train,d143,d143_26,h,3,0.055).
atm(train,d143,d143_27,h,3,0.055).
atm(train,d143,d143_28,h,3,0.055).
atm(train,d143,d143_29,h,3,0.105).
atm(train,d144,d144_1,c,14,0.706).
atm(train,d144,d144_2,o,49,-0.644).
atm(train,d144,d144_3,c,10,0.256).
atm(train,d144,d144_4,c,10,-0.144).
atm(train,d144,d144_5,h,3,0.107).
atm(train,d144,d144_6,h,3,0.106).
atm(train,d144,d144_7,c,10,-0.093).
atm(train,d144,d144_8,c,10,-0.144).
atm(train,d144,d144_9,h,3,0.057).
atm(train,d144,d144_10,h,3,0.057).
atm(train,d144,d144_11,h,3,0.057).
atm(train,d144,d144_12,h,3,0.057).
atm(train,d144,d144_13,h,3,0.057).
atm(train,d144,d144_14,c,10,-0.093).
atm(train,d144,d144_15,h,3,-0.023).
atm(train,d144,d144_16,c,10,-0.093).
atm(train,d144,d144_17,h,3,0.057).
atm(train,d144,d144_18,h,3,0.057).
atm(train,d144,d144_19,c,10,-0.093).
atm(train,d144,d144_20,h,3,0.057).
atm(train,d144,d144_21,h,3,0.057).
atm(train,d144,d144_22,c,10,-0.144).
atm(train,d144,d144_23,h,3,0.057).
atm(train,d144,d144_24,h,3,0.057).
atm(train,d144,d144_25,h,3,0.057).
atm(train,d144,d144_26,h,3,0.057).
atm(train,d144,d144_27,h,3,0.057).
atm(train,d144,d144_28,o,51,-0.544).
atm(train,d144,d144_29,c,10,-0.093).
atm(train,d144,d144_30,c,10,-0.093).
atm(train,d144,d144_31,c,14,0.706).
atm(train,d144,d144_32,o,51,-0.544).
atm(train,d144,d144_33,o,49,-0.644).
atm(train,d144,d144_34,c,10,0.256).
atm(train,d144,d144_35,c,10,-0.144).
atm(train,d144,d144_36,h,3,0.107).
atm(train,d144,d144_37,h,3,0.106).
atm(train,d144,d144_38,c,10,-0.093).
atm(train,d144,d144_39,c,10,-0.144).
atm(train,d144,d144_40,h,3,0.057).
atm(train,d144,d144_41,h,3,0.057).
atm(train,d144,d144_42,h,3,0.057).
atm(train,d144,d144_43,h,3,0.057).
atm(train,d144,d144_44,h,3,0.057).
atm(train,d144,d144_45,c,10,-0.093).
atm(train,d144,d144_46,h,3,-0.023).
atm(train,d144,d144_47,c,10,-0.093).
atm(train,d144,d144_48,h,3,0.057).
atm(train,d144,d144_49,h,3,0.057).
atm(train,d144,d144_50,c,10,-0.093).
atm(train,d144,d144_51,h,3,0.057).
atm(train,d144,d144_52,h,3,0.057).
atm(train,d144,d144_53,c,10,-0.144).
atm(train,d144,d144_54,h,3,0.057).
atm(train,d144,d144_55,h,3,0.057).
atm(train,d144,d144_56,h,3,0.057).
atm(train,d144,d144_57,h,3,0.057).
atm(train,d144,d144_58,h,3,0.057).
atm(train,d144,d144_59,c,10,-0.093).
atm(train,d144,d144_60,h,3,0.057).
atm(train,d144,d144_61,h,3,0.057).
atm(train,d144,d144_62,c,10,-0.093).
atm(train,d144,d144_63,h,3,0.057).
atm(train,d144,d144_64,h,3,0.057).
atm(train,d144,d144_65,h,3,0.057).
atm(train,d144,d144_66,h,3,0.057).
atm(train,d144,d144_67,h,3,0.057).
atm(train,d144,d144_68,h,3,0.057).
atm(train,d145,d145_1,c,22,-0.124).
atm(train,d145,d145_2,c,22,-0.124).
atm(train,d145,d145_3,c,22,0.006).
atm(train,d145,d145_4,c,22,0.006).
atm(train,d145,d145_5,c,22,-0.124).
atm(train,d145,d145_6,c,22,-0.124).
atm(train,d145,d145_7,h,3,0.136).
atm(train,d145,d145_8,h,3,0.136).
atm(train,d145,d145_9,h,3,0.136).
atm(train,d145,d145_10,h,3,0.136).
atm(train,d145,d145_11,c,14,0.705).
atm(train,d145,d145_12,c,10,0.255).
atm(train,d145,d145_13,c,10,-0.094).
atm(train,d145,d145_14,c,10,-0.094).
atm(train,d145,d145_15,h,3,0.056).
atm(train,d145,d145_16,h,3,0.056).
atm(train,d145,d145_17,c,10,-0.144).
atm(train,d145,d145_18,h,3,0.056).
atm(train,d145,d145_19,h,3,0.056).
atm(train,d145,d145_20,h,3,0.056).
atm(train,d145,d145_21,h,3,0.056).
atm(train,d145,d145_22,h,3,0.056).
atm(train,d145,d145_23,o,51,-0.545).
atm(train,d145,d145_24,c,14,0.705).
atm(train,d145,d145_25,o,49,-0.645).
atm(train,d145,d145_26,c,10,0.235).
atm(train,d145,d145_27,o,51,-0.545).
atm(train,d145,d145_28,o,49,-0.645).
atm(train,d145,d145_29,h,3,0.106).
atm(train,d145,d145_30,h,3,0.106).
atm(train,d145,d145_31,c,22,-0.124).
atm(train,d145,d145_32,h,3,0.106).
atm(train,d145,d145_33,h,3,0.106).
atm(train,d145,d145_34,c,22,-0.124).
atm(train,d145,d145_35,c,22,-0.124).
atm(train,d145,d145_36,c,22,-0.124).
atm(train,d145,d145_37,c,22,-0.124).
atm(train,d145,d145_38,c,22,-0.124).
atm(train,d145,d145_39,h,3,0.136).
atm(train,d145,d145_40,h,3,0.136).
atm(train,d145,d145_41,h,3,0.136).
atm(train,d145,d145_42,h,3,0.136).
atm(train,d145,d145_43,h,3,0.136).
atm(train,d146,d146_1,c,22,0.005).
atm(train,d146,d146_2,c,22,-0.125).
atm(train,d146,d146_3,c,22,0.186).
atm(train,d146,d146_4,c,22,0.186).
atm(train,d146,d146_5,c,22,-0.125).
atm(train,d146,d146_6,c,22,-0.125).
atm(train,d146,d146_7,h,3,0.136).
atm(train,d146,d146_8,h,3,0.136).
atm(train,d146,d146_9,h,3,0.136).
atm(train,d146,d146_10,o,50,-0.224).
atm(train,d146,d146_11,c,10,0.095).
atm(train,d146,d146_12,o,50,-0.224).
atm(train,d146,d146_13,h,3,0.055).
atm(train,d146,d146_14,h,3,0.055).
atm(train,d146,d146_15,c,10,-0.095).
atm(train,d146,d146_16,c,10,-0.095).
atm(train,d146,d146_17,h,3,0.055).
atm(train,d146,d146_18,h,3,0.055).
atm(train,d146,d146_19,c,10,-0.144).
atm(train,d146,d146_20,h,3,0.055).
atm(train,d146,d146_21,h,3,0.055).
atm(train,d146,d146_22,h,3,0.055).
atm(train,d146,d146_23,s,76,-0.045).
atm(train,d146,d146_24,h,3,0.055).
atm(train,d146,d146_25,c,10,-0.095).
atm(train,d146,d146_26,o,40,-0.244).
atm(train,d146,d146_27,c,10,-0.095).
atm(train,d146,d146_28,h,3,0.055).
atm(train,d146,d146_29,h,3,0.055).
atm(train,d146,d146_30,c,10,-0.095).
atm(train,d146,d146_31,h,3,0.055).
atm(train,d146,d146_32,h,3,0.055).
atm(train,d146,d146_33,c,10,-0.095).
atm(train,d146,d146_34,h,3,0.055).
atm(train,d146,d146_35,h,3,0.055).
atm(train,d146,d146_36,c,10,-0.095).
atm(train,d146,d146_37,h,3,0.055).
atm(train,d146,d146_38,h,3,0.055).
atm(train,d146,d146_39,c,10,-0.095).
atm(train,d146,d146_40,h,3,0.055).
atm(train,d146,d146_41,h,3,0.055).
atm(train,d146,d146_42,c,10,-0.095).
atm(train,d146,d146_43,h,3,0.055).
atm(train,d146,d146_44,h,3,0.055).
atm(train,d146,d146_45,c,10,-0.144).
atm(train,d146,d146_46,h,3,0.055).
atm(train,d146,d146_47,h,3,0.055).
atm(train,d146,d146_48,h,3,0.055).
atm(train,d146,d146_49,h,3,0.055).
atm(train,d146,d146_50,h,3,0.055).
atm(train,d147,d147_1,p,62,1.133).
atm(train,d147,d147_2,o,40,-0.527).
atm(train,d147,d147_3,o,49,-0.647).
atm(train,d147,d147_4,c,10,-0.048).
atm(train,d147,d147_5,c,10,-0.127).
atm(train,d147,d147_6,h,3,0.093).
atm(train,d147,d147_7,h,3,0.093).
atm(train,d147,d147_8,c,10,-0.078).
atm(train,d147,d147_9,c,10,-0.078).
atm(train,d147,d147_10,h,3,0.072).
atm(train,d147,d147_11,h,3,0.072).
atm(train,d147,d147_12,c,10,-0.078).
atm(train,d147,d147_13,h,3,-0.008).
atm(train,d147,d147_14,c,10,-0.127).
atm(train,d147,d147_15,h,3,0.072).
atm(train,d147,d147_16,h,3,0.072).
atm(train,d147,d147_17,h,3,0.072).
atm(train,d147,d147_18,h,3,0.072).
atm(train,d147,d147_19,h,3,0.072).
atm(train,d147,d147_20,c,10,-0.078).
atm(train,d147,d147_21,h,3,0.072).
atm(train,d147,d147_22,h,3,0.072).
atm(train,d147,d147_23,c,10,-0.127).
atm(train,d147,d147_24,h,3,0.072).
atm(train,d147,d147_25,h,3,0.072).
atm(train,d147,d147_26,h,3,0.072).
atm(train,d147,d147_27,h,3,0.072).
atm(train,d147,d147_28,h,3,0.072).
atm(train,d147,d147_29,o,49,-0.647).
atm(train,d147,d147_30,c,10,-0.048).
atm(train,d147,d147_31,o,49,-0.647).
atm(train,d147,d147_32,c,10,-0.048).
atm(train,d147,d147_33,c,10,-0.127).
atm(train,d147,d147_34,h,3,0.093).
atm(train,d147,d147_35,h,3,0.093).
atm(train,d147,d147_36,c,10,-0.078).
atm(train,d147,d147_37,c,10,-0.078).
atm(train,d147,d147_38,h,3,-0.008).
atm(train,d147,d147_39,c,10,-0.127).
atm(train,d147,d147_40,h,3,0.072).
atm(train,d147,d147_41,h,3,0.072).
atm(train,d147,d147_42,h,3,0.072).
atm(train,d147,d147_43,h,3,0.072).
atm(train,d147,d147_44,h,3,0.072).
atm(train,d147,d147_45,c,10,-0.078).
atm(train,d147,d147_46,h,3,0.072).
atm(train,d147,d147_47,h,3,0.072).
atm(train,d147,d147_48,c,10,-0.078).
atm(train,d147,d147_49,h,3,0.072).
atm(train,d147,d147_50,h,3,0.072).
atm(train,d147,d147_51,c,10,-0.127).
atm(train,d147,d147_52,h,3,0.072).
atm(train,d147,d147_53,h,3,0.072).
atm(train,d147,d147_54,h,3,0.072).
atm(train,d147,d147_55,h,3,0.072).
atm(train,d147,d147_56,h,3,0.072).
atm(train,d147,d147_57,c,10,-0.127).
atm(train,d147,d147_58,h,3,0.093).
atm(train,d147,d147_59,h,3,0.093).
atm(train,d147,d147_60,c,10,-0.078).
atm(train,d147,d147_61,c,10,-0.078).
atm(train,d147,d147_62,h,3,-0.008).
atm(train,d147,d147_63,c,10,-0.127).
atm(train,d147,d147_64,h,3,0.072).
atm(train,d147,d147_65,h,3,0.072).
atm(train,d147,d147_66,h,3,0.072).
atm(train,d147,d147_67,h,3,0.072).
atm(train,d147,d147_68,h,3,0.072).
atm(train,d147,d147_69,c,10,-0.078).
atm(train,d147,d147_70,h,3,0.072).
atm(train,d147,d147_71,h,3,0.072).
atm(train,d147,d147_72,c,10,-0.078).
atm(train,d147,d147_73,h,3,0.072).
atm(train,d147,d147_74,h,3,0.072).
atm(train,d147,d147_75,c,10,-0.127).
atm(train,d147,d147_76,h,3,0.072).
atm(train,d147,d147_77,h,3,0.072).
atm(train,d147,d147_78,h,3,0.072).
atm(train,d147,d147_79,h,3,0.072).
atm(train,d147,d147_80,h,3,0.072).
atm(train,d148,d148_1,c,22,-0.115).
atm(train,d148,d148_2,c,22,-0.115).
atm(train,d148,d148_3,c,22,-0.115).
atm(train,d148,d148_4,c,22,-0.115).
atm(train,d148,d148_5,c,22,-0.115).
atm(train,d148,d148_6,c,22,-0.115).
atm(train,d148,d148_7,h,3,0.145).
atm(train,d148,d148_8,h,3,0.145).
atm(train,d148,d148_9,h,3,0.144).
atm(train,d148,d148_10,h,3,0.144).
atm(train,d148,d148_11,h,3,0.144).
atm(train,d148,d148_12,c,10,0.015).
atm(train,d148,d148_13,c,10,-0.085).
atm(train,d148,d148_14,h,3,0.065).
atm(train,d148,d148_15,h,3,0.065).
atm(train,d148,d148_16,h,3,0.065).
atm(train,d148,d148_17,o,45,-0.636).
atm(train,d148,d148_18,h,3,0.065).
atm(train,d148,d148_19,h,8,0.414).
atm(train,d149,d149_1,c,22,-0.125).
atm(train,d149,d149_2,c,22,-0.125).
atm(train,d149,d149_3,c,22,-0.125).
atm(train,d149,d149_4,c,22,-0.125).
atm(train,d149,d149_5,c,22,-0.125).
atm(train,d149,d149_6,c,22,0.255).
atm(train,d149,d149_7,h,3,0.135).
atm(train,d149,d149_8,h,3,0.135).
atm(train,d149,d149_9,h,3,0.135).
atm(train,d149,d149_10,h,3,0.135).
atm(train,d149,d149_11,o,45,-0.646).
atm(train,d149,d149_12,c,10,0.005).
atm(train,d149,d149_13,c,22,-0.125).
atm(train,d149,d149_14,c,22,-0.125).
atm(train,d149,d149_15,c,22,-0.125).
atm(train,d149,d149_16,c,22,0.254).
atm(train,d149,d149_17,c,22,-0.125).
atm(train,d149,d149_18,c,22,-0.125).
atm(train,d149,d149_19,h,3,0.135).
atm(train,d149,d149_20,h,3,0.135).
atm(train,d149,d149_21,h,3,0.135).
atm(train,d149,d149_22,h,3,0.135).
atm(train,d149,d149_23,c,10,-0.095).
atm(train,d149,d149_24,h,3,0.055).
atm(train,d149,d149_25,h,3,0.055).
atm(train,d149,d149_26,h,3,0.055).
atm(train,d149,d149_27,c,10,-0.095).
atm(train,d149,d149_28,h,3,0.055).
atm(train,d149,d149_29,h,3,0.055).
atm(train,d149,d149_30,h,3,0.055).
atm(train,d149,d149_31,o,45,-0.646).
atm(train,d149,d149_32,h,8,0.404).
atm(train,d149,d149_33,h,8,0.404).
atm(train,d15,d15_1,c,22,-0.13).
atm(train,d15,d15_2,c,22,-0.13).
atm(train,d15,d15_3,c,22,-0.13).
atm(train,d15,d15_4,c,22,-0.13).
atm(train,d15,d15_5,c,22,-0.13).
atm(train,d15,d15_6,c,22,0.2).
atm(train,d15,d15_7,h,3,0.13).
atm(train,d15,d15_8,h,3,0.13).
atm(train,d15,d15_9,h,3,0.13).
atm(train,d15,d15_10,h,3,0.13).
atm(train,d15,d15_11,c,10,0.0).
atm(train,d15,d15_12,c,22,-0.13).
atm(train,d15,d15_13,h,3,0.05).
atm(train,d15,d15_14,h,3,0.05).
atm(train,d15,d15_15,c,22,-0.13).
atm(train,d15,d15_16,c,22,-0.13).
atm(train,d15,d15_17,c,22,0.2).
atm(train,d15,d15_18,c,22,-0.13).
atm(train,d15,d15_19,c,22,-0.13).
atm(train,d15,d15_20,h,3,0.13).
atm(train,d15,d15_21,h,3,0.13).
atm(train,d15,d15_22,h,3,0.13).
atm(train,d15,d15_23,h,3,0.13).
atm(train,d15,d15_24,n,32,-0.78).
atm(train,d15,d15_25,n,32,-0.78).
atm(train,d15,d15_26,h,1,0.33).
atm(train,d15,d15_27,h,1,0.33).
atm(train,d15,d15_28,h,1,0.33).
atm(train,d15,d15_29,h,1,0.33).
atm(train,d150,d150_1,c,22,-0.133).
atm(train,d150,d150_2,c,22,-0.003).
atm(train,d150,d150_3,c,22,-0.133).
atm(train,d150,d150_4,c,22,-0.133).
atm(train,d150,d150_5,c,22,-0.003).
atm(train,d150,d150_6,c,22,-0.133).
atm(train,d150,d150_7,h,3,0.127).
atm(train,d150,d150_8,h,3,0.127).
atm(train,d150,d150_9,h,3,0.127).
atm(train,d150,d150_10,h,3,0.127).
atm(train,d150,d150_11,c,14,0.696).
atm(train,d150,d150_12,o,49,-0.654).
atm(train,d150,d150_13,c,10,0.236).
atm(train,d150,d150_14,h,3,0.097).
atm(train,d150,d150_15,h,3,0.097).
atm(train,d150,d150_16,h,3,0.097).
atm(train,d150,d150_17,c,14,0.696).
atm(train,d150,d150_18,o,49,-0.654).
atm(train,d150,d150_19,c,10,0.236).
atm(train,d150,d150_20,h,3,0.097).
atm(train,d150,d150_21,h,3,0.097).
atm(train,d150,d150_22,h,3,0.097).
atm(train,d150,d150_23,o,51,-0.554).
atm(train,d150,d150_24,o,51,-0.554).
atm(train,d151,d151_1,c,22,0.252).
atm(train,d151,d151_2,c,22,0.252).
atm(train,d151,d151_3,c,22,0.252).
atm(train,d151,d151_4,c,22,-0.128).
atm(train,d151,d151_5,c,22,0.002).
atm(train,d151,d151_6,c,22,-0.128).
atm(train,d151,d151_7,h,3,0.132).
atm(train,d151,d151_8,h,3,0.132).
atm(train,d151,d151_9,o,45,-0.649).
atm(train,d151,d151_10,o,45,-0.648).
atm(train,d151,d151_11,o,45,-0.649).
atm(train,d151,d151_12,c,14,0.701).
atm(train,d151,d151_13,o,49,-0.649).
atm(train,d151,d151_14,c,10,0.252).
atm(train,d151,d151_15,o,51,-0.548).
atm(train,d151,d151_16,h,8,0.402).
atm(train,d151,d151_17,h,8,0.402).
atm(train,d151,d151_18,h,8,0.402).
atm(train,d151,d151_19,c,10,-0.098).
atm(train,d151,d151_20,h,3,0.102).
atm(train,d151,d151_21,h,3,0.102).
atm(train,d151,d151_22,c,10,-0.148).
atm(train,d151,d151_23,h,3,0.052).
atm(train,d151,d151_24,h,3,0.052).
atm(train,d151,d151_25,h,3,0.052).
atm(train,d151,d151_26,h,3,0.052).
atm(train,d151,d151_27,h,3,0.052).
atm(train,d152,d152_1,c,22,-0.114).
atm(train,d152,d152_2,c,22,-0.114).
atm(train,d152,d152_3,c,22,-0.114).
atm(train,d152,d152_4,c,22,-0.114).
atm(train,d152,d152_5,c,22,-0.114).
atm(train,d152,d152_6,c,22,-0.114).
atm(train,d152,d152_7,h,3,0.146).
atm(train,d152,d152_8,h,3,0.146).
atm(train,d152,d152_9,h,3,0.146).
atm(train,d152,d152_10,h,3,0.146).
atm(train,d152,d152_11,h,3,0.146).
atm(train,d152,d152_12,c,10,0.016).
atm(train,d152,d152_13,n,36,-0.283).
atm(train,d152,d152_14,c,14,0.617).
atm(train,d152,d152_15,h,3,0.066).
atm(train,d152,d152_16,o,40,-0.533).
atm(train,d152,d152_17,n,32,-0.383).
atm(train,d152,d152_18,c,193,-0.084).
atm(train,d152,d152_19,c,192,0.597).
atm(train,d152,d152_20,n,33,-0.233).
atm(train,d152,d152_21,c,193,-0.084).
atm(train,d152,d152_22,h,3,0.066).
atm(train,d152,d152_23,o,40,-0.533).
atm(train,d152,d152_24,s,74,-0.084).
atm(train,d152,d152_25,h,3,0.066).
atm(train,d152,d152_26,c,10,-0.084).
atm(train,d152,d152_27,c,10,0.116).
atm(train,d152,d152_28,c,14,0.797).
atm(train,d152,d152_29,h,3,0.066).
atm(train,d152,d152_30,o,45,-0.653).
atm(train,d152,d152_31,o,51,-0.593).
atm(train,d152,d152_32,c,10,-0.084).
atm(train,d152,d152_33,h,3,0.066).
atm(train,d152,d152_34,h,3,0.066).
atm(train,d152,d152_35,h,3,0.066).
atm(train,d152,d152_36,c,10,-0.084).
atm(train,d152,d152_37,h,3,0.066).
atm(train,d152,d152_38,h,3,0.066).
atm(train,d152,d152_39,h,3,0.066).
atm(train,d152,d152_40,h,1,0.166).
atm(train,d152,d152_41,h,1,0.166).
atm(train,d152,d152_42,h,1,0.267).
atm(train,d152,d152_43,h,1,0.267).
atm(train,d153,d153_1,c,22,-0.13).
atm(train,d153,d153_2,c,22,0.0).
atm(train,d153,d153_3,c,22,0.25).
atm(train,d153,d153_4,c,22,-0.13).
atm(train,d153,d153_5,c,22,0.25).
atm(train,d153,d153_6,c,22,-0.13).
atm(train,d153,d153_7,h,3,0.13).
atm(train,d153,d153_8,h,3,0.13).
atm(train,d153,d153_9,h,3,0.13).
atm(train,d153,d153_10,o,45,-0.65).
atm(train,d153,d153_11,h,8,0.4).
atm(train,d153,d153_12,o,45,-0.65).
atm(train,d153,d153_13,c,10,-0.1).
atm(train,d153,d153_14,c,10,-0.1).
atm(train,d153,d153_15,h,3,0.05).
atm(train,d153,d153_16,h,3,0.05).
atm(train,d153,d153_17,c,10,-0.1).
atm(train,d153,d153_18,h,3,0.05).
atm(train,d153,d153_19,h,3,0.05).
atm(train,d153,d153_20,c,10,-0.1).
atm(train,d153,d153_21,h,3,0.05).
atm(train,d153,d153_22,h,3,0.05).
atm(train,d153,d153_23,c,10,-0.1).
atm(train,d153,d153_24,h,3,0.05).
atm(train,d153,d153_25,h,3,0.05).
atm(train,d153,d153_26,h,8,0.4).
atm(train,d153,d153_27,c,10,-0.15).
atm(train,d153,d153_28,h,3,0.05).
atm(train,d153,d153_29,h,3,0.05).
atm(train,d153,d153_30,h,3,0.05).
atm(train,d153,d153_31,h,3,0.05).
atm(train,d153,d153_32,h,3,0.05).
atm(train,d154,d154_1,c,22,-0.119).
atm(train,d154,d154_2,c,22,0.011).
atm(train,d154,d154_3,c,22,-0.119).
atm(train,d154,d154_4,c,22,0.261).
atm(train,d154,d154_5,c,22,0.261).
atm(train,d154,d154_6,c,22,-0.119).
atm(train,d154,d154_7,h,3,0.141).
atm(train,d154,d154_8,h,3,0.141).
atm(train,d154,d154_9,o,45,-0.64).
atm(train,d154,d154_10,c,10,-0.089).
atm(train,d154,d154_11,c,10,-0.089).
atm(train,d154,d154_12,h,3,0.061).
atm(train,d154,d154_13,h,3,0.061).
atm(train,d154,d154_14,h,8,0.41).
atm(train,d154,d154_15,h,3,0.141).
atm(train,d154,d154_16,o,45,-0.64).
atm(train,d154,d154_17,h,8,0.41).
atm(train,d154,d154_18,c,10,-0.089).
atm(train,d154,d154_19,h,3,0.061).
atm(train,d154,d154_20,h,3,0.061).
atm(train,d154,d154_21,h,3,0.061).
atm(train,d154,d154_22,n,36,-0.29).
atm(train,d154,d154_23,h,1,0.161).
atm(train,d154,d154_24,h,1,0.161).
atm(train,d154,d154_25,c,14,0.79).
atm(train,d154,d154_26,o,45,-0.66).
atm(train,d154,d154_27,o,51,-0.6).
atm(train,d154,d154_28,h,1,0.261).
atm(train,d155,d155_1,c,22,-0.105).
atm(train,d155,d155_2,c,22,0.275).
atm(train,d155,d155_3,c,22,0.025).
atm(train,d155,d155_4,c,22,-0.105).
atm(train,d155,d155_5,c,22,-0.105).
atm(train,d155,d155_6,c,22,-0.105).
atm(train,d155,d155_7,h,3,0.155).
atm(train,d155,d155_8,h,3,0.155).
atm(train,d155,d155_9,h,3,0.155).
atm(train,d155,d155_10,c,14,0.575).
atm(train,d155,d155_11,c,16,-0.175).
atm(train,d155,d155_12,c,10,0.125).
atm(train,d155,d155_13,c,10,0.025).
atm(train,d155,d155_14,c,16,-0.075).
atm(train,d155,d155_15,c,10,0.125).
atm(train,d155,d155_16,c,10,-0.125).
atm(train,d155,d155_17,c,10,0.215).
atm(train,d155,d155_18,h,3,0.125).
atm(train,d155,d155_19,c,14,0.605).
atm(train,d155,d155_20,c,16,-0.175).
atm(train,d155,d155_21,c,16,-0.075).
atm(train,d155,d155_22,c,10,0.125).
atm(train,d155,d155_23,h,3,0.075).
atm(train,d155,d155_24,h,3,-0.005).
atm(train,d155,d155_25,o,45,-0.625).
atm(train,d155,d155_26,c,10,-0.075).
atm(train,d155,d155_27,h,3,0.075).
atm(train,d155,d155_28,h,3,0.075).
atm(train,d155,d155_29,h,3,0.075).
atm(train,d155,d155_30,o,45,-0.625).
atm(train,d155,d155_31,h,3,0.085).
atm(train,d155,d155_32,n,36,-0.275).
atm(train,d155,d155_33,h,3,0.075).
atm(train,d155,d155_34,c,10,-0.125).
atm(train,d155,d155_35,h,3,0.075).
atm(train,d155,d155_36,h,3,0.075).
atm(train,d155,d155_37,h,3,0.075).
atm(train,d155,d155_38,c,10,-0.125).
atm(train,d155,d155_39,h,3,0.075).
atm(train,d155,d155_40,h,3,0.075).
atm(train,d155,d155_41,h,3,0.075).
atm(train,d155,d155_42,o,45,-0.625).
atm(train,d155,d155_43,c,10,0.115).
atm(train,d155,d155_44,o,50,-0.205).
atm(train,d155,d155_45,h,3,0.075).
atm(train,d155,d155_46,h,3,0.075).
atm(train,d155,d155_47,n,36,-0.275).
atm(train,d155,d155_48,o,45,-0.625).
atm(train,d155,d155_49,o,45,-0.625).
atm(train,d155,d155_50,o,40,-0.525).
atm(train,d155,d155_51,o,42,-0.555).
atm(train,d155,d155_52,h,8,0.425).
atm(train,d155,d155_53,h,8,0.425).
atm(train,d155,d155_54,h,8,0.425).
atm(train,d155,d155_55,h,8,0.425).
atm(train,d155,d155_56,h,1,0.175).
atm(train,d155,d155_57,h,1,0.175).
atm(train,d155,d155_58,h,8,0.425).
atm(train,d156,d156_1,c,22,-0.139).
atm(train,d156,d156_2,c,22,0.172).
atm(train,d156,d156_3,c,22,-0.139).
atm(train,d156,d156_4,c,22,-0.139).
atm(train,d156,d156_5,c,22,0.172).
atm(train,d156,d156_6,c,22,0.172).
atm(train,d156,d156_7,h,3,0.121).
atm(train,d156,d156_8,h,3,0.121).
atm(train,d156,d156_9,c,10,-0.009).
atm(train,d156,d156_10,o,50,-0.238).
atm(train,d156,d156_11,c,10,-0.109).
atm(train,d156,d156_12,c,10,0.056).
atm(train,d156,d156_13,h,3,0.041).
atm(train,d156,d156_14,h,3,0.041).
atm(train,d156,d156_15,h,3,0.091).
atm(train,d156,d156_16,h,3,0.041).
atm(train,d156,d156_17,c,14,0.542).
atm(train,d156,d156_18,c,22,-0.009).
atm(train,d156,d156_19,o,50,-0.238).
atm(train,d156,d156_20,c,22,0.172).
atm(train,d156,d156_21,c,22,-0.009).
atm(train,d156,d156_22,c,22,0.172).
atm(train,d156,d156_23,c,22,-0.139).
atm(train,d156,d156_24,c,22,-0.139).
atm(train,d156,d156_25,h,3,0.121).
atm(train,d156,d156_26,h,3,0.121).
atm(train,d156,d156_27,c,10,-0.109).
atm(train,d156,d156_28,c,10,0.091).
atm(train,d156,d156_29,o,50,-0.238).
atm(train,d156,d156_30,h,3,0.041).
atm(train,d156,d156_31,h,3,0.041).
atm(train,d156,d156_32,c,16,-0.208).
atm(train,d156,d156_33,c,16,-0.208).
atm(train,d156,d156_34,h,3,0.091).
atm(train,d156,d156_35,h,3,0.091).
atm(train,d156,d156_36,c,10,0.081).
atm(train,d156,d156_37,h,3,0.041).
atm(train,d156,d156_38,h,3,0.041).
atm(train,d156,d156_39,h,3,0.041).
atm(train,d156,d156_40,o,50,-0.238).
atm(train,d156,d156_41,c,10,0.081).
atm(train,d156,d156_42,h,3,0.041).
atm(train,d156,d156_43,h,3,0.041).
atm(train,d156,d156_44,h,3,0.041).
atm(train,d156,d156_45,o,50,-0.238).
atm(train,d156,d156_46,c,10,0.081).
atm(train,d156,d156_47,h,3,0.041).
atm(train,d156,d156_48,h,3,0.041).
atm(train,d156,d156_49,h,3,0.041).
atm(train,d156,d156_50,o,42,-0.588).
atm(train,d156,d156_51,h,3,0.011).
atm(train,d157,d157_1,c,22,-0.122).
atm(train,d157,d157_2,c,22,-0.122).
atm(train,d157,d157_3,c,22,-0.122).
atm(train,d157,d157_4,c,22,-0.121).
atm(train,d157,d157_5,c,22,-0.121).
atm(train,d157,d157_6,c,22,-0.121).
atm(train,d157,d157_7,h,3,0.139).
atm(train,d157,d157_8,h,3,0.139).
atm(train,d157,d157_9,h,3,0.139).
atm(train,d157,d157_10,h,3,0.139).
atm(train,d157,d157_11,h,3,0.139).
atm(train,d157,d157_12,c,22,-0.121).
atm(train,d157,d157_13,c,22,-0.121).
atm(train,d157,d157_14,c,22,-0.121).
atm(train,d157,d157_15,c,22,-0.121).
atm(train,d157,d157_16,c,22,-0.122).
atm(train,d157,d157_17,c,22,-0.122).
atm(train,d157,d157_18,h,3,0.139).
atm(train,d157,d157_19,h,3,0.139).
atm(train,d157,d157_20,h,3,0.139).
atm(train,d157,d157_21,h,3,0.139).
atm(train,d157,d157_22,h,3,0.139).
atm(train,d157,d157_23,c,10,0.008).
atm(train,d157,d157_24,o,50,-0.36).
atm(train,d157,d157_25,h,3,0.058).
atm(train,d157,d157_26,c,10,0.028).
atm(train,d157,d157_27,c,10,0.008).
atm(train,d157,d157_28,h,3,0.068).
atm(train,d157,d157_29,h,3,0.068).
atm(train,d157,d157_30,n,36,-0.291).
atm(train,d157,d157_31,h,3,0.058).
atm(train,d157,d157_32,h,3,0.058).
atm(train,d157,d157_33,c,10,0.008).
atm(train,d157,d157_34,h,3,0.058).
atm(train,d157,d157_35,h,3,0.058).
atm(train,d157,d157_36,h,3,0.058).
atm(train,d157,d157_37,c,10,0.008).
atm(train,d157,d157_38,h,3,0.058).
atm(train,d157,d157_39,h,3,0.058).
atm(train,d157,d157_40,h,3,0.058).
atm(train,d158,d158_1,na,81,0.65).
atm(train,d158,d158_2,f,92,-0.65).
atm(train,d159,d159_1,c,22,0.007).
atm(train,d159,d159_2,c,22,-0.123).
atm(train,d159,d159_3,c,22,-0.122).
atm(train,d159,d159_4,c,22,-0.123).
atm(train,d159,d159_5,c,22,-0.123).
atm(train,d159,d159_6,c,22,-0.123).
atm(train,d159,d159_7,h,3,0.138).
atm(train,d159,d159_8,h,3,0.138).
atm(train,d159,d159_9,h,3,0.138).
atm(train,d159,d159_10,h,3,0.138).
atm(train,d159,d159_11,c,14,0.558).
atm(train,d159,d159_12,c,10,0.007).
atm(train,d159,d159_13,h,3,0.057).
atm(train,d159,d159_14,h,3,0.057).
atm(train,d159,d159_15,h,3,0.057).
atm(train,d159,d159_16,o,42,-0.572).
atm(train,d159,d159_17,s,77,0.358).
atm(train,d159,d159_18,c,14,0.608).
atm(train,d159,d159_19,n,32,-0.392).
atm(train,d159,d159_20,c,10,-0.093).
atm(train,d159,d159_21,c,10,-0.093).
atm(train,d159,d159_22,c,10,-0.093).
atm(train,d159,d159_23,c,10,-0.093).
atm(train,d159,d159_24,c,10,-0.093).
atm(train,d159,d159_25,c,10,-0.093).
atm(train,d159,d159_26,h,3,0.107).
atm(train,d159,d159_27,h,3,0.057).
atm(train,d159,d159_28,h,3,0.057).
atm(train,d159,d159_29,h,3,0.057).
atm(train,d159,d159_30,h,3,0.057).
atm(train,d159,d159_31,h,3,0.057).
atm(train,d159,d159_32,h,3,0.057).
atm(train,d159,d159_33,h,3,0.057).
atm(train,d159,d159_34,h,3,0.057).
atm(train,d159,d159_35,h,3,0.057).
atm(train,d159,d159_36,h,3,0.057).
atm(train,d159,d159_37,h,1,0.308).
atm(train,d159,d159_38,o,40,-0.542).
atm(train,d159,d159_39,o,40,-0.242).
atm(train,d159,d159_40,o,40,-0.242).
atm(train,d159,d159_41,n,32,-0.392).
atm(train,d159,d159_42,h,1,0.308).
atm(train,d16,d16_1,c,22,-0.124).
atm(train,d16,d16_2,c,22,-0.124).
atm(train,d16,d16_3,c,22,0.006).
atm(train,d16,d16_4,c,22,0.006).
atm(train,d16,d16_5,c,22,-0.124).
atm(train,d16,d16_6,c,22,-0.124).
atm(train,d16,d16_7,h,3,0.135).
atm(train,d16,d16_8,h,3,0.135).
atm(train,d16,d16_9,h,3,0.135).
atm(train,d16,d16_10,h,3,0.135).
atm(train,d16,d16_11,c,14,0.555).
atm(train,d16,d16_12,c,22,0.006).
atm(train,d16,d16_13,c,22,0.006).
atm(train,d16,d16_14,c,14,0.555).
atm(train,d16,d16_15,c,22,-0.124).
atm(train,d16,d16_16,c,22,-0.124).
atm(train,d16,d16_17,c,22,-0.175).
atm(train,d16,d16_18,c,22,-0.124).
atm(train,d16,d16_19,h,3,0.135).
atm(train,d16,d16_20,h,3,0.135).
atm(train,d16,d16_21,o,40,-0.545).
atm(train,d16,d16_22,o,40,-0.545).
atm(train,d16,d16_23,n,38,0.805).
atm(train,d16,d16_24,c,10,0.006).
atm(train,d16,d16_25,h,3,0.056).
atm(train,d16,d16_26,h,3,0.056).
atm(train,d16,d16_27,h,3,0.056).
atm(train,d16,d16_28,o,40,-0.395).
atm(train,d16,d16_29,o,40,-0.395).
atm(train,d160,d160_1,c,10,0.158).
atm(train,d160,d160_2,c,16,-0.042).
atm(train,d160,d160_3,c,16,-0.142).
atm(train,d160,d160_4,c,14,0.756).
atm(train,d160,d160_5,o,49,-0.593).
atm(train,d160,d160_6,o,45,-0.593).
atm(train,d160,d160_7,o,45,-0.593).
atm(train,d160,d160_8,c,10,0.248).
atm(train,d160,d160_9,h,3,0.108).
atm(train,d160,d160_10,o,45,-0.593).
atm(train,d160,d160_11,c,10,0.228).
atm(train,d160,d160_12,h,3,0.118).
atm(train,d160,d160_13,o,45,-0.593).
atm(train,d160,d160_14,h,3,0.098).
atm(train,d160,d160_15,h,3,0.098).
atm(train,d160,d160_16,o,51,-0.494).
atm(train,d160,d160_17,h,8,0.458).
atm(train,d160,d160_18,h,8,0.458).
atm(train,d160,d160_19,h,8,0.458).
atm(train,d160,d160_20,h,8,0.457).
atm(train,d161,d161_1,c,22,-0.126).
atm(train,d161,d161_2,c,22,0.254).
atm(train,d161,d161_3,c,22,-0.126).
atm(train,d161,d161_4,c,22,-0.126).
atm(train,d161,d161_5,c,22,-0.176).
atm(train,d161,d161_6,c,22,-0.126).
atm(train,d161,d161_7,h,3,0.134).
atm(train,d161,d161_8,h,3,0.134).
atm(train,d161,d161_9,c,10,0.005).
atm(train,d161,d161_10,h,3,0.055).
atm(train,d161,d161_11,h,3,0.055).
atm(train,d161,d161_12,h,3,0.055).
atm(train,d161,d161_13,c,10,-0.095).
atm(train,d161,d161_14,c,10,-0.095).
atm(train,d161,d161_15,h,3,0.055).
atm(train,d161,d161_16,h,3,0.055).
atm(train,d161,d161_17,h,3,0.055).
atm(train,d161,d161_18,c,10,-0.095).
atm(train,d161,d161_19,h,3,0.055).
atm(train,d161,d161_20,h,3,0.055).
atm(train,d161,d161_21,h,3,0.055).
atm(train,d161,d161_22,c,10,-0.095).
atm(train,d161,d161_23,h,3,0.055).
atm(train,d161,d161_24,h,3,0.055).
atm(train,d161,d161_25,h,3,0.055).
atm(train,d161,d161_26,o,45,-0.646).
atm(train,d161,d161_27,c,10,-0.095).
atm(train,d161,d161_28,c,10,-0.095).
atm(train,d161,d161_29,h,3,0.055).
atm(train,d161,d161_30,h,3,0.055).
atm(train,d161,d161_31,h,3,0.055).
atm(train,d161,d161_32,c,10,-0.095).
atm(train,d161,d161_33,h,3,0.055).
atm(train,d161,d161_34,h,3,0.055).
atm(train,d161,d161_35,h,3,0.055).
atm(train,d161,d161_36,c,10,-0.095).
atm(train,d161,d161_37,h,3,0.055).
atm(train,d161,d161_38,h,3,0.055).
atm(train,d161,d161_39,h,3,0.055).
atm(train,d161,d161_40,h,8,0.404).
atm(train,d162,d162_1,c,22,-0.142).
atm(train,d162,d162_2,c,22,-0.142).
atm(train,d162,d162_3,c,22,0.168).
atm(train,d162,d162_4,c,22,0.169).
atm(train,d162,d162_5,c,22,-0.142).
atm(train,d162,d162_6,c,22,-0.142).
atm(train,d162,d162_7,h,3,0.118).
atm(train,d162,d162_8,h,3,0.118).
atm(train,d162,d162_9,h,3,0.118).
atm(train,d162,d162_10,h,3,0.118).
atm(train,d162,d162_11,c,22,0.169).
atm(train,d162,d162_12,c,22,-0.142).
atm(train,d162,d162_13,c,22,-0.142).
atm(train,d162,d162_14,c,22,-0.142).
atm(train,d162,d162_15,c,22,-0.142).
atm(train,d162,d162_16,c,22,0.168).
atm(train,d162,d162_17,h,3,0.118).
atm(train,d162,d162_18,h,3,0.118).
atm(train,d162,d162_19,h,3,0.118).
atm(train,d162,d162_20,h,3,0.118).
atm(train,d162,d162_21,o,50,-0.241).
atm(train,d162,d162_22,o,50,-0.241).
atm(train,d163,d163_1,c,10,-0.112).
atm(train,d163,d163_2,c,10,-0.112).
atm(train,d163,d163_3,n,32,-0.512).
atm(train,d163,d163_4,h,3,-0.062).
atm(train,d163,d163_5,h,3,-0.062).
atm(train,d163,d163_6,c,14,0.488).
atm(train,d163,d163_7,c,14,0.488).
atm(train,d163,d163_8,o,50,-0.342).
atm(train,d163,d163_9,o,51,-0.661).
atm(train,d163,d163_10,na,81,0.889).
atm(train,d163,d163_11,o,51,-0.661).
atm(train,d163,d163_12,o,50,-0.342).
atm(train,d163,d163_13,na,81,0.889).
atm(train,d163,d163_14,n,32,-0.512).
atm(train,d163,d163_15,h,3,-0.062).
atm(train,d163,d163_16,h,3,-0.062).
atm(train,d163,d163_17,c,14,0.488).
atm(train,d163,d163_18,c,14,0.488).
atm(train,d163,d163_19,o,51,-0.661).
atm(train,d163,d163_20,o,50,-0.342).
atm(train,d163,d163_21,na,81,0.889).
atm(train,d163,d163_22,o,51,-0.661).
atm(train,d163,d163_23,o,50,-0.342).
atm(train,d163,d163_24,na,81,0.889).
atm(train,d164,d164_1,c,10,0.15).
atm(train,d164,d164_2,c,10,-0.09).
atm(train,d164,d164_3,c,10,0.2).
atm(train,d164,d164_4,c,10,0.03).
atm(train,d164,d164_5,o,50,-0.36).
atm(train,d164,d164_6,c,10,0.075).
atm(train,d164,d164_7,o,50,-0.36).
atm(train,d164,d164_8,c,10,0.2).
atm(train,d164,d164_9,c,10,0.2).
atm(train,d164,d164_10,c,10,0.2).
atm(train,d164,d164_11,c,10,0.32).
atm(train,d164,d164_12,o,50,-0.36).
atm(train,d164,d164_13,c,10,0.15).
atm(train,d164,d164_14,h,3,0.06).
atm(train,d164,d164_15,h,3,0.045).
atm(train,d164,d164_16,h,3,0.045).
atm(train,d164,d164_17,h,3,0.07).
atm(train,d164,d164_18,o,45,-0.64).
atm(train,d164,d164_19,c,10,-0.09).
atm(train,d164,d164_20,h,3,0.06).
atm(train,d164,d164_21,h,3,0.06).
atm(train,d164,d164_22,h,3,0.06).
atm(train,d164,d164_23,h,3,0.07).
atm(train,d164,d164_24,o,45,-0.64).
atm(train,d164,d164_25,o,45,-0.64).
atm(train,d164,d164_26,c,10,0.18).
atm(train,d164,d164_27,h,3,0.07).
atm(train,d164,d164_28,o,50,-0.36).
atm(train,d164,d164_29,o,45,-0.64).
atm(train,d164,d164_30,h,3,0.07).
atm(train,d164,d164_31,h,3,0.07).
atm(train,d164,d164_32,h,3,0.045).
atm(train,d164,d164_33,c,10,0.03).
atm(train,d164,d164_34,h,8,0.41).
atm(train,d164,d164_35,h,8,0.41).
atm(train,d164,d164_36,h,8,0.41).
atm(train,d164,d164_37,h,8,0.41).
atm(train,d164,d164_38,o,45,-0.64).
atm(train,d164,d164_39,h,3,0.05).
atm(train,d164,d164_40,h,3,0.05).
atm(train,d164,d164_41,h,8,0.41).
atm(train,d164,d164_42,h,3,0.07).
atm(train,d164,d164_43,h,3,0.07).
atm(train,d164,d164_44,h,3,0.07).
atm(train,d165,d165_1,c,22,-0.117).
atm(train,d165,d165_2,c,22,-0.117).
atm(train,d165,d165_3,c,22,-0.117).
atm(train,d165,d165_4,c,22,-0.117).
atm(train,d165,d165_5,c,22,-0.117).
atm(train,d165,d165_6,c,22,-0.117).
atm(train,d165,d165_7,h,3,0.143).
atm(train,d165,d165_8,h,3,0.143).
atm(train,d165,d165_9,h,3,0.143).
atm(train,d165,d165_10,h,3,0.143).
atm(train,d165,d165_11,h,3,0.143).
atm(train,d165,d165_12,c,10,0.013).
atm(train,d165,d165_13,c,14,0.562).
atm(train,d165,d165_14,c,22,0.013).
atm(train,d165,d165_15,c,22,-0.117).
atm(train,d165,d165_16,c,22,-0.117).
atm(train,d165,d165_17,c,22,-0.117).
atm(train,d165,d165_18,c,22,-0.117).
atm(train,d165,d165_19,c,22,-0.117).
atm(train,d165,d165_20,h,3,0.143).
atm(train,d165,d165_21,h,3,0.143).
atm(train,d165,d165_22,h,3,0.143).
atm(train,d165,d165_23,h,3,0.143).
atm(train,d165,d165_24,h,3,0.143).
atm(train,d165,d165_25,o,45,-0.638).
atm(train,d165,d165_26,h,3,0.063).
atm(train,d165,d165_27,o,42,-0.568).
atm(train,d165,d165_28,h,8,0.412).
atm(train,d166,d166_1,c,10,-0.1).
atm(train,d166,d166_2,c,10,-0.1).
atm(train,d166,d166_3,c,10,-0.1).
atm(train,d166,d166_4,h,3,0.05).
atm(train,d166,d166_5,h,3,0.05).
atm(train,d166,d166_6,c,10,0.0).
atm(train,d166,d166_7,h,3,0.05).
atm(train,d166,d166_8,h,3,0.05).
atm(train,d166,d166_9,n,32,-0.4).
atm(train,d166,d166_10,h,3,0.05).
atm(train,d166,d166_11,h,3,0.05).
atm(train,d166,d166_12,c,14,0.6).
atm(train,d166,d166_13,c,10,-0.1).
atm(train,d166,d166_14,h,3,0.05).
atm(train,d166,d166_15,h,3,0.05).
atm(train,d166,d166_16,h,3,0.05).
atm(train,d166,d166_17,h,3,0.05).
atm(train,d166,d166_18,o,40,-0.55).
atm(train,d166,d166_19,h,1,0.25).
atm(train,d167,d167_1,c,22,-0.123).
atm(train,d167,d167_2,c,22,-0.123).
atm(train,d167,d167_3,c,22,-0.123).
atm(train,d167,d167_4,c,22,-0.123).
atm(train,d167,d167_5,c,22,-0.123).
atm(train,d167,d167_6,c,22,-0.123).
atm(train,d167,d167_7,h,3,0.136).
atm(train,d167,d167_8,h,3,0.136).
atm(train,d167,d167_9,h,3,0.136).
atm(train,d167,d167_10,h,3,0.136).
atm(train,d167,d167_11,h,3,0.137).
atm(train,d167,d167_12,c,10,0.007).
atm(train,d167,d167_13,o,45,-0.644).
atm(train,d167,d167_14,c,10,0.057).
atm(train,d167,d167_15,h,3,0.057).
atm(train,d167,d167_16,c,10,-0.144).
atm(train,d167,d167_17,h,3,0.057).
atm(train,d167,d167_18,h,3,0.057).
atm(train,d167,d167_19,h,3,0.057).
atm(train,d167,d167_20,n,36,-0.294).
atm(train,d167,d167_21,h,3,0.107).
atm(train,d167,d167_22,c,10,0.007).
atm(train,d167,d167_23,h,8,0.406).
atm(train,d167,d167_24,h,3,0.057).
atm(train,d167,d167_25,h,3,0.057).
atm(train,d167,d167_26,h,3,0.057).
atm(train,d167,d167_27,h,1,0.156).
atm(train,d168,d168_1,c,10,-0.082).
atm(train,d168,d168_2,c,10,-0.132).
atm(train,d168,d168_3,c,10,0.208).
atm(train,d168,d168_4,c,10,-0.082).
atm(train,d168,d168_5,c,10,-0.132).
atm(train,d168,d168_6,c,10,-0.082).
atm(train,d168,d168_7,h,3,0.067).
atm(train,d168,d168_8,h,3,0.067).
atm(train,d168,d168_9,h,3,0.067).
atm(train,d168,d168_10,h,3,0.067).
atm(train,d168,d168_11,h,3,0.067).
atm(train,d168,d168_12,h,3,0.067).
atm(train,d168,d168_13,c,10,-0.132).
atm(train,d168,d168_14,h,3,-0.013).
atm(train,d168,d168_15,h,3,0.067).
atm(train,d168,d168_16,h,3,0.067).
atm(train,d168,d168_17,h,3,0.067).
atm(train,d168,d168_18,o,45,-0.632).
atm(train,d168,d168_19,h,3,0.078).
atm(train,d168,d168_20,c,10,-0.132).
atm(train,d168,d168_21,h,3,-0.013).
atm(train,d168,d168_22,c,10,-0.132).
atm(train,d168,d168_23,h,3,0.067).
atm(train,d168,d168_24,h,3,0.067).
atm(train,d168,d168_25,h,3,0.067).
atm(train,d168,d168_26,c,10,-0.132).
atm(train,d168,d168_27,h,3,-0.013).
atm(train,d168,d168_28,h,3,0.067).
atm(train,d168,d168_29,h,3,0.067).
atm(train,d168,d168_30,h,3,0.067).
atm(train,d168,d168_31,h,8,0.418).
atm(train,d169,d169_1,c,22,-0.13).
atm(train,d169,d169_2,c,22,-0.13).
atm(train,d169,d169_3,c,22,-0.13).
atm(train,d169,d169_4,c,22,-0.13).
atm(train,d169,d169_5,c,22,0.25).
atm(train,d169,d169_6,c,22,-0.13).
atm(train,d169,d169_7,h,3,0.13).
atm(train,d169,d169_8,h,3,0.13).
atm(train,d169,d169_9,h,3,0.13).
atm(train,d169,d169_10,h,3,0.13).
atm(train,d169,d169_11,h,3,0.13).
atm(train,d169,d169_12,o,45,-0.65).
atm(train,d169,d169_13,h,8,0.4).
atm(train,d17,d17_1,c,22,-0.107).
atm(train,d17,d17_2,c,22,-0.107).
atm(train,d17,d17_3,c,27,0.023).
atm(train,d17,d17_4,c,27,0.023).
atm(train,d17,d17_5,c,22,-0.107).
atm(train,d17,d17_6,c,22,-0.107).
atm(train,d17,d17_7,h,3,0.153).
atm(train,d17,d17_8,h,3,0.153).
atm(train,d17,d17_9,h,3,0.153).
atm(train,d17,d17_10,c,22,-0.107).
atm(train,d17,d17_11,c,22,-0.107).
atm(train,d17,d17_12,c,22,-0.107).
atm(train,d17,d17_13,c,22,-0.107).
atm(train,d17,d17_14,h,3,0.153).
atm(train,d17,d17_15,h,3,0.153).
atm(train,d17,d17_16,h,3,0.153).
atm(train,d17,d17_17,n,32,-0.758).
atm(train,d17,d17_18,n,32,-0.758).
atm(train,d17,d17_19,h,1,0.352).
atm(train,d17,d17_20,h,1,0.352).
atm(train,d17,d17_21,h,1,0.352).
atm(train,d17,d17_22,h,1,0.352).
atm(train,d170,d170_1,c,22,-0.107).
atm(train,d170,d170_2,c,22,-0.107).
atm(train,d170,d170_3,c,22,0.023).
atm(train,d170,d170_4,c,22,0.023).
atm(train,d170,d170_5,c,22,-0.107).
atm(train,d170,d170_6,c,22,-0.107).
atm(train,d170,d170_7,h,3,0.153).
atm(train,d170,d170_8,h,3,0.153).
atm(train,d170,d170_9,h,3,0.153).
atm(train,d170,d170_10,h,3,0.153).
atm(train,d170,d170_11,c,14,0.724).
atm(train,d170,d170_12,o,49,-0.626).
atm(train,d170,d170_13,c,14,0.724).
atm(train,d170,d170_14,o,51,-0.526).
atm(train,d170,d170_15,o,51,-0.526).
atm(train,d171,d171_1,c,22,-0.166).
atm(train,d171,d171_2,c,22,-0.117).
atm(train,d171,d171_3,c,22,-0.117).
atm(train,d171,d171_4,c,22,-0.117).
atm(train,d171,d171_5,c,22,-0.117).
atm(train,d171,d171_6,c,22,-0.117).
atm(train,d171,d171_7,h,3,0.143).
atm(train,d171,d171_8,h,3,0.144).
atm(train,d171,d171_9,h,3,0.144).
atm(train,d171,d171_10,h,3,0.144).
atm(train,d171,d171_11,c,10,0.013).
atm(train,d171,d171_12,h,3,0.063).
atm(train,d171,d171_13,h,3,0.063).
atm(train,d171,d171_14,h,3,0.063).
atm(train,d171,d171_15,s,77,0.364).
atm(train,d171,d171_16,n,32,-0.386).
atm(train,d171,d171_17,c,14,0.614).
atm(train,d171,d171_18,n,32,-0.386).
atm(train,d171,d171_19,o,40,-0.236).
atm(train,d171,d171_20,o,40,-0.236).
atm(train,d171,d171_21,h,1,0.314).
atm(train,d171,d171_22,o,40,-0.536).
atm(train,d171,d171_23,c,10,0.013).
atm(train,d171,d171_24,c,10,-0.087).
atm(train,d171,d171_25,c,10,-0.087).
atm(train,d171,d171_26,c,10,-0.087).
atm(train,d171,d171_27,c,10,-0.087).
atm(train,d171,d171_28,c,10,0.013).
atm(train,d171,d171_29,n,36,-0.286).
atm(train,d171,d171_30,h,3,0.063).
atm(train,d171,d171_31,h,3,0.063).
atm(train,d171,d171_32,h,3,0.063).
atm(train,d171,d171_33,h,3,0.063).
atm(train,d171,d171_34,h,3,0.063).
atm(train,d171,d171_35,h,3,0.063).
atm(train,d171,d171_36,h,3,0.063).
atm(train,d171,d171_37,h,3,0.063).
atm(train,d171,d171_38,h,3,0.063).
atm(train,d171,d171_39,h,3,0.063).
atm(train,d171,d171_40,h,3,0.063).
atm(train,d171,d171_41,h,3,0.063).
atm(train,d171,d171_42,h,1,0.314).
atm(train,d172,d172_1,sn,113,0.99).
atm(train,d172,d172_2,o,45,-0.66).
atm(train,d172,d172_3,c,22,-0.14).
atm(train,d172,d172_4,c,22,-0.14).
atm(train,d172,d172_5,c,22,-0.14).
atm(train,d172,d172_6,c,22,-0.14).
atm(train,d172,d172_7,c,22,-0.14).
atm(train,d172,d172_8,c,22,-0.14).
atm(train,d172,d172_9,c,22,-0.14).
atm(train,d172,d172_10,c,22,-0.14).
atm(train,d172,d172_11,h,3,0.12).
atm(train,d172,d172_12,h,3,0.12).
atm(train,d172,d172_13,h,3,0.12).
atm(train,d172,d172_14,h,3,0.12).
atm(train,d172,d172_15,h,3,0.12).
atm(train,d172,d172_16,c,22,-0.14).
atm(train,d172,d172_17,c,22,-0.14).
atm(train,d172,d172_18,c,22,-0.14).
atm(train,d172,d172_19,c,22,-0.14).
atm(train,d172,d172_20,c,22,-0.14).
atm(train,d172,d172_21,h,3,0.12).
atm(train,d172,d172_22,h,3,0.12).
atm(train,d172,d172_23,h,3,0.12).
atm(train,d172,d172_24,h,3,0.12).
atm(train,d172,d172_25,h,3,0.12).
atm(train,d172,d172_26,c,22,-0.14).
atm(train,d172,d172_27,c,22,-0.14).
atm(train,d172,d172_28,c,22,-0.14).
atm(train,d172,d172_29,c,22,-0.14).
atm(train,d172,d172_30,c,22,-0.14).
atm(train,d172,d172_31,h,3,0.12).
atm(train,d172,d172_32,h,3,0.12).
atm(train,d172,d172_33,h,3,0.12).
atm(train,d172,d172_34,h,3,0.12).
atm(train,d172,d172_35,h,3,0.12).
atm(train,d172,d172_36,h,1,0.39).
atm(train,d173,d173_1,c,10,0.217).
atm(train,d173,d173_2,c,10,-0.073).
atm(train,d173,d173_3,c,10,-0.123).
atm(train,d173,d173_4,c,10,-0.273).
atm(train,d173,d173_5,c,10,-0.073).
atm(train,d173,d173_6,c,10,-0.073).
atm(train,d173,d173_7,h,3,0.077).
atm(train,d173,d173_8,h,3,0.077).
atm(train,d173,d173_9,h,3,0.077).
atm(train,d173,d173_10,h,3,0.077).
atm(train,d173,d173_11,h,3,0.077).
atm(train,d173,d173_12,h,3,0.077).
atm(train,d173,d173_13,c,10,-0.073).
atm(train,d173,d173_14,c,10,-0.073).
atm(train,d173,d173_15,c,10,-0.123).
atm(train,d173,d173_16,c,10,-0.123).
atm(train,d173,d173_17,h,3,-0.003).
atm(train,d173,d173_18,h,3,0.077).
atm(train,d173,d173_19,h,3,0.077).
atm(train,d173,d173_20,h,3,0.077).
atm(train,d173,d173_21,h,3,0.077).
atm(train,d173,d173_22,c,10,-0.123).
atm(train,d173,d173_23,c,10,-0.273).
atm(train,d173,d173_24,c,10,-0.073).
atm(train,d173,d173_25,c,10,-0.073).
atm(train,d173,d173_26,h,3,-0.003).
atm(train,d173,d173_27,h,3,-0.003).
atm(train,d173,d173_28,h,3,0.077).
atm(train,d173,d173_29,h,3,0.077).
atm(train,d173,d173_30,h,3,0.077).
atm(train,d173,d173_31,h,3,0.077).
atm(train,d173,d173_32,c,10,-0.123).
atm(train,d173,d173_33,c,10,-0.073).
atm(train,d173,d173_34,c,10,-0.073).
atm(train,d173,d173_35,h,3,0.077).
atm(train,d173,d173_36,h,3,0.077).
atm(train,d173,d173_37,h,3,0.077).
atm(train,d173,d173_38,h,3,0.077).
atm(train,d173,d173_39,h,3,-0.003).
atm(train,d173,d173_40,o,45,-0.623).
atm(train,d173,d173_41,h,3,0.087).
atm(train,d173,d173_42,c,10,-0.063).
atm(train,d173,d173_43,h,3,0.077).
atm(train,d173,d173_44,h,3,0.077).
atm(train,d173,d173_45,h,3,0.077).
atm(train,d173,d173_46,c,10,-0.063).
atm(train,d173,d173_47,h,3,0.077).
atm(train,d173,d173_48,h,3,0.077).
atm(train,d173,d173_49,h,3,0.077).
atm(train,d173,d173_50,c,10,-0.123).
atm(train,d173,d173_51,h,3,-0.003).
atm(train,d173,d173_52,c,10,-0.123).
atm(train,d173,d173_53,h,3,0.077).
atm(train,d173,d173_54,h,3,0.077).
atm(train,d173,d173_55,h,3,0.077).
atm(train,d173,d173_56,c,10,-0.073).
atm(train,d173,d173_57,h,3,-0.003).
atm(train,d173,d173_58,c,10,-0.073).
atm(train,d173,d173_59,h,3,0.077).
atm(train,d173,d173_60,h,3,0.077).
atm(train,d173,d173_61,c,14,0.808).
atm(train,d173,d173_62,h,3,0.077).
atm(train,d173,d173_63,h,3,0.077).
atm(train,d173,d173_64,o,45,-0.643).
atm(train,d173,d173_65,o,51,-0.583).
atm(train,d173,d173_66,h,8,0.427).
atm(train,d173,d173_67,h,1,0.277).
atm(train,d174,d174_1,o,45,-0.65).
atm(train,d174,d174_2,c,10,0.17).
atm(train,d174,d174_3,c,10,0.19).
atm(train,d174,d174_4,h,3,0.04).
atm(train,d174,d174_5,h,3,0.04).
atm(train,d174,d174_6,o,45,-0.65).
atm(train,d174,d174_7,c,10,0.19).
atm(train,d174,d174_8,h,3,0.06).
atm(train,d174,d174_9,o,45,-0.65).
atm(train,d174,d174_10,c,10,0.19).
atm(train,d174,d174_11,h,3,0.06).
atm(train,d174,d174_12,o,45,-0.65).
atm(train,d174,d174_13,c,10,0.19).
atm(train,d174,d174_14,h,3,0.06).
atm(train,d174,d174_15,o,45,-0.65).
atm(train,d174,d174_16,c,10,0.17).
atm(train,d174,d174_17,h,3,0.06).
atm(train,d174,d174_18,o,45,-0.65).
atm(train,d174,d174_19,h,3,0.04).
atm(train,d174,d174_20,h,3,0.04).
atm(train,d174,d174_21,h,8,0.4).
atm(train,d174,d174_22,h,8,0.4).
atm(train,d174,d174_23,h,8,0.4).
atm(train,d174,d174_24,h,8,0.4).
atm(train,d174,d174_25,h,8,0.4).
atm(train,d174,d174_26,h,8,0.4).
atm(train,d175,d175_1,c,22,-0.192).
atm(train,d175,d175_2,c,22,-0.192).
atm(train,d175,d175_3,c,22,-0.192).
atm(train,d175,d175_4,c,22,-0.062).
atm(train,d175,d175_5,c,22,-0.192).
atm(train,d175,d175_6,c,22,-0.192).
atm(train,d175,d175_7,h,3,0.068).
atm(train,d175,d175_8,h,3,0.068).
atm(train,d175,d175_9,h,3,0.068).
atm(train,d175,d175_10,h,3,0.068).
atm(train,d175,d175_11,h,3,0.068).
atm(train,d175,d175_12,c,10,-0.162).
atm(train,d175,d175_13,c,10,-0.062).
atm(train,d175,d175_14,h,3,-0.012).
atm(train,d175,d175_15,h,3,-0.012).
atm(train,d175,d175_16,n,32,-0.461).
atm(train,d175,d175_17,h,3,-0.012).
atm(train,d175,d175_18,h,3,-0.012).
atm(train,d175,d175_19,c,14,0.539).
atm(train,d175,d175_20,n,37,-0.112).
atm(train,d175,d175_21,n,32,-0.461).
atm(train,d175,d175_22,c,14,0.539).
atm(train,d175,d175_23,n,37,-0.112).
atm(train,d175,d175_24,n,37,-0.511).
atm(train,d175,d175_25,h,1,0.238).
atm(train,d175,d175_26,h,1,0.239).
atm(train,d175,d175_27,h,1,0.239).
atm(train,d175,d175_28,h,1,0.239).
atm(train,d175,d175_29,h,2,0.289).
atm(train,d175,d175_30,h,2,0.289).
atm(train,d176,d176_1,c,22,-0.13).
atm(train,d176,d176_2,c,22,-0.13).
atm(train,d176,d176_3,c,22,0.0).
atm(train,d176,d176_4,c,22,0.0).
atm(train,d176,d176_5,c,22,-0.13).
atm(train,d176,d176_6,c,22,-0.13).
atm(train,d176,d176_7,h,3,0.13).
atm(train,d176,d176_8,h,3,0.13).
atm(train,d176,d176_9,h,3,0.13).
atm(train,d176,d176_10,h,3,0.13).
atm(train,d176,d176_11,c,14,0.55).
atm(train,d176,d176_12,o,40,-0.55).
atm(train,d176,d176_13,n,32,-0.6).
atm(train,d176,d176_14,c,14,0.55).
atm(train,d176,d176_15,n,32,-0.6).
atm(train,d176,d176_16,o,40,-0.55).
atm(train,d176,d176_17,h,1,0.3).
atm(train,d176,d176_18,h,1,0.3).
atm(train,d176,d176_19,h,1,0.3).
atm(train,d176,d176_20,h,1,0.3).
atm(train,d177,d177_1,c,22,-0.124).
atm(train,d177,d177_2,c,22,-0.124).
atm(train,d177,d177_3,c,22,0.187).
atm(train,d177,d177_4,c,22,0.186).
atm(train,d177,d177_5,c,22,-0.124).
atm(train,d177,d177_6,c,22,0.006).
atm(train,d177,d177_7,h,3,0.136).
atm(train,d177,d177_8,h,3,0.136).
atm(train,d177,d177_9,o,50,-0.223).
atm(train,d177,d177_10,c,10,0.096).
atm(train,d177,d177_11,o,50,-0.223).
atm(train,d177,d177_12,h,3,0.056).
atm(train,d177,d177_13,h,3,0.056).
atm(train,d177,d177_14,c,10,-0.094).
atm(train,d177,d177_15,c,10,-0.094).
atm(train,d177,d177_16,h,3,0.056).
atm(train,d177,d177_17,h,3,0.056).
atm(train,d177,d177_18,c,10,-0.144).
atm(train,d177,d177_19,h,3,0.056).
atm(train,d177,d177_20,h,3,0.056).
atm(train,d177,d177_21,h,3,0.056).
atm(train,d177,d177_22,h,3,0.056).
atm(train,d177,d177_23,h,3,0.056).
atm(train,d177,d177_24,c,10,0.026).
atm(train,d177,d177_25,o,50,-0.363).
atm(train,d177,d177_26,h,3,0.056).
atm(train,d177,d177_27,h,3,0.056).
atm(train,d177,d177_28,c,10,0.026).
atm(train,d177,d177_29,c,10,0.026).
atm(train,d177,d177_30,h,3,0.066).
atm(train,d177,d177_31,h,3,0.066).
atm(train,d177,d177_32,o,50,-0.363).
atm(train,d177,d177_33,h,3,0.066).
atm(train,d177,d177_34,h,3,0.066).
atm(train,d177,d177_35,c,10,0.026).
atm(train,d177,d177_36,c,10,0.026).
atm(train,d177,d177_37,h,3,0.066).
atm(train,d177,d177_38,h,3,0.066).
atm(train,d177,d177_39,o,50,-0.363).
atm(train,d177,d177_40,h,3,0.066).
atm(train,d177,d177_41,h,3,0.066).
atm(train,d177,d177_42,c,10,0.026).
atm(train,d177,d177_43,c,10,-0.094).
atm(train,d177,d177_44,h,3,0.066).
atm(train,d177,d177_45,h,3,0.066).
atm(train,d177,d177_46,c,10,-0.094).
atm(train,d177,d177_47,h,3,0.056).
atm(train,d177,d177_48,h,3,0.056).
atm(train,d177,d177_49,c,10,-0.144).
atm(train,d177,d177_50,h,3,0.056).
atm(train,d177,d177_51,h,3,0.056).
atm(train,d177,d177_52,h,3,0.056).
atm(train,d177,d177_53,h,3,0.056).
atm(train,d177,d177_54,h,3,0.056).
atm(train,d178,d178_1,ti,134,1.034).
atm(train,d178,d178_2,o,40,-0.517).
atm(train,d178,d178_3,o,40,-0.517).
atm(train,d179,d179_1,c,22,-0.12).
atm(train,d179,d179_2,c,22,-0.12).
atm(train,d179,d179_3,c,22,-0.12).
atm(train,d179,d179_4,c,22,-0.17).
atm(train,d179,d179_5,c,22,-0.12).
atm(train,d179,d179_6,c,22,-0.12).
atm(train,d179,d179_7,h,3,0.14).
atm(train,d179,d179_8,h,3,0.14).
atm(train,d179,d179_9,h,3,0.14).
atm(train,d179,d179_10,h,3,0.14).
atm(train,d179,d179_11,c,10,0.01).
atm(train,d179,d179_12,h,3,0.06).
atm(train,d179,d179_13,h,3,0.06).
atm(train,d179,d179_14,h,3,0.06).
atm(train,d179,d179_15,s,77,0.36).
atm(train,d179,d179_16,o,40,-0.24).
atm(train,d179,d179_17,o,40,-0.24).
atm(train,d179,d179_18,n,32,-0.39).
atm(train,d179,d179_19,c,14,0.61).
atm(train,d179,d179_20,o,40,-0.54).
atm(train,d179,d179_21,n,32,-0.39).
atm(train,d179,d179_22,c,10,0.01).
atm(train,d179,d179_23,c,10,-0.09).
atm(train,d179,d179_24,h,3,0.06).
atm(train,d179,d179_25,h,3,0.06).
atm(train,d179,d179_26,c,10,-0.09).
atm(train,d179,d179_27,h,3,0.06).
atm(train,d179,d179_28,h,3,0.06).
atm(train,d179,d179_29,c,10,-0.14).
atm(train,d179,d179_30,h,3,0.06).
atm(train,d179,d179_31,h,3,0.06).
atm(train,d179,d179_32,h,3,0.06).
atm(train,d179,d179_33,h,3,0.06).
atm(train,d179,d179_34,h,3,0.06).
atm(train,d179,d179_35,h,1,0.31).
atm(train,d179,d179_36,h,1,0.31).
atm(train,d18,d18_1,c,21,0.03).
atm(train,d18,d18_2,c,21,0.03).
atm(train,d18,d18_3,c,21,0.03).
atm(train,d18,d18_4,s,72,-0.2).
atm(train,d18,d18_5,n,38,0.831).
atm(train,d18,d18_6,o,40,-0.369).
atm(train,d18,d18_7,o,40,-0.369).
atm(train,d18,d18_8,n,32,-0.369).
atm(train,d18,d18_9,c,14,0.631).
atm(train,d18,d18_10,n,32,-0.369).
atm(train,d18,d18_11,o,40,-0.519).
atm(train,d18,d18_12,c,10,0.03).
atm(train,d18,d18_13,c,10,-0.07).
atm(train,d18,d18_14,h,1,0.331).
atm(train,d18,d18_15,h,1,0.331).
atm(train,d18,d18_16,h,3,0.08).
atm(train,d18,d18_17,h,3,0.08).
atm(train,d18,d18_18,h,3,0.08).
atm(train,d18,d18_19,h,3,0.08).
atm(train,d18,d18_20,h,3,0.08).
atm(train,d18,d18_21,n,34,-0.509).
atm(train,d18,d18_22,h,3,0.13).
atm(train,d180,d180_1,c,22,-0.119).
atm(train,d180,d180_2,c,22,-0.119).
atm(train,d180,d180_3,c,26,-0.029).
atm(train,d180,d180_4,c,26,0.111).
atm(train,d180,d180_5,c,22,-0.119).
atm(train,d180,d180_6,c,22,-0.119).
atm(train,d180,d180_7,c,21,-0.019).
atm(train,d180,d180_8,c,21,-0.029).
atm(train,d180,d180_9,n,34,-0.349).
atm(train,d180,d180_10,h,3,0.141).
atm(train,d180,d180_11,h,3,0.141).
atm(train,d180,d180_12,h,3,0.111).
atm(train,d180,d180_13,h,3,0.141).
atm(train,d180,d180_14,h,1,0.311).
atm(train,d180,d180_15,c,10,-0.089).
atm(train,d180,d180_16,c,10,-0.089).
atm(train,d180,d180_17,n,36,-0.289).
atm(train,d180,d180_18,c,14,0.792).
atm(train,d180,d180_19,o,45,-0.658).
atm(train,d180,d180_20,o,51,-0.598).
atm(train,d180,d180_21,h,1,0.261).
atm(train,d180,d180_22,h,1,0.161).
atm(train,d180,d180_23,h,1,0.161).
atm(train,d180,d180_24,h,3,0.061).
atm(train,d180,d180_25,h,3,0.061).
atm(train,d180,d180_26,h,3,0.061).
atm(train,d180,d180_27,h,3,0.111).
atm(train,d181,d181_1,c,22,-0.118).
atm(train,d181,d181_2,c,22,-0.118).
atm(train,d181,d181_3,c,22,-0.118).
atm(train,d181,d181_4,c,22,-0.118).
atm(train,d181,d181_5,c,22,-0.118).
atm(train,d181,d181_6,c,22,-0.118).
atm(train,d181,d181_7,h,3,0.142).
atm(train,d181,d181_8,h,3,0.142).
atm(train,d181,d181_9,h,3,0.142).
atm(train,d181,d181_10,h,3,0.142).
atm(train,d181,d181_11,h,3,0.142).
atm(train,d181,d181_12,c,10,0.102).
atm(train,d181,d181_13,o,45,-0.639).
atm(train,d181,d181_14,h,3,0.062).
atm(train,d181,d181_15,h,3,0.062).
atm(train,d181,d181_16,h,8,0.411).
atm(train,d182,d182_1,c,22,-0.143).
atm(train,d182,d182_2,c,22,-0.143).
atm(train,d182,d182_3,c,22,-0.143).
atm(train,d182,d182_4,c,22,0.167).
atm(train,d182,d182_5,c,22,-0.143).
atm(train,d182,d182_6,c,22,-0.143).
atm(train,d182,d182_7,h,3,0.117).
atm(train,d182,d182_8,h,3,0.117).
atm(train,d182,d182_9,h,3,0.117).
atm(train,d182,d182_10,h,3,0.117).
atm(train,d182,d182_11,h,3,0.117).
atm(train,d182,d182_12,o,50,-0.243).
atm(train,d182,d182_13,c,10,0.077).
atm(train,d182,d182_14,c,14,0.588).
atm(train,d182,d182_15,h,3,0.037).
atm(train,d182,d182_16,h,3,0.037).
atm(train,d182,d182_17,o,40,-0.562).
atm(train,d182,d182_18,n,32,-0.413).
atm(train,d182,d182_19,c,193,-0.113).
atm(train,d182,d182_20,h,1,0.237).
atm(train,d182,d182_21,c,192,0.568).
atm(train,d182,d182_22,c,193,-0.113).
atm(train,d182,d182_23,h,3,0.037).
atm(train,d182,d182_24,n,33,-0.263).
atm(train,d182,d182_25,o,40,-0.562).
atm(train,d182,d182_26,s,74,-0.113).
atm(train,d182,d182_27,c,10,-0.113).
atm(train,d182,d182_28,c,10,0.087).
atm(train,d182,d182_29,h,3,0.037).
atm(train,d182,d182_30,c,14,0.537).
atm(train,d182,d182_31,h,3,0.037).
atm(train,d182,d182_32,o,50,-0.243).
atm(train,d182,d182_33,o,51,-0.562).
atm(train,d182,d182_34,k,83,0.988).
atm(train,d182,d182_35,c,10,-0.113).
atm(train,d182,d182_36,h,3,0.037).
atm(train,d182,d182_37,h,3,0.037).
atm(train,d182,d182_38,h,3,0.037).
atm(train,d182,d182_39,c,10,-0.113).
atm(train,d182,d182_40,h,3,0.037).
atm(train,d182,d182_41,h,3,0.037).
atm(train,d182,d182_42,h,3,0.037).
atm(train,d183,d183_1,c,22,-0.106).
atm(train,d183,d183_2,c,27,0.024).
atm(train,d183,d183_3,c,22,0.024).
atm(train,d183,d183_4,c,22,0.024).
atm(train,d183,d183_5,c,22,0.275).
atm(train,d183,d183_6,c,22,-0.106).
atm(train,d183,d183_7,h,3,0.154).
atm(train,d183,d183_8,h,3,0.154).
atm(train,d183,d183_9,c,10,-0.076).
atm(train,d183,d183_10,c,10,0.124).
atm(train,d183,d183_11,c,16,-0.176).
atm(train,d183,d183_12,c,14,0.575).
atm(train,d183,d183_13,h,3,0.074).
atm(train,d183,d183_14,h,3,0.074).
atm(train,d183,d183_15,c,10,-0.076).
atm(train,d183,d183_16,c,10,-0.126).
atm(train,d183,d183_17,c,10,0.124).
atm(train,d183,d183_18,c,16,-0.076).
atm(train,d183,d183_19,h,3,0.124).
atm(train,d183,d183_20,h,3,0.074).
atm(train,d183,d183_21,h,3,0.074).
atm(train,d183,d183_22,c,10,0.124).
atm(train,d183,d183_23,c,16,-0.076).
atm(train,d183,d183_24,c,16,-0.175).
atm(train,d183,d183_25,c,14,0.605).
atm(train,d183,d183_26,c,10,0.114).
atm(train,d183,d183_27,h,3,0.074).
atm(train,d183,d183_28,h,3,0.074).
atm(train,d183,d183_29,h,3,0.074).
atm(train,d183,d183_30,o,45,-0.625).
atm(train,d183,d183_31,h,8,0.425).
atm(train,d183,d183_32,o,45,-0.625).
atm(train,d183,d183_33,h,8,0.425).
atm(train,d183,d183_34,o,40,-0.525).
atm(train,d183,d183_35,o,42,-0.555).
atm(train,d183,d183_36,o,45,-0.625).
atm(train,d183,d183_37,h,8,0.425).
atm(train,d183,d183_38,o,45,-0.625).
atm(train,d183,d183_39,h,3,-0.006).
atm(train,d183,d183_40,h,8,0.425).
atm(train,d183,d183_41,n,36,-0.275).
atm(train,d183,d183_42,h,3,0.074).
atm(train,d183,d183_43,c,10,-0.126).
atm(train,d183,d183_44,h,3,0.074).
atm(train,d183,d183_45,h,3,0.074).
atm(train,d183,d183_46,h,3,0.074).
atm(train,d183,d183_47,c,10,-0.126).
atm(train,d183,d183_48,h,3,0.074).
atm(train,d183,d183_49,h,3,0.074).
atm(train,d183,d183_50,h,3,0.074).
atm(train,d183,d183_51,c,14,0.575).
atm(train,d183,d183_52,n,32,-0.575).
atm(train,d183,d183_53,o,40,-0.525).
atm(train,d183,d183_54,h,1,0.325).
atm(train,d183,d183_55,h,1,0.325).
atm(train,d183,d183_56,o,45,-0.625).
atm(train,d183,d183_57,h,8,0.425).
atm(train,d184,d184_1,c,22,-0.136).
atm(train,d184,d184_2,c,22,-0.136).
atm(train,d184,d184_3,c,22,-0.136).
atm(train,d184,d184_4,c,22,-0.007).
atm(train,d184,d184_5,c,22,-0.136).
atm(train,d184,d184_6,c,22,-0.136).
atm(train,d184,d184_7,h,3,0.123).
atm(train,d184,d184_8,h,3,0.123).
atm(train,d184,d184_9,h,3,0.124).
atm(train,d184,d184_10,h,3,0.124).
atm(train,d184,d184_11,h,3,0.123).
atm(train,d184,d184_12,c,10,-0.107).
atm(train,d184,d184_13,c,10,0.043).
atm(train,d184,d184_14,h,3,0.043).
atm(train,d184,d184_15,h,3,0.043).
atm(train,d184,d184_16,c,10,-0.156).
atm(train,d184,d184_17,h,3,0.043).
atm(train,d184,d184_18,h,3,0.043).
atm(train,d184,d184_19,h,3,0.043).
atm(train,d184,d184_20,n,36,-0.306).
atm(train,d184,d184_21,h,3,0.093).
atm(train,d184,d184_22,h,1,0.144).
atm(train,d184,d184_23,h,1,0.144).
atm(train,d185,d185_1,c,10,-0.068).
atm(train,d185,d185_2,c,10,-0.068).
atm(train,d185,d185_3,c,14,0.731).
atm(train,d185,d185_4,h,3,0.082).
atm(train,d185,d185_5,h,3,0.082).
atm(train,d185,d185_6,c,14,0.731).
atm(train,d185,d185_7,h,3,0.082).
atm(train,d185,d185_8,h,3,0.082).
atm(train,d185,d185_9,o,51,-0.518).
atm(train,d185,d185_10,o,51,-0.518).
atm(train,d185,d185_11,o,49,-0.618).
atm(train,d186,d186a_1,c,10,0.191).
atm(train,d186,d186a_2,c,10,0.171).
atm(train,d186,d186a_3,h,3,0.06).
atm(train,d186,d186a_4,o,45,-0.649).
atm(train,d186,d186a_5,h,8,0.401).
atm(train,d186,d186a_6,o,45,-0.649).
atm(train,d186,d186a_7,c,10,-0.1).
atm(train,d186,d186a_8,h,3,0.05).
atm(train,d186,d186a_9,h,3,0.05).
atm(train,d186,d186a_10,h,3,0.05).
atm(train,d186,d186a_11,h,8,0.401).
atm(train,d186,d186a_12,c,10,0.251).
atm(train,d186,d186a_13,o,49,-0.649).
atm(train,d186,d186a_14,c,14,0.701).
atm(train,d186,d186a_15,h,3,0.181).
atm(train,d186,d186a_16,c,10,-0.099).
atm(train,d186,d186a_17,c,10,-0.149).
atm(train,d186,d186a_18,h,3,0.05).
atm(train,d186,d186a_19,h,3,0.05).
atm(train,d186,d186a_20,h,3,0.05).
atm(train,d186,d186a_21,h,3,0.05).
atm(train,d186,d186a_22,h,3,0.05).
atm(train,d186,d186a_23,o,51,-0.549).
atm(train,d186,d186a_24,c,10,-0.1).
atm(train,d186,d186a_25,c,10,0.141).
atm(train,d186,d186a_26,c,10,-0.149).
atm(train,d186,d186a_27,h,3,0.05).
atm(train,d186,d186a_28,h,3,0.05).
atm(train,d186,d186a_29,h,3,0.05).
atm(train,d186,d186a_30,h,3,0.08).
atm(train,d186,d186a_31,c,10,-0.149).
atm(train,d186,d186a_32,c,10,0.141).
atm(train,d186,d186a_33,h,3,0.035).
atm(train,d186,d186a_34,o,50,-0.369).
atm(train,d186,d186a_35,c,10,0.311).
atm(train,d186,d186a_36,o,50,-0.369).
atm(train,d186,d186a_37,c,10,0.141).
atm(train,d186,d186a_38,c,10,0.191).
atm(train,d186,d186a_39,c,10,-0.099).
atm(train,d186,d186a_40,c,10,-0.1).
atm(train,d186,d186a_41,h,3,0.05).
atm(train,d186,d186a_42,h,3,0.05).
atm(train,d186,d186a_43,c,10,-0.149).
atm(train,d186,d186a_44,h,3,0.035).
atm(train,d186,d186a_45,h,3,0.05).
atm(train,d186,d186a_46,h,3,0.05).
atm(train,d186,d186a_47,h,3,0.05).
atm(train,d186,d186a_48,o,45,-0.649).
atm(train,d186,d186a_49,h,3,0.06).
atm(train,d186,d186a_50,h,8,0.401).
atm(train,d186,d186a_51,o,50,-0.369).
atm(train,d186,d186a_52,c,10,0.02).
atm(train,d186,d186a_53,h,3,0.06).
atm(train,d186,d186a_54,h,3,0.06).
atm(train,d186,d186a_55,h,3,0.06).
atm(train,d186,d186a_56,c,10,-0.1).
atm(train,d186,d186a_57,h,3,0.05).
atm(train,d186,d186a_58,h,3,0.05).
atm(train,d186,d186a_59,h,3,0.05).
atm(train,d186,d186a_60,h,3,0.05).
atm(train,d186,d186a_61,c,10,-0.149).
atm(train,d186,d186a_62,h,3,0.05).
atm(train,d186,d186a_63,h,3,0.05).
atm(train,d186,d186a_64,h,3,0.05).
atm(train,d186,d186a_65,h,3,-0.03).
atm(train,d186,d186a_66,c,10,0.171).
atm(train,d186,d186a_67,c,10,-0.1).
atm(train,d186,d186a_68,c,14,0.581).
atm(train,d186,d186a_69,o,42,-0.579).
atm(train,d186,d186a_70,c,10,-0.099).
atm(train,d186,d186a_71,c,10,-0.099).
atm(train,d186,d186a_72,h,3,0.05).
atm(train,d186,d186a_73,h,3,0.05).
atm(train,d186,d186a_74,c,10,-0.149).
atm(train,d186,d186a_75,h,3,0.05).
atm(train,d186,d186a_76,h,3,0.05).
atm(train,d186,d186a_77,h,3,0.05).
atm(train,d186,d186a_78,h,3,0.08).
atm(train,d186,d186a_79,c,10,-0.149).
atm(train,d186,d186a_80,h,3,0.05).
atm(train,d186,d186a_81,h,3,0.05).
atm(train,d186,d186a_82,h,3,0.05).
atm(train,d186,d186a_83,h,3,0.08).
atm(train,d186,d186a_84,c,10,-0.1).
atm(train,d186,d186a_85,h,3,0.05).
atm(train,d186,d186a_86,h,3,0.05).
atm(train,d186,d186a_87,h,3,0.05).
atm(train,d186,d186a_88,o,45,-0.649).
atm(train,d186,d186a_89,h,8,0.401).
atm(train,d186,d186a_90,h,3,0.035).
atm(train,d186,d186a_91,o,50,-0.369).
atm(train,d186,d186a_92,c,10,0.311).
atm(train,d186,d186a_93,o,50,-0.369).
atm(train,d186,d186a_94,c,10,0.141).
atm(train,d186,d186a_95,c,10,-0.1).
atm(train,d186,d186a_96,c,10,0.05).
atm(train,d186,d186a_97,c,10,0.191).
atm(train,d186,d186a_98,h,3,0.05).
atm(train,d186,d186a_99,h,3,0.05).
atm(train,d186,d186a_100,h,3,0.05).
atm(train,d186,d186a_101,o,45,-0.649).
atm(train,d186,d186a_102,h,3,0.06).
atm(train,d186,d186a_103,h,8,0.401).
atm(train,d186,d186a_104,n,36,-0.299).
atm(train,d186,d186a_105,h,3,0.101).
atm(train,d186,d186a_106,c,10,0.101).
atm(train,d186,d186a_107,h,3,0.05).
atm(train,d186,d186a_108,h,3,0.05).
atm(train,d186,d186a_109,h,3,0.05).
atm(train,d186,d186a_110,c,10,0.101).
atm(train,d186,d186a_111,h,3,0.05).
atm(train,d186,d186a_112,h,3,0.05).
atm(train,d186,d186a_113,h,3,0.05).
atm(train,d186,d186a_114,c,10,0.101).
atm(train,d186,d186a_115,h,3,0.05).
atm(train,d186,d186a_116,h,3,0.05).
atm(train,d186,d186a_117,h,3,0.05).
atm(train,d186,d186a_118,c,10,-0.149).
atm(train,d186,d186a_119,h,3,0.035).
atm(train,d186,d186a_120,h,3,0.05).
atm(train,d186,d186a_121,h,3,0.05).
atm(train,d186,d186a_122,h,3,0.05).
atm(train,d186,d186b_1,c,10,-0.145).
atm(train,d186,d186b_2,c,10,-0.095).
atm(train,d186,d186b_3,h,3,0.054).
atm(train,d186,d186b_4,h,3,0.055).
atm(train,d186,d186b_5,h,3,0.055).
atm(train,d186,d186b_6,c,10,-0.095).
atm(train,d186,d186b_7,h,3,0.054).
atm(train,d186,d186b_8,h,3,0.054).
atm(train,d186,d186b_9,c,10,-0.095).
atm(train,d186,d186b_10,h,3,0.054).
atm(train,d186,d186b_11,h,3,0.054).
atm(train,d186,d186b_12,c,10,-0.095).
atm(train,d186,d186b_13,h,3,0.054).
atm(train,d186,d186b_14,h,3,0.054).
atm(train,d186,d186b_15,c,10,-0.095).
atm(train,d186,d186b_16,h,3,0.054).
atm(train,d186,d186b_17,h,3,0.054).
atm(train,d186,d186b_18,c,10,-0.095).
atm(train,d186,d186b_19,h,3,0.054).
atm(train,d186,d186b_20,h,3,0.054).
atm(train,d186,d186b_21,c,10,-0.095).
atm(train,d186,d186b_22,h,3,0.054).
atm(train,d186,d186b_23,h,3,0.055).
atm(train,d186,d186b_24,c,10,-0.095).
atm(train,d186,d186b_25,h,3,0.055).
atm(train,d186,d186b_26,h,3,0.055).
atm(train,d186,d186b_27,c,10,-0.095).
atm(train,d186,d186b_28,h,3,0.054).
atm(train,d186,d186b_29,h,3,0.054).
atm(train,d186,d186b_30,c,10,-0.095).
atm(train,d186,d186b_31,h,3,0.054).
atm(train,d186,d186b_32,h,3,0.054).
atm(train,d186,d186b_33,c,10,-0.095).
atm(train,d186,d186b_34,h,3,0.054).
atm(train,d186,d186b_35,h,3,0.054).
atm(train,d186,d186b_36,c,10,-0.095).
atm(train,d186,d186b_37,h,3,0.054).
atm(train,d186,d186b_38,h,3,0.054).
atm(train,d186,d186b_39,c,10,-0.095).
atm(train,d186,d186b_40,h,3,0.054).
atm(train,d186,d186b_41,h,3,0.054).
atm(train,d186,d186b_42,c,10,-0.095).
atm(train,d186,d186b_43,h,3,0.054).
atm(train,d186,d186b_44,h,3,0.054).
atm(train,d186,d186b_45,c,10,-0.095).
atm(train,d186,d186b_46,h,3,0.054).
atm(train,d186,d186b_47,h,3,0.054).
atm(train,d186,d186b_48,c,10,-0.095).
atm(train,d186,d186b_49,h,3,0.054).
atm(train,d186,d186b_50,h,3,0.054).
atm(train,d186,d186b_51,c,14,0.785).
atm(train,d186,d186b_52,h,3,0.054).
atm(train,d186,d186b_53,h,3,0.054).
atm(train,d186,d186b_54,o,45,-0.665).
atm(train,d186,d186b_55,o,51,-0.605).
atm(train,d186,d186b_56,h,1,0.255).
atm(train,d187,d187_1,c,22,-0.121).
atm(train,d187,d187_2,c,22,-0.121).
atm(train,d187,d187_3,c,22,-0.121).
atm(train,d187,d187_4,c,22,-0.121).
atm(train,d187,d187_5,c,22,-0.121).
atm(train,d187,d187_6,c,22,0.259).
atm(train,d187,d187_7,h,3,0.139).
atm(train,d187,d187_8,h,3,0.139).
atm(train,d187,d187_9,h,3,0.139).
atm(train,d187,d187_10,h,3,0.139).
atm(train,d187,d187_11,o,45,-0.64).
atm(train,d187,d187_12,c,10,0.009).
atm(train,d187,d187_13,o,45,-0.64).
atm(train,d187,d187_14,c,10,0.009).
atm(train,d187,d187_15,h,3,0.059).
atm(train,d187,d187_16,n,36,-0.29).
atm(train,d187,d187_17,h,3,0.059).
atm(train,d187,d187_18,h,3,0.059).
atm(train,d187,d187_19,c,10,0.009).
atm(train,d187,d187_20,h,3,0.059).
atm(train,d187,d187_21,h,3,0.059).
atm(train,d187,d187_22,h,3,0.059).
atm(train,d187,d187_23,h,8,0.41).
atm(train,d187,d187_24,h,1,0.159).
atm(train,d187,d187_25,h,8,0.41).
atm(train,d188,d188a_1,c,22,-0.127).
atm(train,d188,d188a_2,c,22,-0.127).
atm(train,d188,d188a_3,c,22,-0.176).
atm(train,d188,d188a_4,c,22,-0.127).
atm(train,d188,d188a_5,c,22,-0.176).
atm(train,d188,d188a_6,c,22,-0.127).
atm(train,d188,d188a_7,h,3,0.134).
atm(train,d188,d188a_8,h,3,0.134).
atm(train,d188,d188a_9,h,3,0.134).
atm(train,d188,d188a_10,h,3,0.134).
atm(train,d188,d188a_11,c,10,0.003).
atm(train,d188,d188a_12,h,3,0.053).
atm(train,d188,d188a_13,h,3,0.053).
atm(train,d188,d188a_14,h,3,0.053).
atm(train,d188,d188a_15,c,10,0.003).
atm(train,d188,d188a_16,h,3,0.053).
atm(train,d188,d188a_17,h,3,0.053).
atm(train,d188,d188a_18,h,3,0.053).
atm(train,d188,d188b_1,c,22,-0.127).
atm(train,d188,d188b_2,c,22,-0.176).
atm(train,d188,d188b_3,c,22,-0.127).
atm(train,d188,d188b_4,c,22,-0.127).
atm(train,d188,d188b_5,c,22,-0.176).
atm(train,d188,d188b_6,c,22,-0.127).
atm(train,d188,d188b_7,h,3,0.134).
atm(train,d188,d188b_8,h,3,0.134).
atm(train,d188,d188b_9,h,3,0.134).
atm(train,d188,d188b_10,c,10,0.003).
atm(train,d188,d188b_11,h,3,0.053).
atm(train,d188,d188b_12,h,3,0.053).
atm(train,d188,d188b_13,h,3,0.053).
atm(train,d188,d188b_14,h,3,0.134).
atm(train,d188,d188b_15,c,10,0.003).
atm(train,d188,d188b_16,h,3,0.053).
atm(train,d188,d188b_17,h,3,0.053).
atm(train,d188,d188b_18,h,3,0.053).
atm(train,d188,d188c_1,c,22,-0.127).
atm(train,d188,d188c_2,c,22,-0.127).
atm(train,d188,d188c_3,c,22,-0.127).
atm(train,d188,d188c_4,c,22,-0.176).
atm(train,d188,d188c_5,c,22,-0.176).
atm(train,d188,d188c_6,c,22,-0.127).
atm(train,d188,d188c_7,h,3,0.134).
atm(train,d188,d188c_8,h,3,0.134).
atm(train,d188,d188c_9,h,3,0.134).
atm(train,d188,d188c_10,h,3,0.134).
atm(train,d188,d188c_11,c,10,0.003).
atm(train,d188,d188c_12,h,3,0.053).
atm(train,d188,d188c_13,h,3,0.053).
atm(train,d188,d188c_14,h,3,0.053).
atm(train,d188,d188c_15,c,10,0.003).
atm(train,d188,d188c_16,h,3,0.053).
atm(train,d188,d188c_17,h,3,0.053).
atm(train,d188,d188c_18,h,3,0.053).
atm(train,d188,d188d_1,c,22,-0.132).
atm(train,d188,d188d_2,c,22,-0.132).
atm(train,d188,d188d_3,c,22,-0.132).
atm(train,d188,d188d_4,c,22,-0.133).
atm(train,d188,d188d_5,c,22,-0.003).
atm(train,d188,d188d_6,c,22,-0.132).
atm(train,d188,d188d_7,h,3,0.127).
atm(train,d188,d188d_8,h,3,0.127).
atm(train,d188,d188d_9,h,3,0.127).
atm(train,d188,d188d_10,h,3,0.127).
atm(train,d188,d188d_11,h,3,0.127).
atm(train,d188,d188d_12,c,10,-0.103).
atm(train,d188,d188d_13,c,10,-0.103).
atm(train,d188,d188d_14,h,3,0.047).
atm(train,d188,d188d_15,h,3,0.047).
atm(train,d188,d188d_16,h,3,0.047).
atm(train,d188,d188d_17,h,3,0.047).
atm(train,d188,d188d_18,h,3,0.047).
atm(train,d189,d189_1,c,22,-0.12).
atm(train,d189,d189_2,c,22,-0.12).
atm(train,d189,d189_3,c,22,-0.12).
atm(train,d189,d189_4,c,22,-0.12).
atm(train,d189,d189_5,c,22,-0.121).
atm(train,d189,d189_6,c,22,-0.12).
atm(train,d189,d189_7,h,3,0.139).
atm(train,d189,d189_8,h,3,0.139).
atm(train,d189,d189_9,h,3,0.139).
atm(train,d189,d189_10,h,3,0.139).
atm(train,d189,d189_11,h,3,0.139).
atm(train,d189,d189_12,c,10,0.01).
atm(train,d189,d189_13,c,10,0.01).
atm(train,d189,d189_14,n,36,-0.291).
atm(train,d189,d189_15,h,3,0.06).
atm(train,d189,d189_16,h,3,0.06).
atm(train,d189,d189_17,c,10,0.01).
atm(train,d189,d189_18,h,3,0.06).
atm(train,d189,d189_19,h,3,0.06).
atm(train,d189,d189_20,h,3,0.06).
atm(train,d189,d189_21,o,45,-0.641).
atm(train,d189,d189_22,h,3,0.06).
atm(train,d189,d189_23,h,8,0.409).
atm(train,d189,d189_24,h,1,0.159).
atm(train,d19,d19_1,c,22,-0.125).
atm(train,d19,d19_2,c,22,-0.125).
atm(train,d19,d19_3,c,27,0.005).
atm(train,d19,d19_4,c,27,0.005).
atm(train,d19,d19_5,c,22,0.005).
atm(train,d19,d19_6,c,22,-0.124).
atm(train,d19,d19_7,h,3,0.136).
atm(train,d19,d19_8,h,3,0.136).
atm(train,d19,d19_9,h,3,0.136).
atm(train,d19,d19_10,c,22,-0.125).
atm(train,d19,d19_11,c,22,-0.124).
atm(train,d19,d19_12,c,22,-0.125).
atm(train,d19,d19_13,c,22,0.005).
atm(train,d19,d19_14,h,3,0.136).
atm(train,d19,d19_15,h,3,0.136).
atm(train,d19,d19_16,c,10,-0.095).
atm(train,d19,d19_17,c,10,-0.095).
atm(train,d19,d19_18,h,3,0.055).
atm(train,d19,d19_19,h,3,0.055).
atm(train,d19,d19_20,h,3,0.055).
atm(train,d19,d19_21,h,3,0.055).
atm(train,d19,d19_22,n,38,0.806).
atm(train,d19,d19_23,o,40,-0.394).
atm(train,d19,d19_24,o,40,-0.394).
atm(train,d190,d190_1,c,22,-0.128).
atm(train,d190,d190_2,c,22,-0.128).
atm(train,d190,d190_3,c,22,-0.128).
atm(train,d190,d190_4,c,22,-0.128).
atm(train,d190,d190_5,c,22,-0.178).
atm(train,d190,d190_6,c,22,-0.128).
atm(train,d190,d190_7,h,3,0.132).
atm(train,d190,d190_8,h,3,0.132).
atm(train,d190,d190_9,h,3,0.132).
atm(train,d190,d190_10,h,3,0.132).
atm(train,d190,d190_11,h,3,0.132).
atm(train,d190,d190_12,c,10,0.002).
atm(train,d190,d190_13,h,3,0.052).
atm(train,d190,d190_14,h,3,0.052).
atm(train,d190,d190_15,h,3,0.052).
atm(train,d191,d191_1,c,10,-0.126).
atm(train,d191,d191_2,c,10,-0.126).
atm(train,d191,d191_3,h,3,0.075).
atm(train,d191,d191_4,h,3,0.075).
atm(train,d191,d191_5,h,3,0.075).
atm(train,d191,d191_6,c,10,-0.126).
atm(train,d191,d191_7,h,3,0.075).
atm(train,d191,d191_8,h,3,0.075).
atm(train,d191,d191_9,h,3,0.075).
atm(train,d191,d191_10,c,10,-0.075).
atm(train,d191,d191_11,h,3,-0.005).
atm(train,d191,d191_12,c,14,0.724).
atm(train,d191,d191_13,h,3,0.075).
atm(train,d191,d191_14,h,3,0.075).
atm(train,d191,d191_15,o,51,-0.526).
atm(train,d191,d191_16,o,49,-0.626).
atm(train,d191,d191_17,c,10,0.115).
atm(train,d191,d191_18,c,16,-0.176).
atm(train,d191,d191_19,h,3,0.075).
atm(train,d191,d191_20,h,3,0.075).
atm(train,d191,d191_21,c,16,-0.176).
atm(train,d191,d191_22,h,3,0.124).
atm(train,d191,d191_23,h,3,0.125).
atm(train,d191,d191_24,h,3,0.124).
atm(train,d192,d192_1,c,22,-0.12).
atm(train,d192,d192_2,c,22,-0.12).
atm(train,d192,d192_3,c,22,-0.12).
atm(train,d192,d192_4,c,22,0.01).
atm(train,d192,d192_5,c,22,0.209).
atm(train,d192,d192_6,c,22,-0.12).
atm(train,d192,d192_7,h,3,0.14).
atm(train,d192,d192_8,h,3,0.14).
atm(train,d192,d192_9,h,3,0.14).
atm(train,d192,d192_10,h,3,0.139).
atm(train,d192,d192_11,n,32,-0.771).
atm(train,d192,d192_12,c,14,0.709).
atm(train,d192,d192_13,o,49,-0.641).
atm(train,d192,d192_14,c,10,0.1).
atm(train,d192,d192_15,c,16,-0.191).
atm(train,d192,d192_16,h,3,0.06).
atm(train,d192,d192_17,h,3,0.06).
atm(train,d192,d192_18,c,16,-0.191).
atm(train,d192,d192_19,h,1,0.339).
atm(train,d192,d192_20,h,1,0.339).
atm(train,d192,d192_21,o,51,-0.541).
atm(train,d192,d192_22,h,3,0.11).
atm(train,d192,d192_23,c,22,0.11).
atm(train,d192,d192_24,h,3,0.11).
atm(train,d192,d192_25,c,22,-0.12).
atm(train,d192,d192_26,c,22,-0.12).
atm(train,d192,d192_27,c,22,-0.12).
atm(train,d192,d192_28,c,22,-0.12).
atm(train,d192,d192_29,c,22,-0.12).
atm(train,d192,d192_30,h,3,0.14).
atm(train,d192,d192_31,h,3,0.14).
atm(train,d192,d192_32,h,3,0.14).
atm(train,d192,d192_33,h,3,0.14).
atm(train,d192,d192_34,h,3,0.14).
atm(train,d193,d193_1,c,10,0.033).
atm(train,d193,d193_2,o,50,-0.358).
atm(train,d193,d193_3,c,10,0.033).
atm(train,d193,d193_4,c,10,0.033).
atm(train,d193,d193_5,o,50,-0.358).
atm(train,d193,d193_6,c,10,0.033).
atm(train,d193,d193_7,h,3,0.073).
atm(train,d193,d193_8,h,3,0.073).
atm(train,d193,d193_9,h,3,0.073).
atm(train,d193,d193_10,h,3,0.073).
atm(train,d193,d193_11,h,3,0.073).
atm(train,d193,d193_12,h,3,0.073).
atm(train,d193,d193_13,h,3,0.073).
atm(train,d193,d193_14,h,3,0.073).
atm(train,d194,d194_1,s,70,-0.65).
atm(train,d194,d194_2,se,102,0.65).
atm(train,d195,d195_1,c,22,-0.14).
atm(train,d195,d195_2,c,22,-0.14).
atm(train,d195,d195_3,c,26,0.091).
atm(train,d195,d195_4,c,26,0.091).
atm(train,d195,d195_5,c,22,-0.14).
atm(train,d195,d195_6,c,22,-0.14).
atm(train,d195,d195_7,c,21,-0.109).
atm(train,d195,d195_8,c,21,-0.14).
atm(train,d195,d195_9,o,52,-0.039).
atm(train,d195,d195_10,h,3,0.121).
atm(train,d195,d195_11,h,3,0.121).
atm(train,d195,d195_12,h,3,0.091).
atm(train,d195,d195_13,h,3,0.121).
atm(train,d195,d195_14,h,3,0.091).
atm(train,d195,d195_15,h,3,0.121).
atm(train,d196,d196_1,c,21,0.184).
atm(train,d196,d196_2,c,21,-0.145).
atm(train,d196,d196_3,c,21,-0.146).
atm(train,d196,d196_4,c,21,-0.145).
atm(train,d196,d196_5,o,52,-0.045).
atm(train,d196,d196_6,c,14,0.214).
atm(train,d196,d196_7,o,41,-0.446).
atm(train,d196,d196_8,h,3,0.184).
atm(train,d196,d196_9,h,3,0.115).
atm(train,d196,d196_10,h,3,0.115).
atm(train,d196,d196_11,h,3,0.115).
atm(train,d197,d197_1,c,22,-0.122).
atm(train,d197,d197_2,c,22,0.208).
atm(train,d197,d197_3,c,22,-0.122).
atm(train,d197,d197_4,c,22,-0.122).
atm(train,d197,d197_5,c,22,-0.122).
atm(train,d197,d197_6,c,22,-0.122).
atm(train,d197,d197_7,h,3,0.138).
atm(train,d197,d197_8,h,3,0.138).
atm(train,d197,d197_9,h,3,0.138).
atm(train,d197,d197_10,h,3,0.138).
atm(train,d197,d197_11,h,3,0.138).
atm(train,d197,d197_12,c,22,-0.122).
atm(train,d197,d197_13,c,22,-0.122).
atm(train,d197,d197_14,c,22,-0.122).
atm(train,d197,d197_15,c,22,0.208).
atm(train,d197,d197_16,c,22,-0.122).
atm(train,d197,d197_17,c,22,-0.122).
atm(train,d197,d197_18,h,3,0.138).
atm(train,d197,d197_19,h,3,0.138).
atm(train,d197,d197_20,h,3,0.138).
atm(train,d197,d197_21,h,3,0.138).
atm(train,d197,d197_22,h,3,0.138).
atm(train,d197,d197_23,n,32,-0.391).
atm(train,d197,d197_24,n,32,-0.391).
atm(train,d197,d197_25,c,14,0.609).
atm(train,d197,d197_26,c,10,-0.092).
atm(train,d197,d197_27,c,14,0.609).
atm(train,d197,d197_28,c,10,-0.092).
atm(train,d197,d197_29,h,3,0.058).
atm(train,d197,d197_30,c,10,-0.092).
atm(train,d197,d197_31,h,3,0.058).
atm(train,d197,d197_32,h,3,0.058).
atm(train,d197,d197_33,c,10,-0.092).
atm(train,d197,d197_34,h,3,0.058).
atm(train,d197,d197_35,h,3,0.058).
atm(train,d197,d197_36,c,10,-0.142).
atm(train,d197,d197_37,h,3,0.058).
atm(train,d197,d197_38,h,3,0.058).
atm(train,d197,d197_39,o,40,-0.541).
atm(train,d197,d197_40,o,40,-0.541).
atm(train,d197,d197_41,h,3,0.058).
atm(train,d197,d197_42,h,3,0.058).
atm(train,d197,d197_43,h,3,0.058).
atm(train,d198,d198_1,c,10,-0.11).
atm(train,d198,d198_2,c,10,-0.01).
atm(train,d198,d198_3,h,3,0.04).
atm(train,d198,d198_4,h,3,0.04).
atm(train,d198,d198_5,h,3,0.04).
atm(train,d198,d198_6,n,36,-0.41).
atm(train,d198,d198_7,h,3,0.04).
atm(train,d198,d198_8,h,3,0.04).
atm(train,d198,d198_9,c,14,0.54).
atm(train,d198,d198_10,n,36,-0.41).
atm(train,d198,d198_11,h,1,0.14).
atm(train,d198,d198_12,c,10,-0.01).
atm(train,d198,d198_13,h,1,0.14).
atm(train,d198,d198_14,c,10,-0.11).
atm(train,d198,d198_15,h,3,0.04).
atm(train,d198,d198_16,h,3,0.04).
atm(train,d198,d198_17,h,3,0.04).
atm(train,d198,d198_18,h,3,0.04).
atm(train,d198,d198_19,h,3,0.04).
atm(train,d198,d198_20,s,75,-0.16).
atm(train,d199,d199_1,c,22,-0.092).
atm(train,d199,d199_2,c,22,-0.092).
atm(train,d199,d199_3,c,22,-0.092).
atm(train,d199,d199_4,c,22,0.238).
atm(train,d199,d199_5,c,22,-0.092).
atm(train,d199,d199_6,c,22,-0.092).
atm(train,d199,d199_7,h,3,0.168).
atm(train,d199,d199_8,h,3,0.168).
atm(train,d199,d199_9,h,3,0.168).
atm(train,d199,d199_10,h,3,0.168).
atm(train,d199,d199_11,h,3,0.168).
atm(train,d199,d199_12,n,32,-0.362).
atm(train,d199,d199_13,c,22,0.238).
atm(train,d199,d199_14,c,22,-0.092).
atm(train,d199,d199_15,c,22,-0.092).
atm(train,d199,d199_16,c,22,-0.092).
atm(train,d199,d199_17,c,22,-0.092).
atm(train,d199,d199_18,c,22,-0.092).
atm(train,d199,d199_19,h,3,0.168).
atm(train,d199,d199_20,h,3,0.168).
atm(train,d199,d199_21,h,3,0.168).
atm(train,d199,d199_22,h,3,0.168).
atm(train,d199,d199_23,h,3,0.168).
atm(train,d199,d199_24,n,32,-0.362).
atm(train,d199,d199_25,o,40,-0.512).
atm(train,d2,d2_1,c,22,-0.151).
atm(train,d2,d2_2,c,22,-0.151).
atm(train,d2,d2_3,c,26,0.079).
atm(train,d2,d2_4,c,26,0.279).
atm(train,d2,d2_5,c,22,-0.151).
atm(train,d2,d2_6,c,22,-0.151).
atm(train,d2,d2_7,h,3,0.109).
atm(train,d2,d2_8,h,3,0.109).
atm(train,d2,d2_9,h,3,0.109).
atm(train,d2,d2_10,h,3,0.109).
atm(train,d2,d2_11,c,26,0.079).
atm(train,d2,d2_12,c,26,0.279).
atm(train,d2,d2_13,n,34,-0.421).
atm(train,d2,d2_14,c,22,-0.151).
atm(train,d2,d2_15,c,22,-0.151).
atm(train,d2,d2_16,c,22,0.179).
atm(train,d2,d2_17,c,22,-0.151).
atm(train,d2,d2_18,h,3,0.109).
atm(train,d2,d2_19,h,3,0.109).
atm(train,d2,d2_20,h,3,0.109).
atm(train,d2,d2_21,c,10,-0.021).
atm(train,d2,d2_22,c,10,-0.121).
atm(train,d2,d2_23,h,3,0.029).
atm(train,d2,d2_24,h,3,0.029).
atm(train,d2,d2_25,h,3,0.029).
atm(train,d2,d2_26,h,3,0.029).
atm(train,d2,d2_27,h,3,0.029).
atm(train,d2,d2_28,n,32,-0.801).
atm(train,d2,d2_29,h,1,0.309).
atm(train,d2,d2_30,h,1,0.309).
atm(train,d20,d20_1,c,22,-0.137).
atm(train,d20,d20_2,c,22,-0.137).
atm(train,d20,d20_3,c,22,-0.137).
atm(train,d20,d20_4,c,22,0.173).
atm(train,d20,d20_5,c,22,0.193).
atm(train,d20,d20_6,c,22,-0.137).
atm(train,d20,d20_7,h,3,0.123).
atm(train,d20,d20_8,h,3,0.123).
atm(train,d20,d20_9,h,3,0.123).
atm(train,d20,d20_10,o,50,-0.237).
atm(train,d20,d20_11,n,32,-0.787).
atm(train,d20,d20_12,n,38,0.793).
atm(train,d20,d20_13,c,10,0.083).
atm(train,d20,d20_14,h,3,0.043).
atm(train,d20,d20_15,h,3,0.043).
atm(train,d20,d20_16,h,3,0.043).
atm(train,d20,d20_17,o,40,-0.407).
atm(train,d20,d20_18,o,40,-0.407).
atm(train,d20,d20_19,h,1,0.323).
atm(train,d20,d20_20,h,1,0.323).
atm(train,d200,d200_1,c,22,-0.13).
atm(train,d200,d200_2,c,22,-0.13).
atm(train,d200,d200_3,c,22,-0.13).
atm(train,d200,d200_4,c,22,-0.13).
atm(train,d200,d200_5,c,22,0.0).
atm(train,d200,d200_6,c,22,-0.13).
atm(train,d200,d200_7,h,3,0.13).
atm(train,d200,d200_8,h,3,0.13).
atm(train,d200,d200_9,h,3,0.13).
atm(train,d200,d200_10,h,3,0.13).
atm(train,d200,d200_11,h,3,0.13).
atm(train,d200,d200_12,c,14,0.23).
atm(train,d200,d200_13,o,41,-0.43).
atm(train,d200,d200_14,h,3,0.2).
atm(train,d201,d201_1,c,29,-0.005).
atm(train,d201,d201_2,c,27,-0.105).
atm(train,d201,d201_3,c,27,-0.105).
atm(train,d201,d201_4,c,22,0.095).
atm(train,d201,d201_5,c,22,-0.136).
atm(train,d201,d201_6,c,22,-0.136).
atm(train,d201,d201_7,c,29,-0.005).
atm(train,d201,d201_8,c,29,-0.005).
atm(train,d201,d201_9,c,22,0.095).
atm(train,d201,d201_10,c,14,0.544).
atm(train,d201,d201_11,c,22,0.095).
atm(train,d201,d201_12,c,22,-0.135).
atm(train,d201,d201_13,c,22,-0.135).
atm(train,d201,d201_14,c,22,-0.135).
atm(train,d201,d201_15,c,22,-0.135).
atm(train,d201,d201_16,c,22,-0.135).
atm(train,d201,d201_17,c,22,-0.136).
atm(train,d201,d201_18,o,40,-0.556).
atm(train,d201,d201_19,c,29,-0.005).
atm(train,d201,d201_20,c,22,0.095).
atm(train,d201,d201_21,c,14,0.544).
atm(train,d201,d201_22,c,22,-0.135).
atm(train,d201,d201_23,c,22,-0.136).
atm(train,d201,d201_24,c,22,-0.136).
atm(train,d201,d201_25,c,22,-0.136).
atm(train,d201,d201_26,h,3,0.125).
atm(train,d201,d201_27,h,3,0.125).
atm(train,d201,d201_28,h,3,0.125).
atm(train,d201,d201_29,h,3,0.125).
atm(train,d201,d201_30,h,3,0.125).
atm(train,d201,d201_31,h,3,0.125).
atm(train,d201,d201_32,h,3,0.125).
atm(train,d201,d201_33,h,3,0.125).
atm(train,d201,d201_34,h,3,0.125).
atm(train,d201,d201_35,h,3,0.125).
atm(train,d201,d201_36,h,3,0.125).
atm(train,d201,d201_37,h,3,0.125).
atm(train,d201,d201_38,o,40,-0.556).
atm(train,d202,d202_1,c,10,-0.299).
atm(train,d202,d202_2,c,10,0.101).
atm(train,d202,d202_3,c,16,-0.199).
atm(train,d202,d202_4,c,16,-0.199).
atm(train,d202,d202_5,c,14,0.581).
atm(train,d202,d202_6,c,10,-0.099).
atm(train,d202,d202_7,h,3,0.052).
atm(train,d202,d202_8,h,3,0.052).
atm(train,d202,d202_9,h,3,0.052).
atm(train,d202,d202_10,h,3,0.052).
atm(train,d202,d202_11,c,10,-0.088).
atm(train,d202,d202_12,h,3,0.052).
atm(train,d202,d202_13,h,3,0.052).
atm(train,d202,d202_14,h,3,0.052).
atm(train,d202,d202_15,c,10,-0.088).
atm(train,d202,d202_16,h,3,0.052).
atm(train,d202,d202_17,h,3,0.052).
atm(train,d202,d202_18,h,3,0.052).
atm(train,d202,d202_19,c,10,0.092).
atm(train,d202,d202_20,h,3,0.052).
atm(train,d202,d202_21,h,3,0.052).
atm(train,d202,d202_22,h,3,0.052).
atm(train,d202,d202_23,o,42,-0.579).
atm(train,d202,d202_24,h,3,0.101).
atm(train,d203,d203_1,c,22,0.874).
atm(train,d203,d203_2,n,35,-0.766).
atm(train,d203,d203_3,c,22,0.874).
atm(train,d203,d203_4,n,35,-0.766).
atm(train,d203,d203_5,c,22,0.874).
atm(train,d203,d203_6,n,35,-0.766).
atm(train,d203,d203_7,n,32,-0.776).
atm(train,d203,d203_8,n,32,-0.776).
atm(train,d203,d203_9,n,32,-0.776).
atm(train,d203,d203_10,h,1,0.334).
atm(train,d203,d203_11,h,1,0.334).
atm(train,d203,d203_12,h,1,0.334).
atm(train,d203,d203_13,h,1,0.334).
atm(train,d203,d203_14,h,1,0.334).
atm(train,d203,d203_15,h,1,0.334).
atm(train,d204,d204_1,n,36,-0.334).
atm(train,d204,d204_2,c,10,0.055).
atm(train,d204,d204_3,h,3,0.015).
atm(train,d204,d204_4,h,3,0.015).
atm(train,d204,d204_5,h,3,0.015).
atm(train,d204,d204_6,c,14,0.516).
atm(train,d204,d204_7,n,36,-0.334).
atm(train,d204,d204_8,c,10,-0.035).
atm(train,d204,d204_9,h,3,0.015).
atm(train,d204,d204_10,h,3,0.015).
atm(train,d204,d204_11,h,3,0.015).
atm(train,d204,d204_12,c,10,-0.035).
atm(train,d204,d204_13,h,3,0.015).
atm(train,d204,d204_14,h,3,0.015).
atm(train,d204,d204_15,h,3,0.015).
atm(train,d204,d204_16,s,75,-0.184).
atm(train,d204,d204_17,h,1,0.216).
atm(train,d205,d205_1,s,74,-0.213).
atm(train,d205,d205_2,zn,87,1.908).
atm(train,d205,d205_3,s,70,-0.393).
atm(train,d205,d205_4,s,70,-0.392).
atm(train,d205,d205_5,s,74,-0.213).
atm(train,d205,d205_6,c,192,0.488).
atm(train,d205,d205_7,n,36,-0.393).
atm(train,d205,d205_8,c,10,-0.093).
atm(train,d205,d205_9,h,3,-0.043).
atm(train,d205,d205_10,h,3,-0.043).
atm(train,d205,d205_11,h,3,-0.043).
atm(train,d205,d205_12,c,10,-0.093).
atm(train,d205,d205_13,h,3,-0.043).
atm(train,d205,d205_14,h,3,-0.043).
atm(train,d205,d205_15,h,3,-0.043).
atm(train,d205,d205_16,c,192,0.488).
atm(train,d205,d205_17,n,36,-0.392).
atm(train,d205,d205_18,c,10,-0.093).
atm(train,d205,d205_19,h,3,-0.043).
atm(train,d205,d205_20,h,3,-0.043).
atm(train,d205,d205_21,h,3,-0.043).
atm(train,d205,d205_22,c,10,-0.093).
atm(train,d205,d205_23,h,3,-0.043).
atm(train,d205,d205_24,h,3,-0.043).
atm(train,d205,d205_25,h,3,-0.043).
atm(train,d206,d206_1,c,10,-0.121).
atm(train,d206,d206_2,c,10,0.079).
atm(train,d206,d206_3,c,10,0.079).
atm(train,d206,d206_4,c,16,-0.221).
atm(train,d206,d206_5,c,16,-0.221).
atm(train,d206,d206_6,c,10,0.079).
atm(train,d206,d206_7,c,10,0.07).
atm(train,d206,d206_8,c,16,-0.221).
atm(train,d206,d206_9,c,10,0.07).
atm(train,d206,d206_10,c,16,-0.221).
atm(train,d206,d206_11,h,3,0.03).
atm(train,d206,d206_12,h,3,0.03).
atm(train,d206,d206_13,h,3,0.03).
atm(train,d206,d206_14,h,3,0.03).
atm(train,d206,d206_15,h,3,0.03).
atm(train,d206,d206_16,h,3,0.03).
atm(train,d206,d206_17,h,3,0.03).
atm(train,d206,d206_18,h,3,0.03).
atm(train,d206,d206_19,h,3,0.03).
atm(train,d206,d206_20,h,3,0.03).
atm(train,d206,d206_21,h,3,0.03).
atm(train,d206,d206_22,h,3,0.03).
atm(train,d206,d206_23,h,3,0.03).
atm(train,d206,d206_24,h,3,0.079).
atm(train,d206,d206_25,h,3,0.079).
atm(train,d206,d206_26,h,3,0.08).
atm(train,d207,d207_1,c,22,-0.104).
atm(train,d207,d207_2,c,22,-0.104).
atm(train,d207,d207_3,c,22,0.026).
atm(train,d207,d207_4,c,22,0.026).
atm(train,d207,d207_5,c,22,-0.104).
atm(train,d207,d207_6,c,22,-0.104).
atm(train,d207,d207_7,h,3,0.156).
atm(train,d207,d207_8,h,3,0.155).
atm(train,d207,d207_9,h,3,0.155).
atm(train,d207,d207_10,h,3,0.156).
atm(train,d207,d207_11,c,14,0.725).
atm(train,d207,d207_12,o,49,-0.625).
atm(train,d207,d207_13,c,10,0.116).
atm(train,d207,d207_14,c,16,-0.175).
atm(train,d207,d207_15,h,3,0.076).
atm(train,d207,d207_16,h,3,0.076).
atm(train,d207,d207_17,c,16,-0.175).
atm(train,d207,d207_18,o,51,-0.525).
atm(train,d207,d207_19,h,3,0.126).
atm(train,d207,d207_20,h,3,0.126).
atm(train,d207,d207_21,h,3,0.126).
atm(train,d207,d207_22,c,14,0.725).
atm(train,d207,d207_23,o,49,-0.625).
atm(train,d207,d207_24,c,10,0.116).
atm(train,d207,d207_25,c,16,-0.175).
atm(train,d207,d207_26,h,3,0.076).
atm(train,d207,d207_27,h,3,0.076).
atm(train,d207,d207_28,c,16,-0.175).
atm(train,d207,d207_29,o,51,-0.525).
atm(train,d207,d207_30,h,3,0.126).
atm(train,d207,d207_31,h,3,0.126).
atm(train,d207,d207_32,h,3,0.126).
atm(train,d208_1,d208_1_1,n,36,-0.335).
atm(train,d208_1,d208_1_2,c,14,0.515).
atm(train,d208_1,d208_1_3,n,36,-0.336).
atm(train,d208_1,d208_1_4,s,75,-0.186).
atm(train,d208_1,d208_1_5,n,36,-0.336).
atm(train,d208_1,d208_1_6,c,14,0.515).
atm(train,d208_1,d208_1_7,n,36,-0.335).
atm(train,d208_1,d208_1_8,s,75,-0.186).
atm(train,d208_1,d208_1_9,h,1,0.114).
atm(train,d208_1,d208_1_10,h,1,0.114).
atm(train,d208_1,d208_1_11,h,1,0.114).
atm(train,d208_1,d208_1_12,h,1,0.114).
atm(train,d208_1,d208_1_13,h,1,0.114).
atm(train,d208_1,d208_1_14,h,1,0.114).
atm(train,d208_2,d208_2_1,te,129,0.97).
atm(train,d208_2,d208_2_2,s,74,-0.149).
atm(train,d208_2,d208_2_3,s,74,-0.149).
atm(train,d208_2,d208_2_4,s,74,-0.149).
atm(train,d208_2,d208_2_5,s,74,-0.149).
atm(train,d208_2,d208_2_6,c,14,0.521).
atm(train,d208_2,d208_2_7,n,36,-0.329).
atm(train,d208_2,d208_2_8,c,10,-0.029).
atm(train,d208_2,d208_2_9,c,10,-0.129).
atm(train,d208_2,d208_2_10,h,3,0.021).
atm(train,d208_2,d208_2_11,h,3,0.021).
atm(train,d208_2,d208_2_12,h,3,0.021).
atm(train,d208_2,d208_2_13,h,3,0.021).
atm(train,d208_2,d208_2_14,h,3,0.021).
atm(train,d208_2,d208_2_15,c,10,-0.029).
atm(train,d208_2,d208_2_16,c,10,-0.129).
atm(train,d208_2,d208_2_17,h,3,0.021).
atm(train,d208_2,d208_2_18,h,3,0.021).
atm(train,d208_2,d208_2_19,h,3,0.021).
atm(train,d208_2,d208_2_20,h,3,0.021).
atm(train,d208_2,d208_2_21,h,3,0.021).
atm(train,d208_2,d208_2_22,s,75,-0.179).
atm(train,d208_2,d208_2_23,c,14,0.521).
atm(train,d208_2,d208_2_24,n,36,-0.329).
atm(train,d208_2,d208_2_25,c,10,-0.029).
atm(train,d208_2,d208_2_26,c,10,-0.129).
atm(train,d208_2,d208_2_27,h,3,0.021).
atm(train,d208_2,d208_2_28,h,3,0.021).
atm(train,d208_2,d208_2_29,h,3,0.021).
atm(train,d208_2,d208_2_30,h,3,0.021).
atm(train,d208_2,d208_2_31,h,3,0.021).
atm(train,d208_2,d208_2_32,c,10,-0.029).
atm(train,d208_2,d208_2_33,c,10,-0.129).
atm(train,d208_2,d208_2_34,h,3,0.021).
atm(train,d208_2,d208_2_35,h,3,0.021).
atm(train,d208_2,d208_2_36,h,3,0.021).
atm(train,d208_2,d208_2_37,h,3,0.021).
atm(train,d208_2,d208_2_38,h,3,0.021).
atm(train,d208_2,d208_2_39,c,14,0.52).
atm(train,d208_2,d208_2_40,n,36,-0.329).
atm(train,d208_2,d208_2_41,c,10,-0.029).
atm(train,d208_2,d208_2_42,c,10,-0.129).
atm(train,d208_2,d208_2_43,h,3,0.021).
atm(train,d208_2,d208_2_44,h,3,0.021).
atm(train,d208_2,d208_2_45,h,3,0.021).
atm(train,d208_2,d208_2_46,h,3,0.021).
atm(train,d208_2,d208_2_47,h,3,0.021).
atm(train,d208_2,d208_2_48,c,10,-0.029).
atm(train,d208_2,d208_2_49,c,10,-0.129).
atm(train,d208_2,d208_2_50,h,3,0.021).
atm(train,d208_2,d208_2_51,h,3,0.021).
atm(train,d208_2,d208_2_52,h,3,0.021).
atm(train,d208_2,d208_2_53,h,3,0.021).
atm(train,d208_2,d208_2_54,h,3,0.021).
atm(train,d208_2,d208_2_55,c,14,0.52).
atm(train,d208_2,d208_2_56,n,36,-0.329).
atm(train,d208_2,d208_2_57,c,10,-0.029).
atm(train,d208_2,d208_2_58,c,10,-0.129).
atm(train,d208_2,d208_2_59,h,3,0.021).
atm(train,d208_2,d208_2_60,h,3,0.021).
atm(train,d208_2,d208_2_61,h,3,0.021).
atm(train,d208_2,d208_2_62,h,3,0.021).
atm(train,d208_2,d208_2_63,h,3,0.021).
atm(train,d208_2,d208_2_64,c,10,-0.029).
atm(train,d208_2,d208_2_65,c,10,-0.129).
atm(train,d208_2,d208_2_66,h,3,0.021).
atm(train,d208_2,d208_2_67,h,3,0.021).
atm(train,d208_2,d208_2_68,h,3,0.021).
atm(train,d208_2,d208_2_69,h,3,0.021).
atm(train,d208_2,d208_2_70,h,3,0.021).
atm(train,d208_2,d208_2_71,s,75,-0.179).
atm(train,d208_2,d208_2_72,s,75,-0.179).
atm(train,d208_2,d208_2_73,s,75,-0.179).
atm(train,d209,d209_1,c,22,-0.137).
atm(train,d209,d209_2,c,22,0.244).
atm(train,d209,d209_3,c,22,0.174).
atm(train,d209,d209_4,c,22,-0.137).
atm(train,d209,d209_5,c,22,-0.137).
atm(train,d209,d209_6,c,22,-0.136).
atm(train,d209,d209_7,h,3,0.123).
atm(train,d209,d209_8,h,3,0.123).
atm(train,d209,d209_9,h,3,0.123).
atm(train,d209,d209_10,o,45,-0.656).
atm(train,d209,d209_11,o,50,-0.236).
atm(train,d209,d209_12,c,10,0.083).
atm(train,d209,d209_13,h,3,0.043).
atm(train,d209,d209_14,h,3,0.043).
atm(train,d209,d209_15,h,3,0.043).
atm(train,d209,d209_16,h,8,0.394).
atm(train,d209,d209_17,c,10,0.093).
atm(train,d209,d209_18,c,16,-0.206).
atm(train,d209,d209_19,c,16,-0.206).
atm(train,d209,d209_20,h,3,0.093).
atm(train,d209,d209_21,h,3,0.093).
atm(train,d209,d209_22,h,3,0.093).
atm(train,d209,d209_23,h,3,0.043).
atm(train,d209,d209_24,h,3,0.043).
atm(train,d21,d21_1,c,22,-0.104).
atm(train,d21,d21_2,c,22,-0.104).
atm(train,d21,d21_3,c,22,-0.104).
atm(train,d21,d21_4,c,22,0.207).
atm(train,d21,d21_5,c,22,-0.104).
atm(train,d21,d21_6,c,22,-0.104).
atm(train,d21,d21_7,h,3,0.156).
atm(train,d21,d21_8,h,3,0.157).
atm(train,d21,d21_9,h,3,0.156).
atm(train,d21,d21_10,h,3,0.156).
atm(train,d21,d21_11,c,22,0.207).
atm(train,d21,d21_12,c,22,-0.104).
atm(train,d21,d21_13,c,22,-0.104).
atm(train,d21,d21_14,c,22,-0.104).
atm(train,d21,d21_15,c,22,-0.104).
atm(train,d21,d21_16,c,22,-0.104).
atm(train,d21,d21_17,h,3,0.156).
atm(train,d21,d21_18,h,3,0.157).
atm(train,d21,d21_19,h,3,0.156).
atm(train,d21,d21_20,o,50,-0.203).
atm(train,d21,d21_21,cl,93,-0.173).
atm(train,d21,d21_22,cl,93,-0.173).
atm(train,d21,d21_23,n,38,0.827).
atm(train,d21,d21_24,o,40,-0.373).
atm(train,d21,d21_25,o,40,-0.373).
atm(train,d210,d210_1,c,22,-0.131).
atm(train,d210,d210_2,c,22,-0.131).
atm(train,d210,d210_3,c,22,-0.131).
atm(train,d210,d210_4,c,22,-0.131).
atm(train,d210,d210_5,c,22,0.199).
atm(train,d210,d210_6,c,22,-0.131).
atm(train,d210,d210_7,h,3,0.129).
atm(train,d210,d210_8,h,3,0.129).
atm(train,d210,d210_9,h,3,0.129).
atm(train,d210,d210_10,h,3,0.129).
atm(train,d210,d210_11,c,232,1.0).
atm(train,d210,d210_12,f,92,-0.301).
atm(train,d210,d210_13,f,92,-0.3).
atm(train,d210,d210_14,f,92,-0.3).
atm(train,d210,d210_15,n,32,-0.4).
atm(train,d210,d210_16,c,14,0.6).
atm(train,d210,d210_17,n,32,-0.4).
atm(train,d210,d210_18,c,10,-0.001).
atm(train,d210,d210_19,h,3,0.049).
atm(train,d210,d210_20,h,3,0.049).
atm(train,d210,d210_21,h,3,0.049).
atm(train,d210,d210_22,c,10,-0.001).
atm(train,d210,d210_23,h,3,0.049).
atm(train,d210,d210_24,h,3,0.049).
atm(train,d210,d210_25,h,3,0.049).
atm(train,d210,d210_26,o,40,-0.55).
atm(train,d210,d210_27,h,1,0.299).
atm(train,d211,d211_1,c,10,-0.102).
atm(train,d211,d211_2,s,74,-0.103).
atm(train,d211,d211_3,h,3,0.047).
atm(train,d211,d211_4,h,3,0.047).
atm(train,d211,d211_5,h,3,0.047).
atm(train,d211,d211_6,c,10,-0.103).
atm(train,d211,d211_7,c,10,-0.103).
atm(train,d211,d211_8,h,3,0.047).
atm(train,d211,d211_9,h,3,0.047).
atm(train,d211,d211_10,h,3,0.047).
atm(train,d211,d211_11,c,10,-0.103).
atm(train,d211,d211_12,h,3,0.047).
atm(train,d211,d211_13,h,3,0.047).
atm(train,d211,d211_14,h,3,0.047).
atm(train,d211,d211_15,c,14,0.598).
atm(train,d211,d211_16,n,32,-0.402).
atm(train,d211,d211_17,o,50,-0.232).
atm(train,d211,d211_18,c,14,0.598).
atm(train,d211,d211_19,n,32,-0.402).
atm(train,d211,d211_20,c,10,-0.003).
atm(train,d211,d211_21,h,3,0.047).
atm(train,d211,d211_22,o,51,-0.552).
atm(train,d211,d211_23,h,3,0.047).
atm(train,d211,d211_24,h,3,0.047).
atm(train,d211,d211_25,h,3,0.047).
atm(train,d211,d211_26,h,1,0.298).
atm(train,d212,d212_1,c,27,-0.027).
atm(train,d212,d212_2,c,27,-0.027).
atm(train,d212,d212_3,c,22,-0.157).
atm(train,d212,d212_4,c,22,-0.157).
atm(train,d212,d212_5,c,22,-0.157).
atm(train,d212,d212_6,c,22,-0.077).
atm(train,d212,d212_7,h,3,0.103).
atm(train,d212,d212_8,h,3,0.103).
atm(train,d212,d212_9,c,22,-0.157).
atm(train,d212,d212_10,c,22,-0.157).
atm(train,d212,d212_11,c,22,-0.157).
atm(train,d212,d212_12,c,22,-0.157).
atm(train,d212,d212_13,h,3,0.103).
atm(train,d212,d212_14,h,3,0.103).
atm(train,d212,d212_15,h,3,0.103).
atm(train,d212,d212_16,h,3,0.103).
atm(train,d212,d212_17,s,78,0.724).
atm(train,d212,d212_18,o,50,-0.257).
atm(train,d212,d212_19,na,81,0.974).
atm(train,d212,d212_20,o,40,-0.277).
atm(train,d212,d212_21,o,40,-0.277).
atm(train,d212,d212_22,n,32,-0.427).
atm(train,d212,d212_23,n,32,-0.427).
atm(train,d212,d212_24,c,22,0.173).
atm(train,d212,d212_25,c,22,-0.157).
atm(train,d212,d212_26,c,22,-0.077).
atm(train,d212,d212_27,c,27,-0.027).
atm(train,d212,d212_28,c,27,-0.027).
atm(train,d212,d212_29,c,22,0.223).
atm(train,d212,d212_30,h,3,0.103).
atm(train,d212,d212_31,c,22,-0.157).
atm(train,d212,d212_32,c,22,-0.157).
atm(train,d212,d212_33,c,22,-0.157).
atm(train,d212,d212_34,c,22,-0.157).
atm(train,d212,d212_35,h,3,0.103).
atm(train,d212,d212_36,h,3,0.103).
atm(train,d212,d212_37,h,3,0.103).
atm(train,d212,d212_38,h,3,0.103).
atm(train,d212,d212_39,o,45,-0.676).
atm(train,d212,d212_40,h,8,0.373).
atm(train,d212,d212_41,s,78,0.724).
atm(train,d212,d212_42,o,50,-0.257).
atm(train,d212,d212_43,na,81,0.974).
atm(train,d212,d212_44,o,40,-0.277).
atm(train,d212,d212_45,o,40,-0.277).
atm(train,d213,d213_1,c,22,-0.12).
atm(train,d213,d213_2,c,22,-0.12).
atm(train,d213,d213_3,c,22,-0.12).
atm(train,d213,d213_4,c,22,0.01).
atm(train,d213,d213_5,c,22,0.21).
atm(train,d213,d213_6,c,22,-0.12).
atm(train,d213,d213_7,h,3,0.14).
atm(train,d213,d213_8,h,3,0.14).
atm(train,d213,d213_9,h,3,0.14).
atm(train,d213,d213_10,h,3,0.14).
atm(train,d213,d213_11,n,32,-0.77).
atm(train,d213,d213_12,c,14,0.79).
atm(train,d213,d213_13,h,1,0.34).
atm(train,d213,d213_14,h,1,0.34).
atm(train,d213,d213_15,o,45,-0.66).
atm(train,d213,d213_16,o,51,-0.6).
atm(train,d213,d213_17,h,1,0.26).
atm(train,d214,d214_1,c,10,-0.105).
atm(train,d214,d214_2,c,10,-0.105).
atm(train,d214,d214_3,c,10,0.045).
atm(train,d214,d214_4,c,10,-0.105).
atm(train,d214,d214_5,c,10,-0.105).
atm(train,d214,d214_6,c,10,-0.105).
atm(train,d214,d214_7,h,3,0.045).
atm(train,d214,d214_8,h,3,0.045).
atm(train,d214,d214_9,h,3,0.045).
atm(train,d214,d214_10,h,3,0.045).
atm(train,d214,d214_11,h,3,0.045).
atm(train,d214,d214_12,h,3,0.045).
atm(train,d214,d214_13,h,3,0.045).
atm(train,d214,d214_14,h,3,0.045).
atm(train,d214,d214_15,h,3,0.045).
atm(train,d214,d214_16,h,3,0.045).
atm(train,d214,d214_17,n,36,-0.405).
atm(train,d214,d214_18,h,3,0.095).
atm(train,d214,d214_19,c,14,0.545).
atm(train,d214,d214_20,n,36,-0.405).
atm(train,d214,d214_21,c,10,0.045).
atm(train,d214,d214_22,h,1,0.145).
atm(train,d214,d214_23,h,1,0.145).
atm(train,d214,d214_24,s,75,-0.155).
atm(train,d214,d214_25,c,10,-0.105).
atm(train,d214,d214_26,c,10,-0.105).
atm(train,d214,d214_27,c,10,-0.105).
atm(train,d214,d214_28,c,10,-0.105).
atm(train,d214,d214_29,c,10,-0.105).
atm(train,d214,d214_30,h,3,0.095).
atm(train,d214,d214_31,h,3,0.045).
atm(train,d214,d214_32,h,3,0.045).
atm(train,d214,d214_33,h,3,0.045).
atm(train,d214,d214_34,h,3,0.045).
atm(train,d214,d214_35,h,3,0.045).
atm(train,d214,d214_36,h,3,0.045).
atm(train,d214,d214_37,h,3,0.045).
atm(train,d214,d214_38,h,3,0.045).
atm(train,d214,d214_39,h,3,0.045).
atm(train,d214,d214_40,h,3,0.045).
atm(train,d215,d215_1,c,22,0.207).
atm(train,d215,d215_2,n,35,-0.583).
atm(train,d215,d215_3,c,22,0.457).
atm(train,d215,d215_4,c,22,-0.173).
atm(train,d215,d215_5,c,22,0.058).
atm(train,d215,d215_6,c,22,-0.173).
atm(train,d215,d215_7,h,3,0.088).
atm(train,d215,d215_8,h,3,0.088).
atm(train,d215,d215_9,h,3,0.088).
atm(train,d215,d215_10,c,10,-0.142).
atm(train,d215,d215_11,c,10,-0.142).
atm(train,d215,d215_12,h,3,0.008).
atm(train,d215,d215_13,h,3,0.008).
atm(train,d215,d215_14,h,3,0.008).
atm(train,d215,d215_15,h,3,0.008).
atm(train,d215,d215_16,h,3,0.008).
atm(train,d215,d215_17,c,14,0.507).
atm(train,d215,d215_18,n,36,-0.343).
atm(train,d215,d215_19,s,75,-0.193).
atm(train,d215,d215_20,h,1,0.108).
atm(train,d215,d215_21,h,1,0.108).
atm(train,d216,d216_1,s,74,-0.173).
atm(train,d216,d216_2,pb,121,0.948).
atm(train,d216,d216_3,s,70,-0.353).
atm(train,d216,d216_4,s,70,-0.352).
atm(train,d216,d216_5,s,74,-0.173).
atm(train,d216,d216_6,c,192,0.528).
atm(train,d216,d216_7,n,36,-0.353).
atm(train,d216,d216_8,c,10,-0.053).
atm(train,d216,d216_9,h,3,-0.003).
atm(train,d216,d216_10,h,3,-0.003).
atm(train,d216,d216_11,h,3,-0.003).
atm(train,d216,d216_12,c,10,-0.053).
atm(train,d216,d216_13,h,3,-0.003).
atm(train,d216,d216_14,h,3,-0.003).
atm(train,d216,d216_15,h,3,-0.003).
atm(train,d216,d216_16,c,192,0.528).
atm(train,d216,d216_17,n,36,-0.352).
atm(train,d216,d216_18,c,10,-0.053).
atm(train,d216,d216_19,h,3,-0.003).
atm(train,d216,d216_20,h,3,-0.003).
atm(train,d216,d216_21,h,3,-0.003).
atm(train,d216,d216_22,c,10,-0.053).
atm(train,d216,d216_23,h,3,-0.003).
atm(train,d216,d216_24,h,3,-0.003).
atm(train,d216,d216_25,h,3,-0.003).
atm(train,d217,d217_1,c,22,-0.147).
atm(train,d217,d217_2,c,22,0.183).
atm(train,d217,d217_3,c,22,-0.147).
atm(train,d217,d217_4,c,22,-0.147).
atm(train,d217,d217_5,c,22,-0.147).
atm(train,d217,d217_6,c,22,-0.147).
atm(train,d217,d217_7,h,3,0.113).
atm(train,d217,d217_8,h,3,0.113).
atm(train,d217,d217_9,h,3,0.113).
atm(train,d217,d217_10,h,3,0.113).
atm(train,d217,d217_11,h,3,0.113).
atm(train,d217,d217_12,n,32,-0.417).
atm(train,d217,d217_13,c,14,0.583).
atm(train,d217,d217_14,c,10,0.073).
atm(train,d217,d217_15,c,14,0.584).
atm(train,d217,d217_16,n,32,-0.417).
atm(train,d217,d217_17,h,3,0.033).
atm(train,d217,d217_18,h,3,0.033).
atm(train,d217,d217_19,c,10,-0.017).
atm(train,d217,d217_20,h,3,0.033).
atm(train,d217,d217_21,h,3,0.033).
atm(train,d217,d217_22,h,3,0.033).
atm(train,d217,d217_23,o,40,-0.567).
atm(train,d218,d218_1,c,10,-0.168).
atm(train,d218,d218_2,c,10,-0.067).
atm(train,d218,d218_3,h,3,-0.017).
atm(train,d218,d218_4,h,3,-0.017).
atm(train,d218,d218_5,h,3,-0.017).
atm(train,d218,d218_6,n,36,-0.368).
atm(train,d218,d218_7,h,3,-0.017).
atm(train,d218,d218_8,h,3,-0.017).
atm(train,d218,d218_9,c,10,-0.067).
atm(train,d218,d218_10,c,10,-0.168).
atm(train,d218,d218_11,h,3,-0.017).
atm(train,d218,d218_12,h,3,-0.017).
atm(train,d218,d218_13,h,3,-0.017).
atm(train,d218,d218_14,h,3,-0.017).
atm(train,d218,d218_15,h,3,-0.017).
atm(train,d218,d218_16,c,14,0.482).
atm(train,d218,d218_17,s,74,-0.188).
atm(train,d218,d218_18,na,81,0.932).
atm(train,d218,d218_19,s,75,-0.218).
atm(train,d219,d219_1,c,10,-0.069).
atm(train,d219,d219_2,c,10,0.031).
atm(train,d219,d219_3,h,3,0.08).
atm(train,d219,d219_4,h,3,0.081).
atm(train,d219,d219_5,h,3,0.081).
atm(train,d219,d219_6,n,36,-0.27).
atm(train,d219,d219_7,h,3,0.081).
atm(train,d219,d219_8,h,3,0.08).
atm(train,d219,d219_9,c,10,0.031).
atm(train,d219,d219_10,c,10,-0.069).
atm(train,d219,d219_11,h,3,0.08).
atm(train,d219,d219_12,h,3,0.08).
atm(train,d219,d219_13,h,3,0.08).
atm(train,d219,d219_14,h,3,0.08).
atm(train,d219,d219_15,h,3,0.08).
atm(train,d219,d219_16,c,14,-0.069).
atm(train,d219,d219_17,s,70,-0.27).
atm(train,d219,d219_18,s,70,-0.27).
atm(train,d219,d219_19,c,14,-0.069).
atm(train,d219,d219_20,n,36,-0.27).
atm(train,d219,d219_21,s,75,-0.12).
atm(train,d219,d219_22,s,75,-0.12).
atm(train,d219,d219_23,c,10,0.031).
atm(train,d219,d219_24,c,10,-0.069).
atm(train,d219,d219_25,h,3,0.081).
atm(train,d219,d219_26,h,3,0.081).
atm(train,d219,d219_27,h,3,0.081).
atm(train,d219,d219_28,h,3,0.081).
atm(train,d219,d219_29,h,3,0.081).
atm(train,d219,d219_30,c,10,0.031).
atm(train,d219,d219_31,c,10,-0.069).
atm(train,d219,d219_32,h,3,0.08).
atm(train,d219,d219_33,h,3,0.081).
atm(train,d219,d219_34,h,3,0.081).
atm(train,d219,d219_35,h,3,0.08).
atm(train,d219,d219_36,h,3,0.08).
atm(train,d22,d22_1,c,22,-0.112).
atm(train,d22,d22_2,c,22,-0.112).
atm(train,d22,d22_3,c,22,-0.112).
atm(train,d22,d22_4,c,22,0.218).
atm(train,d22,d22_5,c,22,-0.112).
atm(train,d22,d22_6,c,22,-0.112).
atm(train,d22,d22_7,h,3,0.148).
atm(train,d22,d22_8,h,3,0.148).
atm(train,d22,d22_9,h,3,0.148).
atm(train,d22,d22_10,h,3,0.148).
atm(train,d22,d22_11,h,3,0.148).
atm(train,d22,d22_12,c,22,0.218).
atm(train,d22,d22_13,c,22,-0.112).
atm(train,d22,d22_14,c,22,-0.112).
atm(train,d22,d22_15,c,22,0.218).
atm(train,d22,d22_16,c,22,-0.112).
atm(train,d22,d22_17,c,22,-0.112).
atm(train,d22,d22_18,h,3,0.148).
atm(train,d22,d22_19,h,3,0.148).
atm(train,d22,d22_20,h,3,0.148).
atm(train,d22,d22_21,h,3,0.148).
atm(train,d22,d22_22,n,32,-0.382).
atm(train,d22,d22_23,n,32,-0.382).
atm(train,d22,d22_24,o,40,-0.532).
atm(train,d22,d22_25,h,1,0.318).
atm(train,d220,d220a_1,c,22,-0.128).
atm(train,d220,d220a_2,c,22,-0.128).
atm(train,d220,d220a_3,c,22,0.102).
atm(train,d220,d220a_4,c,22,-0.128).
atm(train,d220,d220a_5,c,22,-0.179).
atm(train,d220,d220a_6,c,22,-0.129).
atm(train,d220,d220a_7,h,3,0.131).
atm(train,d220,d220a_8,h,3,0.131).
atm(train,d220,d220a_9,h,3,0.131).
atm(train,d220,d220a_10,h,3,0.131).
atm(train,d220,d220a_11,c,10,0.002).
atm(train,d220,d220a_12,h,3,0.052).
atm(train,d220,d220a_13,h,3,0.052).
atm(train,d220,d220a_14,h,3,0.052).
atm(train,d220,d220a_15,c,16,-0.199).
atm(train,d220,d220a_16,c,16,-0.199).
atm(train,d220,d220a_17,h,3,0.102).
atm(train,d220,d220a_18,h,3,0.102).
atm(train,d220,d220a_19,h,3,0.102).
atm(train,d220,d220b_1,c,22,-0.128).
atm(train,d220,d220b_2,c,22,0.102).
atm(train,d220,d220b_3,c,22,-0.128).
atm(train,d220,d220b_4,c,22,-0.128).
atm(train,d220,d220b_5,c,22,-0.179).
atm(train,d220,d220b_6,c,22,-0.129).
atm(train,d220,d220b_7,h,3,0.131).
atm(train,d220,d220b_8,h,3,0.131).
atm(train,d220,d220b_9,h,3,0.131).
atm(train,d220,d220b_10,c,10,0.002).
atm(train,d220,d220b_11,h,3,0.052).
atm(train,d220,d220b_12,h,3,0.052).
atm(train,d220,d220b_13,h,3,0.052).
atm(train,d220,d220b_14,c,16,-0.199).
atm(train,d220,d220b_15,c,16,-0.199).
atm(train,d220,d220b_16,h,3,0.102).
atm(train,d220,d220b_17,h,3,0.102).
atm(train,d220,d220b_18,h,3,0.102).
atm(train,d220,d220b_19,h,3,0.131).
atm(train,d221,d221_1,c,22,-0.081).
atm(train,d221,d221_2,c,22,-0.161).
atm(train,d221,d221_3,c,27,-0.031).
atm(train,d221,d221_4,c,27,-0.031).
atm(train,d221,d221_5,c,22,-0.161).
atm(train,d221,d221_6,c,22,-0.161).
atm(train,d221,d221_7,h,3,0.099).
atm(train,d221,d221_8,h,3,0.099).
atm(train,d221,d221_9,h,3,0.099).
atm(train,d221,d221_10,c,22,-0.161).
atm(train,d221,d221_11,c,22,-0.161).
atm(train,d221,d221_12,c,22,0.219).
atm(train,d221,d221_13,c,22,-0.161).
atm(train,d221,d221_14,h,3,0.099).
atm(train,d221,d221_15,h,3,0.099).
atm(train,d221,d221_16,s,78,0.719).
atm(train,d221,d221_17,o,50,-0.261).
atm(train,d221,d221_18,na,81,0.969).
atm(train,d221,d221_19,o,40,-0.281).
atm(train,d221,d221_20,o,40,-0.281).
atm(train,d221,d221_21,n,32,-0.431).
atm(train,d221,d221_22,n,32,-0.431).
atm(train,d221,d221_23,c,22,0.169).
atm(train,d221,d221_24,c,22,-0.161).
atm(train,d221,d221_25,c,22,-0.161).
atm(train,d221,d221_26,c,22,-0.081).
atm(train,d221,d221_27,c,22,-0.161).
atm(train,d221,d221_28,c,22,-0.161).
atm(train,d221,d221_29,h,3,0.099).
atm(train,d221,d221_30,h,3,0.099).
atm(train,d221,d221_31,h,3,0.099).
atm(train,d221,d221_32,h,3,0.099).
atm(train,d221,d221_33,s,78,0.719).
atm(train,d221,d221_34,o,50,-0.261).
atm(train,d221,d221_35,na,81,0.968).
atm(train,d221,d221_36,o,45,-0.681).
atm(train,d221,d221_37,h,8,0.369).
atm(train,d221,d221_38,o,40,-0.281).
atm(train,d221,d221_39,o,40,-0.281).
atm(train,d222,d222_1,c,22,-0.131).
atm(train,d222,d222_2,c,22,0.249).
atm(train,d222,d222_3,c,22,-0.131).
atm(train,d222,d222_4,c,22,-0.181).
atm(train,d222,d222_5,c,22,0.199).
atm(train,d222,d222_6,c,22,-0.181).
atm(train,d222,d222_7,h,3,0.129).
atm(train,d222,d222_8,h,3,0.129).
atm(train,d222,d222_9,n,32,-0.402).
atm(train,d222,d222_10,c,10,-0.001).
atm(train,d222,d222_11,h,3,0.049).
atm(train,d222,d222_12,h,3,0.049).
atm(train,d222,d222_13,h,3,0.049).
atm(train,d222,d222_14,c,10,-0.001).
atm(train,d222,d222_15,h,3,0.049).
atm(train,d222,d222_16,h,3,0.049).
atm(train,d222,d222_17,h,3,0.049).
atm(train,d222,d222_18,c,10,-0.001).
atm(train,d222,d222_19,h,3,0.049).
atm(train,d222,d222_20,h,3,0.049).
atm(train,d222,d222_21,h,3,0.049).
atm(train,d222,d222_22,c,10,-0.001).
atm(train,d222,d222_23,h,3,0.049).
atm(train,d222,d222_24,h,3,0.049).
atm(train,d222,d222_25,h,3,0.049).
atm(train,d222,d222_26,o,49,-0.352).
atm(train,d222,d222_27,c,14,0.598).
atm(train,d222,d222_28,n,32,-0.402).
atm(train,d222,d222_29,c,10,-0.001).
atm(train,d222,d222_30,h,3,0.049).
atm(train,d222,d222_31,h,3,0.049).
atm(train,d222,d222_32,h,3,0.049).
atm(train,d222,d222_33,o,51,-0.552).
atm(train,d222,d222_34,h,1,0.298).
atm(train,d223,d223_1,c,22,-0.155).
atm(train,d223,d223_2,c,22,-0.155).
atm(train,d223,d223_3,c,22,-0.155).
atm(train,d223,d223_4,c,22,-0.155).
atm(train,d223,d223_5,c,22,0.175).
atm(train,d223,d223_6,c,22,-0.155).
atm(train,d223,d223_7,h,3,0.105).
atm(train,d223,d223_8,h,3,0.105).
atm(train,d223,d223_9,h,3,0.105).
atm(train,d223,d223_10,h,3,0.105).
atm(train,d223,d223_11,h,3,0.105).
atm(train,d223,d223_12,n,32,-0.425).
atm(train,d223,d223_13,c,14,0.575).
atm(train,d223,d223_14,n,36,-0.425).
atm(train,d223,d223_15,s,75,-0.175).
atm(train,d223,d223_16,h,1,0.275).
atm(train,d223,d223_17,h,1,0.125).
atm(train,d223,d223_18,h,1,0.125).
atm(train,d224,d224_1,c,10,0.096).
atm(train,d224,d224_2,c,16,-0.194).
atm(train,d224,d224_3,c,16,-0.193).
atm(train,d224,d224_4,c,10,0.096).
atm(train,d224,d224_5,s,77,0.207).
atm(train,d224,d224_6,h,3,0.041).
atm(train,d224,d224_7,h,3,0.041).
atm(train,d224,d224_8,h,3,0.041).
atm(train,d224,d224_9,h,3,0.041).
atm(train,d224,d224_10,h,3,0.106).
atm(train,d224,d224_11,h,3,0.106).
atm(train,d224,d224_12,o,40,-0.194).
atm(train,d224,d224_13,o,40,-0.194).
atm(train,d225,d225a_1,c,10,0.08).
atm(train,d225,d225a_2,c,16,-0.21).
atm(train,d225,d225a_3,h,3,0.039).
atm(train,d225,d225a_4,h,3,0.039).
atm(train,d225,d225a_5,h,3,0.039).
atm(train,d225,d225a_6,c,10,0.08).
atm(train,d225,d225a_7,h,3,0.039).
atm(train,d225,d225a_8,h,3,0.039).
atm(train,d225,d225a_9,h,3,0.039).
atm(train,d225,d225a_10,c,16,-0.21).
atm(train,d225,d225a_11,c,10,0.09).
atm(train,d225,d225a_12,h,3,0.09).
atm(train,d225,d225a_13,c,10,0.09).
atm(train,d225,d225a_14,h,3,0.039).
atm(train,d225,d225a_15,h,3,0.039).
atm(train,d225,d225a_16,c,16,-0.21).
atm(train,d225,d225a_17,h,3,0.039).
atm(train,d225,d225a_18,h,3,0.039).
atm(train,d225,d225a_19,c,10,0.079).
atm(train,d225,d225a_20,h,3,0.039).
atm(train,d225,d225a_21,h,3,0.039).
atm(train,d225,d225a_22,h,3,0.039).
atm(train,d225,d225a_23,c,16,-0.21).
atm(train,d225,d225a_24,c,10,0.08).
atm(train,d225,d225a_25,o,49,-0.66).
atm(train,d225,d225a_26,h,3,0.039).
atm(train,d225,d225a_27,h,3,0.039).
atm(train,d225,d225a_28,c,14,0.69).
atm(train,d225,d225a_29,o,51,-0.56).
atm(train,d225,d225a_30,c,10,-0.011).
atm(train,d225,d225a_31,h,3,0.039).
atm(train,d225,d225a_32,h,3,0.039).
atm(train,d225,d225a_33,h,3,0.039).
atm(train,d225,d225a_34,h,3,0.09).
atm(train,d225,d225b_1,c,10,0.082).
atm(train,d225,d225b_2,c,16,-0.209).
atm(train,d225,d225b_3,h,3,0.042).
atm(train,d225,d225b_4,h,3,0.042).
atm(train,d225,d225b_5,h,3,0.042).
atm(train,d225,d225b_6,c,10,0.082).
atm(train,d225,d225b_7,h,3,0.042).
atm(train,d225,d225b_8,h,3,0.042).
atm(train,d225,d225b_9,h,3,0.042).
atm(train,d225,d225b_10,c,16,-0.209).
atm(train,d225,d225b_11,c,10,0.091).
atm(train,d225,d225b_12,h,3,0.091).
atm(train,d225,d225b_13,c,10,-0.109).
atm(train,d225,d225b_14,h,3,0.042).
atm(train,d225,d225b_15,h,3,0.042).
atm(train,d225,d225b_16,c,10,-0.159).
atm(train,d225,d225b_17,h,3,0.042).
atm(train,d225,d225b_18,h,3,0.042).
atm(train,d225,d225b_19,c,10,-0.159).
atm(train,d225,d225b_20,h,3,0.042).
atm(train,d225,d225b_21,h,3,0.042).
atm(train,d225,d225b_22,h,3,0.042).
atm(train,d225,d225b_23,c,10,-0.109).
atm(train,d225,d225b_24,c,10,0.241).
atm(train,d225,d225b_25,o,49,-0.659).
atm(train,d225,d225b_26,h,3,0.092).
atm(train,d225,d225b_27,h,3,0.092).
atm(train,d225,d225b_28,c,14,0.691).
atm(train,d225,d225b_29,o,51,-0.559).
atm(train,d225,d225b_30,c,10,-0.008).
atm(train,d225,d225b_31,h,3,0.042).
atm(train,d225,d225b_32,h,3,0.042).
atm(train,d225,d225b_33,h,3,0.042).
atm(train,d225,d225b_34,h,3,-0.038).
atm(train,d225,d225b_35,h,3,0.042).
atm(train,d225,d225b_36,h,3,0.042).
atm(train,d226,d226_1,c,22,0.245).
atm(train,d226,d226_2,n,35,-0.545).
atm(train,d226,d226_3,c,22,0.245).
atm(train,d226,d226_4,c,22,-0.136).
atm(train,d226,d226_5,c,22,-0.136).
atm(train,d226,d226_6,c,22,-0.136).
atm(train,d226,d226_7,h,3,0.124).
atm(train,d226,d226_8,h,3,0.124).
atm(train,d226,d226_9,h,3,0.124).
atm(train,d226,d226_10,h,3,0.124).
atm(train,d226,d226_11,c,10,0.084).
atm(train,d226,d226_12,cl,93,-0.205).
atm(train,d226,d226_13,h,3,0.044).
atm(train,d226,d226_14,h,3,0.044).
atm(train,d227,d227_1,br,94,-0.118).
atm(train,d227,d227_2,c,10,-0.068).
atm(train,d227,d227_3,c,10,-0.068).
atm(train,d227,d227_4,h,3,0.082).
atm(train,d227,d227_5,h,3,0.082).
atm(train,d227,d227_6,c,10,0.131).
atm(train,d227,d227_7,cl,93,-0.169).
atm(train,d227,d227_8,h,3,0.082).
atm(train,d227,d227_9,h,3,0.082).
atm(train,d227,d227_10,br,94,-0.118).
atm(train,d227,d227_11,h,3,0.082).
atm(train,d228,d228_1,c,10,-0.063).
atm(train,d228,d228_2,br,94,-0.112).
atm(train,d228,d228_3,c,10,-0.063).
atm(train,d228,d228_4,h,3,0.088).
atm(train,d228,d228_5,h,3,0.087).
atm(train,d228,d228_6,br,94,-0.112).
atm(train,d228,d228_7,h,3,0.088).
atm(train,d228,d228_8,h,3,0.087).
atm(train,d229,d229_1,c,10,0.1).
atm(train,d229,d229_2,c,10,0.1).
atm(train,d229,d229_3,cl,93,-0.2).
atm(train,d229,d229_4,h,3,0.05).
atm(train,d229,d229_5,h,3,0.05).
atm(train,d229,d229_6,cl,93,-0.2).
atm(train,d229,d229_7,h,3,0.05).
atm(train,d229,d229_8,h,3,0.05).
atm(train,d230,d230_1,cl,93,-0.155).
atm(train,d230,d230_2,c,16,-0.155).
atm(train,d230,d230_3,c,16,-0.155).
atm(train,d230,d230_4,h,3,0.146).
atm(train,d230,d230_5,c,10,0.136).
atm(train,d230,d230_6,h,3,0.146).
atm(train,d230,d230_7,cl,93,-0.155).
atm(train,d230,d230_8,h,3,0.096).
atm(train,d230,d230_9,h,3,0.096).
atm(train,d231,d231_1,c,22,-0.118).
atm(train,d231,d231_2,c,22,-0.118).
atm(train,d231,d231_3,c,22,0.211).
atm(train,d231,d231_4,c,22,-0.118).
atm(train,d231,d231_5,c,22,-0.118).
atm(train,d231,d231_6,c,22,0.012).
atm(train,d231,d231_7,h,3,0.141).
atm(train,d231,d231_8,h,3,0.141).
atm(train,d231,d231_9,h,3,0.141).
atm(train,d231,d231_10,h,3,0.141).
atm(train,d231,d231_11,c,14,0.711).
atm(train,d231,d231_12,o,49,-0.639).
atm(train,d231,d231_13,c,10,0.261).
atm(train,d231,d231_14,c,10,0.112).
atm(train,d231,d231_15,c,16,-0.189).
atm(train,d231,d231_16,c,10,0.112).
atm(train,d231,d231_17,c,10,-0.088).
atm(train,d231,d231_18,c,10,-0.088).
atm(train,d231,d231_19,h,3,0.191).
atm(train,d231,d231_20,h,3,0.062).
atm(train,d231,d231_21,h,3,0.062).
atm(train,d231,d231_22,h,3,0.062).
atm(train,d231,d231_23,h,3,0.062).
atm(train,d231,d231_24,h,3,0.062).
atm(train,d231,d231_25,h,3,0.062).
atm(train,d231,d231_26,c,16,-0.189).
atm(train,d231,d231_27,c,10,0.112).
atm(train,d231,d231_28,c,10,-0.139).
atm(train,d231,d231_29,c,10,-0.139).
atm(train,d231,d231_30,h,3,0.062).
atm(train,d231,d231_31,h,3,0.062).
atm(train,d231,d231_32,h,3,0.062).
atm(train,d231,d231_33,c,10,-0.138).
atm(train,d231,d231_34,c,10,-0.289).
atm(train,d231,d231_35,c,10,-0.088).
atm(train,d231,d231_36,c,10,-0.088).
atm(train,d231,d231_37,h,3,-0.018).
atm(train,d231,d231_38,h,3,-0.018).
atm(train,d231,d231_39,h,3,0.062).
atm(train,d231,d231_40,h,3,0.062).
atm(train,d231,d231_41,h,3,0.062).
atm(train,d231,d231_42,h,3,0.062).
atm(train,d231,d231_43,c,10,-0.088).
atm(train,d231,d231_44,c,10,-0.088).
atm(train,d231,d231_45,c,10,-0.139).
atm(train,d231,d231_46,h,3,-0.018).
atm(train,d231,d231_47,h,3,0.062).
atm(train,d231,d231_48,h,3,0.062).
atm(train,d231,d231_49,h,3,0.062).
atm(train,d231,d231_50,h,3,0.062).
atm(train,d231,d231_51,o,51,-0.539).
atm(train,d231,d231_52,n,32,-0.389).
atm(train,d231,d231_53,c,10,0.012).
atm(train,d231,d231_54,c,10,0.112).
atm(train,d231,d231_55,h,3,0.062).
atm(train,d231,d231_56,h,3,0.062).
atm(train,d231,d231_57,cl,93,-0.189).
atm(train,d231,d231_58,h,3,0.062).
atm(train,d231,d231_59,h,3,0.062).
atm(train,d231,d231_60,c,10,0.012).
atm(train,d231,d231_61,c,10,0.112).
atm(train,d231,d231_62,h,3,0.062).
atm(train,d231,d231_63,h,3,0.062).
atm(train,d231,d231_64,cl,93,-0.189).
atm(train,d231,d231_65,h,3,0.062).
atm(train,d231,d231_66,h,3,0.062).
atm(train,d231,d231_67,h,3,0.112).
atm(train,d231,d231_68,c,10,-0.078).
atm(train,d231,d231_69,h,3,0.062).
atm(train,d231,d231_70,h,3,0.062).
atm(train,d231,d231_71,h,3,0.062).
atm(train,d231,d231_72,c,10,-0.139).
atm(train,d231,d231_73,h,3,-0.018).
atm(train,d231,d231_74,c,10,-0.139).
atm(train,d231,d231_75,h,3,0.062).
atm(train,d231,d231_76,h,3,0.062).
atm(train,d231,d231_77,h,3,0.062).
atm(train,d231,d231_78,c,10,-0.088).
atm(train,d231,d231_79,h,3,-0.018).
atm(train,d231,d231_80,c,10,-0.088).
atm(train,d231,d231_81,h,3,0.062).
atm(train,d231,d231_82,h,3,0.062).
atm(train,d231,d231_83,c,10,-0.088).
atm(train,d231,d231_84,h,3,0.062).
atm(train,d231,d231_85,h,3,0.062).
atm(train,d231,d231_86,c,10,-0.139).
atm(train,d231,d231_87,h,3,0.062).
atm(train,d231,d231_88,h,3,0.062).
atm(train,d231,d231_89,c,10,-0.139).
atm(train,d231,d231_90,h,3,0.062).
atm(train,d231,d231_91,h,3,0.062).
atm(train,d231,d231_92,h,3,0.062).
atm(train,d231,d231_93,c,10,-0.139).
atm(train,d231,d231_94,h,3,-0.018).
atm(train,d231,d231_95,h,3,0.062).
atm(train,d231,d231_96,h,3,0.062).
atm(train,d231,d231_97,h,3,0.062).
atm(train,d232,d232_1,c,191,-0.082).
atm(train,d232,d232_2,c,191,-0.082).
atm(train,d232,d232_3,c,10,0.108).
atm(train,d232,d232_4,h,3,0.068).
atm(train,d232,d232_5,h,3,0.068).
atm(train,d232,d232_6,h,3,0.068).
atm(train,d232,d232_7,o,50,-0.352).
atm(train,d232,d232_8,h,3,0.068).
atm(train,d232,d232_9,h,3,0.068).
atm(train,d232,d232_10,h,3,0.068).
atm(train,d233,d233_1,c,22,0.056).
atm(train,d233,d233_2,c,22,-0.074).
atm(train,d233,d233_3,c,22,-0.074).
atm(train,d233,d233_4,c,22,-0.074).
atm(train,d233,d233_5,c,22,-0.074).
atm(train,d233,d233_6,c,22,-0.074).
atm(train,d233,d233_7,h,3,0.186).
atm(train,d233,d233_8,h,3,0.186).
atm(train,d233,d233_9,cl,93,-0.144).
atm(train,d233,d233_10,cl,93,-0.144).
atm(train,d233,d233_11,cl,93,-0.144).
atm(train,d233,d233_12,c,14,0.757).
atm(train,d233,d233_13,o,49,-0.594).
atm(train,d233,d233_14,o,45,-0.594).
atm(train,d233,d233_15,cl,93,-0.144).
atm(train,d233,d233_16,p,62,1.167).
atm(train,d233,d233_17,o,40,-0.494).
atm(train,d233,d233_18,o,49,-0.614).
atm(train,d233,d233_19,c,10,0.146).
atm(train,d233,d233_20,h,3,0.126).
atm(train,d233,d233_21,h,3,0.126).
atm(train,d233,d233_22,h,3,0.126).
atm(train,d233,d233_23,o,49,-0.614).
atm(train,d233,d233_24,c,10,0.146).
atm(train,d233,d233_25,h,3,0.126).
atm(train,d233,d233_26,h,3,0.126).
atm(train,d233,d233_27,h,3,0.126).
atm(train,d233,d233_28,h,1,0.456).
atm(train,d234,d234_1,c,10,0.122).
atm(train,d234,d234_2,o,49,-0.637).
atm(train,d234,d234_3,h,3,0.102).
atm(train,d234,d234_4,h,3,0.102).
atm(train,d234,d234_5,h,3,0.102).
atm(train,d234,d234_6,p,62,1.143).
atm(train,d234,d234_7,o,49,-0.637).
atm(train,d234,d234_8,o,49,-0.637).
atm(train,d234,d234_9,c,10,0.123).
atm(train,d234,d234_10,h,3,0.102).
atm(train,d234,d234_11,h,3,0.102).
atm(train,d234,d234_12,h,3,0.102).
atm(train,d234,d234_13,c,10,0.122).
atm(train,d234,d234_14,h,3,0.102).
atm(train,d234,d234_15,h,3,0.102).
atm(train,d234,d234_16,h,3,0.102).
atm(train,d234,d234_17,o,40,-0.517).
atm(train,d235,d235_1,p,62,1.176).
atm(train,d235,d235_2,o,40,-0.484).
atm(train,d235,d235_3,o,49,-0.604).
atm(train,d235,d235_4,c,10,-0.005).
atm(train,d235,d235_5,c,10,-0.035).
atm(train,d235,d235_6,h,3,0.136).
atm(train,d235,d235_7,h,3,0.136).
atm(train,d235,d235_8,br,94,-0.085).
atm(train,d235,d235_9,c,10,-0.035).
atm(train,d235,d235_10,h,3,0.116).
atm(train,d235,d235_11,br,94,-0.085).
atm(train,d235,d235_12,h,3,0.115).
atm(train,d235,d235_13,h,3,0.116).
atm(train,d235,d235_14,o,49,-0.604).
atm(train,d235,d235_15,c,10,-0.005).
atm(train,d235,d235_16,o,49,-0.604).
atm(train,d235,d235_17,c,10,-0.005).
atm(train,d235,d235_18,c,10,-0.035).
atm(train,d235,d235_19,h,3,0.136).
atm(train,d235,d235_20,h,3,0.136).
atm(train,d235,d235_21,c,10,-0.035).
atm(train,d235,d235_22,br,94,-0.085).
atm(train,d235,d235_23,h,3,0.115).
atm(train,d235,d235_24,h,3,0.115).
atm(train,d235,d235_25,br,94,-0.085).
atm(train,d235,d235_26,h,3,0.115).
atm(train,d235,d235_27,c,10,-0.035).
atm(train,d235,d235_28,h,3,0.136).
atm(train,d235,d235_29,h,3,0.136).
atm(train,d235,d235_30,c,10,-0.035).
atm(train,d235,d235_31,br,94,-0.085).
atm(train,d235,d235_32,h,3,0.115).
atm(train,d235,d235_33,br,94,-0.085).
atm(train,d235,d235_34,h,3,0.116).
atm(train,d235,d235_35,h,3,0.116).
atm(train,d236,d236_1,c,16,-0.202).
atm(train,d236,d236_2,c,16,-0.202).
atm(train,d236,d236_3,h,3,0.098).
atm(train,d236,d236_4,h,3,0.098).
atm(train,d236,d236_5,c,10,0.088).
atm(train,d236,d236_6,h,3,0.047).
atm(train,d236,d236_7,h,3,0.047).
atm(train,d236,d236_8,h,3,0.047).
atm(train,d236,d236_9,c,10,0.088).
atm(train,d236,d236_10,cl,93,-0.203).
atm(train,d236,d236_11,h,3,0.047).
atm(train,d236,d236_12,h,3,0.047).
atm(train,d237,d237_1,p,62,1.162).
atm(train,d237,d237_2,o,40,-0.497).
atm(train,d237,d237_3,o,49,-0.618).
atm(train,d237,d237_4,o,49,-0.618).
atm(train,d237,d237_5,c,10,0.143).
atm(train,d237,d237_6,h,3,0.123).
atm(train,d237,d237_7,h,3,0.123).
atm(train,d237,d237_8,h,3,0.123).
atm(train,d237,d237_9,c,10,0.143).
atm(train,d237,d237_10,h,3,0.123).
atm(train,d237,d237_11,h,3,0.123).
atm(train,d237,d237_12,h,3,0.123).
atm(train,d237,d237_13,o,49,-0.618).
atm(train,d237,d237_14,c,16,-0.147).
atm(train,d237,d237_15,c,16,0.453).
atm(train,d237,d237_16,h,3,0.153).
atm(train,d237,d237_17,cl,93,-0.147).
atm(train,d237,d237_18,cl,93,-0.147).
atm(train,d238,d238_1,c,22,-0.111).
atm(train,d238,d238_2,c,22,-0.111).
atm(train,d238,d238_3,c,22,0.198).
atm(train,d238,d238_4,c,22,-0.111).
atm(train,d238,d238_5,c,22,0.198).
atm(train,d238,d238_6,c,22,-0.111).
atm(train,d238,d238_7,h,3,0.148).
atm(train,d238,d238_8,h,3,0.148).
atm(train,d238,d238_9,h,3,0.148).
atm(train,d238,d238_10,h,3,0.148).
atm(train,d238,d238_11,c,10,0.109).
atm(train,d238,d238_12,c,191,-0.081).
atm(train,d238,d238_13,c,191,-0.081).
atm(train,d238,d238_14,o,50,-0.352).
atm(train,d238,d238_15,h,3,0.069).
atm(train,d238,d238_16,h,3,0.069).
atm(train,d238,d238_17,h,3,0.069).
atm(train,d238,d238_18,o,50,-0.212).
atm(train,d238,d238_19,h,3,0.069).
atm(train,d238,d238_20,h,3,0.069).
atm(train,d238,d238_21,o,50,-0.212).
atm(train,d238,d238_22,c,10,0.109).
atm(train,d238,d238_23,c,191,-0.081).
atm(train,d238,d238_24,h,3,0.069).
atm(train,d238,d238_25,h,3,0.069).
atm(train,d238,d238_26,c,191,-0.081).
atm(train,d238,d238_27,o,50,-0.352).
atm(train,d238,d238_28,h,3,0.069).
atm(train,d238,d238_29,h,3,0.069).
atm(train,d238,d238_30,h,3,0.069).
atm(train,d239,d239_1,c,10,-0.088).
atm(train,d239,d239_2,c,10,-0.088).
atm(train,d239,d239_3,br,94,-0.139).
atm(train,d239,d239_4,h,3,0.063).
atm(train,d239,d239_5,h,3,0.063).
atm(train,d239,d239_6,h,3,0.063).
atm(train,d239,d239_7,h,3,0.063).
atm(train,d239,d239_8,h,3,0.063).
atm(train,d23_1,d23_1_1,c,22,0.189).
atm(train,d23_1,d23_1_2,c,22,-0.141).
atm(train,d23_1,d23_1_3,c,22,-0.141).
atm(train,d23_1,d23_1_4,c,22,0.169).
atm(train,d23_1,d23_1_5,c,22,-0.141).
atm(train,d23_1,d23_1_6,c,22,-0.141).
atm(train,d23_1,d23_1_7,h,3,0.119).
atm(train,d23_1,d23_1_8,h,3,0.119).
atm(train,d23_1,d23_1_9,h,3,0.119).
atm(train,d23_1,d23_1_10,h,3,0.119).
atm(train,d23_1,d23_1_11,c,22,0.169).
atm(train,d23_1,d23_1_12,c,22,-0.141).
atm(train,d23_1,d23_1_13,c,22,-0.141).
atm(train,d23_1,d23_1_14,c,22,0.189).
atm(train,d23_1,d23_1_15,c,22,-0.141).
atm(train,d23_1,d23_1_16,c,22,-0.141).
atm(train,d23_1,d23_1_17,h,3,0.119).
atm(train,d23_1,d23_1_18,h,3,0.119).
atm(train,d23_1,d23_1_19,h,3,0.119).
atm(train,d23_1,d23_1_20,h,3,0.119).
atm(train,d23_1,d23_1_21,o,50,-0.24).
atm(train,d23_1,d23_1_22,n,32,-0.79).
atm(train,d23_1,d23_1_23,n,32,-0.79).
atm(train,d23_1,d23_1_24,h,1,0.32).
atm(train,d23_1,d23_1_25,h,1,0.32).
atm(train,d23_1,d23_1_26,h,1,0.32).
atm(train,d23_1,d23_1_27,h,1,0.32).
atm(train,d23_2,d23_2_1,c,22,-0.169).
atm(train,d23_2,d23_2_2,c,22,-0.169).
atm(train,d23_2,d23_2_3,c,22,-0.169).
atm(train,d23_2,d23_2_4,c,22,0.161).
atm(train,d23_2,d23_2_5,c,22,-0.169).
atm(train,d23_2,d23_2_6,c,22,-0.169).
atm(train,d23_2,d23_2_7,h,3,0.091).
atm(train,d23_2,d23_2_8,h,3,0.091).
atm(train,d23_2,d23_2_9,h,3,0.091).
atm(train,d23_2,d23_2_10,h,3,0.091).
atm(train,d23_2,d23_2_11,h,3,0.091).
atm(train,d23_2,d23_2_12,c,22,0.161).
atm(train,d23_2,d23_2_13,c,22,-0.169).
atm(train,d23_2,d23_2_14,c,22,-0.169).
atm(train,d23_2,d23_2_15,c,22,0.9).
atm(train,d23_2,d23_2_16,n,35,-0.81).
atm(train,d23_2,d23_2_17,c,22,0.73).
atm(train,d23_2,d23_2_18,h,3,0.091).
atm(train,d23_2,d23_2_19,h,3,0.091).
atm(train,d23_2,d23_2_20,n,32,-0.44).
atm(train,d23_2,d23_2_21,n,32,-0.44).
atm(train,d23_2,d23_2_22,n,32,-0.44).
atm(train,d23_2,d23_2_23,n,32,-0.44).
atm(train,d23_2,d23_2_24,h,1,0.291).
atm(train,d23_2,d23_2_25,h,1,0.291).
atm(train,d23_2,d23_2_26,h,1,0.291).
atm(train,d23_2,d23_2_27,h,1,0.291).
atm(train,d24,d24_1,c,22,0.208).
atm(train,d24,d24_2,c,22,-0.123).
atm(train,d24,d24_3,c,22,-0.123).
atm(train,d24,d24_4,c,22,-0.123).
atm(train,d24,d24_5,c,22,-0.123).
atm(train,d24,d24_6,c,22,-0.123).
atm(train,d24,d24_7,h,3,0.137).
atm(train,d24,d24_8,h,3,0.138).
atm(train,d24,d24_9,h,3,0.138).
atm(train,d24,d24_10,h,3,0.138).
atm(train,d24,d24_11,c,22,-0.123).
atm(train,d24,d24_12,c,22,-0.123).
atm(train,d24,d24_13,c,22,-0.123).
atm(train,d24,d24_14,c,22,0.208).
atm(train,d24,d24_15,c,22,-0.123).
atm(train,d24,d24_16,c,22,-0.123).
atm(train,d24,d24_17,h,3,0.137).
atm(train,d24,d24_18,h,3,0.137).
atm(train,d24,d24_19,h,3,0.137).
atm(train,d24,d24_20,h,3,0.137).
atm(train,d24,d24_21,s,74,-0.093).
atm(train,d24,d24_22,n,32,-0.772).
atm(train,d24,d24_23,n,32,-0.772).
atm(train,d24,d24_24,h,1,0.338).
atm(train,d24,d24_25,h,1,0.338).
atm(train,d24,d24_26,h,1,0.338).
atm(train,d24,d24_27,h,1,0.338).
atm(train,d240,d240_1,c,191,-0.056).
atm(train,d240,d240_2,c,191,-0.056).
atm(train,d240,d240_3,o,50,-0.327).
atm(train,d240,d240_4,h,3,0.094).
atm(train,d240,d240_5,h,3,0.094).
atm(train,d240,d240_6,c,10,0.133).
atm(train,d240,d240_7,h,3,0.094).
atm(train,d240,d240_8,o,45,-0.607).
atm(train,d240,d240_9,h,3,0.094).
atm(train,d240,d240_10,h,3,0.094).
atm(train,d240,d240_11,h,8,0.443).
atm(train,d241,d241_1,c,10,0.174).
atm(train,d241,d241_2,c,10,0.194).
atm(train,d241,d241_3,o,45,-0.647).
atm(train,d241,d241_4,h,3,0.044).
atm(train,d241,d241_5,h,3,0.044).
atm(train,d241,d241_6,o,45,-0.647).
atm(train,d241,d241_7,c,10,-0.096).
atm(train,d241,d241_8,h,3,0.064).
atm(train,d241,d241_9,i,95,-0.046).
atm(train,d241,d241_10,h,3,0.054).
atm(train,d241,d241_11,h,3,0.054).
atm(train,d241,d241_12,h,8,0.404).
atm(train,d241,d241_13,h,8,0.404).
atm(train,d242,d242_1,c,191,-0.057).
atm(train,d242,d242_2,c,10,-0.057).
atm(train,d242,d242_3,c,10,-0.057).
atm(train,d242,d242_4,c,10,-0.057).
atm(train,d242,d242_5,c,10,-0.057).
atm(train,d242,d242_6,c,191,-0.057).
atm(train,d242,d242_7,h,3,0.092).
atm(train,d242,d242_8,h,3,0.093).
atm(train,d242,d242_9,h,3,0.093).
atm(train,d242,d242_10,h,3,0.093).
atm(train,d242,d242_11,h,3,0.093).
atm(train,d242,d242_12,h,3,0.093).
atm(train,d242,d242_13,o,50,-0.328).
atm(train,d242,d242_14,h,3,0.092).
atm(train,d242,d242_15,h,3,0.092).
atm(train,d242,d242_16,c,191,-0.057).
atm(train,d242,d242_17,h,3,0.093).
atm(train,d242,d242_18,c,191,-0.057).
atm(train,d242,d242_19,o,50,-0.328).
atm(train,d242,d242_20,h,3,0.092).
atm(train,d242,d242_21,h,3,0.093).
atm(train,d242,d242_22,h,3,0.093).
atm(train,d243,d243_1,c,10,0.108).
atm(train,d243,d243_2,cl,93,-0.191).
atm(train,d243,d243_3,c,10,0.073).
atm(train,d243,d243_4,h,3,0.058).
atm(train,d243,d243_5,h,3,0.058).
atm(train,d243,d243_6,c,10,-0.142).
atm(train,d243,d243_7,h,3,0.058).
atm(train,d243,d243_8,h,3,0.058).
atm(train,d243,d243_9,h,3,0.058).
atm(train,d243,d243_10,o,50,-0.361).
atm(train,d243,d243_11,h,3,0.043).
atm(train,d243,d243_12,c,10,0.073).
atm(train,d243,d243_13,c,10,-0.142).
atm(train,d243,d243_14,h,3,0.058).
atm(train,d243,d243_15,h,3,0.058).
atm(train,d243,d243_16,h,3,0.058).
atm(train,d243,d243_17,c,10,0.108).
atm(train,d243,d243_18,h,3,0.043).
atm(train,d243,d243_19,cl,93,-0.192).
atm(train,d243,d243_20,h,3,0.058).
atm(train,d243,d243_21,h,3,0.058).
atm(train,d244,d244_1,p,61,1.295).
atm(train,d244,d244_2,o,40,-0.562).
atm(train,d244,d244_3,o,49,-0.682).
atm(train,d244,d244_4,h,1,0.239).
atm(train,d244,d244_5,o,49,-0.682).
atm(train,d244,d244_6,c,10,0.079).
atm(train,d244,d244_7,h,3,0.039).
atm(train,d244,d244_8,h,3,0.039).
atm(train,d244,d244_9,h,3,0.039).
atm(train,d244,d244_10,c,10,0.079).
atm(train,d244,d244_11,h,3,0.039).
atm(train,d244,d244_12,h,3,0.039).
atm(train,d244,d244_13,h,3,0.039).
atm(train,d245,d245_1,c,191,-0.072).
atm(train,d245,d245_2,c,191,-0.072).
atm(train,d245,d245_3,c,10,-0.072).
atm(train,d245,d245_4,c,10,-0.072).
atm(train,d245,d245_5,h,3,0.079).
atm(train,d245,d245_6,h,3,0.079).
atm(train,d245,d245_7,h,3,0.078).
atm(train,d245,d245_8,h,3,0.078).
atm(train,d245,d245_9,h,3,0.079).
atm(train,d245,d245_10,o,50,-0.341).
atm(train,d245,d245_11,h,3,0.079).
atm(train,d245,d245_12,h,3,0.079).
atm(train,d245,d245_13,h,3,0.078).
atm(train,d246,d246_1,c,22,-0.12).
atm(train,d246,d246_2,c,22,0.01).
atm(train,d246,d246_3,c,22,-0.12).
atm(train,d246,d246_4,c,22,-0.12).
atm(train,d246,d246_5,c,22,0.21).
atm(train,d246,d246_6,c,22,-0.12).
atm(train,d246,d246_7,h,3,0.14).
atm(train,d246,d246_8,h,3,0.14).
atm(train,d246,d246_9,h,3,0.14).
atm(train,d246,d246_10,h,3,0.14).
atm(train,d246,d246_11,n,32,-0.391).
atm(train,d246,d246_12,c,10,0.01).
atm(train,d246,d246_13,c,10,0.11).
atm(train,d246,d246_14,h,3,0.06).
atm(train,d246,d246_15,h,3,0.06).
atm(train,d246,d246_16,cl,93,-0.19).
atm(train,d246,d246_17,h,3,0.06).
atm(train,d246,d246_18,h,3,0.06).
atm(train,d246,d246_19,c,10,0.01).
atm(train,d246,d246_20,c,10,0.11).
atm(train,d246,d246_21,h,3,0.06).
atm(train,d246,d246_22,h,3,0.06).
atm(train,d246,d246_23,cl,93,-0.19).
atm(train,d246,d246_24,h,3,0.06).
atm(train,d246,d246_25,h,3,0.06).
atm(train,d246,d246_26,c,14,0.709).
atm(train,d246,d246_27,o,49,-0.641).
atm(train,d246,d246_28,c,22,0.26).
atm(train,d246,d246_29,o,51,-0.541).
atm(train,d246,d246_30,c,22,-0.12).
atm(train,d246,d246_31,c,22,0.01).
atm(train,d246,d246_32,c,22,-0.12).
atm(train,d246,d246_33,c,22,-0.12).
atm(train,d246,d246_34,c,22,-0.12).
atm(train,d246,d246_35,h,3,0.14).
atm(train,d246,d246_36,h,3,0.14).
atm(train,d246,d246_37,h,3,0.14).
atm(train,d246,d246_38,c,10,-0.09).
atm(train,d246,d246_39,c,10,-0.09).
atm(train,d246,d246_40,c,10,-0.14).
atm(train,d246,d246_41,c,10,0.01).
atm(train,d246,d246_42,h,3,0.06).
atm(train,d246,d246_43,h,3,0.06).
atm(train,d246,d246_44,h,3,0.06).
atm(train,d246,d246_45,h,3,0.06).
atm(train,d246,d246_46,c,10,-0.14).
atm(train,d246,d246_47,c,10,-0.291).
atm(train,d246,d246_48,c,10,-0.09).
atm(train,d246,d246_49,c,10,-0.09).
atm(train,d246,d246_50,h,3,0.11).
atm(train,d246,d246_51,h,3,-0.02).
atm(train,d246,d246_52,h,3,0.06).
atm(train,d246,d246_53,h,3,0.06).
atm(train,d246,d246_54,h,3,0.06).
atm(train,d246,d246_55,h,3,0.06).
atm(train,d246,d246_56,c,10,-0.09).
atm(train,d246,d246_57,c,10,-0.09).
atm(train,d246,d246_58,c,10,0.259).
atm(train,d246,d246_59,h,3,-0.02).
atm(train,d246,d246_60,h,3,0.06).
atm(train,d246,d246_61,h,3,0.06).
atm(train,d246,d246_62,h,3,0.06).
atm(train,d246,d246_63,h,3,0.06).
atm(train,d246,d246_64,c,10,-0.08).
atm(train,d246,d246_65,h,3,0.06).
atm(train,d246,d246_66,h,3,0.06).
atm(train,d246,d246_67,h,3,0.06).
atm(train,d246,d246_68,o,49,-0.641).
atm(train,d246,d246_69,h,3,0.19).
atm(train,d246,d246_70,c,14,0.709).
atm(train,d246,d246_71,c,22,0.01).
atm(train,d246,d246_72,o,51,-0.541).
atm(train,d246,d246_73,c,22,-0.12).
atm(train,d246,d246_74,c,22,0.21).
atm(train,d246,d246_75,c,22,-0.12).
atm(train,d246,d246_76,c,22,-0.12).
atm(train,d246,d246_77,c,22,-0.12).
atm(train,d246,d246_78,h,3,0.14).
atm(train,d246,d246_79,h,3,0.14).
atm(train,d246,d246_80,h,3,0.14).
atm(train,d246,d246_81,h,3,0.14).
atm(train,d246,d246_82,n,32,-0.391).
atm(train,d246,d246_83,c,10,0.01).
atm(train,d246,d246_84,c,10,0.11).
atm(train,d246,d246_85,h,3,0.06).
atm(train,d246,d246_86,h,3,0.06).
atm(train,d246,d246_87,cl,93,-0.19).
atm(train,d246,d246_88,h,3,0.06).
atm(train,d246,d246_89,h,3,0.06).
atm(train,d246,d246_90,c,10,0.01).
atm(train,d246,d246_91,c,10,0.11).
atm(train,d246,d246_92,h,3,0.06).
atm(train,d246,d246_93,h,3,0.06).
atm(train,d246,d246_94,cl,93,-0.19).
atm(train,d246,d246_95,h,3,0.06).
atm(train,d246,d246_96,h,3,0.06).
atm(train,d247,d247_1,c,10,0.011).
atm(train,d247,d247_2,c,10,0.031).
atm(train,d247,d247_3,o,50,-0.358).
atm(train,d247,d247_4,c,10,0.031).
atm(train,d247,d247_5,c,10,0.011).
atm(train,d247,d247_6,n,36,-0.288).
atm(train,d247,d247_7,h,3,0.061).
atm(train,d247,d247_8,h,3,0.061).
atm(train,d247,d247_9,h,3,0.071).
atm(train,d247,d247_10,h,3,0.071).
atm(train,d247,d247_11,h,3,0.071).
atm(train,d247,d247_12,h,3,0.071).
atm(train,d247,d247_13,h,3,0.061).
atm(train,d247,d247_14,h,3,0.061).
atm(train,d247,d247_15,p,61,1.319).
atm(train,d247,d247_16,o,49,-0.658).
atm(train,d247,d247_17,o,49,-0.658).
atm(train,d247,d247_18,c,10,0.102).
atm(train,d247,d247_19,h,3,0.061).
atm(train,d247,d247_20,h,3,0.061).
atm(train,d247,d247_21,h,3,0.061).
atm(train,d247,d247_22,c,10,0.101).
atm(train,d247,d247_23,h,3,0.061).
atm(train,d247,d247_24,h,3,0.061).
atm(train,d247,d247_25,h,3,0.061).
atm(train,d247,d247_26,o,40,-0.538).
atm(train,d248,d248_1,o,49,-0.652).
atm(train,d248,d248_2,c,193,-0.083).
atm(train,d248,d248_3,c,192,0.598).
atm(train,d248,d248_4,c,193,-0.083).
atm(train,d248,d248_5,h,3,0.067).
atm(train,d248,d248_6,h,3,0.067).
atm(train,d248,d248_7,c,10,0.108).
atm(train,d248,d248_8,h,3,0.067).
atm(train,d248,d248_9,h,3,0.067).
atm(train,d248,d248_10,h,3,0.067).
atm(train,d248,d248_11,c,10,0.108).
atm(train,d248,d248_12,h,3,0.067).
atm(train,d248,d248_13,h,3,0.067).
atm(train,d248,d248_14,h,3,0.067).
atm(train,d248,d248_15,o,51,-0.532).
atm(train,d249,d249_1,c,10,-0.271).
atm(train,d249,d249_2,c,10,-0.121).
atm(train,d249,d249_3,c,10,-0.072).
atm(train,d249,d249_4,c,10,-0.072).
atm(train,d249,d249_5,c,10,-0.121).
atm(train,d249,d249_6,c,10,-0.072).
atm(train,d249,d249_7,h,3,0.079).
atm(train,d249,d249_8,h,3,0.078).
atm(train,d249,d249_9,h,3,0.078).
atm(train,d249,d249_10,h,3,0.078).
atm(train,d249,d249_11,c,10,-0.072).
atm(train,d249,d249_12,h,3,-0.002).
atm(train,d249,d249_13,h,3,0.078).
atm(train,d249,d249_14,h,3,0.078).
atm(train,d249,d249_15,h,3,-0.002).
atm(train,d249,d249_16,c,10,-0.062).
atm(train,d249,d249_17,h,3,0.078).
atm(train,d249,d249_18,h,3,0.078).
atm(train,d249,d249_19,h,3,0.078).
atm(train,d249,d249_20,c,10,-0.062).
atm(train,d249,d249_21,h,3,0.079).
atm(train,d249,d249_22,h,3,0.078).
atm(train,d249,d249_23,h,3,0.078).
atm(train,d249,d249_24,c,10,-0.072).
atm(train,d249,d249_25,h,3,0.078).
atm(train,d249,d249_26,h,3,0.078).
atm(train,d249,d249_27,h,3,0.078).
atm(train,d249,d249_28,cl,93,-0.171).
atm(train,d25,d25_1,c,22,-0.133).
atm(train,d25,d25_2,c,22,-0.133).
atm(train,d25,d25_3,c,22,-0.133).
atm(train,d25,d25_4,c,22,-0.183).
atm(train,d25,d25_5,c,22,0.197).
atm(train,d25,d25_6,c,22,-0.133).
atm(train,d25,d25_7,h,3,0.127).
atm(train,d25,d25_8,h,3,0.127).
atm(train,d25,d25_9,h,3,0.127).
atm(train,d25,d25_10,h,3,0.127).
atm(train,d25,d25_11,n,32,-0.782).
atm(train,d25,d25_12,c,10,-0.003).
atm(train,d25,d25_13,h,3,0.047).
atm(train,d25,d25_14,h,3,0.047).
atm(train,d25,d25_15,h,3,0.047).
atm(train,d25,d25_16,h,1,0.327).
atm(train,d25,d25_17,h,1,0.327).
atm(train,d250,d250_1,cl,93,-0.2).
atm(train,d250,d250_2,c,10,0.1).
atm(train,d250,d250_3,c,10,0.15).
atm(train,d250,d250_4,h,3,0.05).
atm(train,d250,d250_5,h,3,0.05).
atm(train,d250,d250_6,cl,93,-0.2).
atm(train,d250,d250_7,c,10,-0.15).
atm(train,d250,d250_8,h,3,0.05).
atm(train,d250,d250_9,h,3,0.05).
atm(train,d250,d250_10,h,3,0.05).
atm(train,d250,d250_11,h,3,0.05).
atm(train,d251,d251_1,o,40,-0.503).
atm(train,d251,d251_2,p,62,1.157).
atm(train,d251,d251_3,o,49,-0.623).
atm(train,d251,d251_4,c,10,-0.022).
atm(train,d251,d251_5,c,10,0.148).
atm(train,d251,d251_6,h,3,0.118).
atm(train,d251,d251_7,h,3,0.118).
atm(train,d251,d251_8,cl,93,-0.153).
atm(train,d251,d251_9,h,3,0.098).
atm(train,d251,d251_10,h,3,0.098).
atm(train,d251,d251_11,o,49,-0.623).
atm(train,d251,d251_12,c,10,-0.022).
atm(train,d251,d251_13,c,10,0.148).
atm(train,d251,d251_14,h,3,0.118).
atm(train,d251,d251_15,h,3,0.118).
atm(train,d251,d251_16,cl,93,-0.153).
atm(train,d251,d251_17,h,3,0.098).
atm(train,d251,d251_18,h,3,0.098).
atm(train,d251,d251_19,o,49,-0.623).
atm(train,d251,d251_20,c,10,-0.022).
atm(train,d251,d251_21,c,10,0.148).
atm(train,d251,d251_22,h,3,0.118).
atm(train,d251,d251_23,h,3,0.118).
atm(train,d251,d251_24,cl,93,-0.153).
atm(train,d251,d251_25,h,3,0.098).
atm(train,d251,d251_26,h,3,0.098).
atm(train,d252,d252_1,c,16,-0.167).
atm(train,d252,d252_2,c,16,-0.167).
atm(train,d252,d252_3,h,3,0.134).
atm(train,d252,d252_4,h,3,0.133).
atm(train,d252,d252_5,c,10,0.124).
atm(train,d252,d252_6,h,3,0.134).
atm(train,d252,d252_7,n,32,-0.367).
atm(train,d252,d252_8,h,3,0.084).
atm(train,d252,d252_9,h,3,0.084).
atm(train,d252,d252_10,c,15,0.124).
atm(train,d252,d252_11,s,75,-0.116).
atm(train,d253,d253_1,p,61,1.299).
atm(train,d253,d253_2,o,40,-0.558).
atm(train,d253,d253_3,c,10,0.082).
atm(train,d253,d253_4,h,3,0.041).
atm(train,d253,d253_5,h,3,0.041).
atm(train,d253,d253_6,h,3,0.041).
atm(train,d253,d253_7,o,49,-0.678).
atm(train,d253,d253_8,c,10,0.082).
atm(train,d253,d253_9,h,3,0.041).
atm(train,d253,d253_10,h,3,0.041).
atm(train,d253,d253_11,h,3,0.041).
atm(train,d253,d253_12,o,49,-0.678).
atm(train,d253,d253_13,c,10,0.082).
atm(train,d253,d253_14,h,3,0.041).
atm(train,d253,d253_15,h,3,0.041).
atm(train,d253,d253_16,h,3,0.041).
atm(train,d254,d254_1,c,191,-0.068).
atm(train,d254,d254_2,c,191,-0.068).
atm(train,d254,d254_3,o,50,-0.337).
atm(train,d254,d254_4,h,3,0.082).
atm(train,d254,d254_5,h,3,0.082).
atm(train,d254,d254_6,c,10,0.052).
atm(train,d254,d254_7,h,3,0.082).
atm(train,d254,d254_8,o,50,-0.337).
atm(train,d254,d254_9,h,3,0.082).
atm(train,d254,d254_10,h,3,0.082).
atm(train,d254,d254_11,c,10,0.122).
atm(train,d254,d254_12,c,16,-0.167).
atm(train,d254,d254_13,h,3,0.082).
atm(train,d254,d254_14,h,3,0.082).
atm(train,d254,d254_15,c,16,-0.167).
atm(train,d254,d254_16,h,3,0.132).
atm(train,d254,d254_17,h,3,0.132).
atm(train,d254,d254_18,h,3,0.132).
atm(train,d255,d255_1,c,10,-0.15).
atm(train,d255,d255_2,c,10,-0.1).
atm(train,d255,d255_3,h,3,0.05).
atm(train,d255,d255_4,h,3,0.05).
atm(train,d255,d255_5,h,3,0.05).
atm(train,d255,d255_6,c,10,0.1).
atm(train,d255,d255_7,h,3,0.05).
atm(train,d255,d255_8,h,3,0.05).
atm(train,d255,d255_9,cl,93,-0.2).
atm(train,d255,d255_10,h,3,0.05).
atm(train,d255,d255_11,h,3,0.05).
atm(train,d256,d256_1,c,22,0.077).
atm(train,d256,d256_2,c,22,-0.053).
atm(train,d256,d256_3,c,22,-0.053).
atm(train,d256,d256_4,c,22,-0.053).
atm(train,d256,d256_5,c,22,-0.053).
atm(train,d256,d256_6,c,22,0.277).
atm(train,d256,d256_7,h,3,0.207).
atm(train,d256,d256_8,h,3,0.207).
atm(train,d256,d256_9,n,32,-0.323).
atm(train,d256,d256_10,n,32,-0.323).
atm(train,d256,d256_11,n,32,-0.323).
atm(train,d256,d256_12,c,14,0.628).
atm(train,d256,d256_13,c,10,0.167).
atm(train,d256,d256_14,s,74,-0.043).
atm(train,d256,d256_15,h,3,0.127).
atm(train,d256,d256_16,h,3,0.127).
atm(train,d256,d256_17,p,60,-0.223).
atm(train,d256,d256_18,o,49,-0.592).
atm(train,d256,d256_19,o,49,-0.592).
atm(train,d256,d256_20,c,10,0.167).
atm(train,d256,d256_21,h,3,0.127).
atm(train,d256,d256_22,h,3,0.127).
atm(train,d256,d256_23,h,3,0.127).
atm(train,d256,d256_24,s,70,-0.223).
atm(train,d256,d256_25,c,10,0.167).
atm(train,d256,d256_26,h,3,0.127).
atm(train,d256,d256_27,h,3,0.127).
atm(train,d256,d256_28,h,3,0.127).
atm(train,d256,d256_29,o,40,-0.473).
atm(train,d256,d256_30,h,3,0.207).
atm(train,d256,d256_31,h,3,0.207).
atm(train,d257,d257_1,c,10,-0.086).
atm(train,d257,d257_2,c,10,-0.086).
atm(train,d257,d257_3,h,3,0.064).
atm(train,d257,d257_4,h,3,0.064).
atm(train,d257,d257_5,h,3,0.064).
atm(train,d257,d257_6,o,49,-0.655).
atm(train,d257,d257_7,h,3,0.064).
atm(train,d257,d257_8,h,3,0.064).
atm(train,d257,d257_9,p,61,1.322).
atm(train,d257,d257_10,o,49,-0.655).
atm(train,d257,d257_11,c,10,-0.086).
atm(train,d257,d257_12,c,10,-0.086).
atm(train,d257,d257_13,h,3,0.064).
atm(train,d257,d257_14,h,3,0.064).
atm(train,d257,d257_15,h,3,0.064).
atm(train,d257,d257_16,h,3,0.064).
atm(train,d257,d257_17,h,3,0.064).
atm(train,d257,d257_18,s,75,-0.136).
atm(train,d257,d257_19,o,49,-0.655).
atm(train,d257,d257_20,c,22,0.265).
atm(train,d257,d257_21,c,22,-0.116).
atm(train,d257,d257_22,c,22,-0.116).
atm(train,d257,d257_23,c,22,-0.166).
atm(train,d257,d257_24,c,22,-0.116).
atm(train,d257,d257_25,c,22,-0.116).
atm(train,d257,d257_26,h,3,0.144).
atm(train,d257,d257_27,h,3,0.144).
atm(train,d257,d257_28,h,3,0.144).
atm(train,d257,d257_29,s,74,-0.086).
atm(train,d257,d257_30,c,10,0.104).
atm(train,d257,d257_31,h,3,0.064).
atm(train,d257,d257_32,h,3,0.064).
atm(train,d257,d257_33,h,3,0.064).
atm(train,d257,d257_34,c,10,0.014).
atm(train,d257,d257_35,h,3,0.064).
atm(train,d257,d257_36,h,3,0.064).
atm(train,d257,d257_37,h,3,0.064).
atm(train,d258,d258_1,p,62,1.142).
atm(train,d258,d258_2,o,40,-0.518).
atm(train,d258,d258_3,o,49,-0.638).
atm(train,d258,d258_4,c,10,0.122).
atm(train,d258,d258_5,h,3,0.101).
atm(train,d258,d258_6,h,3,0.101).
atm(train,d258,d258_7,h,3,0.101).
atm(train,d258,d258_8,o,49,-0.638).
atm(train,d258,d258_9,c,10,0.122).
atm(train,d258,d258_10,h,3,0.101).
atm(train,d258,d258_11,h,3,0.101).
atm(train,d258,d258_12,h,3,0.101).
atm(train,d258,d258_13,o,49,-0.638).
atm(train,d258,d258_14,c,16,-0.069).
atm(train,d258,d258_15,c,10,0.122).
atm(train,d258,d258_16,h,3,0.081).
atm(train,d258,d258_17,h,3,0.081).
atm(train,d258,d258_18,h,3,0.081).
atm(train,d258,d258_19,c,16,-0.168).
atm(train,d258,d258_20,cl,93,-0.168).
atm(train,d258,d258_21,c,14,0.632).
atm(train,d258,d258_22,o,40,-0.518).
atm(train,d258,d258_23,n,32,-0.368).
atm(train,d258,d258_24,c,10,0.031).
atm(train,d258,d258_25,c,10,-0.069).
atm(train,d258,d258_26,h,3,0.081).
atm(train,d258,d258_27,h,3,0.081).
atm(train,d258,d258_28,h,3,0.081).
atm(train,d258,d258_29,h,3,0.081).
atm(train,d258,d258_30,h,3,0.081).
atm(train,d258,d258_31,c,10,0.031).
atm(train,d258,d258_32,c,10,-0.069).
atm(train,d258,d258_33,h,3,0.081).
atm(train,d258,d258_34,h,3,0.081).
atm(train,d258,d258_35,h,3,0.081).
atm(train,d258,d258_36,h,3,0.081).
atm(train,d258,d258_37,h,3,0.081).
atm(train,d259,d259_1,c,22,-0.128).
atm(train,d259,d259_2,c,22,-0.128).
atm(train,d259,d259_3,c,22,-0.128).
atm(train,d259,d259_4,c,22,-0.128).
atm(train,d259,d259_5,c,22,0.002).
atm(train,d259,d259_6,c,22,-0.128).
atm(train,d259,d259_7,h,3,0.133).
atm(train,d259,d259_8,h,3,0.132).
atm(train,d259,d259_9,h,3,0.133).
atm(train,d259,d259_10,h,3,0.132).
atm(train,d259,d259_11,h,3,0.133).
atm(train,d259,d259_12,c,14,0.553).
atm(train,d259,d259_13,c,10,0.092).
atm(train,d259,d259_14,cl,93,-0.197).
atm(train,d259,d259_15,h,3,0.052).
atm(train,d259,d259_16,h,3,0.052).
atm(train,d259,d259_17,o,42,-0.577).
atm(train,d26,d26_1,c,22,-0.179).
atm(train,d26,d26_2,c,22,-0.179).
atm(train,d26,d26_3,c,22,-0.13).
atm(train,d26,d26_4,c,22,-0.179).
atm(train,d26,d26_5,c,22,0.201).
atm(train,d26,d26_6,c,22,-0.129).
atm(train,d26,d26_7,h,3,0.131).
atm(train,d26,d26_8,h,3,0.131).
atm(train,d26,d26_9,n,32,-0.779).
atm(train,d26,d26_10,c,10,0.0).
atm(train,d26,d26_11,h,3,0.05).
atm(train,d26,d26_12,h,3,0.05).
atm(train,d26,d26_13,h,3,0.05).
atm(train,d26,d26_14,c,10,0.0).
atm(train,d26,d26_15,h,3,0.05).
atm(train,d26,d26_16,h,3,0.05).
atm(train,d26,d26_17,h,3,0.05).
atm(train,d26,d26_18,c,10,0.0).
atm(train,d26,d26_19,h,3,0.05).
atm(train,d26,d26_20,h,3,0.05).
atm(train,d26,d26_21,h,3,0.05).
atm(train,d26,d26_22,h,1,0.331).
atm(train,d26,d26_23,h,1,0.331).
atm(train,d260,d260_1,c,10,-0.258).
atm(train,d260,d260_2,o,50,-0.188).
atm(train,d260,d260_3,n,38,0.842).
atm(train,d260,d260_4,o,40,-0.358).
atm(train,d260,d260_5,o,40,-0.358).
atm(train,d260,d260_6,o,50,-0.188).
atm(train,d260,d260_7,n,38,0.842).
atm(train,d260,d260_8,o,40,-0.358).
atm(train,d260,d260_9,o,40,-0.358).
atm(train,d260,d260_10,o,50,-0.188).
atm(train,d260,d260_11,n,38,0.843).
atm(train,d260,d260_12,o,50,-0.188).
atm(train,d260,d260_13,n,38,0.843).
atm(train,d260,d260_14,o,40,-0.358).
atm(train,d260,d260_15,o,40,-0.358).
atm(train,d260,d260_16,o,40,-0.358).
atm(train,d260,d260_17,o,40,-0.358).
atm(train,d260,d260_18,c,10,-0.058).
atm(train,d260,d260_19,h,3,0.092).
atm(train,d260,d260_20,h,3,0.092).
atm(train,d260,d260_21,c,10,-0.058).
atm(train,d260,d260_22,h,3,0.092).
atm(train,d260,d260_23,h,3,0.092).
atm(train,d260,d260_24,c,10,-0.058).
atm(train,d260,d260_25,h,3,0.092).
atm(train,d260,d260_26,h,3,0.092).
atm(train,d260,d260_27,c,10,-0.058).
atm(train,d260,d260_28,h,3,0.092).
atm(train,d260,d260_29,h,3,0.092).
atm(train,d261,d261_1,c,10,-0.15).
atm(train,d261,d261_2,c,10,-0.1).
atm(train,d261,d261_3,c,10,-0.1).
atm(train,d261,d261_4,h,3,0.05).
atm(train,d261,d261_5,h,3,0.05).
atm(train,d261,d261_6,c,10,0.1).
atm(train,d261,d261_7,h,3,0.05).
atm(train,d261,d261_8,h,3,0.05).
atm(train,d261,d261_9,cl,93,-0.2).
atm(train,d261,d261_10,h,3,0.05).
atm(train,d261,d261_11,h,3,0.05).
atm(train,d261,d261_12,h,3,0.05).
atm(train,d261,d261_13,h,3,0.05).
atm(train,d261,d261_14,h,3,0.05).
atm(train,d262,d262_1,n,31,-0.625).
atm(train,d262,d262_2,c,19,-0.525).
atm(train,d262,d262_3,n,31,-0.625).
atm(train,d262,d262_4,ca,84,1.775).
atm(train,d263,d263_1,cl,93,-0.2).
atm(train,d263,d263_2,c,10,0.1).
atm(train,d263,d263_3,c,10,0.17).
atm(train,d263,d263_4,h,3,0.05).
atm(train,d263,d263_5,h,3,0.05).
atm(train,d263,d263_6,o,45,-0.65).
atm(train,d263,d263_7,h,3,0.04).
atm(train,d263,d263_8,h,3,0.04).
atm(train,d263,d263_9,h,8,0.4).
atm(train,d264,d264_1,c,10,-0.073).
atm(train,d264,d264_2,c,10,-0.073).
atm(train,d264,d264_3,h,3,0.077).
atm(train,d264,d264_4,h,3,0.077).
atm(train,d264,d264_5,h,3,0.077).
atm(train,d264,d264_6,o,49,-0.644).
atm(train,d264,d264_7,h,3,0.077).
atm(train,d264,d264_8,h,3,0.077).
atm(train,d264,d264_9,p,61,1.333).
atm(train,d264,d264_10,o,49,-0.644).
atm(train,d264,d264_11,c,10,-0.073).
atm(train,d264,d264_12,c,10,-0.073).
atm(train,d264,d264_13,h,3,0.077).
atm(train,d264,d264_14,h,3,0.077).
atm(train,d264,d264_15,h,3,0.077).
atm(train,d264,d264_16,h,3,0.077).
atm(train,d264,d264_17,h,3,0.077).
atm(train,d264,d264_18,s,75,-0.123).
atm(train,d264,d264_19,o,49,-0.644).
atm(train,d264,d264_20,c,22,0.276).
atm(train,d264,d264_21,c,22,-0.103).
atm(train,d264,d264_22,c,22,-0.103).
atm(train,d264,d264_23,c,22,0.127).
atm(train,d264,d264_24,c,22,0.276).
atm(train,d264,d264_25,c,22,-0.103).
atm(train,d264,d264_26,h,3,0.157).
atm(train,d264,d264_27,h,3,0.156).
atm(train,d264,d264_28,h,3,0.157).
atm(train,d264,d264_29,c,16,-0.174).
atm(train,d264,d264_30,c,16,-0.174).
atm(train,d264,d264_31,c,14,0.726).
atm(train,d264,d264_32,o,49,-0.624).
atm(train,d264,d264_33,o,51,-0.524).
atm(train,d264,d264_34,c,10,0.117).
atm(train,d264,d264_35,h,3,0.077).
atm(train,d264,d264_36,h,3,0.077).
atm(train,d264,d264_37,h,3,0.077).
atm(train,d264,d264_38,cl,93,-0.174).
atm(train,d265,d265_1,c,10,0.147).
atm(train,d265,d265_2,o,49,-0.614).
atm(train,d265,d265_3,p,60,-0.243).
atm(train,d265,d265_4,o,49,-0.614).
atm(train,d265,d265_5,c,10,0.147).
atm(train,d265,d265_6,s,74,-0.062).
atm(train,d265,d265_7,c,10,-0.042).
atm(train,d265,d265_8,c,14,0.657).
atm(train,d265,d265_9,h,3,0.108).
atm(train,d265,d265_10,h,3,0.107).
atm(train,d265,d265_11,n,32,-0.343).
atm(train,d265,d265_12,c,10,0.058).
atm(train,d265,d265_13,h,3,0.108).
atm(train,d265,d265_14,h,3,0.108).
atm(train,d265,d265_15,h,3,0.108).
atm(train,d265,d265_16,s,75,-0.092).
atm(train,d265,d265_17,o,40,-0.493).
atm(train,d265,d265_18,h,1,0.307).
atm(train,d265,d265_19,h,3,0.108).
atm(train,d265,d265_20,h,3,0.108).
atm(train,d265,d265_21,h,3,0.108).
atm(train,d265,d265_22,h,3,0.108).
atm(train,d265,d265_23,h,3,0.108).
atm(train,d265,d265_24,h,3,0.108).
atm(train,d266,d266_1,c,10,0.097).
atm(train,d266,d266_2,o,49,-0.662).
atm(train,d266,d266_3,p,61,1.315).
atm(train,d266,d266_4,o,49,-0.662).
atm(train,d266,d266_5,c,10,0.097).
atm(train,d266,d266_6,s,74,-0.113).
atm(train,d266,d266_7,c,10,-0.093).
atm(train,d266,d266_8,c,14,0.708).
atm(train,d266,d266_9,o,40,-0.543).
atm(train,d266,d266_10,o,51,-0.543).
atm(train,d266,d266_11,h,3,0.057).
atm(train,d266,d266_12,h,3,0.057).
atm(train,d266,d266_13,h,3,0.057).
atm(train,d266,d266_14,h,3,0.057).
atm(train,d266,d266_15,h,3,0.057).
atm(train,d266,d266_16,h,3,0.057).
atm(train,d266,d266_17,o,49,-0.642).
atm(train,d266,d266_18,c,10,0.257).
atm(train,d266,d266_19,c,10,-0.093).
atm(train,d266,d266_20,h,3,0.107).
atm(train,d266,d266_21,h,3,0.107).
atm(train,d266,d266_22,h,3,0.057).
atm(train,d266,d266_23,h,3,0.057).
atm(train,d266,d266_24,h,3,0.057).
atm(train,d266,d266_25,c,14,0.708).
atm(train,d266,d266_26,o,49,-0.642).
atm(train,d266,d266_27,c,10,0.257).
atm(train,d266,d266_28,c,10,-0.093).
atm(train,d266,d266_29,h,3,0.107).
atm(train,d266,d266_30,h,3,0.107).
atm(train,d266,d266_31,h,3,0.057).
atm(train,d266,d266_32,h,3,0.057).
atm(train,d266,d266_33,h,3,0.057).
atm(train,d266,d266_34,o,51,-0.543).
atm(train,d266,d266_35,c,10,-0.093).
atm(train,d266,d266_36,h,3,0.057).
atm(train,d266,d266_37,h,3,0.057).
atm(train,d266,d266_38,h,3,0.057).
atm(train,d267,d267_1,o,45,-0.625).
atm(train,d267,d267_2,c,10,0.116).
atm(train,d267,d267_3,p,60,-0.274).
atm(train,d267,d267_4,h,3,0.076).
atm(train,d267,d267_5,h,3,0.076).
atm(train,d267,d267_6,c,10,0.116).
atm(train,d267,d267_7,o,45,-0.625).
atm(train,d267,d267_8,h,3,0.076).
atm(train,d267,d267_9,h,3,0.076).
atm(train,d267,d267_10,c,10,0.116).
atm(train,d267,d267_11,c,10,0.116).
atm(train,d267,d267_12,h,8,0.425).
atm(train,d267,d267_13,h,8,0.426).
atm(train,d267,d267_14,o,45,-0.625).
atm(train,d267,d267_15,h,3,0.076).
atm(train,d267,d267_16,h,3,0.076).
atm(train,d267,d267_17,h,8,0.425).
atm(train,d267,d267_18,o,45,-0.625).
atm(train,d267,d267_19,h,3,0.076).
atm(train,d267,d267_20,h,3,0.076).
atm(train,d267,d267_21,h,8,0.426).
atm(train,d268,d268_1,c,22,0.227).
atm(train,d268,d268_2,n,35,-0.563).
atm(train,d268,d268_3,c,22,0.477).
atm(train,d268,d268_4,c,22,-0.153).
atm(train,d268,d268_5,c,22,-0.153).
atm(train,d268,d268_6,c,22,-0.154).
atm(train,d268,d268_7,h,3,0.106).
atm(train,d268,d268_8,h,3,0.106).
atm(train,d268,d268_9,h,3,0.106).
atm(train,d268,d268_10,h,3,0.106).
atm(train,d268,d268_11,c,10,0.066).
atm(train,d268,d268_12,cl,93,-0.223).
atm(train,d268,d268_13,h,3,0.026).
atm(train,d268,d268_14,h,3,0.026).
atm(train,d269,d269_1,c,10,-0.061).
atm(train,d269,d269_2,c,10,-0.061).
atm(train,d269,d269_3,h,3,0.089).
atm(train,d269,d269_4,h,3,0.089).
atm(train,d269,d269_5,h,3,0.089).
atm(train,d269,d269_6,o,49,-0.63).
atm(train,d269,d269_7,h,3,0.089).
atm(train,d269,d269_8,h,3,0.089).
atm(train,d269,d269_9,p,61,1.347).
atm(train,d269,d269_10,o,49,-0.63).
atm(train,d269,d269_11,c,10,-0.061).
atm(train,d269,d269_12,c,10,-0.061).
atm(train,d269,d269_13,h,3,0.089).
atm(train,d269,d269_14,h,3,0.089).
atm(train,d269,d269_15,h,3,0.089).
atm(train,d269,d269_16,h,3,0.089).
atm(train,d269,d269_17,h,3,0.089).
atm(train,d269,d269_18,s,75,-0.11).
atm(train,d269,d269_19,o,49,-0.63).
atm(train,d269,d269_20,c,22,0.29).
atm(train,d269,d269_21,c,22,-0.09).
atm(train,d269,d269_22,c,22,0.54).
atm(train,d269,d269_23,n,35,-0.73).
atm(train,d269,d269_24,c,22,-0.091).
atm(train,d269,d269_25,n,35,-0.73).
atm(train,d269,d269_26,h,3,0.17).
atm(train,d269,d269_27,c,10,0.039).
atm(train,d269,d269_28,h,3,0.089).
atm(train,d269,d269_29,h,3,0.089).
atm(train,d269,d269_30,h,3,0.089).
atm(train,d269,d269_31,c,10,-0.061).
atm(train,d269,d269_32,c,10,-0.11).
atm(train,d269,d269_33,h,3,0.089).
atm(train,d269,d269_34,h,3,0.089).
atm(train,d269,d269_35,h,3,0.089).
atm(train,d269,d269_36,c,10,-0.11).
atm(train,d269,d269_37,h,3,0.089).
atm(train,d269,d269_38,h,3,0.089).
atm(train,d269,d269_39,h,3,0.089).
atm(train,d269,d269_40,h,3,0.089).
atm(train,d27,d27_1,n,32,-0.399).
atm(train,d27,d27_2,c,10,0.001).
atm(train,d27,d27_3,c,14,0.602).
atm(train,d27,d27_4,n,32,-0.399).
atm(train,d27,d27_5,c,14,0.602).
atm(train,d27,d27_6,h,3,0.051).
atm(train,d27,d27_7,h,3,0.051).
atm(train,d27,d27_8,n,32,-0.398).
atm(train,d27,d27_9,o,40,-0.548).
atm(train,d27,d27_10,h,1,0.251).
atm(train,d27,d27_11,o,40,-0.548).
atm(train,d27,d27_12,n,38,0.802).
atm(train,d27,d27_13,o,40,-0.399).
atm(train,d27,d27_14,o,40,-0.399).
atm(train,d27,d27_15,c,21,0.001).
atm(train,d27,d27_16,c,21,-0.129).
atm(train,d27,d27_17,c,21,-0.129).
atm(train,d27,d27_18,c,21,0.101).
atm(train,d27,d27_19,o,52,-0.029).
atm(train,d27,d27_20,c,14,0.602).
atm(train,d27,d27_21,h,3,0.051).
atm(train,d27,d27_22,h,3,0.131).
atm(train,d27,d27_23,h,3,0.131).
atm(train,d270,d270_1,c,10,0.104).
atm(train,d270,d270_2,o,50,-0.287).
atm(train,d270,d270_3,c,10,-0.016).
atm(train,d270,d270_4,c,10,-0.016).
atm(train,d270,d270_5,o,50,-0.287).
atm(train,d270,d270_6,c,10,0.104).
atm(train,d270,d270_7,h,3,0.143).
atm(train,d270,d270_8,h,3,0.143).
atm(train,d270,d270_9,h,3,0.143).
atm(train,d270,d270_10,h,3,0.143).
atm(train,d270,d270_11,s,74,-0.036).
atm(train,d270,d270_12,h,3,0.133).
atm(train,d270,d270_13,p,60,-0.217).
atm(train,d270,d270_14,s,74,-0.036).
atm(train,d270,d270_15,h,3,0.133).
atm(train,d270,d270_16,p,60,-0.217).
atm(train,d270,d270_17,s,75,-0.066).
atm(train,d270,d270_18,s,75,-0.066).
atm(train,d270,d270_19,o,49,-0.587).
atm(train,d270,d270_20,o,49,-0.587).
atm(train,d270,d270_21,c,10,-0.016).
atm(train,d270,d270_22,c,10,-0.016).
atm(train,d270,d270_23,h,3,0.134).
atm(train,d270,d270_24,h,3,0.134).
atm(train,d270,d270_25,h,3,0.134).
atm(train,d270,d270_26,h,3,0.134).
atm(train,d270,d270_27,h,3,0.134).
atm(train,d270,d270_28,c,10,-0.016).
atm(train,d270,d270_29,c,10,-0.016).
atm(train,d270,d270_30,h,3,0.134).
atm(train,d270,d270_31,h,3,0.134).
atm(train,d270,d270_32,h,3,0.134).
atm(train,d270,d270_33,h,3,0.134).
atm(train,d270,d270_34,h,3,0.133).
atm(train,d270,d270_35,o,49,-0.587).
atm(train,d270,d270_36,c,10,-0.016).
atm(train,d270,d270_37,c,10,-0.016).
atm(train,d270,d270_38,h,3,0.133).
atm(train,d270,d270_39,h,3,0.133).
atm(train,d270,d270_40,h,3,0.134).
atm(train,d270,d270_41,h,3,0.134).
atm(train,d270,d270_42,h,3,0.133).
atm(train,d270,d270_43,o,49,-0.587).
atm(train,d270,d270_44,c,10,-0.016).
atm(train,d270,d270_45,c,10,-0.016).
atm(train,d270,d270_46,h,3,0.133).
atm(train,d270,d270_47,h,3,0.133).
atm(train,d270,d270_48,h,3,0.134).
atm(train,d270,d270_49,h,3,0.134).
atm(train,d270,d270_50,h,3,0.134).
atm(train,d271,d271_1,c,10,0.129).
atm(train,d271,d271_2,o,49,-0.631).
atm(train,d271,d271_3,h,3,0.089).
atm(train,d271,d271_4,h,3,0.089).
atm(train,d271,d271_5,h,3,0.089).
atm(train,d271,d271_6,p,60,-0.261).
atm(train,d271,d271_7,s,74,-0.081).
atm(train,d271,d271_8,s,75,-0.111).
atm(train,d271,d271_9,o,49,-0.631).
atm(train,d271,d271_10,c,10,0.129).
atm(train,d271,d271_11,h,3,0.089).
atm(train,d271,d271_12,h,3,0.089).
atm(train,d271,d271_13,h,3,0.089).
atm(train,d271,d271_14,c,14,0.738).
atm(train,d271,d271_15,c,14,0.738).
atm(train,d271,d271_16,o,51,-0.511).
atm(train,d271,d271_17,o,51,-0.511).
atm(train,d271,d271_18,o,49,-0.611).
atm(train,d271,d271_19,c,10,0.289).
atm(train,d271,d271_20,c,10,-0.061).
atm(train,d271,d271_21,h,3,0.139).
atm(train,d271,d271_22,h,3,0.139).
atm(train,d271,d271_23,h,3,0.089).
atm(train,d271,d271_24,h,3,0.089).
atm(train,d271,d271_25,h,3,0.089).
atm(train,d271,d271_26,o,49,-0.611).
atm(train,d271,d271_27,c,10,0.289).
atm(train,d271,d271_28,c,10,-0.061).
atm(train,d271,d271_29,h,3,0.139).
atm(train,d271,d271_30,h,3,0.139).
atm(train,d271,d271_31,h,3,0.089).
atm(train,d271,d271_32,h,3,0.089).
atm(train,d271,d271_33,h,3,0.089).
atm(train,d271,d271_34,c,10,-0.061).
atm(train,d271,d271_35,c,10,-0.061).
atm(train,d271,d271_36,h,3,0.089).
atm(train,d271,d271_37,h,3,0.089).
atm(train,d271,d271_38,h,3,0.089).
atm(train,d272,d272_1,c,10,0.132).
atm(train,d272,d272_2,cl,93,-0.158).
atm(train,d272,d272_3,cl,93,-0.158).
atm(train,d272,d272_4,h,3,0.092).
atm(train,d272,d272_5,h,3,0.092).
atm(train,d273,d273_1,c,10,0.172).
atm(train,d273,d273_2,br,94,-0.068).
atm(train,d273,d273_3,cl,93,-0.118).
atm(train,d273,d273_4,cl,93,-0.118).
atm(train,d273,d273_5,h,3,0.132).
atm(train,d274,d274_1,c,10,0.16).
atm(train,d274,d274_2,br,94,-0.09).
atm(train,d274,d274_3,br,94,-0.09).
atm(train,d274,d274_4,br,94,-0.09).
atm(train,d274,d274_5,h,3,0.11).
atm(train,d275,d275_1,c,10,0.17).
atm(train,d275,d275_2,cl,93,-0.13).
atm(train,d275,d275_3,br,94,-0.08).
atm(train,d275,d275_4,br,94,-0.08).
atm(train,d275,d275_5,h,3,0.12).
atm(train,d276,d276_1,c,10,0.092).
atm(train,d276,d276_2,i,95,-0.048).
atm(train,d276,d276_3,i,95,-0.048).
atm(train,d276,d276_4,i,95,-0.048).
atm(train,d276,d276_5,h,3,0.052).
atm(train,d277,d277_1,c,10,-0.001).
atm(train,d277,d277_2,n,32,-0.401).
atm(train,d277,d277_3,c,10,-0.001).
atm(train,d277,d277_4,c,22,0.199).
atm(train,d277,d277_5,c,22,-0.131).
atm(train,d277,d277_6,c,22,-0.131).
atm(train,d277,d277_7,c,22,-0.131).
atm(train,d277,d277_8,c,22,-0.131).
atm(train,d277,d277_9,c,22,-0.131).
atm(train,d277,d277_10,h,3,0.129).
atm(train,d277,d277_11,h,3,0.129).
atm(train,d277,d277_12,h,3,0.129).
atm(train,d277,d277_13,h,3,0.129).
atm(train,d277,d277_14,c,10,-0.001).
atm(train,d277,d277_15,c,22,-0.131).
atm(train,d277,d277_16,h,3,0.049).
atm(train,d277,d277_17,h,3,0.049).
atm(train,d277,d277_18,c,22,-0.131).
atm(train,d277,d277_19,c,22,-0.131).
atm(train,d277,d277_20,c,22,0.199).
atm(train,d277,d277_21,c,22,-0.131).
atm(train,d277,d277_22,c,22,-0.131).
atm(train,d277,d277_23,h,3,0.129).
atm(train,d277,d277_24,h,3,0.129).
atm(train,d277,d277_25,h,3,0.129).
atm(train,d277,d277_26,h,3,0.129).
atm(train,d277,d277_27,n,32,-0.4).
atm(train,d277,d277_28,c,10,-0.001).
atm(train,d277,d277_29,c,10,-0.001).
atm(train,d277,d277_30,h,3,0.049).
atm(train,d277,d277_31,h,3,0.049).
atm(train,d277,d277_32,h,3,0.049).
atm(train,d277,d277_33,h,3,0.049).
atm(train,d277,d277_34,h,3,0.049).
atm(train,d277,d277_35,h,3,0.049).
atm(train,d277,d277_36,h,3,0.049).
atm(train,d277,d277_37,h,3,0.049).
atm(train,d277,d277_38,h,3,0.049).
atm(train,d277,d277_39,h,3,0.049).
atm(train,d277,d277_40,h,3,0.049).
atm(train,d277,d277_41,h,3,0.049).
atm(train,d278,d278_1,c,10,-0.005).
atm(train,d278,d278_2,n,32,-0.405).
atm(train,d278,d278_3,c,10,-0.005).
atm(train,d278,d278_4,c,22,0.195).
atm(train,d278,d278_5,c,22,-0.135).
atm(train,d278,d278_6,c,22,-0.135).
atm(train,d278,d278_7,c,22,-0.005).
atm(train,d278,d278_8,c,22,-0.135).
atm(train,d278,d278_9,c,22,-0.135).
atm(train,d278,d278_10,h,3,0.125).
atm(train,d278,d278_11,h,3,0.125).
atm(train,d278,d278_12,h,3,0.125).
atm(train,d278,d278_13,h,3,0.125).
atm(train,d278,d278_14,c,14,0.545).
atm(train,d278,d278_15,c,22,-0.005).
atm(train,d278,d278_16,c,22,-0.135).
atm(train,d278,d278_17,c,22,-0.135).
atm(train,d278,d278_18,c,22,0.195).
atm(train,d278,d278_19,c,22,-0.135).
atm(train,d278,d278_20,c,22,-0.135).
atm(train,d278,d278_21,h,3,0.125).
atm(train,d278,d278_22,h,3,0.125).
atm(train,d278,d278_23,h,3,0.125).
atm(train,d278,d278_24,h,3,0.125).
atm(train,d278,d278_25,n,32,-0.405).
atm(train,d278,d278_26,c,10,-0.005).
atm(train,d278,d278_27,c,10,-0.005).
atm(train,d278,d278_28,o,40,-0.555).
atm(train,d278,d278_29,h,3,0.045).
atm(train,d278,d278_30,h,3,0.045).
atm(train,d278,d278_31,h,3,0.045).
atm(train,d278,d278_32,h,3,0.045).
atm(train,d278,d278_33,h,3,0.045).
atm(train,d278,d278_34,h,3,0.045).
atm(train,d278,d278_35,h,3,0.045).
atm(train,d278,d278_36,h,3,0.045).
atm(train,d278,d278_37,h,3,0.045).
atm(train,d278,d278_38,h,3,0.045).
atm(train,d278,d278_39,h,3,0.045).
atm(train,d278,d278_40,h,3,0.045).
atm(train,d279,d279_1,c,10,-0.005).
atm(train,d279,d279_2,n,32,-0.405).
atm(train,d279,d279_3,c,10,-0.005).
atm(train,d279,d279_4,c,22,0.195).
atm(train,d279,d279_5,c,22,-0.135).
atm(train,d279,d279_6,c,22,-0.135).
atm(train,d279,d279_7,c,22,-0.135).
atm(train,d279,d279_8,c,22,-0.135).
atm(train,d279,d279_9,c,22,-0.135).
atm(train,d279,d279_10,h,3,0.125).
atm(train,d279,d279_11,h,3,0.125).
atm(train,d279,d279_12,h,3,0.125).
atm(train,d279,d279_13,h,3,0.125).
atm(train,d279,d279_14,h,3,0.125).
atm(train,d279,d279_15,h,3,0.045).
atm(train,d279,d279_16,h,3,0.045).
atm(train,d279,d279_17,h,3,0.045).
atm(train,d279,d279_18,h,3,0.045).
atm(train,d279,d279_19,h,3,0.045).
atm(train,d279,d279_20,h,3,0.045).
atm(train,d28,d28_1,c,21,-0.014).
atm(train,d28,d28_2,c,21,-0.144).
atm(train,d28,d28_3,c,21,-0.144).
atm(train,d28,d28_4,c,21,0.086).
atm(train,d28,d28_5,o,52,-0.044).
atm(train,d28,d28_6,n,38,0.787).
atm(train,d28,d28_7,c,14,0.588).
atm(train,d28,d28_8,n,32,-0.414).
atm(train,d28,d28_9,n,32,-0.414).
atm(train,d28,d28_10,c,14,0.588).
atm(train,d28,d28_11,n,32,-0.612).
atm(train,d28,d28_12,o,40,-0.414).
atm(train,d28,d28_13,o,40,-0.414).
atm(train,d28,d28_14,h,3,0.036).
atm(train,d28,d28_15,h,1,0.287).
atm(train,d28,d28_16,h,1,0.287).
atm(train,d28,d28_17,h,1,0.287).
atm(train,d28,d28_18,o,40,-0.564).
atm(train,d28,d28_19,h,3,0.116).
atm(train,d28,d28_20,h,3,0.116).
atm(train,d280,d280_1,c,10,0.003).
atm(train,d280,d280_2,n,32,-0.397).
atm(train,d280,d280_3,c,10,0.003).
atm(train,d280,d280_4,c,22,0.203).
atm(train,d280,d280_5,c,22,-0.127).
atm(train,d280,d280_6,c,22,-0.127).
atm(train,d280,d280_7,c,22,0.203).
atm(train,d280,d280_8,c,22,-0.127).
atm(train,d280,d280_9,c,22,-0.127).
atm(train,d280,d280_10,h,3,0.133).
atm(train,d280,d280_11,h,3,0.133).
atm(train,d280,d280_12,h,3,0.133).
atm(train,d280,d280_13,h,3,0.133).
atm(train,d280,d280_14,n,32,-0.397).
atm(train,d280,d280_15,n,32,-0.397).
atm(train,d280,d280_16,s,78,0.754).
atm(train,d280,d280_17,o,50,-0.227).
atm(train,d280,d280_18,na,81,1.004).
atm(train,d280,d280_19,o,40,-0.547).
atm(train,d280,d280_20,o,40,-0.547).
atm(train,d280,d280_21,h,3,0.053).
atm(train,d280,d280_22,h,3,0.053).
atm(train,d280,d280_23,h,3,0.053).
atm(train,d280,d280_24,h,3,0.053).
atm(train,d280,d280_25,h,3,0.053).
atm(train,d280,d280_26,h,3,0.053).
atm(train,d281,d281_1,n,38,0.793).
atm(train,d281,d281_2,c,10,0.083).
atm(train,d281,d281_3,n,38,0.793).
atm(train,d281,d281_4,n,38,0.793).
atm(train,d281,d281_5,n,38,0.794).
atm(train,d281,d281_6,o,40,-0.407).
atm(train,d281,d281_7,o,40,-0.407).
atm(train,d281,d281_8,o,40,-0.407).
atm(train,d281,d281_9,o,40,-0.407).
atm(train,d281,d281_10,o,40,-0.407).
atm(train,d281,d281_11,o,40,-0.407).
atm(train,d281,d281_12,o,40,-0.407).
atm(train,d281,d281_13,o,40,-0.407).
atm(train,d282,d282_1,n,38,0.811).
atm(train,d282,d282_2,c,10,0.012).
atm(train,d282,d282_3,c,10,-0.088).
atm(train,d282,d282_4,h,3,0.062).
atm(train,d282,d282_5,h,3,0.062).
atm(train,d282,d282_6,c,14,0.791).
atm(train,d282,d282_7,o,45,-0.659).
atm(train,d282,d282_8,o,51,-0.599).
atm(train,d282,d282_9,o,40,-0.389).
atm(train,d282,d282_10,o,40,-0.389).
atm(train,d282,d282_11,h,1,0.262).
atm(train,d282,d282_12,h,3,0.062).
atm(train,d282,d282_13,h,3,0.062).
atm(train,d283,d283_1,c,16,-0.193).
atm(train,d283,d283_2,c,16,-0.194).
atm(train,d283,d283_3,h,3,0.107).
atm(train,d283,d283_4,h,3,0.107).
atm(train,d283,d283_5,c,14,0.706).
atm(train,d283,d283_6,h,3,0.107).
atm(train,d283,d283_7,o,51,-0.544).
atm(train,d283,d283_8,o,49,-0.644).
atm(train,d283,d283_9,c,10,0.256).
atm(train,d283,d283_10,c,10,-0.093).
atm(train,d283,d283_11,h,3,0.107).
atm(train,d283,d283_12,h,3,0.107).
atm(train,d283,d283_13,h,3,0.057).
atm(train,d283,d283_14,h,3,0.057).
atm(train,d283,d283_15,h,3,0.057).
atm(train,d284,d284_1,c,16,-0.181).
atm(train,d284,d284_2,c,16,-0.181).
atm(train,d284,d284_3,h,3,0.119).
atm(train,d284,d284_4,h,3,0.119).
atm(train,d284,d284_5,c,14,0.618).
atm(train,d284,d284_6,h,3,0.119).
atm(train,d284,d284_7,o,40,-0.532).
atm(train,d284,d284_8,n,32,-0.382).
atm(train,d284,d284_9,c,10,0.109).
atm(train,d284,d284_10,o,45,-0.632).
atm(train,d284,d284_11,h,3,0.069).
atm(train,d284,d284_12,h,3,0.069).
atm(train,d284,d284_13,h,1,0.268).
atm(train,d284,d284_14,h,8,0.418).
atm(train,d285,d285_1,c,16,-0.206).
atm(train,d285,d285_2,c,16,-0.205).
atm(train,d285,d285_3,h,3,0.095).
atm(train,d285,d285_4,h,3,0.095).
atm(train,d285,d285_5,c,10,0.085).
atm(train,d285,d285_6,h,3,0.045).
atm(train,d285,d285_7,h,3,0.045).
atm(train,d285,d285_8,h,3,0.045).
atm(train,d285,d285_9,c,14,0.694).
atm(train,d285,d285_10,o,49,-0.656).
atm(train,d285,d285_11,c,10,0.234).
atm(train,d285,d285_12,h,3,0.095).
atm(train,d285,d285_13,h,3,0.095).
atm(train,d285,d285_14,h,3,0.095).
atm(train,d285,d285_15,o,51,-0.556).
atm(train,d286,d286_1,c,14,0.103).
atm(train,d286,d286_2,c,10,-0.037).
atm(train,d286,d286_3,c,14,0.103).
atm(train,d286,d286_4,o,41,-0.556).
atm(train,d286,d286_5,h,3,0.073).
atm(train,d286,d286_6,o,41,-0.556).
atm(train,d286,d286_7,h,3,0.073).
atm(train,d286,d286_8,na,81,0.874).
atm(train,d286,d286_9,h,3,-0.077).
atm(train,d287,d287_1,c,10,-0.092).
atm(train,d287,d287_2,c,10,0.258).
atm(train,d287,d287_3,o,50,-0.362).
atm(train,d287,d287_4,c,10,0.318).
atm(train,d287,d287_5,o,50,-0.362).
atm(train,d287,d287_6,c,10,0.148).
atm(train,d287,d287_7,h,3,0.058).
atm(train,d287,d287_8,h,3,0.058).
atm(train,d287,d287_9,c,10,-0.142).
atm(train,d287,d287_10,h,3,0.043).
atm(train,d287,d287_11,h,3,0.058).
atm(train,d287,d287_12,h,3,0.058).
atm(train,d287,d287_13,h,3,0.058).
atm(train,d287,d287_14,c,10,-0.092).
atm(train,d287,d287_15,h,3,0.058).
atm(train,d287,d287_16,h,3,0.058).
atm(train,d287,d287_17,h,3,0.058).
atm(train,d287,d287_18,h,3,0.058).
atm(train,d287,d287_19,o,49,-0.643).
atm(train,d287,d287_20,h,3,0.058).
atm(train,d287,d287_21,c,14,0.707).
atm(train,d287,d287_22,c,10,0.008).
atm(train,d287,d287_23,h,3,0.058).
atm(train,d287,d287_24,h,3,0.058).
atm(train,d287,d287_25,h,3,0.058).
atm(train,d287,d287_26,o,51,-0.543).
atm(train,d288,d288_1,c,10,-0.104).
atm(train,d288,d288_2,c,10,-0.003).
atm(train,d288,d288_3,n,36,-0.304).
atm(train,d288,d288_4,h,3,0.047).
atm(train,d288,d288_5,h,3,0.047).
atm(train,d288,d288_6,c,10,-0.003).
atm(train,d288,d288_7,c,10,-0.104).
atm(train,d288,d288_8,h,3,0.047).
atm(train,d288,d288_9,h,3,0.047).
atm(train,d288,d288_10,c,14,0.546).
atm(train,d288,d288_11,s,74,-0.104).
atm(train,d288,d288_12,c,10,0.086).
atm(train,d288,d288_13,s,75,-0.154).
atm(train,d288,d288_14,c,16,-0.204).
atm(train,d288,d288_15,h,3,0.047).
atm(train,d288,d288_16,h,3,0.047).
atm(train,d288,d288_17,cl,93,-0.204).
atm(train,d288,d288_18,c,16,-0.204).
atm(train,d288,d288_19,h,3,0.096).
atm(train,d288,d288_20,h,3,0.096).
atm(train,d288,d288_21,h,3,0.047).
atm(train,d288,d288_22,h,3,0.047).
atm(train,d288,d288_23,h,3,0.047).
atm(train,d288,d288_24,h,3,0.047).
atm(train,d288,d288_25,h,3,0.047).
atm(train,d288,d288_26,h,3,0.047).
atm(train,d289,d289_1,c,10,0.092).
atm(train,d289,d289_2,c,16,-0.199).
atm(train,d289,d289_3,h,3,0.052).
atm(train,d289,d289_4,h,3,0.052).
atm(train,d289,d289_5,h,3,0.052).
atm(train,d289,d289_6,c,10,0.092).
atm(train,d289,d289_7,h,3,0.052).
atm(train,d289,d289_8,h,3,0.052).
atm(train,d289,d289_9,h,3,0.052).
atm(train,d289,d289_10,c,16,-0.199).
atm(train,d289,d289_11,cl,93,-0.199).
atm(train,d289,d289_12,h,3,0.101).
atm(train,d29,d29a_1,c,22,-0.028).
atm(train,d29,d29a_2,c,22,0.301).
atm(train,d29,d29a_3,c,22,-0.028).
atm(train,d29,d29a_4,c,22,0.301).
atm(train,d29,d29a_5,c,22,-0.078).
atm(train,d29,d29a_6,c,22,-0.028).
atm(train,d29,d29a_7,h,3,0.231).
atm(train,d29,d29a_8,h,3,0.232).
atm(train,d29,d29a_9,h,3,0.231).
atm(train,d29,d29a_10,c,10,0.102).
atm(train,d29,d29a_11,h,3,0.152).
atm(train,d29,d29a_12,h,3,0.152).
atm(train,d29,d29a_13,h,3,0.152).
atm(train,d29,d29a_14,n,32,-0.299).
atm(train,d29,d29a_15,n,32,-0.299).
atm(train,d29,d29a_16,c,16,-0.098).
atm(train,d29,d29a_17,o,40,-0.449).
atm(train,d29,d29a_18,c,16,-0.098).
atm(train,d29,d29a_19,o,40,-0.449).
atm(train,d29,d29b_1,c,22,-0.028).
atm(train,d29,d29b_2,c,22,-0.028).
atm(train,d29,d29b_3,c,22,-0.028).
atm(train,d29,d29b_4,c,22,0.301).
atm(train,d29,d29b_5,c,22,-0.078).
atm(train,d29,d29b_6,c,22,0.301).
atm(train,d29,d29b_7,h,3,0.231).
atm(train,d29,d29b_8,h,3,0.232).
atm(train,d29,d29b_9,h,3,0.231).
atm(train,d29,d29b_10,c,10,0.102).
atm(train,d29,d29b_11,h,3,0.152).
atm(train,d29,d29b_12,h,3,0.152).
atm(train,d29,d29b_13,h,3,0.152).
atm(train,d29,d29b_14,n,32,-0.299).
atm(train,d29,d29b_15,n,32,-0.299).
atm(train,d29,d29b_16,c,16,-0.098).
atm(train,d29,d29b_17,o,40,-0.449).
atm(train,d29,d29b_18,c,16,-0.098).
atm(train,d29,d29b_19,o,40,-0.449).
atm(train,d290,d290_1,c,22,-0.099).
atm(train,d290,d290_2,c,22,-0.099).
atm(train,d290,d290_3,c,22,0.031).
atm(train,d290,d290_4,c,22,0.031).
atm(train,d290,d290_5,c,22,-0.099).
atm(train,d290,d290_6,c,22,-0.099).
atm(train,d290,d290_7,h,3,0.161).
atm(train,d290,d290_8,h,3,0.161).
atm(train,d290,d290_9,h,3,0.161).
atm(train,d290,d290_10,h,3,0.161).
atm(train,d290,d290_11,c,14,0.581).
atm(train,d290,d290_12,n,32,-0.369).
atm(train,d290,d290_13,c,14,0.581).
atm(train,d290,d290_14,o,40,-0.519).
atm(train,d290,d290_15,o,40,-0.519).
atm(train,d290,d290_16,s,74,-0.089).
atm(train,d290,d290_17,c,10,0.531).
atm(train,d290,d290_18,cl,93,-0.169).
atm(train,d290,d290_19,cl,93,-0.169).
atm(train,d290,d290_20,cl,93,-0.169).
atm(train,d291,d291_1,n,32,-0.414).
atm(train,d291,d291_2,c,14,0.586).
atm(train,d291,d291_3,o,49,-0.664).
atm(train,d291,d291_4,c,10,0.226).
atm(train,d291,d291_5,h,3,0.086).
atm(train,d291,d291_6,h,3,0.086).
atm(train,d291,d291_7,h,3,0.086).
atm(train,d291,d291_8,o,51,-0.564).
atm(train,d291,d291_9,h,1,0.286).
atm(train,d291,d291_10,h,1,0.286).
atm(train,d292,d292_1,o,45,-0.654).
atm(train,d292,d292_2,c,14,0.796).
atm(train,d292,d292_3,h,1,0.265).
atm(train,d292,d292_4,o,51,-0.594).
atm(train,d292,d292_5,c,10,-0.085).
atm(train,d292,d292_6,c,10,-0.085).
atm(train,d292,d292_7,h,3,0.065).
atm(train,d292,d292_8,h,3,0.065).
atm(train,d292,d292_9,c,14,0.616).
atm(train,d292,d292_10,h,3,0.065).
atm(train,d292,d292_11,h,3,0.065).
atm(train,d292,d292_12,o,40,-0.534).
atm(train,d292,d292_13,n,32,-0.385).
atm(train,d292,d292_14,n,36,-0.285).
atm(train,d292,d292_15,h,1,0.265).
atm(train,d292,d292_16,c,10,0.015).
atm(train,d292,d292_17,h,3,0.065).
atm(train,d292,d292_18,h,3,0.065).
atm(train,d292,d292_19,h,3,0.065).
atm(train,d292,d292_20,c,10,0.015).
atm(train,d292,d292_21,h,3,0.065).
atm(train,d292,d292_22,h,3,0.065).
atm(train,d292,d292_23,h,3,0.065).
atm(train,d293,d293_1,c,22,-0.051).
atm(train,d293,d293_2,c,22,-0.051).
atm(train,d293,d293_3,c,26,-0.021).
atm(train,d293,d293_4,c,26,-0.021).
atm(train,d293,d293_5,c,22,-0.051).
atm(train,d293,d293_6,c,22,-0.051).
atm(train,d293,d293_7,h,3,0.208).
atm(train,d293,d293_8,h,3,0.209).
atm(train,d293,d293_9,h,3,0.209).
atm(train,d293,d293_10,h,3,0.208).
atm(train,d293,d293_11,n,34,-0.322).
atm(train,d293,d293_12,n,34,-0.322).
atm(train,d293,d293_13,n,34,-0.322).
atm(train,d293,d293_14,h,1,0.378).
atm(train,d294,d294_1,c,22,-0.103).
atm(train,d294,d294_2,c,22,0.278).
atm(train,d294,d294_3,c,27,-0.073).
atm(train,d294,d294_4,c,27,0.027).
atm(train,d294,d294_5,c,22,-0.103).
atm(train,d294,d294_6,c,22,-0.103).
atm(train,d294,d294_7,h,3,0.157).
atm(train,d294,d294_8,h,3,0.157).
atm(train,d294,d294_9,h,3,0.157).
atm(train,d294,d294_10,n,35,-0.742).
atm(train,d294,d294_11,c,22,0.277).
atm(train,d294,d294_12,c,22,-0.103).
atm(train,d294,d294_13,c,22,-0.103).
atm(train,d294,d294_14,h,3,0.157).
atm(train,d294,d294_15,h,3,0.157).
atm(train,d294,d294_16,h,3,0.157).
atm(train,d294,d294_17,o,45,-0.622).
atm(train,d294,d294_18,h,8,0.428).
atm(train,d295,d295_1,c,16,-0.215).
atm(train,d295,d295_2,c,16,-0.215).
atm(train,d295,d295_3,h,3,0.085).
atm(train,d295,d295_4,h,3,0.084).
atm(train,d295,d295_5,c,10,0.074).
atm(train,d295,d295_6,h,3,0.085).
atm(train,d295,d295_7,h,3,0.034).
atm(train,d295,d295_8,h,3,0.034).
atm(train,d295,d295_9,h,3,0.034).
atm(train,d3,d3_1,c,22,-0.132).
atm(train,d3,d3_2,c,22,-0.132).
atm(train,d3,d3_3,c,22,-0.002).
atm(train,d3,d3_4,c,22,-0.002).
atm(train,d3,d3_5,c,22,-0.132).
atm(train,d3,d3_6,c,22,-0.132).
atm(train,d3,d3_7,h,3,0.128).
atm(train,d3,d3_8,h,3,0.128).
atm(train,d3,d3_9,h,3,0.128).
atm(train,d3,d3_10,h,3,0.128).
atm(train,d3,d3_11,c,14,0.549).
atm(train,d3,d3_12,c,22,-0.002).
atm(train,d3,d3_13,c,22,-0.002).
atm(train,d3,d3_14,c,14,0.549).
atm(train,d3,d3_15,c,22,-0.132).
atm(train,d3,d3_16,c,22,-0.132).
atm(train,d3,d3_17,c,22,-0.182).
atm(train,d3,d3_18,c,22,0.199).
atm(train,d3,d3_19,h,3,0.128).
atm(train,d3,d3_20,h,3,0.128).
atm(train,d3,d3_21,o,40,-0.551).
atm(train,d3,d3_22,o,40,-0.551).
atm(train,d3,d3_23,c,10,-0.002).
atm(train,d3,d3_24,h,3,0.048).
atm(train,d3,d3_25,h,3,0.048).
atm(train,d3,d3_26,h,3,0.048).
atm(train,d3,d3_27,n,32,-0.781).
atm(train,d3,d3_28,h,1,0.329).
atm(train,d3,d3_29,h,1,0.329).
atm(train,d30,d30_1,c,22,-0.112).
atm(train,d30,d30_2,c,22,-0.112).
atm(train,d30,d30_3,c,22,-0.112).
atm(train,d30,d30_4,c,22,-0.112).
atm(train,d30,d30_5,c,22,0.218).
atm(train,d30,d30_6,c,22,-0.112).
atm(train,d30,d30_7,h,3,0.148).
atm(train,d30,d30_8,h,3,0.148).
atm(train,d30,d30_9,h,3,0.148).
atm(train,d30,d30_10,h,3,0.148).
atm(train,d30,d30_11,n,32,-0.763).
atm(train,d30,d30_12,cl,93,-0.182).
atm(train,d30,d30_13,h,1,0.347).
atm(train,d30,d30_14,h,1,0.348).
atm(train,d31,d31a_1,c,22,0.184).
atm(train,d31,d31a_2,c,22,0.164).
atm(train,d31,d31a_3,c,22,-0.146).
atm(train,d31,d31a_4,c,29,-0.016).
atm(train,d31,d31a_5,c,22,-0.146).
atm(train,d31,d31a_6,c,22,-0.146).
atm(train,d31,d31a_7,h,3,0.114).
atm(train,d31,d31a_8,h,3,0.114).
atm(train,d31,d31a_9,h,3,0.114).
atm(train,d31,d31a_10,c,29,-0.016).
atm(train,d31,d31a_11,c,22,-0.146).
atm(train,d31,d31a_12,c,22,0.164).
atm(train,d31,d31a_13,c,22,0.184).
atm(train,d31,d31a_14,c,22,-0.146).
atm(train,d31,d31a_15,c,22,-0.146).
atm(train,d31,d31a_16,h,3,0.114).
atm(train,d31,d31a_17,h,3,0.114).
atm(train,d31,d31a_18,h,3,0.114).
atm(train,d31,d31a_19,n,32,-0.795).
atm(train,d31,d31a_20,o,50,-0.246).
atm(train,d31,d31a_21,c,10,0.074).
atm(train,d31,d31a_22,h,3,0.034).
atm(train,d31,d31a_23,h,3,0.034).
atm(train,d31,d31a_24,h,3,0.034).
atm(train,d31,d31a_25,o,50,-0.246).
atm(train,d31,d31a_26,c,10,0.074).
atm(train,d31,d31a_27,h,3,0.034).
atm(train,d31,d31a_28,h,3,0.034).
atm(train,d31,d31a_29,h,3,0.034).
atm(train,d31,d31a_30,n,32,-0.795).
atm(train,d31,d31a_31,h,1,0.314).
atm(train,d31,d31a_32,h,1,0.314).
atm(train,d31,d31a_33,h,1,0.315).
atm(train,d31,d31a_34,h,1,0.315).
atm(train,d31,d31b_1,cl,93,-0.225).
atm(train,d31,d31b_2,h,1,0.225).
atm(train,d32,d32a_1,c,22,0.197).
atm(train,d32,d32a_2,c,22,-0.183).
atm(train,d32,d32a_3,c,22,-0.133).
atm(train,d32,d32a_4,c,29,-0.003).
atm(train,d32,d32a_5,c,22,-0.133).
atm(train,d32,d32a_6,c,22,-0.133).
atm(train,d32,d32a_7,h,3,0.127).
atm(train,d32,d32a_8,h,3,0.127).
atm(train,d32,d32a_9,h,3,0.127).
atm(train,d32,d32a_10,c,29,-0.003).
atm(train,d32,d32a_11,c,22,-0.133).
atm(train,d32,d32a_12,c,22,-0.183).
atm(train,d32,d32a_13,c,22,0.197).
atm(train,d32,d32a_14,c,22,-0.133).
atm(train,d32,d32a_15,c,22,-0.133).
atm(train,d32,d32a_16,h,3,0.127).
atm(train,d32,d32a_17,h,3,0.127).
atm(train,d32,d32a_18,h,3,0.127).
atm(train,d32,d32a_19,n,32,-0.784).
atm(train,d32,d32a_20,n,32,-0.784).
atm(train,d32,d32a_21,c,10,-0.003).
atm(train,d32,d32a_22,h,3,0.047).
atm(train,d32,d32a_23,h,3,0.047).
atm(train,d32,d32a_24,h,3,0.047).
atm(train,d32,d32a_25,c,10,-0.003).
atm(train,d32,d32a_26,h,3,0.047).
atm(train,d32,d32a_27,h,3,0.047).
atm(train,d32,d32a_28,h,3,0.047).
atm(train,d32,d32a_29,h,1,0.326).
atm(train,d32,d32a_30,h,1,0.327).
atm(train,d32,d32a_31,h,1,0.327).
atm(train,d32,d32a_32,h,1,0.326).
atm(train,d32,d32b_1,cl,93,-0.225).
atm(train,d32,d32b_2,h,1,0.225).
atm(train,d33,d33_1,c,22,-0.136).
atm(train,d33,d33_2,c,22,-0.136).
atm(train,d33,d33_3,c,22,-0.136).
atm(train,d33,d33_4,c,22,-0.136).
atm(train,d33,d33_5,c,22,0.195).
atm(train,d33,d33_6,c,22,-0.136).
atm(train,d33,d33_7,h,3,0.124).
atm(train,d33,d33_8,h,3,0.124).
atm(train,d33,d33_9,h,3,0.124).
atm(train,d33,d33_10,h,3,0.124).
atm(train,d33,d33_11,h,3,0.124).
atm(train,d33,d33_12,n,32,-0.785).
atm(train,d33,d33_13,h,1,0.325).
atm(train,d33,d33_14,h,1,0.325).
atm(train,d34,d34_1,c,22,-0.113).
atm(train,d34,d34_2,c,22,-0.113).
atm(train,d34,d34_3,c,22,-0.113).
atm(train,d34,d34_4,c,22,0.216).
atm(train,d34,d34_5,c,22,-0.113).
atm(train,d34,d34_6,c,22,-0.113).
atm(train,d34,d34_7,h,3,0.146).
atm(train,d34,d34_8,h,3,0.146).
atm(train,d34,d34_9,h,3,0.146).
atm(train,d34,d34_10,h,3,0.147).
atm(train,d34,d34_11,h,3,0.146).
atm(train,d34,d34_12,c,22,0.216).
atm(train,d34,d34_13,c,22,-0.113).
atm(train,d34,d34_14,c,22,-0.113).
atm(train,d34,d34_15,c,22,-0.113).
atm(train,d34,d34_16,c,22,-0.113).
atm(train,d34,d34_17,c,22,-0.113).
atm(train,d34,d34_18,h,3,0.147).
atm(train,d34,d34_19,h,3,0.147).
atm(train,d34,d34_20,h,3,0.147).
atm(train,d34,d34_21,h,3,0.147).
atm(train,d34,d34_22,h,3,0.147).
atm(train,d34,d34_23,n,32,-0.384).
atm(train,d34,d34_24,n,32,-0.384).
atm(train,d35,d35_1,c,22,-0.114).
atm(train,d35,d35_2,c,22,-0.114).
atm(train,d35,d35_3,c,22,-0.114).
atm(train,d35,d35_4,c,22,-0.163).
atm(train,d35,d35_5,c,22,0.217).
atm(train,d35,d35_6,c,22,-0.114).
atm(train,d35,d35_7,h,3,0.147).
atm(train,d35,d35_8,h,3,0.147).
atm(train,d35,d35_9,h,3,0.146).
atm(train,d35,d35_10,n,32,-0.763).
atm(train,d35,d35_11,c,10,0.016).
atm(train,d35,d35_12,h,3,0.066).
atm(train,d35,d35_13,h,3,0.066).
atm(train,d35,d35_14,h,3,0.066).
atm(train,d35,d35_15,cl,93,-0.183).
atm(train,d35,d35_16,h,1,0.347).
atm(train,d35,d35_17,h,1,0.347).
atm(train,d36,d36a_1,c,22,-0.084).
atm(train,d36,d36a_2,c,22,-0.084).
atm(train,d36,d36a_3,c,22,-0.004).
atm(train,d36,d36a_4,c,22,0.246).
atm(train,d36,d36a_5,c,22,-0.084).
atm(train,d36,d36a_6,c,22,-0.134).
atm(train,d36,d36a_7,h,3,0.176).
atm(train,d36,d36a_8,h,3,0.176).
atm(train,d36,d36a_9,c,22,-0.084).
atm(train,d36,d36a_10,c,27,0.046).
atm(train,d36,d36a_11,c,27,0.046).
atm(train,d36,d36a_12,c,22,-0.084).
atm(train,d36,d36a_13,c,22,-0.084).
atm(train,d36,d36a_14,c,22,0.296).
atm(train,d36,d36a_15,h,3,0.176).
atm(train,d36,d36a_16,h,3,0.176).
atm(train,d36,d36a_17,c,22,-0.084).
atm(train,d36,d36a_18,c,22,-0.084).
atm(train,d36,d36a_19,c,22,-0.084).
atm(train,d36,d36a_20,c,22,-0.084).
atm(train,d36,d36a_21,h,3,0.176).
atm(train,d36,d36a_22,h,3,0.176).
atm(train,d36,d36a_23,h,3,0.176).
atm(train,d36,d36a_24,h,3,0.176).
atm(train,d36,d36a_25,n,32,-0.355).
atm(train,d36,d36a_26,n,32,-0.354).
atm(train,d36,d36a_27,o,45,-0.605).
atm(train,d36,d36a_28,cl,93,-0.154).
atm(train,d36,d36a_29,c,10,0.046).
atm(train,d36,d36a_30,h,3,0.096).
atm(train,d36,d36a_31,h,3,0.096).
atm(train,d36,d36a_32,h,3,0.096).
atm(train,d36,d36a_33,h,8,0.445).
atm(train,d36,d36a_34,s,78,0.795).
atm(train,d36,d36a_35,o,45,-0.605).
atm(train,d36,d36a_36,o,40,-0.505).
atm(train,d36,d36a_37,o,40,-0.505).
atm(train,d36,d36a_38,h,1,0.445).
atm(train,d36,d36b_1,ba,115,0.0).
atm(train,d37,d37_1,c,22,0.244).
atm(train,d37,d37_2,c,22,0.224).
atm(train,d37,d37_3,c,22,-0.085).
atm(train,d37,d37_4,c,29,0.045).
atm(train,d37,d37_5,c,22,-0.085).
atm(train,d37,d37_6,c,22,-0.085).
atm(train,d37,d37_7,h,3,0.175).
atm(train,d37,d37_8,h,3,0.175).
atm(train,d37,d37_9,h,3,0.175).
atm(train,d37,d37_10,c,29,0.045).
atm(train,d37,d37_11,c,22,-0.085).
atm(train,d37,d37_12,c,22,0.224).
atm(train,d37,d37_13,c,22,0.244).
atm(train,d37,d37_14,c,22,-0.085).
atm(train,d37,d37_15,c,22,-0.085).
atm(train,d37,d37_16,h,3,0.175).
atm(train,d37,d37_17,h,3,0.175).
atm(train,d37,d37_18,h,3,0.175).
atm(train,d37,d37_19,o,50,-0.186).
atm(train,d37,d37_20,c,10,0.135).
atm(train,d37,d37_21,h,3,0.095).
atm(train,d37,d37_22,h,3,0.095).
atm(train,d37,d37_23,h,3,0.095).
atm(train,d37,d37_24,n,32,-0.356).
atm(train,d37,d37_25,o,50,-0.186).
atm(train,d37,d37_26,c,10,0.135).
atm(train,d37,d37_27,h,3,0.095).
atm(train,d37,d37_28,h,3,0.095).
atm(train,d37,d37_29,h,3,0.095).
atm(train,d37,d37_30,n,32,-0.356).
atm(train,d37,d37_31,c,16,-0.155).
atm(train,d37,d37_32,o,40,-0.506).
atm(train,d37,d37_33,c,16,-0.155).
atm(train,d37,d37_34,o,40,-0.506).
atm(train,d38,d38_1,c,22,-0.115).
atm(train,d38,d38_2,c,22,-0.115).
atm(train,d38,d38_3,c,22,-0.115).
atm(train,d38,d38_4,c,22,-0.115).
atm(train,d38,d38_5,c,22,-0.165).
atm(train,d38,d38_6,c,22,-0.115).
atm(train,d38,d38_7,h,3,0.145).
atm(train,d38,d38_8,h,3,0.145).
atm(train,d38,d38_9,h,3,0.145).
atm(train,d38,d38_10,c,10,0.015).
atm(train,d38,d38_11,h,3,0.065).
atm(train,d38,d38_12,h,3,0.065).
atm(train,d38,d38_13,h,3,0.065).
atm(train,d38,d38_14,n,38,0.816).
atm(train,d38,d38_15,n,38,0.816).
atm(train,d38,d38_16,o,40,-0.384).
atm(train,d38,d38_17,o,40,-0.384).
atm(train,d38,d38_18,o,40,-0.384).
atm(train,d38,d38_19,o,40,-0.385).
atm(train,d39,d39_1,c,22,-0.126).
atm(train,d39,d39_2,c,22,-0.126).
atm(train,d39,d39_3,c,22,-0.126).
atm(train,d39,d39_4,c,22,-0.176).
atm(train,d39,d39_5,c,22,0.204).
atm(train,d39,d39_6,c,22,-0.126).
atm(train,d39,d39_7,h,3,0.134).
atm(train,d39,d39_8,h,3,0.134).
atm(train,d39,d39_9,h,3,0.134).
atm(train,d39,d39_10,n,32,-0.775).
atm(train,d39,d39_11,c,10,0.004).
atm(train,d39,d39_12,h,3,0.054).
atm(train,d39,d39_13,h,3,0.054).
atm(train,d39,d39_14,h,3,0.054).
atm(train,d39,d39_15,n,38,0.805).
atm(train,d39,d39_16,o,40,-0.395).
atm(train,d39,d39_17,o,40,-0.395).
atm(train,d39,d39_18,h,1,0.334).
atm(train,d39,d39_19,h,1,0.334).
atm(train,d4,d4_1,c,22,-0.145).
atm(train,d4,d4_2,c,22,-0.145).
atm(train,d4,d4_3,c,22,-0.145).
atm(train,d4,d4_4,c,22,0.165).
atm(train,d4,d4_5,c,22,0.185).
atm(train,d4,d4_6,c,22,-0.145).
atm(train,d4,d4_7,h,3,0.115).
atm(train,d4,d4_8,h,3,0.115).
atm(train,d4,d4_9,h,3,0.115).
atm(train,d4,d4_10,h,3,0.115).
atm(train,d4,d4_11,n,32,-0.795).
atm(train,d4,d4_12,h,1,0.315).
atm(train,d4,d4_13,h,1,0.315).
atm(train,d4,d4_14,o,50,-0.245).
atm(train,d4,d4_15,c,10,0.075).
atm(train,d4,d4_16,h,3,0.035).
atm(train,d4,d4_17,h,3,0.035).
atm(train,d4,d4_18,h,3,0.035).
atm(train,d40,d40_1,c,22,0.211).
atm(train,d40,d40_2,c,22,-0.12).
atm(train,d40,d40_3,c,22,-0.12).
atm(train,d40,d40_4,c,22,-0.12).
atm(train,d40,d40_5,c,22,-0.12).
atm(train,d40,d40_6,c,22,-0.12).
atm(train,d40,d40_7,h,3,0.14).
atm(train,d40,d40_8,h,3,0.14).
atm(train,d40,d40_9,h,3,0.14).
atm(train,d40,d40_10,h,3,0.14).
atm(train,d40,d40_11,c,22,-0.12).
atm(train,d40,d40_12,c,22,-0.12).
atm(train,d40,d40_13,c,22,-0.12).
atm(train,d40,d40_14,c,22,0.211).
atm(train,d40,d40_15,c,22,-0.12).
atm(train,d40,d40_16,c,22,-0.12).
atm(train,d40,d40_17,h,3,0.14).
atm(train,d40,d40_18,h,3,0.14).
atm(train,d40,d40_19,h,3,0.14).
atm(train,d40,d40_20,h,3,0.14).
atm(train,d40,d40_21,s,77,0.211).
atm(train,d40,d40_22,n,32,-0.769).
atm(train,d40,d40_23,n,32,-0.769).
atm(train,d40,d40_24,o,40,-0.19).
atm(train,d40,d40_25,o,40,-0.189).
atm(train,d40,d40_26,h,1,0.341).
atm(train,d40,d40_27,h,1,0.341).
atm(train,d40,d40_28,h,1,0.341).
atm(train,d40,d40_29,h,1,0.341).
atm(train,d41,d41_1,c,22,-0.122).
atm(train,d41,d41_2,c,22,-0.123).
atm(train,d41,d41_3,c,22,-0.123).
atm(train,d41,d41_4,c,22,-0.123).
atm(train,d41,d41_5,c,22,0.208).
atm(train,d41,d41_6,c,22,-0.123).
atm(train,d41,d41_7,h,3,0.138).
atm(train,d41,d41_8,h,3,0.138).
atm(train,d41,d41_9,c,232,1.008).
atm(train,d41,d41_10,f,92,-0.292).
atm(train,d41,d41_11,f,92,-0.292).
atm(train,d41,d41_12,f,92,-0.292).
atm(train,d41,d41_13,n,32,-0.392).
atm(train,d41,d41_14,c,10,0.007).
atm(train,d41,d41_15,c,10,0.007).
atm(train,d41,d41_16,n,38,0.808).
atm(train,d41,d41_17,n,38,0.808).
atm(train,d41,d41_18,o,40,-0.392).
atm(train,d41,d41_19,o,40,-0.392).
atm(train,d41,d41_20,o,40,-0.392).
atm(train,d41,d41_21,o,40,-0.392).
atm(train,d41,d41_22,c,10,-0.093).
atm(train,d41,d41_23,c,10,-0.142).
atm(train,d41,d41_24,h,3,0.057).
atm(train,d41,d41_25,h,3,0.057).
atm(train,d41,d41_26,h,3,0.057).
atm(train,d41,d41_27,h,3,0.057).
atm(train,d41,d41_28,h,3,0.057).
atm(train,d41,d41_29,c,10,-0.093).
atm(train,d41,d41_30,c,10,-0.142).
atm(train,d41,d41_31,h,3,0.057).
atm(train,d41,d41_32,h,3,0.057).
atm(train,d41,d41_33,h,3,0.057).
atm(train,d41,d41_34,h,3,0.057).
atm(train,d41,d41_35,h,3,0.057).
atm(train,d41,d41_36,h,3,0.057).
atm(train,d41,d41_37,h,3,0.057).
atm(train,d41,d41_38,h,3,0.057).
atm(train,d41,d41_39,h,3,0.057).
atm(train,d42,d42_1,c,22,-0.114).
atm(train,d42,d42_2,c,22,-0.114).
atm(train,d42,d42_3,c,22,-0.114).
atm(train,d42,d42_4,c,22,-0.163).
atm(train,d42,d42_5,c,22,0.217).
atm(train,d42,d42_6,c,22,-0.114).
atm(train,d42,d42_7,h,3,0.147).
atm(train,d42,d42_8,h,3,0.147).
atm(train,d42,d42_9,h,3,0.146).
atm(train,d42,d42_10,n,32,-0.763).
atm(train,d42,d42_11,c,10,0.016).
atm(train,d42,d42_12,h,3,0.066).
atm(train,d42,d42_13,h,3,0.066).
atm(train,d42,d42_14,h,3,0.066).
atm(train,d42,d42_15,cl,93,-0.183).
atm(train,d42,d42_16,h,1,0.347).
atm(train,d42,d42_17,h,1,0.347).
atm(train,d43,d43_1,c,22,-0.14).
atm(train,d43,d43_2,c,22,0.19).
atm(train,d43,d43_3,c,22,-0.01).
atm(train,d43,d43_4,c,22,-0.01).
atm(train,d43,d43_5,c,22,0.19).
atm(train,d43,d43_6,c,22,-0.14).
atm(train,d43,d43_7,h,3,0.12).
atm(train,d43,d43_8,h,3,0.12).
atm(train,d43,d43_9,c,14,0.54).
atm(train,d43,d43_10,c,22,-0.01).
atm(train,d43,d43_11,c,22,-0.01).
atm(train,d43,d43_12,c,14,0.54).
atm(train,d43,d43_13,c,22,0.19).
atm(train,d43,d43_14,c,22,-0.14).
atm(train,d43,d43_15,c,22,-0.14).
atm(train,d43,d43_16,c,22,0.19).
atm(train,d43,d43_17,h,3,0.12).
atm(train,d43,d43_18,h,3,0.12).
atm(train,d43,d43_19,o,40,-0.56).
atm(train,d43,d43_20,o,40,-0.56).
atm(train,d43,d43_21,n,32,-0.79).
atm(train,d43,d43_22,n,32,-0.79).
atm(train,d43,d43_23,h,1,0.32).
atm(train,d43,d43_24,h,1,0.32).
atm(train,d43,d43_25,h,1,0.32).
atm(train,d43,d43_26,h,1,0.32).
atm(train,d43,d43_27,n,32,-0.79).
atm(train,d43,d43_28,n,32,-0.79).
atm(train,d43,d43_29,h,1,0.32).
atm(train,d43,d43_30,h,1,0.32).
atm(train,d43,d43_31,h,1,0.32).
atm(train,d43,d43_32,h,1,0.32).
atm(train,d44,d44_1,c,22,-0.142).
atm(train,d44,d44_2,c,22,0.168).
atm(train,d44,d44_3,c,22,-0.142).
atm(train,d44,d44_4,c,22,-0.192).
atm(train,d44,d44_5,c,22,0.188).
atm(train,d44,d44_6,c,22,-0.141).
atm(train,d44,d44_7,h,3,0.119).
atm(train,d44,d44_8,h,3,0.119).
atm(train,d44,d44_9,h,3,0.119).
atm(train,d44,d44_10,n,32,-0.792).
atm(train,d44,d44_11,c,10,-0.011).
atm(train,d44,d44_12,h,3,0.039).
atm(train,d44,d44_13,h,3,0.039).
atm(train,d44,d44_14,h,3,0.039).
atm(train,d44,d44_15,o,50,-0.242).
atm(train,d44,d44_16,c,10,0.079).
atm(train,d44,d44_17,h,3,0.039).
atm(train,d44,d44_18,h,3,0.039).
atm(train,d44,d44_19,h,3,0.039).
atm(train,d44,d44_20,h,1,0.318).
atm(train,d44,d44_21,h,1,0.318).
atm(train,d45,d45_1,c,22,-0.099).
atm(train,d45,d45_2,c,22,0.231).
atm(train,d45,d45_3,c,22,-0.099).
atm(train,d45,d45_4,c,22,-0.099).
atm(train,d45,d45_5,c,22,0.231).
atm(train,d45,d45_6,c,22,-0.099).
atm(train,d45,d45_7,h,3,0.161).
atm(train,d45,d45_8,h,3,0.161).
atm(train,d45,d45_9,n,32,-0.748).
atm(train,d45,d45_10,n,32,-0.748).
atm(train,d45,d45_11,cl,93,-0.169).
atm(train,d45,d45_12,cl,93,-0.169).
atm(train,d45,d45_13,h,1,0.361).
atm(train,d45,d45_14,h,1,0.362).
atm(train,d45,d45_15,h,1,0.362).
atm(train,d45,d45_16,h,1,0.361).
atm(train,d46,d46_1,c,22,-0.102).
atm(train,d46,d46_2,c,22,-0.102).
atm(train,d46,d46_3,c,26,-0.072).
atm(train,d46,d46_4,c,26,-0.012).
atm(train,d46,d46_5,c,22,-0.102).
atm(train,d46,d46_6,c,22,-0.102).
atm(train,d46,d46_7,h,3,0.158).
atm(train,d46,d46_8,h,3,0.158).
atm(train,d46,d46_9,h,3,0.158).
atm(train,d46,d46_10,n,38,0.829).
atm(train,d46,d46_11,o,40,-0.372).
atm(train,d46,d46_12,o,40,-0.372).
atm(train,d46,d46_13,n,34,-0.511).
atm(train,d46,d46_14,n,34,-0.511).
atm(train,d46,d46_15,c,21,0.298).
atm(train,d46,d46_16,h,1,0.529).
atm(train,d46,d46_17,h,3,0.128).
atm(train,d47,d47_1,c,22,-0.106).
atm(train,d47,d47_2,c,22,-0.106).
atm(train,d47,d47_3,c,22,-0.106).
atm(train,d47,d47_4,c,22,0.223).
atm(train,d47,d47_5,c,22,-0.106).
atm(train,d47,d47_6,c,22,-0.106).
atm(train,d47,d47_7,h,3,0.154).
atm(train,d47,d47_8,h,3,0.153).
atm(train,d47,d47_9,h,3,0.153).
atm(train,d47,d47_10,h,3,0.153).
atm(train,d47,d47_11,h,3,0.153).
atm(train,d47,d47_12,c,22,-0.106).
atm(train,d47,d47_13,c,22,0.273).
atm(train,d47,d47_14,c,22,-0.106).
atm(train,d47,d47_15,c,22,-0.106).
atm(train,d47,d47_16,c,27,0.024).
atm(train,d47,d47_17,c,27,0.024).
atm(train,d47,d47_18,h,3,0.153).
atm(train,d47,d47_19,h,3,0.153).
atm(train,d47,d47_20,c,22,-0.106).
atm(train,d47,d47_21,c,22,-0.106).
atm(train,d47,d47_22,c,22,-0.106).
atm(train,d47,d47_23,c,22,-0.106).
atm(train,d47,d47_24,h,3,0.153).
atm(train,d47,d47_25,h,3,0.153).
atm(train,d47,d47_26,h,3,0.154).
atm(train,d47,d47_27,h,3,0.154).
atm(train,d47,d47_28,n,32,-0.377).
atm(train,d47,d47_29,n,32,-0.377).
atm(train,d47,d47_30,o,45,-0.627).
atm(train,d47,d47_31,h,8,0.423).
atm(train,d48,d48_1,c,22,-0.136).
atm(train,d48,d48_2,c,22,0.173).
atm(train,d48,d48_3,c,22,0.193).
atm(train,d48,d48_4,c,22,-0.136).
atm(train,d48,d48_5,c,22,0.193).
atm(train,d48,d48_6,c,22,-0.136).
atm(train,d48,d48_7,h,3,0.124).
atm(train,d48,d48_8,h,3,0.124).
atm(train,d48,d48_9,h,3,0.124).
atm(train,d48,d48_10,n,32,-0.407).
atm(train,d48,d48_11,c,14,0.593).
atm(train,d48,d48_12,c,10,-0.157).
atm(train,d48,d48_13,h,3,0.044).
atm(train,d48,d48_14,h,3,0.044).
atm(train,d48,d48_15,h,3,0.044).
atm(train,d48,d48_16,o,40,-0.557).
atm(train,d48,d48_17,h,1,0.243).
atm(train,d48,d48_18,n,32,-0.787).
atm(train,d48,d48_19,o,50,-0.237).
atm(train,d48,d48_20,c,10,-0.106).
atm(train,d48,d48_21,c,10,-0.106).
atm(train,d48,d48_22,h,3,0.044).
atm(train,d48,d48_23,h,3,0.044).
atm(train,d48,d48_24,h,3,0.044).
atm(train,d48,d48_25,h,1,0.323).
atm(train,d48,d48_26,h,1,0.323).
atm(train,d48,d48_27,h,3,0.044).
atm(train,d48,d48_28,h,3,0.044).
atm(train,d49,d49_1,c,22,-0.127).
atm(train,d49,d49_2,c,22,0.253).
atm(train,d49,d49_3,c,22,-0.127).
atm(train,d49,d49_4,c,22,-0.127).
atm(train,d49,d49_5,c,22,0.203).
atm(train,d49,d49_6,c,22,-0.127).
atm(train,d49,d49_7,h,3,0.133).
atm(train,d49,d49_8,h,3,0.133).
atm(train,d49,d49_9,h,3,0.133).
atm(train,d49,d49_10,n,32,-0.777).
atm(train,d49,d49_11,o,45,-0.647).
atm(train,d49,d49_12,n,38,0.802).
atm(train,d49,d49_13,h,1,0.333).
atm(train,d49,d49_14,h,1,0.333).
atm(train,d49,d49_15,o,40,-0.397).
atm(train,d49,d49_16,o,40,-0.397).
atm(train,d49,d49_17,h,8,0.403).
atm(train,d5,d5_1,c,22,-0.119).
atm(train,d5,d5_2,c,22,-0.119).
atm(train,d5,d5_3,c,22,0.211).
atm(train,d5,d5_4,c,22,-0.119).
atm(train,d5,d5_5,c,22,0.211).
atm(train,d5,d5_6,c,22,-0.119).
atm(train,d5,d5_7,h,3,0.141).
atm(train,d5,d5_8,h,3,0.141).
atm(train,d5,d5_9,h,3,0.141).
atm(train,d5,d5_10,n,32,-0.77).
atm(train,d5,d5_11,n,32,-0.77).
atm(train,d5,d5_12,cl,93,-0.189).
atm(train,d5,d5_13,h,1,0.34).
atm(train,d5,d5_14,h,1,0.34).
atm(train,d5,d5_15,h,1,0.34).
atm(train,d5,d5_16,h,1,0.34).
atm(train,d50,d50_1,c,21,0.034).
atm(train,d50,d50_2,c,21,0.034).
atm(train,d50,d50_3,n,34,-0.505).
atm(train,d50,d50_4,c,21,0.034).
atm(train,d50,d50_5,s,72,-0.196).
atm(train,d50,d50_6,n,32,-0.366).
atm(train,d50,d50_7,n,38,0.835).
atm(train,d50,d50_8,o,40,-0.366).
atm(train,d50,d50_9,o,40,-0.366).
atm(train,d50,d50_10,h,1,0.364).
atm(train,d50,d50_11,h,1,0.364).
atm(train,d50,d50_12,h,3,0.134).
atm(train,d51,d51_1,c,16,-0.169).
atm(train,d51,d51_2,c,14,0.632).
atm(train,d51,d51_3,c,16,-0.169).
atm(train,d51,d51_4,c,16,-0.169).
atm(train,d51,d51_5,c,14,0.632).
atm(train,d51,d51_6,c,16,-0.169).
atm(train,d51,d51_7,h,3,0.131).
atm(train,d51,d51_8,h,3,0.131).
atm(train,d51,d51_9,h,3,0.131).
atm(train,d51,d51_10,h,3,0.131).
atm(train,d51,d51_11,n,32,-0.369).
atm(train,d51,d51_12,n,32,-0.369).
atm(train,d51,d51_13,o,45,-0.618).
atm(train,d51,d51_14,o,45,-0.618).
atm(train,d51,d51_15,h,1,0.431).
atm(train,d51,d51_16,h,1,0.431).
atm(train,d52,d52_1,c,22,-0.134).
atm(train,d52,d52_2,c,22,-0.133).
atm(train,d52,d52_3,c,22,-0.133).
atm(train,d52,d52_4,c,29,-0.003).
atm(train,d52,d52_5,c,22,-0.134).
atm(train,d52,d52_6,c,22,-0.134).
atm(train,d52,d52_7,h,3,0.127).
atm(train,d52,d52_8,h,3,0.127).
atm(train,d52,d52_9,h,3,0.127).
atm(train,d52,d52_10,h,3,0.127).
atm(train,d52,d52_11,h,3,0.127).
atm(train,d52,d52_12,c,29,-0.003).
atm(train,d52,d52_13,c,22,0.196).
atm(train,d52,d52_14,c,22,-0.133).
atm(train,d52,d52_15,c,22,-0.134).
atm(train,d52,d52_16,c,22,-0.133).
atm(train,d52,d52_17,c,22,-0.133).
atm(train,d52,d52_18,h,3,0.127).
atm(train,d52,d52_19,h,3,0.127).
atm(train,d52,d52_20,h,3,0.127).
atm(train,d52,d52_21,h,3,0.127).
atm(train,d52,d52_22,n,32,-0.784).
atm(train,d52,d52_23,h,1,0.326).
atm(train,d52,d52_24,h,1,0.326).
atm(train,d53,d53_1,c,22,-0.081).
atm(train,d53,d53_2,c,22,-0.081).
atm(train,d53,d53_3,c,22,0.249).
atm(train,d53,d53_4,c,22,-0.081).
atm(train,d53,d53_5,c,22,0.049).
atm(train,d53,d53_6,c,22,-0.081).
atm(train,d53,d53_7,h,3,0.179).
atm(train,d53,d53_8,h,3,0.179).
atm(train,d53,d53_9,c,14,0.828).
atm(train,d53,d53_10,o,45,-0.622).
atm(train,d53,d53_11,cl,93,-0.151).
atm(train,d53,d53_12,n,32,-0.732).
atm(train,d53,d53_13,cl,93,-0.151).
atm(train,d53,d53_14,o,51,-0.561).
atm(train,d53,d53_15,h,1,0.299).
atm(train,d53,d53_16,h,1,0.379).
atm(train,d53,d53_17,h,1,0.379).
atm(train,d54,d54_1,c,22,-0.129).
atm(train,d54,d54_2,c,22,0.181).
atm(train,d54,d54_3,c,22,-0.129).
atm(train,d54,d54_4,c,22,-0.129).
atm(train,d54,d54_5,c,22,0.201).
atm(train,d54,d54_6,c,22,-0.129).
atm(train,d54,d54_7,h,3,0.131).
atm(train,d54,d54_8,h,3,0.131).
atm(train,d54,d54_9,h,3,0.131).
atm(train,d54,d54_10,o,50,-0.229).
atm(train,d54,d54_11,c,10,-0.099).
atm(train,d54,d54_12,c,10,-0.099).
atm(train,d54,d54_13,h,3,0.051).
atm(train,d54,d54_14,h,3,0.051).
atm(train,d54,d54_15,h,3,0.051).
atm(train,d54,d54_16,n,38,0.802).
atm(train,d54,d54_17,n,32,-0.399).
atm(train,d54,d54_18,o,40,-0.399).
atm(train,d54,d54_19,o,40,-0.399).
atm(train,d54,d54_20,c,14,0.602).
atm(train,d54,d54_21,c,10,-0.149).
atm(train,d54,d54_22,h,3,0.051).
atm(train,d54,d54_23,h,3,0.051).
atm(train,d54,d54_24,h,3,0.051).
atm(train,d54,d54_25,o,40,-0.549).
atm(train,d54,d54_26,h,1,0.251).
atm(train,d54,d54_27,h,3,0.051).
atm(train,d54,d54_28,h,3,0.051).
atm(train,d55,d55_1,c,22,-0.132).
atm(train,d55,d55_2,c,22,0.198).
atm(train,d55,d55_3,c,22,-0.132).
atm(train,d55,d55_4,c,22,-0.132).
atm(train,d55,d55_5,c,22,0.198).
atm(train,d55,d55_6,c,22,-0.132).
atm(train,d55,d55_7,h,3,0.128).
atm(train,d55,d55_8,h,3,0.128).
atm(train,d55,d55_9,h,3,0.128).
atm(train,d55,d55_10,n,32,-0.781).
atm(train,d55,d55_11,n,38,0.799).
atm(train,d55,d55_12,n,32,-0.781).
atm(train,d55,d55_13,o,40,-0.401).
atm(train,d55,d55_14,o,40,-0.401).
atm(train,d55,d55_15,h,1,0.328).
atm(train,d55,d55_16,h,1,0.328).
atm(train,d55,d55_17,h,1,0.328).
atm(train,d55,d55_18,h,1,0.329).
atm(train,d56,d56_1,c,22,-0.127).
atm(train,d56,d56_2,c,22,-0.127).
atm(train,d56,d56_3,c,22,-0.127).
atm(train,d56,d56_4,c,22,0.203).
atm(train,d56,d56_5,c,22,0.253).
atm(train,d56,d56_6,c,22,-0.127).
atm(train,d56,d56_7,h,3,0.133).
atm(train,d56,d56_8,h,3,0.133).
atm(train,d56,d56_9,h,3,0.133).
atm(train,d56,d56_10,o,45,-0.647).
atm(train,d56,d56_11,n,32,-0.777).
atm(train,d56,d56_12,n,38,0.802).
atm(train,d56,d56_13,o,40,-0.397).
atm(train,d56,d56_14,o,40,-0.397).
atm(train,d56,d56_15,h,1,0.333).
atm(train,d56,d56_16,h,1,0.333).
atm(train,d56,d56_17,h,8,0.403).
atm(train,d57,d57_1,c,22,-0.127).
atm(train,d57,d57_2,c,22,-0.127).
atm(train,d57,d57_3,c,22,-0.127).
atm(train,d57,d57_4,c,22,0.203).
atm(train,d57,d57_5,c,22,0.253).
atm(train,d57,d57_6,c,22,-0.127).
atm(train,d57,d57_7,h,3,0.133).
atm(train,d57,d57_8,h,3,0.133).
atm(train,d57,d57_9,h,3,0.133).
atm(train,d57,d57_10,o,45,-0.647).
atm(train,d57,d57_11,n,32,-0.777).
atm(train,d57,d57_12,n,38,0.802).
atm(train,d57,d57_13,o,40,-0.397).
atm(train,d57,d57_14,o,40,-0.397).
atm(train,d57,d57_15,h,1,0.333).
atm(train,d57,d57_16,h,1,0.333).
atm(train,d57,d57_17,h,8,0.403).
atm(train,d58,d58_1,c,22,-0.212).
atm(train,d58,d58_2,c,22,-0.161).
atm(train,d58,d58_3,c,22,-0.161).
atm(train,d58,d58_4,c,22,0.168).
atm(train,d58,d58_5,c,22,-0.161).
atm(train,d58,d58_6,c,22,-0.161).
atm(train,d58,d58_7,h,3,0.099).
atm(train,d58,d58_8,h,3,0.099).
atm(train,d58,d58_9,h,3,0.099).
atm(train,d58,d58_10,c,22,0.169).
atm(train,d58,d58_11,c,22,-0.161).
atm(train,d58,d58_12,c,22,-0.161).
atm(train,d58,d58_13,c,22,0.169).
atm(train,d58,d58_14,c,22,-0.081).
atm(train,d58,d58_15,c,22,-0.161).
atm(train,d58,d58_16,h,3,0.099).
atm(train,d58,d58_17,h,3,0.099).
atm(train,d58,d58_18,h,3,0.099).
atm(train,d58,d58_19,c,22,0.168).
atm(train,d58,d58_20,c,22,-0.161).
atm(train,d58,d58_21,c,22,-0.161).
atm(train,d58,d58_22,c,22,-0.161).
atm(train,d58,d58_23,c,22,-0.161).
atm(train,d58,d58_24,c,22,-0.161).
atm(train,d58,d58_25,h,3,0.099).
atm(train,d58,d58_26,h,3,0.099).
atm(train,d58,d58_27,h,3,0.099).
atm(train,d58,d58_28,h,3,0.099).
atm(train,d58,d58_29,h,3,0.099).
atm(train,d58,d58_30,c,10,-0.031).
atm(train,d58,d58_31,h,3,0.019).
atm(train,d58,d58_32,h,3,0.019).
atm(train,d58,d58_33,h,3,0.019).
atm(train,d58,d58_34,n,38,0.768).
atm(train,d58,d58_35,n,32,-0.432).
atm(train,d58,d58_36,n,32,-0.432).
atm(train,d58,d58_37,s,78,0.718).
atm(train,d58,d58_38,h,1,0.268).
atm(train,d58,d58_39,h,1,0.268).
atm(train,d58,d58_40,o,40,-0.432).
atm(train,d58,d58_41,o,40,-0.432).
atm(train,d58,d58_42,o,40,-0.282).
atm(train,d58,d58_43,o,50,-0.262).
atm(train,d58,d58_44,o,40,-0.282).
atm(train,d58,d58_45,na,81,0.968).
atm(train,d59,d59_1,c,22,-0.145).
atm(train,d59,d59_2,c,22,0.165).
atm(train,d59,d59_3,c,22,-0.145).
atm(train,d59,d59_4,c,22,-0.145).
atm(train,d59,d59_5,c,22,0.185).
atm(train,d59,d59_6,c,22,-0.145).
atm(train,d59,d59_7,h,3,0.115).
atm(train,d59,d59_8,h,3,0.115).
atm(train,d59,d59_9,h,3,0.115).
atm(train,d59,d59_10,h,3,0.115).
atm(train,d59,d59_11,n,32,-0.795).
atm(train,d59,d59_12,o,50,-0.245).
atm(train,d59,d59_13,c,10,0.075).
atm(train,d59,d59_14,h,3,0.035).
atm(train,d59,d59_15,h,3,0.035).
atm(train,d59,d59_16,h,3,0.035).
atm(train,d59,d59_17,h,1,0.315).
atm(train,d59,d59_18,h,1,0.315).
atm(train,d6,d6_1,c,22,-0.111).
atm(train,d6,d6_2,c,22,-0.111).
atm(train,d6,d6_3,c,22,-0.111).
atm(train,d6,d6_4,c,22,0.218).
atm(train,d6,d6_5,c,22,0.219).
atm(train,d6,d6_6,c,22,-0.111).
atm(train,d6,d6_7,h,3,0.149).
atm(train,d6,d6_8,h,3,0.149).
atm(train,d6,d6_9,n,32,-0.78).
atm(train,d6,d6_10,cl,93,-0.2).
atm(train,d6,d6_11,h,1,0.33).
atm(train,d6,d6_12,h,1,0.33).
atm(train,d6,d6_13,h,3,0.149).
atm(train,d6,d6_14,n,32,-0.78).
atm(train,d6,d6_15,h,1,0.33).
atm(train,d6,d6_16,h,1,0.33).
atm(train,d60,d60_1,c,22,-0.112).
atm(train,d60,d60_2,c,22,-0.112).
atm(train,d60,d60_3,c,22,-0.112).
atm(train,d60,d60_4,c,22,-0.112).
atm(train,d60,d60_5,c,22,0.218).
atm(train,d60,d60_6,c,22,-0.112).
atm(train,d60,d60_7,h,3,0.148).
atm(train,d60,d60_8,h,3,0.148).
atm(train,d60,d60_9,h,3,0.148).
atm(train,d60,d60_10,h,3,0.148).
atm(train,d60,d60_11,n,32,-0.763).
atm(train,d60,d60_12,cl,93,-0.182).
atm(train,d60,d60_13,h,1,0.347).
atm(train,d60,d60_14,h,1,0.348).
atm(train,d61,d61_1,c,22,-0.113).
atm(train,d61,d61_2,c,22,-0.113).
atm(train,d61,d61_3,c,22,-0.113).
atm(train,d61,d61_4,c,22,0.266).
atm(train,d61,d61_5,c,22,-0.113).
atm(train,d61,d61_6,c,22,-0.113).
atm(train,d61,d61_7,h,3,0.146).
atm(train,d61,d61_8,h,3,0.146).
atm(train,d61,d61_9,h,3,0.146).
atm(train,d61,d61_10,h,3,0.147).
atm(train,d61,d61_11,n,38,0.816).
atm(train,d61,d61_12,o,49,-0.654).
atm(train,d61,d61_13,p,61,1.323).
atm(train,d61,d61_14,o,49,-0.654).
atm(train,d61,d61_15,o,40,-0.384).
atm(train,d61,d61_16,o,40,-0.384).
atm(train,d61,d61_17,s,76,-0.033).
atm(train,d61,d61_18,o,49,-0.654).
atm(train,d61,d61_19,c,10,-0.083).
atm(train,d61,d61_20,c,10,-0.083).
atm(train,d61,d61_21,h,3,0.067).
atm(train,d61,d61_22,h,3,0.067).
atm(train,d61,d61_23,h,3,0.067).
atm(train,d61,d61_24,h,3,0.067).
atm(train,d61,d61_25,h,3,0.067).
atm(train,d61,d61_26,c,10,-0.083).
atm(train,d61,d61_27,c,10,-0.083).
atm(train,d61,d61_28,h,3,0.067).
atm(train,d61,d61_29,h,3,0.067).
atm(train,d61,d61_30,h,3,0.067).
atm(train,d61,d61_31,h,3,0.067).
atm(train,d61,d61_32,h,3,0.067).
atm(train,d62,d62_1,c,22,0.216).
atm(train,d62,d62_2,c,22,-0.114).
atm(train,d62,d62_3,c,27,-0.084).
atm(train,d62,d62_4,c,27,0.016).
atm(train,d62,d62_5,c,22,-0.114).
atm(train,d62,d62_6,c,22,-0.114).
atm(train,d62,d62_7,h,3,0.146).
atm(train,d62,d62_8,h,3,0.146).
atm(train,d62,d62_9,h,3,0.146).
atm(train,d62,d62_10,n,35,-0.383).
atm(train,d62,d62_11,c,27,-0.084).
atm(train,d62,d62_12,c,27,0.016).
atm(train,d62,d62_13,c,22,-0.114).
atm(train,d62,d62_14,h,3,0.146).
atm(train,d62,d62_15,c,22,-0.114).
atm(train,d62,d62_16,c,22,0.217).
atm(train,d62,d62_17,c,22,-0.114).
atm(train,d62,d62_18,c,22,-0.114).
atm(train,d62,d62_19,h,3,0.146).
atm(train,d62,d62_20,h,3,0.146).
atm(train,d62,d62_21,h,3,0.146).
atm(train,d62,d62_22,n,32,-0.763).
atm(train,d62,d62_23,n,32,-0.763).
atm(train,d62,d62_24,h,1,0.347).
atm(train,d62,d62_25,h,1,0.347).
atm(train,d62,d62_26,h,1,0.347).
atm(train,d62,d62_27,h,1,0.347).
atm(train,d63,d63_1,c,22,0.194).
atm(train,d63,d63_2,c,22,-0.136).
atm(train,d63,d63_3,c,22,-0.136).
atm(train,d63,d63_4,c,22,0.194).
atm(train,d63,d63_5,c,22,-0.136).
atm(train,d63,d63_6,c,22,-0.136).
atm(train,d63,d63_7,h,3,0.124).
atm(train,d63,d63_8,h,3,0.124).
atm(train,d63,d63_9,h,3,0.124).
atm(train,d63,d63_10,n,32,-0.786).
atm(train,d63,d63_11,n,38,0.794).
atm(train,d63,d63_12,n,32,-0.406).
atm(train,d63,d63_13,c,10,-0.006).
atm(train,d63,d63_14,c,10,0.164).
atm(train,d63,d63_15,h,3,0.044).
atm(train,d63,d63_16,h,3,0.044).
atm(train,d63,d63_17,o,45,-0.656).
atm(train,d63,d63_18,h,3,0.034).
atm(train,d63,d63_19,h,3,0.034).
atm(train,d63,d63_20,o,40,-0.406).
atm(train,d63,d63_21,o,40,-0.406).
atm(train,d63,d63_22,h,1,0.324).
atm(train,d63,d63_23,h,1,0.324).
atm(train,d63,d63_24,h,1,0.294).
atm(train,d63,d63_25,h,8,0.394).
atm(train,d64,d64_1,c,22,-0.14).
atm(train,d64,d64_2,c,22,-0.14).
atm(train,d64,d64_3,c,27,-0.01).
atm(train,d64,d64_4,c,27,-0.01).
atm(train,d64,d64_5,c,22,-0.14).
atm(train,d64,d64_6,c,22,-0.14).
atm(train,d64,d64_7,h,3,0.12).
atm(train,d64,d64_8,h,3,0.12).
atm(train,d64,d64_9,h,3,0.12).
atm(train,d64,d64_10,h,3,0.12).
atm(train,d64,d64_11,c,22,-0.14).
atm(train,d64,d64_12,c,22,-0.14).
atm(train,d64,d64_13,c,22,0.19).
atm(train,d64,d64_14,c,22,-0.14).
atm(train,d64,d64_15,h,3,0.12).
atm(train,d64,d64_16,h,3,0.12).
atm(train,d64,d64_17,h,3,0.12).
atm(train,d64,d64_18,c,22,-0.14).
atm(train,d64,d64_19,c,22,-0.14).
atm(train,d64,d64_20,c,22,-0.14).
atm(train,d64,d64_21,c,22,-0.14).
atm(train,d64,d64_22,c,22,-0.14).
atm(train,d64,d64_23,c,22,0.19).
atm(train,d64,d64_24,h,3,0.12).
atm(train,d64,d64_25,h,3,0.12).
atm(train,d64,d64_26,h,3,0.12).
atm(train,d64,d64_27,h,3,0.12).
atm(train,d64,d64_28,h,3,0.12).
atm(train,d64,d64_29,n,32,-0.41).
atm(train,d64,d64_30,h,1,0.29).
atm(train,d65,d65_1,c,22,-0.115).
atm(train,d65,d65_2,c,22,0.265).
atm(train,d65,d65_3,c,22,-0.115).
atm(train,d65,d65_4,c,22,-0.115).
atm(train,d65,d65_5,c,22,-0.115).
atm(train,d65,d65_6,c,22,-0.115).
atm(train,d65,d65_7,h,3,0.145).
atm(train,d65,d65_8,h,3,0.145).
atm(train,d65,d65_9,h,3,0.145).
atm(train,d65,d65_10,o,45,-0.633).
atm(train,d65,d65_11,n,38,0.817).
atm(train,d65,d65_12,as,101,1.016).
atm(train,d65,d65_13,o,45,-0.633).
atm(train,d65,d65_14,o,45,-0.633).
atm(train,d65,d65_15,o,40,-0.385).
atm(train,d65,d65_16,o,40,-0.385).
atm(train,d65,d65_17,h,8,0.415).
atm(train,d65,d65_18,h,1,0.415).
atm(train,d65,d65_19,h,1,0.415).
atm(train,d65,d65_20,o,40,-0.534).
atm(train,d66,d66_1,c,22,-0.133).
atm(train,d66,d66_2,c,22,-0.133).
atm(train,d66,d66_3,c,29,-0.002).
atm(train,d66,d66_4,c,22,0.098).
atm(train,d66,d66_5,c,22,-0.133).
atm(train,d66,d66_6,c,22,-0.133).
atm(train,d66,d66_7,c,27,-0.102).
atm(train,d66,d66_8,c,27,0.098).
atm(train,d66,d66_9,o,53,-0.303).
atm(train,d66,d66_10,c,27,0.098).
atm(train,d66,d66_11,c,27,-0.102).
atm(train,d66,d66_12,c,29,-0.002).
atm(train,d66,d66_13,c,22,-0.133).
atm(train,d66,d66_14,c,14,0.597).
atm(train,d66,d66_15,c,22,-0.173).
atm(train,d66,d66_16,c,22,-0.133).
atm(train,d66,d66_17,c,22,-0.133).
atm(train,d66,d66_18,c,22,-0.183).
atm(train,d66,d66_19,c,22,0.197).
atm(train,d66,d66_20,c,22,-0.133).
atm(train,d66,d66_21,c,14,0.697).
atm(train,d66,d66_22,o,49,-0.653).
atm(train,d66,d66_23,o,51,-0.553).
atm(train,d66,d66_24,c,10,-0.383).
atm(train,d66,d66_25,h,3,0.127).
atm(train,d66,d66_26,h,3,0.128).
atm(train,d66,d66_27,h,3,0.128).
atm(train,d66,d66_28,h,3,0.128).
atm(train,d66,d66_29,h,3,0.128).
atm(train,d66,d66_30,h,3,0.128).
atm(train,d66,d66_31,h,3,0.108).
atm(train,d66,d66_32,h,3,0.108).
atm(train,d66,d66_33,h,3,0.108).
atm(train,d66,d66_34,h,3,0.127).
atm(train,d66,d66_35,h,3,0.127).
atm(train,d66,d66_36,n,32,-0.403).
atm(train,d66,d66_37,c,10,-0.002).
atm(train,d66,d66_38,c,10,-0.102).
atm(train,d66,d66_39,h,3,0.048).
atm(train,d66,d66_40,h,3,0.048).
atm(train,d66,d66_41,h,3,0.048).
atm(train,d66,d66_42,h,3,0.048).
atm(train,d66,d66_43,h,3,0.048).
atm(train,d66,d66_44,c,10,0.247).
atm(train,d66,d66_45,c,10,-0.102).
atm(train,d66,d66_46,h,3,0.098).
atm(train,d66,d66_47,h,3,0.098).
atm(train,d66,d66_48,h,3,0.048).
atm(train,d66,d66_49,h,3,0.048).
atm(train,d66,d66_50,h,3,0.048).
atm(train,d66,d66_51,n,32,-0.403).
atm(train,d66,d66_52,c,10,-0.002).
atm(train,d66,d66_53,h,3,0.048).
atm(train,d66,d66_54,h,3,0.048).
atm(train,d66,d66_55,h,3,0.048).
atm(train,d66,d66_56,c,10,-0.002).
atm(train,d66,d66_57,c,10,-0.102).
atm(train,d66,d66_58,h,3,0.048).
atm(train,d66,d66_59,h,3,0.048).
atm(train,d66,d66_60,h,3,0.048).
atm(train,d66,d66_61,h,3,0.048).
atm(train,d66,d66_62,h,3,0.048).
atm(train,d66,d66_63,h,1,0.297).
atm(train,d67,d67_1,c,22,-0.132).
atm(train,d67,d67_2,c,22,-0.003).
atm(train,d67,d67_3,c,22,-0.132).
atm(train,d67,d67_4,c,22,-0.132).
atm(train,d67,d67_5,c,22,0.198).
atm(train,d67,d67_6,c,22,-0.132).
atm(train,d67,d67_7,h,3,0.127).
atm(train,d67,d67_8,h,3,0.127).
atm(train,d67,d67_9,h,3,0.127).
atm(train,d67,d67_10,h,3,0.127).
atm(train,d67,d67_11,n,32,-0.403).
atm(train,d67,d67_12,c,14,0.599).
atm(train,d67,d67_13,c,10,-0.153).
atm(train,d67,d67_14,h,3,0.047).
atm(train,d67,d67_15,h,3,0.047).
atm(train,d67,d67_16,h,3,0.047).
atm(train,d67,d67_17,o,40,-0.552).
atm(train,d67,d67_18,c,14,0.548).
atm(train,d67,d67_19,c,10,0.087).
atm(train,d67,d67_20,cl,93,-0.203).
atm(train,d67,d67_21,h,3,0.047).
atm(train,d67,d67_22,h,3,0.047).
atm(train,d67,d67_23,o,42,-0.581).
atm(train,d67,d67_24,h,1,0.248).
atm(train,d68,d68_1,c,22,-0.119).
atm(train,d68,d68_2,c,22,0.211).
atm(train,d68,d68_3,c,22,-0.119).
atm(train,d68,d68_4,c,22,-0.119).
atm(train,d68,d68_5,c,22,0.211).
atm(train,d68,d68_6,c,22,-0.119).
atm(train,d68,d68_7,h,3,0.141).
atm(train,d68,d68_8,h,3,0.141).
atm(train,d68,d68_9,h,3,0.141).
atm(train,d68,d68_10,n,32,-0.77).
atm(train,d68,d68_11,n,32,-0.77).
atm(train,d68,d68_12,cl,93,-0.189).
atm(train,d68,d68_13,h,1,0.34).
atm(train,d68,d68_14,h,1,0.34).
atm(train,d68,d68_15,h,1,0.34).
atm(train,d68,d68_16,h,1,0.34).
atm(train,d69,d69_1,c,22,-0.114).
atm(train,d69,d69_2,c,22,-0.163).
atm(train,d69,d69_3,c,22,-0.114).
atm(train,d69,d69_4,c,22,-0.114).
atm(train,d69,d69_5,c,22,0.217).
atm(train,d69,d69_6,c,22,-0.114).
atm(train,d69,d69_7,h,3,0.147).
atm(train,d69,d69_8,h,3,0.147).
atm(train,d69,d69_9,h,3,0.146).
atm(train,d69,d69_10,n,32,-0.763).
atm(train,d69,d69_11,c,10,0.016).
atm(train,d69,d69_12,h,3,0.066).
atm(train,d69,d69_13,h,3,0.066).
atm(train,d69,d69_14,h,3,0.066).
atm(train,d69,d69_15,cl,93,-0.183).
atm(train,d69,d69_16,h,1,0.347).
atm(train,d69,d69_17,h,1,0.347).
atm(train,d7,d7_1,c,22,-0.163).
atm(train,d7,d7_2,c,22,-0.163).
atm(train,d7,d7_3,c,22,0.067).
atm(train,d7,d7_4,c,22,-0.163).
atm(train,d7,d7_5,c,22,-0.163).
atm(train,d7,d7_6,c,22,0.167).
atm(train,d7,d7_7,h,3,0.097).
atm(train,d7,d7_8,h,3,0.097).
atm(train,d7,d7_9,h,3,0.097).
atm(train,d7,d7_10,c,22,-0.163).
atm(train,d7,d7_11,c,22,0.167).
atm(train,d7,d7_12,c,22,-0.163).
atm(train,d7,d7_13,c,22,-0.163).
atm(train,d7,d7_14,c,22,0.067).
atm(train,d7,d7_15,c,22,-0.163).
atm(train,d7,d7_16,h,3,0.097).
atm(train,d7,d7_17,h,3,0.097).
atm(train,d7,d7_18,h,3,0.097).
atm(train,d7,d7_19,h,3,0.097).
atm(train,d7,d7_20,c,14,0.067).
atm(train,d7,d7_21,c,22,-0.163).
atm(train,d7,d7_22,c,22,-0.163).
atm(train,d7,d7_23,c,14,0.518).
atm(train,d7,d7_24,c,22,-0.163).
atm(train,d7,d7_25,c,22,-0.163).
atm(train,d7,d7_26,h,3,0.097).
atm(train,d7,d7_27,h,3,0.097).
atm(train,d7,d7_28,h,3,0.097).
atm(train,d7,d7_29,h,3,0.097).
atm(train,d7,d7_30,c,16,-0.232).
atm(train,d7,d7_31,n,499,-0.033).
atm(train,d7,d7_32,h,3,0.097).
atm(train,d7,d7_33,n,32,-0.812).
atm(train,d7,d7_34,n,32,-0.812).
atm(train,d7,d7_35,h,1,0.298).
atm(train,d7,d7_36,h,1,0.298).
atm(train,d7,d7_37,h,1,0.298).
atm(train,d7,d7_38,h,1,0.298).
atm(train,d7,d7_39,h,1,0.218).
atm(train,d7,d7_40,h,1,0.218).
atm(train,d70,d70_1,c,22,-0.151).
atm(train,d70,d70_2,c,22,0.159).
atm(train,d70,d70_3,c,22,-0.151).
atm(train,d70,d70_4,c,22,0.159).
atm(train,d70,d70_5,c,22,0.179).
atm(train,d70,d70_6,c,22,-0.151).
atm(train,d70,d70_7,h,3,0.109).
atm(train,d70,d70_8,h,3,0.109).
atm(train,d70,d70_9,h,3,0.109).
atm(train,d70,d70_10,n,32,-0.8).
atm(train,d70,d70_11,o,50,-0.251).
atm(train,d70,d70_12,c,10,0.069).
atm(train,d70,d70_13,h,3,0.029).
atm(train,d70,d70_14,h,3,0.029).
atm(train,d70,d70_15,h,3,0.029).
atm(train,d70,d70_16,o,50,-0.251).
atm(train,d70,d70_17,c,10,0.069).
atm(train,d70,d70_18,h,3,0.029).
atm(train,d70,d70_19,h,3,0.029).
atm(train,d70,d70_20,h,3,0.029).
atm(train,d70,d70_21,h,1,0.309).
atm(train,d70,d70_22,h,1,0.31).
atm(train,d71,d71_1,c,22,-0.12).
atm(train,d71,d71_2,c,22,-0.12).
atm(train,d71,d71_3,c,22,-0.12).
atm(train,d71,d71_4,c,22,-0.12).
atm(train,d71,d71_5,c,22,0.26).
atm(train,d71,d71_6,c,22,-0.12).
atm(train,d71,d71_7,h,3,0.14).
atm(train,d71,d71_8,h,3,0.14).
atm(train,d71,d71_9,h,3,0.14).
atm(train,d71,d71_10,h,3,0.14).
atm(train,d71,d71_11,n,38,0.809).
atm(train,d71,d71_12,o,49,-0.661).
atm(train,d71,d71_13,p,61,1.316).
atm(train,d71,d71_14,o,49,-0.661).
atm(train,d71,d71_15,o,49,-0.661).
atm(train,d71,d71_16,c,10,0.1).
atm(train,d71,d71_17,h,3,0.06).
atm(train,d71,d71_18,h,3,0.06).
atm(train,d71,d71_19,h,3,0.06).
atm(train,d71,d71_20,s,75,-0.14).
atm(train,d71,d71_21,o,40,-0.391).
atm(train,d71,d71_22,o,40,-0.391).
atm(train,d71,d71_23,c,10,0.1).
atm(train,d71,d71_24,h,3,0.06).
atm(train,d71,d71_25,h,3,0.06).
atm(train,d71,d71_26,h,3,0.06).
atm(train,d72,d72_1,c,22,-0.114).
atm(train,d72,d72_2,c,22,-0.114).
atm(train,d72,d72_3,c,22,-0.114).
atm(train,d72,d72_4,c,22,-0.114).
atm(train,d72,d72_5,c,22,0.216).
atm(train,d72,d72_6,c,22,0.016).
atm(train,d72,d72_7,h,3,0.146).
atm(train,d72,d72_8,h,3,0.146).
atm(train,d72,d72_9,h,3,0.146).
atm(train,d72,d72_10,n,32,-0.765).
atm(train,d72,d72_11,c,14,0.795).
atm(train,d72,d72_12,o,45,-0.655).
atm(train,d72,d72_13,n,38,0.815).
atm(train,d72,d72_14,o,40,-0.384).
atm(train,d72,d72_15,o,40,-0.384).
atm(train,d72,d72_16,h,1,0.346).
atm(train,d72,d72_17,h,1,0.346).
atm(train,d72,d72_18,o,51,-0.594).
atm(train,d72,d72_19,h,1,0.266).
atm(train,d73,d73_1,c,22,-0.132).
atm(train,d73,d73_2,c,22,-0.132).
atm(train,d73,d73_3,c,22,-0.132).
atm(train,d73,d73_4,c,22,0.198).
atm(train,d73,d73_5,c,22,0.198).
atm(train,d73,d73_6,c,22,-0.132).
atm(train,d73,d73_7,h,3,0.128).
atm(train,d73,d73_8,h,3,0.128).
atm(train,d73,d73_9,h,3,0.128).
atm(train,d73,d73_10,n,32,-0.781).
atm(train,d73,d73_11,n,32,-0.781).
atm(train,d73,d73_12,n,38,0.799).
atm(train,d73,d73_13,h,1,0.328).
atm(train,d73,d73_14,h,1,0.328).
atm(train,d73,d73_15,h,1,0.329).
atm(train,d73,d73_16,h,1,0.328).
atm(train,d73,d73_17,o,40,-0.401).
atm(train,d73,d73_18,o,40,-0.401).
atm(train,d74,d74_1,c,22,-0.145).
atm(train,d74,d74_2,c,22,-0.145).
atm(train,d74,d74_3,c,22,-0.144).
atm(train,d74,d74_4,c,22,0.186).
atm(train,d74,d74_5,c,22,-0.144).
atm(train,d74,d74_6,c,22,-0.145).
atm(train,d74,d74_7,h,3,0.115).
atm(train,d74,d74_8,h,3,0.115).
atm(train,d74,d74_9,h,3,0.115).
atm(train,d74,d74_10,h,3,0.115).
atm(train,d74,d74_11,h,3,0.115).
atm(train,d74,d74_12,c,22,0.186).
atm(train,d74,d74_13,c,22,-0.145).
atm(train,d74,d74_14,c,22,-0.145).
atm(train,d74,d74_15,c,22,0.186).
atm(train,d74,d74_16,c,22,-0.145).
atm(train,d74,d74_17,c,22,-0.145).
atm(train,d74,d74_18,h,3,0.115).
atm(train,d74,d74_19,h,3,0.115).
atm(train,d74,d74_20,h,3,0.115).
atm(train,d74,d74_21,h,3,0.115).
atm(train,d74,d74_22,n,32,-0.414).
atm(train,d74,d74_23,n,32,-0.794).
atm(train,d74,d74_24,h,1,0.286).
atm(train,d74,d74_25,h,1,0.316).
atm(train,d74,d74_26,h,1,0.316).
atm(train,d75,d75_1,c,22,-0.137).
atm(train,d75,d75_2,c,22,-0.137).
atm(train,d75,d75_3,c,22,-0.137).
atm(train,d75,d75_4,c,22,0.193).
atm(train,d75,d75_5,c,22,-0.187).
atm(train,d75,d75_6,c,22,0.193).
atm(train,d75,d75_7,h,3,0.123).
atm(train,d75,d75_8,h,3,0.123).
atm(train,d75,d75_9,h,3,0.123).
atm(train,d75,d75_10,n,32,-0.786).
atm(train,d75,d75_11,c,10,-0.007).
atm(train,d75,d75_12,h,3,0.043).
atm(train,d75,d75_13,h,3,0.043).
atm(train,d75,d75_14,h,3,0.043).
atm(train,d75,d75_15,n,32,-0.786).
atm(train,d75,d75_16,h,1,0.323).
atm(train,d75,d75_17,h,1,0.323).
atm(train,d75,d75_18,h,1,0.324).
atm(train,d75,d75_19,h,1,0.323).
atm(train,d76,d76_1,c,22,-0.158).
atm(train,d76,d76_2,c,22,-0.158).
atm(train,d76,d76_3,c,22,0.172).
atm(train,d76,d76_4,c,22,-0.158).
atm(train,d76,d76_5,c,22,-0.158).
atm(train,d76,d76_6,c,22,-0.158).
atm(train,d76,d76_7,h,3,0.102).
atm(train,d76,d76_8,h,3,0.102).
atm(train,d76,d76_9,h,3,0.102).
atm(train,d76,d76_10,h,3,0.102).
atm(train,d76,d76_11,h,3,0.102).
atm(train,d76,d76_12,c,27,-0.028).
atm(train,d76,d76_13,c,27,-0.028).
atm(train,d76,d76_14,c,22,-0.158).
atm(train,d76,d76_15,c,22,-0.158).
atm(train,d76,d76_16,c,22,0.222).
atm(train,d76,d76_17,c,22,-0.158).
atm(train,d76,d76_18,h,3,0.102).
atm(train,d76,d76_19,h,3,0.102).
atm(train,d76,d76_20,c,22,-0.078).
atm(train,d76,d76_21,c,22,-0.158).
atm(train,d76,d76_22,c,22,-0.158).
atm(train,d76,d76_23,c,22,-0.158).
atm(train,d76,d76_24,h,3,0.102).
atm(train,d76,d76_25,h,3,0.102).
atm(train,d76,d76_26,n,32,-0.428).
atm(train,d76,d76_27,n,32,-0.428).
atm(train,d76,d76_28,o,45,-0.678).
atm(train,d76,d76_29,s,78,0.721).
atm(train,d76,d76_30,o,40,-0.278).
atm(train,d76,d76_31,o,40,-0.278).
atm(train,d76,d76_32,o,50,-0.258).
atm(train,d76,d76_33,na,81,0.971).
atm(train,d76,d76_34,h,8,0.372).
atm(train,d76,d76_35,c,10,-0.078).
atm(train,d76,d76_36,s,78,0.721).
atm(train,d76,d76_37,h,3,0.022).
atm(train,d76,d76_38,h,3,0.022).
atm(train,d76,d76_39,o,50,-0.258).
atm(train,d76,d76_40,o,40,-0.278).
atm(train,d76,d76_41,o,40,-0.278).
atm(train,d76,d76_42,na,81,0.971).
atm(train,d77,d77_1,c,22,-0.115).
atm(train,d77,d77_2,c,29,0.015).
atm(train,d77,d77_3,c,22,-0.115).
atm(train,d77,d77_4,c,22,-0.115).
atm(train,d77,d77_5,c,22,0.216).
atm(train,d77,d77_6,c,22,-0.115).
atm(train,d77,d77_7,h,3,0.145).
atm(train,d77,d77_8,h,3,0.145).
atm(train,d77,d77_9,h,3,0.145).
atm(train,d77,d77_10,c,22,-0.115).
atm(train,d77,d77_11,c,22,0.216).
atm(train,d77,d77_12,c,22,-0.115).
atm(train,d77,d77_13,c,22,-0.115).
atm(train,d77,d77_14,c,29,0.015).
atm(train,d77,d77_15,c,22,-0.115).
atm(train,d77,d77_16,h,3,0.146).
atm(train,d77,d77_17,h,3,0.146).
atm(train,d77,d77_18,h,3,0.146).
atm(train,d77,d77_19,cl,93,-0.184).
atm(train,d77,d77_20,cl,93,-0.184).
atm(train,d77,d77_21,n,32,-0.384).
atm(train,d77,d77_22,n,32,-0.384).
atm(train,d77,d77_23,c,10,0.105).
atm(train,d77,d77_24,c,14,0.596).
atm(train,d77,d77_25,c,14,0.616).
atm(train,d77,d77_26,n,32,-0.384).
atm(train,d77,d77_27,c,10,0.015).
atm(train,d77,d77_28,h,3,0.065).
atm(train,d77,d77_29,h,3,0.065).
atm(train,d77,d77_30,h,3,0.065).
atm(train,d77,d77_31,o,42,-0.564).
atm(train,d77,d77_32,o,40,-0.534).
atm(train,d77,d77_33,h,3,0.065).
atm(train,d77,d77_34,c,22,0.216).
atm(train,d77,d77_35,c,22,-0.115).
atm(train,d77,d77_36,c,22,-0.115).
atm(train,d77,d77_37,c,22,-0.115).
atm(train,d77,d77_38,c,22,-0.115).
atm(train,d77,d77_39,c,22,-0.115).
atm(train,d77,d77_40,h,3,0.146).
atm(train,d77,d77_41,h,3,0.146).
atm(train,d77,d77_42,h,3,0.146).
atm(train,d77,d77_43,h,3,0.146).
atm(train,d77,d77_44,h,3,0.146).
atm(train,d77,d77_45,h,1,0.266).
atm(train,d77,d77_46,n,32,-0.384).
atm(train,d77,d77_47,n,32,-0.384).
atm(train,d77,d77_48,c,10,0.105).
atm(train,d77,d77_49,c,14,0.596).
atm(train,d77,d77_50,o,42,-0.564).
atm(train,d77,d77_51,c,10,0.015).
atm(train,d77,d77_52,h,3,0.065).
atm(train,d77,d77_53,h,3,0.065).
atm(train,d77,d77_54,h,3,0.065).
atm(train,d77,d77_55,c,14,0.616).
atm(train,d77,d77_56,h,3,0.065).
atm(train,d77,d77_57,o,40,-0.534).
atm(train,d77,d77_58,n,32,-0.384).
atm(train,d77,d77_59,c,22,0.216).
atm(train,d77,d77_60,c,22,-0.115).
atm(train,d77,d77_61,c,22,-0.115).
atm(train,d77,d77_62,c,22,-0.115).
atm(train,d77,d77_63,c,22,-0.115).
atm(train,d77,d77_64,c,22,-0.115).
atm(train,d77,d77_65,h,3,0.145).
atm(train,d77,d77_66,h,3,0.145).
atm(train,d77,d77_67,h,3,0.145).
atm(train,d77,d77_68,h,3,0.145).
atm(train,d77,d77_69,h,3,0.145).
atm(train,d77,d77_70,h,1,0.266).
atm(train,d78,d78_1,c,22,-0.132).
atm(train,d78,d78_2,c,22,0.198).
atm(train,d78,d78_3,c,22,-0.132).
atm(train,d78,d78_4,c,22,-0.132).
atm(train,d78,d78_5,c,22,0.198).
atm(train,d78,d78_6,c,22,-0.132).
atm(train,d78,d78_7,h,3,0.128).
atm(train,d78,d78_8,h,3,0.128).
atm(train,d78,d78_9,h,3,0.128).
atm(train,d78,d78_10,n,32,-0.401).
atm(train,d78,d78_11,c,10,-0.002).
atm(train,d78,d78_12,c,10,0.168).
atm(train,d78,d78_13,h,3,0.048).
atm(train,d78,d78_14,h,3,0.048).
atm(train,d78,d78_15,o,45,-0.651).
atm(train,d78,d78_16,h,3,0.038).
atm(train,d78,d78_17,h,3,0.038).
atm(train,d78,d78_18,c,10,-0.002).
atm(train,d78,d78_19,c,10,0.168).
atm(train,d78,d78_20,h,3,0.048).
atm(train,d78,d78_21,h,3,0.048).
atm(train,d78,d78_22,o,45,-0.651).
atm(train,d78,d78_23,h,3,0.038).
atm(train,d78,d78_24,h,3,0.038).
atm(train,d78,d78_25,n,38,0.799).
atm(train,d78,d78_26,n,32,-0.401).
atm(train,d78,d78_27,c,10,-0.002).
atm(train,d78,d78_28,c,10,0.168).
atm(train,d78,d78_29,h,3,0.048).
atm(train,d78,d78_30,h,3,0.048).
atm(train,d78,d78_31,o,45,-0.651).
atm(train,d78,d78_32,h,3,0.038).
atm(train,d78,d78_33,h,3,0.038).
atm(train,d78,d78_34,o,40,-0.401).
atm(train,d78,d78_35,o,40,-0.401).
atm(train,d78,d78_36,h,1,0.298).
atm(train,d78,d78_37,h,8,0.398).
atm(train,d78,d78_38,h,8,0.398).
atm(train,d78,d78_39,h,8,0.398).
atm(train,d79,d79_1,c,22,-0.125).
atm(train,d79,d79_2,c,22,-0.125).
atm(train,d79,d79_3,c,27,0.005).
atm(train,d79,d79_4,c,27,0.005).
atm(train,d79,d79_5,c,22,-0.125).
atm(train,d79,d79_6,c,22,-0.125).
atm(train,d79,d79_7,h,3,0.134).
atm(train,d79,d79_8,h,3,0.134).
atm(train,d79,d79_9,h,3,0.134).
atm(train,d79,d79_10,h,3,0.134).
atm(train,d79,d79_11,c,22,-0.125).
atm(train,d79,d79_12,c,22,-0.125).
atm(train,d79,d79_13,c,22,-0.125).
atm(train,d79,d79_14,c,22,-0.125).
atm(train,d79,d79_15,h,3,0.135).
atm(train,d79,d79_16,h,3,0.135).
atm(train,d79,d79_17,h,3,0.134).
atm(train,d79,d79_18,n,32,-0.396).
atm(train,d79,d79_19,c,10,0.005).
atm(train,d79,d79_20,c,10,-0.095).
atm(train,d79,d79_21,h,3,0.055).
atm(train,d79,d79_22,h,3,0.055).
atm(train,d79,d79_23,n,36,-0.296).
atm(train,d79,d79_24,h,3,0.055).
atm(train,d79,d79_25,h,3,0.055).
atm(train,d79,d79_26,h,1,0.304).
atm(train,d79,d79_27,h,1,0.154).
atm(train,d79,d79_28,h,1,0.154).
atm(train,d8,d8_1,c,22,-0.17).
atm(train,d8,d8_2,c,22,-0.121).
atm(train,d8,d8_3,c,22,0.21).
atm(train,d8,d8_4,c,22,0.26).
atm(train,d8,d8_5,c,22,-0.121).
atm(train,d8,d8_6,c,22,-0.121).
atm(train,d8,d8_7,h,3,0.139).
atm(train,d8,d8_8,h,3,0.139).
atm(train,d8,d8_9,h,3,0.139).
atm(train,d8,d8_10,c,22,-0.121).
atm(train,d8,d8_11,c,22,-0.121).
atm(train,d8,d8_12,c,22,0.21).
atm(train,d8,d8_13,c,22,-0.121).
atm(train,d8,d8_14,c,22,-0.121).
atm(train,d8,d8_15,c,22,0.21).
atm(train,d8,d8_16,h,3,0.14).
atm(train,d8,d8_17,h,3,0.139).
atm(train,d8,d8_18,h,3,0.139).
atm(train,d8,d8_19,h,3,0.139).
atm(train,d8,d8_20,n,32,-0.39).
atm(train,d8,d8_21,n,32,-0.39).
atm(train,d8,d8_22,c,10,0.009).
atm(train,d8,d8_23,h,3,0.059).
atm(train,d8,d8_24,h,3,0.059).
atm(train,d8,d8_25,h,3,0.059).
atm(train,d8,d8_26,o,45,-0.64).
atm(train,d8,d8_27,h,8,0.41).
atm(train,d8,d8_28,n,32,-0.39).
atm(train,d8,d8_29,c,14,0.61).
atm(train,d8,d8_30,h,1,0.26).
atm(train,d8,d8_31,o,40,-0.54).
atm(train,d8,d8_32,c,10,-0.14).
atm(train,d8,d8_33,h,3,0.059).
atm(train,d8,d8_34,h,3,0.059).
atm(train,d8,d8_35,h,3,0.059).
atm(train,d80,d80_1,c,22,-0.123).
atm(train,d80,d80_2,c,22,-0.123).
atm(train,d80,d80_3,c,27,0.006).
atm(train,d80,d80_4,c,27,0.006).
atm(train,d80,d80_5,c,22,-0.123).
atm(train,d80,d80_6,c,22,-0.123).
atm(train,d80,d80_7,h,3,0.136).
atm(train,d80,d80_8,h,3,0.136).
atm(train,d80,d80_9,h,3,0.137).
atm(train,d80,d80_10,h,3,0.136).
atm(train,d80,d80_11,c,22,-0.123).
atm(train,d80,d80_12,c,22,-0.123).
atm(train,d80,d80_13,c,22,-0.123).
atm(train,d80,d80_14,c,22,-0.123).
atm(train,d80,d80_15,h,3,0.137).
atm(train,d80,d80_16,h,3,0.137).
atm(train,d80,d80_17,h,3,0.137).
atm(train,d80,d80_18,n,38,0.806).
atm(train,d80,d80_19,o,40,-0.395).
atm(train,d80,d80_20,o,40,-0.395).
atm(train,d81,d81_1,c,22,-0.14).
atm(train,d81,d81_2,c,22,0.19).
atm(train,d81,d81_3,c,22,-0.14).
atm(train,d81,d81_4,c,22,-0.14).
atm(train,d81,d81_5,c,22,0.19).
atm(train,d81,d81_6,c,22,-0.14).
atm(train,d81,d81_7,h,3,0.12).
atm(train,d81,d81_8,h,3,0.12).
atm(train,d81,d81_9,h,3,0.12).
atm(train,d81,d81_10,h,3,0.12).
atm(train,d81,d81_11,n,32,-0.79).
atm(train,d81,d81_12,n,32,-0.79).
atm(train,d81,d81_13,h,1,0.32).
atm(train,d81,d81_14,h,1,0.32).
atm(train,d81,d81_15,h,1,0.32).
atm(train,d81,d81_16,h,1,0.32).
atm(train,d82,d82_1,c,22,0.208).
atm(train,d82,d82_2,c,22,-0.122).
atm(train,d82,d82_3,c,22,-0.122).
atm(train,d82,d82_4,c,22,-0.122).
atm(train,d82,d82_5,c,22,-0.122).
atm(train,d82,d82_6,c,22,-0.122).
atm(train,d82,d82_7,h,3,0.138).
atm(train,d82,d82_8,h,3,0.138).
atm(train,d82,d82_9,h,3,0.138).
atm(train,d82,d82_10,h,3,0.138).
atm(train,d82,d82_11,n,32,-0.771).
atm(train,d82,d82_12,c,21,0.008).
atm(train,d82,d82_13,o,52,-0.022).
atm(train,d82,d82_14,n,34,-0.392).
atm(train,d82,d82_15,c,21,0.008).
atm(train,d82,d82_16,c,21,0.008).
atm(train,d82,d82_17,c,10,0.098).
atm(train,d82,d82_18,h,3,0.058).
atm(train,d82,d82_19,h,3,0.058).
atm(train,d82,d82_20,h,3,0.058).
atm(train,d82,d82_21,c,10,0.098).
atm(train,d82,d82_22,h,3,0.058).
atm(train,d82,d82_23,h,3,0.058).
atm(train,d82,d82_24,h,3,0.058).
atm(train,d82,d82_25,h,1,0.338).
atm(train,d82,d82_26,h,1,0.338).
atm(train,d82,d82_27,o,40,-0.242).
atm(train,d82,d82_28,o,40,-0.242).
atm(train,d82,d82_29,n,32,-0.391).
atm(train,d82,d82_30,s,77,0.358).
atm(train,d82,d82_31,h,1,0.308).
atm(train,d83,d83_1,c,22,0.193).
atm(train,d83,d83_2,c,22,-0.137).
atm(train,d83,d83_3,c,22,-0.137).
atm(train,d83,d83_4,c,22,0.193).
atm(train,d83,d83_5,c,22,-0.187).
atm(train,d83,d83_6,c,22,-0.137).
atm(train,d83,d83_7,h,3,0.123).
atm(train,d83,d83_8,h,3,0.123).
atm(train,d83,d83_9,c,10,-0.007).
atm(train,d83,d83_10,h,3,0.043).
atm(train,d83,d83_11,h,3,0.043).
atm(train,d83,d83_12,h,3,0.043).
atm(train,d83,d83_13,n,32,-0.786).
atm(train,d83,d83_14,h,1,0.324).
atm(train,d83,d83_15,h,1,0.323).
atm(train,d83,d83_16,h,3,0.123).
atm(train,d83,d83_17,n,32,-0.786).
atm(train,d83,d83_18,h,1,0.323).
atm(train,d83,d83_19,h,1,0.323).
atm(train,d84,d84a_1,c,10,0.1).
atm(train,d84,d84a_2,c,10,-0.1).
atm(train,d84,d84a_3,c,10,-0.1).
atm(train,d84,d84a_4,h,3,0.05).
atm(train,d84,d84a_5,h,3,0.05).
atm(train,d84,d84a_6,c,10,0.15).
atm(train,d84,d84a_7,h,3,0.05).
atm(train,d84,d84a_8,h,3,0.05).
atm(train,d84,d84a_9,c,10,0.15).
atm(train,d84,d84a_10,c,10,-0.1).
atm(train,d84,d84a_11,c,10,0.15).
atm(train,d84,d84a_12,h,3,0.05).
atm(train,d84,d84a_13,h,3,0.05).
atm(train,d84,d84a_14,c,10,0.4).
atm(train,d84,d84a_15,c,10,0.4).
atm(train,d84,d84a_16,c,10,-0.1).
atm(train,d84,d84a_17,c,10,-0.1).
atm(train,d84,d84a_18,h,3,0.05).
atm(train,d84,d84a_19,h,3,0.05).
atm(train,d84,d84a_20,c,10,-0.15).
atm(train,d84,d84a_21,h,3,0.05).
atm(train,d84,d84a_22,h,3,0.05).
atm(train,d84,d84a_23,cl,93,-0.2).
atm(train,d84,d84a_24,h,3,0.05).
atm(train,d84,d84a_25,h,3,0.05).
atm(train,d84,d84a_26,cl,93,-0.2).
atm(train,d84,d84a_27,h,3,0.05).
atm(train,d84,d84a_28,cl,93,-0.2).
atm(train,d84,d84a_29,h,3,0.05).
atm(train,d84,d84a_30,cl,93,-0.2).
atm(train,d84,d84a_31,cl,93,-0.2).
atm(train,d84,d84a_32,cl,93,-0.2).
atm(train,d84,d84a_33,cl,93,-0.2).
atm(train,d84,d84a_34,cl,93,-0.2).
atm(train,d84,d84a_35,h,3,0.05).
atm(train,d84,d84a_36,h,3,0.05).
atm(train,d84,d84a_37,h,3,0.05).
atm(train,d84,d84a_38,h,3,0.05).
atm(train,d84,d84b_1,c,10,-0.279).
atm(train,d84,d84b_2,c,10,-0.279).
atm(train,d84,d84b_3,c,10,-0.279).
atm(train,d84,d84b_4,c,10,0.421).
atm(train,d84,d84b_5,c,10,0.122).
atm(train,d84,d84b_6,c,10,-0.078).
atm(train,d84,d84b_7,c,10,-0.068).
atm(train,d84,d84b_8,h,3,0.072).
atm(train,d84,d84b_9,h,3,0.072).
atm(train,d84,d84b_10,h,3,0.072).
atm(train,d84,d84b_11,c,10,-0.068).
atm(train,d84,d84b_12,h,3,0.072).
atm(train,d84,d84b_13,h,3,0.072).
atm(train,d84,d84b_14,h,3,0.072).
atm(train,d84,d84b_15,c,10,0.122).
atm(train,d84,d84b_16,c,10,0.521).
atm(train,d84,d84b_17,c,10,0.122).
atm(train,d84,d84b_18,c,10,-0.128).
atm(train,d84,d84b_19,h,3,0.072).
atm(train,d84,d84b_20,h,3,0.072).
atm(train,d84,d84b_21,h,3,0.072).
atm(train,d84,d84b_22,h,3,0.072).
atm(train,d84,d84b_23,h,3,0.072).
atm(train,d84,d84b_24,cl,93,-0.178).
atm(train,d84,d84b_25,cl,93,-0.178).
atm(train,d84,d84b_26,h,3,0.072).
atm(train,d84,d84b_27,cl,93,-0.178).
atm(train,d84,d84b_28,h,3,0.072).
atm(train,d84,d84b_29,h,3,0.072).
atm(train,d84,d84b_30,cl,93,-0.178).
atm(train,d84,d84b_31,cl,93,-0.178).
atm(train,d84,d84b_32,cl,93,-0.178).
atm(train,d84,d84b_33,cl,93,-0.179).
atm(train,d84,d84b_34,h,3,0.072).
atm(train,d84,d84b_35,h,3,0.072).
atm(train,d84,d84b_36,cl,93,-0.178).
atm(train,d84,d84b_37,h,3,0.072).
atm(train,d84,d84b_38,h,3,0.072).
atm(train,d85_1,d85_1_1,c,22,-0.052).
atm(train,d85_1,d85_1_2,c,22,-0.052).
atm(train,d85_1,d85_1_3,c,22,0.259).
atm(train,d85_1,d85_1_4,c,22,0.259).
atm(train,d85_1,d85_1_5,c,22,-0.052).
atm(train,d85_1,d85_1_6,c,22,-0.052).
atm(train,d85_1,d85_1_7,h,3,0.208).
atm(train,d85_1,d85_1_8,c,22,0.259).
atm(train,d85_1,d85_1_9,c,22,-0.052).
atm(train,d85_1,d85_1_10,c,22,-0.052).
atm(train,d85_1,d85_1_11,c,22,-0.052).
atm(train,d85_1,d85_1_12,c,22,-0.052).
atm(train,d85_1,d85_1_13,c,22,0.259).
atm(train,d85_1,d85_1_14,h,3,0.208).
atm(train,d85_1,d85_1_15,o,50,-0.152).
atm(train,d85_1,d85_1_16,o,50,-0.152).
atm(train,d85_1,d85_1_17,cl,93,-0.122).
atm(train,d85_1,d85_1_18,cl,93,-0.122).
atm(train,d85_1,d85_1_19,cl,93,-0.122).
atm(train,d85_1,d85_1_20,cl,93,-0.122).
atm(train,d85_1,d85_1_21,cl,93,-0.122).
atm(train,d85_1,d85_1_22,cl,93,-0.122).
atm(train,d85_2,d85_2_1,c,22,-0.052).
atm(train,d85_2,d85_2_2,c,22,-0.052).
atm(train,d85_2,d85_2_3,c,22,0.259).
atm(train,d85_2,d85_2_4,c,22,0.259).
atm(train,d85_2,d85_2_5,c,22,-0.052).
atm(train,d85_2,d85_2_6,c,22,-0.052).
atm(train,d85_2,d85_2_7,h,3,0.208).
atm(train,d85_2,d85_2_8,c,22,0.259).
atm(train,d85_2,d85_2_9,c,22,-0.052).
atm(train,d85_2,d85_2_10,c,22,-0.052).
atm(train,d85_2,d85_2_11,c,22,-0.052).
atm(train,d85_2,d85_2_12,c,22,-0.052).
atm(train,d85_2,d85_2_13,c,22,0.259).
atm(train,d85_2,d85_2_14,h,3,0.208).
atm(train,d85_2,d85_2_15,o,50,-0.152).
atm(train,d85_2,d85_2_16,o,50,-0.152).
atm(train,d85_2,d85_2_17,cl,93,-0.122).
atm(train,d85_2,d85_2_18,cl,93,-0.122).
atm(train,d85_2,d85_2_19,cl,93,-0.122).
atm(train,d85_2,d85_2_20,cl,93,-0.122).
atm(train,d85_2,d85_2_21,cl,93,-0.122).
atm(train,d85_2,d85_2_22,cl,93,-0.122).
atm(train,d86,d86a_1,c,22,-0.041).
atm(train,d86,d86a_2,c,22,-0.041).
atm(train,d86,d86a_3,c,22,-0.041).
atm(train,d86,d86a_4,c,29,0.089).
atm(train,d86,d86a_5,c,22,-0.041).
atm(train,d86,d86a_6,c,22,-0.041).
atm(train,d86,d86a_7,h,3,0.219).
atm(train,d86,d86a_8,h,3,0.22).
atm(train,d86,d86a_9,c,29,0.089).
atm(train,d86,d86a_10,c,22,-0.041).
atm(train,d86,d86a_11,c,22,-0.041).
atm(train,d86,d86a_12,c,22,-0.041).
atm(train,d86,d86a_13,c,22,-0.041).
atm(train,d86,d86a_14,c,22,-0.041).
atm(train,d86,d86a_15,h,3,0.22).
atm(train,d86,d86a_16,br,94,-0.061).
atm(train,d86,d86a_17,br,94,-0.061).
atm(train,d86,d86a_18,br,94,-0.061).
atm(train,d86,d86a_19,br,94,-0.061).
atm(train,d86,d86a_20,br,94,-0.061).
atm(train,d86,d86a_21,br,94,-0.061).
atm(train,d86,d86a_22,br,94,-0.061).
atm(train,d86,d86b_1,c,22,-0.066).
atm(train,d86,d86b_2,c,22,-0.066).
atm(train,d86,d86b_3,c,22,-0.066).
atm(train,d86,d86b_4,c,29,0.064).
atm(train,d86,d86b_5,c,22,-0.066).
atm(train,d86,d86b_6,c,22,-0.066).
atm(train,d86,d86b_7,h,3,0.193).
atm(train,d86,d86b_8,c,29,0.064).
atm(train,d86,d86b_9,c,22,-0.066).
atm(train,d86,d86b_10,c,22,-0.066).
atm(train,d86,d86b_11,c,22,-0.066).
atm(train,d86,d86b_12,c,22,-0.066).
atm(train,d86,d86b_13,c,22,-0.066).
atm(train,d86,d86b_14,h,3,0.193).
atm(train,d86,d86b_15,br,94,-0.087).
atm(train,d86,d86b_16,br,94,-0.087).
atm(train,d86,d86b_17,h,3,0.193).
atm(train,d86,d86b_18,br,94,-0.086).
atm(train,d86,d86b_19,br,94,-0.087).
atm(train,d86,d86b_20,h,3,0.193).
atm(train,d86,d86b_21,h,3,0.193).
atm(train,d86,d86b_22,br,94,-0.086).
atm(train,d87,d87_1,c,22,-0.082).
atm(train,d87,d87_2,c,22,-0.082).
atm(train,d87,d87_3,c,22,0.229).
atm(train,d87,d87_4,c,22,0.229).
atm(train,d87,d87_5,c,22,-0.082).
atm(train,d87,d87_6,c,22,-0.082).
atm(train,d87,d87_7,h,3,0.178).
atm(train,d87,d87_8,h,3,0.178).
atm(train,d87,d87_9,c,22,0.229).
atm(train,d87,d87_10,c,22,-0.082).
atm(train,d87,d87_11,c,22,-0.082).
atm(train,d87,d87_12,c,22,-0.082).
atm(train,d87,d87_13,c,22,-0.082).
atm(train,d87,d87_14,c,22,0.229).
atm(train,d87,d87_15,h,3,0.178).
atm(train,d87,d87_16,h,3,0.178).
atm(train,d87,d87_17,o,50,-0.182).
atm(train,d87,d87_18,o,50,-0.182).
atm(train,d87,d87_19,cl,93,-0.152).
atm(train,d87,d87_20,cl,93,-0.152).
atm(train,d87,d87_21,cl,93,-0.152).
atm(train,d87,d87_22,cl,93,-0.152).
atm(train,d88,d88_1,c,22,-0.054).
atm(train,d88,d88_2,c,22,-0.054).
atm(train,d88,d88_3,c,22,-0.054).
atm(train,d88,d88_4,c,22,-0.054).
atm(train,d88,d88_5,c,22,0.326).
atm(train,d88,d88_6,c,22,-0.054).
atm(train,d88,d88_7,h,3,0.206).
atm(train,d88,d88_8,h,3,0.206).
atm(train,d88,d88_9,o,45,-0.573).
atm(train,d88,d88_10,cl,93,-0.124).
atm(train,d88,d88_11,cl,93,-0.124).
atm(train,d88,d88_12,cl,93,-0.124).
atm(train,d88,d88_13,h,8,0.477).
atm(train,d89,d89_1,c,16,-0.155).
atm(train,d89,d89_2,c,16,-0.155).
atm(train,d89,d89_3,c,10,0.145).
atm(train,d89,d89_4,c,10,-0.055).
atm(train,d89,d89_5,c,10,0.145).
atm(train,d89,d89_6,c,10,-0.055).
atm(train,d89,d89_7,c,10,0.445).
atm(train,d89,d89_8,h,3,0.095).
atm(train,d89,d89_9,h,3,0.095).
atm(train,d89,d89_10,cl,93,-0.155).
atm(train,d89,d89_11,cl,93,-0.155).
atm(train,d89,d89_12,cl,93,-0.155).
atm(train,d89,d89_13,cl,93,-0.155).
atm(train,d89,d89_14,c,14,0.826).
atm(train,d89,d89_15,h,3,0.125).
atm(train,d89,d89_16,c,14,0.826).
atm(train,d89,d89_17,h,3,0.125).
atm(train,d89,d89_18,o,45,-0.624).
atm(train,d89,d89_19,o,51,-0.564).
atm(train,d89,d89_20,o,51,-0.565).
atm(train,d89,d89_21,h,1,0.295).
atm(train,d89,d89_22,o,45,-0.624).
atm(train,d89,d89_23,h,1,0.295).
atm(train,d9,d9_1,c,22,-0.192).
atm(train,d9,d9_2,c,22,-0.142).
atm(train,d9,d9_3,c,22,-0.141).
atm(train,d9,d9_4,c,22,0.168).
atm(train,d9,d9_5,c,22,0.188).
atm(train,d9,d9_6,c,22,-0.142).
atm(train,d9,d9_7,h,3,0.119).
atm(train,d9,d9_8,h,3,0.119).
atm(train,d9,d9_9,h,3,0.119).
atm(train,d9,d9_10,c,10,-0.011).
atm(train,d9,d9_11,h,3,0.039).
atm(train,d9,d9_12,h,3,0.039).
atm(train,d9,d9_13,h,3,0.039).
atm(train,d9,d9_14,n,32,-0.792).
atm(train,d9,d9_15,o,50,-0.242).
atm(train,d9,d9_16,h,1,0.318).
atm(train,d9,d9_17,h,1,0.318).
atm(train,d9,d9_18,c,10,0.079).
atm(train,d9,d9_19,h,3,0.039).
atm(train,d9,d9_20,h,3,0.039).
atm(train,d9,d9_21,h,3,0.039).
atm(train,d90,d90_1,c,22,-0.075).
atm(train,d90,d90_2,c,22,-0.075).
atm(train,d90,d90_3,c,22,-0.075).
atm(train,d90,d90_4,c,22,-0.075).
atm(train,d90,d90_5,c,22,-0.075).
atm(train,d90,d90_6,c,22,-0.075).
atm(train,d90,d90_7,h,3,0.185).
atm(train,d90,d90_8,h,3,0.185).
atm(train,d90,d90_9,h,3,0.185).
atm(train,d90,d90_10,h,3,0.185).
atm(train,d90,d90_11,cl,93,-0.145).
atm(train,d90,d90_12,cl,93,-0.145).
atm(train,d91,d91_1,cl,93,-0.2).
atm(train,d91,d91_2,c,16,0.4).
atm(train,d91,d91_3,cl,93,-0.2).
atm(train,d91,d91_4,c,16,0.4).
atm(train,d91,d91_5,cl,93,-0.2).
atm(train,d91,d91_6,cl,93,-0.2).
atm(train,d92,d92_1,c,10,0.525).
atm(train,d92,d92_2,c,10,0.525).
atm(train,d92,d92_3,cl,93,-0.175).
atm(train,d92,d92_4,cl,93,-0.175).
atm(train,d92,d92_5,cl,93,-0.175).
atm(train,d92,d92_6,cl,93,-0.175).
atm(train,d92,d92_7,cl,93,-0.175).
atm(train,d92,d92_8,cl,93,-0.175).
atm(train,d93,d93_1,c,22,-0.003).
atm(train,d93,d93_2,c,22,-0.003).
atm(train,d93,d93_3,c,22,-0.003).
atm(train,d93,d93_4,c,22,-0.003).
atm(train,d93,d93_5,c,22,0.377).
atm(train,d93,d93_6,c,22,-0.003).
atm(train,d93,d93_7,o,45,-0.523).
atm(train,d93,d93_8,cl,93,-0.073).
atm(train,d93,d93_9,cl,93,-0.073).
atm(train,d93,d93_10,cl,93,-0.073).
atm(train,d93,d93_11,cl,93,-0.073).
atm(train,d93,d93_12,cl,93,-0.073).
atm(train,d93,d93_13,h,8,0.526).
atm(train,d94,d94_1,c,10,0.094).
atm(train,d94,d94_2,c,10,0.393).
atm(train,d94,d94_3,cl,93,-0.206).
atm(train,d94,d94_4,cl,93,-0.207).
atm(train,d94,d94_5,h,3,0.044).
atm(train,d94,d94_6,cl,93,-0.206).
atm(train,d94,d94_7,h,3,0.044).
atm(train,d94,d94_8,h,3,0.044).
atm(train,d95,d95_1,c,22,-0.118).
atm(train,d95,d95_2,c,22,-0.118).
atm(train,d95,d95_3,c,22,-0.118).
atm(train,d95,d95_4,c,22,-0.118).
atm(train,d95,d95_5,c,22,0.211).
atm(train,d95,d95_6,c,22,-0.118).
atm(train,d95,d95_7,h,3,0.142).
atm(train,d95,d95_8,h,3,0.142).
atm(train,d95,d95_9,h,3,0.141).
atm(train,d95,d95_10,h,3,0.142).
atm(train,d95,d95_11,cl,93,-0.189).
atm(train,d95,d95_12,n,32,-0.389).
atm(train,d95,d95_13,c,14,0.611).
atm(train,d95,d95_14,n,32,-0.389).
atm(train,d95,d95_15,c,10,0.012).
atm(train,d95,d95_16,h,3,0.062).
atm(train,d95,d95_17,h,3,0.062).
atm(train,d95,d95_18,h,3,0.062).
atm(train,d95,d95_19,c,10,0.012).
atm(train,d95,d95_20,h,3,0.062).
atm(train,d95,d95_21,h,3,0.062).
atm(train,d95,d95_22,h,3,0.062).
atm(train,d95,d95_23,o,40,-0.539).
atm(train,d95,d95_24,h,1,0.311).
atm(train,d96,d96_1,c,16,-0.127).
atm(train,d96,d96_2,c,16,-0.126).
atm(train,d96,d96_3,c,10,-0.027).
atm(train,d96,d96_4,c,10,-0.077).
atm(train,d96,d96_5,c,10,-0.077).
atm(train,d96,d96_6,c,10,-0.027).
atm(train,d96,d96_7,c,10,0.224).
atm(train,d96,d96_8,c,10,0.224).
atm(train,d96,d96_9,c,10,-0.027).
atm(train,d96,d96_10,h,3,0.043).
atm(train,d96,d96_11,h,3,0.043).
atm(train,d96,d96_12,h,3,0.123).
atm(train,d96,d96_13,h,3,0.123).
atm(train,d96,d96_14,c,10,0.474).
atm(train,d96,d96_15,cl,93,-0.127).
atm(train,d96,d96_16,cl,93,-0.127).
atm(train,d96,d96_17,cl,93,-0.127).
atm(train,d96,d96_18,h,3,0.123).
atm(train,d96,d96_19,cl,93,-0.127).
atm(train,d96,d96_20,h,3,0.123).
atm(train,d96,d96_21,cl,93,-0.126).
atm(train,d96,d96_22,cl,93,-0.126).
atm(train,d96,d96_23,cl,93,-0.126).
atm(train,d96,d96_24,cl,93,-0.126).
atm(train,d97,d97_1,c,22,-0.097).
atm(train,d97,d97_2,c,22,-0.097).
atm(train,d97,d97_3,c,22,-0.097).
atm(train,d97,d97_4,c,22,-0.097).
atm(train,d97,d97_5,c,22,-0.097).
atm(train,d97,d97_6,c,22,-0.097).
atm(train,d97,d97_7,h,3,0.164).
atm(train,d97,d97_8,h,3,0.164).
atm(train,d97,d97_9,h,3,0.164).
atm(train,d97,d97_10,h,3,0.164).
atm(train,d97,d97_11,c,22,-0.097).
atm(train,d97,d97_12,c,22,-0.097).
atm(train,d97,d97_13,c,22,-0.097).
atm(train,d97,d97_14,c,22,-0.097).
atm(train,d97,d97_15,c,22,-0.097).
atm(train,d97,d97_16,c,22,-0.097).
atm(train,d97,d97_17,h,3,0.163).
atm(train,d97,d97_18,h,3,0.164).
atm(train,d97,d97_19,h,3,0.164).
atm(train,d97,d97_20,h,3,0.164).
atm(train,d97,d97_21,c,10,0.033).
atm(train,d97,d97_22,o,45,-0.616).
atm(train,d97,d97_23,c,14,0.734).
atm(train,d97,d97_24,o,49,-0.616).
atm(train,d97,d97_25,o,51,-0.516).
atm(train,d97,d97_26,c,10,0.284).
atm(train,d97,d97_27,c,10,-0.067).
atm(train,d97,d97_28,h,3,0.133).
atm(train,d97,d97_29,h,3,0.133).
atm(train,d97,d97_30,h,3,0.083).
atm(train,d97,d97_31,h,3,0.083).
atm(train,d97,d97_32,h,3,0.083).
atm(train,d97,d97_33,h,8,0.434).
atm(train,d97,d97_34,cl,93,-0.166).
atm(train,d97,d97_35,cl,93,-0.166).
atm(train,d98,d98_1,c,22,-0.007).
atm(train,d98,d98_2,c,22,-0.007).
atm(train,d98,d98_3,c,22,-0.007).
atm(train,d98,d98_4,c,22,-0.007).
atm(train,d98,d98_5,c,22,-0.007).
atm(train,d98,d98_6,c,22,-0.007).
atm(train,d98,d98_7,cl,93,-0.077).
atm(train,d98,d98_8,c,19,0.552).
atm(train,d98,d98_9,cl,93,-0.077).
atm(train,d98,d98_10,c,19,0.552).
atm(train,d98,d98_11,cl,93,-0.077).
atm(train,d98,d98_12,cl,93,-0.077).
atm(train,d98,d98_13,n,31,-0.377).
atm(train,d98,d98_14,n,31,-0.377).
atm(train,d99,d99_1,c,22,-0.014).
atm(train,d99,d99_2,c,22,-0.014).
atm(train,d99,d99_3,c,22,-0.014).
atm(train,d99,d99_4,c,22,0.297).
atm(train,d99,d99_5,c,22,-0.014).
atm(train,d99,d99_6,c,22,-0.014).
atm(train,d99,d99_7,c,22,0.297).
atm(train,d99,d99_8,c,22,-0.014).
atm(train,d99,d99_9,c,22,-0.014).
atm(train,d99,d99_10,c,22,-0.014).
atm(train,d99,d99_11,c,22,-0.014).
atm(train,d99,d99_12,c,22,-0.014).
atm(train,d99,d99_13,o,50,-0.114).
atm(train,d99,d99_14,br,94,-0.034).
atm(train,d99,d99_15,br,94,-0.034).
atm(train,d99,d99_16,br,94,-0.034).
atm(train,d99,d99_17,br,94,-0.034).
atm(train,d99,d99_18,br,94,-0.034).
atm(train,d99,d99_19,br,94,-0.034).
atm(train,d99,d99_20,br,94,-0.034).
atm(train,d99,d99_21,br,94,-0.034).
atm(train,d99,d99_22,br,94,-0.034).
atm(train,d99,d99_23,br,94,-0.034).
bond(train,d1,d1_1,d1_2,7).
bond(train,d1,d1_2,d1_3,7).
bond(train,d1,d1_3,d1_4,7).
bond(train,d1,d1_4,d1_5,7).
bond(train,d1,d1_5,d1_6,7).
bond(train,d1,d1_6,d1_1,7).
bond(train,d1,d1_1,d1_7,1).
bond(train,d1,d1_2,d1_8,1).
bond(train,d1,d1_5,d1_9,1).
bond(train,d1,d1_6,d1_10,1).
bond(train,d1,d1_3,d1_11,1).
bond(train,d1,d1_11,d1_12,1).
bond(train,d1,d1_12,d1_13,7).
bond(train,d1,d1_13,d1_14,1).
bond(train,d1,d1_14,d1_4,1).
bond(train,d1,d1_12,d1_15,7).
bond(train,d1,d1_15,d1_16,7).
bond(train,d1,d1_16,d1_17,7).
bond(train,d1,d1_17,d1_18,7).
bond(train,d1,d1_18,d1_13,7).
bond(train,d1,d1_15,d1_19,1).
bond(train,d1,d1_16,d1_20,1).
bond(train,d1,d1_18,d1_21,1).
bond(train,d1,d1_14,d1_22,2).
bond(train,d1,d1_11,d1_23,2).
bond(train,d1,d1_17,d1_24,1).
bond(train,d1,d1_24,d1_25,1).
bond(train,d1,d1_24,d1_26,1).
bond(train,d10,d10_1,d10_2,7).
bond(train,d10,d10_2,d10_3,7).
bond(train,d10,d10_3,d10_4,7).
bond(train,d10,d10_4,d10_5,7).
bond(train,d10,d10_5,d10_6,7).
bond(train,d10,d10_6,d10_1,7).
bond(train,d10,d10_1,d10_7,1).
bond(train,d10,d10_2,d10_8,1).
bond(train,d10,d10_3,d10_9,1).
bond(train,d10,d10_4,d10_10,1).
bond(train,d10,d10_6,d10_11,1).
bond(train,d10,d10_5,d10_12,1).
bond(train,d10,d10_12,d10_13,1).
bond(train,d10,d10_12,d10_14,1).
bond(train,d10,d10_14,d10_15,1).
bond(train,d10,d10_13,d10_16,2).
bond(train,d100,d100_1,d100_2,7).
bond(train,d100,d100_2,d100_3,7).
bond(train,d100,d100_3,d100_4,7).
bond(train,d100,d100_4,d100_5,7).
bond(train,d100,d100_5,d100_6,7).
bond(train,d100,d100_6,d100_1,7).
bond(train,d100,d100_2,d100_7,1).
bond(train,d100,d100_3,d100_8,1).
bond(train,d100,d100_5,d100_9,1).
bond(train,d100,d100_6,d100_10,1).
bond(train,d100,d100_11,d100_12,7).
bond(train,d100,d100_12,d100_13,7).
bond(train,d100,d100_13,d100_14,7).
bond(train,d100,d100_14,d100_15,7).
bond(train,d100,d100_15,d100_16,7).
bond(train,d100,d100_16,d100_11,7).
bond(train,d100,d100_11,d100_17,1).
bond(train,d100,d100_12,d100_18,1).
bond(train,d100,d100_14,d100_19,1).
bond(train,d100,d100_15,d100_20,1).
bond(train,d100,d100_4,d100_21,1).
bond(train,d100,d100_21,d100_16,1).
bond(train,d100,d100_21,d100_22,2).
bond(train,d100,d100_22,d100_23,1).
bond(train,d100,d100_22,d100_24,1).
bond(train,d100,d100_1,d100_25,1).
bond(train,d100,d100_13,d100_26,1).
bond(train,d101,d101_1,d101_2,1).
bond(train,d101,d101_2,d101_3,1).
bond(train,d101,d101_3,d101_4,2).
bond(train,d101,d101_4,d101_5,1).
bond(train,d101,d101_5,d101_6,1).
bond(train,d101,d101_6,d101_1,1).
bond(train,d101,d101_6,d101_7,1).
bond(train,d101,d101_7,d101_8,1).
bond(train,d101,d101_8,d101_9,2).
bond(train,d101,d101_9,d101_1,1).
bond(train,d101,d101_1,d101_10,1).
bond(train,d101,d101_6,d101_11,1).
bond(train,d101,d101_8,d101_12,1).
bond(train,d101,d101_9,d101_13,1).
bond(train,d101,d101_5,d101_14,1).
bond(train,d101,d101_14,d101_2,1).
bond(train,d101,d101_14,d101_15,1).
bond(train,d101,d101_14,d101_16,1).
bond(train,d101,d101_7,d101_17,1).
bond(train,d101,d101_7,d101_18,1).
bond(train,d101,d101_5,d101_19,1).
bond(train,d101,d101_4,d101_20,1).
bond(train,d101,d101_3,d101_21,1).
bond(train,d101,d101_2,d101_22,1).
bond(train,d102,d102_1,d102_2,1).
bond(train,d102,d102_1,d102_3,1).
bond(train,d102,d102_1,d102_4,1).
bond(train,d102,d102_1,d102_5,1).
bond(train,d102,d102_2,d102_6,1).
bond(train,d102,d102_2,d102_7,1).
bond(train,d102,d102_2,d102_8,1).
bond(train,d103,d103_1,d103_2,1).
bond(train,d103,d103_2,d103_3,1).
bond(train,d103,d103_2,d103_4,1).
bond(train,d103,d103_2,d103_5,1).
bond(train,d103,d103_1,d103_6,1).
bond(train,d103,d103_1,d103_7,1).
bond(train,d103,d103_1,d103_8,1).
bond(train,d104,d104_1,d104_2,1).
bond(train,d104,d104_1,d104_3,1).
bond(train,d104,d104_1,d104_4,1).
bond(train,d104,d104_1,d104_5,1).
bond(train,d104,d104_2,d104_6,1).
bond(train,d104,d104_2,d104_7,1).
bond(train,d104,d104_2,d104_8,1).
bond(train,d105,d105_1,d105_2,2).
bond(train,d105,d105_1,d105_3,1).
bond(train,d105,d105_1,d105_4,1).
bond(train,d105,d105_2,d105_5,1).
bond(train,d105,d105_2,d105_6,1).
bond(train,d106,d106_1,d106_2,1).
bond(train,d106,d106_2,d106_3,1).
bond(train,d106,d106_3,d106_4,2).
bond(train,d106,d106_4,d106_5,1).
bond(train,d106,d106_5,d106_6,1).
bond(train,d106,d106_6,d106_1,1).
bond(train,d106,d106_6,d106_7,1).
bond(train,d106,d106_7,d106_8,1).
bond(train,d106,d106_8,d106_9,2).
bond(train,d106,d106_9,d106_10,1).
bond(train,d106,d106_10,d106_1,1).
bond(train,d106,d106_1,d106_11,1).
bond(train,d106,d106_6,d106_12,1).
bond(train,d106,d106_8,d106_13,1).
bond(train,d106,d106_9,d106_14,1).
bond(train,d106,d106_5,d106_15,1).
bond(train,d106,d106_15,d106_2,1).
bond(train,d106,d106_7,d106_16,1).
bond(train,d106,d106_7,d106_17,1).
bond(train,d106,d106_16,d106_10,1).
bond(train,d106,d106_10,d106_18,1).
bond(train,d106,d106_15,d106_19,1).
bond(train,d106,d106_15,d106_20,1).
bond(train,d106,d106_5,d106_21,1).
bond(train,d106,d106_2,d106_22,1).
bond(train,d106,d106_4,d106_23,1).
bond(train,d106,d106_3,d106_24,1).
bond(train,d106,d106_16,d106_25,1).
bond(train,d106,d106_16,d106_26,1).
bond(train,d107,d107c_1,d107c_2,1).
bond(train,d107,d107c_2,d107c_3,1).
bond(train,d107,d107c_2,d107c_4,1).
bond(train,d107,d107c_2,d107c_5,1).
bond(train,d107,d107c_3,d107c_6,1).
bond(train,d107,d107c_3,d107c_7,1).
bond(train,d107,d107c_3,d107c_8,1).
bond(train,d107,d107c_6,d107c_9,1).
bond(train,d107,d107c_9,d107c_10,1).
bond(train,d107,d107c_9,d107c_11,1).
bond(train,d107,d107c_9,d107c_12,1).
bond(train,d107,d107c_10,d107c_13,1).
bond(train,d107,d107c_10,d107c_14,1).
bond(train,d107,d107c_10,d107c_15,1).
bond(train,d107,d107c_13,d107c_16,1).
bond(train,d107,d107c_13,d107c_17,1).
bond(train,d107,d107c_13,d107c_18,1).
bond(train,d107,d107c_16,d107c_19,1).
bond(train,d107,d107c_19,d107c_20,1).
bond(train,d107,d107c_20,d107c_21,1).
bond(train,d107,d107c_20,d107c_22,1).
bond(train,d107,d107c_20,d107c_23,1).
bond(train,d107,d107c_21,d107c_24,1).
bond(train,d107,d107c_24,d107c_25,1).
bond(train,d107,d107c_24,d107c_26,1).
bond(train,d107,d107c_24,d107c_27,1).
bond(train,d107,d107c_25,d107c_28,1).
bond(train,d107,d107c_25,d107c_29,1).
bond(train,d107,d107c_25,d107c_30,1).
bond(train,d107,d107c_28,d107c_31,1).
bond(train,d107,d107c_31,d107c_32,1).
bond(train,d107,d107c_31,d107c_33,1).
bond(train,d107,d107c_31,d107c_34,1).
bond(train,d107,d107c_32,d107c_35,1).
bond(train,d107,d107c_35,d107c_36,1).
bond(train,d107,d107c_35,d107c_37,1).
bond(train,d107,d107c_35,d107c_38,1).
bond(train,d107,d107c_36,d107c_39,1).
bond(train,d107,d107c_36,d107c_40,1).
bond(train,d107,d107c_36,d107c_41,1).
bond(train,d107,d107c_39,d107c_42,1).
bond(train,d107,d107c_42,d107c_43,1).
bond(train,d107,d107c_43,d107c_44,1).
bond(train,d107,d107c_43,d107c_45,1).
bond(train,d107,d107c_43,d107c_46,1).
bond(train,d107,d107c_44,d107c_47,1).
bond(train,d107,d107c_44,d107c_48,1).
bond(train,d107,d107c_44,d107c_49,1).
bond(train,d107,d107c_47,d107c_50,1).
bond(train,d107,d107c_47,d107c_51,1).
bond(train,d107,d107c_47,d107c_52,1).
bond(train,d107,d107c_1,d107c_53,1).
bond(train,d107,d107c_1,d107c_54,1).
bond(train,d107,d107c_1,d107c_55,1).
bond(train,d107,d107c_6,d107c_56,1).
bond(train,d107,d107c_6,d107c_57,1).
bond(train,d107,d107c_16,d107c_58,1).
bond(train,d107,d107c_16,d107c_59,1).
bond(train,d107,d107c_19,d107c_60,1).
bond(train,d107,d107c_19,d107c_61,1).
bond(train,d107,d107c_21,d107c_62,1).
bond(train,d107,d107c_21,d107c_63,1).
bond(train,d107,d107c_28,d107c_64,1).
bond(train,d107,d107c_28,d107c_65,1).
bond(train,d107,d107c_32,d107c_66,1).
bond(train,d107,d107c_32,d107c_67,1).
bond(train,d107,d107c_39,d107c_68,1).
bond(train,d107,d107c_39,d107c_69,1).
bond(train,d107,d107c_42,d107c_70,1).
bond(train,d107,d107c_42,d107c_71,1).
bond(train,d107,d107d_1,d107d_2,1).
bond(train,d107,d107d_2,d107d_3,1).
bond(train,d107,d107d_2,d107d_4,1).
bond(train,d107,d107d_2,d107d_5,1).
bond(train,d107,d107d_3,d107d_6,1).
bond(train,d107,d107d_3,d107d_7,1).
bond(train,d107,d107d_3,d107d_8,1).
bond(train,d107,d107d_5,d107d_9,1).
bond(train,d107,d107d_9,d107d_10,1).
bond(train,d107,d107d_9,d107d_11,1).
bond(train,d107,d107d_9,d107d_12,1).
bond(train,d107,d107d_5,d107d_13,1).
bond(train,d107,d107d_13,d107d_14,1).
bond(train,d107,d107d_13,d107d_15,1).
bond(train,d107,d107d_13,d107d_16,1).
bond(train,d107,d107d_4,d107d_17,1).
bond(train,d107,d107d_4,d107d_18,1).
bond(train,d107,d107d_18,d107d_19,1).
bond(train,d107,d107d_18,d107d_20,1).
bond(train,d107,d107d_18,d107d_21,1).
bond(train,d107,d107d_1,d107d_22,1).
bond(train,d107,d107d_22,d107d_23,1).
bond(train,d107,d107d_22,d107d_24,1).
bond(train,d107,d107d_22,d107d_25,1).
bond(train,d107,d107d_1,d107d_26,1).
bond(train,d107,d107d_26,d107d_27,1).
bond(train,d107,d107d_26,d107d_28,1).
bond(train,d107,d107d_26,d107d_29,1).
bond(train,d107,d107d_5,d107d_30,1).
bond(train,d107,d107d_30,d107d_31,1).
bond(train,d107,d107d_30,d107d_32,1).
bond(train,d107,d107d_30,d107d_33,1).
bond(train,d107,d107d_4,d107d_34,1).
bond(train,d107,d107d_1,d107d_35,1).
bond(train,d107,d107d_35,d107d_36,1).
bond(train,d107,d107d_35,d107d_37,1).
bond(train,d107,d107d_35,d107d_38,1).
bond(train,d107,d107d_7,d107d_39,1).
bond(train,d107,d107d_39,d107d_40,1).
bond(train,d107,d107d_39,d107d_41,1).
bond(train,d107,d107d_39,d107d_42,1).
bond(train,d107,d107d_7,d107d_43,1).
bond(train,d107,d107d_7,d107d_44,1).
bond(train,d107,d107d_8,d107d_45,1).
bond(train,d107,d107d_8,d107d_46,1).
bond(train,d107,d107d_8,d107d_47,1).
bond(train,d107,d107d_46,d107d_48,1).
bond(train,d107,d107d_46,d107d_49,1).
bond(train,d107,d107d_46,d107d_50,1).
bond(train,d107,d107d_6,d107d_51,1).
bond(train,d107,d107d_6,d107d_52,1).
bond(train,d107,d107d_6,d107d_53,1).
bond(train,d107,d107d_17,d107d_54,1).
bond(train,d107,d107d_17,d107d_55,1).
bond(train,d107,d107d_17,d107d_56,1).
bond(train,d107,d107d_34,d107d_57,1).
bond(train,d107,d107d_34,d107d_58,1).
bond(train,d107,d107d_34,d107d_59,1).
bond(train,d107,d107d_43,d107d_60,1).
bond(train,d107,d107d_43,d107d_61,1).
bond(train,d107,d107d_43,d107d_62,1).
bond(train,d107,d107d_45,d107d_63,1).
bond(train,d107,d107d_45,d107d_64,1).
bond(train,d107,d107d_45,d107d_65,1).
bond(train,d107,d107d_44,d107d_66,1).
bond(train,d107,d107d_44,d107d_67,1).
bond(train,d107,d107d_44,d107d_68,1).
bond(train,d107,d107d_51,d107d_69,1).
bond(train,d107,d107d_51,d107d_70,1).
bond(train,d107,d107d_51,d107d_71,1).
bond(train,d108,d108_1,d108_2,7).
bond(train,d108,d108_2,d108_3,7).
bond(train,d108,d108_3,d108_4,7).
bond(train,d108,d108_4,d108_5,7).
bond(train,d108,d108_5,d108_6,7).
bond(train,d108,d108_6,d108_1,7).
bond(train,d108,d108_2,d108_7,1).
bond(train,d108,d108_3,d108_8,1).
bond(train,d108,d108_5,d108_9,1).
bond(train,d108,d108_6,d108_10,1).
bond(train,d108,d108_11,d108_12,7).
bond(train,d108,d108_12,d108_13,7).
bond(train,d108,d108_13,d108_14,7).
bond(train,d108,d108_14,d108_15,7).
bond(train,d108,d108_15,d108_16,7).
bond(train,d108,d108_16,d108_11,7).
bond(train,d108,d108_11,d108_17,1).
bond(train,d108,d108_12,d108_18,1).
bond(train,d108,d108_14,d108_19,1).
bond(train,d108,d108_15,d108_20,1).
bond(train,d108,d108_4,d108_21,1).
bond(train,d108,d108_21,d108_16,1).
bond(train,d108,d108_21,d108_22,1).
bond(train,d108,d108_21,d108_23,1).
bond(train,d108,d108_23,d108_24,1).
bond(train,d108,d108_23,d108_25,1).
bond(train,d108,d108_23,d108_26,1).
bond(train,d108,d108_1,d108_27,1).
bond(train,d108,d108_13,d108_28,1).
bond(train,d108,d108_22,d108_29,1).
bond(train,d109,d109_1,d109_2,7).
bond(train,d109,d109_2,d109_3,7).
bond(train,d109,d109_3,d109_4,7).
bond(train,d109,d109_4,d109_5,7).
bond(train,d109,d109_5,d109_1,7).
bond(train,d109,d109_6,d109_7,7).
bond(train,d109,d109_7,d109_8,7).
bond(train,d109,d109_8,d109_9,7).
bond(train,d109,d109_9,d109_10,7).
bond(train,d109,d109_10,d109_11,7).
bond(train,d109,d109_11,d109_6,7).
bond(train,d109,d109_10,d109_12,1).
bond(train,d109,d109_12,d109_13,1).
bond(train,d109,d109_7,d109_14,1).
bond(train,d109,d109_6,d109_15,1).
bond(train,d109,d109_15,d109_16,1).
bond(train,d109,d109_12,d109_17,2).
bond(train,d109,d109_15,d109_18,2).
bond(train,d109,d109_15,d109_19,2).
bond(train,d109,d109_9,d109_20,1).
bond(train,d109,d109_20,d109_21,1).
bond(train,d109,d109_21,d109_1,1).
bond(train,d109,d109_8,d109_22,1).
bond(train,d109,d109_11,d109_23,1).
bond(train,d109,d109_13,d109_24,1).
bond(train,d109,d109_16,d109_25,1).
bond(train,d109,d109_16,d109_26,1).
bond(train,d109,d109_20,d109_27,1).
bond(train,d109,d109_21,d109_28,1).
bond(train,d109,d109_21,d109_29,1).
bond(train,d109,d109_2,d109_30,1).
bond(train,d109,d109_3,d109_31,1).
bond(train,d109,d109_4,d109_32,1).
bond(train,d11,d11_1,d11_2,7).
bond(train,d11,d11_2,d11_3,7).
bond(train,d11,d11_3,d11_4,7).
bond(train,d11,d11_4,d11_5,7).
bond(train,d11,d11_5,d11_6,7).
bond(train,d11,d11_6,d11_1,7).
bond(train,d11,d11_1,d11_7,1).
bond(train,d11,d11_4,d11_8,1).
bond(train,d11,d11_6,d11_9,1).
bond(train,d11,d11_5,d11_10,1).
bond(train,d11,d11_3,d11_11,1).
bond(train,d11,d11_2,d11_12,1).
bond(train,d11,d11_12,d11_13,1).
bond(train,d11,d11_13,d11_14,1).
bond(train,d11,d11_13,d11_15,1).
bond(train,d11,d11_13,d11_16,1).
bond(train,d11,d11_10,d11_17,1).
bond(train,d11,d11_10,d11_18,1).
bond(train,d11,d11_11,d11_19,1).
bond(train,d11,d11_11,d11_20,1).
bond(train,d110,d110_1,d110_2,7).
bond(train,d110,d110_2,d110_3,7).
bond(train,d110,d110_3,d110_4,7).
bond(train,d110,d110_4,d110_5,7).
bond(train,d110,d110_5,d110_6,7).
bond(train,d110,d110_6,d110_1,7).
bond(train,d110,d110_1,d110_7,1).
bond(train,d110,d110_2,d110_8,1).
bond(train,d110,d110_3,d110_9,1).
bond(train,d110,d110_4,d110_10,1).
bond(train,d110,d110_6,d110_11,1).
bond(train,d110,d110_5,d110_12,1).
bond(train,d111,d111_1,d111_2,7).
bond(train,d111,d111_2,d111_3,7).
bond(train,d111,d111_3,d111_4,7).
bond(train,d111,d111_4,d111_5,7).
bond(train,d111,d111_5,d111_6,7).
bond(train,d111,d111_6,d111_1,7).
bond(train,d111,d111_2,d111_7,1).
bond(train,d111,d111_5,d111_8,1).
bond(train,d111,d111_6,d111_9,1).
bond(train,d111,d111_10,d111_11,7).
bond(train,d111,d111_11,d111_12,7).
bond(train,d111,d111_12,d111_13,7).
bond(train,d111,d111_13,d111_14,7).
bond(train,d111,d111_14,d111_15,7).
bond(train,d111,d111_15,d111_10,7).
bond(train,d111,d111_11,d111_16,1).
bond(train,d111,d111_12,d111_17,1).
bond(train,d111,d111_14,d111_18,1).
bond(train,d111,d111_4,d111_19,1).
bond(train,d111,d111_19,d111_15,1).
bond(train,d111,d111_3,d111_20,1).
bond(train,d111,d111_20,d111_10,1).
bond(train,d111,d111_1,d111_21,1).
bond(train,d111,d111_13,d111_22,1).
bond(train,d112,d112_1,d112_2,1).
bond(train,d112,d112_2,d112_3,1).
bond(train,d112,d112_3,d112_4,1).
bond(train,d112,d112_4,d112_5,1).
bond(train,d112,d112_5,d112_6,1).
bond(train,d112,d112_6,d112_1,1).
bond(train,d112,d112_3,d112_7,1).
bond(train,d112,d112_7,d112_8,1).
bond(train,d112,d112_8,d112_9,1).
bond(train,d112,d112_9,d112_10,1).
bond(train,d112,d112_10,d112_4,1).
bond(train,d112,d112_3,d112_11,1).
bond(train,d112,d112_4,d112_12,1).
bond(train,d112,d112_6,d112_13,1).
bond(train,d112,d112_6,d112_14,1).
bond(train,d112,d112_13,d112_1,1).
bond(train,d112,d112_1,d112_15,1).
bond(train,d112,d112_5,d112_16,1).
bond(train,d112,d112_5,d112_17,1).
bond(train,d112,d112_16,d112_2,1).
bond(train,d112,d112_2,d112_18,1).
bond(train,d112,d112_10,d112_19,1).
bond(train,d112,d112_19,d112_7,1).
bond(train,d112,d112_19,d112_20,1).
bond(train,d112,d112_19,d112_21,1).
bond(train,d112,d112_10,d112_22,1).
bond(train,d112,d112_9,d112_23,1).
bond(train,d112,d112_9,d112_24,1).
bond(train,d112,d112_8,d112_25,1).
bond(train,d112,d112_8,d112_26,1).
bond(train,d112,d112_7,d112_27,1).
bond(train,d112,d112_16,d112_28,1).
bond(train,d112,d112_16,d112_29,1).
bond(train,d113,d113_1,d113_2,7).
bond(train,d113,d113_2,d113_3,7).
bond(train,d113,d113_3,d113_4,7).
bond(train,d113,d113_4,d113_5,7).
bond(train,d113,d113_5,d113_6,7).
bond(train,d113,d113_6,d113_1,7).
bond(train,d113,d113_2,d113_7,1).
bond(train,d113,d113_3,d113_8,1).
bond(train,d113,d113_5,d113_9,1).
bond(train,d113,d113_6,d113_10,1).
bond(train,d113,d113_11,d113_12,7).
bond(train,d113,d113_12,d113_13,7).
bond(train,d113,d113_13,d113_14,7).
bond(train,d113,d113_14,d113_15,7).
bond(train,d113,d113_15,d113_16,7).
bond(train,d113,d113_16,d113_11,7).
bond(train,d113,d113_12,d113_17,1).
bond(train,d113,d113_13,d113_18,1).
bond(train,d113,d113_15,d113_19,1).
bond(train,d113,d113_16,d113_20,1).
bond(train,d113,d113_4,d113_21,1).
bond(train,d113,d113_21,d113_11,1).
bond(train,d113,d113_14,d113_22,1).
bond(train,d113,d113_22,d113_23,1).
bond(train,d113,d113_22,d113_24,1).
bond(train,d113,d113_22,d113_25,1).
bond(train,d113,d113_23,d113_26,1).
bond(train,d113,d113_23,d113_27,1).
bond(train,d113,d113_23,d113_28,1).
bond(train,d113,d113_1,d113_29,1).
bond(train,d113,d113_29,d113_30,1).
bond(train,d113,d113_29,d113_31,1).
bond(train,d113,d113_29,d113_32,1).
bond(train,d113,d113_30,d113_33,1).
bond(train,d113,d113_30,d113_34,1).
bond(train,d113,d113_30,d113_35,1).
bond(train,d113,d113_21,d113_36,1).
bond(train,d113,d113_21,d113_37,1).
bond(train,d113,d113_36,d113_38,1).
bond(train,d113,d113_36,d113_39,1).
bond(train,d113,d113_36,d113_40,1).
bond(train,d114,d114_1,d114_2,7).
bond(train,d114,d114_2,d114_3,7).
bond(train,d114,d114_3,d114_4,7).
bond(train,d114,d114_4,d114_5,7).
bond(train,d114,d114_5,d114_6,7).
bond(train,d114,d114_6,d114_1,7).
bond(train,d114,d114_6,d114_7,1).
bond(train,d114,d114_5,d114_8,1).
bond(train,d114,d114_4,d114_9,1).
bond(train,d114,d114_3,d114_10,1).
bond(train,d114,d114_10,d114_11,1).
bond(train,d114,d114_1,d114_12,1).
bond(train,d114,d114_8,d114_13,1).
bond(train,d114,d114_8,d114_14,1).
bond(train,d114,d114_10,d114_15,2).
bond(train,d114,d114_11,d114_16,1).
bond(train,d115,d115_1,d115_2,1).
bond(train,d115,d115_1,d115_3,1).
bond(train,d116,d116_1,d116_2,7).
bond(train,d116,d116_2,d116_3,7).
bond(train,d116,d116_3,d116_4,7).
bond(train,d116,d116_4,d116_5,7).
bond(train,d116,d116_5,d116_6,7).
bond(train,d116,d116_6,d116_1,7).
bond(train,d116,d116_2,d116_7,1).
bond(train,d116,d116_3,d116_8,1).
bond(train,d116,d116_5,d116_9,1).
bond(train,d116,d116_6,d116_10,1).
bond(train,d116,d116_11,d116_12,7).
bond(train,d116,d116_12,d116_13,7).
bond(train,d116,d116_13,d116_14,7).
bond(train,d116,d116_14,d116_15,7).
bond(train,d116,d116_15,d116_16,7).
bond(train,d116,d116_16,d116_11,7).
bond(train,d116,d116_11,d116_17,1).
bond(train,d116,d116_12,d116_18,1).
bond(train,d116,d116_14,d116_19,1).
bond(train,d116,d116_15,d116_20,1).
bond(train,d116,d116_4,d116_21,1).
bond(train,d116,d116_16,d116_21,1).
bond(train,d116,d116_21,d116_22,1).
bond(train,d116,d116_21,d116_23,1).
bond(train,d116,d116_22,d116_24,1).
bond(train,d116,d116_22,d116_25,1).
bond(train,d116,d116_22,d116_26,1).
bond(train,d116,d116_13,d116_27,1).
bond(train,d116,d116_1,d116_28,1).
bond(train,d117,d117_1,d117_2,2).
bond(train,d117,d117_2,d117_3,1).
bond(train,d117,d117_3,d117_4,1).
bond(train,d117,d117_4,d117_5,1).
bond(train,d117,d117_5,d117_6,2).
bond(train,d117,d117_6,d117_1,1).
bond(train,d117,d117_3,d117_7,1).
bond(train,d117,d117_7,d117_8,1).
bond(train,d117,d117_8,d117_9,1).
bond(train,d117,d117_9,d117_10,1).
bond(train,d117,d117_10,d117_4,1).
bond(train,d117,d117_3,d117_11,1).
bond(train,d117,d117_8,d117_12,1).
bond(train,d117,d117_8,d117_13,1).
bond(train,d117,d117_4,d117_14,1).
bond(train,d117,d117_5,d117_15,1).
bond(train,d117,d117_2,d117_16,1).
bond(train,d117,d117_6,d117_17,1).
bond(train,d117,d117_1,d117_18,1).
bond(train,d117,d117_10,d117_19,2).
bond(train,d117,d117_10,d117_20,2).
bond(train,d117,d117_9,d117_21,1).
bond(train,d117,d117_7,d117_22,1).
bond(train,d117,d117_17,d117_23,1).
bond(train,d117,d117_17,d117_24,2).
bond(train,d117,d117_17,d117_25,2).
bond(train,d117,d117_23,d117_26,1).
bond(train,d117,d117_23,d117_27,1).
bond(train,d118,d118_1,d118_2,7).
bond(train,d118,d118_2,d118_3,7).
bond(train,d118,d118_3,d118_4,7).
bond(train,d118,d118_4,d118_5,7).
bond(train,d118,d118_5,d118_6,7).
bond(train,d118,d118_6,d118_1,7).
bond(train,d118,d118_7,d118_8,7).
bond(train,d118,d118_8,d118_9,7).
bond(train,d118,d118_9,d118_10,7).
bond(train,d118,d118_10,d118_11,7).
bond(train,d118,d118_11,d118_12,7).
bond(train,d118,d118_12,d118_7,7).
bond(train,d118,d118_7,d118_13,1).
bond(train,d118,d118_8,d118_14,1).
bond(train,d118,d118_9,d118_15,1).
bond(train,d118,d118_10,d118_16,1).
bond(train,d118,d118_3,d118_17,1).
bond(train,d118,d118_17,d118_12,1).
bond(train,d118,d118_1,d118_18,1).
bond(train,d118,d118_11,d118_19,1).
bond(train,d118,d118_5,d118_20,1).
bond(train,d118,d118_17,d118_21,1).
bond(train,d119,d119_1,d119_2,1).
bond(train,d119,d119_2,d119_3,1).
bond(train,d119,d119_2,d119_4,1).
bond(train,d119,d119_2,d119_5,1).
bond(train,d119,d119_1,d119_6,1).
bond(train,d119,d119_6,d119_7,1).
bond(train,d119,d119_6,d119_8,1).
bond(train,d119,d119_6,d119_9,1).
bond(train,d119,d119_1,d119_10,1).
bond(train,d119,d119_10,d119_11,1).
bond(train,d119,d119_10,d119_12,1).
bond(train,d119,d119_10,d119_13,1).
bond(train,d119,d119_1,d119_14,1).
bond(train,d119,d119_14,d119_15,1).
bond(train,d119,d119_14,d119_16,1).
bond(train,d119,d119_14,d119_17,1).
bond(train,d119,d119_15,d119_18,1).
bond(train,d119,d119_15,d119_19,1).
bond(train,d119,d119_15,d119_20,1).
bond(train,d12,d12_1,d12_2,7).
bond(train,d12,d12_2,d12_3,7).
bond(train,d12,d12_3,d12_4,7).
bond(train,d12,d12_4,d12_5,7).
bond(train,d12,d12_5,d12_6,7).
bond(train,d12,d12_6,d12_1,7).
bond(train,d12,d12_1,d12_7,1).
bond(train,d12,d12_4,d12_8,1).
bond(train,d12,d12_6,d12_9,1).
bond(train,d12,d12_5,d12_10,1).
bond(train,d12,d12_3,d12_11,1).
bond(train,d12,d12_10,d12_12,1).
bond(train,d12,d12_10,d12_13,1).
bond(train,d12,d12_11,d12_14,1).
bond(train,d12,d12_11,d12_15,1).
bond(train,d12,d12_2,d12_16,1).
bond(train,d12,d12_16,d12_17,1).
bond(train,d12,d12_16,d12_18,1).
bond(train,d12,d12_16,d12_19,1).
bond(train,d120,d120_1,d120_2,7).
bond(train,d120,d120_2,d120_3,7).
bond(train,d120,d120_3,d120_4,7).
bond(train,d120,d120_4,d120_5,7).
bond(train,d120,d120_5,d120_6,7).
bond(train,d120,d120_6,d120_1,7).
bond(train,d120,d120_1,d120_7,1).
bond(train,d120,d120_2,d120_8,1).
bond(train,d120,d120_3,d120_9,1).
bond(train,d120,d120_6,d120_10,1).
bond(train,d120,d120_5,d120_11,1).
bond(train,d120,d120_4,d120_12,1).
bond(train,d121,d121_1,d121_2,1).
bond(train,d121,d121_2,d121_3,1).
bond(train,d121,d121_3,d121_4,1).
bond(train,d121,d121_4,d121_5,1).
bond(train,d121,d121_5,d121_6,1).
bond(train,d121,d121_6,d121_1,2).
bond(train,d121,d121_3,d121_7,1).
bond(train,d121,d121_7,d121_8,1).
bond(train,d121,d121_8,d121_9,1).
bond(train,d121,d121_9,d121_10,1).
bond(train,d121,d121_10,d121_4,1).
bond(train,d121,d121_3,d121_11,1).
bond(train,d121,d121_4,d121_12,1).
bond(train,d121,d121_6,d121_13,1).
bond(train,d121,d121_1,d121_14,1).
bond(train,d121,d121_5,d121_15,1).
bond(train,d121,d121_2,d121_16,1).
bond(train,d121,d121_8,d121_17,1).
bond(train,d121,d121_8,d121_18,1).
bond(train,d121,d121_17,d121_9,1).
bond(train,d121,d121_9,d121_19,1).
bond(train,d121,d121_10,d121_20,1).
bond(train,d121,d121_10,d121_21,1).
bond(train,d121,d121_20,d121_7,1).
bond(train,d121,d121_20,d121_22,1).
bond(train,d121,d121_20,d121_23,1).
bond(train,d121,d121_7,d121_24,1).
bond(train,d121,d121_2,d121_25,1).
bond(train,d121,d121_25,d121_5,1).
bond(train,d121,d121_25,d121_26,1).
bond(train,d121,d121_25,d121_27,1).
bond(train,d122,d122_1,d122_2,7).
bond(train,d122,d122_2,d122_3,7).
bond(train,d122,d122_3,d122_4,7).
bond(train,d122,d122_4,d122_5,7).
bond(train,d122,d122_5,d122_6,7).
bond(train,d122,d122_6,d122_1,7).
bond(train,d122,d122_2,d122_7,1).
bond(train,d122,d122_3,d122_8,1).
bond(train,d122,d122_5,d122_9,1).
bond(train,d122,d122_6,d122_10,1).
bond(train,d122,d122_11,d122_12,7).
bond(train,d122,d122_12,d122_13,7).
bond(train,d122,d122_13,d122_14,7).
bond(train,d122,d122_14,d122_15,7).
bond(train,d122,d122_15,d122_16,7).
bond(train,d122,d122_16,d122_11,7).
bond(train,d122,d122_11,d122_17,1).
bond(train,d122,d122_12,d122_18,1).
bond(train,d122,d122_14,d122_19,1).
bond(train,d122,d122_15,d122_20,1).
bond(train,d122,d122_4,d122_21,1).
bond(train,d122,d122_21,d122_16,1).
bond(train,d122,d122_21,d122_22,1).
bond(train,d122,d122_21,d122_23,1).
bond(train,d122,d122_22,d122_24,1).
bond(train,d122,d122_22,d122_25,1).
bond(train,d122,d122_22,d122_26,1).
bond(train,d122,d122_13,d122_27,1).
bond(train,d122,d122_27,d122_28,1).
bond(train,d122,d122_28,d122_29,1).
bond(train,d122,d122_28,d122_30,1).
bond(train,d122,d122_28,d122_31,1).
bond(train,d122,d122_1,d122_32,1).
bond(train,d122,d122_32,d122_33,1).
bond(train,d122,d122_33,d122_34,1).
bond(train,d122,d122_33,d122_35,1).
bond(train,d122,d122_33,d122_36,1).
bond(train,d123,d123_1,d123_2,1).
bond(train,d123,d123_1,d123_3,1).
bond(train,d123,d123_1,d123_4,1).
bond(train,d123,d123_1,d123_5,1).
bond(train,d123,d123_2,d123_6,1).
bond(train,d123,d123_2,d123_7,1).
bond(train,d123,d123_2,d123_8,1).
bond(train,d123,d123_6,d123_9,1).
bond(train,d123,d123_6,d123_10,1).
bond(train,d123,d123_6,d123_11,1).
bond(train,d123,d123_11,d123_12,1).
bond(train,d123,d123_11,d123_13,1).
bond(train,d123,d123_11,d123_14,1).
bond(train,d123,d123_12,d123_15,1).
bond(train,d123,d123_12,d123_16,1).
bond(train,d123,d123_12,d123_17,1).
bond(train,d123,d123_9,d123_18,1).
bond(train,d123,d123_18,d123_19,1).
bond(train,d123,d123_19,d123_20,1).
bond(train,d123,d123_9,d123_21,2).
bond(train,d123,d123_18,d123_22,1).
bond(train,d123,d123_19,d123_23,2).
bond(train,d123,d123_20,d123_24,1).
bond(train,d123,d123_20,d123_25,1).
bond(train,d124,d124_1,d124_2,1).
bond(train,d124,d124_2,d124_3,1).
bond(train,d124,d124_2,d124_4,2).
bond(train,d124,d124_3,d124_5,1).
bond(train,d124,d124_3,d124_6,1).
bond(train,d124,d124_5,d124_7,2).
bond(train,d124,d124_5,d124_8,2).
bond(train,d124,d124_5,d124_9,1).
bond(train,d124,d124_9,d124_10,7).
bond(train,d124,d124_10,d124_11,7).
bond(train,d124,d124_11,d124_12,7).
bond(train,d124,d124_12,d124_13,7).
bond(train,d124,d124_13,d124_14,7).
bond(train,d124,d124_14,d124_9,7).
bond(train,d124,d124_10,d124_15,1).
bond(train,d124,d124_11,d124_16,1).
bond(train,d124,d124_13,d124_17,1).
bond(train,d124,d124_14,d124_18,1).
bond(train,d124,d124_12,d124_19,1).
bond(train,d124,d124_1,d124_20,1).
bond(train,d124,d124_1,d124_21,1).
bond(train,d124,d124_20,d124_22,1).
bond(train,d124,d124_20,d124_23,1).
bond(train,d124,d124_20,d124_24,1).
bond(train,d124,d124_22,d124_25,1).
bond(train,d124,d124_22,d124_26,1).
bond(train,d124,d124_22,d124_27,1).
bond(train,d124,d124_25,d124_28,1).
bond(train,d124,d124_25,d124_29,1).
bond(train,d124,d124_25,d124_30,1).
bond(train,d125,d125_1,d125_2,7).
bond(train,d125,d125_2,d125_3,7).
bond(train,d125,d125_3,d125_4,7).
bond(train,d125,d125_4,d125_5,7).
bond(train,d125,d125_5,d125_6,7).
bond(train,d125,d125_6,d125_1,7).
bond(train,d125,d125_2,d125_7,1).
bond(train,d125,d125_3,d125_8,1).
bond(train,d125,d125_5,d125_9,1).
bond(train,d125,d125_6,d125_10,1).
bond(train,d125,d125_11,d125_12,7).
bond(train,d125,d125_12,d125_13,7).
bond(train,d125,d125_13,d125_14,7).
bond(train,d125,d125_14,d125_15,7).
bond(train,d125,d125_15,d125_16,7).
bond(train,d125,d125_16,d125_11,7).
bond(train,d125,d125_11,d125_17,1).
bond(train,d125,d125_12,d125_18,1).
bond(train,d125,d125_14,d125_19,1).
bond(train,d125,d125_15,d125_20,1).
bond(train,d125,d125_4,d125_21,1).
bond(train,d125,d125_16,d125_21,1).
bond(train,d125,d125_21,d125_22,1).
bond(train,d125,d125_21,d125_23,1).
bond(train,d125,d125_22,d125_24,1).
bond(train,d125,d125_22,d125_25,1).
bond(train,d125,d125_22,d125_26,1).
bond(train,d125,d125_13,d125_27,1).
bond(train,d125,d125_1,d125_28,1).
bond(train,d126,d126_1,d126_2,1).
bond(train,d126,d126_2,d126_3,1).
bond(train,d126,d126_3,d126_4,1).
bond(train,d126,d126_4,d126_5,1).
bond(train,d126,d126_5,d126_6,1).
bond(train,d126,d126_6,d126_1,1).
bond(train,d126,d126_6,d126_7,1).
bond(train,d126,d126_5,d126_8,1).
bond(train,d126,d126_4,d126_9,1).
bond(train,d126,d126_2,d126_10,1).
bond(train,d126,d126_3,d126_11,1).
bond(train,d126,d126_1,d126_12,1).
bond(train,d126,d126_1,d126_13,1).
bond(train,d126,d126_2,d126_14,1).
bond(train,d126,d126_3,d126_15,1).
bond(train,d126,d126_4,d126_16,1).
bond(train,d126,d126_5,d126_17,1).
bond(train,d126,d126_6,d126_18,1).
bond(train,d127,d127_1,d127_2,7).
bond(train,d127,d127_2,d127_3,7).
bond(train,d127,d127_3,d127_4,7).
bond(train,d127,d127_4,d127_5,7).
bond(train,d127,d127_5,d127_6,7).
bond(train,d127,d127_6,d127_1,7).
bond(train,d127,d127_6,d127_7,1).
bond(train,d127,d127_4,d127_8,1).
bond(train,d127,d127_2,d127_9,1).
bond(train,d127,d127_3,d127_10,1).
bond(train,d127,d127_1,d127_11,1).
bond(train,d127,d127_5,d127_12,1).
bond(train,d127,d127_12,d127_13,2).
bond(train,d127,d127_12,d127_14,2).
bond(train,d128,d128_1,d128_2,1).
bond(train,d128,d128_2,d128_3,1).
bond(train,d128,d128_3,d128_4,1).
bond(train,d128,d128_4,d128_5,1).
bond(train,d128,d128_4,d128_6,1).
bond(train,d128,d128_4,d128_7,1).
bond(train,d128,d128_2,d128_8,1).
bond(train,d128,d128_2,d128_9,1).
bond(train,d128,d128_3,d128_10,1).
bond(train,d128,d128_1,d128_11,1).
bond(train,d128,d128_11,d128_12,1).
bond(train,d128,d128_12,d128_8,1).
bond(train,d128,d128_11,d128_13,1).
bond(train,d128,d128_11,d128_14,1).
bond(train,d128,d128_12,d128_13,1).
bond(train,d128,d128_12,d128_15,1).
bond(train,d128,d128_8,d128_16,1).
bond(train,d128,d128_8,d128_17,1).
bond(train,d128,d128_16,d128_10,1).
bond(train,d128,d128_10,d128_18,1).
bond(train,d128,d128_18,d128_6,1).
bond(train,d128,d128_18,d128_19,1).
bond(train,d128,d128_18,d128_20,1).
bond(train,d128,d128_6,d128_21,1).
bond(train,d128,d128_3,d128_22,1).
bond(train,d128,d128_16,d128_23,1).
bond(train,d128,d128_16,d128_24,1).
bond(train,d128,d128_1,d128_23,1).
bond(train,d128,d128_1,d128_25,1).
bond(train,d128,d128_23,d128_6,1).
bond(train,d128,d128_23,d128_26,1).
bond(train,d128,d128_10,d128_27,1).
bond(train,d129,d129_1,d129_2,1).
bond(train,d129,d129_2,d129_3,1).
bond(train,d129,d129_2,d129_4,2).
bond(train,d129,d129_4,d129_5,1).
bond(train,d129,d129_5,d129_6,1).
bond(train,d129,d129_5,d129_7,1).
bond(train,d129,d129_5,d129_8,1).
bond(train,d129,d129_4,d129_9,1).
bond(train,d129,d129_9,d129_10,1).
bond(train,d129,d129_9,d129_11,1).
bond(train,d129,d129_9,d129_12,1).
bond(train,d13,d13_1,d13_2,7).
bond(train,d13,d13_2,d13_3,7).
bond(train,d13,d13_3,d13_4,7).
bond(train,d13,d13_4,d13_5,7).
bond(train,d13,d13_5,d13_6,7).
bond(train,d13,d13_6,d13_1,7).
bond(train,d13,d13_1,d13_7,1).
bond(train,d13,d13_4,d13_8,1).
bond(train,d13,d13_6,d13_9,1).
bond(train,d13,d13_5,d13_10,1).
bond(train,d13,d13_3,d13_11,1).
bond(train,d13,d13_2,d13_12,1).
bond(train,d13,d13_12,d13_13,1).
bond(train,d13,d13_13,d13_14,1).
bond(train,d13,d13_13,d13_15,1).
bond(train,d13,d13_13,d13_16,1).
bond(train,d13,d13_12,d13_17,1).
bond(train,d13,d13_11,d13_18,2).
bond(train,d13,d13_11,d13_19,2).
bond(train,d13,d13_10,d13_20,1).
bond(train,d13,d13_20,d13_21,1).
bond(train,d13,d13_20,d13_22,1).
bond(train,d13,d13_20,d13_23,1).
bond(train,d13,d13_21,d13_24,1).
bond(train,d13,d13_21,d13_25,1).
bond(train,d13,d13_21,d13_26,1).
bond(train,d13,d13_10,d13_27,1).
bond(train,d13,d13_27,d13_28,1).
bond(train,d13,d13_27,d13_29,1).
bond(train,d13,d13_27,d13_30,1).
bond(train,d13,d13_28,d13_31,1).
bond(train,d13,d13_28,d13_32,1).
bond(train,d13,d13_28,d13_33,1).
bond(train,d13,d13_24,d13_34,1).
bond(train,d13,d13_31,d13_35,1).
bond(train,d130,d130_1,d130_2,7).
bond(train,d130,d130_2,d130_3,7).
bond(train,d130,d130_3,d130_4,7).
bond(train,d130,d130_4,d130_5,7).
bond(train,d130,d130_5,d130_6,7).
bond(train,d130,d130_6,d130_1,7).
bond(train,d130,d130_1,d130_7,1).
bond(train,d130,d130_2,d130_8,1).
bond(train,d130,d130_5,d130_9,1).
bond(train,d130,d130_6,d130_10,1).
bond(train,d130,d130_4,d130_11,1).
bond(train,d130,d130_3,d130_12,1).
bond(train,d130,d130_11,d130_13,2).
bond(train,d130,d130_11,d130_14,1).
bond(train,d130,d130_13,d130_15,1).
bond(train,d130,d130_13,d130_16,1).
bond(train,d130,d130_15,d130_17,3).
bond(train,d130,d130_16,d130_18,3).
bond(train,d131,d131_1,d131_2,7).
bond(train,d131,d131_2,d131_3,7).
bond(train,d131,d131_3,d131_4,7).
bond(train,d131,d131_4,d131_5,7).
bond(train,d131,d131_5,d131_6,7).
bond(train,d131,d131_6,d131_1,7).
bond(train,d131,d131_6,d131_7,1).
bond(train,d131,d131_1,d131_8,1).
bond(train,d131,d131_5,d131_9,1).
bond(train,d131,d131_9,d131_10,1).
bond(train,d131,d131_10,d131_11,1).
bond(train,d131,d131_10,d131_12,1).
bond(train,d131,d131_10,d131_13,1).
bond(train,d131,d131_4,d131_14,1).
bond(train,d131,d131_3,d131_15,1).
bond(train,d131,d131_2,d131_16,1).
bond(train,d131,d131_16,d131_17,2).
bond(train,d131,d131_16,d131_18,2).
bond(train,d132,d132_1,d132_2,7).
bond(train,d132,d132_2,d132_3,7).
bond(train,d132,d132_3,d132_4,7).
bond(train,d132,d132_4,d132_5,7).
bond(train,d132,d132_5,d132_6,7).
bond(train,d132,d132_6,d132_1,7).
bond(train,d132,d132_1,d132_7,1).
bond(train,d132,d132_4,d132_8,1).
bond(train,d132,d132_5,d132_9,1).
bond(train,d132,d132_6,d132_10,1).
bond(train,d132,d132_11,d132_12,7).
bond(train,d132,d132_12,d132_13,7).
bond(train,d132,d132_13,d132_14,7).
bond(train,d132,d132_14,d132_15,7).
bond(train,d132,d132_15,d132_16,7).
bond(train,d132,d132_16,d132_11,7).
bond(train,d132,d132_12,d132_17,1).
bond(train,d132,d132_13,d132_18,1).
bond(train,d132,d132_15,d132_19,1).
bond(train,d132,d132_16,d132_20,1).
bond(train,d132,d132_3,d132_21,1).
bond(train,d132,d132_21,d132_11,1).
bond(train,d132,d132_21,d132_22,1).
bond(train,d132,d132_21,d132_23,1).
bond(train,d132,d132_22,d132_24,1).
bond(train,d132,d132_22,d132_25,1).
bond(train,d132,d132_22,d132_26,1).
bond(train,d132,d132_24,d132_27,1).
bond(train,d132,d132_24,d132_28,1).
bond(train,d132,d132_24,d132_29,1).
bond(train,d132,d132_27,d132_30,1).
bond(train,d132,d132_30,d132_31,1).
bond(train,d132,d132_30,d132_32,1).
bond(train,d132,d132_30,d132_33,1).
bond(train,d132,d132_27,d132_34,1).
bond(train,d132,d132_34,d132_35,1).
bond(train,d132,d132_34,d132_36,1).
bond(train,d132,d132_34,d132_37,1).
bond(train,d132,d132_14,d132_38,1).
bond(train,d133,d133_1,d133_2,7).
bond(train,d133,d133_2,d133_3,7).
bond(train,d133,d133_3,d133_4,7).
bond(train,d133,d133_4,d133_5,7).
bond(train,d133,d133_5,d133_6,7).
bond(train,d133,d133_6,d133_1,7).
bond(train,d133,d133_1,d133_7,1).
bond(train,d133,d133_3,d133_8,1).
bond(train,d133,d133_6,d133_9,1).
bond(train,d133,d133_5,d133_10,1).
bond(train,d133,d133_4,d133_11,1).
bond(train,d133,d133_2,d133_12,1).
bond(train,d133,d133_10,d133_13,1).
bond(train,d134,d134_1,d134_2,7).
bond(train,d134,d134_2,d134_3,7).
bond(train,d134,d134_3,d134_4,7).
bond(train,d134,d134_4,d134_5,7).
bond(train,d134,d134_5,d134_6,7).
bond(train,d134,d134_6,d134_1,7).
bond(train,d134,d134_1,d134_7,1).
bond(train,d134,d134_2,d134_8,1).
bond(train,d134,d134_3,d134_9,1).
bond(train,d134,d134_4,d134_10,1).
bond(train,d134,d134_5,d134_11,1).
bond(train,d134,d134_6,d134_12,1).
bond(train,d135,d135_1,d135_2,7).
bond(train,d135,d135_2,d135_3,7).
bond(train,d135,d135_3,d135_4,7).
bond(train,d135,d135_4,d135_5,7).
bond(train,d135,d135_5,d135_6,7).
bond(train,d135,d135_6,d135_1,7).
bond(train,d135,d135_1,d135_7,1).
bond(train,d135,d135_2,d135_8,1).
bond(train,d135,d135_5,d135_9,1).
bond(train,d135,d135_6,d135_10,1).
bond(train,d135,d135_4,d135_11,1).
bond(train,d135,d135_11,d135_13,2).
bond(train,d135,d135_12,d135_14,1).
bond(train,d135,d135_12,d135_15,1).
bond(train,d135,d135_15,d135_16,1).
bond(train,d135,d135_15,d135_17,1).
bond(train,d135,d135_15,d135_18,1).
bond(train,d135,d135_16,d135_19,1).
bond(train,d135,d135_16,d135_20,1).
bond(train,d135,d135_16,d135_21,1).
bond(train,d135,d135_14,d135_22,1).
bond(train,d135,d135_14,d135_23,1).
bond(train,d135,d135_14,d135_24,1).
bond(train,d135,d135_22,d135_25,1).
bond(train,d135,d135_22,d135_26,1).
bond(train,d135,d135_22,d135_27,1).
bond(train,d135,d135_25,d135_28,1).
bond(train,d135,d135_25,d135_29,1).
bond(train,d135,d135_25,d135_30,1).
bond(train,d135,d135_28,d135_31,1).
bond(train,d135,d135_28,d135_32,1).
bond(train,d135,d135_28,d135_33,1).
bond(train,d135,d135_11,d135_34,1).
bond(train,d135,d135_34,d135_35,1).
bond(train,d135,d135_35,d135_12,1).
bond(train,d135,d135_35,d135_36,1).
bond(train,d135,d135_35,d135_37,1).
bond(train,d135,d135_12,d135_38,1).
bond(train,d135,d135_3,d135_39,1).
bond(train,d135,d135_39,d135_40,1).
bond(train,d135,d135_40,d135_41,1).
bond(train,d135,d135_41,d135_42,1).
bond(train,d135,d135_41,d135_43,1).
bond(train,d135,d135_41,d135_44,1).
bond(train,d135,d135_42,d135_45,1).
bond(train,d135,d135_45,d135_46,1).
bond(train,d135,d135_45,d135_47,1).
bond(train,d135,d135_45,d135_48,1).
bond(train,d135,d135_42,d135_49,1).
bond(train,d135,d135_42,d135_50,1).
bond(train,d135,d135_49,d135_51,1).
bond(train,d135,d135_49,d135_52,1).
bond(train,d135,d135_49,d135_53,1).
bond(train,d135,d135_51,d135_54,1).
bond(train,d135,d135_51,d135_55,1).
bond(train,d135,d135_51,d135_56,1).
bond(train,d135,d135_54,d135_57,1).
bond(train,d135,d135_54,d135_58,1).
bond(train,d135,d135_54,d135_59,1).
bond(train,d135,d135_57,d135_60,1).
bond(train,d135,d135_57,d135_61,1).
bond(train,d135,d135_57,d135_62,1).
bond(train,d135,d135_39,d135_63,2).
bond(train,d136,d136_1,d136_2,1).
bond(train,d136,d136_2,d136_3,1).
bond(train,d136,d136_2,d136_4,1).
bond(train,d136,d136_2,d136_5,1).
bond(train,d136,d136_3,d136_6,2).
bond(train,d136,d136_3,d136_7,1).
bond(train,d136,d136_1,d136_8,1).
bond(train,d136,d136_8,d136_9,1).
bond(train,d136,d136_8,d136_10,1).
bond(train,d136,d136_8,d136_11,1).
bond(train,d136,d136_9,d136_12,2).
bond(train,d136,d136_9,d136_13,1).
bond(train,d136,d136_1,d136_14,1).
bond(train,d136,d136_14,d136_15,1).
bond(train,d136,d136_14,d136_16,1).
bond(train,d136,d136_14,d136_17,1).
bond(train,d136,d136_15,d136_18,1).
bond(train,d136,d136_15,d136_19,2).
bond(train,d136,d136_7,d136_20,1).
bond(train,d136,d136_13,d136_21,1).
bond(train,d136,d136_18,d136_22,1).
bond(train,d137,d137_1,d137_2,7).
bond(train,d137,d137_2,d137_3,7).
bond(train,d137,d137_3,d137_4,7).
bond(train,d137,d137_4,d137_5,7).
bond(train,d137,d137_5,d137_6,7).
bond(train,d137,d137_6,d137_1,7).
bond(train,d137,d137_2,d137_7,1).
bond(train,d137,d137_5,d137_8,1).
bond(train,d137,d137_6,d137_9,1).
bond(train,d137,d137_10,d137_11,1).
bond(train,d137,d137_11,d137_12,1).
bond(train,d137,d137_12,d137_13,1).
bond(train,d137,d137_13,d137_14,1).
bond(train,d137,d137_14,d137_15,1).
bond(train,d137,d137_15,d137_10,7).
bond(train,d137,d137_13,d137_16,1).
bond(train,d137,d137_13,d137_17,1).
bond(train,d137,d137_14,d137_18,1).
bond(train,d137,d137_14,d137_19,1).
bond(train,d137,d137_4,d137_15,7).
bond(train,d137,d137_3,d137_20,7).
bond(train,d137,d137_20,d137_10,7).
bond(train,d137,d137_1,d137_21,1).
bond(train,d137,d137_21,d137_22,1).
bond(train,d137,d137_22,d137_23,1).
bond(train,d137,d137_22,d137_24,1).
bond(train,d137,d137_22,d137_25,1).
bond(train,d137,d137_11,d137_26,1).
bond(train,d137,d137_26,d137_27,1).
bond(train,d137,d137_27,d137_28,1).
bond(train,d137,d137_28,d137_29,1).
bond(train,d137,d137_29,d137_12,1).
bond(train,d137,d137_11,d137_30,1).
bond(train,d137,d137_26,d137_31,1).
bond(train,d137,d137_26,d137_32,1).
bond(train,d137,d137_29,d137_33,1).
bond(train,d137,d137_29,d137_34,1).
bond(train,d137,d137_27,d137_35,1).
bond(train,d137,d137_35,d137_36,1).
bond(train,d137,d137_36,d137_37,1).
bond(train,d137,d137_37,d137_38,1).
bond(train,d137,d137_38,d137_28,1).
bond(train,d137,d137_28,d137_39,1).
bond(train,d137,d137_27,d137_40,1).
bond(train,d137,d137_38,d137_41,1).
bond(train,d137,d137_38,d137_42,1).
bond(train,d137,d137_35,d137_43,1).
bond(train,d137,d137_35,d137_44,1).
bond(train,d137,d137_43,d137_45,1).
bond(train,d137,d137_45,d137_46,1).
bond(train,d137,d137_46,d137_47,1).
bond(train,d137,d137_46,d137_48,1).
bond(train,d137,d137_46,d137_49,1).
bond(train,d137,d137_43,d137_50,2).
bond(train,d137,d137_36,d137_51,1).
bond(train,d137,d137_36,d137_52,1).
bond(train,d137,d137_51,d137_53,1).
bond(train,d137,d137_53,d137_54,1).
bond(train,d137,d137_53,d137_55,1).
bond(train,d137,d137_53,d137_56,1).
bond(train,d137,d137_37,d137_57,1).
bond(train,d137,d137_37,d137_58,1).
bond(train,d137,d137_57,d137_59,1).
bond(train,d137,d137_59,d137_60,1).
bond(train,d137,d137_60,d137_61,7).
bond(train,d137,d137_61,d137_62,7).
bond(train,d137,d137_62,d137_63,7).
bond(train,d137,d137_63,d137_64,7).
bond(train,d137,d137_64,d137_65,7).
bond(train,d137,d137_65,d137_60,7).
bond(train,d137,d137_61,d137_66,1).
bond(train,d137,d137_65,d137_67,1).
bond(train,d137,d137_59,d137_68,2).
bond(train,d137,d137_64,d137_69,1).
bond(train,d137,d137_69,d137_70,1).
bond(train,d137,d137_70,d137_71,1).
bond(train,d137,d137_70,d137_72,1).
bond(train,d137,d137_70,d137_73,1).
bond(train,d137,d137_63,d137_74,1).
bond(train,d137,d137_74,d137_75,1).
bond(train,d137,d137_75,d137_76,1).
bond(train,d137,d137_75,d137_77,1).
bond(train,d137,d137_75,d137_78,1).
bond(train,d137,d137_62,d137_79,1).
bond(train,d137,d137_79,d137_80,1).
bond(train,d137,d137_80,d137_81,1).
bond(train,d137,d137_80,d137_82,1).
bond(train,d137,d137_80,d137_83,1).
bond(train,d137,d137_20,d137_84,1).
bond(train,d138,d138_1,d138_2,7).
bond(train,d138,d138_2,d138_3,7).
bond(train,d138,d138_3,d138_4,7).
bond(train,d138,d138_4,d138_5,7).
bond(train,d138,d138_5,d138_6,7).
bond(train,d138,d138_6,d138_1,7).
bond(train,d138,d138_1,d138_7,1).
bond(train,d138,d138_3,d138_8,1).
bond(train,d138,d138_4,d138_9,1).
bond(train,d138,d138_6,d138_10,1).
bond(train,d138,d138_5,d138_11,1).
bond(train,d138,d138_2,d138_12,1).
bond(train,d138,d138_11,d138_13,1).
bond(train,d138,d138_12,d138_14,1).
bond(train,d139,d139_1,d139_2,1).
bond(train,d139,d139_2,d139_3,1).
bond(train,d139,d139_2,d139_4,1).
bond(train,d139,d139_2,d139_5,1).
bond(train,d139,d139_3,d139_6,1).
bond(train,d139,d139_3,d139_7,1).
bond(train,d139,d139_3,d139_8,1).
bond(train,d139,d139_6,d139_9,1).
bond(train,d139,d139_6,d139_10,1).
bond(train,d139,d139_6,d139_11,1).
bond(train,d139,d139_9,d139_12,1).
bond(train,d139,d139_9,d139_13,1).
bond(train,d139,d139_9,d139_14,1).
bond(train,d139,d139_12,d139_15,1).
bond(train,d139,d139_12,d139_16,1).
bond(train,d139,d139_12,d139_17,1).
bond(train,d139,d139_15,d139_18,1).
bond(train,d139,d139_15,d139_19,1).
bond(train,d139,d139_15,d139_20,1).
bond(train,d139,d139_18,d139_21,1).
bond(train,d139,d139_18,d139_22,1).
bond(train,d139,d139_18,d139_23,1).
bond(train,d139,d139_21,d139_24,1).
bond(train,d139,d139_21,d139_25,1).
bond(train,d139,d139_21,d139_26,1).
bond(train,d139,d139_24,d139_27,1).
bond(train,d139,d139_24,d139_28,1).
bond(train,d139,d139_24,d139_29,1).
bond(train,d139,d139_27,d139_30,1).
bond(train,d139,d139_27,d139_31,2).
bond(train,d139,d139_30,d139_32,1).
bond(train,d139,d139_1,d139_33,1).
bond(train,d139,d139_1,d139_34,1).
bond(train,d139,d139_1,d139_35,1).
bond(train,d139,d139_33,d139_36,1).
bond(train,d139,d139_33,d139_37,1).
bond(train,d14,d14_1,d14_2,7).
bond(train,d14,d14_2,d14_3,7).
bond(train,d14,d14_3,d14_4,7).
bond(train,d14,d14_4,d14_5,7).
bond(train,d14,d14_5,d14_6,7).
bond(train,d14,d14_6,d14_1,7).
bond(train,d14,d14_1,d14_7,1).
bond(train,d14,d14_2,d14_8,1).
bond(train,d14,d14_4,d14_9,1).
bond(train,d14,d14_5,d14_10,1).
bond(train,d14,d14_6,d14_11,1).
bond(train,d14,d14_3,d14_12,1).
bond(train,d14,d14_12,d14_13,1).
bond(train,d14,d14_13,d14_14,1).
bond(train,d14,d14_14,d14_15,7).
bond(train,d14,d14_15,d14_16,7).
bond(train,d14,d14_16,d14_17,7).
bond(train,d14,d14_17,d14_18,7).
bond(train,d14,d14_18,d14_19,7).
bond(train,d14,d14_19,d14_14,7).
bond(train,d14,d14_15,d14_20,1).
bond(train,d14,d14_16,d14_21,1).
bond(train,d14,d14_17,d14_22,1).
bond(train,d14,d14_18,d14_23,1).
bond(train,d14,d14_19,d14_24,1).
bond(train,d14,d14_12,d14_25,1).
bond(train,d14,d14_13,d14_26,1).
bond(train,d140,d140_1,d140_2,7).
bond(train,d140,d140_2,d140_3,7).
bond(train,d140,d140_3,d140_4,7).
bond(train,d140,d140_4,d140_5,7).
bond(train,d140,d140_5,d140_6,7).
bond(train,d140,d140_6,d140_1,7).
bond(train,d140,d140_2,d140_7,1).
bond(train,d140,d140_6,d140_8,1).
bond(train,d140,d140_3,d140_9,1).
bond(train,d140,d140_9,d140_10,2).
bond(train,d140,d140_10,d140_11,1).
bond(train,d140,d140_11,d140_12,1).
bond(train,d140,d140_11,d140_13,1).
bond(train,d140,d140_11,d140_14,1).
bond(train,d140,d140_12,d140_15,1).
bond(train,d140,d140_12,d140_16,1).
bond(train,d140,d140_12,d140_17,1).
bond(train,d140,d140_15,d140_18,1).
bond(train,d140,d140_15,d140_19,1).
bond(train,d140,d140_15,d140_20,1).
bond(train,d140,d140_18,d140_21,2).
bond(train,d140,d140_18,d140_22,1).
bond(train,d140,d140_22,d140_23,1).
bond(train,d140,d140_22,d140_24,1).
bond(train,d140,d140_22,d140_25,1).
bond(train,d140,d140_23,d140_26,1).
bond(train,d140,d140_23,d140_27,1).
bond(train,d140,d140_23,d140_28,1).
bond(train,d140,d140_26,d140_29,1).
bond(train,d140,d140_26,d140_30,1).
bond(train,d140,d140_26,d140_31,1).
bond(train,d140,d140_29,d140_32,1).
bond(train,d140,d140_32,d140_33,1).
bond(train,d140,d140_32,d140_34,1).
bond(train,d140,d140_32,d140_35,1).
bond(train,d140,d140_29,d140_36,1).
bond(train,d140,d140_29,d140_37,1).
bond(train,d140,d140_36,d140_38,1).
bond(train,d140,d140_38,d140_4,1).
bond(train,d140,d140_5,d140_39,1).
bond(train,d140,d140_1,d140_40,1).
bond(train,d140,d140_39,d140_41,1).
bond(train,d140,d140_40,d140_42,1).
bond(train,d140,d140_38,d140_43,2).
bond(train,d140,d140_9,d140_44,1).
bond(train,d140,d140_10,d140_45,1).
bond(train,d141,d141_1,d141_2,7).
bond(train,d141,d141_2,d141_3,7).
bond(train,d141,d141_3,d141_4,7).
bond(train,d141,d141_4,d141_5,7).
bond(train,d141,d141_5,d141_6,7).
bond(train,d141,d141_6,d141_1,7).
bond(train,d141,d141_1,d141_7,1).
bond(train,d141,d141_2,d141_8,1).
bond(train,d141,d141_3,d141_9,1).
bond(train,d141,d141_4,d141_10,1).
bond(train,d141,d141_6,d141_11,1).
bond(train,d141,d141_5,d141_12,1).
bond(train,d141,d141_12,d141_13,1).
bond(train,d141,d141_12,d141_14,1).
bond(train,d141,d141_12,d141_15,1).
bond(train,d141,d141_13,d141_16,1).
bond(train,d141,d141_16,d141_17,1).
bond(train,d141,d141_17,d141_18,1).
bond(train,d141,d141_17,d141_19,1).
bond(train,d141,d141_17,d141_20,1).
bond(train,d141,d141_16,d141_21,2).
bond(train,d142,d142_1,d142_2,7).
bond(train,d142,d142_2,d142_3,7).
bond(train,d142,d142_3,d142_4,7).
bond(train,d142,d142_4,d142_5,7).
bond(train,d142,d142_5,d142_6,7).
bond(train,d142,d142_6,d142_1,7).
bond(train,d142,d142_1,d142_7,1).
bond(train,d142,d142_2,d142_8,1).
bond(train,d142,d142_5,d142_9,1).
bond(train,d142,d142_6,d142_10,1).
bond(train,d142,d142_3,d142_11,7).
bond(train,d142,d142_11,d142_12,7).
bond(train,d142,d142_12,d142_13,7).
bond(train,d142,d142_13,d142_4,7).
bond(train,d142,d142_12,d142_14,1).
bond(train,d142,d142_14,d142_15,1).
bond(train,d143,d143_1,d143_2,7).
bond(train,d143,d143_2,d143_3,7).
bond(train,d143,d143_3,d143_4,7).
bond(train,d143,d143_4,d143_5,7).
bond(train,d143,d143_5,d143_6,7).
bond(train,d143,d143_6,d143_1,7).
bond(train,d143,d143_3,d143_7,1).
bond(train,d143,d143_7,d143_8,1).
bond(train,d143,d143_8,d143_9,2).
bond(train,d143,d143_9,d143_10,1).
bond(train,d143,d143_10,d143_4,1).
bond(train,d143,d143_1,d143_11,1).
bond(train,d143,d143_2,d143_12,1).
bond(train,d143,d143_7,d143_13,2).
bond(train,d143,d143_10,d143_14,1).
bond(train,d143,d143_14,d143_15,1).
bond(train,d143,d143_14,d143_16,1).
bond(train,d143,d143_14,d143_17,1).
bond(train,d143,d143_15,d143_18,1).
bond(train,d143,d143_15,d143_19,1).
bond(train,d143,d143_15,d143_20,1).
bond(train,d143,d143_8,d143_21,1).
bond(train,d143,d143_21,d143_22,1).
bond(train,d143,d143_21,d143_23,2).
bond(train,d143,d143_22,d143_24,1).
bond(train,d143,d143_6,d143_25,1).
bond(train,d143,d143_25,d143_26,1).
bond(train,d143,d143_25,d143_27,1).
bond(train,d143,d143_25,d143_28,1).
bond(train,d143,d143_9,d143_29,1).
bond(train,d144,d144_1,d144_2,1).
bond(train,d144,d144_2,d144_3,1).
bond(train,d144,d144_3,d144_4,1).
bond(train,d144,d144_3,d144_5,1).
bond(train,d144,d144_3,d144_6,1).
bond(train,d144,d144_4,d144_7,1).
bond(train,d144,d144_7,d144_8,1).
bond(train,d144,d144_7,d144_9,1).
bond(train,d144,d144_7,d144_10,1).
bond(train,d144,d144_8,d144_11,1).
bond(train,d144,d144_8,d144_12,1).
bond(train,d144,d144_8,d144_13,1).
bond(train,d144,d144_4,d144_14,1).
bond(train,d144,d144_4,d144_15,1).
bond(train,d144,d144_14,d144_16,1).
bond(train,d144,d144_14,d144_17,1).
bond(train,d144,d144_14,d144_18,1).
bond(train,d144,d144_16,d144_19,1).
bond(train,d144,d144_16,d144_20,1).
bond(train,d144,d144_16,d144_21,1).
bond(train,d144,d144_19,d144_22,1).
bond(train,d144,d144_19,d144_23,1).
bond(train,d144,d144_19,d144_24,1).
bond(train,d144,d144_22,d144_25,1).
bond(train,d144,d144_22,d144_26,1).
bond(train,d144,d144_22,d144_27,1).
bond(train,d144,d144_1,d144_28,2).
bond(train,d144,d144_29,d144_31,1).
bond(train,d144,d144_31,d144_32,2).
bond(train,d144,d144_31,d144_33,1).
bond(train,d144,d144_33,d144_34,1).
bond(train,d144,d144_34,d144_35,1).
bond(train,d144,d144_34,d144_36,1).
bond(train,d144,d144_34,d144_37,1).
bond(train,d144,d144_35,d144_38,1).
bond(train,d144,d144_38,d144_39,1).
bond(train,d144,d144_38,d144_40,1).
bond(train,d144,d144_38,d144_41,1).
bond(train,d144,d144_39,d144_42,1).
bond(train,d144,d144_39,d144_43,1).
bond(train,d144,d144_39,d144_44,1).
bond(train,d144,d144_35,d144_45,1).
bond(train,d144,d144_35,d144_46,1).
bond(train,d144,d144_45,d144_47,1).
bond(train,d144,d144_45,d144_48,1).
bond(train,d144,d144_45,d144_49,1).
bond(train,d144,d144_47,d144_50,1).
bond(train,d144,d144_47,d144_51,1).
bond(train,d144,d144_47,d144_52,1).
bond(train,d144,d144_50,d144_53,1).
bond(train,d144,d144_50,d144_54,1).
bond(train,d144,d144_50,d144_55,1).
bond(train,d144,d144_53,d144_56,1).
bond(train,d144,d144_53,d144_57,1).
bond(train,d144,d144_53,d144_58,1).
bond(train,d144,d144_1,d144_30,1).
bond(train,d144,d144_29,d144_59,1).
bond(train,d144,d144_29,d144_60,1).
bond(train,d144,d144_29,d144_61,1).
bond(train,d144,d144_59,d144_62,1).
bond(train,d144,d144_59,d144_63,1).
bond(train,d144,d144_59,d144_64,1).
bond(train,d144,d144_62,d144_30,1).
bond(train,d144,d144_62,d144_65,1).
bond(train,d144,d144_62,d144_66,1).
bond(train,d144,d144_30,d144_67,1).
bond(train,d144,d144_30,d144_68,1).
bond(train,d145,d145_1,d145_2,7).
bond(train,d145,d145_2,d145_3,7).
bond(train,d145,d145_3,d145_4,7).
bond(train,d145,d145_4,d145_5,7).
bond(train,d145,d145_5,d145_6,7).
bond(train,d145,d145_6,d145_1,7).
bond(train,d145,d145_1,d145_7,1).
bond(train,d145,d145_2,d145_8,1).
bond(train,d145,d145_5,d145_9,1).
bond(train,d145,d145_6,d145_10,1).
bond(train,d145,d145_4,d145_11,1).
bond(train,d145,d145_12,d145_13,1).
bond(train,d145,d145_13,d145_14,1).
bond(train,d145,d145_13,d145_15,1).
bond(train,d145,d145_13,d145_16,1).
bond(train,d145,d145_14,d145_17,1).
bond(train,d145,d145_14,d145_18,1).
bond(train,d145,d145_14,d145_19,1).
bond(train,d145,d145_17,d145_20,1).
bond(train,d145,d145_17,d145_21,1).
bond(train,d145,d145_17,d145_22,1).
bond(train,d145,d145_11,d145_23,2).
bond(train,d145,d145_3,d145_24,1).
bond(train,d145,d145_24,d145_25,1).
bond(train,d145,d145_25,d145_26,1).
bond(train,d145,d145_24,d145_27,2).
bond(train,d145,d145_11,d145_28,1).
bond(train,d145,d145_28,d145_12,1).
bond(train,d145,d145_12,d145_29,1).
bond(train,d145,d145_12,d145_30,1).
bond(train,d145,d145_26,d145_31,1).
bond(train,d145,d145_26,d145_32,1).
bond(train,d145,d145_26,d145_33,1).
bond(train,d145,d145_31,d145_34,7).
bond(train,d145,d145_34,d145_35,7).
bond(train,d145,d145_35,d145_36,7).
bond(train,d145,d145_36,d145_37,7).
bond(train,d145,d145_37,d145_38,7).
bond(train,d145,d145_38,d145_31,7).
bond(train,d145,d145_34,d145_39,1).
bond(train,d145,d145_35,d145_40,1).
bond(train,d145,d145_36,d145_41,1).
bond(train,d145,d145_37,d145_42,1).
bond(train,d145,d145_38,d145_43,1).
bond(train,d146,d146_1,d146_2,7).
bond(train,d146,d146_2,d146_3,7).
bond(train,d146,d146_3,d146_4,7).
bond(train,d146,d146_4,d146_5,7).
bond(train,d146,d146_5,d146_6,7).
bond(train,d146,d146_6,d146_1,7).
bond(train,d146,d146_2,d146_7,1).
bond(train,d146,d146_5,d146_8,1).
bond(train,d146,d146_6,d146_9,1).
bond(train,d146,d146_4,d146_10,1).
bond(train,d146,d146_10,d146_11,1).
bond(train,d146,d146_11,d146_12,1).
bond(train,d146,d146_11,d146_13,1).
bond(train,d146,d146_11,d146_14,1).
bond(train,d146,d146_12,d146_3,1).
bond(train,d146,d146_1,d146_15,1).
bond(train,d146,d146_15,d146_16,1).
bond(train,d146,d146_15,d146_17,1).
bond(train,d146,d146_15,d146_18,1).
bond(train,d146,d146_16,d146_19,1).
bond(train,d146,d146_19,d146_20,1).
bond(train,d146,d146_19,d146_21,1).
bond(train,d146,d146_19,d146_22,1).
bond(train,d146,d146_16,d146_23,1).
bond(train,d146,d146_16,d146_24,1).
bond(train,d146,d146_23,d146_25,1).
bond(train,d146,d146_23,d146_26,2).
bond(train,d146,d146_25,d146_27,1).
bond(train,d146,d146_25,d146_28,1).
bond(train,d146,d146_25,d146_29,1).
bond(train,d146,d146_27,d146_30,1).
bond(train,d146,d146_27,d146_31,1).
bond(train,d146,d146_27,d146_32,1).
bond(train,d146,d146_30,d146_33,1).
bond(train,d146,d146_30,d146_34,1).
bond(train,d146,d146_30,d146_35,1).
bond(train,d146,d146_33,d146_36,1).
bond(train,d146,d146_33,d146_37,1).
bond(train,d146,d146_33,d146_38,1).
bond(train,d146,d146_36,d146_39,1).
bond(train,d146,d146_36,d146_40,1).
bond(train,d146,d146_36,d146_41,1).
bond(train,d146,d146_39,d146_42,1).
bond(train,d146,d146_39,d146_43,1).
bond(train,d146,d146_39,d146_44,1).
bond(train,d146,d146_42,d146_45,1).
bond(train,d146,d146_42,d146_46,1).
bond(train,d146,d146_42,d146_47,1).
bond(train,d146,d146_45,d146_48,1).
bond(train,d146,d146_45,d146_49,1).
bond(train,d146,d146_45,d146_50,1).
bond(train,d147,d147_1,d147_2,2).
bond(train,d147,d147_1,d147_3,1).
bond(train,d147,d147_3,d147_4,1).
bond(train,d147,d147_4,d147_5,1).
bond(train,d147,d147_4,d147_6,1).
bond(train,d147,d147_4,d147_7,1).
bond(train,d147,d147_5,d147_8,1).
bond(train,d147,d147_8,d147_9,1).
bond(train,d147,d147_8,d147_10,1).
bond(train,d147,d147_8,d147_11,1).
bond(train,d147,d147_5,d147_12,1).
bond(train,d147,d147_5,d147_13,1).
bond(train,d147,d147_12,d147_14,1).
bond(train,d147,d147_12,d147_15,1).
bond(train,d147,d147_12,d147_16,1).
bond(train,d147,d147_14,d147_17,1).
bond(train,d147,d147_14,d147_18,1).
bond(train,d147,d147_14,d147_19,1).
bond(train,d147,d147_9,d147_20,1).
bond(train,d147,d147_9,d147_21,1).
bond(train,d147,d147_9,d147_22,1).
bond(train,d147,d147_20,d147_23,1).
bond(train,d147,d147_20,d147_24,1).
bond(train,d147,d147_20,d147_25,1).
bond(train,d147,d147_23,d147_26,1).
bond(train,d147,d147_23,d147_27,1).
bond(train,d147,d147_23,d147_28,1).
bond(train,d147,d147_1,d147_29,1).
bond(train,d147,d147_29,d147_30,1).
bond(train,d147,d147_1,d147_31,1).
bond(train,d147,d147_31,d147_32,1).
bond(train,d147,d147_30,d147_33,1).
bond(train,d147,d147_30,d147_34,1).
bond(train,d147,d147_30,d147_35,1).
bond(train,d147,d147_33,d147_36,1).
bond(train,d147,d147_33,d147_37,1).
bond(train,d147,d147_33,d147_38,1).
bond(train,d147,d147_37,d147_39,1).
bond(train,d147,d147_37,d147_40,1).
bond(train,d147,d147_37,d147_41,1).
bond(train,d147,d147_39,d147_42,1).
bond(train,d147,d147_39,d147_43,1).
bond(train,d147,d147_39,d147_44,1).
bond(train,d147,d147_36,d147_45,1).
bond(train,d147,d147_36,d147_46,1).
bond(train,d147,d147_36,d147_47,1).
bond(train,d147,d147_45,d147_48,1).
bond(train,d147,d147_45,d147_49,1).
bond(train,d147,d147_45,d147_50,1).
bond(train,d147,d147_48,d147_51,1).
bond(train,d147,d147_48,d147_52,1).
bond(train,d147,d147_48,d147_53,1).
bond(train,d147,d147_51,d147_54,1).
bond(train,d147,d147_51,d147_55,1).
bond(train,d147,d147_51,d147_56,1).
bond(train,d147,d147_32,d147_57,1).
bond(train,d147,d147_32,d147_58,1).
bond(train,d147,d147_32,d147_59,1).
bond(train,d147,d147_57,d147_60,1).
bond(train,d147,d147_57,d147_61,1).
bond(train,d147,d147_57,d147_62,1).
bond(train,d147,d147_61,d147_63,1).
bond(train,d147,d147_61,d147_64,1).
bond(train,d147,d147_61,d147_65,1).
bond(train,d147,d147_63,d147_66,1).
bond(train,d147,d147_63,d147_67,1).
bond(train,d147,d147_63,d147_68,1).
bond(train,d147,d147_60,d147_69,1).
bond(train,d147,d147_60,d147_70,1).
bond(train,d147,d147_60,d147_71,1).
bond(train,d147,d147_69,d147_72,1).
bond(train,d147,d147_69,d147_73,1).
bond(train,d147,d147_69,d147_74,1).
bond(train,d147,d147_72,d147_75,1).
bond(train,d147,d147_72,d147_76,1).
bond(train,d147,d147_72,d147_77,1).
bond(train,d147,d147_75,d147_78,1).
bond(train,d147,d147_75,d147_79,1).
bond(train,d147,d147_75,d147_80,1).
bond(train,d148,d148_1,d148_2,7).
bond(train,d148,d148_2,d148_3,7).
bond(train,d148,d148_3,d148_4,7).
bond(train,d148,d148_4,d148_5,7).
bond(train,d148,d148_5,d148_6,7).
bond(train,d148,d148_6,d148_1,7).
bond(train,d148,d148_1,d148_7,1).
bond(train,d148,d148_2,d148_8,1).
bond(train,d148,d148_3,d148_9,1).
bond(train,d148,d148_4,d148_10,1).
bond(train,d148,d148_6,d148_11,1).
bond(train,d148,d148_5,d148_12,1).
bond(train,d148,d148_12,d148_13,1).
bond(train,d148,d148_13,d148_14,1).
bond(train,d148,d148_13,d148_15,1).
bond(train,d148,d148_13,d148_16,1).
bond(train,d148,d148_12,d148_17,1).
bond(train,d148,d148_12,d148_18,1).
bond(train,d148,d148_17,d148_19,1).
bond(train,d149,d149_1,d149_2,7).
bond(train,d149,d149_2,d149_3,7).
bond(train,d149,d149_3,d149_4,7).
bond(train,d149,d149_4,d149_5,7).
bond(train,d149,d149_5,d149_6,7).
bond(train,d149,d149_6,d149_1,7).
bond(train,d149,d149_1,d149_7,1).
bond(train,d149,d149_2,d149_8,1).
bond(train,d149,d149_4,d149_9,1).
bond(train,d149,d149_5,d149_10,1).
bond(train,d149,d149_6,d149_11,1).
bond(train,d149,d149_3,d149_12,1).
bond(train,d149,d149_12,d149_13,1).
bond(train,d149,d149_13,d149_14,7).
bond(train,d149,d149_14,d149_15,7).
bond(train,d149,d149_15,d149_16,7).
bond(train,d149,d149_16,d149_17,7).
bond(train,d149,d149_17,d149_18,7).
bond(train,d149,d149_18,d149_13,7).
bond(train,d149,d149_14,d149_19,1).
bond(train,d149,d149_15,d149_20,1).
bond(train,d149,d149_17,d149_21,1).
bond(train,d149,d149_18,d149_22,1).
bond(train,d149,d149_12,d149_23,1).
bond(train,d149,d149_23,d149_24,1).
bond(train,d149,d149_23,d149_25,1).
bond(train,d149,d149_23,d149_26,1).
bond(train,d149,d149_12,d149_27,1).
bond(train,d149,d149_27,d149_28,1).
bond(train,d149,d149_27,d149_29,1).
bond(train,d149,d149_27,d149_30,1).
bond(train,d149,d149_16,d149_31,1).
bond(train,d149,d149_11,d149_32,1).
bond(train,d149,d149_31,d149_33,1).
bond(train,d15,d15_1,d15_2,7).
bond(train,d15,d15_2,d15_3,7).
bond(train,d15,d15_3,d15_4,7).
bond(train,d15,d15_4,d15_5,7).
bond(train,d15,d15_5,d15_6,7).
bond(train,d15,d15_6,d15_1,7).
bond(train,d15,d15_1,d15_7,1).
bond(train,d15,d15_2,d15_8,1).
bond(train,d15,d15_4,d15_9,1).
bond(train,d15,d15_5,d15_10,1).
bond(train,d15,d15_3,d15_11,1).
bond(train,d15,d15_11,d15_12,1).
bond(train,d15,d15_11,d15_13,1).
bond(train,d15,d15_11,d15_14,1).
bond(train,d15,d15_12,d15_15,7).
bond(train,d15,d15_15,d15_16,7).
bond(train,d15,d15_16,d15_17,7).
bond(train,d15,d15_17,d15_18,7).
bond(train,d15,d15_18,d15_19,7).
bond(train,d15,d15_19,d15_12,7).
bond(train,d15,d15_15,d15_20,1).
bond(train,d15,d15_16,d15_21,1).
bond(train,d15,d15_18,d15_22,1).
bond(train,d15,d15_19,d15_23,1).
bond(train,d15,d15_6,d15_24,1).
bond(train,d15,d15_17,d15_25,1).
bond(train,d15,d15_24,d15_26,1).
bond(train,d15,d15_24,d15_27,1).
bond(train,d15,d15_25,d15_28,1).
bond(train,d15,d15_25,d15_29,1).
bond(train,d150,d150_1,d150_2,7).
bond(train,d150,d150_2,d150_3,7).
bond(train,d150,d150_3,d150_4,7).
bond(train,d150,d150_4,d150_5,7).
bond(train,d150,d150_5,d150_6,7).
bond(train,d150,d150_6,d150_1,7).
bond(train,d150,d150_1,d150_7,1).
bond(train,d150,d150_3,d150_8,1).
bond(train,d150,d150_4,d150_9,1).
bond(train,d150,d150_6,d150_10,1).
bond(train,d150,d150_5,d150_11,1).
bond(train,d150,d150_11,d150_12,1).
bond(train,d150,d150_12,d150_13,1).
bond(train,d150,d150_13,d150_14,1).
bond(train,d150,d150_13,d150_15,1).
bond(train,d150,d150_13,d150_16,1).
bond(train,d150,d150_2,d150_17,1).
bond(train,d150,d150_17,d150_18,1).
bond(train,d150,d150_18,d150_19,1).
bond(train,d150,d150_19,d150_20,1).
bond(train,d150,d150_19,d150_21,1).
bond(train,d150,d150_19,d150_22,1).
bond(train,d150,d150_11,d150_23,2).
bond(train,d150,d150_17,d150_24,2).
bond(train,d151,d151_1,d151_2,7).
bond(train,d151,d151_2,d151_3,7).
bond(train,d151,d151_3,d151_4,7).
bond(train,d151,d151_4,d151_5,7).
bond(train,d151,d151_5,d151_6,7).
bond(train,d151,d151_6,d151_1,7).
bond(train,d151,d151_4,d151_7,1).
bond(train,d151,d151_6,d151_8,1).
bond(train,d151,d151_1,d151_9,1).
bond(train,d151,d151_2,d151_10,1).
bond(train,d151,d151_3,d151_11,1).
bond(train,d151,d151_5,d151_12,1).
bond(train,d151,d151_12,d151_13,1).
bond(train,d151,d151_13,d151_14,1).
bond(train,d151,d151_12,d151_15,2).
bond(train,d151,d151_9,d151_16,1).
bond(train,d151,d151_10,d151_17,1).
bond(train,d151,d151_11,d151_18,1).
bond(train,d151,d151_14,d151_19,1).
bond(train,d151,d151_14,d151_20,1).
bond(train,d151,d151_14,d151_21,1).
bond(train,d151,d151_19,d151_22,1).
bond(train,d151,d151_19,d151_23,1).
bond(train,d151,d151_19,d151_24,1).
bond(train,d151,d151_22,d151_25,1).
bond(train,d151,d151_22,d151_26,1).
bond(train,d151,d151_22,d151_27,1).
bond(train,d152,d152_1,d152_2,7).
bond(train,d152,d152_2,d152_3,7).
bond(train,d152,d152_3,d152_4,7).
bond(train,d152,d152_4,d152_5,7).
bond(train,d152,d152_5,d152_6,7).
bond(train,d152,d152_6,d152_1,7).
bond(train,d152,d152_1,d152_7,1).
bond(train,d152,d152_2,d152_8,1).
bond(train,d152,d152_3,d152_9,1).
bond(train,d152,d152_5,d152_10,1).
bond(train,d152,d152_6,d152_11,1).
bond(train,d152,d152_4,d152_12,1).
bond(train,d152,d152_12,d152_13,1).
bond(train,d152,d152_12,d152_14,1).
bond(train,d152,d152_12,d152_15,1).
bond(train,d152,d152_14,d152_16,2).
bond(train,d152,d152_14,d152_17,1).
bond(train,d152,d152_17,d152_18,1).
bond(train,d152,d152_18,d152_19,1).
bond(train,d152,d152_19,d152_20,1).
bond(train,d152,d152_20,d152_21,1).
bond(train,d152,d152_21,d152_18,1).
bond(train,d152,d152_18,d152_22,1).
bond(train,d152,d152_19,d152_23,2).
bond(train,d152,d152_21,d152_24,1).
bond(train,d152,d152_21,d152_25,1).
bond(train,d152,d152_24,d152_26,1).
bond(train,d152,d152_20,d152_27,1).
bond(train,d152,d152_27,d152_26,1).
bond(train,d152,d152_27,d152_28,1).
bond(train,d152,d152_27,d152_29,1).
bond(train,d152,d152_28,d152_30,1).
bond(train,d152,d152_28,d152_31,2).
bond(train,d152,d152_26,d152_32,1).
bond(train,d152,d152_32,d152_33,1).
bond(train,d152,d152_32,d152_34,1).
bond(train,d152,d152_32,d152_35,1).
bond(train,d152,d152_26,d152_36,1).
bond(train,d152,d152_36,d152_37,1).
bond(train,d152,d152_36,d152_38,1).
bond(train,d152,d152_36,d152_39,1).
bond(train,d152,d152_13,d152_40,1).
bond(train,d152,d152_13,d152_41,1).
bond(train,d152,d152_17,d152_42,1).
bond(train,d152,d152_30,d152_43,1).
bond(train,d153,d153_1,d153_2,7).
bond(train,d153,d153_2,d153_3,7).
bond(train,d153,d153_3,d153_4,7).
bond(train,d153,d153_4,d153_5,7).
bond(train,d153,d153_5,d153_6,7).
bond(train,d153,d153_6,d153_1,7).
bond(train,d153,d153_1,d153_7,1).
bond(train,d153,d153_4,d153_8,1).
bond(train,d153,d153_6,d153_9,1).
bond(train,d153,d153_5,d153_10,1).
bond(train,d153,d153_10,d153_11,1).
bond(train,d153,d153_3,d153_12,1).
bond(train,d153,d153_2,d153_13,1).
bond(train,d153,d153_13,d153_14,1).
bond(train,d153,d153_13,d153_15,1).
bond(train,d153,d153_13,d153_16,1).
bond(train,d153,d153_14,d153_17,1).
bond(train,d153,d153_14,d153_18,1).
bond(train,d153,d153_14,d153_19,1).
bond(train,d153,d153_17,d153_20,1).
bond(train,d153,d153_17,d153_21,1).
bond(train,d153,d153_17,d153_22,1).
bond(train,d153,d153_20,d153_23,1).
bond(train,d153,d153_20,d153_24,1).
bond(train,d153,d153_20,d153_25,1).
bond(train,d153,d153_12,d153_26,1).
bond(train,d153,d153_23,d153_27,1).
bond(train,d153,d153_23,d153_28,1).
bond(train,d153,d153_23,d153_29,1).
bond(train,d153,d153_27,d153_30,1).
bond(train,d153,d153_27,d153_31,1).
bond(train,d153,d153_27,d153_32,1).
bond(train,d154,d154_1,d154_2,7).
bond(train,d154,d154_2,d154_3,7).
bond(train,d154,d154_3,d154_4,7).
bond(train,d154,d154_4,d154_5,7).
bond(train,d154,d154_5,d154_6,7).
bond(train,d154,d154_6,d154_1,7).
bond(train,d154,d154_1,d154_7,1).
bond(train,d154,d154_6,d154_8,1).
bond(train,d154,d154_5,d154_9,1).
bond(train,d154,d154_2,d154_10,1).
bond(train,d154,d154_10,d154_11,1).
bond(train,d154,d154_10,d154_12,1).
bond(train,d154,d154_10,d154_13,1).
bond(train,d154,d154_9,d154_14,1).
bond(train,d154,d154_3,d154_15,1).
bond(train,d154,d154_4,d154_16,1).
bond(train,d154,d154_16,d154_17,1).
bond(train,d154,d154_11,d154_18,1).
bond(train,d154,d154_18,d154_19,1).
bond(train,d154,d154_18,d154_20,1).
bond(train,d154,d154_18,d154_21,1).
bond(train,d154,d154_11,d154_22,1).
bond(train,d154,d154_22,d154_23,1).
bond(train,d154,d154_22,d154_24,1).
bond(train,d154,d154_11,d154_25,1).
bond(train,d154,d154_25,d154_26,1).
bond(train,d154,d154_25,d154_27,2).
bond(train,d154,d154_26,d154_28,1).
bond(train,d155,d155_1,d155_2,7).
bond(train,d155,d155_2,d155_3,7).
bond(train,d155,d155_3,d155_4,7).
bond(train,d155,d155_4,d155_5,7).
bond(train,d155,d155_5,d155_6,7).
bond(train,d155,d155_6,d155_1,7).
bond(train,d155,d155_1,d155_7,1).
bond(train,d155,d155_5,d155_8,1).
bond(train,d155,d155_6,d155_9,1).
bond(train,d155,d155_3,d155_10,1).
bond(train,d155,d155_10,d155_11,1).
bond(train,d155,d155_11,d155_12,1).
bond(train,d155,d155_12,d155_13,1).
bond(train,d155,d155_13,d155_4,1).
bond(train,d155,d155_11,d155_14,2).
bond(train,d155,d155_14,d155_15,1).
bond(train,d155,d155_15,d155_16,1).
bond(train,d155,d155_16,d155_17,1).
bond(train,d155,d155_17,d155_12,1).
bond(train,d155,d155_12,d155_18,1).
bond(train,d155,d155_15,d155_19,1).
bond(train,d155,d155_19,d155_20,1).
bond(train,d155,d155_20,d155_21,2).
bond(train,d155,d155_21,d155_22,1).
bond(train,d155,d155_22,d155_16,1).
bond(train,d155,d155_15,d155_23,1).
bond(train,d155,d155_16,d155_24,1).
bond(train,d155,d155_13,d155_25,1).
bond(train,d155,d155_13,d155_26,1).
bond(train,d155,d155_26,d155_27,1).
bond(train,d155,d155_26,d155_28,1).
bond(train,d155,d155_26,d155_29,1).
bond(train,d155,d155_17,d155_30,1).
bond(train,d155,d155_17,d155_31,1).
bond(train,d155,d155_22,d155_32,1).
bond(train,d155,d155_22,d155_33,1).
bond(train,d155,d155_32,d155_34,1).
bond(train,d155,d155_34,d155_35,1).
bond(train,d155,d155_34,d155_36,1).
bond(train,d155,d155_34,d155_37,1).
bond(train,d155,d155_32,d155_38,1).
bond(train,d155,d155_38,d155_39,1).
bond(train,d155,d155_38,d155_40,1).
bond(train,d155,d155_38,d155_41,1).
bond(train,d155,d155_21,d155_42,1).
bond(train,d155,d155_20,d155_43,1).
bond(train,d155,d155_43,d155_44,1).
bond(train,d155,d155_43,d155_45,1).
bond(train,d155,d155_43,d155_46,1).
bond(train,d155,d155_44,d155_47,1).
bond(train,d155,d155_14,d155_48,1).
bond(train,d155,d155_2,d155_49,1).
bond(train,d155,d155_10,d155_50,2).
bond(train,d155,d155_19,d155_51,2).
bond(train,d155,d155_48,d155_52,1).
bond(train,d155,d155_30,d155_53,1).
bond(train,d155,d155_25,d155_54,1).
bond(train,d155,d155_42,d155_55,1).
bond(train,d155,d155_47,d155_56,1).
bond(train,d155,d155_47,d155_57,1).
bond(train,d155,d155_49,d155_58,1).
bond(train,d156,d156_1,d156_2,7).
bond(train,d156,d156_2,d156_3,7).
bond(train,d156,d156_3,d156_4,7).
bond(train,d156,d156_4,d156_5,7).
bond(train,d156,d156_5,d156_6,7).
bond(train,d156,d156_6,d156_1,7).
bond(train,d156,d156_1,d156_7,1).
bond(train,d156,d156_4,d156_8,1).
bond(train,d156,d156_3,d156_9,1).
bond(train,d156,d156_2,d156_10,1).
bond(train,d156,d156_10,d156_11,1).
bond(train,d156,d156_11,d156_12,1).
bond(train,d156,d156_11,d156_13,1).
bond(train,d156,d156_11,d156_14,1).
bond(train,d156,d156_12,d156_9,1).
bond(train,d156,d156_9,d156_15,1).
bond(train,d156,d156_12,d156_16,1).
bond(train,d156,d156_9,d156_17,1).
bond(train,d156,d156_17,d156_18,1).
bond(train,d156,d156_12,d156_19,1).
bond(train,d156,d156_19,d156_20,1).
bond(train,d156,d156_20,d156_21,7).
bond(train,d156,d156_21,d156_22,7).
bond(train,d156,d156_22,d156_23,7).
bond(train,d156,d156_23,d156_24,7).
bond(train,d156,d156_24,d156_18,7).
bond(train,d156,d156_18,d156_20,7).
bond(train,d156,d156_23,d156_25,1).
bond(train,d156,d156_24,d156_26,1).
bond(train,d156,d156_21,d156_27,1).
bond(train,d156,d156_27,d156_28,1).
bond(train,d156,d156_28,d156_29,1).
bond(train,d156,d156_29,d156_22,1).
bond(train,d156,d156_27,d156_30,1).
bond(train,d156,d156_27,d156_31,1).
bond(train,d156,d156_28,d156_32,1).
bond(train,d156,d156_32,d156_33,2).
bond(train,d156,d156_33,d156_34,1).
bond(train,d156,d156_33,d156_35,1).
bond(train,d156,d156_32,d156_36,1).
bond(train,d156,d156_36,d156_37,1).
bond(train,d156,d156_36,d156_38,1).
bond(train,d156,d156_36,d156_39,1).
bond(train,d156,d156_6,d156_40,1).
bond(train,d156,d156_40,d156_41,1).
bond(train,d156,d156_41,d156_42,1).
bond(train,d156,d156_41,d156_43,1).
bond(train,d156,d156_41,d156_44,1).
bond(train,d156,d156_5,d156_45,1).
bond(train,d156,d156_45,d156_46,1).
bond(train,d156,d156_46,d156_47,1).
bond(train,d156,d156_46,d156_48,1).
bond(train,d156,d156_46,d156_49,1).
bond(train,d156,d156_17,d156_50,2).
bond(train,d156,d156_28,d156_51,1).
bond(train,d157,d157_1,d157_2,7).
bond(train,d157,d157_2,d157_3,7).
bond(train,d157,d157_3,d157_4,7).
bond(train,d157,d157_4,d157_5,7).
bond(train,d157,d157_5,d157_6,7).
bond(train,d157,d157_6,d157_1,7).
bond(train,d157,d157_1,d157_7,1).
bond(train,d157,d157_2,d157_8,1).
bond(train,d157,d157_3,d157_9,1).
bond(train,d157,d157_5,d157_10,1).
bond(train,d157,d157_6,d157_11,1).
bond(train,d157,d157_12,d157_13,7).
bond(train,d157,d157_13,d157_14,7).
bond(train,d157,d157_14,d157_15,7).
bond(train,d157,d157_15,d157_16,7).
bond(train,d157,d157_16,d157_17,7).
bond(train,d157,d157_17,d157_12,7).
bond(train,d157,d157_12,d157_18,1).
bond(train,d157,d157_13,d157_19,1).
bond(train,d157,d157_14,d157_20,1).
bond(train,d157,d157_15,d157_21,1).
bond(train,d157,d157_17,d157_22,1).
bond(train,d157,d157_4,d157_23,1).
bond(train,d157,d157_16,d157_23,1).
bond(train,d157,d157_23,d157_24,1).
bond(train,d157,d157_23,d157_25,1).
bond(train,d157,d157_24,d157_26,1).
bond(train,d157,d157_26,d157_27,1).
bond(train,d157,d157_26,d157_28,1).
bond(train,d157,d157_26,d157_29,1).
bond(train,d157,d157_27,d157_30,1).
bond(train,d157,d157_27,d157_31,1).
bond(train,d157,d157_27,d157_32,1).
bond(train,d157,d157_30,d157_33,1).
bond(train,d157,d157_33,d157_34,1).
bond(train,d157,d157_33,d157_35,1).
bond(train,d157,d157_33,d157_36,1).
bond(train,d157,d157_30,d157_37,1).
bond(train,d157,d157_37,d157_38,1).
bond(train,d157,d157_37,d157_39,1).
bond(train,d157,d157_37,d157_40,1).
bond(train,d158,d158_1,d158_2,1).
bond(train,d159,d159_1,d159_2,7).
bond(train,d159,d159_2,d159_3,7).
bond(train,d159,d159_3,d159_4,7).
bond(train,d159,d159_4,d159_5,7).
bond(train,d159,d159_5,d159_6,7).
bond(train,d159,d159_6,d159_1,7).
bond(train,d159,d159_2,d159_7,1).
bond(train,d159,d159_3,d159_8,1).
bond(train,d159,d159_5,d159_9,1).
bond(train,d159,d159_6,d159_10,1).
bond(train,d159,d159_1,d159_11,1).
bond(train,d159,d159_11,d159_12,1).
bond(train,d159,d159_12,d159_13,1).
bond(train,d159,d159_12,d159_14,1).
bond(train,d159,d159_12,d159_15,1).
bond(train,d159,d159_11,d159_16,2).
bond(train,d159,d159_4,d159_17,1).
bond(train,d159,d159_18,d159_19,1).
bond(train,d159,d159_19,d159_20,1).
bond(train,d159,d159_20,d159_21,1).
bond(train,d159,d159_21,d159_22,1).
bond(train,d159,d159_22,d159_23,1).
bond(train,d159,d159_23,d159_24,1).
bond(train,d159,d159_24,d159_25,1).
bond(train,d159,d159_25,d159_20,1).
bond(train,d159,d159_20,d159_26,1).
bond(train,d159,d159_21,d159_27,1).
bond(train,d159,d159_21,d159_28,1).
bond(train,d159,d159_22,d159_29,1).
bond(train,d159,d159_22,d159_30,1).
bond(train,d159,d159_23,d159_31,1).
bond(train,d159,d159_23,d159_32,1).
bond(train,d159,d159_24,d159_33,1).
bond(train,d159,d159_24,d159_34,1).
bond(train,d159,d159_25,d159_35,1).
bond(train,d159,d159_25,d159_36,1).
bond(train,d159,d159_19,d159_37,1).
bond(train,d159,d159_18,d159_38,2).
bond(train,d159,d159_17,d159_39,2).
bond(train,d159,d159_17,d159_40,2).
bond(train,d159,d159_17,d159_41,1).
bond(train,d159,d159_41,d159_18,1).
bond(train,d159,d159_41,d159_42,1).
bond(train,d16,d16_1,d16_2,7).
bond(train,d16,d16_2,d16_3,7).
bond(train,d16,d16_3,d16_4,7).
bond(train,d16,d16_4,d16_5,7).
bond(train,d16,d16_5,d16_6,7).
bond(train,d16,d16_6,d16_1,7).
bond(train,d16,d16_1,d16_7,1).
bond(train,d16,d16_2,d16_8,1).
bond(train,d16,d16_5,d16_9,1).
bond(train,d16,d16_6,d16_10,1).
bond(train,d16,d16_3,d16_11,1).
bond(train,d16,d16_11,d16_12,1).
bond(train,d16,d16_12,d16_13,7).
bond(train,d16,d16_13,d16_14,1).
bond(train,d16,d16_14,d16_4,1).
bond(train,d16,d16_12,d16_15,7).
bond(train,d16,d16_15,d16_16,7).
bond(train,d16,d16_16,d16_17,7).
bond(train,d16,d16_17,d16_18,7).
bond(train,d16,d16_18,d16_13,7).
bond(train,d16,d16_15,d16_19,1).
bond(train,d16,d16_16,d16_20,1).
bond(train,d16,d16_14,d16_21,2).
bond(train,d16,d16_11,d16_22,2).
bond(train,d16,d16_18,d16_23,1).
bond(train,d16,d16_17,d16_24,1).
bond(train,d16,d16_24,d16_25,1).
bond(train,d16,d16_24,d16_26,1).
bond(train,d16,d16_24,d16_27,1).
bond(train,d16,d16_23,d16_28,2).
bond(train,d16,d16_23,d16_29,2).
bond(train,d160,d160_1,d160_2,1).
bond(train,d160,d160_2,d160_3,2).
bond(train,d160,d160_3,d160_4,1).
bond(train,d160,d160_4,d160_5,1).
bond(train,d160,d160_5,d160_1,1).
bond(train,d160,d160_2,d160_6,1).
bond(train,d160,d160_3,d160_7,1).
bond(train,d160,d160_1,d160_8,1).
bond(train,d160,d160_1,d160_9,1).
bond(train,d160,d160_8,d160_10,1).
bond(train,d160,d160_8,d160_11,1).
bond(train,d160,d160_8,d160_12,1).
bond(train,d160,d160_11,d160_13,1).
bond(train,d160,d160_11,d160_14,1).
bond(train,d160,d160_11,d160_15,1).
bond(train,d160,d160_4,d160_16,2).
bond(train,d160,d160_6,d160_17,1).
bond(train,d160,d160_7,d160_18,1).
bond(train,d160,d160_10,d160_19,1).
bond(train,d160,d160_13,d160_20,1).
bond(train,d161,d161_1,d161_2,7).
bond(train,d161,d161_2,d161_3,7).
bond(train,d161,d161_3,d161_4,7).
bond(train,d161,d161_4,d161_5,7).
bond(train,d161,d161_5,d161_6,7).
bond(train,d161,d161_6,d161_1,7).
bond(train,d161,d161_4,d161_7,1).
bond(train,d161,d161_6,d161_8,1).
bond(train,d161,d161_5,d161_9,1).
bond(train,d161,d161_9,d161_10,1).
bond(train,d161,d161_9,d161_11,1).
bond(train,d161,d161_9,d161_12,1).
bond(train,d161,d161_3,d161_13,1).
bond(train,d161,d161_13,d161_14,1).
bond(train,d161,d161_14,d161_15,1).
bond(train,d161,d161_14,d161_16,1).
bond(train,d161,d161_14,d161_17,1).
bond(train,d161,d161_13,d161_18,1).
bond(train,d161,d161_18,d161_19,1).
bond(train,d161,d161_18,d161_20,1).
bond(train,d161,d161_18,d161_21,1).
bond(train,d161,d161_13,d161_22,1).
bond(train,d161,d161_22,d161_23,1).
bond(train,d161,d161_22,d161_24,1).
bond(train,d161,d161_22,d161_25,1).
bond(train,d161,d161_2,d161_26,1).
bond(train,d161,d161_1,d161_27,1).
bond(train,d161,d161_27,d161_28,1).
bond(train,d161,d161_28,d161_29,1).
bond(train,d161,d161_28,d161_30,1).
bond(train,d161,d161_28,d161_31,1).
bond(train,d161,d161_27,d161_32,1).
bond(train,d161,d161_32,d161_33,1).
bond(train,d161,d161_32,d161_34,1).
bond(train,d161,d161_32,d161_35,1).
bond(train,d161,d161_27,d161_36,1).
bond(train,d161,d161_36,d161_37,1).
bond(train,d161,d161_36,d161_38,1).
bond(train,d161,d161_36,d161_39,1).
bond(train,d161,d161_26,d161_40,1).
bond(train,d162,d162_1,d162_2,7).
bond(train,d162,d162_2,d162_3,7).
bond(train,d162,d162_3,d162_4,7).
bond(train,d162,d162_4,d162_5,7).
bond(train,d162,d162_5,d162_6,7).
bond(train,d162,d162_6,d162_1,7).
bond(train,d162,d162_1,d162_7,1).
bond(train,d162,d162_2,d162_8,1).
bond(train,d162,d162_5,d162_9,1).
bond(train,d162,d162_6,d162_10,1).
bond(train,d162,d162_11,d162_12,7).
bond(train,d162,d162_12,d162_13,7).
bond(train,d162,d162_13,d162_14,7).
bond(train,d162,d162_14,d162_15,7).
bond(train,d162,d162_15,d162_16,7).
bond(train,d162,d162_16,d162_11,7).
bond(train,d162,d162_12,d162_17,1).
bond(train,d162,d162_13,d162_18,1).
bond(train,d162,d162_14,d162_19,1).
bond(train,d162,d162_15,d162_20,1).
bond(train,d162,d162_4,d162_21,1).
bond(train,d162,d162_21,d162_16,1).
bond(train,d162,d162_3,d162_22,1).
bond(train,d162,d162_22,d162_11,1).
bond(train,d163,d163_1,d163_2,1).
bond(train,d163,d163_1,d163_3,1).
bond(train,d163,d163_1,d163_4,1).
bond(train,d163,d163_1,d163_5,1).
bond(train,d163,d163_3,d163_6,1).
bond(train,d163,d163_3,d163_7,1).
bond(train,d163,d163_6,d163_8,1).
bond(train,d163,d163_6,d163_9,2).
bond(train,d163,d163_8,d163_10,1).
bond(train,d163,d163_7,d163_11,2).
bond(train,d163,d163_7,d163_12,1).
bond(train,d163,d163_12,d163_13,1).
bond(train,d163,d163_2,d163_14,1).
bond(train,d163,d163_2,d163_15,1).
bond(train,d163,d163_2,d163_16,1).
bond(train,d163,d163_14,d163_17,1).
bond(train,d163,d163_14,d163_18,1).
bond(train,d163,d163_18,d163_19,2).
bond(train,d163,d163_18,d163_20,1).
bond(train,d163,d163_20,d163_21,1).
bond(train,d163,d163_17,d163_22,2).
bond(train,d163,d163_17,d163_23,1).
bond(train,d163,d163_23,d163_24,1).
bond(train,d164,d164_1,d164_2,1).
bond(train,d164,d164_2,d164_3,1).
bond(train,d164,d164_3,d164_4,1).
bond(train,d164,d164_4,d164_5,1).
bond(train,d164,d164_5,d164_6,1).
bond(train,d164,d164_6,d164_1,1).
bond(train,d164,d164_1,d164_7,1).
bond(train,d164,d164_8,d164_9,1).
bond(train,d164,d164_9,d164_10,1).
bond(train,d164,d164_10,d164_11,1).
bond(train,d164,d164_11,d164_12,1).
bond(train,d164,d164_12,d164_13,1).
bond(train,d164,d164_13,d164_8,1).
bond(train,d164,d164_7,d164_11,1).
bond(train,d164,d164_11,d164_14,1).
bond(train,d164,d164_1,d164_15,1).
bond(train,d164,d164_6,d164_16,1).
bond(train,d164,d164_4,d164_17,1).
bond(train,d164,d164_3,d164_18,1).
bond(train,d164,d164_2,d164_19,1).
bond(train,d164,d164_19,d164_20,1).
bond(train,d164,d164_19,d164_21,1).
bond(train,d164,d164_19,d164_22,1).
bond(train,d164,d164_10,d164_23,1).
bond(train,d164,d164_9,d164_24,1).
bond(train,d164,d164_8,d164_25,1).
bond(train,d164,d164_13,d164_26,1).
bond(train,d164,d164_3,d164_27,1).
bond(train,d164,d164_2,d164_28,1).
bond(train,d164,d164_10,d164_29,1).
bond(train,d164,d164_9,d164_30,1).
bond(train,d164,d164_8,d164_31,1).
bond(train,d164,d164_13,d164_32,1).
bond(train,d164,d164_28,d164_33,1).
bond(train,d164,d164_18,d164_34,1).
bond(train,d164,d164_29,d164_35,1).
bond(train,d164,d164_24,d164_36,1).
bond(train,d164,d164_25,d164_37,1).
bond(train,d164,d164_26,d164_38,1).
bond(train,d164,d164_26,d164_39,1).
bond(train,d164,d164_26,d164_40,1).
bond(train,d164,d164_38,d164_41,1).
bond(train,d164,d164_33,d164_6,1).
bond(train,d164,d164_33,d164_42,1).
bond(train,d164,d164_33,d164_43,1).
bond(train,d164,d164_4,d164_44,1).
bond(train,d165,d165_1,d165_2,7).
bond(train,d165,d165_2,d165_3,7).
bond(train,d165,d165_3,d165_4,7).
bond(train,d165,d165_4,d165_5,7).
bond(train,d165,d165_5,d165_6,7).
bond(train,d165,d165_6,d165_1,7).
bond(train,d165,d165_1,d165_7,1).
bond(train,d165,d165_2,d165_8,1).
bond(train,d165,d165_3,d165_9,1).
bond(train,d165,d165_5,d165_10,1).
bond(train,d165,d165_6,d165_11,1).
bond(train,d165,d165_4,d165_12,1).
bond(train,d165,d165_12,d165_13,1).
bond(train,d165,d165_13,d165_14,1).
bond(train,d165,d165_14,d165_15,7).
bond(train,d165,d165_15,d165_16,7).
bond(train,d165,d165_16,d165_17,7).
bond(train,d165,d165_17,d165_18,7).
bond(train,d165,d165_18,d165_19,7).
bond(train,d165,d165_19,d165_14,7).
bond(train,d165,d165_15,d165_20,1).
bond(train,d165,d165_16,d165_21,1).
bond(train,d165,d165_17,d165_22,1).
bond(train,d165,d165_18,d165_23,1).
bond(train,d165,d165_19,d165_24,1).
bond(train,d165,d165_12,d165_25,1).
bond(train,d165,d165_12,d165_26,1).
bond(train,d165,d165_13,d165_27,2).
bond(train,d165,d165_25,d165_28,1).
bond(train,d166,d166_1,d166_2,1).
bond(train,d166,d166_1,d166_3,1).
bond(train,d166,d166_1,d166_4,1).
bond(train,d166,d166_1,d166_5,1).
bond(train,d166,d166_3,d166_6,1).
bond(train,d166,d166_3,d166_7,1).
bond(train,d166,d166_3,d166_8,1).
bond(train,d166,d166_6,d166_9,1).
bond(train,d166,d166_6,d166_10,1).
bond(train,d166,d166_6,d166_11,1).
bond(train,d166,d166_9,d166_12,1).
bond(train,d166,d166_12,d166_13,1).
bond(train,d166,d166_13,d166_2,1).
bond(train,d166,d166_13,d166_14,1).
bond(train,d166,d166_13,d166_15,1).
bond(train,d166,d166_2,d166_16,1).
bond(train,d166,d166_2,d166_17,1).
bond(train,d166,d166_12,d166_18,2).
bond(train,d166,d166_9,d166_19,1).
bond(train,d167,d167_1,d167_2,7).
bond(train,d167,d167_2,d167_3,7).
bond(train,d167,d167_3,d167_4,7).
bond(train,d167,d167_4,d167_5,7).
bond(train,d167,d167_5,d167_6,7).
bond(train,d167,d167_6,d167_1,7).
bond(train,d167,d167_1,d167_7,1).
bond(train,d167,d167_2,d167_8,1).
bond(train,d167,d167_3,d167_9,1).
bond(train,d167,d167_5,d167_10,1).
bond(train,d167,d167_6,d167_11,1).
bond(train,d167,d167_4,d167_12,1).
bond(train,d167,d167_12,d167_13,1).
bond(train,d167,d167_12,d167_14,1).
bond(train,d167,d167_12,d167_15,1).
bond(train,d167,d167_14,d167_16,1).
bond(train,d167,d167_16,d167_17,1).
bond(train,d167,d167_16,d167_18,1).
bond(train,d167,d167_16,d167_19,1).
bond(train,d167,d167_14,d167_20,1).
bond(train,d167,d167_14,d167_21,1).
bond(train,d167,d167_20,d167_22,1).
bond(train,d167,d167_13,d167_23,1).
bond(train,d167,d167_22,d167_24,1).
bond(train,d167,d167_22,d167_25,1).
bond(train,d167,d167_22,d167_26,1).
bond(train,d167,d167_20,d167_27,1).
bond(train,d168,d168_1,d168_2,1).
bond(train,d168,d168_2,d168_3,1).
bond(train,d168,d168_3,d168_4,1).
bond(train,d168,d168_4,d168_5,1).
bond(train,d168,d168_5,d168_6,1).
bond(train,d168,d168_6,d168_1,1).
bond(train,d168,d168_1,d168_7,1).
bond(train,d168,d168_1,d168_8,1).
bond(train,d168,d168_4,d168_9,1).
bond(train,d168,d168_4,d168_10,1).
bond(train,d168,d168_6,d168_11,1).
bond(train,d168,d168_6,d168_12,1).
bond(train,d168,d168_5,d168_13,1).
bond(train,d168,d168_5,d168_14,1).
bond(train,d168,d168_13,d168_15,1).
bond(train,d168,d168_13,d168_16,1).
bond(train,d168,d168_13,d168_17,1).
bond(train,d168,d168_3,d168_18,1).
bond(train,d168,d168_3,d168_19,1).
bond(train,d168,d168_2,d168_20,1).
bond(train,d168,d168_2,d168_21,1).
bond(train,d168,d168_20,d168_22,1).
bond(train,d168,d168_22,d168_23,1).
bond(train,d168,d168_22,d168_24,1).
bond(train,d168,d168_22,d168_25,1).
bond(train,d168,d168_20,d168_26,1).
bond(train,d168,d168_20,d168_27,1).
bond(train,d168,d168_26,d168_28,1).
bond(train,d168,d168_26,d168_29,1).
bond(train,d168,d168_26,d168_30,1).
bond(train,d168,d168_18,d168_31,1).
bond(train,d169,d169_1,d169_2,7).
bond(train,d169,d169_2,d169_3,7).
bond(train,d169,d169_3,d169_4,7).
bond(train,d169,d169_4,d169_5,7).
bond(train,d169,d169_5,d169_6,7).
bond(train,d169,d169_6,d169_1,7).
bond(train,d169,d169_1,d169_7,1).
bond(train,d169,d169_2,d169_8,1).
bond(train,d169,d169_3,d169_9,1).
bond(train,d169,d169_4,d169_10,1).
bond(train,d169,d169_6,d169_11,1).
bond(train,d169,d169_5,d169_12,1).
bond(train,d169,d169_12,d169_13,1).
bond(train,d17,d17_1,d17_2,7).
bond(train,d17,d17_2,d17_3,7).
bond(train,d17,d17_3,d17_4,7).
bond(train,d17,d17_4,d17_5,7).
bond(train,d17,d17_5,d17_6,7).
bond(train,d17,d17_6,d17_1,7).
bond(train,d17,d17_1,d17_7,1).
bond(train,d17,d17_5,d17_8,1).
bond(train,d17,d17_6,d17_9,1).
bond(train,d17,d17_3,d17_10,7).
bond(train,d17,d17_10,d17_11,7).
bond(train,d17,d17_11,d17_12,7).
bond(train,d17,d17_12,d17_13,7).
bond(train,d17,d17_13,d17_4,7).
bond(train,d17,d17_10,d17_14,1).
bond(train,d17,d17_11,d17_15,1).
bond(train,d17,d17_12,d17_16,1).
bond(train,d17,d17_2,d17_17,1).
bond(train,d17,d17_13,d17_18,1).
bond(train,d17,d17_18,d17_19,1).
bond(train,d17,d17_18,d17_20,1).
bond(train,d17,d17_17,d17_21,1).
bond(train,d17,d17_17,d17_22,1).
bond(train,d170,d170_1,d170_2,7).
bond(train,d170,d170_2,d170_3,7).
bond(train,d170,d170_3,d170_4,7).
bond(train,d170,d170_4,d170_5,7).
bond(train,d170,d170_5,d170_6,7).
bond(train,d170,d170_6,d170_1,7).
bond(train,d170,d170_1,d170_7,1).
bond(train,d170,d170_2,d170_8,1).
bond(train,d170,d170_5,d170_9,1).
bond(train,d170,d170_6,d170_10,1).
bond(train,d170,d170_3,d170_11,1).
bond(train,d170,d170_11,d170_12,1).
bond(train,d170,d170_12,d170_13,1).
bond(train,d170,d170_13,d170_4,1).
bond(train,d170,d170_13,d170_14,2).
bond(train,d170,d170_11,d170_15,2).
bond(train,d171,d171_1,d171_2,7).
bond(train,d171,d171_2,d171_3,7).
bond(train,d171,d171_3,d171_4,7).
bond(train,d171,d171_4,d171_5,7).
bond(train,d171,d171_5,d171_6,7).
bond(train,d171,d171_6,d171_1,7).
bond(train,d171,d171_2,d171_7,1).
bond(train,d171,d171_3,d171_8,1).
bond(train,d171,d171_5,d171_9,1).
bond(train,d171,d171_6,d171_10,1).
bond(train,d171,d171_1,d171_11,1).
bond(train,d171,d171_11,d171_12,1).
bond(train,d171,d171_11,d171_13,1).
bond(train,d171,d171_11,d171_14,1).
bond(train,d171,d171_4,d171_15,1).
bond(train,d171,d171_15,d171_16,1).
bond(train,d171,d171_16,d171_17,1).
bond(train,d171,d171_17,d171_18,1).
bond(train,d171,d171_15,d171_19,2).
bond(train,d171,d171_15,d171_20,2).
bond(train,d171,d171_16,d171_21,1).
bond(train,d171,d171_17,d171_22,2).
bond(train,d171,d171_23,d171_24,1).
bond(train,d171,d171_24,d171_25,1).
bond(train,d171,d171_25,d171_26,1).
bond(train,d171,d171_26,d171_27,1).
bond(train,d171,d171_27,d171_28,1).
bond(train,d171,d171_28,d171_29,1).
bond(train,d171,d171_29,d171_23,1).
bond(train,d171,d171_23,d171_30,1).
bond(train,d171,d171_23,d171_31,1).
bond(train,d171,d171_24,d171_32,1).
bond(train,d171,d171_24,d171_33,1).
bond(train,d171,d171_25,d171_34,1).
bond(train,d171,d171_25,d171_35,1).
bond(train,d171,d171_26,d171_36,1).
bond(train,d171,d171_26,d171_37,1).
bond(train,d171,d171_27,d171_38,1).
bond(train,d171,d171_27,d171_39,1).
bond(train,d171,d171_28,d171_40,1).
bond(train,d171,d171_28,d171_41,1).
bond(train,d171,d171_18,d171_29,1).
bond(train,d171,d171_18,d171_42,1).
bond(train,d172,d172_1,d172_2,1).
bond(train,d172,d172_1,d172_3,1).
bond(train,d172,d172_1,d172_4,1).
bond(train,d172,d172_1,d172_5,1).
bond(train,d172,d172_3,d172_6,7).
bond(train,d172,d172_6,d172_7,7).
bond(train,d172,d172_7,d172_8,7).
bond(train,d172,d172_8,d172_9,7).
bond(train,d172,d172_9,d172_10,7).
bond(train,d172,d172_10,d172_3,7).
bond(train,d172,d172_6,d172_11,1).
bond(train,d172,d172_7,d172_12,1).
bond(train,d172,d172_8,d172_13,1).
bond(train,d172,d172_9,d172_14,1).
bond(train,d172,d172_10,d172_15,1).
bond(train,d172,d172_4,d172_16,7).
bond(train,d172,d172_16,d172_17,7).
bond(train,d172,d172_17,d172_18,7).
bond(train,d172,d172_18,d172_19,7).
bond(train,d172,d172_19,d172_20,7).
bond(train,d172,d172_20,d172_4,7).
bond(train,d172,d172_16,d172_21,1).
bond(train,d172,d172_17,d172_22,1).
bond(train,d172,d172_18,d172_23,1).
bond(train,d172,d172_19,d172_24,1).
bond(train,d172,d172_20,d172_25,1).
bond(train,d172,d172_5,d172_26,7).
bond(train,d172,d172_26,d172_27,7).
bond(train,d172,d172_27,d172_28,7).
bond(train,d172,d172_28,d172_29,7).
bond(train,d172,d172_29,d172_30,7).
bond(train,d172,d172_30,d172_5,7).
bond(train,d172,d172_26,d172_31,1).
bond(train,d172,d172_27,d172_32,1).
bond(train,d172,d172_28,d172_33,1).
bond(train,d172,d172_29,d172_34,1).
bond(train,d172,d172_30,d172_35,1).
bond(train,d172,d172_2,d172_36,1).
bond(train,d173,d173_1,d173_2,1).
bond(train,d173,d173_2,d173_3,1).
bond(train,d173,d173_3,d173_4,1).
bond(train,d173,d173_4,d173_5,1).
bond(train,d173,d173_5,d173_6,1).
bond(train,d173,d173_6,d173_1,1).
bond(train,d173,d173_2,d173_7,1).
bond(train,d173,d173_2,d173_8,1).
bond(train,d173,d173_5,d173_9,1).
bond(train,d173,d173_5,d173_10,1).
bond(train,d173,d173_6,d173_11,1).
bond(train,d173,d173_6,d173_12,1).
bond(train,d173,d173_3,d173_13,1).
bond(train,d173,d173_13,d173_14,1).
bond(train,d173,d173_14,d173_15,1).
bond(train,d173,d173_15,d173_16,1).
bond(train,d173,d173_16,d173_4,1).
bond(train,d173,d173_3,d173_17,1).
bond(train,d173,d173_13,d173_18,1).
bond(train,d173,d173_13,d173_19,1).
bond(train,d173,d173_14,d173_20,1).
bond(train,d173,d173_14,d173_21,1).
bond(train,d173,d173_15,d173_22,1).
bond(train,d173,d173_22,d173_23,1).
bond(train,d173,d173_23,d173_24,1).
bond(train,d173,d173_24,d173_25,1).
bond(train,d173,d173_25,d173_16,1).
bond(train,d173,d173_16,d173_26,1).
bond(train,d173,d173_15,d173_27,1).
bond(train,d173,d173_24,d173_28,1).
bond(train,d173,d173_24,d173_29,1).
bond(train,d173,d173_25,d173_30,1).
bond(train,d173,d173_25,d173_31,1).
bond(train,d173,d173_32,d173_33,1).
bond(train,d173,d173_33,d173_34,1).
bond(train,d173,d173_33,d173_35,1).
bond(train,d173,d173_33,d173_36,1).
bond(train,d173,d173_34,d173_37,1).
bond(train,d173,d173_34,d173_38,1).
bond(train,d173,d173_23,d173_32,1).
bond(train,d173,d173_22,d173_34,1).
bond(train,d173,d173_22,d173_39,1).
bond(train,d173,d173_1,d173_40,1).
bond(train,d173,d173_1,d173_41,1).
bond(train,d173,d173_4,d173_42,1).
bond(train,d173,d173_42,d173_43,1).
bond(train,d173,d173_42,d173_44,1).
bond(train,d173,d173_42,d173_45,1).
bond(train,d173,d173_23,d173_46,1).
bond(train,d173,d173_46,d173_47,1).
bond(train,d173,d173_46,d173_48,1).
bond(train,d173,d173_46,d173_49,1).
bond(train,d173,d173_32,d173_50,1).
bond(train,d173,d173_32,d173_51,1).
bond(train,d173,d173_50,d173_52,1).
bond(train,d173,d173_52,d173_53,1).
bond(train,d173,d173_52,d173_54,1).
bond(train,d173,d173_52,d173_55,1).
bond(train,d173,d173_50,d173_56,1).
bond(train,d173,d173_50,d173_57,1).
bond(train,d173,d173_56,d173_58,1).
bond(train,d173,d173_56,d173_59,1).
bond(train,d173,d173_56,d173_60,1).
bond(train,d173,d173_58,d173_61,1).
bond(train,d173,d173_58,d173_62,1).
bond(train,d173,d173_58,d173_63,1).
bond(train,d173,d173_61,d173_64,1).
bond(train,d173,d173_61,d173_65,2).
bond(train,d173,d173_40,d173_66,1).
bond(train,d173,d173_64,d173_67,1).
bond(train,d174,d174_1,d174_2,1).
bond(train,d174,d174_2,d174_3,1).
bond(train,d174,d174_2,d174_4,1).
bond(train,d174,d174_2,d174_5,1).
bond(train,d174,d174_3,d174_6,1).
bond(train,d174,d174_3,d174_7,1).
bond(train,d174,d174_3,d174_8,1).
bond(train,d174,d174_7,d174_9,1).
bond(train,d174,d174_7,d174_10,1).
bond(train,d174,d174_7,d174_11,1).
bond(train,d174,d174_10,d174_12,1).
bond(train,d174,d174_10,d174_13,1).
bond(train,d174,d174_10,d174_14,1).
bond(train,d174,d174_13,d174_15,1).
bond(train,d174,d174_13,d174_16,1).
bond(train,d174,d174_13,d174_17,1).
bond(train,d174,d174_16,d174_18,1).
bond(train,d174,d174_16,d174_19,1).
bond(train,d174,d174_16,d174_20,1).
bond(train,d174,d174_1,d174_21,1).
bond(train,d174,d174_6,d174_22,1).
bond(train,d174,d174_9,d174_23,1).
bond(train,d174,d174_12,d174_24,1).
bond(train,d174,d174_15,d174_25,1).
bond(train,d174,d174_18,d174_26,1).
bond(train,d175,d175_1,d175_2,7).
bond(train,d175,d175_2,d175_3,7).
bond(train,d175,d175_3,d175_4,7).
bond(train,d175,d175_4,d175_5,7).
bond(train,d175,d175_5,d175_6,7).
bond(train,d175,d175_6,d175_1,7).
bond(train,d175,d175_1,d175_7,1).
bond(train,d175,d175_2,d175_8,1).
bond(train,d175,d175_3,d175_9,1).
bond(train,d175,d175_5,d175_10,1).
bond(train,d175,d175_6,d175_11,1).
bond(train,d175,d175_4,d175_12,1).
bond(train,d175,d175_12,d175_13,1).
bond(train,d175,d175_12,d175_14,1).
bond(train,d175,d175_12,d175_15,1).
bond(train,d175,d175_13,d175_16,1).
bond(train,d175,d175_13,d175_17,1).
bond(train,d175,d175_13,d175_18,1).
bond(train,d175,d175_16,d175_19,1).
bond(train,d175,d175_19,d175_20,2).
bond(train,d175,d175_19,d175_21,1).
bond(train,d175,d175_21,d175_22,1).
bond(train,d175,d175_22,d175_23,2).
bond(train,d175,d175_22,d175_24,1).
bond(train,d175,d175_16,d175_25,1).
bond(train,d175,d175_21,d175_26,1).
bond(train,d175,d175_20,d175_27,1).
bond(train,d175,d175_23,d175_28,1).
bond(train,d175,d175_24,d175_29,1).
bond(train,d175,d175_24,d175_30,1).
bond(train,d176,d176_1,d176_2,7).
bond(train,d176,d176_2,d176_3,7).
bond(train,d176,d176_3,d176_4,7).
bond(train,d176,d176_4,d176_5,7).
bond(train,d176,d176_5,d176_6,7).
bond(train,d176,d176_6,d176_1,7).
bond(train,d176,d176_1,d176_7,1).
bond(train,d176,d176_2,d176_8,1).
bond(train,d176,d176_5,d176_9,1).
bond(train,d176,d176_6,d176_10,1).
bond(train,d176,d176_4,d176_11,1).
bond(train,d176,d176_11,d176_12,2).
bond(train,d176,d176_11,d176_13,1).
bond(train,d176,d176_3,d176_14,1).
bond(train,d176,d176_14,d176_15,1).
bond(train,d176,d176_14,d176_16,2).
bond(train,d176,d176_13,d176_17,1).
bond(train,d176,d176_13,d176_18,1).
bond(train,d176,d176_15,d176_19,1).
bond(train,d176,d176_15,d176_20,1).
bond(train,d177,d177_1,d177_2,7).
bond(train,d177,d177_2,d177_3,7).
bond(train,d177,d177_3,d177_4,7).
bond(train,d177,d177_4,d177_5,7).
bond(train,d177,d177_5,d177_6,7).
bond(train,d177,d177_6,d177_1,7).
bond(train,d177,d177_2,d177_7,1).
bond(train,d177,d177_5,d177_8,1).
bond(train,d177,d177_3,d177_9,1).
bond(train,d177,d177_9,d177_10,1).
bond(train,d177,d177_10,d177_11,1).
bond(train,d177,d177_11,d177_4,1).
bond(train,d177,d177_10,d177_12,1).
bond(train,d177,d177_10,d177_13,1).
bond(train,d177,d177_6,d177_14,1).
bond(train,d177,d177_14,d177_15,1).
bond(train,d177,d177_14,d177_16,1).
bond(train,d177,d177_14,d177_17,1).
bond(train,d177,d177_15,d177_18,1).
bond(train,d177,d177_15,d177_19,1).
bond(train,d177,d177_15,d177_20,1).
bond(train,d177,d177_18,d177_21,1).
bond(train,d177,d177_18,d177_22,1).
bond(train,d177,d177_18,d177_23,1).
bond(train,d177,d177_1,d177_24,1).
bond(train,d177,d177_24,d177_25,1).
bond(train,d177,d177_24,d177_26,1).
bond(train,d177,d177_24,d177_27,1).
bond(train,d177,d177_25,d177_28,1).
bond(train,d177,d177_28,d177_29,1).
bond(train,d177,d177_28,d177_30,1).
bond(train,d177,d177_28,d177_31,1).
bond(train,d177,d177_29,d177_32,1).
bond(train,d177,d177_29,d177_33,1).
bond(train,d177,d177_29,d177_34,1).
bond(train,d177,d177_32,d177_35,1).
bond(train,d177,d177_35,d177_36,1).
bond(train,d177,d177_35,d177_37,1).
bond(train,d177,d177_35,d177_38,1).
bond(train,d177,d177_36,d177_39,1).
bond(train,d177,d177_36,d177_40,1).
bond(train,d177,d177_36,d177_41,1).
bond(train,d177,d177_39,d177_42,1).
bond(train,d177,d177_42,d177_43,1).
bond(train,d177,d177_42,d177_44,1).
bond(train,d177,d177_42,d177_45,1).
bond(train,d177,d177_43,d177_46,1).
bond(train,d177,d177_43,d177_47,1).
bond(train,d177,d177_43,d177_48,1).
bond(train,d177,d177_46,d177_49,1).
bond(train,d177,d177_46,d177_50,1).
bond(train,d177,d177_46,d177_51,1).
bond(train,d177,d177_49,d177_52,1).
bond(train,d177,d177_49,d177_53,1).
bond(train,d177,d177_49,d177_54,1).
bond(train,d178,d178_1,d178_2,2).
bond(train,d178,d178_1,d178_3,2).
bond(train,d179,d179_1,d179_2,7).
bond(train,d179,d179_2,d179_3,7).
bond(train,d179,d179_3,d179_4,7).
bond(train,d179,d179_4,d179_5,7).
bond(train,d179,d179_5,d179_6,7).
bond(train,d179,d179_6,d179_1,7).
bond(train,d179,d179_2,d179_7,1).
bond(train,d179,d179_3,d179_8,1).
bond(train,d179,d179_5,d179_9,1).
bond(train,d179,d179_6,d179_10,1).
bond(train,d179,d179_4,d179_11,1).
bond(train,d179,d179_11,d179_12,1).
bond(train,d179,d179_11,d179_13,1).
bond(train,d179,d179_11,d179_14,1).
bond(train,d179,d179_1,d179_15,1).
bond(train,d179,d179_15,d179_16,2).
bond(train,d179,d179_15,d179_17,2).
bond(train,d179,d179_15,d179_18,1).
bond(train,d179,d179_18,d179_19,1).
bond(train,d179,d179_19,d179_20,2).
bond(train,d179,d179_19,d179_21,1).
bond(train,d179,d179_21,d179_22,1).
bond(train,d179,d179_22,d179_23,1).
bond(train,d179,d179_22,d179_24,1).
bond(train,d179,d179_22,d179_25,1).
bond(train,d179,d179_23,d179_26,1).
bond(train,d179,d179_23,d179_27,1).
bond(train,d179,d179_23,d179_28,1).
bond(train,d179,d179_26,d179_29,1).
bond(train,d179,d179_26,d179_30,1).
bond(train,d179,d179_26,d179_31,1).
bond(train,d179,d179_29,d179_32,1).
bond(train,d179,d179_29,d179_33,1).
bond(train,d179,d179_29,d179_34,1).
bond(train,d179,d179_18,d179_35,1).
bond(train,d179,d179_21,d179_36,1).
bond(train,d18,d18_2,d18_3,7).
bond(train,d18,d18_3,d18_4,7).
bond(train,d18,d18_4,d18_1,7).
bond(train,d18,d18_3,d18_5,1).
bond(train,d18,d18_5,d18_6,2).
bond(train,d18,d18_5,d18_7,2).
bond(train,d18,d18_1,d18_8,1).
bond(train,d18,d18_8,d18_9,1).
bond(train,d18,d18_9,d18_10,1).
bond(train,d18,d18_9,d18_11,2).
bond(train,d18,d18_10,d18_12,1).
bond(train,d18,d18_12,d18_13,1).
bond(train,d18,d18_8,d18_14,1).
bond(train,d18,d18_10,d18_15,1).
bond(train,d18,d18_12,d18_16,1).
bond(train,d18,d18_12,d18_17,1).
bond(train,d18,d18_13,d18_18,1).
bond(train,d18,d18_13,d18_19,1).
bond(train,d18,d18_13,d18_20,1).
bond(train,d18,d18_1,d18_21,7).
bond(train,d18,d18_21,d18_2,7).
bond(train,d18,d18_2,d18_22,1).
bond(train,d180,d180_1,d180_2,7).
bond(train,d180,d180_2,d180_3,7).
bond(train,d180,d180_3,d180_4,7).
bond(train,d180,d180_4,d180_5,7).
bond(train,d180,d180_5,d180_6,7).
bond(train,d180,d180_6,d180_1,7).
bond(train,d180,d180_7,d180_8,7).
bond(train,d180,d180_8,d180_9,7).
bond(train,d180,d180_4,d180_7,7).
bond(train,d180,d180_3,d180_9,7).
bond(train,d180,d180_1,d180_10,1).
bond(train,d180,d180_2,d180_11,1).
bond(train,d180,d180_5,d180_12,1).
bond(train,d180,d180_6,d180_13,1).
bond(train,d180,d180_9,d180_14,1).
bond(train,d180,d180_7,d180_15,1).
bond(train,d180,d180_15,d180_16,1).
bond(train,d180,d180_16,d180_17,1).
bond(train,d180,d180_16,d180_18,1).
bond(train,d180,d180_18,d180_19,1).
bond(train,d180,d180_18,d180_20,2).
bond(train,d180,d180_19,d180_21,1).
bond(train,d180,d180_17,d180_22,1).
bond(train,d180,d180_17,d180_23,1).
bond(train,d180,d180_16,d180_24,1).
bond(train,d180,d180_15,d180_25,1).
bond(train,d180,d180_15,d180_26,1).
bond(train,d180,d180_8,d180_27,1).
bond(train,d181,d181_1,d181_2,7).
bond(train,d181,d181_2,d181_3,7).
bond(train,d181,d181_3,d181_4,7).
bond(train,d181,d181_4,d181_5,7).
bond(train,d181,d181_5,d181_6,7).
bond(train,d181,d181_6,d181_1,7).
bond(train,d181,d181_1,d181_7,1).
bond(train,d181,d181_2,d181_8,1).
bond(train,d181,d181_3,d181_9,1).
bond(train,d181,d181_4,d181_10,1).
bond(train,d181,d181_6,d181_11,1).
bond(train,d181,d181_5,d181_12,1).
bond(train,d181,d181_12,d181_13,1).
bond(train,d181,d181_12,d181_14,1).
bond(train,d181,d181_12,d181_15,1).
bond(train,d181,d181_13,d181_16,1).
bond(train,d182,d182_1,d182_2,7).
bond(train,d182,d182_2,d182_3,7).
bond(train,d182,d182_3,d182_4,7).
bond(train,d182,d182_4,d182_5,7).
bond(train,d182,d182_5,d182_6,7).
bond(train,d182,d182_6,d182_1,7).
bond(train,d182,d182_1,d182_7,1).
bond(train,d182,d182_2,d182_8,1).
bond(train,d182,d182_3,d182_9,1).
bond(train,d182,d182_5,d182_10,1).
bond(train,d182,d182_6,d182_11,1).
bond(train,d182,d182_4,d182_12,1).
bond(train,d182,d182_12,d182_13,1).
bond(train,d182,d182_13,d182_14,1).
bond(train,d182,d182_13,d182_15,1).
bond(train,d182,d182_13,d182_16,1).
bond(train,d182,d182_14,d182_17,2).
bond(train,d182,d182_14,d182_18,1).
bond(train,d182,d182_18,d182_19,1).
bond(train,d182,d182_18,d182_20,1).
bond(train,d182,d182_19,d182_21,1).
bond(train,d182,d182_19,d182_22,1).
bond(train,d182,d182_19,d182_23,1).
bond(train,d182,d182_22,d182_24,1).
bond(train,d182,d182_24,d182_21,1).
bond(train,d182,d182_21,d182_25,2).
bond(train,d182,d182_22,d182_26,1).
bond(train,d182,d182_26,d182_27,1).
bond(train,d182,d182_27,d182_28,1).
bond(train,d182,d182_28,d182_24,1).
bond(train,d182,d182_22,d182_29,1).
bond(train,d182,d182_28,d182_30,1).
bond(train,d182,d182_28,d182_31,1).
bond(train,d182,d182_30,d182_32,1).
bond(train,d182,d182_30,d182_33,2).
bond(train,d182,d182_32,d182_34,1).
bond(train,d182,d182_27,d182_35,1).
bond(train,d182,d182_35,d182_36,1).
bond(train,d182,d182_35,d182_37,1).
bond(train,d182,d182_35,d182_38,1).
bond(train,d182,d182_27,d182_39,1).
bond(train,d182,d182_39,d182_40,1).
bond(train,d182,d182_39,d182_41,1).
bond(train,d182,d182_39,d182_42,1).
bond(train,d183,d183_1,d183_2,7).
bond(train,d183,d183_2,d183_3,7).
bond(train,d183,d183_3,d183_4,7).
bond(train,d183,d183_4,d183_5,7).
bond(train,d183,d183_5,d183_6,7).
bond(train,d183,d183_6,d183_1,7).
bond(train,d183,d183_1,d183_7,1).
bond(train,d183,d183_6,d183_8,1).
bond(train,d183,d183_3,d183_9,1).
bond(train,d183,d183_9,d183_10,1).
bond(train,d183,d183_10,d183_11,1).
bond(train,d183,d183_11,d183_12,1).
bond(train,d183,d183_12,d183_4,1).
bond(train,d183,d183_9,d183_13,1).
bond(train,d183,d183_9,d183_14,1).
bond(train,d183,d183_10,d183_15,1).
bond(train,d183,d183_15,d183_16,1).
bond(train,d183,d183_16,d183_17,1).
bond(train,d183,d183_17,d183_18,1).
bond(train,d183,d183_18,d183_11,2).
bond(train,d183,d183_10,d183_19,1).
bond(train,d183,d183_15,d183_20,1).
bond(train,d183,d183_15,d183_21,1).
bond(train,d183,d183_16,d183_22,1).
bond(train,d183,d183_22,d183_23,1).
bond(train,d183,d183_23,d183_24,2).
bond(train,d183,d183_24,d183_25,1).
bond(train,d183,d183_25,d183_17,1).
bond(train,d183,d183_2,d183_26,1).
bond(train,d183,d183_26,d183_27,1).
bond(train,d183,d183_26,d183_28,1).
bond(train,d183,d183_26,d183_29,1).
bond(train,d183,d183_2,d183_30,1).
bond(train,d183,d183_30,d183_31,1).
bond(train,d183,d183_5,d183_32,1).
bond(train,d183,d183_32,d183_33,1).
bond(train,d183,d183_12,d183_34,2).
bond(train,d183,d183_25,d183_35,2).
bond(train,d183,d183_18,d183_36,1).
bond(train,d183,d183_36,d183_37,1).
bond(train,d183,d183_17,d183_38,1).
bond(train,d183,d183_16,d183_39,1).
bond(train,d183,d183_38,d183_40,1).
bond(train,d183,d183_22,d183_41,1).
bond(train,d183,d183_22,d183_42,1).
bond(train,d183,d183_41,d183_43,1).
bond(train,d183,d183_43,d183_44,1).
bond(train,d183,d183_43,d183_45,1).
bond(train,d183,d183_43,d183_46,1).
bond(train,d183,d183_41,d183_47,1).
bond(train,d183,d183_47,d183_48,1).
bond(train,d183,d183_47,d183_49,1).
bond(train,d183,d183_47,d183_50,1).
bond(train,d183,d183_24,d183_51,1).
bond(train,d183,d183_51,d183_52,1).
bond(train,d183,d183_51,d183_53,2).
bond(train,d183,d183_52,d183_54,1).
bond(train,d183,d183_52,d183_55,1).
bond(train,d183,d183_23,d183_56,1).
bond(train,d183,d183_56,d183_57,1).
bond(train,d184,d184_1,d184_2,7).
bond(train,d184,d184_2,d184_3,7).
bond(train,d184,d184_3,d184_4,7).
bond(train,d184,d184_4,d184_5,7).
bond(train,d184,d184_5,d184_6,7).
bond(train,d184,d184_6,d184_1,7).
bond(train,d184,d184_1,d184_7,1).
bond(train,d184,d184_2,d184_8,1).
bond(train,d184,d184_3,d184_9,1).
bond(train,d184,d184_5,d184_10,1).
bond(train,d184,d184_6,d184_11,1).
bond(train,d184,d184_4,d184_12,1).
bond(train,d184,d184_12,d184_13,1).
bond(train,d184,d184_12,d184_14,1).
bond(train,d184,d184_12,d184_15,1).
bond(train,d184,d184_13,d184_16,1).
bond(train,d184,d184_16,d184_17,1).
bond(train,d184,d184_16,d184_18,1).
bond(train,d184,d184_16,d184_19,1).
bond(train,d184,d184_13,d184_20,1).
bond(train,d184,d184_13,d184_21,1).
bond(train,d184,d184_20,d184_22,1).
bond(train,d184,d184_20,d184_23,1).
bond(train,d185,d185_1,d185_2,1).
bond(train,d185,d185_1,d185_3,1).
bond(train,d185,d185_1,d185_4,1).
bond(train,d185,d185_1,d185_5,1).
bond(train,d185,d185_2,d185_6,1).
bond(train,d185,d185_2,d185_7,1).
bond(train,d185,d185_2,d185_8,1).
bond(train,d185,d185_3,d185_9,2).
bond(train,d185,d185_6,d185_10,2).
bond(train,d185,d185_6,d185_11,1).
bond(train,d185,d185_3,d185_11,1).
bond(train,d186,d186a_1,d186a_2,1).
bond(train,d186,d186a_1,d186a_3,1).
bond(train,d186,d186a_1,d186a_4,1).
bond(train,d186,d186a_4,d186a_5,1).
bond(train,d186,d186a_2,d186a_6,1).
bond(train,d186,d186a_2,d186a_7,1).
bond(train,d186,d186a_7,d186a_8,1).
bond(train,d186,d186a_7,d186a_9,1).
bond(train,d186,d186a_7,d186a_10,1).
bond(train,d186,d186a_6,d186a_11,1).
bond(train,d186,d186a_2,d186a_12,1).
bond(train,d186,d186a_12,d186a_13,1).
bond(train,d186,d186a_13,d186a_14,1).
bond(train,d186,d186a_12,d186a_15,1).
bond(train,d186,d186a_12,d186a_16,1).
bond(train,d186,d186a_16,d186a_17,1).
bond(train,d186,d186a_16,d186a_18,1).
bond(train,d186,d186a_16,d186a_19,1).
bond(train,d186,d186a_17,d186a_20,1).
bond(train,d186,d186a_17,d186a_21,1).
bond(train,d186,d186a_17,d186a_22,1).
bond(train,d186,d186a_14,d186a_23,2).
bond(train,d186,d186a_14,d186a_24,1).
bond(train,d186,d186a_24,d186a_25,1).
bond(train,d186,d186a_24,d186a_26,1).
bond(train,d186,d186a_26,d186a_27,1).
bond(train,d186,d186a_26,d186a_28,1).
bond(train,d186,d186a_26,d186a_29,1).
bond(train,d186,d186a_24,d186a_30,1).
bond(train,d186,d186a_25,d186a_31,1).
bond(train,d186,d186a_31,d186a_32,1).
bond(train,d186,d186a_25,d186a_33,1).
bond(train,d186,d186a_25,d186a_34,1).
bond(train,d186,d186a_34,d186a_35,1).
bond(train,d186,d186a_35,d186a_36,1).
bond(train,d186,d186a_36,d186a_37,1).
bond(train,d186,d186a_37,d186a_38,1).
bond(train,d186,d186a_38,d186a_39,1).
bond(train,d186,d186a_39,d186a_40,1).
bond(train,d186,d186a_40,d186a_35,1).
bond(train,d186,d186a_40,d186a_41,1).
bond(train,d186,d186a_40,d186a_42,1).
bond(train,d186,d186a_37,d186a_43,1).
bond(train,d186,d186a_37,d186a_44,1).
bond(train,d186,d186a_43,d186a_45,1).
bond(train,d186,d186a_43,d186a_46,1).
bond(train,d186,d186a_43,d186a_47,1).
bond(train,d186,d186a_38,d186a_48,1).
bond(train,d186,d186a_38,d186a_49,1).
bond(train,d186,d186a_48,d186a_50,1).
bond(train,d186,d186a_39,d186a_51,1).
bond(train,d186,d186a_51,d186a_52,1).
bond(train,d186,d186a_52,d186a_53,1).
bond(train,d186,d186a_52,d186a_54,1).
bond(train,d186,d186a_52,d186a_55,1).
bond(train,d186,d186a_39,d186a_56,1).
bond(train,d186,d186a_56,d186a_57,1).
bond(train,d186,d186a_56,d186a_58,1).
bond(train,d186,d186a_56,d186a_59,1).
bond(train,d186,d186a_35,d186a_60,1).
bond(train,d186,d186a_31,d186a_61,1).
bond(train,d186,d186a_61,d186a_62,1).
bond(train,d186,d186a_61,d186a_63,1).
bond(train,d186,d186a_61,d186a_64,1).
bond(train,d186,d186a_31,d186a_65,1).
bond(train,d186,d186a_32,d186a_66,1).
bond(train,d186,d186a_1,d186a_67,1).
bond(train,d186,d186a_67,d186a_68,1).
bond(train,d186,d186a_68,d186a_69,2).
bond(train,d186,d186a_68,d186a_70,1).
bond(train,d186,d186a_70,d186a_71,1).
bond(train,d186,d186a_71,d186a_66,1).
bond(train,d186,d186a_71,d186a_72,1).
bond(train,d186,d186a_71,d186a_73,1).
bond(train,d186,d186a_67,d186a_74,1).
bond(train,d186,d186a_74,d186a_75,1).
bond(train,d186,d186a_74,d186a_76,1).
bond(train,d186,d186a_74,d186a_77,1).
bond(train,d186,d186a_67,d186a_78,1).
bond(train,d186,d186a_70,d186a_79,1).
bond(train,d186,d186a_79,d186a_80,1).
bond(train,d186,d186a_79,d186a_81,1).
bond(train,d186,d186a_79,d186a_82,1).
bond(train,d186,d186a_70,d186a_83,1).
bond(train,d186,d186a_66,d186a_84,1).
bond(train,d186,d186a_84,d186a_85,1).
bond(train,d186,d186a_84,d186a_86,1).
bond(train,d186,d186a_84,d186a_87,1).
bond(train,d186,d186a_66,d186a_88,1).
bond(train,d186,d186a_88,d186a_89,1).
bond(train,d186,d186a_32,d186a_90,1).
bond(train,d186,d186a_32,d186a_91,1).
bond(train,d186,d186a_91,d186a_92,1).
bond(train,d186,d186a_92,d186a_93,1).
bond(train,d186,d186a_93,d186a_94,1).
bond(train,d186,d186a_94,d186a_95,1).
bond(train,d186,d186a_95,d186a_96,1).
bond(train,d186,d186a_96,d186a_97,1).
bond(train,d186,d186a_97,d186a_92,1).
bond(train,d186,d186a_95,d186a_98,1).
bond(train,d186,d186a_95,d186a_99,1).
bond(train,d186,d186a_92,d186a_100,1).
bond(train,d186,d186a_97,d186a_101,1).
bond(train,d186,d186a_97,d186a_102,1).
bond(train,d186,d186a_101,d186a_103,1).
bond(train,d186,d186a_96,d186a_104,1).
bond(train,d186,d186a_96,d186a_105,1).
bond(train,d186,d186a_104,d186a_106,1).
bond(train,d186,d186a_106,d186a_107,1).
bond(train,d186,d186a_106,d186a_108,1).
bond(train,d186,d186a_106,d186a_109,1).
bond(train,d186,d186a_104,d186a_110,1).
bond(train,d186,d186a_110,d186a_111,1).
bond(train,d186,d186a_110,d186a_112,1).
bond(train,d186,d186a_110,d186a_113,1).
bond(train,d186,d186a_104,d186a_114,1).
bond(train,d186,d186a_114,d186a_115,1).
bond(train,d186,d186a_114,d186a_116,1).
bond(train,d186,d186a_114,d186a_117,1).
bond(train,d186,d186a_94,d186a_118,1).
bond(train,d186,d186a_94,d186a_119,1).
bond(train,d186,d186a_118,d186a_120,1).
bond(train,d186,d186a_118,d186a_121,1).
bond(train,d186,d186a_118,d186a_122,1).
bond(train,d186,d186b_1,d186b_2,1).
bond(train,d186,d186b_1,d186b_3,1).
bond(train,d186,d186b_1,d186b_4,1).
bond(train,d186,d186b_1,d186b_5,1).
bond(train,d186,d186b_2,d186b_6,1).
bond(train,d186,d186b_2,d186b_7,1).
bond(train,d186,d186b_2,d186b_8,1).
bond(train,d186,d186b_6,d186b_9,1).
bond(train,d186,d186b_6,d186b_10,1).
bond(train,d186,d186b_6,d186b_11,1).
bond(train,d186,d186b_9,d186b_12,1).
bond(train,d186,d186b_9,d186b_13,1).
bond(train,d186,d186b_9,d186b_14,1).
bond(train,d186,d186b_12,d186b_15,1).
bond(train,d186,d186b_12,d186b_16,1).
bond(train,d186,d186b_12,d186b_17,1).
bond(train,d186,d186b_15,d186b_18,1).
bond(train,d186,d186b_15,d186b_19,1).
bond(train,d186,d186b_15,d186b_20,1).
bond(train,d186,d186b_18,d186b_21,1).
bond(train,d186,d186b_18,d186b_22,1).
bond(train,d186,d186b_18,d186b_23,1).
bond(train,d186,d186b_21,d186b_24,1).
bond(train,d186,d186b_21,d186b_25,1).
bond(train,d186,d186b_21,d186b_26,1).
bond(train,d186,d186b_24,d186b_27,1).
bond(train,d186,d186b_24,d186b_28,1).
bond(train,d186,d186b_24,d186b_29,1).
bond(train,d186,d186b_27,d186b_30,1).
bond(train,d186,d186b_27,d186b_31,1).
bond(train,d186,d186b_27,d186b_32,1).
bond(train,d186,d186b_30,d186b_33,1).
bond(train,d186,d186b_30,d186b_34,1).
bond(train,d186,d186b_30,d186b_35,1).
bond(train,d186,d186b_33,d186b_36,1).
bond(train,d186,d186b_33,d186b_37,1).
bond(train,d186,d186b_33,d186b_38,1).
bond(train,d186,d186b_36,d186b_39,1).
bond(train,d186,d186b_36,d186b_40,1).
bond(train,d186,d186b_36,d186b_41,1).
bond(train,d186,d186b_39,d186b_42,1).
bond(train,d186,d186b_39,d186b_43,1).
bond(train,d186,d186b_39,d186b_44,1).
bond(train,d186,d186b_42,d186b_45,1).
bond(train,d186,d186b_42,d186b_46,1).
bond(train,d186,d186b_42,d186b_47,1).
bond(train,d186,d186b_45,d186b_48,1).
bond(train,d186,d186b_45,d186b_49,1).
bond(train,d186,d186b_45,d186b_50,1).
bond(train,d186,d186b_48,d186b_51,1).
bond(train,d186,d186b_48,d186b_52,1).
bond(train,d186,d186b_48,d186b_53,1).
bond(train,d186,d186b_51,d186b_54,1).
bond(train,d186,d186b_51,d186b_55,2).
bond(train,d186,d186b_54,d186b_56,1).
bond(train,d187,d187_1,d187_2,7).
bond(train,d187,d187_2,d187_3,7).
bond(train,d187,d187_3,d187_4,7).
bond(train,d187,d187_4,d187_5,7).
bond(train,d187,d187_5,d187_6,7).
bond(train,d187,d187_6,d187_1,7).
bond(train,d187,d187_1,d187_7,1).
bond(train,d187,d187_2,d187_8,1).
bond(train,d187,d187_3,d187_9,1).
bond(train,d187,d187_5,d187_10,1).
bond(train,d187,d187_6,d187_11,1).
bond(train,d187,d187_4,d187_12,1).
bond(train,d187,d187_12,d187_13,1).
bond(train,d187,d187_12,d187_14,1).
bond(train,d187,d187_12,d187_15,1).
bond(train,d187,d187_14,d187_16,1).
bond(train,d187,d187_14,d187_17,1).
bond(train,d187,d187_14,d187_18,1).
bond(train,d187,d187_16,d187_19,1).
bond(train,d187,d187_19,d187_20,1).
bond(train,d187,d187_19,d187_21,1).
bond(train,d187,d187_19,d187_22,1).
bond(train,d187,d187_13,d187_23,1).
bond(train,d187,d187_16,d187_24,1).
bond(train,d187,d187_11,d187_25,1).
bond(train,d188,d188a_1,d188a_2,7).
bond(train,d188,d188a_2,d188a_3,7).
bond(train,d188,d188a_3,d188a_4,7).
bond(train,d188,d188a_4,d188a_5,7).
bond(train,d188,d188a_5,d188a_6,7).
bond(train,d188,d188a_6,d188a_1,7).
bond(train,d188,d188a_1,d188a_7,1).
bond(train,d188,d188a_2,d188a_8,1).
bond(train,d188,d188a_4,d188a_9,1).
bond(train,d188,d188a_6,d188a_10,1).
bond(train,d188,d188a_5,d188a_11,1).
bond(train,d188,d188a_11,d188a_12,1).
bond(train,d188,d188a_11,d188a_13,1).
bond(train,d188,d188a_11,d188a_14,1).
bond(train,d188,d188a_3,d188a_15,1).
bond(train,d188,d188a_15,d188a_16,1).
bond(train,d188,d188a_15,d188a_17,1).
bond(train,d188,d188a_15,d188a_18,1).
bond(train,d188,d188b_1,d188b_2,7).
bond(train,d188,d188b_2,d188b_3,7).
bond(train,d188,d188b_3,d188b_4,7).
bond(train,d188,d188b_4,d188b_5,7).
bond(train,d188,d188b_5,d188b_6,7).
bond(train,d188,d188b_6,d188b_1,7).
bond(train,d188,d188b_1,d188b_7,1).
bond(train,d188,d188b_4,d188b_8,1).
bond(train,d188,d188b_6,d188b_9,1).
bond(train,d188,d188b_5,d188b_10,1).
bond(train,d188,d188b_10,d188b_11,1).
bond(train,d188,d188b_10,d188b_12,1).
bond(train,d188,d188b_10,d188b_13,1).
bond(train,d188,d188b_3,d188b_14,1).
bond(train,d188,d188b_2,d188b_15,1).
bond(train,d188,d188b_15,d188b_16,1).
bond(train,d188,d188b_15,d188b_17,1).
bond(train,d188,d188b_15,d188b_18,1).
bond(train,d188,d188c_1,d188c_2,7).
bond(train,d188,d188c_2,d188c_3,7).
bond(train,d188,d188c_3,d188c_4,7).
bond(train,d188,d188c_4,d188c_5,7).
bond(train,d188,d188c_5,d188c_6,7).
bond(train,d188,d188c_6,d188c_1,7).
bond(train,d188,d188c_1,d188c_7,1).
bond(train,d188,d188c_2,d188c_8,1).
bond(train,d188,d188c_3,d188c_9,1).
bond(train,d188,d188c_6,d188c_10,1).
bond(train,d188,d188c_5,d188c_11,1).
bond(train,d188,d188c_11,d188c_12,1).
bond(train,d188,d188c_11,d188c_13,1).
bond(train,d188,d188c_11,d188c_14,1).
bond(train,d188,d188c_4,d188c_15,1).
bond(train,d188,d188c_15,d188c_16,1).
bond(train,d188,d188c_15,d188c_17,1).
bond(train,d188,d188c_15,d188c_18,1).
bond(train,d188,d188d_1,d188d_2,7).
bond(train,d188,d188d_2,d188d_3,7).
bond(train,d188,d188d_3,d188d_4,7).
bond(train,d188,d188d_4,d188d_5,7).
bond(train,d188,d188d_5,d188d_6,7).
bond(train,d188,d188d_6,d188d_1,7).
bond(train,d188,d188d_1,d188d_7,1).
bond(train,d188,d188d_2,d188d_8,1).
bond(train,d188,d188d_3,d188d_9,1).
bond(train,d188,d188d_4,d188d_10,1).
bond(train,d188,d188d_6,d188d_11,1).
bond(train,d188,d188d_5,d188d_12,1).
bond(train,d188,d188d_12,d188d_13,1).
bond(train,d188,d188d_12,d188d_14,1).
bond(train,d188,d188d_12,d188d_15,1).
bond(train,d188,d188d_13,d188d_16,1).
bond(train,d188,d188d_13,d188d_17,1).
bond(train,d188,d188d_13,d188d_18,1).
bond(train,d189,d189_1,d189_2,7).
bond(train,d189,d189_2,d189_3,7).
bond(train,d189,d189_3,d189_4,7).
bond(train,d189,d189_4,d189_5,7).
bond(train,d189,d189_5,d189_6,7).
bond(train,d189,d189_6,d189_1,7).
bond(train,d189,d189_1,d189_7,1).
bond(train,d189,d189_2,d189_8,1).
bond(train,d189,d189_3,d189_9,1).
bond(train,d189,d189_4,d189_10,1).
bond(train,d189,d189_6,d189_11,1).
bond(train,d189,d189_5,d189_12,1).
bond(train,d189,d189_12,d189_13,1).
bond(train,d189,d189_13,d189_14,1).
bond(train,d189,d189_13,d189_15,1).
bond(train,d189,d189_13,d189_16,1).
bond(train,d189,d189_14,d189_17,1).
bond(train,d189,d189_17,d189_18,1).
bond(train,d189,d189_17,d189_19,1).
bond(train,d189,d189_17,d189_20,1).
bond(train,d189,d189_12,d189_21,1).
bond(train,d189,d189_12,d189_22,1).
bond(train,d189,d189_21,d189_23,1).
bond(train,d189,d189_14,d189_24,1).
bond(train,d19,d19_1,d19_2,7).
bond(train,d19,d19_2,d19_3,7).
bond(train,d19,d19_3,d19_4,7).
bond(train,d19,d19_4,d19_5,7).
bond(train,d19,d19_5,d19_6,7).
bond(train,d19,d19_6,d19_1,7).
bond(train,d19,d19_1,d19_7,1).
bond(train,d19,d19_2,d19_8,1).
bond(train,d19,d19_6,d19_9,1).
bond(train,d19,d19_3,d19_10,7).
bond(train,d19,d19_10,d19_11,7).
bond(train,d19,d19_11,d19_12,7).
bond(train,d19,d19_12,d19_13,7).
bond(train,d19,d19_13,d19_4,7).
bond(train,d19,d19_11,d19_14,1).
bond(train,d19,d19_12,d19_15,1).
bond(train,d19,d19_5,d19_16,1).
bond(train,d19,d19_13,d19_17,1).
bond(train,d19,d19_17,d19_16,1).
bond(train,d19,d19_17,d19_18,1).
bond(train,d19,d19_17,d19_19,1).
bond(train,d19,d19_16,d19_20,1).
bond(train,d19,d19_16,d19_21,1).
bond(train,d19,d19_10,d19_22,1).
bond(train,d19,d19_22,d19_23,2).
bond(train,d19,d19_22,d19_24,2).
bond(train,d190,d190_1,d190_2,7).
bond(train,d190,d190_2,d190_3,7).
bond(train,d190,d190_3,d190_4,7).
bond(train,d190,d190_4,d190_5,7).
bond(train,d190,d190_5,d190_6,7).
bond(train,d190,d190_6,d190_1,7).
bond(train,d190,d190_1,d190_7,1).
bond(train,d190,d190_2,d190_8,1).
bond(train,d190,d190_3,d190_9,1).
bond(train,d190,d190_4,d190_10,1).
bond(train,d190,d190_6,d190_11,1).
bond(train,d190,d190_5,d190_12,1).
bond(train,d190,d190_12,d190_13,1).
bond(train,d190,d190_12,d190_14,1).
bond(train,d190,d190_12,d190_15,1).
bond(train,d191,d191_1,d191_2,1).
bond(train,d191,d191_1,d191_3,1).
bond(train,d191,d191_1,d191_4,1).
bond(train,d191,d191_1,d191_5,1).
bond(train,d191,d191_2,d191_6,1).
bond(train,d191,d191_6,d191_7,1).
bond(train,d191,d191_6,d191_8,1).
bond(train,d191,d191_6,d191_9,1).
bond(train,d191,d191_2,d191_10,1).
bond(train,d191,d191_2,d191_11,1).
bond(train,d191,d191_10,d191_12,1).
bond(train,d191,d191_10,d191_13,1).
bond(train,d191,d191_10,d191_14,1).
bond(train,d191,d191_12,d191_15,2).
bond(train,d191,d191_12,d191_16,1).
bond(train,d191,d191_16,d191_17,1).
bond(train,d191,d191_17,d191_18,1).
bond(train,d191,d191_17,d191_19,1).
bond(train,d191,d191_17,d191_20,1).
bond(train,d191,d191_18,d191_21,2).
bond(train,d191,d191_18,d191_22,1).
bond(train,d191,d191_21,d191_23,1).
bond(train,d191,d191_21,d191_24,1).
bond(train,d192,d192_1,d192_2,7).
bond(train,d192,d192_2,d192_3,7).
bond(train,d192,d192_3,d192_4,7).
bond(train,d192,d192_4,d192_5,7).
bond(train,d192,d192_5,d192_6,7).
bond(train,d192,d192_6,d192_1,7).
bond(train,d192,d192_1,d192_7,1).
bond(train,d192,d192_2,d192_8,1).
bond(train,d192,d192_3,d192_9,1).
bond(train,d192,d192_6,d192_10,1).
bond(train,d192,d192_5,d192_11,1).
bond(train,d192,d192_4,d192_12,1).
bond(train,d192,d192_12,d192_13,1).
bond(train,d192,d192_13,d192_14,1).
bond(train,d192,d192_14,d192_15,1).
bond(train,d192,d192_14,d192_16,1).
bond(train,d192,d192_14,d192_17,1).
bond(train,d192,d192_15,d192_18,2).
bond(train,d192,d192_11,d192_19,1).
bond(train,d192,d192_11,d192_20,1).
bond(train,d192,d192_12,d192_21,2).
bond(train,d192,d192_15,d192_22,1).
bond(train,d192,d192_18,d192_23,1).
bond(train,d192,d192_18,d192_24,1).
bond(train,d192,d192_23,d192_25,7).
bond(train,d192,d192_25,d192_26,7).
bond(train,d192,d192_26,d192_27,7).
bond(train,d192,d192_27,d192_28,7).
bond(train,d192,d192_28,d192_29,7).
bond(train,d192,d192_29,d192_23,7).
bond(train,d192,d192_25,d192_30,1).
bond(train,d192,d192_26,d192_31,1).
bond(train,d192,d192_27,d192_32,1).
bond(train,d192,d192_28,d192_33,1).
bond(train,d192,d192_29,d192_34,1).
bond(train,d193,d193_1,d193_2,1).
bond(train,d193,d193_2,d193_3,1).
bond(train,d193,d193_3,d193_4,1).
bond(train,d193,d193_4,d193_5,1).
bond(train,d193,d193_5,d193_6,1).
bond(train,d193,d193_6,d193_1,1).
bond(train,d193,d193_1,d193_7,1).
bond(train,d193,d193_1,d193_8,1).
bond(train,d193,d193_3,d193_9,1).
bond(train,d193,d193_3,d193_10,1).
bond(train,d193,d193_4,d193_11,1).
bond(train,d193,d193_4,d193_12,1).
bond(train,d193,d193_6,d193_13,1).
bond(train,d193,d193_6,d193_14,1).
bond(train,d194,d194_1,d194_2,2).
bond(train,d195,d195_1,d195_2,7).
bond(train,d195,d195_2,d195_3,7).
bond(train,d195,d195_3,d195_4,7).
bond(train,d195,d195_4,d195_5,7).
bond(train,d195,d195_5,d195_6,7).
bond(train,d195,d195_6,d195_1,7).
bond(train,d195,d195_7,d195_8,7).
bond(train,d195,d195_8,d195_9,7).
bond(train,d195,d195_4,d195_7,7).
bond(train,d195,d195_3,d195_9,7).
bond(train,d195,d195_1,d195_10,1).
bond(train,d195,d195_2,d195_11,1).
bond(train,d195,d195_5,d195_12,1).
bond(train,d195,d195_6,d195_13,1).
bond(train,d195,d195_7,d195_14,1).
bond(train,d195,d195_8,d195_15,1).
bond(train,d196,d196_1,d196_2,7).
bond(train,d196,d196_2,d196_3,7).
bond(train,d196,d196_3,d196_4,7).
bond(train,d196,d196_4,d196_5,7).
bond(train,d196,d196_5,d196_1,7).
bond(train,d196,d196_1,d196_6,1).
bond(train,d196,d196_6,d196_7,2).
bond(train,d196,d196_6,d196_8,1).
bond(train,d196,d196_4,d196_9,1).
bond(train,d196,d196_3,d196_10,1).
bond(train,d196,d196_2,d196_11,1).
bond(train,d197,d197_1,d197_2,7).
bond(train,d197,d197_2,d197_3,7).
bond(train,d197,d197_3,d197_4,7).
bond(train,d197,d197_4,d197_5,7).
bond(train,d197,d197_5,d197_6,7).
bond(train,d197,d197_6,d197_1,7).
bond(train,d197,d197_1,d197_7,1).
bond(train,d197,d197_3,d197_8,1).
bond(train,d197,d197_4,d197_9,1).
bond(train,d197,d197_5,d197_10,1).
bond(train,d197,d197_6,d197_11,1).
bond(train,d197,d197_12,d197_13,7).
bond(train,d197,d197_13,d197_14,7).
bond(train,d197,d197_14,d197_15,7).
bond(train,d197,d197_15,d197_16,7).
bond(train,d197,d197_16,d197_17,7).
bond(train,d197,d197_17,d197_12,7).
bond(train,d197,d197_12,d197_18,1).
bond(train,d197,d197_13,d197_19,1).
bond(train,d197,d197_14,d197_20,1).
bond(train,d197,d197_16,d197_21,1).
bond(train,d197,d197_17,d197_22,1).
bond(train,d197,d197_15,d197_23,1).
bond(train,d197,d197_2,d197_24,1).
bond(train,d197,d197_23,d197_24,1).
bond(train,d197,d197_23,d197_25,1).
bond(train,d197,d197_25,d197_26,1).
bond(train,d197,d197_26,d197_27,1).
bond(train,d197,d197_27,d197_24,1).
bond(train,d197,d197_26,d197_28,1).
bond(train,d197,d197_26,d197_29,1).
bond(train,d197,d197_28,d197_30,1).
bond(train,d197,d197_28,d197_31,1).
bond(train,d197,d197_28,d197_32,1).
bond(train,d197,d197_30,d197_33,1).
bond(train,d197,d197_30,d197_34,1).
bond(train,d197,d197_30,d197_35,1).
bond(train,d197,d197_33,d197_36,1).
bond(train,d197,d197_33,d197_37,1).
bond(train,d197,d197_33,d197_38,1).
bond(train,d197,d197_25,d197_39,2).
bond(train,d197,d197_27,d197_40,2).
bond(train,d197,d197_36,d197_41,1).
bond(train,d197,d197_36,d197_42,1).
bond(train,d197,d197_36,d197_43,1).
bond(train,d198,d198_1,d198_2,1).
bond(train,d198,d198_1,d198_3,1).
bond(train,d198,d198_1,d198_4,1).
bond(train,d198,d198_1,d198_5,1).
bond(train,d198,d198_2,d198_6,1).
bond(train,d198,d198_2,d198_7,1).
bond(train,d198,d198_2,d198_8,1).
bond(train,d198,d198_6,d198_9,1).
bond(train,d198,d198_9,d198_10,1).
bond(train,d198,d198_6,d198_11,1).
bond(train,d198,d198_10,d198_12,1).
bond(train,d198,d198_10,d198_13,1).
bond(train,d198,d198_12,d198_14,1).
bond(train,d198,d198_12,d198_15,1).
bond(train,d198,d198_12,d198_16,1).
bond(train,d198,d198_14,d198_17,1).
bond(train,d198,d198_14,d198_18,1).
bond(train,d198,d198_14,d198_19,1).
bond(train,d198,d198_9,d198_20,2).
bond(train,d199,d199_1,d199_2,7).
bond(train,d199,d199_2,d199_3,7).
bond(train,d199,d199_3,d199_4,7).
bond(train,d199,d199_4,d199_5,7).
bond(train,d199,d199_5,d199_6,7).
bond(train,d199,d199_6,d199_1,7).
bond(train,d199,d199_1,d199_7,1).
bond(train,d199,d199_2,d199_8,1).
bond(train,d199,d199_3,d199_9,1).
bond(train,d199,d199_5,d199_10,1).
bond(train,d199,d199_6,d199_11,1).
bond(train,d199,d199_4,d199_12,1).
bond(train,d199,d199_12,d199_13,1).
bond(train,d199,d199_13,d199_14,7).
bond(train,d199,d199_14,d199_15,7).
bond(train,d199,d199_15,d199_16,7).
bond(train,d199,d199_16,d199_17,7).
bond(train,d199,d199_17,d199_18,7).
bond(train,d199,d199_18,d199_13,7).
bond(train,d199,d199_14,d199_19,1).
bond(train,d199,d199_15,d199_20,1).
bond(train,d199,d199_16,d199_21,1).
bond(train,d199,d199_17,d199_22,1).
bond(train,d199,d199_18,d199_23,1).
bond(train,d199,d199_12,d199_24,1).
bond(train,d199,d199_24,d199_25,2).
bond(train,d2,d2_1,d2_2,7).
bond(train,d2,d2_2,d2_3,7).
bond(train,d2,d2_3,d2_4,7).
bond(train,d2,d2_4,d2_5,7).
bond(train,d2,d2_5,d2_6,7).
bond(train,d2,d2_6,d2_1,7).
bond(train,d2,d2_1,d2_7,1).
bond(train,d2,d2_2,d2_8,1).
bond(train,d2,d2_5,d2_9,1).
bond(train,d2,d2_6,d2_10,1).
bond(train,d2,d2_3,d2_11,7).
bond(train,d2,d2_11,d2_12,7).
bond(train,d2,d2_12,d2_13,7).
bond(train,d2,d2_13,d2_4,7).
bond(train,d2,d2_12,d2_14,7).
bond(train,d2,d2_14,d2_15,7).
bond(train,d2,d2_15,d2_16,7).
bond(train,d2,d2_16,d2_17,7).
bond(train,d2,d2_17,d2_11,7).
bond(train,d2,d2_14,d2_18,1).
bond(train,d2,d2_15,d2_19,1).
bond(train,d2,d2_17,d2_20,1).
bond(train,d2,d2_13,d2_21,1).
bond(train,d2,d2_21,d2_22,1).
bond(train,d2,d2_21,d2_23,1).
bond(train,d2,d2_21,d2_24,1).
bond(train,d2,d2_22,d2_25,1).
bond(train,d2,d2_22,d2_26,1).
bond(train,d2,d2_22,d2_27,1).
bond(train,d2,d2_16,d2_28,1).
bond(train,d2,d2_28,d2_29,1).
bond(train,d2,d2_28,d2_30,1).
bond(train,d20,d20_1,d20_2,7).
bond(train,d20,d20_2,d20_3,7).
bond(train,d20,d20_3,d20_4,7).
bond(train,d20,d20_4,d20_5,7).
bond(train,d20,d20_5,d20_6,7).
bond(train,d20,d20_6,d20_1,7).
bond(train,d20,d20_2,d20_7,1).
bond(train,d20,d20_3,d20_8,1).
bond(train,d20,d20_6,d20_9,1).
bond(train,d20,d20_4,d20_10,1).
bond(train,d20,d20_5,d20_11,1).
bond(train,d20,d20_1,d20_12,1).
bond(train,d20,d20_10,d20_13,1).
bond(train,d20,d20_13,d20_14,1).
bond(train,d20,d20_13,d20_15,1).
bond(train,d20,d20_13,d20_16,1).
bond(train,d20,d20_12,d20_17,2).
bond(train,d20,d20_12,d20_18,2).
bond(train,d20,d20_11,d20_19,1).
bond(train,d20,d20_11,d20_20,1).
bond(train,d200,d200_1,d200_2,7).
bond(train,d200,d200_2,d200_3,7).
bond(train,d200,d200_3,d200_4,7).
bond(train,d200,d200_4,d200_5,7).
bond(train,d200,d200_5,d200_6,7).
bond(train,d200,d200_6,d200_1,7).
bond(train,d200,d200_1,d200_7,1).
bond(train,d200,d200_2,d200_8,1).
bond(train,d200,d200_3,d200_9,1).
bond(train,d200,d200_4,d200_10,1).
bond(train,d200,d200_6,d200_11,1).
bond(train,d200,d200_5,d200_12,1).
bond(train,d200,d200_12,d200_13,2).
bond(train,d200,d200_12,d200_14,1).
bond(train,d201,d201_1,d201_2,7).
bond(train,d201,d201_2,d201_3,7).
bond(train,d201,d201_3,d201_4,7).
bond(train,d201,d201_4,d201_5,7).
bond(train,d201,d201_5,d201_6,7).
bond(train,d201,d201_6,d201_1,7).
bond(train,d201,d201_3,d201_7,7).
bond(train,d201,d201_7,d201_8,1).
bond(train,d201,d201_8,d201_9,7).
bond(train,d201,d201_9,d201_10,1).
bond(train,d201,d201_10,d201_4,1).
bond(train,d201,d201_2,d201_11,7).
bond(train,d201,d201_11,d201_12,7).
bond(train,d201,d201_12,d201_13,7).
bond(train,d201,d201_13,d201_7,7).
bond(train,d201,d201_8,d201_14,7).
bond(train,d201,d201_14,d201_15,7).
bond(train,d201,d201_15,d201_16,7).
bond(train,d201,d201_16,d201_17,7).
bond(train,d201,d201_17,d201_9,7).
bond(train,d201,d201_10,d201_18,2).
bond(train,d201,d201_1,d201_19,1).
bond(train,d201,d201_19,d201_20,7).
bond(train,d201,d201_20,d201_21,1).
bond(train,d201,d201_21,d201_11,1).
bond(train,d201,d201_19,d201_22,7).
bond(train,d201,d201_22,d201_23,7).
bond(train,d201,d201_23,d201_24,7).
bond(train,d201,d201_24,d201_25,7).
bond(train,d201,d201_25,d201_20,7).
bond(train,d201,d201_5,d201_26,1).
bond(train,d201,d201_6,d201_27,1).
bond(train,d201,d201_12,d201_28,1).
bond(train,d201,d201_13,d201_29,1).
bond(train,d201,d201_14,d201_30,1).
bond(train,d201,d201_15,d201_31,1).
bond(train,d201,d201_16,d201_32,1).
bond(train,d201,d201_17,d201_33,1).
bond(train,d201,d201_22,d201_34,1).
bond(train,d201,d201_23,d201_35,1).
bond(train,d201,d201_24,d201_36,1).
bond(train,d201,d201_25,d201_37,1).
bond(train,d201,d201_21,d201_38,2).
bond(train,d202,d202_1,d202_2,1).
bond(train,d202,d202_2,d202_3,1).
bond(train,d202,d202_3,d202_4,2).
bond(train,d202,d202_4,d202_5,1).
bond(train,d202,d202_5,d202_6,1).
bond(train,d202,d202_6,d202_1,1).
bond(train,d202,d202_2,d202_7,1).
bond(train,d202,d202_2,d202_8,1).
bond(train,d202,d202_6,d202_9,1).
bond(train,d202,d202_6,d202_10,1).
bond(train,d202,d202_1,d202_11,1).
bond(train,d202,d202_11,d202_12,1).
bond(train,d202,d202_11,d202_13,1).
bond(train,d202,d202_11,d202_14,1).
bond(train,d202,d202_1,d202_15,1).
bond(train,d202,d202_15,d202_16,1).
bond(train,d202,d202_15,d202_17,1).
bond(train,d202,d202_15,d202_18,1).
bond(train,d202,d202_3,d202_19,1).
bond(train,d202,d202_19,d202_20,1).
bond(train,d202,d202_19,d202_21,1).
bond(train,d202,d202_19,d202_22,1).
bond(train,d202,d202_5,d202_23,2).
bond(train,d202,d202_4,d202_24,1).
bond(train,d203,d203_1,d203_2,7).
bond(train,d203,d203_2,d203_3,7).
bond(train,d203,d203_3,d203_4,7).
bond(train,d203,d203_4,d203_5,7).
bond(train,d203,d203_5,d203_6,7).
bond(train,d203,d203_6,d203_1,7).
bond(train,d203,d203_5,d203_7,1).
bond(train,d203,d203_3,d203_8,1).
bond(train,d203,d203_1,d203_9,1).
bond(train,d203,d203_7,d203_10,1).
bond(train,d203,d203_7,d203_11,1).
bond(train,d203,d203_9,d203_12,1).
bond(train,d203,d203_9,d203_13,1).
bond(train,d203,d203_8,d203_14,1).
bond(train,d203,d203_8,d203_15,1).
bond(train,d204,d204_1,d204_2,1).
bond(train,d204,d204_2,d204_3,1).
bond(train,d204,d204_2,d204_4,1).
bond(train,d204,d204_2,d204_5,1).
bond(train,d204,d204_1,d204_6,1).
bond(train,d204,d204_6,d204_7,1).
bond(train,d204,d204_7,d204_8,1).
bond(train,d204,d204_8,d204_9,1).
bond(train,d204,d204_8,d204_10,1).
bond(train,d204,d204_8,d204_11,1).
bond(train,d204,d204_7,d204_12,1).
bond(train,d204,d204_12,d204_13,1).
bond(train,d204,d204_12,d204_14,1).
bond(train,d204,d204_12,d204_15,1).
bond(train,d204,d204_6,d204_16,2).
bond(train,d204,d204_1,d204_17,1).
bond(train,d205,d205_1,d205_2,1).
bond(train,d205,d205_2,d205_3,1).
bond(train,d205,d205_2,d205_4,1).
bond(train,d205,d205_2,d205_5,1).
bond(train,d205,d205_1,d205_6,1).
bond(train,d205,d205_3,d205_6,2).
bond(train,d205,d205_6,d205_7,1).
bond(train,d205,d205_7,d205_8,1).
bond(train,d205,d205_8,d205_9,1).
bond(train,d205,d205_8,d205_10,1).
bond(train,d205,d205_8,d205_11,1).
bond(train,d205,d205_7,d205_12,1).
bond(train,d205,d205_12,d205_13,1).
bond(train,d205,d205_12,d205_14,1).
bond(train,d205,d205_12,d205_15,1).
bond(train,d205,d205_4,d205_16,2).
bond(train,d205,d205_5,d205_16,1).
bond(train,d205,d205_16,d205_17,1).
bond(train,d205,d205_17,d205_18,1).
bond(train,d205,d205_18,d205_19,1).
bond(train,d205,d205_18,d205_20,1).
bond(train,d205,d205_18,d205_21,1).
bond(train,d205,d205_17,d205_22,1).
bond(train,d205,d205_22,d205_23,1).
bond(train,d205,d205_22,d205_24,1).
bond(train,d205,d205_22,d205_25,1).
bond(train,d206,d206_1,d206_2,1).
bond(train,d206,d206_2,d206_3,1).
bond(train,d206,d206_3,d206_4,1).
bond(train,d206,d206_4,d206_5,2).
bond(train,d206,d206_5,d206_6,1).
bond(train,d206,d206_6,d206_1,1).
bond(train,d206,d206_5,d206_7,1).
bond(train,d206,d206_2,d206_8,1).
bond(train,d206,d206_8,d206_9,1).
bond(train,d206,d206_8,d206_10,2).
bond(train,d206,d206_1,d206_11,1).
bond(train,d206,d206_1,d206_12,1).
bond(train,d206,d206_2,d206_13,1).
bond(train,d206,d206_3,d206_14,1).
bond(train,d206,d206_3,d206_15,1).
bond(train,d206,d206_6,d206_16,1).
bond(train,d206,d206_6,d206_17,1).
bond(train,d206,d206_7,d206_18,1).
bond(train,d206,d206_7,d206_19,1).
bond(train,d206,d206_7,d206_20,1).
bond(train,d206,d206_9,d206_21,1).
bond(train,d206,d206_9,d206_22,1).
bond(train,d206,d206_9,d206_23,1).
bond(train,d206,d206_10,d206_24,1).
bond(train,d206,d206_10,d206_25,1).
bond(train,d206,d206_4,d206_26,1).
bond(train,d207,d207_1,d207_2,7).
bond(train,d207,d207_2,d207_3,7).
bond(train,d207,d207_3,d207_4,7).
bond(train,d207,d207_4,d207_5,7).
bond(train,d207,d207_5,d207_6,7).
bond(train,d207,d207_6,d207_1,7).
bond(train,d207,d207_1,d207_7,1).
bond(train,d207,d207_2,d207_8,1).
bond(train,d207,d207_5,d207_9,1).
bond(train,d207,d207_6,d207_10,1).
bond(train,d207,d207_4,d207_11,1).
bond(train,d207,d207_11,d207_12,1).
bond(train,d207,d207_12,d207_13,1).
bond(train,d207,d207_13,d207_14,1).
bond(train,d207,d207_13,d207_15,1).
bond(train,d207,d207_13,d207_16,1).
bond(train,d207,d207_14,d207_17,2).
bond(train,d207,d207_11,d207_18,2).
bond(train,d207,d207_14,d207_19,1).
bond(train,d207,d207_17,d207_20,1).
bond(train,d207,d207_17,d207_21,1).
bond(train,d207,d207_22,d207_23,1).
bond(train,d207,d207_23,d207_24,1).
bond(train,d207,d207_24,d207_25,1).
bond(train,d207,d207_24,d207_26,1).
bond(train,d207,d207_24,d207_27,1).
bond(train,d207,d207_25,d207_28,2).
bond(train,d207,d207_22,d207_29,2).
bond(train,d207,d207_25,d207_30,1).
bond(train,d207,d207_28,d207_31,1).
bond(train,d207,d207_28,d207_32,1).
bond(train,d207,d207_3,d207_22,1).
bond(train,d208_1,d208_1_1,d208_1_2,1).
bond(train,d208_1,d208_1_2,d208_1_3,1).
bond(train,d208_1,d208_1_2,d208_1_4,2).
bond(train,d208_1,d208_1_3,d208_1_5,1).
bond(train,d208_1,d208_1_5,d208_1_6,1).
bond(train,d208_1,d208_1_6,d208_1_7,1).
bond(train,d208_1,d208_1_6,d208_1_8,2).
bond(train,d208_1,d208_1_1,d208_1_9,1).
bond(train,d208_1,d208_1_1,d208_1_10,1).
bond(train,d208_1,d208_1_3,d208_1_11,1).
bond(train,d208_1,d208_1_5,d208_1_12,1).
bond(train,d208_1,d208_1_7,d208_1_13,1).
bond(train,d208_1,d208_1_7,d208_1_14,1).
bond(train,d208_2,d208_2_1,d208_2_2,1).
bond(train,d208_2,d208_2_1,d208_2_3,1).
bond(train,d208_2,d208_2_1,d208_2_4,1).
bond(train,d208_2,d208_2_1,d208_2_5,1).
bond(train,d208_2,d208_2_2,d208_2_6,1).
bond(train,d208_2,d208_2_6,d208_2_7,1).
bond(train,d208_2,d208_2_7,d208_2_8,1).
bond(train,d208_2,d208_2_8,d208_2_9,1).
bond(train,d208_2,d208_2_8,d208_2_10,1).
bond(train,d208_2,d208_2_8,d208_2_11,1).
bond(train,d208_2,d208_2_9,d208_2_12,1).
bond(train,d208_2,d208_2_9,d208_2_13,1).
bond(train,d208_2,d208_2_9,d208_2_14,1).
bond(train,d208_2,d208_2_7,d208_2_15,1).
bond(train,d208_2,d208_2_15,d208_2_16,1).
bond(train,d208_2,d208_2_15,d208_2_17,1).
bond(train,d208_2,d208_2_15,d208_2_18,1).
bond(train,d208_2,d208_2_16,d208_2_19,1).
bond(train,d208_2,d208_2_16,d208_2_20,1).
bond(train,d208_2,d208_2_16,d208_2_21,1).
bond(train,d208_2,d208_2_6,d208_2_22,2).
bond(train,d208_2,d208_2_4,d208_2_23,1).
bond(train,d208_2,d208_2_23,d208_2_24,1).
bond(train,d208_2,d208_2_24,d208_2_25,1).
bond(train,d208_2,d208_2_25,d208_2_26,1).
bond(train,d208_2,d208_2_25,d208_2_27,1).
bond(train,d208_2,d208_2_25,d208_2_28,1).
bond(train,d208_2,d208_2_26,d208_2_29,1).
bond(train,d208_2,d208_2_26,d208_2_30,1).
bond(train,d208_2,d208_2_26,d208_2_31,1).
bond(train,d208_2,d208_2_24,d208_2_32,1).
bond(train,d208_2,d208_2_32,d208_2_33,1).
bond(train,d208_2,d208_2_32,d208_2_34,1).
bond(train,d208_2,d208_2_32,d208_2_35,1).
bond(train,d208_2,d208_2_33,d208_2_36,1).
bond(train,d208_2,d208_2_33,d208_2_37,1).
bond(train,d208_2,d208_2_33,d208_2_38,1).
bond(train,d208_2,d208_2_3,d208_2_39,1).
bond(train,d208_2,d208_2_39,d208_2_40,1).
bond(train,d208_2,d208_2_40,d208_2_41,1).
bond(train,d208_2,d208_2_41,d208_2_42,1).
bond(train,d208_2,d208_2_41,d208_2_43,1).
bond(train,d208_2,d208_2_41,d208_2_44,1).
bond(train,d208_2,d208_2_42,d208_2_45,1).
bond(train,d208_2,d208_2_42,d208_2_46,1).
bond(train,d208_2,d208_2_42,d208_2_47,1).
bond(train,d208_2,d208_2_40,d208_2_48,1).
bond(train,d208_2,d208_2_48,d208_2_49,1).
bond(train,d208_2,d208_2_48,d208_2_50,1).
bond(train,d208_2,d208_2_48,d208_2_51,1).
bond(train,d208_2,d208_2_49,d208_2_52,1).
bond(train,d208_2,d208_2_49,d208_2_53,1).
bond(train,d208_2,d208_2_49,d208_2_54,1).
bond(train,d208_2,d208_2_5,d208_2_55,1).
bond(train,d208_2,d208_2_55,d208_2_56,1).
bond(train,d208_2,d208_2_56,d208_2_57,1).
bond(train,d208_2,d208_2_57,d208_2_58,1).
bond(train,d208_2,d208_2_57,d208_2_59,1).
bond(train,d208_2,d208_2_57,d208_2_60,1).
bond(train,d208_2,d208_2_58,d208_2_61,1).
bond(train,d208_2,d208_2_58,d208_2_62,1).
bond(train,d208_2,d208_2_58,d208_2_63,1).
bond(train,d208_2,d208_2_56,d208_2_64,1).
bond(train,d208_2,d208_2_64,d208_2_65,1).
bond(train,d208_2,d208_2_64,d208_2_66,1).
bond(train,d208_2,d208_2_64,d208_2_67,1).
bond(train,d208_2,d208_2_65,d208_2_68,1).
bond(train,d208_2,d208_2_65,d208_2_69,1).
bond(train,d208_2,d208_2_65,d208_2_70,1).
bond(train,d208_2,d208_2_23,d208_2_71,2).
bond(train,d208_2,d208_2_55,d208_2_72,2).
bond(train,d208_2,d208_2_39,d208_2_73,2).
bond(train,d209,d209_1,d209_2,7).
bond(train,d209,d209_2,d209_3,7).
bond(train,d209,d209_3,d209_4,7).
bond(train,d209,d209_4,d209_5,7).
bond(train,d209,d209_5,d209_6,7).
bond(train,d209,d209_6,d209_1,7).
bond(train,d209,d209_1,d209_7,1).
bond(train,d209,d209_4,d209_8,1).
bond(train,d209,d209_6,d209_9,1).
bond(train,d209,d209_2,d209_10,1).
bond(train,d209,d209_3,d209_11,1).
bond(train,d209,d209_11,d209_12,1).
bond(train,d209,d209_12,d209_13,1).
bond(train,d209,d209_12,d209_14,1).
bond(train,d209,d209_12,d209_15,1).
bond(train,d209,d209_10,d209_16,1).
bond(train,d209,d209_17,d209_18,1).
bond(train,d209,d209_18,d209_19,2).
bond(train,d209,d209_18,d209_20,1).
bond(train,d209,d209_19,d209_21,1).
bond(train,d209,d209_19,d209_22,1).
bond(train,d209,d209_17,d209_5,1).
bond(train,d209,d209_17,d209_23,1).
bond(train,d209,d209_17,d209_24,1).
bond(train,d21,d21_1,d21_2,7).
bond(train,d21,d21_2,d21_3,7).
bond(train,d21,d21_3,d21_4,7).
bond(train,d21,d21_4,d21_5,7).
bond(train,d21,d21_5,d21_6,7).
bond(train,d21,d21_6,d21_1,7).
bond(train,d21,d21_2,d21_7,1).
bond(train,d21,d21_3,d21_8,1).
bond(train,d21,d21_5,d21_9,1).
bond(train,d21,d21_6,d21_10,1).
bond(train,d21,d21_11,d21_12,7).
bond(train,d21,d21_12,d21_13,7).
bond(train,d21,d21_13,d21_14,7).
bond(train,d21,d21_14,d21_15,7).
bond(train,d21,d21_15,d21_16,7).
bond(train,d21,d21_16,d21_11,7).
bond(train,d21,d21_12,d21_17,1).
bond(train,d21,d21_13,d21_18,1).
bond(train,d21,d21_15,d21_19,1).
bond(train,d21,d21_4,d21_20,1).
bond(train,d21,d21_20,d21_11,1).
bond(train,d21,d21_16,d21_21,1).
bond(train,d21,d21_14,d21_22,1).
bond(train,d21,d21_1,d21_23,1).
bond(train,d21,d21_23,d21_24,2).
bond(train,d21,d21_23,d21_25,2).
bond(train,d210,d210_1,d210_2,7).
bond(train,d210,d210_2,d210_3,7).
bond(train,d210,d210_3,d210_4,7).
bond(train,d210,d210_4,d210_5,7).
bond(train,d210,d210_5,d210_6,7).
bond(train,d210,d210_6,d210_1,7).
bond(train,d210,d210_1,d210_7,1).
bond(train,d210,d210_2,d210_8,1).
bond(train,d210,d210_4,d210_9,1).
bond(train,d210,d210_6,d210_10,1).
bond(train,d210,d210_3,d210_11,1).
bond(train,d210,d210_11,d210_12,1).
bond(train,d210,d210_11,d210_13,1).
bond(train,d210,d210_11,d210_14,1).
bond(train,d210,d210_5,d210_15,1).
bond(train,d210,d210_15,d210_16,1).
bond(train,d210,d210_16,d210_17,1).
bond(train,d210,d210_17,d210_18,1).
bond(train,d210,d210_18,d210_19,1).
bond(train,d210,d210_18,d210_20,1).
bond(train,d210,d210_18,d210_21,1).
bond(train,d210,d210_17,d210_22,1).
bond(train,d210,d210_22,d210_23,1).
bond(train,d210,d210_22,d210_24,1).
bond(train,d210,d210_22,d210_25,1).
bond(train,d210,d210_16,d210_26,2).
bond(train,d210,d210_15,d210_27,1).
bond(train,d211,d211_1,d211_2,1).
bond(train,d211,d211_1,d211_3,1).
bond(train,d211,d211_1,d211_4,1).
bond(train,d211,d211_1,d211_5,1).
bond(train,d211,d211_2,d211_6,1).
bond(train,d211,d211_6,d211_7,1).
bond(train,d211,d211_7,d211_8,1).
bond(train,d211,d211_7,d211_9,1).
bond(train,d211,d211_7,d211_10,1).
bond(train,d211,d211_6,d211_11,1).
bond(train,d211,d211_11,d211_12,1).
bond(train,d211,d211_11,d211_13,1).
bond(train,d211,d211_11,d211_14,1).
bond(train,d211,d211_6,d211_15,1).
bond(train,d211,d211_15,d211_16,2).
bond(train,d211,d211_16,d211_17,1).
bond(train,d211,d211_17,d211_18,1).
bond(train,d211,d211_18,d211_19,1).
bond(train,d211,d211_19,d211_20,1).
bond(train,d211,d211_15,d211_21,1).
bond(train,d211,d211_18,d211_22,2).
bond(train,d211,d211_20,d211_23,1).
bond(train,d211,d211_20,d211_24,1).
bond(train,d211,d211_20,d211_25,1).
bond(train,d211,d211_19,d211_26,1).
bond(train,d212,d212_1,d212_2,7).
bond(train,d212,d212_2,d212_3,7).
bond(train,d212,d212_3,d212_4,7).
bond(train,d212,d212_4,d212_5,7).
bond(train,d212,d212_5,d212_6,7).
bond(train,d212,d212_6,d212_1,7).
bond(train,d212,d212_4,d212_7,1).
bond(train,d212,d212_5,d212_8,1).
bond(train,d212,d212_1,d212_9,7).
bond(train,d212,d212_9,d212_10,7).
bond(train,d212,d212_10,d212_11,7).
bond(train,d212,d212_11,d212_12,7).
bond(train,d212,d212_12,d212_2,7).
bond(train,d212,d212_9,d212_13,1).
bond(train,d212,d212_10,d212_14,1).
bond(train,d212,d212_11,d212_15,1).
bond(train,d212,d212_12,d212_16,1).
bond(train,d212,d212_6,d212_17,1).
bond(train,d212,d212_17,d212_18,1).
bond(train,d212,d212_18,d212_19,1).
bond(train,d212,d212_17,d212_20,2).
bond(train,d212,d212_17,d212_21,2).
bond(train,d212,d212_3,d212_22,1).
bond(train,d212,d212_22,d212_23,2).
bond(train,d212,d212_23,d212_24,1).
bond(train,d212,d212_24,d212_25,7).
bond(train,d212,d212_25,d212_26,7).
bond(train,d212,d212_26,d212_27,7).
bond(train,d212,d212_27,d212_28,7).
bond(train,d212,d212_28,d212_29,7).
bond(train,d212,d212_29,d212_24,7).
bond(train,d212,d212_25,d212_30,1).
bond(train,d212,d212_27,d212_31,7).
bond(train,d212,d212_31,d212_32,7).
bond(train,d212,d212_32,d212_33,7).
bond(train,d212,d212_33,d212_34,7).
bond(train,d212,d212_34,d212_28,7).
bond(train,d212,d212_31,d212_35,1).
bond(train,d212,d212_32,d212_36,1).
bond(train,d212,d212_33,d212_37,1).
bond(train,d212,d212_34,d212_38,1).
bond(train,d212,d212_29,d212_39,1).
bond(train,d212,d212_39,d212_40,1).
bond(train,d212,d212_26,d212_41,1).
bond(train,d212,d212_41,d212_42,1).
bond(train,d212,d212_42,d212_43,1).
bond(train,d212,d212_41,d212_44,2).
bond(train,d212,d212_41,d212_45,2).
bond(train,d213,d213_1,d213_2,7).
bond(train,d213,d213_2,d213_3,7).
bond(train,d213,d213_3,d213_4,7).
bond(train,d213,d213_4,d213_5,7).
bond(train,d213,d213_5,d213_6,7).
bond(train,d213,d213_6,d213_1,7).
bond(train,d213,d213_1,d213_7,1).
bond(train,d213,d213_2,d213_8,1).
bond(train,d213,d213_3,d213_9,1).
bond(train,d213,d213_6,d213_10,1).
bond(train,d213,d213_5,d213_11,1).
bond(train,d213,d213_4,d213_12,1).
bond(train,d213,d213_11,d213_13,1).
bond(train,d213,d213_11,d213_14,1).
bond(train,d213,d213_12,d213_15,1).
bond(train,d213,d213_12,d213_16,2).
bond(train,d213,d213_15,d213_17,1).
bond(train,d214,d214_1,d214_2,1).
bond(train,d214,d214_2,d214_3,1).
bond(train,d214,d214_3,d214_4,1).
bond(train,d214,d214_4,d214_5,1).
bond(train,d214,d214_5,d214_6,1).
bond(train,d214,d214_6,d214_1,1).
bond(train,d214,d214_1,d214_7,1).
bond(train,d214,d214_1,d214_8,1).
bond(train,d214,d214_2,d214_9,1).
bond(train,d214,d214_2,d214_10,1).
bond(train,d214,d214_4,d214_11,1).
bond(train,d214,d214_4,d214_12,1).
bond(train,d214,d214_5,d214_13,1).
bond(train,d214,d214_5,d214_14,1).
bond(train,d214,d214_6,d214_15,1).
bond(train,d214,d214_6,d214_16,1).
bond(train,d214,d214_3,d214_17,1).
bond(train,d214,d214_3,d214_18,1).
bond(train,d214,d214_17,d214_19,1).
bond(train,d214,d214_19,d214_20,1).
bond(train,d214,d214_20,d214_21,1).
bond(train,d214,d214_17,d214_22,1).
bond(train,d214,d214_20,d214_23,1).
bond(train,d214,d214_19,d214_24,2).
bond(train,d214,d214_21,d214_25,1).
bond(train,d214,d214_25,d214_26,1).
bond(train,d214,d214_26,d214_27,1).
bond(train,d214,d214_27,d214_28,1).
bond(train,d214,d214_28,d214_29,1).
bond(train,d214,d214_29,d214_21,1).
bond(train,d214,d214_21,d214_30,1).
bond(train,d214,d214_25,d214_31,1).
bond(train,d214,d214_25,d214_32,1).
bond(train,d214,d214_26,d214_33,1).
bond(train,d214,d214_26,d214_34,1).
bond(train,d214,d214_27,d214_35,1).
bond(train,d214,d214_27,d214_36,1).
bond(train,d214,d214_28,d214_37,1).
bond(train,d214,d214_28,d214_38,1).
bond(train,d214,d214_29,d214_39,1).
bond(train,d214,d214_29,d214_40,1).
bond(train,d215,d215_1,d215_2,7).
bond(train,d215,d215_2,d215_3,7).
bond(train,d215,d215_3,d215_4,7).
bond(train,d215,d215_4,d215_5,7).
bond(train,d215,d215_5,d215_6,7).
bond(train,d215,d215_6,d215_1,7).
bond(train,d215,d215_1,d215_7,1).
bond(train,d215,d215_4,d215_8,1).
bond(train,d215,d215_6,d215_9,1).
bond(train,d215,d215_3,d215_10,1).
bond(train,d215,d215_10,d215_11,1).
bond(train,d215,d215_10,d215_12,1).
bond(train,d215,d215_10,d215_13,1).
bond(train,d215,d215_11,d215_14,1).
bond(train,d215,d215_11,d215_15,1).
bond(train,d215,d215_11,d215_16,1).
bond(train,d215,d215_5,d215_17,1).
bond(train,d215,d215_17,d215_18,1).
bond(train,d215,d215_17,d215_19,2).
bond(train,d215,d215_18,d215_20,1).
bond(train,d215,d215_18,d215_21,1).
bond(train,d216,d216_1,d216_2,1).
bond(train,d216,d216_2,d216_3,1).
bond(train,d216,d216_2,d216_4,1).
bond(train,d216,d216_2,d216_5,1).
bond(train,d216,d216_1,d216_6,1).
bond(train,d216,d216_3,d216_6,2).
bond(train,d216,d216_6,d216_7,1).
bond(train,d216,d216_7,d216_8,1).
bond(train,d216,d216_8,d216_9,1).
bond(train,d216,d216_8,d216_10,1).
bond(train,d216,d216_8,d216_11,1).
bond(train,d216,d216_7,d216_12,1).
bond(train,d216,d216_12,d216_13,1).
bond(train,d216,d216_12,d216_14,1).
bond(train,d216,d216_12,d216_15,1).
bond(train,d216,d216_4,d216_16,2).
bond(train,d216,d216_5,d216_16,1).
bond(train,d216,d216_16,d216_17,1).
bond(train,d216,d216_17,d216_18,1).
bond(train,d216,d216_18,d216_19,1).
bond(train,d216,d216_18,d216_20,1).
bond(train,d216,d216_18,d216_21,1).
bond(train,d216,d216_17,d216_22,1).
bond(train,d216,d216_22,d216_23,1).
bond(train,d216,d216_22,d216_24,1).
bond(train,d216,d216_22,d216_25,1).
bond(train,d217,d217_1,d217_2,7).
bond(train,d217,d217_2,d217_3,7).
bond(train,d217,d217_3,d217_4,7).
bond(train,d217,d217_4,d217_5,7).
bond(train,d217,d217_5,d217_6,7).
bond(train,d217,d217_6,d217_1,7).
bond(train,d217,d217_1,d217_7,1).
bond(train,d217,d217_3,d217_8,1).
bond(train,d217,d217_4,d217_9,1).
bond(train,d217,d217_5,d217_10,1).
bond(train,d217,d217_6,d217_11,1).
bond(train,d217,d217_12,d217_13,2).
bond(train,d217,d217_13,d217_14,1).
bond(train,d217,d217_14,d217_15,1).
bond(train,d217,d217_15,d217_16,1).
bond(train,d217,d217_16,d217_12,1).
bond(train,d217,d217_14,d217_17,1).
bond(train,d217,d217_14,d217_18,1).
bond(train,d217,d217_16,d217_2,1).
bond(train,d217,d217_13,d217_19,1).
bond(train,d217,d217_19,d217_20,1).
bond(train,d217,d217_19,d217_21,1).
bond(train,d217,d217_19,d217_22,1).
bond(train,d217,d217_15,d217_23,2).
bond(train,d218,d218_1,d218_2,1).
bond(train,d218,d218_1,d218_3,1).
bond(train,d218,d218_1,d218_4,1).
bond(train,d218,d218_1,d218_5,1).
bond(train,d218,d218_2,d218_6,1).
bond(train,d218,d218_2,d218_7,1).
bond(train,d218,d218_2,d218_8,1).
bond(train,d218,d218_6,d218_9,1).
bond(train,d218,d218_9,d218_10,1).
bond(train,d218,d218_9,d218_11,1).
bond(train,d218,d218_9,d218_12,1).
bond(train,d218,d218_10,d218_13,1).
bond(train,d218,d218_10,d218_14,1).
bond(train,d218,d218_10,d218_15,1).
bond(train,d218,d218_6,d218_16,1).
bond(train,d218,d218_16,d218_17,1).
bond(train,d218,d218_17,d218_18,1).
bond(train,d218,d218_16,d218_19,2).
bond(train,d219,d219_1,d219_2,1).
bond(train,d219,d219_1,d219_3,1).
bond(train,d219,d219_1,d219_4,1).
bond(train,d219,d219_1,d219_5,1).
bond(train,d219,d219_2,d219_6,1).
bond(train,d219,d219_2,d219_7,1).
bond(train,d219,d219_2,d219_8,1).
bond(train,d219,d219_6,d219_9,1).
bond(train,d219,d219_9,d219_10,1).
bond(train,d219,d219_9,d219_11,1).
bond(train,d219,d219_9,d219_12,1).
bond(train,d219,d219_10,d219_13,1).
bond(train,d219,d219_10,d219_14,1).
bond(train,d219,d219_10,d219_15,1).
bond(train,d219,d219_6,d219_16,1).
bond(train,d219,d219_16,d219_17,1).
bond(train,d219,d219_17,d219_18,1).
bond(train,d219,d219_18,d219_19,1).
bond(train,d219,d219_19,d219_20,1).
bond(train,d219,d219_16,d219_21,2).
bond(train,d219,d219_19,d219_22,2).
bond(train,d219,d219_20,d219_23,1).
bond(train,d219,d219_23,d219_24,1).
bond(train,d219,d219_23,d219_25,1).
bond(train,d219,d219_23,d219_26,1).
bond(train,d219,d219_24,d219_27,1).
bond(train,d219,d219_24,d219_28,1).
bond(train,d219,d219_24,d219_29,1).
bond(train,d219,d219_20,d219_30,1).
bond(train,d219,d219_30,d219_31,1).
bond(train,d219,d219_30,d219_32,1).
bond(train,d219,d219_30,d219_33,1).
bond(train,d219,d219_31,d219_34,1).
bond(train,d219,d219_31,d219_35,1).
bond(train,d219,d219_31,d219_36,1).
bond(train,d22,d22_1,d22_2,7).
bond(train,d22,d22_2,d22_3,7).
bond(train,d22,d22_3,d22_4,7).
bond(train,d22,d22_4,d22_5,7).
bond(train,d22,d22_5,d22_6,7).
bond(train,d22,d22_6,d22_1,7).
bond(train,d22,d22_1,d22_7,1).
bond(train,d22,d22_2,d22_8,1).
bond(train,d22,d22_3,d22_9,1).
bond(train,d22,d22_5,d22_10,1).
bond(train,d22,d22_6,d22_11,1).
bond(train,d22,d22_12,d22_13,7).
bond(train,d22,d22_13,d22_14,7).
bond(train,d22,d22_14,d22_15,7).
bond(train,d22,d22_15,d22_16,7).
bond(train,d22,d22_16,d22_17,7).
bond(train,d22,d22_17,d22_12,7).
bond(train,d22,d22_13,d22_18,1).
bond(train,d22,d22_14,d22_19,1).
bond(train,d22,d22_16,d22_20,1).
bond(train,d22,d22_17,d22_21,1).
bond(train,d22,d22_4,d22_22,1).
bond(train,d22,d22_22,d22_12,1).
bond(train,d22,d22_15,d22_23,1).
bond(train,d22,d22_23,d22_24,2).
bond(train,d22,d22_22,d22_25,1).
bond(train,d220,d220a_1,d220a_2,7).
bond(train,d220,d220a_2,d220a_3,7).
bond(train,d220,d220a_3,d220a_4,7).
bond(train,d220,d220a_4,d220a_5,7).
bond(train,d220,d220a_5,d220a_6,7).
bond(train,d220,d220a_6,d220a_1,7).
bond(train,d220,d220a_1,d220a_7,1).
bond(train,d220,d220a_2,d220a_8,1).
bond(train,d220,d220a_4,d220a_9,1).
bond(train,d220,d220a_6,d220a_10,1).
bond(train,d220,d220a_5,d220a_11,1).
bond(train,d220,d220a_11,d220a_12,1).
bond(train,d220,d220a_11,d220a_13,1).
bond(train,d220,d220a_11,d220a_14,1).
bond(train,d220,d220a_3,d220a_15,1).
bond(train,d220,d220a_15,d220a_16,2).
bond(train,d220,d220a_15,d220a_17,1).
bond(train,d220,d220a_16,d220a_18,1).
bond(train,d220,d220a_16,d220a_19,1).
bond(train,d220,d220b_1,d220b_2,7).
bond(train,d220,d220b_2,d220b_3,7).
bond(train,d220,d220b_3,d220b_4,7).
bond(train,d220,d220b_4,d220b_5,7).
bond(train,d220,d220b_5,d220b_6,7).
bond(train,d220,d220b_6,d220b_1,7).
bond(train,d220,d220b_1,d220b_7,1).
bond(train,d220,d220b_4,d220b_8,1).
bond(train,d220,d220b_6,d220b_9,1).
bond(train,d220,d220b_5,d220b_10,1).
bond(train,d220,d220b_10,d220b_11,1).
bond(train,d220,d220b_10,d220b_12,1).
bond(train,d220,d220b_10,d220b_13,1).
bond(train,d220,d220b_2,d220b_14,1).
bond(train,d220,d220b_14,d220b_15,2).
bond(train,d220,d220b_14,d220b_16,1).
bond(train,d220,d220b_15,d220b_17,1).
bond(train,d220,d220b_15,d220b_18,1).
bond(train,d220,d220b_3,d220b_19,1).
bond(train,d221,d221_1,d221_2,7).
bond(train,d221,d221_2,d221_3,7).
bond(train,d221,d221_3,d221_4,7).
bond(train,d221,d221_4,d221_5,7).
bond(train,d221,d221_5,d221_6,7).
bond(train,d221,d221_6,d221_1,7).
bond(train,d221,d221_2,d221_7,1).
bond(train,d221,d221_5,d221_8,1).
bond(train,d221,d221_6,d221_9,1).
bond(train,d221,d221_3,d221_10,7).
bond(train,d221,d221_10,d221_11,7).
bond(train,d221,d221_11,d221_12,7).
bond(train,d221,d221_12,d221_13,7).
bond(train,d221,d221_13,d221_4,7).
bond(train,d221,d221_10,d221_14,1).
bond(train,d221,d221_11,d221_15,1).
bond(train,d221,d221_1,d221_16,1).
bond(train,d221,d221_16,d221_17,1).
bond(train,d221,d221_17,d221_18,1).
bond(train,d221,d221_16,d221_19,2).
bond(train,d221,d221_16,d221_20,2).
bond(train,d221,d221_13,d221_21,1).
bond(train,d221,d221_21,d221_22,2).
bond(train,d221,d221_22,d221_23,1).
bond(train,d221,d221_23,d221_24,7).
bond(train,d221,d221_24,d221_25,7).
bond(train,d221,d221_25,d221_26,7).
bond(train,d221,d221_26,d221_27,7).
bond(train,d221,d221_27,d221_28,7).
bond(train,d221,d221_28,d221_23,7).
bond(train,d221,d221_24,d221_29,1).
bond(train,d221,d221_25,d221_30,1).
bond(train,d221,d221_27,d221_31,1).
bond(train,d221,d221_28,d221_32,1).
bond(train,d221,d221_26,d221_33,1).
bond(train,d221,d221_33,d221_34,1).
bond(train,d221,d221_34,d221_35,1).
bond(train,d221,d221_12,d221_36,1).
bond(train,d221,d221_36,d221_37,1).
bond(train,d221,d221_33,d221_38,2).
bond(train,d221,d221_33,d221_39,2).
bond(train,d222,d222_1,d222_2,7).
bond(train,d222,d222_2,d222_3,7).
bond(train,d222,d222_3,d222_4,7).
bond(train,d222,d222_4,d222_5,7).
bond(train,d222,d222_5,d222_6,7).
bond(train,d222,d222_6,d222_1,7).
bond(train,d222,d222_1,d222_7,1).
bond(train,d222,d222_3,d222_8,1).
bond(train,d222,d222_5,d222_9,1).
bond(train,d222,d222_9,d222_10,1).
bond(train,d222,d222_10,d222_11,1).
bond(train,d222,d222_10,d222_12,1).
bond(train,d222,d222_10,d222_13,1).
bond(train,d222,d222_9,d222_14,1).
bond(train,d222,d222_14,d222_15,1).
bond(train,d222,d222_14,d222_16,1).
bond(train,d222,d222_14,d222_17,1).
bond(train,d222,d222_4,d222_18,1).
bond(train,d222,d222_18,d222_19,1).
bond(train,d222,d222_18,d222_20,1).
bond(train,d222,d222_18,d222_21,1).
bond(train,d222,d222_6,d222_22,1).
bond(train,d222,d222_22,d222_23,1).
bond(train,d222,d222_22,d222_24,1).
bond(train,d222,d222_22,d222_25,1).
bond(train,d222,d222_2,d222_26,1).
bond(train,d222,d222_26,d222_27,1).
bond(train,d222,d222_27,d222_28,1).
bond(train,d222,d222_28,d222_29,1).
bond(train,d222,d222_29,d222_30,1).
bond(train,d222,d222_29,d222_31,1).
bond(train,d222,d222_29,d222_32,1).
bond(train,d222,d222_27,d222_33,2).
bond(train,d222,d222_28,d222_34,1).
bond(train,d223,d223_1,d223_2,7).
bond(train,d223,d223_2,d223_3,7).
bond(train,d223,d223_3,d223_4,7).
bond(train,d223,d223_4,d223_5,7).
bond(train,d223,d223_5,d223_6,7).
bond(train,d223,d223_6,d223_1,7).
bond(train,d223,d223_1,d223_7,1).
bond(train,d223,d223_2,d223_8,1).
bond(train,d223,d223_3,d223_9,1).
bond(train,d223,d223_4,d223_10,1).
bond(train,d223,d223_6,d223_11,1).
bond(train,d223,d223_5,d223_12,1).
bond(train,d223,d223_12,d223_13,1).
bond(train,d223,d223_13,d223_14,1).
bond(train,d223,d223_13,d223_15,2).
bond(train,d223,d223_12,d223_16,1).
bond(train,d223,d223_14,d223_17,1).
bond(train,d223,d223_14,d223_18,1).
bond(train,d224,d224_1,d224_2,1).
bond(train,d224,d224_2,d224_3,2).
bond(train,d224,d224_3,d224_4,1).
bond(train,d224,d224_4,d224_5,1).
bond(train,d224,d224_5,d224_1,1).
bond(train,d224,d224_1,d224_6,1).
bond(train,d224,d224_1,d224_7,1).
bond(train,d224,d224_4,d224_8,1).
bond(train,d224,d224_4,d224_9,1).
bond(train,d224,d224_2,d224_10,1).
bond(train,d224,d224_3,d224_11,1).
bond(train,d224,d224_5,d224_12,2).
bond(train,d224,d224_5,d224_13,2).
bond(train,d225,d225a_1,d225a_2,1).
bond(train,d225,d225a_1,d225a_3,1).
bond(train,d225,d225a_1,d225a_4,1).
bond(train,d225,d225a_1,d225a_5,1).
bond(train,d225,d225a_2,d225a_6,1).
bond(train,d225,d225a_6,d225a_7,1).
bond(train,d225,d225a_6,d225a_8,1).
bond(train,d225,d225a_6,d225a_9,1).
bond(train,d225,d225a_2,d225a_10,2).
bond(train,d225,d225a_10,d225a_11,1).
bond(train,d225,d225a_10,d225a_12,1).
bond(train,d225,d225a_11,d225a_13,1).
bond(train,d225,d225a_11,d225a_14,1).
bond(train,d225,d225a_11,d225a_15,1).
bond(train,d225,d225a_13,d225a_16,1).
bond(train,d225,d225a_13,d225a_17,1).
bond(train,d225,d225a_13,d225a_18,1).
bond(train,d225,d225a_16,d225a_19,1).
bond(train,d225,d225a_19,d225a_20,1).
bond(train,d225,d225a_19,d225a_21,1).
bond(train,d225,d225a_19,d225a_22,1).
bond(train,d225,d225a_16,d225a_23,2).
bond(train,d225,d225a_23,d225a_24,1).
bond(train,d225,d225a_24,d225a_25,1).
bond(train,d225,d225a_24,d225a_26,1).
bond(train,d225,d225a_24,d225a_27,1).
bond(train,d225,d225a_25,d225a_28,1).
bond(train,d225,d225a_28,d225a_29,2).
bond(train,d225,d225a_28,d225a_30,1).
bond(train,d225,d225a_30,d225a_31,1).
bond(train,d225,d225a_30,d225a_32,1).
bond(train,d225,d225a_30,d225a_33,1).
bond(train,d225,d225a_23,d225a_34,1).
bond(train,d225,d225b_1,d225b_2,1).
bond(train,d225,d225b_1,d225b_3,1).
bond(train,d225,d225b_1,d225b_4,1).
bond(train,d225,d225b_1,d225b_5,1).
bond(train,d225,d225b_2,d225b_6,1).
bond(train,d225,d225b_6,d225b_7,1).
bond(train,d225,d225b_6,d225b_8,1).
bond(train,d225,d225b_6,d225b_9,1).
bond(train,d225,d225b_2,d225b_10,2).
bond(train,d225,d225b_10,d225b_11,1).
bond(train,d225,d225b_10,d225b_12,1).
bond(train,d225,d225b_11,d225b_13,1).
bond(train,d225,d225b_11,d225b_14,1).
bond(train,d225,d225b_11,d225b_15,1).
bond(train,d225,d225b_13,d225b_16,1).
bond(train,d225,d225b_13,d225b_17,1).
bond(train,d225,d225b_13,d225b_18,1).
bond(train,d225,d225b_16,d225b_19,1).
bond(train,d225,d225b_19,d225b_20,1).
bond(train,d225,d225b_19,d225b_21,1).
bond(train,d225,d225b_19,d225b_22,1).
bond(train,d225,d225b_16,d225b_23,1).
bond(train,d225,d225b_23,d225b_24,1).
bond(train,d225,d225b_24,d225b_25,1).
bond(train,d225,d225b_24,d225b_26,1).
bond(train,d225,d225b_24,d225b_27,1).
bond(train,d225,d225b_25,d225b_28,1).
bond(train,d225,d225b_28,d225b_29,2).
bond(train,d225,d225b_28,d225b_30,1).
bond(train,d225,d225b_30,d225b_31,1).
bond(train,d225,d225b_30,d225b_32,1).
bond(train,d225,d225b_30,d225b_33,1).
bond(train,d225,d225b_16,d225b_34,1).
bond(train,d225,d225b_23,d225b_35,1).
bond(train,d225,d225b_23,d225b_36,1).
bond(train,d226,d226_1,d226_2,7).
bond(train,d226,d226_2,d226_3,7).
bond(train,d226,d226_3,d226_4,7).
bond(train,d226,d226_4,d226_5,7).
bond(train,d226,d226_5,d226_6,7).
bond(train,d226,d226_6,d226_1,7).
bond(train,d226,d226_1,d226_7,1).
bond(train,d226,d226_3,d226_8,1).
bond(train,d226,d226_5,d226_9,1).
bond(train,d226,d226_6,d226_10,1).
bond(train,d226,d226_4,d226_11,1).
bond(train,d226,d226_11,d226_12,1).
bond(train,d226,d226_11,d226_13,1).
bond(train,d226,d226_11,d226_14,1).
bond(train,d227,d227_1,d227_2,1).
bond(train,d227,d227_2,d227_3,1).
bond(train,d227,d227_2,d227_4,1).
bond(train,d227,d227_2,d227_5,1).
bond(train,d227,d227_3,d227_6,1).
bond(train,d227,d227_6,d227_7,1).
bond(train,d227,d227_6,d227_8,1).
bond(train,d227,d227_6,d227_9,1).
bond(train,d227,d227_3,d227_10,1).
bond(train,d227,d227_3,d227_11,1).
bond(train,d228,d228_1,d228_2,1).
bond(train,d228,d228_1,d228_3,1).
bond(train,d228,d228_1,d228_4,1).
bond(train,d228,d228_1,d228_5,1).
bond(train,d228,d228_3,d228_6,1).
bond(train,d228,d228_3,d228_7,1).
bond(train,d228,d228_3,d228_8,1).
bond(train,d229,d229_1,d229_2,1).
bond(train,d229,d229_1,d229_3,1).
bond(train,d229,d229_1,d229_4,1).
bond(train,d229,d229_1,d229_5,1).
bond(train,d229,d229_2,d229_6,1).
bond(train,d229,d229_2,d229_7,1).
bond(train,d229,d229_2,d229_8,1).
bond(train,d230,d230_1,d230_2,1).
bond(train,d230,d230_2,d230_3,2).
bond(train,d230,d230_2,d230_4,1).
bond(train,d230,d230_3,d230_5,1).
bond(train,d230,d230_3,d230_6,1).
bond(train,d230,d230_5,d230_7,1).
bond(train,d230,d230_5,d230_8,1).
bond(train,d230,d230_5,d230_9,1).
bond(train,d231,d231_1,d231_2,7).
bond(train,d231,d231_2,d231_3,7).
bond(train,d231,d231_3,d231_4,7).
bond(train,d231,d231_4,d231_5,7).
bond(train,d231,d231_5,d231_6,7).
bond(train,d231,d231_6,d231_1,7).
bond(train,d231,d231_1,d231_7,1).
bond(train,d231,d231_2,d231_8,1).
bond(train,d231,d231_4,d231_9,1).
bond(train,d231,d231_5,d231_10,1).
bond(train,d231,d231_6,d231_11,1).
bond(train,d231,d231_11,d231_12,1).
bond(train,d231,d231_12,d231_13,1).
bond(train,d231,d231_13,d231_14,1).
bond(train,d231,d231_14,d231_15,1).
bond(train,d231,d231_15,d231_16,1).
bond(train,d231,d231_16,d231_17,1).
bond(train,d231,d231_17,d231_18,1).
bond(train,d231,d231_18,d231_13,1).
bond(train,d231,d231_13,d231_19,1).
bond(train,d231,d231_14,d231_20,1).
bond(train,d231,d231_14,d231_21,1).
bond(train,d231,d231_17,d231_22,1).
bond(train,d231,d231_17,d231_23,1).
bond(train,d231,d231_18,d231_24,1).
bond(train,d231,d231_18,d231_25,1).
bond(train,d231,d231_15,d231_26,2).
bond(train,d231,d231_26,d231_27,1).
bond(train,d231,d231_27,d231_28,1).
bond(train,d231,d231_28,d231_29,1).
bond(train,d231,d231_29,d231_16,1).
bond(train,d231,d231_27,d231_30,1).
bond(train,d231,d231_27,d231_31,1).
bond(train,d231,d231_16,d231_32,1).
bond(train,d231,d231_28,d231_33,1).
bond(train,d231,d231_33,d231_34,1).
bond(train,d231,d231_34,d231_35,1).
bond(train,d231,d231_35,d231_36,1).
bond(train,d231,d231_36,d231_29,1).
bond(train,d231,d231_29,d231_37,1).
bond(train,d231,d231_28,d231_38,1).
bond(train,d231,d231_35,d231_39,1).
bond(train,d231,d231_35,d231_40,1).
bond(train,d231,d231_36,d231_41,1).
bond(train,d231,d231_36,d231_42,1).
bond(train,d231,d231_33,d231_43,1).
bond(train,d231,d231_43,d231_44,1).
bond(train,d231,d231_44,d231_45,1).
bond(train,d231,d231_45,d231_34,1).
bond(train,d231,d231_33,d231_46,1).
bond(train,d231,d231_43,d231_47,1).
bond(train,d231,d231_43,d231_48,1).
bond(train,d231,d231_44,d231_49,1).
bond(train,d231,d231_44,d231_50,1).
bond(train,d231,d231_11,d231_51,2).
bond(train,d231,d231_3,d231_52,1).
bond(train,d231,d231_52,d231_53,1).
bond(train,d231,d231_53,d231_54,1).
bond(train,d231,d231_53,d231_55,1).
bond(train,d231,d231_53,d231_56,1).
bond(train,d231,d231_54,d231_57,1).
bond(train,d231,d231_54,d231_58,1).
bond(train,d231,d231_54,d231_59,1).
bond(train,d231,d231_52,d231_60,1).
bond(train,d231,d231_60,d231_61,1).
bond(train,d231,d231_60,d231_62,1).
bond(train,d231,d231_60,d231_63,1).
bond(train,d231,d231_61,d231_64,1).
bond(train,d231,d231_61,d231_65,1).
bond(train,d231,d231_61,d231_66,1).
bond(train,d231,d231_26,d231_67,1).
bond(train,d231,d231_34,d231_68,1).
bond(train,d231,d231_68,d231_69,1).
bond(train,d231,d231_68,d231_70,1).
bond(train,d231,d231_68,d231_71,1).
bond(train,d231,d231_45,d231_72,1).
bond(train,d231,d231_45,d231_73,1).
bond(train,d231,d231_72,d231_74,1).
bond(train,d231,d231_74,d231_75,1).
bond(train,d231,d231_74,d231_76,1).
bond(train,d231,d231_74,d231_77,1).
bond(train,d231,d231_72,d231_78,1).
bond(train,d231,d231_72,d231_79,1).
bond(train,d231,d231_78,d231_80,1).
bond(train,d231,d231_78,d231_81,1).
bond(train,d231,d231_78,d231_82,1).
bond(train,d231,d231_80,d231_83,1).
bond(train,d231,d231_80,d231_84,1).
bond(train,d231,d231_80,d231_85,1).
bond(train,d231,d231_83,d231_86,1).
bond(train,d231,d231_83,d231_87,1).
bond(train,d231,d231_83,d231_88,1).
bond(train,d231,d231_86,d231_89,1).
bond(train,d231,d231_89,d231_90,1).
bond(train,d231,d231_89,d231_91,1).
bond(train,d231,d231_89,d231_92,1).
bond(train,d231,d231_86,d231_93,1).
bond(train,d231,d231_86,d231_94,1).
bond(train,d231,d231_93,d231_95,1).
bond(train,d231,d231_93,d231_96,1).
bond(train,d231,d231_93,d231_97,1).
bond(train,d232,d232_1,d232_2,1).
bond(train,d232,d232_2,d232_3,1).
bond(train,d232,d232_3,d232_4,1).
bond(train,d232,d232_3,d232_5,1).
bond(train,d232,d232_3,d232_6,1).
bond(train,d232,d232_1,d232_7,1).
bond(train,d232,d232_1,d232_8,1).
bond(train,d232,d232_1,d232_9,1).
bond(train,d232,d232_7,d232_2,1).
bond(train,d232,d232_2,d232_10,1).
bond(train,d233,d233_1,d233_2,7).
bond(train,d233,d233_2,d233_3,7).
bond(train,d233,d233_3,d233_4,7).
bond(train,d233,d233_4,d233_5,7).
bond(train,d233,d233_5,d233_6,7).
bond(train,d233,d233_6,d233_1,7).
bond(train,d233,d233_3,d233_7,1).
bond(train,d233,d233_6,d233_8,1).
bond(train,d233,d233_5,d233_9,1).
bond(train,d233,d233_4,d233_10,1).
bond(train,d233,d233_2,d233_11,1).
bond(train,d233,d233_1,d233_12,1).
bond(train,d233,d233_12,d233_13,1).
bond(train,d233,d233_12,d233_14,2).
bond(train,d233,d233_14,d233_15,1).
bond(train,d233,d233_13,d233_16,1).
bond(train,d233,d233_16,d233_17,2).
bond(train,d233,d233_16,d233_18,1).
bond(train,d233,d233_18,d233_19,1).
bond(train,d233,d233_19,d233_20,1).
bond(train,d233,d233_19,d233_21,1).
bond(train,d233,d233_19,d233_22,1).
bond(train,d233,d233_16,d233_23,1).
bond(train,d233,d233_23,d233_24,1).
bond(train,d233,d233_24,d233_25,1).
bond(train,d233,d233_24,d233_26,1).
bond(train,d233,d233_24,d233_27,1).
bond(train,d233,d233_14,d233_28,1).
bond(train,d234,d234_1,d234_2,1).
bond(train,d234,d234_1,d234_3,1).
bond(train,d234,d234_1,d234_4,1).
bond(train,d234,d234_1,d234_5,1).
bond(train,d234,d234_2,d234_6,1).
bond(train,d234,d234_6,d234_7,1).
bond(train,d234,d234_6,d234_8,1).
bond(train,d234,d234_8,d234_9,1).
bond(train,d234,d234_9,d234_10,1).
bond(train,d234,d234_9,d234_11,1).
bond(train,d234,d234_9,d234_12,1).
bond(train,d234,d234_7,d234_13,1).
bond(train,d234,d234_13,d234_14,1).
bond(train,d234,d234_13,d234_15,1).
bond(train,d234,d234_13,d234_16,1).
bond(train,d234,d234_6,d234_17,2).
bond(train,d235,d235_1,d235_2,2).
bond(train,d235,d235_1,d235_3,1).
bond(train,d235,d235_3,d235_4,1).
bond(train,d235,d235_4,d235_5,1).
bond(train,d235,d235_4,d235_6,1).
bond(train,d235,d235_4,d235_7,1).
bond(train,d235,d235_5,d235_8,1).
bond(train,d235,d235_5,d235_9,1).
bond(train,d235,d235_5,d235_10,1).
bond(train,d235,d235_9,d235_11,1).
bond(train,d235,d235_9,d235_12,1).
bond(train,d235,d235_9,d235_13,1).
bond(train,d235,d235_1,d235_14,1).
bond(train,d235,d235_14,d235_15,1).
bond(train,d235,d235_1,d235_16,1).
bond(train,d235,d235_16,d235_17,1).
bond(train,d235,d235_15,d235_18,1).
bond(train,d235,d235_15,d235_19,1).
bond(train,d235,d235_15,d235_20,1).
bond(train,d235,d235_18,d235_21,1).
bond(train,d235,d235_21,d235_22,1).
bond(train,d235,d235_21,d235_23,1).
bond(train,d235,d235_21,d235_24,1).
bond(train,d235,d235_18,d235_25,1).
bond(train,d235,d235_18,d235_26,1).
bond(train,d235,d235_17,d235_27,1).
bond(train,d235,d235_17,d235_28,1).
bond(train,d235,d235_17,d235_29,1).
bond(train,d235,d235_27,d235_30,1).
bond(train,d235,d235_27,d235_31,1).
bond(train,d235,d235_27,d235_32,1).
bond(train,d235,d235_30,d235_33,1).
bond(train,d235,d235_30,d235_34,1).
bond(train,d235,d235_30,d235_35,1).
bond(train,d236,d236_1,d236_2,2).
bond(train,d236,d236_2,d236_3,1).
bond(train,d236,d236_2,d236_4,1).
bond(train,d236,d236_1,d236_5,1).
bond(train,d236,d236_5,d236_6,1).
bond(train,d236,d236_5,d236_7,1).
bond(train,d236,d236_5,d236_8,1).
bond(train,d236,d236_1,d236_9,1).
bond(train,d236,d236_9,d236_10,1).
bond(train,d236,d236_9,d236_11,1).
bond(train,d236,d236_9,d236_12,1).
bond(train,d237,d237_1,d237_2,2).
bond(train,d237,d237_1,d237_3,1).
bond(train,d237,d237_1,d237_4,1).
bond(train,d237,d237_4,d237_5,1).
bond(train,d237,d237_5,d237_6,1).
bond(train,d237,d237_5,d237_7,1).
bond(train,d237,d237_5,d237_8,1).
bond(train,d237,d237_3,d237_9,1).
bond(train,d237,d237_9,d237_10,1).
bond(train,d237,d237_9,d237_11,1).
bond(train,d237,d237_9,d237_12,1).
bond(train,d237,d237_1,d237_13,1).
bond(train,d237,d237_13,d237_14,1).
bond(train,d237,d237_14,d237_15,2).
bond(train,d237,d237_14,d237_16,1).
bond(train,d237,d237_15,d237_17,1).
bond(train,d237,d237_15,d237_18,1).
bond(train,d238,d238_1,d238_2,7).
bond(train,d238,d238_2,d238_3,7).
bond(train,d238,d238_3,d238_4,7).
bond(train,d238,d238_4,d238_5,7).
bond(train,d238,d238_5,d238_6,7).
bond(train,d238,d238_6,d238_1,7).
bond(train,d238,d238_1,d238_7,1).
bond(train,d238,d238_2,d238_8,1).
bond(train,d238,d238_4,d238_9,1).
bond(train,d238,d238_6,d238_10,1).
bond(train,d238,d238_11,d238_12,1).
bond(train,d238,d238_12,d238_13,1).
bond(train,d238,d238_12,d238_14,1).
bond(train,d238,d238_12,d238_15,1).
bond(train,d238,d238_13,d238_14,1).
bond(train,d238,d238_13,d238_16,1).
bond(train,d238,d238_13,d238_17,1).
bond(train,d238,d238_5,d238_18,1).
bond(train,d238,d238_18,d238_11,1).
bond(train,d238,d238_11,d238_19,1).
bond(train,d238,d238_11,d238_20,1).
bond(train,d238,d238_3,d238_21,1).
bond(train,d238,d238_21,d238_22,1).
bond(train,d238,d238_22,d238_23,1).
bond(train,d238,d238_22,d238_24,1).
bond(train,d238,d238_22,d238_25,1).
bond(train,d238,d238_23,d238_26,1).
bond(train,d238,d238_23,d238_27,1).
bond(train,d238,d238_23,d238_28,1).
bond(train,d238,d238_27,d238_26,1).
bond(train,d238,d238_26,d238_29,1).
bond(train,d238,d238_26,d238_30,1).
bond(train,d239,d239_1,d239_2,1).
bond(train,d239,d239_2,d239_3,1).
bond(train,d239,d239_2,d239_4,1).
bond(train,d239,d239_2,d239_5,1).
bond(train,d239,d239_1,d239_6,1).
bond(train,d239,d239_1,d239_7,1).
bond(train,d239,d239_1,d239_8,1).
bond(train,d23_1,d23_1_1,d23_1_2,7).
bond(train,d23_1,d23_1_2,d23_1_3,7).
bond(train,d23_1,d23_1_3,d23_1_4,7).
bond(train,d23_1,d23_1_4,d23_1_5,7).
bond(train,d23_1,d23_1_5,d23_1_6,7).
bond(train,d23_1,d23_1_6,d23_1_1,7).
bond(train,d23_1,d23_1_2,d23_1_7,1).
bond(train,d23_1,d23_1_3,d23_1_8,1).
bond(train,d23_1,d23_1_5,d23_1_9,1).
bond(train,d23_1,d23_1_6,d23_1_10,1).
bond(train,d23_1,d23_1_11,d23_1_12,7).
bond(train,d23_1,d23_1_12,d23_1_13,7).
bond(train,d23_1,d23_1_13,d23_1_14,7).
bond(train,d23_1,d23_1_14,d23_1_15,7).
bond(train,d23_1,d23_1_15,d23_1_16,7).
bond(train,d23_1,d23_1_16,d23_1_11,7).
bond(train,d23_1,d23_1_12,d23_1_17,1).
bond(train,d23_1,d23_1_13,d23_1_18,1).
bond(train,d23_1,d23_1_15,d23_1_19,1).
bond(train,d23_1,d23_1_16,d23_1_20,1).
bond(train,d23_1,d23_1_4,d23_1_21,1).
bond(train,d23_1,d23_1_21,d23_1_11,1).
bond(train,d23_1,d23_1_14,d23_1_22,1).
bond(train,d23_1,d23_1_1,d23_1_23,1).
bond(train,d23_1,d23_1_23,d23_1_24,1).
bond(train,d23_1,d23_1_23,d23_1_25,1).
bond(train,d23_1,d23_1_22,d23_1_26,1).
bond(train,d23_1,d23_1_22,d23_1_27,1).
bond(train,d23_2,d23_2_1,d23_2_2,7).
bond(train,d23_2,d23_2_2,d23_2_3,7).
bond(train,d23_2,d23_2_3,d23_2_4,7).
bond(train,d23_2,d23_2_4,d23_2_5,7).
bond(train,d23_2,d23_2_5,d23_2_6,7).
bond(train,d23_2,d23_2_6,d23_2_1,7).
bond(train,d23_2,d23_2_1,d23_2_7,1).
bond(train,d23_2,d23_2_2,d23_2_8,1).
bond(train,d23_2,d23_2_3,d23_2_9,1).
bond(train,d23_2,d23_2_5,d23_2_10,1).
bond(train,d23_2,d23_2_6,d23_2_11,1).
bond(train,d23_2,d23_2_12,d23_2_13,7).
bond(train,d23_2,d23_2_13,d23_2_14,7).
bond(train,d23_2,d23_2_14,d23_2_15,7).
bond(train,d23_2,d23_2_15,d23_2_16,7).
bond(train,d23_2,d23_2_16,d23_2_17,7).
bond(train,d23_2,d23_2_17,d23_2_12,7).
bond(train,d23_2,d23_2_13,d23_2_18,1).
bond(train,d23_2,d23_2_14,d23_2_19,1).
bond(train,d23_2,d23_2_4,d23_2_20,1).
bond(train,d23_2,d23_2_20,d23_2_21,2).
bond(train,d23_2,d23_2_21,d23_2_12,1).
bond(train,d23_2,d23_2_17,d23_2_22,1).
bond(train,d23_2,d23_2_15,d23_2_23,1).
bond(train,d23_2,d23_2_22,d23_2_24,1).
bond(train,d23_2,d23_2_22,d23_2_25,1).
bond(train,d23_2,d23_2_23,d23_2_26,1).
bond(train,d23_2,d23_2_23,d23_2_27,1).
bond(train,d24,d24_1,d24_2,7).
bond(train,d24,d24_2,d24_3,7).
bond(train,d24,d24_3,d24_4,7).
bond(train,d24,d24_4,d24_5,7).
bond(train,d24,d24_5,d24_6,7).
bond(train,d24,d24_6,d24_1,7).
bond(train,d24,d24_2,d24_7,1).
bond(train,d24,d24_3,d24_8,1).
bond(train,d24,d24_5,d24_9,1).
bond(train,d24,d24_6,d24_10,1).
bond(train,d24,d24_11,d24_12,7).
bond(train,d24,d24_12,d24_13,7).
bond(train,d24,d24_13,d24_14,7).
bond(train,d24,d24_14,d24_15,7).
bond(train,d24,d24_15,d24_16,7).
bond(train,d24,d24_16,d24_11,7).
bond(train,d24,d24_12,d24_17,1).
bond(train,d24,d24_13,d24_18,1).
bond(train,d24,d24_15,d24_19,1).
bond(train,d24,d24_16,d24_20,1).
bond(train,d24,d24_4,d24_21,1).
bond(train,d24,d24_21,d24_11,1).
bond(train,d24,d24_14,d24_22,1).
bond(train,d24,d24_1,d24_23,1).
bond(train,d24,d24_23,d24_24,1).
bond(train,d24,d24_23,d24_25,1).
bond(train,d24,d24_22,d24_26,1).
bond(train,d24,d24_22,d24_27,1).
bond(train,d240,d240_1,d240_2,1).
bond(train,d240,d240_1,d240_3,1).
bond(train,d240,d240_1,d240_4,1).
bond(train,d240,d240_1,d240_5,1).
bond(train,d240,d240_3,d240_2,1).
bond(train,d240,d240_2,d240_6,1).
bond(train,d240,d240_2,d240_7,1).
bond(train,d240,d240_6,d240_8,1).
bond(train,d240,d240_6,d240_9,1).
bond(train,d240,d240_6,d240_10,1).
bond(train,d240,d240_8,d240_11,1).
bond(train,d241,d241_1,d241_2,1).
bond(train,d241,d241_1,d241_3,1).
bond(train,d241,d241_1,d241_4,1).
bond(train,d241,d241_1,d241_5,1).
bond(train,d241,d241_2,d241_6,1).
bond(train,d241,d241_2,d241_7,1).
bond(train,d241,d241_2,d241_8,1).
bond(train,d241,d241_7,d241_9,1).
bond(train,d241,d241_7,d241_10,1).
bond(train,d241,d241_7,d241_11,1).
bond(train,d241,d241_3,d241_12,1).
bond(train,d241,d241_6,d241_13,1).
bond(train,d242,d242_1,d242_2,1).
bond(train,d242,d242_2,d242_3,1).
bond(train,d242,d242_3,d242_4,1).
bond(train,d242,d242_4,d242_5,1).
bond(train,d242,d242_5,d242_6,1).
bond(train,d242,d242_6,d242_1,1).
bond(train,d242,d242_2,d242_7,1).
bond(train,d242,d242_2,d242_8,1).
bond(train,d242,d242_3,d242_9,1).
bond(train,d242,d242_3,d242_10,1).
bond(train,d242,d242_5,d242_11,1).
bond(train,d242,d242_5,d242_12,1).
bond(train,d242,d242_6,d242_13,1).
bond(train,d242,d242_6,d242_14,1).
bond(train,d242,d242_1,d242_13,1).
bond(train,d242,d242_1,d242_15,1).
bond(train,d242,d242_4,d242_16,1).
bond(train,d242,d242_4,d242_17,1).
bond(train,d242,d242_16,d242_18,1).
bond(train,d242,d242_16,d242_19,1).
bond(train,d242,d242_16,d242_20,1).
bond(train,d242,d242_19,d242_18,1).
bond(train,d242,d242_18,d242_21,1).
bond(train,d242,d242_18,d242_22,1).
bond(train,d243,d243_1,d243_2,1).
bond(train,d243,d243_1,d243_3,1).
bond(train,d243,d243_1,d243_4,1).
bond(train,d243,d243_1,d243_5,1).
bond(train,d243,d243_3,d243_6,1).
bond(train,d243,d243_6,d243_7,1).
bond(train,d243,d243_6,d243_8,1).
bond(train,d243,d243_6,d243_9,1).
bond(train,d243,d243_3,d243_10,1).
bond(train,d243,d243_3,d243_11,1).
bond(train,d243,d243_10,d243_12,1).
bond(train,d243,d243_12,d243_13,1).
bond(train,d243,d243_13,d243_14,1).
bond(train,d243,d243_13,d243_15,1).
bond(train,d243,d243_13,d243_16,1).
bond(train,d243,d243_12,d243_17,1).
bond(train,d243,d243_12,d243_18,1).
bond(train,d243,d243_17,d243_19,1).
bond(train,d243,d243_17,d243_20,1).
bond(train,d243,d243_17,d243_21,1).
bond(train,d244,d244_1,d244_2,2).
bond(train,d244,d244_1,d244_3,1).
bond(train,d244,d244_1,d244_4,1).
bond(train,d244,d244_1,d244_5,1).
bond(train,d244,d244_3,d244_6,1).
bond(train,d244,d244_6,d244_7,1).
bond(train,d244,d244_6,d244_8,1).
bond(train,d244,d244_6,d244_9,1).
bond(train,d244,d244_5,d244_10,1).
bond(train,d244,d244_10,d244_11,1).
bond(train,d244,d244_10,d244_12,1).
bond(train,d244,d244_10,d244_13,1).
bond(train,d245,d245_1,d245_2,1).
bond(train,d245,d245_2,d245_3,1).
bond(train,d245,d245_3,d245_4,1).
bond(train,d245,d245_3,d245_5,1).
bond(train,d245,d245_3,d245_6,1).
bond(train,d245,d245_4,d245_7,1).
bond(train,d245,d245_4,d245_8,1).
bond(train,d245,d245_4,d245_9,1).
bond(train,d245,d245_1,d245_10,1).
bond(train,d245,d245_1,d245_11,1).
bond(train,d245,d245_1,d245_12,1).
bond(train,d245,d245_10,d245_2,1).
bond(train,d245,d245_2,d245_13,1).
bond(train,d246,d246_1,d246_2,7).
bond(train,d246,d246_2,d246_3,7).
bond(train,d246,d246_3,d246_4,7).
bond(train,d246,d246_4,d246_5,7).
bond(train,d246,d246_5,d246_6,7).
bond(train,d246,d246_6,d246_1,7).
bond(train,d246,d246_1,d246_7,1).
bond(train,d246,d246_3,d246_8,1).
bond(train,d246,d246_4,d246_9,1).
bond(train,d246,d246_6,d246_10,1).
bond(train,d246,d246_5,d246_11,1).
bond(train,d246,d246_11,d246_12,1).
bond(train,d246,d246_12,d246_13,1).
bond(train,d246,d246_12,d246_14,1).
bond(train,d246,d246_12,d246_15,1).
bond(train,d246,d246_13,d246_16,1).
bond(train,d246,d246_13,d246_17,1).
bond(train,d246,d246_13,d246_18,1).
bond(train,d246,d246_11,d246_19,1).
bond(train,d246,d246_19,d246_20,1).
bond(train,d246,d246_19,d246_21,1).
bond(train,d246,d246_19,d246_22,1).
bond(train,d246,d246_20,d246_23,1).
bond(train,d246,d246_20,d246_24,1).
bond(train,d246,d246_20,d246_25,1).
bond(train,d246,d246_2,d246_26,1).
bond(train,d246,d246_26,d246_27,1).
bond(train,d246,d246_27,d246_28,1).
bond(train,d246,d246_26,d246_29,2).
bond(train,d246,d246_28,d246_30,7).
bond(train,d246,d246_30,d246_31,7).
bond(train,d246,d246_31,d246_32,7).
bond(train,d246,d246_32,d246_33,7).
bond(train,d246,d246_33,d246_34,7).
bond(train,d246,d246_34,d246_28,7).
bond(train,d246,d246_30,d246_35,1).
bond(train,d246,d246_33,d246_36,1).
bond(train,d246,d246_34,d246_37,1).
bond(train,d246,d246_31,d246_38,1).
bond(train,d246,d246_38,d246_39,1).
bond(train,d246,d246_39,d246_40,1).
bond(train,d246,d246_40,d246_41,1).
bond(train,d246,d246_41,d246_32,1).
bond(train,d246,d246_38,d246_42,1).
bond(train,d246,d246_38,d246_43,1).
bond(train,d246,d246_39,d246_44,1).
bond(train,d246,d246_39,d246_45,1).
bond(train,d246,d246_40,d246_46,1).
bond(train,d246,d246_46,d246_47,1).
bond(train,d246,d246_47,d246_48,1).
bond(train,d246,d246_48,d246_49,1).
bond(train,d246,d246_49,d246_41,1).
bond(train,d246,d246_41,d246_50,1).
bond(train,d246,d246_40,d246_51,1).
bond(train,d246,d246_48,d246_52,1).
bond(train,d246,d246_48,d246_53,1).
bond(train,d246,d246_49,d246_54,1).
bond(train,d246,d246_49,d246_55,1).
bond(train,d246,d246_46,d246_56,1).
bond(train,d246,d246_56,d246_57,1).
bond(train,d246,d246_57,d246_58,1).
bond(train,d246,d246_58,d246_47,1).
bond(train,d246,d246_46,d246_59,1).
bond(train,d246,d246_56,d246_60,1).
bond(train,d246,d246_56,d246_61,1).
bond(train,d246,d246_57,d246_62,1).
bond(train,d246,d246_57,d246_63,1).
bond(train,d246,d246_47,d246_64,1).
bond(train,d246,d246_64,d246_65,1).
bond(train,d246,d246_64,d246_66,1).
bond(train,d246,d246_64,d246_67,1).
bond(train,d246,d246_58,d246_68,1).
bond(train,d246,d246_58,d246_69,1).
bond(train,d246,d246_68,d246_70,1).
bond(train,d246,d246_70,d246_71,1).
bond(train,d246,d246_70,d246_72,2).
bond(train,d246,d246_71,d246_73,7).
bond(train,d246,d246_73,d246_74,7).
bond(train,d246,d246_74,d246_75,7).
bond(train,d246,d246_75,d246_76,7).
bond(train,d246,d246_76,d246_77,7).
bond(train,d246,d246_77,d246_71,7).
bond(train,d246,d246_73,d246_78,1).
bond(train,d246,d246_75,d246_79,1).
bond(train,d246,d246_76,d246_80,1).
bond(train,d246,d246_77,d246_81,1).
bond(train,d246,d246_74,d246_82,1).
bond(train,d246,d246_82,d246_83,1).
bond(train,d246,d246_83,d246_84,1).
bond(train,d246,d246_83,d246_85,1).
bond(train,d246,d246_83,d246_86,1).
bond(train,d246,d246_84,d246_87,1).
bond(train,d246,d246_84,d246_88,1).
bond(train,d246,d246_84,d246_89,1).
bond(train,d246,d246_82,d246_90,1).
bond(train,d246,d246_90,d246_91,1).
bond(train,d246,d246_90,d246_92,1).
bond(train,d246,d246_90,d246_93,1).
bond(train,d246,d246_91,d246_94,1).
bond(train,d246,d246_91,d246_95,1).
bond(train,d246,d246_91,d246_96,1).
bond(train,d247,d247_1,d247_2,1).
bond(train,d247,d247_2,d247_3,1).
bond(train,d247,d247_3,d247_4,1).
bond(train,d247,d247_4,d247_5,1).
bond(train,d247,d247_5,d247_6,1).
bond(train,d247,d247_6,d247_1,1).
bond(train,d247,d247_1,d247_7,1).
bond(train,d247,d247_1,d247_8,1).
bond(train,d247,d247_2,d247_9,1).
bond(train,d247,d247_2,d247_10,1).
bond(train,d247,d247_4,d247_11,1).
bond(train,d247,d247_4,d247_12,1).
bond(train,d247,d247_5,d247_13,1).
bond(train,d247,d247_5,d247_14,1).
bond(train,d247,d247_6,d247_15,1).
bond(train,d247,d247_15,d247_16,1).
bond(train,d247,d247_15,d247_17,1).
bond(train,d247,d247_17,d247_18,1).
bond(train,d247,d247_18,d247_19,1).
bond(train,d247,d247_18,d247_20,1).
bond(train,d247,d247_18,d247_21,1).
bond(train,d247,d247_16,d247_22,1).
bond(train,d247,d247_22,d247_23,1).
bond(train,d247,d247_22,d247_24,1).
bond(train,d247,d247_22,d247_25,1).
bond(train,d247,d247_15,d247_26,2).
bond(train,d248,d248_1,d248_2,1).
bond(train,d248,d248_1,d248_3,1).
bond(train,d248,d248_3,d248_4,1).
bond(train,d248,d248_2,d248_4,1).
bond(train,d248,d248_2,d248_5,1).
bond(train,d248,d248_2,d248_6,1).
bond(train,d248,d248_4,d248_7,1).
bond(train,d248,d248_7,d248_8,1).
bond(train,d248,d248_7,d248_9,1).
bond(train,d248,d248_7,d248_10,1).
bond(train,d248,d248_4,d248_11,1).
bond(train,d248,d248_11,d248_12,1).
bond(train,d248,d248_11,d248_13,1).
bond(train,d248,d248_11,d248_14,1).
bond(train,d248,d248_3,d248_15,2).
bond(train,d249,d249_1,d249_2,1).
bond(train,d249,d249_2,d249_3,1).
bond(train,d249,d249_3,d249_4,1).
bond(train,d249,d249_4,d249_5,1).
bond(train,d249,d249_5,d249_6,1).
bond(train,d249,d249_6,d249_1,1).
bond(train,d249,d249_3,d249_7,1).
bond(train,d249,d249_3,d249_8,1).
bond(train,d249,d249_4,d249_9,1).
bond(train,d249,d249_4,d249_10,1).
bond(train,d249,d249_5,d249_11,1).
bond(train,d249,d249_5,d249_12,1).
bond(train,d249,d249_11,d249_2,1).
bond(train,d249,d249_11,d249_13,1).
bond(train,d249,d249_11,d249_14,1).
bond(train,d249,d249_2,d249_15,1).
bond(train,d249,d249_1,d249_16,1).
bond(train,d249,d249_16,d249_17,1).
bond(train,d249,d249_16,d249_18,1).
bond(train,d249,d249_16,d249_19,1).
bond(train,d249,d249_1,d249_20,1).
bond(train,d249,d249_20,d249_21,1).
bond(train,d249,d249_20,d249_22,1).
bond(train,d249,d249_20,d249_23,1).
bond(train,d249,d249_6,d249_24,1).
bond(train,d249,d249_24,d249_25,1).
bond(train,d249,d249_24,d249_26,1).
bond(train,d249,d249_24,d249_27,1).
bond(train,d249,d249_6,d249_28,1).
bond(train,d25,d25_1,d25_2,7).
bond(train,d25,d25_2,d25_3,7).
bond(train,d25,d25_3,d25_4,7).
bond(train,d25,d25_4,d25_5,7).
bond(train,d25,d25_5,d25_6,7).
bond(train,d25,d25_6,d25_1,7).
bond(train,d25,d25_1,d25_7,1).
bond(train,d25,d25_2,d25_8,1).
bond(train,d25,d25_3,d25_9,1).
bond(train,d25,d25_6,d25_10,1).
bond(train,d25,d25_5,d25_11,1).
bond(train,d25,d25_4,d25_12,1).
bond(train,d25,d25_12,d25_13,1).
bond(train,d25,d25_12,d25_14,1).
bond(train,d25,d25_12,d25_15,1).
bond(train,d25,d25_11,d25_16,1).
bond(train,d25,d25_11,d25_17,1).
bond(train,d250,d250_1,d250_2,1).
bond(train,d250,d250_2,d250_3,1).
bond(train,d250,d250_2,d250_4,1).
bond(train,d250,d250_2,d250_5,1).
bond(train,d250,d250_3,d250_6,1).
bond(train,d250,d250_3,d250_7,1).
bond(train,d250,d250_3,d250_8,1).
bond(train,d250,d250_7,d250_9,1).
bond(train,d250,d250_7,d250_10,1).
bond(train,d250,d250_7,d250_11,1).
bond(train,d251,d251_1,d251_2,2).
bond(train,d251,d251_2,d251_3,1).
bond(train,d251,d251_3,d251_4,1).
bond(train,d251,d251_4,d251_5,1).
bond(train,d251,d251_4,d251_6,1).
bond(train,d251,d251_4,d251_7,1).
bond(train,d251,d251_5,d251_8,1).
bond(train,d251,d251_5,d251_9,1).
bond(train,d251,d251_5,d251_10,1).
bond(train,d251,d251_2,d251_11,1).
bond(train,d251,d251_11,d251_12,1).
bond(train,d251,d251_12,d251_13,1).
bond(train,d251,d251_12,d251_14,1).
bond(train,d251,d251_12,d251_15,1).
bond(train,d251,d251_13,d251_16,1).
bond(train,d251,d251_13,d251_17,1).
bond(train,d251,d251_13,d251_18,1).
bond(train,d251,d251_2,d251_19,1).
bond(train,d251,d251_19,d251_20,1).
bond(train,d251,d251_20,d251_21,1).
bond(train,d251,d251_20,d251_22,1).
bond(train,d251,d251_20,d251_23,1).
bond(train,d251,d251_21,d251_24,1).
bond(train,d251,d251_21,d251_25,1).
bond(train,d251,d251_21,d251_26,1).
bond(train,d252,d252_1,d252_2,2).
bond(train,d252,d252_1,d252_3,1).
bond(train,d252,d252_1,d252_4,1).
bond(train,d252,d252_2,d252_5,1).
bond(train,d252,d252_2,d252_6,1).
bond(train,d252,d252_5,d252_7,1).
bond(train,d252,d252_5,d252_8,1).
bond(train,d252,d252_5,d252_9,1).
bond(train,d252,d252_7,d252_10,2).
bond(train,d252,d252_10,d252_11,2).
bond(train,d253,d253_1,d253_2,2).
bond(train,d253,d253_1,d253_3,1).
bond(train,d253,d253_3,d253_4,1).
bond(train,d253,d253_3,d253_5,1).
bond(train,d253,d253_3,d253_6,1).
bond(train,d253,d253_1,d253_7,1).
bond(train,d253,d253_7,d253_8,1).
bond(train,d253,d253_8,d253_9,1).
bond(train,d253,d253_8,d253_10,1).
bond(train,d253,d253_8,d253_11,1).
bond(train,d253,d253_1,d253_12,1).
bond(train,d253,d253_12,d253_13,1).
bond(train,d253,d253_13,d253_14,1).
bond(train,d253,d253_13,d253_15,1).
bond(train,d253,d253_13,d253_16,1).
bond(train,d254,d254_1,d254_2,1).
bond(train,d254,d254_1,d254_3,1).
bond(train,d254,d254_1,d254_4,1).
bond(train,d254,d254_1,d254_5,1).
bond(train,d254,d254_3,d254_2,1).
bond(train,d254,d254_2,d254_6,1).
bond(train,d254,d254_2,d254_7,1).
bond(train,d254,d254_6,d254_8,1).
bond(train,d254,d254_6,d254_9,1).
bond(train,d254,d254_6,d254_10,1).
bond(train,d254,d254_8,d254_11,1).
bond(train,d254,d254_11,d254_12,1).
bond(train,d254,d254_11,d254_13,1).
bond(train,d254,d254_11,d254_14,1).
bond(train,d254,d254_12,d254_15,2).
bond(train,d254,d254_12,d254_16,1).
bond(train,d254,d254_15,d254_17,1).
bond(train,d254,d254_15,d254_18,1).
bond(train,d255,d255_1,d255_2,1).
bond(train,d255,d255_1,d255_3,1).
bond(train,d255,d255_1,d255_4,1).
bond(train,d255,d255_1,d255_5,1).
bond(train,d255,d255_2,d255_6,1).
bond(train,d255,d255_2,d255_7,1).
bond(train,d255,d255_2,d255_8,1).
bond(train,d255,d255_6,d255_9,1).
bond(train,d255,d255_6,d255_10,1).
bond(train,d255,d255_6,d255_11,1).
bond(train,d256,d256_1,d256_2,7).
bond(train,d256,d256_2,d256_3,7).
bond(train,d256,d256_3,d256_4,7).
bond(train,d256,d256_4,d256_5,7).
bond(train,d256,d256_5,d256_6,7).
bond(train,d256,d256_6,d256_1,7).
bond(train,d256,d256_2,d256_7,1).
bond(train,d256,d256_5,d256_8,1).
bond(train,d256,d256_6,d256_9,1).
bond(train,d256,d256_9,d256_10,2).
bond(train,d256,d256_10,d256_11,1).
bond(train,d256,d256_11,d256_12,1).
bond(train,d256,d256_12,d256_1,1).
bond(train,d256,d256_11,d256_13,1).
bond(train,d256,d256_13,d256_14,1).
bond(train,d256,d256_13,d256_15,1).
bond(train,d256,d256_13,d256_16,1).
bond(train,d256,d256_14,d256_17,1).
bond(train,d256,d256_17,d256_18,1).
bond(train,d256,d256_17,d256_19,1).
bond(train,d256,d256_19,d256_20,1).
bond(train,d256,d256_20,d256_21,1).
bond(train,d256,d256_20,d256_22,1).
bond(train,d256,d256_20,d256_23,1).
bond(train,d256,d256_17,d256_24,2).
bond(train,d256,d256_18,d256_25,1).
bond(train,d256,d256_25,d256_26,1).
bond(train,d256,d256_25,d256_27,1).
bond(train,d256,d256_25,d256_28,1).
bond(train,d256,d256_12,d256_29,2).
bond(train,d256,d256_3,d256_30,1).
bond(train,d256,d256_4,d256_31,1).
bond(train,d257,d257_1,d257_2,1).
bond(train,d257,d257_1,d257_3,1).
bond(train,d257,d257_1,d257_4,1).
bond(train,d257,d257_1,d257_5,1).
bond(train,d257,d257_2,d257_6,1).
bond(train,d257,d257_2,d257_7,1).
bond(train,d257,d257_2,d257_8,1).
bond(train,d257,d257_6,d257_9,1).
bond(train,d257,d257_9,d257_10,1).
bond(train,d257,d257_10,d257_11,1).
bond(train,d257,d257_11,d257_12,1).
bond(train,d257,d257_11,d257_13,1).
bond(train,d257,d257_11,d257_14,1).
bond(train,d257,d257_12,d257_15,1).
bond(train,d257,d257_12,d257_16,1).
bond(train,d257,d257_12,d257_17,1).
bond(train,d257,d257_9,d257_18,2).
bond(train,d257,d257_9,d257_19,1).
bond(train,d257,d257_19,d257_20,1).
bond(train,d257,d257_20,d257_21,7).
bond(train,d257,d257_21,d257_22,7).
bond(train,d257,d257_22,d257_23,7).
bond(train,d257,d257_23,d257_24,7).
bond(train,d257,d257_24,d257_25,7).
bond(train,d257,d257_25,d257_20,7).
bond(train,d257,d257_21,d257_26,1).
bond(train,d257,d257_22,d257_27,1).
bond(train,d257,d257_25,d257_28,1).
bond(train,d257,d257_24,d257_29,1).
bond(train,d257,d257_29,d257_30,1).
bond(train,d257,d257_30,d257_31,1).
bond(train,d257,d257_30,d257_32,1).
bond(train,d257,d257_30,d257_33,1).
bond(train,d257,d257_23,d257_34,1).
bond(train,d257,d257_34,d257_35,1).
bond(train,d257,d257_34,d257_36,1).
bond(train,d257,d257_34,d257_37,1).
bond(train,d258,d258_1,d258_2,2).
bond(train,d258,d258_1,d258_3,1).
bond(train,d258,d258_3,d258_4,1).
bond(train,d258,d258_4,d258_5,1).
bond(train,d258,d258_4,d258_6,1).
bond(train,d258,d258_4,d258_7,1).
bond(train,d258,d258_1,d258_8,1).
bond(train,d258,d258_8,d258_9,1).
bond(train,d258,d258_9,d258_10,1).
bond(train,d258,d258_9,d258_11,1).
bond(train,d258,d258_9,d258_12,1).
bond(train,d258,d258_1,d258_13,1).
bond(train,d258,d258_13,d258_14,1).
bond(train,d258,d258_14,d258_15,1).
bond(train,d258,d258_15,d258_16,1).
bond(train,d258,d258_15,d258_17,1).
bond(train,d258,d258_15,d258_18,1).
bond(train,d258,d258_14,d258_19,2).
bond(train,d258,d258_19,d258_20,1).
bond(train,d258,d258_19,d258_21,1).
bond(train,d258,d258_21,d258_22,2).
bond(train,d258,d258_21,d258_23,1).
bond(train,d258,d258_23,d258_24,1).
bond(train,d258,d258_24,d258_25,1).
bond(train,d258,d258_24,d258_26,1).
bond(train,d258,d258_24,d258_27,1).
bond(train,d258,d258_25,d258_28,1).
bond(train,d258,d258_25,d258_29,1).
bond(train,d258,d258_25,d258_30,1).
bond(train,d258,d258_23,d258_31,1).
bond(train,d258,d258_31,d258_32,1).
bond(train,d258,d258_31,d258_33,1).
bond(train,d258,d258_31,d258_34,1).
bond(train,d258,d258_32,d258_35,1).
bond(train,d258,d258_32,d258_36,1).
bond(train,d258,d258_32,d258_37,1).
bond(train,d259,d259_1,d259_2,7).
bond(train,d259,d259_2,d259_3,7).
bond(train,d259,d259_3,d259_4,7).
bond(train,d259,d259_4,d259_5,7).
bond(train,d259,d259_5,d259_6,7).
bond(train,d259,d259_6,d259_1,7).
bond(train,d259,d259_1,d259_7,1).
bond(train,d259,d259_2,d259_8,1).
bond(train,d259,d259_3,d259_9,1).
bond(train,d259,d259_4,d259_10,1).
bond(train,d259,d259_6,d259_11,1).
bond(train,d259,d259_5,d259_12,1).
bond(train,d259,d259_12,d259_13,1).
bond(train,d259,d259_13,d259_14,1).
bond(train,d259,d259_13,d259_15,1).
bond(train,d259,d259_13,d259_16,1).
bond(train,d259,d259_12,d259_17,2).
bond(train,d26,d26_1,d26_2,7).
bond(train,d26,d26_2,d26_3,7).
bond(train,d26,d26_3,d26_4,7).
bond(train,d26,d26_4,d26_5,7).
bond(train,d26,d26_5,d26_6,7).
bond(train,d26,d26_6,d26_1,7).
bond(train,d26,d26_3,d26_7,1).
bond(train,d26,d26_6,d26_8,1).
bond(train,d26,d26_5,d26_9,1).
bond(train,d26,d26_4,d26_10,1).
bond(train,d26,d26_10,d26_11,1).
bond(train,d26,d26_10,d26_12,1).
bond(train,d26,d26_10,d26_13,1).
bond(train,d26,d26_2,d26_14,1).
bond(train,d26,d26_14,d26_15,1).
bond(train,d26,d26_14,d26_16,1).
bond(train,d26,d26_14,d26_17,1).
bond(train,d26,d26_1,d26_18,1).
bond(train,d26,d26_18,d26_19,1).
bond(train,d26,d26_18,d26_20,1).
bond(train,d26,d26_18,d26_21,1).
bond(train,d26,d26_9,d26_22,1).
bond(train,d26,d26_9,d26_23,1).
bond(train,d260,d260_2,d260_3,1).
bond(train,d260,d260_3,d260_4,2).
bond(train,d260,d260_3,d260_5,2).
bond(train,d260,d260_6,d260_7,1).
bond(train,d260,d260_7,d260_8,2).
bond(train,d260,d260_7,d260_9,2).
bond(train,d260,d260_10,d260_11,1).
bond(train,d260,d260_12,d260_13,1).
bond(train,d260,d260_13,d260_14,2).
bond(train,d260,d260_13,d260_15,2).
bond(train,d260,d260_11,d260_16,2).
bond(train,d260,d260_11,d260_17,2).
bond(train,d260,d260_1,d260_18,1).
bond(train,d260,d260_18,d260_2,1).
bond(train,d260,d260_18,d260_19,1).
bond(train,d260,d260_18,d260_20,1).
bond(train,d260,d260_1,d260_21,1).
bond(train,d260,d260_21,d260_6,1).
bond(train,d260,d260_21,d260_22,1).
bond(train,d260,d260_21,d260_23,1).
bond(train,d260,d260_1,d260_24,1).
bond(train,d260,d260_10,d260_24,1).
bond(train,d260,d260_24,d260_25,1).
bond(train,d260,d260_24,d260_26,1).
bond(train,d260,d260_1,d260_27,1).
bond(train,d260,d260_27,d260_12,1).
bond(train,d260,d260_27,d260_28,1).
bond(train,d260,d260_27,d260_29,1).
bond(train,d261,d261_1,d261_2,1).
bond(train,d261,d261_2,d261_3,1).
bond(train,d261,d261_2,d261_4,1).
bond(train,d261,d261_2,d261_5,1).
bond(train,d261,d261_3,d261_6,1).
bond(train,d261,d261_3,d261_7,1).
bond(train,d261,d261_3,d261_8,1).
bond(train,d261,d261_6,d261_9,1).
bond(train,d261,d261_6,d261_10,1).
bond(train,d261,d261_6,d261_11,1).
bond(train,d261,d261_1,d261_12,1).
bond(train,d261,d261_1,d261_13,1).
bond(train,d261,d261_1,d261_14,1).
bond(train,d262,d262_1,d262_2,1).
bond(train,d262,d262_2,d262_3,3).
bond(train,d262,d262_1,d262_4,2).
bond(train,d263,d263_1,d263_2,1).
bond(train,d263,d263_2,d263_3,1).
bond(train,d263,d263_2,d263_4,1).
bond(train,d263,d263_2,d263_5,1).
bond(train,d263,d263_3,d263_6,1).
bond(train,d263,d263_3,d263_7,1).
bond(train,d263,d263_3,d263_8,1).
bond(train,d263,d263_6,d263_9,1).
bond(train,d264,d264_1,d264_2,1).
bond(train,d264,d264_1,d264_3,1).
bond(train,d264,d264_1,d264_4,1).
bond(train,d264,d264_1,d264_5,1).
bond(train,d264,d264_2,d264_6,1).
bond(train,d264,d264_2,d264_7,1).
bond(train,d264,d264_2,d264_8,1).
bond(train,d264,d264_6,d264_9,1).
bond(train,d264,d264_9,d264_10,1).
bond(train,d264,d264_10,d264_11,1).
bond(train,d264,d264_11,d264_12,1).
bond(train,d264,d264_11,d264_13,1).
bond(train,d264,d264_11,d264_14,1).
bond(train,d264,d264_12,d264_15,1).
bond(train,d264,d264_12,d264_16,1).
bond(train,d264,d264_12,d264_17,1).
bond(train,d264,d264_9,d264_18,2).
bond(train,d264,d264_9,d264_19,1).
bond(train,d264,d264_19,d264_20,1).
bond(train,d264,d264_20,d264_21,7).
bond(train,d264,d264_21,d264_22,7).
bond(train,d264,d264_22,d264_23,7).
bond(train,d264,d264_23,d264_24,7).
bond(train,d264,d264_24,d264_25,7).
bond(train,d264,d264_25,d264_20,7).
bond(train,d264,d264_21,d264_26,1).
bond(train,d264,d264_22,d264_27,1).
bond(train,d264,d264_25,d264_28,1).
bond(train,d264,d264_23,d264_29,1).
bond(train,d264,d264_29,d264_30,2).
bond(train,d264,d264_30,d264_31,1).
bond(train,d264,d264_31,d264_32,1).
bond(train,d264,d264_32,d264_24,1).
bond(train,d264,d264_31,d264_33,2).
bond(train,d264,d264_29,d264_34,1).
bond(train,d264,d264_34,d264_35,1).
bond(train,d264,d264_34,d264_36,1).
bond(train,d264,d264_34,d264_37,1).
bond(train,d264,d264_30,d264_38,1).
bond(train,d265,d265_1,d265_2,1).
bond(train,d265,d265_2,d265_3,1).
bond(train,d265,d265_3,d265_4,1).
bond(train,d265,d265_4,d265_5,1).
bond(train,d265,d265_3,d265_6,1).
bond(train,d265,d265_6,d265_7,1).
bond(train,d265,d265_7,d265_8,1).
bond(train,d265,d265_7,d265_9,1).
bond(train,d265,d265_7,d265_10,1).
bond(train,d265,d265_8,d265_11,1).
bond(train,d265,d265_11,d265_12,1).
bond(train,d265,d265_12,d265_13,1).
bond(train,d265,d265_12,d265_14,1).
bond(train,d265,d265_12,d265_15,1).
bond(train,d265,d265_3,d265_16,2).
bond(train,d265,d265_8,d265_17,2).
bond(train,d265,d265_11,d265_18,1).
bond(train,d265,d265_1,d265_19,1).
bond(train,d265,d265_1,d265_20,1).
bond(train,d265,d265_1,d265_21,1).
bond(train,d265,d265_5,d265_22,1).
bond(train,d265,d265_5,d265_23,1).
bond(train,d265,d265_5,d265_24,1).
bond(train,d266,d266_1,d266_2,1).
bond(train,d266,d266_2,d266_3,1).
bond(train,d266,d266_3,d266_4,1).
bond(train,d266,d266_4,d266_5,1).
bond(train,d266,d266_3,d266_6,1).
bond(train,d266,d266_6,d266_7,1).
bond(train,d266,d266_3,d266_9,2).
bond(train,d266,d266_8,d266_10,2).
bond(train,d266,d266_1,d266_11,1).
bond(train,d266,d266_1,d266_12,1).
bond(train,d266,d266_1,d266_13,1).
bond(train,d266,d266_5,d266_14,1).
bond(train,d266,d266_5,d266_15,1).
bond(train,d266,d266_5,d266_16,1).
bond(train,d266,d266_8,d266_17,1).
bond(train,d266,d266_17,d266_18,1).
bond(train,d266,d266_18,d266_19,1).
bond(train,d266,d266_18,d266_20,1).
bond(train,d266,d266_18,d266_21,1).
bond(train,d266,d266_19,d266_22,1).
bond(train,d266,d266_19,d266_23,1).
bond(train,d266,d266_19,d266_24,1).
bond(train,d266,d266_7,d266_25,1).
bond(train,d266,d266_25,d266_26,1).
bond(train,d266,d266_26,d266_27,1).
bond(train,d266,d266_27,d266_28,1).
bond(train,d266,d266_27,d266_29,1).
bond(train,d266,d266_27,d266_30,1).
bond(train,d266,d266_28,d266_31,1).
bond(train,d266,d266_28,d266_32,1).
bond(train,d266,d266_28,d266_33,1).
bond(train,d266,d266_25,d266_34,2).
bond(train,d266,d266_7,d266_35,1).
bond(train,d266,d266_35,d266_8,1).
bond(train,d266,d266_35,d266_36,1).
bond(train,d266,d266_35,d266_37,1).
bond(train,d266,d266_7,d266_38,1).
bond(train,d267,d267_1,d267_2,1).
bond(train,d267,d267_2,d267_3,1).
bond(train,d267,d267_2,d267_4,1).
bond(train,d267,d267_2,d267_5,1).
bond(train,d267,d267_3,d267_6,1).
bond(train,d267,d267_6,d267_7,1).
bond(train,d267,d267_6,d267_8,1).
bond(train,d267,d267_6,d267_9,1).
bond(train,d267,d267_3,d267_10,1).
bond(train,d267,d267_3,d267_11,1).
bond(train,d267,d267_1,d267_12,1).
bond(train,d267,d267_7,d267_13,1).
bond(train,d267,d267_10,d267_14,1).
bond(train,d267,d267_10,d267_15,1).
bond(train,d267,d267_10,d267_16,1).
bond(train,d267,d267_14,d267_17,1).
bond(train,d267,d267_11,d267_18,1).
bond(train,d267,d267_11,d267_19,1).
bond(train,d267,d267_11,d267_20,1).
bond(train,d267,d267_18,d267_21,1).
bond(train,d268,d268_1,d268_2,7).
bond(train,d268,d268_2,d268_3,7).
bond(train,d268,d268_3,d268_4,7).
bond(train,d268,d268_4,d268_5,7).
bond(train,d268,d268_5,d268_6,7).
bond(train,d268,d268_6,d268_1,7).
bond(train,d268,d268_1,d268_7,1).
bond(train,d268,d268_4,d268_8,1).
bond(train,d268,d268_5,d268_9,1).
bond(train,d268,d268_6,d268_10,1).
bond(train,d268,d268_3,d268_11,1).
bond(train,d268,d268_11,d268_12,1).
bond(train,d268,d268_11,d268_13,1).
bond(train,d268,d268_11,d268_14,1).
bond(train,d269,d269_1,d269_2,1).
bond(train,d269,d269_1,d269_3,1).
bond(train,d269,d269_1,d269_4,1).
bond(train,d269,d269_1,d269_5,1).
bond(train,d269,d269_2,d269_6,1).
bond(train,d269,d269_2,d269_7,1).
bond(train,d269,d269_2,d269_8,1).
bond(train,d269,d269_6,d269_9,1).
bond(train,d269,d269_9,d269_10,1).
bond(train,d269,d269_10,d269_11,1).
bond(train,d269,d269_11,d269_12,1).
bond(train,d269,d269_11,d269_13,1).
bond(train,d269,d269_11,d269_14,1).
bond(train,d269,d269_12,d269_15,1).
bond(train,d269,d269_12,d269_16,1).
bond(train,d269,d269_12,d269_17,1).
bond(train,d269,d269_9,d269_18,2).
bond(train,d269,d269_9,d269_19,1).
bond(train,d269,d269_19,d269_20,1).
bond(train,d269,d269_20,d269_21,7).
bond(train,d269,d269_21,d269_22,7).
bond(train,d269,d269_22,d269_23,7).
bond(train,d269,d269_23,d269_24,7).
bond(train,d269,d269_24,d269_25,7).
bond(train,d269,d269_25,d269_20,7).
bond(train,d269,d269_21,d269_26,1).
bond(train,d269,d269_22,d269_27,1).
bond(train,d269,d269_27,d269_28,1).
bond(train,d269,d269_27,d269_29,1).
bond(train,d269,d269_27,d269_30,1).
bond(train,d269,d269_24,d269_31,1).
bond(train,d269,d269_31,d269_32,1).
bond(train,d269,d269_32,d269_33,1).
bond(train,d269,d269_32,d269_34,1).
bond(train,d269,d269_32,d269_35,1).
bond(train,d269,d269_31,d269_36,1).
bond(train,d269,d269_31,d269_37,1).
bond(train,d269,d269_36,d269_38,1).
bond(train,d269,d269_36,d269_39,1).
bond(train,d269,d269_36,d269_40,1).
bond(train,d27,d27_1,d27_2,1).
bond(train,d27,d27_2,d27_3,1).
bond(train,d27,d27_3,d27_4,1).
bond(train,d27,d27_4,d27_5,1).
bond(train,d27,d27_5,d27_1,1).
bond(train,d27,d27_2,d27_6,1).
bond(train,d27,d27_2,d27_7,1).
bond(train,d27,d27_8,d27_1,1).
bond(train,d27,d27_3,d27_9,2).
bond(train,d27,d27_4,d27_10,1).
bond(train,d27,d27_5,d27_11,2).
bond(train,d27,d27_12,d27_13,2).
bond(train,d27,d27_12,d27_14,2).
bond(train,d27,d27_12,d27_15,1).
bond(train,d27,d27_16,d27_17,7).
bond(train,d27,d27_17,d27_18,7).
bond(train,d27,d27_18,d27_19,7).
bond(train,d27,d27_15,d27_16,7).
bond(train,d27,d27_15,d27_19,7).
bond(train,d27,d27_18,d27_20,1).
bond(train,d27,d27_20,d27_8,2).
bond(train,d27,d27_20,d27_21,1).
bond(train,d27,d27_16,d27_22,1).
bond(train,d27,d27_17,d27_23,1).
bond(train,d270,d270_1,d270_2,1).
bond(train,d270,d270_2,d270_3,1).
bond(train,d270,d270_3,d270_4,1).
bond(train,d270,d270_4,d270_5,1).
bond(train,d270,d270_5,d270_6,1).
bond(train,d270,d270_6,d270_1,1).
bond(train,d270,d270_1,d270_7,1).
bond(train,d270,d270_1,d270_8,1).
bond(train,d270,d270_6,d270_9,1).
bond(train,d270,d270_6,d270_10,1).
bond(train,d270,d270_4,d270_11,1).
bond(train,d270,d270_4,d270_12,1).
bond(train,d270,d270_11,d270_13,1).
bond(train,d270,d270_3,d270_14,1).
bond(train,d270,d270_3,d270_15,1).
bond(train,d270,d270_14,d270_16,1).
bond(train,d270,d270_13,d270_17,2).
bond(train,d270,d270_16,d270_18,2).
bond(train,d270,d270_13,d270_19,1).
bond(train,d270,d270_13,d270_20,1).
bond(train,d270,d270_19,d270_21,1).
bond(train,d270,d270_21,d270_22,1).
bond(train,d270,d270_21,d270_23,1).
bond(train,d270,d270_21,d270_24,1).
bond(train,d270,d270_22,d270_25,1).
bond(train,d270,d270_22,d270_26,1).
bond(train,d270,d270_22,d270_27,1).
bond(train,d270,d270_20,d270_28,1).
bond(train,d270,d270_28,d270_29,1).
bond(train,d270,d270_28,d270_30,1).
bond(train,d270,d270_28,d270_31,1).
bond(train,d270,d270_29,d270_32,1).
bond(train,d270,d270_29,d270_33,1).
bond(train,d270,d270_29,d270_34,1).
bond(train,d270,d270_16,d270_35,1).
bond(train,d270,d270_35,d270_36,1).
bond(train,d270,d270_36,d270_37,1).
bond(train,d270,d270_36,d270_38,1).
bond(train,d270,d270_36,d270_39,1).
bond(train,d270,d270_37,d270_40,1).
bond(train,d270,d270_37,d270_41,1).
bond(train,d270,d270_37,d270_42,1).
bond(train,d270,d270_16,d270_43,1).
bond(train,d270,d270_43,d270_44,1).
bond(train,d270,d270_44,d270_45,1).
bond(train,d270,d270_44,d270_46,1).
bond(train,d270,d270_44,d270_47,1).
bond(train,d270,d270_45,d270_48,1).
bond(train,d270,d270_45,d270_49,1).
bond(train,d270,d270_45,d270_50,1).
bond(train,d271,d271_1,d271_2,1).
bond(train,d271,d271_1,d271_3,1).
bond(train,d271,d271_1,d271_4,1).
bond(train,d271,d271_1,d271_5,1).
bond(train,d271,d271_2,d271_6,1).
bond(train,d271,d271_6,d271_7,1).
bond(train,d271,d271_6,d271_8,2).
bond(train,d271,d271_6,d271_9,1).
bond(train,d271,d271_9,d271_10,1).
bond(train,d271,d271_10,d271_11,1).
bond(train,d271,d271_10,d271_12,1).
bond(train,d271,d271_10,d271_13,1).
bond(train,d271,d271_15,d271_16,2).
bond(train,d271,d271_14,d271_17,2).
bond(train,d271,d271_15,d271_18,1).
bond(train,d271,d271_18,d271_19,1).
bond(train,d271,d271_19,d271_20,1).
bond(train,d271,d271_19,d271_21,1).
bond(train,d271,d271_19,d271_22,1).
bond(train,d271,d271_20,d271_23,1).
bond(train,d271,d271_20,d271_24,1).
bond(train,d271,d271_20,d271_25,1).
bond(train,d271,d271_14,d271_26,1).
bond(train,d271,d271_26,d271_27,1).
bond(train,d271,d271_27,d271_28,1).
bond(train,d271,d271_27,d271_29,1).
bond(train,d271,d271_27,d271_30,1).
bond(train,d271,d271_28,d271_31,1).
bond(train,d271,d271_28,d271_32,1).
bond(train,d271,d271_28,d271_33,1).
bond(train,d271,d271_7,d271_34,1).
bond(train,d271,d271_34,d271_15,1).
bond(train,d271,d271_34,d271_35,1).
bond(train,d271,d271_34,d271_36,1).
bond(train,d271,d271_35,d271_14,1).
bond(train,d271,d271_35,d271_37,1).
bond(train,d271,d271_35,d271_38,1).
bond(train,d272,d272_1,d272_2,1).
bond(train,d272,d272_1,d272_3,1).
bond(train,d272,d272_1,d272_4,1).
bond(train,d272,d272_1,d272_5,1).
bond(train,d273,d273_1,d273_2,1).
bond(train,d273,d273_1,d273_3,1).
bond(train,d273,d273_1,d273_4,1).
bond(train,d273,d273_1,d273_5,1).
bond(train,d274,d274_1,d274_2,1).
bond(train,d274,d274_1,d274_3,1).
bond(train,d274,d274_1,d274_4,1).
bond(train,d274,d274_1,d274_5,1).
bond(train,d275,d275_1,d275_2,1).
bond(train,d275,d275_1,d275_3,1).
bond(train,d275,d275_1,d275_4,1).
bond(train,d275,d275_1,d275_5,1).
bond(train,d276,d276_1,d276_2,1).
bond(train,d276,d276_1,d276_3,1).
bond(train,d276,d276_1,d276_4,1).
bond(train,d276,d276_1,d276_5,1).
bond(train,d277,d277_1,d277_2,1).
bond(train,d277,d277_2,d277_3,1).
bond(train,d277,d277_2,d277_4,1).
bond(train,d277,d277_4,d277_5,7).
bond(train,d277,d277_5,d277_6,7).
bond(train,d277,d277_6,d277_7,7).
bond(train,d277,d277_7,d277_8,7).
bond(train,d277,d277_8,d277_9,7).
bond(train,d277,d277_9,d277_4,7).
bond(train,d277,d277_5,d277_10,1).
bond(train,d277,d277_6,d277_11,1).
bond(train,d277,d277_8,d277_12,1).
bond(train,d277,d277_9,d277_13,1).
bond(train,d277,d277_7,d277_14,1).
bond(train,d277,d277_14,d277_15,1).
bond(train,d277,d277_14,d277_16,1).
bond(train,d277,d277_14,d277_17,1).
bond(train,d277,d277_15,d277_18,7).
bond(train,d277,d277_18,d277_19,7).
bond(train,d277,d277_19,d277_20,7).
bond(train,d277,d277_20,d277_21,7).
bond(train,d277,d277_21,d277_22,7).
bond(train,d277,d277_22,d277_15,7).
bond(train,d277,d277_18,d277_23,1).
bond(train,d277,d277_19,d277_24,1).
bond(train,d277,d277_21,d277_25,1).
bond(train,d277,d277_22,d277_26,1).
bond(train,d277,d277_20,d277_27,1).
bond(train,d277,d277_27,d277_28,1).
bond(train,d277,d277_27,d277_29,1).
bond(train,d277,d277_28,d277_30,1).
bond(train,d277,d277_28,d277_31,1).
bond(train,d277,d277_28,d277_32,1).
bond(train,d277,d277_29,d277_33,1).
bond(train,d277,d277_29,d277_34,1).
bond(train,d277,d277_29,d277_35,1).
bond(train,d277,d277_3,d277_36,1).
bond(train,d277,d277_3,d277_37,1).
bond(train,d277,d277_3,d277_38,1).
bond(train,d277,d277_1,d277_39,1).
bond(train,d277,d277_1,d277_40,1).
bond(train,d277,d277_1,d277_41,1).
bond(train,d278,d278_1,d278_2,1).
bond(train,d278,d278_2,d278_3,1).
bond(train,d278,d278_2,d278_4,1).
bond(train,d278,d278_4,d278_5,7).
bond(train,d278,d278_5,d278_6,7).
bond(train,d278,d278_6,d278_7,7).
bond(train,d278,d278_7,d278_8,7).
bond(train,d278,d278_8,d278_9,7).
bond(train,d278,d278_9,d278_4,7).
bond(train,d278,d278_5,d278_10,1).
bond(train,d278,d278_6,d278_11,1).
bond(train,d278,d278_8,d278_12,1).
bond(train,d278,d278_9,d278_13,1).
bond(train,d278,d278_7,d278_14,1).
bond(train,d278,d278_14,d278_15,1).
bond(train,d278,d278_15,d278_16,7).
bond(train,d278,d278_16,d278_17,7).
bond(train,d278,d278_17,d278_18,7).
bond(train,d278,d278_18,d278_19,7).
bond(train,d278,d278_19,d278_20,7).
bond(train,d278,d278_20,d278_15,7).
bond(train,d278,d278_16,d278_21,1).
bond(train,d278,d278_17,d278_22,1).
bond(train,d278,d278_19,d278_23,1).
bond(train,d278,d278_20,d278_24,1).
bond(train,d278,d278_18,d278_25,1).
bond(train,d278,d278_25,d278_26,1).
bond(train,d278,d278_25,d278_27,1).
bond(train,d278,d278_14,d278_28,2).
bond(train,d278,d278_26,d278_29,1).
bond(train,d278,d278_26,d278_30,1).
bond(train,d278,d278_26,d278_31,1).
bond(train,d278,d278_27,d278_32,1).
bond(train,d278,d278_27,d278_33,1).
bond(train,d278,d278_27,d278_34,1).
bond(train,d278,d278_3,d278_35,1).
bond(train,d278,d278_3,d278_36,1).
bond(train,d278,d278_3,d278_37,1).
bond(train,d278,d278_1,d278_38,1).
bond(train,d278,d278_1,d278_39,1).
bond(train,d278,d278_1,d278_40,1).
bond(train,d279,d279_1,d279_2,1).
bond(train,d279,d279_2,d279_3,1).
bond(train,d279,d279_2,d279_4,1).
bond(train,d279,d279_4,d279_5,7).
bond(train,d279,d279_5,d279_6,7).
bond(train,d279,d279_6,d279_7,7).
bond(train,d279,d279_7,d279_8,7).
bond(train,d279,d279_8,d279_9,7).
bond(train,d279,d279_9,d279_4,7).
bond(train,d279,d279_5,d279_10,1).
bond(train,d279,d279_6,d279_11,1).
bond(train,d279,d279_8,d279_12,1).
bond(train,d279,d279_9,d279_13,1).
bond(train,d279,d279_7,d279_14,1).
bond(train,d279,d279_3,d279_15,1).
bond(train,d279,d279_3,d279_16,1).
bond(train,d279,d279_3,d279_17,1).
bond(train,d279,d279_1,d279_18,1).
bond(train,d279,d279_1,d279_19,1).
bond(train,d279,d279_1,d279_20,1).
bond(train,d28,d28_1,d28_2,7).
bond(train,d28,d28_2,d28_3,7).
bond(train,d28,d28_3,d28_4,7).
bond(train,d28,d28_4,d28_5,7).
bond(train,d28,d28_5,d28_1,7).
bond(train,d28,d28_1,d28_6,1).
bond(train,d28,d28_4,d28_7,1).
bond(train,d28,d28_7,d28_8,2).
bond(train,d28,d28_8,d28_9,1).
bond(train,d28,d28_9,d28_10,1).
bond(train,d28,d28_10,d28_11,1).
bond(train,d28,d28_6,d28_12,2).
bond(train,d28,d28_6,d28_13,2).
bond(train,d28,d28_7,d28_14,1).
bond(train,d28,d28_9,d28_15,1).
bond(train,d28,d28_11,d28_16,1).
bond(train,d28,d28_11,d28_17,1).
bond(train,d28,d28_10,d28_18,2).
bond(train,d28,d28_2,d28_19,1).
bond(train,d28,d28_3,d28_20,1).
bond(train,d280,d280_1,d280_2,1).
bond(train,d280,d280_2,d280_3,1).
bond(train,d280,d280_2,d280_4,1).
bond(train,d280,d280_4,d280_5,7).
bond(train,d280,d280_5,d280_6,7).
bond(train,d280,d280_6,d280_7,7).
bond(train,d280,d280_7,d280_8,7).
bond(train,d280,d280_8,d280_9,7).
bond(train,d280,d280_9,d280_4,7).
bond(train,d280,d280_5,d280_10,1).
bond(train,d280,d280_6,d280_11,1).
bond(train,d280,d280_8,d280_12,1).
bond(train,d280,d280_9,d280_13,1).
bond(train,d280,d280_7,d280_14,1).
bond(train,d280,d280_14,d280_15,2).
bond(train,d280,d280_15,d280_16,1).
bond(train,d280,d280_16,d280_17,1).
bond(train,d280,d280_17,d280_18,1).
bond(train,d280,d280_16,d280_19,2).
bond(train,d280,d280_16,d280_20,2).
bond(train,d280,d280_3,d280_21,1).
bond(train,d280,d280_3,d280_22,1).
bond(train,d280,d280_3,d280_23,1).
bond(train,d280,d280_1,d280_24,1).
bond(train,d280,d280_1,d280_25,1).
bond(train,d280,d280_1,d280_26,1).
bond(train,d281,d281_1,d281_2,1).
bond(train,d281,d281_2,d281_3,1).
bond(train,d281,d281_2,d281_4,1).
bond(train,d281,d281_2,d281_5,1).
bond(train,d281,d281_1,d281_6,2).
bond(train,d281,d281_1,d281_7,2).
bond(train,d281,d281_3,d281_8,2).
bond(train,d281,d281_3,d281_9,2).
bond(train,d281,d281_5,d281_10,2).
bond(train,d281,d281_5,d281_11,2).
bond(train,d281,d281_4,d281_12,2).
bond(train,d281,d281_4,d281_13,2).
bond(train,d282,d282_1,d282_2,1).
bond(train,d282,d282_2,d282_3,1).
bond(train,d282,d282_2,d282_4,1).
bond(train,d282,d282_2,d282_5,1).
bond(train,d282,d282_6,d282_7,1).
bond(train,d282,d282_6,d282_8,2).
bond(train,d282,d282_1,d282_9,2).
bond(train,d282,d282_1,d282_10,2).
bond(train,d282,d282_7,d282_11,1).
bond(train,d282,d282_3,d282_6,1).
bond(train,d282,d282_3,d282_12,1).
bond(train,d282,d282_3,d282_13,1).
bond(train,d283,d283_1,d283_2,2).
bond(train,d283,d283_1,d283_3,1).
bond(train,d283,d283_1,d283_4,1).
bond(train,d283,d283_2,d283_5,1).
bond(train,d283,d283_2,d283_6,1).
bond(train,d283,d283_5,d283_7,2).
bond(train,d283,d283_5,d283_8,1).
bond(train,d283,d283_8,d283_9,1).
bond(train,d283,d283_9,d283_10,1).
bond(train,d283,d283_9,d283_11,1).
bond(train,d283,d283_9,d283_12,1).
bond(train,d283,d283_10,d283_13,1).
bond(train,d283,d283_10,d283_14,1).
bond(train,d283,d283_10,d283_15,1).
bond(train,d284,d284_1,d284_2,2).
bond(train,d284,d284_1,d284_3,1).
bond(train,d284,d284_1,d284_4,1).
bond(train,d284,d284_2,d284_5,1).
bond(train,d284,d284_2,d284_6,1).
bond(train,d284,d284_5,d284_7,2).
bond(train,d284,d284_5,d284_8,1).
bond(train,d284,d284_8,d284_9,1).
bond(train,d284,d284_9,d284_10,1).
bond(train,d284,d284_9,d284_11,1).
bond(train,d284,d284_9,d284_12,1).
bond(train,d284,d284_8,d284_13,1).
bond(train,d284,d284_10,d284_14,1).
bond(train,d285,d285_1,d285_2,2).
bond(train,d285,d285_1,d285_3,1).
bond(train,d285,d285_1,d285_4,1).
bond(train,d285,d285_2,d285_5,1).
bond(train,d285,d285_5,d285_6,1).
bond(train,d285,d285_5,d285_7,1).
bond(train,d285,d285_5,d285_8,1).
bond(train,d285,d285_2,d285_9,1).
bond(train,d285,d285_9,d285_10,1).
bond(train,d285,d285_10,d285_11,1).
bond(train,d285,d285_11,d285_12,1).
bond(train,d285,d285_11,d285_13,1).
bond(train,d285,d285_11,d285_14,1).
bond(train,d285,d285_9,d285_15,2).
bond(train,d286,d286_1,d286_2,1).
bond(train,d286,d286_2,d286_3,1).
bond(train,d286,d286_1,d286_4,2).
bond(train,d286,d286_1,d286_5,1).
bond(train,d286,d286_3,d286_6,2).
bond(train,d286,d286_3,d286_7,1).
bond(train,d286,d286_2,d286_8,1).
bond(train,d286,d286_2,d286_9,1).
bond(train,d287,d287_1,d287_2,1).
bond(train,d287,d287_2,d287_3,1).
bond(train,d287,d287_3,d287_4,1).
bond(train,d287,d287_4,d287_5,1).
bond(train,d287,d287_5,d287_6,1).
bond(train,d287,d287_6,d287_1,1).
bond(train,d287,d287_1,d287_7,1).
bond(train,d287,d287_1,d287_8,1).
bond(train,d287,d287_6,d287_9,1).
bond(train,d287,d287_6,d287_10,1).
bond(train,d287,d287_9,d287_11,1).
bond(train,d287,d287_9,d287_12,1).
bond(train,d287,d287_9,d287_13,1).
bond(train,d287,d287_4,d287_14,1).
bond(train,d287,d287_4,d287_15,1).
bond(train,d287,d287_14,d287_16,1).
bond(train,d287,d287_14,d287_17,1).
bond(train,d287,d287_14,d287_18,1).
bond(train,d287,d287_2,d287_19,1).
bond(train,d287,d287_2,d287_20,1).
bond(train,d287,d287_19,d287_21,1).
bond(train,d287,d287_21,d287_22,1).
bond(train,d287,d287_22,d287_23,1).
bond(train,d287,d287_22,d287_24,1).
bond(train,d287,d287_22,d287_25,1).
bond(train,d287,d287_21,d287_26,2).
bond(train,d288,d288_1,d288_2,1).
bond(train,d288,d288_2,d288_3,1).
bond(train,d288,d288_2,d288_4,1).
bond(train,d288,d288_2,d288_5,1).
bond(train,d288,d288_3,d288_6,1).
bond(train,d288,d288_6,d288_7,1).
bond(train,d288,d288_6,d288_8,1).
bond(train,d288,d288_6,d288_9,1).
bond(train,d288,d288_3,d288_10,1).
bond(train,d288,d288_10,d288_11,1).
bond(train,d288,d288_11,d288_12,1).
bond(train,d288,d288_10,d288_13,2).
bond(train,d288,d288_12,d288_14,1).
bond(train,d288,d288_12,d288_15,1).
bond(train,d288,d288_12,d288_16,1).
bond(train,d288,d288_14,d288_17,1).
bond(train,d288,d288_14,d288_18,2).
bond(train,d288,d288_18,d288_19,1).
bond(train,d288,d288_18,d288_20,1).
bond(train,d288,d288_7,d288_21,1).
bond(train,d288,d288_7,d288_22,1).
bond(train,d288,d288_7,d288_23,1).
bond(train,d288,d288_1,d288_24,1).
bond(train,d288,d288_1,d288_25,1).
bond(train,d288,d288_1,d288_26,1).
bond(train,d289,d289_1,d289_2,1).
bond(train,d289,d289_1,d289_3,1).
bond(train,d289,d289_1,d289_4,1).
bond(train,d289,d289_1,d289_5,1).
bond(train,d289,d289_2,d289_6,1).
bond(train,d289,d289_6,d289_7,1).
bond(train,d289,d289_6,d289_8,1).
bond(train,d289,d289_6,d289_9,1).
bond(train,d289,d289_2,d289_10,2).
bond(train,d289,d289_10,d289_11,1).
bond(train,d289,d289_10,d289_12,1).
bond(train,d29,d29a_1,d29a_2,7).
bond(train,d29,d29a_2,d29a_3,7).
bond(train,d29,d29a_3,d29a_4,7).
bond(train,d29,d29a_4,d29a_5,7).
bond(train,d29,d29a_5,d29a_6,7).
bond(train,d29,d29a_6,d29a_1,7).
bond(train,d29,d29a_1,d29a_7,1).
bond(train,d29,d29a_3,d29a_8,1).
bond(train,d29,d29a_6,d29a_9,1).
bond(train,d29,d29a_5,d29a_10,1).
bond(train,d29,d29a_10,d29a_11,1).
bond(train,d29,d29a_10,d29a_12,1).
bond(train,d29,d29a_10,d29a_13,1).
bond(train,d29,d29a_4,d29a_14,1).
bond(train,d29,d29a_2,d29a_15,1).
bond(train,d29,d29a_14,d29a_16,2).
bond(train,d29,d29a_16,d29a_17,2).
bond(train,d29,d29a_15,d29a_18,2).
bond(train,d29,d29a_18,d29a_19,2).
bond(train,d29,d29b_1,d29b_2,7).
bond(train,d29,d29b_2,d29b_3,7).
bond(train,d29,d29b_3,d29b_4,7).
bond(train,d29,d29b_4,d29b_5,7).
bond(train,d29,d29b_5,d29b_6,7).
bond(train,d29,d29b_6,d29b_1,7).
bond(train,d29,d29b_1,d29b_7,1).
bond(train,d29,d29b_2,d29b_8,1).
bond(train,d29,d29b_3,d29b_9,1).
bond(train,d29,d29b_5,d29b_10,1).
bond(train,d29,d29b_10,d29b_11,1).
bond(train,d29,d29b_10,d29b_12,1).
bond(train,d29,d29b_10,d29b_13,1).
bond(train,d29,d29b_6,d29b_14,1).
bond(train,d29,d29b_4,d29b_15,1).
bond(train,d29,d29b_14,d29b_16,2).
bond(train,d29,d29b_16,d29b_17,2).
bond(train,d29,d29b_15,d29b_18,2).
bond(train,d29,d29b_18,d29b_19,2).
bond(train,d290,d290_1,d290_2,7).
bond(train,d290,d290_2,d290_3,7).
bond(train,d290,d290_3,d290_4,7).
bond(train,d290,d290_4,d290_5,7).
bond(train,d290,d290_5,d290_6,7).
bond(train,d290,d290_6,d290_1,7).
bond(train,d290,d290_1,d290_7,1).
bond(train,d290,d290_2,d290_8,1).
bond(train,d290,d290_5,d290_9,1).
bond(train,d290,d290_6,d290_10,1).
bond(train,d290,d290_3,d290_11,1).
bond(train,d290,d290_11,d290_12,1).
bond(train,d290,d290_12,d290_13,1).
bond(train,d290,d290_13,d290_4,1).
bond(train,d290,d290_13,d290_14,2).
bond(train,d290,d290_11,d290_15,2).
bond(train,d290,d290_12,d290_16,1).
bond(train,d290,d290_16,d290_17,1).
bond(train,d290,d290_17,d290_18,1).
bond(train,d290,d290_17,d290_19,1).
bond(train,d290,d290_17,d290_20,1).
bond(train,d291,d291_1,d291_2,1).
bond(train,d291,d291_2,d291_3,1).
bond(train,d291,d291_3,d291_4,1).
bond(train,d291,d291_4,d291_5,1).
bond(train,d291,d291_4,d291_6,1).
bond(train,d291,d291_4,d291_7,1).
bond(train,d291,d291_2,d291_8,2).
bond(train,d291,d291_1,d291_9,1).
bond(train,d291,d291_1,d291_10,1).
bond(train,d292,d292_1,d292_2,1).
bond(train,d292,d292_1,d292_3,1).
bond(train,d292,d292_2,d292_4,2).
bond(train,d292,d292_2,d292_5,1).
bond(train,d292,d292_5,d292_6,1).
bond(train,d292,d292_5,d292_7,1).
bond(train,d292,d292_5,d292_8,1).
bond(train,d292,d292_6,d292_9,1).
bond(train,d292,d292_6,d292_10,1).
bond(train,d292,d292_6,d292_11,1).
bond(train,d292,d292_9,d292_12,2).
bond(train,d292,d292_9,d292_13,1).
bond(train,d292,d292_13,d292_14,1).
bond(train,d292,d292_13,d292_15,1).
bond(train,d292,d292_14,d292_16,1).
bond(train,d292,d292_16,d292_17,1).
bond(train,d292,d292_16,d292_18,1).
bond(train,d292,d292_16,d292_19,1).
bond(train,d292,d292_14,d292_20,1).
bond(train,d292,d292_20,d292_21,1).
bond(train,d292,d292_20,d292_22,1).
bond(train,d292,d292_20,d292_23,1).
bond(train,d293,d293_1,d293_2,7).
bond(train,d293,d293_2,d293_3,7).
bond(train,d293,d293_3,d293_4,7).
bond(train,d293,d293_4,d293_5,7).
bond(train,d293,d293_5,d293_6,7).
bond(train,d293,d293_6,d293_1,7).
bond(train,d293,d293_1,d293_7,1).
bond(train,d293,d293_2,d293_8,1).
bond(train,d293,d293_5,d293_9,1).
bond(train,d293,d293_6,d293_10,1).
bond(train,d293,d293_3,d293_11,7).
bond(train,d293,d293_11,d293_12,7).
bond(train,d293,d293_12,d293_13,7).
bond(train,d293,d293_13,d293_4,7).
bond(train,d293,d293_12,d293_14,1).
bond(train,d294,d294_1,d294_2,7).
bond(train,d294,d294_2,d294_3,7).
bond(train,d294,d294_3,d294_4,7).
bond(train,d294,d294_4,d294_5,7).
bond(train,d294,d294_5,d294_6,7).
bond(train,d294,d294_6,d294_1,7).
bond(train,d294,d294_1,d294_7,1).
bond(train,d294,d294_5,d294_8,1).
bond(train,d294,d294_6,d294_9,1).
bond(train,d294,d294_3,d294_10,7).
bond(train,d294,d294_10,d294_11,7).
bond(train,d294,d294_11,d294_12,7).
bond(train,d294,d294_12,d294_13,7).
bond(train,d294,d294_13,d294_4,7).
bond(train,d294,d294_11,d294_14,1).
bond(train,d294,d294_12,d294_15,1).
bond(train,d294,d294_13,d294_16,1).
bond(train,d294,d294_2,d294_17,1).
bond(train,d294,d294_17,d294_18,1).
bond(train,d295,d295_1,d295_2,2).
bond(train,d295,d295_1,d295_3,1).
bond(train,d295,d295_1,d295_4,1).
bond(train,d295,d295_2,d295_5,1).
bond(train,d295,d295_2,d295_6,1).
bond(train,d295,d295_5,d295_7,1).
bond(train,d295,d295_5,d295_8,1).
bond(train,d295,d295_5,d295_9,1).
bond(train,d3,d3_1,d3_2,7).
bond(train,d3,d3_2,d3_3,7).
bond(train,d3,d3_3,d3_4,7).
bond(train,d3,d3_4,d3_5,7).
bond(train,d3,d3_5,d3_6,7).
bond(train,d3,d3_6,d3_1,7).
bond(train,d3,d3_1,d3_7,1).
bond(train,d3,d3_2,d3_8,1).
bond(train,d3,d3_5,d3_9,1).
bond(train,d3,d3_6,d3_10,1).
bond(train,d3,d3_3,d3_11,1).
bond(train,d3,d3_11,d3_12,1).
bond(train,d3,d3_12,d3_13,7).
bond(train,d3,d3_13,d3_14,1).
bond(train,d3,d3_14,d3_4,1).
bond(train,d3,d3_12,d3_15,7).
bond(train,d3,d3_15,d3_16,7).
bond(train,d3,d3_16,d3_17,7).
bond(train,d3,d3_17,d3_18,7).
bond(train,d3,d3_18,d3_13,7).
bond(train,d3,d3_15,d3_19,1).
bond(train,d3,d3_16,d3_20,1).
bond(train,d3,d3_14,d3_21,2).
bond(train,d3,d3_11,d3_22,2).
bond(train,d3,d3_17,d3_23,1).
bond(train,d3,d3_23,d3_24,1).
bond(train,d3,d3_23,d3_25,1).
bond(train,d3,d3_23,d3_26,1).
bond(train,d3,d3_18,d3_27,1).
bond(train,d3,d3_27,d3_28,1).
bond(train,d3,d3_27,d3_29,1).
bond(train,d30,d30_1,d30_2,7).
bond(train,d30,d30_2,d30_3,7).
bond(train,d30,d30_3,d30_4,7).
bond(train,d30,d30_4,d30_5,7).
bond(train,d30,d30_5,d30_6,7).
bond(train,d30,d30_6,d30_1,7).
bond(train,d30,d30_1,d30_7,1).
bond(train,d30,d30_3,d30_8,1).
bond(train,d30,d30_4,d30_9,1).
bond(train,d30,d30_6,d30_10,1).
bond(train,d30,d30_5,d30_11,1).
bond(train,d30,d30_2,d30_12,1).
bond(train,d30,d30_11,d30_13,1).
bond(train,d30,d30_11,d30_14,1).
bond(train,d31,d31a_1,d31a_2,7).
bond(train,d31,d31a_2,d31a_3,7).
bond(train,d31,d31a_3,d31a_4,7).
bond(train,d31,d31a_4,d31a_5,7).
bond(train,d31,d31a_5,d31a_6,7).
bond(train,d31,d31a_6,d31a_1,7).
bond(train,d31,d31a_3,d31a_7,1).
bond(train,d31,d31a_5,d31a_8,1).
bond(train,d31,d31a_6,d31a_9,1).
bond(train,d31,d31a_10,d31a_11,7).
bond(train,d31,d31a_11,d31a_12,7).
bond(train,d31,d31a_12,d31a_13,7).
bond(train,d31,d31a_13,d31a_14,7).
bond(train,d31,d31a_14,d31a_15,7).
bond(train,d31,d31a_15,d31a_10,7).
bond(train,d31,d31a_11,d31a_16,1).
bond(train,d31,d31a_14,d31a_17,1).
bond(train,d31,d31a_15,d31a_18,1).
bond(train,d31,d31a_4,d31a_10,1).
bond(train,d31,d31a_1,d31a_19,1).
bond(train,d31,d31a_2,d31a_20,1).
bond(train,d31,d31a_20,d31a_21,1).
bond(train,d31,d31a_21,d31a_22,1).
bond(train,d31,d31a_21,d31a_23,1).
bond(train,d31,d31a_21,d31a_24,1).
bond(train,d31,d31a_12,d31a_25,1).
bond(train,d31,d31a_25,d31a_26,1).
bond(train,d31,d31a_26,d31a_27,1).
bond(train,d31,d31a_26,d31a_28,1).
bond(train,d31,d31a_26,d31a_29,1).
bond(train,d31,d31a_13,d31a_30,1).
bond(train,d31,d31a_19,d31a_31,1).
bond(train,d31,d31a_19,d31a_32,1).
bond(train,d31,d31a_30,d31a_33,1).
bond(train,d31,d31a_30,d31a_34,1).
bond(train,d31,d31b_1,d31b_2,1).
bond(train,d32,d32a_1,d32a_2,7).
bond(train,d32,d32a_2,d32a_3,7).
bond(train,d32,d32a_3,d32a_4,7).
bond(train,d32,d32a_4,d32a_5,7).
bond(train,d32,d32a_5,d32a_6,7).
bond(train,d32,d32a_6,d32a_1,7).
bond(train,d32,d32a_3,d32a_7,1).
bond(train,d32,d32a_5,d32a_8,1).
bond(train,d32,d32a_6,d32a_9,1).
bond(train,d32,d32a_10,d32a_11,7).
bond(train,d32,d32a_11,d32a_12,7).
bond(train,d32,d32a_12,d32a_13,7).
bond(train,d32,d32a_13,d32a_14,7).
bond(train,d32,d32a_14,d32a_15,7).
bond(train,d32,d32a_15,d32a_10,7).
bond(train,d32,d32a_11,d32a_16,1).
bond(train,d32,d32a_14,d32a_17,1).
bond(train,d32,d32a_15,d32a_18,1).
bond(train,d32,d32a_4,d32a_10,1).
bond(train,d32,d32a_1,d32a_19,1).
bond(train,d32,d32a_13,d32a_20,1).
bond(train,d32,d32a_2,d32a_21,1).
bond(train,d32,d32a_21,d32a_22,1).
bond(train,d32,d32a_21,d32a_23,1).
bond(train,d32,d32a_21,d32a_24,1).
bond(train,d32,d32a_12,d32a_25,1).
bond(train,d32,d32a_25,d32a_26,1).
bond(train,d32,d32a_25,d32a_27,1).
bond(train,d32,d32a_25,d32a_28,1).
bond(train,d32,d32a_19,d32a_29,1).
bond(train,d32,d32a_19,d32a_30,1).
bond(train,d32,d32a_20,d32a_31,1).
bond(train,d32,d32a_20,d32a_32,1).
bond(train,d32,d32b_1,d32b_2,1).
bond(train,d33,d33_1,d33_2,7).
bond(train,d33,d33_2,d33_3,7).
bond(train,d33,d33_3,d33_4,7).
bond(train,d33,d33_4,d33_5,7).
bond(train,d33,d33_5,d33_6,7).
bond(train,d33,d33_6,d33_1,7).
bond(train,d33,d33_1,d33_7,1).
bond(train,d33,d33_2,d33_8,1).
bond(train,d33,d33_3,d33_9,1).
bond(train,d33,d33_4,d33_10,1).
bond(train,d33,d33_6,d33_11,1).
bond(train,d33,d33_5,d33_12,1).
bond(train,d33,d33_12,d33_13,1).
bond(train,d33,d33_12,d33_14,1).
bond(train,d34,d34_1,d34_2,7).
bond(train,d34,d34_2,d34_3,7).
bond(train,d34,d34_3,d34_4,7).
bond(train,d34,d34_4,d34_5,7).
bond(train,d34,d34_5,d34_6,7).
bond(train,d34,d34_6,d34_1,7).
bond(train,d34,d34_1,d34_7,1).
bond(train,d34,d34_2,d34_8,1).
bond(train,d34,d34_3,d34_9,1).
bond(train,d34,d34_5,d34_10,1).
bond(train,d34,d34_6,d34_11,1).
bond(train,d34,d34_12,d34_13,7).
bond(train,d34,d34_13,d34_14,7).
bond(train,d34,d34_14,d34_15,7).
bond(train,d34,d34_15,d34_16,7).
bond(train,d34,d34_16,d34_17,7).
bond(train,d34,d34_17,d34_12,7).
bond(train,d34,d34_13,d34_18,1).
bond(train,d34,d34_14,d34_19,1).
bond(train,d34,d34_15,d34_20,1).
bond(train,d34,d34_16,d34_21,1).
bond(train,d34,d34_17,d34_22,1).
bond(train,d34,d34_4,d34_23,1).
bond(train,d34,d34_12,d34_24,1).
bond(train,d34,d34_23,d34_24,2).
bond(train,d35,d35_1,d35_2,7).
bond(train,d35,d35_2,d35_3,7).
bond(train,d35,d35_3,d35_4,7).
bond(train,d35,d35_4,d35_5,7).
bond(train,d35,d35_5,d35_6,7).
bond(train,d35,d35_6,d35_1,7).
bond(train,d35,d35_2,d35_7,1).
bond(train,d35,d35_3,d35_8,1).
bond(train,d35,d35_6,d35_9,1).
bond(train,d35,d35_5,d35_10,1).
bond(train,d35,d35_4,d35_11,1).
bond(train,d35,d35_11,d35_12,1).
bond(train,d35,d35_11,d35_13,1).
bond(train,d35,d35_11,d35_14,1).
bond(train,d35,d35_1,d35_15,1).
bond(train,d35,d35_10,d35_16,1).
bond(train,d35,d35_10,d35_17,1).
bond(train,d36,d36a_1,d36a_2,7).
bond(train,d36,d36a_2,d36a_3,7).
bond(train,d36,d36a_3,d36a_4,7).
bond(train,d36,d36a_4,d36a_5,7).
bond(train,d36,d36a_5,d36a_6,7).
bond(train,d36,d36a_6,d36a_1,7).
bond(train,d36,d36a_2,d36a_7,1).
bond(train,d36,d36a_5,d36a_8,1).
bond(train,d36,d36a_9,d36a_10,7).
bond(train,d36,d36a_10,d36a_11,7).
bond(train,d36,d36a_11,d36a_12,7).
bond(train,d36,d36a_12,d36a_13,7).
bond(train,d36,d36a_13,d36a_14,7).
bond(train,d36,d36a_14,d36a_9,7).
bond(train,d36,d36a_12,d36a_15,1).
bond(train,d36,d36a_13,d36a_16,1).
bond(train,d36,d36a_10,d36a_17,7).
bond(train,d36,d36a_17,d36a_18,7).
bond(train,d36,d36a_18,d36a_19,7).
bond(train,d36,d36a_19,d36a_20,7).
bond(train,d36,d36a_20,d36a_11,7).
bond(train,d36,d36a_17,d36a_21,1).
bond(train,d36,d36a_18,d36a_22,1).
bond(train,d36,d36a_19,d36a_23,1).
bond(train,d36,d36a_20,d36a_24,1).
bond(train,d36,d36a_4,d36a_25,1).
bond(train,d36,d36a_9,d36a_26,1).
bond(train,d36,d36a_14,d36a_27,1).
bond(train,d36,d36a_1,d36a_28,1).
bond(train,d36,d36a_6,d36a_29,1).
bond(train,d36,d36a_29,d36a_30,1).
bond(train,d36,d36a_29,d36a_31,1).
bond(train,d36,d36a_29,d36a_32,1).
bond(train,d36,d36a_25,d36a_26,2).
bond(train,d36,d36a_27,d36a_33,1).
bond(train,d36,d36a_34,d36a_35,1).
bond(train,d36,d36a_34,d36a_36,2).
bond(train,d36,d36a_34,d36a_37,2).
bond(train,d36,d36a_3,d36a_34,1).
bond(train,d36,d36a_35,d36a_38,1).
bond(train,d37,d37_1,d37_2,7).
bond(train,d37,d37_2,d37_3,7).
bond(train,d37,d37_3,d37_4,7).
bond(train,d37,d37_4,d37_5,7).
bond(train,d37,d37_5,d37_6,7).
bond(train,d37,d37_6,d37_1,7).
bond(train,d37,d37_3,d37_7,1).
bond(train,d37,d37_5,d37_8,1).
bond(train,d37,d37_6,d37_9,1).
bond(train,d37,d37_10,d37_11,7).
bond(train,d37,d37_11,d37_12,7).
bond(train,d37,d37_12,d37_13,7).
bond(train,d37,d37_13,d37_14,7).
bond(train,d37,d37_14,d37_15,7).
bond(train,d37,d37_15,d37_10,7).
bond(train,d37,d37_11,d37_16,1).
bond(train,d37,d37_14,d37_17,1).
bond(train,d37,d37_15,d37_18,1).
bond(train,d37,d37_4,d37_10,1).
bond(train,d37,d37_2,d37_19,1).
bond(train,d37,d37_19,d37_20,1).
bond(train,d37,d37_20,d37_21,1).
bond(train,d37,d37_20,d37_22,1).
bond(train,d37,d37_20,d37_23,1).
bond(train,d37,d37_1,d37_24,1).
bond(train,d37,d37_12,d37_25,1).
bond(train,d37,d37_25,d37_26,1).
bond(train,d37,d37_26,d37_27,1).
bond(train,d37,d37_26,d37_28,1).
bond(train,d37,d37_26,d37_29,1).
bond(train,d37,d37_13,d37_30,1).
bond(train,d37,d37_24,d37_31,2).
bond(train,d37,d37_31,d37_32,2).
bond(train,d37,d37_30,d37_33,2).
bond(train,d37,d37_33,d37_34,2).
bond(train,d38,d38_1,d38_2,7).
bond(train,d38,d38_2,d38_3,7).
bond(train,d38,d38_3,d38_4,7).
bond(train,d38,d38_4,d38_5,7).
bond(train,d38,d38_5,d38_6,7).
bond(train,d38,d38_6,d38_1,7).
bond(train,d38,d38_1,d38_7,1).
bond(train,d38,d38_3,d38_8,1).
bond(train,d38,d38_6,d38_9,1).
bond(train,d38,d38_5,d38_10,1).
bond(train,d38,d38_10,d38_11,1).
bond(train,d38,d38_10,d38_12,1).
bond(train,d38,d38_10,d38_13,1).
bond(train,d38,d38_4,d38_14,1).
bond(train,d38,d38_2,d38_15,1).
bond(train,d38,d38_14,d38_16,2).
bond(train,d38,d38_14,d38_17,2).
bond(train,d38,d38_15,d38_18,2).
bond(train,d38,d38_15,d38_19,2).
bond(train,d39,d39_1,d39_2,7).
bond(train,d39,d39_2,d39_3,7).
bond(train,d39,d39_3,d39_4,7).
bond(train,d39,d39_4,d39_5,7).
bond(train,d39,d39_5,d39_6,7).
bond(train,d39,d39_6,d39_1,7).
bond(train,d39,d39_2,d39_7,1).
bond(train,d39,d39_3,d39_8,1).
bond(train,d39,d39_6,d39_9,1).
bond(train,d39,d39_5,d39_10,1).
bond(train,d39,d39_4,d39_11,1).
bond(train,d39,d39_11,d39_12,1).
bond(train,d39,d39_11,d39_13,1).
bond(train,d39,d39_11,d39_14,1).
bond(train,d39,d39_1,d39_15,1).
bond(train,d39,d39_15,d39_16,2).
bond(train,d39,d39_15,d39_17,2).
bond(train,d39,d39_10,d39_18,1).
bond(train,d39,d39_10,d39_19,1).
bond(train,d4,d4_1,d4_2,7).
bond(train,d4,d4_2,d4_3,7).
bond(train,d4,d4_3,d4_4,7).
bond(train,d4,d4_4,d4_5,7).
bond(train,d4,d4_5,d4_6,7).
bond(train,d4,d4_6,d4_1,7).
bond(train,d4,d4_1,d4_7,1).
bond(train,d4,d4_2,d4_8,1).
bond(train,d4,d4_3,d4_9,1).
bond(train,d4,d4_6,d4_10,1).
bond(train,d4,d4_5,d4_11,1).
bond(train,d4,d4_11,d4_12,1).
bond(train,d4,d4_11,d4_13,1).
bond(train,d4,d4_4,d4_14,1).
bond(train,d4,d4_14,d4_15,1).
bond(train,d4,d4_15,d4_16,1).
bond(train,d4,d4_15,d4_17,1).
bond(train,d4,d4_15,d4_18,1).
bond(train,d40,d40_1,d40_2,7).
bond(train,d40,d40_2,d40_3,7).
bond(train,d40,d40_3,d40_4,7).
bond(train,d40,d40_4,d40_5,7).
bond(train,d40,d40_5,d40_6,7).
bond(train,d40,d40_6,d40_1,7).
bond(train,d40,d40_2,d40_7,1).
bond(train,d40,d40_3,d40_8,1).
bond(train,d40,d40_5,d40_9,1).
bond(train,d40,d40_6,d40_10,1).
bond(train,d40,d40_11,d40_12,7).
bond(train,d40,d40_12,d40_13,7).
bond(train,d40,d40_13,d40_14,7).
bond(train,d40,d40_14,d40_15,7).
bond(train,d40,d40_15,d40_16,7).
bond(train,d40,d40_16,d40_11,7).
bond(train,d40,d40_12,d40_17,1).
bond(train,d40,d40_13,d40_18,1).
bond(train,d40,d40_15,d40_19,1).
bond(train,d40,d40_16,d40_20,1).
bond(train,d40,d40_4,d40_21,1).
bond(train,d40,d40_21,d40_11,1).
bond(train,d40,d40_14,d40_22,1).
bond(train,d40,d40_1,d40_23,1).
bond(train,d40,d40_21,d40_24,2).
bond(train,d40,d40_21,d40_25,2).
bond(train,d40,d40_23,d40_26,1).
bond(train,d40,d40_23,d40_27,1).
bond(train,d40,d40_22,d40_28,1).
bond(train,d40,d40_22,d40_29,1).
bond(train,d41,d41_1,d41_2,7).
bond(train,d41,d41_2,d41_3,7).
bond(train,d41,d41_3,d41_4,7).
bond(train,d41,d41_4,d41_5,7).
bond(train,d41,d41_5,d41_6,7).
bond(train,d41,d41_6,d41_1,7).
bond(train,d41,d41_1,d41_7,1).
bond(train,d41,d41_3,d41_8,1).
bond(train,d41,d41_2,d41_9,1).
bond(train,d41,d41_9,d41_10,1).
bond(train,d41,d41_9,d41_11,1).
bond(train,d41,d41_9,d41_12,1).
bond(train,d41,d41_5,d41_13,1).
bond(train,d41,d41_13,d41_14,1).
bond(train,d41,d41_13,d41_15,1).
bond(train,d41,d41_4,d41_16,1).
bond(train,d41,d41_6,d41_17,1).
bond(train,d41,d41_16,d41_18,2).
bond(train,d41,d41_16,d41_19,2).
bond(train,d41,d41_17,d41_20,2).
bond(train,d41,d41_17,d41_21,2).
bond(train,d41,d41_14,d41_22,1).
bond(train,d41,d41_22,d41_23,1).
bond(train,d41,d41_22,d41_24,1).
bond(train,d41,d41_22,d41_25,1).
bond(train,d41,d41_23,d41_26,1).
bond(train,d41,d41_23,d41_27,1).
bond(train,d41,d41_23,d41_28,1).
bond(train,d41,d41_15,d41_29,1).
bond(train,d41,d41_29,d41_30,1).
bond(train,d41,d41_29,d41_31,1).
bond(train,d41,d41_29,d41_32,1).
bond(train,d41,d41_30,d41_33,1).
bond(train,d41,d41_30,d41_34,1).
bond(train,d41,d41_30,d41_35,1).
bond(train,d41,d41_15,d41_36,1).
bond(train,d41,d41_15,d41_37,1).
bond(train,d41,d41_14,d41_38,1).
bond(train,d41,d41_14,d41_39,1).
bond(train,d42,d42_1,d42_2,7).
bond(train,d42,d42_2,d42_3,7).
bond(train,d42,d42_3,d42_4,7).
bond(train,d42,d42_4,d42_5,7).
bond(train,d42,d42_5,d42_6,7).
bond(train,d42,d42_6,d42_1,7).
bond(train,d42,d42_1,d42_7,1).
bond(train,d42,d42_3,d42_8,1).
bond(train,d42,d42_6,d42_9,1).
bond(train,d42,d42_5,d42_10,1).
bond(train,d42,d42_4,d42_11,1).
bond(train,d42,d42_11,d42_12,1).
bond(train,d42,d42_11,d42_13,1).
bond(train,d42,d42_11,d42_14,1).
bond(train,d42,d42_2,d42_15,1).
bond(train,d42,d42_10,d42_16,1).
bond(train,d42,d42_10,d42_17,1).
bond(train,d43,d43_1,d43_2,7).
bond(train,d43,d43_2,d43_3,7).
bond(train,d43,d43_3,d43_4,7).
bond(train,d43,d43_4,d43_5,7).
bond(train,d43,d43_5,d43_6,7).
bond(train,d43,d43_6,d43_1,7).
bond(train,d43,d43_1,d43_7,1).
bond(train,d43,d43_6,d43_8,1).
bond(train,d43,d43_3,d43_9,1).
bond(train,d43,d43_9,d43_10,1).
bond(train,d43,d43_10,d43_11,7).
bond(train,d43,d43_11,d43_12,1).
bond(train,d43,d43_12,d43_4,1).
bond(train,d43,d43_10,d43_13,7).
bond(train,d43,d43_13,d43_14,7).
bond(train,d43,d43_14,d43_15,7).
bond(train,d43,d43_15,d43_16,7).
bond(train,d43,d43_16,d43_11,7).
bond(train,d43,d43_14,d43_17,1).
bond(train,d43,d43_15,d43_18,1).
bond(train,d43,d43_12,d43_19,2).
bond(train,d43,d43_9,d43_20,2).
bond(train,d43,d43_16,d43_21,1).
bond(train,d43,d43_13,d43_22,1).
bond(train,d43,d43_21,d43_23,1).
bond(train,d43,d43_21,d43_24,1).
bond(train,d43,d43_22,d43_25,1).
bond(train,d43,d43_22,d43_26,1).
bond(train,d43,d43_5,d43_27,1).
bond(train,d43,d43_2,d43_28,1).
bond(train,d43,d43_27,d43_29,1).
bond(train,d43,d43_27,d43_30,1).
bond(train,d43,d43_28,d43_31,1).
bond(train,d43,d43_28,d43_32,1).
bond(train,d44,d44_1,d44_2,7).
bond(train,d44,d44_2,d44_3,7).
bond(train,d44,d44_3,d44_4,7).
bond(train,d44,d44_4,d44_5,7).
bond(train,d44,d44_5,d44_6,7).
bond(train,d44,d44_6,d44_1,7).
bond(train,d44,d44_1,d44_7,1).
bond(train,d44,d44_3,d44_8,1).
bond(train,d44,d44_6,d44_9,1).
bond(train,d44,d44_5,d44_10,1).
bond(train,d44,d44_4,d44_11,1).
bond(train,d44,d44_11,d44_12,1).
bond(train,d44,d44_11,d44_13,1).
bond(train,d44,d44_11,d44_14,1).
bond(train,d44,d44_2,d44_15,1).
bond(train,d44,d44_15,d44_16,1).
bond(train,d44,d44_16,d44_17,1).
bond(train,d44,d44_16,d44_18,1).
bond(train,d44,d44_16,d44_19,1).
bond(train,d44,d44_10,d44_20,1).
bond(train,d44,d44_10,d44_21,1).
bond(train,d45,d45_1,d45_2,7).
bond(train,d45,d45_2,d45_3,7).
bond(train,d45,d45_3,d45_4,7).
bond(train,d45,d45_4,d45_5,7).
bond(train,d45,d45_5,d45_6,7).
bond(train,d45,d45_6,d45_1,7).
bond(train,d45,d45_1,d45_7,1).
bond(train,d45,d45_3,d45_8,1).
bond(train,d45,d45_5,d45_9,1).
bond(train,d45,d45_2,d45_10,1).
bond(train,d45,d45_4,d45_11,1).
bond(train,d45,d45_6,d45_12,1).
bond(train,d45,d45_9,d45_13,1).
bond(train,d45,d45_9,d45_14,1).
bond(train,d45,d45_10,d45_15,1).
bond(train,d45,d45_10,d45_16,1).
bond(train,d46,d46_1,d46_2,7).
bond(train,d46,d46_2,d46_3,7).
bond(train,d46,d46_3,d46_4,7).
bond(train,d46,d46_4,d46_5,7).
bond(train,d46,d46_5,d46_6,7).
bond(train,d46,d46_6,d46_1,7).
bond(train,d46,d46_2,d46_7,1).
bond(train,d46,d46_5,d46_8,1).
bond(train,d46,d46_6,d46_9,1).
bond(train,d46,d46_1,d46_10,1).
bond(train,d46,d46_10,d46_11,2).
bond(train,d46,d46_10,d46_12,2).
bond(train,d46,d46_4,d46_13,7).
bond(train,d46,d46_3,d46_14,7).
bond(train,d46,d46_14,d46_15,7).
bond(train,d46,d46_15,d46_13,7).
bond(train,d46,d46_13,d46_16,1).
bond(train,d46,d46_15,d46_17,1).
bond(train,d47,d47_1,d47_2,7).
bond(train,d47,d47_2,d47_3,7).
bond(train,d47,d47_3,d47_4,7).
bond(train,d47,d47_4,d47_5,7).
bond(train,d47,d47_5,d47_6,7).
bond(train,d47,d47_6,d47_1,7).
bond(train,d47,d47_1,d47_7,1).
bond(train,d47,d47_2,d47_8,1).
bond(train,d47,d47_3,d47_9,1).
bond(train,d47,d47_5,d47_10,1).
bond(train,d47,d47_6,d47_11,1).
bond(train,d47,d47_12,d47_13,7).
bond(train,d47,d47_13,d47_14,7).
bond(train,d47,d47_14,d47_15,7).
bond(train,d47,d47_15,d47_16,7).
bond(train,d47,d47_16,d47_17,7).
bond(train,d47,d47_17,d47_12,7).
bond(train,d47,d47_14,d47_18,1).
bond(train,d47,d47_15,d47_19,1).
bond(train,d47,d47_16,d47_20,7).
bond(train,d47,d47_20,d47_21,7).
bond(train,d47,d47_21,d47_22,7).
bond(train,d47,d47_22,d47_23,7).
bond(train,d47,d47_23,d47_17,7).
bond(train,d47,d47_20,d47_24,1).
bond(train,d47,d47_21,d47_25,1).
bond(train,d47,d47_22,d47_26,1).
bond(train,d47,d47_23,d47_27,1).
bond(train,d47,d47_4,d47_28,1).
bond(train,d47,d47_12,d47_29,1).
bond(train,d47,d47_13,d47_30,1).
bond(train,d47,d47_28,d47_29,2).
bond(train,d47,d47_30,d47_31,1).
bond(train,d48,d48_1,d48_2,7).
bond(train,d48,d48_2,d48_3,7).
bond(train,d48,d48_3,d48_4,7).
bond(train,d48,d48_4,d48_5,7).
bond(train,d48,d48_5,d48_6,7).
bond(train,d48,d48_6,d48_1,7).
bond(train,d48,d48_1,d48_7,1).
bond(train,d48,d48_4,d48_8,1).
bond(train,d48,d48_6,d48_9,1).
bond(train,d48,d48_5,d48_10,1).
bond(train,d48,d48_10,d48_11,1).
bond(train,d48,d48_11,d48_12,1).
bond(train,d48,d48_12,d48_13,1).
bond(train,d48,d48_12,d48_14,1).
bond(train,d48,d48_12,d48_15,1).
bond(train,d48,d48_11,d48_16,2).
bond(train,d48,d48_10,d48_17,1).
bond(train,d48,d48_3,d48_18,1).
bond(train,d48,d48_2,d48_19,1).
bond(train,d48,d48_19,d48_20,1).
bond(train,d48,d48_20,d48_21,1).
bond(train,d48,d48_21,d48_22,1).
bond(train,d48,d48_21,d48_23,1).
bond(train,d48,d48_21,d48_24,1).
bond(train,d48,d48_18,d48_25,1).
bond(train,d48,d48_18,d48_26,1).
bond(train,d48,d48_20,d48_27,1).
bond(train,d48,d48_20,d48_28,1).
bond(train,d49,d49_1,d49_2,7).
bond(train,d49,d49_2,d49_3,7).
bond(train,d49,d49_3,d49_4,7).
bond(train,d49,d49_4,d49_5,7).
bond(train,d49,d49_5,d49_6,7).
bond(train,d49,d49_6,d49_1,7).
bond(train,d49,d49_1,d49_7,1).
bond(train,d49,d49_4,d49_8,1).
bond(train,d49,d49_6,d49_9,1).
bond(train,d49,d49_5,d49_10,1).
bond(train,d49,d49_2,d49_11,1).
bond(train,d49,d49_3,d49_12,1).
bond(train,d49,d49_10,d49_13,1).
bond(train,d49,d49_10,d49_14,1).
bond(train,d49,d49_12,d49_15,2).
bond(train,d49,d49_12,d49_16,2).
bond(train,d49,d49_11,d49_17,1).
bond(train,d5,d5_1,d5_2,7).
bond(train,d5,d5_2,d5_3,7).
bond(train,d5,d5_3,d5_4,7).
bond(train,d5,d5_4,d5_5,7).
bond(train,d5,d5_5,d5_6,7).
bond(train,d5,d5_6,d5_1,7).
bond(train,d5,d5_1,d5_7,1).
bond(train,d5,d5_4,d5_8,1).
bond(train,d5,d5_6,d5_9,1).
bond(train,d5,d5_5,d5_10,1).
bond(train,d5,d5_3,d5_11,1).
bond(train,d5,d5_2,d5_12,1).
bond(train,d5,d5_10,d5_13,1).
bond(train,d5,d5_10,d5_14,1).
bond(train,d5,d5_11,d5_15,1).
bond(train,d5,d5_11,d5_16,1).
bond(train,d50,d50_1,d50_2,7).
bond(train,d50,d50_2,d50_3,7).
bond(train,d50,d50_3,d50_4,7).
bond(train,d50,d50_4,d50_5,7).
bond(train,d50,d50_5,d50_1,7).
bond(train,d50,d50_4,d50_6,1).
bond(train,d50,d50_1,d50_7,1).
bond(train,d50,d50_7,d50_8,2).
bond(train,d50,d50_7,d50_9,2).
bond(train,d50,d50_6,d50_10,1).
bond(train,d50,d50_6,d50_11,1).
bond(train,d50,d50_2,d50_12,1).
bond(train,d51,d51_1,d51_2,1).
bond(train,d51,d51_2,d51_3,1).
bond(train,d51,d51_3,d51_4,2).
bond(train,d51,d51_4,d51_5,1).
bond(train,d51,d51_5,d51_6,1).
bond(train,d51,d51_6,d51_1,2).
bond(train,d51,d51_6,d51_7,1).
bond(train,d51,d51_1,d51_8,1).
bond(train,d51,d51_3,d51_9,1).
bond(train,d51,d51_4,d51_10,1).
bond(train,d51,d51_5,d51_11,2).
bond(train,d51,d51_2,d51_12,2).
bond(train,d51,d51_11,d51_13,1).
bond(train,d51,d51_12,d51_14,1).
bond(train,d51,d51_13,d51_15,1).
bond(train,d51,d51_14,d51_16,1).
bond(train,d52,d52_1,d52_2,7).
bond(train,d52,d52_2,d52_3,7).
bond(train,d52,d52_3,d52_4,7).
bond(train,d52,d52_4,d52_5,7).
bond(train,d52,d52_5,d52_6,7).
bond(train,d52,d52_6,d52_1,7).
bond(train,d52,d52_1,d52_7,1).
bond(train,d52,d52_2,d52_8,1).
bond(train,d52,d52_3,d52_9,1).
bond(train,d52,d52_5,d52_10,1).
bond(train,d52,d52_6,d52_11,1).
bond(train,d52,d52_12,d52_13,7).
bond(train,d52,d52_13,d52_14,7).
bond(train,d52,d52_14,d52_15,7).
bond(train,d52,d52_15,d52_16,7).
bond(train,d52,d52_16,d52_17,7).
bond(train,d52,d52_17,d52_12,7).
bond(train,d52,d52_14,d52_18,1).
bond(train,d52,d52_15,d52_19,1).
bond(train,d52,d52_16,d52_20,1).
bond(train,d52,d52_17,d52_21,1).
bond(train,d52,d52_4,d52_12,1).
bond(train,d52,d52_13,d52_22,1).
bond(train,d52,d52_22,d52_23,1).
bond(train,d52,d52_22,d52_24,1).
bond(train,d53,d53_1,d53_2,7).
bond(train,d53,d53_2,d53_3,7).
bond(train,d53,d53_3,d53_4,7).
bond(train,d53,d53_4,d53_5,7).
bond(train,d53,d53_5,d53_6,7).
bond(train,d53,d53_6,d53_1,7).
bond(train,d53,d53_2,d53_7,1).
bond(train,d53,d53_6,d53_8,1).
bond(train,d53,d53_5,d53_9,1).
bond(train,d53,d53_9,d53_10,1).
bond(train,d53,d53_4,d53_11,1).
bond(train,d53,d53_3,d53_12,1).
bond(train,d53,d53_1,d53_13,1).
bond(train,d53,d53_9,d53_14,2).
bond(train,d53,d53_10,d53_15,1).
bond(train,d53,d53_12,d53_16,1).
bond(train,d53,d53_12,d53_17,1).
bond(train,d54,d54_1,d54_2,7).
bond(train,d54,d54_2,d54_3,7).
bond(train,d54,d54_3,d54_4,7).
bond(train,d54,d54_4,d54_5,7).
bond(train,d54,d54_5,d54_6,7).
bond(train,d54,d54_6,d54_1,7).
bond(train,d54,d54_1,d54_7,1).
bond(train,d54,d54_4,d54_8,1).
bond(train,d54,d54_6,d54_9,1).
bond(train,d54,d54_2,d54_10,1).
bond(train,d54,d54_10,d54_11,1).
bond(train,d54,d54_11,d54_12,1).
bond(train,d54,d54_12,d54_13,1).
bond(train,d54,d54_12,d54_14,1).
bond(train,d54,d54_12,d54_15,1).
bond(train,d54,d54_3,d54_16,1).
bond(train,d54,d54_5,d54_17,1).
bond(train,d54,d54_16,d54_18,2).
bond(train,d54,d54_16,d54_19,2).
bond(train,d54,d54_17,d54_20,1).
bond(train,d54,d54_20,d54_21,1).
bond(train,d54,d54_21,d54_22,1).
bond(train,d54,d54_21,d54_23,1).
bond(train,d54,d54_21,d54_24,1).
bond(train,d54,d54_20,d54_25,2).
bond(train,d54,d54_17,d54_26,1).
bond(train,d54,d54_11,d54_27,1).
bond(train,d54,d54_11,d54_28,1).
bond(train,d55,d55_1,d55_2,7).
bond(train,d55,d55_2,d55_3,7).
bond(train,d55,d55_3,d55_4,7).
bond(train,d55,d55_4,d55_5,7).
bond(train,d55,d55_5,d55_6,7).
bond(train,d55,d55_6,d55_1,7).
bond(train,d55,d55_1,d55_7,1).
bond(train,d55,d55_3,d55_8,1).
bond(train,d55,d55_6,d55_9,1).
bond(train,d55,d55_5,d55_10,1).
bond(train,d55,d55_4,d55_11,1).
bond(train,d55,d55_2,d55_12,1).
bond(train,d55,d55_11,d55_13,2).
bond(train,d55,d55_11,d55_14,2).
bond(train,d55,d55_10,d55_15,1).
bond(train,d55,d55_10,d55_16,1).
bond(train,d55,d55_12,d55_17,1).
bond(train,d55,d55_12,d55_18,1).
bond(train,d56,d56_1,d56_2,7).
bond(train,d56,d56_2,d56_3,7).
bond(train,d56,d56_3,d56_4,7).
bond(train,d56,d56_4,d56_5,7).
bond(train,d56,d56_5,d56_6,7).
bond(train,d56,d56_6,d56_1,7).
bond(train,d56,d56_1,d56_7,1).
bond(train,d56,d56_3,d56_8,1).
bond(train,d56,d56_6,d56_9,1).
bond(train,d56,d56_5,d56_10,1).
bond(train,d56,d56_4,d56_11,1).
bond(train,d56,d56_2,d56_12,1).
bond(train,d56,d56_12,d56_13,2).
bond(train,d56,d56_12,d56_14,2).
bond(train,d56,d56_11,d56_15,1).
bond(train,d56,d56_11,d56_16,1).
bond(train,d56,d56_10,d56_17,1).
bond(train,d57,d57_1,d57_2,7).
bond(train,d57,d57_2,d57_3,7).
bond(train,d57,d57_3,d57_4,7).
bond(train,d57,d57_4,d57_5,7).
bond(train,d57,d57_5,d57_6,7).
bond(train,d57,d57_6,d57_1,7).
bond(train,d57,d57_2,d57_7,1).
bond(train,d57,d57_3,d57_8,1).
bond(train,d57,d57_6,d57_9,1).
bond(train,d57,d57_5,d57_10,1).
bond(train,d57,d57_4,d57_11,1).
bond(train,d57,d57_1,d57_12,1).
bond(train,d57,d57_12,d57_13,2).
bond(train,d57,d57_12,d57_14,2).
bond(train,d57,d57_11,d57_15,1).
bond(train,d57,d57_11,d57_16,1).
bond(train,d57,d57_10,d57_17,1).
bond(train,d58,d58_1,d58_2,7).
bond(train,d58,d58_2,d58_3,7).
bond(train,d58,d58_3,d58_4,7).
bond(train,d58,d58_4,d58_5,7).
bond(train,d58,d58_5,d58_6,7).
bond(train,d58,d58_6,d58_1,7).
bond(train,d58,d58_2,d58_7,1).
bond(train,d58,d58_3,d58_8,1).
bond(train,d58,d58_6,d58_9,1).
bond(train,d58,d58_10,d58_11,7).
bond(train,d58,d58_11,d58_12,7).
bond(train,d58,d58_12,d58_13,7).
bond(train,d58,d58_13,d58_14,7).
bond(train,d58,d58_14,d58_15,7).
bond(train,d58,d58_15,d58_10,7).
bond(train,d58,d58_11,d58_16,1).
bond(train,d58,d58_12,d58_17,1).
bond(train,d58,d58_15,d58_18,1).
bond(train,d58,d58_19,d58_20,7).
bond(train,d58,d58_20,d58_21,7).
bond(train,d58,d58_21,d58_22,7).
bond(train,d58,d58_22,d58_23,7).
bond(train,d58,d58_23,d58_24,7).
bond(train,d58,d58_24,d58_19,7).
bond(train,d58,d58_20,d58_25,1).
bond(train,d58,d58_21,d58_26,1).
bond(train,d58,d58_22,d58_27,1).
bond(train,d58,d58_23,d58_28,1).
bond(train,d58,d58_24,d58_29,1).
bond(train,d58,d58_1,d58_30,1).
bond(train,d58,d58_30,d58_31,1).
bond(train,d58,d58_30,d58_32,1).
bond(train,d58,d58_30,d58_33,1).
bond(train,d58,d58_5,d58_34,1).
bond(train,d58,d58_4,d58_35,1).
bond(train,d58,d58_35,d58_10,1).
bond(train,d58,d58_13,d58_36,1).
bond(train,d58,d58_36,d58_19,1).
bond(train,d58,d58_14,d58_37,1).
bond(train,d58,d58_35,d58_38,1).
bond(train,d58,d58_36,d58_39,1).
bond(train,d58,d58_34,d58_40,2).
bond(train,d58,d58_34,d58_41,2).
bond(train,d58,d58_37,d58_42,2).
bond(train,d58,d58_37,d58_43,1).
bond(train,d58,d58_37,d58_44,2).
bond(train,d58,d58_43,d58_45,1).
bond(train,d59,d59_1,d59_2,7).
bond(train,d59,d59_2,d59_3,7).
bond(train,d59,d59_3,d59_4,7).
bond(train,d59,d59_4,d59_5,7).
bond(train,d59,d59_5,d59_6,7).
bond(train,d59,d59_6,d59_1,7).
bond(train,d59,d59_1,d59_7,1).
bond(train,d59,d59_3,d59_8,1).
bond(train,d59,d59_4,d59_9,1).
bond(train,d59,d59_6,d59_10,1).
bond(train,d59,d59_5,d59_11,1).
bond(train,d59,d59_2,d59_12,1).
bond(train,d59,d59_12,d59_13,1).
bond(train,d59,d59_13,d59_14,1).
bond(train,d59,d59_13,d59_15,1).
bond(train,d59,d59_13,d59_16,1).
bond(train,d59,d59_11,d59_17,1).
bond(train,d59,d59_11,d59_18,1).
bond(train,d6,d6_1,d6_2,7).
bond(train,d6,d6_2,d6_3,7).
bond(train,d6,d6_3,d6_4,7).
bond(train,d6,d6_4,d6_5,7).
bond(train,d6,d6_5,d6_6,7).
bond(train,d6,d6_6,d6_1,7).
bond(train,d6,d6_1,d6_7,1).
bond(train,d6,d6_6,d6_8,1).
bond(train,d6,d6_5,d6_9,1).
bond(train,d6,d6_2,d6_10,1).
bond(train,d6,d6_9,d6_11,1).
bond(train,d6,d6_9,d6_12,1).
bond(train,d6,d6_3,d6_13,1).
bond(train,d6,d6_4,d6_14,1).
bond(train,d6,d6_14,d6_15,1).
bond(train,d6,d6_14,d6_16,1).
bond(train,d60,d60_1,d60_2,7).
bond(train,d60,d60_2,d60_3,7).
bond(train,d60,d60_3,d60_4,7).
bond(train,d60,d60_4,d60_5,7).
bond(train,d60,d60_5,d60_6,7).
bond(train,d60,d60_6,d60_1,7).
bond(train,d60,d60_1,d60_7,1).
bond(train,d60,d60_3,d60_8,1).
bond(train,d60,d60_4,d60_9,1).
bond(train,d60,d60_6,d60_10,1).
bond(train,d60,d60_5,d60_11,1).
bond(train,d60,d60_2,d60_12,1).
bond(train,d60,d60_11,d60_13,1).
bond(train,d60,d60_11,d60_14,1).
bond(train,d61,d61_1,d61_2,7).
bond(train,d61,d61_2,d61_3,7).
bond(train,d61,d61_3,d61_4,7).
bond(train,d61,d61_4,d61_5,7).
bond(train,d61,d61_5,d61_6,7).
bond(train,d61,d61_6,d61_1,7).
bond(train,d61,d61_2,d61_7,1).
bond(train,d61,d61_3,d61_8,1).
bond(train,d61,d61_5,d61_9,1).
bond(train,d61,d61_6,d61_10,1).
bond(train,d61,d61_1,d61_11,1).
bond(train,d61,d61_4,d61_12,1).
bond(train,d61,d61_12,d61_13,1).
bond(train,d61,d61_13,d61_14,1).
bond(train,d61,d61_11,d61_15,2).
bond(train,d61,d61_11,d61_16,2).
bond(train,d61,d61_13,d61_17,2).
bond(train,d61,d61_13,d61_18,1).
bond(train,d61,d61_18,d61_19,1).
bond(train,d61,d61_19,d61_20,1).
bond(train,d61,d61_19,d61_21,1).
bond(train,d61,d61_19,d61_22,1).
bond(train,d61,d61_20,d61_23,1).
bond(train,d61,d61_20,d61_24,1).
bond(train,d61,d61_20,d61_25,1).
bond(train,d61,d61_14,d61_26,1).
bond(train,d61,d61_26,d61_27,1).
bond(train,d61,d61_26,d61_28,1).
bond(train,d61,d61_26,d61_29,1).
bond(train,d61,d61_27,d61_30,1).
bond(train,d61,d61_27,d61_31,1).
bond(train,d61,d61_27,d61_32,1).
bond(train,d62,d62_1,d62_2,7).
bond(train,d62,d62_2,d62_3,7).
bond(train,d62,d62_3,d62_4,7).
bond(train,d62,d62_4,d62_5,7).
bond(train,d62,d62_5,d62_6,7).
bond(train,d62,d62_6,d62_1,7).
bond(train,d62,d62_2,d62_7,1).
bond(train,d62,d62_5,d62_8,1).
bond(train,d62,d62_6,d62_9,1).
bond(train,d62,d62_3,d62_10,7).
bond(train,d62,d62_10,d62_11,7).
bond(train,d62,d62_11,d62_12,7).
bond(train,d62,d62_12,d62_13,7).
bond(train,d62,d62_13,d62_4,7).
bond(train,d62,d62_13,d62_14,1).
bond(train,d62,d62_11,d62_15,7).
bond(train,d62,d62_15,d62_16,7).
bond(train,d62,d62_16,d62_17,7).
bond(train,d62,d62_17,d62_18,7).
bond(train,d62,d62_18,d62_12,7).
bond(train,d62,d62_15,d62_19,1).
bond(train,d62,d62_17,d62_20,1).
bond(train,d62,d62_18,d62_21,1).
bond(train,d62,d62_1,d62_22,1).
bond(train,d62,d62_16,d62_23,1).
bond(train,d62,d62_22,d62_24,1).
bond(train,d62,d62_22,d62_25,1).
bond(train,d62,d62_23,d62_26,1).
bond(train,d62,d62_23,d62_27,1).
bond(train,d63,d63_1,d63_2,7).
bond(train,d63,d63_2,d63_3,7).
bond(train,d63,d63_3,d63_4,7).
bond(train,d63,d63_4,d63_5,7).
bond(train,d63,d63_5,d63_6,7).
bond(train,d63,d63_6,d63_1,7).
bond(train,d63,d63_2,d63_7,1).
bond(train,d63,d63_5,d63_8,1).
bond(train,d63,d63_6,d63_9,1).
bond(train,d63,d63_1,d63_10,1).
bond(train,d63,d63_3,d63_11,1).
bond(train,d63,d63_4,d63_12,1).
bond(train,d63,d63_12,d63_13,1).
bond(train,d63,d63_13,d63_14,1).
bond(train,d63,d63_13,d63_15,1).
bond(train,d63,d63_13,d63_16,1).
bond(train,d63,d63_14,d63_17,1).
bond(train,d63,d63_14,d63_18,1).
bond(train,d63,d63_14,d63_19,1).
bond(train,d63,d63_11,d63_20,2).
bond(train,d63,d63_11,d63_21,2).
bond(train,d63,d63_10,d63_22,1).
bond(train,d63,d63_10,d63_23,1).
bond(train,d63,d63_12,d63_24,1).
bond(train,d63,d63_17,d63_25,1).
bond(train,d64,d64_1,d64_2,7).
bond(train,d64,d64_2,d64_3,7).
bond(train,d64,d64_3,d64_4,7).
bond(train,d64,d64_4,d64_5,7).
bond(train,d64,d64_5,d64_6,7).
bond(train,d64,d64_6,d64_1,7).
bond(train,d64,d64_1,d64_7,1).
bond(train,d64,d64_2,d64_8,1).
bond(train,d64,d64_5,d64_9,1).
bond(train,d64,d64_6,d64_10,1).
bond(train,d64,d64_3,d64_11,7).
bond(train,d64,d64_11,d64_12,7).
bond(train,d64,d64_12,d64_13,7).
bond(train,d64,d64_13,d64_14,7).
bond(train,d64,d64_14,d64_4,7).
bond(train,d64,d64_11,d64_15,1).
bond(train,d64,d64_12,d64_16,1).
bond(train,d64,d64_14,d64_17,1).
bond(train,d64,d64_18,d64_19,7).
bond(train,d64,d64_19,d64_20,7).
bond(train,d64,d64_20,d64_21,7).
bond(train,d64,d64_21,d64_22,7).
bond(train,d64,d64_22,d64_23,7).
bond(train,d64,d64_23,d64_18,7).
bond(train,d64,d64_18,d64_24,1).
bond(train,d64,d64_19,d64_25,1).
bond(train,d64,d64_20,d64_26,1).
bond(train,d64,d64_21,d64_27,1).
bond(train,d64,d64_22,d64_28,1).
bond(train,d64,d64_13,d64_29,1).
bond(train,d64,d64_29,d64_23,1).
bond(train,d64,d64_29,d64_30,1).
bond(train,d65,d65_1,d65_2,7).
bond(train,d65,d65_2,d65_3,7).
bond(train,d65,d65_3,d65_4,7).
bond(train,d65,d65_4,d65_5,7).
bond(train,d65,d65_5,d65_6,7).
bond(train,d65,d65_6,d65_1,7).
bond(train,d65,d65_1,d65_7,1).
bond(train,d65,d65_4,d65_8,1).
bond(train,d65,d65_6,d65_9,1).
bond(train,d65,d65_2,d65_10,1).
bond(train,d65,d65_3,d65_11,1).
bond(train,d65,d65_5,d65_12,1).
bond(train,d65,d65_12,d65_13,1).
bond(train,d65,d65_12,d65_14,1).
bond(train,d65,d65_11,d65_15,2).
bond(train,d65,d65_11,d65_16,2).
bond(train,d65,d65_10,d65_17,1).
bond(train,d65,d65_14,d65_18,1).
bond(train,d65,d65_13,d65_19,1).
bond(train,d65,d65_12,d65_20,2).
bond(train,d66,d66_1,d66_2,7).
bond(train,d66,d66_2,d66_3,7).
bond(train,d66,d66_3,d66_4,7).
bond(train,d66,d66_4,d66_5,7).
bond(train,d66,d66_5,d66_6,7).
bond(train,d66,d66_6,d66_1,7).
bond(train,d66,d66_7,d66_8,7).
bond(train,d66,d66_8,d66_9,7).
bond(train,d66,d66_9,d66_10,7).
bond(train,d66,d66_10,d66_11,7).
bond(train,d66,d66_11,d66_12,7).
bond(train,d66,d66_12,d66_7,7).
bond(train,d66,d66_10,d66_13,7).
bond(train,d66,d66_13,d66_14,7).
bond(train,d66,d66_14,d66_15,7).
bond(train,d66,d66_15,d66_16,7).
bond(train,d66,d66_16,d66_11,7).
bond(train,d66,d66_7,d66_17,7).
bond(train,d66,d66_17,d66_18,7).
bond(train,d66,d66_18,d66_19,7).
bond(train,d66,d66_19,d66_20,7).
bond(train,d66,d66_20,d66_8,7).
bond(train,d66,d66_3,d66_12,1).
bond(train,d66,d66_4,d66_21,1).
bond(train,d66,d66_21,d66_22,1).
bond(train,d66,d66_21,d66_23,2).
bond(train,d66,d66_15,d66_24,1).
bond(train,d66,d66_1,d66_25,1).
bond(train,d66,d66_2,d66_26,1).
bond(train,d66,d66_5,d66_27,1).
bond(train,d66,d66_6,d66_28,1).
bond(train,d66,d66_17,d66_29,1).
bond(train,d66,d66_20,d66_30,1).
bond(train,d66,d66_24,d66_31,1).
bond(train,d66,d66_24,d66_32,1).
bond(train,d66,d66_24,d66_33,1).
bond(train,d66,d66_16,d66_34,1).
bond(train,d66,d66_13,d66_35,1).
bond(train,d66,d66_14,d66_36,2).
bond(train,d66,d66_36,d66_37,1).
bond(train,d66,d66_37,d66_38,1).
bond(train,d66,d66_37,d66_39,1).
bond(train,d66,d66_37,d66_40,1).
bond(train,d66,d66_38,d66_41,1).
bond(train,d66,d66_38,d66_42,1).
bond(train,d66,d66_38,d66_43,1).
bond(train,d66,d66_22,d66_44,1).
bond(train,d66,d66_44,d66_45,1).
bond(train,d66,d66_44,d66_46,1).
bond(train,d66,d66_44,d66_47,1).
bond(train,d66,d66_45,d66_48,1).
bond(train,d66,d66_45,d66_49,1).
bond(train,d66,d66_45,d66_50,1).
bond(train,d66,d66_19,d66_51,1).
bond(train,d66,d66_18,d66_52,1).
bond(train,d66,d66_52,d66_53,1).
bond(train,d66,d66_52,d66_54,1).
bond(train,d66,d66_52,d66_55,1).
bond(train,d66,d66_51,d66_56,1).
bond(train,d66,d66_56,d66_57,1).
bond(train,d66,d66_56,d66_58,1).
bond(train,d66,d66_56,d66_59,1).
bond(train,d66,d66_57,d66_60,1).
bond(train,d66,d66_57,d66_61,1).
bond(train,d66,d66_57,d66_62,1).
bond(train,d66,d66_51,d66_63,1).
bond(train,d67,d67_1,d67_2,7).
bond(train,d67,d67_2,d67_3,7).
bond(train,d67,d67_3,d67_4,7).
bond(train,d67,d67_4,d67_5,7).
bond(train,d67,d67_5,d67_6,7).
bond(train,d67,d67_6,d67_1,7).
bond(train,d67,d67_1,d67_7,1).
bond(train,d67,d67_3,d67_8,1).
bond(train,d67,d67_4,d67_9,1).
bond(train,d67,d67_6,d67_10,1).
bond(train,d67,d67_5,d67_11,1).
bond(train,d67,d67_11,d67_12,1).
bond(train,d67,d67_12,d67_13,1).
bond(train,d67,d67_13,d67_14,1).
bond(train,d67,d67_13,d67_15,1).
bond(train,d67,d67_13,d67_16,1).
bond(train,d67,d67_12,d67_17,2).
bond(train,d67,d67_2,d67_18,1).
bond(train,d67,d67_18,d67_19,1).
bond(train,d67,d67_19,d67_20,1).
bond(train,d67,d67_19,d67_21,1).
bond(train,d67,d67_19,d67_22,1).
bond(train,d67,d67_18,d67_23,2).
bond(train,d67,d67_11,d67_24,1).
bond(train,d68,d68_1,d68_2,7).
bond(train,d68,d68_2,d68_3,7).
bond(train,d68,d68_3,d68_4,7).
bond(train,d68,d68_4,d68_5,7).
bond(train,d68,d68_5,d68_6,7).
bond(train,d68,d68_6,d68_1,7).
bond(train,d68,d68_1,d68_7,1).
bond(train,d68,d68_3,d68_8,1).
bond(train,d68,d68_6,d68_9,1).
bond(train,d68,d68_5,d68_10,1).
bond(train,d68,d68_2,d68_11,1).
bond(train,d68,d68_4,d68_12,1).
bond(train,d68,d68_10,d68_13,1).
bond(train,d68,d68_10,d68_14,1).
bond(train,d68,d68_11,d68_15,1).
bond(train,d68,d68_11,d68_16,1).
bond(train,d69,d69_1,d69_2,7).
bond(train,d69,d69_2,d69_3,7).
bond(train,d69,d69_3,d69_4,7).
bond(train,d69,d69_4,d69_5,7).
bond(train,d69,d69_5,d69_6,7).
bond(train,d69,d69_6,d69_1,7).
bond(train,d69,d69_1,d69_7,1).
bond(train,d69,d69_4,d69_8,1).
bond(train,d69,d69_6,d69_9,1).
bond(train,d69,d69_5,d69_10,1).
bond(train,d69,d69_2,d69_11,1).
bond(train,d69,d69_11,d69_12,1).
bond(train,d69,d69_11,d69_13,1).
bond(train,d69,d69_11,d69_14,1).
bond(train,d69,d69_3,d69_15,1).
bond(train,d69,d69_10,d69_16,1).
bond(train,d69,d69_10,d69_17,1).
bond(train,d7,d7_1,d7_2,7).
bond(train,d7,d7_2,d7_3,7).
bond(train,d7,d7_3,d7_4,7).
bond(train,d7,d7_4,d7_5,7).
bond(train,d7,d7_5,d7_6,7).
bond(train,d7,d7_6,d7_1,7).
bond(train,d7,d7_2,d7_7,1).
bond(train,d7,d7_4,d7_8,1).
bond(train,d7,d7_5,d7_9,1).
bond(train,d7,d7_10,d7_11,7).
bond(train,d7,d7_11,d7_12,7).
bond(train,d7,d7_12,d7_13,7).
bond(train,d7,d7_13,d7_14,7).
bond(train,d7,d7_14,d7_15,7).
bond(train,d7,d7_15,d7_10,7).
bond(train,d7,d7_10,d7_16,1).
bond(train,d7,d7_12,d7_17,1).
bond(train,d7,d7_13,d7_18,1).
bond(train,d7,d7_15,d7_19,1).
bond(train,d7,d7_20,d7_21,7).
bond(train,d7,d7_21,d7_22,7).
bond(train,d7,d7_22,d7_23,7).
bond(train,d7,d7_23,d7_24,7).
bond(train,d7,d7_24,d7_25,7).
bond(train,d7,d7_25,d7_20,7).
bond(train,d7,d7_21,d7_26,1).
bond(train,d7,d7_22,d7_27,1).
bond(train,d7,d7_24,d7_28,1).
bond(train,d7,d7_25,d7_29,1).
bond(train,d7,d7_3,d7_30,1).
bond(train,d7,d7_30,d7_14,1).
bond(train,d7,d7_30,d7_20,2).
bond(train,d7,d7_23,d7_31,2).
bond(train,d7,d7_1,d7_32,1).
bond(train,d7,d7_6,d7_33,1).
bond(train,d7,d7_11,d7_34,1).
bond(train,d7,d7_33,d7_35,1).
bond(train,d7,d7_33,d7_36,1).
bond(train,d7,d7_34,d7_37,1).
bond(train,d7,d7_34,d7_38,1).
bond(train,d7,d7_31,d7_39,1).
bond(train,d7,d7_31,d7_40,1).
bond(train,d70,d70_1,d70_2,7).
bond(train,d70,d70_2,d70_3,7).
bond(train,d70,d70_3,d70_4,7).
bond(train,d70,d70_4,d70_5,7).
bond(train,d70,d70_5,d70_6,7).
bond(train,d70,d70_6,d70_1,7).
bond(train,d70,d70_1,d70_7,1).
bond(train,d70,d70_3,d70_8,1).
bond(train,d70,d70_6,d70_9,1).
bond(train,d70,d70_5,d70_10,1).
bond(train,d70,d70_2,d70_11,1).
bond(train,d70,d70_11,d70_12,1).
bond(train,d70,d70_12,d70_13,1).
bond(train,d70,d70_12,d70_14,1).
bond(train,d70,d70_12,d70_15,1).
bond(train,d70,d70_4,d70_16,1).
bond(train,d70,d70_16,d70_17,1).
bond(train,d70,d70_17,d70_18,1).
bond(train,d70,d70_17,d70_19,1).
bond(train,d70,d70_17,d70_20,1).
bond(train,d70,d70_10,d70_21,1).
bond(train,d70,d70_10,d70_22,1).
bond(train,d71,d71_1,d71_2,7).
bond(train,d71,d71_2,d71_3,7).
bond(train,d71,d71_3,d71_4,7).
bond(train,d71,d71_4,d71_5,7).
bond(train,d71,d71_5,d71_6,7).
bond(train,d71,d71_6,d71_1,7).
bond(train,d71,d71_1,d71_7,1).
bond(train,d71,d71_3,d71_8,1).
bond(train,d71,d71_4,d71_9,1).
bond(train,d71,d71_6,d71_10,1).
bond(train,d71,d71_2,d71_11,1).
bond(train,d71,d71_5,d71_12,1).
bond(train,d71,d71_12,d71_13,1).
bond(train,d71,d71_13,d71_14,1).
bond(train,d71,d71_13,d71_15,1).
bond(train,d71,d71_15,d71_16,1).
bond(train,d71,d71_16,d71_17,1).
bond(train,d71,d71_16,d71_18,1).
bond(train,d71,d71_16,d71_19,1).
bond(train,d71,d71_13,d71_20,2).
bond(train,d71,d71_11,d71_21,2).
bond(train,d71,d71_11,d71_22,2).
bond(train,d71,d71_14,d71_23,1).
bond(train,d71,d71_23,d71_24,1).
bond(train,d71,d71_23,d71_25,1).
bond(train,d71,d71_23,d71_26,1).
bond(train,d72,d72_1,d72_2,7).
bond(train,d72,d72_2,d72_3,7).
bond(train,d72,d72_3,d72_4,7).
bond(train,d72,d72_4,d72_5,7).
bond(train,d72,d72_5,d72_6,7).
bond(train,d72,d72_6,d72_1,7).
bond(train,d72,d72_1,d72_7,1).
bond(train,d72,d72_2,d72_8,1).
bond(train,d72,d72_4,d72_9,1).
bond(train,d72,d72_5,d72_10,1).
bond(train,d72,d72_6,d72_11,1).
bond(train,d72,d72_11,d72_12,1).
bond(train,d72,d72_3,d72_13,1).
bond(train,d72,d72_13,d72_14,2).
bond(train,d72,d72_13,d72_15,2).
bond(train,d72,d72_10,d72_16,1).
bond(train,d72,d72_10,d72_17,1).
bond(train,d72,d72_11,d72_18,2).
bond(train,d72,d72_12,d72_19,1).
bond(train,d73,d73_1,d73_2,7).
bond(train,d73,d73_2,d73_3,7).
bond(train,d73,d73_3,d73_4,7).
bond(train,d73,d73_4,d73_5,7).
bond(train,d73,d73_5,d73_6,7).
bond(train,d73,d73_6,d73_1,7).
bond(train,d73,d73_1,d73_7,1).
bond(train,d73,d73_3,d73_8,1).
bond(train,d73,d73_6,d73_9,1).
bond(train,d73,d73_5,d73_10,1).
bond(train,d73,d73_4,d73_11,1).
bond(train,d73,d73_2,d73_12,1).
bond(train,d73,d73_10,d73_13,1).
bond(train,d73,d73_10,d73_14,1).
bond(train,d73,d73_11,d73_15,1).
bond(train,d73,d73_11,d73_16,1).
bond(train,d73,d73_12,d73_17,2).
bond(train,d73,d73_12,d73_18,2).
bond(train,d74,d74_1,d74_2,7).
bond(train,d74,d74_2,d74_3,7).
bond(train,d74,d74_3,d74_4,7).
bond(train,d74,d74_4,d74_5,7).
bond(train,d74,d74_5,d74_6,7).
bond(train,d74,d74_6,d74_1,7).
bond(train,d74,d74_1,d74_7,1).
bond(train,d74,d74_2,d74_8,1).
bond(train,d74,d74_3,d74_9,1).
bond(train,d74,d74_5,d74_10,1).
bond(train,d74,d74_6,d74_11,1).
bond(train,d74,d74_12,d74_13,7).
bond(train,d74,d74_13,d74_14,7).
bond(train,d74,d74_14,d74_15,7).
bond(train,d74,d74_15,d74_16,7).
bond(train,d74,d74_16,d74_17,7).
bond(train,d74,d74_17,d74_12,7).
bond(train,d74,d74_13,d74_18,1).
bond(train,d74,d74_14,d74_19,1).
bond(train,d74,d74_16,d74_20,1).
bond(train,d74,d74_17,d74_21,1).
bond(train,d74,d74_4,d74_22,1).
bond(train,d74,d74_22,d74_12,1).
bond(train,d74,d74_15,d74_23,1).
bond(train,d74,d74_22,d74_24,1).
bond(train,d74,d74_23,d74_25,1).
bond(train,d74,d74_23,d74_26,1).
bond(train,d75,d75_1,d75_2,7).
bond(train,d75,d75_2,d75_3,7).
bond(train,d75,d75_3,d75_4,7).
bond(train,d75,d75_4,d75_5,7).
bond(train,d75,d75_5,d75_6,7).
bond(train,d75,d75_6,d75_1,7).
bond(train,d75,d75_1,d75_7,1).
bond(train,d75,d75_2,d75_8,1).
bond(train,d75,d75_3,d75_9,1).
bond(train,d75,d75_6,d75_10,1).
bond(train,d75,d75_5,d75_11,1).
bond(train,d75,d75_11,d75_12,1).
bond(train,d75,d75_11,d75_13,1).
bond(train,d75,d75_11,d75_14,1).
bond(train,d75,d75_4,d75_15,1).
bond(train,d75,d75_10,d75_16,1).
bond(train,d75,d75_10,d75_17,1).
bond(train,d75,d75_15,d75_18,1).
bond(train,d75,d75_15,d75_19,1).
bond(train,d76,d76_1,d76_2,7).
bond(train,d76,d76_2,d76_3,7).
bond(train,d76,d76_3,d76_4,7).
bond(train,d76,d76_4,d76_5,7).
bond(train,d76,d76_5,d76_6,7).
bond(train,d76,d76_6,d76_1,7).
bond(train,d76,d76_1,d76_7,1).
bond(train,d76,d76_2,d76_8,1).
bond(train,d76,d76_4,d76_9,1).
bond(train,d76,d76_5,d76_10,1).
bond(train,d76,d76_6,d76_11,1).
bond(train,d76,d76_12,d76_13,7).
bond(train,d76,d76_13,d76_14,7).
bond(train,d76,d76_14,d76_15,7).
bond(train,d76,d76_15,d76_16,7).
bond(train,d76,d76_16,d76_17,7).
bond(train,d76,d76_17,d76_12,7).
bond(train,d76,d76_14,d76_18,1).
bond(train,d76,d76_15,d76_19,1).
bond(train,d76,d76_12,d76_20,7).
bond(train,d76,d76_20,d76_21,7).
bond(train,d76,d76_21,d76_22,7).
bond(train,d76,d76_22,d76_23,7).
bond(train,d76,d76_23,d76_13,7).
bond(train,d76,d76_21,d76_24,1).
bond(train,d76,d76_23,d76_25,1).
bond(train,d76,d76_3,d76_26,1).
bond(train,d76,d76_26,d76_27,2).
bond(train,d76,d76_27,d76_17,1).
bond(train,d76,d76_16,d76_28,1).
bond(train,d76,d76_20,d76_29,1).
bond(train,d76,d76_29,d76_30,2).
bond(train,d76,d76_29,d76_31,2).
bond(train,d76,d76_29,d76_32,1).
bond(train,d76,d76_32,d76_33,1).
bond(train,d76,d76_28,d76_34,1).
bond(train,d76,d76_22,d76_35,1).
bond(train,d76,d76_35,d76_36,1).
bond(train,d76,d76_35,d76_37,1).
bond(train,d76,d76_35,d76_38,1).
bond(train,d76,d76_36,d76_39,1).
bond(train,d76,d76_36,d76_40,2).
bond(train,d76,d76_36,d76_41,2).
bond(train,d76,d76_39,d76_42,1).
bond(train,d77,d77_1,d77_2,7).
bond(train,d77,d77_2,d77_3,7).
bond(train,d77,d77_3,d77_4,7).
bond(train,d77,d77_4,d77_5,7).
bond(train,d77,d77_5,d77_6,7).
bond(train,d77,d77_6,d77_1,7).
bond(train,d77,d77_1,d77_7,1).
bond(train,d77,d77_3,d77_8,1).
bond(train,d77,d77_4,d77_9,1).
bond(train,d77,d77_10,d77_11,7).
bond(train,d77,d77_11,d77_12,7).
bond(train,d77,d77_12,d77_13,7).
bond(train,d77,d77_13,d77_14,7).
bond(train,d77,d77_14,d77_15,7).
bond(train,d77,d77_15,d77_10,7).
bond(train,d77,d77_12,d77_16,1).
bond(train,d77,d77_13,d77_17,1).
bond(train,d77,d77_15,d77_18,1).
bond(train,d77,d77_14,d77_2,1).
bond(train,d77,d77_6,d77_19,1).
bond(train,d77,d77_10,d77_20,1).
bond(train,d77,d77_5,d77_21,1).
bond(train,d77,d77_21,d77_22,2).
bond(train,d77,d77_23,d77_24,1).
bond(train,d77,d77_23,d77_25,1).
bond(train,d77,d77_25,d77_26,1).
bond(train,d77,d77_24,d77_27,1).
bond(train,d77,d77_27,d77_28,1).
bond(train,d77,d77_27,d77_29,1).
bond(train,d77,d77_27,d77_30,1).
bond(train,d77,d77_24,d77_31,2).
bond(train,d77,d77_25,d77_32,2).
bond(train,d77,d77_22,d77_23,1).
bond(train,d77,d77_23,d77_33,1).
bond(train,d77,d77_26,d77_34,1).
bond(train,d77,d77_34,d77_35,7).
bond(train,d77,d77_35,d77_36,7).
bond(train,d77,d77_36,d77_37,7).
bond(train,d77,d77_37,d77_38,7).
bond(train,d77,d77_38,d77_39,7).
bond(train,d77,d77_39,d77_34,7).
bond(train,d77,d77_35,d77_40,1).
bond(train,d77,d77_36,d77_41,1).
bond(train,d77,d77_37,d77_42,1).
bond(train,d77,d77_38,d77_43,1).
bond(train,d77,d77_39,d77_44,1).
bond(train,d77,d77_26,d77_45,1).
bond(train,d77,d77_11,d77_46,1).
bond(train,d77,d77_46,d77_47,2).
bond(train,d77,d77_47,d77_48,1).
bond(train,d77,d77_48,d77_49,1).
bond(train,d77,d77_49,d77_50,2).
bond(train,d77,d77_49,d77_51,1).
bond(train,d77,d77_51,d77_52,1).
bond(train,d77,d77_51,d77_53,1).
bond(train,d77,d77_51,d77_54,1).
bond(train,d77,d77_48,d77_55,1).
bond(train,d77,d77_48,d77_56,1).
bond(train,d77,d77_55,d77_57,2).
bond(train,d77,d77_55,d77_58,1).
bond(train,d77,d77_58,d77_59,1).
bond(train,d77,d77_59,d77_60,7).
bond(train,d77,d77_60,d77_61,7).
bond(train,d77,d77_61,d77_62,7).
bond(train,d77,d77_62,d77_63,7).
bond(train,d77,d77_63,d77_64,7).
bond(train,d77,d77_64,d77_59,7).
bond(train,d77,d77_60,d77_65,1).
bond(train,d77,d77_61,d77_66,1).
bond(train,d77,d77_62,d77_67,1).
bond(train,d77,d77_63,d77_68,1).
bond(train,d77,d77_64,d77_69,1).
bond(train,d77,d77_58,d77_70,1).
bond(train,d78,d78_1,d78_2,7).
bond(train,d78,d78_2,d78_3,7).
bond(train,d78,d78_3,d78_4,7).
bond(train,d78,d78_4,d78_5,7).
bond(train,d78,d78_5,d78_6,7).
bond(train,d78,d78_6,d78_1,7).
bond(train,d78,d78_1,d78_7,1).
bond(train,d78,d78_4,d78_8,1).
bond(train,d78,d78_6,d78_9,1).
bond(train,d78,d78_5,d78_10,1).
bond(train,d78,d78_10,d78_11,1).
bond(train,d78,d78_11,d78_12,1).
bond(train,d78,d78_11,d78_13,1).
bond(train,d78,d78_11,d78_14,1).
bond(train,d78,d78_12,d78_15,1).
bond(train,d78,d78_12,d78_16,1).
bond(train,d78,d78_12,d78_17,1).
bond(train,d78,d78_10,d78_18,1).
bond(train,d78,d78_18,d78_19,1).
bond(train,d78,d78_18,d78_20,1).
bond(train,d78,d78_18,d78_21,1).
bond(train,d78,d78_19,d78_22,1).
bond(train,d78,d78_19,d78_23,1).
bond(train,d78,d78_19,d78_24,1).
bond(train,d78,d78_3,d78_25,1).
bond(train,d78,d78_2,d78_26,1).
bond(train,d78,d78_26,d78_27,1).
bond(train,d78,d78_27,d78_28,1).
bond(train,d78,d78_27,d78_29,1).
bond(train,d78,d78_27,d78_30,1).
bond(train,d78,d78_28,d78_31,1).
bond(train,d78,d78_28,d78_32,1).
bond(train,d78,d78_28,d78_33,1).
bond(train,d78,d78_25,d78_34,2).
bond(train,d78,d78_25,d78_35,2).
bond(train,d78,d78_26,d78_36,1).
bond(train,d78,d78_22,d78_37,1).
bond(train,d78,d78_15,d78_38,1).
bond(train,d78,d78_31,d78_39,1).
bond(train,d79,d79_1,d79_2,7).
bond(train,d79,d79_2,d79_3,7).
bond(train,d79,d79_3,d79_4,7).
bond(train,d79,d79_4,d79_5,7).
bond(train,d79,d79_5,d79_6,7).
bond(train,d79,d79_6,d79_1,7).
bond(train,d79,d79_1,d79_7,1).
bond(train,d79,d79_2,d79_8,1).
bond(train,d79,d79_5,d79_9,1).
bond(train,d79,d79_6,d79_10,1).
bond(train,d79,d79_3,d79_11,7).
bond(train,d79,d79_11,d79_12,7).
bond(train,d79,d79_12,d79_13,7).
bond(train,d79,d79_13,d79_14,7).
bond(train,d79,d79_14,d79_4,7).
bond(train,d79,d79_11,d79_15,1).
bond(train,d79,d79_12,d79_16,1).
bond(train,d79,d79_13,d79_17,1).
bond(train,d79,d79_14,d79_18,1).
bond(train,d79,d79_18,d79_19,1).
bond(train,d79,d79_19,d79_20,1).
bond(train,d79,d79_19,d79_21,1).
bond(train,d79,d79_19,d79_22,1).
bond(train,d79,d79_20,d79_23,1).
bond(train,d79,d79_20,d79_24,1).
bond(train,d79,d79_20,d79_25,1).
bond(train,d79,d79_18,d79_26,1).
bond(train,d79,d79_23,d79_27,1).
bond(train,d79,d79_23,d79_28,1).
bond(train,d8,d8_1,d8_2,7).
bond(train,d8,d8_2,d8_3,7).
bond(train,d8,d8_3,d8_4,7).
bond(train,d8,d8_4,d8_5,7).
bond(train,d8,d8_5,d8_6,7).
bond(train,d8,d8_6,d8_1,7).
bond(train,d8,d8_2,d8_7,1).
bond(train,d8,d8_5,d8_8,1).
bond(train,d8,d8_6,d8_9,1).
bond(train,d8,d8_10,d8_11,7).
bond(train,d8,d8_11,d8_12,7).
bond(train,d8,d8_12,d8_13,7).
bond(train,d8,d8_13,d8_14,7).
bond(train,d8,d8_14,d8_15,7).
bond(train,d8,d8_15,d8_10,7).
bond(train,d8,d8_10,d8_16,1).
bond(train,d8,d8_11,d8_17,1).
bond(train,d8,d8_13,d8_18,1).
bond(train,d8,d8_14,d8_19,1).
bond(train,d8,d8_3,d8_20,1).
bond(train,d8,d8_15,d8_21,1).
bond(train,d8,d8_20,d8_21,2).
bond(train,d8,d8_1,d8_22,1).
bond(train,d8,d8_22,d8_23,1).
bond(train,d8,d8_22,d8_24,1).
bond(train,d8,d8_22,d8_25,1).
bond(train,d8,d8_4,d8_26,1).
bond(train,d8,d8_26,d8_27,1).
bond(train,d8,d8_12,d8_28,1).
bond(train,d8,d8_28,d8_29,1).
bond(train,d8,d8_28,d8_30,1).
bond(train,d8,d8_29,d8_31,2).
bond(train,d8,d8_29,d8_32,1).
bond(train,d8,d8_32,d8_33,1).
bond(train,d8,d8_32,d8_34,1).
bond(train,d8,d8_32,d8_35,1).
bond(train,d80,d80_1,d80_2,7).
bond(train,d80,d80_2,d80_3,7).
bond(train,d80,d80_3,d80_4,7).
bond(train,d80,d80_4,d80_5,7).
bond(train,d80,d80_5,d80_6,7).
bond(train,d80,d80_6,d80_1,7).
bond(train,d80,d80_1,d80_7,1).
bond(train,d80,d80_2,d80_8,1).
bond(train,d80,d80_5,d80_9,1).
bond(train,d80,d80_6,d80_10,1).
bond(train,d80,d80_3,d80_11,7).
bond(train,d80,d80_11,d80_12,7).
bond(train,d80,d80_12,d80_13,7).
bond(train,d80,d80_13,d80_14,7).
bond(train,d80,d80_14,d80_4,7).
bond(train,d80,d80_11,d80_15,1).
bond(train,d80,d80_12,d80_16,1).
bond(train,d80,d80_13,d80_17,1).
bond(train,d80,d80_14,d80_18,1).
bond(train,d80,d80_18,d80_19,2).
bond(train,d80,d80_18,d80_20,2).
bond(train,d81,d81_1,d81_2,7).
bond(train,d81,d81_2,d81_3,7).
bond(train,d81,d81_3,d81_4,7).
bond(train,d81,d81_4,d81_5,7).
bond(train,d81,d81_5,d81_6,7).
bond(train,d81,d81_6,d81_1,7).
bond(train,d81,d81_1,d81_7,1).
bond(train,d81,d81_3,d81_8,1).
bond(train,d81,d81_4,d81_9,1).
bond(train,d81,d81_6,d81_10,1).
bond(train,d81,d81_5,d81_11,1).
bond(train,d81,d81_2,d81_12,1).
bond(train,d81,d81_11,d81_13,1).
bond(train,d81,d81_11,d81_14,1).
bond(train,d81,d81_12,d81_15,1).
bond(train,d81,d81_12,d81_16,1).
bond(train,d82,d82_1,d82_2,7).
bond(train,d82,d82_2,d82_3,7).
bond(train,d82,d82_3,d82_4,7).
bond(train,d82,d82_4,d82_5,7).
bond(train,d82,d82_5,d82_6,7).
bond(train,d82,d82_6,d82_1,7).
bond(train,d82,d82_2,d82_7,1).
bond(train,d82,d82_3,d82_8,1).
bond(train,d82,d82_5,d82_9,1).
bond(train,d82,d82_6,d82_10,1).
bond(train,d82,d82_1,d82_11,1).
bond(train,d82,d82_12,d82_13,7).
bond(train,d82,d82_13,d82_14,7).
bond(train,d82,d82_12,d82_15,7).
bond(train,d82,d82_15,d82_16,7).
bond(train,d82,d82_14,d82_16,7).
bond(train,d82,d82_15,d82_17,1).
bond(train,d82,d82_17,d82_18,1).
bond(train,d82,d82_17,d82_19,1).
bond(train,d82,d82_17,d82_20,1).
bond(train,d82,d82_16,d82_21,1).
bond(train,d82,d82_21,d82_22,1).
bond(train,d82,d82_21,d82_23,1).
bond(train,d82,d82_21,d82_24,1).
bond(train,d82,d82_11,d82_25,1).
bond(train,d82,d82_11,d82_26,1).
bond(train,d82,d82_4,d82_30,1).
bond(train,d82,d82_30,d82_29,1).
bond(train,d82,d82_29,d82_12,1).
bond(train,d82,d82_29,d82_31,1).
bond(train,d82,d82_30,d82_27,2).
bond(train,d82,d82_30,d82_28,2).
bond(train,d83,d83_1,d83_2,7).
bond(train,d83,d83_2,d83_3,7).
bond(train,d83,d83_3,d83_4,7).
bond(train,d83,d83_4,d83_5,7).
bond(train,d83,d83_5,d83_6,7).
bond(train,d83,d83_6,d83_1,7).
bond(train,d83,d83_3,d83_7,1).
bond(train,d83,d83_6,d83_8,1).
bond(train,d83,d83_5,d83_9,1).
bond(train,d83,d83_9,d83_10,1).
bond(train,d83,d83_9,d83_11,1).
bond(train,d83,d83_9,d83_12,1).
bond(train,d83,d83_4,d83_13,1).
bond(train,d83,d83_13,d83_14,1).
bond(train,d83,d83_13,d83_15,1).
bond(train,d83,d83_2,d83_16,1).
bond(train,d83,d83_1,d83_17,1).
bond(train,d83,d83_17,d83_18,1).
bond(train,d83,d83_17,d83_19,1).
bond(train,d84,d84a_1,d84a_2,1).
bond(train,d84,d84a_2,d84a_3,1).
bond(train,d84,d84a_2,d84a_4,1).
bond(train,d84,d84a_2,d84a_5,1).
bond(train,d84,d84a_3,d84a_6,1).
bond(train,d84,d84a_3,d84a_7,1).
bond(train,d84,d84a_3,d84a_8,1).
bond(train,d84,d84a_6,d84a_9,1).
bond(train,d84,d84a_9,d84a_10,1).
bond(train,d84,d84a_10,d84a_11,1).
bond(train,d84,d84a_10,d84a_12,1).
bond(train,d84,d84a_10,d84a_13,1).
bond(train,d84,d84a_11,d84a_14,1).
bond(train,d84,d84a_14,d84a_15,1).
bond(train,d84,d84a_15,d84a_16,1).
bond(train,d84,d84a_16,d84a_17,1).
bond(train,d84,d84a_16,d84a_18,1).
bond(train,d84,d84a_16,d84a_19,1).
bond(train,d84,d84a_17,d84a_20,1).
bond(train,d84,d84a_17,d84a_21,1).
bond(train,d84,d84a_17,d84a_22,1).
bond(train,d84,d84a_1,d84a_23,1).
bond(train,d84,d84a_1,d84a_24,1).
bond(train,d84,d84a_1,d84a_25,1).
bond(train,d84,d84a_6,d84a_26,1).
bond(train,d84,d84a_6,d84a_27,1).
bond(train,d84,d84a_9,d84a_28,1).
bond(train,d84,d84a_9,d84a_29,1).
bond(train,d84,d84a_14,d84a_30,1).
bond(train,d84,d84a_14,d84a_31,1).
bond(train,d84,d84a_15,d84a_32,1).
bond(train,d84,d84a_15,d84a_33,1).
bond(train,d84,d84a_11,d84a_34,1).
bond(train,d84,d84a_11,d84a_35,1).
bond(train,d84,d84a_20,d84a_36,1).
bond(train,d84,d84a_20,d84a_37,1).
bond(train,d84,d84a_20,d84a_38,1).
bond(train,d84,d84b_1,d84b_2,1).
bond(train,d84,d84b_2,d84b_3,1).
bond(train,d84,d84b_2,d84b_4,1).
bond(train,d84,d84b_2,d84b_5,1).
bond(train,d84,d84b_1,d84b_6,1).
bond(train,d84,d84b_1,d84b_7,1).
bond(train,d84,d84b_7,d84b_8,1).
bond(train,d84,d84b_7,d84b_9,1).
bond(train,d84,d84b_7,d84b_10,1).
bond(train,d84,d84b_1,d84b_11,1).
bond(train,d84,d84b_11,d84b_12,1).
bond(train,d84,d84b_11,d84b_13,1).
bond(train,d84,d84b_11,d84b_14,1).
bond(train,d84,d84b_3,d84b_15,1).
bond(train,d84,d84b_3,d84b_16,1).
bond(train,d84,d84b_3,d84b_17,1).
bond(train,d84,d84b_6,d84b_18,1).
bond(train,d84,d84b_6,d84b_19,1).
bond(train,d84,d84b_6,d84b_20,1).
bond(train,d84,d84b_18,d84b_21,1).
bond(train,d84,d84b_18,d84b_22,1).
bond(train,d84,d84b_18,d84b_23,1).
bond(train,d84,d84b_4,d84b_24,1).
bond(train,d84,d84b_4,d84b_25,1).
bond(train,d84,d84b_4,d84b_26,1).
bond(train,d84,d84b_5,d84b_27,1).
bond(train,d84,d84b_5,d84b_28,1).
bond(train,d84,d84b_5,d84b_29,1).
bond(train,d84,d84b_16,d84b_30,1).
bond(train,d84,d84b_16,d84b_31,1).
bond(train,d84,d84b_16,d84b_32,1).
bond(train,d84,d84b_17,d84b_33,1).
bond(train,d84,d84b_17,d84b_34,1).
bond(train,d84,d84b_17,d84b_35,1).
bond(train,d84,d84b_15,d84b_36,1).
bond(train,d84,d84b_15,d84b_37,1).
bond(train,d84,d84b_15,d84b_38,1).
bond(train,d85_1,d85_1_1,d85_1_2,7).
bond(train,d85_1,d85_1_2,d85_1_3,7).
bond(train,d85_1,d85_1_3,d85_1_4,7).
bond(train,d85_1,d85_1_4,d85_1_5,7).
bond(train,d85_1,d85_1_5,d85_1_6,7).
bond(train,d85_1,d85_1_6,d85_1_1,7).
bond(train,d85_1,d85_1_5,d85_1_7,1).
bond(train,d85_1,d85_1_8,d85_1_9,7).
bond(train,d85_1,d85_1_9,d85_1_10,7).
bond(train,d85_1,d85_1_10,d85_1_11,7).
bond(train,d85_1,d85_1_11,d85_1_12,7).
bond(train,d85_1,d85_1_12,d85_1_13,7).
bond(train,d85_1,d85_1_13,d85_1_8,7).
bond(train,d85_1,d85_1_9,d85_1_14,1).
bond(train,d85_1,d85_1_4,d85_1_15,1).
bond(train,d85_1,d85_1_15,d85_1_13,1).
bond(train,d85_1,d85_1_3,d85_1_16,1).
bond(train,d85_1,d85_1_16,d85_1_8,1).
bond(train,d85_1,d85_1_12,d85_1_17,1).
bond(train,d85_1,d85_1_11,d85_1_18,1).
bond(train,d85_1,d85_1_10,d85_1_19,1).
bond(train,d85_1,d85_1_2,d85_1_20,1).
bond(train,d85_1,d85_1_1,d85_1_21,1).
bond(train,d85_1,d85_1_6,d85_1_22,1).
bond(train,d85_2,d85_2_1,d85_2_2,7).
bond(train,d85_2,d85_2_2,d85_2_3,7).
bond(train,d85_2,d85_2_3,d85_2_4,7).
bond(train,d85_2,d85_2_4,d85_2_5,7).
bond(train,d85_2,d85_2_5,d85_2_6,7).
bond(train,d85_2,d85_2_6,d85_2_1,7).
bond(train,d85_2,d85_2_2,d85_2_7,1).
bond(train,d85_2,d85_2_8,d85_2_9,7).
bond(train,d85_2,d85_2_9,d85_2_10,7).
bond(train,d85_2,d85_2_10,d85_2_11,7).
bond(train,d85_2,d85_2_11,d85_2_12,7).
bond(train,d85_2,d85_2_12,d85_2_13,7).
bond(train,d85_2,d85_2_13,d85_2_8,7).
bond(train,d85_2,d85_2_9,d85_2_14,1).
bond(train,d85_2,d85_2_4,d85_2_15,1).
bond(train,d85_2,d85_2_15,d85_2_13,1).
bond(train,d85_2,d85_2_3,d85_2_16,1).
bond(train,d85_2,d85_2_16,d85_2_8,1).
bond(train,d85_2,d85_2_12,d85_2_17,1).
bond(train,d85_2,d85_2_11,d85_2_18,1).
bond(train,d85_2,d85_2_10,d85_2_19,1).
bond(train,d85_2,d85_2_5,d85_2_20,1).
bond(train,d85_2,d85_2_6,d85_2_21,1).
bond(train,d85_2,d85_2_1,d85_2_22,1).
bond(train,d86,d86a_1,d86a_2,7).
bond(train,d86,d86a_2,d86a_3,7).
bond(train,d86,d86a_3,d86a_4,7).
bond(train,d86,d86a_4,d86a_5,7).
bond(train,d86,d86a_5,d86a_6,7).
bond(train,d86,d86a_6,d86a_1,7).
bond(train,d86,d86a_2,d86a_7,1).
bond(train,d86,d86a_3,d86a_8,1).
bond(train,d86,d86a_9,d86a_10,7).
bond(train,d86,d86a_10,d86a_11,7).
bond(train,d86,d86a_11,d86a_12,7).
bond(train,d86,d86a_12,d86a_13,7).
bond(train,d86,d86a_13,d86a_14,7).
bond(train,d86,d86a_14,d86a_9,7).
bond(train,d86,d86a_10,d86a_15,1).
bond(train,d86,d86a_4,d86a_9,1).
bond(train,d86,d86a_5,d86a_16,1).
bond(train,d86,d86a_6,d86a_17,1).
bond(train,d86,d86a_1,d86a_18,1).
bond(train,d86,d86a_14,d86a_19,1).
bond(train,d86,d86a_13,d86a_20,1).
bond(train,d86,d86a_12,d86a_21,1).
bond(train,d86,d86a_11,d86a_22,1).
bond(train,d86,d86b_1,d86b_2,7).
bond(train,d86,d86b_2,d86b_3,7).
bond(train,d86,d86b_3,d86b_4,7).
bond(train,d86,d86b_4,d86b_5,7).
bond(train,d86,d86b_5,d86b_6,7).
bond(train,d86,d86b_6,d86b_1,7).
bond(train,d86,d86b_2,d86b_7,1).
bond(train,d86,d86b_8,d86b_9,7).
bond(train,d86,d86b_9,d86b_10,7).
bond(train,d86,d86b_10,d86b_11,7).
bond(train,d86,d86b_11,d86b_12,7).
bond(train,d86,d86b_12,d86b_13,7).
bond(train,d86,d86b_13,d86b_8,7).
bond(train,d86,d86b_13,d86b_14,1).
bond(train,d86,d86b_4,d86b_8,1).
bond(train,d86,d86b_5,d86b_15,1).
bond(train,d86,d86b_9,d86b_16,1).
bond(train,d86,d86b_6,d86b_17,1).
bond(train,d86,d86b_1,d86b_18,1).
bond(train,d86,d86b_3,d86b_19,1).
bond(train,d86,d86b_11,d86b_20,1).
bond(train,d86,d86b_10,d86b_21,1).
bond(train,d86,d86b_12,d86b_22,1).
bond(train,d87,d87_1,d87_2,7).
bond(train,d87,d87_2,d87_3,7).
bond(train,d87,d87_3,d87_4,7).
bond(train,d87,d87_4,d87_5,7).
bond(train,d87,d87_5,d87_6,7).
bond(train,d87,d87_6,d87_1,7).
bond(train,d87,d87_2,d87_7,1).
bond(train,d87,d87_5,d87_8,1).
bond(train,d87,d87_9,d87_10,7).
bond(train,d87,d87_10,d87_11,7).
bond(train,d87,d87_11,d87_12,7).
bond(train,d87,d87_12,d87_13,7).
bond(train,d87,d87_13,d87_14,7).
bond(train,d87,d87_14,d87_9,7).
bond(train,d87,d87_10,d87_15,1).
bond(train,d87,d87_13,d87_16,1).
bond(train,d87,d87_4,d87_17,1).
bond(train,d87,d87_17,d87_14,1).
bond(train,d87,d87_3,d87_18,1).
bond(train,d87,d87_18,d87_9,1).
bond(train,d87,d87_6,d87_19,1).
bond(train,d87,d87_1,d87_20,1).
bond(train,d87,d87_12,d87_21,1).
bond(train,d87,d87_11,d87_22,1).
bond(train,d88,d88_1,d88_2,7).
bond(train,d88,d88_2,d88_3,7).
bond(train,d88,d88_3,d88_4,7).
bond(train,d88,d88_4,d88_5,7).
bond(train,d88,d88_5,d88_6,7).
bond(train,d88,d88_6,d88_1,7).
bond(train,d88,d88_1,d88_7,1).
bond(train,d88,d88_3,d88_8,1).
bond(train,d88,d88_5,d88_9,1).
bond(train,d88,d88_4,d88_10,1).
bond(train,d88,d88_6,d88_11,1).
bond(train,d88,d88_2,d88_12,1).
bond(train,d88,d88_9,d88_13,1).
bond(train,d89,d89_1,d89_2,2).
bond(train,d89,d89_2,d89_3,1).
bond(train,d89,d89_3,d89_4,1).
bond(train,d89,d89_1,d89_5,1).
bond(train,d89,d89_5,d89_6,1).
bond(train,d89,d89_6,d89_4,1).
bond(train,d89,d89_3,d89_7,1).
bond(train,d89,d89_3,d89_8,1).
bond(train,d89,d89_7,d89_5,1).
bond(train,d89,d89_5,d89_9,1).
bond(train,d89,d89_7,d89_10,1).
bond(train,d89,d89_7,d89_11,1).
bond(train,d89,d89_1,d89_12,1).
bond(train,d89,d89_2,d89_13,1).
bond(train,d89,d89_6,d89_14,1).
bond(train,d89,d89_6,d89_15,1).
bond(train,d89,d89_4,d89_16,1).
bond(train,d89,d89_4,d89_17,1).
bond(train,d89,d89_16,d89_18,1).
bond(train,d89,d89_14,d89_19,2).
bond(train,d89,d89_16,d89_20,2).
bond(train,d89,d89_18,d89_21,1).
bond(train,d89,d89_14,d89_22,1).
bond(train,d89,d89_22,d89_23,1).
bond(train,d9,d9_1,d9_2,7).
bond(train,d9,d9_2,d9_3,7).
bond(train,d9,d9_3,d9_4,7).
bond(train,d9,d9_4,d9_5,7).
bond(train,d9,d9_5,d9_6,7).
bond(train,d9,d9_6,d9_1,7).
bond(train,d9,d9_2,d9_7,1).
bond(train,d9,d9_3,d9_8,1).
bond(train,d9,d9_6,d9_9,1).
bond(train,d9,d9_1,d9_10,1).
bond(train,d9,d9_10,d9_11,1).
bond(train,d9,d9_10,d9_12,1).
bond(train,d9,d9_10,d9_13,1).
bond(train,d9,d9_5,d9_14,1).
bond(train,d9,d9_4,d9_15,1).
bond(train,d9,d9_14,d9_16,1).
bond(train,d9,d9_14,d9_17,1).
bond(train,d9,d9_15,d9_18,1).
bond(train,d9,d9_18,d9_19,1).
bond(train,d9,d9_18,d9_20,1).
bond(train,d9,d9_18,d9_21,1).
bond(train,d90,d90_1,d90_2,7).
bond(train,d90,d90_2,d90_3,7).
bond(train,d90,d90_3,d90_4,7).
bond(train,d90,d90_4,d90_5,7).
bond(train,d90,d90_5,d90_6,7).
bond(train,d90,d90_6,d90_1,7).
bond(train,d90,d90_1,d90_7,1).
bond(train,d90,d90_3,d90_8,1).
bond(train,d90,d90_4,d90_9,1).
bond(train,d90,d90_6,d90_10,1).
bond(train,d90,d90_5,d90_11,1).
bond(train,d90,d90_2,d90_12,1).
bond(train,d91,d91_1,d91_2,1).
bond(train,d91,d91_2,d91_3,1).
bond(train,d91,d91_2,d91_4,2).
bond(train,d91,d91_4,d91_5,1).
bond(train,d91,d91_4,d91_6,1).
bond(train,d92,d92_1,d92_2,1).
bond(train,d92,d92_1,d92_3,1).
bond(train,d92,d92_1,d92_4,1).
bond(train,d92,d92_1,d92_5,1).
bond(train,d92,d92_2,d92_6,1).
bond(train,d92,d92_2,d92_7,1).
bond(train,d92,d92_2,d92_8,1).
bond(train,d93,d93_1,d93_2,7).
bond(train,d93,d93_2,d93_3,7).
bond(train,d93,d93_3,d93_4,7).
bond(train,d93,d93_4,d93_5,7).
bond(train,d93,d93_5,d93_6,7).
bond(train,d93,d93_6,d93_1,7).
bond(train,d93,d93_5,d93_7,1).
bond(train,d93,d93_4,d93_8,1).
bond(train,d93,d93_3,d93_9,1).
bond(train,d93,d93_2,d93_10,1).
bond(train,d93,d93_1,d93_11,1).
bond(train,d93,d93_6,d93_12,1).
bond(train,d93,d93_7,d93_13,1).
bond(train,d94,d94_1,d94_2,1).
bond(train,d94,d94_2,d94_3,1).
bond(train,d94,d94_2,d94_4,1).
bond(train,d94,d94_2,d94_5,1).
bond(train,d94,d94_1,d94_6,1).
bond(train,d94,d94_1,d94_7,1).
bond(train,d94,d94_1,d94_8,1).
bond(train,d95,d95_1,d95_2,7).
bond(train,d95,d95_2,d95_3,7).
bond(train,d95,d95_3,d95_4,7).
bond(train,d95,d95_4,d95_5,7).
bond(train,d95,d95_5,d95_6,7).
bond(train,d95,d95_6,d95_1,7).
bond(train,d95,d95_1,d95_7,1).
bond(train,d95,d95_3,d95_8,1).
bond(train,d95,d95_4,d95_9,1).
bond(train,d95,d95_6,d95_10,1).
bond(train,d95,d95_2,d95_11,1).
bond(train,d95,d95_5,d95_12,1).
bond(train,d95,d95_12,d95_13,1).
bond(train,d95,d95_13,d95_14,1).
bond(train,d95,d95_14,d95_15,1).
bond(train,d95,d95_15,d95_16,1).
bond(train,d95,d95_15,d95_17,1).
bond(train,d95,d95_15,d95_18,1).
bond(train,d95,d95_14,d95_19,1).
bond(train,d95,d95_19,d95_20,1).
bond(train,d95,d95_19,d95_21,1).
bond(train,d95,d95_19,d95_22,1).
bond(train,d95,d95_13,d95_23,2).
bond(train,d95,d95_12,d95_24,1).
bond(train,d96,d96_1,d96_2,2).
bond(train,d96,d96_2,d96_3,1).
bond(train,d96,d96_3,d96_4,1).
bond(train,d96,d96_4,d96_5,1).
bond(train,d96,d96_5,d96_6,1).
bond(train,d96,d96_6,d96_1,1).
bond(train,d96,d96_4,d96_7,1).
bond(train,d96,d96_7,d96_8,1).
bond(train,d96,d96_8,d96_9,1).
bond(train,d96,d96_9,d96_5,1).
bond(train,d96,d96_5,d96_10,1).
bond(train,d96,d96_4,d96_11,1).
bond(train,d96,d96_9,d96_12,1).
bond(train,d96,d96_9,d96_13,1).
bond(train,d96,d96_6,d96_14,1).
bond(train,d96,d96_14,d96_3,1).
bond(train,d96,d96_14,d96_15,1).
bond(train,d96,d96_14,d96_16,1).
bond(train,d96,d96_8,d96_17,1).
bond(train,d96,d96_8,d96_18,1).
bond(train,d96,d96_7,d96_19,1).
bond(train,d96,d96_7,d96_20,1).
bond(train,d96,d96_3,d96_21,1).
bond(train,d96,d96_2,d96_22,1).
bond(train,d96,d96_1,d96_23,1).
bond(train,d96,d96_6,d96_24,1).
bond(train,d97,d97_1,d97_2,7).
bond(train,d97,d97_2,d97_3,7).
bond(train,d97,d97_3,d97_4,7).
bond(train,d97,d97_4,d97_5,7).
bond(train,d97,d97_5,d97_6,7).
bond(train,d97,d97_6,d97_1,7).
bond(train,d97,d97_2,d97_7,1).
bond(train,d97,d97_3,d97_8,1).
bond(train,d97,d97_5,d97_9,1).
bond(train,d97,d97_6,d97_10,1).
bond(train,d97,d97_11,d97_12,7).
bond(train,d97,d97_12,d97_13,7).
bond(train,d97,d97_13,d97_14,7).
bond(train,d97,d97_14,d97_15,7).
bond(train,d97,d97_15,d97_16,7).
bond(train,d97,d97_16,d97_11,7).
bond(train,d97,d97_12,d97_17,1).
bond(train,d97,d97_13,d97_18,1).
bond(train,d97,d97_15,d97_19,1).
bond(train,d97,d97_16,d97_20,1).
bond(train,d97,d97_4,d97_21,1).
bond(train,d97,d97_21,d97_11,1).
bond(train,d97,d97_21,d97_22,1).
bond(train,d97,d97_21,d97_23,1).
bond(train,d97,d97_23,d97_24,1).
bond(train,d97,d97_23,d97_25,2).
bond(train,d97,d97_24,d97_26,1).
bond(train,d97,d97_26,d97_27,1).
bond(train,d97,d97_26,d97_28,1).
bond(train,d97,d97_26,d97_29,1).
bond(train,d97,d97_27,d97_30,1).
bond(train,d97,d97_27,d97_31,1).
bond(train,d97,d97_27,d97_32,1).
bond(train,d97,d97_22,d97_33,1).
bond(train,d97,d97_1,d97_34,1).
bond(train,d97,d97_14,d97_35,1).
bond(train,d98,d98_1,d98_2,7).
bond(train,d98,d98_2,d98_3,7).
bond(train,d98,d98_3,d98_4,7).
bond(train,d98,d98_4,d98_5,7).
bond(train,d98,d98_5,d98_6,7).
bond(train,d98,d98_6,d98_1,7).
bond(train,d98,d98_5,d98_7,1).
bond(train,d98,d98_4,d98_8,1).
bond(train,d98,d98_3,d98_9,1).
bond(train,d98,d98_2,d98_10,1).
bond(train,d98,d98_1,d98_11,1).
bond(train,d98,d98_6,d98_12,1).
bond(train,d98,d98_8,d98_13,3).
bond(train,d98,d98_10,d98_14,3).
bond(train,d99,d99_1,d99_2,7).
bond(train,d99,d99_2,d99_3,7).
bond(train,d99,d99_3,d99_4,7).
bond(train,d99,d99_4,d99_5,7).
bond(train,d99,d99_5,d99_6,7).
bond(train,d99,d99_6,d99_1,7).
bond(train,d99,d99_7,d99_8,7).
bond(train,d99,d99_8,d99_9,7).
bond(train,d99,d99_9,d99_10,7).
bond(train,d99,d99_10,d99_11,7).
bond(train,d99,d99_11,d99_12,7).
bond(train,d99,d99_12,d99_7,7).
bond(train,d99,d99_4,d99_13,1).
bond(train,d99,d99_13,d99_7,1).
bond(train,d99,d99_5,d99_14,1).
bond(train,d99,d99_6,d99_15,1).
bond(train,d99,d99_1,d99_16,1).
bond(train,d99,d99_2,d99_17,1).
bond(train,d99,d99_3,d99_18,1).
bond(train,d99,d99_8,d99_19,1).
bond(train,d99,d99_11,d99_20,1).
bond(train,d99,d99_10,d99_21,1).
bond(train,d99,d99_9,d99_22,1).
bond(train,d99,d99_12,d99_23,1).
has_property(train,d1,salmonella,p).
has_property(train,d1,salmonella_n,p).
has_property(train,d1,cytogen_ca,p).
has_property(train,d1,cytogen_sce,p).
has_property(train,d2,salmonella,p).
has_property(train,d2,cytogen_ca,n).
has_property(train,d2,cytogen_sce,p).
has_property(train,d3,salmonella,p).
has_property(train,d3,cytogen_ca,p).
has_property(train,d3,cytogen_sce,p).
has_property(train,d4,salmonella_n,p).
has_property(train,d5,salmonella,p).
has_property(train,d5,salmonella_n,p).
has_property(train,d5,cytogen_ca,p).
has_property(train,d5,cytogen_sce,p).
has_property(train,d6,salmonella,p).
has_property(train,d6,salmonella_n,p).
has_property(train,d6,cytogen_ca,p).
has_property(train,d6,cytogen_sce,p).
has_property(train,d7,salmonella,p).
has_property(train,d7,cytogen_ca,n).
has_property(train,d8,salmonella,p).
has_property(train,d8,mouse_lymph,p).
has_property(train,d8,cytogen_ca,n).
has_property(train,d8,cytogen_sce,p).
has_property(train,d8,chromaberr,n).
has_property(train,d8,chromex,p).
has_property(train,d9,salmonella,p).
has_property(train,d9,salmonella_n,p).
has_property(train,d9,cytogen_ca,p).
has_property(train,d9,cytogen_sce,p).
has_property(train,d10,salmonella,p).
has_property(train,d10,salmonella_n,p).
has_property(train,d10,cytogen_ca,p).
has_property(train,d10,cytogen_sce,p).
has_property(train,d11,salmonella,p).
has_property(train,d11,salmonella_n,p).
has_property(train,d11,mouse_lymph,p).
has_property(train,d11,cytogen_ca,p).
has_property(train,d11,cytogen_sce,p).
has_property(train,d12,salmonella,p).
has_property(train,d12,cytogen_ca,p).
has_property(train,d12,cytogen_sce,p).
has_property(train,d12,micronuc_m,n).
has_property(train,d12,micronuc_f,n).
has_property(train,d13,salmonella,p).
has_property(train,d13,mouse_lymph,p).
has_property(train,d13,drosophila_slrl,p).
has_property(train,d13,drosophila_rt,n).
has_property(train,d13,cytogen_ca,p).
has_property(train,d13,cytogen_sce,p).
has_property(train,d13,chromaberr,p).
has_property(train,d14,salmonella,p).
has_property(train,d14,salmonella_n,p).
has_property(train,d14,cytogen_ca,p).
has_property(train,d14,cytogen_sce,p).
has_property(train,d15,salmonella,p).
has_property(train,d15,mouse_lymph,p).
has_property(train,d15,drosophila_slrl,p).
has_property(train,d15,drosophila_rt,n).
has_property(train,d15,cytogen_ca,p).
has_property(train,d15,cytogen_sce,p).
has_property(train,d16,salmonella,p).
has_property(train,d16,cytogen_ca,p).
has_property(train,d16,cytogen_sce,p).
has_property(train,d17,salmonella,p).
has_property(train,d17,salmonella_n,p).
has_property(train,d17,cytogen_ca,p).
has_property(train,d17,cytogen_sce,p).
has_property(train,d18,salmonella,p).
has_property(train,d18,cytogen_ca,n).
has_property(train,d18,cytogen_sce,p).
has_property(train,d19,salmonella,p).
has_property(train,d19,salmonella_n,p).
has_property(train,d19,cytogen_ca,p).
has_property(train,d19,cytogen_sce,p).
has_property(train,d20,salmonella,p).
has_property(train,d20,cytogen_ca,n).
has_property(train,d20,cytogen_sce,p).
has_property(train,d21,salmonella_n,p).
has_property(train,d21,mouse_lymph,p).
has_property(train,d21,cytogen_ca,n).
has_property(train,d21,cytogen_sce,n).
has_property(train,d22,salmonella,p).
has_property(train,d22,mouse_lymph,p).
has_property(train,d22,cytogen_ca,p).
has_property(train,d22,cytogen_sce,p).
has_property(train,d23_1,salmonella,p).
has_property(train,d23_1,mouse_lymph,p).
has_property(train,d23_1,cytogen_ca,p).
has_property(train,d23_1,cytogen_sce,p).
has_property(train,d23_1,chromaberr,p).
has_property(train,d23_1,chromex,n).
has_property(train,d23_2,mouse_lymph,p).
has_property(train,d23_2,cytogen_ca,p).
has_property(train,d23_2,cytogen_sce,p).
has_property(train,d25,salmonella,n).
has_property(train,d25,cytogen_sce,p).
has_property(train,d25,chromex,p).
has_property(train,d26,salmonella,p).
has_property(train,d26,cytogen_ca,p).
has_property(train,d26,cytogen_sce,p).
has_property(train,d27,salmonella,p).
has_property(train,d27,salmonella,p).
has_property(train,d27,mouse_lymph,p).
has_property(train,d27,cytogen_ca,p).
has_property(train,d27,cytogen_sce,p).
has_property(train,d28,salmonella,p).
has_property(train,d28,mouse_lymph,p).
has_property(train,d28,cytogen_ca,p).
has_property(train,d28,cytogen_sce,p).
has_property(train,d29,salmonella,p).
has_property(train,d29,drosophila_slrl,p).
has_property(train,d29,drosophila_rt,p).
has_property(train,d29,cytogen_ca,n).
has_property(train,d29,cytogen_sce,p).
has_property(train,d29,chromaberr,n).
has_property(train,d31,mouse_lymph,p).
has_property(train,d32,salmonella,p).
has_property(train,d32,drosophila_slrl,p).
has_property(train,d32,drosophila_rt,n).
has_property(train,d32,cytogen_ca,p).
has_property(train,d32,cytogen_sce,p).
has_property(train,d33,salmonella_n,n).
has_property(train,d34,salmonella,p).
has_property(train,d34,cytogen_sce,p).
has_property(train,d35,salmonella,n).
has_property(train,d35,mouse_lymph,n).
has_property(train,d35,cytogen_ca,n).
has_property(train,d35,cytogen_sce,n).
has_property(train,d36,salmonella,p).
has_property(train,d36,mouse_lymph,n).
has_property(train,d36,cytogen_ca,n).
has_property(train,d36,cytogen_sce,n).
has_property(train,d36,chromaberr,n).
has_property(train,d36,chromex,p).
has_property(train,d37,salmonella,p).
has_property(train,d37,salmonella_n,p).
has_property(train,d37,cytogen_ca,p).
has_property(train,d37,cytogen_sce,p).
has_property(train,d38,salmonella,p).
has_property(train,d38,salmonella_n,p).
has_property(train,d38,mouse_lymph,p).
has_property(train,d38,drosophila_slrl,p).
has_property(train,d38,drosophila_rt,n).
has_property(train,d38,cytogen_ca,n).
has_property(train,d38,cytogen_sce,p).
has_property(train,d39,salmonella,p).
has_property(train,d39,salmonella_n,p).
has_property(train,d39,cytogen_ca,p).
has_property(train,d39,cytogen_sce,p).
has_property(train,d40,salmonella,n).
has_property(train,d40,salmonella_n,n).
has_property(train,d40,mouse_lymph,n).
has_property(train,d40,cytogen_ca,p).
has_property(train,d41,salmonella,p).
has_property(train,d41,cytogen_ca,n).
has_property(train,d41,cytogen_sce,n).
has_property(train,d42,salmonella,n).
has_property(train,d42,mouse_lymph,p).
has_property(train,d42,cytogen_ca,p).
has_property(train,d42,cytogen_sce,p).
has_property(train,d43,salmonella,p).
has_property(train,d43,mouse_lymph,p).
has_property(train,d43,cytogen_ca,p).
has_property(train,d43,cytogen_sce,p).
has_property(train,d44,salmonella,p).
has_property(train,d44,salmonella_n,n).
has_property(train,d44,cytogen_ca,p).
has_property(train,d44,cytogen_sce,p).
has_property(train,d45,salmonella,p).
has_property(train,d45,mouse_lymph,p).
has_property(train,d45,cytogen_ca,p).
has_property(train,d45,cytogen_sce,p).
has_property(train,d45,chromaberr,p).
has_property(train,d45,chromex,p).
has_property(train,d46,salmonella,p).
has_property(train,d46,salmonella_n,p).
has_property(train,d46,cytogen_ca,p).
has_property(train,d46,cytogen_sce,p).
has_property(train,d47,salmonella,p).
has_property(train,d47,mouse_lymph,p).
has_property(train,d47,cytogen_ca,n).
has_property(train,d47,cytogen_sce,p).
has_property(train,d47,chromaberr,n).
has_property(train,d47,chromex,p).
has_property(train,d48,salmonella,p).
has_property(train,d48,salmonella_n,p).
has_property(train,d48,cytogen_ca,n).
has_property(train,d48,cytogen_sce,p).
has_property(train,d49,salmonella,p).
has_property(train,d49,salmonella_n,p).
has_property(train,d49,mouse_lymph,p).
has_property(train,d49,cytogen_ca,p).
has_property(train,d49,cytogen_sce,p).
has_property(train,d50,salmonella,p).
has_property(train,d50,salmonella_n,p).
has_property(train,d50,mouse_lymph,p).
has_property(train,d50,cytogen_ca,p).
has_property(train,d50,cytogen_sce,p).
has_property(train,d51,salmonella,p).
has_property(train,d51,salmonella_n,p).
has_property(train,d51,mouse_lymph,p).
has_property(train,d51,cytogen_ca,p).
has_property(train,d51,cytogen_sce,p).
has_property(train,d52,salmonella,p).
has_property(train,d52,mouse_lymph,p).
has_property(train,d52,cytogen_ca,p).
has_property(train,d52,cytogen_sce,n).
has_property(train,d52,chromaberr,n).
has_property(train,d53,salmonella,p).
has_property(train,d53,cytogen_ca,p).
has_property(train,d53,cytogen_sce,p).
has_property(train,d54,salmonella,p).
has_property(train,d54,cytogen_ca,n).
has_property(train,d54,cytogen_sce,n).
has_property(train,d55,salmonella,p).
has_property(train,d55,salmonella_n,p).
has_property(train,d55,mouse_lymph,p).
has_property(train,d55,cytogen_ca,p).
has_property(train,d55,cytogen_sce,p).
has_property(train,d56,salmonella,p).
has_property(train,d56,mouse_lymph,p).
has_property(train,d56,cytogen_ca,p).
has_property(train,d56,cytogen_sce,p).
has_property(train,d57,salmonella,p).
has_property(train,d57,mouse_lymph,p).
has_property(train,d57,cytogen_ca,p).
has_property(train,d57,cytogen_sce,p).
has_property(train,d58,salmonella,p).
has_property(train,d58,cytogen_ca,p).
has_property(train,d58,cytogen_sce,p).
has_property(train,d59,salmonella_n,p).
has_property(train,d59,mouse_lymph,p).
has_property(train,d60,salmonella,n).
has_property(train,d60,salmonella_n,p).
has_property(train,d60,mouse_lymph,p).
has_property(train,d60,cytogen_ca,p).
has_property(train,d60,cytogen_sce,p).
has_property(train,d61,salmonella,p).
has_property(train,d61,cytogen_ca,n).
has_property(train,d61,cytogen_sce,p).
has_property(train,d62,salmonella_n,p).
has_property(train,d63,salmonella,p).
has_property(train,d63,mouse_lymph,p).
has_property(train,d63,cytogen_ca,p).
has_property(train,d63,cytogen_sce,p).
has_property(train,d64,salmonella,n).
has_property(train,d64,mouse_lymph,p).
has_property(train,d64,cytogen_ca,n).
has_property(train,d64,cytogen_sce,p).
has_property(train,d65,salmonella,n).
has_property(train,d65,mouse_lymph,p).
has_property(train,d65,cytogen_ca,n).
has_property(train,d65,cytogen_sce,n).
has_property(train,d66,salmonella,n).
has_property(train,d66,mouse_lymph,p).
has_property(train,d66,cytogen_ca,p).
has_property(train,d66,cytogen_sce,p).
has_property(train,d67,salmonella,p).
has_property(train,d67,salmonella_n,p).
has_property(train,d67,cytogen_ca,p).
has_property(train,d67,cytogen_sce,p).
has_property(train,d68,salmonella,p).
has_property(train,d68,mouse_lymph,p).
has_property(train,d68,cytogen_ca,p).
has_property(train,d68,cytogen_sce,p).
has_property(train,d69,salmonella,n).
has_property(train,d69,cytogen_sce,p).
has_property(train,d70,salmonella,p).
has_property(train,d70,salmonella_n,p).
has_property(train,d71,salmonella,p).
has_property(train,d71,cytogen_ca,n).
has_property(train,d71,cytogen_sce,p).
has_property(train,d72,salmonella,p).
has_property(train,d72,mouse_lymph,p).
has_property(train,d72,cytogen_ca,p).
has_property(train,d72,cytogen_sce,p).
has_property(train,d73,salmonella,p).
has_property(train,d73,salmonella_n,p).
has_property(train,d73,mouse_lymph,p).
has_property(train,d73,cytogen_ca,n).
has_property(train,d73,cytogen_sce,p).
has_property(train,d74,salmonella,n).
has_property(train,d74,mouse_lymph,p).
has_property(train,d74,cytogen_ca,p).
has_property(train,d74,cytogen_sce,p).
has_property(train,d75,salmonella,p).
has_property(train,d75,mouse_lymph,p).
has_property(train,d75,cytogen_ca,p).
has_property(train,d75,cytogen_sce,p).
has_property(train,d75,chromaberr,n).
has_property(train,d75,chromex,n).
has_property(train,d76,salmonella,n).
has_property(train,d76,mouse_lymph,p).
has_property(train,d76,cytogen_ca,p).
has_property(train,d76,cytogen_sce,n).
has_property(train,d76,chromaberr,n).
has_property(train,d76,chromex,p).
has_property(train,d77,salmonella,n).
has_property(train,d77,salmonella_reduc,n).
has_property(train,d77,mouse_lymph,n).
has_property(train,d77,cytogen_ca,n).
has_property(train,d78,salmonella,p).
has_property(train,d78,mouse_lymph,p).
has_property(train,d78,cytogen_ca,n).
has_property(train,d78,cytogen_sce,p).
has_property(train,d78,chromaberr,n).
has_property(train,d78,chromex,p).
has_property(train,d79,salmonella,p).
has_property(train,d79,mouse_lymph,n).
has_property(train,d79,cytogen_ca,p).
has_property(train,d79,cytogen_sce,p).
has_property(train,d80,salmonella,p).
has_property(train,d80,salmonella_n,p).
has_property(train,d80,cytogen_ca,p).
has_property(train,d80,cytogen_sce,n).
has_property(train,d81,salmonella,p).
has_property(train,d81,salmonella_n,p).
has_property(train,d81,mouse_lymph,p).
has_property(train,d81,cytogen_ca,p).
has_property(train,d81,cytogen_sce,p).
has_property(train,d82,salmonella,n).
has_property(train,d82,salmonella_n,n).
has_property(train,d82,mouse_lymph,p).
has_property(train,d82,cytogen_ca,n).
has_property(train,d82,cytogen_sce,p).
has_property(train,d82,chromaberr,n).
has_property(train,d83,salmonella,p).
has_property(train,d83,salmonella_n,p).
has_property(train,d83,mouse_lymph,p).
has_property(train,d83,cytogen_ca,p).
has_property(train,d83,cytogen_sce,p).
has_property(train,d86,mouse_lymph,n).
has_property(train,d86,chromaberr,n).
has_property(train,d87,salmonella,n).
has_property(train,d87,mouse_lymph,n).
has_property(train,d87,cytogen_ca,n).
has_property(train,d87,cytogen_sce,n).
has_property(train,d87,chromaberr,n).
has_property(train,d89,salmonella,n).
has_property(train,d89,mouse_lymph,p).
has_property(train,d89,cytogen_sce,p).
has_property(train,d90,salmonella,n).
has_property(train,d90,cytogen_ca,n).
has_property(train,d90,cytogen_sce,n).
has_property(train,d90,micronuc_m,n).
has_property(train,d90,micronuc_f,n).
has_property(train,d90,chromaberr,n).
has_property(train,d91,salmonella,n).
has_property(train,d91,cytogen_ca,n).
has_property(train,d91,cytogen_sce,n).
has_property(train,d91,chromaberr,n).
has_property(train,d92,salmonella,n).
has_property(train,d92,cytogen_ca,n).
has_property(train,d92,cytogen_sce,p).
has_property(train,d93,salmonella,n).
has_property(train,d93,cytogen_ca,p).
has_property(train,d93,cytogen_sce,p).
has_property(train,d94,salmonella,n).
has_property(train,d94,cytogen_ca,p).
has_property(train,d94,cytogen_sce,p).
has_property(train,d95,salmonella,n).
has_property(train,d95,mouse_lymph,n).
has_property(train,d95,cytogen_ca,p).
has_property(train,d95,cytogen_sce,p).
has_property(train,d95,chromaberr,n).
has_property(train,d96,salmonella,p).
has_property(train,d96,mouse_lymph,p).
has_property(train,d97,salmonella,n).
has_property(train,d97,mouse_lymph,n).
has_property(train,d97,cytogen_ca,n).
has_property(train,d97,cytogen_sce,n).
has_property(train,d98,salmonella,n).
has_property(train,d98,mouse_lymph,p).
has_property(train,d98,cytogen_ca,p).
has_property(train,d98,cytogen_sce,p).
has_property(train,d99,salmonella,n).
has_property(train,d99,mouse_lymph,n).
has_property(train,d99,cytogen_ca,n).
has_property(train,d99,cytogen_sce,n).
has_property(train,d100,salmonella,n).
has_property(train,d100,mouse_lymph,p).
has_property(train,d100,drosophila_slrl,p).
has_property(train,d100,drosophila_rt,n).
has_property(train,d100,cytogen_ca,n).
has_property(train,d100,cytogen_sce,p).
has_property(train,d101,salmonella,n).
has_property(train,d101,mouse_lymph,p).
has_property(train,d101,cytogen_ca,n).
has_property(train,d101,cytogen_sce,p).
has_property(train,d102,salmonella,n).
has_property(train,d102,mouse_lymph,p).
has_property(train,d102,cytogen_ca,n).
has_property(train,d102,cytogen_sce,p).
has_property(train,d102,chromaberr,n).
has_property(train,d103,salmonella,n).
has_property(train,d103,mouse_lymph,n).
has_property(train,d103,cytogen_ca,n).
has_property(train,d103,cytogen_sce,p).
has_property(train,d103,chromaberr,p).
has_property(train,d104,salmonella,n).
has_property(train,d104,mouse_lymph,n).
has_property(train,d104,cytogen_ca,n).
has_property(train,d104,cytogen_sce,p).
has_property(train,d105,salmonella,n).
has_property(train,d105,mouse_lymph,p).
has_property(train,d105,cytogen_ca,n).
has_property(train,d105,cytogen_sce,p).
has_property(train,d105,chromaberr,n).
has_property(train,d106,salmonella,n).
has_property(train,d108,salmonella,n).
has_property(train,d108,mouse_lymph,p).
has_property(train,d108,cytogen_ca,n).
has_property(train,d108,cytogen_sce,n).
has_property(train,d109,salmonella,n).
has_property(train,d109,mouse_lymph,p).
has_property(train,d109,cytogen_ca,p).
has_property(train,d109,cytogen_sce,p).
has_property(train,d110,salmonella,n).
has_property(train,d110,mouse_lymph,p).
has_property(train,d110,cytogen_ca,n).
has_property(train,d110,cytogen_sce,p).
has_property(train,d110,chromex,n).
has_property(train,d111,salmonella,n).
has_property(train,d112,salmonella,n).
has_property(train,d112,mouse_lymph,p).
has_property(train,d112,cytogen_ca,n).
has_property(train,d112,cytogen_sce,p).
has_property(train,d113,salmonella,p).
has_property(train,d113,cytogen_ca,n).
has_property(train,d113,cytogen_sce,p).
has_property(train,d114,salmonella,n).
has_property(train,d114,cytogen_ca,p).
has_property(train,d114,cytogen_sce,p).
has_property(train,d115,salmonella,n).
has_property(train,d115,mouse_lymph,n).
has_property(train,d115,cytogen_ca,p).
has_property(train,d115,cytogen_sce,p).
has_property(train,d115,chromaberr,p).
has_property(train,d115,chromex,p).
has_property(train,d116,salmonella,n).
has_property(train,d116,cytogen_ca,n).
has_property(train,d116,cytogen_sce,n).
has_property(train,d117,salmonella,n).
has_property(train,d117,mouse_lymph,p).
has_property(train,d117,cytogen_ca,n).
has_property(train,d117,cytogen_sce,p).
has_property(train,d118,salmonella,n).
has_property(train,d118,salmonella_n,n).
has_property(train,d118,mouse_lymph,p).
has_property(train,d118,cytogen_ca,n).
has_property(train,d118,cytogen_sce,p).
has_property(train,d119,salmonella,n).
has_property(train,d119,mouse_lymph,n).
has_property(train,d119,cytogen_ca,n).
has_property(train,d119,cytogen_sce,n).
has_property(train,d120,salmonella,n).
has_property(train,d120,mouse_lymph,p).
has_property(train,d120,cytogen_ca,n).
has_property(train,d120,cytogen_sce,p).
has_property(train,d120,micronuc_m,n).
has_property(train,d120,micronuc_f,n).
has_property(train,d120,chromaberr,n).
has_property(train,d120,chromex,n).
has_property(train,d121,salmonella,n).
has_property(train,d121,mouse_lymph,n).
has_property(train,d121,cytogen_ca,n).
has_property(train,d121,cytogen_sce,n).
has_property(train,d122,salmonella,n).
has_property(train,d122,salmonella_n,n).
has_property(train,d122,mouse_lymph,p).
has_property(train,d122,cytogen_ca,n).
has_property(train,d122,cytogen_sce,p).
has_property(train,d123,salmonella,n).
has_property(train,d123,cytogen_ca,n).
has_property(train,d123,cytogen_sce,n).
has_property(train,d124,salmonella,n).
has_property(train,d124,mouse_lymph,n).
has_property(train,d124,cytogen_ca,n).
has_property(train,d124,cytogen_sce,p).
has_property(train,d125,salmonella,n).
has_property(train,d125,mouse_lymph,n).
has_property(train,d125,cytogen_ca,n).
has_property(train,d125,cytogen_sce,n).
has_property(train,d126,salmonella,n).
has_property(train,d126,cytogen_ca,n).
has_property(train,d126,cytogen_sce,n).
has_property(train,d127,salmonella,n).
has_property(train,d127,mouse_lymph,n).
has_property(train,d127,cytogen_ca,p).
has_property(train,d127,cytogen_sce,n).
has_property(train,d128,salmonella,p).
has_property(train,d128,cytogen_ca,n).
has_property(train,d128,cytogen_sce,p).
has_property(train,d129,salmonella,n).
has_property(train,d129,mouse_lymph,p).
has_property(train,d130,mouse_lymph,p).
has_property(train,d130,cytogen_ca,p).
has_property(train,d130,cytogen_sce,p).
has_property(train,d131,salmonella,n).
has_property(train,d131,salmonella_n,n).
has_property(train,d131,mouse_lymph,p).
has_property(train,d131,cytogen_sce,p).
has_property(train,d132,salmonella,n).
has_property(train,d132,mouse_lymph,n).
has_property(train,d132,cytogen_ca,p).
has_property(train,d132,cytogen_sce,p).
has_property(train,d133,salmonella,n).
has_property(train,d133,mouse_lymph,p).
has_property(train,d133,cytogen_ca,n).
has_property(train,d133,cytogen_sce,p).
has_property(train,d134,salmonella,n).
has_property(train,d134,mouse_lymph,n).
has_property(train,d134,cytogen_ca,n).
has_property(train,d134,cytogen_sce,p).
has_property(train,d134,chromex,p).
has_property(train,d135,salmonella,n).
has_property(train,d135,mouse_lymph,n).
has_property(train,d135,cytogen_ca,p).
has_property(train,d135,cytogen_sce,n).
has_property(train,d135,cytogen_ca,n).
has_property(train,d135,cytogen_sce,n).
has_property(train,d136,salmonella,n).
has_property(train,d136,cytogen_ca,n).
has_property(train,d136,cytogen_sce,n).
has_property(train,d137,salmonella,n).
has_property(train,d137,salmonella_n,n).
has_property(train,d137,mouse_lymph,n).
has_property(train,d137,cytogen_ca,n).
has_property(train,d137,cytogen_sce,n).
has_property(train,d137,chromaberr,p).
has_property(train,d137,chromex,n).
has_property(train,d138,salmonella,n).
has_property(train,d138,mouse_lymph,p).
has_property(train,d138,cytogen_ca,p).
has_property(train,d138,cytogen_sce,p).
has_property(train,d139,salmonella,n).
has_property(train,d139,mouse_lymph,n).
has_property(train,d139,cytogen_ca,n).
has_property(train,d139,cytogen_sce,p).
has_property(train,d139,chromaberr,n).
has_property(train,d140,salmonella,n).
has_property(train,d140,mouse_lymph,n).
has_property(train,d140,cytogen_ca,p).
has_property(train,d140,cytogen_sce,p).
has_property(train,d141,salmonella,n).
has_property(train,d141,mouse_lymph,p).
has_property(train,d141,cytogen_ca,n).
has_property(train,d141,cytogen_sce,n).
has_property(train,d141,micronuc_m,n).
has_property(train,d141,micronuc_f,n).
has_property(train,d141,chromaberr,n).
has_property(train,d141,chromex,n).
has_property(train,d142,mouse_lymph,p).
has_property(train,d142,cytogen_ca,p).
has_property(train,d143,salmonella,n).
has_property(train,d143,mouse_lymph,n).
has_property(train,d143,cytogen_ca,n).
has_property(train,d143,cytogen_sce,n).
has_property(train,d144,salmonella,n).
has_property(train,d144,mouse_lymph,n).
has_property(train,d144,cytogen_ca,p).
has_property(train,d144,chromaberr,n).
has_property(train,d144,chromex,n).
has_property(train,d145,salmonella,n).
has_property(train,d145,mouse_lymph,n).
has_property(train,d145,cytogen_ca,n).
has_property(train,d145,cytogen_sce,n).
has_property(train,d145,chromaberr,p).
has_property(train,d145,chromex,p).
has_property(train,d146,salmonella,n).
has_property(train,d146,mouse_lymph,p).
has_property(train,d146,cytogen_ca,n).
has_property(train,d146,cytogen_sce,n).
has_property(train,d147,salmonella,n).
has_property(train,d147,mouse_lymph,n).
has_property(train,d147,cytogen_ca,n).
has_property(train,d147,cytogen_sce,n).
has_property(train,d147,chromaberr,n).
has_property(train,d148,salmonella,n).
has_property(train,d148,mouse_lymph,p).
has_property(train,d148,cytogen_ca,p).
has_property(train,d148,cytogen_sce,n).
has_property(train,d149,salmonella,n).
has_property(train,d149,mouse_lymph,n).
has_property(train,d149,cytogen_ca,n).
has_property(train,d150,salmonella,n).
has_property(train,d150,mouse_lymph,n).
has_property(train,d150,cytogen_ca,n).
has_property(train,d150,cytogen_sce,n).
has_property(train,d151,salmonella,n).
has_property(train,d151,mouse_lymph,p).
has_property(train,d151,cytogen_ca,p).
has_property(train,d151,cytogen_sce,p).
has_property(train,d151,chromaberr,p).
has_property(train,d152,salmonella,n).
has_property(train,d152,mouse_lymph,n).
has_property(train,d152,cytogen_ca,n).
has_property(train,d152,cytogen_sce,n).
has_property(train,d153,salmonella,n).
has_property(train,d153,mouse_lymph,p).
has_property(train,d153,cytogen_ca,n).
has_property(train,d153,cytogen_sce,p).
has_property(train,d155,salmonella,n).
has_property(train,d155,mouse_lymph,p).
has_property(train,d155,cytogen_ca,n).
has_property(train,d156,salmonella,n).
has_property(train,d156,mouse_lymph,p).
has_property(train,d156,cytogen_ca,n).
has_property(train,d157,salmonella,n).
has_property(train,d157,mouse_lymph,n).
has_property(train,d157,cytogen_ca,p).
has_property(train,d157,cytogen_sce,n).
has_property(train,d158,salmonella,n).
has_property(train,d158,mouse_lymph,p).
has_property(train,d158,cytogen_ca,n).
has_property(train,d158,cytogen_sce,p).
has_property(train,d159,salmonella,n).
has_property(train,d159,salmonella_n,n).
has_property(train,d159,mouse_lymph,n).
has_property(train,d159,cytogen_ca,n).
has_property(train,d160,salmonella,p).
has_property(train,d160,cytogen_ca,n).
has_property(train,d160,cytogen_sce,p).
has_property(train,d160,chromaberr,p).
has_property(train,d160,chromex,p).
has_property(train,d161,salmonella,n).
has_property(train,d161,mouse_lymph,p).
has_property(train,d161,cytogen_ca,n).
has_property(train,d161,cytogen_sce,n).
has_property(train,d163,salmonella,n).
has_property(train,d163,salmonella_n,n).
has_property(train,d163,mouse_lymph,n).
has_property(train,d163,cytogen_ca,n).
has_property(train,d163,cytogen_sce,n).
has_property(train,d165,salmonella,n).
has_property(train,d165,mouse_lymph,p).
has_property(train,d165,cytogen_ca,n).
has_property(train,d166,salmonella,n).
has_property(train,d166,mouse_lymph,n).
has_property(train,d166,cytogen_ca,n).
has_property(train,d166,cytogen_sce,n).
has_property(train,d167,salmonella,n).
has_property(train,d167,mouse_lymph,n).
has_property(train,d167,cytogen_sce,p).
has_property(train,d168,salmonella,n).
has_property(train,d168,mouse_lymph,n).
has_property(train,d168,cytogen_ca,n).
has_property(train,d168,cytogen_sce,n).
has_property(train,d169,salmonella,n).
has_property(train,d169,mouse_lymph,p).
has_property(train,d169,cytogen_ca,p).
has_property(train,d169,cytogen_sce,p).
has_property(train,d170,salmonella,n).
has_property(train,d170,mouse_lymph,p).
has_property(train,d170,cytogen_ca,n).
has_property(train,d170,cytogen_sce,n).
has_property(train,d171,mouse_lymph,n).
has_property(train,d171,cytogen_ca,n).
has_property(train,d171,cytogen_sce,n).
has_property(train,d172,salmonella,n).
has_property(train,d172,salmonella_n,n).
has_property(train,d172,mouse_lymph,p).
has_property(train,d172,cytogen_ca,n).
has_property(train,d172,cytogen_sce,n).
has_property(train,d173,salmonella,n).
has_property(train,d173,salmonella_n,n).
has_property(train,d173,mouse_lymph,p).
has_property(train,d173,cytogen_ca,n).
has_property(train,d173,cytogen_sce,n).
has_property(train,d174,salmonella,n).
has_property(train,d174,mouse_lymph,n).
has_property(train,d174,cytogen_ca,n).
has_property(train,d174,cytogen_sce,n).
has_property(train,d176,salmonella,n).
has_property(train,d176,mouse_lymph,n).
has_property(train,d176,cytogen_ca,n).
has_property(train,d176,cytogen_sce,n).
has_property(train,d177,salmonella,n).
has_property(train,d177,mouse_lymph,p).
has_property(train,d177,cytogen_ca,n).
has_property(train,d177,cytogen_sce,n).
has_property(train,d178,salmonella,n).
has_property(train,d178,salmonella_n,n).
has_property(train,d178,mouse_lymph,n).
has_property(train,d178,cytogen_sce,n).
has_property(train,d178,chromaberr,n).
has_property(train,d179,salmonella,n).
has_property(train,d179,mouse_lymph,n).
has_property(train,d179,cytogen_ca,n).
has_property(train,d179,cytogen_sce,p).
has_property(train,d180,salmonella,n).
has_property(train,d180,cytogen_ca,n).
has_property(train,d180,cytogen_sce,n).
has_property(train,d181,salmonella,n).
has_property(train,d181,cytogen_ca,p).
has_property(train,d181,cytogen_sce,p).
has_property(train,d182,salmonella,n).
has_property(train,d182,mouse_lymph,p).
has_property(train,d182,cytogen_ca,n).
has_property(train,d183,salmonella,n).
has_property(train,d183,cytogen_ca,n).
has_property(train,d183,cytogen_sce,n).
has_property(train,d184,cytogen_ca,n).
has_property(train,d184,cytogen_sce,n).
has_property(train,d185,salmonella,n).
has_property(train,d185,cytogen_ca,n).
has_property(train,d185,cytogen_sce,n).
has_property(train,d186,salmonella,n).
has_property(train,d186,cytogen_ca,n).
has_property(train,d186,cytogen_sce,n).
has_property(train,d187,salmonella,n).
has_property(train,d187,mouse_lymph,p).
has_property(train,d187,cytogen_ca,n).
has_property(train,d187,cytogen_sce,p).
has_property(train,d188,salmonella,n).
has_property(train,d188,mouse_lymph,p).
has_property(train,d188,cytogen_ca,n).
has_property(train,d188,cytogen_sce,n).
has_property(train,d190,salmonella,n).
has_property(train,d190,cytogen_ca,n).
has_property(train,d190,cytogen_sce,n).
has_property(train,d191,salmonella,n).
has_property(train,d191,mouse_lymph,p).
has_property(train,d191,cytogen_ca,p).
has_property(train,d191,cytogen_sce,p).
has_property(train,d191,chromaberr,n).
has_property(train,d192,salmonella,n).
has_property(train,d192,salmonella_n,n).
has_property(train,d192,mouse_lymph,p).
has_property(train,d192,cytogen_ca,n).
has_property(train,d192,cytogen_sce,n).
has_property(train,d192,chromaberr,n).
has_property(train,d193,salmonella,n).
has_property(train,d193,mouse_lymph,n).
has_property(train,d193,cytogen_ca,n).
has_property(train,d193,cytogen_sce,p).
has_property(train,d195,salmonella,n).
has_property(train,d195,mouse_lymph,p).
has_property(train,d195,cytogen_ca,n).
has_property(train,d195,cytogen_sce,p).
has_property(train,d196,mouse_lymph,p).
has_property(train,d196,drosophila_slrl,p).
has_property(train,d196,drosophila_rt,n).
has_property(train,d196,cytogen_ca,p).
has_property(train,d196,cytogen_sce,p).
has_property(train,d196,chromaberr,n).
has_property(train,d197,salmonella,n).
has_property(train,d197,mouse_lymph,p).
has_property(train,d197,cytogen_ca,p).
has_property(train,d197,cytogen_sce,n).
has_property(train,d198,salmonella,n).
has_property(train,d198,mouse_lymph,p).
has_property(train,d198,cytogen_ca,n).
has_property(train,d198,cytogen_sce,n).
has_property(train,d199,salmonella,n).
has_property(train,d199,salmonella_n,n).
has_property(train,d199,mouse_lymph,p).
has_property(train,d199,cytogen_ca,n).
has_property(train,d199,cytogen_sce,p).
has_property(train,d200,salmonella,n).
has_property(train,d200,mouse_lymph,p).
has_property(train,d200,cytogen_ca,n).
has_property(train,d200,cytogen_sce,p).
has_property(train,d201,salmonella,n).
has_property(train,d201,cytogen_ca,n).
has_property(train,d201,cytogen_sce,p).
has_property(train,d202,salmonella,n).
has_property(train,d202,mouse_lymph,p).
has_property(train,d202,cytogen_ca,n).
has_property(train,d202,cytogen_sce,p).
has_property(train,d202,chromaberr,n).
has_property(train,d202,chromex,n).
has_property(train,d203,salmonella,n).
has_property(train,d203,mouse_lymph,n).
has_property(train,d203,cytogen_ca,n).
has_property(train,d203,chromex,p).
has_property(train,d204,salmonella,n).
has_property(train,d204,mouse_lymph,p).
has_property(train,d204,cytogen_ca,n).
has_property(train,d204,cytogen_sce,n).
has_property(train,d205,salmonella,p).
has_property(train,d205,mouse_lymph,p).
has_property(train,d205,drosophila_slrl,p).
has_property(train,d205,drosophila_rt,n).
has_property(train,d205,cytogen_ca,p).
has_property(train,d205,cytogen_sce,n).
has_property(train,d205,chromaberr,n).
has_property(train,d206,salmonella,n).
has_property(train,d206,mouse_lymph,n).
has_property(train,d206,cytogen_ca,n).
has_property(train,d206,cytogen_sce,n).
has_property(train,d207,salmonella,n).
has_property(train,d207,mouse_lymph,p).
has_property(train,d207,cytogen_ca,p).
has_property(train,d207,cytogen_sce,p).
has_property(train,d207,chromaberr,p).
has_property(train,d207,chromex,p).
has_property(train,d208_1,salmonella,n).
has_property(train,d208_1,cytogen_ca,n).
has_property(train,d208_1,cytogen_sce,p).
has_property(train,d208_2,salmonella,n).
has_property(train,d208_2,mouse_lymph,p).
has_property(train,d208_2,cytogen_sce,n).
has_property(train,d209,salmonella,n).
has_property(train,d209,mouse_lymph,p).
has_property(train,d209,cytogen_ca,p).
has_property(train,d209,cytogen_sce,p).
has_property(train,d211,salmonella,n).
has_property(train,d211,salmonella_n,n).
has_property(train,d211,mouse_lymph,p).
has_property(train,d211,cytogen_ca,n).
has_property(train,d211,cytogen_sce,p).
has_property(train,d212,salmonella,n).
has_property(train,d212,mouse_lymph,n).
has_property(train,d212,cytogen_ca,n).
has_property(train,d212,chromex,p).
has_property(train,d213,salmonella,n).
has_property(train,d213,mouse_lymph,p).
has_property(train,d213,cytogen_ca,p).
has_property(train,d213,cytogen_sce,p).
has_property(train,d213,chromex,p).
has_property(train,d214,salmonella,n).
has_property(train,d214,salmonella_n,n).
has_property(train,d214,mouse_lymph,p).
has_property(train,d214,cytogen_ca,n).
has_property(train,d214,cytogen_sce,p).
has_property(train,d215,salmonella,n).
has_property(train,d215,mouse_lymph,p).
has_property(train,d215,cytogen_ca,n).
has_property(train,d215,cytogen_sce,n).
has_property(train,d216,salmonella,p).
has_property(train,d216,cytogen_ca,n).
has_property(train,d216,cytogen_sce,n).
has_property(train,d217,salmonella,n).
has_property(train,d217,salmonella_n,n).
has_property(train,d217,mouse_lymph,p).
has_property(train,d217,cytogen_ca,n).
has_property(train,d217,cytogen_sce,p).
has_property(train,d218,salmonella,n).
has_property(train,d218,mouse_lymph,p).
has_property(train,d218,cytogen_ca,n).
has_property(train,d218,cytogen_sce,n).
has_property(train,d219,salmonella,n).
has_property(train,d219,mouse_lymph,p).
has_property(train,d219,cytogen_ca,p).
has_property(train,d219,cytogen_sce,n).
has_property(train,d221,salmonella,n).
has_property(train,d221,mouse_lymph,p).
has_property(train,d221,cytogen_ca,n).
has_property(train,d221,chromaberr,n).
has_property(train,d224,salmonella,n).
has_property(train,d224,mouse_lymph,n).
has_property(train,d224,cytogen_ca,n).
has_property(train,d224,cytogen_sce,n).
has_property(train,d225,salmonella,n).
has_property(train,d225,mouse_lymph,p).
has_property(train,d225,cytogen_ca,n).
has_property(train,d225,cytogen_sce,p).
has_property(train,d225,chromaberr,p).
has_property(train,d225,chromex,n).
has_property(train,d226,salmonella,p).
has_property(train,d226,salmonella_n,p).
has_property(train,d226,mouse_lymph,p).
has_property(train,d226,drosophila_slrl,p).
has_property(train,d226,drosophila_rt,n).
has_property(train,d226,cytogen_ca,p).
has_property(train,d226,cytogen_sce,p).
has_property(train,d227,salmonella,p).
has_property(train,d227,mouse_lymph,p).
has_property(train,d227,drosophila_slrl,p).
has_property(train,d227,drosophila_rt,p).
has_property(train,d227,cytogen_ca,p).
has_property(train,d227,cytogen_sce,p).
has_property(train,d227,chromaberr,n).
has_property(train,d228,salmonella,p).
has_property(train,d228,salmonella_n,p).
has_property(train,d228,mouse_lymph,p).
has_property(train,d228,drosophila_slrl,p).
has_property(train,d228,drosophila_rt,p).
has_property(train,d228,cytogen_ca,p).
has_property(train,d228,cytogen_sce,p).
has_property(train,d228,chromaberr,n).
has_property(train,d229,salmonella,p).
has_property(train,d229,cytogen_ca,p).
has_property(train,d229,cytogen_sce,p).
has_property(train,d229,micronuc_m,n).
has_property(train,d229,micronuc_f,n).
has_property(train,d230,salmonella,p).
has_property(train,d230,mouse_lymph,p).
has_property(train,d230,drosophila_slrl,p).
has_property(train,d230,drosophila_rt,n).
has_property(train,d230,cytogen_ca,n).
has_property(train,d230,cytogen_sce,p).
has_property(train,d230,chromaberr,p).
has_property(train,d231,salmonella,n).
has_property(train,d231,mouse_lymph,n).
has_property(train,d232,salmonella,p).
has_property(train,d232,mouse_lymph,p).
has_property(train,d232,drosophila_slrl,p).
has_property(train,d232,drosophila_rt,p).
has_property(train,d232,cytogen_ca,p).
has_property(train,d232,cytogen_sce,p).
has_property(train,d232,chromaberr,p).
has_property(train,d233,salmonella,n).
has_property(train,d233,cytogen_ca,n).
has_property(train,d233,cytogen_sce,p).
has_property(train,d234,salmonella,p).
has_property(train,d234,cytogen_ca,p).
has_property(train,d234,cytogen_sce,p).
has_property(train,d235,salmonella_n,p).
has_property(train,d236,salmonella,p).
has_property(train,d236,mouse_lymph,p).
has_property(train,d236,drosophila_slrl,p).
has_property(train,d236,drosophila_rt,n).
has_property(train,d236,cytogen_ca,p).
has_property(train,d236,cytogen_sce,p).
has_property(train,d236,chromaberr,p).
has_property(train,d236,chromex,p).
has_property(train,d237,salmonella,p).
has_property(train,d237,mouse_lymph,p).
has_property(train,d237,cytogen_ca,p).
has_property(train,d237,cytogen_sce,p).
has_property(train,d237,chromaberr,n).
has_property(train,d237,chromex,n).
has_property(train,d238,salmonella,p).
has_property(train,d238,mouse_lymph,p).
has_property(train,d238,drosophila_slrl,p).
has_property(train,d238,drosophila_rt,p).
has_property(train,d238,cytogen_ca,p).
has_property(train,d238,cytogen_sce,p).
has_property(train,d238,chromaberr,p).
has_property(train,d238,chromex,p).
has_property(train,d239,salmonella,n).
has_property(train,d239,cytogen_ca,n).
has_property(train,d239,cytogen_sce,p).
has_property(train,d240,salmonella,p).
has_property(train,d240,mouse_lymph,p).
has_property(train,d240,drosophila_slrl,p).
has_property(train,d240,drosophila_rt,p).
has_property(train,d240,cytogen_ca,p).
has_property(train,d240,cytogen_sce,p).
has_property(train,d241,salmonella,p).
has_property(train,d241,mouse_lymph,p).
has_property(train,d241,cytogen_ca,p).
has_property(train,d241,cytogen_sce,p).
has_property(train,d242,salmonella,p).
has_property(train,d242,mouse_lymph,p).
has_property(train,d242,cytogen_ca,p).
has_property(train,d242,cytogen_sce,p).
has_property(train,d243,salmonella,p).
has_property(train,d243,salmonella,p).
has_property(train,d243,mouse_lymph,p).
has_property(train,d243,cytogen_ca,p).
has_property(train,d243,cytogen_sce,p).
has_property(train,d243,chromaberr,n).
has_property(train,d243,chromex,n).
has_property(train,d244,salmonella,p).
has_property(train,d244,mouse_lymph,p).
has_property(train,d244,cytogen_ca,p).
has_property(train,d244,cytogen_sce,p).
has_property(train,d245,salmonella,p).
has_property(train,d245,salmonella_n,n).
has_property(train,d245,mouse_lymph,p).
has_property(train,d245,drosophila_slrl,p).
has_property(train,d245,drosophila_rt,p).
has_property(train,d245,cytogen_ca,p).
has_property(train,d245,cytogen_sce,p).
has_property(train,d247,salmonella,n).
has_property(train,d247,mouse_lymph,p).
has_property(train,d247,drosophila_slrl,p).
has_property(train,d247,drosophila_rt,n).
has_property(train,d247,cytogen_ca,p).
has_property(train,d247,cytogen_sce,p).
has_property(train,d248,salmonella_n,p).
has_property(train,d249,salmonella,p).
has_property(train,d250,salmonella,p).
has_property(train,d250,mouse_lymph,p).
has_property(train,d250,cytogen_ca,p).
has_property(train,d250,cytogen_sce,p).
has_property(train,d250,chromaberr,n).
has_property(train,d250,chromex,n).
has_property(train,d251,salmonella,n).
has_property(train,d251,cytogen_ca,n).
has_property(train,d252,salmonella,p).
has_property(train,d252,mouse_lymph,p).
has_property(train,d252,cytogen_ca,p).
has_property(train,d252,cytogen_sce,p).
has_property(train,d252,chromex,p).
has_property(train,d253,salmonella,n).
has_property(train,d253,mouse_lymph,p).
has_property(train,d253,drosophila_slrl,p).
has_property(train,d253,drosophila_rt,n).
has_property(train,d253,cytogen_ca,n).
has_property(train,d253,cytogen_sce,p).
has_property(train,d254,salmonella,p).
has_property(train,d254,drosophila_slrl,p).
has_property(train,d254,drosophila_rt,n).
has_property(train,d254,cytogen_ca,p).
has_property(train,d254,cytogen_sce,p).
has_property(train,d254,micronuc_m,n).
has_property(train,d254,micronuc_f,n).
has_property(train,d255,salmonella,p).
has_property(train,d256,salmonella,p).
has_property(train,d256,cytogen_ca,p).
has_property(train,d256,cytogen_sce,p).
has_property(train,d257,salmonella,n).
has_property(train,d257,cytogen_ca,n).
has_property(train,d257,cytogen_sce,p).
has_property(train,d258,salmonella,p).
has_property(train,d259,salmonella,n).
has_property(train,d259,cytogen_ca,p).
has_property(train,d259,cytogen_sce,n).
has_property(train,d260,salmonella,n).
has_property(train,d260,cytogen_ca,n).
has_property(train,d260,cytogen_sce,p).
has_property(train,d261,salmonella,n).
has_property(train,d261,cytogen_ca,n).
has_property(train,d261,cytogen_sce,n).
has_property(train,d262,salmonella,p).
has_property(train,d262,mouse_lymph,n).
has_property(train,d262,cytogen_ca,n).
has_property(train,d262,cytogen_sce,n).
has_property(train,d263,salmonella,p).
has_property(train,d263,mouse_lymph,p).
has_property(train,d263,cytogen_ca,p).
has_property(train,d263,cytogen_sce,p).
has_property(train,d263,chromaberr,n).
has_property(train,d263,chromex,n).
has_property(train,d264,salmonella,n).
has_property(train,d264,salmonella_n,n).
has_property(train,d264,mouse_lymph,n).
has_property(train,d264,cytogen_ca,n).
has_property(train,d264,cytogen_sce,n).
has_property(train,d265,salmonella,p).
has_property(train,d265,cytogen_sce,p).
has_property(train,d266,salmonella,n).
has_property(train,d266,mouse_lymph,p).
has_property(train,d266,cytogen_ca,n).
has_property(train,d266,cytogen_sce,p).
has_property(train,d266,chromaberr,n).
has_property(train,d267,salmonella,n).
has_property(train,d267,mouse_lymph,p).
has_property(train,d267,cytogen_ca,p).
has_property(train,d267,cytogen_sce,p).
has_property(train,d268,salmonella,p).
has_property(train,d268,mouse_lymph,p).
has_property(train,d268,drosophila_slrl,p).
has_property(train,d268,drosophila_rt,n).
has_property(train,d268,cytogen_ca,p).
has_property(train,d268,cytogen_sce,p).
has_property(train,d269,salmonella,n).
has_property(train,d269,salmonella_n,n).
has_property(train,d269,mouse_lymph,p).
has_property(train,d269,cytogen_ca,n).
has_property(train,d269,cytogen_sce,n).
has_property(train,d270,salmonella,p).
has_property(train,d270,mouse_lymph,n).
has_property(train,d270,cytogen_ca,n).
has_property(train,d270,cytogen_sce,p).
has_property(train,d271,salmonella,n).
has_property(train,d271,cytogen_ca,p).
has_property(train,d271,cytogen_sce,p).
has_property(train,d272,salmonella,p).
has_property(train,d272,cytogen_ca,n).
has_property(train,d272,cytogen_sce,n).
has_property(train,d273,salmonella,n).
has_property(train,d273,mouse_lymph,p).
has_property(train,d273,cytogen_ca,n).
has_property(train,d274,mouse_lymph,p).
has_property(train,d274,drosophila_slrl,p).
has_property(train,d274,drosophila_rt,n).
has_property(train,d274,cytogen_ca,n).
has_property(train,d274,cytogen_sce,p).
has_property(train,d274,chromaberr,n).
has_property(train,d274,chromex,p).
has_property(train,d275,salmonella,n).
has_property(train,d275,mouse_lymph,p).
has_property(train,d275,cytogen_ca,n).
has_property(train,d275,cytogen_sce,p).
has_property(train,d275,chromex,n).
has_property(train,d276,salmonella,p).
has_property(train,d276,cytogen_ca,n).
has_property(train,d276,cytogen_sce,p).
has_property(train,d277,salmonella,p).
has_property(train,d277,salmonella_n,n).
has_property(train,d277,mouse_lymph,p).
has_property(train,d277,cytogen_ca,n).
has_property(train,d277,cytogen_sce,n).
has_property(train,d278,salmonella,p).
has_property(train,d278,salmonella_n,p).
has_property(train,d278,mouse_lymph,p).
has_property(train,d278,cytogen_ca,p).
has_property(train,d278,cytogen_sce,p).
has_property(train,d279,salmonella,n).
has_property(train,d279,mouse_lymph,p).
has_property(train,d279,cytogen_ca,p).
has_property(train,d279,cytogen_sce,p).
has_property(train,d280,salmonella,p).
has_property(train,d280,mouse_lymph,p).
has_property(train,d280,cytogen_ca,n).
has_property(train,d280,cytogen_sce,p).
has_property(train,d281,salmonella,p).
has_property(train,d281,cytogen_ca,p).
has_property(train,d281,cytogen_sce,p).
has_property(train,d282,salmonella,p).
has_property(train,d282,salmonella_n,p).
has_property(train,d282,mouse_lymph,p).
has_property(train,d282,cytogen_ca,p).
has_property(train,d282,cytogen_sce,p).
has_property(train,d283,salmonella,p).
has_property(train,d283,mouse_lymph,p).
has_property(train,d283,cytogen_ca,p).
has_property(train,d283,cytogen_sce,p).
has_property(train,d283,chromaberr,n).
has_property(train,d283,chromex,n).
has_property(train,d284,salmonella,n).
has_property(train,d284,cytogen_ca,p).
has_property(train,d284,cytogen_sce,p).
has_property(train,d285,salmonella,p).
has_property(train,d285,mouse_lymph,p).
has_property(train,d285,cytogen_ca,p).
has_property(train,d285,cytogen_sce,p).
has_property(train,d286,salmonella,n).
has_property(train,d286,mouse_lymph,p).
has_property(train,d286,cytogen_ca,n).
has_property(train,d286,cytogen_sce,p).
has_property(train,d287,salmonella,p).
has_property(train,d287,cytogen_ca,p).
has_property(train,d287,cytogen_sce,p).
has_property(train,d288,salmonella,p).
has_property(train,d289,salmonella,n).
has_property(train,d289,mouse_lymph,p).
has_property(train,d289,drosophila_slrl,p).
has_property(train,d289,drosophila_rt,p).
has_property(train,d289,cytogen_ca,n).
has_property(train,d289,cytogen_sce,p).
has_property(train,d290,salmonella,p).
has_property(train,d290,cytogen_ca,p).
has_property(train,d290,cytogen_sce,p).
has_property(train,d291,salmonella,n).
has_property(train,d291,salmonella_n,n).
has_property(train,d291,mouse_lymph,n).
has_property(train,d291,cytogen_ca,n).
has_property(train,d291,cytogen_sce,n).
has_property(train,d292,salmonella,n).
has_property(train,d292,salmonella_n,n).
has_property(train,d292,cytogen_ca,n).
has_property(train,d292,cytogen_sce,n).
has_property(train,d293,salmonella,p).
has_property(train,d293,cytogen_ca,p).
has_property(train,d293,cytogen_sce,p).
has_property(train,d294,salmonella,p).
has_property(train,d294,mouse_lymph,p).
has_property(train,d294,cytogen_ca,p).
has_property(train,d294,cytogen_sce,p).
has_property(train,d294,chromaberr,n).
has_property(train,d294,chromex,n).
has_property(train,d295,salmonella,p).
has_property(train,d295,mouse_lymph,n).
ind(train,d1,amino,1).
ind(train,d2,amino,1).
ind(train,d3,amino,1).
ind(train,d4,amino,1).
ind(train,d5,amino,2).
ind(train,d6,amino,2).
ind(train,d7,amino,2).
ind(train,d9,amino,1).
ind(train,d11,amino,2).
ind(train,d12,amino,2).
ind(train,d15,amino,2).
ind(train,d17,amino,2).
ind(train,d20,amino,1).
ind(train,d23_1,amino,2).
ind(train,d23_2,amino,2).
ind(train,d24,amino,2).
ind(train,d25,amino,1).
ind(train,d26,amino,1).
ind(train,d28,amino,1).
ind(train,d30,amino,1).
ind(train,d31,amino,2).
ind(train,d32,amino,2).
ind(train,d33,amino,1).
ind(train,d35,amino,1).
ind(train,d39,amino,1).
ind(train,d40,amino,2).
ind(train,d42,amino,1).
ind(train,d43,amino,4).
ind(train,d44,amino,1).
ind(train,d45,amino,2).
ind(train,d48,amino,1).
ind(train,d49,amino,1).
ind(train,d50,amino,1).
ind(train,d52,amino,1).
ind(train,d53,amino,1).
ind(train,d55,amino,2).
ind(train,d56,amino,1).
ind(train,d57,amino,1).
ind(train,d192,amino,1).
ind(train,d203,amino,3).
ind(train,d291,amino,1).
ind(train,d59,amino,1).
ind(train,d60,amino,1).
ind(train,d62,amino,2).
ind(train,d63,amino,1).
ind(train,d68,amino,2).
ind(train,d69,amino,1).
ind(train,d70,amino,1).
ind(train,d72,amino,1).
ind(train,d73,amino,2).
ind(train,d74,amino,1).
ind(train,d75,amino,2).
ind(train,d81,amino,2).
ind(train,d82,amino,1).
ind(train,d83,amino,2).
ind(train,d114,amino,1).
ind(train,d123,amino,1).
ind(train,d176,amino,2).
ind(train,d183,amino,1).
ind(train,d213,amino,1).
ind(train,d8,di8,1).
ind(train,d48,di8,1).
ind(train,d54,di8,1).
ind(train,d67,di8,1).
ind(train,d1,di10,1).
ind(train,d2,di10,1).
ind(train,d3,di10,1).
ind(train,d4,di10,1).
ind(train,d5,di10,2).
ind(train,d6,di10,2).
ind(train,d7,di10,2).
ind(train,d8,di10,1).
ind(train,d9,di10,1).
ind(train,d10,di10,1).
ind(train,d11,di10,2).
ind(train,d12,di10,2).
ind(train,d13,di10,2).
ind(train,d14,di10,2).
ind(train,d15,di10,2).
ind(train,d17,di10,2).
ind(train,d18,di10,2).
ind(train,d20,di10,1).
ind(train,d22,di10,1).
ind(train,d23_1,di10,2).
ind(train,d23_2,di10,2).
ind(train,d24,di10,2).
ind(train,d25,di10,1).
ind(train,d26,di10,1).
ind(train,d27,di10,2).
ind(train,d28,di10,2).
ind(train,d30,di10,1).
ind(train,d31,di10,2).
ind(train,d32,di10,2).
ind(train,d33,di10,1).
ind(train,d35,di10,1).
ind(train,d39,di10,1).
ind(train,d40,di10,2).
ind(train,d41,di10,1).
ind(train,d42,di10,1).
ind(train,d43,di10,4).
ind(train,d44,di10,1).
ind(train,d45,di10,2).
ind(train,d48,di10,2).
ind(train,d49,di10,1).
ind(train,d50,di10,1).
ind(train,d52,di10,1).
ind(train,d53,di10,1).
ind(train,d54,di10,1).
ind(train,d55,di10,2).
ind(train,d56,di10,1).
ind(train,d57,di10,1).
ind(train,d58,di10,2).
ind(train,d95,di10,2).
ind(train,d109,di10,1).
ind(train,d143,di10,1).
ind(train,d192,di10,1).
ind(train,d197,di10,2).
ind(train,d199,di10,1).
ind(train,d203,di10,3).
ind(train,d231,di10,1).
ind(train,d246,di10,2).
ind(train,d277,di10,2).
ind(train,d278,di10,2).
ind(train,d279,di10,1).
ind(train,d284,di10,1).
ind(train,d290,di10,1).
ind(train,d291,di10,1).
ind(train,d292,di10,1).
ind(train,d59,di10,1).
ind(train,d60,di10,1).
ind(train,d62,di10,2).
ind(train,d63,di10,2).
ind(train,d64,di10,1).
ind(train,d66,di10,1).
ind(train,d67,di10,1).
ind(train,d68,di10,2).
ind(train,d69,di10,1).
ind(train,d70,di10,1).
ind(train,d72,di10,1).
ind(train,d73,di10,2).
ind(train,d74,di10,2).
ind(train,d75,di10,2).
ind(train,d77,di10,2).
ind(train,d78,di10,2).
ind(train,d79,di10,1).
ind(train,d81,di10,2).
ind(train,d82,di10,2).
ind(train,d83,di10,2).
ind(train,d114,di10,1).
ind(train,d118,di10,1).
ind(train,d123,di10,2).
ind(train,d124,di10,2).
ind(train,d152,di10,1).
ind(train,d159,di10,2).
ind(train,d163,di10,2).
ind(train,d166,di10,1).
ind(train,d171,di10,2).
ind(train,d175,di10,2).
ind(train,d176,di10,2).
ind(train,d179,di10,2).
ind(train,d182,di10,1).
ind(train,d183,di10,1).
ind(train,d210,di10,2).
ind(train,d211,di10,1).
ind(train,d213,di10,1).
ind(train,d217,di10,1).
ind(train,d222,di10,2).
ind(train,d223,di10,1).
ind(train,d256,di10,1).
ind(train,d258,di10,1).
ind(train,d265,di10,1).
ind(train,d280,di10,1).
ind(train,d8,di23,1).
ind(train,d23_2,di23,1).
ind(train,d34,di23,1).
ind(train,d36,di23,1).
ind(train,d47,di23,1).
ind(train,d76,di23,1).
ind(train,d212,di23,1).
ind(train,d221,di23,1).
ind(train,d29,cyanate,4).
ind(train,d37,cyanate,2).
ind(train,d8,di48,1).
ind(train,d48,di48,1).
ind(train,d54,di48,1).
ind(train,d95,di48,1).
ind(train,d67,di48,1).
ind(train,d77,di48,2).
ind(train,d210,di48,1).
ind(train,d223,di48,1).
ind(train,d51,di51,2).
ind(train,d97,ethoxy,1).
ind(train,d283,ethoxy,1).
ind(train,d61,ethoxy,2).
ind(train,d66,ethoxy,1).
ind(train,d257,ethoxy,2).
ind(train,d264,ethoxy,2).
ind(train,d266,ethoxy,2).
ind(train,d269,ethoxy,2).
ind(train,d270,ethoxy,4).
ind(train,d271,ethoxy,2).
ind(train,d22,di64,1).
ind(train,d58,di64,2).
ind(train,d64,di64,1).
ind(train,d74,di64,1).
ind(train,d118,di64,1).
ind(train,d18,di66,1).
ind(train,d143,di66,2).
ind(train,d66,di66,2).
ind(train,d258,di66,4).
ind(train,d8,di67a,1).
ind(train,d48,di67a,1).
ind(train,d54,di67a,1).
ind(train,d67,di67a,1).
ind(train,d84,halide10,16).
ind(train,d89,halide10,2).
ind(train,d92,halide10,6).
ind(train,d94,halide10,3).
ind(train,d96,halide10,6).
ind(train,d101,halide10,5).
ind(train,d102,halide10,5).
ind(train,d103,halide10,4).
ind(train,d104,halide10,4).
ind(train,d106,halide10,4).
ind(train,d107,halide10,20).
ind(train,d108,halide10,3).
ind(train,d226,halide10,1).
ind(train,d227,halide10,3).
ind(train,d228,halide10,2).
ind(train,d229,halide10,2).
ind(train,d230,halide10,1).
ind(train,d231,halide10,2).
ind(train,d235,halide10,6).
ind(train,d236,halide10,1).
ind(train,d239,halide10,1).
ind(train,d241,halide10,1).
ind(train,d243,halide10,2).
ind(train,d246,halide10,4).
ind(train,d249,halide10,1).
ind(train,d250,halide10,2).
ind(train,d255,halide10,1).
ind(train,d272,halide10,2).
ind(train,d273,halide10,3).
ind(train,d274,halide10,3).
ind(train,d275,halide10,3).
ind(train,d290,halide10,3).
ind(train,d67,halide10,1).
ind(train,d112,halide10,6).
ind(train,d113,halide10,2).
ind(train,d116,halide10,2).
ind(train,d119,halide10,1).
ind(train,d121,halide10,4).
ind(train,d122,halide10,3).
ind(train,d123,halide10,1).
ind(train,d125,halide10,3).
ind(train,d126,halide10,6).
ind(train,d128,halide10,6).
ind(train,d259,halide10,1).
ind(train,d261,halide10,1).
ind(train,d263,halide10,1).
ind(train,d268,halide10,1).
ind(train,d276,halide10,3).
ind(train,d251,halide10,3).
ind(train,d137,methoxy,1).
ind(train,d233,methoxy,2).
ind(train,d234,methoxy,3).
ind(train,d237,methoxy,2).
ind(train,d244,methoxy,2).
ind(train,d247,methoxy,2).
ind(train,d253,methoxy,2).
ind(train,d291,methoxy,1).
ind(train,d71,methoxy,2).
ind(train,d150,methoxy,2).
ind(train,d256,methoxy,2).
ind(train,d258,methoxy,2).
ind(train,d265,methoxy,2).
ind(train,d266,methoxy,2).
ind(train,d271,methoxy,2).
ind(train,d285,methoxy,1).
ind(train,d227,di227,6).
ind(train,d228,di227,6).
ind(train,d235,di227,18).
ind(train,d239,di227,3).
ind(train,d273,di227,3).
ind(train,d274,di227,9).
ind(train,d275,di227,6).
ind(train,d123,di227,3).
ind(train,d232,di232,1).
ind(train,d238,di232,2).
ind(train,d240,di232,1).
ind(train,d242,di232,1).
ind(train,d245,di232,1).
ind(train,d254,di232,1).
ind(train,d205,ring_size_4,2).
ind(train,d248,ring_size_4,1).
ind(train,d152,ring_size_4,1).
ind(train,d182,ring_size_4,1).
ind(train,d216,ring_size_4,2).
ind(train,d260,di260,4).
ind(train,d13,methanol,2).
ind(train,d240,methanol,1).
ind(train,d241,methanol,1).
ind(train,d284,methanol,1).
ind(train,d63,methanol,1).
ind(train,d78,methanol,3).
ind(train,d160,methanol,1).
ind(train,d164,methanol,1).
ind(train,d174,methanol,2).
ind(train,d181,methanol,1).
ind(train,d263,methanol,1).
ind(train,d267,methanol,4).
ind(train,d281,di281,4).
ind(train,d282,di281,1).
ind(train,d13,nitro,1).
ind(train,d16,nitro,1).
ind(train,d18,nitro,1).
ind(train,d19,nitro,1).
ind(train,d20,nitro,1).
ind(train,d21,nitro,1).
ind(train,d27,nitro,1).
ind(train,d28,nitro,1).
ind(train,d38,nitro,2).
ind(train,d39,nitro,1).
ind(train,d41,nitro,2).
ind(train,d46,nitro,1).
ind(train,d49,nitro,1).
ind(train,d50,nitro,1).
ind(train,d54,nitro,1).
ind(train,d55,nitro,1).
ind(train,d56,nitro,1).
ind(train,d57,nitro,1).
ind(train,d58,nitro,1).
ind(train,d281,nitro,4).
ind(train,d61,nitro,1).
ind(train,d63,nitro,1).
ind(train,d65,nitro,1).
ind(train,d71,nitro,1).
ind(train,d72,nitro,1).
ind(train,d73,nitro,1).
ind(train,d78,nitro,1).
ind(train,d80,nitro,1).
ind(train,d127,nitro,1).
ind(train,d131,nitro,1).
ind(train,d260,nitro,4).
ind(train,d282,nitro,1).
ashby_alert(train,d1,amino,[d1_17, d1_24, d1_25, d1_26]).
ashby_alert(train,d11,amino,[d11_5, d11_10, d11_17, d11_18]).
ashby_alert(train,d11,amino,[d11_3, d11_11, d11_19, d11_20]).
ashby_alert(train,d114,amino,[d114_5, d114_8, d114_13, d114_14]).
ashby_alert(train,d12,amino,[d12_5, d12_10, d12_12, d12_13]).
ashby_alert(train,d12,amino,[d12_3, d12_11, d12_14, d12_15]).
ashby_alert(train,d123,amino,[d123_19, d123_20, d123_24, d123_25]).
ashby_alert(train,d15,amino,[d15_6, d15_24, d15_26, d15_27]).
ashby_alert(train,d15,amino,[d15_17, d15_25, d15_28, d15_29]).
ashby_alert(train,d17,amino,[d17_2, d17_17, d17_21, d17_22]).
ashby_alert(train,d17,amino,[d17_13, d17_18, d17_19, d17_20]).
ashby_alert(train,d176,amino,[d176_11, d176_13, d176_17, d176_18]).
ashby_alert(train,d176,amino,[d176_14, d176_15, d176_19, d176_20]).
ashby_alert(train,d183,amino,[d183_51, d183_52, d183_54, d183_55]).
ashby_alert(train,d192,amino,[d192_5, d192_11, d192_19, d192_20]).
ashby_alert(train,d2,amino,[d2_16, d2_28, d2_29, d2_30]).
ashby_alert(train,d20,amino,[d20_5, d20_11, d20_19, d20_20]).
ashby_alert(train,d203,amino,[d203_5, d203_7, d203_10, d203_11]).
ashby_alert(train,d203,amino,[d203_3, d203_8, d203_14, d203_15]).
ashby_alert(train,d203,amino,[d203_1, d203_9, d203_12, d203_13]).
ashby_alert(train,d213,amino,[d213_5, d213_11, d213_13, d213_14]).
ashby_alert(train,d23_1,amino,[d23_1_14, d23_1_22, d23_1_26, d23_1_27]).
ashby_alert(train,d23_1,amino,[d23_1_1, d23_1_23, d23_1_24, d23_1_25]).
ashby_alert(train,d23_2,amino,[d23_2_17, d23_2_22, d23_2_24, d23_2_25]).
ashby_alert(train,d23_2,amino,[d23_2_15, d23_2_23, d23_2_26, d23_2_27]).
ashby_alert(train,d24,amino,[d24_14, d24_22, d24_26, d24_27]).
ashby_alert(train,d24,amino,[d24_1, d24_23, d24_24, d24_25]).
ashby_alert(train,d25,amino,[d25_5, d25_11, d25_16, d25_17]).
ashby_alert(train,d26,amino,[d26_5, d26_9, d26_22, d26_23]).
ashby_alert(train,d28,amino,[d28_10, d28_11, d28_16, d28_17]).
ashby_alert(train,d291,amino,[d291_2, d291_1, d291_10, d291_9]).
ashby_alert(train,d3,amino,[d3_18, d3_27, d3_28, d3_29]).
ashby_alert(train,d30,amino,[d30_5, d30_11, d30_13, d30_14]).
ashby_alert(train,d31,amino,[d31a_1, d31a_19, d31a_31, d31a_32]).
ashby_alert(train,d31,amino,[d31a_13, d31a_30, d31a_33, d31a_34]).
ashby_alert(train,d32,amino,[d32a_1, d32a_19, d32a_29, d32a_30]).
ashby_alert(train,d32,amino,[d32a_13, d32a_20, d32a_31, d32a_32]).
ashby_alert(train,d33,amino,[d33_5, d33_12, d33_13, d33_14]).
ashby_alert(train,d35,amino,[d35_5, d35_10, d35_16, d35_17]).
ashby_alert(train,d39,amino,[d39_5, d39_10, d39_18, d39_19]).
ashby_alert(train,d4,amino,[d4_5, d4_11, d4_12, d4_13]).
ashby_alert(train,d40,amino,[d40_14, d40_22, d40_28, d40_29]).
ashby_alert(train,d40,amino,[d40_1, d40_23, d40_26, d40_27]).
ashby_alert(train,d42,amino,[d42_5, d42_10, d42_16, d42_17]).
ashby_alert(train,d43,amino,[d43_16, d43_21, d43_23, d43_24]).
ashby_alert(train,d43,amino,[d43_13, d43_22, d43_25, d43_26]).
ashby_alert(train,d43,amino,[d43_5, d43_27, d43_29, d43_30]).
ashby_alert(train,d43,amino,[d43_2, d43_28, d43_31, d43_32]).
ashby_alert(train,d44,amino,[d44_5, d44_10, d44_20, d44_21]).
ashby_alert(train,d45,amino,[d45_5, d45_9, d45_13, d45_14]).
ashby_alert(train,d45,amino,[d45_2, d45_10, d45_15, d45_16]).
ashby_alert(train,d48,amino,[d48_3, d48_18, d48_25, d48_26]).
ashby_alert(train,d49,amino,[d49_5, d49_10, d49_13, d49_14]).
ashby_alert(train,d5,amino,[d5_5, d5_10, d5_13, d5_14]).
ashby_alert(train,d5,amino,[d5_3, d5_11, d5_15, d5_16]).
ashby_alert(train,d50,amino,[d50_4, d50_6, d50_10, d50_11]).
ashby_alert(train,d52,amino,[d52_13, d52_22, d52_23, d52_24]).
ashby_alert(train,d53,amino,[d53_3, d53_12, d53_16, d53_17]).
ashby_alert(train,d55,amino,[d55_5, d55_10, d55_15, d55_16]).
ashby_alert(train,d55,amino,[d55_2, d55_12, d55_17, d55_18]).
ashby_alert(train,d56,amino,[d56_4, d56_11, d56_15, d56_16]).
ashby_alert(train,d57,amino,[d57_4, d57_11, d57_15, d57_16]).
ashby_alert(train,d59,amino,[d59_5, d59_11, d59_17, d59_18]).
ashby_alert(train,d6,amino,[d6_5, d6_9, d6_11, d6_12]).
ashby_alert(train,d6,amino,[d6_4, d6_14, d6_15, d6_16]).
ashby_alert(train,d60,amino,[d60_5, d60_11, d60_13, d60_14]).
ashby_alert(train,d62,amino,[d62_1, d62_22, d62_24, d62_25]).
ashby_alert(train,d62,amino,[d62_16, d62_23, d62_26, d62_27]).
ashby_alert(train,d63,amino,[d63_1, d63_10, d63_22, d63_23]).
ashby_alert(train,d68,amino,[d68_5, d68_10, d68_13, d68_14]).
ashby_alert(train,d68,amino,[d68_2, d68_11, d68_15, d68_16]).
ashby_alert(train,d69,amino,[d69_5, d69_10, d69_16, d69_17]).
ashby_alert(train,d7,amino,[d7_6, d7_33, d7_35, d7_36]).
ashby_alert(train,d7,amino,[d7_11, d7_34, d7_37, d7_38]).
ashby_alert(train,d70,amino,[d70_5, d70_10, d70_21, d70_22]).
ashby_alert(train,d72,amino,[d72_5, d72_10, d72_16, d72_17]).
ashby_alert(train,d73,amino,[d73_5, d73_10, d73_13, d73_14]).
ashby_alert(train,d73,amino,[d73_4, d73_11, d73_15, d73_16]).
ashby_alert(train,d74,amino,[d74_15, d74_23, d74_25, d74_26]).
ashby_alert(train,d75,amino,[d75_6, d75_10, d75_16, d75_17]).
ashby_alert(train,d75,amino,[d75_4, d75_15, d75_18, d75_19]).
ashby_alert(train,d81,amino,[d81_5, d81_11, d81_13, d81_14]).
ashby_alert(train,d81,amino,[d81_2, d81_12, d81_15, d81_16]).
ashby_alert(train,d82,amino,[d82_1, d82_11, d82_25, d82_26]).
ashby_alert(train,d83,amino,[d83_4, d83_13, d83_14, d83_15]).
ashby_alert(train,d83,amino,[d83_1, d83_17, d83_18, d83_19]).
ashby_alert(train,d9,amino,[d9_5, d9_14, d9_16, d9_17]).
ashby_alert(train,d7,di7,[d7_31, d7_39, d7_40, d7_23, d7_24, d7_25, d7_20, d7_30, d7_28, d7_29]).
ashby_alert(train,d48,di8,[d48_5, d48_10, d48_17, d48_11, d48_12, d48_16, d48_15, d48_14, d48_13]).
ashby_alert(train,d54,di8,[d54_5, d54_17, d54_26, d54_20, d54_21, d54_25, d54_24, d54_23, d54_22]).
ashby_alert(train,d67,di8,[d67_5, d67_11, d67_24, d67_12, d67_13, d67_17, d67_16, d67_15, d67_14]).
ashby_alert(train,d8,di8,[d8_12, d8_28, d8_30, d8_29, d8_32, d8_31, d8_35, d8_34, d8_33]).
ashby_alert(train,d1,di10,[d1_24, d1_26, d1_25, d1_17]).
ashby_alert(train,d10,di10,[d10_12, d10_5, d10_14, d10_13]).
ashby_alert(train,d109,di10,[d109_20, d109_9, d109_27, d109_21]).
ashby_alert(train,d11,di10,[d11_10, d11_5, d11_18, d11_17]).
ashby_alert(train,d11,di10,[d11_11, d11_3, d11_20, d11_19]).
ashby_alert(train,d114,di10,[d114_8, d114_5, d114_14, d114_13]).
ashby_alert(train,d118,di10,[d118_17, d118_3, d118_21, d118_12]).
ashby_alert(train,d12,di10,[d12_10, d12_5, d12_13, d12_12]).
ashby_alert(train,d12,di10,[d12_11, d12_3, d12_15, d12_14]).
ashby_alert(train,d123,di10,[d123_18, d123_9, d123_22, d123_19]).
ashby_alert(train,d123,di10,[d123_20, d123_25, d123_24, d123_19]).
ashby_alert(train,d124,di10,[d124_1, d124_21, d124_20, d124_2]).
ashby_alert(train,d124,di10,[d124_3, d124_6, d124_5, d124_2]).
ashby_alert(train,d13,di10,[d13_10, d13_5, d13_27, d13_20]).
ashby_alert(train,d13,di10,[d13_12, d13_2, d13_17, d13_13]).
ashby_alert(train,d14,di10,[d14_12, d14_3, d14_25, d14_13]).
ashby_alert(train,d14,di10,[d14_13, d14_26, d14_14, d14_12]).
ashby_alert(train,d143,di10,[d143_10, d143_9, d143_4, d143_14]).
ashby_alert(train,d15,di10,[d15_24, d15_6, d15_27, d15_26]).
ashby_alert(train,d15,di10,[d15_25, d15_29, d15_28, d15_17]).
ashby_alert(train,d152,di10,[d152_17, d152_42, d152_18, d152_14]).
ashby_alert(train,d159,di10,[d159_19, d159_37, d159_20, d159_18]).
ashby_alert(train,d159,di10,[d159_41, d159_42, d159_18, d159_17]).
ashby_alert(train,d163,di10,[d163_3, d163_7, d163_6, d163_1]).
ashby_alert(train,d163,di10,[d163_14, d163_2, d163_18, d163_17]).
ashby_alert(train,d166,di10,[d166_9, d166_6, d166_19, d166_12]).
ashby_alert(train,d17,di10,[d17_17, d17_22, d17_21, d17_2]).
ashby_alert(train,d17,di10,[d17_18, d17_20, d17_19, d17_13]).
ashby_alert(train,d171,di10,[d171_16, d171_21, d171_17, d171_15]).
ashby_alert(train,d171,di10,[d171_18, d171_42, d171_29, d171_17]).
ashby_alert(train,d175,di10,[d175_16, d175_25, d175_19, d175_13]).
ashby_alert(train,d175,di10,[d175_21, d175_26, d175_22, d175_19]).
ashby_alert(train,d176,di10,[d176_13, d176_18, d176_17, d176_11]).
ashby_alert(train,d176,di10,[d176_15, d176_20, d176_19, d176_14]).
ashby_alert(train,d179,di10,[d179_18, d179_35, d179_19, d179_15]).
ashby_alert(train,d179,di10,[d179_21, d179_36, d179_22, d179_19]).
ashby_alert(train,d18,di10,[d18_8, d18_9, d18_14, d18_1]).
ashby_alert(train,d18,di10,[d18_10, d18_9, d18_15, d18_12]).
ashby_alert(train,d182,di10,[d182_18, d182_20, d182_19, d182_14]).
ashby_alert(train,d183,di10,[d183_52, d183_55, d183_54, d183_51]).
ashby_alert(train,d192,di10,[d192_11, d192_5, d192_20, d192_19]).
ashby_alert(train,d197,di10,[d197_23, d197_25, d197_24, d197_15]).
ashby_alert(train,d197,di10,[d197_24, d197_27, d197_23, d197_2]).
ashby_alert(train,d199,di10,[d199_12, d199_4, d199_24, d199_13]).
ashby_alert(train,d2,di10,[d2_28, d2_30, d2_29, d2_16]).
ashby_alert(train,d20,di10,[d20_11, d20_5, d20_20, d20_19]).
ashby_alert(train,d203,di10,[d203_7, d203_5, d203_11, d203_10]).
ashby_alert(train,d203,di10,[d203_8, d203_3, d203_15, d203_14]).
ashby_alert(train,d203,di10,[d203_9, d203_13, d203_12, d203_1]).
ashby_alert(train,d210,di10,[d210_15, d210_5, d210_27, d210_16]).
ashby_alert(train,d210,di10,[d210_17, d210_22, d210_18, d210_16]).
ashby_alert(train,d211,di10,[d211_19, d211_26, d211_20, d211_18]).
ashby_alert(train,d213,di10,[d213_11, d213_5, d213_14, d213_13]).
ashby_alert(train,d217,di10,[d217_16, d217_2, d217_15, d217_12]).
ashby_alert(train,d22,di10,[d22_22, d22_4, d22_25, d22_12]).
ashby_alert(train,d222,di10,[d222_9, d222_5, d222_14, d222_10]).
ashby_alert(train,d222,di10,[d222_28, d222_34, d222_29, d222_27]).
ashby_alert(train,d223,di10,[d223_12, d223_5, d223_16, d223_13]).
ashby_alert(train,d231,di10,[d231_52, d231_60, d231_53, d231_3]).
ashby_alert(train,d23_1,di10,[d23_1_22, d23_1_27, d23_1_26, d23_1_14]).
ashby_alert(train,d23_1,di10,[d23_1_23, d23_1_25, d23_1_24, d23_1_1]).
ashby_alert(train,d23_2,di10,[d23_2_22, d23_2_25, d23_2_24, d23_2_17]).
ashby_alert(train,d23_2,di10,[d23_2_23, d23_2_27, d23_2_26, d23_2_15]).
ashby_alert(train,d24,di10,[d24_22, d24_27, d24_26, d24_14]).
ashby_alert(train,d24,di10,[d24_23, d24_25, d24_24, d24_1]).
ashby_alert(train,d246,di10,[d246_11, d246_5, d246_19, d246_12]).
ashby_alert(train,d246,di10,[d246_82, d246_90, d246_83, d246_74]).
ashby_alert(train,d25,di10,[d25_11, d25_5, d25_17, d25_16]).
ashby_alert(train,d256,di10,[d256_11, d256_13, d256_12, d256_10]).
ashby_alert(train,d258,di10,[d258_23, d258_31, d258_24, d258_21]).
ashby_alert(train,d26,di10,[d26_9, d26_5, d26_23, d26_22]).
ashby_alert(train,d265,di10,[d265_11, d265_8, d265_18, d265_12]).
ashby_alert(train,d27,di10,[d27_1, d27_8, d27_5, d27_2]).
ashby_alert(train,d27,di10,[d27_4, d27_5, d27_3, d27_10]).
ashby_alert(train,d277,di10,[d277_2, d277_4, d277_3, d277_1]).
ashby_alert(train,d277,di10,[d277_27, d277_29, d277_28, d277_20]).
ashby_alert(train,d278,di10,[d278_2, d278_4, d278_3, d278_1]).
ashby_alert(train,d278,di10,[d278_25, d278_27, d278_26, d278_18]).
ashby_alert(train,d279,di10,[d279_2, d279_4, d279_3, d279_1]).
ashby_alert(train,d28,di10,[d28_9, d28_8, d28_15, d28_10]).
ashby_alert(train,d28,di10,[d28_11, d28_17, d28_16, d28_10]).
ashby_alert(train,d280,di10,[d280_2, d280_4, d280_3, d280_1]).
ashby_alert(train,d284,di10,[d284_8, d284_9, d284_5, d284_13]).
ashby_alert(train,d290,di10,[d290_12, d290_16, d290_13, d290_11]).
ashby_alert(train,d291,di10,[d291_1, d291_9, d291_2, d291_10]).
ashby_alert(train,d292,di10,[d292_13, d292_9, d292_15, d292_14]).
ashby_alert(train,d3,di10,[d3_27, d3_29, d3_28, d3_18]).
ashby_alert(train,d30,di10,[d30_11, d30_5, d30_14, d30_13]).
ashby_alert(train,d31,di10,[d31a_19, d31a_32, d31a_31, d31a_1]).
ashby_alert(train,d31,di10,[d31a_30, d31a_34, d31a_33, d31a_13]).
ashby_alert(train,d32,di10,[d32a_19, d32a_30, d32a_29, d32a_1]).
ashby_alert(train,d32,di10,[d32a_20, d32a_32, d32a_31, d32a_13]).
ashby_alert(train,d33,di10,[d33_12, d33_5, d33_14, d33_13]).
ashby_alert(train,d35,di10,[d35_10, d35_5, d35_17, d35_16]).
ashby_alert(train,d39,di10,[d39_10, d39_5, d39_19, d39_18]).
ashby_alert(train,d4,di10,[d4_11, d4_5, d4_13, d4_12]).
ashby_alert(train,d40,di10,[d40_22, d40_29, d40_28, d40_14]).
ashby_alert(train,d40,di10,[d40_23, d40_27, d40_26, d40_1]).
ashby_alert(train,d41,di10,[d41_13, d41_5, d41_15, d41_14]).
ashby_alert(train,d42,di10,[d42_10, d42_5, d42_17, d42_16]).
ashby_alert(train,d43,di10,[d43_21, d43_24, d43_23, d43_16]).
ashby_alert(train,d43,di10,[d43_22, d43_26, d43_25, d43_13]).
ashby_alert(train,d43,di10,[d43_27, d43_5, d43_30, d43_29]).
ashby_alert(train,d43,di10,[d43_28, d43_32, d43_31, d43_2]).
ashby_alert(train,d44,di10,[d44_10, d44_5, d44_21, d44_20]).
ashby_alert(train,d45,di10,[d45_9, d45_5, d45_14, d45_13]).
ashby_alert(train,d45,di10,[d45_10, d45_2, d45_16, d45_15]).
ashby_alert(train,d48,di10,[d48_10, d48_5, d48_17, d48_11]).
ashby_alert(train,d48,di10,[d48_18, d48_3, d48_26, d48_25]).
ashby_alert(train,d49,di10,[d49_10, d49_5, d49_14, d49_13]).
ashby_alert(train,d5,di10,[d5_10, d5_5, d5_14, d5_13]).
ashby_alert(train,d5,di10,[d5_11, d5_3, d5_16, d5_15]).
ashby_alert(train,d50,di10,[d50_6, d50_4, d50_11, d50_10]).
ashby_alert(train,d52,di10,[d52_22, d52_24, d52_23, d52_13]).
ashby_alert(train,d53,di10,[d53_12, d53_3, d53_17, d53_16]).
ashby_alert(train,d54,di10,[d54_17, d54_5, d54_26, d54_20]).
ashby_alert(train,d55,di10,[d55_10, d55_5, d55_16, d55_15]).
ashby_alert(train,d55,di10,[d55_12, d55_2, d55_18, d55_17]).
ashby_alert(train,d56,di10,[d56_11, d56_4, d56_16, d56_15]).
ashby_alert(train,d57,di10,[d57_11, d57_4, d57_16, d57_15]).
ashby_alert(train,d58,di10,[d58_35, d58_4, d58_38, d58_10]).
ashby_alert(train,d58,di10,[d58_36, d58_39, d58_19, d58_13]).
ashby_alert(train,d59,di10,[d59_11, d59_5, d59_18, d59_17]).
ashby_alert(train,d6,di10,[d6_9, d6_5, d6_12, d6_11]).
ashby_alert(train,d6,di10,[d6_14, d6_4, d6_16, d6_15]).
ashby_alert(train,d60,di10,[d60_11, d60_5, d60_14, d60_13]).
ashby_alert(train,d62,di10,[d62_22, d62_25, d62_24, d62_1]).
ashby_alert(train,d62,di10,[d62_23, d62_27, d62_26, d62_16]).
ashby_alert(train,d63,di10,[d63_10, d63_23, d63_22, d63_1]).
ashby_alert(train,d63,di10,[d63_12, d63_4, d63_24, d63_13]).
ashby_alert(train,d64,di10,[d64_29, d64_30, d64_23, d64_13]).
ashby_alert(train,d66,di10,[d66_51, d66_63, d66_56, d66_19]).
ashby_alert(train,d67,di10,[d67_11, d67_5, d67_24, d67_12]).
ashby_alert(train,d68,di10,[d68_10, d68_5, d68_14, d68_13]).
ashby_alert(train,d68,di10,[d68_11, d68_2, d68_16, d68_15]).
ashby_alert(train,d69,di10,[d69_10, d69_5, d69_17, d69_16]).
ashby_alert(train,d7,di10,[d7_33, d7_6, d7_36, d7_35]).
ashby_alert(train,d7,di10,[d7_34, d7_38, d7_37, d7_11]).
ashby_alert(train,d70,di10,[d70_10, d70_5, d70_22, d70_21]).
ashby_alert(train,d72,di10,[d72_10, d72_5, d72_17, d72_16]).
ashby_alert(train,d73,di10,[d73_10, d73_5, d73_14, d73_13]).
ashby_alert(train,d73,di10,[d73_11, d73_4, d73_16, d73_15]).
ashby_alert(train,d74,di10,[d74_22, d74_4, d74_24, d74_12]).
ashby_alert(train,d74,di10,[d74_23, d74_26, d74_25, d74_15]).
ashby_alert(train,d75,di10,[d75_10, d75_6, d75_17, d75_16]).
ashby_alert(train,d75,di10,[d75_15, d75_4, d75_19, d75_18]).
ashby_alert(train,d77,di10,[d77_26, d77_45, d77_34, d77_25]).
ashby_alert(train,d77,di10,[d77_58, d77_70, d77_59, d77_55]).
ashby_alert(train,d78,di10,[d78_10, d78_5, d78_18, d78_11]).
ashby_alert(train,d78,di10,[d78_26, d78_36, d78_27, d78_2]).
ashby_alert(train,d79,di10,[d79_18, d79_26, d79_19, d79_14]).
ashby_alert(train,d8,di10,[d8_28, d8_30, d8_29, d8_12]).
ashby_alert(train,d81,di10,[d81_11, d81_5, d81_14, d81_13]).
ashby_alert(train,d81,di10,[d81_12, d81_2, d81_16, d81_15]).
ashby_alert(train,d82,di10,[d82_11, d82_26, d82_25, d82_1]).
ashby_alert(train,d82,di10,[d82_29, d82_31, d82_30, d82_12]).
ashby_alert(train,d83,di10,[d83_13, d83_4, d83_15, d83_14]).
ashby_alert(train,d83,di10,[d83_17, d83_19, d83_18, d83_1]).
ashby_alert(train,d9,di10,[d9_14, d9_5, d9_17, d9_16]).
ashby_alert(train,d95,di10,[d95_12, d95_5, d95_24, d95_13]).
ashby_alert(train,d95,di10,[d95_14, d95_19, d95_15, d95_13]).
ashby_alert(train,d13,di13,[d13_2, d13_12, d13_17, d13_13, d13_16, d13_15]).
ashby_alert(train,d14,di14,[d14_13, d14_12, d14_26, d14_25]).
ashby_alert(train,d22,di22,[d22_15, d22_23, d22_24]).
ashby_alert(train,d212,di23,[d212_24, d212_23, d212_22, d212_3]).
ashby_alert(train,d221,di23,[d221_23, d221_22, d221_21, d221_13]).
ashby_alert(train,d23_2,di23,[d23_2_12, d23_2_21, d23_2_20, d23_2_4]).
ashby_alert(train,d34,di23,[d34_12, d34_24, d34_23, d34_4]).
ashby_alert(train,d36,di23,[d36a_9, d36a_26, d36a_25, d36a_4]).
ashby_alert(train,d47,di23,[d47_12, d47_29, d47_28, d47_4]).
ashby_alert(train,d76,di23,[d76_17, d76_27, d76_26, d76_3]).
ashby_alert(train,d8,di23,[d8_15, d8_21, d8_20, d8_3]).
ashby_alert(train,d28,di28,[d28_9, d28_8, d28_15]).
ashby_alert(train,d29,cyanate,[d29a_4, d29a_14, d29a_16]).
ashby_alert(train,d29,cyanate,[d29a_2, d29a_15, d29a_18]).
ashby_alert(train,d29,cyanate,[d29b_6, d29b_14, d29b_16]).
ashby_alert(train,d29,cyanate,[d29b_4, d29b_15, d29b_18]).
ashby_alert(train,d37,cyanate,[d37_1, d37_24, d37_31]).
ashby_alert(train,d37,cyanate,[d37_13, d37_30, d37_33]).
ashby_alert(train,d210,di48,[d210_5, d210_15, d210_27]).
ashby_alert(train,d223,di48,[d223_5, d223_12, d223_16]).
ashby_alert(train,d48,di48,[d48_5, d48_10, d48_17]).
ashby_alert(train,d54,di48,[d54_5, d54_17, d54_26]).
ashby_alert(train,d67,di48,[d67_5, d67_11, d67_24]).
ashby_alert(train,d77,di48,[d77_34, d77_26, d77_45]).
ashby_alert(train,d77,di48,[d77_59, d77_58, d77_70]).
ashby_alert(train,d8,di48,[d8_12, d8_28, d8_30]).
ashby_alert(train,d95,di48,[d95_5, d95_12, d95_24]).
ashby_alert(train,d51,di51,[d51_13, d51_15, d51_11, d51_5, d51_6, d51_1]).
ashby_alert(train,d51,di51,[d51_14, d51_16, d51_12, d51_2, d51_3, d51_4]).
ashby_alert(train,d257,ethoxy,[d257_6, d257_2, d257_8, d257_7, d257_1, d257_5, d257_4, d257_3]).
ashby_alert(train,d257,ethoxy,[d257_10, d257_11, d257_14, d257_13, d257_12, d257_17, d257_16, d257_15]).
ashby_alert(train,d264,ethoxy,[d264_6, d264_2, d264_8, d264_7, d264_1, d264_5, d264_4, d264_3]).
ashby_alert(train,d264,ethoxy,[d264_10, d264_11, d264_14, d264_13, d264_12, d264_17, d264_16, d264_15]).
ashby_alert(train,d266,ethoxy,[d266_17, d266_18, d266_21, d266_20, d266_19, d266_24, d266_23, d266_22]).
ashby_alert(train,d266,ethoxy,[d266_26, d266_27, d266_30, d266_29, d266_28, d266_33, d266_32, d266_31]).
ashby_alert(train,d269,ethoxy,[d269_6, d269_2, d269_8, d269_7, d269_1, d269_5, d269_4, d269_3]).
ashby_alert(train,d269,ethoxy,[d269_10, d269_11, d269_14, d269_13, d269_12, d269_17, d269_16, d269_15]).
ashby_alert(train,d270,ethoxy,[d270_19, d270_21, d270_24, d270_23, d270_22, d270_27, d270_26, d270_25]).
ashby_alert(train,d270,ethoxy,[d270_20, d270_28, d270_31, d270_30, d270_29, d270_34, d270_33, d270_32]).
ashby_alert(train,d270,ethoxy,[d270_35, d270_36, d270_39, d270_38, d270_37, d270_42, d270_41, d270_40]).
ashby_alert(train,d270,ethoxy,[d270_43, d270_44, d270_47, d270_46, d270_45, d270_50, d270_49, d270_48]).
ashby_alert(train,d271,ethoxy,[d271_18, d271_19, d271_22, d271_21, d271_20, d271_25, d271_24, d271_23]).
ashby_alert(train,d271,ethoxy,[d271_26, d271_27, d271_30, d271_29, d271_28, d271_33, d271_32, d271_31]).
ashby_alert(train,d283,ethoxy,[d283_8, d283_9, d283_12, d283_11, d283_10, d283_15, d283_14, d283_13]).
ashby_alert(train,d61,ethoxy,[d61_14, d61_26, d61_29, d61_28, d61_27, d61_32, d61_31, d61_30]).
ashby_alert(train,d61,ethoxy,[d61_18, d61_19, d61_22, d61_21, d61_20, d61_25, d61_24, d61_23]).
ashby_alert(train,d66,ethoxy,[d66_22, d66_44, d66_47, d66_46, d66_45, d66_50, d66_49, d66_48]).
ashby_alert(train,d97,ethoxy,[d97_24, d97_26, d97_29, d97_28, d97_27, d97_32, d97_31, d97_30]).
ashby_alert(train,d118,di64,[d118_17, d118_3, d118_12, d118_21]).
ashby_alert(train,d22,di64,[d22_22, d22_4, d22_12, d22_25]).
ashby_alert(train,d58,di64,[d58_35, d58_4, d58_10, d58_38]).
ashby_alert(train,d58,di64,[d58_36, d58_19, d58_13, d58_39]).
ashby_alert(train,d64,di64,[d64_29, d64_23, d64_13, d64_30]).
ashby_alert(train,d74,di64,[d74_22, d74_4, d74_12, d74_24]).
ashby_alert(train,d143,di66,[d143_9, d143_10, d143_14, d143_17, d143_16, d143_15, d143_20, d143_19, d143_18]).
ashby_alert(train,d143,di66,[d143_4, d143_10, d143_14, d143_17, d143_16, d143_15, d143_20, d143_19, d143_18]).
ashby_alert(train,d18,di66,[d18_9, d18_10, d18_12, d18_17, d18_16, d18_13, d18_20, d18_19, d18_18]).
ashby_alert(train,d258,di66,[d258_21, d258_23, d258_24, d258_27, d258_26, d258_25, d258_30, d258_29, d258_28]).
ashby_alert(train,d258,di66,[d258_21, d258_23, d258_31, d258_34, d258_33, d258_32, d258_37, d258_36, d258_35]).
ashby_alert(train,d258,di66,[d258_24, d258_23, d258_31, d258_34, d258_33, d258_32, d258_37, d258_36, d258_35]).
ashby_alert(train,d258,di66,[d258_31, d258_23, d258_24, d258_27, d258_26, d258_25, d258_30, d258_29, d258_28]).
ashby_alert(train,d66,di66,[d66_14, d66_36, d66_37, d66_40, d66_39, d66_38, d66_43, d66_42, d66_41]).
ashby_alert(train,d66,di66,[d66_19, d66_51, d66_56, d66_59, d66_58, d66_57, d66_62, d66_61, d66_60]).
ashby_alert(train,d48,di67a,[d48_5, d48_10, d48_11, d48_16, d48_12, d48_15, d48_14, d48_13, d48_17]).
ashby_alert(train,d54,di67a,[d54_5, d54_17, d54_20, d54_25, d54_21, d54_24, d54_23, d54_22, d54_26]).
ashby_alert(train,d67,di67a,[d67_5, d67_11, d67_12, d67_17, d67_13, d67_16, d67_15, d67_14, d67_24]).
ashby_alert(train,d8,di67a,[d8_12, d8_28, d8_29, d8_31, d8_32, d8_35, d8_34, d8_33, d8_30]).
ashby_alert(train,d101,halide10,[d101_22, d101_2]).
ashby_alert(train,d101,halide10,[d101_15, d101_14]).
ashby_alert(train,d101,halide10,[d101_16, d101_14]).
ashby_alert(train,d101,halide10,[d101_7, d101_17]).
ashby_alert(train,d101,halide10,[d101_5, d101_19]).
ashby_alert(train,d102,halide10,[d102_3, d102_1]).
ashby_alert(train,d102,halide10,[d102_4, d102_1]).
ashby_alert(train,d102,halide10,[d102_5, d102_1]).
ashby_alert(train,d102,halide10,[d102_6, d102_2]).
ashby_alert(train,d102,halide10,[d102_7, d102_2]).
ashby_alert(train,d103,halide10,[d103_6, d103_1]).
ashby_alert(train,d103,halide10,[d103_7, d103_1]).
ashby_alert(train,d103,halide10,[d103_8, d103_1]).
ashby_alert(train,d103,halide10,[d103_3, d103_2]).
ashby_alert(train,d104,halide10,[d104_3, d104_1]).
ashby_alert(train,d104,halide10,[d104_4, d104_1]).
ashby_alert(train,d104,halide10,[d104_6, d104_2]).
ashby_alert(train,d104,halide10,[d104_7, d104_2]).
ashby_alert(train,d106,halide10,[d106_22, d106_2]).
ashby_alert(train,d106,halide10,[d106_19, d106_15]).
ashby_alert(train,d106,halide10,[d106_20, d106_15]).
ashby_alert(train,d106,halide10,[d106_5, d106_21]).
ashby_alert(train,d107,halide10,[d107c_53, d107c_1]).
ashby_alert(train,d107,halide10,[d107c_58, d107c_16]).
ashby_alert(train,d107,halide10,[d107c_60, d107c_19]).
ashby_alert(train,d107,halide10,[d107c_62, d107c_21]).
ashby_alert(train,d107,halide10,[d107c_64, d107c_28]).
ashby_alert(train,d107,halide10,[d107c_66, d107c_32]).
ashby_alert(train,d107,halide10,[d107c_68, d107c_39]).
ashby_alert(train,d107,halide10,[d107c_70, d107c_42]).
ashby_alert(train,d107,halide10,[d107c_6, d107c_56]).
ashby_alert(train,d107,halide10,[d107c_6, d107c_57]).
ashby_alert(train,d107,halide10,[d107d_54, d107d_17]).
ashby_alert(train,d107,halide10,[d107d_55, d107d_17]).
ashby_alert(train,d107,halide10,[d107d_56, d107d_17]).
ashby_alert(train,d107,halide10,[d107d_57, d107d_34]).
ashby_alert(train,d107,halide10,[d107d_58, d107d_34]).
ashby_alert(train,d107,halide10,[d107d_59, d107d_34]).
ashby_alert(train,d107,halide10,[d107d_60, d107d_43]).
ashby_alert(train,d107,halide10,[d107d_66, d107d_44]).
ashby_alert(train,d107,halide10,[d107d_63, d107d_45]).
ashby_alert(train,d107,halide10,[d107d_69, d107d_51]).
ashby_alert(train,d108,halide10,[d108_24, d108_23]).
ashby_alert(train,d108,halide10,[d108_25, d108_23]).
ashby_alert(train,d108,halide10,[d108_26, d108_23]).
ashby_alert(train,d112,halide10,[d112_22, d112_10]).
ashby_alert(train,d112,halide10,[d112_20, d112_19]).
ashby_alert(train,d112,halide10,[d112_21, d112_19]).
ashby_alert(train,d112,halide10,[d112_9, d112_23]).
ashby_alert(train,d112,halide10,[d112_8, d112_25]).
ashby_alert(train,d112,halide10,[d112_7, d112_27]).
ashby_alert(train,d113,halide10,[d113_38, d113_36]).
ashby_alert(train,d113,halide10,[d113_39, d113_36]).
ashby_alert(train,d116,halide10,[d116_24, d116_22]).
ashby_alert(train,d116,halide10,[d116_25, d116_22]).
ashby_alert(train,d119,halide10,[d119_18, d119_15]).
ashby_alert(train,d121,halide10,[d121_5, d121_15]).
ashby_alert(train,d121,halide10,[d121_2, d121_16]).
ashby_alert(train,d121,halide10,[d121_26, d121_25]).
ashby_alert(train,d121,halide10,[d121_27, d121_25]).
ashby_alert(train,d122,halide10,[d122_24, d122_22]).
ashby_alert(train,d122,halide10,[d122_25, d122_22]).
ashby_alert(train,d122,halide10,[d122_26, d122_22]).
ashby_alert(train,d123,halide10,[d123_6, d123_10]).
ashby_alert(train,d125,halide10,[d125_24, d125_22]).
ashby_alert(train,d125,halide10,[d125_25, d125_22]).
ashby_alert(train,d125,halide10,[d125_26, d125_22]).
ashby_alert(train,d126,halide10,[d126_12, d126_1]).
ashby_alert(train,d126,halide10,[d126_9, d126_4]).
ashby_alert(train,d126,halide10,[d126_8, d126_5]).
ashby_alert(train,d126,halide10,[d126_7, d126_6]).
ashby_alert(train,d126,halide10,[d126_2, d126_10]).
ashby_alert(train,d126,halide10,[d126_3, d126_11]).
ashby_alert(train,d128,halide10,[d128_5, d128_4]).
ashby_alert(train,d128,halide10,[d128_27, d128_10]).
ashby_alert(train,d128,halide10,[d128_19, d128_18]).
ashby_alert(train,d128,halide10,[d128_20, d128_18]).
ashby_alert(train,d128,halide10,[d128_6, d128_21]).
ashby_alert(train,d128,halide10,[d128_3, d128_22]).
ashby_alert(train,d226,halide10,[d226_12, d226_11]).
ashby_alert(train,d227,halide10,[d227_2, d227_1]).
ashby_alert(train,d227,halide10,[d227_7, d227_6]).
ashby_alert(train,d227,halide10,[d227_3, d227_10]).
ashby_alert(train,d228,halide10,[d228_2, d228_1]).
ashby_alert(train,d228,halide10,[d228_6, d228_3]).
ashby_alert(train,d229,halide10,[d229_3, d229_1]).
ashby_alert(train,d229,halide10,[d229_6, d229_2]).
ashby_alert(train,d230,halide10,[d230_7, d230_5]).
ashby_alert(train,d231,halide10,[d231_57, d231_54]).
ashby_alert(train,d231,halide10,[d231_64, d231_61]).
ashby_alert(train,d235,halide10,[d235_8, d235_5]).
ashby_alert(train,d235,halide10,[d235_9, d235_11]).
ashby_alert(train,d235,halide10,[d235_25, d235_18]).
ashby_alert(train,d235,halide10,[d235_22, d235_21]).
ashby_alert(train,d235,halide10,[d235_31, d235_27]).
ashby_alert(train,d235,halide10,[d235_33, d235_30]).
ashby_alert(train,d236,halide10,[d236_9, d236_10]).
ashby_alert(train,d239,halide10,[d239_3, d239_2]).
ashby_alert(train,d241,halide10,[d241_9, d241_7]).
ashby_alert(train,d243,halide10,[d243_2, d243_1]).
ashby_alert(train,d243,halide10,[d243_19, d243_17]).
ashby_alert(train,d246,halide10,[d246_16, d246_13]).
ashby_alert(train,d246,halide10,[d246_23, d246_20]).
ashby_alert(train,d246,halide10,[d246_87, d246_84]).
ashby_alert(train,d246,halide10,[d246_94, d246_91]).
ashby_alert(train,d249,halide10,[d249_6, d249_28]).
ashby_alert(train,d250,halide10,[d250_2, d250_1]).
ashby_alert(train,d250,halide10,[d250_6, d250_3]).
ashby_alert(train,d251,halide10,[d251_8, d251_5]).
ashby_alert(train,d251,halide10,[d251_16, d251_13]).
ashby_alert(train,d251,halide10,[d251_24, d251_21]).
ashby_alert(train,d255,halide10,[d255_9, d255_6]).
ashby_alert(train,d259,halide10,[d259_14, d259_13]).
ashby_alert(train,d261,halide10,[d261_9, d261_6]).
ashby_alert(train,d263,halide10,[d263_2, d263_1]).
ashby_alert(train,d268,halide10,[d268_12, d268_11]).
ashby_alert(train,d272,halide10,[d272_2, d272_1]).
ashby_alert(train,d272,halide10,[d272_3, d272_1]).
ashby_alert(train,d273,halide10,[d273_2, d273_1]).
ashby_alert(train,d273,halide10,[d273_3, d273_1]).
ashby_alert(train,d273,halide10,[d273_4, d273_1]).
ashby_alert(train,d274,halide10,[d274_2, d274_1]).
ashby_alert(train,d274,halide10,[d274_3, d274_1]).
ashby_alert(train,d274,halide10,[d274_4, d274_1]).
ashby_alert(train,d275,halide10,[d275_2, d275_1]).
ashby_alert(train,d275,halide10,[d275_3, d275_1]).
ashby_alert(train,d275,halide10,[d275_4, d275_1]).
ashby_alert(train,d276,halide10,[d276_2, d276_1]).
ashby_alert(train,d276,halide10,[d276_3, d276_1]).
ashby_alert(train,d276,halide10,[d276_4, d276_1]).
ashby_alert(train,d290,halide10,[d290_18, d290_17]).
ashby_alert(train,d290,halide10,[d290_19, d290_17]).
ashby_alert(train,d290,halide10,[d290_20, d290_17]).
ashby_alert(train,d67,halide10,[d67_20, d67_19]).
ashby_alert(train,d84,halide10,[d84a_23, d84a_1]).
ashby_alert(train,d84,halide10,[d84a_34, d84a_11]).
ashby_alert(train,d84,halide10,[d84a_30, d84a_14]).
ashby_alert(train,d84,halide10,[d84a_31, d84a_14]).
ashby_alert(train,d84,halide10,[d84a_32, d84a_15]).
ashby_alert(train,d84,halide10,[d84a_33, d84a_15]).
ashby_alert(train,d84,halide10,[d84a_6, d84a_26]).
ashby_alert(train,d84,halide10,[d84a_9, d84a_28]).
ashby_alert(train,d84,halide10,[d84b_36, d84b_15]).
ashby_alert(train,d84,halide10,[d84b_30, d84b_16]).
ashby_alert(train,d84,halide10,[d84b_31, d84b_16]).
ashby_alert(train,d84,halide10,[d84b_32, d84b_16]).
ashby_alert(train,d84,halide10,[d84b_33, d84b_17]).
ashby_alert(train,d84,halide10,[d84b_4, d84b_24]).
ashby_alert(train,d84,halide10,[d84b_4, d84b_25]).
ashby_alert(train,d84,halide10,[d84b_5, d84b_27]).
ashby_alert(train,d89,halide10,[d89_7, d89_10]).
ashby_alert(train,d89,halide10,[d89_7, d89_11]).
ashby_alert(train,d92,halide10,[d92_3, d92_1]).
ashby_alert(train,d92,halide10,[d92_4, d92_1]).
ashby_alert(train,d92,halide10,[d92_5, d92_1]).
ashby_alert(train,d92,halide10,[d92_6, d92_2]).
ashby_alert(train,d92,halide10,[d92_7, d92_2]).
ashby_alert(train,d92,halide10,[d92_8, d92_2]).
ashby_alert(train,d94,halide10,[d94_6, d94_1]).
ashby_alert(train,d94,halide10,[d94_3, d94_2]).
ashby_alert(train,d94,halide10,[d94_4, d94_2]).
ashby_alert(train,d96,halide10,[d96_15, d96_14]).
ashby_alert(train,d96,halide10,[d96_16, d96_14]).
ashby_alert(train,d96,halide10,[d96_8, d96_17]).
ashby_alert(train,d96,halide10,[d96_7, d96_19]).
ashby_alert(train,d96,halide10,[d96_3, d96_21]).
ashby_alert(train,d96,halide10,[d96_6, d96_24]).
ashby_alert(train,d137,methoxy,[d137_45, d137_46, d137_49, d137_48, d137_47]).
ashby_alert(train,d150,methoxy,[d150_12, d150_13, d150_16, d150_15, d150_14]).
ashby_alert(train,d150,methoxy,[d150_18, d150_19, d150_22, d150_21, d150_20]).
ashby_alert(train,d233,methoxy,[d233_18, d233_19, d233_22, d233_21, d233_20]).
ashby_alert(train,d233,methoxy,[d233_23, d233_24, d233_27, d233_26, d233_25]).
ashby_alert(train,d234,methoxy,[d234_2, d234_1, d234_5, d234_4, d234_3]).
ashby_alert(train,d234,methoxy,[d234_7, d234_13, d234_16, d234_15, d234_14]).
ashby_alert(train,d234,methoxy,[d234_8, d234_9, d234_12, d234_11, d234_10]).
ashby_alert(train,d237,methoxy,[d237_3, d237_9, d237_12, d237_11, d237_10]).
ashby_alert(train,d237,methoxy,[d237_4, d237_5, d237_8, d237_7, d237_6]).
ashby_alert(train,d244,methoxy,[d244_3, d244_6, d244_9, d244_8, d244_7]).
ashby_alert(train,d244,methoxy,[d244_5, d244_10, d244_13, d244_12, d244_11]).
ashby_alert(train,d247,methoxy,[d247_16, d247_22, d247_25, d247_24, d247_23]).
ashby_alert(train,d247,methoxy,[d247_17, d247_18, d247_21, d247_20, d247_19]).
ashby_alert(train,d253,methoxy,[d253_7, d253_8, d253_9, d253_11, d253_10]).
ashby_alert(train,d253,methoxy,[d253_12, d253_13, d253_16, d253_15, d253_14]).
ashby_alert(train,d256,methoxy,[d256_18, d256_25, d256_28, d256_27, d256_26]).
ashby_alert(train,d256,methoxy,[d256_19, d256_20, d256_23, d256_22, d256_21]).
ashby_alert(train,d258,methoxy,[d258_3, d258_4, d258_7, d258_6, d258_5]).
ashby_alert(train,d258,methoxy,[d258_8, d258_9, d258_12, d258_11, d258_10]).
ashby_alert(train,d265,methoxy,[d265_2, d265_1, d265_21, d265_20, d265_19]).
ashby_alert(train,d265,methoxy,[d265_4, d265_5, d265_24, d265_23, d265_22]).
ashby_alert(train,d266,methoxy,[d266_2, d266_1, d266_13, d266_12, d266_11]).
ashby_alert(train,d266,methoxy,[d266_4, d266_5, d266_16, d266_15, d266_14]).
ashby_alert(train,d271,methoxy,[d271_2, d271_1, d271_5, d271_4, d271_3]).
ashby_alert(train,d271,methoxy,[d271_9, d271_10, d271_13, d271_12, d271_11]).
ashby_alert(train,d285,methoxy,[d285_10, d285_11, d285_14, d285_13, d285_12]).
ashby_alert(train,d291,methoxy,[d291_3, d291_4, d291_7, d291_6, d291_5]).
ashby_alert(train,d71,methoxy,[d71_14, d71_23, d71_26, d71_25, d71_24]).
ashby_alert(train,d71,methoxy,[d71_15, d71_16, d71_19, d71_18, d71_17]).
ashby_alert(train,d123,di227,[d123_6, d123_9, d123_11, d123_10]).
ashby_alert(train,d123,di227,[d123_6, d123_9, d123_2, d123_10]).
ashby_alert(train,d123,di227,[d123_6, d123_2, d123_11, d123_10]).
ashby_alert(train,d227,di227,[d227_2, d227_4, d227_3, d227_1]).
ashby_alert(train,d227,di227,[d227_2, d227_5, d227_3, d227_1]).
ashby_alert(train,d227,di227,[d227_2, d227_5, d227_4, d227_1]).
ashby_alert(train,d227,di227,[d227_3, d227_6, d227_11, d227_10]).
ashby_alert(train,d227,di227,[d227_3, d227_6, d227_2, d227_10]).
ashby_alert(train,d227,di227,[d227_3, d227_2, d227_11, d227_10]).
ashby_alert(train,d228,di227,[d228_1, d228_4, d228_3, d228_2]).
ashby_alert(train,d228,di227,[d228_1, d228_5, d228_3, d228_2]).
ashby_alert(train,d228,di227,[d228_1, d228_5, d228_4, d228_2]).
ashby_alert(train,d228,di227,[d228_3, d228_7, d228_1, d228_6]).
ashby_alert(train,d228,di227,[d228_3, d228_8, d228_7, d228_6]).
ashby_alert(train,d228,di227,[d228_3, d228_8, d228_1, d228_6]).
ashby_alert(train,d235,di227,[d235_5, d235_9, d235_10, d235_8]).
ashby_alert(train,d235,di227,[d235_5, d235_9, d235_4, d235_8]).
ashby_alert(train,d235,di227,[d235_5, d235_4, d235_10, d235_8]).
ashby_alert(train,d235,di227,[d235_9, d235_13, d235_12, d235_11]).
ashby_alert(train,d235,di227,[d235_9, d235_5, d235_12, d235_11]).
ashby_alert(train,d235,di227,[d235_9, d235_5, d235_13, d235_11]).
ashby_alert(train,d235,di227,[d235_18, d235_21, d235_15, d235_25]).
ashby_alert(train,d235,di227,[d235_18, d235_26, d235_21, d235_25]).
ashby_alert(train,d235,di227,[d235_18, d235_26, d235_15, d235_25]).
ashby_alert(train,d235,di227,[d235_21, d235_23, d235_18, d235_22]).
ashby_alert(train,d235,di227,[d235_21, d235_24, d235_23, d235_22]).
ashby_alert(train,d235,di227,[d235_21, d235_24, d235_18, d235_22]).
ashby_alert(train,d235,di227,[d235_27, d235_30, d235_17, d235_31]).
ashby_alert(train,d235,di227,[d235_27, d235_32, d235_30, d235_31]).
ashby_alert(train,d235,di227,[d235_27, d235_32, d235_17, d235_31]).
ashby_alert(train,d235,di227,[d235_30, d235_34, d235_27, d235_33]).
ashby_alert(train,d235,di227,[d235_30, d235_35, d235_34, d235_33]).
ashby_alert(train,d235,di227,[d235_30, d235_35, d235_27, d235_33]).
ashby_alert(train,d239,di227,[d239_2, d239_4, d239_1, d239_3]).
ashby_alert(train,d239,di227,[d239_2, d239_5, d239_4, d239_3]).
ashby_alert(train,d239,di227,[d239_2, d239_5, d239_1, d239_3]).
ashby_alert(train,d273,di227,[d273_1, d273_4, d273_3, d273_2]).
ashby_alert(train,d273,di227,[d273_1, d273_5, d273_3, d273_2]).
ashby_alert(train,d273,di227,[d273_1, d273_5, d273_4, d273_2]).
ashby_alert(train,d274,di227,[d274_1, d274_3, d274_2, d274_4]).
ashby_alert(train,d274,di227,[d274_1, d274_4, d274_2, d274_3]).
ashby_alert(train,d274,di227,[d274_1, d274_4, d274_3, d274_2]).
ashby_alert(train,d274,di227,[d274_1, d274_5, d274_2, d274_3]).
ashby_alert(train,d274,di227,[d274_1, d274_5, d274_2, d274_4]).
ashby_alert(train,d274,di227,[d274_1, d274_5, d274_3, d274_2]).
ashby_alert(train,d274,di227,[d274_1, d274_5, d274_3, d274_4]).
ashby_alert(train,d274,di227,[d274_1, d274_5, d274_4, d274_2]).
ashby_alert(train,d274,di227,[d274_1, d274_5, d274_4, d274_3]).
ashby_alert(train,d275,di227,[d275_1, d275_3, d275_2, d275_4]).
ashby_alert(train,d275,di227,[d275_1, d275_4, d275_2, d275_3]).
ashby_alert(train,d275,di227,[d275_1, d275_5, d275_2, d275_3]).
ashby_alert(train,d275,di227,[d275_1, d275_5, d275_2, d275_4]).
ashby_alert(train,d275,di227,[d275_1, d275_5, d275_3, d275_4]).
ashby_alert(train,d275,di227,[d275_1, d275_5, d275_4, d275_3]).
ashby_alert(train,d232,di232,[d232_1, d232_9, d232_8, d232_2, d232_10, d232_7]).
ashby_alert(train,d238,di232,[d238_13, d238_17, d238_16, d238_12, d238_15, d238_14]).
ashby_alert(train,d238,di232,[d238_26, d238_30, d238_29, d238_23, d238_28, d238_27]).
ashby_alert(train,d240,di232,[d240_1, d240_5, d240_4, d240_2, d240_7, d240_3]).
ashby_alert(train,d242,di232,[d242_18, d242_22, d242_21, d242_16, d242_20, d242_19]).
ashby_alert(train,d245,di232,[d245_1, d245_12, d245_11, d245_2, d245_13, d245_10]).
ashby_alert(train,d254,di232,[d254_1, d254_5, d254_4, d254_2, d254_7, d254_3]).
ashby_alert(train,d152,ring_size_4,[d152_18, d152_19, d152_20, d152_21]).
ashby_alert(train,d182,ring_size_4,[d182_19, d182_21, d182_24, d182_22]).
ashby_alert(train,d205,ring_size_4,[d205_1, d205_2, d205_3, d205_6]).
ashby_alert(train,d205,ring_size_4,[d205_16, d205_4, d205_2, d205_5]).
ashby_alert(train,d216,ring_size_4,[d216_1, d216_2, d216_3, d216_6]).
ashby_alert(train,d216,ring_size_4,[d216_16, d216_4, d216_2, d216_5]).
ashby_alert(train,d248,ring_size_4,[d248_1, d248_2, d248_4, d248_3]).
ashby_alert(train,d252,di252,[d252_11, d252_10, d252_7]).
ashby_alert(train,d260,di260,[d260_3, d260_4, d260_5, d260_2]).
ashby_alert(train,d260,di260,[d260_7, d260_8, d260_9, d260_6]).
ashby_alert(train,d260,di260,[d260_11, d260_16, d260_17, d260_10]).
ashby_alert(train,d260,di260,[d260_13, d260_14, d260_15, d260_12]).
ashby_alert(train,d262,cyanide,[d262_2, d262_3, d262_1]).
ashby_alert(train,d13,methanol,[d13_21, d13_26, d13_25, d13_24, d13_34]).
ashby_alert(train,d13,methanol,[d13_28, d13_33, d13_32, d13_31, d13_35]).
ashby_alert(train,d160,methanol,[d160_11, d160_15, d160_14, d160_13, d160_20]).
ashby_alert(train,d164,methanol,[d164_26, d164_40, d164_39, d164_38, d164_41]).
ashby_alert(train,d174,methanol,[d174_2, d174_5, d174_4, d174_1, d174_21]).
ashby_alert(train,d174,methanol,[d174_16, d174_20, d174_19, d174_18, d174_26]).
ashby_alert(train,d181,methanol,[d181_12, d181_15, d181_14, d181_13, d181_16]).
ashby_alert(train,d240,methanol,[d240_6, d240_9, d240_10, d240_8, d240_11]).
ashby_alert(train,d241,methanol,[d241_1, d241_5, d241_4, d241_3, d241_12]).
ashby_alert(train,d263,methanol,[d263_3, d263_8, d263_7, d263_6, d263_9]).
ashby_alert(train,d267,methanol,[d267_2, d267_5, d267_4, d267_1, d267_12]).
ashby_alert(train,d267,methanol,[d267_6, d267_9, d267_8, d267_7, d267_13]).
ashby_alert(train,d267,methanol,[d267_10, d267_16, d267_15, d267_14, d267_17]).
ashby_alert(train,d267,methanol,[d267_11, d267_20, d267_19, d267_18, d267_21]).
ashby_alert(train,d284,methanol,[d284_9, d284_12, d284_11, d284_10, d284_14]).
ashby_alert(train,d63,methanol,[d63_14, d63_19, d63_18, d63_17, d63_25]).
ashby_alert(train,d78,methanol,[d78_12, d78_17, d78_16, d78_15, d78_38]).
ashby_alert(train,d78,methanol,[d78_19, d78_24, d78_23, d78_22, d78_37]).
ashby_alert(train,d78,methanol,[d78_28, d78_33, d78_32, d78_31, d78_39]).
ashby_alert(train,d281,di281,[d281_1, d281_6, d281_7, d281_2]).
ashby_alert(train,d281,di281,[d281_3, d281_8, d281_9, d281_2]).
ashby_alert(train,d281,di281,[d281_4, d281_12, d281_13, d281_2]).
ashby_alert(train,d281,di281,[d281_5, d281_10, d281_11, d281_2]).
ashby_alert(train,d282,di281,[d282_1, d282_10, d282_9, d282_2]).
ashby_alert(train,d287,di287,[d287_4, d287_3, d287_5, d287_15, d287_14, d287_18, d287_17, d287_16]).
ashby_alert(train,d127,nitro,[d127_5, d127_12, d127_13, d127_14]).
ashby_alert(train,d13,nitro,[d13_3, d13_11, d13_18, d13_19]).
ashby_alert(train,d131,nitro,[d131_2, d131_16, d131_17, d131_18]).
ashby_alert(train,d16,nitro,[d16_18, d16_23, d16_28, d16_29]).
ashby_alert(train,d18,nitro,[d18_3, d18_5, d18_6, d18_7]).
ashby_alert(train,d19,nitro,[d19_10, d19_22, d19_23, d19_24]).
ashby_alert(train,d20,nitro,[d20_1, d20_12, d20_17, d20_18]).
ashby_alert(train,d21,nitro,[d21_1, d21_23, d21_24, d21_25]).
ashby_alert(train,d260,nitro,[d260_2, d260_3, d260_4, d260_5]).
ashby_alert(train,d260,nitro,[d260_6, d260_7, d260_8, d260_9]).
ashby_alert(train,d260,nitro,[d260_10, d260_11, d260_16, d260_17]).
ashby_alert(train,d260,nitro,[d260_12, d260_13, d260_14, d260_15]).
ashby_alert(train,d27,nitro,[d27_15, d27_12, d27_13, d27_14]).
ashby_alert(train,d28,nitro,[d28_1, d28_6, d28_12, d28_13]).
ashby_alert(train,d281,nitro,[d281_2, d281_1, d281_6, d281_7]).
ashby_alert(train,d281,nitro,[d281_2, d281_3, d281_8, d281_9]).
ashby_alert(train,d281,nitro,[d281_2, d281_4, d281_12, d281_13]).
ashby_alert(train,d281,nitro,[d281_2, d281_5, d281_10, d281_11]).
ashby_alert(train,d282,nitro,[d282_2, d282_1, d282_10, d282_9]).
ashby_alert(train,d38,nitro,[d38_4, d38_14, d38_16, d38_17]).
ashby_alert(train,d38,nitro,[d38_2, d38_15, d38_18, d38_19]).
ashby_alert(train,d39,nitro,[d39_1, d39_15, d39_16, d39_17]).
ashby_alert(train,d41,nitro,[d41_4, d41_16, d41_18, d41_19]).
ashby_alert(train,d41,nitro,[d41_6, d41_17, d41_20, d41_21]).
ashby_alert(train,d46,nitro,[d46_1, d46_10, d46_11, d46_12]).
ashby_alert(train,d49,nitro,[d49_3, d49_12, d49_15, d49_16]).
ashby_alert(train,d50,nitro,[d50_1, d50_7, d50_8, d50_9]).
ashby_alert(train,d54,nitro,[d54_3, d54_16, d54_18, d54_19]).
ashby_alert(train,d55,nitro,[d55_4, d55_11, d55_13, d55_14]).
ashby_alert(train,d56,nitro,[d56_2, d56_12, d56_13, d56_14]).
ashby_alert(train,d57,nitro,[d57_1, d57_12, d57_13, d57_14]).
ashby_alert(train,d58,nitro,[d58_5, d58_34, d58_40, d58_41]).
ashby_alert(train,d61,nitro,[d61_1, d61_11, d61_15, d61_16]).
ashby_alert(train,d63,nitro,[d63_3, d63_11, d63_20, d63_21]).
ashby_alert(train,d65,nitro,[d65_3, d65_11, d65_15, d65_16]).
ashby_alert(train,d71,nitro,[d71_2, d71_11, d71_21, d71_22]).
ashby_alert(train,d72,nitro,[d72_3, d72_13, d72_14, d72_15]).
ashby_alert(train,d73,nitro,[d73_2, d73_12, d73_17, d73_18]).
ashby_alert(train,d78,nitro,[d78_3, d78_25, d78_34, d78_35]).
ashby_alert(train,d80,nitro,[d80_14, d80_18, d80_19, d80_20]).
six_ring(train,d1,[d1_1, d1_2, d1_3, d1_4, d1_5, d1_6]).
six_ring(train,d1,[d1_3, d1_4, d1_14, d1_13, d1_12, d1_11]).
six_ring(train,d1,[d1_12, d1_13, d1_18, d1_17, d1_16, d1_15]).
non_ar_6c_ring(train,d1,[d1_1, d1_2, d1_3, d1_4, d1_5, d1_6]).
non_ar_6c_ring(train,d1,[d1_3, d1_4, d1_14, d1_13, d1_12, d1_11]).
non_ar_6c_ring(train,d1,[d1_12, d1_13, d1_18, d1_17, d1_16, d1_15]).
ketone(train,d1,[d1_22, d1_14, d1_13, d1_4]).
ketone(train,d1,[d1_23, d1_11, d1_12, d1_3]).
amine(train,d1,[d1_24, d1_25, d1_26, d1_17]).
six_ring(train,d10,[d10_1, d10_2, d10_3, d10_4, d10_5, d10_6]).
non_ar_6c_ring(train,d10,[d10_1, d10_2, d10_3, d10_4, d10_5, d10_6]).
alcohol(train,d10,[d10_14, d10_15, d10_12]).
ether(train,d10,[d10_14, d10_12, d10_15]).
six_ring(train,d100,[d100_1, d100_2, d100_3, d100_4, d100_5, d100_6]).
six_ring(train,d100,[d100_11, d100_12, d100_13, d100_14, d100_15, d100_16]).
non_ar_6c_ring(train,d100,[d100_1, d100_2, d100_3, d100_4, d100_5, d100_6]).
non_ar_6c_ring(train,d100,[d100_11, d100_12, d100_13, d100_14, d100_15, d100_16]).
ar_halide(train,d100,[d100_23, d100_22]).
ar_halide(train,d100,[d100_24, d100_22]).
ar_halide(train,d100,[d100_25, d100_1]).
ar_halide(train,d100,[d100_26, d100_13]).
five_ring(train,d101,[d101_1, d101_2, d101_14, d101_5, d101_6]).
five_ring(train,d101,[d101_1, d101_6, d101_7, d101_8, d101_9]).
five_ring(train,d101,[d101_2, d101_14, d101_5, d101_4, d101_3]).
non_ar_5c_ring(train,d101,[d101_1, d101_2, d101_14, d101_5, d101_6]).
non_ar_5c_ring(train,d101,[d101_1, d101_6, d101_7, d101_8, d101_9]).
non_ar_5c_ring(train,d101,[d101_2, d101_14, d101_5, d101_4, d101_3]).
six_ring(train,d101,[d101_1, d101_2, d101_3, d101_4, d101_5, d101_6]).
non_ar_6c_ring(train,d101,[d101_1, d101_2, d101_3, d101_4, d101_5, d101_6]).
ar_halide(train,d101,[d101_15, d101_14]).
ar_halide(train,d101,[d101_16, d101_14]).
ar_halide(train,d101,[d101_17, d101_7]).
ar_halide(train,d101,[d101_19, d101_5]).
ar_halide(train,d101,[d101_20, d101_4]).
ar_halide(train,d101,[d101_21, d101_3]).
ar_halide(train,d101,[d101_22, d101_2]).
alkyl_halide(train,d101,[d101_15, d101_14]).
alkyl_halide(train,d101,[d101_16, d101_14]).
alkyl_halide(train,d101,[d101_17, d101_7]).
alkyl_halide(train,d101,[d101_19, d101_5]).
alkyl_halide(train,d101,[d101_20, d101_4]).
alkyl_halide(train,d101,[d101_21, d101_3]).
alkyl_halide(train,d101,[d101_22, d101_2]).
ar_halide(train,d102,[d102_3, d102_1]).
ar_halide(train,d102,[d102_4, d102_1]).
ar_halide(train,d102,[d102_5, d102_1]).
ar_halide(train,d102,[d102_6, d102_2]).
ar_halide(train,d102,[d102_7, d102_2]).
alkyl_halide(train,d102,[d102_3, d102_1]).
alkyl_halide(train,d102,[d102_4, d102_1]).
alkyl_halide(train,d102,[d102_5, d102_1]).
alkyl_halide(train,d102,[d102_6, d102_2]).
alkyl_halide(train,d102,[d102_7, d102_2]).
ar_halide(train,d103,[d103_3, d103_2]).
ar_halide(train,d103,[d103_6, d103_1]).
ar_halide(train,d103,[d103_7, d103_1]).
ar_halide(train,d103,[d103_8, d103_1]).
alkyl_halide(train,d103,[d103_3, d103_2]).
alkyl_halide(train,d103,[d103_6, d103_1]).
alkyl_halide(train,d103,[d103_7, d103_1]).
alkyl_halide(train,d103,[d103_8, d103_1]).
ar_halide(train,d104,[d104_3, d104_1]).
ar_halide(train,d104,[d104_4, d104_1]).
ar_halide(train,d104,[d104_6, d104_2]).
ar_halide(train,d104,[d104_7, d104_2]).
alkyl_halide(train,d104,[d104_3, d104_1]).
alkyl_halide(train,d104,[d104_4, d104_1]).
alkyl_halide(train,d104,[d104_6, d104_2]).
alkyl_halide(train,d104,[d104_7, d104_2]).
ar_halide(train,d105,[d105_3, d105_1]).
ar_halide(train,d105,[d105_4, d105_1]).
ar_halide(train,d105,[d105_5, d105_2]).
five_ring(train,d106,[d106_1, d106_2, d106_15, d106_5, d106_6]).
five_ring(train,d106,[d106_1, d106_10, d106_16, d106_7, d106_6]).
five_ring(train,d106,[d106_2, d106_15, d106_5, d106_4, d106_3]).
five_ring(train,d106,[d106_7, d106_16, d106_10, d106_9, d106_8]).
non_ar_5c_ring(train,d106,[d106_1, d106_2, d106_15, d106_5, d106_6]).
non_ar_5c_ring(train,d106,[d106_1, d106_10, d106_16, d106_7, d106_6]).
non_ar_5c_ring(train,d106,[d106_2, d106_15, d106_5, d106_4, d106_3]).
non_ar_5c_ring(train,d106,[d106_7, d106_16, d106_10, d106_9, d106_8]).
six_ring(train,d106,[d106_1, d106_2, d106_3, d106_4, d106_5, d106_6]).
six_ring(train,d106,[d106_1, d106_6, d106_7, d106_8, d106_9, d106_10]).
non_ar_6c_ring(train,d106,[d106_1, d106_2, d106_3, d106_4, d106_5, d106_6]).
non_ar_6c_ring(train,d106,[d106_1, d106_6, d106_7, d106_8, d106_9, d106_10]).
ar_halide(train,d106,[d106_19, d106_15]).
ar_halide(train,d106,[d106_20, d106_15]).
ar_halide(train,d106,[d106_21, d106_5]).
ar_halide(train,d106,[d106_22, d106_2]).
ar_halide(train,d106,[d106_23, d106_4]).
ar_halide(train,d106,[d106_24, d106_3]).
alkyl_halide(train,d106,[d106_19, d106_15]).
alkyl_halide(train,d106,[d106_20, d106_15]).
alkyl_halide(train,d106,[d106_21, d106_5]).
alkyl_halide(train,d106,[d106_22, d106_2]).
alkyl_halide(train,d106,[d106_23, d106_4]).
alkyl_halide(train,d106,[d106_24, d106_3]).
ar_halide(train,d107,[d107c_53, d107c_1]).
ar_halide(train,d107,[d107c_56, d107c_6]).
ar_halide(train,d107,[d107c_57, d107c_6]).
ar_halide(train,d107,[d107c_58, d107c_16]).
ar_halide(train,d107,[d107c_60, d107c_19]).
ar_halide(train,d107,[d107c_62, d107c_21]).
ar_halide(train,d107,[d107c_64, d107c_28]).
ar_halide(train,d107,[d107c_66, d107c_32]).
ar_halide(train,d107,[d107c_68, d107c_39]).
ar_halide(train,d107,[d107c_70, d107c_42]).
ar_halide(train,d107,[d107d_54, d107d_17]).
ar_halide(train,d107,[d107d_55, d107d_17]).
ar_halide(train,d107,[d107d_56, d107d_17]).
ar_halide(train,d107,[d107d_57, d107d_34]).
ar_halide(train,d107,[d107d_58, d107d_34]).
ar_halide(train,d107,[d107d_59, d107d_34]).
ar_halide(train,d107,[d107d_60, d107d_43]).
ar_halide(train,d107,[d107d_63, d107d_45]).
ar_halide(train,d107,[d107d_66, d107d_44]).
ar_halide(train,d107,[d107d_69, d107d_51]).
alkyl_halide(train,d107,[d107c_53, d107c_1]).
alkyl_halide(train,d107,[d107c_56, d107c_6]).
alkyl_halide(train,d107,[d107c_57, d107c_6]).
alkyl_halide(train,d107,[d107c_58, d107c_16]).
alkyl_halide(train,d107,[d107c_60, d107c_19]).
alkyl_halide(train,d107,[d107c_62, d107c_21]).
alkyl_halide(train,d107,[d107c_64, d107c_28]).
alkyl_halide(train,d107,[d107c_66, d107c_32]).
alkyl_halide(train,d107,[d107c_68, d107c_39]).
alkyl_halide(train,d107,[d107c_70, d107c_42]).
alkyl_halide(train,d107,[d107d_54, d107d_17]).
alkyl_halide(train,d107,[d107d_55, d107d_17]).
alkyl_halide(train,d107,[d107d_56, d107d_17]).
alkyl_halide(train,d107,[d107d_57, d107d_34]).
alkyl_halide(train,d107,[d107d_58, d107d_34]).
alkyl_halide(train,d107,[d107d_59, d107d_34]).
alkyl_halide(train,d107,[d107d_60, d107d_43]).
alkyl_halide(train,d107,[d107d_63, d107d_45]).
alkyl_halide(train,d107,[d107d_66, d107d_44]).
alkyl_halide(train,d107,[d107d_69, d107d_51]).
methyl(train,d107,[d107c_47, d107c_44, d107c_50, d107c_51, d107c_52]).
methyl(train,d107,[d107d_9, d107d_5, d107d_10, d107d_11, d107d_12]).
methyl(train,d107,[d107d_13, d107d_5, d107d_14, d107d_15, d107d_16]).
methyl(train,d107,[d107d_18, d107d_4, d107d_19, d107d_20, d107d_21]).
methyl(train,d107,[d107d_22, d107d_1, d107d_23, d107d_24, d107d_25]).
methyl(train,d107,[d107d_26, d107d_1, d107d_27, d107d_28, d107d_29]).
methyl(train,d107,[d107d_30, d107d_5, d107d_31, d107d_32, d107d_33]).
methyl(train,d107,[d107d_35, d107d_1, d107d_36, d107d_37, d107d_38]).
methyl(train,d107,[d107d_39, d107d_7, d107d_40, d107d_41, d107d_42]).
methyl(train,d107,[d107d_46, d107d_8, d107d_48, d107d_49, d107d_50]).
six_ring(train,d108,[d108_1, d108_2, d108_3, d108_4, d108_5, d108_6]).
six_ring(train,d108,[d108_11, d108_12, d108_13, d108_14, d108_15, d108_16]).
non_ar_6c_ring(train,d108,[d108_1, d108_2, d108_3, d108_4, d108_5, d108_6]).
non_ar_6c_ring(train,d108,[d108_11, d108_12, d108_13, d108_14, d108_15, d108_16]).
ar_halide(train,d108,[d108_24, d108_23]).
ar_halide(train,d108,[d108_25, d108_23]).
ar_halide(train,d108,[d108_26, d108_23]).
ar_halide(train,d108,[d108_27, d108_1]).
ar_halide(train,d108,[d108_28, d108_13]).
alkyl_halide(train,d108,[d108_24, d108_23]).
alkyl_halide(train,d108,[d108_25, d108_23]).
alkyl_halide(train,d108,[d108_26, d108_23]).
alcohol(train,d108,[d108_22, d108_29, d108_21]).
ether(train,d108,[d108_22, d108_21, d108_29]).
five_ring(train,d109,[d109_1, d109_2, d109_3, d109_4, d109_5]).
non_ar_hetero_5_ring(train,d109,[d109_1, d109_2, d109_3, d109_4, d109_5]).
six_ring(train,d109,[d109_6, d109_7, d109_8, d109_9, d109_10, d109_11]).
non_ar_6c_ring(train,d109,[d109_6, d109_7, d109_8, d109_9, d109_10, d109_11]).
ar_halide(train,d109,[d109_14, d109_7]).
alcohol(train,d109,[d109_13, d109_24, d109_12]).
sulfide(train,d109,[d109_15, d109_16, d109_6]).
ether(train,d109,[d109_13, d109_12, d109_24]).
ketone(train,d109,[d109_17, d109_12, d109_10, d109_13]).
amine(train,d109,[d109_16, d109_25, d109_26, d109_15]).
amine(train,d109,[d109_20, d109_27, d109_9, d109_21]).
six_ring(train,d11,[d11_1, d11_2, d11_3, d11_4, d11_5, d11_6]).
non_ar_6c_ring(train,d11,[d11_1, d11_2, d11_3, d11_4, d11_5, d11_6]).
ether(train,d11,[d11_12, d11_13, d11_2]).
amine(train,d11,[d11_10, d11_17, d11_18, d11_5]).
amine(train,d11,[d11_11, d11_19, d11_20, d11_3]).
methoxy(train,d11,[d11_13, d11_12, d11_14, d11_15, d11_16]).
methyl(train,d11,[d11_13, d11_12, d11_14, d11_15, d11_16]).
six_ring(train,d110,[d110_1, d110_2, d110_3, d110_4, d110_5, d110_6]).
non_ar_6c_ring(train,d110,[d110_1, d110_2, d110_3, d110_4, d110_5, d110_6]).
ar_halide(train,d110,[d110_12, d110_5]).
six_ring(train,d111,[d111_1, d111_2, d111_3, d111_4, d111_5, d111_6]).
six_ring(train,d111,[d111_3, d111_4, d111_19, d111_15, d111_10, d111_20]).
six_ring(train,d111,[d111_10, d111_11, d111_12, d111_13, d111_14, d111_15]).
non_ar_hetero_6_ring(train,d111,[d111_3, d111_4, d111_19, d111_15, d111_10, d111_20]).
non_ar_6c_ring(train,d111,[d111_1, d111_2, d111_3, d111_4, d111_5, d111_6]).
non_ar_6c_ring(train,d111,[d111_10, d111_11, d111_12, d111_13, d111_14, d111_15]).
ar_halide(train,d111,[d111_21, d111_1]).
ar_halide(train,d111,[d111_22, d111_13]).
ether(train,d111,[d111_19, d111_15, d111_4]).
ether(train,d111,[d111_20, d111_10, d111_3]).
five_ring(train,d112,[d112_1, d112_2, d112_16, d112_5, d112_6]).
five_ring(train,d112,[d112_1, d112_13, d112_6, d112_5, d112_6]).
five_ring(train,d112,[d112_2, d112_16, d112_5, d112_4, d112_3]).
five_ring(train,d112,[d112_3, d112_4, d112_10, d112_19, d112_7]).
five_ring(train,d112,[d112_7, d112_19, d112_10, d112_9, d112_8]).
non_ar_hetero_5_ring(train,d112,[d112_1, d112_13, d112_6, d112_14, d112_6]).
non_ar_hetero_5_ring(train,d112,[d112_1, d112_13, d112_6, d112_5, d112_6]).
non_ar_hetero_5_ring(train,d112,[d112_6, d112_13, d112_1, d112_2, d112_1]).
non_ar_hetero_5_ring(train,d112,[d112_6, d112_13, d112_1, d112_15, d112_1]).
non_ar_5c_ring(train,d112,[d112_1, d112_2, d112_16, d112_5, d112_6]).
non_ar_5c_ring(train,d112,[d112_2, d112_16, d112_5, d112_4, d112_3]).
non_ar_5c_ring(train,d112,[d112_3, d112_4, d112_10, d112_19, d112_7]).
non_ar_5c_ring(train,d112,[d112_7, d112_19, d112_10, d112_9, d112_8]).
six_ring(train,d112,[d112_1, d112_2, d112_3, d112_4, d112_5, d112_6]).
six_ring(train,d112,[d112_1, d112_2, d112_16, d112_5, d112_6, d112_13]).
six_ring(train,d112,[d112_1, d112_6, d112_13, d112_1, d112_6, d112_13]).
six_ring(train,d112,[d112_3, d112_4, d112_10, d112_9, d112_8, d112_7]).
non_ar_hetero_6_ring(train,d112,[d112_1, d112_2, d112_16, d112_5, d112_6, d112_13]).
non_ar_hetero_6_ring(train,d112,[d112_1, d112_6, d112_13, d112_1, d112_6, d112_13]).
non_ar_6c_ring(train,d112,[d112_1, d112_2, d112_3, d112_4, d112_5, d112_6]).
non_ar_6c_ring(train,d112,[d112_3, d112_4, d112_10, d112_9, d112_8, d112_7]).
ar_halide(train,d112,[d112_20, d112_19]).
ar_halide(train,d112,[d112_21, d112_19]).
ar_halide(train,d112,[d112_22, d112_10]).
ar_halide(train,d112,[d112_23, d112_9]).
ar_halide(train,d112,[d112_25, d112_8]).
ar_halide(train,d112,[d112_27, d112_7]).
alkyl_halide(train,d112,[d112_20, d112_19]).
alkyl_halide(train,d112,[d112_21, d112_19]).
alkyl_halide(train,d112,[d112_22, d112_10]).
alkyl_halide(train,d112,[d112_23, d112_9]).
alkyl_halide(train,d112,[d112_25, d112_8]).
alkyl_halide(train,d112,[d112_27, d112_7]).
ether(train,d112,[d112_13, d112_1, d112_6]).
six_ring(train,d113,[d113_1, d113_2, d113_3, d113_4, d113_5, d113_6]).
six_ring(train,d113,[d113_11, d113_12, d113_13, d113_14, d113_15, d113_16]).
non_ar_6c_ring(train,d113,[d113_1, d113_2, d113_3, d113_4, d113_5, d113_6]).
non_ar_6c_ring(train,d113,[d113_11, d113_12, d113_13, d113_14, d113_15, d113_16]).
ar_halide(train,d113,[d113_38, d113_36]).
ar_halide(train,d113,[d113_39, d113_36]).
alkyl_halide(train,d113,[d113_38, d113_36]).
alkyl_halide(train,d113,[d113_39, d113_36]).
methyl(train,d113,[d113_23, d113_22, d113_26, d113_27, d113_28]).
methyl(train,d113,[d113_30, d113_29, d113_33, d113_34, d113_35]).
six_ring(train,d114,[d114_1, d114_2, d114_3, d114_4, d114_5, d114_6]).
non_ar_hetero_6_ring(train,d114,[d114_1, d114_2, d114_3, d114_4, d114_5, d114_6]).
ar_halide(train,d114,[d114_7, d114_6]).
ar_halide(train,d114,[d114_9, d114_4]).
ar_halide(train,d114,[d114_12, d114_1]).
alcohol(train,d114,[d114_11, d114_16, d114_10]).
ether(train,d114,[d114_11, d114_10, d114_16]).
ketone(train,d114,[d114_15, d114_10, d114_11, d114_3]).
amine(train,d114,[d114_8, d114_13, d114_14, d114_5]).
six_ring(train,d116,[d116_1, d116_2, d116_3, d116_4, d116_5, d116_6]).
six_ring(train,d116,[d116_11, d116_12, d116_13, d116_14, d116_15, d116_16]).
non_ar_6c_ring(train,d116,[d116_1, d116_2, d116_3, d116_4, d116_5, d116_6]).
non_ar_6c_ring(train,d116,[d116_11, d116_12, d116_13, d116_14, d116_15, d116_16]).
ar_halide(train,d116,[d116_24, d116_22]).
ar_halide(train,d116,[d116_25, d116_22]).
ar_halide(train,d116,[d116_27, d116_13]).
ar_halide(train,d116,[d116_28, d116_1]).
alkyl_halide(train,d116,[d116_24, d116_22]).
alkyl_halide(train,d116,[d116_25, d116_22]).
six_ring(train,d117,[d117_1, d117_2, d117_3, d117_4, d117_5, d117_6]).
six_ring(train,d117,[d117_3, d117_4, d117_10, d117_9, d117_8, d117_7]).
non_ar_hetero_6_ring(train,d117,[d117_3, d117_4, d117_10, d117_9, d117_8, d117_7]).
non_ar_6c_ring(train,d117,[d117_1, d117_2, d117_3, d117_4, d117_5, d117_6]).
ar_halide(train,d117,[d117_18, d117_1]).
alkyl_halide(train,d117,[d117_18, d117_1]).
sulfide(train,d117,[d117_10, d117_4, d117_9]).
sulfide(train,d117,[d117_17, d117_23, d117_6]).
amine(train,d117,[d117_23, d117_26, d117_27, d117_17]).
amine(train,d117,[d117_7, d117_22, d117_8, d117_3]).
amine(train,d117,[d117_9, d117_21, d117_8, d117_10]).
six_ring(train,d118,[d118_1, d118_2, d118_3, d118_4, d118_5, d118_6]).
six_ring(train,d118,[d118_7, d118_8, d118_9, d118_10, d118_11, d118_12]).
non_ar_hetero_6_ring(train,d118,[d118_1, d118_2, d118_3, d118_4, d118_5, d118_6]).
non_ar_6c_ring(train,d118,[d118_7, d118_8, d118_9, d118_10, d118_11, d118_12]).
ar_halide(train,d118,[d118_18, d118_1]).
ar_halide(train,d118,[d118_19, d118_11]).
ar_halide(train,d118,[d118_20, d118_5]).
amine(train,d118,[d118_17, d118_21, d118_3, d118_12]).
ar_halide(train,d119,[d119_18, d119_15]).
alkyl_halide(train,d119,[d119_18, d119_15]).
methyl(train,d119,[d119_2, d119_1, d119_3, d119_4, d119_5]).
methyl(train,d119,[d119_6, d119_1, d119_7, d119_8, d119_9]).
methyl(train,d119,[d119_10, d119_1, d119_11, d119_12, d119_13]).
six_ring(train,d12,[d12_1, d12_2, d12_3, d12_4, d12_5, d12_6]).
non_ar_6c_ring(train,d12,[d12_1, d12_2, d12_3, d12_4, d12_5, d12_6]).
amine(train,d12,[d12_10, d12_12, d12_13, d12_5]).
amine(train,d12,[d12_11, d12_14, d12_15, d12_3]).
methyl(train,d12,[d12_16, d12_2, d12_17, d12_18, d12_19]).
six_ring(train,d120,[d120_1, d120_2, d120_3, d120_4, d120_5, d120_6]).
non_ar_6c_ring(train,d120,[d120_1, d120_2, d120_3, d120_4, d120_5, d120_6]).
ar_halide(train,d120,[d120_11, d120_5]).
ar_halide(train,d120,[d120_12, d120_4]).
five_ring(train,d121,[d121_1, d121_2, d121_25, d121_5, d121_6]).
five_ring(train,d121,[d121_2, d121_25, d121_5, d121_4, d121_3]).
five_ring(train,d121,[d121_3, d121_4, d121_10, d121_20, d121_7]).
five_ring(train,d121,[d121_7, d121_20, d121_10, d121_9, d121_8]).
five_ring(train,d121,[d121_8, d121_17, d121_9, d121_10, d121_9]).
five_ring(train,d121,[d121_9, d121_17, d121_8, d121_7, d121_8]).
non_ar_hetero_5_ring(train,d121,[d121_8, d121_17, d121_9, d121_10, d121_9]).
non_ar_hetero_5_ring(train,d121,[d121_8, d121_17, d121_9, d121_19, d121_9]).
non_ar_hetero_5_ring(train,d121,[d121_9, d121_17, d121_8, d121_18, d121_8]).
non_ar_hetero_5_ring(train,d121,[d121_9, d121_17, d121_8, d121_7, d121_8]).
non_ar_5c_ring(train,d121,[d121_1, d121_2, d121_25, d121_5, d121_6]).
non_ar_5c_ring(train,d121,[d121_2, d121_25, d121_5, d121_4, d121_3]).
non_ar_5c_ring(train,d121,[d121_3, d121_4, d121_10, d121_20, d121_7]).
non_ar_5c_ring(train,d121,[d121_7, d121_20, d121_10, d121_9, d121_8]).
six_ring(train,d121,[d121_1, d121_2, d121_3, d121_4, d121_5, d121_6]).
six_ring(train,d121,[d121_3, d121_4, d121_10, d121_9, d121_8, d121_7]).
six_ring(train,d121,[d121_7, d121_8, d121_17, d121_9, d121_10, d121_20]).
six_ring(train,d121,[d121_8, d121_9, d121_17, d121_8, d121_9, d121_17]).
non_ar_hetero_6_ring(train,d121,[d121_7, d121_8, d121_17, d121_9, d121_10, d121_20]).
non_ar_hetero_6_ring(train,d121,[d121_8, d121_9, d121_17, d121_8, d121_9, d121_17]).
non_ar_6c_ring(train,d121,[d121_1, d121_2, d121_3, d121_4, d121_5, d121_6]).
non_ar_6c_ring(train,d121,[d121_3, d121_4, d121_10, d121_9, d121_8, d121_7]).
ar_halide(train,d121,[d121_13, d121_6]).
ar_halide(train,d121,[d121_14, d121_1]).
ar_halide(train,d121,[d121_15, d121_5]).
ar_halide(train,d121,[d121_16, d121_2]).
ar_halide(train,d121,[d121_26, d121_25]).
ar_halide(train,d121,[d121_27, d121_25]).
alkyl_halide(train,d121,[d121_13, d121_6]).
alkyl_halide(train,d121,[d121_14, d121_1]).
alkyl_halide(train,d121,[d121_15, d121_5]).
alkyl_halide(train,d121,[d121_16, d121_2]).
alkyl_halide(train,d121,[d121_26, d121_25]).
alkyl_halide(train,d121,[d121_27, d121_25]).
ether(train,d121,[d121_17, d121_8, d121_9]).
six_ring(train,d122,[d122_1, d122_2, d122_3, d122_4, d122_5, d122_6]).
six_ring(train,d122,[d122_11, d122_12, d122_13, d122_14, d122_15, d122_16]).
non_ar_6c_ring(train,d122,[d122_1, d122_2, d122_3, d122_4, d122_5, d122_6]).
non_ar_6c_ring(train,d122,[d122_11, d122_12, d122_13, d122_14, d122_15, d122_16]).
ar_halide(train,d122,[d122_24, d122_22]).
ar_halide(train,d122,[d122_25, d122_22]).
ar_halide(train,d122,[d122_26, d122_22]).
alkyl_halide(train,d122,[d122_24, d122_22]).
alkyl_halide(train,d122,[d122_25, d122_22]).
alkyl_halide(train,d122,[d122_26, d122_22]).
ether(train,d122,[d122_27, d122_13, d122_28]).
ether(train,d122,[d122_32, d122_1, d122_33]).
methoxy(train,d122,[d122_28, d122_27, d122_29, d122_30, d122_31]).
methoxy(train,d122,[d122_33, d122_32, d122_34, d122_35, d122_36]).
methyl(train,d122,[d122_28, d122_27, d122_29, d122_30, d122_31]).
methyl(train,d122,[d122_33, d122_32, d122_34, d122_35, d122_36]).
ar_halide(train,d123,[d123_10, d123_6]).
alkyl_halide(train,d123,[d123_10, d123_6]).
ketone(train,d123,[d123_21, d123_9, d123_18, d123_6]).
ketone(train,d123,[d123_23, d123_19, d123_18, d123_20]).
amine(train,d123,[d123_20, d123_24, d123_25, d123_19]).
amine(train,d123,[d123_18, d123_22, d123_9, d123_19]).
methyl(train,d123,[d123_1, d123_2, d123_3, d123_4, d123_5]).
methyl(train,d123,[d123_12, d123_11, d123_15, d123_16, d123_17]).
six_ring(train,d124,[d124_9, d124_10, d124_11, d124_12, d124_13, d124_14]).
non_ar_6c_ring(train,d124,[d124_9, d124_10, d124_11, d124_12, d124_13, d124_14]).
ar_halide(train,d124,[d124_19, d124_12]).
sulfide(train,d124,[d124_5, d124_3, d124_9]).
ketone(train,d124,[d124_4, d124_2, d124_1, d124_3]).
amine(train,d124,[d124_1, d124_21, d124_20, d124_2]).
amine(train,d124,[d124_3, d124_6, d124_5, d124_2]).
methyl(train,d124,[d124_25, d124_22, d124_28, d124_29, d124_30]).
six_ring(train,d125,[d125_1, d125_2, d125_3, d125_4, d125_5, d125_6]).
six_ring(train,d125,[d125_11, d125_12, d125_13, d125_14, d125_15, d125_16]).
non_ar_6c_ring(train,d125,[d125_1, d125_2, d125_3, d125_4, d125_5, d125_6]).
non_ar_6c_ring(train,d125,[d125_11, d125_12, d125_13, d125_14, d125_15, d125_16]).
ar_halide(train,d125,[d125_24, d125_22]).
ar_halide(train,d125,[d125_25, d125_22]).
ar_halide(train,d125,[d125_26, d125_22]).
ar_halide(train,d125,[d125_27, d125_13]).
ar_halide(train,d125,[d125_28, d125_1]).
alkyl_halide(train,d125,[d125_24, d125_22]).
alkyl_halide(train,d125,[d125_25, d125_22]).
alkyl_halide(train,d125,[d125_26, d125_22]).
six_ring(train,d126,[d126_1, d126_2, d126_3, d126_4, d126_5, d126_6]).
non_ar_6c_ring(train,d126,[d126_1, d126_2, d126_3, d126_4, d126_5, d126_6]).
ar_halide(train,d126,[d126_7, d126_6]).
ar_halide(train,d126,[d126_8, d126_5]).
ar_halide(train,d126,[d126_9, d126_4]).
ar_halide(train,d126,[d126_10, d126_2]).
ar_halide(train,d126,[d126_11, d126_3]).
ar_halide(train,d126,[d126_12, d126_1]).
alkyl_halide(train,d126,[d126_7, d126_6]).
alkyl_halide(train,d126,[d126_8, d126_5]).
alkyl_halide(train,d126,[d126_9, d126_4]).
alkyl_halide(train,d126,[d126_10, d126_2]).
alkyl_halide(train,d126,[d126_11, d126_3]).
alkyl_halide(train,d126,[d126_12, d126_1]).
six_ring(train,d127,[d127_1, d127_2, d127_3, d127_4, d127_5, d127_6]).
non_ar_6c_ring(train,d127,[d127_1, d127_2, d127_3, d127_4, d127_5, d127_6]).
ar_halide(train,d127,[d127_7, d127_6]).
ar_halide(train,d127,[d127_8, d127_4]).
ar_halide(train,d127,[d127_9, d127_2]).
ar_halide(train,d127,[d127_10, d127_3]).
ar_halide(train,d127,[d127_11, d127_1]).
nitro(train,d127,[d127_12, d127_5, d127_13, d127_14]).
five_ring(train,d128,[d128_1, d128_2, d128_8, d128_16, d128_23]).
five_ring(train,d128,[d128_1, d128_11, d128_12, d128_8, d128_2]).
five_ring(train,d128,[d128_2, d128_3, d128_10, d128_16, d128_8]).
five_ring(train,d128,[d128_3, d128_10, d128_18, d128_6, d128_4]).
five_ring(train,d128,[d128_6, d128_18, d128_10, d128_16, d128_23]).
five_ring(train,d128,[d128_13, d128_11, d128_12, d128_8, d128_12]).
non_ar_hetero_5_ring(train,d128,[d128_11, d128_13, d128_12, d128_8, d128_12]).
non_ar_hetero_5_ring(train,d128,[d128_11, d128_13, d128_12, d128_15, d128_12]).
non_ar_hetero_5_ring(train,d128,[d128_12, d128_13, d128_11, d128_14, d128_11]).
non_ar_hetero_5_ring(train,d128,[d128_12, d128_13, d128_11, d128_1, d128_11]).
non_ar_5c_ring(train,d128,[d128_1, d128_2, d128_8, d128_16, d128_23]).
non_ar_5c_ring(train,d128,[d128_1, d128_11, d128_12, d128_8, d128_2]).
non_ar_5c_ring(train,d128,[d128_2, d128_3, d128_10, d128_16, d128_8]).
non_ar_5c_ring(train,d128,[d128_3, d128_10, d128_18, d128_6, d128_4]).
non_ar_5c_ring(train,d128,[d128_6, d128_18, d128_10, d128_16, d128_23]).
six_ring(train,d128,[d128_1, d128_2, d128_8, d128_12, d128_13, d128_11]).
six_ring(train,d128,[d128_1, d128_2, d128_3, d128_4, d128_6, d128_23]).
six_ring(train,d128,[d128_1, d128_2, d128_3, d128_10, d128_16, d128_23]).
six_ring(train,d128,[d128_1, d128_11, d128_12, d128_8, d128_16, d128_23]).
six_ring(train,d128,[d128_3, d128_4, d128_6, d128_23, d128_16, d128_10]).
six_ring(train,d128,[d128_11, d128_12, d128_13, d128_11, d128_12, d128_13]).
non_ar_hetero_6_ring(train,d128,[d128_1, d128_2, d128_8, d128_12, d128_13, d128_11]).
non_ar_hetero_6_ring(train,d128,[d128_11, d128_12, d128_13, d128_11, d128_12, d128_13]).
non_ar_6c_ring(train,d128,[d128_1, d128_2, d128_3, d128_4, d128_6, d128_23]).
non_ar_6c_ring(train,d128,[d128_1, d128_2, d128_3, d128_10, d128_16, d128_23]).
non_ar_6c_ring(train,d128,[d128_1, d128_11, d128_12, d128_8, d128_16, d128_23]).
non_ar_6c_ring(train,d128,[d128_3, d128_4, d128_6, d128_23, d128_16, d128_10]).
ar_halide(train,d128,[d128_5, d128_4]).
ar_halide(train,d128,[d128_19, d128_18]).
ar_halide(train,d128,[d128_20, d128_18]).
ar_halide(train,d128,[d128_21, d128_6]).
ar_halide(train,d128,[d128_22, d128_3]).
ar_halide(train,d128,[d128_27, d128_10]).
alkyl_halide(train,d128,[d128_5, d128_4]).
alkyl_halide(train,d128,[d128_19, d128_18]).
alkyl_halide(train,d128,[d128_20, d128_18]).
alkyl_halide(train,d128,[d128_21, d128_6]).
alkyl_halide(train,d128,[d128_22, d128_3]).
alkyl_halide(train,d128,[d128_27, d128_10]).
ether(train,d128,[d128_13, d128_11, d128_12]).
ar_halide(train,d129,[d129_1, d129_2]).
ar_halide(train,d129,[d129_3, d129_2]).
methyl(train,d129,[d129_5, d129_4, d129_6, d129_7, d129_8]).
methyl(train,d129,[d129_9, d129_4, d129_10, d129_11, d129_12]).
six_ring(train,d13,[d13_1, d13_2, d13_3, d13_4, d13_5, d13_6]).
non_ar_6c_ring(train,d13,[d13_1, d13_2, d13_3, d13_4, d13_5, d13_6]).
alcohol(train,d13,[d13_24, d13_34, d13_21]).
alcohol(train,d13,[d13_31, d13_35, d13_28]).
ether(train,d13,[d13_24, d13_21, d13_34]).
ether(train,d13,[d13_31, d13_28, d13_35]).
amine(train,d13,[d13_12, d13_17, d13_2, d13_13]).
methyl(train,d13,[d13_13, d13_12, d13_14, d13_15, d13_16]).
nitro(train,d13,[d13_11, d13_3, d13_18, d13_19]).
six_ring(train,d130,[d130_1, d130_2, d130_3, d130_4, d130_5, d130_6]).
non_ar_6c_ring(train,d130,[d130_1, d130_2, d130_3, d130_4, d130_5, d130_6]).
ar_halide(train,d130,[d130_12, d130_3]).
six_ring(train,d131,[d131_1, d131_2, d131_3, d131_4, d131_5, d131_6]).
non_ar_6c_ring(train,d131,[d131_1, d131_2, d131_3, d131_4, d131_5, d131_6]).
ar_halide(train,d131,[d131_7, d131_6]).
ar_halide(train,d131,[d131_8, d131_1]).
ar_halide(train,d131,[d131_14, d131_4]).
ar_halide(train,d131,[d131_15, d131_3]).
ether(train,d131,[d131_9, d131_10, d131_5]).
methoxy(train,d131,[d131_10, d131_9, d131_11, d131_12, d131_13]).
methyl(train,d131,[d131_10, d131_9, d131_11, d131_12, d131_13]).
nitro(train,d131,[d131_16, d131_2, d131_17, d131_18]).
six_ring(train,d132,[d132_1, d132_2, d132_3, d132_4, d132_5, d132_6]).
six_ring(train,d132,[d132_11, d132_12, d132_13, d132_14, d132_15, d132_16]).
non_ar_hetero_6_ring(train,d132,[d132_1, d132_2, d132_3, d132_4, d132_5, d132_6]).
non_ar_6c_ring(train,d132,[d132_11, d132_12, d132_13, d132_14, d132_15, d132_16]).
ar_halide(train,d132,[d132_38, d132_14]).
methyl(train,d132,[d132_30, d132_27, d132_31, d132_32, d132_33]).
methyl(train,d132,[d132_34, d132_27, d132_35, d132_36, d132_37]).
six_ring(train,d133,[d133_1, d133_2, d133_3, d133_4, d133_5, d133_6]).
non_ar_6c_ring(train,d133,[d133_1, d133_2, d133_3, d133_4, d133_5, d133_6]).
ar_halide(train,d133,[d133_11, d133_4]).
ar_halide(train,d133,[d133_12, d133_2]).
phenol(train,d133,[d133_10, d133_13, d133_5]).
six_ring(train,d134,[d134_1, d134_2, d134_3, d134_4, d134_5, d134_6]).
non_ar_6c_ring(train,d134,[d134_1, d134_2, d134_3, d134_4, d134_5, d134_6]).
six_ring(train,d135,[d135_1, d135_2, d135_3, d135_4, d135_5, d135_6]).
non_ar_6c_ring(train,d135,[d135_1, d135_2, d135_3, d135_4, d135_5, d135_6]).
ether(train,d135,[d135_34, d135_11, d135_35]).
ether(train,d135,[d135_40, d135_39, d135_41]).
ketone(train,d135,[d135_13, d135_11, d135_34, d135_4]).
ketone(train,d135,[d135_63, d135_39, d135_3, d135_40]).
methyl(train,d135,[d135_16, d135_15, d135_19, d135_20, d135_21]).
methyl(train,d135,[d135_28, d135_25, d135_31, d135_32, d135_33]).
methyl(train,d135,[d135_45, d135_42, d135_46, d135_47, d135_48]).
methyl(train,d135,[d135_57, d135_54, d135_60, d135_61, d135_62]).
ester(train,d136,[d136_3, d136_7, d136_6, d136_2, d136_4]).
ester(train,d136,[d136_3, d136_7, d136_6, d136_2, d136_5]).
ester(train,d136,[d136_3, d136_7, d136_6, d136_2, d136_1]).
ester(train,d136,[d136_9, d136_13, d136_12, d136_8, d136_10]).
ester(train,d136,[d136_9, d136_13, d136_12, d136_8, d136_11]).
ester(train,d136,[d136_9, d136_13, d136_12, d136_8, d136_1]).
ester(train,d136,[d136_15, d136_18, d136_19, d136_14, d136_16]).
ester(train,d136,[d136_15, d136_18, d136_19, d136_14, d136_17]).
ester(train,d136,[d136_15, d136_18, d136_19, d136_14, d136_1]).
alcohol(train,d136,[d136_7, d136_20, d136_3]).
alcohol(train,d136,[d136_13, d136_21, d136_9]).
alcohol(train,d136,[d136_18, d136_22, d136_15]).
ether(train,d136,[d136_18, d136_15, d136_22]).
ketone(train,d136,[d136_6, d136_3, d136_2, d136_7]).
ketone(train,d136,[d136_12, d136_9, d136_13, d136_8]).
ketone(train,d136,[d136_19, d136_15, d136_14, d136_18]).
five_ring(train,d137,[d137_3, d137_20, d137_10, d137_15, d137_4]).
non_ar_hetero_5_ring(train,d137,[d137_3, d137_4, d137_15, d137_10, d137_20]).
six_ring(train,d137,[d137_1, d137_2, d137_3, d137_4, d137_5, d137_6]).
six_ring(train,d137,[d137_10, d137_11, d137_12, d137_13, d137_14, d137_15]).
six_ring(train,d137,[d137_11, d137_12, d137_29, d137_28, d137_27, d137_26]).
six_ring(train,d137,[d137_27, d137_28, d137_38, d137_37, d137_36, d137_35]).
six_ring(train,d137,[d137_60, d137_61, d137_62, d137_63, d137_64, d137_65]).
non_ar_hetero_6_ring(train,d137,[d137_10, d137_11, d137_12, d137_13, d137_14, d137_15]).
non_ar_hetero_6_ring(train,d137,[d137_11, d137_12, d137_29, d137_28, d137_27, d137_26]).
non_ar_6c_ring(train,d137,[d137_1, d137_2, d137_3, d137_4, d137_5, d137_6]).
non_ar_6c_ring(train,d137,[d137_27, d137_28, d137_38, d137_37, d137_36, d137_35]).
non_ar_6c_ring(train,d137,[d137_60, d137_61, d137_62, d137_63, d137_64, d137_65]).
ester(train,d137,[d137_43, d137_45, d137_50, d137_35, d137_36]).
ester(train,d137,[d137_43, d137_45, d137_50, d137_35, d137_44]).
ester(train,d137,[d137_43, d137_45, d137_50, d137_35, d137_27]).
ether(train,d137,[d137_21, d137_1, d137_22]).
ether(train,d137,[d137_45, d137_43, d137_46]).
ether(train,d137,[d137_51, d137_36, d137_53]).
ether(train,d137,[d137_57, d137_37, d137_59]).
ether(train,d137,[d137_69, d137_64, d137_70]).
ether(train,d137,[d137_74, d137_63, d137_75]).
ether(train,d137,[d137_79, d137_62, d137_80]).
ketone(train,d137,[d137_50, d137_43, d137_35, d137_45]).
ketone(train,d137,[d137_68, d137_59, d137_57, d137_60]).
methoxy(train,d137,[d137_22, d137_21, d137_23, d137_24, d137_25]).
methoxy(train,d137,[d137_46, d137_45, d137_47, d137_48, d137_49]).
methoxy(train,d137,[d137_53, d137_51, d137_54, d137_55, d137_56]).
methoxy(train,d137,[d137_70, d137_69, d137_71, d137_72, d137_73]).
methoxy(train,d137,[d137_75, d137_74, d137_76, d137_77, d137_78]).
methoxy(train,d137,[d137_80, d137_79, d137_81, d137_82, d137_83]).
methyl(train,d137,[d137_22, d137_21, d137_23, d137_24, d137_25]).
methyl(train,d137,[d137_46, d137_45, d137_47, d137_48, d137_49]).
methyl(train,d137,[d137_53, d137_51, d137_54, d137_55, d137_56]).
methyl(train,d137,[d137_70, d137_69, d137_71, d137_72, d137_73]).
methyl(train,d137,[d137_75, d137_74, d137_76, d137_77, d137_78]).
methyl(train,d137,[d137_80, d137_79, d137_81, d137_82, d137_83]).
six_ring(train,d138,[d138_1, d138_2, d138_3, d138_4, d138_5, d138_6]).
non_ar_6c_ring(train,d138,[d138_1, d138_2, d138_3, d138_4, d138_5, d138_6]).
phenol(train,d138,[d138_11, d138_13, d138_5]).
phenol(train,d138,[d138_12, d138_14, d138_2]).
ester(train,d139,[d139_27, d139_30, d139_31, d139_24, d139_28]).
ester(train,d139,[d139_27, d139_30, d139_31, d139_24, d139_29]).
ester(train,d139,[d139_27, d139_30, d139_31, d139_24, d139_21]).
alcohol(train,d139,[d139_30, d139_32, d139_27]).
ether(train,d139,[d139_30, d139_27, d139_32]).
ketone(train,d139,[d139_31, d139_27, d139_24, d139_30]).
amine(train,d139,[d139_33, d139_36, d139_37, d139_1]).
six_ring(train,d14,[d14_1, d14_2, d14_3, d14_4, d14_5, d14_6]).
six_ring(train,d14,[d14_14, d14_15, d14_16, d14_17, d14_18, d14_19]).
non_ar_6c_ring(train,d14,[d14_1, d14_2, d14_3, d14_4, d14_5, d14_6]).
non_ar_6c_ring(train,d14,[d14_14, d14_15, d14_16, d14_17, d14_18, d14_19]).
amine(train,d14,[d14_12, d14_25, d14_3, d14_13]).
amine(train,d14,[d14_13, d14_26, d14_14, d14_12]).
six_ring(train,d140,[d140_1, d140_2, d140_3, d140_4, d140_5, d140_6]).
non_ar_6c_ring(train,d140,[d140_1, d140_2, d140_3, d140_4, d140_5, d140_6]).
phenol(train,d140,[d140_39, d140_41, d140_5]).
phenol(train,d140,[d140_40, d140_42, d140_1]).
ether(train,d140,[d140_36, d140_29, d140_38]).
ether(train,d140,[d140_40, d140_1, d140_42]).
ketone(train,d140,[d140_21, d140_18, d140_15, d140_22]).
ketone(train,d140,[d140_43, d140_38, d140_36, d140_4]).
methyl(train,d140,[d140_32, d140_29, d140_33, d140_34, d140_35]).
six_ring(train,d141,[d141_1, d141_2, d141_3, d141_4, d141_5, d141_6]).
non_ar_6c_ring(train,d141,[d141_1, d141_2, d141_3, d141_4, d141_5, d141_6]).
ester(train,d141,[d141_16, d141_13, d141_21, d141_17, d141_18]).
ester(train,d141,[d141_16, d141_13, d141_21, d141_17, d141_19]).
ester(train,d141,[d141_16, d141_13, d141_21, d141_17, d141_20]).
ether(train,d141,[d141_13, d141_12, d141_16]).
ketone(train,d141,[d141_21, d141_16, d141_13, d141_17]).
methyl(train,d141,[d141_17, d141_16, d141_18, d141_19, d141_20]).
five_ring(train,d142,[d142_3, d142_11, d142_12, d142_13, d142_4]).
non_ar_hetero_5_ring(train,d142,[d142_3, d142_4, d142_13, d142_12, d142_11]).
six_ring(train,d142,[d142_1, d142_2, d142_3, d142_4, d142_5, d142_6]).
non_ar_6c_ring(train,d142,[d142_1, d142_2, d142_3, d142_4, d142_5, d142_6]).
sulfide(train,d142,[d142_14, d142_12, d142_15]).
six_ring(train,d143,[d143_1, d143_2, d143_3, d143_4, d143_5, d143_6]).
six_ring(train,d143,[d143_3, d143_4, d143_10, d143_9, d143_8, d143_7]).
non_ar_hetero_6_ring(train,d143,[d143_1, d143_2, d143_3, d143_4, d143_5, d143_6]).
non_ar_hetero_6_ring(train,d143,[d143_3, d143_4, d143_10, d143_9, d143_8, d143_7]).
ester(train,d143,[d143_21, d143_22, d143_23, d143_8, d143_7]).
alcohol(train,d143,[d143_22, d143_24, d143_21]).
ether(train,d143,[d143_22, d143_21, d143_24]).
ketone(train,d143,[d143_13, d143_7, d143_3, d143_8]).
ketone(train,d143,[d143_23, d143_21, d143_22, d143_8]).
methyl(train,d143,[d143_15, d143_14, d143_18, d143_19, d143_20]).
methyl(train,d143,[d143_25, d143_6, d143_26, d143_27, d143_28]).
ester(train,d144,[d144_1, d144_2, d144_28, d144_30, d144_67]).
ester(train,d144,[d144_1, d144_2, d144_28, d144_30, d144_68]).
ester(train,d144,[d144_1, d144_2, d144_28, d144_30, d144_62]).
ester(train,d144,[d144_31, d144_33, d144_32, d144_29, d144_59]).
ester(train,d144,[d144_31, d144_33, d144_32, d144_29, d144_60]).
ester(train,d144,[d144_31, d144_33, d144_32, d144_29, d144_61]).
ether(train,d144,[d144_2, d144_1, d144_3]).
ether(train,d144,[d144_33, d144_31, d144_34]).
ketone(train,d144,[d144_28, d144_1, d144_2, d144_30]).
ketone(train,d144,[d144_32, d144_31, d144_29, d144_33]).
methyl(train,d144,[d144_8, d144_7, d144_11, d144_12, d144_13]).
methyl(train,d144,[d144_22, d144_19, d144_25, d144_26, d144_27]).
methyl(train,d144,[d144_39, d144_38, d144_42, d144_43, d144_44]).
methyl(train,d144,[d144_53, d144_50, d144_56, d144_57, d144_58]).
six_ring(train,d145,[d145_1, d145_2, d145_3, d145_4, d145_5, d145_6]).
six_ring(train,d145,[d145_31, d145_34, d145_35, d145_36, d145_37, d145_38]).
non_ar_6c_ring(train,d145,[d145_1, d145_2, d145_3, d145_4, d145_5, d145_6]).
non_ar_6c_ring(train,d145,[d145_31, d145_34, d145_35, d145_36, d145_37, d145_38]).
ether(train,d145,[d145_25, d145_24, d145_26]).
ether(train,d145,[d145_28, d145_11, d145_12]).
ketone(train,d145,[d145_23, d145_11, d145_28, d145_4]).
ketone(train,d145,[d145_27, d145_24, d145_25, d145_3]).
methyl(train,d145,[d145_17, d145_14, d145_20, d145_21, d145_22]).
five_ring(train,d146,[d146_3, d146_12, d146_11, d146_10, d146_4]).
non_ar_hetero_5_ring(train,d146,[d146_3, d146_4, d146_10, d146_11, d146_12]).
six_ring(train,d146,[d146_1, d146_2, d146_3, d146_4, d146_5, d146_6]).
non_ar_6c_ring(train,d146,[d146_1, d146_2, d146_3, d146_4, d146_5, d146_6]).
sulfide(train,d146,[d146_23, d146_16, d146_25]).
ether(train,d146,[d146_10, d146_11, d146_4]).
ether(train,d146,[d146_12, d146_11, d146_3]).
methyl(train,d146,[d146_19, d146_16, d146_20, d146_21, d146_22]).
methyl(train,d146,[d146_45, d146_42, d146_48, d146_49, d146_50]).
ether(train,d147,[d147_3, d147_1, d147_4]).
ether(train,d147,[d147_29, d147_1, d147_30]).
ether(train,d147,[d147_31, d147_1, d147_32]).
methyl(train,d147,[d147_14, d147_12, d147_17, d147_18, d147_19]).
methyl(train,d147,[d147_23, d147_20, d147_26, d147_27, d147_28]).
methyl(train,d147,[d147_39, d147_37, d147_42, d147_43, d147_44]).
methyl(train,d147,[d147_51, d147_48, d147_54, d147_55, d147_56]).
methyl(train,d147,[d147_63, d147_61, d147_66, d147_67, d147_68]).
methyl(train,d147,[d147_75, d147_72, d147_78, d147_79, d147_80]).
six_ring(train,d148,[d148_1, d148_2, d148_3, d148_4, d148_5, d148_6]).
non_ar_6c_ring(train,d148,[d148_1, d148_2, d148_3, d148_4, d148_5, d148_6]).
alcohol(train,d148,[d148_17, d148_19, d148_12]).
ether(train,d148,[d148_17, d148_12, d148_19]).
methyl(train,d148,[d148_13, d148_12, d148_14, d148_15, d148_16]).
six_ring(train,d149,[d149_1, d149_2, d149_3, d149_4, d149_5, d149_6]).
six_ring(train,d149,[d149_13, d149_14, d149_15, d149_16, d149_17, d149_18]).
non_ar_6c_ring(train,d149,[d149_1, d149_2, d149_3, d149_4, d149_5, d149_6]).
non_ar_6c_ring(train,d149,[d149_13, d149_14, d149_15, d149_16, d149_17, d149_18]).
phenol(train,d149,[d149_11, d149_32, d149_6]).
phenol(train,d149,[d149_31, d149_33, d149_16]).
ether(train,d149,[d149_31, d149_16, d149_33]).
methyl(train,d149,[d149_23, d149_12, d149_24, d149_25, d149_26]).
methyl(train,d149,[d149_27, d149_12, d149_28, d149_29, d149_30]).
six_ring(train,d15,[d15_1, d15_2, d15_3, d15_4, d15_5, d15_6]).
six_ring(train,d15,[d15_12, d15_15, d15_16, d15_17, d15_18, d15_19]).
non_ar_6c_ring(train,d15,[d15_1, d15_2, d15_3, d15_4, d15_5, d15_6]).
non_ar_6c_ring(train,d15,[d15_12, d15_15, d15_16, d15_17, d15_18, d15_19]).
amine(train,d15,[d15_24, d15_26, d15_27, d15_6]).
amine(train,d15,[d15_25, d15_28, d15_29, d15_17]).
six_ring(train,d150,[d150_1, d150_2, d150_3, d150_4, d150_5, d150_6]).
non_ar_6c_ring(train,d150,[d150_1, d150_2, d150_3, d150_4, d150_5, d150_6]).
ether(train,d150,[d150_12, d150_11, d150_13]).
ether(train,d150,[d150_18, d150_17, d150_19]).
ketone(train,d150,[d150_23, d150_11, d150_12, d150_5]).
ketone(train,d150,[d150_24, d150_17, d150_18, d150_2]).
methoxy(train,d150,[d150_13, d150_12, d150_14, d150_15, d150_16]).
methoxy(train,d150,[d150_19, d150_18, d150_20, d150_21, d150_22]).
methyl(train,d150,[d150_13, d150_12, d150_14, d150_15, d150_16]).
methyl(train,d150,[d150_19, d150_18, d150_20, d150_21, d150_22]).
six_ring(train,d151,[d151_1, d151_2, d151_3, d151_4, d151_5, d151_6]).
non_ar_6c_ring(train,d151,[d151_1, d151_2, d151_3, d151_4, d151_5, d151_6]).
phenol(train,d151,[d151_9, d151_16, d151_1]).
phenol(train,d151,[d151_10, d151_17, d151_2]).
phenol(train,d151,[d151_11, d151_18, d151_3]).
ether(train,d151,[d151_9, d151_1, d151_16]).
ether(train,d151,[d151_13, d151_12, d151_14]).
ketone(train,d151,[d151_15, d151_12, d151_13, d151_5]).
methyl(train,d151,[d151_22, d151_19, d151_25, d151_26, d151_27]).
five_ring(train,d152,[d152_20, d152_21, d152_24, d152_26, d152_27]).
non_ar_hetero_5_ring(train,d152,[d152_20, d152_21, d152_24, d152_26, d152_27]).
six_ring(train,d152,[d152_1, d152_2, d152_3, d152_4, d152_5, d152_6]).
six_ring(train,d152,[d152_18, d152_19, d152_20, d152_27, d152_20, d152_21]).
six_ring(train,d152,[d152_19, d152_20, d152_21, d152_24, d152_21, d152_18]).
six_ring(train,d152,[d152_20, d152_21, d152_18, d152_17, d152_18, d152_19]).
six_ring(train,d152,[d152_21, d152_18, d152_19, d152_23, d152_19, d152_20]).
non_ar_hetero_6_ring(train,d152,[d152_18, d152_19, d152_20, d152_27, d152_20, d152_21]).
non_ar_hetero_6_ring(train,d152,[d152_19, d152_20, d152_21, d152_24, d152_21, d152_18]).
non_ar_hetero_6_ring(train,d152,[d152_19, d152_20, d152_21, d152_25, d152_21, d152_18]).
non_ar_hetero_6_ring(train,d152,[d152_20, d152_21, d152_18, d152_22, d152_18, d152_19]).
non_ar_hetero_6_ring(train,d152,[d152_20, d152_21, d152_18, d152_17, d152_18, d152_19]).
non_ar_hetero_6_ring(train,d152,[d152_21, d152_18, d152_19, d152_23, d152_19, d152_20]).
non_ar_6c_ring(train,d152,[d152_1, d152_2, d152_3, d152_4, d152_5, d152_6]).
ester(train,d152,[d152_28, d152_30, d152_31, d152_27, d152_26]).
ester(train,d152,[d152_28, d152_30, d152_31, d152_27, d152_29]).
ester(train,d152,[d152_28, d152_30, d152_31, d152_27, d152_20]).
alcohol(train,d152,[d152_30, d152_43, d152_28]).
sulfide(train,d152,[d152_24, d152_21, d152_26]).
ether(train,d152,[d152_30, d152_28, d152_43]).
ketone(train,d152,[d152_16, d152_14, d152_12, d152_17]).
ketone(train,d152,[d152_23, d152_19, d152_18, d152_20]).
ketone(train,d152,[d152_31, d152_28, d152_27, d152_30]).
amine(train,d152,[d152_13, d152_40, d152_41, d152_12]).
amine(train,d152,[d152_17, d152_42, d152_18, d152_14]).
methyl(train,d152,[d152_32, d152_26, d152_33, d152_34, d152_35]).
methyl(train,d152,[d152_36, d152_26, d152_37, d152_38, d152_39]).
six_ring(train,d153,[d153_1, d153_2, d153_3, d153_4, d153_5, d153_6]).
non_ar_6c_ring(train,d153,[d153_1, d153_2, d153_3, d153_4, d153_5, d153_6]).
phenol(train,d153,[d153_10, d153_11, d153_5]).
phenol(train,d153,[d153_12, d153_26, d153_3]).
methyl(train,d153,[d153_27, d153_23, d153_30, d153_31, d153_32]).
six_ring(train,d154,[d154_1, d154_2, d154_3, d154_4, d154_5, d154_6]).
non_ar_6c_ring(train,d154,[d154_1, d154_2, d154_3, d154_4, d154_5, d154_6]).
ester(train,d154,[d154_25, d154_26, d154_27, d154_11, d154_18]).
ester(train,d154,[d154_25, d154_26, d154_27, d154_11, d154_22]).
ester(train,d154,[d154_25, d154_26, d154_27, d154_11, d154_10]).
phenol(train,d154,[d154_9, d154_14, d154_5]).
phenol(train,d154,[d154_16, d154_17, d154_4]).
alcohol(train,d154,[d154_26, d154_28, d154_25]).
ether(train,d154,[d154_26, d154_25, d154_28]).
ketone(train,d154,[d154_27, d154_25, d154_11, d154_26]).
amine(train,d154,[d154_22, d154_23, d154_24, d154_11]).
methyl(train,d154,[d154_18, d154_11, d154_19, d154_20, d154_21]).
six_ring(train,d155,[d155_1, d155_2, d155_3, d155_4, d155_5, d155_6]).
six_ring(train,d155,[d155_3, d155_4, d155_13, d155_12, d155_11, d155_10]).
six_ring(train,d155,[d155_11, d155_12, d155_17, d155_16, d155_15, d155_14]).
six_ring(train,d155,[d155_15, d155_16, d155_22, d155_21, d155_20, d155_19]).
non_ar_6c_ring(train,d155,[d155_1, d155_2, d155_3, d155_4, d155_5, d155_6]).
non_ar_6c_ring(train,d155,[d155_3, d155_4, d155_13, d155_12, d155_11, d155_10]).
non_ar_6c_ring(train,d155,[d155_11, d155_12, d155_17, d155_16, d155_15, d155_14]).
non_ar_6c_ring(train,d155,[d155_15, d155_16, d155_22, d155_21, d155_20, d155_19]).
ester(train,d155,[d155_14, d155_48, d155_11, d155_15, d155_16]).
ester(train,d155,[d155_14, d155_48, d155_11, d155_15, d155_19]).
ester(train,d155,[d155_14, d155_48, d155_11, d155_15, d155_23]).
ester(train,d155,[d155_21, d155_42, d155_20, d155_22, d155_16]).
ester(train,d155,[d155_21, d155_42, d155_20, d155_22, d155_32]).
ester(train,d155,[d155_21, d155_42, d155_20, d155_22, d155_33]).
phenol(train,d155,[d155_49, d155_58, d155_2]).
alcohol(train,d155,[d155_25, d155_54, d155_13]).
alcohol(train,d155,[d155_30, d155_53, d155_17]).
alcohol(train,d155,[d155_42, d155_55, d155_21]).
alcohol(train,d155,[d155_48, d155_52, d155_14]).
ether(train,d155,[d155_25, d155_13, d155_54]).
ether(train,d155,[d155_30, d155_17, d155_53]).
ether(train,d155,[d155_42, d155_21, d155_55]).
ether(train,d155,[d155_44, d155_43, d155_47]).
ether(train,d155,[d155_48, d155_14, d155_52]).
ether(train,d155,[d155_49, d155_2, d155_58]).
ketone(train,d155,[d155_50, d155_10, d155_11, d155_3]).
ketone(train,d155,[d155_51, d155_19, d155_15, d155_20]).
amine(train,d155,[d155_47, d155_56, d155_57, d155_44]).
methyl(train,d155,[d155_26, d155_13, d155_27, d155_28, d155_29]).
methyl(train,d155,[d155_34, d155_32, d155_35, d155_36, d155_37]).
methyl(train,d155,[d155_38, d155_32, d155_39, d155_40, d155_41]).
five_ring(train,d156,[d156_21, d156_22, d156_29, d156_28, d156_27]).
non_ar_hetero_5_ring(train,d156,[d156_21, d156_22, d156_29, d156_28, d156_27]).
six_ring(train,d156,[d156_1, d156_2, d156_3, d156_4, d156_5, d156_6]).
six_ring(train,d156,[d156_2, d156_3, d156_9, d156_12, d156_11, d156_10]).
six_ring(train,d156,[d156_9, d156_17, d156_18, d156_20, d156_19, d156_12]).
six_ring(train,d156,[d156_18, d156_20, d156_21, d156_22, d156_23, d156_24]).
non_ar_hetero_6_ring(train,d156,[d156_2, d156_3, d156_9, d156_12, d156_11, d156_10]).
non_ar_hetero_6_ring(train,d156,[d156_9, d156_17, d156_18, d156_20, d156_19, d156_12]).
non_ar_6c_ring(train,d156,[d156_1, d156_2, d156_3, d156_4, d156_5, d156_6]).
non_ar_6c_ring(train,d156,[d156_18, d156_20, d156_21, d156_22, d156_23, d156_24]).
ether(train,d156,[d156_10, d156_11, d156_2]).
ether(train,d156,[d156_19, d156_12, d156_20]).
ether(train,d156,[d156_29, d156_22, d156_28]).
ether(train,d156,[d156_40, d156_41, d156_6]).
ether(train,d156,[d156_45, d156_46, d156_5]).
ketone(train,d156,[d156_50, d156_17, d156_18, d156_9]).
methoxy(train,d156,[d156_41, d156_40, d156_42, d156_43, d156_44]).
methoxy(train,d156,[d156_46, d156_45, d156_47, d156_48, d156_49]).
methyl(train,d156,[d156_36, d156_32, d156_37, d156_38, d156_39]).
methyl(train,d156,[d156_41, d156_40, d156_42, d156_43, d156_44]).
methyl(train,d156,[d156_46, d156_45, d156_47, d156_48, d156_49]).
six_ring(train,d157,[d157_1, d157_2, d157_3, d157_4, d157_5, d157_6]).
six_ring(train,d157,[d157_12, d157_13, d157_14, d157_15, d157_16, d157_17]).
non_ar_6c_ring(train,d157,[d157_1, d157_2, d157_3, d157_4, d157_5, d157_6]).
non_ar_6c_ring(train,d157,[d157_12, d157_13, d157_14, d157_15, d157_16, d157_17]).
ether(train,d157,[d157_24, d157_23, d157_26]).
methyl(train,d157,[d157_33, d157_30, d157_34, d157_35, d157_36]).
methyl(train,d157,[d157_37, d157_30, d157_38, d157_39, d157_40]).
six_ring(train,d159,[d159_1, d159_2, d159_3, d159_4, d159_5, d159_6]).
six_ring(train,d159,[d159_20, d159_21, d159_22, d159_23, d159_24, d159_25]).
non_ar_6c_ring(train,d159,[d159_1, d159_2, d159_3, d159_4, d159_5, d159_6]).
non_ar_6c_ring(train,d159,[d159_20, d159_21, d159_22, d159_23, d159_24, d159_25]).
sulfide(train,d159,[d159_17, d159_4, d159_41]).
ketone(train,d159,[d159_16, d159_11, d159_1, d159_12]).
ketone(train,d159,[d159_38, d159_18, d159_19, d159_41]).
amine(train,d159,[d159_19, d159_37, d159_20, d159_18]).
amine(train,d159,[d159_41, d159_42, d159_18, d159_17]).
methyl(train,d159,[d159_12, d159_11, d159_13, d159_14, d159_15]).
six_ring(train,d16,[d16_1, d16_2, d16_3, d16_4, d16_5, d16_6]).
six_ring(train,d16,[d16_3, d16_4, d16_14, d16_13, d16_12, d16_11]).
six_ring(train,d16,[d16_12, d16_13, d16_18, d16_17, d16_16, d16_15]).
non_ar_6c_ring(train,d16,[d16_1, d16_2, d16_3, d16_4, d16_5, d16_6]).
non_ar_6c_ring(train,d16,[d16_3, d16_4, d16_14, d16_13, d16_12, d16_11]).
non_ar_6c_ring(train,d16,[d16_12, d16_13, d16_18, d16_17, d16_16, d16_15]).
ketone(train,d16,[d16_21, d16_14, d16_13, d16_4]).
ketone(train,d16,[d16_22, d16_11, d16_12, d16_3]).
methyl(train,d16,[d16_24, d16_17, d16_25, d16_26, d16_27]).
nitro(train,d16,[d16_23, d16_18, d16_28, d16_29]).
five_ring(train,d160,[d160_1, d160_2, d160_3, d160_4, d160_5]).
non_ar_hetero_5_ring(train,d160,[d160_1, d160_2, d160_3, d160_4, d160_5]).
ester(train,d160,[d160_2, d160_6, d160_3, d160_1, d160_8]).
ester(train,d160,[d160_2, d160_6, d160_3, d160_1, d160_9]).
ester(train,d160,[d160_2, d160_6, d160_3, d160_1, d160_5]).
ester(train,d160,[d160_3, d160_7, d160_2, d160_4, d160_5]).
ester(train,d160,[d160_4, d160_5, d160_16, d160_3, d160_7]).
alcohol(train,d160,[d160_6, d160_17, d160_2]).
alcohol(train,d160,[d160_7, d160_18, d160_3]).
alcohol(train,d160,[d160_10, d160_19, d160_8]).
alcohol(train,d160,[d160_13, d160_20, d160_11]).
ether(train,d160,[d160_5, d160_1, d160_4]).
ether(train,d160,[d160_13, d160_11, d160_20]).
ketone(train,d160,[d160_16, d160_4, d160_3, d160_5]).
six_ring(train,d161,[d161_1, d161_2, d161_3, d161_4, d161_5, d161_6]).
non_ar_6c_ring(train,d161,[d161_1, d161_2, d161_3, d161_4, d161_5, d161_6]).
phenol(train,d161,[d161_26, d161_40, d161_2]).
ether(train,d161,[d161_26, d161_2, d161_40]).
methyl(train,d161,[d161_9, d161_5, d161_10, d161_11, d161_12]).
methyl(train,d161,[d161_14, d161_13, d161_15, d161_16, d161_17]).
methyl(train,d161,[d161_18, d161_13, d161_19, d161_20, d161_21]).
methyl(train,d161,[d161_22, d161_13, d161_23, d161_24, d161_25]).
methyl(train,d161,[d161_28, d161_27, d161_29, d161_30, d161_31]).
methyl(train,d161,[d161_32, d161_27, d161_33, d161_34, d161_35]).
methyl(train,d161,[d161_36, d161_27, d161_37, d161_38, d161_39]).
six_ring(train,d162,[d162_1, d162_2, d162_3, d162_4, d162_5, d162_6]).
six_ring(train,d162,[d162_3, d162_4, d162_21, d162_16, d162_11, d162_22]).
six_ring(train,d162,[d162_11, d162_12, d162_13, d162_14, d162_15, d162_16]).
non_ar_hetero_6_ring(train,d162,[d162_3, d162_4, d162_21, d162_16, d162_11, d162_22]).
non_ar_6c_ring(train,d162,[d162_1, d162_2, d162_3, d162_4, d162_5, d162_6]).
non_ar_6c_ring(train,d162,[d162_11, d162_12, d162_13, d162_14, d162_15, d162_16]).
ether(train,d162,[d162_21, d162_16, d162_4]).
ether(train,d162,[d162_22, d162_11, d162_3]).
ester(train,d163,[d163_6, d163_8, d163_9, d163_3, d163_7]).
ester(train,d163,[d163_6, d163_8, d163_9, d163_3, d163_1]).
ester(train,d163,[d163_7, d163_12, d163_11, d163_3, d163_6]).
ester(train,d163,[d163_7, d163_12, d163_11, d163_3, d163_1]).
ester(train,d163,[d163_17, d163_23, d163_22, d163_14, d163_18]).
ester(train,d163,[d163_17, d163_23, d163_22, d163_14, d163_2]).
ester(train,d163,[d163_18, d163_20, d163_19, d163_14, d163_17]).
ester(train,d163,[d163_18, d163_20, d163_19, d163_14, d163_2]).
ether(train,d163,[d163_8, d163_10, d163_6]).
ether(train,d163,[d163_12, d163_13, d163_7]).
ether(train,d163,[d163_20, d163_18, d163_21]).
ether(train,d163,[d163_23, d163_17, d163_24]).
ketone(train,d163,[d163_9, d163_6, d163_3, d163_8]).
ketone(train,d163,[d163_11, d163_7, d163_12, d163_3]).
ketone(train,d163,[d163_19, d163_18, d163_14, d163_20]).
ketone(train,d163,[d163_22, d163_17, d163_14, d163_23]).
five_ring(train,d164,[d164_1, d164_2, d164_28, d164_33, d164_6]).
non_ar_hetero_5_ring(train,d164,[d164_1, d164_2, d164_28, d164_33, d164_6]).
six_ring(train,d164,[d164_1, d164_2, d164_3, d164_4, d164_5, d164_6]).
six_ring(train,d164,[d164_8, d164_9, d164_10, d164_11, d164_12, d164_13]).
non_ar_hetero_6_ring(train,d164,[d164_1, d164_2, d164_3, d164_4, d164_5, d164_6]).
non_ar_hetero_6_ring(train,d164,[d164_8, d164_9, d164_10, d164_11, d164_12, d164_13]).
alcohol(train,d164,[d164_18, d164_34, d164_3]).
alcohol(train,d164,[d164_24, d164_36, d164_9]).
alcohol(train,d164,[d164_25, d164_37, d164_8]).
alcohol(train,d164,[d164_29, d164_35, d164_10]).
alcohol(train,d164,[d164_38, d164_41, d164_26]).
ether(train,d164,[d164_5, d164_4, d164_6]).
ether(train,d164,[d164_7, d164_1, d164_11]).
ether(train,d164,[d164_12, d164_11, d164_13]).
ether(train,d164,[d164_18, d164_3, d164_34]).
ether(train,d164,[d164_28, d164_2, d164_33]).
ether(train,d164,[d164_29, d164_10, d164_35]).
ether(train,d164,[d164_38, d164_26, d164_41]).
methyl(train,d164,[d164_19, d164_2, d164_20, d164_21, d164_22]).
six_ring(train,d165,[d165_1, d165_2, d165_3, d165_4, d165_5, d165_6]).
six_ring(train,d165,[d165_14, d165_15, d165_16, d165_17, d165_18, d165_19]).
non_ar_6c_ring(train,d165,[d165_1, d165_2, d165_3, d165_4, d165_5, d165_6]).
non_ar_6c_ring(train,d165,[d165_14, d165_15, d165_16, d165_17, d165_18, d165_19]).
alcohol(train,d165,[d165_25, d165_28, d165_12]).
ether(train,d165,[d165_25, d165_12, d165_28]).
ketone(train,d165,[d165_27, d165_13, d165_12, d165_14]).
ketone(train,d166,[d166_18, d166_12, d166_13, d166_9]).
amine(train,d166,[d166_9, d166_19, d166_6, d166_12]).
six_ring(train,d167,[d167_1, d167_2, d167_3, d167_4, d167_5, d167_6]).
non_ar_6c_ring(train,d167,[d167_1, d167_2, d167_3, d167_4, d167_5, d167_6]).
alcohol(train,d167,[d167_13, d167_23, d167_12]).
ether(train,d167,[d167_13, d167_12, d167_23]).
amine(train,d167,[d167_20, d167_27, d167_22, d167_14]).
methyl(train,d167,[d167_16, d167_14, d167_17, d167_18, d167_19]).
methyl(train,d167,[d167_22, d167_20, d167_24, d167_25, d167_26]).
six_ring(train,d168,[d168_1, d168_2, d168_3, d168_4, d168_5, d168_6]).
non_ar_6c_ring(train,d168,[d168_1, d168_2, d168_3, d168_4, d168_5, d168_6]).
alcohol(train,d168,[d168_18, d168_31, d168_3]).
ether(train,d168,[d168_18, d168_3, d168_31]).
methyl(train,d168,[d168_13, d168_5, d168_15, d168_16, d168_17]).
methyl(train,d168,[d168_22, d168_20, d168_23, d168_24, d168_25]).
methyl(train,d168,[d168_26, d168_20, d168_28, d168_29, d168_30]).
six_ring(train,d169,[d169_1, d169_2, d169_3, d169_4, d169_5, d169_6]).
non_ar_6c_ring(train,d169,[d169_1, d169_2, d169_3, d169_4, d169_5, d169_6]).
phenol(train,d169,[d169_12, d169_13, d169_5]).
six_ring(train,d17,[d17_1, d17_2, d17_3, d17_4, d17_5, d17_6]).
six_ring(train,d17,[d17_3, d17_4, d17_13, d17_12, d17_11, d17_10]).
non_ar_6c_ring(train,d17,[d17_1, d17_2, d17_3, d17_4, d17_5, d17_6]).
non_ar_6c_ring(train,d17,[d17_3, d17_4, d17_13, d17_12, d17_11, d17_10]).
amine(train,d17,[d17_17, d17_21, d17_22, d17_2]).
amine(train,d17,[d17_18, d17_19, d17_20, d17_13]).
five_ring(train,d170,[d170_3, d170_11, d170_12, d170_13, d170_4]).
non_ar_hetero_5_ring(train,d170,[d170_3, d170_4, d170_13, d170_12, d170_11]).
six_ring(train,d170,[d170_1, d170_2, d170_3, d170_4, d170_5, d170_6]).
non_ar_6c_ring(train,d170,[d170_1, d170_2, d170_3, d170_4, d170_5, d170_6]).
ether(train,d170,[d170_12, d170_11, d170_13]).
ketone(train,d170,[d170_14, d170_13, d170_12, d170_4]).
ketone(train,d170,[d170_15, d170_11, d170_12, d170_3]).
six_ring(train,d171,[d171_1, d171_2, d171_3, d171_4, d171_5, d171_6]).
non_ar_6c_ring(train,d171,[d171_1, d171_2, d171_3, d171_4, d171_5, d171_6]).
sulfide(train,d171,[d171_15, d171_16, d171_4]).
ketone(train,d171,[d171_22, d171_17, d171_16, d171_18]).
amine(train,d171,[d171_16, d171_21, d171_17, d171_15]).
amine(train,d171,[d171_18, d171_42, d171_29, d171_17]).
methyl(train,d171,[d171_11, d171_1, d171_12, d171_13, d171_14]).
six_ring(train,d172,[d172_3, d172_6, d172_7, d172_8, d172_9, d172_10]).
six_ring(train,d172,[d172_4, d172_16, d172_17, d172_18, d172_19, d172_20]).
six_ring(train,d172,[d172_5, d172_26, d172_27, d172_28, d172_29, d172_30]).
non_ar_6c_ring(train,d172,[d172_3, d172_6, d172_7, d172_8, d172_9, d172_10]).
non_ar_6c_ring(train,d172,[d172_4, d172_16, d172_17, d172_18, d172_19, d172_20]).
non_ar_6c_ring(train,d172,[d172_5, d172_26, d172_27, d172_28, d172_29, d172_30]).
alcohol(train,d172,[d172_2, d172_36, d172_1]).
ether(train,d172,[d172_2, d172_1, d172_36]).
five_ring(train,d173,[d173_22, d173_23, d173_32, d173_33, d173_34]).
non_ar_5c_ring(train,d173,[d173_22, d173_23, d173_32, d173_33, d173_34]).
six_ring(train,d173,[d173_1, d173_2, d173_3, d173_4, d173_5, d173_6]).
six_ring(train,d173,[d173_3, d173_4, d173_16, d173_15, d173_14, d173_13]).
six_ring(train,d173,[d173_15, d173_16, d173_25, d173_24, d173_23, d173_22]).
non_ar_6c_ring(train,d173,[d173_1, d173_2, d173_3, d173_4, d173_5, d173_6]).
non_ar_6c_ring(train,d173,[d173_3, d173_4, d173_16, d173_15, d173_14, d173_13]).
non_ar_6c_ring(train,d173,[d173_15, d173_16, d173_25, d173_24, d173_23, d173_22]).
ester(train,d173,[d173_61, d173_64, d173_65, d173_58, d173_62]).
ester(train,d173,[d173_61, d173_64, d173_65, d173_58, d173_63]).
ester(train,d173,[d173_61, d173_64, d173_65, d173_58, d173_56]).
alcohol(train,d173,[d173_40, d173_66, d173_1]).
alcohol(train,d173,[d173_64, d173_67, d173_61]).
ether(train,d173,[d173_40, d173_1, d173_66]).
ether(train,d173,[d173_64, d173_61, d173_67]).
ketone(train,d173,[d173_65, d173_61, d173_58, d173_64]).
methyl(train,d173,[d173_42, d173_4, d173_43, d173_44, d173_45]).
methyl(train,d173,[d173_46, d173_23, d173_47, d173_48, d173_49]).
methyl(train,d173,[d173_52, d173_50, d173_53, d173_54, d173_55]).
alcohol(train,d174,[d174_1, d174_21, d174_2]).
alcohol(train,d174,[d174_6, d174_22, d174_3]).
alcohol(train,d174,[d174_9, d174_23, d174_7]).
alcohol(train,d174,[d174_12, d174_24, d174_10]).
alcohol(train,d174,[d174_15, d174_25, d174_13]).
alcohol(train,d174,[d174_18, d174_26, d174_16]).
ether(train,d174,[d174_1, d174_2, d174_21]).
ether(train,d174,[d174_12, d174_10, d174_24]).
ether(train,d174,[d174_15, d174_13, d174_25]).
ether(train,d174,[d174_18, d174_16, d174_26]).
six_ring(train,d175,[d175_1, d175_2, d175_3, d175_4, d175_5, d175_6]).
non_ar_6c_ring(train,d175,[d175_1, d175_2, d175_3, d175_4, d175_5, d175_6]).
imine(train,d175,[d175_19, d175_20]).
imine(train,d175,[d175_22, d175_23]).
amine(train,d175,[d175_24, d175_29, d175_30, d175_22]).
amine(train,d175,[d175_16, d175_25, d175_19, d175_13]).
amine(train,d175,[d175_21, d175_26, d175_22, d175_19]).
six_ring(train,d176,[d176_1, d176_2, d176_3, d176_4, d176_5, d176_6]).
non_ar_6c_ring(train,d176,[d176_1, d176_2, d176_3, d176_4, d176_5, d176_6]).
ketone(train,d176,[d176_12, d176_11, d176_13, d176_4]).
ketone(train,d176,[d176_16, d176_14, d176_15, d176_3]).
amine(train,d176,[d176_13, d176_17, d176_18, d176_11]).
amine(train,d176,[d176_15, d176_19, d176_20, d176_14]).
five_ring(train,d177,[d177_3, d177_4, d177_11, d177_10, d177_9]).
non_ar_hetero_5_ring(train,d177,[d177_3, d177_4, d177_11, d177_10, d177_9]).
six_ring(train,d177,[d177_1, d177_2, d177_3, d177_4, d177_5, d177_6]).
non_ar_6c_ring(train,d177,[d177_1, d177_2, d177_3, d177_4, d177_5, d177_6]).
ether(train,d177,[d177_9, d177_10, d177_3]).
ether(train,d177,[d177_11, d177_10, d177_4]).
ether(train,d177,[d177_25, d177_24, d177_28]).
ether(train,d177,[d177_32, d177_29, d177_35]).
ether(train,d177,[d177_39, d177_36, d177_42]).
methyl(train,d177,[d177_18, d177_15, d177_21, d177_22, d177_23]).
methyl(train,d177,[d177_49, d177_46, d177_52, d177_53, d177_54]).
six_ring(train,d179,[d179_1, d179_2, d179_3, d179_4, d179_5, d179_6]).
non_ar_6c_ring(train,d179,[d179_1, d179_2, d179_3, d179_4, d179_5, d179_6]).
sulfide(train,d179,[d179_15, d179_1, d179_18]).
ketone(train,d179,[d179_20, d179_19, d179_18, d179_21]).
amine(train,d179,[d179_18, d179_35, d179_19, d179_15]).
amine(train,d179,[d179_21, d179_36, d179_22, d179_19]).
methyl(train,d179,[d179_11, d179_4, d179_12, d179_13, d179_14]).
methyl(train,d179,[d179_29, d179_26, d179_32, d179_33, d179_34]).
five_ring(train,d18,[d18_1, d18_21, d18_2, d18_3, d18_4]).
non_ar_hetero_5_ring(train,d18,[d18_1, d18_21, d18_2, d18_3, d18_4]).
ketone(train,d18,[d18_11, d18_9, d18_10, d18_8]).
amine(train,d18,[d18_8, d18_14, d18_9, d18_1]).
amine(train,d18,[d18_10, d18_15, d18_9, d18_12]).
methyl(train,d18,[d18_13, d18_12, d18_18, d18_19, d18_20]).
nitro(train,d18,[d18_5, d18_3, d18_6, d18_7]).
five_ring(train,d180,[d180_3, d180_4, d180_7, d180_8, d180_9]).
non_ar_hetero_5_ring(train,d180,[d180_3, d180_4, d180_7, d180_8, d180_9]).
six_ring(train,d180,[d180_1, d180_2, d180_3, d180_4, d180_5, d180_6]).
non_ar_6c_ring(train,d180,[d180_1, d180_2, d180_3, d180_4, d180_5, d180_6]).
ester(train,d180,[d180_18, d180_19, d180_20, d180_16, d180_17]).
ester(train,d180,[d180_18, d180_19, d180_20, d180_16, d180_24]).
ester(train,d180,[d180_18, d180_19, d180_20, d180_16, d180_15]).
alcohol(train,d180,[d180_19, d180_21, d180_18]).
ether(train,d180,[d180_19, d180_18, d180_21]).
ketone(train,d180,[d180_20, d180_18, d180_16, d180_19]).
amine(train,d180,[d180_17, d180_22, d180_23, d180_16]).
six_ring(train,d181,[d181_1, d181_2, d181_3, d181_4, d181_5, d181_6]).
non_ar_6c_ring(train,d181,[d181_1, d181_2, d181_3, d181_4, d181_5, d181_6]).
alcohol(train,d181,[d181_13, d181_16, d181_12]).
ether(train,d181,[d181_13, d181_12, d181_16]).
five_ring(train,d182,[d182_22, d182_24, d182_28, d182_27, d182_26]).
non_ar_hetero_5_ring(train,d182,[d182_22, d182_24, d182_28, d182_27, d182_26]).
six_ring(train,d182,[d182_1, d182_2, d182_3, d182_4, d182_5, d182_6]).
six_ring(train,d182,[d182_19, d182_21, d182_24, d182_28, d182_24, d182_22]).
six_ring(train,d182,[d182_21, d182_19, d182_22, d182_26, d182_22, d182_24]).
six_ring(train,d182,[d182_22, d182_24, d182_21, d182_25, d182_21, d182_19]).
six_ring(train,d182,[d182_24, d182_21, d182_19, d182_18, d182_19, d182_22]).
non_ar_hetero_6_ring(train,d182,[d182_19, d182_21, d182_24, d182_28, d182_24, d182_22]).
non_ar_hetero_6_ring(train,d182,[d182_21, d182_19, d182_22, d182_26, d182_22, d182_24]).
non_ar_hetero_6_ring(train,d182,[d182_21, d182_19, d182_22, d182_29, d182_22, d182_24]).
non_ar_hetero_6_ring(train,d182,[d182_22, d182_24, d182_21, d182_25, d182_21, d182_19]).
non_ar_hetero_6_ring(train,d182,[d182_24, d182_21, d182_19, d182_23, d182_19, d182_22]).
non_ar_hetero_6_ring(train,d182,[d182_24, d182_21, d182_19, d182_18, d182_19, d182_22]).
non_ar_6c_ring(train,d182,[d182_1, d182_2, d182_3, d182_4, d182_5, d182_6]).
ester(train,d182,[d182_30, d182_32, d182_33, d182_28, d182_24]).
ester(train,d182,[d182_30, d182_32, d182_33, d182_28, d182_31]).
ester(train,d182,[d182_30, d182_32, d182_33, d182_28, d182_27]).
sulfide(train,d182,[d182_26, d182_22, d182_27]).
ether(train,d182,[d182_12, d182_13, d182_4]).
ether(train,d182,[d182_32, d182_30, d182_34]).
ketone(train,d182,[d182_17, d182_14, d182_13, d182_18]).
ketone(train,d182,[d182_25, d182_21, d182_19, d182_24]).
ketone(train,d182,[d182_33, d182_30, d182_28, d182_32]).
amine(train,d182,[d182_18, d182_20, d182_19, d182_14]).
methyl(train,d182,[d182_35, d182_27, d182_36, d182_37, d182_38]).
methyl(train,d182,[d182_39, d182_27, d182_40, d182_41, d182_42]).
six_ring(train,d183,[d183_1, d183_2, d183_3, d183_4, d183_5, d183_6]).
six_ring(train,d183,[d183_3, d183_4, d183_12, d183_11, d183_10, d183_9]).
six_ring(train,d183,[d183_10, d183_11, d183_18, d183_17, d183_16, d183_15]).
six_ring(train,d183,[d183_16, d183_17, d183_25, d183_24, d183_23, d183_22]).
non_ar_6c_ring(train,d183,[d183_1, d183_2, d183_3, d183_4, d183_5, d183_6]).
non_ar_6c_ring(train,d183,[d183_3, d183_4, d183_12, d183_11, d183_10, d183_9]).
non_ar_6c_ring(train,d183,[d183_10, d183_11, d183_18, d183_17, d183_16, d183_15]).
non_ar_6c_ring(train,d183,[d183_16, d183_17, d183_25, d183_24, d183_23, d183_22]).
ester(train,d183,[d183_18, d183_36, d183_11, d183_17, d183_38]).
ester(train,d183,[d183_18, d183_36, d183_11, d183_17, d183_16]).
ester(train,d183,[d183_18, d183_36, d183_11, d183_17, d183_25]).
ester(train,d183,[d183_23, d183_56, d183_24, d183_22, d183_41]).
ester(train,d183,[d183_23, d183_56, d183_24, d183_22, d183_42]).
ester(train,d183,[d183_23, d183_56, d183_24, d183_22, d183_16]).
phenol(train,d183,[d183_32, d183_33, d183_5]).
alcohol(train,d183,[d183_30, d183_31, d183_2]).
alcohol(train,d183,[d183_36, d183_37, d183_18]).
alcohol(train,d183,[d183_38, d183_40, d183_17]).
alcohol(train,d183,[d183_56, d183_57, d183_23]).
ether(train,d183,[d183_30, d183_2, d183_31]).
ether(train,d183,[d183_36, d183_18, d183_37]).
ether(train,d183,[d183_38, d183_17, d183_40]).
ether(train,d183,[d183_56, d183_23, d183_57]).
ketone(train,d183,[d183_34, d183_12, d183_11, d183_4]).
ketone(train,d183,[d183_35, d183_25, d183_17, d183_24]).
ketone(train,d183,[d183_53, d183_51, d183_24, d183_52]).
amine(train,d183,[d183_52, d183_54, d183_55, d183_51]).
methyl(train,d183,[d183_26, d183_2, d183_27, d183_28, d183_29]).
methyl(train,d183,[d183_43, d183_41, d183_44, d183_45, d183_46]).
methyl(train,d183,[d183_47, d183_41, d183_48, d183_49, d183_50]).
six_ring(train,d184,[d184_1, d184_2, d184_3, d184_4, d184_5, d184_6]).
non_ar_6c_ring(train,d184,[d184_1, d184_2, d184_3, d184_4, d184_5, d184_6]).
amine(train,d184,[d184_20, d184_22, d184_23, d184_13]).
methyl(train,d184,[d184_16, d184_13, d184_17, d184_18, d184_19]).
five_ring(train,d185,[d185_1, d185_2, d185_6, d185_11, d185_3]).
non_ar_hetero_5_ring(train,d185,[d185_1, d185_2, d185_6, d185_11, d185_3]).
ester(train,d185,[d185_3, d185_11, d185_9, d185_1, d185_2]).
ester(train,d185,[d185_3, d185_11, d185_9, d185_1, d185_4]).
ester(train,d185,[d185_3, d185_11, d185_9, d185_1, d185_5]).
ester(train,d185,[d185_6, d185_11, d185_10, d185_2, d185_7]).
ester(train,d185,[d185_6, d185_11, d185_10, d185_2, d185_8]).
ester(train,d185,[d185_6, d185_11, d185_10, d185_2, d185_1]).
ether(train,d185,[d185_11, d185_3, d185_6]).
ketone(train,d185,[d185_9, d185_3, d185_1, d185_11]).
ketone(train,d185,[d185_10, d185_6, d185_11, d185_2]).
six_ring(train,d186,[d186a_35, d186a_36, d186a_37, d186a_38, d186a_39, d186a_40]).
six_ring(train,d186,[d186a_92, d186a_93, d186a_94, d186a_95, d186a_96, d186a_97]).
non_ar_hetero_6_ring(train,d186,[d186a_35, d186a_36, d186a_37, d186a_38, d186a_39, d186a_40]).
non_ar_hetero_6_ring(train,d186,[d186a_92, d186a_93, d186a_94, d186a_95, d186a_96, d186a_97]).
ester(train,d186,[d186a_14, d186a_13, d186a_23, d186a_24, d186a_25]).
ester(train,d186,[d186a_14, d186a_13, d186a_23, d186a_24, d186a_26]).
ester(train,d186,[d186a_14, d186a_13, d186a_23, d186a_24, d186a_30]).
ester(train,d186,[d186b_51, d186b_54, d186b_55, d186b_48, d186b_52]).
ester(train,d186,[d186b_51, d186b_54, d186b_55, d186b_48, d186b_53]).
ester(train,d186,[d186b_51, d186b_54, d186b_55, d186b_48, d186b_45]).
alcohol(train,d186,[d186a_4, d186a_5, d186a_1]).
alcohol(train,d186,[d186a_6, d186a_11, d186a_2]).
alcohol(train,d186,[d186a_48, d186a_50, d186a_38]).
alcohol(train,d186,[d186a_88, d186a_89, d186a_66]).
alcohol(train,d186,[d186a_101, d186a_103, d186a_97]).
alcohol(train,d186,[d186b_54, d186b_56, d186b_51]).
ether(train,d186,[d186a_4, d186a_1, d186a_5]).
ether(train,d186,[d186a_13, d186a_12, d186a_14]).
ether(train,d186,[d186a_34, d186a_25, d186a_35]).
ether(train,d186,[d186a_36, d186a_35, d186a_37]).
ether(train,d186,[d186a_48, d186a_38, d186a_50]).
ether(train,d186,[d186a_51, d186a_39, d186a_52]).
ether(train,d186,[d186a_88, d186a_66, d186a_89]).
ether(train,d186,[d186a_91, d186a_32, d186a_92]).
ether(train,d186,[d186a_93, d186a_92, d186a_94]).
ether(train,d186,[d186b_54, d186b_51, d186b_56]).
ketone(train,d186,[d186a_23, d186a_14, d186a_13, d186a_24]).
ketone(train,d186,[d186a_69, d186a_68, d186a_67, d186a_70]).
ketone(train,d186,[d186b_55, d186b_51, d186b_48, d186b_54]).
methoxy(train,d186,[d186a_52, d186a_51, d186a_53, d186a_54, d186a_55]).
methyl(train,d186,[d186a_7, d186a_2, d186a_10, d186a_8, d186a_9]).
methyl(train,d186,[d186a_17, d186a_16, d186a_20, d186a_21, d186a_22]).
methyl(train,d186,[d186a_26, d186a_24, d186a_27, d186a_28, d186a_29]).
methyl(train,d186,[d186a_43, d186a_37, d186a_45, d186a_46, d186a_47]).
methyl(train,d186,[d186a_52, d186a_51, d186a_53, d186a_54, d186a_55]).
methyl(train,d186,[d186a_56, d186a_39, d186a_57, d186a_58, d186a_59]).
methyl(train,d186,[d186a_61, d186a_31, d186a_62, d186a_63, d186a_64]).
methyl(train,d186,[d186a_74, d186a_67, d186a_75, d186a_76, d186a_77]).
methyl(train,d186,[d186a_79, d186a_70, d186a_80, d186a_81, d186a_82]).
methyl(train,d186,[d186a_84, d186a_66, d186a_85, d186a_86, d186a_87]).
methyl(train,d186,[d186a_106, d186a_104, d186a_107, d186a_108, d186a_109]).
methyl(train,d186,[d186a_110, d186a_104, d186a_111, d186a_112, d186a_113]).
methyl(train,d186,[d186a_114, d186a_104, d186a_115, d186a_116, d186a_117]).
methyl(train,d186,[d186a_118, d186a_94, d186a_120, d186a_121, d186a_122]).
methyl(train,d186,[d186b_1, d186b_2, d186b_3, d186b_4, d186b_5]).
six_ring(train,d187,[d187_1, d187_2, d187_3, d187_4, d187_5, d187_6]).
non_ar_6c_ring(train,d187,[d187_1, d187_2, d187_3, d187_4, d187_5, d187_6]).
phenol(train,d187,[d187_11, d187_25, d187_6]).
alcohol(train,d187,[d187_13, d187_23, d187_12]).
ether(train,d187,[d187_13, d187_12, d187_23]).
amine(train,d187,[d187_16, d187_24, d187_19, d187_14]).
methyl(train,d187,[d187_19, d187_16, d187_20, d187_21, d187_22]).
six_ring(train,d188,[d188a_1, d188a_2, d188a_3, d188a_4, d188a_5, d188a_6]).
six_ring(train,d188,[d188b_1, d188b_2, d188b_3, d188b_4, d188b_5, d188b_6]).
six_ring(train,d188,[d188c_1, d188c_2, d188c_3, d188c_4, d188c_5, d188c_6]).
six_ring(train,d188,[d188d_1, d188d_2, d188d_3, d188d_4, d188d_5, d188d_6]).
non_ar_6c_ring(train,d188,[d188a_1, d188a_2, d188a_3, d188a_4, d188a_5, d188a_6]).
non_ar_6c_ring(train,d188,[d188b_1, d188b_2, d188b_3, d188b_4, d188b_5, d188b_6]).
non_ar_6c_ring(train,d188,[d188c_1, d188c_2, d188c_3, d188c_4, d188c_5, d188c_6]).
non_ar_6c_ring(train,d188,[d188d_1, d188d_2, d188d_3, d188d_4, d188d_5, d188d_6]).
methyl(train,d188,[d188a_11, d188a_5, d188a_12, d188a_13, d188a_14]).
methyl(train,d188,[d188a_15, d188a_3, d188a_16, d188a_17, d188a_18]).
methyl(train,d188,[d188b_10, d188b_5, d188b_11, d188b_12, d188b_13]).
methyl(train,d188,[d188b_15, d188b_2, d188b_16, d188b_17, d188b_18]).
methyl(train,d188,[d188c_11, d188c_5, d188c_12, d188c_13, d188c_14]).
methyl(train,d188,[d188c_15, d188c_4, d188c_16, d188c_17, d188c_18]).
methyl(train,d188,[d188d_13, d188d_12, d188d_16, d188d_17, d188d_18]).
six_ring(train,d189,[d189_1, d189_2, d189_3, d189_4, d189_5, d189_6]).
non_ar_6c_ring(train,d189,[d189_1, d189_2, d189_3, d189_4, d189_5, d189_6]).
alcohol(train,d189,[d189_21, d189_23, d189_12]).
ether(train,d189,[d189_21, d189_12, d189_23]).
amine(train,d189,[d189_14, d189_24, d189_17, d189_13]).
methyl(train,d189,[d189_17, d189_14, d189_18, d189_19, d189_20]).
five_ring(train,d19,[d19_4, d19_13, d19_17, d19_16, d19_5]).
non_ar_5c_ring(train,d19,[d19_4, d19_13, d19_17, d19_16, d19_5]).
six_ring(train,d19,[d19_1, d19_2, d19_3, d19_4, d19_5, d19_6]).
six_ring(train,d19,[d19_3, d19_4, d19_13, d19_12, d19_11, d19_10]).
non_ar_6c_ring(train,d19,[d19_1, d19_2, d19_3, d19_4, d19_5, d19_6]).
non_ar_6c_ring(train,d19,[d19_3, d19_4, d19_13, d19_12, d19_11, d19_10]).
nitro(train,d19,[d19_22, d19_10, d19_23, d19_24]).
six_ring(train,d190,[d190_1, d190_2, d190_3, d190_4, d190_5, d190_6]).
non_ar_6c_ring(train,d190,[d190_1, d190_2, d190_3, d190_4, d190_5, d190_6]).
methyl(train,d190,[d190_12, d190_5, d190_13, d190_14, d190_15]).
ester(train,d191,[d191_12, d191_16, d191_15, d191_10, d191_13]).
ester(train,d191,[d191_12, d191_16, d191_15, d191_10, d191_14]).
ester(train,d191,[d191_12, d191_16, d191_15, d191_10, d191_2]).
ether(train,d191,[d191_16, d191_12, d191_17]).
ketone(train,d191,[d191_15, d191_12, d191_10, d191_16]).
methyl(train,d191,[d191_1, d191_2, d191_3, d191_4, d191_5]).
methyl(train,d191,[d191_6, d191_2, d191_7, d191_8, d191_9]).
six_ring(train,d192,[d192_1, d192_2, d192_3, d192_4, d192_5, d192_6]).
six_ring(train,d192,[d192_23, d192_25, d192_26, d192_27, d192_28, d192_29]).
non_ar_6c_ring(train,d192,[d192_1, d192_2, d192_3, d192_4, d192_5, d192_6]).
non_ar_6c_ring(train,d192,[d192_23, d192_25, d192_26, d192_27, d192_28, d192_29]).
ether(train,d192,[d192_13, d192_12, d192_14]).
ketone(train,d192,[d192_21, d192_12, d192_13, d192_4]).
amine(train,d192,[d192_11, d192_19, d192_20, d192_5]).
six_ring(train,d193,[d193_1, d193_2, d193_3, d193_4, d193_5, d193_6]).
non_ar_hetero_6_ring(train,d193,[d193_1, d193_2, d193_3, d193_4, d193_5, d193_6]).
ether(train,d193,[d193_2, d193_1, d193_3]).
ether(train,d193,[d193_5, d193_4, d193_6]).
five_ring(train,d195,[d195_3, d195_4, d195_7, d195_8, d195_9]).
non_ar_hetero_5_ring(train,d195,[d195_3, d195_4, d195_7, d195_8, d195_9]).
six_ring(train,d195,[d195_1, d195_2, d195_3, d195_4, d195_5, d195_6]).
non_ar_6c_ring(train,d195,[d195_1, d195_2, d195_3, d195_4, d195_5, d195_6]).
five_ring(train,d196,[d196_1, d196_2, d196_3, d196_4, d196_5]).
non_ar_hetero_5_ring(train,d196,[d196_1, d196_2, d196_3, d196_4, d196_5]).
five_ring(train,d197,[d197_23, d197_24, d197_27, d197_26, d197_25]).
non_ar_hetero_5_ring(train,d197,[d197_23, d197_24, d197_27, d197_26, d197_25]).
six_ring(train,d197,[d197_1, d197_2, d197_3, d197_4, d197_5, d197_6]).
six_ring(train,d197,[d197_12, d197_13, d197_14, d197_15, d197_16, d197_17]).
non_ar_6c_ring(train,d197,[d197_1, d197_2, d197_3, d197_4, d197_5, d197_6]).
non_ar_6c_ring(train,d197,[d197_12, d197_13, d197_14, d197_15, d197_16, d197_17]).
ketone(train,d197,[d197_39, d197_25, d197_23, d197_26]).
ketone(train,d197,[d197_40, d197_27, d197_24, d197_26]).
methyl(train,d197,[d197_36, d197_33, d197_41, d197_42, d197_43]).
amine(train,d198,[d198_6, d198_11, d198_9, d198_2]).
amine(train,d198,[d198_10, d198_13, d198_9, d198_12]).
methyl(train,d198,[d198_1, d198_2, d198_3, d198_4, d198_5]).
methyl(train,d198,[d198_14, d198_12, d198_17, d198_18, d198_19]).
six_ring(train,d199,[d199_1, d199_2, d199_3, d199_4, d199_5, d199_6]).
six_ring(train,d199,[d199_13, d199_14, d199_15, d199_16, d199_17, d199_18]).
non_ar_6c_ring(train,d199,[d199_1, d199_2, d199_3, d199_4, d199_5, d199_6]).
non_ar_6c_ring(train,d199,[d199_13, d199_14, d199_15, d199_16, d199_17, d199_18]).
five_ring(train,d2,[d2_3, d2_11, d2_12, d2_13, d2_4]).
non_ar_hetero_5_ring(train,d2,[d2_3, d2_4, d2_13, d2_12, d2_11]).
six_ring(train,d2,[d2_1, d2_2, d2_3, d2_4, d2_5, d2_6]).
six_ring(train,d2,[d2_11, d2_12, d2_14, d2_15, d2_16, d2_17]).
non_ar_6c_ring(train,d2,[d2_1, d2_2, d2_3, d2_4, d2_5, d2_6]).
non_ar_6c_ring(train,d2,[d2_11, d2_12, d2_14, d2_15, d2_16, d2_17]).
amine(train,d2,[d2_28, d2_29, d2_30, d2_16]).
methyl(train,d2,[d2_22, d2_21, d2_25, d2_26, d2_27]).
six_ring(train,d20,[d20_1, d20_2, d20_3, d20_4, d20_5, d20_6]).
non_ar_6c_ring(train,d20,[d20_1, d20_2, d20_3, d20_4, d20_5, d20_6]).
ether(train,d20,[d20_10, d20_13, d20_4]).
amine(train,d20,[d20_11, d20_19, d20_20, d20_5]).
methoxy(train,d20,[d20_13, d20_10, d20_14, d20_15, d20_16]).
methyl(train,d20,[d20_13, d20_10, d20_14, d20_15, d20_16]).
nitro(train,d20,[d20_12, d20_1, d20_17, d20_18]).
six_ring(train,d200,[d200_1, d200_2, d200_3, d200_4, d200_5, d200_6]).
non_ar_6c_ring(train,d200,[d200_1, d200_2, d200_3, d200_4, d200_5, d200_6]).
six_ring(train,d201,[d201_1, d201_2, d201_11, d201_21, d201_20, d201_19]).
six_ring(train,d201,[d201_1, d201_2, d201_3, d201_4, d201_5, d201_6]).
six_ring(train,d201,[d201_2, d201_3, d201_7, d201_13, d201_12, d201_11]).
six_ring(train,d201,[d201_3, d201_4, d201_10, d201_9, d201_8, d201_7]).
six_ring(train,d201,[d201_8, d201_9, d201_17, d201_16, d201_15, d201_14]).
six_ring(train,d201,[d201_19, d201_20, d201_25, d201_24, d201_23, d201_22]).
non_ar_6c_ring(train,d201,[d201_1, d201_2, d201_11, d201_21, d201_20, d201_19]).
non_ar_6c_ring(train,d201,[d201_1, d201_2, d201_3, d201_4, d201_5, d201_6]).
non_ar_6c_ring(train,d201,[d201_2, d201_3, d201_7, d201_13, d201_12, d201_11]).
non_ar_6c_ring(train,d201,[d201_3, d201_4, d201_10, d201_9, d201_8, d201_7]).
non_ar_6c_ring(train,d201,[d201_8, d201_9, d201_17, d201_16, d201_15, d201_14]).
non_ar_6c_ring(train,d201,[d201_19, d201_20, d201_25, d201_24, d201_23, d201_22]).
ketone(train,d201,[d201_18, d201_10, d201_4, d201_9]).
ketone(train,d201,[d201_38, d201_21, d201_11, d201_20]).
six_ring(train,d202,[d202_1, d202_2, d202_3, d202_4, d202_5, d202_6]).
non_ar_6c_ring(train,d202,[d202_1, d202_2, d202_3, d202_4, d202_5, d202_6]).
ketone(train,d202,[d202_23, d202_5, d202_4, d202_6]).
methyl(train,d202,[d202_11, d202_1, d202_12, d202_13, d202_14]).
methyl(train,d202,[d202_15, d202_1, d202_16, d202_17, d202_18]).
methyl(train,d202,[d202_19, d202_3, d202_20, d202_21, d202_22]).
six_ring(train,d203,[d203_1, d203_2, d203_3, d203_4, d203_5, d203_6]).
non_ar_hetero_6_ring(train,d203,[d203_1, d203_2, d203_3, d203_4, d203_5, d203_6]).
amine(train,d203,[d203_7, d203_10, d203_11, d203_5]).
amine(train,d203,[d203_8, d203_14, d203_15, d203_3]).
amine(train,d203,[d203_9, d203_12, d203_13, d203_1]).
amine(train,d204,[d204_1, d204_17, d204_6, d204_2]).
methyl(train,d204,[d204_2, d204_1, d204_3, d204_4, d204_5]).
methyl(train,d204,[d204_8, d204_7, d204_10, d204_11, d204_9]).
methyl(train,d204,[d204_12, d204_7, d204_13, d204_14, d204_15]).
six_ring(train,d205,[d205_2, d205_3, d205_6, d205_7, d205_6, d205_1]).
six_ring(train,d205,[d205_2, d205_4, d205_16, d205_17, d205_16, d205_5]).
six_ring(train,d205,[d205_6, d205_1, d205_2, d205_4, d205_2, d205_3]).
six_ring(train,d205,[d205_6, d205_1, d205_2, d205_5, d205_2, d205_3]).
six_ring(train,d205,[d205_16, d205_4, d205_2, d205_3, d205_2, d205_5]).
six_ring(train,d205,[d205_16, d205_4, d205_2, d205_1, d205_2, d205_5]).
non_ar_hetero_6_ring(train,d205,[d205_2, d205_3, d205_6, d205_7, d205_6, d205_1]).
non_ar_hetero_6_ring(train,d205,[d205_2, d205_4, d205_16, d205_17, d205_16, d205_5]).
non_ar_hetero_6_ring(train,d205,[d205_6, d205_1, d205_2, d205_4, d205_2, d205_3]).
non_ar_hetero_6_ring(train,d205,[d205_6, d205_1, d205_2, d205_5, d205_2, d205_3]).
non_ar_hetero_6_ring(train,d205,[d205_16, d205_4, d205_2, d205_3, d205_2, d205_5]).
non_ar_hetero_6_ring(train,d205,[d205_16, d205_4, d205_2, d205_1, d205_2, d205_5]).
sulfide(train,d205,[d205_1, d205_2, d205_6]).
sulfide(train,d205,[d205_5, d205_16, d205_2]).
methyl(train,d205,[d205_8, d205_7, d205_10, d205_11, d205_9]).
methyl(train,d205,[d205_12, d205_7, d205_13, d205_14, d205_15]).
methyl(train,d205,[d205_18, d205_17, d205_19, d205_20, d205_21]).
methyl(train,d205,[d205_22, d205_17, d205_23, d205_24, d205_25]).
six_ring(train,d206,[d206_1, d206_2, d206_3, d206_4, d206_5, d206_6]).
non_ar_6c_ring(train,d206,[d206_1, d206_2, d206_3, d206_4, d206_5, d206_6]).
methyl(train,d206,[d206_7, d206_5, d206_18, d206_19, d206_20]).
methyl(train,d206,[d206_9, d206_8, d206_21, d206_22, d206_23]).
six_ring(train,d207,[d207_1, d207_2, d207_3, d207_4, d207_5, d207_6]).
non_ar_6c_ring(train,d207,[d207_1, d207_2, d207_3, d207_4, d207_5, d207_6]).
ether(train,d207,[d207_12, d207_11, d207_13]).
ether(train,d207,[d207_23, d207_22, d207_24]).
ketone(train,d207,[d207_18, d207_11, d207_12, d207_4]).
ketone(train,d207,[d207_29, d207_22, d207_23, d207_3]).
amine(train,d208_1,[d208_1_1, d208_1_10, d208_1_9, d208_1_2]).
amine(train,d208_1,[d208_1_7, d208_1_13, d208_1_14, d208_1_6]).
amine(train,d208_1,[d208_1_3, d208_1_11, d208_1_5, d208_1_2]).
amine(train,d208_1,[d208_1_5, d208_1_12, d208_1_6, d208_1_3]).
sulfide(train,d208_2,[d208_2_2, d208_2_1, d208_2_6]).
sulfide(train,d208_2,[d208_2_3, d208_2_1, d208_2_39]).
sulfide(train,d208_2,[d208_2_4, d208_2_1, d208_2_23]).
sulfide(train,d208_2,[d208_2_5, d208_2_1, d208_2_55]).
methyl(train,d208_2,[d208_2_9, d208_2_8, d208_2_12, d208_2_13, d208_2_14]).
methyl(train,d208_2,[d208_2_16, d208_2_15, d208_2_19, d208_2_20, d208_2_21]).
methyl(train,d208_2,[d208_2_26, d208_2_25, d208_2_29, d208_2_30, d208_2_31]).
methyl(train,d208_2,[d208_2_33, d208_2_32, d208_2_36, d208_2_37, d208_2_38]).
methyl(train,d208_2,[d208_2_42, d208_2_41, d208_2_45, d208_2_46, d208_2_47]).
methyl(train,d208_2,[d208_2_49, d208_2_48, d208_2_52, d208_2_53, d208_2_54]).
methyl(train,d208_2,[d208_2_58, d208_2_57, d208_2_61, d208_2_62, d208_2_63]).
methyl(train,d208_2,[d208_2_65, d208_2_64, d208_2_68, d208_2_69, d208_2_70]).
six_ring(train,d209,[d209_1, d209_2, d209_3, d209_4, d209_5, d209_6]).
non_ar_6c_ring(train,d209,[d209_1, d209_2, d209_3, d209_4, d209_5, d209_6]).
phenol(train,d209,[d209_10, d209_16, d209_2]).
ether(train,d209,[d209_11, d209_12, d209_3]).
methoxy(train,d209,[d209_12, d209_11, d209_13, d209_14, d209_15]).
methyl(train,d209,[d209_12, d209_11, d209_13, d209_14, d209_15]).
six_ring(train,d21,[d21_1, d21_2, d21_3, d21_4, d21_5, d21_6]).
six_ring(train,d21,[d21_11, d21_12, d21_13, d21_14, d21_15, d21_16]).
non_ar_6c_ring(train,d21,[d21_1, d21_2, d21_3, d21_4, d21_5, d21_6]).
non_ar_6c_ring(train,d21,[d21_11, d21_12, d21_13, d21_14, d21_15, d21_16]).
ar_halide(train,d21,[d21_21, d21_16]).
ar_halide(train,d21,[d21_22, d21_14]).
ether(train,d21,[d21_20, d21_11, d21_4]).
nitro(train,d21,[d21_23, d21_1, d21_24, d21_25]).
six_ring(train,d210,[d210_1, d210_2, d210_3, d210_4, d210_5, d210_6]).
non_ar_6c_ring(train,d210,[d210_1, d210_2, d210_3, d210_4, d210_5, d210_6]).
ketone(train,d210,[d210_26, d210_16, d210_15, d210_17]).
amine(train,d210,[d210_15, d210_27, d210_5, d210_16]).
methyl(train,d210,[d210_18, d210_17, d210_19, d210_20, d210_21]).
methyl(train,d210,[d210_22, d210_17, d210_23, d210_24, d210_25]).
imine(train,d211,[d211_15, d211_16]).
ester(train,d211,[d211_18, d211_17, d211_22, d211_19, d211_20]).
ester(train,d211,[d211_18, d211_17, d211_22, d211_19, d211_26]).
sulfide(train,d211,[d211_2, d211_1, d211_6]).
ether(train,d211,[d211_17, d211_16, d211_18]).
ketone(train,d211,[d211_22, d211_18, d211_17, d211_19]).
amine(train,d211,[d211_19, d211_26, d211_20, d211_18]).
methyl(train,d211,[d211_1, d211_2, d211_3, d211_4, d211_5]).
methyl(train,d211,[d211_7, d211_6, d211_10, d211_8, d211_9]).
methyl(train,d211,[d211_11, d211_6, d211_12, d211_13, d211_14]).
methyl(train,d211,[d211_20, d211_19, d211_23, d211_24, d211_25]).
six_ring(train,d212,[d212_1, d212_2, d212_12, d212_11, d212_10, d212_9]).
six_ring(train,d212,[d212_1, d212_2, d212_3, d212_4, d212_5, d212_6]).
six_ring(train,d212,[d212_24, d212_25, d212_26, d212_27, d212_28, d212_29]).
six_ring(train,d212,[d212_27, d212_28, d212_34, d212_33, d212_32, d212_31]).
non_ar_6c_ring(train,d212,[d212_1, d212_2, d212_12, d212_11, d212_10, d212_9]).
non_ar_6c_ring(train,d212,[d212_1, d212_2, d212_3, d212_4, d212_5, d212_6]).
non_ar_6c_ring(train,d212,[d212_24, d212_25, d212_26, d212_27, d212_28, d212_29]).
non_ar_6c_ring(train,d212,[d212_27, d212_28, d212_34, d212_33, d212_32, d212_31]).
phenol(train,d212,[d212_39, d212_40, d212_29]).
sulfide(train,d212,[d212_17, d212_18, d212_6]).
sulfide(train,d212,[d212_41, d212_26, d212_42]).
ether(train,d212,[d212_18, d212_17, d212_19]).
ether(train,d212,[d212_39, d212_29, d212_40]).
ether(train,d212,[d212_42, d212_41, d212_43]).
sulfo(train,d212,[d212_17, d212_18, d212_20, d212_21, d212_6]).
sulfo(train,d212,[d212_41, d212_42, d212_44, d212_45, d212_26]).
six_ring(train,d213,[d213_1, d213_2, d213_3, d213_4, d213_5, d213_6]).
non_ar_6c_ring(train,d213,[d213_1, d213_2, d213_3, d213_4, d213_5, d213_6]).
alcohol(train,d213,[d213_15, d213_17, d213_12]).
ether(train,d213,[d213_15, d213_12, d213_17]).
ketone(train,d213,[d213_16, d213_12, d213_15, d213_4]).
amine(train,d213,[d213_11, d213_13, d213_14, d213_5]).
six_ring(train,d214,[d214_1, d214_2, d214_3, d214_4, d214_5, d214_6]).
six_ring(train,d214,[d214_21, d214_25, d214_26, d214_27, d214_28, d214_29]).
non_ar_6c_ring(train,d214,[d214_1, d214_2, d214_3, d214_4, d214_5, d214_6]).
non_ar_6c_ring(train,d214,[d214_21, d214_25, d214_26, d214_27, d214_28, d214_29]).
amine(train,d214,[d214_17, d214_22, d214_3, d214_19]).
amine(train,d214,[d214_20, d214_23, d214_21, d214_19]).
six_ring(train,d215,[d215_1, d215_2, d215_3, d215_4, d215_5, d215_6]).
non_ar_hetero_6_ring(train,d215,[d215_1, d215_2, d215_3, d215_4, d215_5, d215_6]).
amine(train,d215,[d215_18, d215_20, d215_21, d215_17]).
methyl(train,d215,[d215_11, d215_10, d215_14, d215_15, d215_16]).
six_ring(train,d216,[d216_2, d216_3, d216_6, d216_7, d216_6, d216_1]).
six_ring(train,d216,[d216_2, d216_4, d216_16, d216_17, d216_16, d216_5]).
six_ring(train,d216,[d216_6, d216_1, d216_2, d216_4, d216_2, d216_3]).
six_ring(train,d216,[d216_6, d216_1, d216_2, d216_5, d216_2, d216_3]).
six_ring(train,d216,[d216_16, d216_4, d216_2, d216_3, d216_2, d216_5]).
six_ring(train,d216,[d216_16, d216_4, d216_2, d216_1, d216_2, d216_5]).
non_ar_hetero_6_ring(train,d216,[d216_2, d216_3, d216_6, d216_7, d216_6, d216_1]).
non_ar_hetero_6_ring(train,d216,[d216_2, d216_4, d216_16, d216_17, d216_16, d216_5]).
non_ar_hetero_6_ring(train,d216,[d216_6, d216_1, d216_2, d216_4, d216_2, d216_3]).
non_ar_hetero_6_ring(train,d216,[d216_6, d216_1, d216_2, d216_5, d216_2, d216_3]).
non_ar_hetero_6_ring(train,d216,[d216_16, d216_4, d216_2, d216_3, d216_2, d216_5]).
non_ar_hetero_6_ring(train,d216,[d216_16, d216_4, d216_2, d216_1, d216_2, d216_5]).
sulfide(train,d216,[d216_1, d216_2, d216_6]).
sulfide(train,d216,[d216_5, d216_16, d216_2]).
methyl(train,d216,[d216_8, d216_7, d216_10, d216_11, d216_9]).
methyl(train,d216,[d216_12, d216_7, d216_13, d216_14, d216_15]).
methyl(train,d216,[d216_18, d216_17, d216_19, d216_20, d216_21]).
methyl(train,d216,[d216_22, d216_17, d216_23, d216_24, d216_25]).
five_ring(train,d217,[d217_12, d217_13, d217_14, d217_15, d217_16]).
non_ar_hetero_5_ring(train,d217,[d217_12, d217_13, d217_14, d217_15, d217_16]).
six_ring(train,d217,[d217_1, d217_2, d217_3, d217_4, d217_5, d217_6]).
non_ar_6c_ring(train,d217,[d217_1, d217_2, d217_3, d217_4, d217_5, d217_6]).
imine(train,d217,[d217_13, d217_12]).
ketone(train,d217,[d217_23, d217_15, d217_14, d217_16]).
methyl(train,d217,[d217_19, d217_13, d217_20, d217_21, d217_22]).
sulfide(train,d218,[d218_17, d218_16, d218_18]).
methyl(train,d218,[d218_1, d218_2, d218_3, d218_4, d218_5]).
methyl(train,d218,[d218_10, d218_9, d218_13, d218_14, d218_15]).
sulfide(train,d219,[d219_17, d219_16, d219_18]).
sulfide(train,d219,[d219_18, d219_17, d219_19]).
methyl(train,d219,[d219_1, d219_2, d219_3, d219_4, d219_5]).
methyl(train,d219,[d219_10, d219_9, d219_13, d219_14, d219_15]).
methyl(train,d219,[d219_24, d219_23, d219_27, d219_28, d219_29]).
methyl(train,d219,[d219_31, d219_30, d219_34, d219_35, d219_36]).
six_ring(train,d22,[d22_1, d22_2, d22_3, d22_4, d22_5, d22_6]).
six_ring(train,d22,[d22_12, d22_13, d22_14, d22_15, d22_16, d22_17]).
non_ar_6c_ring(train,d22,[d22_1, d22_2, d22_3, d22_4, d22_5, d22_6]).
non_ar_6c_ring(train,d22,[d22_12, d22_13, d22_14, d22_15, d22_16, d22_17]).
amine(train,d22,[d22_22, d22_25, d22_4, d22_12]).
six_ring(train,d220,[d220a_1, d220a_2, d220a_3, d220a_4, d220a_5, d220a_6]).
six_ring(train,d220,[d220b_1, d220b_2, d220b_3, d220b_4, d220b_5, d220b_6]).
non_ar_6c_ring(train,d220,[d220a_1, d220a_2, d220a_3, d220a_4, d220a_5, d220a_6]).
non_ar_6c_ring(train,d220,[d220b_1, d220b_2, d220b_3, d220b_4, d220b_5, d220b_6]).
methyl(train,d220,[d220a_11, d220a_5, d220a_12, d220a_13, d220a_14]).
methyl(train,d220,[d220b_10, d220b_5, d220b_11, d220b_12, d220b_13]).
six_ring(train,d221,[d221_1, d221_2, d221_3, d221_4, d221_5, d221_6]).
six_ring(train,d221,[d221_3, d221_4, d221_13, d221_12, d221_11, d221_10]).
six_ring(train,d221,[d221_23, d221_24, d221_25, d221_26, d221_27, d221_28]).
non_ar_6c_ring(train,d221,[d221_1, d221_2, d221_3, d221_4, d221_5, d221_6]).
non_ar_6c_ring(train,d221,[d221_3, d221_4, d221_13, d221_12, d221_11, d221_10]).
non_ar_6c_ring(train,d221,[d221_23, d221_24, d221_25, d221_26, d221_27, d221_28]).
phenol(train,d221,[d221_36, d221_37, d221_12]).
sulfide(train,d221,[d221_16, d221_1, d221_17]).
sulfide(train,d221,[d221_33, d221_26, d221_34]).
ether(train,d221,[d221_17, d221_16, d221_18]).
ether(train,d221,[d221_34, d221_33, d221_35]).
ether(train,d221,[d221_36, d221_12, d221_37]).
sulfo(train,d221,[d221_16, d221_17, d221_19, d221_20, d221_1]).
sulfo(train,d221,[d221_33, d221_34, d221_38, d221_39, d221_26]).
six_ring(train,d222,[d222_1, d222_2, d222_3, d222_4, d222_5, d222_6]).
non_ar_6c_ring(train,d222,[d222_1, d222_2, d222_3, d222_4, d222_5, d222_6]).
ester(train,d222,[d222_27, d222_26, d222_33, d222_28, d222_29]).
ester(train,d222,[d222_27, d222_26, d222_33, d222_28, d222_34]).
ether(train,d222,[d222_26, d222_2, d222_27]).
ketone(train,d222,[d222_33, d222_27, d222_26, d222_28]).
amine(train,d222,[d222_28, d222_34, d222_29, d222_27]).
methyl(train,d222,[d222_10, d222_9, d222_11, d222_12, d222_13]).
methyl(train,d222,[d222_14, d222_9, d222_15, d222_16, d222_17]).
methyl(train,d222,[d222_18, d222_4, d222_19, d222_20, d222_21]).
methyl(train,d222,[d222_22, d222_6, d222_23, d222_24, d222_25]).
methyl(train,d222,[d222_29, d222_28, d222_30, d222_31, d222_32]).
six_ring(train,d223,[d223_1, d223_2, d223_3, d223_4, d223_5, d223_6]).
non_ar_6c_ring(train,d223,[d223_1, d223_2, d223_3, d223_4, d223_5, d223_6]).
amine(train,d223,[d223_14, d223_17, d223_18, d223_13]).
amine(train,d223,[d223_12, d223_16, d223_5, d223_13]).
five_ring(train,d224,[d224_1, d224_2, d224_3, d224_4, d224_5]).
non_ar_hetero_5_ring(train,d224,[d224_1, d224_2, d224_3, d224_4, d224_5]).
sulfide(train,d224,[d224_5, d224_1, d224_4]).
sulfo(train,d224,[d224_5, d224_12, d224_13, d224_1, d224_4]).
ester(train,d225,[d225a_28, d225a_25, d225a_29, d225a_30, d225a_31]).
ester(train,d225,[d225a_28, d225a_25, d225a_29, d225a_30, d225a_32]).
ester(train,d225,[d225a_28, d225a_25, d225a_29, d225a_30, d225a_33]).
ester(train,d225,[d225b_28, d225b_25, d225b_29, d225b_30, d225b_31]).
ester(train,d225,[d225b_28, d225b_25, d225b_29, d225b_30, d225b_32]).
ester(train,d225,[d225b_28, d225b_25, d225b_29, d225b_30, d225b_33]).
ether(train,d225,[d225a_25, d225a_24, d225a_28]).
ether(train,d225,[d225b_25, d225b_24, d225b_28]).
ketone(train,d225,[d225a_29, d225a_28, d225a_25, d225a_30]).
ketone(train,d225,[d225b_29, d225b_28, d225b_25, d225b_30]).
methyl(train,d225,[d225a_1, d225a_2, d225a_3, d225a_4, d225a_5]).
methyl(train,d225,[d225a_6, d225a_2, d225a_7, d225a_8, d225a_9]).
methyl(train,d225,[d225a_19, d225a_16, d225a_20, d225a_21, d225a_22]).
methyl(train,d225,[d225a_30, d225a_28, d225a_31, d225a_32, d225a_33]).
methyl(train,d225,[d225b_1, d225b_2, d225b_3, d225b_4, d225b_5]).
methyl(train,d225,[d225b_6, d225b_2, d225b_7, d225b_8, d225b_9]).
methyl(train,d225,[d225b_19, d225b_16, d225b_20, d225b_21, d225b_22]).
methyl(train,d225,[d225b_30, d225b_28, d225b_31, d225b_32, d225b_33]).
six_ring(train,d226,[d226_1, d226_2, d226_3, d226_4, d226_5, d226_6]).
non_ar_hetero_6_ring(train,d226,[d226_1, d226_2, d226_3, d226_4, d226_5, d226_6]).
ar_halide(train,d226,[d226_12, d226_11]).
ar_halide(train,d227,[d227_1, d227_2]).
ar_halide(train,d227,[d227_7, d227_6]).
ar_halide(train,d227,[d227_10, d227_3]).
alkyl_halide(train,d227,[d227_1, d227_2]).
alkyl_halide(train,d227,[d227_7, d227_6]).
alkyl_halide(train,d227,[d227_10, d227_3]).
ar_halide(train,d228,[d228_2, d228_1]).
ar_halide(train,d228,[d228_6, d228_3]).
alkyl_halide(train,d228,[d228_2, d228_1]).
alkyl_halide(train,d228,[d228_6, d228_3]).
ar_halide(train,d229,[d229_3, d229_1]).
ar_halide(train,d229,[d229_6, d229_2]).
alkyl_halide(train,d229,[d229_3, d229_1]).
alkyl_halide(train,d229,[d229_6, d229_2]).
ar_halide(train,d230,[d230_1, d230_2]).
ar_halide(train,d230,[d230_7, d230_5]).
alkyl_halide(train,d230,[d230_7, d230_5]).
five_ring(train,d231,[d231_33, d231_34, d231_45, d231_44, d231_43]).
non_ar_5c_ring(train,d231,[d231_33, d231_34, d231_45, d231_44, d231_43]).
six_ring(train,d231,[d231_1, d231_2, d231_3, d231_4, d231_5, d231_6]).
six_ring(train,d231,[d231_13, d231_14, d231_15, d231_16, d231_17, d231_18]).
six_ring(train,d231,[d231_15, d231_16, d231_29, d231_28, d231_27, d231_26]).
six_ring(train,d231,[d231_28, d231_29, d231_36, d231_35, d231_34, d231_33]).
non_ar_6c_ring(train,d231,[d231_1, d231_2, d231_3, d231_4, d231_5, d231_6]).
non_ar_6c_ring(train,d231,[d231_13, d231_14, d231_15, d231_16, d231_17, d231_18]).
non_ar_6c_ring(train,d231,[d231_15, d231_16, d231_29, d231_28, d231_27, d231_26]).
non_ar_6c_ring(train,d231,[d231_28, d231_29, d231_36, d231_35, d231_34, d231_33]).
ar_halide(train,d231,[d231_57, d231_54]).
ar_halide(train,d231,[d231_64, d231_61]).
alkyl_halide(train,d231,[d231_57, d231_54]).
alkyl_halide(train,d231,[d231_64, d231_61]).
ether(train,d231,[d231_12, d231_11, d231_13]).
ketone(train,d231,[d231_51, d231_11, d231_12, d231_6]).
methyl(train,d231,[d231_68, d231_34, d231_69, d231_70, d231_71]).
methyl(train,d231,[d231_74, d231_72, d231_75, d231_76, d231_77]).
methyl(train,d231,[d231_89, d231_86, d231_90, d231_91, d231_92]).
methyl(train,d231,[d231_93, d231_86, d231_95, d231_96, d231_97]).
five_ring(train,d232,[d232_7, d232_1, d232_2, d232_3, d232_2]).
non_ar_hetero_5_ring(train,d232,[d232_1, d232_7, d232_2, d232_3, d232_2]).
non_ar_hetero_5_ring(train,d232,[d232_1, d232_7, d232_2, d232_10, d232_2]).
non_ar_hetero_5_ring(train,d232,[d232_2, d232_7, d232_1, d232_8, d232_1]).
non_ar_hetero_5_ring(train,d232,[d232_2, d232_7, d232_1, d232_9, d232_1]).
six_ring(train,d232,[d232_1, d232_2, d232_7, d232_1, d232_2, d232_7]).
non_ar_hetero_6_ring(train,d232,[d232_1, d232_2, d232_7, d232_1, d232_2, d232_7]).
ether(train,d232,[d232_7, d232_1, d232_2]).
methyl(train,d232,[d232_3, d232_2, d232_4, d232_5, d232_6]).
six_ring(train,d233,[d233_1, d233_2, d233_3, d233_4, d233_5, d233_6]).
non_ar_6c_ring(train,d233,[d233_1, d233_2, d233_3, d233_4, d233_5, d233_6]).
ar_halide(train,d233,[d233_9, d233_5]).
ar_halide(train,d233,[d233_10, d233_4]).
ar_halide(train,d233,[d233_11, d233_2]).
phenol(train,d233,[d233_14, d233_28, d233_15]).
ether(train,d233,[d233_13, d233_12, d233_16]).
ether(train,d233,[d233_14, d233_15, d233_28]).
ether(train,d233,[d233_18, d233_16, d233_19]).
ether(train,d233,[d233_23, d233_16, d233_24]).
ketone(train,d233,[d233_14, d233_12, d233_1, d233_13]).
methoxy(train,d233,[d233_19, d233_18, d233_20, d233_21, d233_22]).
methoxy(train,d233,[d233_24, d233_23, d233_25, d233_26, d233_27]).
methyl(train,d233,[d233_19, d233_18, d233_20, d233_21, d233_22]).
methyl(train,d233,[d233_24, d233_23, d233_25, d233_26, d233_27]).
ether(train,d234,[d234_2, d234_1, d234_6]).
ether(train,d234,[d234_7, d234_13, d234_6]).
ether(train,d234,[d234_8, d234_6, d234_9]).
methoxy(train,d234,[d234_1, d234_2, d234_3, d234_4, d234_5]).
methoxy(train,d234,[d234_9, d234_8, d234_10, d234_11, d234_12]).
methoxy(train,d234,[d234_13, d234_7, d234_14, d234_15, d234_16]).
methyl(train,d234,[d234_1, d234_2, d234_3, d234_4, d234_5]).
methyl(train,d234,[d234_9, d234_8, d234_10, d234_11, d234_12]).
methyl(train,d234,[d234_13, d234_7, d234_14, d234_15, d234_16]).
ar_halide(train,d235,[d235_8, d235_5]).
ar_halide(train,d235,[d235_11, d235_9]).
ar_halide(train,d235,[d235_22, d235_21]).
ar_halide(train,d235,[d235_25, d235_18]).
ar_halide(train,d235,[d235_31, d235_27]).
ar_halide(train,d235,[d235_33, d235_30]).
alkyl_halide(train,d235,[d235_8, d235_5]).
alkyl_halide(train,d235,[d235_11, d235_9]).
alkyl_halide(train,d235,[d235_22, d235_21]).
alkyl_halide(train,d235,[d235_25, d235_18]).
alkyl_halide(train,d235,[d235_31, d235_27]).
alkyl_halide(train,d235,[d235_33, d235_30]).
ether(train,d235,[d235_3, d235_1, d235_4]).
ether(train,d235,[d235_14, d235_1, d235_15]).
ether(train,d235,[d235_16, d235_1, d235_17]).
ar_halide(train,d236,[d236_10, d236_9]).
alkyl_halide(train,d236,[d236_10, d236_9]).
methyl(train,d236,[d236_5, d236_1, d236_6, d236_7, d236_8]).
ar_halide(train,d237,[d237_17, d237_15]).
ar_halide(train,d237,[d237_18, d237_15]).
ether(train,d237,[d237_3, d237_1, d237_9]).
ether(train,d237,[d237_4, d237_1, d237_5]).
ether(train,d237,[d237_13, d237_1, d237_14]).
methoxy(train,d237,[d237_5, d237_4, d237_6, d237_7, d237_8]).
methoxy(train,d237,[d237_9, d237_3, d237_10, d237_11, d237_12]).
methyl(train,d237,[d237_5, d237_4, d237_6, d237_7, d237_8]).
methyl(train,d237,[d237_9, d237_3, d237_10, d237_11, d237_12]).
non_ar_hetero_5_ring(train,d238,[d238_12, d238_14, d238_13, d238_16, d238_13]).
non_ar_hetero_5_ring(train,d238,[d238_12, d238_14, d238_13, d238_17, d238_13]).
non_ar_hetero_5_ring(train,d238,[d238_13, d238_14, d238_12, d238_15, d238_12]).
non_ar_hetero_5_ring(train,d238,[d238_13, d238_14, d238_12, d238_11, d238_12]).
non_ar_hetero_5_ring(train,d238,[d238_23, d238_27, d238_26, d238_29, d238_26]).
non_ar_hetero_5_ring(train,d238,[d238_23, d238_27, d238_26, d238_30, d238_26]).
non_ar_hetero_5_ring(train,d238,[d238_26, d238_27, d238_23, d238_28, d238_23]).
non_ar_hetero_5_ring(train,d238,[d238_26, d238_27, d238_23, d238_22, d238_23]).
six_ring(train,d238,[d238_1, d238_2, d238_3, d238_4, d238_5, d238_6]).
six_ring(train,d238,[d238_12, d238_13, d238_14, d238_12, d238_13, d238_14]).
six_ring(train,d238,[d238_23, d238_26, d238_27, d238_23, d238_26, d238_27]).
non_ar_hetero_6_ring(train,d238,[d238_12, d238_13, d238_14, d238_12, d238_13, d238_14]).
non_ar_hetero_6_ring(train,d238,[d238_23, d238_26, d238_27, d238_23, d238_26, d238_27]).
non_ar_6c_ring(train,d238,[d238_1, d238_2, d238_3, d238_4, d238_5, d238_6]).
ether(train,d238,[d238_14, d238_12, d238_13]).
ether(train,d238,[d238_18, d238_11, d238_5]).
ether(train,d238,[d238_21, d238_22, d238_3]).
ether(train,d238,[d238_27, d238_23, d238_26]).
ar_halide(train,d239,[d239_3, d239_2]).
alkyl_halide(train,d239,[d239_3, d239_2]).
methyl(train,d239,[d239_1, d239_2, d239_6, d239_7, d239_8]).
six_ring(train,d23_1,[d23_1_1, d23_1_2, d23_1_3, d23_1_4, d23_1_5, d23_1_6]).
six_ring(train,d23_1,[d23_1_11, d23_1_12, d23_1_13, d23_1_14, d23_1_15, d23_1_16]).
non_ar_6c_ring(train,d23_1,[d23_1_1, d23_1_2, d23_1_3, d23_1_4, d23_1_5, d23_1_6]).
non_ar_6c_ring(train,d23_1,[d23_1_11, d23_1_12, d23_1_13, d23_1_14, d23_1_15, d23_1_16]).
ether(train,d23_1,[d23_1_21, d23_1_11, d23_1_4]).
amine(train,d23_1,[d23_1_22, d23_1_26, d23_1_27, d23_1_14]).
amine(train,d23_1,[d23_1_23, d23_1_24, d23_1_25, d23_1_1]).
six_ring(train,d23_2,[d23_2_1, d23_2_2, d23_2_3, d23_2_4, d23_2_5, d23_2_6]).
six_ring(train,d23_2,[d23_2_12, d23_2_13, d23_2_14, d23_2_15, d23_2_16, d23_2_17]).
non_ar_hetero_6_ring(train,d23_2,[d23_2_12, d23_2_13, d23_2_14, d23_2_15, d23_2_16, d23_2_17]).
non_ar_6c_ring(train,d23_2,[d23_2_1, d23_2_2, d23_2_3, d23_2_4, d23_2_5, d23_2_6]).
amine(train,d23_2,[d23_2_22, d23_2_24, d23_2_25, d23_2_17]).
amine(train,d23_2,[d23_2_23, d23_2_26, d23_2_27, d23_2_15]).
six_ring(train,d24,[d24_1, d24_2, d24_3, d24_4, d24_5, d24_6]).
six_ring(train,d24,[d24_11, d24_12, d24_13, d24_14, d24_15, d24_16]).
non_ar_6c_ring(train,d24,[d24_1, d24_2, d24_3, d24_4, d24_5, d24_6]).
non_ar_6c_ring(train,d24,[d24_11, d24_12, d24_13, d24_14, d24_15, d24_16]).
sulfide(train,d24,[d24_21, d24_11, d24_4]).
amine(train,d24,[d24_22, d24_26, d24_27, d24_14]).
amine(train,d24,[d24_23, d24_24, d24_25, d24_1]).
five_ring(train,d240,[d240_3, d240_1, d240_2, d240_6, d240_2]).
non_ar_hetero_5_ring(train,d240,[d240_1, d240_3, d240_2, d240_6, d240_2]).
non_ar_hetero_5_ring(train,d240,[d240_1, d240_3, d240_2, d240_7, d240_2]).
non_ar_hetero_5_ring(train,d240,[d240_2, d240_3, d240_1, d240_4, d240_1]).
non_ar_hetero_5_ring(train,d240,[d240_2, d240_3, d240_1, d240_5, d240_1]).
six_ring(train,d240,[d240_1, d240_2, d240_3, d240_1, d240_2, d240_3]).
non_ar_hetero_6_ring(train,d240,[d240_1, d240_2, d240_3, d240_1, d240_2, d240_3]).
alcohol(train,d240,[d240_8, d240_11, d240_6]).
ether(train,d240,[d240_3, d240_1, d240_2]).
ar_halide(train,d241,[d241_9, d241_7]).
alkyl_halide(train,d241,[d241_9, d241_7]).
alcohol(train,d241,[d241_3, d241_12, d241_1]).
alcohol(train,d241,[d241_6, d241_13, d241_2]).
ether(train,d241,[d241_3, d241_1, d241_12]).
five_ring(train,d242,[d242_1, d242_13, d242_6, d242_5, d242_6]).
non_ar_hetero_5_ring(train,d242,[d242_1, d242_13, d242_6, d242_14, d242_6]).
non_ar_hetero_5_ring(train,d242,[d242_1, d242_13, d242_6, d242_5, d242_6]).
non_ar_hetero_5_ring(train,d242,[d242_6, d242_13, d242_1, d242_2, d242_1]).
non_ar_hetero_5_ring(train,d242,[d242_6, d242_13, d242_1, d242_15, d242_1]).
non_ar_hetero_5_ring(train,d242,[d242_16, d242_19, d242_18, d242_21, d242_18]).
non_ar_hetero_5_ring(train,d242,[d242_16, d242_19, d242_18, d242_22, d242_18]).
non_ar_hetero_5_ring(train,d242,[d242_18, d242_19, d242_16, d242_20, d242_16]).
non_ar_hetero_5_ring(train,d242,[d242_18, d242_19, d242_16, d242_4, d242_16]).
six_ring(train,d242,[d242_1, d242_2, d242_3, d242_4, d242_5, d242_6]).
six_ring(train,d242,[d242_1, d242_13, d242_6, d242_1, d242_13, d242_6]).
six_ring(train,d242,[d242_16, d242_18, d242_19, d242_16, d242_18, d242_19]).
non_ar_hetero_6_ring(train,d242,[d242_1, d242_13, d242_6, d242_1, d242_13, d242_6]).
non_ar_hetero_6_ring(train,d242,[d242_16, d242_18, d242_19, d242_16, d242_18, d242_19]).
non_ar_6c_ring(train,d242,[d242_1, d242_2, d242_3, d242_4, d242_5, d242_6]).
ether(train,d242,[d242_13, d242_1, d242_6]).
ether(train,d242,[d242_19, d242_16, d242_18]).
ar_halide(train,d243,[d243_2, d243_1]).
ar_halide(train,d243,[d243_19, d243_17]).
alkyl_halide(train,d243,[d243_2, d243_1]).
alkyl_halide(train,d243,[d243_19, d243_17]).
ether(train,d243,[d243_10, d243_12, d243_3]).
methyl(train,d243,[d243_6, d243_3, d243_7, d243_8, d243_9]).
methyl(train,d243,[d243_13, d243_12, d243_14, d243_15, d243_16]).
ether(train,d244,[d244_3, d244_1, d244_6]).
ether(train,d244,[d244_5, d244_1, d244_10]).
methoxy(train,d244,[d244_6, d244_3, d244_7, d244_8, d244_9]).
methoxy(train,d244,[d244_10, d244_5, d244_11, d244_12, d244_13]).
methyl(train,d244,[d244_6, d244_3, d244_7, d244_8, d244_9]).
methyl(train,d244,[d244_10, d244_5, d244_11, d244_12, d244_13]).
five_ring(train,d245,[d245_1, d245_10, d245_2, d245_3, d245_2]).
non_ar_hetero_5_ring(train,d245,[d245_1, d245_10, d245_2, d245_3, d245_2]).
non_ar_hetero_5_ring(train,d245,[d245_1, d245_10, d245_2, d245_13, d245_2]).
non_ar_hetero_5_ring(train,d245,[d245_2, d245_10, d245_1, d245_11, d245_1]).
non_ar_hetero_5_ring(train,d245,[d245_2, d245_10, d245_1, d245_12, d245_1]).
six_ring(train,d245,[d245_1, d245_2, d245_10, d245_1, d245_2, d245_10]).
non_ar_hetero_6_ring(train,d245,[d245_1, d245_2, d245_10, d245_1, d245_2, d245_10]).
ether(train,d245,[d245_10, d245_1, d245_2]).
methyl(train,d245,[d245_4, d245_3, d245_7, d245_8, d245_9]).
five_ring(train,d246,[d246_46, d246_47, d246_58, d246_57, d246_56]).
non_ar_5c_ring(train,d246,[d246_46, d246_47, d246_58, d246_57, d246_56]).
six_ring(train,d246,[d246_1, d246_2, d246_3, d246_4, d246_5, d246_6]).
six_ring(train,d246,[d246_28, d246_30, d246_31, d246_32, d246_33, d246_34]).
six_ring(train,d246,[d246_31, d246_32, d246_41, d246_40, d246_39, d246_38]).
six_ring(train,d246,[d246_40, d246_41, d246_49, d246_48, d246_47, d246_46]).
six_ring(train,d246,[d246_71, d246_73, d246_74, d246_75, d246_76, d246_77]).
non_ar_6c_ring(train,d246,[d246_1, d246_2, d246_3, d246_4, d246_5, d246_6]).
non_ar_6c_ring(train,d246,[d246_28, d246_30, d246_31, d246_32, d246_33, d246_34]).
non_ar_6c_ring(train,d246,[d246_31, d246_32, d246_41, d246_40, d246_39, d246_38]).
non_ar_6c_ring(train,d246,[d246_40, d246_41, d246_49, d246_48, d246_47, d246_46]).
non_ar_6c_ring(train,d246,[d246_71, d246_73, d246_74, d246_75, d246_76, d246_77]).
ar_halide(train,d246,[d246_16, d246_13]).
ar_halide(train,d246,[d246_23, d246_20]).
ar_halide(train,d246,[d246_87, d246_84]).
ar_halide(train,d246,[d246_94, d246_91]).
alkyl_halide(train,d246,[d246_16, d246_13]).
alkyl_halide(train,d246,[d246_23, d246_20]).
alkyl_halide(train,d246,[d246_87, d246_84]).
alkyl_halide(train,d246,[d246_94, d246_91]).
ether(train,d246,[d246_27, d246_26, d246_28]).
ether(train,d246,[d246_68, d246_58, d246_70]).
ketone(train,d246,[d246_29, d246_26, d246_2, d246_27]).
ketone(train,d246,[d246_72, d246_70, d246_68, d246_71]).
methyl(train,d246,[d246_64, d246_47, d246_65, d246_66, d246_67]).
six_ring(train,d247,[d247_1, d247_2, d247_3, d247_4, d247_5, d247_6]).
non_ar_hetero_6_ring(train,d247,[d247_1, d247_2, d247_3, d247_4, d247_5, d247_6]).
ether(train,d247,[d247_3, d247_2, d247_4]).
ether(train,d247,[d247_16, d247_15, d247_22]).
ether(train,d247,[d247_17, d247_15, d247_18]).
methoxy(train,d247,[d247_18, d247_17, d247_19, d247_20, d247_21]).
methoxy(train,d247,[d247_22, d247_16, d247_23, d247_24, d247_25]).
methyl(train,d247,[d247_18, d247_17, d247_19, d247_20, d247_21]).
methyl(train,d247,[d247_22, d247_16, d247_23, d247_24, d247_25]).
six_ring(train,d248,[d248_1, d248_2, d248_4, d248_7, d248_4, d248_3]).
six_ring(train,d248,[d248_1, d248_2, d248_4, d248_11, d248_4, d248_3]).
six_ring(train,d248,[d248_2, d248_4, d248_3, d248_15, d248_3, d248_1]).
non_ar_hetero_6_ring(train,d248,[d248_1, d248_2, d248_4, d248_7, d248_4, d248_3]).
non_ar_hetero_6_ring(train,d248,[d248_1, d248_2, d248_4, d248_11, d248_4, d248_3]).
non_ar_hetero_6_ring(train,d248,[d248_2, d248_4, d248_3, d248_15, d248_3, d248_1]).
non_ar_hetero_6_ring(train,d248,[d248_3, d248_4, d248_2, d248_5, d248_2, d248_1]).
non_ar_hetero_6_ring(train,d248,[d248_3, d248_4, d248_2, d248_6, d248_2, d248_1]).
ester(train,d248,[d248_3, d248_1, d248_15, d248_4, d248_7]).
ester(train,d248,[d248_3, d248_1, d248_15, d248_4, d248_11]).
ester(train,d248,[d248_3, d248_1, d248_15, d248_4, d248_2]).
ether(train,d248,[d248_1, d248_2, d248_3]).
ketone(train,d248,[d248_15, d248_3, d248_1, d248_4]).
methyl(train,d248,[d248_7, d248_4, d248_10, d248_8, d248_9]).
methyl(train,d248,[d248_11, d248_4, d248_12, d248_13, d248_14]).
five_ring(train,d249,[d249_1, d249_2, d249_11, d249_5, d249_6]).
five_ring(train,d249,[d249_2, d249_11, d249_5, d249_4, d249_3]).
non_ar_5c_ring(train,d249,[d249_1, d249_2, d249_11, d249_5, d249_6]).
non_ar_5c_ring(train,d249,[d249_2, d249_11, d249_5, d249_4, d249_3]).
six_ring(train,d249,[d249_1, d249_2, d249_3, d249_4, d249_5, d249_6]).
non_ar_6c_ring(train,d249,[d249_1, d249_2, d249_3, d249_4, d249_5, d249_6]).
ar_halide(train,d249,[d249_28, d249_6]).
alkyl_halide(train,d249,[d249_28, d249_6]).
methyl(train,d249,[d249_16, d249_1, d249_17, d249_18, d249_19]).
methyl(train,d249,[d249_20, d249_1, d249_21, d249_22, d249_23]).
methyl(train,d249,[d249_24, d249_6, d249_25, d249_26, d249_27]).
six_ring(train,d25,[d25_1, d25_2, d25_3, d25_4, d25_5, d25_6]).
non_ar_6c_ring(train,d25,[d25_1, d25_2, d25_3, d25_4, d25_5, d25_6]).
amine(train,d25,[d25_11, d25_16, d25_17, d25_5]).
methyl(train,d25,[d25_12, d25_4, d25_13, d25_14, d25_15]).
ar_halide(train,d250,[d250_1, d250_2]).
ar_halide(train,d250,[d250_6, d250_3]).
alkyl_halide(train,d250,[d250_1, d250_2]).
alkyl_halide(train,d250,[d250_6, d250_3]).
methyl(train,d250,[d250_7, d250_3, d250_10, d250_11, d250_9]).
ar_halide(train,d251,[d251_8, d251_5]).
ar_halide(train,d251,[d251_16, d251_13]).
ar_halide(train,d251,[d251_24, d251_21]).
alkyl_halide(train,d251,[d251_8, d251_5]).
alkyl_halide(train,d251,[d251_16, d251_13]).
alkyl_halide(train,d251,[d251_24, d251_21]).
ether(train,d251,[d251_3, d251_2, d251_4]).
ether(train,d251,[d251_11, d251_12, d251_2]).
ether(train,d251,[d251_19, d251_2, d251_20]).
imine(train,d252,[d252_10, d252_7]).
ether(train,d253,[d253_7, d253_1, d253_8]).
ether(train,d253,[d253_12, d253_1, d253_13]).
methoxy(train,d253,[d253_8, d253_7, d253_10, d253_11, d253_9]).
methoxy(train,d253,[d253_13, d253_12, d253_14, d253_15, d253_16]).
methyl(train,d253,[d253_3, d253_1, d253_4, d253_5, d253_6]).
methyl(train,d253,[d253_8, d253_7, d253_10, d253_11, d253_9]).
methyl(train,d253,[d253_13, d253_12, d253_14, d253_15, d253_16]).
five_ring(train,d254,[d254_3, d254_1, d254_2, d254_6, d254_2]).
non_ar_hetero_5_ring(train,d254,[d254_1, d254_3, d254_2, d254_6, d254_2]).
non_ar_hetero_5_ring(train,d254,[d254_1, d254_3, d254_2, d254_7, d254_2]).
non_ar_hetero_5_ring(train,d254,[d254_2, d254_3, d254_1, d254_4, d254_1]).
non_ar_hetero_5_ring(train,d254,[d254_2, d254_3, d254_1, d254_5, d254_1]).
six_ring(train,d254,[d254_1, d254_2, d254_3, d254_1, d254_2, d254_3]).
non_ar_hetero_6_ring(train,d254,[d254_1, d254_2, d254_3, d254_1, d254_2, d254_3]).
ether(train,d254,[d254_3, d254_1, d254_2]).
ether(train,d254,[d254_8, d254_11, d254_6]).
ar_halide(train,d255,[d255_9, d255_6]).
alkyl_halide(train,d255,[d255_9, d255_6]).
methyl(train,d255,[d255_1, d255_2, d255_3, d255_4, d255_5]).
six_ring(train,d256,[d256_1, d256_2, d256_3, d256_4, d256_5, d256_6]).
six_ring(train,d256,[d256_1, d256_6, d256_9, d256_10, d256_11, d256_12]).
non_ar_hetero_6_ring(train,d256,[d256_1, d256_6, d256_9, d256_10, d256_11, d256_12]).
non_ar_6c_ring(train,d256,[d256_1, d256_2, d256_3, d256_4, d256_5, d256_6]).
sulfide(train,d256,[d256_14, d256_13, d256_17]).
ether(train,d256,[d256_18, d256_17, d256_25]).
ether(train,d256,[d256_19, d256_17, d256_20]).
ketone(train,d256,[d256_29, d256_12, d256_1, d256_11]).
methoxy(train,d256,[d256_20, d256_19, d256_21, d256_22, d256_23]).
methoxy(train,d256,[d256_25, d256_18, d256_26, d256_27, d256_28]).
methyl(train,d256,[d256_20, d256_19, d256_21, d256_22, d256_23]).
methyl(train,d256,[d256_25, d256_18, d256_26, d256_27, d256_28]).
six_ring(train,d257,[d257_20, d257_21, d257_22, d257_23, d257_24, d257_25]).
non_ar_6c_ring(train,d257,[d257_20, d257_21, d257_22, d257_23, d257_24, d257_25]).
sulfide(train,d257,[d257_29, d257_24, d257_30]).
ether(train,d257,[d257_6, d257_2, d257_9]).
ether(train,d257,[d257_10, d257_11, d257_9]).
ether(train,d257,[d257_19, d257_20, d257_9]).
methyl(train,d257,[d257_1, d257_2, d257_3, d257_4, d257_5]).
methyl(train,d257,[d257_12, d257_11, d257_15, d257_16, d257_17]).
methyl(train,d257,[d257_30, d257_29, d257_31, d257_32, d257_33]).
methyl(train,d257,[d257_34, d257_23, d257_35, d257_36, d257_37]).
ar_halide(train,d258,[d258_20, d258_19]).
alkyl_halide(train,d258,[d258_20, d258_19]).
ester(train,d258,[d258_14, d258_13, d258_19, d258_15, d258_16]).
ester(train,d258,[d258_14, d258_13, d258_19, d258_15, d258_17]).
ester(train,d258,[d258_14, d258_13, d258_19, d258_15, d258_18]).
ether(train,d258,[d258_3, d258_1, d258_4]).
ether(train,d258,[d258_8, d258_1, d258_9]).
ether(train,d258,[d258_13, d258_1, d258_14]).
ketone(train,d258,[d258_22, d258_21, d258_19, d258_23]).
methoxy(train,d258,[d258_4, d258_3, d258_5, d258_6, d258_7]).
methoxy(train,d258,[d258_9, d258_8, d258_10, d258_11, d258_12]).
methyl(train,d258,[d258_4, d258_3, d258_5, d258_6, d258_7]).
methyl(train,d258,[d258_9, d258_8, d258_10, d258_11, d258_12]).
methyl(train,d258,[d258_15, d258_14, d258_16, d258_17, d258_18]).
methyl(train,d258,[d258_25, d258_24, d258_28, d258_29, d258_30]).
methyl(train,d258,[d258_32, d258_31, d258_35, d258_36, d258_37]).
six_ring(train,d259,[d259_1, d259_2, d259_3, d259_4, d259_5, d259_6]).
non_ar_6c_ring(train,d259,[d259_1, d259_2, d259_3, d259_4, d259_5, d259_6]).
ar_halide(train,d259,[d259_14, d259_13]).
alkyl_halide(train,d259,[d259_14, d259_13]).
ketone(train,d259,[d259_17, d259_12, d259_13, d259_5]).
six_ring(train,d26,[d26_1, d26_2, d26_3, d26_4, d26_5, d26_6]).
non_ar_6c_ring(train,d26,[d26_1, d26_2, d26_3, d26_4, d26_5, d26_6]).
amine(train,d26,[d26_9, d26_22, d26_23, d26_5]).
methyl(train,d26,[d26_10, d26_4, d26_11, d26_12, d26_13]).
methyl(train,d26,[d26_14, d26_2, d26_15, d26_16, d26_17]).
methyl(train,d26,[d26_18, d26_1, d26_19, d26_20, d26_21]).
ether(train,d260,[d260_2, d260_18, d260_3]).
ether(train,d260,[d260_6, d260_21, d260_7]).
ether(train,d260,[d260_10, d260_11, d260_24]).
ether(train,d260,[d260_12, d260_13, d260_27]).
nitro(train,d260,[d260_3, d260_2, d260_4, d260_5]).
nitro(train,d260,[d260_7, d260_6, d260_8, d260_9]).
nitro(train,d260,[d260_11, d260_10, d260_16, d260_17]).
nitro(train,d260,[d260_13, d260_12, d260_14, d260_15]).
ar_halide(train,d261,[d261_9, d261_6]).
alkyl_halide(train,d261,[d261_9, d261_6]).
methyl(train,d261,[d261_1, d261_2, d261_12, d261_13, d261_14]).
ar_halide(train,d263,[d263_1, d263_2]).
alkyl_halide(train,d263,[d263_1, d263_2]).
alcohol(train,d263,[d263_6, d263_9, d263_3]).
ether(train,d263,[d263_6, d263_3, d263_9]).
six_ring(train,d264,[d264_20, d264_21, d264_22, d264_23, d264_24, d264_25]).
six_ring(train,d264,[d264_23, d264_24, d264_32, d264_31, d264_30, d264_29]).
non_ar_hetero_6_ring(train,d264,[d264_23, d264_24, d264_32, d264_31, d264_30, d264_29]).
non_ar_6c_ring(train,d264,[d264_20, d264_21, d264_22, d264_23, d264_24, d264_25]).
ar_halide(train,d264,[d264_38, d264_30]).
alkyl_halide(train,d264,[d264_38, d264_30]).
ester(train,d264,[d264_31, d264_32, d264_33, d264_30, d264_38]).
ether(train,d264,[d264_6, d264_2, d264_9]).
ether(train,d264,[d264_10, d264_11, d264_9]).
ether(train,d264,[d264_19, d264_20, d264_9]).
ether(train,d264,[d264_32, d264_24, d264_31]).
ketone(train,d264,[d264_33, d264_31, d264_30, d264_32]).
methyl(train,d264,[d264_1, d264_2, d264_3, d264_4, d264_5]).
methyl(train,d264,[d264_12, d264_11, d264_15, d264_16, d264_17]).
methyl(train,d264,[d264_34, d264_29, d264_35, d264_36, d264_37]).
sulfide(train,d265,[d265_6, d265_3, d265_7]).
ether(train,d265,[d265_2, d265_1, d265_3]).
ether(train,d265,[d265_4, d265_3, d265_5]).
ketone(train,d265,[d265_17, d265_8, d265_11, d265_7]).
amine(train,d265,[d265_11, d265_18, d265_8, d265_12]).
methoxy(train,d265,[d265_1, d265_2, d265_19, d265_20, d265_21]).
methoxy(train,d265,[d265_5, d265_4, d265_22, d265_23, d265_24]).
methyl(train,d265,[d265_1, d265_2, d265_19, d265_20, d265_21]).
methyl(train,d265,[d265_5, d265_4, d265_22, d265_23, d265_24]).
methyl(train,d265,[d265_12, d265_11, d265_13, d265_14, d265_15]).
ester(train,d266,[d266_8, d266_17, d266_10, d266_35, d266_36]).
ester(train,d266,[d266_8, d266_17, d266_10, d266_35, d266_37]).
ester(train,d266,[d266_8, d266_17, d266_10, d266_35, d266_7]).
ester(train,d266,[d266_25, d266_26, d266_34, d266_7, d266_35]).
ester(train,d266,[d266_25, d266_26, d266_34, d266_7, d266_38]).
ester(train,d266,[d266_25, d266_26, d266_34, d266_7, d266_6]).
sulfide(train,d266,[d266_6, d266_3, d266_7]).
ether(train,d266,[d266_2, d266_1, d266_3]).
ether(train,d266,[d266_4, d266_3, d266_5]).
ether(train,d266,[d266_17, d266_18, d266_8]).
ether(train,d266,[d266_26, d266_25, d266_27]).
ketone(train,d266,[d266_10, d266_8, d266_17, d266_35]).
ketone(train,d266,[d266_34, d266_25, d266_26, d266_7]).
methoxy(train,d266,[d266_1, d266_2, d266_11, d266_12, d266_13]).
methoxy(train,d266,[d266_5, d266_4, d266_14, d266_15, d266_16]).
methyl(train,d266,[d266_1, d266_2, d266_11, d266_12, d266_13]).
methyl(train,d266,[d266_5, d266_4, d266_14, d266_15, d266_16]).
methyl(train,d266,[d266_19, d266_18, d266_22, d266_23, d266_24]).
methyl(train,d266,[d266_28, d266_27, d266_31, d266_32, d266_33]).
alcohol(train,d267,[d267_1, d267_12, d267_2]).
alcohol(train,d267,[d267_7, d267_13, d267_6]).
alcohol(train,d267,[d267_14, d267_17, d267_10]).
alcohol(train,d267,[d267_18, d267_21, d267_11]).
ether(train,d267,[d267_14, d267_10, d267_17]).
ether(train,d267,[d267_18, d267_11, d267_21]).
six_ring(train,d268,[d268_1, d268_2, d268_3, d268_4, d268_5, d268_6]).
non_ar_hetero_6_ring(train,d268,[d268_1, d268_2, d268_3, d268_4, d268_5, d268_6]).
ar_halide(train,d268,[d268_12, d268_11]).
six_ring(train,d269,[d269_20, d269_21, d269_22, d269_23, d269_24, d269_25]).
non_ar_hetero_6_ring(train,d269,[d269_20, d269_21, d269_22, d269_23, d269_24, d269_25]).
ether(train,d269,[d269_6, d269_2, d269_9]).
ether(train,d269,[d269_10, d269_11, d269_9]).
ether(train,d269,[d269_19, d269_20, d269_9]).
methyl(train,d269,[d269_1, d269_2, d269_3, d269_4, d269_5]).
methyl(train,d269,[d269_12, d269_11, d269_15, d269_16, d269_17]).
methyl(train,d269,[d269_27, d269_22, d269_28, d269_29, d269_30]).
methyl(train,d269,[d269_32, d269_31, d269_33, d269_34, d269_35]).
methyl(train,d269,[d269_36, d269_31, d269_38, d269_39, d269_40]).
five_ring(train,d27,[d27_1, d27_2, d27_3, d27_4, d27_5]).
five_ring(train,d27,[d27_15, d27_16, d27_17, d27_18, d27_19]).
non_ar_hetero_5_ring(train,d27,[d27_1, d27_2, d27_3, d27_4, d27_5]).
non_ar_hetero_5_ring(train,d27,[d27_15, d27_16, d27_17, d27_18, d27_19]).
imine(train,d27,[d27_20, d27_8]).
ketone(train,d27,[d27_9, d27_3, d27_2, d27_4]).
ketone(train,d27,[d27_11, d27_5, d27_1, d27_4]).
amine(train,d27,[d27_4, d27_10, d27_5, d27_3]).
nitro(train,d27,[d27_12, d27_15, d27_13, d27_14]).
six_ring(train,d270,[d270_1, d270_2, d270_3, d270_4, d270_5, d270_6]).
non_ar_hetero_6_ring(train,d270,[d270_1, d270_2, d270_3, d270_4, d270_5, d270_6]).
sulfide(train,d270,[d270_11, d270_13, d270_4]).
sulfide(train,d270,[d270_14, d270_16, d270_3]).
ether(train,d270,[d270_2, d270_1, d270_3]).
ether(train,d270,[d270_5, d270_4, d270_6]).
ether(train,d270,[d270_19, d270_13, d270_21]).
ether(train,d270,[d270_20, d270_13, d270_28]).
ether(train,d270,[d270_35, d270_16, d270_36]).
ether(train,d270,[d270_43, d270_16, d270_44]).
methyl(train,d270,[d270_22, d270_21, d270_25, d270_26, d270_27]).
methyl(train,d270,[d270_29, d270_28, d270_32, d270_33, d270_34]).
methyl(train,d270,[d270_37, d270_36, d270_40, d270_41, d270_42]).
methyl(train,d270,[d270_45, d270_44, d270_48, d270_49, d270_50]).
ester(train,d271,[d271_14, d271_26, d271_17, d271_35, d271_37]).
ester(train,d271,[d271_14, d271_26, d271_17, d271_35, d271_38]).
ester(train,d271,[d271_14, d271_26, d271_17, d271_35, d271_34]).
ester(train,d271,[d271_15, d271_18, d271_16, d271_34, d271_35]).
ester(train,d271,[d271_15, d271_18, d271_16, d271_34, d271_36]).
ester(train,d271,[d271_15, d271_18, d271_16, d271_34, d271_7]).
sulfide(train,d271,[d271_7, d271_34, d271_6]).
ether(train,d271,[d271_2, d271_1, d271_6]).
ether(train,d271,[d271_9, d271_10, d271_6]).
ether(train,d271,[d271_18, d271_15, d271_19]).
ether(train,d271,[d271_26, d271_14, d271_27]).
ketone(train,d271,[d271_16, d271_15, d271_18, d271_34]).
ketone(train,d271,[d271_17, d271_14, d271_26, d271_35]).
methoxy(train,d271,[d271_1, d271_2, d271_3, d271_4, d271_5]).
methoxy(train,d271,[d271_10, d271_9, d271_11, d271_12, d271_13]).
methyl(train,d271,[d271_1, d271_2, d271_3, d271_4, d271_5]).
methyl(train,d271,[d271_10, d271_9, d271_11, d271_12, d271_13]).
methyl(train,d271,[d271_20, d271_19, d271_23, d271_24, d271_25]).
methyl(train,d271,[d271_28, d271_27, d271_31, d271_32, d271_33]).
ar_halide(train,d272,[d272_2, d272_1]).
ar_halide(train,d272,[d272_3, d272_1]).
ar_halide(train,d273,[d273_2, d273_1]).
ar_halide(train,d273,[d273_3, d273_1]).
ar_halide(train,d273,[d273_4, d273_1]).
ar_halide(train,d274,[d274_2, d274_1]).
ar_halide(train,d274,[d274_3, d274_1]).
ar_halide(train,d274,[d274_4, d274_1]).
ar_halide(train,d275,[d275_2, d275_1]).
ar_halide(train,d275,[d275_3, d275_1]).
ar_halide(train,d275,[d275_4, d275_1]).
ar_halide(train,d276,[d276_2, d276_1]).
ar_halide(train,d276,[d276_3, d276_1]).
ar_halide(train,d276,[d276_4, d276_1]).
six_ring(train,d277,[d277_4, d277_5, d277_6, d277_7, d277_8, d277_9]).
six_ring(train,d277,[d277_15, d277_18, d277_19, d277_20, d277_21, d277_22]).
non_ar_6c_ring(train,d277,[d277_4, d277_5, d277_6, d277_7, d277_8, d277_9]).
non_ar_6c_ring(train,d277,[d277_15, d277_18, d277_19, d277_20, d277_21, d277_22]).
methyl(train,d277,[d277_1, d277_2, d277_39, d277_40, d277_41]).
methyl(train,d277,[d277_3, d277_2, d277_36, d277_37, d277_38]).
methyl(train,d277,[d277_28, d277_27, d277_30, d277_31, d277_32]).
methyl(train,d277,[d277_29, d277_27, d277_33, d277_34, d277_35]).
six_ring(train,d278,[d278_4, d278_5, d278_6, d278_7, d278_8, d278_9]).
six_ring(train,d278,[d278_15, d278_16, d278_17, d278_18, d278_19, d278_20]).
non_ar_6c_ring(train,d278,[d278_4, d278_5, d278_6, d278_7, d278_8, d278_9]).
non_ar_6c_ring(train,d278,[d278_15, d278_16, d278_17, d278_18, d278_19, d278_20]).
ketone(train,d278,[d278_28, d278_14, d278_15, d278_7]).
methyl(train,d278,[d278_1, d278_2, d278_38, d278_39, d278_40]).
methyl(train,d278,[d278_3, d278_2, d278_35, d278_36, d278_37]).
methyl(train,d278,[d278_26, d278_25, d278_29, d278_30, d278_31]).
methyl(train,d278,[d278_27, d278_25, d278_32, d278_33, d278_34]).
six_ring(train,d279,[d279_4, d279_5, d279_6, d279_7, d279_8, d279_9]).
non_ar_6c_ring(train,d279,[d279_4, d279_5, d279_6, d279_7, d279_8, d279_9]).
methyl(train,d279,[d279_1, d279_2, d279_18, d279_19, d279_20]).
methyl(train,d279,[d279_3, d279_2, d279_15, d279_16, d279_17]).
five_ring(train,d28,[d28_1, d28_2, d28_3, d28_4, d28_5]).
non_ar_hetero_5_ring(train,d28,[d28_1, d28_2, d28_3, d28_4, d28_5]).
imine(train,d28,[d28_7, d28_8]).
ketone(train,d28,[d28_18, d28_10, d28_11, d28_9]).
amine(train,d28,[d28_11, d28_16, d28_17, d28_10]).
amine(train,d28,[d28_9, d28_15, d28_8, d28_10]).
nitro(train,d28,[d28_6, d28_1, d28_12, d28_13]).
six_ring(train,d280,[d280_4, d280_5, d280_6, d280_7, d280_8, d280_9]).
non_ar_6c_ring(train,d280,[d280_4, d280_5, d280_6, d280_7, d280_8, d280_9]).
sulfide(train,d280,[d280_16, d280_15, d280_17]).
ether(train,d280,[d280_17, d280_16, d280_18]).
methyl(train,d280,[d280_1, d280_2, d280_24, d280_25, d280_26]).
methyl(train,d280,[d280_3, d280_2, d280_21, d280_22, d280_23]).
sulfo(train,d280,[d280_16, d280_17, d280_19, d280_20, d280_15]).
nitro(train,d281,[d281_1, d281_2, d281_6, d281_7]).
nitro(train,d281,[d281_3, d281_2, d281_8, d281_9]).
nitro(train,d281,[d281_4, d281_2, d281_12, d281_13]).
nitro(train,d281,[d281_5, d281_2, d281_10, d281_11]).
ester(train,d282,[d282_6, d282_7, d282_8, d282_3, d282_12]).
ester(train,d282,[d282_6, d282_7, d282_8, d282_3, d282_13]).
ester(train,d282,[d282_6, d282_7, d282_8, d282_3, d282_2]).
alcohol(train,d282,[d282_7, d282_11, d282_6]).
ketone(train,d282,[d282_8, d282_6, d282_3, d282_7]).
nitro(train,d282,[d282_1, d282_2, d282_10, d282_9]).
ester(train,d283,[d283_5, d283_8, d283_7, d283_2, d283_6]).
ether(train,d283,[d283_8, d283_5, d283_9]).
ketone(train,d283,[d283_7, d283_5, d283_2, d283_8]).
methyl(train,d283,[d283_10, d283_9, d283_13, d283_14, d283_15]).
alcohol(train,d284,[d284_10, d284_14, d284_9]).
ketone(train,d284,[d284_7, d284_5, d284_2, d284_8]).
amine(train,d284,[d284_8, d284_13, d284_9, d284_5]).
ester(train,d285,[d285_9, d285_10, d285_15, d285_2, d285_5]).
ether(train,d285,[d285_10, d285_11, d285_9]).
ketone(train,d285,[d285_15, d285_9, d285_10, d285_2]).
methoxy(train,d285,[d285_11, d285_10, d285_12, d285_13, d285_14]).
methyl(train,d285,[d285_5, d285_2, d285_6, d285_7, d285_8]).
methyl(train,d285,[d285_11, d285_10, d285_12, d285_13, d285_14]).
six_ring(train,d287,[d287_1, d287_2, d287_3, d287_4, d287_5, d287_6]).
non_ar_hetero_6_ring(train,d287,[d287_1, d287_2, d287_3, d287_4, d287_5, d287_6]).
ester(train,d287,[d287_21, d287_19, d287_26, d287_22, d287_23]).
ester(train,d287,[d287_21, d287_19, d287_26, d287_22, d287_24]).
ester(train,d287,[d287_21, d287_19, d287_26, d287_22, d287_25]).
ether(train,d287,[d287_3, d287_2, d287_4]).
ether(train,d287,[d287_5, d287_4, d287_6]).
ether(train,d287,[d287_19, d287_2, d287_21]).
ketone(train,d287,[d287_26, d287_21, d287_19, d287_22]).
methyl(train,d287,[d287_9, d287_6, d287_11, d287_12, d287_13]).
methyl(train,d287,[d287_14, d287_4, d287_16, d287_17, d287_18]).
methyl(train,d287,[d287_22, d287_21, d287_23, d287_24, d287_25]).
ar_halide(train,d288,[d288_17, d288_14]).
alkyl_halide(train,d288,[d288_17, d288_14]).
sulfide(train,d288,[d288_11, d288_10, d288_12]).
methyl(train,d288,[d288_1, d288_2, d288_24, d288_25, d288_26]).
methyl(train,d288,[d288_7, d288_6, d288_21, d288_22, d288_23]).
ar_halide(train,d289,[d289_11, d289_10]).
methyl(train,d289,[d289_1, d289_2, d289_3, d289_4, d289_5]).
methyl(train,d289,[d289_6, d289_2, d289_7, d289_8, d289_9]).
six_ring(train,d29,[d29a_1, d29a_2, d29a_3, d29a_4, d29a_5, d29a_6]).
six_ring(train,d29,[d29b_1, d29b_2, d29b_3, d29b_4, d29b_5, d29b_6]).
non_ar_6c_ring(train,d29,[d29a_1, d29a_2, d29a_3, d29a_4, d29a_5, d29a_6]).
non_ar_6c_ring(train,d29,[d29b_1, d29b_2, d29b_3, d29b_4, d29b_5, d29b_6]).
imine(train,d29,[d29a_16, d29a_14]).
imine(train,d29,[d29a_18, d29a_15]).
imine(train,d29,[d29b_16, d29b_14]).
imine(train,d29,[d29b_18, d29b_15]).
methyl(train,d29,[d29a_10, d29a_5, d29a_11, d29a_12, d29a_13]).
methyl(train,d29,[d29b_10, d29b_5, d29b_11, d29b_12, d29b_13]).
five_ring(train,d290,[d290_3, d290_11, d290_12, d290_13, d290_4]).
non_ar_hetero_5_ring(train,d290,[d290_3, d290_4, d290_13, d290_12, d290_11]).
six_ring(train,d290,[d290_1, d290_2, d290_3, d290_4, d290_5, d290_6]).
non_ar_6c_ring(train,d290,[d290_1, d290_2, d290_3, d290_4, d290_5, d290_6]).
ar_halide(train,d290,[d290_18, d290_17]).
ar_halide(train,d290,[d290_19, d290_17]).
ar_halide(train,d290,[d290_20, d290_17]).
alkyl_halide(train,d290,[d290_18, d290_17]).
alkyl_halide(train,d290,[d290_19, d290_17]).
alkyl_halide(train,d290,[d290_20, d290_17]).
sulfide(train,d290,[d290_16, d290_12, d290_17]).
ketone(train,d290,[d290_14, d290_13, d290_12, d290_4]).
ketone(train,d290,[d290_15, d290_11, d290_12, d290_3]).
ester(train,d291,[d291_2, d291_3, d291_8, d291_1, d291_9]).
ester(train,d291,[d291_2, d291_3, d291_8, d291_1, d291_10]).
ether(train,d291,[d291_3, d291_2, d291_4]).
ketone(train,d291,[d291_8, d291_2, d291_1, d291_3]).
amine(train,d291,[d291_1, d291_10, d291_9, d291_2]).
methoxy(train,d291,[d291_4, d291_3, d291_5, d291_6, d291_7]).
methyl(train,d291,[d291_4, d291_3, d291_5, d291_6, d291_7]).
ester(train,d292,[d292_2, d292_1, d292_4, d292_5, d292_6]).
ester(train,d292,[d292_2, d292_1, d292_4, d292_5, d292_7]).
ester(train,d292,[d292_2, d292_1, d292_4, d292_5, d292_8]).
alcohol(train,d292,[d292_1, d292_3, d292_2]).
ether(train,d292,[d292_1, d292_2, d292_3]).
ketone(train,d292,[d292_4, d292_2, d292_1, d292_5]).
ketone(train,d292,[d292_12, d292_9, d292_13, d292_6]).
amine(train,d292,[d292_13, d292_15, d292_9, d292_14]).
methyl(train,d292,[d292_16, d292_14, d292_17, d292_18, d292_19]).
methyl(train,d292,[d292_20, d292_14, d292_21, d292_22, d292_23]).
five_ring(train,d293,[d293_3, d293_11, d293_12, d293_13, d293_4]).
non_ar_hetero_5_ring(train,d293,[d293_3, d293_4, d293_13, d293_12, d293_11]).
six_ring(train,d293,[d293_1, d293_2, d293_3, d293_4, d293_5, d293_6]).
non_ar_6c_ring(train,d293,[d293_1, d293_2, d293_3, d293_4, d293_5, d293_6]).
six_ring(train,d294,[d294_1, d294_2, d294_3, d294_4, d294_5, d294_6]).
six_ring(train,d294,[d294_3, d294_4, d294_13, d294_12, d294_11, d294_10]).
non_ar_hetero_6_ring(train,d294,[d294_3, d294_4, d294_13, d294_12, d294_11, d294_10]).
non_ar_6c_ring(train,d294,[d294_1, d294_2, d294_3, d294_4, d294_5, d294_6]).
phenol(train,d294,[d294_17, d294_18, d294_2]).
methyl(train,d295,[d295_5, d295_2, d295_7, d295_8, d295_9]).
six_ring(train,d3,[d3_1, d3_2, d3_3, d3_4, d3_5, d3_6]).
six_ring(train,d3,[d3_3, d3_4, d3_14, d3_13, d3_12, d3_11]).
six_ring(train,d3,[d3_12, d3_13, d3_18, d3_17, d3_16, d3_15]).
non_ar_6c_ring(train,d3,[d3_1, d3_2, d3_3, d3_4, d3_5, d3_6]).
non_ar_6c_ring(train,d3,[d3_3, d3_4, d3_14, d3_13, d3_12, d3_11]).
non_ar_6c_ring(train,d3,[d3_12, d3_13, d3_18, d3_17, d3_16, d3_15]).
ketone(train,d3,[d3_21, d3_14, d3_13, d3_4]).
ketone(train,d3,[d3_22, d3_11, d3_12, d3_3]).
amine(train,d3,[d3_27, d3_28, d3_29, d3_18]).
methyl(train,d3,[d3_23, d3_17, d3_24, d3_25, d3_26]).
six_ring(train,d30,[d30_1, d30_2, d30_3, d30_4, d30_5, d30_6]).
non_ar_6c_ring(train,d30,[d30_1, d30_2, d30_3, d30_4, d30_5, d30_6]).
ar_halide(train,d30,[d30_12, d30_2]).
amine(train,d30,[d30_11, d30_13, d30_14, d30_5]).
six_ring(train,d31,[d31a_1, d31a_2, d31a_3, d31a_4, d31a_5, d31a_6]).
six_ring(train,d31,[d31a_10, d31a_11, d31a_12, d31a_13, d31a_14, d31a_15]).
non_ar_6c_ring(train,d31,[d31a_1, d31a_2, d31a_3, d31a_4, d31a_5, d31a_6]).
non_ar_6c_ring(train,d31,[d31a_10, d31a_11, d31a_12, d31a_13, d31a_14, d31a_15]).
ether(train,d31,[d31a_20, d31a_2, d31a_21]).
ether(train,d31,[d31a_25, d31a_12, d31a_26]).
amine(train,d31,[d31a_19, d31a_31, d31a_32, d31a_1]).
amine(train,d31,[d31a_30, d31a_33, d31a_34, d31a_13]).
methoxy(train,d31,[d31a_21, d31a_20, d31a_22, d31a_23, d31a_24]).
methoxy(train,d31,[d31a_26, d31a_25, d31a_27, d31a_28, d31a_29]).
methyl(train,d31,[d31a_21, d31a_20, d31a_22, d31a_23, d31a_24]).
methyl(train,d31,[d31a_26, d31a_25, d31a_27, d31a_28, d31a_29]).
six_ring(train,d32,[d32a_1, d32a_2, d32a_3, d32a_4, d32a_5, d32a_6]).
six_ring(train,d32,[d32a_10, d32a_11, d32a_12, d32a_13, d32a_14, d32a_15]).
non_ar_6c_ring(train,d32,[d32a_1, d32a_2, d32a_3, d32a_4, d32a_5, d32a_6]).
non_ar_6c_ring(train,d32,[d32a_10, d32a_11, d32a_12, d32a_13, d32a_14, d32a_15]).
amine(train,d32,[d32a_19, d32a_29, d32a_30, d32a_1]).
amine(train,d32,[d32a_20, d32a_31, d32a_32, d32a_13]).
methyl(train,d32,[d32a_21, d32a_2, d32a_22, d32a_23, d32a_24]).
methyl(train,d32,[d32a_25, d32a_12, d32a_26, d32a_27, d32a_28]).
six_ring(train,d33,[d33_1, d33_2, d33_3, d33_4, d33_5, d33_6]).
non_ar_6c_ring(train,d33,[d33_1, d33_2, d33_3, d33_4, d33_5, d33_6]).
amine(train,d33,[d33_12, d33_13, d33_14, d33_5]).
six_ring(train,d34,[d34_1, d34_2, d34_3, d34_4, d34_5, d34_6]).
six_ring(train,d34,[d34_12, d34_13, d34_14, d34_15, d34_16, d34_17]).
non_ar_6c_ring(train,d34,[d34_1, d34_2, d34_3, d34_4, d34_5, d34_6]).
non_ar_6c_ring(train,d34,[d34_12, d34_13, d34_14, d34_15, d34_16, d34_17]).
six_ring(train,d35,[d35_1, d35_2, d35_3, d35_4, d35_5, d35_6]).
non_ar_6c_ring(train,d35,[d35_1, d35_2, d35_3, d35_4, d35_5, d35_6]).
ar_halide(train,d35,[d35_15, d35_1]).
amine(train,d35,[d35_10, d35_16, d35_17, d35_5]).
methyl(train,d35,[d35_11, d35_4, d35_12, d35_13, d35_14]).
six_ring(train,d36,[d36a_1, d36a_2, d36a_3, d36a_4, d36a_5, d36a_6]).
six_ring(train,d36,[d36a_9, d36a_10, d36a_11, d36a_12, d36a_13, d36a_14]).
six_ring(train,d36,[d36a_10, d36a_11, d36a_20, d36a_19, d36a_18, d36a_17]).
non_ar_6c_ring(train,d36,[d36a_1, d36a_2, d36a_3, d36a_4, d36a_5, d36a_6]).
non_ar_6c_ring(train,d36,[d36a_9, d36a_10, d36a_11, d36a_12, d36a_13, d36a_14]).
non_ar_6c_ring(train,d36,[d36a_10, d36a_11, d36a_20, d36a_19, d36a_18, d36a_17]).
ar_halide(train,d36,[d36a_28, d36a_1]).
phenol(train,d36,[d36a_27, d36a_33, d36a_14]).
alcohol(train,d36,[d36a_35, d36a_38, d36a_34]).
sulfide(train,d36,[d36a_34, d36a_3, d36a_35]).
ether(train,d36,[d36a_27, d36a_14, d36a_33]).
ether(train,d36,[d36a_35, d36a_34, d36a_38]).
methyl(train,d36,[d36a_29, d36a_6, d36a_30, d36a_31, d36a_32]).
sulfo(train,d36,[d36a_34, d36a_35, d36a_36, d36a_37, d36a_3]).
six_ring(train,d37,[d37_1, d37_2, d37_3, d37_4, d37_5, d37_6]).
six_ring(train,d37,[d37_10, d37_11, d37_12, d37_13, d37_14, d37_15]).
non_ar_6c_ring(train,d37,[d37_1, d37_2, d37_3, d37_4, d37_5, d37_6]).
non_ar_6c_ring(train,d37,[d37_10, d37_11, d37_12, d37_13, d37_14, d37_15]).
imine(train,d37,[d37_31, d37_24]).
imine(train,d37,[d37_33, d37_30]).
ether(train,d37,[d37_19, d37_2, d37_20]).
ether(train,d37,[d37_25, d37_12, d37_26]).
methoxy(train,d37,[d37_20, d37_19, d37_21, d37_22, d37_23]).
methoxy(train,d37,[d37_26, d37_25, d37_27, d37_28, d37_29]).
methyl(train,d37,[d37_20, d37_19, d37_21, d37_22, d37_23]).
methyl(train,d37,[d37_26, d37_25, d37_27, d37_28, d37_29]).
six_ring(train,d38,[d38_1, d38_2, d38_3, d38_4, d38_5, d38_6]).
non_ar_6c_ring(train,d38,[d38_1, d38_2, d38_3, d38_4, d38_5, d38_6]).
methyl(train,d38,[d38_10, d38_5, d38_11, d38_12, d38_13]).
nitro(train,d38,[d38_14, d38_4, d38_16, d38_17]).
nitro(train,d38,[d38_15, d38_2, d38_18, d38_19]).
six_ring(train,d39,[d39_1, d39_2, d39_3, d39_4, d39_5, d39_6]).
non_ar_6c_ring(train,d39,[d39_1, d39_2, d39_3, d39_4, d39_5, d39_6]).
amine(train,d39,[d39_10, d39_18, d39_19, d39_5]).
methyl(train,d39,[d39_11, d39_4, d39_12, d39_13, d39_14]).
nitro(train,d39,[d39_15, d39_1, d39_16, d39_17]).
six_ring(train,d4,[d4_1, d4_2, d4_3, d4_4, d4_5, d4_6]).
non_ar_6c_ring(train,d4,[d4_1, d4_2, d4_3, d4_4, d4_5, d4_6]).
ether(train,d4,[d4_14, d4_15, d4_4]).
amine(train,d4,[d4_11, d4_12, d4_13, d4_5]).
methoxy(train,d4,[d4_15, d4_14, d4_16, d4_17, d4_18]).
methyl(train,d4,[d4_15, d4_14, d4_16, d4_17, d4_18]).
six_ring(train,d40,[d40_1, d40_2, d40_3, d40_4, d40_5, d40_6]).
six_ring(train,d40,[d40_11, d40_12, d40_13, d40_14, d40_15, d40_16]).
non_ar_6c_ring(train,d40,[d40_1, d40_2, d40_3, d40_4, d40_5, d40_6]).
non_ar_6c_ring(train,d40,[d40_11, d40_12, d40_13, d40_14, d40_15, d40_16]).
sulfide(train,d40,[d40_21, d40_11, d40_4]).
amine(train,d40,[d40_22, d40_28, d40_29, d40_14]).
amine(train,d40,[d40_23, d40_26, d40_27, d40_1]).
sulfo(train,d40,[d40_21, d40_24, d40_25, d40_11, d40_4]).
six_ring(train,d41,[d41_1, d41_2, d41_3, d41_4, d41_5, d41_6]).
non_ar_6c_ring(train,d41,[d41_1, d41_2, d41_3, d41_4, d41_5, d41_6]).
methyl(train,d41,[d41_23, d41_22, d41_26, d41_27, d41_28]).
methyl(train,d41,[d41_30, d41_29, d41_33, d41_34, d41_35]).
nitro(train,d41,[d41_16, d41_4, d41_18, d41_19]).
nitro(train,d41,[d41_17, d41_6, d41_20, d41_21]).
six_ring(train,d42,[d42_1, d42_2, d42_3, d42_4, d42_5, d42_6]).
non_ar_6c_ring(train,d42,[d42_1, d42_2, d42_3, d42_4, d42_5, d42_6]).
ar_halide(train,d42,[d42_15, d42_2]).
amine(train,d42,[d42_10, d42_16, d42_17, d42_5]).
methyl(train,d42,[d42_11, d42_4, d42_12, d42_13, d42_14]).
six_ring(train,d43,[d43_1, d43_2, d43_3, d43_4, d43_5, d43_6]).
six_ring(train,d43,[d43_3, d43_4, d43_12, d43_11, d43_10, d43_9]).
six_ring(train,d43,[d43_10, d43_11, d43_16, d43_15, d43_14, d43_13]).
non_ar_6c_ring(train,d43,[d43_1, d43_2, d43_3, d43_4, d43_5, d43_6]).
non_ar_6c_ring(train,d43,[d43_3, d43_4, d43_12, d43_11, d43_10, d43_9]).
non_ar_6c_ring(train,d43,[d43_10, d43_11, d43_16, d43_15, d43_14, d43_13]).
ketone(train,d43,[d43_19, d43_12, d43_11, d43_4]).
ketone(train,d43,[d43_20, d43_9, d43_10, d43_3]).
amine(train,d43,[d43_21, d43_23, d43_24, d43_16]).
amine(train,d43,[d43_22, d43_25, d43_26, d43_13]).
amine(train,d43,[d43_27, d43_29, d43_30, d43_5]).
amine(train,d43,[d43_28, d43_31, d43_32, d43_2]).
six_ring(train,d44,[d44_1, d44_2, d44_3, d44_4, d44_5, d44_6]).
non_ar_6c_ring(train,d44,[d44_1, d44_2, d44_3, d44_4, d44_5, d44_6]).
ether(train,d44,[d44_15, d44_16, d44_2]).
amine(train,d44,[d44_10, d44_20, d44_21, d44_5]).
methoxy(train,d44,[d44_16, d44_15, d44_17, d44_18, d44_19]).
methyl(train,d44,[d44_11, d44_4, d44_12, d44_13, d44_14]).
methyl(train,d44,[d44_16, d44_15, d44_17, d44_18, d44_19]).
six_ring(train,d45,[d45_1, d45_2, d45_3, d45_4, d45_5, d45_6]).
non_ar_6c_ring(train,d45,[d45_1, d45_2, d45_3, d45_4, d45_5, d45_6]).
ar_halide(train,d45,[d45_11, d45_4]).
ar_halide(train,d45,[d45_12, d45_6]).
amine(train,d45,[d45_9, d45_13, d45_14, d45_5]).
amine(train,d45,[d45_10, d45_15, d45_16, d45_2]).
five_ring(train,d46,[d46_3, d46_14, d46_15, d46_13, d46_4]).
non_ar_hetero_5_ring(train,d46,[d46_3, d46_4, d46_13, d46_15, d46_14]).
six_ring(train,d46,[d46_1, d46_2, d46_3, d46_4, d46_5, d46_6]).
non_ar_6c_ring(train,d46,[d46_1, d46_2, d46_3, d46_4, d46_5, d46_6]).
nitro(train,d46,[d46_10, d46_1, d46_11, d46_12]).
six_ring(train,d47,[d47_1, d47_2, d47_3, d47_4, d47_5, d47_6]).
six_ring(train,d47,[d47_12, d47_13, d47_14, d47_15, d47_16, d47_17]).
six_ring(train,d47,[d47_16, d47_17, d47_23, d47_22, d47_21, d47_20]).
non_ar_6c_ring(train,d47,[d47_1, d47_2, d47_3, d47_4, d47_5, d47_6]).
non_ar_6c_ring(train,d47,[d47_12, d47_13, d47_14, d47_15, d47_16, d47_17]).
non_ar_6c_ring(train,d47,[d47_16, d47_17, d47_23, d47_22, d47_21, d47_20]).
phenol(train,d47,[d47_30, d47_31, d47_13]).
ether(train,d47,[d47_30, d47_13, d47_31]).
six_ring(train,d48,[d48_1, d48_2, d48_3, d48_4, d48_5, d48_6]).
non_ar_6c_ring(train,d48,[d48_1, d48_2, d48_3, d48_4, d48_5, d48_6]).
ether(train,d48,[d48_19, d48_2, d48_20]).
ketone(train,d48,[d48_16, d48_11, d48_10, d48_12]).
amine(train,d48,[d48_18, d48_25, d48_26, d48_3]).
amine(train,d48,[d48_10, d48_17, d48_5, d48_11]).
methyl(train,d48,[d48_12, d48_11, d48_13, d48_14, d48_15]).
methyl(train,d48,[d48_21, d48_20, d48_22, d48_23, d48_24]).
six_ring(train,d49,[d49_1, d49_2, d49_3, d49_4, d49_5, d49_6]).
non_ar_6c_ring(train,d49,[d49_1, d49_2, d49_3, d49_4, d49_5, d49_6]).
phenol(train,d49,[d49_11, d49_17, d49_2]).
amine(train,d49,[d49_10, d49_13, d49_14, d49_5]).
nitro(train,d49,[d49_12, d49_3, d49_15, d49_16]).
six_ring(train,d5,[d5_1, d5_2, d5_3, d5_4, d5_5, d5_6]).
non_ar_6c_ring(train,d5,[d5_1, d5_2, d5_3, d5_4, d5_5, d5_6]).
ar_halide(train,d5,[d5_12, d5_2]).
amine(train,d5,[d5_10, d5_13, d5_14, d5_5]).
amine(train,d5,[d5_11, d5_15, d5_16, d5_3]).
five_ring(train,d50,[d50_1, d50_2, d50_3, d50_4, d50_5]).
non_ar_hetero_5_ring(train,d50,[d50_1, d50_2, d50_3, d50_4, d50_5]).
amine(train,d50,[d50_6, d50_10, d50_11, d50_4]).
nitro(train,d50,[d50_7, d50_1, d50_8, d50_9]).
six_ring(train,d51,[d51_1, d51_2, d51_3, d51_4, d51_5, d51_6]).
non_ar_6c_ring(train,d51,[d51_1, d51_2, d51_3, d51_4, d51_5, d51_6]).
imine(train,d51,[d51_2, d51_12]).
imine(train,d51,[d51_5, d51_11]).
alcohol(train,d51,[d51_13, d51_15, d51_11]).
alcohol(train,d51,[d51_14, d51_16, d51_12]).
ether(train,d51,[d51_13, d51_11, d51_15]).
ether(train,d51,[d51_14, d51_12, d51_16]).
six_ring(train,d52,[d52_1, d52_2, d52_3, d52_4, d52_5, d52_6]).
six_ring(train,d52,[d52_12, d52_13, d52_14, d52_15, d52_16, d52_17]).
non_ar_6c_ring(train,d52,[d52_1, d52_2, d52_3, d52_4, d52_5, d52_6]).
non_ar_6c_ring(train,d52,[d52_12, d52_13, d52_14, d52_15, d52_16, d52_17]).
amine(train,d52,[d52_22, d52_23, d52_24, d52_13]).
six_ring(train,d53,[d53_1, d53_2, d53_3, d53_4, d53_5, d53_6]).
non_ar_6c_ring(train,d53,[d53_1, d53_2, d53_3, d53_4, d53_5, d53_6]).
ar_halide(train,d53,[d53_11, d53_4]).
ar_halide(train,d53,[d53_13, d53_1]).
alcohol(train,d53,[d53_10, d53_15, d53_9]).
ketone(train,d53,[d53_14, d53_9, d53_10, d53_5]).
amine(train,d53,[d53_12, d53_16, d53_17, d53_3]).
six_ring(train,d54,[d54_1, d54_2, d54_3, d54_4, d54_5, d54_6]).
non_ar_6c_ring(train,d54,[d54_1, d54_2, d54_3, d54_4, d54_5, d54_6]).
ether(train,d54,[d54_10, d54_11, d54_2]).
ketone(train,d54,[d54_25, d54_20, d54_17, d54_21]).
amine(train,d54,[d54_17, d54_26, d54_5, d54_20]).
methyl(train,d54,[d54_12, d54_11, d54_13, d54_14, d54_15]).
methyl(train,d54,[d54_21, d54_20, d54_22, d54_23, d54_24]).
nitro(train,d54,[d54_16, d54_3, d54_18, d54_19]).
six_ring(train,d55,[d55_1, d55_2, d55_3, d55_4, d55_5, d55_6]).
non_ar_6c_ring(train,d55,[d55_1, d55_2, d55_3, d55_4, d55_5, d55_6]).
amine(train,d55,[d55_10, d55_15, d55_16, d55_5]).
amine(train,d55,[d55_12, d55_17, d55_18, d55_2]).
nitro(train,d55,[d55_11, d55_4, d55_13, d55_14]).
six_ring(train,d56,[d56_1, d56_2, d56_3, d56_4, d56_5, d56_6]).
non_ar_6c_ring(train,d56,[d56_1, d56_2, d56_3, d56_4, d56_5, d56_6]).
phenol(train,d56,[d56_10, d56_17, d56_5]).
amine(train,d56,[d56_11, d56_15, d56_16, d56_4]).
nitro(train,d56,[d56_12, d56_2, d56_13, d56_14]).
six_ring(train,d57,[d57_1, d57_2, d57_3, d57_4, d57_5, d57_6]).
non_ar_6c_ring(train,d57,[d57_1, d57_2, d57_3, d57_4, d57_5, d57_6]).
phenol(train,d57,[d57_10, d57_17, d57_5]).
amine(train,d57,[d57_11, d57_15, d57_16, d57_4]).
nitro(train,d57,[d57_12, d57_1, d57_13, d57_14]).
six_ring(train,d58,[d58_1, d58_2, d58_3, d58_4, d58_5, d58_6]).
six_ring(train,d58,[d58_10, d58_11, d58_12, d58_13, d58_14, d58_15]).
six_ring(train,d58,[d58_19, d58_20, d58_21, d58_22, d58_23, d58_24]).
non_ar_6c_ring(train,d58,[d58_1, d58_2, d58_3, d58_4, d58_5, d58_6]).
non_ar_6c_ring(train,d58,[d58_10, d58_11, d58_12, d58_13, d58_14, d58_15]).
non_ar_6c_ring(train,d58,[d58_19, d58_20, d58_21, d58_22, d58_23, d58_24]).
sulfide(train,d58,[d58_37, d58_14, d58_43]).
ether(train,d58,[d58_43, d58_37, d58_45]).
amine(train,d58,[d58_35, d58_38, d58_4, d58_10]).
amine(train,d58,[d58_36, d58_39, d58_19, d58_13]).
methyl(train,d58,[d58_30, d58_1, d58_31, d58_32, d58_33]).
sulfo(train,d58,[d58_37, d58_42, d58_43, d58_44, d58_14]).
nitro(train,d58,[d58_34, d58_5, d58_40, d58_41]).
six_ring(train,d59,[d59_1, d59_2, d59_3, d59_4, d59_5, d59_6]).
non_ar_6c_ring(train,d59,[d59_1, d59_2, d59_3, d59_4, d59_5, d59_6]).
ether(train,d59,[d59_12, d59_13, d59_2]).
amine(train,d59,[d59_11, d59_17, d59_18, d59_5]).
methoxy(train,d59,[d59_13, d59_12, d59_14, d59_15, d59_16]).
methyl(train,d59,[d59_13, d59_12, d59_14, d59_15, d59_16]).
six_ring(train,d6,[d6_1, d6_2, d6_3, d6_4, d6_5, d6_6]).
non_ar_6c_ring(train,d6,[d6_1, d6_2, d6_3, d6_4, d6_5, d6_6]).
ar_halide(train,d6,[d6_10, d6_2]).
amine(train,d6,[d6_9, d6_11, d6_12, d6_5]).
amine(train,d6,[d6_14, d6_15, d6_16, d6_4]).
six_ring(train,d60,[d60_1, d60_2, d60_3, d60_4, d60_5, d60_6]).
non_ar_6c_ring(train,d60,[d60_1, d60_2, d60_3, d60_4, d60_5, d60_6]).
ar_halide(train,d60,[d60_12, d60_2]).
amine(train,d60,[d60_11, d60_13, d60_14, d60_5]).
six_ring(train,d61,[d61_1, d61_2, d61_3, d61_4, d61_5, d61_6]).
non_ar_6c_ring(train,d61,[d61_1, d61_2, d61_3, d61_4, d61_5, d61_6]).
ether(train,d61,[d61_12, d61_13, d61_4]).
ether(train,d61,[d61_14, d61_13, d61_26]).
ether(train,d61,[d61_18, d61_13, d61_19]).
methyl(train,d61,[d61_20, d61_19, d61_23, d61_24, d61_25]).
methyl(train,d61,[d61_27, d61_26, d61_30, d61_31, d61_32]).
nitro(train,d61,[d61_11, d61_1, d61_15, d61_16]).
six_ring(train,d62,[d62_1, d62_2, d62_3, d62_4, d62_5, d62_6]).
six_ring(train,d62,[d62_3, d62_4, d62_13, d62_12, d62_11, d62_10]).
six_ring(train,d62,[d62_11, d62_12, d62_18, d62_17, d62_16, d62_15]).
non_ar_hetero_6_ring(train,d62,[d62_3, d62_4, d62_13, d62_12, d62_11, d62_10]).
non_ar_6c_ring(train,d62,[d62_1, d62_2, d62_3, d62_4, d62_5, d62_6]).
non_ar_6c_ring(train,d62,[d62_11, d62_12, d62_18, d62_17, d62_16, d62_15]).
amine(train,d62,[d62_22, d62_24, d62_25, d62_1]).
amine(train,d62,[d62_23, d62_26, d62_27, d62_16]).
six_ring(train,d63,[d63_1, d63_2, d63_3, d63_4, d63_5, d63_6]).
non_ar_6c_ring(train,d63,[d63_1, d63_2, d63_3, d63_4, d63_5, d63_6]).
alcohol(train,d63,[d63_17, d63_25, d63_14]).
ether(train,d63,[d63_17, d63_14, d63_25]).
amine(train,d63,[d63_10, d63_22, d63_23, d63_1]).
amine(train,d63,[d63_12, d63_24, d63_4, d63_13]).
nitro(train,d63,[d63_11, d63_3, d63_20, d63_21]).
six_ring(train,d64,[d64_1, d64_2, d64_3, d64_4, d64_5, d64_6]).
six_ring(train,d64,[d64_3, d64_4, d64_14, d64_13, d64_12, d64_11]).
six_ring(train,d64,[d64_18, d64_19, d64_20, d64_21, d64_22, d64_23]).
non_ar_6c_ring(train,d64,[d64_1, d64_2, d64_3, d64_4, d64_5, d64_6]).
non_ar_6c_ring(train,d64,[d64_3, d64_4, d64_14, d64_13, d64_12, d64_11]).
non_ar_6c_ring(train,d64,[d64_18, d64_19, d64_20, d64_21, d64_22, d64_23]).
amine(train,d64,[d64_29, d64_30, d64_23, d64_13]).
six_ring(train,d65,[d65_1, d65_2, d65_3, d65_4, d65_5, d65_6]).
non_ar_6c_ring(train,d65,[d65_1, d65_2, d65_3, d65_4, d65_5, d65_6]).
phenol(train,d65,[d65_10, d65_17, d65_2]).
alcohol(train,d65,[d65_13, d65_19, d65_12]).
alcohol(train,d65,[d65_14, d65_18, d65_12]).
ether(train,d65,[d65_13, d65_12, d65_19]).
ether(train,d65,[d65_14, d65_12, d65_18]).
nitro(train,d65,[d65_11, d65_3, d65_15, d65_16]).
six_ring(train,d66,[d66_1, d66_2, d66_3, d66_4, d66_5, d66_6]).
six_ring(train,d66,[d66_7, d66_8, d66_20, d66_19, d66_18, d66_17]).
six_ring(train,d66,[d66_7, d66_8, d66_9, d66_10, d66_11, d66_12]).
six_ring(train,d66,[d66_10, d66_11, d66_16, d66_15, d66_14, d66_13]).
non_ar_hetero_6_ring(train,d66,[d66_7, d66_8, d66_9, d66_10, d66_11, d66_12]).
non_ar_6c_ring(train,d66,[d66_1, d66_2, d66_3, d66_4, d66_5, d66_6]).
non_ar_6c_ring(train,d66,[d66_7, d66_8, d66_20, d66_19, d66_18, d66_17]).
non_ar_6c_ring(train,d66,[d66_10, d66_11, d66_16, d66_15, d66_14, d66_13]).
imine(train,d66,[d66_14, d66_36]).
ether(train,d66,[d66_22, d66_21, d66_44]).
ketone(train,d66,[d66_23, d66_21, d66_22, d66_4]).
amine(train,d66,[d66_51, d66_63, d66_56, d66_19]).
methyl(train,d66,[d66_24, d66_15, d66_31, d66_32, d66_33]).
methyl(train,d66,[d66_38, d66_37, d66_41, d66_42, d66_43]).
methyl(train,d66,[d66_45, d66_44, d66_48, d66_49, d66_50]).
methyl(train,d66,[d66_52, d66_18, d66_53, d66_54, d66_55]).
methyl(train,d66,[d66_57, d66_56, d66_60, d66_61, d66_62]).
six_ring(train,d67,[d67_1, d67_2, d67_3, d67_4, d67_5, d67_6]).
non_ar_6c_ring(train,d67,[d67_1, d67_2, d67_3, d67_4, d67_5, d67_6]).
ar_halide(train,d67,[d67_20, d67_19]).
alkyl_halide(train,d67,[d67_20, d67_19]).
ketone(train,d67,[d67_17, d67_12, d67_11, d67_13]).
ketone(train,d67,[d67_23, d67_18, d67_19, d67_2]).
amine(train,d67,[d67_11, d67_24, d67_5, d67_12]).
methyl(train,d67,[d67_13, d67_12, d67_14, d67_15, d67_16]).
six_ring(train,d68,[d68_1, d68_2, d68_3, d68_4, d68_5, d68_6]).
non_ar_6c_ring(train,d68,[d68_1, d68_2, d68_3, d68_4, d68_5, d68_6]).
ar_halide(train,d68,[d68_12, d68_4]).
amine(train,d68,[d68_10, d68_13, d68_14, d68_5]).
amine(train,d68,[d68_11, d68_15, d68_16, d68_2]).
six_ring(train,d69,[d69_1, d69_2, d69_3, d69_4, d69_5, d69_6]).
non_ar_6c_ring(train,d69,[d69_1, d69_2, d69_3, d69_4, d69_5, d69_6]).
ar_halide(train,d69,[d69_15, d69_3]).
amine(train,d69,[d69_10, d69_16, d69_17, d69_5]).
methyl(train,d69,[d69_11, d69_2, d69_12, d69_13, d69_14]).
six_ring(train,d7,[d7_1, d7_2, d7_3, d7_4, d7_5, d7_6]).
six_ring(train,d7,[d7_10, d7_11, d7_12, d7_13, d7_14, d7_15]).
six_ring(train,d7,[d7_20, d7_21, d7_22, d7_23, d7_24, d7_25]).
non_ar_6c_ring(train,d7,[d7_1, d7_2, d7_3, d7_4, d7_5, d7_6]).
non_ar_6c_ring(train,d7,[d7_10, d7_11, d7_12, d7_13, d7_14, d7_15]).
non_ar_6c_ring(train,d7,[d7_20, d7_21, d7_22, d7_23, d7_24, d7_25]).
imine(train,d7,[d7_23, d7_31]).
amine(train,d7,[d7_33, d7_35, d7_36, d7_6]).
amine(train,d7,[d7_34, d7_37, d7_38, d7_11]).
six_ring(train,d70,[d70_1, d70_2, d70_3, d70_4, d70_5, d70_6]).
non_ar_6c_ring(train,d70,[d70_1, d70_2, d70_3, d70_4, d70_5, d70_6]).
ether(train,d70,[d70_11, d70_12, d70_2]).
ether(train,d70,[d70_16, d70_17, d70_4]).
amine(train,d70,[d70_10, d70_21, d70_22, d70_5]).
methoxy(train,d70,[d70_12, d70_11, d70_13, d70_14, d70_15]).
methoxy(train,d70,[d70_17, d70_16, d70_18, d70_19, d70_20]).
methyl(train,d70,[d70_12, d70_11, d70_13, d70_14, d70_15]).
methyl(train,d70,[d70_17, d70_16, d70_18, d70_19, d70_20]).
six_ring(train,d71,[d71_1, d71_2, d71_3, d71_4, d71_5, d71_6]).
non_ar_6c_ring(train,d71,[d71_1, d71_2, d71_3, d71_4, d71_5, d71_6]).
ether(train,d71,[d71_12, d71_13, d71_5]).
ether(train,d71,[d71_14, d71_13, d71_23]).
ether(train,d71,[d71_15, d71_13, d71_16]).
methoxy(train,d71,[d71_16, d71_15, d71_17, d71_18, d71_19]).
methoxy(train,d71,[d71_23, d71_14, d71_24, d71_25, d71_26]).
methyl(train,d71,[d71_16, d71_15, d71_17, d71_18, d71_19]).
methyl(train,d71,[d71_23, d71_14, d71_24, d71_25, d71_26]).
nitro(train,d71,[d71_11, d71_2, d71_21, d71_22]).
six_ring(train,d72,[d72_1, d72_2, d72_3, d72_4, d72_5, d72_6]).
non_ar_6c_ring(train,d72,[d72_1, d72_2, d72_3, d72_4, d72_5, d72_6]).
alcohol(train,d72,[d72_12, d72_19, d72_11]).
ether(train,d72,[d72_12, d72_11, d72_19]).
ketone(train,d72,[d72_18, d72_11, d72_12, d72_6]).
amine(train,d72,[d72_10, d72_16, d72_17, d72_5]).
nitro(train,d72,[d72_13, d72_3, d72_14, d72_15]).
six_ring(train,d73,[d73_1, d73_2, d73_3, d73_4, d73_5, d73_6]).
non_ar_6c_ring(train,d73,[d73_1, d73_2, d73_3, d73_4, d73_5, d73_6]).
amine(train,d73,[d73_10, d73_13, d73_14, d73_5]).
amine(train,d73,[d73_11, d73_15, d73_16, d73_4]).
nitro(train,d73,[d73_12, d73_2, d73_17, d73_18]).
six_ring(train,d74,[d74_1, d74_2, d74_3, d74_4, d74_5, d74_6]).
six_ring(train,d74,[d74_12, d74_13, d74_14, d74_15, d74_16, d74_17]).
non_ar_6c_ring(train,d74,[d74_1, d74_2, d74_3, d74_4, d74_5, d74_6]).
non_ar_6c_ring(train,d74,[d74_12, d74_13, d74_14, d74_15, d74_16, d74_17]).
amine(train,d74,[d74_23, d74_25, d74_26, d74_15]).
amine(train,d74,[d74_22, d74_24, d74_4, d74_12]).
six_ring(train,d75,[d75_1, d75_2, d75_3, d75_4, d75_5, d75_6]).
non_ar_6c_ring(train,d75,[d75_1, d75_2, d75_3, d75_4, d75_5, d75_6]).
amine(train,d75,[d75_10, d75_16, d75_17, d75_6]).
amine(train,d75,[d75_15, d75_18, d75_19, d75_4]).
methyl(train,d75,[d75_11, d75_5, d75_12, d75_13, d75_14]).
six_ring(train,d76,[d76_1, d76_2, d76_3, d76_4, d76_5, d76_6]).
six_ring(train,d76,[d76_12, d76_13, d76_23, d76_22, d76_21, d76_20]).
six_ring(train,d76,[d76_12, d76_13, d76_14, d76_15, d76_16, d76_17]).
non_ar_6c_ring(train,d76,[d76_1, d76_2, d76_3, d76_4, d76_5, d76_6]).
non_ar_6c_ring(train,d76,[d76_12, d76_13, d76_23, d76_22, d76_21, d76_20]).
non_ar_6c_ring(train,d76,[d76_12, d76_13, d76_14, d76_15, d76_16, d76_17]).
phenol(train,d76,[d76_28, d76_34, d76_16]).
sulfide(train,d76,[d76_29, d76_20, d76_32]).
sulfide(train,d76,[d76_36, d76_35, d76_39]).
ether(train,d76,[d76_28, d76_16, d76_34]).
ether(train,d76,[d76_32, d76_29, d76_33]).
ether(train,d76,[d76_39, d76_36, d76_42]).
sulfo(train,d76,[d76_29, d76_30, d76_31, d76_32, d76_20]).
sulfo(train,d76,[d76_36, d76_39, d76_40, d76_41, d76_35]).
six_ring(train,d77,[d77_1, d77_2, d77_3, d77_4, d77_5, d77_6]).
six_ring(train,d77,[d77_10, d77_11, d77_12, d77_13, d77_14, d77_15]).
six_ring(train,d77,[d77_34, d77_35, d77_36, d77_37, d77_38, d77_39]).
six_ring(train,d77,[d77_59, d77_60, d77_61, d77_62, d77_63, d77_64]).
non_ar_6c_ring(train,d77,[d77_1, d77_2, d77_3, d77_4, d77_5, d77_6]).
non_ar_6c_ring(train,d77,[d77_10, d77_11, d77_12, d77_13, d77_14, d77_15]).
non_ar_6c_ring(train,d77,[d77_34, d77_35, d77_36, d77_37, d77_38, d77_39]).
non_ar_6c_ring(train,d77,[d77_59, d77_60, d77_61, d77_62, d77_63, d77_64]).
ar_halide(train,d77,[d77_19, d77_6]).
ar_halide(train,d77,[d77_20, d77_10]).
ketone(train,d77,[d77_31, d77_24, d77_23, d77_27]).
ketone(train,d77,[d77_32, d77_25, d77_23, d77_26]).
ketone(train,d77,[d77_50, d77_49, d77_48, d77_51]).
ketone(train,d77,[d77_57, d77_55, d77_48, d77_58]).
amine(train,d77,[d77_26, d77_45, d77_34, d77_25]).
amine(train,d77,[d77_58, d77_70, d77_59, d77_55]).
methyl(train,d77,[d77_27, d77_24, d77_28, d77_29, d77_30]).
methyl(train,d77,[d77_51, d77_49, d77_52, d77_53, d77_54]).
six_ring(train,d78,[d78_1, d78_2, d78_3, d78_4, d78_5, d78_6]).
non_ar_6c_ring(train,d78,[d78_1, d78_2, d78_3, d78_4, d78_5, d78_6]).
alcohol(train,d78,[d78_15, d78_38, d78_12]).
alcohol(train,d78,[d78_22, d78_37, d78_19]).
alcohol(train,d78,[d78_31, d78_39, d78_28]).
ether(train,d78,[d78_15, d78_12, d78_38]).
ether(train,d78,[d78_22, d78_19, d78_37]).
ether(train,d78,[d78_31, d78_28, d78_39]).
amine(train,d78,[d78_26, d78_36, d78_27, d78_2]).
nitro(train,d78,[d78_25, d78_3, d78_34, d78_35]).
six_ring(train,d79,[d79_1, d79_2, d79_3, d79_4, d79_5, d79_6]).
six_ring(train,d79,[d79_3, d79_4, d79_14, d79_13, d79_12, d79_11]).
non_ar_6c_ring(train,d79,[d79_1, d79_2, d79_3, d79_4, d79_5, d79_6]).
non_ar_6c_ring(train,d79,[d79_3, d79_4, d79_14, d79_13, d79_12, d79_11]).
amine(train,d79,[d79_23, d79_27, d79_28, d79_20]).
amine(train,d79,[d79_18, d79_26, d79_19, d79_14]).
six_ring(train,d8,[d8_1, d8_2, d8_3, d8_4, d8_5, d8_6]).
six_ring(train,d8,[d8_10, d8_11, d8_12, d8_13, d8_14, d8_15]).
non_ar_6c_ring(train,d8,[d8_1, d8_2, d8_3, d8_4, d8_5, d8_6]).
non_ar_6c_ring(train,d8,[d8_10, d8_11, d8_12, d8_13, d8_14, d8_15]).
phenol(train,d8,[d8_26, d8_27, d8_4]).
ketone(train,d8,[d8_31, d8_29, d8_28, d8_32]).
amine(train,d8,[d8_28, d8_30, d8_29, d8_12]).
methyl(train,d8,[d8_22, d8_1, d8_23, d8_24, d8_25]).
methyl(train,d8,[d8_32, d8_29, d8_33, d8_34, d8_35]).
six_ring(train,d80,[d80_1, d80_2, d80_3, d80_4, d80_5, d80_6]).
six_ring(train,d80,[d80_3, d80_4, d80_14, d80_13, d80_12, d80_11]).
non_ar_6c_ring(train,d80,[d80_1, d80_2, d80_3, d80_4, d80_5, d80_6]).
non_ar_6c_ring(train,d80,[d80_3, d80_4, d80_14, d80_13, d80_12, d80_11]).
nitro(train,d80,[d80_18, d80_14, d80_19, d80_20]).
six_ring(train,d81,[d81_1, d81_2, d81_3, d81_4, d81_5, d81_6]).
non_ar_6c_ring(train,d81,[d81_1, d81_2, d81_3, d81_4, d81_5, d81_6]).
amine(train,d81,[d81_11, d81_13, d81_14, d81_5]).
amine(train,d81,[d81_12, d81_15, d81_16, d81_2]).
five_ring(train,d82,[d82_12, d82_13, d82_14, d82_16, d82_15]).
non_ar_hetero_5_ring(train,d82,[d82_12, d82_13, d82_14, d82_16, d82_15]).
six_ring(train,d82,[d82_1, d82_2, d82_3, d82_4, d82_5, d82_6]).
non_ar_6c_ring(train,d82,[d82_1, d82_2, d82_3, d82_4, d82_5, d82_6]).
sulfide(train,d82,[d82_30, d82_29, d82_4]).
amine(train,d82,[d82_11, d82_25, d82_26, d82_1]).
amine(train,d82,[d82_29, d82_31, d82_30, d82_12]).
methyl(train,d82,[d82_17, d82_15, d82_18, d82_19, d82_20]).
methyl(train,d82,[d82_21, d82_16, d82_22, d82_23, d82_24]).
six_ring(train,d83,[d83_1, d83_2, d83_3, d83_4, d83_5, d83_6]).
non_ar_6c_ring(train,d83,[d83_1, d83_2, d83_3, d83_4, d83_5, d83_6]).
amine(train,d83,[d83_13, d83_14, d83_15, d83_4]).
amine(train,d83,[d83_17, d83_18, d83_19, d83_1]).
methyl(train,d83,[d83_9, d83_5, d83_10, d83_11, d83_12]).
ar_halide(train,d84,[d84a_23, d84a_1]).
ar_halide(train,d84,[d84a_26, d84a_6]).
ar_halide(train,d84,[d84a_28, d84a_9]).
ar_halide(train,d84,[d84a_30, d84a_14]).
ar_halide(train,d84,[d84a_31, d84a_14]).
ar_halide(train,d84,[d84a_32, d84a_15]).
ar_halide(train,d84,[d84a_33, d84a_15]).
ar_halide(train,d84,[d84a_34, d84a_11]).
ar_halide(train,d84,[d84b_24, d84b_4]).
ar_halide(train,d84,[d84b_25, d84b_4]).
ar_halide(train,d84,[d84b_27, d84b_5]).
ar_halide(train,d84,[d84b_30, d84b_16]).
ar_halide(train,d84,[d84b_31, d84b_16]).
ar_halide(train,d84,[d84b_32, d84b_16]).
ar_halide(train,d84,[d84b_33, d84b_17]).
ar_halide(train,d84,[d84b_36, d84b_15]).
alkyl_halide(train,d84,[d84a_23, d84a_1]).
alkyl_halide(train,d84,[d84a_26, d84a_6]).
alkyl_halide(train,d84,[d84a_28, d84a_9]).
alkyl_halide(train,d84,[d84a_30, d84a_14]).
alkyl_halide(train,d84,[d84a_31, d84a_14]).
alkyl_halide(train,d84,[d84a_32, d84a_15]).
alkyl_halide(train,d84,[d84a_33, d84a_15]).
alkyl_halide(train,d84,[d84a_34, d84a_11]).
alkyl_halide(train,d84,[d84b_24, d84b_4]).
alkyl_halide(train,d84,[d84b_25, d84b_4]).
alkyl_halide(train,d84,[d84b_27, d84b_5]).
alkyl_halide(train,d84,[d84b_30, d84b_16]).
alkyl_halide(train,d84,[d84b_31, d84b_16]).
alkyl_halide(train,d84,[d84b_32, d84b_16]).
alkyl_halide(train,d84,[d84b_33, d84b_17]).
alkyl_halide(train,d84,[d84b_36, d84b_15]).
methyl(train,d84,[d84a_20, d84a_17, d84a_36, d84a_37, d84a_38]).
methyl(train,d84,[d84b_7, d84b_1, d84b_10, d84b_8, d84b_9]).
methyl(train,d84,[d84b_11, d84b_1, d84b_12, d84b_13, d84b_14]).
methyl(train,d84,[d84b_18, d84b_6, d84b_21, d84b_22, d84b_23]).
six_ring(train,d85_1,[d85_1_1, d85_1_2, d85_1_3, d85_1_4, d85_1_5, d85_1_6]).
six_ring(train,d85_1,[d85_1_3, d85_1_4, d85_1_15, d85_1_13, d85_1_8, d85_1_16]).
six_ring(train,d85_1,[d85_1_8, d85_1_9, d85_1_10, d85_1_11, d85_1_12, d85_1_13]).
non_ar_hetero_6_ring(train,d85_1,[d85_1_3, d85_1_4, d85_1_15, d85_1_13, d85_1_8, d85_1_16]).
non_ar_6c_ring(train,d85_1,[d85_1_1, d85_1_2, d85_1_3, d85_1_4, d85_1_5, d85_1_6]).
non_ar_6c_ring(train,d85_1,[d85_1_8, d85_1_9, d85_1_10, d85_1_11, d85_1_12, d85_1_13]).
ar_halide(train,d85_1,[d85_1_17, d85_1_12]).
ar_halide(train,d85_1,[d85_1_18, d85_1_11]).
ar_halide(train,d85_1,[d85_1_19, d85_1_10]).
ar_halide(train,d85_1,[d85_1_20, d85_1_2]).
ar_halide(train,d85_1,[d85_1_21, d85_1_1]).
ar_halide(train,d85_1,[d85_1_22, d85_1_6]).
ether(train,d85_1,[d85_1_15, d85_1_13, d85_1_4]).
ether(train,d85_1,[d85_1_16, d85_1_3, d85_1_8]).
six_ring(train,d85_2,[d85_2_1, d85_2_2, d85_2_3, d85_2_4, d85_2_5, d85_2_6]).
six_ring(train,d85_2,[d85_2_3, d85_2_4, d85_2_15, d85_2_13, d85_2_8, d85_2_16]).
six_ring(train,d85_2,[d85_2_8, d85_2_9, d85_2_10, d85_2_11, d85_2_12, d85_2_13]).
non_ar_hetero_6_ring(train,d85_2,[d85_2_3, d85_2_4, d85_2_15, d85_2_13, d85_2_8, d85_2_16]).
non_ar_6c_ring(train,d85_2,[d85_2_1, d85_2_2, d85_2_3, d85_2_4, d85_2_5, d85_2_6]).
non_ar_6c_ring(train,d85_2,[d85_2_8, d85_2_9, d85_2_10, d85_2_11, d85_2_12, d85_2_13]).
ar_halide(train,d85_2,[d85_2_17, d85_2_12]).
ar_halide(train,d85_2,[d85_2_18, d85_2_11]).
ar_halide(train,d85_2,[d85_2_19, d85_2_10]).
ar_halide(train,d85_2,[d85_2_20, d85_2_5]).
ar_halide(train,d85_2,[d85_2_21, d85_2_6]).
ar_halide(train,d85_2,[d85_2_22, d85_2_1]).
ether(train,d85_2,[d85_2_15, d85_2_13, d85_2_4]).
ether(train,d85_2,[d85_2_16, d85_2_3, d85_2_8]).
six_ring(train,d86,[d86a_1, d86a_2, d86a_3, d86a_4, d86a_5, d86a_6]).
six_ring(train,d86,[d86a_9, d86a_10, d86a_11, d86a_12, d86a_13, d86a_14]).
six_ring(train,d86,[d86b_1, d86b_2, d86b_3, d86b_4, d86b_5, d86b_6]).
six_ring(train,d86,[d86b_8, d86b_9, d86b_10, d86b_11, d86b_12, d86b_13]).
non_ar_6c_ring(train,d86,[d86a_1, d86a_2, d86a_3, d86a_4, d86a_5, d86a_6]).
non_ar_6c_ring(train,d86,[d86a_9, d86a_10, d86a_11, d86a_12, d86a_13, d86a_14]).
non_ar_6c_ring(train,d86,[d86b_1, d86b_2, d86b_3, d86b_4, d86b_5, d86b_6]).
non_ar_6c_ring(train,d86,[d86b_8, d86b_9, d86b_10, d86b_11, d86b_12, d86b_13]).
ar_halide(train,d86,[d86a_16, d86a_5]).
ar_halide(train,d86,[d86a_17, d86a_6]).
ar_halide(train,d86,[d86a_18, d86a_1]).
ar_halide(train,d86,[d86a_19, d86a_14]).
ar_halide(train,d86,[d86a_20, d86a_13]).
ar_halide(train,d86,[d86a_21, d86a_12]).
ar_halide(train,d86,[d86a_22, d86a_11]).
ar_halide(train,d86,[d86b_15, d86b_5]).
ar_halide(train,d86,[d86b_16, d86b_9]).
ar_halide(train,d86,[d86b_18, d86b_1]).
ar_halide(train,d86,[d86b_19, d86b_3]).
ar_halide(train,d86,[d86b_22, d86b_12]).
six_ring(train,d87,[d87_1, d87_2, d87_3, d87_4, d87_5, d87_6]).
six_ring(train,d87,[d87_3, d87_4, d87_17, d87_14, d87_9, d87_18]).
six_ring(train,d87,[d87_9, d87_10, d87_11, d87_12, d87_13, d87_14]).
non_ar_hetero_6_ring(train,d87,[d87_3, d87_4, d87_17, d87_14, d87_9, d87_18]).
non_ar_6c_ring(train,d87,[d87_1, d87_2, d87_3, d87_4, d87_5, d87_6]).
non_ar_6c_ring(train,d87,[d87_9, d87_10, d87_11, d87_12, d87_13, d87_14]).
ar_halide(train,d87,[d87_19, d87_6]).
ar_halide(train,d87,[d87_20, d87_1]).
ar_halide(train,d87,[d87_21, d87_12]).
ar_halide(train,d87,[d87_22, d87_11]).
ether(train,d87,[d87_17, d87_14, d87_4]).
ether(train,d87,[d87_18, d87_3, d87_9]).
six_ring(train,d88,[d88_1, d88_2, d88_3, d88_4, d88_5, d88_6]).
non_ar_6c_ring(train,d88,[d88_1, d88_2, d88_3, d88_4, d88_5, d88_6]).
ar_halide(train,d88,[d88_10, d88_4]).
ar_halide(train,d88,[d88_11, d88_6]).
ar_halide(train,d88,[d88_12, d88_2]).
phenol(train,d88,[d88_9, d88_13, d88_5]).
five_ring(train,d89,[d89_1, d89_2, d89_3, d89_7, d89_5]).
five_ring(train,d89,[d89_3, d89_4, d89_6, d89_5, d89_7]).
non_ar_5c_ring(train,d89,[d89_1, d89_2, d89_3, d89_7, d89_5]).
non_ar_5c_ring(train,d89,[d89_3, d89_4, d89_6, d89_5, d89_7]).
six_ring(train,d89,[d89_1, d89_2, d89_3, d89_4, d89_6, d89_5]).
non_ar_6c_ring(train,d89,[d89_1, d89_2, d89_3, d89_4, d89_6, d89_5]).
ar_halide(train,d89,[d89_10, d89_7]).
ar_halide(train,d89,[d89_11, d89_7]).
ar_halide(train,d89,[d89_12, d89_1]).
ar_halide(train,d89,[d89_13, d89_2]).
alkyl_halide(train,d89,[d89_10, d89_7]).
alkyl_halide(train,d89,[d89_11, d89_7]).
alkyl_halide(train,d89,[d89_12, d89_1]).
alkyl_halide(train,d89,[d89_13, d89_2]).
ester(train,d89,[d89_14, d89_22, d89_19, d89_6, d89_4]).
ester(train,d89,[d89_14, d89_22, d89_19, d89_6, d89_15]).
ester(train,d89,[d89_14, d89_22, d89_19, d89_6, d89_5]).
ester(train,d89,[d89_16, d89_18, d89_20, d89_4, d89_17]).
ester(train,d89,[d89_16, d89_18, d89_20, d89_4, d89_3]).
ester(train,d89,[d89_16, d89_18, d89_20, d89_4, d89_6]).
alcohol(train,d89,[d89_18, d89_21, d89_16]).
alcohol(train,d89,[d89_22, d89_23, d89_14]).
ether(train,d89,[d89_18, d89_16, d89_21]).
ether(train,d89,[d89_22, d89_14, d89_23]).
ketone(train,d89,[d89_19, d89_14, d89_22, d89_6]).
ketone(train,d89,[d89_20, d89_16, d89_18, d89_4]).
six_ring(train,d9,[d9_1, d9_2, d9_3, d9_4, d9_5, d9_6]).
non_ar_6c_ring(train,d9,[d9_1, d9_2, d9_3, d9_4, d9_5, d9_6]).
ether(train,d9,[d9_15, d9_18, d9_4]).
amine(train,d9,[d9_14, d9_16, d9_17, d9_5]).
methoxy(train,d9,[d9_18, d9_15, d9_19, d9_20, d9_21]).
methyl(train,d9,[d9_10, d9_1, d9_11, d9_12, d9_13]).
methyl(train,d9,[d9_18, d9_15, d9_19, d9_20, d9_21]).
six_ring(train,d90,[d90_1, d90_2, d90_3, d90_4, d90_5, d90_6]).
non_ar_6c_ring(train,d90,[d90_1, d90_2, d90_3, d90_4, d90_5, d90_6]).
ar_halide(train,d90,[d90_11, d90_5]).
ar_halide(train,d90,[d90_12, d90_2]).
ar_halide(train,d91,[d91_1, d91_2]).
ar_halide(train,d91,[d91_3, d91_2]).
ar_halide(train,d91,[d91_5, d91_4]).
ar_halide(train,d91,[d91_6, d91_4]).
ar_halide(train,d92,[d92_3, d92_1]).
ar_halide(train,d92,[d92_4, d92_1]).
ar_halide(train,d92,[d92_5, d92_1]).
ar_halide(train,d92,[d92_6, d92_2]).
ar_halide(train,d92,[d92_7, d92_2]).
ar_halide(train,d92,[d92_8, d92_2]).
alkyl_halide(train,d92,[d92_3, d92_1]).
alkyl_halide(train,d92,[d92_4, d92_1]).
alkyl_halide(train,d92,[d92_5, d92_1]).
alkyl_halide(train,d92,[d92_6, d92_2]).
alkyl_halide(train,d92,[d92_7, d92_2]).
alkyl_halide(train,d92,[d92_8, d92_2]).
six_ring(train,d93,[d93_1, d93_2, d93_3, d93_4, d93_5, d93_6]).
non_ar_6c_ring(train,d93,[d93_1, d93_2, d93_3, d93_4, d93_5, d93_6]).
ar_halide(train,d93,[d93_8, d93_4]).
ar_halide(train,d93,[d93_9, d93_3]).
ar_halide(train,d93,[d93_10, d93_2]).
ar_halide(train,d93,[d93_11, d93_1]).
ar_halide(train,d93,[d93_12, d93_6]).
phenol(train,d93,[d93_7, d93_13, d93_5]).
ar_halide(train,d94,[d94_3, d94_2]).
ar_halide(train,d94,[d94_4, d94_2]).
ar_halide(train,d94,[d94_6, d94_1]).
alkyl_halide(train,d94,[d94_3, d94_2]).
alkyl_halide(train,d94,[d94_4, d94_2]).
alkyl_halide(train,d94,[d94_6, d94_1]).
six_ring(train,d95,[d95_1, d95_2, d95_3, d95_4, d95_5, d95_6]).
non_ar_6c_ring(train,d95,[d95_1, d95_2, d95_3, d95_4, d95_5, d95_6]).
ar_halide(train,d95,[d95_11, d95_2]).
ketone(train,d95,[d95_23, d95_13, d95_12, d95_14]).
amine(train,d95,[d95_12, d95_24, d95_5, d95_13]).
methyl(train,d95,[d95_15, d95_14, d95_16, d95_17, d95_18]).
methyl(train,d95,[d95_19, d95_14, d95_20, d95_21, d95_22]).
five_ring(train,d96,[d96_1, d96_2, d96_3, d96_14, d96_6]).
five_ring(train,d96,[d96_3, d96_14, d96_6, d96_5, d96_4]).
five_ring(train,d96,[d96_4, d96_5, d96_9, d96_8, d96_7]).
non_ar_5c_ring(train,d96,[d96_1, d96_2, d96_3, d96_14, d96_6]).
non_ar_5c_ring(train,d96,[d96_3, d96_14, d96_6, d96_5, d96_4]).
non_ar_5c_ring(train,d96,[d96_4, d96_5, d96_9, d96_8, d96_7]).
six_ring(train,d96,[d96_1, d96_2, d96_3, d96_4, d96_5, d96_6]).
non_ar_6c_ring(train,d96,[d96_1, d96_2, d96_3, d96_4, d96_5, d96_6]).
ar_halide(train,d96,[d96_15, d96_14]).
ar_halide(train,d96,[d96_16, d96_14]).
ar_halide(train,d96,[d96_17, d96_8]).
ar_halide(train,d96,[d96_19, d96_7]).
ar_halide(train,d96,[d96_21, d96_3]).
ar_halide(train,d96,[d96_22, d96_2]).
ar_halide(train,d96,[d96_23, d96_1]).
ar_halide(train,d96,[d96_24, d96_6]).
alkyl_halide(train,d96,[d96_15, d96_14]).
alkyl_halide(train,d96,[d96_16, d96_14]).
alkyl_halide(train,d96,[d96_17, d96_8]).
alkyl_halide(train,d96,[d96_19, d96_7]).
alkyl_halide(train,d96,[d96_21, d96_3]).
alkyl_halide(train,d96,[d96_22, d96_2]).
alkyl_halide(train,d96,[d96_23, d96_1]).
alkyl_halide(train,d96,[d96_24, d96_6]).
six_ring(train,d97,[d97_1, d97_2, d97_3, d97_4, d97_5, d97_6]).
six_ring(train,d97,[d97_11, d97_12, d97_13, d97_14, d97_15, d97_16]).
non_ar_6c_ring(train,d97,[d97_1, d97_2, d97_3, d97_4, d97_5, d97_6]).
non_ar_6c_ring(train,d97,[d97_11, d97_12, d97_13, d97_14, d97_15, d97_16]).
ar_halide(train,d97,[d97_34, d97_1]).
ar_halide(train,d97,[d97_35, d97_14]).
ester(train,d97,[d97_23, d97_24, d97_25, d97_21, d97_11]).
ester(train,d97,[d97_23, d97_24, d97_25, d97_21, d97_22]).
ester(train,d97,[d97_23, d97_24, d97_25, d97_21, d97_4]).
alcohol(train,d97,[d97_22, d97_33, d97_21]).
ether(train,d97,[d97_22, d97_21, d97_33]).
ether(train,d97,[d97_24, d97_23, d97_26]).
ketone(train,d97,[d97_25, d97_23, d97_21, d97_24]).
methyl(train,d97,[d97_27, d97_26, d97_30, d97_31, d97_32]).
six_ring(train,d98,[d98_1, d98_2, d98_3, d98_4, d98_5, d98_6]).
non_ar_6c_ring(train,d98,[d98_1, d98_2, d98_3, d98_4, d98_5, d98_6]).
ar_halide(train,d98,[d98_7, d98_5]).
ar_halide(train,d98,[d98_9, d98_3]).
ar_halide(train,d98,[d98_11, d98_1]).
ar_halide(train,d98,[d98_12, d98_6]).
six_ring(train,d99,[d99_1, d99_2, d99_3, d99_4, d99_5, d99_6]).
six_ring(train,d99,[d99_7, d99_8, d99_9, d99_10, d99_11, d99_12]).
non_ar_6c_ring(train,d99,[d99_1, d99_2, d99_3, d99_4, d99_5, d99_6]).
non_ar_6c_ring(train,d99,[d99_7, d99_8, d99_9, d99_10, d99_11, d99_12]).
ar_halide(train,d99,[d99_14, d99_5]).
ar_halide(train,d99,[d99_15, d99_6]).
ar_halide(train,d99,[d99_16, d99_1]).
ar_halide(train,d99,[d99_17, d99_2]).
ar_halide(train,d99,[d99_18, d99_3]).
ar_halide(train,d99,[d99_19, d99_8]).
ar_halide(train,d99,[d99_20, d99_11]).
ar_halide(train,d99,[d99_21, d99_10]).
ar_halide(train,d99,[d99_22, d99_9]).
ar_halide(train,d99,[d99_23, d99_12]).
ether(train,d99,[d99_13, d99_4, d99_7]).
mutagenic(test,d297).
mutagenic(test,d300).
mutagenic(test,d308).
mutagenic(test,d309).
mutagenic(test,d311).
mutagenic(test,d313).
mutagenic(test,d314).
mutagenic(test,d322).
mutagenic(test,d323).
mutagenic(test,d324).
mutagenic(test,d329).
mutagenic(test,d330).
mutagenic(test,d332).
mutagenic(test,d334).
ames(test,d316).
ames(test,d319).
ames(test,d320).
ames(test,d323).
ames(test,d324).
ames(test,d325).
ames(test,d328).
ames(test,d329).
ames(test,d331).
ames(test,d332).
ames(test,d333).
ames(test,d334).
ames(test,d335).
ames(test,d336).
ames(test,d337).
atm(test,d296,d296_1,c,22,-0.13).
atm(test,d296,d296_2,c,22,-0.13).
atm(test,d296,d296_3,c,27,0.0).
atm(test,d296,d296_4,c,27,0.0).
atm(test,d296,d296_5,c,22,-0.13).
atm(test,d296,d296_6,c,22,-0.13).
atm(test,d296,d296_7,h,3,0.13).
atm(test,d296,d296_8,h,3,0.13).
atm(test,d296,d296_9,h,3,0.13).
atm(test,d296,d296_10,h,3,0.13).
atm(test,d296,d296_11,c,22,-0.13).
atm(test,d296,d296_12,c,22,-0.13).
atm(test,d296,d296_13,c,22,-0.13).
atm(test,d296,d296_14,c,22,-0.13).
atm(test,d296,d296_15,h,3,0.13).
atm(test,d296,d296_16,h,3,0.13).
atm(test,d296,d296_17,h,3,0.13).
atm(test,d296,d296_18,h,3,0.13).
atm(test,d297,d297_1,c,10,0.074).
atm(test,d297,d297_2,c,10,0.074).
atm(test,d297,d297_3,c,10,0.029).
atm(test,d297,d297_4,o,50,-0.362).
atm(test,d297,d297_5,h,3,0.069).
atm(test,d297,d297_6,h,3,0.069).
atm(test,d297,d297_7,c,10,0.074).
atm(test,d297,d297_8,o,50,-0.362).
atm(test,d297,d297_9,h,3,0.044).
atm(test,d297,d297_10,c,10,0.029).
atm(test,d297,d297_11,c,10,0.029).
atm(test,d297,d297_12,h,3,0.069).
atm(test,d297,d297_13,h,3,0.069).
atm(test,d297,d297_14,o,50,-0.362).
atm(test,d297,d297_15,h,3,0.069).
atm(test,d297,d297_16,h,3,0.069).
atm(test,d297,d297_17,c,10,0.029).
atm(test,d297,d297_18,c,10,0.029).
atm(test,d297,d297_19,h,3,0.069).
atm(test,d297,d297_20,h,3,0.069).
atm(test,d297,d297_21,o,50,-0.362).
atm(test,d297,d297_22,h,3,0.069).
atm(test,d297,d297_23,h,3,0.069).
atm(test,d297,d297_24,c,10,0.029).
atm(test,d297,d297_25,c,10,0.029).
atm(test,d297,d297_26,h,3,0.069).
atm(test,d297,d297_27,h,3,0.069).
atm(test,d297,d297_28,o,50,-0.361).
atm(test,d297,d297_29,h,3,0.069).
atm(test,d297,d297_30,h,3,0.069).
atm(test,d297,d297_31,c,10,0.029).
atm(test,d297,d297_32,c,10,0.029).
atm(test,d297,d297_33,h,3,0.069).
atm(test,d297,d297_34,h,3,0.069).
atm(test,d297,d297_35,o,50,-0.362).
atm(test,d297,d297_36,h,3,0.069).
atm(test,d297,d297_37,h,3,0.069).
atm(test,d297,d297_38,c,10,0.029).
atm(test,d297,d297_39,c,10,0.179).
atm(test,d297,d297_40,h,3,0.069).
atm(test,d297,d297_41,h,3,0.069).
atm(test,d297,d297_42,o,45,-0.642).
atm(test,d297,d297_43,h,3,0.049).
atm(test,d297,d297_44,h,3,0.049).
atm(test,d297,d297_45,h,8,0.408).
atm(test,d297,d297_46,o,50,-0.362).
atm(test,d297,d297_47,h,3,0.044).
atm(test,d297,d297_48,c,10,0.029).
atm(test,d297,d297_49,c,10,0.029).
atm(test,d297,d297_50,h,3,0.069).
atm(test,d297,d297_51,h,3,0.069).
atm(test,d297,d297_52,o,50,-0.361).
atm(test,d297,d297_53,h,3,0.069).
atm(test,d297,d297_54,h,3,0.069).
atm(test,d297,d297_55,c,10,0.029).
atm(test,d297,d297_56,c,10,0.029).
atm(test,d297,d297_57,h,3,0.069).
atm(test,d297,d297_58,h,3,0.069).
atm(test,d297,d297_59,o,50,-0.362).
atm(test,d297,d297_60,h,3,0.069).
atm(test,d297,d297_61,h,3,0.069).
atm(test,d297,d297_62,c,10,0.029).
atm(test,d297,d297_63,c,10,0.029).
atm(test,d297,d297_64,h,3,0.069).
atm(test,d297,d297_65,h,3,0.069).
atm(test,d297,d297_66,o,50,-0.361).
atm(test,d297,d297_67,h,3,0.069).
atm(test,d297,d297_68,h,3,0.069).
atm(test,d297,d297_69,c,10,0.029).
atm(test,d297,d297_70,c,10,0.029).
atm(test,d297,d297_71,h,3,0.069).
atm(test,d297,d297_72,h,3,0.069).
atm(test,d297,d297_73,o,50,-0.362).
atm(test,d297,d297_74,h,3,0.069).
atm(test,d297,d297_75,h,3,0.069).
atm(test,d297,d297_76,c,10,0.029).
atm(test,d297,d297_77,c,10,0.179).
atm(test,d297,d297_78,h,3,0.069).
atm(test,d297,d297_79,h,3,0.069).
atm(test,d297,d297_80,o,45,-0.642).
atm(test,d297,d297_81,h,3,0.049).
atm(test,d297,d297_82,h,3,0.049).
atm(test,d297,d297_83,h,8,0.408).
atm(test,d297,d297_84,c,10,0.074).
atm(test,d297,d297_85,h,3,0.044).
atm(test,d297,d297_86,c,10,0.029).
atm(test,d297,d297_87,o,50,-0.362).
atm(test,d297,d297_88,h,3,0.044).
atm(test,d297,d297_89,o,50,-0.362).
atm(test,d297,d297_90,h,3,0.069).
atm(test,d297,d297_91,h,3,0.069).
atm(test,d297,d297_92,c,10,0.029).
atm(test,d297,d297_93,c,10,0.029).
atm(test,d297,d297_94,h,3,0.069).
atm(test,d297,d297_95,h,3,0.069).
atm(test,d297,d297_96,o,50,-0.362).
atm(test,d297,d297_97,h,3,0.069).
atm(test,d297,d297_98,h,3,0.069).
atm(test,d297,d297_99,c,10,0.029).
atm(test,d297,d297_100,c,10,0.029).
atm(test,d297,d297_101,h,3,0.069).
atm(test,d297,d297_102,h,3,0.069).
atm(test,d297,d297_103,o,50,-0.362).
atm(test,d297,d297_104,h,3,0.069).
atm(test,d297,d297_105,h,3,0.069).
atm(test,d297,d297_106,c,10,0.029).
atm(test,d297,d297_107,c,10,0.029).
atm(test,d297,d297_108,h,3,0.069).
atm(test,d297,d297_109,h,3,0.069).
atm(test,d297,d297_110,o,50,-0.362).
atm(test,d297,d297_111,h,3,0.069).
atm(test,d297,d297_112,h,3,0.069).
atm(test,d297,d297_113,c,10,0.029).
atm(test,d297,d297_114,c,10,0.029).
atm(test,d297,d297_115,h,3,0.069).
atm(test,d297,d297_116,h,3,0.069).
atm(test,d297,d297_117,o,50,-0.362).
atm(test,d297,d297_118,h,3,0.069).
atm(test,d297,d297_119,h,3,0.069).
atm(test,d297,d297_120,c,10,0.029).
atm(test,d297,d297_121,c,10,0.179).
atm(test,d297,d297_122,h,3,0.069).
atm(test,d297,d297_123,h,3,0.069).
atm(test,d297,d297_124,o,45,-0.642).
atm(test,d297,d297_125,h,3,0.049).
atm(test,d297,d297_126,h,3,0.049).
atm(test,d297,d297_127,h,8,0.408).
atm(test,d297,d297_128,c,10,0.029).
atm(test,d297,d297_129,c,10,0.029).
atm(test,d297,d297_130,h,3,0.069).
atm(test,d297,d297_131,h,3,0.069).
atm(test,d297,d297_132,o,50,-0.362).
atm(test,d297,d297_133,h,3,0.069).
atm(test,d297,d297_134,h,3,0.069).
atm(test,d297,d297_135,c,10,0.029).
atm(test,d297,d297_136,c,10,0.029).
atm(test,d297,d297_137,h,3,0.069).
atm(test,d297,d297_138,h,3,0.069).
atm(test,d297,d297_139,o,50,-0.362).
atm(test,d297,d297_140,h,3,0.069).
atm(test,d297,d297_141,h,3,0.069).
atm(test,d297,d297_142,c,10,0.029).
atm(test,d297,d297_143,c,10,0.029).
atm(test,d297,d297_144,h,3,0.069).
atm(test,d297,d297_145,h,3,0.069).
atm(test,d297,d297_146,o,50,-0.361).
atm(test,d297,d297_147,h,3,0.069).
atm(test,d297,d297_148,h,3,0.069).
atm(test,d297,d297_149,c,10,0.029).
atm(test,d297,d297_150,c,10,0.029).
atm(test,d297,d297_151,h,3,0.069).
atm(test,d297,d297_152,h,3,0.069).
atm(test,d297,d297_153,o,50,-0.362).
atm(test,d297,d297_154,h,3,0.069).
atm(test,d297,d297_155,h,3,0.069).
atm(test,d297,d297_156,c,10,0.029).
atm(test,d297,d297_157,c,10,0.259).
atm(test,d297,d297_158,h,3,0.069).
atm(test,d297,d297_159,h,3,0.069).
atm(test,d297,d297_160,o,49,-0.642).
atm(test,d297,d297_161,h,3,0.109).
atm(test,d297,d297_162,h,3,0.109).
atm(test,d297,d297_163,c,14,0.708).
atm(test,d297,d297_164,o,51,-0.542).
atm(test,d297,d297_165,c,10,-0.091).
atm(test,d297,d297_166,c,10,-0.091).
atm(test,d297,d297_167,h,3,0.059).
atm(test,d297,d297_168,h,3,0.059).
atm(test,d297,d297_169,c,10,-0.091).
atm(test,d297,d297_170,h,3,0.059).
atm(test,d297,d297_171,h,3,0.059).
atm(test,d297,d297_172,c,10,-0.091).
atm(test,d297,d297_173,h,3,0.059).
atm(test,d297,d297_174,h,3,0.059).
atm(test,d297,d297_175,c,10,-0.091).
atm(test,d297,d297_176,h,3,0.059).
atm(test,d297,d297_177,h,3,0.059).
atm(test,d297,d297_178,c,10,0.109).
atm(test,d297,d297_179,h,3,0.059).
atm(test,d297,d297_180,h,3,0.059).
atm(test,d297,d297_181,c,16,-0.191).
atm(test,d297,d297_182,c,16,-0.191).
atm(test,d297,d297_183,c,10,0.109).
atm(test,d297,d297_184,c,10,-0.091).
atm(test,d297,d297_185,c,10,-0.091).
atm(test,d297,d297_186,h,3,0.059).
atm(test,d297,d297_187,h,3,0.059).
atm(test,d297,d297_188,c,10,-0.091).
atm(test,d297,d297_189,h,3,0.059).
atm(test,d297,d297_190,h,3,0.059).
atm(test,d297,d297_191,c,10,-0.091).
atm(test,d297,d297_192,h,3,0.059).
atm(test,d297,d297_193,h,3,0.059).
atm(test,d297,d297_194,c,10,-0.091).
atm(test,d297,d297_195,h,3,0.059).
atm(test,d297,d297_196,h,3,0.059).
atm(test,d297,d297_197,c,10,-0.091).
atm(test,d297,d297_198,h,3,0.059).
atm(test,d297,d297_199,h,3,0.059).
atm(test,d297,d297_200,c,10,-0.091).
atm(test,d297,d297_201,h,3,0.059).
atm(test,d297,d297_202,h,3,0.059).
atm(test,d297,d297_203,c,10,-0.141).
atm(test,d297,d297_204,h,3,0.059).
atm(test,d297,d297_205,h,3,0.059).
atm(test,d297,d297_206,h,3,0.059).
atm(test,d297,d297_207,h,3,0.059).
atm(test,d297,d297_208,h,3,0.059).
atm(test,d297,d297_209,h,3,0.059).
atm(test,d297,d297_210,h,3,0.059).
atm(test,d297,d297_211,h,3,0.059).
atm(test,d297,d297_212,h,3,0.059).
atm(test,d297,d297_213,h,3,0.109).
atm(test,d297,d297_214,h,3,0.109).
atm(test,d298,d298_1,c,22,-0.127).
atm(test,d298,d298_2,c,22,-0.127).
atm(test,d298,d298_3,c,22,-0.127).
atm(test,d298,d298_4,c,22,0.202).
atm(test,d298,d298_5,c,22,-0.127).
atm(test,d298,d298_6,c,22,-0.127).
atm(test,d298,d298_7,h,3,0.132).
atm(test,d298,d298_8,h,3,0.132).
atm(test,d298,d298_9,h,3,0.132).
atm(test,d298,d298_10,h,3,0.132).
atm(test,d298,d298_11,s,74,-0.097).
atm(test,d298,d298_12,c,22,-0.127).
atm(test,d298,d298_13,c,22,0.202).
atm(test,d298,d298_14,n,32,-0.398).
atm(test,d298,d298_15,c,22,-0.127).
atm(test,d298,d298_16,c,22,-0.127).
atm(test,d298,d298_17,c,22,-0.127).
atm(test,d298,d298_18,c,22,-0.127).
atm(test,d298,d298_19,h,3,0.132).
atm(test,d298,d298_20,h,3,0.133).
atm(test,d298,d298_21,h,3,0.133).
atm(test,d298,d298_22,h,3,0.133).
atm(test,d298,d298_23,c,10,0.003).
atm(test,d298,d298_24,c,10,0.053).
atm(test,d298,d298_25,h,3,0.053).
atm(test,d298,d298_26,h,3,0.053).
atm(test,d298,d298_27,c,10,-0.148).
atm(test,d298,d298_28,h,3,0.053).
atm(test,d298,d298_29,h,3,0.053).
atm(test,d298,d298_30,h,3,0.053).
atm(test,d298,d298_31,n,36,-0.298).
atm(test,d298,d298_32,h,3,0.103).
atm(test,d298,d298_33,c,10,0.003).
atm(test,d298,d298_34,h,3,0.053).
atm(test,d298,d298_35,h,3,0.053).
atm(test,d298,d298_36,h,3,0.053).
atm(test,d298,d298_37,c,10,0.003).
atm(test,d298,d298_38,h,3,0.053).
atm(test,d298,d298_39,h,3,0.053).
atm(test,d298,d298_40,h,3,0.053).
atm(test,d299,d299_1,c,22,-0.13).
atm(test,d299,d299_2,c,22,-0.13).
atm(test,d299,d299_3,c,22,0.25).
atm(test,d299,d299_4,c,22,-0.13).
atm(test,d299,d299_5,c,22,0.25).
atm(test,d299,d299_6,c,22,-0.13).
atm(test,d299,d299_7,h,3,0.13).
atm(test,d299,d299_8,h,3,0.13).
atm(test,d299,d299_9,h,3,0.13).
atm(test,d299,d299_10,h,3,0.13).
atm(test,d299,d299_11,o,45,-0.65).
atm(test,d299,d299_12,o,45,-0.65).
atm(test,d299,d299_13,h,8,0.4).
atm(test,d299,d299_14,h,8,0.4).
atm(test,d300,d300_1,c,10,-0.096).
atm(test,d300,d300_2,c,10,-0.096).
atm(test,d300,d300_3,c,14,0.705).
atm(test,d300,d300_4,h,3,0.054).
atm(test,d300,d300_5,h,3,0.054).
atm(test,d300,d300_6,o,49,-0.645).
atm(test,d300,d300_7,o,51,-0.546).
atm(test,d300,d300_8,c,10,0.254).
atm(test,d300,d300_9,h,3,0.054).
atm(test,d300,d300_10,h,3,0.054).
atm(test,d300,d300_11,h,3,0.104).
atm(test,d300,d300_12,h,3,0.104).
atm(test,d302,d302_1,cl,93,-0.144).
atm(test,d302,d302_2,c,10,-0.044).
atm(test,d302,d302_3,c,14,0.837).
atm(test,d302,d302_4,h,3,0.106).
atm(test,d302,d302_5,h,3,0.106).
atm(test,d302,d302_6,o,45,-0.613).
atm(test,d302,d302_7,o,51,-0.554).
atm(test,d302,d302_8,h,1,0.306).
atm(test,d303,d303_1,c,22,-0.121).
atm(test,d303,d303_2,c,22,-0.121).
atm(test,d303,d303_3,c,22,-0.121).
atm(test,d303,d303_4,c,22,-0.121).
atm(test,d303,d303_5,c,22,0.259).
atm(test,d303,d303_6,c,22,-0.121).
atm(test,d303,d303_7,h,3,0.139).
atm(test,d303,d303_8,h,3,0.139).
atm(test,d303,d303_9,h,3,0.139).
atm(test,d303,d303_10,h,3,0.139).
atm(test,d303,d303_11,o,45,-0.642).
atm(test,d303,d303_12,n,38,0.808).
atm(test,d303,d303_13,o,40,-0.392).
atm(test,d303,d303_14,o,40,-0.392).
atm(test,d303,d303_15,h,8,0.408).
atm(test,d304,d304a_1,c,22,-0.113).
atm(test,d304,d304a_2,c,22,-0.163).
atm(test,d304,d304a_3,c,22,0.266).
atm(test,d304,d304a_4,c,22,-0.163).
atm(test,d304,d304a_5,c,22,-0.113).
atm(test,d304,d304a_6,c,22,-0.164).
atm(test,d304,d304a_7,h,3,0.147).
atm(test,d304,d304a_8,h,3,0.147).
atm(test,d304,d304a_9,o,49,-0.654).
atm(test,d304,d304a_10,p,62,1.126).
atm(test,d304,d304a_11,o,40,-0.534).
atm(test,d304,d304a_12,c,10,0.017).
atm(test,d304,d304a_13,h,3,0.067).
atm(test,d304,d304a_14,h,3,0.067).
atm(test,d304,d304a_15,h,3,0.067).
atm(test,d304,d304a_16,c,10,0.017).
atm(test,d304,d304a_17,h,3,0.067).
atm(test,d304,d304a_18,h,3,0.067).
atm(test,d304,d304a_19,h,3,0.067).
atm(test,d304,d304a_20,c,10,0.017).
atm(test,d304,d304a_21,h,3,0.067).
atm(test,d304,d304a_22,h,3,0.067).
atm(test,d304,d304a_23,h,3,0.067).
atm(test,d304,d304a_24,o,45,-0.634).
atm(test,d304,d304a_25,o,45,-0.634).
atm(test,d304,d304a_26,h,1,0.416).
atm(test,d304,d304a_27,h,1,0.416).
atm(test,d304,d304b_1,c,22,-0.164).
atm(test,d304,d304b_2,c,22,-0.163).
atm(test,d304,d304b_3,c,22,0.266).
atm(test,d304,d304b_4,c,22,-0.113).
atm(test,d304,d304b_5,c,22,-0.113).
atm(test,d304,d304b_6,c,22,-0.163).
atm(test,d304,d304b_7,h,3,0.147).
atm(test,d304,d304b_8,o,49,-0.654).
atm(test,d304,d304b_9,p,62,1.126).
atm(test,d304,d304b_10,o,40,-0.534).
atm(test,d304,d304b_11,c,10,0.017).
atm(test,d304,d304b_12,h,3,0.067).
atm(test,d304,d304b_13,h,3,0.067).
atm(test,d304,d304b_14,h,3,0.067).
atm(test,d304,d304b_15,c,10,0.017).
atm(test,d304,d304b_16,h,3,0.067).
atm(test,d304,d304b_17,h,3,0.067).
atm(test,d304,d304b_18,h,3,0.067).
atm(test,d304,d304b_19,o,45,-0.634).
atm(test,d304,d304b_20,o,45,-0.634).
atm(test,d304,d304b_21,h,1,0.416).
atm(test,d304,d304b_22,h,1,0.416).
atm(test,d304,d304b_23,c,10,0.017).
atm(test,d304,d304b_24,h,3,0.067).
atm(test,d304,d304b_25,h,3,0.067).
atm(test,d304,d304b_26,h,3,0.067).
atm(test,d304,d304b_27,h,3,0.147).
atm(test,d305,d305_1,c,22,-0.111).
atm(test,d305,d305_2,c,22,-0.111).
atm(test,d305,d305_3,c,22,-0.111).
atm(test,d305,d305_4,c,22,-0.111).
atm(test,d305,d305_5,c,22,0.268).
atm(test,d305,d305_6,c,22,-0.111).
atm(test,d305,d305_7,h,3,0.149).
atm(test,d305,d305_8,h,3,0.149).
atm(test,d305,d305_9,h,3,0.149).
atm(test,d305,d305_10,c,10,0.019).
atm(test,d305,d305_11,c,22,-0.111).
atm(test,d305,d305_12,h,3,0.069).
atm(test,d305,d305_13,h,3,0.069).
atm(test,d305,d305_14,c,22,-0.111).
atm(test,d305,d305_15,c,22,-0.111).
atm(test,d305,d305_16,c,22,-0.111).
atm(test,d305,d305_17,c,22,-0.111).
atm(test,d305,d305_18,c,22,-0.111).
atm(test,d305,d305_19,h,3,0.149).
atm(test,d305,d305_20,h,3,0.149).
atm(test,d305,d305_21,h,3,0.149).
atm(test,d305,d305_22,h,3,0.149).
atm(test,d305,d305_23,h,3,0.149).
atm(test,d305,d305_24,o,45,-0.632).
atm(test,d305,d305_25,cl,93,-0.182).
atm(test,d305,d305_26,h,8,0.418).
atm(test,d306,d306_1,o,45,-0.619).
atm(test,d306,d306_2,c,10,0.201).
atm(test,d306,d306_3,c,10,-0.269).
atm(test,d306,d306_4,h,3,0.072).
atm(test,d306,d306_5,h,3,0.072).
atm(test,d306,d306_6,c,10,-0.068).
atm(test,d306,d306_7,br,94,-0.119).
atm(test,d306,d306_8,h,3,0.082).
atm(test,d306,d306_9,h,3,0.082).
atm(test,d306,d306_10,c,10,0.201).
atm(test,d306,d306_11,o,45,-0.619).
atm(test,d306,d306_12,h,3,0.072).
atm(test,d306,d306_13,h,3,0.072).
atm(test,d306,d306_14,c,10,-0.068).
atm(test,d306,d306_15,br,94,-0.118).
atm(test,d306,d306_16,h,3,0.082).
atm(test,d306,d306_17,h,3,0.082).
atm(test,d306,d306_18,h,8,0.431).
atm(test,d306,d306_19,h,8,0.431).
atm(test,d307,d307_1,c,10,0.166).
atm(test,d307,d307_2,o,45,-0.654).
atm(test,d307,d307_3,c,10,-0.105).
atm(test,d307,d307_4,h,3,0.045).
atm(test,d307,d307_5,h,3,0.045).
atm(test,d307,d307_6,h,3,0.045).
atm(test,d307,d307_7,c,10,-0.104).
atm(test,d307,d307_8,h,3,0.045).
atm(test,d307,d307_9,h,3,0.045).
atm(test,d307,d307_10,h,3,0.045).
atm(test,d307,d307_11,c,10,-0.104).
atm(test,d307,d307_12,h,3,0.045).
atm(test,d307,d307_13,h,3,0.045).
atm(test,d307,d307_14,h,3,0.045).
atm(test,d307,d307_15,h,8,0.396).
atm(test,d308,d308_1,c,22,-0.13).
atm(test,d308,d308_2,c,22,-0.13).
atm(test,d308,d308_3,c,22,0.0).
atm(test,d308,d308_4,c,22,0.25).
atm(test,d308,d308_5,c,22,-0.13).
atm(test,d308,d308_6,c,22,-0.13).
atm(test,d308,d308_7,h,3,0.13).
atm(test,d308,d308_8,h,3,0.13).
atm(test,d308,d308_9,h,3,0.13).
atm(test,d308,d308_10,h,3,0.13).
atm(test,d308,d308_11,c,10,-0.1).
atm(test,d308,d308_12,c,10,-0.1).
atm(test,d308,d308_13,c,14,0.7).
atm(test,d308,d308_14,o,49,-0.4).
atm(test,d308,d308_15,h,3,0.05).
atm(test,d308,d308_16,h,3,0.05).
atm(test,d308,d308_17,h,3,0.05).
atm(test,d308,d308_18,h,3,0.05).
atm(test,d308,d308_19,o,51,-0.55).
atm(test,d309,d309_1,c,10,0.17).
atm(test,d309,d309_2,c,10,0.17).
atm(test,d309,d309_3,o,45,-0.65).
atm(test,d309,d309_4,h,3,0.04).
atm(test,d309,d309_5,h,3,0.04).
atm(test,d309,d309_6,o,45,-0.65).
atm(test,d309,d309_7,h,3,0.04).
atm(test,d309,d309_8,h,3,0.04).
atm(test,d309,d309_9,h,8,0.4).
atm(test,d309,d309_10,h,8,0.4).
atm(test,d311,d311_1,c,22,-0.133).
atm(test,d311,d311_2,c,22,-0.133).
atm(test,d311,d311_3,c,22,-0.133).
atm(test,d311,d311_4,c,22,-0.133).
atm(test,d311,d311_5,c,22,-0.133).
atm(test,d311,d311_6,c,22,-0.133).
atm(test,d311,d311_7,h,3,0.127).
atm(test,d311,d311_8,h,3,0.127).
atm(test,d311,d311_9,h,3,0.127).
atm(test,d311,d311_10,h,3,0.127).
atm(test,d311,d311_11,h,3,0.127).
atm(test,d311,d311_12,c,10,-0.003).
atm(test,d311,d311_13,c,10,-0.103).
atm(test,d311,d311_14,c,10,-0.103).
atm(test,d311,d311_15,c,10,-0.103).
atm(test,d311,d311_16,c,10,-0.003).
atm(test,d311,d311_17,n,36,-0.303).
atm(test,d311,d311_18,c,10,0.047).
atm(test,d311,d311_19,h,3,0.047).
atm(test,d311,d311_20,h,3,0.047).
atm(test,d311,d311_21,h,3,0.047).
atm(test,d311,d311_22,h,3,0.047).
atm(test,d311,d311_23,h,3,0.047).
atm(test,d311,d311_24,h,3,0.047).
atm(test,d311,d311_25,h,3,0.047).
atm(test,d311,d311_26,h,3,0.047).
atm(test,d311,d311_27,h,3,0.097).
atm(test,d311,d311_28,c,14,0.696).
atm(test,d311,d311_29,h,3,0.097).
atm(test,d311,d311_30,o,49,-0.654).
atm(test,d311,d311_31,c,10,0.237).
atm(test,d311,d311_32,h,3,0.097).
atm(test,d311,d311_33,h,3,0.097).
atm(test,d311,d311_34,h,3,0.097).
atm(test,d311,d311_35,o,51,-0.553).
atm(test,d311,d311_36,h,1,0.147).
atm(test,d312,d312_1,c,14,0.631).
atm(test,d312,d312_2,n,32,-0.369).
atm(test,d312,d312_3,c,21,0.03).
atm(test,d312,d312_4,c,21,0.13).
atm(test,d312,d312_5,c,14,0.581).
atm(test,d312,d312_6,n,32,-0.369).
atm(test,d312,d312_7,n,34,-0.369).
atm(test,d312,d312_8,c,21,0.231).
atm(test,d312,d312_9,n,34,-0.509).
atm(test,d312,d312_10,h,3,0.13).
atm(test,d312,d312_11,o,40,-0.519).
atm(test,d312,d312_12,c,10,-0.12).
atm(test,d312,d312_13,h,3,0.08).
atm(test,d312,d312_14,h,3,0.08).
atm(test,d312,d312_15,h,3,0.08).
atm(test,d312,d312_16,o,40,-0.519).
atm(test,d312,d312_17,c,10,0.03).
atm(test,d312,d312_18,h,3,0.08).
atm(test,d312,d312_19,h,3,0.08).
atm(test,d312,d312_20,h,3,0.08).
atm(test,d312,d312_21,h,1,0.531).
atm(test,d313,d313_1,c,22,0.26).
atm(test,d313,d313_2,c,22,-0.12).
atm(test,d313,d313_3,c,22,-0.17).
atm(test,d313,d313_4,c,22,-0.12).
atm(test,d313,d313_5,c,22,-0.12).
atm(test,d313,d313_6,c,22,-0.12).
atm(test,d313,d313_7,h,3,0.14).
atm(test,d313,d313_8,h,3,0.14).
atm(test,d313,d313_9,c,22,-0.12).
atm(test,d313,d313_10,c,22,-0.17).
atm(test,d313,d313_11,c,22,-0.12).
atm(test,d313,d313_12,c,22,0.26).
atm(test,d313,d313_13,c,22,-0.12).
atm(test,d313,d313_14,c,22,-0.12).
atm(test,d313,d313_15,h,3,0.14).
atm(test,d313,d313_16,h,3,0.14).
atm(test,d313,d313_17,s,74,-0.09).
atm(test,d313,d313_18,c,10,-0.09).
atm(test,d313,d313_19,c,10,-0.09).
atm(test,d313,d313_20,o,45,-0.64).
atm(test,d313,d313_21,c,10,0.011).
atm(test,d313,d313_22,h,3,0.061).
atm(test,d313,d313_23,h,3,0.061).
atm(test,d313,d313_24,h,3,0.061).
atm(test,d313,d313_25,h,8,0.41).
atm(test,d313,d313_26,c,10,-0.089).
atm(test,d313,d313_27,c,10,-0.09).
atm(test,d313,d313_28,o,45,-0.64).
atm(test,d313,d313_29,h,8,0.41).
atm(test,d313,d313_30,c,10,0.011).
atm(test,d313,d313_31,h,3,0.061).
atm(test,d313,d313_32,h,3,0.061).
atm(test,d313,d313_33,h,3,0.061).
atm(test,d313,d313_34,c,10,-0.089).
atm(test,d313,d313_35,h,3,0.061).
atm(test,d313,d313_36,h,3,0.061).
atm(test,d313,d313_37,h,3,0.061).
atm(test,d313,d313_38,c,10,-0.089).
atm(test,d313,d313_39,h,3,0.061).
atm(test,d313,d313_40,h,3,0.061).
atm(test,d313,d313_41,h,3,0.061).
atm(test,d313,d313_42,c,10,-0.089).
atm(test,d313,d313_43,h,3,0.061).
atm(test,d313,d313_44,h,3,0.061).
atm(test,d313,d313_45,h,3,0.061).
atm(test,d313,d313_46,c,10,-0.09).
atm(test,d313,d313_47,h,3,0.061).
atm(test,d313,d313_48,h,3,0.061).
atm(test,d313,d313_49,h,3,0.061).
atm(test,d313,d313_50,h,3,0.061).
atm(test,d313,d313_51,h,3,0.061).
atm(test,d313,d313_52,h,3,0.061).
atm(test,d313,d313_53,h,3,0.061).
atm(test,d313,d313_54,h,3,0.061).
atm(test,d313,d313_55,h,3,0.061).
atm(test,d314,d314_1,c,22,0.881).
atm(test,d314,d314_2,n,35,-0.759).
atm(test,d314,d314_3,c,27,0.401).
atm(test,d314,d314_4,c,27,-0.088).
atm(test,d314,d314_5,c,22,0.781).
atm(test,d314,d314_6,n,35,-0.759).
atm(test,d314,d314_7,n,35,-0.759).
atm(test,d314,d314_8,c,22,0.781).
atm(test,d314,d314_9,c,29,0.012).
atm(test,d314,d314_10,n,35,-0.759).
atm(test,d314,d314_11,c,29,0.012).
atm(test,d314,d314_12,c,22,-0.118).
atm(test,d314,d314_13,c,22,-0.118).
atm(test,d314,d314_14,c,22,-0.118).
atm(test,d314,d314_15,c,22,-0.118).
atm(test,d314,d314_16,c,22,-0.118).
atm(test,d314,d314_17,h,3,0.142).
atm(test,d314,d314_18,h,3,0.142).
atm(test,d314,d314_19,h,3,0.142).
atm(test,d314,d314_20,h,3,0.142).
atm(test,d314,d314_21,h,3,0.142).
atm(test,d314,d314_22,n,32,-0.759).
atm(test,d314,d314_23,n,32,-0.769).
atm(test,d314,d314_24,n,32,-0.388).
atm(test,d314,d314_25,h,1,0.342).
atm(test,d314,d314_26,h,1,0.342).
atm(test,d314,d314_27,h,1,0.342).
atm(test,d314,d314_28,h,1,0.342).
atm(test,d314,d314_29,h,1,0.342).
atm(test,d314,d314_30,h,1,0.342).
atm(test,d315,d315_1,c,22,-0.117).
atm(test,d315,d315_2,c,22,-0.117).
atm(test,d315,d315_3,c,22,-0.117).
atm(test,d315,d315_4,c,22,-0.117).
atm(test,d315,d315_5,c,22,-0.117).
atm(test,d315,d315_6,c,22,-0.117).
atm(test,d315,d315_7,h,3,0.143).
atm(test,d315,d315_8,h,3,0.143).
atm(test,d315,d315_9,h,3,0.143).
atm(test,d315,d315_10,h,3,0.143).
atm(test,d315,d315_11,h,3,0.143).
atm(test,d315,d315_12,c,22,-0.117).
atm(test,d315,d315_13,c,22,-0.117).
atm(test,d315,d315_14,c,22,-0.117).
atm(test,d315,d315_15,c,22,-0.117).
atm(test,d315,d315_16,c,22,-0.117).
atm(test,d315,d315_17,c,22,-0.117).
atm(test,d315,d315_18,h,3,0.143).
atm(test,d315,d315_19,h,3,0.143).
atm(test,d315,d315_20,h,3,0.143).
atm(test,d315,d315_21,h,3,0.143).
atm(test,d315,d315_22,h,3,0.143).
atm(test,d315,d315_23,c,10,0.013).
atm(test,d315,d315_24,n,32,-0.386).
atm(test,d315,d315_25,c,14,0.614).
atm(test,d315,d315_26,n,32,-0.386).
atm(test,d315,d315_27,c,14,0.614).
atm(test,d315,d315_28,o,40,-0.536).
atm(test,d315,d315_29,o,40,-0.536).
atm(test,d315,d315_30,h,1,0.314).
atm(test,d315,d315_31,h,1,0.263).
atm(test,d316,d316_1,c,22,-0.039).
atm(test,d316,d316_2,c,22,-0.039).
atm(test,d316,d316_3,c,22,-0.039).
atm(test,d316,d316_4,c,22,-0.039).
atm(test,d316,d316_5,c,22,0.272).
atm(test,d316,d316_6,c,22,-0.039).
atm(test,d316,d316_7,o,50,-0.139).
atm(test,d316,d316_8,c,10,0.182).
atm(test,d316,d316_9,h,3,0.142).
atm(test,d316,d316_10,h,3,0.141).
atm(test,d316,d316_11,h,3,0.142).
atm(test,d316,d316_12,cl,93,-0.109).
atm(test,d316,d316_13,cl,93,-0.109).
atm(test,d316,d316_14,cl,93,-0.109).
atm(test,d316,d316_15,cl,93,-0.109).
atm(test,d316,d316_16,cl,93,-0.109).
atm(test,d317,d317_1,n,36,-0.25).
atm(test,d317,d317_2,cl,93,-0.15).
atm(test,d317,d317_3,h,1,0.2).
atm(test,d317,d317_4,h,1,0.2).
atm(test,d318,d318_1,c,22,0.23).
atm(test,d318,d318_2,c,22,-0.1).
atm(test,d318,d318_3,c,22,-0.1).
atm(test,d318,d318_4,c,22,0.13).
atm(test,d318,d318_5,c,22,-0.02).
atm(test,d318,d318_6,c,22,-0.1).
atm(test,d318,d318_7,h,3,0.16).
atm(test,d318,d318_8,h,3,0.16).
atm(test,d318,d318_9,h,3,0.16).
atm(test,d318,d318_10,c,16,-0.17).
atm(test,d318,d318_11,c,16,-0.17).
atm(test,d318,d318_12,h,3,0.13).
atm(test,d318,d318_13,c,22,0.13).
atm(test,d318,d318_14,h,3,0.13).
atm(test,d318,d318_15,c,22,-0.1).
atm(test,d318,d318_16,c,22,-0.1).
atm(test,d318,d318_17,c,22,0.23).
atm(test,d318,d318_18,c,22,-0.1).
atm(test,d318,d318_19,c,22,-0.02).
atm(test,d318,d318_20,h,3,0.16).
atm(test,d318,d318_21,h,3,0.16).
atm(test,d318,d318_22,h,3,0.16).
atm(test,d318,d318_23,n,32,-0.75).
atm(test,d318,d318_24,s,78,0.78).
atm(test,d318,d318_25,n,32,-0.75).
atm(test,d318,d318_26,s,78,0.78).
atm(test,d318,d318_27,h,1,0.36).
atm(test,d318,d318_28,h,1,0.36).
atm(test,d318,d318_29,h,1,0.36).
atm(test,d318,d318_30,h,1,0.36).
atm(test,d318,d318_31,o,45,-0.62).
atm(test,d318,d318_32,h,1,0.43).
atm(test,d318,d318_33,o,45,-0.62).
atm(test,d318,d318_34,o,40,-0.52).
atm(test,d318,d318_35,o,40,-0.52).
atm(test,d318,d318_36,o,40,-0.52).
atm(test,d318,d318_37,o,40,-0.52).
atm(test,d318,d318_38,h,1,0.43).
atm(test,d319,d319_1,c,10,0.042).
atm(test,d319,d319_2,br,94,-0.198).
atm(test,d319,d319_3,h,3,0.052).
atm(test,d319,d319_4,h,3,0.052).
atm(test,d319,d319_5,h,3,0.052).
atm(test,d320,d320_1,c,22,-0.108).
atm(test,d320,d320_2,c,22,-0.108).
atm(test,d320,d320_3,c,22,-0.108).
atm(test,d320,d320_4,c,22,-0.108).
atm(test,d320,d320_5,c,22,0.022).
atm(test,d320,d320_6,c,22,-0.108).
atm(test,d320,d320_7,h,3,0.152).
atm(test,d320,d320_8,h,3,0.152).
atm(test,d320,d320_9,h,3,0.152).
atm(test,d320,d320_10,h,3,0.152).
atm(test,d320,d320_11,c,14,0.803).
atm(test,d320,d320_12,o,45,-0.647).
atm(test,d320,d320_13,n,38,0.823).
atm(test,d320,d320_14,o,51,-0.587).
atm(test,d320,d320_15,o,40,-0.377).
atm(test,d320,d320_16,o,40,-0.377).
atm(test,d320,d320_17,h,1,0.272).
atm(test,d322,d322_1,c,27,0.108).
atm(test,d322,d322_2,c,27,-0.092).
atm(test,d322,d322_3,c,22,-0.122).
atm(test,d322,d322_4,c,29,0.008).
atm(test,d322,d322_5,c,22,-0.122).
atm(test,d322,d322_6,c,22,-0.122).
atm(test,d322,d322_7,c,29,0.008).
atm(test,d322,d322_8,c,22,-0.122).
atm(test,d322,d322_9,c,22,-0.122).
atm(test,d322,d322_10,c,22,0.208).
atm(test,d322,d322_11,c,22,0.188).
atm(test,d322,d322_12,c,22,-0.122).
atm(test,d322,d322_13,o,50,-0.222).
atm(test,d322,d322_14,cu,96,2.007).
atm(test,d322,d322_15,n,32,-0.393).
atm(test,d322,d322_16,n,32,-0.393).
atm(test,d322,d322_17,c,22,0.258).
atm(test,d322,d322_18,c,22,0.208).
atm(test,d322,d322_19,c,22,-0.042).
atm(test,d322,d322_20,c,22,-0.122).
atm(test,d322,d322_21,c,27,0.008).
atm(test,d322,d322_22,c,27,0.008).
atm(test,d322,d322_23,o,50,-0.222).
atm(test,d322,d322_24,c,22,-0.122).
atm(test,d322,d322_25,c,22,-0.042).
atm(test,d322,d322_26,c,22,-0.122).
atm(test,d322,d322_27,c,22,-0.122).
atm(test,d322,d322_28,n,32,-0.773).
atm(test,d322,d322_29,s,78,0.757).
atm(test,d322,d322_30,s,78,0.757).
atm(test,d322,d322_31,o,45,-0.643).
atm(test,d322,d322_32,o,45,-0.643).
atm(test,d322,d322_33,o,40,-0.543).
atm(test,d322,d322_34,o,40,-0.543).
atm(test,d322,d322_35,o,40,-0.543).
atm(test,d322,d322_36,o,40,-0.543).
atm(test,d322,d322_37,o,53,-0.292).
atm(test,d322,d322_38,n,35,-0.393).
atm(test,d322,d322_39,cu,96,2.007).
atm(test,d322,d322_40,n,34,-0.393).
atm(test,d322,d322_41,c,27,0.008).
atm(test,d322,d322_42,c,22,-0.122).
atm(test,d322,d322_43,c,22,-0.042).
atm(test,d322,d322_44,c,29,0.008).
atm(test,d322,d322_45,c,22,0.258).
atm(test,d322,d322_46,c,27,0.008).
atm(test,d322,d322_47,o,50,-0.222).
atm(test,d322,d322_48,s,78,0.757).
atm(test,d322,d322_49,o,45,-0.643).
atm(test,d322,d322_50,c,22,-0.122).
atm(test,d322,d322_51,c,22,-0.122).
atm(test,d322,d322_52,c,22,-0.042).
atm(test,d322,d322_53,c,22,-0.122).
atm(test,d322,d322_54,n,32,-0.773).
atm(test,d322,d322_55,s,78,0.757).
atm(test,d322,d322_56,o,45,-0.643).
atm(test,d322,d322_57,o,40,-0.543).
atm(test,d322,d322_58,o,40,-0.543).
atm(test,d322,d322_59,o,40,-0.543).
atm(test,d322,d322_60,o,40,-0.543).
atm(test,d322,d322_61,h,3,0.138).
atm(test,d322,d322_62,h,3,0.138).
atm(test,d322,d322_63,h,3,0.138).
atm(test,d322,d322_64,h,3,0.138).
atm(test,d322,d322_65,h,3,0.138).
atm(test,d322,d322_66,h,3,0.138).
atm(test,d322,d322_67,h,3,0.138).
atm(test,d322,d322_68,h,3,0.138).
atm(test,d322,d322_69,h,3,0.138).
atm(test,d322,d322_70,h,1,0.338).
atm(test,d322,d322_71,h,1,0.338).
atm(test,d322,d322_72,h,1,0.407).
atm(test,d322,d322_73,h,1,0.407).
atm(test,d322,d322_74,h,3,0.138).
atm(test,d322,d322_75,h,1,0.407).
atm(test,d322,d322_76,h,3,0.138).
atm(test,d322,d322_77,h,3,0.138).
atm(test,d322,d322_78,h,1,0.337).
atm(test,d322,d322_79,h,1,0.337).
atm(test,d322,d322_80,h,1,0.407).
atm(test,d323,d323_1,c,22,-0.155).
atm(test,d323,d323_2,c,22,-0.105).
atm(test,d323,d323_3,c,22,-0.105).
atm(test,d323,d323_4,c,22,0.224).
atm(test,d323,d323_5,c,22,-0.105).
atm(test,d323,d323_6,c,22,-0.105).
atm(test,d323,d323_7,h,3,0.155).
atm(test,d323,d323_8,h,3,0.155).
atm(test,d323,d323_9,h,3,0.154).
atm(test,d323,d323_10,n,32,-0.376).
atm(test,d323,d323_11,n,32,-0.376).
atm(test,d323,d323_12,c,22,-0.105).
atm(test,d323,d323_13,c,27,0.025).
atm(test,d323,d323_14,c,27,0.025).
atm(test,d323,d323_15,c,22,-0.105).
atm(test,d323,d323_16,c,22,-0.105).
atm(test,d323,d323_17,c,22,0.274).
atm(test,d323,d323_18,h,3,0.155).
atm(test,d323,d323_19,h,3,0.155).
atm(test,d323,d323_20,c,22,-0.105).
atm(test,d323,d323_21,c,22,-0.105).
atm(test,d323,d323_22,c,22,-0.105).
atm(test,d323,d323_23,c,22,-0.105).
atm(test,d323,d323_24,h,3,0.155).
atm(test,d323,d323_25,h,3,0.155).
atm(test,d323,d323_26,h,3,0.155).
atm(test,d323,d323_27,h,3,0.155).
atm(test,d323,d323_28,c,10,0.025).
atm(test,d323,d323_29,h,3,0.075).
atm(test,d323,d323_30,h,3,0.075).
atm(test,d323,d323_31,h,3,0.075).
atm(test,d323,d323_32,o,45,-0.626).
atm(test,d323,d323_33,n,38,0.824).
atm(test,d323,d323_34,h,8,0.424).
atm(test,d323,d323_35,o,40,-0.376).
atm(test,d323,d323_36,o,40,-0.376).
atm(test,d324,d324_1,c,22,-0.115).
atm(test,d324,d324_2,c,22,-0.115).
atm(test,d324,d324_3,c,27,0.015).
atm(test,d324,d324_4,c,27,0.015).
atm(test,d324,d324_5,c,22,-0.115).
atm(test,d324,d324_6,c,22,-0.115).
atm(test,d324,d324_7,h,3,0.145).
atm(test,d324,d324_8,h,3,0.145).
atm(test,d324,d324_9,h,3,0.145).
atm(test,d324,d324_10,h,3,0.145).
atm(test,d324,d324_11,c,22,-0.115).
atm(test,d324,d324_12,c,22,0.015).
atm(test,d324,d324_13,c,22,0.265).
atm(test,d324,d324_14,c,22,-0.115).
atm(test,d324,d324_15,h,3,0.145).
atm(test,d324,d324_16,c,22,-0.115).
atm(test,d324,d324_17,c,22,-0.115).
atm(test,d324,d324_18,c,22,-0.115).
atm(test,d324,d324_19,c,22,-0.115).
atm(test,d324,d324_20,c,22,0.215).
atm(test,d324,d324_21,c,22,-0.115).
atm(test,d324,d324_22,h,3,0.145).
atm(test,d324,d324_23,h,3,0.145).
atm(test,d324,d324_24,h,3,0.145).
atm(test,d324,d324_25,h,3,0.145).
atm(test,d324,d324_26,c,14,0.566).
atm(test,d324,d324_27,n,32,-0.435).
atm(test,d324,d324_28,h,1,0.265).
atm(test,d324,d324_29,o,40,-0.534).
atm(test,d324,d324_30,n,38,0.816).
atm(test,d324,d324_31,o,40,-0.385).
atm(test,d324,d324_32,o,40,-0.385).
atm(test,d324,d324_33,o,45,-0.634).
atm(test,d324,d324_34,h,8,0.415).
atm(test,d324,d324_35,n,32,-0.385).
atm(test,d324,d324_36,n,32,-0.385).
atm(test,d324,d324_37,c,22,0.215).
atm(test,d324,d324_38,c,22,-0.115).
atm(test,d324,d324_39,c,22,-0.115).
atm(test,d324,d324_40,c,22,-0.115).
atm(test,d324,d324_41,c,22,-0.115).
atm(test,d324,d324_42,c,22,0.195).
atm(test,d324,d324_43,h,3,0.145).
atm(test,d324,d324_44,h,3,0.145).
atm(test,d324,d324_45,h,3,0.145).
atm(test,d324,d324_46,o,50,-0.215).
atm(test,d324,d324_47,c,10,0.105).
atm(test,d324,d324_48,h,3,0.065).
atm(test,d324,d324_49,h,3,0.065).
atm(test,d324,d324_50,h,3,0.065).
atm(test,d324,d324_51,n,38,0.816).
atm(test,d324,d324_52,o,40,-0.385).
atm(test,d324,d324_53,o,40,-0.385).
atm(test,d325,d325_1,c,22,-0.139).
atm(test,d325,d325_2,c,22,0.191).
atm(test,d325,d325_3,c,22,-0.139).
atm(test,d325,d325_4,c,22,0.191).
atm(test,d325,d325_5,c,22,0.241).
atm(test,d325,d325_6,c,22,-0.139).
atm(test,d325,d325_7,h,3,0.121).
atm(test,d325,d325_8,h,3,0.121).
atm(test,d325,d325_9,h,3,0.121).
atm(test,d325,d325_10,o,45,-0.66).
atm(test,d325,d325_11,n,32,-0.79).
atm(test,d325,d325_12,n,32,-0.79).
atm(test,d325,d325_13,h,1,0.321).
atm(test,d325,d325_14,h,1,0.32).
atm(test,d325,d325_15,h,1,0.32).
atm(test,d325,d325_16,h,1,0.32).
atm(test,d325,d325_17,h,8,0.39).
atm(test,d326,d326_1,c,22,-0.135).
atm(test,d326,d326_2,c,22,0.195).
atm(test,d326,d326_3,c,22,-0.135).
atm(test,d326,d326_4,c,22,-0.135).
atm(test,d326,d326_5,c,22,0.245).
atm(test,d326,d326_6,c,22,-0.135).
atm(test,d326,d326_7,h,3,0.125).
atm(test,d326,d326_8,h,3,0.125).
atm(test,d326,d326_9,h,3,0.125).
atm(test,d326,d326_10,h,3,0.125).
atm(test,d326,d326_11,o,45,-0.655).
atm(test,d326,d326_12,n,32,-0.405).
atm(test,d326,d326_13,c,14,0.595).
atm(test,d326,d326_14,h,8,0.395).
atm(test,d326,d326_15,h,1,0.245).
atm(test,d326,d326_16,o,40,-0.555).
atm(test,d326,d326_17,c,10,-0.155).
atm(test,d326,d326_18,h,3,0.045).
atm(test,d326,d326_19,h,3,0.045).
atm(test,d326,d326_20,h,3,0.045).
atm(test,d327,d327_1,c,22,0.952).
atm(test,d327,d327_2,n,35,-0.758).
atm(test,d327,d327_3,c,22,0.262).
atm(test,d327,d327_4,c,22,-0.119).
atm(test,d327,d327_5,c,22,-0.119).
atm(test,d327,d327_6,c,22,-0.119).
atm(test,d327,d327_7,h,3,0.141).
atm(test,d327,d327_8,h,3,0.141).
atm(test,d327,d327_9,h,3,0.141).
atm(test,d327,d327_10,h,3,0.141).
atm(test,d327,d327_11,n,32,-0.388).
atm(test,d327,d327_12,s,77,0.362).
atm(test,d327,d327_13,c,22,-0.119).
atm(test,d327,d327_14,c,22,-0.119).
atm(test,d327,d327_15,c,22,-0.119).
atm(test,d327,d327_16,c,22,0.211).
atm(test,d327,d327_17,c,22,-0.119).
atm(test,d327,d327_18,c,22,-0.119).
atm(test,d327,d327_19,h,3,0.141).
atm(test,d327,d327_20,h,3,0.141).
atm(test,d327,d327_21,h,3,0.141).
atm(test,d327,d327_22,h,3,0.141).
atm(test,d327,d327_23,n,32,-0.388).
atm(test,d327,d327_24,n,32,-0.388).
atm(test,d327,d327_25,c,22,0.212).
atm(test,d327,d327_26,c,22,-0.119).
atm(test,d327,d327_27,c,22,-0.119).
atm(test,d327,d327_28,c,22,0.262).
atm(test,d327,d327_29,c,22,0.011).
atm(test,d327,d327_30,c,22,-0.119).
atm(test,d327,d327_31,h,3,0.141).
atm(test,d327,d327_32,h,3,0.141).
atm(test,d327,d327_33,h,3,0.141).
atm(test,d327,d327_34,c,14,0.792).
atm(test,d327,d327_35,o,45,-0.658).
atm(test,d327,d327_36,o,51,-0.598).
atm(test,d327,d327_37,o,45,-0.638).
atm(test,d327,d327_38,h,1,0.312).
atm(test,d327,d327_39,o,40,-0.238).
atm(test,d327,d327_40,o,40,-0.238).
atm(test,d327,d327_41,h,1,0.262).
atm(test,d327,d327_42,h,8,0.412).
atm(test,d328,d328a_1,c,22,-0.174).
atm(test,d328,d328a_2,c,22,-0.174).
atm(test,d328,d328a_3,c,22,-0.174).
atm(test,d328,d328a_4,c,22,-0.174).
atm(test,d328,d328a_5,c,22,-0.174).
atm(test,d328,d328a_6,h,3,0.087).
atm(test,d328,d328a_7,h,3,0.087).
atm(test,d328,d328a_8,h,3,0.087).
atm(test,d328,d328a_9,h,3,0.087).
atm(test,d328,d328a_10,h,3,0.087).
atm(test,d328,d328a_11,h,3,0.087).
atm(test,d328,d328a_12,h,3,0.087).
atm(test,d328,d328a_13,h,3,0.087).
atm(test,d328,d328a_14,h,3,0.087).
atm(test,d328,d328a_15,h,3,0.087).
atm(test,d328,d328b_1,cl,93,-0.4).
atm(test,d328,d328b_2,ti,134,0.8).
atm(test,d328,d328b_3,cl,93,-0.4).
atm(test,d329,d329_1,c,22,-0.102).
atm(test,d329,d329_2,c,22,-0.102).
atm(test,d329,d329_3,c,27,0.028).
atm(test,d329,d329_4,c,27,0.028).
atm(test,d329,d329_5,c,22,-0.102).
atm(test,d329,d329_6,c,22,0.279).
atm(test,d329,d329_7,h,3,0.159).
atm(test,d329,d329_8,h,3,0.158).
atm(test,d329,d329_9,c,22,-0.102).
atm(test,d329,d329_10,c,22,-0.022).
atm(test,d329,d329_11,c,22,-0.102).
atm(test,d329,d329_12,c,22,-0.022).
atm(test,d329,d329_13,h,3,0.159).
atm(test,d329,d329_14,h,3,0.158).
atm(test,d329,d329_15,o,45,-0.621).
atm(test,d329,d329_16,h,8,0.429).
atm(test,d329,d329_17,s,78,0.779).
atm(test,d329,d329_18,o,45,-0.621).
atm(test,d329,d329_19,s,78,0.779).
atm(test,d329,d329_20,o,45,-0.621).
atm(test,d329,d329_21,o,40,-0.521).
atm(test,d329,d329_22,o,40,-0.521).
atm(test,d329,d329_23,o,40,-0.521).
atm(test,d329,d329_24,o,40,-0.521).
atm(test,d329,d329_25,h,1,0.429).
atm(test,d329,d329_26,h,1,0.429).
atm(test,d329,d329_27,n,32,-0.371).
atm(test,d329,d329_28,n,32,-0.371).
atm(test,d329,d329_29,c,22,0.229).
atm(test,d329,d329_30,c,22,-0.102).
atm(test,d329,d329_31,c,22,-0.102).
atm(test,d329,d329_32,c,29,0.028).
atm(test,d329,d329_33,c,22,-0.102).
atm(test,d329,d329_34,c,22,-0.152).
atm(test,d329,d329_35,h,3,0.159).
atm(test,d329,d329_36,h,3,0.159).
atm(test,d329,d329_37,h,3,0.158).
atm(test,d329,d329_38,c,29,0.028).
atm(test,d329,d329_39,c,22,-0.102).
atm(test,d329,d329_40,c,22,-0.102).
atm(test,d329,d329_41,c,22,0.229).
atm(test,d329,d329_42,c,22,-0.152).
atm(test,d329,d329_43,c,22,-0.102).
atm(test,d329,d329_44,h,3,0.159).
atm(test,d329,d329_45,h,3,0.159).
atm(test,d329,d329_46,h,3,0.159).
atm(test,d329,d329_47,c,10,0.028).
atm(test,d329,d329_48,h,3,0.078).
atm(test,d329,d329_49,h,3,0.078).
atm(test,d329,d329_50,h,3,0.078).
atm(test,d329,d329_51,c,10,0.028).
atm(test,d329,d329_52,h,3,0.078).
atm(test,d329,d329_53,h,3,0.078).
atm(test,d329,d329_54,h,3,0.078).
atm(test,d329,d329_55,n,32,-0.371).
atm(test,d329,d329_56,n,32,-0.371).
atm(test,d329,d329_57,c,22,0.229).
atm(test,d329,d329_58,c,22,-0.102).
atm(test,d329,d329_59,c,22,-0.102).
atm(test,d329,d329_60,c,22,0.209).
atm(test,d329,d329_61,c,22,-0.102).
atm(test,d329,d329_62,c,22,-0.102).
atm(test,d329,d329_63,h,3,0.159).
atm(test,d329,d329_64,h,3,0.159).
atm(test,d329,d329_65,h,3,0.158).
atm(test,d329,d329_66,h,3,0.158).
atm(test,d329,d329_67,o,50,-0.221).
atm(test,d329,d329_68,s,78,0.779).
atm(test,d329,d329_69,c,22,-0.022).
atm(test,d329,d329_70,c,22,-0.102).
atm(test,d329,d329_71,c,22,-0.102).
atm(test,d329,d329_72,c,22,-0.152).
atm(test,d329,d329_73,c,22,-0.102).
atm(test,d329,d329_74,c,22,-0.102).
atm(test,d329,d329_75,h,3,0.159).
atm(test,d329,d329_76,h,3,0.159).
atm(test,d329,d329_77,h,3,0.159).
atm(test,d329,d329_78,h,3,0.159).
atm(test,d329,d329_79,c,10,0.028).
atm(test,d329,d329_80,h,3,0.078).
atm(test,d329,d329_81,h,3,0.078).
atm(test,d329,d329_82,h,3,0.078).
atm(test,d329,d329_83,o,40,-0.221).
atm(test,d329,d329_84,o,40,-0.221).
atm(test,d330,d330_1,c,22,0.24).
atm(test,d330,d330_2,c,22,-0.09).
atm(test,d330,d330_3,c,22,-0.09).
atm(test,d330,d330_4,c,29,0.04).
atm(test,d330,d330_5,c,22,-0.09).
atm(test,d330,d330_6,c,22,0.22).
atm(test,d330,d330_7,h,3,0.17).
atm(test,d330,d330_8,h,3,0.17).
atm(test,d330,d330_9,h,3,0.17).
atm(test,d330,d330_10,c,22,-0.09).
atm(test,d330,d330_11,c,22,-0.09).
atm(test,d330,d330_12,c,22,0.24).
atm(test,d330,d330_13,c,22,0.22).
atm(test,d330,d330_14,c,22,-0.09).
atm(test,d330,d330_15,c,29,0.04).
atm(test,d330,d330_16,h,3,0.17).
atm(test,d330,d330_17,h,3,0.17).
atm(test,d330,d330_18,h,3,0.17).
atm(test,d330,d330_19,n,32,-0.36).
atm(test,d330,d330_20,n,32,-0.36).
atm(test,d330,d330_21,c,22,0.24).
atm(test,d330,d330_22,c,22,0.29).
atm(test,d330,d330_23,c,27,0.04).
atm(test,d330,d330_24,c,27,0.04).
atm(test,d330,d330_25,c,22,-0.09).
atm(test,d330,d330_26,c,22,-0.01).
atm(test,d330,d330_27,h,3,0.17).
atm(test,d330,d330_28,c,22,-0.09).
atm(test,d330,d330_29,c,22,-0.09).
atm(test,d330,d330_30,c,22,-0.01).
atm(test,d330,d330_31,c,22,-0.09).
atm(test,d330,d330_32,h,3,0.17).
atm(test,d330,d330_33,h,3,0.17).
atm(test,d330,d330_34,s,78,0.79).
atm(test,d330,d330_35,o,45,-0.61).
atm(test,d330,d330_36,o,40,-0.51).
atm(test,d330,d330_37,o,40,-0.51).
atm(test,d330,d330_38,h,1,0.44).
atm(test,d330,d330_39,o,45,-0.61).
atm(test,d330,d330_40,h,8,0.44).
atm(test,d330,d330_41,n,32,-0.74).
atm(test,d330,d330_42,h,1,0.37).
atm(test,d330,d330_43,h,1,0.37).
atm(test,d330,d330_44,s,78,0.79).
atm(test,d330,d330_45,o,45,-0.61).
atm(test,d330,d330_46,o,40,-0.51).
atm(test,d330,d330_47,o,40,-0.51).
atm(test,d330,d330_48,h,1,0.44).
atm(test,d330,d330_49,n,32,-0.36).
atm(test,d330,d330_50,n,32,-0.36).
atm(test,d330,d330_51,c,22,0.24).
atm(test,d330,d330_52,c,22,-0.01).
atm(test,d330,d330_53,c,22,-0.09).
atm(test,d330,d330_54,c,27,0.04).
atm(test,d330,d330_55,c,27,0.04).
atm(test,d330,d330_56,c,22,0.29).
atm(test,d330,d330_57,h,3,0.17).
atm(test,d330,d330_58,c,22,-0.09).
atm(test,d330,d330_59,c,22,-0.01).
atm(test,d330,d330_60,c,22,-0.09).
atm(test,d330,d330_61,c,22,-0.09).
atm(test,d330,d330_62,h,3,0.17).
atm(test,d330,d330_63,h,3,0.17).
atm(test,d330,d330_64,s,78,0.79).
atm(test,d330,d330_65,o,40,-0.51).
atm(test,d330,d330_66,o,40,-0.51).
atm(test,d330,d330_67,o,45,-0.61).
atm(test,d330,d330_68,o,45,-0.61).
atm(test,d330,d330_69,n,32,-0.74).
atm(test,d330,d330_70,s,78,0.79).
atm(test,d330,d330_71,o,45,-0.61).
atm(test,d330,d330_72,o,40,-0.51).
atm(test,d330,d330_73,o,40,-0.51).
atm(test,d330,d330_74,h,1,0.44).
atm(test,d330,d330_75,h,1,0.44).
atm(test,d330,d330_76,h,8,0.44).
atm(test,d330,d330_77,h,1,0.37).
atm(test,d330,d330_78,h,1,0.37).
atm(test,d330,d330_79,o,50,-0.19).
atm(test,d330,d330_80,c,10,0.13).
atm(test,d330,d330_81,h,3,0.09).
atm(test,d330,d330_82,h,3,0.09).
atm(test,d330,d330_83,h,3,0.09).
atm(test,d330,d330_84,o,50,-0.19).
atm(test,d330,d330_85,c,10,0.13).
atm(test,d330,d330_86,h,3,0.09).
atm(test,d330,d330_87,h,3,0.09).
atm(test,d330,d330_88,h,3,0.09).
atm(test,d331,d331_1,c,22,-0.109).
atm(test,d331,d331_2,c,22,-0.109).
atm(test,d331,d331_3,c,22,0.121).
atm(test,d331,d331_4,c,22,0.27).
atm(test,d331,d331_5,c,22,-0.109).
atm(test,d331,d331_6,c,22,-0.109).
atm(test,d331,d331_7,h,3,0.151).
atm(test,d331,d331_8,h,3,0.15).
atm(test,d331,d331_9,h,3,0.151).
atm(test,d331,d331_10,h,3,0.151).
atm(test,d331,d331_11,c,16,-0.18).
atm(test,d331,d331_12,c,16,-0.18).
atm(test,d331,d331_13,c,14,0.72).
atm(test,d331,d331_14,o,49,-0.63).
atm(test,d331,d331_15,o,51,-0.53).
atm(test,d331,d331_16,h,3,0.121).
atm(test,d331,d331_17,h,3,0.121).
atm(test,d332,d332_1,o,45,-0.62).
atm(test,d332,d332_2,c,10,0.199).
atm(test,d332,d332_3,c,10,-0.071).
atm(test,d332,d332_4,h,3,0.069).
atm(test,d332,d332_5,h,3,0.069).
atm(test,d332,d332_6,c,10,-0.071).
atm(test,d332,d332_7,br,94,-0.121).
atm(test,d332,d332_8,h,3,0.079).
atm(test,d332,d332_9,h,3,0.079).
atm(test,d332,d332_10,br,94,-0.121).
atm(test,d332,d332_11,h,3,0.079).
atm(test,d332,d332_12,h,8,0.43).
atm(test,d333,d333_1,c,22,0.197).
atm(test,d333,d333_2,c,22,-0.133).
atm(test,d333,d333_3,c,22,-0.133).
atm(test,d333,d333_4,c,29,-0.003).
atm(test,d333,d333_5,c,22,-0.133).
atm(test,d333,d333_6,c,22,-0.183).
atm(test,d333,d333_7,h,3,0.127).
atm(test,d333,d333_8,h,3,0.127).
atm(test,d333,d333_9,h,3,0.127).
atm(test,d333,d333_10,c,29,-0.003).
atm(test,d333,d333_11,c,22,-0.133).
atm(test,d333,d333_12,c,22,-0.133).
atm(test,d333,d333_13,c,22,0.197).
atm(test,d333,d333_14,c,22,-0.183).
atm(test,d333,d333_15,c,22,-0.133).
atm(test,d333,d333_16,h,3,0.127).
atm(test,d333,d333_17,h,3,0.127).
atm(test,d333,d333_18,h,3,0.127).
atm(test,d333,d333_19,c,10,-0.003).
atm(test,d333,d333_20,h,3,0.047).
atm(test,d333,d333_21,h,3,0.047).
atm(test,d333,d333_22,h,3,0.047).
atm(test,d333,d333_23,n,32,-0.784).
atm(test,d333,d333_24,c,10,-0.003).
atm(test,d333,d333_25,h,3,0.047).
atm(test,d333,d333_26,h,3,0.047).
atm(test,d333,d333_27,h,3,0.047).
atm(test,d333,d333_28,n,32,-0.784).
atm(test,d333,d333_29,h,1,0.326).
atm(test,d333,d333_30,h,1,0.327).
atm(test,d333,d333_31,h,1,0.327).
atm(test,d333,d333_32,h,1,0.326).
atm(test,d334,d334_1,c,22,-0.126).
atm(test,d334,d334_2,c,22,-0.126).
atm(test,d334,d334_3,c,22,-0.126).
atm(test,d334,d334_4,c,22,-0.126).
atm(test,d334,d334_5,c,22,0.204).
atm(test,d334,d334_6,c,22,0.254).
atm(test,d334,d334_7,h,3,0.134).
atm(test,d334,d334_8,h,3,0.134).
atm(test,d334,d334_9,h,3,0.134).
atm(test,d334,d334_10,n,38,0.805).
atm(test,d334,d334_11,o,45,-0.645).
atm(test,d334,d334_12,n,32,-0.396).
atm(test,d334,d334_13,c,10,0.004).
atm(test,d334,d334_14,c,10,0.174).
atm(test,d334,d334_15,h,3,0.054).
atm(test,d334,d334_16,h,3,0.054).
atm(test,d334,d334_17,o,45,-0.645).
atm(test,d334,d334_18,h,3,0.044).
atm(test,d334,d334_19,h,3,0.044).
atm(test,d334,d334_20,c,10,0.004).
atm(test,d334,d334_21,c,10,0.174).
atm(test,d334,d334_22,h,3,0.054).
atm(test,d334,d334_23,h,3,0.054).
atm(test,d334,d334_24,o,45,-0.645).
atm(test,d334,d334_25,h,3,0.044).
atm(test,d334,d334_26,h,3,0.044).
atm(test,d334,d334_27,o,40,-0.396).
atm(test,d334,d334_28,o,40,-0.396).
atm(test,d334,d334_29,h,8,0.404).
atm(test,d334,d334_30,h,8,0.405).
atm(test,d334,d334_31,h,8,0.405).
atm(test,d335,d335_1,c,22,-0.127).
atm(test,d335,d335_2,c,22,-0.127).
atm(test,d335,d335_3,c,22,-0.127).
atm(test,d335,d335_4,c,22,-0.127).
atm(test,d335,d335_5,c,22,0.203).
atm(test,d335,d335_6,c,22,-0.127).
atm(test,d335,d335_7,h,3,0.133).
atm(test,d335,d335_8,h,3,0.133).
atm(test,d335,d335_9,h,3,0.133).
atm(test,d335,d335_10,h,3,0.133).
atm(test,d335,d335_11,n,32,-0.776).
atm(test,d335,d335_12,n,38,0.804).
atm(test,d335,d335_13,o,40,-0.397).
atm(test,d335,d335_14,o,40,-0.397).
atm(test,d335,d335_15,h,1,0.333).
atm(test,d335,d335_16,h,1,0.333).
atm(test,d336,d336_1,c,22,-0.133).
atm(test,d336,d336_2,c,22,-0.133).
atm(test,d336,d336_3,c,22,-0.133).
atm(test,d336,d336_4,c,22,-0.133).
atm(test,d336,d336_5,c,22,0.176).
atm(test,d336,d336_6,c,22,-0.134).
atm(test,d336,d336_7,h,3,0.127).
atm(test,d336,d336_8,h,3,0.127).
atm(test,d336,d336_9,h,3,0.127).
atm(test,d336,d336_10,h,3,0.127).
atm(test,d336,d336_11,o,50,-0.234).
atm(test,d336,d336_12,c,10,0.087).
atm(test,d336,d336_13,h,3,0.047).
atm(test,d336,d336_14,h,3,0.047).
atm(test,d336,d336_15,h,3,0.047).
atm(test,d336,d336_16,n,38,0.796).
atm(test,d336,d336_17,o,40,-0.404).
atm(test,d336,d336_18,o,40,-0.404).
atm(test,d337,d337_1,c,10,0.1).
atm(test,d337,d337_2,c,10,0.15).
atm(test,d337,d337_3,c,10,0.1).
atm(test,d337,d337_4,cl,93,-0.2).
atm(test,d337,d337_5,h,3,0.05).
atm(test,d337,d337_6,h,3,0.05).
atm(test,d337,d337_7,cl,93,-0.2).
atm(test,d337,d337_8,h,3,0.05).
atm(test,d337,d337_9,h,3,0.05).
atm(test,d337,d337_10,cl,93,-0.2).
atm(test,d337,d337_11,h,3,0.05).
bond(test,d296,d296_1,d296_2,7).
bond(test,d296,d296_2,d296_3,7).
bond(test,d296,d296_3,d296_4,7).
bond(test,d296,d296_4,d296_5,7).
bond(test,d296,d296_5,d296_6,7).
bond(test,d296,d296_6,d296_1,7).
bond(test,d296,d296_1,d296_7,1).
bond(test,d296,d296_2,d296_8,1).
bond(test,d296,d296_5,d296_9,1).
bond(test,d296,d296_6,d296_10,1).
bond(test,d296,d296_3,d296_11,7).
bond(test,d296,d296_11,d296_12,7).
bond(test,d296,d296_12,d296_13,7).
bond(test,d296,d296_13,d296_14,7).
bond(test,d296,d296_14,d296_4,7).
bond(test,d296,d296_11,d296_15,1).
bond(test,d296,d296_12,d296_16,1).
bond(test,d296,d296_13,d296_17,1).
bond(test,d296,d296_14,d296_18,1).
bond(test,d297,d297_1,d297_2,1).
bond(test,d297,d297_1,d297_3,1).
bond(test,d297,d297_3,d297_4,1).
bond(test,d297,d297_3,d297_5,1).
bond(test,d297,d297_3,d297_6,1).
bond(test,d297,d297_2,d297_7,1).
bond(test,d297,d297_7,d297_4,1).
bond(test,d297,d297_1,d297_8,1).
bond(test,d297,d297_1,d297_9,1).
bond(test,d297,d297_10,d297_11,1).
bond(test,d297,d297_11,d297_12,1).
bond(test,d297,d297_11,d297_13,1).
bond(test,d297,d297_8,d297_11,1).
bond(test,d297,d297_10,d297_14,1).
bond(test,d297,d297_10,d297_15,1).
bond(test,d297,d297_10,d297_16,1).
bond(test,d297,d297_14,d297_17,1).
bond(test,d297,d297_17,d297_18,1).
bond(test,d297,d297_17,d297_19,1).
bond(test,d297,d297_17,d297_20,1).
bond(test,d297,d297_18,d297_21,1).
bond(test,d297,d297_18,d297_22,1).
bond(test,d297,d297_18,d297_23,1).
bond(test,d297,d297_21,d297_24,1).
bond(test,d297,d297_24,d297_25,1).
bond(test,d297,d297_24,d297_26,1).
bond(test,d297,d297_24,d297_27,1).
bond(test,d297,d297_25,d297_28,1).
bond(test,d297,d297_25,d297_29,1).
bond(test,d297,d297_25,d297_30,1).
bond(test,d297,d297_28,d297_31,1).
bond(test,d297,d297_31,d297_32,1).
bond(test,d297,d297_31,d297_33,1).
bond(test,d297,d297_31,d297_34,1).
bond(test,d297,d297_32,d297_35,1).
bond(test,d297,d297_32,d297_36,1).
bond(test,d297,d297_32,d297_37,1).
bond(test,d297,d297_35,d297_38,1).
bond(test,d297,d297_38,d297_39,1).
bond(test,d297,d297_38,d297_40,1).
bond(test,d297,d297_38,d297_41,1).
bond(test,d297,d297_39,d297_42,1).
bond(test,d297,d297_39,d297_43,1).
bond(test,d297,d297_39,d297_44,1).
bond(test,d297,d297_42,d297_45,1).
bond(test,d297,d297_2,d297_46,1).
bond(test,d297,d297_2,d297_47,1).
bond(test,d297,d297_48,d297_49,1).
bond(test,d297,d297_49,d297_50,1).
bond(test,d297,d297_49,d297_51,1).
bond(test,d297,d297_48,d297_52,1).
bond(test,d297,d297_48,d297_53,1).
bond(test,d297,d297_48,d297_54,1).
bond(test,d297,d297_52,d297_55,1).
bond(test,d297,d297_55,d297_56,1).
bond(test,d297,d297_55,d297_57,1).
bond(test,d297,d297_55,d297_58,1).
bond(test,d297,d297_56,d297_59,1).
bond(test,d297,d297_56,d297_60,1).
bond(test,d297,d297_56,d297_61,1).
bond(test,d297,d297_59,d297_62,1).
bond(test,d297,d297_62,d297_63,1).
bond(test,d297,d297_62,d297_64,1).
bond(test,d297,d297_62,d297_65,1).
bond(test,d297,d297_63,d297_66,1).
bond(test,d297,d297_63,d297_67,1).
bond(test,d297,d297_63,d297_68,1).
bond(test,d297,d297_66,d297_69,1).
bond(test,d297,d297_69,d297_70,1).
bond(test,d297,d297_69,d297_71,1).
bond(test,d297,d297_69,d297_72,1).
bond(test,d297,d297_70,d297_73,1).
bond(test,d297,d297_70,d297_74,1).
bond(test,d297,d297_70,d297_75,1).
bond(test,d297,d297_73,d297_76,1).
bond(test,d297,d297_76,d297_77,1).
bond(test,d297,d297_76,d297_78,1).
bond(test,d297,d297_76,d297_79,1).
bond(test,d297,d297_46,d297_49,1).
bond(test,d297,d297_77,d297_80,1).
bond(test,d297,d297_77,d297_81,1).
bond(test,d297,d297_77,d297_82,1).
bond(test,d297,d297_80,d297_83,1).
bond(test,d297,d297_7,d297_84,1).
bond(test,d297,d297_7,d297_85,1).
bond(test,d297,d297_84,d297_86,1).
bond(test,d297,d297_84,d297_87,1).
bond(test,d297,d297_84,d297_88,1).
bond(test,d297,d297_86,d297_89,1).
bond(test,d297,d297_86,d297_90,1).
bond(test,d297,d297_86,d297_91,1).
bond(test,d297,d297_92,d297_93,1).
bond(test,d297,d297_93,d297_94,1).
bond(test,d297,d297_93,d297_95,1).
bond(test,d297,d297_92,d297_96,1).
bond(test,d297,d297_92,d297_97,1).
bond(test,d297,d297_92,d297_98,1).
bond(test,d297,d297_96,d297_99,1).
bond(test,d297,d297_99,d297_100,1).
bond(test,d297,d297_99,d297_101,1).
bond(test,d297,d297_99,d297_102,1).
bond(test,d297,d297_100,d297_103,1).
bond(test,d297,d297_100,d297_104,1).
bond(test,d297,d297_100,d297_105,1).
bond(test,d297,d297_103,d297_106,1).
bond(test,d297,d297_106,d297_107,1).
bond(test,d297,d297_106,d297_108,1).
bond(test,d297,d297_106,d297_109,1).
bond(test,d297,d297_107,d297_110,1).
bond(test,d297,d297_107,d297_111,1).
bond(test,d297,d297_107,d297_112,1).
bond(test,d297,d297_110,d297_113,1).
bond(test,d297,d297_113,d297_114,1).
bond(test,d297,d297_113,d297_115,1).
bond(test,d297,d297_113,d297_116,1).
bond(test,d297,d297_114,d297_117,1).
bond(test,d297,d297_114,d297_118,1).
bond(test,d297,d297_114,d297_119,1).
bond(test,d297,d297_117,d297_120,1).
bond(test,d297,d297_120,d297_121,1).
bond(test,d297,d297_120,d297_122,1).
bond(test,d297,d297_120,d297_123,1).
bond(test,d297,d297_121,d297_124,1).
bond(test,d297,d297_121,d297_125,1).
bond(test,d297,d297_121,d297_126,1).
bond(test,d297,d297_124,d297_127,1).
bond(test,d297,d297_87,d297_93,1).
bond(test,d297,d297_128,d297_129,1).
bond(test,d297,d297_129,d297_130,1).
bond(test,d297,d297_129,d297_131,1).
bond(test,d297,d297_128,d297_132,1).
bond(test,d297,d297_128,d297_133,1).
bond(test,d297,d297_128,d297_134,1).
bond(test,d297,d297_132,d297_135,1).
bond(test,d297,d297_135,d297_136,1).
bond(test,d297,d297_135,d297_137,1).
bond(test,d297,d297_135,d297_138,1).
bond(test,d297,d297_136,d297_139,1).
bond(test,d297,d297_136,d297_140,1).
bond(test,d297,d297_136,d297_141,1).
bond(test,d297,d297_139,d297_142,1).
bond(test,d297,d297_142,d297_143,1).
bond(test,d297,d297_142,d297_144,1).
bond(test,d297,d297_142,d297_145,1).
bond(test,d297,d297_143,d297_146,1).
bond(test,d297,d297_143,d297_147,1).
bond(test,d297,d297_143,d297_148,1).
bond(test,d297,d297_146,d297_149,1).
bond(test,d297,d297_149,d297_150,1).
bond(test,d297,d297_149,d297_151,1).
bond(test,d297,d297_149,d297_152,1).
bond(test,d297,d297_150,d297_153,1).
bond(test,d297,d297_150,d297_154,1).
bond(test,d297,d297_150,d297_155,1).
bond(test,d297,d297_153,d297_156,1).
bond(test,d297,d297_156,d297_157,1).
bond(test,d297,d297_156,d297_158,1).
bond(test,d297,d297_156,d297_159,1).
bond(test,d297,d297_157,d297_160,1).
bond(test,d297,d297_157,d297_161,1).
bond(test,d297,d297_157,d297_162,1).
bond(test,d297,d297_89,d297_129,1).
bond(test,d297,d297_160,d297_163,1).
bond(test,d297,d297_163,d297_164,2).
bond(test,d297,d297_163,d297_165,1).
bond(test,d297,d297_165,d297_166,1).
bond(test,d297,d297_165,d297_167,1).
bond(test,d297,d297_165,d297_168,1).
bond(test,d297,d297_166,d297_169,1).
bond(test,d297,d297_166,d297_170,1).
bond(test,d297,d297_166,d297_171,1).
bond(test,d297,d297_169,d297_172,1).
bond(test,d297,d297_169,d297_173,1).
bond(test,d297,d297_169,d297_174,1).
bond(test,d297,d297_172,d297_175,1).
bond(test,d297,d297_172,d297_176,1).
bond(test,d297,d297_172,d297_177,1).
bond(test,d297,d297_175,d297_178,1).
bond(test,d297,d297_175,d297_179,1).
bond(test,d297,d297_175,d297_180,1).
bond(test,d297,d297_178,d297_181,1).
bond(test,d297,d297_181,d297_182,2).
bond(test,d297,d297_182,d297_183,1).
bond(test,d297,d297_183,d297_184,1).
bond(test,d297,d297_184,d297_185,1).
bond(test,d297,d297_184,d297_186,1).
bond(test,d297,d297_184,d297_187,1).
bond(test,d297,d297_185,d297_188,1).
bond(test,d297,d297_185,d297_189,1).
bond(test,d297,d297_185,d297_190,1).
bond(test,d297,d297_188,d297_191,1).
bond(test,d297,d297_188,d297_192,1).
bond(test,d297,d297_188,d297_193,1).
bond(test,d297,d297_191,d297_194,1).
bond(test,d297,d297_191,d297_195,1).
bond(test,d297,d297_191,d297_196,1).
bond(test,d297,d297_194,d297_197,1).
bond(test,d297,d297_194,d297_198,1).
bond(test,d297,d297_194,d297_199,1).
bond(test,d297,d297_197,d297_200,1).
bond(test,d297,d297_197,d297_201,1).
bond(test,d297,d297_197,d297_202,1).
bond(test,d297,d297_200,d297_203,1).
bond(test,d297,d297_200,d297_204,1).
bond(test,d297,d297_200,d297_205,1).
bond(test,d297,d297_203,d297_206,1).
bond(test,d297,d297_203,d297_207,1).
bond(test,d297,d297_203,d297_208,1).
bond(test,d297,d297_183,d297_209,1).
bond(test,d297,d297_183,d297_210,1).
bond(test,d297,d297_178,d297_211,1).
bond(test,d297,d297_178,d297_212,1).
bond(test,d297,d297_181,d297_213,1).
bond(test,d297,d297_182,d297_214,1).
bond(test,d298,d298_1,d298_2,7).
bond(test,d298,d298_2,d298_3,7).
bond(test,d298,d298_3,d298_4,7).
bond(test,d298,d298_4,d298_5,7).
bond(test,d298,d298_5,d298_6,7).
bond(test,d298,d298_6,d298_1,7).
bond(test,d298,d298_1,d298_7,1).
bond(test,d298,d298_2,d298_8,1).
bond(test,d298,d298_5,d298_9,1).
bond(test,d298,d298_6,d298_10,1).
bond(test,d298,d298_3,d298_11,1).
bond(test,d298,d298_11,d298_12,1).
bond(test,d298,d298_12,d298_13,7).
bond(test,d298,d298_13,d298_14,1).
bond(test,d298,d298_14,d298_4,1).
bond(test,d298,d298_12,d298_15,7).
bond(test,d298,d298_15,d298_16,7).
bond(test,d298,d298_16,d298_17,7).
bond(test,d298,d298_17,d298_18,7).
bond(test,d298,d298_18,d298_13,7).
bond(test,d298,d298_15,d298_19,1).
bond(test,d298,d298_16,d298_20,1).
bond(test,d298,d298_17,d298_21,1).
bond(test,d298,d298_18,d298_22,1).
bond(test,d298,d298_14,d298_23,1).
bond(test,d298,d298_23,d298_24,1).
bond(test,d298,d298_23,d298_25,1).
bond(test,d298,d298_23,d298_26,1).
bond(test,d298,d298_24,d298_27,1).
bond(test,d298,d298_27,d298_28,1).
bond(test,d298,d298_27,d298_29,1).
bond(test,d298,d298_27,d298_30,1).
bond(test,d298,d298_24,d298_31,1).
bond(test,d298,d298_24,d298_32,1).
bond(test,d298,d298_31,d298_33,1).
bond(test,d298,d298_33,d298_34,1).
bond(test,d298,d298_33,d298_35,1).
bond(test,d298,d298_33,d298_36,1).
bond(test,d298,d298_31,d298_37,1).
bond(test,d298,d298_37,d298_38,1).
bond(test,d298,d298_37,d298_39,1).
bond(test,d298,d298_37,d298_40,1).
bond(test,d299,d299_1,d299_2,7).
bond(test,d299,d299_2,d299_3,7).
bond(test,d299,d299_3,d299_4,7).
bond(test,d299,d299_4,d299_5,7).
bond(test,d299,d299_5,d299_6,7).
bond(test,d299,d299_6,d299_1,7).
bond(test,d299,d299_1,d299_7,1).
bond(test,d299,d299_2,d299_8,1).
bond(test,d299,d299_4,d299_9,1).
bond(test,d299,d299_6,d299_10,1).
bond(test,d299,d299_5,d299_11,1).
bond(test,d299,d299_3,d299_12,1).
bond(test,d299,d299_11,d299_13,1).
bond(test,d299,d299_12,d299_14,1).
bond(test,d300,d300_1,d300_2,1).
bond(test,d300,d300_1,d300_3,1).
bond(test,d300,d300_1,d300_4,1).
bond(test,d300,d300_1,d300_5,1).
bond(test,d300,d300_3,d300_6,1).
bond(test,d300,d300_3,d300_7,2).
bond(test,d300,d300_2,d300_8,1).
bond(test,d300,d300_2,d300_9,1).
bond(test,d300,d300_2,d300_10,1).
bond(test,d300,d300_8,d300_6,1).
bond(test,d300,d300_8,d300_11,1).
bond(test,d300,d300_8,d300_12,1).
bond(test,d302,d302_1,d302_2,1).
bond(test,d302,d302_2,d302_3,1).
bond(test,d302,d302_2,d302_4,1).
bond(test,d302,d302_2,d302_5,1).
bond(test,d302,d302_3,d302_6,1).
bond(test,d302,d302_3,d302_7,2).
bond(test,d302,d302_6,d302_8,1).
bond(test,d303,d303_1,d303_2,7).
bond(test,d303,d303_2,d303_3,7).
bond(test,d303,d303_3,d303_4,7).
bond(test,d303,d303_4,d303_5,7).
bond(test,d303,d303_5,d303_6,7).
bond(test,d303,d303_6,d303_1,7).
bond(test,d303,d303_1,d303_7,1).
bond(test,d303,d303_3,d303_8,1).
bond(test,d303,d303_4,d303_9,1).
bond(test,d303,d303_6,d303_10,1).
bond(test,d303,d303_5,d303_11,1).
bond(test,d303,d303_2,d303_12,1).
bond(test,d303,d303_12,d303_13,2).
bond(test,d303,d303_12,d303_14,2).
bond(test,d303,d303_11,d303_15,1).
bond(test,d304,d304a_1,d304a_2,7).
bond(test,d304,d304a_2,d304a_3,7).
bond(test,d304,d304a_3,d304a_4,7).
bond(test,d304,d304a_4,d304a_5,7).
bond(test,d304,d304a_5,d304a_6,7).
bond(test,d304,d304a_6,d304a_1,7).
bond(test,d304,d304a_1,d304a_7,1).
bond(test,d304,d304a_5,d304a_8,1).
bond(test,d304,d304a_3,d304a_9,1).
bond(test,d304,d304a_9,d304a_10,1).
bond(test,d304,d304a_10,d304a_11,2).
bond(test,d304,d304a_2,d304a_12,1).
bond(test,d304,d304a_12,d304a_13,1).
bond(test,d304,d304a_12,d304a_14,1).
bond(test,d304,d304a_12,d304a_15,1).
bond(test,d304,d304a_4,d304a_16,1).
bond(test,d304,d304a_16,d304a_17,1).
bond(test,d304,d304a_16,d304a_18,1).
bond(test,d304,d304a_16,d304a_19,1).
bond(test,d304,d304a_6,d304a_20,1).
bond(test,d304,d304a_20,d304a_21,1).
bond(test,d304,d304a_20,d304a_22,1).
bond(test,d304,d304a_20,d304a_23,1).
bond(test,d304,d304a_10,d304a_24,1).
bond(test,d304,d304a_10,d304a_25,1).
bond(test,d304,d304a_24,d304a_26,1).
bond(test,d304,d304a_25,d304a_27,1).
bond(test,d304,d304b_1,d304b_2,7).
bond(test,d304,d304b_2,d304b_3,7).
bond(test,d304,d304b_3,d304b_4,7).
bond(test,d304,d304b_4,d304b_5,7).
bond(test,d304,d304b_5,d304b_6,7).
bond(test,d304,d304b_6,d304b_1,7).
bond(test,d304,d304b_5,d304b_7,1).
bond(test,d304,d304b_3,d304b_8,1).
bond(test,d304,d304b_8,d304b_9,1).
bond(test,d304,d304b_9,d304b_10,2).
bond(test,d304,d304b_2,d304b_11,1).
bond(test,d304,d304b_11,d304b_12,1).
bond(test,d304,d304b_11,d304b_13,1).
bond(test,d304,d304b_11,d304b_14,1).
bond(test,d304,d304b_6,d304b_15,1).
bond(test,d304,d304b_15,d304b_16,1).
bond(test,d304,d304b_15,d304b_17,1).
bond(test,d304,d304b_15,d304b_18,1).
bond(test,d304,d304b_9,d304b_19,1).
bond(test,d304,d304b_9,d304b_20,1).
bond(test,d304,d304b_19,d304b_21,1).
bond(test,d304,d304b_20,d304b_22,1).
bond(test,d304,d304b_1,d304b_23,1).
bond(test,d304,d304b_23,d304b_24,1).
bond(test,d304,d304b_23,d304b_25,1).
bond(test,d304,d304b_23,d304b_26,1).
bond(test,d304,d304b_4,d304b_27,1).
bond(test,d305,d305_1,d305_2,7).
bond(test,d305,d305_2,d305_3,7).
bond(test,d305,d305_3,d305_4,7).
bond(test,d305,d305_4,d305_5,7).
bond(test,d305,d305_5,d305_6,7).
bond(test,d305,d305_6,d305_1,7).
bond(test,d305,d305_1,d305_7,1).
bond(test,d305,d305_3,d305_8,1).
bond(test,d305,d305_6,d305_9,1).
bond(test,d305,d305_4,d305_10,1).
bond(test,d305,d305_10,d305_11,1).
bond(test,d305,d305_10,d305_12,1).
bond(test,d305,d305_10,d305_13,1).
bond(test,d305,d305_11,d305_14,7).
bond(test,d305,d305_14,d305_15,7).
bond(test,d305,d305_15,d305_16,7).
bond(test,d305,d305_16,d305_17,7).
bond(test,d305,d305_17,d305_18,7).
bond(test,d305,d305_18,d305_11,7).
bond(test,d305,d305_14,d305_19,1).
bond(test,d305,d305_15,d305_20,1).
bond(test,d305,d305_16,d305_21,1).
bond(test,d305,d305_17,d305_22,1).
bond(test,d305,d305_18,d305_23,1).
bond(test,d305,d305_5,d305_24,1).
bond(test,d305,d305_2,d305_25,1).
bond(test,d305,d305_24,d305_26,1).
bond(test,d306,d306_1,d306_2,1).
bond(test,d306,d306_2,d306_3,1).
bond(test,d306,d306_2,d306_4,1).
bond(test,d306,d306_2,d306_5,1).
bond(test,d306,d306_3,d306_6,1).
bond(test,d306,d306_6,d306_7,1).
bond(test,d306,d306_6,d306_8,1).
bond(test,d306,d306_6,d306_9,1).
bond(test,d306,d306_3,d306_10,1).
bond(test,d306,d306_10,d306_11,1).
bond(test,d306,d306_10,d306_12,1).
bond(test,d306,d306_10,d306_13,1).
bond(test,d306,d306_3,d306_14,1).
bond(test,d306,d306_14,d306_15,1).
bond(test,d306,d306_14,d306_16,1).
bond(test,d306,d306_14,d306_17,1).
bond(test,d306,d306_1,d306_18,1).
bond(test,d306,d306_11,d306_19,1).
bond(test,d307,d307_1,d307_2,1).
bond(test,d307,d307_1,d307_3,1).
bond(test,d307,d307_3,d307_4,1).
bond(test,d307,d307_3,d307_5,1).
bond(test,d307,d307_3,d307_6,1).
bond(test,d307,d307_1,d307_7,1).
bond(test,d307,d307_7,d307_8,1).
bond(test,d307,d307_7,d307_9,1).
bond(test,d307,d307_7,d307_10,1).
bond(test,d307,d307_1,d307_11,1).
bond(test,d307,d307_11,d307_12,1).
bond(test,d307,d307_11,d307_13,1).
bond(test,d307,d307_11,d307_14,1).
bond(test,d307,d307_2,d307_15,1).
bond(test,d308,d308_1,d308_2,7).
bond(test,d308,d308_2,d308_3,7).
bond(test,d308,d308_3,d308_4,7).
bond(test,d308,d308_4,d308_5,7).
bond(test,d308,d308_5,d308_6,7).
bond(test,d308,d308_6,d308_1,7).
bond(test,d308,d308_1,d308_7,1).
bond(test,d308,d308_2,d308_8,1).
bond(test,d308,d308_5,d308_9,1).
bond(test,d308,d308_6,d308_10,1).
bond(test,d308,d308_3,d308_11,1).
bond(test,d308,d308_11,d308_12,1).
bond(test,d308,d308_12,d308_13,1).
bond(test,d308,d308_13,d308_14,1).
bond(test,d308,d308_14,d308_4,1).
bond(test,d308,d308_11,d308_15,1).
bond(test,d308,d308_11,d308_16,1).
bond(test,d308,d308_12,d308_17,1).
bond(test,d308,d308_12,d308_18,1).
bond(test,d308,d308_13,d308_19,2).
bond(test,d309,d309_1,d309_2,1).
bond(test,d309,d309_1,d309_3,1).
bond(test,d309,d309_1,d309_4,1).
bond(test,d309,d309_1,d309_5,1).
bond(test,d309,d309_2,d309_6,1).
bond(test,d309,d309_2,d309_7,1).
bond(test,d309,d309_2,d309_8,1).
bond(test,d309,d309_3,d309_9,1).
bond(test,d309,d309_6,d309_10,1).
bond(test,d311,d311_1,d311_2,7).
bond(test,d311,d311_2,d311_3,7).
bond(test,d311,d311_3,d311_4,7).
bond(test,d311,d311_4,d311_5,7).
bond(test,d311,d311_5,d311_6,7).
bond(test,d311,d311_6,d311_1,7).
bond(test,d311,d311_1,d311_7,1).
bond(test,d311,d311_2,d311_8,1).
bond(test,d311,d311_3,d311_9,1).
bond(test,d311,d311_5,d311_10,1).
bond(test,d311,d311_6,d311_11,1).
bond(test,d311,d311_4,d311_12,1).
bond(test,d311,d311_13,d311_14,1).
bond(test,d311,d311_14,d311_15,1).
bond(test,d311,d311_15,d311_16,1).
bond(test,d311,d311_16,d311_17,1).
bond(test,d311,d311_17,d311_18,1).
bond(test,d311,d311_18,d311_13,1).
bond(test,d311,d311_13,d311_19,1).
bond(test,d311,d311_13,d311_20,1).
bond(test,d311,d311_14,d311_21,1).
bond(test,d311,d311_14,d311_22,1).
bond(test,d311,d311_15,d311_23,1).
bond(test,d311,d311_15,d311_24,1).
bond(test,d311,d311_16,d311_25,1).
bond(test,d311,d311_16,d311_26,1).
bond(test,d311,d311_12,d311_18,1).
bond(test,d311,d311_18,d311_27,1).
bond(test,d311,d311_12,d311_28,1).
bond(test,d311,d311_12,d311_29,1).
bond(test,d311,d311_28,d311_30,1).
bond(test,d311,d311_30,d311_31,1).
bond(test,d311,d311_31,d311_32,1).
bond(test,d311,d311_31,d311_33,1).
bond(test,d311,d311_31,d311_34,1).
bond(test,d311,d311_28,d311_35,2).
bond(test,d311,d311_17,d311_36,1).
bond(test,d312,d312_1,d312_2,1).
bond(test,d312,d312_2,d312_3,1).
bond(test,d312,d312_3,d312_4,7).
bond(test,d312,d312_4,d312_5,1).
bond(test,d312,d312_5,d312_6,1).
bond(test,d312,d312_6,d312_1,1).
bond(test,d312,d312_3,d312_7,7).
bond(test,d312,d312_7,d312_8,7).
bond(test,d312,d312_8,d312_9,7).
bond(test,d312,d312_9,d312_4,7).
bond(test,d312,d312_8,d312_10,1).
bond(test,d312,d312_5,d312_11,2).
bond(test,d312,d312_6,d312_12,1).
bond(test,d312,d312_12,d312_13,1).
bond(test,d312,d312_12,d312_14,1).
bond(test,d312,d312_12,d312_15,1).
bond(test,d312,d312_1,d312_16,2).
bond(test,d312,d312_2,d312_17,1).
bond(test,d312,d312_17,d312_18,1).
bond(test,d312,d312_17,d312_19,1).
bond(test,d312,d312_17,d312_20,1).
bond(test,d312,d312_7,d312_21,1).
bond(test,d313,d313_1,d313_2,7).
bond(test,d313,d313_2,d313_3,7).
bond(test,d313,d313_3,d313_4,7).
bond(test,d313,d313_4,d313_5,7).
bond(test,d313,d313_5,d313_6,7).
bond(test,d313,d313_6,d313_1,7).
bond(test,d313,d313_2,d313_7,1).
bond(test,d313,d313_5,d313_8,1).
bond(test,d313,d313_9,d313_10,7).
bond(test,d313,d313_10,d313_11,7).
bond(test,d313,d313_11,d313_12,7).
bond(test,d313,d313_12,d313_13,7).
bond(test,d313,d313_13,d313_14,7).
bond(test,d313,d313_14,d313_9,7).
bond(test,d313,d313_11,d313_15,1).
bond(test,d313,d313_14,d313_16,1).
bond(test,d313,d313_4,d313_17,1).
bond(test,d313,d313_17,d313_9,1).
bond(test,d313,d313_6,d313_18,1).
bond(test,d313,d313_18,d313_19,1).
bond(test,d313,d313_1,d313_20,1).
bond(test,d313,d313_3,d313_21,1).
bond(test,d313,d313_21,d313_22,1).
bond(test,d313,d313_21,d313_23,1).
bond(test,d313,d313_21,d313_24,1).
bond(test,d313,d313_20,d313_25,1).
bond(test,d313,d313_13,d313_26,1).
bond(test,d313,d313_26,d313_27,1).
bond(test,d313,d313_12,d313_28,1).
bond(test,d313,d313_28,d313_29,1).
bond(test,d313,d313_10,d313_30,1).
bond(test,d313,d313_30,d313_31,1).
bond(test,d313,d313_30,d313_32,1).
bond(test,d313,d313_30,d313_33,1).
bond(test,d313,d313_18,d313_34,1).
bond(test,d313,d313_34,d313_35,1).
bond(test,d313,d313_34,d313_36,1).
bond(test,d313,d313_34,d313_37,1).
bond(test,d313,d313_18,d313_38,1).
bond(test,d313,d313_38,d313_39,1).
bond(test,d313,d313_38,d313_40,1).
bond(test,d313,d313_38,d313_41,1).
bond(test,d313,d313_26,d313_42,1).
bond(test,d313,d313_42,d313_43,1).
bond(test,d313,d313_42,d313_44,1).
bond(test,d313,d313_42,d313_45,1).
bond(test,d313,d313_26,d313_46,1).
bond(test,d313,d313_46,d313_47,1).
bond(test,d313,d313_46,d313_48,1).
bond(test,d313,d313_46,d313_49,1).
bond(test,d313,d313_27,d313_50,1).
bond(test,d313,d313_27,d313_51,1).
bond(test,d313,d313_27,d313_52,1).
bond(test,d313,d313_19,d313_53,1).
bond(test,d313,d313_19,d313_54,1).
bond(test,d313,d313_19,d313_55,1).
bond(test,d314,d314_1,d314_2,7).
bond(test,d314,d314_2,d314_3,7).
bond(test,d314,d314_3,d314_4,7).
bond(test,d314,d314_4,d314_5,7).
bond(test,d314,d314_5,d314_6,7).
bond(test,d314,d314_6,d314_1,7).
bond(test,d314,d314_3,d314_7,7).
bond(test,d314,d314_7,d314_8,7).
bond(test,d314,d314_8,d314_9,7).
bond(test,d314,d314_9,d314_10,7).
bond(test,d314,d314_10,d314_4,7).
bond(test,d314,d314_11,d314_12,7).
bond(test,d314,d314_12,d314_13,7).
bond(test,d314,d314_13,d314_14,7).
bond(test,d314,d314_14,d314_15,7).
bond(test,d314,d314_15,d314_16,7).
bond(test,d314,d314_16,d314_11,7).
bond(test,d314,d314_12,d314_17,1).
bond(test,d314,d314_13,d314_18,1).
bond(test,d314,d314_14,d314_19,1).
bond(test,d314,d314_15,d314_20,1).
bond(test,d314,d314_16,d314_21,1).
bond(test,d314,d314_9,d314_11,1).
bond(test,d314,d314_5,d314_22,1).
bond(test,d314,d314_1,d314_23,1).
bond(test,d314,d314_8,d314_24,1).
bond(test,d314,d314_22,d314_25,1).
bond(test,d314,d314_22,d314_26,1).
bond(test,d314,d314_23,d314_27,1).
bond(test,d314,d314_23,d314_28,1).
bond(test,d314,d314_24,d314_29,1).
bond(test,d314,d314_24,d314_30,1).
bond(test,d315,d315_1,d315_2,7).
bond(test,d315,d315_2,d315_3,7).
bond(test,d315,d315_3,d315_4,7).
bond(test,d315,d315_4,d315_5,7).
bond(test,d315,d315_5,d315_6,7).
bond(test,d315,d315_6,d315_1,7).
bond(test,d315,d315_1,d315_7,1).
bond(test,d315,d315_2,d315_8,1).
bond(test,d315,d315_3,d315_9,1).
bond(test,d315,d315_5,d315_10,1).
bond(test,d315,d315_6,d315_11,1).
bond(test,d315,d315_12,d315_13,7).
bond(test,d315,d315_13,d315_14,7).
bond(test,d315,d315_14,d315_15,7).
bond(test,d315,d315_15,d315_16,7).
bond(test,d315,d315_16,d315_17,7).
bond(test,d315,d315_17,d315_12,7).
bond(test,d315,d315_12,d315_18,1).
bond(test,d315,d315_14,d315_19,1).
bond(test,d315,d315_15,d315_20,1).
bond(test,d315,d315_16,d315_21,1).
bond(test,d315,d315_17,d315_22,1).
bond(test,d315,d315_4,d315_23,1).
bond(test,d315,d315_23,d315_13,1).
bond(test,d315,d315_23,d315_24,1).
bond(test,d315,d315_23,d315_25,1).
bond(test,d315,d315_25,d315_26,1).
bond(test,d315,d315_26,d315_27,1).
bond(test,d315,d315_27,d315_24,1).
bond(test,d315,d315_25,d315_28,2).
bond(test,d315,d315_27,d315_29,2).
bond(test,d315,d315_24,d315_30,1).
bond(test,d315,d315_26,d315_31,1).
bond(test,d316,d316_1,d316_2,7).
bond(test,d316,d316_2,d316_3,7).
bond(test,d316,d316_3,d316_4,7).
bond(test,d316,d316_4,d316_5,7).
bond(test,d316,d316_5,d316_6,7).
bond(test,d316,d316_6,d316_1,7).
bond(test,d316,d316_5,d316_7,1).
bond(test,d316,d316_7,d316_8,1).
bond(test,d316,d316_8,d316_9,1).
bond(test,d316,d316_8,d316_10,1).
bond(test,d316,d316_8,d316_11,1).
bond(test,d316,d316_4,d316_12,1).
bond(test,d316,d316_3,d316_13,1).
bond(test,d316,d316_2,d316_14,1).
bond(test,d316,d316_1,d316_15,1).
bond(test,d316,d316_6,d316_16,1).
bond(test,d317,d317_1,d317_2,1).
bond(test,d317,d317_1,d317_3,1).
bond(test,d317,d317_1,d317_4,1).
bond(test,d318,d318_1,d318_2,7).
bond(test,d318,d318_2,d318_3,7).
bond(test,d318,d318_3,d318_4,7).
bond(test,d318,d318_4,d318_5,7).
bond(test,d318,d318_5,d318_6,7).
bond(test,d318,d318_6,d318_1,7).
bond(test,d318,d318_2,d318_7,1).
bond(test,d318,d318_3,d318_8,1).
bond(test,d318,d318_6,d318_9,1).
bond(test,d318,d318_4,d318_10,1).
bond(test,d318,d318_10,d318_11,2).
bond(test,d318,d318_10,d318_12,1).
bond(test,d318,d318_11,d318_13,1).
bond(test,d318,d318_11,d318_14,1).
bond(test,d318,d318_13,d318_15,7).
bond(test,d318,d318_15,d318_16,7).
bond(test,d318,d318_16,d318_17,7).
bond(test,d318,d318_17,d318_18,7).
bond(test,d318,d318_18,d318_19,7).
bond(test,d318,d318_19,d318_13,7).
bond(test,d318,d318_15,d318_20,1).
bond(test,d318,d318_16,d318_21,1).
bond(test,d318,d318_18,d318_22,1).
bond(test,d318,d318_1,d318_23,1).
bond(test,d318,d318_5,d318_24,1).
bond(test,d318,d318_17,d318_25,1).
bond(test,d318,d318_19,d318_26,1).
bond(test,d318,d318_23,d318_27,1).
bond(test,d318,d318_23,d318_28,1).
bond(test,d318,d318_25,d318_29,1).
bond(test,d318,d318_25,d318_30,1).
bond(test,d318,d318_24,d318_31,1).
bond(test,d318,d318_31,d318_32,1).
bond(test,d318,d318_26,d318_33,1).
bond(test,d318,d318_24,d318_34,2).
bond(test,d318,d318_24,d318_35,2).
bond(test,d318,d318_26,d318_36,2).
bond(test,d318,d318_26,d318_37,2).
bond(test,d318,d318_33,d318_38,1).
bond(test,d319,d319_1,d319_2,1).
bond(test,d319,d319_1,d319_3,1).
bond(test,d319,d319_1,d319_4,1).
bond(test,d319,d319_1,d319_5,1).
bond(test,d320,d320_1,d320_2,7).
bond(test,d320,d320_2,d320_3,7).
bond(test,d320,d320_3,d320_4,7).
bond(test,d320,d320_4,d320_5,7).
bond(test,d320,d320_5,d320_6,7).
bond(test,d320,d320_6,d320_1,7).
bond(test,d320,d320_1,d320_7,1).
bond(test,d320,d320_3,d320_8,1).
bond(test,d320,d320_4,d320_9,1).
bond(test,d320,d320_6,d320_10,1).
bond(test,d320,d320_5,d320_11,1).
bond(test,d320,d320_11,d320_12,1).
bond(test,d320,d320_2,d320_13,1).
bond(test,d320,d320_11,d320_14,2).
bond(test,d320,d320_13,d320_15,2).
bond(test,d320,d320_13,d320_16,2).
bond(test,d320,d320_12,d320_17,1).
bond(test,d322,d322_1,d322_2,7).
bond(test,d322,d322_2,d322_3,7).
bond(test,d322,d322_3,d322_4,7).
bond(test,d322,d322_4,d322_5,7).
bond(test,d322,d322_5,d322_6,7).
bond(test,d322,d322_6,d322_1,7).
bond(test,d322,d322_7,d322_8,7).
bond(test,d322,d322_8,d322_9,7).
bond(test,d322,d322_9,d322_10,7).
bond(test,d322,d322_10,d322_11,7).
bond(test,d322,d322_11,d322_12,7).
bond(test,d322,d322_12,d322_7,7).
bond(test,d322,d322_4,d322_7,1).
bond(test,d322,d322_11,d322_13,1).
bond(test,d322,d322_13,d322_14,1).
bond(test,d322,d322_10,d322_15,1).
bond(test,d322,d322_15,d322_16,2).
bond(test,d322,d322_14,d322_16,1).
bond(test,d322,d322_17,d322_18,7).
bond(test,d322,d322_18,d322_19,7).
bond(test,d322,d322_19,d322_20,7).
bond(test,d322,d322_20,d322_21,7).
bond(test,d322,d322_21,d322_22,7).
bond(test,d322,d322_22,d322_17,7).
bond(test,d322,d322_16,d322_18,1).
bond(test,d322,d322_14,d322_23,1).
bond(test,d322,d322_23,d322_17,1).
bond(test,d322,d322_21,d322_24,7).
bond(test,d322,d322_24,d322_25,7).
bond(test,d322,d322_25,d322_26,7).
bond(test,d322,d322_26,d322_27,7).
bond(test,d322,d322_27,d322_22,7).
bond(test,d322,d322_27,d322_28,1).
bond(test,d322,d322_19,d322_29,1).
bond(test,d322,d322_25,d322_30,1).
bond(test,d322,d322_29,d322_31,1).
bond(test,d322,d322_30,d322_32,1).
bond(test,d322,d322_30,d322_33,2).
bond(test,d322,d322_30,d322_34,2).
bond(test,d322,d322_29,d322_35,2).
bond(test,d322,d322_29,d322_36,2).
bond(test,d322,d322_1,d322_37,7).
bond(test,d322,d322_2,d322_38,7).
bond(test,d322,d322_37,d322_39,7).
bond(test,d322,d322_39,d322_40,7).
bond(test,d322,d322_40,d322_38,7).
bond(test,d322,d322_41,d322_42,7).
bond(test,d322,d322_42,d322_43,7).
bond(test,d322,d322_43,d322_44,7).
bond(test,d322,d322_44,d322_45,7).
bond(test,d322,d322_45,d322_46,7).
bond(test,d322,d322_46,d322_41,7).
bond(test,d322,d322_40,d322_44,1).
bond(test,d322,d322_39,d322_47,1).
bond(test,d322,d322_47,d322_45,1).
bond(test,d322,d322_43,d322_48,1).
bond(test,d322,d322_48,d322_49,1).
bond(test,d322,d322_46,d322_50,7).
bond(test,d322,d322_50,d322_51,7).
bond(test,d322,d322_51,d322_52,7).
bond(test,d322,d322_52,d322_53,7).
bond(test,d322,d322_53,d322_41,7).
bond(test,d322,d322_50,d322_54,1).
bond(test,d322,d322_52,d322_55,1).
bond(test,d322,d322_55,d322_56,1).
bond(test,d322,d322_55,d322_57,2).
bond(test,d322,d322_55,d322_58,2).
bond(test,d322,d322_48,d322_59,2).
bond(test,d322,d322_48,d322_60,2).
bond(test,d322,d322_3,d322_61,1).
bond(test,d322,d322_5,d322_62,1).
bond(test,d322,d322_6,d322_63,1).
bond(test,d322,d322_8,d322_64,1).
bond(test,d322,d322_9,d322_65,1).
bond(test,d322,d322_12,d322_66,1).
bond(test,d322,d322_20,d322_67,1).
bond(test,d322,d322_24,d322_68,1).
bond(test,d322,d322_26,d322_69,1).
bond(test,d322,d322_28,d322_70,1).
bond(test,d322,d322_28,d322_71,1).
bond(test,d322,d322_31,d322_72,1).
bond(test,d322,d322_32,d322_73,1).
bond(test,d322,d322_42,d322_74,1).
bond(test,d322,d322_49,d322_75,1).
bond(test,d322,d322_51,d322_76,1).
bond(test,d322,d322_53,d322_77,1).
bond(test,d322,d322_54,d322_78,1).
bond(test,d322,d322_54,d322_79,1).
bond(test,d322,d322_56,d322_80,1).
bond(test,d323,d323_1,d323_2,7).
bond(test,d323,d323_2,d323_3,7).
bond(test,d323,d323_3,d323_4,7).
bond(test,d323,d323_4,d323_5,7).
bond(test,d323,d323_5,d323_6,7).
bond(test,d323,d323_6,d323_1,7).
bond(test,d323,d323_2,d323_7,1).
bond(test,d323,d323_3,d323_8,1).
bond(test,d323,d323_6,d323_9,1).
bond(test,d323,d323_4,d323_10,1).
bond(test,d323,d323_10,d323_11,2).
bond(test,d323,d323_11,d323_12,1).
bond(test,d323,d323_12,d323_13,7).
bond(test,d323,d323_13,d323_14,7).
bond(test,d323,d323_14,d323_15,7).
bond(test,d323,d323_15,d323_16,7).
bond(test,d323,d323_16,d323_17,7).
bond(test,d323,d323_17,d323_12,7).
bond(test,d323,d323_15,d323_18,1).
bond(test,d323,d323_16,d323_19,1).
bond(test,d323,d323_13,d323_20,7).
bond(test,d323,d323_20,d323_21,7).
bond(test,d323,d323_21,d323_22,7).
bond(test,d323,d323_22,d323_23,7).
bond(test,d323,d323_23,d323_14,7).
bond(test,d323,d323_20,d323_24,1).
bond(test,d323,d323_21,d323_25,1).
bond(test,d323,d323_22,d323_26,1).
bond(test,d323,d323_23,d323_27,1).
bond(test,d323,d323_1,d323_28,1).
bond(test,d323,d323_28,d323_29,1).
bond(test,d323,d323_28,d323_30,1).
bond(test,d323,d323_28,d323_31,1).
bond(test,d323,d323_17,d323_32,1).
bond(test,d323,d323_5,d323_33,1).
bond(test,d323,d323_32,d323_34,1).
bond(test,d323,d323_33,d323_35,2).
bond(test,d323,d323_33,d323_36,2).
bond(test,d324,d324_1,d324_2,7).
bond(test,d324,d324_2,d324_3,7).
bond(test,d324,d324_3,d324_4,7).
bond(test,d324,d324_4,d324_5,7).
bond(test,d324,d324_5,d324_6,7).
bond(test,d324,d324_6,d324_1,7).
bond(test,d324,d324_1,d324_7,1).
bond(test,d324,d324_2,d324_8,1).
bond(test,d324,d324_5,d324_9,1).
bond(test,d324,d324_6,d324_10,1).
bond(test,d324,d324_3,d324_11,7).
bond(test,d324,d324_11,d324_12,7).
bond(test,d324,d324_12,d324_13,7).
bond(test,d324,d324_13,d324_14,7).
bond(test,d324,d324_14,d324_4,7).
bond(test,d324,d324_11,d324_15,1).
bond(test,d324,d324_16,d324_17,7).
bond(test,d324,d324_17,d324_18,7).
bond(test,d324,d324_18,d324_19,7).
bond(test,d324,d324_19,d324_20,7).
bond(test,d324,d324_20,d324_21,7).
bond(test,d324,d324_21,d324_16,7).
bond(test,d324,d324_16,d324_22,1).
bond(test,d324,d324_18,d324_23,1).
bond(test,d324,d324_19,d324_24,1).
bond(test,d324,d324_21,d324_25,1).
bond(test,d324,d324_12,d324_26,1).
bond(test,d324,d324_26,d324_27,1).
bond(test,d324,d324_27,d324_20,1).
bond(test,d324,d324_27,d324_28,1).
bond(test,d324,d324_26,d324_29,2).
bond(test,d324,d324_17,d324_30,1).
bond(test,d324,d324_30,d324_31,2).
bond(test,d324,d324_30,d324_32,2).
bond(test,d324,d324_13,d324_33,1).
bond(test,d324,d324_33,d324_34,1).
bond(test,d324,d324_14,d324_35,1).
bond(test,d324,d324_35,d324_36,2).
bond(test,d324,d324_36,d324_37,1).
bond(test,d324,d324_37,d324_38,7).
bond(test,d324,d324_38,d324_39,7).
bond(test,d324,d324_39,d324_40,7).
bond(test,d324,d324_40,d324_41,7).
bond(test,d324,d324_41,d324_42,7).
bond(test,d324,d324_42,d324_37,7).
bond(test,d324,d324_38,d324_43,1).
bond(test,d324,d324_40,d324_44,1).
bond(test,d324,d324_41,d324_45,1).
bond(test,d324,d324_42,d324_46,1).
bond(test,d324,d324_46,d324_47,1).
bond(test,d324,d324_47,d324_48,1).
bond(test,d324,d324_47,d324_49,1).
bond(test,d324,d324_47,d324_50,1).
bond(test,d324,d324_39,d324_51,1).
bond(test,d324,d324_51,d324_52,2).
bond(test,d324,d324_51,d324_53,2).
bond(test,d325,d325_1,d325_2,7).
bond(test,d325,d325_2,d325_3,7).
bond(test,d325,d325_3,d325_4,7).
bond(test,d325,d325_4,d325_5,7).
bond(test,d325,d325_5,d325_6,7).
bond(test,d325,d325_6,d325_1,7).
bond(test,d325,d325_1,d325_7,1).
bond(test,d325,d325_3,d325_8,1).
bond(test,d325,d325_6,d325_9,1).
bond(test,d325,d325_5,d325_10,1).
bond(test,d325,d325_4,d325_11,1).
bond(test,d325,d325_2,d325_12,1).
bond(test,d325,d325_11,d325_13,1).
bond(test,d325,d325_11,d325_14,1).
bond(test,d325,d325_12,d325_15,1).
bond(test,d325,d325_12,d325_16,1).
bond(test,d325,d325_10,d325_17,1).
bond(test,d326,d326_1,d326_2,7).
bond(test,d326,d326_2,d326_3,7).
bond(test,d326,d326_3,d326_4,7).
bond(test,d326,d326_4,d326_5,7).
bond(test,d326,d326_5,d326_6,7).
bond(test,d326,d326_6,d326_1,7).
bond(test,d326,d326_1,d326_7,1).
bond(test,d326,d326_3,d326_8,1).
bond(test,d326,d326_4,d326_9,1).
bond(test,d326,d326_6,d326_10,1).
bond(test,d326,d326_5,d326_11,1).
bond(test,d326,d326_2,d326_12,1).
bond(test,d326,d326_12,d326_13,1).
bond(test,d326,d326_11,d326_14,1).
bond(test,d326,d326_12,d326_15,1).
bond(test,d326,d326_13,d326_16,2).
bond(test,d326,d326_13,d326_17,1).
bond(test,d326,d326_17,d326_18,1).
bond(test,d326,d326_17,d326_19,1).
bond(test,d326,d326_17,d326_20,1).
bond(test,d327,d327_1,d327_2,7).
bond(test,d327,d327_2,d327_3,7).
bond(test,d327,d327_3,d327_4,7).
bond(test,d327,d327_4,d327_5,7).
bond(test,d327,d327_5,d327_6,7).
bond(test,d327,d327_6,d327_1,7).
bond(test,d327,d327_3,d327_7,1).
bond(test,d327,d327_4,d327_8,1).
bond(test,d327,d327_5,d327_9,1).
bond(test,d327,d327_6,d327_10,1).
bond(test,d327,d327_1,d327_11,1).
bond(test,d327,d327_11,d327_12,1).
bond(test,d327,d327_12,d327_13,1).
bond(test,d327,d327_13,d327_14,7).
bond(test,d327,d327_14,d327_15,7).
bond(test,d327,d327_15,d327_16,7).
bond(test,d327,d327_16,d327_17,7).
bond(test,d327,d327_17,d327_18,7).
bond(test,d327,d327_18,d327_13,7).
bond(test,d327,d327_14,d327_19,1).
bond(test,d327,d327_15,d327_20,1).
bond(test,d327,d327_17,d327_21,1).
bond(test,d327,d327_18,d327_22,1).
bond(test,d327,d327_16,d327_23,1).
bond(test,d327,d327_23,d327_24,2).
bond(test,d327,d327_24,d327_25,1).
bond(test,d327,d327_25,d327_26,7).
bond(test,d327,d327_26,d327_27,7).
bond(test,d327,d327_27,d327_28,7).
bond(test,d327,d327_28,d327_29,7).
bond(test,d327,d327_29,d327_30,7).
bond(test,d327,d327_30,d327_25,7).
bond(test,d327,d327_26,d327_31,1).
bond(test,d327,d327_27,d327_32,1).
bond(test,d327,d327_30,d327_33,1).
bond(test,d327,d327_29,d327_34,1).
bond(test,d327,d327_34,d327_35,1).
bond(test,d327,d327_34,d327_36,2).
bond(test,d327,d327_28,d327_37,1).
bond(test,d327,d327_11,d327_38,1).
bond(test,d327,d327_12,d327_39,2).
bond(test,d327,d327_12,d327_40,2).
bond(test,d327,d327_35,d327_41,1).
bond(test,d327,d327_37,d327_42,1).
bond(test,d328,d328a_1,d328a_2,7).
bond(test,d328,d328a_2,d328a_3,7).
bond(test,d328,d328a_3,d328a_4,7).
bond(test,d328,d328a_4,d328a_5,7).
bond(test,d328,d328a_5,d328a_1,7).
bond(test,d328,d328a_1,d328a_6,1).
bond(test,d328,d328a_1,d328a_7,1).
bond(test,d328,d328a_2,d328a_8,1).
bond(test,d328,d328a_2,d328a_9,1).
bond(test,d328,d328a_3,d328a_10,1).
bond(test,d328,d328a_3,d328a_11,1).
bond(test,d328,d328a_4,d328a_12,1).
bond(test,d328,d328a_4,d328a_13,1).
bond(test,d328,d328a_5,d328a_14,1).
bond(test,d328,d328a_5,d328a_15,1).
bond(test,d328,d328b_1,d328b_2,1).
bond(test,d328,d328b_2,d328b_3,1).
bond(test,d329,d329_1,d329_2,7).
bond(test,d329,d329_2,d329_3,7).
bond(test,d329,d329_3,d329_4,7).
bond(test,d329,d329_4,d329_5,7).
bond(test,d329,d329_5,d329_6,7).
bond(test,d329,d329_6,d329_1,7).
bond(test,d329,d329_1,d329_7,1).
bond(test,d329,d329_2,d329_8,1).
bond(test,d329,d329_3,d329_9,7).
bond(test,d329,d329_9,d329_10,7).
bond(test,d329,d329_10,d329_11,7).
bond(test,d329,d329_11,d329_12,7).
bond(test,d329,d329_12,d329_4,7).
bond(test,d329,d329_9,d329_13,1).
bond(test,d329,d329_11,d329_14,1).
bond(test,d329,d329_6,d329_15,1).
bond(test,d329,d329_15,d329_16,1).
bond(test,d329,d329_10,d329_17,1).
bond(test,d329,d329_12,d329_19,1).
bond(test,d329,d329_19,d329_20,1).
bond(test,d329,d329_19,d329_21,2).
bond(test,d329,d329_19,d329_22,2).
bond(test,d329,d329_17,d329_18,1).
bond(test,d329,d329_17,d329_23,2).
bond(test,d329,d329_17,d329_24,2).
bond(test,d329,d329_18,d329_25,1).
bond(test,d329,d329_20,d329_26,1).
bond(test,d329,d329_5,d329_27,1).
bond(test,d329,d329_27,d329_28,2).
bond(test,d329,d329_28,d329_29,1).
bond(test,d329,d329_29,d329_30,7).
bond(test,d329,d329_30,d329_31,7).
bond(test,d329,d329_31,d329_32,7).
bond(test,d329,d329_32,d329_33,7).
bond(test,d329,d329_33,d329_34,7).
bond(test,d329,d329_34,d329_29,7).
bond(test,d329,d329_30,d329_35,1).
bond(test,d329,d329_31,d329_36,1).
bond(test,d329,d329_33,d329_37,1).
bond(test,d329,d329_32,d329_38,1).
bond(test,d329,d329_38,d329_39,7).
bond(test,d329,d329_39,d329_40,7).
bond(test,d329,d329_40,d329_41,7).
bond(test,d329,d329_41,d329_42,7).
bond(test,d329,d329_42,d329_43,7).
bond(test,d329,d329_43,d329_38,7).
bond(test,d329,d329_39,d329_44,1).
bond(test,d329,d329_40,d329_45,1).
bond(test,d329,d329_43,d329_46,1).
bond(test,d329,d329_34,d329_47,1).
bond(test,d329,d329_47,d329_48,1).
bond(test,d329,d329_47,d329_49,1).
bond(test,d329,d329_47,d329_50,1).
bond(test,d329,d329_42,d329_51,1).
bond(test,d329,d329_51,d329_52,1).
bond(test,d329,d329_51,d329_53,1).
bond(test,d329,d329_51,d329_54,1).
bond(test,d329,d329_41,d329_55,1).
bond(test,d329,d329_55,d329_56,2).
bond(test,d329,d329_56,d329_57,1).
bond(test,d329,d329_57,d329_58,7).
bond(test,d329,d329_58,d329_59,7).
bond(test,d329,d329_59,d329_60,7).
bond(test,d329,d329_60,d329_61,7).
bond(test,d329,d329_61,d329_62,7).
bond(test,d329,d329_62,d329_57,7).
bond(test,d329,d329_58,d329_63,1).
bond(test,d329,d329_59,d329_64,1).
bond(test,d329,d329_61,d329_65,1).
bond(test,d329,d329_62,d329_66,1).
bond(test,d329,d329_60,d329_67,1).
bond(test,d329,d329_67,d329_68,1).
bond(test,d329,d329_69,d329_70,7).
bond(test,d329,d329_70,d329_71,7).
bond(test,d329,d329_71,d329_72,7).
bond(test,d329,d329_72,d329_73,7).
bond(test,d329,d329_73,d329_74,7).
bond(test,d329,d329_74,d329_69,7).
bond(test,d329,d329_70,d329_75,1).
bond(test,d329,d329_71,d329_76,1).
bond(test,d329,d329_73,d329_77,1).
bond(test,d329,d329_74,d329_78,1).
bond(test,d329,d329_72,d329_79,1).
bond(test,d329,d329_79,d329_80,1).
bond(test,d329,d329_79,d329_81,1).
bond(test,d329,d329_79,d329_82,1).
bond(test,d329,d329_69,d329_68,1).
bond(test,d329,d329_68,d329_83,2).
bond(test,d329,d329_68,d329_84,2).
bond(test,d330,d330_1,d330_2,7).
bond(test,d330,d330_2,d330_3,7).
bond(test,d330,d330_3,d330_4,7).
bond(test,d330,d330_4,d330_5,7).
bond(test,d330,d330_5,d330_6,7).
bond(test,d330,d330_6,d330_1,7).
bond(test,d330,d330_2,d330_7,1).
bond(test,d330,d330_3,d330_8,1).
bond(test,d330,d330_5,d330_9,1).
bond(test,d330,d330_10,d330_11,7).
bond(test,d330,d330_11,d330_12,7).
bond(test,d330,d330_12,d330_13,7).
bond(test,d330,d330_13,d330_14,7).
bond(test,d330,d330_14,d330_15,7).
bond(test,d330,d330_15,d330_10,7).
bond(test,d330,d330_10,d330_16,1).
bond(test,d330,d330_11,d330_17,1).
bond(test,d330,d330_14,d330_18,1).
bond(test,d330,d330_4,d330_15,1).
bond(test,d330,d330_1,d330_19,1).
bond(test,d330,d330_19,d330_20,2).
bond(test,d330,d330_20,d330_21,1).
bond(test,d330,d330_21,d330_22,7).
bond(test,d330,d330_22,d330_23,7).
bond(test,d330,d330_23,d330_24,7).
bond(test,d330,d330_24,d330_25,7).
bond(test,d330,d330_25,d330_26,7).
bond(test,d330,d330_26,d330_21,7).
bond(test,d330,d330_25,d330_27,1).
bond(test,d330,d330_23,d330_28,7).
bond(test,d330,d330_28,d330_29,7).
bond(test,d330,d330_29,d330_30,7).
bond(test,d330,d330_30,d330_31,7).
bond(test,d330,d330_31,d330_24,7).
bond(test,d330,d330_29,d330_32,1).
bond(test,d330,d330_31,d330_33,1).
bond(test,d330,d330_26,d330_34,1).
bond(test,d330,d330_34,d330_35,1).
bond(test,d330,d330_34,d330_36,2).
bond(test,d330,d330_34,d330_37,2).
bond(test,d330,d330_35,d330_38,1).
bond(test,d330,d330_22,d330_39,1).
bond(test,d330,d330_39,d330_40,1).
bond(test,d330,d330_28,d330_41,1).
bond(test,d330,d330_41,d330_42,1).
bond(test,d330,d330_41,d330_43,1).
bond(test,d330,d330_30,d330_44,1).
bond(test,d330,d330_44,d330_45,1).
bond(test,d330,d330_44,d330_46,2).
bond(test,d330,d330_44,d330_47,2).
bond(test,d330,d330_45,d330_48,1).
bond(test,d330,d330_12,d330_49,1).
bond(test,d330,d330_49,d330_50,2).
bond(test,d330,d330_50,d330_51,1).
bond(test,d330,d330_51,d330_52,7).
bond(test,d330,d330_52,d330_53,7).
bond(test,d330,d330_53,d330_54,7).
bond(test,d330,d330_54,d330_55,7).
bond(test,d330,d330_55,d330_56,7).
bond(test,d330,d330_56,d330_51,7).
bond(test,d330,d330_53,d330_57,1).
bond(test,d330,d330_54,d330_58,7).
bond(test,d330,d330_58,d330_59,7).
bond(test,d330,d330_59,d330_60,7).
bond(test,d330,d330_60,d330_61,7).
bond(test,d330,d330_61,d330_55,7).
bond(test,d330,d330_58,d330_62,1).
bond(test,d330,d330_60,d330_63,1).
bond(test,d330,d330_52,d330_64,1).
bond(test,d330,d330_64,d330_65,2).
bond(test,d330,d330_64,d330_66,2).
bond(test,d330,d330_64,d330_67,1).
bond(test,d330,d330_56,d330_68,1).
bond(test,d330,d330_61,d330_69,1).
bond(test,d330,d330_59,d330_70,1).
bond(test,d330,d330_70,d330_71,1).
bond(test,d330,d330_70,d330_72,2).
bond(test,d330,d330_70,d330_73,2).
bond(test,d330,d330_67,d330_74,1).
bond(test,d330,d330_71,d330_75,1).
bond(test,d330,d330_68,d330_76,1).
bond(test,d330,d330_69,d330_77,1).
bond(test,d330,d330_69,d330_78,1).
bond(test,d330,d330_6,d330_79,1).
bond(test,d330,d330_79,d330_80,1).
bond(test,d330,d330_80,d330_81,1).
bond(test,d330,d330_80,d330_82,1).
bond(test,d330,d330_80,d330_83,1).
bond(test,d330,d330_13,d330_84,1).
bond(test,d330,d330_84,d330_85,1).
bond(test,d330,d330_85,d330_86,1).
bond(test,d330,d330_85,d330_87,1).
bond(test,d330,d330_85,d330_88,1).
bond(test,d331,d331_1,d331_2,7).
bond(test,d331,d331_2,d331_3,7).
bond(test,d331,d331_3,d331_4,7).
bond(test,d331,d331_4,d331_5,7).
bond(test,d331,d331_5,d331_6,7).
bond(test,d331,d331_6,d331_1,7).
bond(test,d331,d331_1,d331_7,1).
bond(test,d331,d331_2,d331_8,1).
bond(test,d331,d331_5,d331_9,1).
bond(test,d331,d331_6,d331_10,1).
bond(test,d331,d331_3,d331_11,1).
bond(test,d331,d331_11,d331_12,2).
bond(test,d331,d331_12,d331_13,1).
bond(test,d331,d331_13,d331_14,1).
bond(test,d331,d331_14,d331_4,1).
bond(test,d331,d331_13,d331_15,2).
bond(test,d331,d331_11,d331_16,1).
bond(test,d331,d331_12,d331_17,1).
bond(test,d332,d332_1,d332_2,1).
bond(test,d332,d332_2,d332_3,1).
bond(test,d332,d332_2,d332_4,1).
bond(test,d332,d332_2,d332_5,1).
bond(test,d332,d332_3,d332_6,1).
bond(test,d332,d332_6,d332_7,1).
bond(test,d332,d332_6,d332_8,1).
bond(test,d332,d332_6,d332_9,1).
bond(test,d332,d332_3,d332_10,1).
bond(test,d332,d332_3,d332_11,1).
bond(test,d332,d332_1,d332_12,1).
bond(test,d333,d333_1,d333_2,7).
bond(test,d333,d333_2,d333_3,7).
bond(test,d333,d333_3,d333_4,7).
bond(test,d333,d333_4,d333_5,7).
bond(test,d333,d333_5,d333_6,7).
bond(test,d333,d333_6,d333_1,7).
bond(test,d333,d333_2,d333_7,1).
bond(test,d333,d333_3,d333_8,1).
bond(test,d333,d333_5,d333_9,1).
bond(test,d333,d333_10,d333_11,7).
bond(test,d333,d333_11,d333_12,7).
bond(test,d333,d333_12,d333_13,7).
bond(test,d333,d333_13,d333_14,7).
bond(test,d333,d333_14,d333_15,7).
bond(test,d333,d333_15,d333_10,7).
bond(test,d333,d333_11,d333_16,1).
bond(test,d333,d333_12,d333_17,1).
bond(test,d333,d333_15,d333_18,1).
bond(test,d333,d333_4,d333_10,1).
bond(test,d333,d333_6,d333_19,1).
bond(test,d333,d333_19,d333_20,1).
bond(test,d333,d333_19,d333_21,1).
bond(test,d333,d333_19,d333_22,1).
bond(test,d333,d333_1,d333_23,1).
bond(test,d333,d333_14,d333_24,1).
bond(test,d333,d333_24,d333_25,1).
bond(test,d333,d333_24,d333_26,1).
bond(test,d333,d333_24,d333_27,1).
bond(test,d333,d333_13,d333_28,1).
bond(test,d333,d333_23,d333_29,1).
bond(test,d333,d333_23,d333_30,1).
bond(test,d333,d333_28,d333_31,1).
bond(test,d333,d333_28,d333_32,1).
bond(test,d334,d334_1,d334_2,7).
bond(test,d334,d334_2,d334_3,7).
bond(test,d334,d334_3,d334_4,7).
bond(test,d334,d334_4,d334_5,7).
bond(test,d334,d334_5,d334_6,7).
bond(test,d334,d334_6,d334_1,7).
bond(test,d334,d334_1,d334_7,1).
bond(test,d334,d334_3,d334_8,1).
bond(test,d334,d334_4,d334_9,1).
bond(test,d334,d334_2,d334_10,1).
bond(test,d334,d334_6,d334_11,1).
bond(test,d334,d334_5,d334_12,1).
bond(test,d334,d334_12,d334_13,1).
bond(test,d334,d334_13,d334_14,1).
bond(test,d334,d334_13,d334_15,1).
bond(test,d334,d334_13,d334_16,1).
bond(test,d334,d334_14,d334_17,1).
bond(test,d334,d334_14,d334_18,1).
bond(test,d334,d334_14,d334_19,1).
bond(test,d334,d334_12,d334_20,1).
bond(test,d334,d334_20,d334_21,1).
bond(test,d334,d334_20,d334_22,1).
bond(test,d334,d334_20,d334_23,1).
bond(test,d334,d334_21,d334_24,1).
bond(test,d334,d334_21,d334_25,1).
bond(test,d334,d334_21,d334_26,1).
bond(test,d334,d334_10,d334_27,2).
bond(test,d334,d334_10,d334_28,2).
bond(test,d334,d334_11,d334_29,1).
bond(test,d334,d334_17,d334_30,1).
bond(test,d334,d334_24,d334_31,1).
bond(test,d335,d335_1,d335_2,7).
bond(test,d335,d335_2,d335_3,7).
bond(test,d335,d335_3,d335_4,7).
bond(test,d335,d335_4,d335_5,7).
bond(test,d335,d335_5,d335_6,7).
bond(test,d335,d335_6,d335_1,7).
bond(test,d335,d335_1,d335_7,1).
bond(test,d335,d335_3,d335_8,1).
bond(test,d335,d335_4,d335_9,1).
bond(test,d335,d335_6,d335_10,1).
bond(test,d335,d335_5,d335_11,1).
bond(test,d335,d335_2,d335_12,1).
bond(test,d335,d335_12,d335_13,2).
bond(test,d335,d335_12,d335_14,2).
bond(test,d335,d335_11,d335_15,1).
bond(test,d335,d335_11,d335_16,1).
bond(test,d336,d336_1,d336_2,7).
bond(test,d336,d336_2,d336_3,7).
bond(test,d336,d336_3,d336_4,7).
bond(test,d336,d336_4,d336_5,7).
bond(test,d336,d336_5,d336_6,7).
bond(test,d336,d336_6,d336_1,7).
bond(test,d336,d336_1,d336_7,1).
bond(test,d336,d336_2,d336_8,1).
bond(test,d336,d336_3,d336_9,1).
bond(test,d336,d336_6,d336_10,1).
bond(test,d336,d336_5,d336_11,1).
bond(test,d336,d336_11,d336_12,1).
bond(test,d336,d336_12,d336_13,1).
bond(test,d336,d336_12,d336_14,1).
bond(test,d336,d336_12,d336_15,1).
bond(test,d336,d336_4,d336_16,1).
bond(test,d336,d336_16,d336_17,2).
bond(test,d336,d336_16,d336_18,2).
bond(test,d337,d337_1,d337_2,1).
bond(test,d337,d337_2,d337_3,1).
bond(test,d337,d337_3,d337_4,1).
bond(test,d337,d337_3,d337_5,1).
bond(test,d337,d337_3,d337_6,1).
bond(test,d337,d337_1,d337_7,1).
bond(test,d337,d337_1,d337_8,1).
bond(test,d337,d337_1,d337_9,1).
bond(test,d337,d337_2,d337_10,1).
bond(test,d337,d337_2,d337_11,1).
has_property(test,d296,salmonella,n).
has_property(test,d296,cytogen_ca,p).
has_property(test,d296,cytogen_sce,p).
has_property(test,d297,salmonella,n).
has_property(test,d297,cytogen_ca,p).
has_property(test,d297,cytogen_sce,n).
has_property(test,d298,salmonella,n).
has_property(test,d298,cytogen_ca,n).
has_property(test,d299,salmonella,n).
has_property(test,d299,mouse_lymph,p).
has_property(test,d299,cytogen_ca,p).
has_property(test,d299,cytogen_sce,p).
has_property(test,d300,salmonella,n).
has_property(test,d300,cytogen_ca,p).
has_property(test,d300,cytogen_sce,p).
has_property(test,d302,salmonella,n).
has_property(test,d302,mouse_lymph,p).
has_property(test,d302,cytogen_ca,n).
has_property(test,d302,cytogen_sce,p).
has_property(test,d303,salmonella,n).
has_property(test,d303,cytogen_ca,p).
has_property(test,d303,cytogen_sce,n).
has_property(test,d304,salmonella,n).
has_property(test,d304,cytogen_ca,n).
has_property(test,d304,cytogen_sce,n).
has_property(test,d305,salmonella,n).
has_property(test,d305,cytogen_ca,n).
has_property(test,d305,cytogen_sce,n).
has_property(test,d306,salmonella,n).
has_property(test,d306,cytogen_ca,p).
has_property(test,d307,salmonella,n).
has_property(test,d307,mouse_lymph,n).
has_property(test,d307,cytogen_ca,n).
has_property(test,d307,cytogen_sce,n).
has_property(test,d307,micronuc_m,n).
has_property(test,d307,micronuc_f,n).
has_property(test,d308,salmonella,n).
has_property(test,d308,cytogen_ca,n).
has_property(test,d308,cytogen_sce,p).
has_property(test,d308,micronuc_m,n).
has_property(test,d308,micronuc_f,n).
has_property(test,d309,salmonella,n).
has_property(test,d309,mouse_lymph,n).
has_property(test,d309,cytogen_ca,n).
has_property(test,d309,cytogen_sce,n).
has_property(test,d311,salmonella,n).
has_property(test,d311,cytogen_ca,p).
has_property(test,d311,cytogen_sce,p).
has_property(test,d312,salmonella,n).
has_property(test,d312,cytogen_ca,n).
has_property(test,d312,cytogen_sce,p).
has_property(test,d312,micronuc_m,p).
has_property(test,d312,micronuc_f,n).
has_property(test,d312,chromaberr,n).
has_property(test,d313,salmonella,n).
has_property(test,d313,cytogen_ca,n).
has_property(test,d313,cytogen_sce,p).
has_property(test,d314,salmonella,n).
has_property(test,d314,cytogen_ca,n).
has_property(test,d314,cytogen_sce,p).
has_property(test,d315,salmonella,n).
has_property(test,d315,mouse_lymph,n).
has_property(test,d315,cytogen_ca,n).
has_property(test,d315,cytogen_sce,p).
has_property(test,d315,chromaberr,n).
has_property(test,d316,salmonella,p).
has_property(test,d316,mouse_lymph,p).
has_property(test,d316,cytogen_ca,n).
has_property(test,d316,cytogen_sce,p).
has_property(test,d318,salmonella,n).
has_property(test,d318,cytogen_ca,n).
has_property(test,d318,cytogen_sce,n).
has_property(test,d319,salmonella,p).
has_property(test,d319,micronuc_m,p).
has_property(test,d319,micronuc_f,p).
has_property(test,d320,salmonella,p).
has_property(test,d320,cytogen_ca,p).
has_property(test,d320,cytogen_sce,p).
has_property(test,d320,micronuc_m,n).
has_property(test,d320,micronuc_f,n).
has_property(test,d322,salmonella,n).
has_property(test,d322,salmonella_reduc,n).
has_property(test,d322,cytogen_ca,n).
has_property(test,d322,cytogen_sce,p).
has_property(test,d323,salmonella,p).
has_property(test,d323,cytogen_ca,n).
has_property(test,d323,cytogen_sce,n).
has_property(test,d324,salmonella,p).
has_property(test,d324,cytogen_ca,n).
has_property(test,d324,cytogen_sce,p).
has_property(test,d325,salmonella,p).
has_property(test,d325,mouse_lymph,p).
has_property(test,d325,cytogen_ca,n).
has_property(test,d325,cytogen_sce,n).
has_property(test,d326,salmonella,n).
has_property(test,d326,cytogen_ca,p).
has_property(test,d326,cytogen_sce,p).
has_property(test,d327,salmonella,n).
has_property(test,d327,cytogen_ca,n).
has_property(test,d327,cytogen_sce,n).
has_property(test,d327,micronuc_m,p).
has_property(test,d327,micronuc_f,p).
has_property(test,d328,salmonella,p).
has_property(test,d328,salmonella_n,n).
has_property(test,d328,cytogen_ca,n).
has_property(test,d328,cytogen_sce,n).
has_property(test,d329,salmonella,p).
has_property(test,d329,salmonella_reduc,p).
has_property(test,d329,cytogen_ca,n).
has_property(test,d329,cytogen_sce,n).
has_property(test,d330,salmonella,n).
has_property(test,d330,salmonella_reduc,p).
has_property(test,d330,cytogen_ca,n).
has_property(test,d330,cytogen_sce,n).
has_property(test,d331,salmonella,p).
has_property(test,d331,cytogen_ca,p).
has_property(test,d331,cytogen_sce,p).
has_property(test,d331,micronuc_m,n).
has_property(test,d331,micronuc_f,n).
has_property(test,d332,salmonella,p).
has_property(test,d332,mouse_lymph,p).
has_property(test,d332,drosophila_slrl,p).
has_property(test,d332,drosophila_rt,p).
has_property(test,d332,cytogen_ca,p).
has_property(test,d332,cytogen_sce,p).
has_property(test,d333,salmonella,p).
has_property(test,d333,salmonella_reduc,p).
has_property(test,d333,mouse_lymph,p).
has_property(test,d333,cytogen_ca,p).
has_property(test,d333,cytogen_sce,p).
has_property(test,d335,salmonella,p).
has_property(test,d335,mouse_lymph,p).
has_property(test,d335,cytogen_ca,p).
has_property(test,d335,cytogen_sce,p).
has_property(test,d336,salmonella,p).
has_property(test,d336,mouse_lymph,p).
has_property(test,d336,cytogen_ca,p).
has_property(test,d336,cytogen_sce,p).
has_property(test,d337,salmonella,p).
has_property(test,d337,mouse_lymph,p).
has_property(test,d337,cytogen_ca,p).
has_property(test,d337,cytogen_sce,p).
has_property(test,d334,salmonella,p).
has_property(test,d334,drosophila_slrl,p).
has_property(test,d334,drosophila_rt,n).
has_property(test,d334,cytogen_ca,n).
has_property(test,d334,cytogen_sce,p).
ind(test,d314,amino,3).
ind(test,d322,amino,2).
ind(test,d325,amino,2).
ind(test,d330,amino,2).
ind(test,d333,amino,2).
ind(test,d318,amino,2).
ind(test,d335,amino,1).
ind(test,d326,di8,1).
ind(test,d314,di10,3).
ind(test,d315,di10,2).
ind(test,d322,di10,2).
ind(test,d325,di10,2).
ind(test,d330,di10,2).
ind(test,d333,di10,2).
ind(test,d298,di10,1).
ind(test,d318,di10,2).
ind(test,d324,di10,1).
ind(test,d326,di10,1).
ind(test,d334,di10,1).
ind(test,d335,di10,1).
ind(test,d322,di23,1).
ind(test,d323,di23,1).
ind(test,d329,di23,2).
ind(test,d330,di23,2).
ind(test,d324,di23,1).
ind(test,d324,di48,1).
ind(test,d326,di48,1).
ind(test,d326,di67a,1).
ind(test,d306,halide10,2).
ind(test,d332,halide10,2).
ind(test,d337,halide10,3).
ind(test,d302,halide10,1).
ind(test,d319,halide10,1).
ind(test,d311,methoxy,1).
ind(test,d306,di227,6).
ind(test,d332,di227,6).
ind(test,d319,di227,3).
ind(test,d306,methanol,2).
ind(test,d332,methanol,1).
ind(test,d297,methanol,3).
ind(test,d309,methanol,2).
ind(test,d334,methanol,2).
ind(test,d320,nitro,1).
ind(test,d323,nitro,1).
ind(test,d336,nitro,1).
ind(test,d303,nitro,1).
ind(test,d324,nitro,2).
ind(test,d334,nitro,1).
ind(test,d335,nitro,1).
ashby_alert(test,d314,amino,[d314_5, d314_22, d314_25, d314_26]).
ashby_alert(test,d314,amino,[d314_1, d314_23, d314_27, d314_28]).
ashby_alert(test,d314,amino,[d314_8, d314_24, d314_29, d314_30]).
ashby_alert(test,d318,amino,[d318_1, d318_23, d318_27, d318_28]).
ashby_alert(test,d318,amino,[d318_17, d318_25, d318_29, d318_30]).
ashby_alert(test,d322,amino,[d322_27, d322_28, d322_70, d322_71]).
ashby_alert(test,d322,amino,[d322_50, d322_54, d322_78, d322_79]).
ashby_alert(test,d325,amino,[d325_4, d325_11, d325_13, d325_14]).
ashby_alert(test,d325,amino,[d325_2, d325_12, d325_15, d325_16]).
ashby_alert(test,d330,amino,[d330_28, d330_41, d330_42, d330_43]).
ashby_alert(test,d330,amino,[d330_61, d330_69, d330_77, d330_78]).
ashby_alert(test,d333,amino,[d333_1, d333_23, d333_29, d333_30]).
ashby_alert(test,d333,amino,[d333_13, d333_28, d333_31, d333_32]).
ashby_alert(test,d335,amino,[d335_5, d335_11, d335_15, d335_16]).
ashby_alert(test,d326,di8,[d326_2, d326_12, d326_15, d326_13, d326_17, d326_16, d326_20, d326_19, d326_18]).
ashby_alert(test,d298,di10,[d298_14, d298_4, d298_23, d298_13]).
ashby_alert(test,d312,di10,[d312_2, d312_3, d312_17, d312_1]).
ashby_alert(test,d312,di10,[d312_6, d312_5, d312_12, d312_1]).
ashby_alert(test,d314,di10,[d314_22, d314_5, d314_26, d314_25]).
ashby_alert(test,d314,di10,[d314_23, d314_28, d314_27, d314_1]).
ashby_alert(test,d314,di10,[d314_24, d314_8, d314_30, d314_29]).
ashby_alert(test,d315,di10,[d315_24, d315_30, d315_27, d315_23]).
ashby_alert(test,d315,di10,[d315_26, d315_31, d315_27, d315_25]).
ashby_alert(test,d318,di10,[d318_23, d318_28, d318_27, d318_1]).
ashby_alert(test,d318,di10,[d318_25, d318_30, d318_29, d318_17]).
ashby_alert(test,d322,di10,[d322_28, d322_71, d322_70, d322_27]).
ashby_alert(test,d322,di10,[d322_54, d322_79, d322_78, d322_50]).
ashby_alert(test,d324,di10,[d324_27, d324_28, d324_26, d324_20]).
ashby_alert(test,d325,di10,[d325_11, d325_4, d325_14, d325_13]).
ashby_alert(test,d325,di10,[d325_12, d325_2, d325_16, d325_15]).
ashby_alert(test,d326,di10,[d326_12, d326_2, d326_15, d326_13]).
ashby_alert(test,d327,di10,[d327_11, d327_38, d327_12, d327_1]).
ashby_alert(test,d330,di10,[d330_41, d330_43, d330_42, d330_28]).
ashby_alert(test,d330,di10,[d330_69, d330_78, d330_77, d330_61]).
ashby_alert(test,d333,di10,[d333_23, d333_30, d333_29, d333_1]).
ashby_alert(test,d333,di10,[d333_28, d333_32, d333_31, d333_13]).
ashby_alert(test,d334,di10,[d334_12, d334_5, d334_20, d334_13]).
ashby_alert(test,d335,di10,[d335_11, d335_5, d335_16, d335_15]).
ashby_alert(test,d322,di23,[d322_18, d322_16, d322_15, d322_10]).
ashby_alert(test,d323,di23,[d323_12, d323_11, d323_10, d323_4]).
ashby_alert(test,d324,di23,[d324_37, d324_36, d324_35, d324_14]).
ashby_alert(test,d327,di23,[d327_25, d327_24, d327_23, d327_16]).
ashby_alert(test,d329,di23,[d329_29, d329_28, d329_27, d329_5]).
ashby_alert(test,d329,di23,[d329_57, d329_56, d329_55, d329_41]).
ashby_alert(test,d330,di23,[d330_21, d330_20, d330_19, d330_1]).
ashby_alert(test,d330,di23,[d330_51, d330_50, d330_49, d330_12]).
ashby_alert(test,d324,di48,[d324_20, d324_27, d324_28]).
ashby_alert(test,d326,di48,[d326_2, d326_12, d326_15]).
ashby_alert(test,d326,di67a,[d326_2, d326_12, d326_13, d326_16, d326_17, d326_20, d326_19, d326_18, d326_15]).
ashby_alert(test,d302,halide10,[d302_2, d302_1]).
ashby_alert(test,d306,halide10,[d306_7, d306_6]).
ashby_alert(test,d306,halide10,[d306_15, d306_14]).
ashby_alert(test,d319,halide10,[d319_2, d319_1]).
ashby_alert(test,d332,halide10,[d332_7, d332_6]).
ashby_alert(test,d332,halide10,[d332_3, d332_10]).
ashby_alert(test,d337,halide10,[d337_7, d337_1]).
ashby_alert(test,d337,halide10,[d337_4, d337_3]).
ashby_alert(test,d337,halide10,[d337_2, d337_10]).
ashby_alert(test,d311,methoxy,[d311_30, d311_31, d311_34, d311_33, d311_32]).
ashby_alert(test,d306,di227,[d306_6, d306_8, d306_3, d306_7]).
ashby_alert(test,d306,di227,[d306_6, d306_9, d306_8, d306_7]).
ashby_alert(test,d306,di227,[d306_6, d306_9, d306_3, d306_7]).
ashby_alert(test,d306,di227,[d306_14, d306_17, d306_16, d306_15]).
ashby_alert(test,d306,di227,[d306_14, d306_3, d306_16, d306_15]).
ashby_alert(test,d306,di227,[d306_14, d306_3, d306_17, d306_15]).
ashby_alert(test,d319,di227,[d319_1, d319_4, d319_3, d319_2]).
ashby_alert(test,d319,di227,[d319_1, d319_5, d319_3, d319_2]).
ashby_alert(test,d319,di227,[d319_1, d319_5, d319_4, d319_2]).
ashby_alert(test,d332,di227,[d332_3, d332_6, d332_11, d332_10]).
ashby_alert(test,d332,di227,[d332_3, d332_6, d332_2, d332_10]).
ashby_alert(test,d332,di227,[d332_3, d332_2, d332_11, d332_10]).
ashby_alert(test,d332,di227,[d332_6, d332_8, d332_3, d332_7]).
ashby_alert(test,d332,di227,[d332_6, d332_9, d332_8, d332_7]).
ashby_alert(test,d332,di227,[d332_6, d332_9, d332_3, d332_7]).
ashby_alert(test,d297,methanol,[d297_39, d297_44, d297_43, d297_42, d297_45]).
ashby_alert(test,d297,methanol,[d297_77, d297_82, d297_81, d297_80, d297_83]).
ashby_alert(test,d297,methanol,[d297_121, d297_126, d297_125, d297_124, d297_127]).
ashby_alert(test,d306,methanol,[d306_2, d306_5, d306_4, d306_1, d306_18]).
ashby_alert(test,d306,methanol,[d306_10, d306_13, d306_12, d306_11, d306_19]).
ashby_alert(test,d309,methanol,[d309_1, d309_5, d309_4, d309_3, d309_9]).
ashby_alert(test,d309,methanol,[d309_2, d309_8, d309_7, d309_6, d309_10]).
ashby_alert(test,d332,methanol,[d332_2, d332_5, d332_4, d332_1, d332_12]).
ashby_alert(test,d334,methanol,[d334_14, d334_19, d334_18, d334_17, d334_30]).
ashby_alert(test,d334,methanol,[d334_21, d334_26, d334_25, d334_24, d334_31]).
ashby_alert(test,d303,nitro,[d303_2, d303_12, d303_13, d303_14]).
ashby_alert(test,d320,nitro,[d320_2, d320_13, d320_15, d320_16]).
ashby_alert(test,d323,nitro,[d323_5, d323_33, d323_35, d323_36]).
ashby_alert(test,d324,nitro,[d324_17, d324_30, d324_31, d324_32]).
ashby_alert(test,d324,nitro,[d324_39, d324_51, d324_52, d324_53]).
ashby_alert(test,d334,nitro,[d334_2, d334_10, d334_27, d334_28]).
ashby_alert(test,d335,nitro,[d335_2, d335_12, d335_13, d335_14]).
ashby_alert(test,d336,nitro,[d336_4, d336_16, d336_17, d336_18]).
six_ring(test,d296,[d296_1, d296_2, d296_3, d296_4, d296_5, d296_6]).
six_ring(test,d296,[d296_3, d296_4, d296_14, d296_13, d296_12, d296_11]).
non_ar_6c_ring(test,d296,[d296_1, d296_2, d296_3, d296_4, d296_5, d296_6]).
non_ar_6c_ring(test,d296,[d296_3, d296_4, d296_14, d296_13, d296_12, d296_11]).
five_ring(test,d297,[d297_1, d297_2, d297_7, d297_4, d297_3]).
non_ar_hetero_5_ring(test,d297,[d297_1, d297_2, d297_7, d297_4, d297_3]).
ester(test,d297,[d297_163, d297_160, d297_164, d297_165, d297_166]).
ester(test,d297,[d297_163, d297_160, d297_164, d297_165, d297_167]).
ester(test,d297,[d297_163, d297_160, d297_164, d297_165, d297_168]).
alcohol(test,d297,[d297_42, d297_45, d297_39]).
alcohol(test,d297,[d297_80, d297_83, d297_77]).
alcohol(test,d297,[d297_124, d297_127, d297_121]).
ether(test,d297,[d297_4, d297_3, d297_7]).
ether(test,d297,[d297_8, d297_1, d297_11]).
ether(test,d297,[d297_14, d297_10, d297_17]).
ether(test,d297,[d297_21, d297_18, d297_24]).
ether(test,d297,[d297_28, d297_25, d297_31]).
ether(test,d297,[d297_35, d297_32, d297_38]).
ether(test,d297,[d297_42, d297_39, d297_45]).
ether(test,d297,[d297_46, d297_2, d297_49]).
ether(test,d297,[d297_52, d297_48, d297_55]).
ether(test,d297,[d297_59, d297_56, d297_62]).
ether(test,d297,[d297_66, d297_63, d297_69]).
ether(test,d297,[d297_73, d297_70, d297_76]).
ether(test,d297,[d297_80, d297_77, d297_83]).
ether(test,d297,[d297_87, d297_84, d297_93]).
ether(test,d297,[d297_89, d297_129, d297_86]).
ether(test,d297,[d297_96, d297_92, d297_99]).
ether(test,d297,[d297_103, d297_100, d297_106]).
ether(test,d297,[d297_110, d297_107, d297_113]).
ether(test,d297,[d297_117, d297_114, d297_120]).
ether(test,d297,[d297_124, d297_121, d297_127]).
ether(test,d297,[d297_132, d297_128, d297_135]).
ether(test,d297,[d297_139, d297_136, d297_142]).
ether(test,d297,[d297_146, d297_143, d297_149]).
ether(test,d297,[d297_153, d297_150, d297_156]).
ether(test,d297,[d297_160, d297_157, d297_163]).
ketone(test,d297,[d297_164, d297_163, d297_160, d297_165]).
methyl(test,d297,[d297_203, d297_200, d297_206, d297_207, d297_208]).
six_ring(test,d298,[d298_1, d298_2, d298_3, d298_4, d298_5, d298_6]).
six_ring(test,d298,[d298_3, d298_4, d298_14, d298_13, d298_12, d298_11]).
six_ring(test,d298,[d298_12, d298_13, d298_18, d298_17, d298_16, d298_15]).
non_ar_hetero_6_ring(test,d298,[d298_3, d298_4, d298_14, d298_13, d298_12, d298_11]).
non_ar_6c_ring(test,d298,[d298_1, d298_2, d298_3, d298_4, d298_5, d298_6]).
non_ar_6c_ring(test,d298,[d298_12, d298_13, d298_18, d298_17, d298_16, d298_15]).
sulfide(test,d298,[d298_11, d298_12, d298_3]).
methyl(test,d298,[d298_27, d298_24, d298_28, d298_29, d298_30]).
methyl(test,d298,[d298_33, d298_31, d298_34, d298_35, d298_36]).
methyl(test,d298,[d298_37, d298_31, d298_38, d298_39, d298_40]).
six_ring(test,d299,[d299_1, d299_2, d299_3, d299_4, d299_5, d299_6]).
non_ar_6c_ring(test,d299,[d299_1, d299_2, d299_3, d299_4, d299_5, d299_6]).
phenol(test,d299,[d299_11, d299_13, d299_5]).
phenol(test,d299,[d299_12, d299_14, d299_3]).
five_ring(test,d300,[d300_1, d300_2, d300_8, d300_6, d300_3]).
non_ar_hetero_5_ring(test,d300,[d300_1, d300_2, d300_8, d300_6, d300_3]).
ester(test,d300,[d300_3, d300_6, d300_7, d300_1, d300_2]).
ester(test,d300,[d300_3, d300_6, d300_7, d300_1, d300_4]).
ester(test,d300,[d300_3, d300_6, d300_7, d300_1, d300_5]).
ether(test,d300,[d300_6, d300_3, d300_8]).
ketone(test,d300,[d300_7, d300_3, d300_1, d300_6]).
ar_halide(test,d302,[d302_1, d302_2]).
alkyl_halide(test,d302,[d302_1, d302_2]).
ester(test,d302,[d302_3, d302_6, d302_7, d302_2, d302_4]).
ester(test,d302,[d302_3, d302_6, d302_7, d302_2, d302_5]).
ester(test,d302,[d302_3, d302_6, d302_7, d302_2, d302_1]).
alcohol(test,d302,[d302_6, d302_8, d302_3]).
ether(test,d302,[d302_6, d302_3, d302_8]).
ketone(test,d302,[d302_7, d302_3, d302_2, d302_6]).
six_ring(test,d303,[d303_1, d303_2, d303_3, d303_4, d303_5, d303_6]).
non_ar_6c_ring(test,d303,[d303_1, d303_2, d303_3, d303_4, d303_5, d303_6]).
phenol(test,d303,[d303_11, d303_15, d303_5]).
nitro(test,d303,[d303_12, d303_2, d303_13, d303_14]).
six_ring(test,d304,[d304a_1, d304a_2, d304a_3, d304a_4, d304a_5, d304a_6]).
six_ring(test,d304,[d304b_1, d304b_2, d304b_3, d304b_4, d304b_5, d304b_6]).
non_ar_6c_ring(test,d304,[d304a_1, d304a_2, d304a_3, d304a_4, d304a_5, d304a_6]).
non_ar_6c_ring(test,d304,[d304b_1, d304b_2, d304b_3, d304b_4, d304b_5, d304b_6]).
alcohol(test,d304,[d304a_24, d304a_26, d304a_10]).
alcohol(test,d304,[d304a_25, d304a_27, d304a_10]).
alcohol(test,d304,[d304b_19, d304b_21, d304b_9]).
alcohol(test,d304,[d304b_20, d304b_22, d304b_9]).
ether(test,d304,[d304a_9, d304a_10, d304a_3]).
ether(test,d304,[d304a_24, d304a_10, d304a_26]).
ether(test,d304,[d304a_25, d304a_10, d304a_27]).
ether(test,d304,[d304b_8, d304b_3, d304b_9]).
methyl(test,d304,[d304a_12, d304a_2, d304a_13, d304a_14, d304a_15]).
methyl(test,d304,[d304a_16, d304a_4, d304a_17, d304a_18, d304a_19]).
methyl(test,d304,[d304a_20, d304a_6, d304a_21, d304a_22, d304a_23]).
methyl(test,d304,[d304b_11, d304b_2, d304b_12, d304b_13, d304b_14]).
methyl(test,d304,[d304b_15, d304b_6, d304b_16, d304b_17, d304b_18]).
methyl(test,d304,[d304b_23, d304b_1, d304b_24, d304b_25, d304b_26]).
six_ring(test,d305,[d305_1, d305_2, d305_3, d305_4, d305_5, d305_6]).
six_ring(test,d305,[d305_11, d305_14, d305_15, d305_16, d305_17, d305_18]).
non_ar_6c_ring(test,d305,[d305_1, d305_2, d305_3, d305_4, d305_5, d305_6]).
non_ar_6c_ring(test,d305,[d305_11, d305_14, d305_15, d305_16, d305_17, d305_18]).
ar_halide(test,d305,[d305_25, d305_2]).
phenol(test,d305,[d305_24, d305_26, d305_5]).
ar_halide(test,d306,[d306_7, d306_6]).
ar_halide(test,d306,[d306_15, d306_14]).
alkyl_halide(test,d306,[d306_7, d306_6]).
alkyl_halide(test,d306,[d306_15, d306_14]).
alcohol(test,d306,[d306_1, d306_18, d306_2]).
alcohol(test,d306,[d306_11, d306_19, d306_10]).
ether(test,d306,[d306_11, d306_10, d306_19]).
alcohol(test,d307,[d307_2, d307_15, d307_1]).
ether(test,d307,[d307_2, d307_1, d307_15]).
methyl(test,d307,[d307_3, d307_1, d307_4, d307_5, d307_6]).
methyl(test,d307,[d307_7, d307_1, d307_10, d307_8, d307_9]).
methyl(test,d307,[d307_11, d307_1, d307_12, d307_13, d307_14]).
six_ring(test,d308,[d308_1, d308_2, d308_3, d308_4, d308_5, d308_6]).
six_ring(test,d308,[d308_3, d308_4, d308_14, d308_13, d308_12, d308_11]).
non_ar_hetero_6_ring(test,d308,[d308_3, d308_4, d308_14, d308_13, d308_12, d308_11]).
non_ar_6c_ring(test,d308,[d308_1, d308_2, d308_3, d308_4, d308_5, d308_6]).
ester(test,d308,[d308_13, d308_14, d308_19, d308_12, d308_17]).
ester(test,d308,[d308_13, d308_14, d308_19, d308_12, d308_18]).
ester(test,d308,[d308_13, d308_14, d308_19, d308_12, d308_11]).
ether(test,d308,[d308_14, d308_13, d308_4]).
ketone(test,d308,[d308_19, d308_13, d308_12, d308_14]).
alcohol(test,d309,[d309_3, d309_9, d309_1]).
alcohol(test,d309,[d309_6, d309_10, d309_2]).
ether(test,d309,[d309_3, d309_1, d309_9]).
six_ring(test,d311,[d311_1, d311_2, d311_3, d311_4, d311_5, d311_6]).
six_ring(test,d311,[d311_13, d311_14, d311_15, d311_16, d311_17, d311_18]).
non_ar_hetero_6_ring(test,d311,[d311_13, d311_14, d311_15, d311_16, d311_17, d311_18]).
non_ar_6c_ring(test,d311,[d311_1, d311_2, d311_3, d311_4, d311_5, d311_6]).
ester(test,d311,[d311_28, d311_30, d311_35, d311_12, d311_18]).
ester(test,d311,[d311_28, d311_30, d311_35, d311_12, d311_29]).
ester(test,d311,[d311_28, d311_30, d311_35, d311_12, d311_4]).
ether(test,d311,[d311_30, d311_28, d311_31]).
ketone(test,d311,[d311_35, d311_28, d311_12, d311_30]).
amine(test,d311,[d311_17, d311_36, d311_18, d311_16]).
methoxy(test,d311,[d311_31, d311_30, d311_32, d311_33, d311_34]).
methyl(test,d311,[d311_31, d311_30, d311_32, d311_33, d311_34]).
five_ring(test,d312,[d312_3, d312_4, d312_9, d312_8, d312_7]).
non_ar_hetero_5_ring(test,d312,[d312_3, d312_4, d312_9, d312_8, d312_7]).
six_ring(test,d312,[d312_1, d312_2, d312_3, d312_4, d312_5, d312_6]).
non_ar_hetero_6_ring(test,d312,[d312_1, d312_2, d312_3, d312_4, d312_5, d312_6]).
ketone(test,d312,[d312_11, d312_5, d312_4, d312_6]).
ketone(test,d312,[d312_16, d312_1, d312_2, d312_6]).
methyl(test,d312,[d312_12, d312_6, d312_13, d312_14, d312_15]).
methyl(test,d312,[d312_17, d312_2, d312_18, d312_19, d312_20]).
six_ring(test,d313,[d313_1, d313_2, d313_3, d313_4, d313_5, d313_6]).
six_ring(test,d313,[d313_9, d313_10, d313_11, d313_12, d313_13, d313_14]).
non_ar_6c_ring(test,d313,[d313_1, d313_2, d313_3, d313_4, d313_5, d313_6]).
non_ar_6c_ring(test,d313,[d313_9, d313_10, d313_11, d313_12, d313_13, d313_14]).
phenol(test,d313,[d313_20, d313_25, d313_1]).
phenol(test,d313,[d313_28, d313_29, d313_12]).
sulfide(test,d313,[d313_17, d313_4, d313_9]).
ether(test,d313,[d313_20, d313_1, d313_25]).
ether(test,d313,[d313_28, d313_12, d313_29]).
methyl(test,d313,[d313_19, d313_18, d313_53, d313_54, d313_55]).
methyl(test,d313,[d313_21, d313_3, d313_22, d313_23, d313_24]).
methyl(test,d313,[d313_27, d313_26, d313_50, d313_51, d313_52]).
methyl(test,d313,[d313_30, d313_10, d313_31, d313_32, d313_33]).
methyl(test,d313,[d313_34, d313_18, d313_35, d313_36, d313_37]).
methyl(test,d313,[d313_38, d313_18, d313_39, d313_40, d313_41]).
methyl(test,d313,[d313_42, d313_26, d313_43, d313_44, d313_45]).
methyl(test,d313,[d313_46, d313_26, d313_47, d313_48, d313_49]).
six_ring(test,d314,[d314_1, d314_2, d314_3, d314_4, d314_5, d314_6]).
six_ring(test,d314,[d314_3, d314_4, d314_10, d314_9, d314_8, d314_7]).
six_ring(test,d314,[d314_11, d314_12, d314_13, d314_14, d314_15, d314_16]).
non_ar_hetero_6_ring(test,d314,[d314_1, d314_2, d314_3, d314_4, d314_5, d314_6]).
non_ar_hetero_6_ring(test,d314,[d314_3, d314_4, d314_10, d314_9, d314_8, d314_7]).
non_ar_6c_ring(test,d314,[d314_11, d314_12, d314_13, d314_14, d314_15, d314_16]).
amine(test,d314,[d314_22, d314_25, d314_26, d314_5]).
amine(test,d314,[d314_23, d314_27, d314_28, d314_1]).
amine(test,d314,[d314_24, d314_29, d314_30, d314_8]).
five_ring(test,d315,[d315_23, d315_24, d315_27, d315_26, d315_25]).
non_ar_hetero_5_ring(test,d315,[d315_23, d315_24, d315_27, d315_26, d315_25]).
six_ring(test,d315,[d315_1, d315_2, d315_3, d315_4, d315_5, d315_6]).
six_ring(test,d315,[d315_12, d315_13, d315_14, d315_15, d315_16, d315_17]).
non_ar_6c_ring(test,d315,[d315_1, d315_2, d315_3, d315_4, d315_5, d315_6]).
non_ar_6c_ring(test,d315,[d315_12, d315_13, d315_14, d315_15, d315_16, d315_17]).
ketone(test,d315,[d315_28, d315_25, d315_23, d315_26]).
ketone(test,d315,[d315_29, d315_27, d315_24, d315_26]).
amine(test,d315,[d315_24, d315_30, d315_27, d315_23]).
amine(test,d315,[d315_26, d315_31, d315_27, d315_25]).
six_ring(test,d316,[d316_1, d316_2, d316_3, d316_4, d316_5, d316_6]).
non_ar_6c_ring(test,d316,[d316_1, d316_2, d316_3, d316_4, d316_5, d316_6]).
ar_halide(test,d316,[d316_12, d316_4]).
ar_halide(test,d316,[d316_13, d316_3]).
ar_halide(test,d316,[d316_14, d316_2]).
ar_halide(test,d316,[d316_15, d316_1]).
ar_halide(test,d316,[d316_16, d316_6]).
ether(test,d316,[d316_7, d316_5, d316_8]).
methoxy(test,d316,[d316_8, d316_7, d316_10, d316_11, d316_9]).
methyl(test,d316,[d316_8, d316_7, d316_10, d316_11, d316_9]).
amine(test,d317,[d317_1, d317_3, d317_4, d317_2]).
six_ring(test,d318,[d318_1, d318_2, d318_3, d318_4, d318_5, d318_6]).
six_ring(test,d318,[d318_13, d318_15, d318_16, d318_17, d318_18, d318_19]).
non_ar_6c_ring(test,d318,[d318_1, d318_2, d318_3, d318_4, d318_5, d318_6]).
non_ar_6c_ring(test,d318,[d318_13, d318_15, d318_16, d318_17, d318_18, d318_19]).
alcohol(test,d318,[d318_31, d318_32, d318_24]).
alcohol(test,d318,[d318_33, d318_38, d318_26]).
sulfide(test,d318,[d318_24, d318_31, d318_5]).
sulfide(test,d318,[d318_26, d318_19, d318_33]).
ether(test,d318,[d318_31, d318_24, d318_32]).
ether(test,d318,[d318_33, d318_26, d318_38]).
amine(test,d318,[d318_23, d318_27, d318_28, d318_1]).
amine(test,d318,[d318_25, d318_29, d318_30, d318_17]).
sulfo(test,d318,[d318_24, d318_31, d318_34, d318_35, d318_5]).
sulfo(test,d318,[d318_26, d318_33, d318_36, d318_37, d318_19]).
ar_halide(test,d319,[d319_2, d319_1]).
methyl(test,d319,[d319_1, d319_2, d319_3, d319_4, d319_5]).
six_ring(test,d320,[d320_1, d320_2, d320_3, d320_4, d320_5, d320_6]).
non_ar_6c_ring(test,d320,[d320_1, d320_2, d320_3, d320_4, d320_5, d320_6]).
alcohol(test,d320,[d320_12, d320_17, d320_11]).
ether(test,d320,[d320_12, d320_11, d320_17]).
ketone(test,d320,[d320_14, d320_11, d320_12, d320_5]).
nitro(test,d320,[d320_13, d320_2, d320_15, d320_16]).
five_ring(test,d322,[d322_14, d322_16, d322_18, d322_17, d322_23]).
five_ring(test,d322,[d322_39, d322_40, d322_44, d322_45, d322_47]).
non_ar_hetero_5_ring(test,d322,[d322_14, d322_16, d322_18, d322_17, d322_23]).
non_ar_hetero_5_ring(test,d322,[d322_39, d322_40, d322_44, d322_45, d322_47]).
six_ring(test,d322,[d322_1, d322_2, d322_38, d322_40, d322_39, d322_37]).
six_ring(test,d322,[d322_1, d322_2, d322_3, d322_4, d322_5, d322_6]).
six_ring(test,d322,[d322_7, d322_8, d322_9, d322_10, d322_11, d322_12]).
six_ring(test,d322,[d322_10, d322_11, d322_13, d322_14, d322_16, d322_15]).
six_ring(test,d322,[d322_17, d322_18, d322_19, d322_20, d322_21, d322_22]).
six_ring(test,d322,[d322_21, d322_22, d322_27, d322_26, d322_25, d322_24]).
six_ring(test,d322,[d322_41, d322_42, d322_43, d322_44, d322_45, d322_46]).
six_ring(test,d322,[d322_41, d322_46, d322_50, d322_51, d322_52, d322_53]).
non_ar_hetero_6_ring(test,d322,[d322_1, d322_2, d322_38, d322_40, d322_39, d322_37]).
non_ar_hetero_6_ring(test,d322,[d322_10, d322_11, d322_13, d322_14, d322_16, d322_15]).
non_ar_6c_ring(test,d322,[d322_1, d322_2, d322_3, d322_4, d322_5, d322_6]).
non_ar_6c_ring(test,d322,[d322_7, d322_8, d322_9, d322_10, d322_11, d322_12]).
non_ar_6c_ring(test,d322,[d322_17, d322_18, d322_19, d322_20, d322_21, d322_22]).
non_ar_6c_ring(test,d322,[d322_21, d322_22, d322_27, d322_26, d322_25, d322_24]).
non_ar_6c_ring(test,d322,[d322_41, d322_42, d322_43, d322_44, d322_45, d322_46]).
non_ar_6c_ring(test,d322,[d322_41, d322_46, d322_50, d322_51, d322_52, d322_53]).
alcohol(test,d322,[d322_31, d322_72, d322_29]).
alcohol(test,d322,[d322_32, d322_73, d322_30]).
alcohol(test,d322,[d322_49, d322_75, d322_48]).
alcohol(test,d322,[d322_56, d322_80, d322_55]).
sulfide(test,d322,[d322_29, d322_19, d322_31]).
sulfide(test,d322,[d322_30, d322_25, d322_32]).
sulfide(test,d322,[d322_48, d322_43, d322_49]).
sulfide(test,d322,[d322_55, d322_52, d322_56]).
ether(test,d322,[d322_13, d322_11, d322_14]).
ether(test,d322,[d322_23, d322_14, d322_17]).
ether(test,d322,[d322_31, d322_29, d322_72]).
ether(test,d322,[d322_32, d322_30, d322_73]).
ether(test,d322,[d322_47, d322_39, d322_45]).
ether(test,d322,[d322_49, d322_48, d322_75]).
ether(test,d322,[d322_56, d322_55, d322_80]).
amine(test,d322,[d322_28, d322_70, d322_71, d322_27]).
amine(test,d322,[d322_54, d322_78, d322_79, d322_50]).
sulfo(test,d322,[d322_29, d322_31, d322_35, d322_36, d322_19]).
sulfo(test,d322,[d322_30, d322_32, d322_33, d322_34, d322_25]).
sulfo(test,d322,[d322_48, d322_49, d322_59, d322_60, d322_43]).
sulfo(test,d322,[d322_55, d322_56, d322_57, d322_58, d322_52]).
six_ring(test,d323,[d323_1, d323_2, d323_3, d323_4, d323_5, d323_6]).
six_ring(test,d323,[d323_12, d323_13, d323_14, d323_15, d323_16, d323_17]).
six_ring(test,d323,[d323_13, d323_14, d323_23, d323_22, d323_21, d323_20]).
non_ar_6c_ring(test,d323,[d323_1, d323_2, d323_3, d323_4, d323_5, d323_6]).
non_ar_6c_ring(test,d323,[d323_12, d323_13, d323_14, d323_15, d323_16, d323_17]).
non_ar_6c_ring(test,d323,[d323_13, d323_14, d323_23, d323_22, d323_21, d323_20]).
phenol(test,d323,[d323_32, d323_34, d323_17]).
ether(test,d323,[d323_32, d323_17, d323_34]).
methyl(test,d323,[d323_28, d323_1, d323_29, d323_30, d323_31]).
nitro(test,d323,[d323_33, d323_5, d323_35, d323_36]).
six_ring(test,d324,[d324_1, d324_2, d324_3, d324_4, d324_5, d324_6]).
six_ring(test,d324,[d324_3, d324_4, d324_14, d324_13, d324_12, d324_11]).
six_ring(test,d324,[d324_16, d324_17, d324_18, d324_19, d324_20, d324_21]).
six_ring(test,d324,[d324_37, d324_38, d324_39, d324_40, d324_41, d324_42]).
non_ar_6c_ring(test,d324,[d324_1, d324_2, d324_3, d324_4, d324_5, d324_6]).
non_ar_6c_ring(test,d324,[d324_3, d324_4, d324_14, d324_13, d324_12, d324_11]).
non_ar_6c_ring(test,d324,[d324_16, d324_17, d324_18, d324_19, d324_20, d324_21]).
non_ar_6c_ring(test,d324,[d324_37, d324_38, d324_39, d324_40, d324_41, d324_42]).
phenol(test,d324,[d324_33, d324_34, d324_13]).
ether(test,d324,[d324_33, d324_13, d324_34]).
ether(test,d324,[d324_46, d324_42, d324_47]).
ketone(test,d324,[d324_29, d324_26, d324_12, d324_27]).
amine(test,d324,[d324_27, d324_28, d324_26, d324_20]).
methoxy(test,d324,[d324_47, d324_46, d324_48, d324_49, d324_50]).
methyl(test,d324,[d324_47, d324_46, d324_48, d324_49, d324_50]).
nitro(test,d324,[d324_30, d324_17, d324_31, d324_32]).
nitro(test,d324,[d324_51, d324_39, d324_52, d324_53]).
six_ring(test,d325,[d325_1, d325_2, d325_3, d325_4, d325_5, d325_6]).
non_ar_6c_ring(test,d325,[d325_1, d325_2, d325_3, d325_4, d325_5, d325_6]).
phenol(test,d325,[d325_10, d325_17, d325_5]).
amine(test,d325,[d325_11, d325_13, d325_14, d325_4]).
amine(test,d325,[d325_12, d325_15, d325_16, d325_2]).
six_ring(test,d326,[d326_1, d326_2, d326_3, d326_4, d326_5, d326_6]).
non_ar_6c_ring(test,d326,[d326_1, d326_2, d326_3, d326_4, d326_5, d326_6]).
phenol(test,d326,[d326_11, d326_14, d326_5]).
ketone(test,d326,[d326_16, d326_13, d326_12, d326_17]).
amine(test,d326,[d326_12, d326_15, d326_2, d326_13]).
methyl(test,d326,[d326_17, d326_13, d326_18, d326_19, d326_20]).
six_ring(test,d327,[d327_1, d327_2, d327_3, d327_4, d327_5, d327_6]).
six_ring(test,d327,[d327_13, d327_14, d327_15, d327_16, d327_17, d327_18]).
six_ring(test,d327,[d327_25, d327_26, d327_27, d327_28, d327_29, d327_30]).
non_ar_hetero_6_ring(test,d327,[d327_1, d327_2, d327_3, d327_4, d327_5, d327_6]).
non_ar_6c_ring(test,d327,[d327_13, d327_14, d327_15, d327_16, d327_17, d327_18]).
non_ar_6c_ring(test,d327,[d327_25, d327_26, d327_27, d327_28, d327_29, d327_30]).
phenol(test,d327,[d327_37, d327_42, d327_28]).
alcohol(test,d327,[d327_35, d327_41, d327_34]).
sulfide(test,d327,[d327_12, d327_11, d327_13]).
ether(test,d327,[d327_35, d327_34, d327_41]).
ether(test,d327,[d327_37, d327_28, d327_42]).
ketone(test,d327,[d327_36, d327_34, d327_29, d327_35]).
amine(test,d327,[d327_11, d327_38, d327_12, d327_1]).
five_ring(test,d328,[d328a_1, d328a_2, d328a_3, d328a_4, d328a_5]).
non_ar_5c_ring(test,d328,[d328a_1, d328a_2, d328a_3, d328a_4, d328a_5]).
six_ring(test,d329,[d329_1, d329_2, d329_3, d329_4, d329_5, d329_6]).
six_ring(test,d329,[d329_3, d329_4, d329_12, d329_11, d329_10, d329_9]).
six_ring(test,d329,[d329_29, d329_30, d329_31, d329_32, d329_33, d329_34]).
six_ring(test,d329,[d329_38, d329_39, d329_40, d329_41, d329_42, d329_43]).
six_ring(test,d329,[d329_57, d329_58, d329_59, d329_60, d329_61, d329_62]).
six_ring(test,d329,[d329_69, d329_70, d329_71, d329_72, d329_73, d329_74]).
non_ar_6c_ring(test,d329,[d329_1, d329_2, d329_3, d329_4, d329_5, d329_6]).
non_ar_6c_ring(test,d329,[d329_3, d329_4, d329_12, d329_11, d329_10, d329_9]).
non_ar_6c_ring(test,d329,[d329_29, d329_30, d329_31, d329_32, d329_33, d329_34]).
non_ar_6c_ring(test,d329,[d329_38, d329_39, d329_40, d329_41, d329_42, d329_43]).
non_ar_6c_ring(test,d329,[d329_57, d329_58, d329_59, d329_60, d329_61, d329_62]).
non_ar_6c_ring(test,d329,[d329_69, d329_70, d329_71, d329_72, d329_73, d329_74]).
phenol(test,d329,[d329_15, d329_16, d329_6]).
alcohol(test,d329,[d329_18, d329_25, d329_17]).
alcohol(test,d329,[d329_20, d329_26, d329_19]).
sulfide(test,d329,[d329_17, d329_10, d329_18]).
sulfide(test,d329,[d329_19, d329_12, d329_20]).
sulfide(test,d329,[d329_68, d329_67, d329_69]).
ether(test,d329,[d329_18, d329_17, d329_25]).
ether(test,d329,[d329_20, d329_19, d329_26]).
ether(test,d329,[d329_67, d329_60, d329_68]).
methyl(test,d329,[d329_47, d329_34, d329_48, d329_49, d329_50]).
methyl(test,d329,[d329_51, d329_42, d329_52, d329_53, d329_54]).
methyl(test,d329,[d329_79, d329_72, d329_80, d329_81, d329_82]).
sulfo(test,d329,[d329_17, d329_18, d329_23, d329_24, d329_10]).
sulfo(test,d329,[d329_19, d329_20, d329_21, d329_22, d329_12]).
sulfo(test,d329,[d329_68, d329_67, d329_83, d329_84, d329_69]).
six_ring(test,d330,[d330_1, d330_2, d330_3, d330_4, d330_5, d330_6]).
six_ring(test,d330,[d330_10, d330_11, d330_12, d330_13, d330_14, d330_15]).
six_ring(test,d330,[d330_21, d330_22, d330_23, d330_24, d330_25, d330_26]).
six_ring(test,d330,[d330_23, d330_24, d330_31, d330_30, d330_29, d330_28]).
six_ring(test,d330,[d330_51, d330_52, d330_53, d330_54, d330_55, d330_56]).
six_ring(test,d330,[d330_54, d330_55, d330_61, d330_60, d330_59, d330_58]).
non_ar_6c_ring(test,d330,[d330_1, d330_2, d330_3, d330_4, d330_5, d330_6]).
non_ar_6c_ring(test,d330,[d330_10, d330_11, d330_12, d330_13, d330_14, d330_15]).
non_ar_6c_ring(test,d330,[d330_21, d330_22, d330_23, d330_24, d330_25, d330_26]).
non_ar_6c_ring(test,d330,[d330_23, d330_24, d330_31, d330_30, d330_29, d330_28]).
non_ar_6c_ring(test,d330,[d330_51, d330_52, d330_53, d330_54, d330_55, d330_56]).
non_ar_6c_ring(test,d330,[d330_54, d330_55, d330_61, d330_60, d330_59, d330_58]).
phenol(test,d330,[d330_39, d330_40, d330_22]).
phenol(test,d330,[d330_68, d330_76, d330_56]).
alcohol(test,d330,[d330_35, d330_38, d330_34]).
alcohol(test,d330,[d330_45, d330_48, d330_44]).
alcohol(test,d330,[d330_67, d330_74, d330_64]).
alcohol(test,d330,[d330_71, d330_75, d330_70]).
sulfide(test,d330,[d330_34, d330_26, d330_35]).
sulfide(test,d330,[d330_44, d330_30, d330_45]).
sulfide(test,d330,[d330_64, d330_52, d330_67]).
sulfide(test,d330,[d330_70, d330_59, d330_71]).
ether(test,d330,[d330_35, d330_34, d330_38]).
ether(test,d330,[d330_39, d330_22, d330_40]).
ether(test,d330,[d330_45, d330_44, d330_48]).
ether(test,d330,[d330_67, d330_64, d330_74]).
ether(test,d330,[d330_68, d330_56, d330_76]).
ether(test,d330,[d330_71, d330_70, d330_75]).
ether(test,d330,[d330_79, d330_6, d330_80]).
ether(test,d330,[d330_84, d330_13, d330_85]).
amine(test,d330,[d330_41, d330_42, d330_43, d330_28]).
amine(test,d330,[d330_69, d330_77, d330_78, d330_61]).
methoxy(test,d330,[d330_80, d330_79, d330_81, d330_82, d330_83]).
methoxy(test,d330,[d330_85, d330_84, d330_86, d330_87, d330_88]).
methyl(test,d330,[d330_80, d330_79, d330_81, d330_82, d330_83]).
methyl(test,d330,[d330_85, d330_84, d330_86, d330_87, d330_88]).
sulfo(test,d330,[d330_34, d330_35, d330_36, d330_37, d330_26]).
sulfo(test,d330,[d330_44, d330_45, d330_46, d330_47, d330_30]).
sulfo(test,d330,[d330_64, d330_65, d330_66, d330_67, d330_52]).
sulfo(test,d330,[d330_70, d330_71, d330_72, d330_73, d330_59]).
six_ring(test,d331,[d331_1, d331_2, d331_3, d331_4, d331_5, d331_6]).
six_ring(test,d331,[d331_3, d331_4, d331_14, d331_13, d331_12, d331_11]).
non_ar_hetero_6_ring(test,d331,[d331_3, d331_4, d331_14, d331_13, d331_12, d331_11]).
non_ar_6c_ring(test,d331,[d331_1, d331_2, d331_3, d331_4, d331_5, d331_6]).
ester(test,d331,[d331_13, d331_14, d331_15, d331_12, d331_17]).
ether(test,d331,[d331_14, d331_13, d331_4]).
ketone(test,d331,[d331_15, d331_13, d331_12, d331_14]).
ar_halide(test,d332,[d332_7, d332_6]).
ar_halide(test,d332,[d332_10, d332_3]).
alkyl_halide(test,d332,[d332_7, d332_6]).
alkyl_halide(test,d332,[d332_10, d332_3]).
alcohol(test,d332,[d332_1, d332_12, d332_2]).
six_ring(test,d333,[d333_1, d333_2, d333_3, d333_4, d333_5, d333_6]).
six_ring(test,d333,[d333_10, d333_11, d333_12, d333_13, d333_14, d333_15]).
non_ar_6c_ring(test,d333,[d333_1, d333_2, d333_3, d333_4, d333_5, d333_6]).
non_ar_6c_ring(test,d333,[d333_10, d333_11, d333_12, d333_13, d333_14, d333_15]).
amine(test,d333,[d333_23, d333_29, d333_30, d333_1]).
amine(test,d333,[d333_28, d333_31, d333_32, d333_13]).
methyl(test,d333,[d333_19, d333_6, d333_20, d333_21, d333_22]).
methyl(test,d333,[d333_24, d333_14, d333_25, d333_26, d333_27]).
six_ring(test,d334,[d334_1, d334_2, d334_3, d334_4, d334_5, d334_6]).
non_ar_6c_ring(test,d334,[d334_1, d334_2, d334_3, d334_4, d334_5, d334_6]).
phenol(test,d334,[d334_11, d334_29, d334_6]).
alcohol(test,d334,[d334_17, d334_30, d334_14]).
alcohol(test,d334,[d334_24, d334_31, d334_21]).
ether(test,d334,[d334_17, d334_14, d334_30]).
ether(test,d334,[d334_24, d334_21, d334_31]).
nitro(test,d334,[d334_10, d334_2, d334_27, d334_28]).
six_ring(test,d335,[d335_1, d335_2, d335_3, d335_4, d335_5, d335_6]).
non_ar_6c_ring(test,d335,[d335_1, d335_2, d335_3, d335_4, d335_5, d335_6]).
amine(test,d335,[d335_11, d335_15, d335_16, d335_5]).
nitro(test,d335,[d335_12, d335_2, d335_13, d335_14]).
six_ring(test,d336,[d336_1, d336_2, d336_3, d336_4, d336_5, d336_6]).
non_ar_6c_ring(test,d336,[d336_1, d336_2, d336_3, d336_4, d336_5, d336_6]).
ether(test,d336,[d336_11, d336_12, d336_5]).
methoxy(test,d336,[d336_12, d336_11, d336_13, d336_14, d336_15]).
methyl(test,d336,[d336_12, d336_11, d336_13, d336_14, d336_15]).
nitro(test,d336,[d336_16, d336_4, d336_17, d336_18]).
ar_halide(test,d337,[d337_4, d337_3]).
ar_halide(test,d337,[d337_7, d337_1]).
ar_halide(test,d337,[d337_10, d337_2]).
alkyl_halide(test,d337,[d337_4, d337_3]).
alkyl_halide(test,d337,[d337_7, d337_1]).
alkyl_halide(test,d337,[d337_10, d337_2]).
