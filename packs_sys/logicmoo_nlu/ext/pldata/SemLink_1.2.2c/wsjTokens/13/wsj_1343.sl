nw/wsj/13/wsj_1343.parse 0 5 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 5:0-rel 6:1-ARG1=Topic 
nw/wsj/13/wsj_1343.parse 0 16 gold build-v 26.1-1 Building build.01 1 ----- 7:1*14:1-ARG0=Agent;Agent 16:0-rel 17:2-ARG1=Product;Created_entity 
nw/wsj/13/wsj_1343.parse 0 27 gold make-v 26.1-1 Manufacturing make.01 2 ----- 17:1*24:1*25:1-ARG0=Agent;Manufacturer 27:0-rel 28:2-ARG1=Product;Product 17:1*24:1-LINK-SLC 
nw/wsj/13/wsj_1343.parse 1 10 gold begin-v 55.1-1 Process_start begin.01 1 ----- 0:2-ARG1=Theme;Event 9:0-ARGM-MOD 10:0-rel 11:1-ARGM-TMP 14:1-ARGM-ADV 
nw/wsj/13/wsj_1343.parse 1 16 gold expect-v 62 IN expect.01 1 ----- 15:1,17:2-ARG1=Theme 16:0-rel 
nw/wsj/13/wsj_1343.parse 1 19 gold start-v 55.1-1 Process_start start.01 1 ----- 15:1*17:1-ARG1=Theme;Event 19:0-rel 20:1-ARGM-TMP 
nw/wsj/13/wsj_1343.parse 2 10 gold employ-v 13.5.3 Employing employ.01 1 ----- 0:2-ARGM-TMP 6:1-ARG0=Agent 9:0-ARGM-MOD 10:0-rel 11:2-ARG1=Theme 
nw/wsj/13/wsj_1343.parse 2 15 gold have-v 100 IN have.03 1 ----- 0:2-ARGM-TMP 6:1-ARG0=Pivot 9:0-ARGM-MOD 15:0-rel 16:2-ARG1=Theme 
nw/wsj/13/wsj_1343.parse 3 3 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/13/wsj_1343.parse 3 9 gold make-v 26.1-1 Manufacturing make.01 2 ----- 5:1-ARG0=Agent;Manufacturer 8:0-ARGM-MOD 9:0-rel 10:2-ARG1=Product;Product 
nw/wsj/13/wsj_1343.parse 4 2 gold have-v 100 IN have.03 1 ----- 0:1-ARG0=Pivot 1:1-ARGM-TMP 2:0-rel 3:3-ARG1=Theme 
nw/wsj/13/wsj_1343.parse 4 13 gold make-v 26.1-1 Manufacturing make.01 2 ----- 3:2*10:1*11:1-ARG0=Agent;Manufacturer 13:0-rel 14:1-ARG1=Product;Product 3:2*10:1-LINK-SLC 
nw/wsj/13/wsj_1343.parse 5 2 gold purchase-v 13.5.2-1 Commerce_buy purchase.01 1 ----- 0:1-ARG0=Agent;Buyer 1:1-ARGM-DIS 2:0-rel 3:2-ARG1=Theme;Goods 6:1-ARG2=Source 
