nw/wsj/24/wsj_2426.parse 0 11 gold say-v 37.7-1 IN say.01 1 ----- 0:2-ARG0=Agent 11:0-rel 12:1-ARG1=Topic 
nw/wsj/24/wsj_2426.parse 0 14 gold agree-v 36.1-1 IN agree.01 3 ----- 13:1-ARG0=Agent 14:0-rel 15:2-ARG1=Theme 
nw/wsj/24/wsj_2426.parse 0 17 gold join-v 22.1-2-1 Cause_to_amalgamate join.01 3 ----- 13:1*15:1-ARG0=Agent;Agent 17:0-rel 18:1-ARG1=Patient;Part_1 28:2-ARGM-PNC 
nw/wsj/24/wsj_2426.parse 0 30 gold propose-v 37.7-1 Statement propose.01 1 ----- 13:1*28:1-ARG0=Agent;Speaker 30:0-rel 31:2-ARG1=Topic;Message 13:1*28:1-LINK-PRO 
nw/wsj/24/wsj_2426.parse 1 7 gold buy-v 13.5.1 Commerce_buy buy.01 1 ----- 0:1*5:1-ARG0=Agent;Buyer 7:0-rel 8:2-ARG1=Theme;Goods 29:1-ARGM-TMP 
nw/wsj/24/wsj_2426.parse 2 3 gold replace-v 13.6 IN replace.01 1 ----- 0:1-ARG2=Co-Theme 2:0-ARGM-MOD 3:0-rel 4:3-ARG1=Theme 
nw/wsj/24/wsj_2426.parse 2 9 gold make-v 26.1-1 Manufacturing make.01 2 ----- 4:1*10:1-ARG1=Product;Product 9:0-rel 11:1-ARG0=Agent;Manufacturer 4:1*10:1-LINK-PSV 
nw/wsj/24/wsj_2426.parse 2 26 gold use-v 105 IN use.01 1 ----- 4:2*22:1*27:1-ARG1 23:1*28:1-ARG0 26:0-rel 28:2-ARG2 4:2*22:1-LINK-SLC 
nw/wsj/24/wsj_2426.parse 2 30 gold train-v 13.5.3 Education_teaching train.01 1 ----- 23:1*28:1-ARG0=Agent 30:0-rel 31:1-ARG2=Theme 
nw/wsj/24/wsj_2426.parse 3 1 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/24/wsj_2426.parse 3 8 gold buy-v 13.5.1 Commerce_buy buy.01 1 ----- 3:1-ARG0=Agent;Buyer 6:0-ARGM-MOD 7:1-ARGM-DIS 8:0-rel 9:2-ARG1=Theme;Goods 
nw/wsj/24/wsj_2426.parse 3 17 gold replace-v 13.6 IN replace.01 1 ----- 9:1*14:1*15:1-ARG2=Co-Theme 17:0-rel 18:2-ARG1=Theme 9:1*14:1-LINK-SLC 
nw/wsj/24/wsj_2426.parse 3 21 gold make-v 26.1-1 Manufacturing make.01 2 ----- 18:1*22:1-ARG1=Product;Product 21:0-rel 23:1-ARG0=Agent;Manufacturer 18:1*22:1-LINK-PSV 
nw/wsj/24/wsj_2426.parse 4 8 gold license-v 101 NF license.01 1 ----- 0:1-ARGM-LOC 6:1-ARG0=Agent 7:0-ARGM-MOD 8:0-rel 9:1*10:1-ARG2 10:2-ARG1=Theme 
nw/wsj/24/wsj_2426.parse 4 12 gold build-v 26.1-1 Building build.01 1 ----- 9:1*10:1-ARG0=Agent;Agent 12:0-rel 13:1-ARG1=Product;Created_entity 
nw/wsj/24/wsj_2426.parse 4 24 gold supply-v 13.4.1-1 NF supply.01 1 ----- 0:1-ARGM-LOC 6:1-ARG0=Agent 23:0-ARGM-MOD 24:0-rel 25:1-ARG1=Theme 
nw/wsj/24/wsj_2426.parse 5 2 gold build-v 26.1-1 Building build.01 1 ----- 0:1-ARG0=Agent;Agent 1:0-ARGM-MOD 2:0-rel 3:1-ARG1=Product;Created_entity 15:1-ARGM-LOC 22:2-ARGM-ADV 
nw/wsj/24/wsj_2426.parse 5 26 gold order-v 13.5.1 Request order.02 2 ----- 22:0-ARGM-MOD 23:1-ARG0=Agent;Speaker 26:0-rel 27:1-ARG1=Theme;Message 
nw/wsj/24/wsj_2426.parse 6 8 gold say-v 37.7-1 IN say.01 1 ----- 0:2-ARG0=Agent 8:0-rel 9:1-ARG1=Topic 
nw/wsj/24/wsj_2426.parse 6 21 gold compete-v 36.4-1 Competition compete.01 1 ----- 15:1*18:1-ARG0=Agent 19:0-ARGM-MOD 21:0-rel 22:1-ARG2=Topic 
