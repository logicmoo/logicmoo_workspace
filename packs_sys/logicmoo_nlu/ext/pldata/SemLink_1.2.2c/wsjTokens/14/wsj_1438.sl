nw/wsj/14/wsj_1438.parse 0 16 gold join-v 22.1-2-1 Cause_to_amalgamate join.01 2 ----- 14:1-ARG0=Agent;Agent 15:0-ARGM-MOD 16:0-rel 17:1-ARG1=Patient;Part_1 20:1-ARGM-DIR 28:1-ARGM-TMP 
nw/wsj/14/wsj_1438.parse 2 8 gold trade-v 13.6-1 Exchange trade.01 1 ----- 0:1-ARG0=Agent 8:0-rel 9:2-ARG1=Theme 
nw/wsj/14/wsj_1438.parse 3 1 gold invest-v 13.4.2 NF invest.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARGM-ADV 3:1-ARG2=Recipient 
nw/wsj/14/wsj_1438.parse 4 7 gold hold-v 15.1-1 NF hold.01 null ----- 6:0-ARG1=Theme 7:0-rel 8:0-ARG0=Agent 
nw/wsj/14/wsj_1438.parse 4 11 gold join-v 22.1-2-1 Cause_to_amalgamate join.01 2 ----- 0:2-ARG0=Agent;Agent 10:0-ARGM-MOD 11:0-rel 12:0,13:0,14:0-ARG1=Patient;Part_1 15:0-ARGM-TMP 16:1-ARGM-MNR 
nw/wsj/14/wsj_1438.parse 5 2 gold begin-v 55.1-1 Activity_start begin.01 2 ----- 0:1-ARG0=Agent;Agent 2:0-rel 3:2-ARG1=Theme;Activity 
nw/wsj/14/wsj_1438.parse 5 4 gold trade-v 13.6-1 Exchange trade.01 null ----- 0:1*3:1-ARG0=Agent 4:0-rel 5:1-ARGM-MNR 
nw/wsj/14/wsj_1438.parse 6 16 gold use-v 105 IN use.01 1 ----- 10:2*17:1-ARG1 16:0-rel 18:2-ARG2 10:2*17:1-LINK-PSV 
nw/wsj/14/wsj_1438.parse 6 26 gold start-v 55.1-1 Activity_start start.01 2 ----- 0:2-ARG0=Agent;Agent 26:0-rel 27:1-ARG1=Theme;Activity 29:1-ARG2=Instrument 
nw/wsj/14/wsj_1438.parse 7 12 gold start-v 55.1-1 Activity_start start.01 2 ----- 0:3,13:2-ARG1=Theme;Activity 12:0-rel 
nw/wsj/14/wsj_1438.parse 7 14 gold trade-v 13.6-1 Exchange trade.01 null ----- 0:3*13:1-ARG1=Theme 14:0-rel 15:1-ARGM-MNR 
nw/wsj/14/wsj_1438.parse 8 8 gold start-v 55.1-1 Activity_start start.01 2 ----- 0:2,9:2-ARG1=Theme;Activity 8:0-rel 
nw/wsj/14/wsj_1438.parse 8 10 gold trade-v 13.6-1 Exchange trade.01 null ----- 0:2*9:1-ARG1=Theme 10:0-rel 11:1-ARGM-MNR 
nw/wsj/14/wsj_1438.parse 10 9 gold put-v 9.1-2 IN put.01 null ----- 8:1-ARG0=Agent 9:0-rel 
nw/wsj/14/wsj_1438.parse 10 11 gold call-v 60 IN call.03 null ----- 8:1-ARG0=Agent 11:0-rel 
nw/wsj/14/wsj_1438.parse 10 20 gold start-v 55.1-1 Activity_start start.01 2 ----- 0:1-ARGM-DIS 2:1-ARGM-LOC 8:5*21:2-ARG1=Theme;Activity 20:0-rel 
nw/wsj/14/wsj_1438.parse 10 22 gold trade-v 13.6-1 Exchange trade.01 null ----- 8:5*21:1-ARG1=Theme 22:0-rel 
nw/wsj/14/wsj_1438.parse 11 4 gold make-v 26.1-1 Intentionally_create make.01 2 ----- 0:2-ARG0=Agent;Creator 4:0-rel 5:2-ARG1=Product;Created_entity 
nw/wsj/14/wsj_1438.parse 12 1 gold give-v 13.1-1 Giving give.01 4 ----- 0:1-ARG0=Agent;Donor 1:0-rel 2:1-ARG2=Recipient;Recipient 4:3-ARG1=Theme;Theme 
nw/wsj/14/wsj_1438.parse 12 16 gold buy-v 13.5.1 Commerce_buy buy.01 1 ----- 2:1*14:1-ARG0=Agent;Buyer 16:0-rel 19:1-ARG1=Theme;Goods 21:1-ARG3=Asset 25:1-ARGM-TMP 2:1*14:1-LINK-PRO 
nw/wsj/14/wsj_1438.parse 12 18 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 2:1*14:1-ARG0=Agent;Seller 18:0-rel 19:1-ARG1=Theme;Goods 21:1-ARG3 25:1-ARGM-TMP 2:1*14:1-LINK-PRO 
nw/wsj/14/wsj_1438.parse 12 23 gold set-v 9.1-2 IN set.01 null ----- 23:0-rel 24:0-ARG1=Theme 
nw/wsj/14/wsj_1438.parse 12 27 gold set-v 9.1-2 IN set.01 null ----- 27:0-rel 28:0-ARG1=Theme 
