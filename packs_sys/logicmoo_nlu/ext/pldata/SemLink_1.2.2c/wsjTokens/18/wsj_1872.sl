nw/wsj/18/wsj_1872.parse 0 3 gold plan-v 62 Purpose plan.01 null ----- 0:1*4:1-ARG0=Experiencer;Agent 3:0-rel 4:2-ARG1 
nw/wsj/18/wsj_1872.parse 0 6 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1*4:1-ARG0=Agent;Seller 6:0-rel 7:3-ARG1=Theme;Goods 16:1-ARGM-TMP 17:1-ARGM-LOC 
nw/wsj/18/wsj_1872.parse 1 5 gold date-v 25.3 NF date.01 null ----- 0:1*6:1-ARG1=Destination 3:0-ARGM-MOD 5:0-rel 7:2-ARG2=Theme 
nw/wsj/18/wsj_1872.parse 2 16 gold receive-v 13.5.2 Receiving receive.01 null ----- 0:2*17:1-ARG1=Theme;Theme 14:0-ARGM-MOD 16:0-rel 18:1-ARGM-TMP 22:1-ARGM-TMP 23:1-ARGM-LOC 
nw/wsj/18/wsj_1872.parse 3 16 gold use-v 105 IN use.01 null ----- 0:2-ARG0 15:0-ARGM-MOD 16:0-rel 17:1-ARG1 19:2-ARG2 
nw/wsj/18/wsj_1872.parse 3 21 gold merge-v 22.1-1-1 Amalgamation merge.01 1 ----- 0:2*19:1-ARG0=Agent 21:0-rel 22:1*27:1-ARG1=Patient;Part_1 
nw/wsj/18/wsj_1872.parse 4 10 gold borrow-v 13.5.2 NF borrow.01 null ----- 2:2*6:1*7:1*11:1-ARG1=Theme 10:0-rel 12:1*13:2-ARGM-PRP 2:2*6:1-LINK-SLC 
nw/wsj/18/wsj_1872.parse 5 12 gold borrow-v 13.5.2 NF borrow.01 1 ----- 0:1*4:3*13:1-ARG1=Theme 10:1-ARGM-TMP 12:0-rel 14:1-ARG0=Agent 
