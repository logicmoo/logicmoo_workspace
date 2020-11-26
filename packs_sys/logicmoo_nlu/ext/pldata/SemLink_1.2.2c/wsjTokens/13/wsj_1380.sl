nw/wsj/13/wsj_1380.parse 0 10 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 2 ----- 0:2-ARG1 10:0-rel 11:1-ARG4 
nw/wsj/13/wsj_1380.parse 0 14 gold record-v 25.4 Recording record.01 null ----- 14:0-rel 15:0-ARG1=Theme 
nw/wsj/13/wsj_1380.parse 1 26 gold begin-v 55.1-1 Activity_start begin.01 1 ----- 24:1,27:2-ARG1=Theme;Activity 26:0-rel 32:1-ARGM-TMP 
nw/wsj/13/wsj_1380.parse 1 28 gold make-v 29.3 Causation make.02 3 ----- 24:1*27:1-ARG0=Agent 28:0-rel 29:2-ARG1=Theme 
nw/wsj/13/wsj_1380.parse 2 8 gold trade-v 13.6-1 Exchange trade.01 null ----- 0:2*15:1-ARG1=Theme 8:0-rel 9:1-ARGM-MNR 12:1-ARGM-TMP 
nw/wsj/13/wsj_1380.parse 3 16 gold trade-v 13.6-1 Exchange trade.01 1 ----- 5:1*8:1*20:1-ARGM-TMP 9:2-ARG1=Theme 16:0-rel 17:1-ARGM-MNR 5:1*8:1-LINK-SLC 
nw/wsj/13/wsj_1380.parse 6 3 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/13/wsj_1380.parse 6 12 gold involve-v 86.2-1 NF involve.01 2 ----- 11:1-ARG2 12:0-rel 13:2-ARG1 
nw/wsj/13/wsj_1380.parse 7 14 gold buy-v 13.5.1 Commerce_buy buy.01 1 ----- 6:2*11:1*39:1-ARGM-LOC 13:1-ARG0=Agent;Buyer 14:0-rel 17:2-ARG1=Theme;Goods 6:2*11:1-LINK-SLC 
nw/wsj/13/wsj_1380.parse 7 16 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 6:2*11:1*39:1-ARGM-LOC 13:1-ARG0=Agent;Seller 16:0-rel 17:2-ARG1=Theme;Goods 6:2*11:1-LINK-SLC 
nw/wsj/13/wsj_1380.parse 8 12 gold create-v 27 Creating create.01 1 ----- 10:1-ARG0=Cause 11:0-ARGM-MOD 12:0-rel 13:2-ARG1=Theme 
nw/wsj/13/wsj_1380.parse 9 18 gold involve-v 86.2-1 NF involve.01 2 ----- 12:2-ARG2 18:0-rel 19:2-ARG1 
nw/wsj/13/wsj_1380.parse 10 3 gold report-v 37.7-1 Statement report.01 1 ----- 0:1-ARGM-DIS 2:1-ARG0 3:0-rel 4:3-ARG1 
