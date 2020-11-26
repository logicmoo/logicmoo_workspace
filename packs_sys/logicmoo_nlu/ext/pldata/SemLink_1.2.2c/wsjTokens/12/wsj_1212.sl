nw/wsj/12/wsj_1212.parse 1 12 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 10:1-ARG0=Agent;Seller 12:0-rel 15:1-ARGM-MNR 
nw/wsj/12/wsj_1212.parse 1 14 gold buy-v 13.5.1 Commerce_buy buy.01 4 ----- 10:1-ARG0=Agent;Buyer 14:0-rel 15:1-ARGM-MNR 
nw/wsj/12/wsj_1212.parse 1 21 gold trade-v 13.6-1 Exchange trade.01 null ----- 20:1*27:1-ARG0=Agent 21:0-rel 22:1-ARGM-MNR 
nw/wsj/12/wsj_1212.parse 1 28 gold behave-v 29.6 NF behave.01 1 ----- 18:1-ARGM-ADV 27:1-ARG0=Agent 28:0-rel 29:1-ARG1=Attribute 
nw/wsj/12/wsj_1212.parse 7 1 gold need-v 32.1-1-1 Required_event need.01 2 ----- 0:1-ARG0=Pivot 1:0-rel 2:2-ARG1=Theme 
nw/wsj/12/wsj_1212.parse 7 4 gold get-v 26.6.2 IN get.04 3 ----- 0:1*2:1-ARG0=Agent 4:0-rel 5:2-ARG1=Patient 
nw/wsj/12/wsj_1212.parse 7 17 gold want-v 32.1-1-1 Desiring want.01 1 ----- 16:1-ARG0=Pivot;Experiencer 17:0-rel 18:2-ARG1=Theme;Event/Focal_participant 
