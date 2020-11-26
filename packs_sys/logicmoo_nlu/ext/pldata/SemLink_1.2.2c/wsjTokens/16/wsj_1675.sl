nw/wsj/16/wsj_1675.parse 0 3 gold try-v 61 Attempt try.01 null ----- 0:1*4:1-ARG0=Agent 2:0-ARGM-MOD 3:0-rel 4:2-ARG1=Theme 
nw/wsj/16/wsj_1675.parse 0 6 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1*4:1-ARG0=Agent;Seller 6:0-rel 7:2-ARG1=Theme;Goods 19:1-ARGM-MNR 
nw/wsj/16/wsj_1675.parse 0 24 gold say-v 37.7-1 IN say.01 null ----- 24:0-rel 25:1-ARG1=Topic 27:2-ARG0=Agent 
nw/wsj/16/wsj_1675.parse 2 1 gold include-v 65 NF include.01 null ----- 0:1-ARG2=Location 1:0-rel 2:2-ARG1=Theme 
nw/wsj/16/wsj_1675.parse 3 10 gold establish-v 55.5-1 Intentionally_create establish.01 null ----- 5:1-ARG1 10:0-rel 11:2-ARGM-PRP 
nw/wsj/16/wsj_1675.parse 3 15 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 12:1*13:1-ARG0=Agent;Seller 15:0-rel 18:3-ARG1=Theme;Goods 
nw/wsj/16/wsj_1675.parse 3 17 gold merge-v 22.1-1-1 Cause_to_amalgamate merge.01 1 ----- 12:1*13:1-ARG0=Agent;Agent 17:0-rel 18:3-ARG1=Patient;Part_1 
nw/wsj/16/wsj_1675.parse 4 14 gold try-v 61 Attempt try.01 null ----- 0:2*15:1-ARG0=Agent 14:0-rel 15:2-ARG1=Theme 
nw/wsj/16/wsj_1675.parse 4 17 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:2*15:1-ARG0=Agent;Seller 17:0-rel 18:3-ARG1=Theme;Goods 23:1-ARGM-MNR 
nw/wsj/16/wsj_1675.parse 5 4 gold hold-v 15.1-1 Manipulation hold.01 null ----- 0:1*2:1*3:1-ARG0=Agent;Agent 4:0-rel 5:2-ARG1=Theme;Entity 0:1*2:1-LINK-SLC 
nw/wsj/16/wsj_1675.parse 5 24 gold liquidate-v 42.1 Killing liquidate.01 2 ----- 0:2*25:1-ARG1=Patient;Victim 24:0-rel 
nw/wsj/16/wsj_1675.parse 6 2 gold hold-v 15.1-1 Manipulation hold.01 null ----- 0:1*3:1-ARG1=Theme;Entity 2:0-rel 4:1-ARG0=Agent;Agent 0:1*3:1-LINK-PSV 
nw/wsj/16/wsj_1675.parse 6 9 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:2*10:1-ARG1=Theme;Goods 6:0-ARGM-MOD 7:0-ARGM-NEG 9:0-rel 11:1-ARGM-MNR 
nw/wsj/16/wsj_1675.parse 6 15 gold say-v 37.7-1 IN say.01 null ----- 0:3*17:1-ARG1=Topic 13:1-ARG0=Agent 15:0-rel 18:1-ARGM-LOC 
nw/wsj/16/wsj_1675.parse 7 2 gold need-v 32.1-1-1 Required_event need.01 null ----- 1:1*3:1-ARG0=Pivot 2:0-rel 3:2-ARG1=Theme 
nw/wsj/16/wsj_1675.parse 7 5 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 1:1*3:1-ARG0=Agent;Buyer 5:0-rel 6:1-ARG1=Theme;Goods 
nw/wsj/16/wsj_1675.parse 7 13 gold say-v 37.7-1 IN say.01 null ----- 11:1-ARG0=Agent 13:0-rel 0:1-ARG1-DSP=Topic 
nw/wsj/16/wsj_1675.parse 7 19 gold get-v 26.6.2 IN get.05 null ----- 1:1*18:1-ARG0=Agent 19:0-rel 20:1-ARGM-DIR 21:1-ARG1=Patient 1:1*18:1-LINK-PRO 
