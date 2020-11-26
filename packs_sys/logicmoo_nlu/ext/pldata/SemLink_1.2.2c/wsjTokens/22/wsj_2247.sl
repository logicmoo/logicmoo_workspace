nw/wsj/22/wsj_2247.parse 0 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/22/wsj_2247.parse 0 6 gold agree-v 36.1-1 IN agree.01 null ----- 5:1*7:1-ARG0=Agent 6:0-rel 7:2-ARG1=Theme 
nw/wsj/22/wsj_2247.parse 0 9 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 5:1*7:1-ARG0=Agent;Buyer 9:0-rel 10:2-ARG1=Theme;Goods 23:1-ARGM-ADV 
nw/wsj/22/wsj_2247.parse 0 19 gold hold-v 15.1-1 NF hold.01 null ----- 18:0-ARGM-MNR 19:0-rel 20:0,21:0-ARG1=Theme 
nw/wsj/22/wsj_2247.parse 1 12 gold have-v 100 IN have.03 null ----- 0:1-ARGM-MNR 7:3-ARG0=Pivot 12:0-rel 13:1-ARG1=Theme 
nw/wsj/22/wsj_2247.parse 1 17 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 7:3*15:1-ARG0=Agent;Recipient 17:0-rel 18:2-ARG1=Theme;Theme 41:1-ARGM-MNR 7:3*15:1-LINK-PRO 
nw/wsj/22/wsj_2247.parse 1 34 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 28:3*35:1-ARG1=Theme;Goods 34:0-rel 36:2-ARGM-LOC 28:3*35:1-LINK-PSV 
nw/wsj/22/wsj_2247.parse 2 5 gold hold-v 15.1-1 Manipulation hold.01 null ----- 0:1*6:1-ARG1=Theme;Entity 4:1-ARGM-TMP 5:0-rel 7:1-ARG0=Agent;Agent 
nw/wsj/22/wsj_2247.parse 2 22 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 8:2*19:1*20:1-ARG0 21:0-ARGM-MOD 22:0-rel 23:2-ARG1 8:2*19:1-LINK-SLC 
nw/wsj/22/wsj_2247.parse 3 5 gold have-v 100 IN have.03 null ----- 0:1-ARGM-TMP 3:1-ARG0=Pivot 4:0-ARGM-MOD 5:0-rel 6:1,7:1-ARG1=Theme 
nw/wsj/22/wsj_2247.parse 3 24 gold say-v 37.7-1 IN say.01 null ----- 0:2*26:1-ARG1=Topic 22:1-ARG0=Agent 24:0-rel 
nw/wsj/22/wsj_2247.parse 4 4 gold make-v 26.1-1 Manufacturing make.01 null ----- 0:1*2:1*3:1-ARG0=Agent;Manufacturer 4:0-rel 5:3-ARG1=Product;Product 0:1*2:1-LINK-SLC 
nw/wsj/22/wsj_2247.parse 4 9 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 5:2*10:1-ARG1=Theme;Goods 9:0-rel 11:1-ARGM-ADV 12:1-ARGM-MNR 5:2*10:1-LINK-PSV 
nw/wsj/22/wsj_2247.parse 4 23 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 23:0-rel 24:1-ARG1=Topic 
nw/wsj/22/wsj_2247.parse 5 4 gold begin-v 55.1-1 Activity_start begin.01 null ----- 0:1-ARGM-TMP 3:1-ARG0=Agent;Agent 4:0-rel 5:2-ARG1=Theme;Activity 
nw/wsj/22/wsj_2247.parse 5 8 gold price-v 54.4 NF price.01 null ----- 7:0-ARGM-MNR 8:0-rel 9:0-ARG1=Theme 
nw/wsj/22/wsj_2247.parse 5 24 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 16:1*21:1*25:1-ARG1=Theme;Theme 22:1-ARG0=Agent;Recipient 23:1-ARGM-TMP 24:0-rel 16:1*21:1-LINK-SLC 
