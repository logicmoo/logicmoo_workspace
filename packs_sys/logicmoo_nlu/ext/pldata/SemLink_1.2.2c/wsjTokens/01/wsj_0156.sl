nw/wsj/01/wsj_0156.parse 0 7 gold fail-v 75-1-1 NF fail.01 null ----- 0:2*6:1-ARG1 7:0-rel 8:2-ARG2 
nw/wsj/01/wsj_0156.parse 0 10 gold find-v 13.5.1 IN find.01 null ----- 0:2*8:1-ARG0=Agent 10:0-rel 11:2-ARG1=Theme 0:2*8:1-LINK-PRO 
nw/wsj/01/wsj_0156.parse 0 21 gold say-v 37.7-1 IN say.01 null ----- 0:2*6:1-ARG0=Agent 5:1-ARGM-TMP 21:0-rel 22:1-ARG1=Topic 
nw/wsj/01/wsj_0156.parse 0 25 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 23:1*37:1-ARG0=Agent;Seller 24:0-ARGM-MOD 25:0-rel 26:2-ARG1=Theme;Goods 31:1-ARG2=Recipient 
nw/wsj/01/wsj_0156.parse 0 39 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 null ----- 23:1*37:1-ARG0=Cause 39:0-rel 40:2-ARG1=Patient 
nw/wsj/01/wsj_0156.parse 1 25 gold announce-v 37.7-1 Statement announce.01 null ----- 21:1*23:1*39:1-ARGM-TMP 24:1-ARG0=Agent;Speaker 25:0-rel 26:1-ARG1=Topic;Message 21:1*23:1-LINK-SLC 
nw/wsj/01/wsj_0156.parse 2 2 gold say-v 37.7-1 IN say.01 null ----- 0:0-ARGM-DIS 1:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/01/wsj_0156.parse 2 9 gold find-v 13.5.1 IN find.01 null ----- 4:1*7:1-ARG0=Agent 9:0-rel 10:2-ARG1=Theme 
nw/wsj/01/wsj_0156.parse 2 18 gold consider-v 29.9-1-1-1 Categorization consider.01 null ----- 17:1-ARG0=Agent;Cognizer 18:0-rel 19:2-ARG1=Theme;Item 
nw/wsj/01/wsj_0156.parse 5 6 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:1*4:1-ARG0=Agent;Buyer 6:0-rel 7:2-ARG1=Theme;Goods 18:1-ARGM-TMP 
nw/wsj/01/wsj_0156.parse 5 21 gold say-v 37.7-1 IN say.01 null ----- 0:2*23:1-ARG1=Topic 21:0-rel 24:2-ARG0=Agent 
nw/wsj/01/wsj_0156.parse 6 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/01/wsj_0156.parse 6 4 gold hope-v 32.2-1 Desiring hope.01 1 ----- 3:1*5:1-ARG0=Pivot 4:0-rel 5:2-ARG1=Theme 
nw/wsj/01/wsj_0156.parse 6 8 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 null ----- 3:1*5:1-ARG0=Cause 7:1-ARGM-ADV 8:0-rel 9:2-ARG1=Patient 13:1-ARG4=Goal 
