nw/wsj/17/wsj_1707.parse 0 28 gold reach-v 13.5.1 NF reach.01 null ----- 0:3-ARG0=Agent 28:0-rel 29:2-ARG1=Theme 
nw/wsj/17/wsj_1707.parse 0 32 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 null ----- 29:1-ARG0=Cause 31:1-ARGM-ADV 32:0-rel 33:2-ARG1=Patient 
nw/wsj/17/wsj_1707.parse 1 10 gold combine-v 22.1-1-1 Amalgamation combine.01 null ----- 10:0-rel 11:0-ARG1=Patient;Part_1 
nw/wsj/17/wsj_1707.parse 1 23 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-LOC 5:2-ARG0=Agent 23:0-rel 24:1-ARG1=Topic 
nw/wsj/17/wsj_1707.parse 1 32 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 null ----- 31:1-ARG0=Cause 32:0-rel 34:3-ARG1=Patient 
nw/wsj/17/wsj_1707.parse 1 44 gold result-v 48.1.1 NF result.01 1 ----- 34:2*41:1*42:1-ARG2=Location 44:0-rel 45:1-ARG1=Theme 34:2*41:1-LINK-SLC 
nw/wsj/17/wsj_1707.parse 1 49 gold own-v 100 NF own.01 null ----- 46:1-ARG0=Pivot 49:0-rel 51:2-ARG1=Theme 
nw/wsj/17/wsj_1707.parse 2 8 gold own-v 100 NF own.01 null ----- 0:1*2:1*7:1-ARG0=Pivot 8:0-rel 9:2-ARG1=Theme 0:1*2:1-LINK-SLC 
nw/wsj/17/wsj_1707.parse 2 18 gold agree-v 36.1-1 IN agree.01 null ----- 0:2*19:1-ARG0=Agent 18:0-rel 19:2-ARG1=Theme 
nw/wsj/17/wsj_1707.parse 2 40 gold know-v 29.5-1 IN know.01 2 ----- 36:1*41:1-ARG1=Theme 40:0-rel 42:1-ARG2=Predicate 36:1*41:1-LINK-PSV 
nw/wsj/17/wsj_1707.parse 3 6 gold force-v 59 NF force.01 null ----- 0:1-ARGM-DIS 3:1-ARG0=Agent 5:0-ARGM-MOD 6:0-rel 7:1*8:1-ARG1=Patient 8:2-ARG2=Result 
nw/wsj/17/wsj_1707.parse 3 10 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 7:1*8:1-ARG0=Agent;Buyer 10:0-rel 11:3-ARG1=Theme;Goods 
nw/wsj/17/wsj_1707.parse 3 20 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 7:1*8:1-ARG0=Agent;Seller 20:0-rel 21:1-ARG1=Theme;Goods 25:1-ARG2=Recipient 27:1-ARG3 
nw/wsj/17/wsj_1707.parse 3 30 gold set-v 9.1-2 IN set.01 null ----- 28:1*31:1-ARG1=Theme 30:0-rel 32:1-ARG0=Agent 28:1*31:1-LINK-PSV 
