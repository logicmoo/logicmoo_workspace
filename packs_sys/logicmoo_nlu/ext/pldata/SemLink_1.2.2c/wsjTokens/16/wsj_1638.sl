nw/wsj/16/wsj_1638.parse 0 5 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 5:0-rel 6:1-ARG1=Topic 
nw/wsj/16/wsj_1638.parse 0 8 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 7:1-ARG0=Agent;Recipient 8:0-rel 9:2-ARG1=Theme;Theme 21:1-ARGM-LOC 
nw/wsj/16/wsj_1638.parse 0 24 gold purchase-v 13.5.2-1 Commerce_buy purchase.01 null ----- 23:1-ARG0=Agent;Buyer 24:0-rel 
nw/wsj/16/wsj_1638.parse 0 28 gold lease-v 13.1-1 IN lease.02 null ----- 7:1*23:1-ARG0=Agent 28:0-rel 7:1*23:1-LINK-PRO 
nw/wsj/16/wsj_1638.parse 0 30 gold value-v 54.4 NF value.01 null ----- 22:1*31:1-ARG1=Theme 30:0-rel 32:1-ARG2=Value 22:1*31:1-LINK-PSV 
nw/wsj/16/wsj_1638.parse 1 5 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 5:0-rel 6:1-ARG1=Topic 
nw/wsj/16/wsj_1638.parse 1 8 gold lease-v 13.1-1 Renting_out lease.02 2 ----- 7:1-ARG0=Agent 8:0-rel 9:1-ARG1=Theme 13:1-ARG2=Recipient 
nw/wsj/16/wsj_1638.parse 2 3 gold hold-v 15.1-1 Manipulation hold.01 null ----- 0:1-ARG0=Agent;Agent 3:0-rel 4:2-ARG1=Theme;Entity 
