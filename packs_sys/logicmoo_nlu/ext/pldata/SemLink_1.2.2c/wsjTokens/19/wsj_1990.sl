nw/wsj/19/wsj_1990.parse 0 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/19/wsj_1990.parse 0 6 gold reach-v 13.5.1 NF reach.01 null ----- 5:1-ARG0=Agent 6:0-rel 7:1-ARG1=Theme 
nw/wsj/19/wsj_1990.parse 0 12 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 5:1*10:1-ARG0=Agent;Recipient 12:0-rel 13:2-ARG1=Theme;Theme 5:1*10:1-LINK-PRO 
nw/wsj/19/wsj_1990.parse 1 2 gold propose-v 37.7-1 Statement propose.01 null ----- 2:0-rel 3:0-ARG1=Topic;Message 
nw/wsj/19/wsj_1990.parse 1 9 gold receive-v 13.5.2 Receiving receive.01 null ----- 0:1-ARGM-LOC 5:2-ARG0=Agent;Donor 8:0-ARGM-MOD 9:0-rel 10:1-ARG1=Theme;Theme 13:1-ARG3 
nw/wsj/19/wsj_1990.parse 1 23 gold own-v 100 NF own.01 null ----- 14:2,15:1*24:1-ARG1=Theme 22:0-ARGM-NEG 23:0-rel 25:1-ARG0=Pivot 14:2*24:1-LINK-PSV 
nw/wsj/19/wsj_1990.parse 1 31 gold own-v 100 NF own.01 null ----- 26:1*28:1*29:1-ARG0=Pivot 30:1-ARGM-TMP 31:0-rel 32:1-ARG1=Theme 26:1*28:1-LINK-SLC 
nw/wsj/19/wsj_1990.parse 2 16 gold say-v 37.7-1 IN say.01 null ----- 0:2*18:1-ARG1=Topic 15:1-ARG0=Agent 16:0-rel 
