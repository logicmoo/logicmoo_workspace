nw/wsj/06/wsj_0646.parse 0 4 gold declare-v 37.7-1 Statement declare.02 null ----- 0:1-ARG0=Agent;Speaker 4:0-rel 5:1-ARG1=Topic;Message 
nw/wsj/06/wsj_0646.parse 0 16 gold boost-v 102 NF boost.01 1 ----- 0:1-ARG0=Agent 15:1-ARGM-MNR 16:0-rel 17:1-ARG1=Theme 
nw/wsj/06/wsj_0646.parse 1 4 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 4:0-rel 5:1-ARG1=Topic 
nw/wsj/06/wsj_0646.parse 1 11 gold distribute-v 13.2-1 Dispersal distribute.01 null ----- 6:1*12:1-ARG1=Theme 9:0-ARGM-MOD 11:0-rel 13:1-ARGM-TMP 15:1-ARG2=Recipient 
nw/wsj/06/wsj_0646.parse 2 12 gold receive-v 13.5.2 Receiving receive.01 null ----- 4:1*8:1*22:1-ARGM-LOC 10:1-ARG0=Agent;Donor 12:0-rel 13:1-ARG1=Theme;Theme 4:1*8:1-LINK-SLC 
nw/wsj/06/wsj_0646.parse 2 26 gold declare-v 37.7-1 Statement declare.02 null ----- 0:1*25:1-ARG0=Agent;Speaker 26:0-rel 27:2-ARG1=Topic;Message 
