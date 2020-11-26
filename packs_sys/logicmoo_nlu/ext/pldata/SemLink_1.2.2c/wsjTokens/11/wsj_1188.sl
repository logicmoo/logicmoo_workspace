nw/wsj/11/wsj_1188.parse 0 16 gold announce-v 37.7-1 Statement announce.01 1 ----- 0:2-ARG0=Agent;Speaker 16:0-rel 17:1-ARG1=Topic;Message 
nw/wsj/11/wsj_1188.parse 0 20 gold agree-v 36.1-1 IN agree.01 3 ----- 18:1-ARG0=Agent 20:0-rel 21:1-ARG1=Theme 
nw/wsj/11/wsj_1188.parse 2 2 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/11/wsj_1188.parse 2 11 gold merge-v 22.1-1-1 Amalgamation merge.01 1 ----- 0:1*9:1-ARG1=Patient;Part_1 11:0-rel 12:1-ARG3 0:1*9:1-LINK-PRO 
nw/wsj/11/wsj_1188.parse 2 19 gold call-v 29.3 IN call.01 5 ----- 13:1*15:1*16:1*20:1-ARG1=Theme 19:0-rel 22:1-ARG2=Result 13:1*15:1-LINK-SLC 
nw/wsj/11/wsj_1188.parse 2 27 gold send-v 11.1-1 Sending send.01 1 ----- 4:2-ARG1=Theme;Theme 27:0-rel 29:1-ARGM-TMP 30:1-ARG2=Destination;Goal/Recipient 
nw/wsj/11/wsj_1188.parse 3 3 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARGM-LOC 8:1-ARG1=Topic 
nw/wsj/11/wsj_1188.parse 3 14 gold consider-v 29.9-1-1-1 Cogitation consider.01 2 ----- 9:1*15:1-ARG1=Theme;Topic 14:0-rel 16:1-ARGM-LOC 22:1-ARGM-TMP 
nw/wsj/11/wsj_1188.parse 4 1 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/11/wsj_1188.parse 4 7 gold submit-v 13.2-1-1 IN submit.01 2 ----- 3:1*8:1-ARG1=Theme 5:0-ARGM-MOD 7:0-rel 9:1-ARG2=Recipient 14:1-ARGM-TMP 
nw/wsj/11/wsj_1188.parse 6 2 gold require-v 103 NF require.01 1 ----- 0:1-ARG0=Pivot 2:0-rel 3:2-ARG1=Theme 
