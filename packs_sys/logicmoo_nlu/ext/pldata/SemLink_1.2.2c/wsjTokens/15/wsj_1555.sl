nw/wsj/15/wsj_1555.parse 0 5 gold announce-v 37.7-1 Statement announce.01 1 ----- 0:1-ARG0=Agent;Speaker 4:0-ARGM-MOD 5:0-rel 6:1-ARGM-TMP 7:2-ARG1=Topic;Message 
nw/wsj/15/wsj_1555.parse 0 12 gold allow-v 64 IN allow.01 1 ----- 7:1*10:1*11:1-ARG0 12:0-rel 13:3-ARG1 7:1*10:1-LINK-SLC 
nw/wsj/15/wsj_1555.parse 0 24 gold send-v 11.1-1 Sending send.01 1 ----- 13:2-ARG0=Agent;Sender 24:0-rel 25:1-ARG1=Theme;Theme 27:1-ARG2=Destination;Goal/Recipient 
nw/wsj/15/wsj_1555.parse 1 1 gold call-v 29.3 IN call.01 5 ----- 0:1*2:1*7:1-ARG1=Theme 1:0-rel 3:2-ARG2=Result 
nw/wsj/15/wsj_1555.parse 1 9 gold act-v 29.6-1 NF act.01 1 ----- 0:2-ARGM-ADV 7:1-ARG0=Agent 9:0-rel 10:2-ARG1=Attribute 
nw/wsj/15/wsj_1555.parse 2 3 gold have-v 100 IN have.03 4 ----- 1:1-ARG0=Pivot 3:0-rel 4:1-ARG1=Theme 
nw/wsj/15/wsj_1555.parse 2 11 gold ask-v 60-1 Request ask.02 2 ----- 0:1-ARGM-ADV 9:1-ARG0 11:0-rel 12:2-ARG2 17:1-ARG1 
nw/wsj/15/wsj_1555.parse 3 16 gold send-v 11.1-1 Sending send.01 1 ----- 0:1-ARG0=Agent;Sender 1:1-ARGM-TMP 16:0-rel 17:1-ARG1=Theme;Theme 19:1-ARG2=Destination;Goal/Recipient 
nw/wsj/15/wsj_1555.parse 4 3 gold claim-v 37.7-1 Statement claim.01 1 ----- 0:1-ARG0=Agent;Speaker 3:0-rel 4:1-ARG1=Topic;Message 
nw/wsj/15/wsj_1555.parse 4 7 gold allow-v 64 IN allow.01 1 ----- 5:1-ARG0 7:0-rel 8:2-ARG1 
nw/wsj/15/wsj_1555.parse 5 7 gold say-v 37.7-1 IN say.01 1 ----- 7:0-rel 10:2-ARG0=Agent 0:2-ARG1-DSP=Topic 
nw/wsj/15/wsj_1555.parse 5 39 gold distribute-v 13.2-1 Dispersal distribute.01 null ----- 39:0-rel 40:0-ARG1=Theme 
nw/wsj/15/wsj_1555.parse 6 9 gold sit-v 47.6 Being_located sit.01 3 ----- 0:1-ARGM-LOC 6:1-ARG1=Theme 8:1-ARGM-TMP 9:0-rel 10:2-ARGM-MNR 12:1-ARGM-TMP 
nw/wsj/15/wsj_1555.parse 7 8 gold say-v 37.7-1 IN say.01 1 ----- 7:1-ARG0=Agent 8:0-rel 0:2-ARG1-DSP=Topic 
nw/wsj/15/wsj_1555.parse 7 14 gold get-v 13.5.1-1 IN get.01 1 ----- 0:1-ARGM-MNR 13:1-ARG0=Agent 14:0-rel 15:1-ARG1=Theme 17:1-ARG3=Asset 
nw/wsj/15/wsj_1555.parse 8 7 gold ship-v 11.1-1 Sending ship.01 1 ----- 0:1*3:1*4:1*8:1-ARG1=Theme;Theme 5:0-ARGM-MOD 7:0-rel 9:1-ARGM-TMP 0:1*3:1-LINK-SLC 
nw/wsj/15/wsj_1555.parse 10 3 gold work-v 73-3 IN work.01 2 ----- 0:1-ARG0=Agent 2:1-ARGM-TMP 3:0-rel 4:1-ARG1=Theme 
nw/wsj/15/wsj_1555.parse 10 15 gold make-v 26.1-1 Manufacturing make.01 2 ----- 14:1*16:1-ARG1=Product;Product 15:0-rel 17:1-ARG0=Agent;Manufacturer 14:1*16:1-LINK-PSV 
nw/wsj/15/wsj_1555.parse 11 3 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1*13:2-ARG1=Topic 
nw/wsj/15/wsj_1555.parse 11 7 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 5:1-ARG0=Agent;Seller 6:0-ARGM-MOD 7:0-rel 8:2;13:2-ARG1=Theme;Goods 10:2-ARGM-TMP 
nw/wsj/15/wsj_1555.parse 12 3 gold differ-v 23.4 Similarity differ.02 1 ----- 0:1-ARG1=Theme 3:0-rel 4:1-ARG1=Theme 
nw/wsj/15/wsj_1555.parse 13 4 gold call-v 29.3 IN call.01 5 ----- 0:1*5:1-ARG1=Theme 4:0-rel 7:1-ARG2=Result 0:1*5:1-LINK-PSV 
nw/wsj/15/wsj_1555.parse 13 11 gold develop-v 26.1 IN develop.02 1 ----- 0:1-ARG1=Product 11:0-rel 13:1-ARG0=Agent 
nw/wsj/15/wsj_1555.parse 13 28 gold split-v 23.2 Cause_to_fragment split.01 1 ----- 0:2*34:1-ARG0=Agent;Agent 21:1-ARGM-DIS 28:0,30:1-rel 29:1-ARG1=Patient;Whole_patient 31:1-ARG2=Co-Patient 34:2-ARGM-ADV 
nw/wsj/15/wsj_1555.parse 14 13 gold work-v 73-3 IN work.01 2 ----- 1:2*11:1-ARG0=Agent 13:0-rel 14:1-ARG3=Co-Agent 
nw/wsj/15/wsj_1555.parse 15 11 gold work-v 73-3 IN work.01 2 ----- 0:1*4:1*9:1-ARG0=Agent 11:0-rel 12:1-ARG3=Co-Agent 
nw/wsj/15/wsj_1555.parse 15 19 gold say-v 37.7-1 IN say.01 1 ----- 16:1-ARG0=Agent 19:0-rel 0:3-ARG1-DSP=Topic 
nw/wsj/15/wsj_1555.parse 16 4 gold turn-v 26.6.2 Cause_change turn.02 2 ----- 0:1-ARG0=Agent 4:0-rel 5:1-ARG1=Patient 7:0,18:1-ARG2=Goal 9:2-ARGM-DIS 
nw/wsj/15/wsj_1555.parse 16 15 gold concern-v 31.1 Cause_emotion concern.01 1 ----- 12:1*16:1-ARG1=Experiencer 15:0-rel 
nw/wsj/15/wsj_1555.parse 16 24 gold say-v 37.7-1 IN say.01 1 ----- 24:0-rel 25:1-ARG1=Topic 27:2-ARG0=Agent 
