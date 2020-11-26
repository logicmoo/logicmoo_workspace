nw/wsj/14/wsj_1458.parse 0 5 gold surround-v 9.8 NF surround.01 1 ----- 0:1-ARG1=Theme 5:0-rel 6:2-ARG2=Destination 
nw/wsj/14/wsj_1458.parse 1 2 gold accuse-v 81 Notification_of_charges accuse.01 1 ----- 0:1-ARG0=Agent;Arraign_authority 2:0-rel 3:2-ARG1=Theme;Accused 9:1-ARG2=Attribute;Charges 
nw/wsj/14/wsj_1458.parse 1 11 gold try-v 61 Attempt try.01 1 ----- 3:2*10:1-ARG0=Agent 11:0-rel 12:2-ARG1=Theme 
nw/wsj/14/wsj_1458.parse 1 14 gold extort-v 10.5 NF extort.01 1 ----- 3:2*10:1*12:1-ARG0=Agent 14:0-rel 15:3-ARG1=Theme 23:1-ARG2=Source 26:1-ARGM-ADV 
nw/wsj/14/wsj_1458.parse 1 42 gold find-v 13.5.1 IN find.01 1 ----- 42:0-rel 43:2-ARG1=Theme 
nw/wsj/14/wsj_1458.parse 1 44 gold hang-v 9.2-1 Execution hang.01 null ----- 39:1*43:1-ARG1=Theme 44:0-rel 46:1-ARGM-LOC 52:1-ARGM-TMP 39:1*43:1-LINK-PRO 
nw/wsj/14/wsj_1458.parse 3 5 gold lend-v 13.1 NF lend.01 2 ----- 0:2*6:1-ARG1=Theme 5:0-rel 7:1-ARG2=Recipient 
nw/wsj/14/wsj_1458.parse 3 20 gold own-v 100 NF own.01 1 ----- 11:2*17:1*18:1*21:1-ARG1=Theme 20:0-rel 23:1-ARGM-ADV 27:1-ARG0=Pivot 11:2*17:1-LINK-SLC 
nw/wsj/14/wsj_1458.parse 4 5 gold deny-v 29.5-1 Statement deny.01 1 ----- 0:1*3:1*4:1-ARG0=Agent;Speaker 5:0-rel 6:1-ARG1=Predicate;Message 0:1*3:1-LINK-SLC 
nw/wsj/14/wsj_1458.parse 5 3 gold say-v 37.7-1 IN say.01 4 ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/14/wsj_1458.parse 5 10 gold obtain-v 13.5.2-1 Getting obtain.01 1 ----- 5:2-ARG0=Agent;Recipient 10:0-rel 11:3-ARG1=Theme;Theme 16:1-ARG2=Source;Source 
nw/wsj/14/wsj_1458.parse 6 2 gold speculate-v 29.5-1 NF speculate.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Theme 
nw/wsj/14/wsj_1458.parse 6 6 gold contain-v 47.8 IN contain.01 1 ----- 4:1-ARG0=Theme 6:0-rel 7:2-ARG1=Co-Theme 
nw/wsj/14/wsj_1458.parse 6 20 gold show-v 78-1-1 IN show.01 1 ----- 7:1-ARG0 20:0-rel 21:1-ARG1 
nw/wsj/14/wsj_1458.parse 6 25 gold know-v 29.5-1 IN know.01 2 ----- 22:1*26:1-ARG1=Theme 25:0-rel 27:1-ARG2=Predicate 22:1*26:1-LINK-PSV 
