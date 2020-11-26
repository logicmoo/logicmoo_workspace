nw/wsj/01/wsj_0163.parse 0 4 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 4:0-rel 5:1-ARG1=Topic 
nw/wsj/01/wsj_0163.parse 0 8 gold convert-v 26.6.1-1 IN convert.01 1 ----- 6:1-ARG0=Agent 7:0-ARGM-MOD 8:0-rel 9:1-ARG1=Patient 17:1-ARG2=Result 22:1-ARGM-MNR 
nw/wsj/01/wsj_0163.parse 0 20 gold trade-v 13.6-1 Exchange trade.01 null ----- 19:0-ARGM-MNR 20:0-rel 21:0-ARG1=Theme 
nw/wsj/01/wsj_0163.parse 1 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/01/wsj_0163.parse 1 8 gold expect-v 62 IN expect.01 null ----- 4:1,9:2-ARG1=Theme 7:0-ARGM-NEG 8:0-rel 
nw/wsj/01/wsj_0163.parse 1 11 gold have-v 100 IN have.03 null ----- 4:1*9:1-ARG0=Pivot 11:0-rel 12:2-ARG1=Theme 
nw/wsj/01/wsj_0163.parse 2 13 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 13:0-rel 14:1-ARG1=Topic 
nw/wsj/01/wsj_0163.parse 2 18 gold exchange-v 13.6-1 IN exchange.01 1 ----- 15:1-ARG0=Agent 17:0-ARGM-MOD 18:0-rel 19:1-ARG1=Theme 21:1-ARG3=Co-Theme 
nw/wsj/01/wsj_0163.parse 3 7 gold liquidate-v 42.1 Killing liquidate.01 2 ----- 0:1*8:1-ARG1=Patient;Victim 5:0-ARGM-MOD 7:0-rel 
nw/wsj/01/wsj_0163.parse 3 15 gold distribute-v 13.2-1 Dispersal distribute.01 null ----- 10:2*16:1-ARG1=Theme 15:0-rel 17:1-ARG2=Recipient 
nw/wsj/01/wsj_0163.parse 4 2 gold receive-v 13.5.2 Receiving receive.01 null ----- 0:1-ARG0=Agent;Donor 1:0-ARGM-MOD 2:0-rel 3:1-ARG1=Theme;Theme 13:1-ARGM-TMP 
nw/wsj/01/wsj_0163.parse 4 17 gold liquidate-v 42.1 Killing liquidate.01 2 ----- 14:1*18:1-ARG1=Patient;Victim 17:0-rel 19:1-ARGM-TMP 
nw/wsj/01/wsj_0163.parse 4 25 gold say-v 37.7-1 IN say.01 null ----- 23:1-ARG0=Agent 25:0-rel 26:1-ARG1=Topic 
nw/wsj/01/wsj_0163.parse 5 2 gold expect-v 62 IN expect.01 null ----- 2:0-rel 3:1-ARG1=Theme 
nw/wsj/01/wsj_0163.parse 5 23 gold issue-v 13.3 NF issue.01 null ----- 4:2*24:1-ARG1=Theme 21:0-ARGM-MOD 23:0-rel 25:1-ARGM-TMP 
nw/wsj/01/wsj_0163.parse 6 8 gold hold-v 15.1-1 Manipulation hold.01 null ----- 0:2-ARG0=Agent;Agent 8:0-rel 9:3-ARG1=Theme;Entity 
