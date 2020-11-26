nw/wsj/17/wsj_1718.parse 0 3 gold introduce-v 22.2-3-1 NF introduce.02 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG2=Co-Patient 9:2-ARG1=Patient 
nw/wsj/17/wsj_1718.parse 0 13 gold extend-v 47.1-1 NF extend.01 null ----- 9:1*10:1*11:1-ARG0 13:0-rel 14:1-ARG1=Theme 17:1-ARG4 9:1*10:1-LINK-SLC 
nw/wsj/17/wsj_1718.parse 1 24 gold say-v 37.7-1 IN say.01 null ----- 0:2*26:1-ARG1=Topic 22:1-ARG0=Agent 24:0-rel 
nw/wsj/17/wsj_1718.parse 2 4 gold follow-v 51.6 Cotheme follow.02 null ----- 0:1-ARG1=Theme;Cotheme 4:0-rel 5:2-ARG2 
nw/wsj/17/wsj_1718.parse 2 13 gold hold-v 29.5-1 Opinion hold.02 null ----- 5:1*11:1*12:1-ARG0=Agent 13:0-rel 14:1-ARG1=Theme 5:1*11:1-LINK-SLC 
nw/wsj/17/wsj_1718.parse 4 10 gold oppose-v 22.2-3 NF oppose.01 null ----- 0:2-ARG0=Patient 9:1-ARGM-MNR 10:0-rel 11:2-ARG1=Co-Patient 
nw/wsj/17/wsj_1718.parse 5 15 gold require-v 103 NF require.01 null ----- 0:2-ARG0=Pivot 14:0-ARGM-NEG 15:0-rel 16:1-ARG1=Theme 18:1-ARGM-CAU 
