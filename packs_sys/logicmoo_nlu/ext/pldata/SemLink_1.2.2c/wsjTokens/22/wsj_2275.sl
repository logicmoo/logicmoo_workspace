nw/wsj/22/wsj_2275.parse 2 8 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 8:0-rel 9:1-ARG1=Topic 
nw/wsj/22/wsj_2275.parse 2 12 gold work-v 73-3 IN work.01 null ----- 10:1*14:1-ARG0=Agent 12:0-rel 13:1-ARG3=Co-Agent 14:2-ARG1=Theme 
nw/wsj/22/wsj_2275.parse 3 9 gold replace-v 13.6 IN replace.01 null ----- 0:1*7:1*8:1-ARG0=Agent 9:0-rel 10:2-ARG1=Theme 0:1*7:1-LINK-SLC 
nw/wsj/22/wsj_2275.parse 3 13 gold expire-v 48.2 Death expire.01 1 ----- 10:1*11:1*12:1-ARG1=Patient;Protagonist 13:0-rel 14:1-ARGM-TMP 10:1*11:1-LINK-SLC 
nw/wsj/22/wsj_2275.parse 4 2 gold follow-v 47.8 Cotheme follow.01 null ----- 0:1-ARG1=Theme;Theme 2:0-rel 3:2-ARG2=Co-Theme;Cotheme 
nw/wsj/22/wsj_2275.parse 5 10 gold remain-v 47.1-1 State_continue remain.01 null ----- 0:1-ARGM-TMP 2:2-ARG1=Theme 10:0-rel 11:1-ARG3 
nw/wsj/22/wsj_2275.parse 6 7 gold agree-v 36.1-1 IN agree.01 null ----- 0:2-ARG0=Agent 5:1-ARGM-TMP 7:0-rel 8:1-ARG1=Theme 
