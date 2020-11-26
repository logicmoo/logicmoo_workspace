nw/wsj/02/wsj_0229.parse 0 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/02/wsj_0229.parse 0 8 gold elect-v 29.1 Change_of_leadership elect.01 null ----- 4:1*9:1-ARG1=Theme;New_leader 8:0-rel 10:2-ARG2=Result;Role 
nw/wsj/02/wsj_0229.parse 1 11 gold have-v 100 IN have.03 null ----- 0:1*9:1*10:1-ARG0=Pivot 11:0-rel 12:2-ARG1=Theme 0:1*9:1-LINK-SLC 
nw/wsj/02/wsj_0229.parse 2 12 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 12:0-rel 13:1-ARG1=Topic 
nw/wsj/02/wsj_0229.parse 3 6 gold pursue-v 51.6 Cotheme pursue.01 null ----- 1:1*4:1-ARG0=Agent;Theme 3:0-ARGM-MOD 6:0-rel 7:1-ARG1=Theme;Cotheme 
nw/wsj/02/wsj_0229.parse 3 13 gold say-v 37.7-1 IN say.01 null ----- 1:2*14:1-ARG1=Topic 11:1-ARG0=Agent 13:0-rel 
nw/wsj/02/wsj_0229.parse 5 5 gold say-v 37.7-1 IN say.01 null ----- 3:1-ARG0=Agent 5:0-rel 0:2-ARG1-DSP=Topic 
nw/wsj/02/wsj_0229.parse 6 4 gold end-v 55.4-1 Cause_to_end end.01 null ----- 1:1-ARG1=Theme 4:0-rel 5:1-ARGM-TMP 
nw/wsj/02/wsj_0229.parse 6 10 gold report-v 37.7-1 Statement report.01 null ----- 0:1-ARGM-TMP 8:1-ARG0 10:0-rel 11:2-ARG1 
nw/wsj/02/wsj_0229.parse 7 1 gold compare-v 22.2-2 Evaluative_comparison compare.01 null ----- 0:1-ARG1=Patient 1:0-rel 2:1-ARG2=Co-Patient 
