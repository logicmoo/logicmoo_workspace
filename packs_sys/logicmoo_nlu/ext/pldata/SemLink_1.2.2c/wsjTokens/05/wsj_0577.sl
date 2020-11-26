nw/wsj/05/wsj_0577.parse 0 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/05/wsj_0577.parse 1 4 gold emerge-v 48.1.1 IN emerge.01 null ----- 0:1-ARG0=Theme 4:0-rel 5:1-ARG1=Location 
nw/wsj/05/wsj_0577.parse 2 7 gold end-v 55.4-1 Cause_to_end end.01 null ----- 1:1-ARG1=Theme 7:0-rel 8:1-ARGM-TMP 
nw/wsj/05/wsj_0577.parse 2 14 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG3 11:1-ARG0=Agent 14:0-rel 15:1-ARG1=Topic 
nw/wsj/05/wsj_0577.parse 2 17 gold expect-v 62 IN expect.01 null ----- 16:1-ARG0=Experiencer 17:0-rel 18:2-ARG1=Theme 
nw/wsj/05/wsj_0577.parse 2 29 gold say-v 37.7-1 IN say.01 null ----- 22:2,26:1,31:2-ARG1=Topic 27:1-ARG0=Agent 29:0-rel 22:2*26:1-LINK-SLC 
nw/wsj/05/wsj_0577.parse 3 2 gold compare-v 22.2-2 Evaluative_comparison compare.01 null ----- 0:1-ARG1=Patient 1:0-ARGM-MOD 2:0-rel 3:1-ARG2=Co-Patient 
nw/wsj/05/wsj_0577.parse 3 5 gold estimate-v 54.4 Estimating estimate.01 1 ----- 5:0-rel 6:0-ARG1=Theme 7:1-ARG2=Value 
nw/wsj/05/wsj_0577.parse 3 22 gold include-v 65 NF include.01 null ----- 4:1,7:1,12:0*13:2*20:1*21:1-ARG2=Location 22:0-rel 23:1-ARG1=Theme 13:2*20:1-LINK-SLC 
