nw/wsj/19/wsj_1922.parse 0 12 gold rate-v 54.4 NF rate.01 null ----- 0:1*13:1-ARG1=Theme 3:2-ARGM-PRD 12:0-rel 14:2-ARG2=Value 0:1*13:1-LINK-PRO 
nw/wsj/19/wsj_1922.parse 0 24 gold match-v 22.2-1 Compatibility match.01 1 ----- 0:2-ARG1=Patient;Item_1/Items 23:0-ARGM-NEG 24:0-rel 25:3-ARG1=Patient;Item_1/Items 
nw/wsj/19/wsj_1922.parse 0 31 gold rate-v 54.4 NF rate.01 null ----- 25:2*32:1-ARG1=Theme 31:0-rel 33:1-ARG2=Value 25:2*32:1-LINK-PSV 
nw/wsj/19/wsj_1922.parse 1 21 gold mean-v 29.5-1 NF mean.01 null ----- 0:2-ARG0=Agent 21:0-rel 22:1-ARG1=Theme 
nw/wsj/19/wsj_1922.parse 1 35 gold say-v 37.7-1 IN say.01 null ----- 0:3*36:1-ARG1=Topic 35:0-rel 37:2-ARG0=Agent 
nw/wsj/19/wsj_1922.parse 2 23 gold add-v 37.7 NF add.01 null ----- 21:1-ARG0=Agent 23:0-rel 24:1-ARG1=Topic 
nw/wsj/19/wsj_1922.parse 5 5 gold visit-v 36.3-1 IN visit.01 null ----- 1:1*6:1-ARG1=Co-Agent 5:0-rel 7:1-ARGM-ADV 
nw/wsj/19/wsj_1922.parse 5 11 gold perch-v 47.6 Placing perch.01 null ----- 8:1*12:1-ARG1=Theme 11:0-rel 13:1-ARG2=Location 16:1-ARGM-ADV 8:1*12:1-LINK-PSV 
nw/wsj/19/wsj_1922.parse 6 9 gold feel-v 35.1 Appearance feel.03 null ----- 1:2-ARG0=Agent 8:0-ARGM-MOD 9:0-rel 10:1-ARG1=Theme 
nw/wsj/19/wsj_1922.parse 6 14 gold behave-v 29.6 NF behave.01 2 ----- 13:1-ARG0=Agent 14:0-rel 15:1-ARG1=Attribute 17:2-ARGM-TMP 
nw/wsj/19/wsj_1922.parse 6 19 gold shake-v 47.3 Cause_to_move_in_place shake.01 null ----- 18:1-ARG0=Agent;Agent/Cause 19:0-rel 20:1-ARG1=Theme;Theme/Bodypart_of_agent 
nw/wsj/19/wsj_1922.parse 6 24 gold say-v 37.7-1 IN say.01 null ----- 1:4*25:1-ARG1=Topic 24:0-rel 26:2-ARG0=Agent 37:1-ARGM-LOC 
nw/wsj/19/wsj_1922.parse 8 2 gold behave-v 29.6 NF behave.01 2 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Attribute 
nw/wsj/19/wsj_1922.parse 9 9 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 9:0-rel 12:2-ARG1=Topic 
nw/wsj/19/wsj_1922.parse 10 2 gold drive-v 11.5 Cause_motion drive.01 null ----- 0:1-ARG0=Agent 1:0-ARGM-MOD 2:0-rel 3:1-ARG1=Theme 4:1-ARGM-LOC 
nw/wsj/19/wsj_1922.parse 10 7 gold build-v 26.1-1 Building build.01 null ----- 0:1-ARG0=Agent;Agent 1:0-ARGM-MOD 7:0-rel 8:1-ARGM-LOC 
nw/wsj/19/wsj_1922.parse 12 21 gold predict-v 78 Predicting predict.01 1 ----- 19:1-ARG0=Cause;Evidence/Speaker 21:0-rel 22:1-ARG1=Topic;Eventuality 
nw/wsj/19/wsj_1922.parse 13 12 gold say-v 37.7-1 IN say.01 null ----- 1:3*13:1-ARG1=Topic 11:1-ARG0=Agent 12:0-rel 
nw/wsj/19/wsj_1922.parse 14 3 gold know-v 29.5-1 IN know.01 1 ----- 1:1-ARG0=Agent 3:0-rel 4:1-ARGM-TMP 8:1-ARG1=Theme 
nw/wsj/19/wsj_1922.parse 14 11 gold get-v 13.5.1-1 IN get.01 null ----- 9:1-ARG0=Agent 11:0-rel 12:2-ARG1=Theme 
nw/wsj/19/wsj_1922.parse 15 13 gold have-v 100 IN have.03 null ----- 0:2-ARG0=Pivot 13:0-rel 14:2-ARG1=Theme 
nw/wsj/19/wsj_1922.parse 15 29 gold leave-v 13.3 Causation leave.02 null ----- 28:1-ARG0=Agent 29:0-rel 30:2*36:1-ARG1=Theme 36:2-ARG2=Goal 42:1-ARGM-TMP 
nw/wsj/19/wsj_1922.parse 15 47 gold tumble-v 45.6-1 Change_position_on_a_scale tumble.01 1 ----- 43:2*52:1-ARG1=Patient;Item 47:0-rel 48:1-ARG4 52:2-ARGM-PRD 
nw/wsj/19/wsj_1922.parse 16 9 gold say-v 37.7-1 IN say.01 null ----- 7:1-ARG0=Agent 9:0-rel 0:3-ARG1-DSP=Topic 
nw/wsj/19/wsj_1922.parse 18 1 gold know-v 29.5-1 IN know.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Theme 
nw/wsj/19/wsj_1922.parse 20 15 gold follow-v 47.8 Cotheme follow.01 null ----- 12:1*16:1-ARG2=Co-Theme;Cotheme 15:0-rel 17:1-ARG1=Theme;Theme 12:1*16:1-LINK-PSV 
nw/wsj/19/wsj_1922.parse 20 24 gold release-v 80-1 Releasing release.01 null ----- 6:2-ARG0=Cause 11:1-ARGM-PRD 23:0-ARGM-NEG 24:0-rel 25:2*35:2-ARG1=Theme 
nw/wsj/19/wsj_1922.parse 21 11 gold predict-v 78 Predicting predict.01 1 ----- 1:2*12:1-ARG1=Topic;Eventuality 9:1-ARG0=Cause;Evidence/Speaker 11:0-rel 13:1-ARGM-LOC 
nw/wsj/19/wsj_1922.parse 22 4 gold have-v 100 IN have.03 null ----- 1:1-ARG0=Pivot 4:0-rel 5:2-ARG1=Theme 
nw/wsj/19/wsj_1922.parse 23 8 gold solve-v 84 NF solve.01 null ----- 0:2-ARG0=Agent 7:0-ARGM-NEG 8:0-rel 9:1-ARG1=Theme 
