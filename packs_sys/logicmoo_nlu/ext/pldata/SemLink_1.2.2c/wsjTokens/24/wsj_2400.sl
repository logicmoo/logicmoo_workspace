nw/wsj/24/wsj_2400.parse 2 4 gold expect-v 62 IN expect.01 1 ----- 0:1,5:2-ARG1=Theme 4:0-rel 22:1-ARGM-ADV 
nw/wsj/24/wsj_2400.parse 3 10 gold expect-v 62 IN expect.01 1 ----- 0:3,11:2-ARG1=Theme 10:0-rel 
nw/wsj/24/wsj_2400.parse 3 13 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 2 ----- 0:3*11:1-ARG1 13:0-rel 15:1-ARGM-ADV 
nw/wsj/24/wsj_2400.parse 3 24 gold report-v 37.7-1 Statement report.01 1 ----- 20:1*25:1-ARG1 24:0-rel 26:1-ARGM-TMP 27:1-ARGM-LOC 20:1*25:1-LINK-PSV 
nw/wsj/24/wsj_2400.parse 4 23 gold get-v 26.6.2 IN get.03 11.3 ----- 22:1*24:1-ARG1=Patient 23:0-rel 24:2-ARG2=Goal 
nw/wsj/24/wsj_2400.parse 4 25 gold start-v 55.1-1 Process_start start.01 1 ----- 22:1*24:1*26:1-ARG1=Theme;Event 25:0-rel 27:1-ARGM-LOC 
nw/wsj/24/wsj_2400.parse 5 11 gold expect-v 62 IN expect.01 1 ----- 10:1-ARG0=Experiencer 11:0-rel 12:2-ARG1=Theme 
nw/wsj/24/wsj_2400.parse 5 14 gold see-v 30.1-1 Perception_experience see.01 3 ----- 6:2*15:1-ARG1=Stimulus;Phenomenon 10:1*12:1-ARG0=Experiencer;Perceiver_passive 14:0-rel 16:1-ARGM-LOC 
nw/wsj/24/wsj_2400.parse 6 10 gold think-v 29.9-2 IN think.01 1 ----- 0:1-ARGM-TMP 2:2,11:2-ARG1 10:0-rel 
nw/wsj/24/wsj_2400.parse 7 14 gold say-v 37.7-1 IN say.01 1 ----- 0:1*16:1-ARG1=Topic 14:0-rel 17:2-ARG0=Agent 
nw/wsj/24/wsj_2400.parse 8 9 gold combine-v 22.1-1-1 Amalgamation combine.01 1 ----- 7:1*10:1-ARG1=Patient;Part_1 9:0-rel 11:1-ARG2=Co-Patient;Part_2 
nw/wsj/24/wsj_2400.parse 8 29 gold plummet-v 45.6-1 Change_position_on_a_scale plummet.01 1 ----- 28:1-ARG1=Patient;Item 29:0-rel 
nw/wsj/24/wsj_2400.parse 9 9 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARGM-DIS 4:2-ARG0=Agent 9:0-rel 10:1-ARG1=Topic 
nw/wsj/24/wsj_2400.parse 9 23 gold make-v 26.1-1 IN make.01 1 ----- 11:2*24:1-ARG1=Product 21:1-ARGM-TMP 23:0-rel 
nw/wsj/24/wsj_2400.parse 10 20 gold begin-v 55.1-1 Process_start begin.01 1 ----- 19:1,21:2-ARG1=Theme;Event 20:0-rel 
nw/wsj/24/wsj_2400.parse 10 27 gold say-v 37.7-1 IN say.01 1 ----- 1:2*28:1-ARG1=Topic 26:1-ARG0=Agent 27:0-rel 
nw/wsj/24/wsj_2400.parse 11 2 gold think-v 29.9-2 IN think.01 1 ----- 0:1,3:2-ARG1 2:0-rel 
nw/wsj/24/wsj_2400.parse 11 6 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 2 ----- 0:1*3:1-ARG1 6:0-rel 
nw/wsj/24/wsj_2400.parse 11 24 gold say-v 37.7-1 IN say.01 1 ----- 0:2*25:1-ARG1=Topic 23:1-ARG0=Agent 24:0-rel 
nw/wsj/24/wsj_2400.parse 12 5 gold split-v 23.2 Cause_to_fragment split.01 1 ----- 0:2-ARG1=Patient;Whole_patient 5:0-rel 6:1-ARG2=Co-Patient 
nw/wsj/24/wsj_2400.parse 12 10 gold read-v 37.1.1-1 NF read.01 3 ----- 7:1*8:1*9:1-ARG0=Agent 10:0-rel 11:3-ARG1=Topic 19:1-ARGM-ADV 7:1*8:1-LINK-SLC 
nw/wsj/24/wsj_2400.parse 12 29 gold use-v 105 IN use.01 1 ----- 26:1*27:1*28:1-ARG0 29:0-rel 30:1-ARG1 37:1-ARGM-LOC 26:1*27:1-LINK-SLC 
nw/wsj/24/wsj_2400.parse 12 33 gold comfort-v 31.1 Experiencer_obj comfort.01 1 ----- 31:0,32:0-ARGM-EXT 33:0-rel 34:1,36:0-ARG1=Experiencer;Experiencer 
nw/wsj/24/wsj_2400.parse 13 16 gold run-v 47.7 IN run.04 2 ----- 14:1-ARG1=Theme 16:0-rel 17:1-ARG3 23:1-ARG4 30:1-ARGM-ADV 
nw/wsj/24/wsj_2400.parse 14 8 gold help-v 72-1 Assistance help.01 1 ----- 0:2*6:1*7:1-ARG0=Agent;Helper 8:0-rel 9:1-ARG1=Theme;Goal/Focal_entity 0:2*6:1-LINK-SLC 
nw/wsj/24/wsj_2400.parse 14 9 gold push-v 23.2 Cause_change_of_position_on_a_scale push.01 6.1 ----- 0:2*6:1*7:1-ARG0=Agent 9:0-rel 10:1-ARG2=Co-Patient 11:1-ARG1=Patient 0:2*6:1-LINK-SLC 
nw/wsj/24/wsj_2400.parse 14 17 gold expect-v 62 IN expect.01 1 ----- 0:3,18:2-ARG1=Theme 17:0-rel 
nw/wsj/24/wsj_2400.parse 15 3 gold expect-v 62 IN expect.01 1 ----- 0:1-ARG0=Experiencer 3:0-rel 4:2-ARG1=Theme 12:1-ARGM-TMP 
nw/wsj/24/wsj_2400.parse 16 17 gold expect-v 62 IN expect.01 1 ----- 16:1-ARG0=Experiencer 17:0-rel 18:2-ARG1=Theme 
nw/wsj/24/wsj_2400.parse 17 3 gold have-v 100 IN have.03 6 ----- 1:1-ARG0=Pivot 3:0-rel 4:1-ARG1=Theme 7:1-ARGM-LOC 
nw/wsj/24/wsj_2400.parse 17 23 gold say-v 37.7-1 IN say.01 1 ----- 1:3*24:1-ARG1=Topic 22:1-ARG0=Agent 23:0-rel 
nw/wsj/24/wsj_2400.parse 18 3 gold expect-v 29.5-1 IN expect.01 1 ----- 0:1,4:2-ARG1 3:0-rel 
nw/wsj/24/wsj_2400.parse 18 12 gold jump-v 45.6-1 Change_position_on_a_scale jump.01 1 ----- 10:1-ARG1=Patient;Item 12:0-rel 13:2-ARG2=Extent;Difference 
nw/wsj/24/wsj_2400.parse 18 19 gold say-v 37.7-1 IN say.01 1 ----- 0:3*21:1-ARG1 19:0-rel 22:2-ARG0 
nw/wsj/24/wsj_2400.parse 19 2 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0 1:1-ARGM-DIS 2:0-rel 3:1-ARG1 
nw/wsj/24/wsj_2400.parse 19 5 gold think-v 29.9-2 IN think.01 1 ----- 4:1-ARG0 5:0-rel 6:1-ARG1 
nw/wsj/24/wsj_2400.parse 20 1 gold expect-v 29.5-1 IN expect.01 1 ----- 0:1-ARG0 1:0-rel 2:2-ARG1 
nw/wsj/24/wsj_2400.parse 21 3 gold expect-v 29.5-1 IN expect.01 1 ----- 3:0-rel 4:2-ARG1 
nw/wsj/24/wsj_2400.parse 22 1 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0 1:0-rel 2:1-ARG1 
nw/wsj/24/wsj_2400.parse 22 11 gold signal-v 37.4 NF signal.01 2 ----- 3:2-ARG0=Agent 11:0-rel 12:2-ARG1=Topic 18:1-ARGM-ADV 
nw/wsj/24/wsj_2400.parse 22 20 gold remain-v 47.1-1 State_continue remain.01 1 ----- 19:1-ARG1=Theme 20:0-rel 21:1-ARG3 23:1-ARGM-ADV 
