nw/wsj/15/wsj_1500.parse 0 6 gold expect-v 62 IN expect.01 1 ----- 0:2,7:2-ARG1=Theme 5:1-ARGM-MNR 6:0-rel 
nw/wsj/15/wsj_1500.parse 0 24 gold fail-v 74-2 IN fail.01 1 ----- 14:3*25:1-ARG1 24:0-rel 25:2-ARG2 
nw/wsj/15/wsj_1500.parse 0 27 gold show-v 78-1-1 IN show.01 1 ----- 14:3*25:1-ARG0 27:0-rel 28:2-ARG1 
nw/wsj/15/wsj_1500.parse 1 15 gold help-v 72-1 Assistance help.01 1 ----- 0:5-ARG0=Agent;Helper 15:0-rel 16:2-ARG1=Theme;Goal/Focal_entity 
nw/wsj/15/wsj_1500.parse 1 18 gold prevent-v 67 Preventing prevent.01 1 ----- 16:1-ARG0=Agent;Preventing_cause 18:0-rel 19:2-ARG1=Theme;Event 23:1-ARGM-TMP 
nw/wsj/15/wsj_1500.parse 2 2 gold reckon-v 29.9-1 Awareness reckon.01 1 ----- 0:0-ARGM-DIS 1:1-ARG0 2:0-rel 3:1-ARG1 
nw/wsj/15/wsj_1500.parse 2 19 gold announce-v 37.7-1 Statement announce.01 1 ----- 13:1*17:1-ARG0=Agent;Speaker 19:0-rel 20:1-ARG1=Topic;Message 24:1-ARGM-LOC 29:1-ARGM-TMP 13:1*17:1-LINK-PRO 
nw/wsj/15/wsj_1500.parse 3 9 gold force-v 59 NF force.01 1 ----- 6:1*10:1-ARG1=Patient 9:0-rel 11:2-ARG2=Result 
nw/wsj/15/wsj_1500.parse 3 37 gold say-v 37.7-1 IN say.01 1 ----- 0:2*39:1-ARG1=Topic 31:2-ARG0=Agent 37:0-rel 
nw/wsj/15/wsj_1500.parse 4 19 gold say-v 37.7-1 IN say.01 1 ----- 1:3*20:1-ARG1=Topic 19:0-rel 21:2-ARG0=Agent 
nw/wsj/15/wsj_1500.parse 6 5 gold range-v 47.7 NF range.01 1 ----- 0:2-ARG1=Theme 5:0-rel 6:1-ARG2 
nw/wsj/15/wsj_1500.parse 6 11 gold expect-v 62 IN expect.01 1 ----- 9:1-ARG0=Experiencer 11:0-rel 12:2-ARG1=Theme 
nw/wsj/15/wsj_1500.parse 6 15 gold show-v 78-1-1 IN show.01 1 ----- 12:1-ARG0 15:0-rel 16:2-ARG1 
nw/wsj/15/wsj_1500.parse 6 37 gold report-v 37.7-1 Statement report.01 1 ----- 21:2*38:1-ARG1 37:0-rel 39:1-ARGM-TMP 21:2*38:1-LINK-PSV 
nw/wsj/15/wsj_1500.parse 7 10 gold register-v 54.1-1 NF register.01 3 ----- 4:1*11:1-ARG1=Theme 10:0-rel 12:1-ARGM-TMP 4:1*11:1-LINK-PSV 
nw/wsj/15/wsj_1500.parse 7 15 gold top-v 90-1 NF top.02 1 ----- 0:2*16:1-ARG1=Co-Theme 15:0-rel 17:2-ARG0=Theme 
nw/wsj/15/wsj_1500.parse 8 11 gold say-v 37.7-1 IN say.01 1 ----- 0:2-ARG0=Agent 11:0-rel 12:1-ARG1=Topic 
nw/wsj/15/wsj_1500.parse 8 23 gold transform-v 26.6.1 NF transform.01 1 ----- 18:2-ARG0=Agent 23:0-rel 24:1-ARG1=Patient 25:2-ARGM-PNC 
nw/wsj/15/wsj_1500.parse 9 6 gold remain-v 47.1-1 State_continue remain.01 1 ----- 0:1-ARGM-TMP 5:1-ARG1=Theme 6:0-rel 7:1-ARG3 15:1-ARGM-ADV 
nw/wsj/15/wsj_1500.parse 9 16 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 16:0-rel 17:0,18:2,22:0-ARG1 
nw/wsj/15/wsj_1500.parse 10 1 gold reckon-v 29.9-1 Awareness reckon.01 1 ----- 0:1-ARG0 1:0-rel 2:1-ARG1 
nw/wsj/15/wsj_1500.parse 11 4 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARGM-DIS 2:1-ARG0=Agent 4:0-rel 5:1-ARG1=Topic 
nw/wsj/15/wsj_1500.parse 11 7 gold believe-v 29.5-1 Awareness believe.01 1 ----- 6:1-ARG0 7:0-rel 8:1-ARG1 
nw/wsj/15/wsj_1500.parse 11 18 gold lead-v 59 Causation lead.03 1 ----- 9:2-ARG0=Agent 17:0-ARGM-MOD 18:0-rel 19:1-ARG2=Result 
nw/wsj/15/wsj_1500.parse 13 16 gold warn-v 37.9-1 NF warn.01 1 ----- 0:2-ARG0=Agent 16:0-rel 17:1-ARG1=Topic 
nw/wsj/15/wsj_1500.parse 13 37 gold want-v 32.1-1-1 Desiring want.01 1 ----- 35:1-ARG0=Pivot;Experiencer 36:0-ARGM-MOD 37:0-rel 38:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/15/wsj_1500.parse 13 40 gold see-v 30.1-1 Grasp see.01 3 ----- 35:1*38:1-ARG0=Experiencer 40:0-rel 41:2-ARG1=Stimulus 46:1-ARGM-TMP 
nw/wsj/15/wsj_1500.parse 13 48 gold adjust-v 26.9 NF adjust.01 1 ----- 35:1*38:1*47:1-ARG0=Agent 48:0-rel 49:1-ARG1=Patient 
nw/wsj/15/wsj_1500.parse 14 11 gold want-v 32.1-1-1 Desiring want.01 1 ----- 0:1-ARGM-DIS 8:1-ARG0=Pivot;Experiencer 10:0-ARGM-MOD 11:0-rel 12:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/15/wsj_1500.parse 15 7 gold remain-v 47.1-1 State_continue remain.01 1 ----- 0:1-ARGM-TMP 2:2-ARG1=Theme 7:0-rel 8:1-ARG3 
nw/wsj/15/wsj_1500.parse 16 8 gold warn-v 37.9-1 NF warn.01 1 ----- 0:1-ARGM-LOC 6:1-ARG0=Agent 8:0-rel 9:1-ARG1=Topic 
nw/wsj/15/wsj_1500.parse 16 15 gold expect-v 62 IN expect.01 1 ----- 10:1*16:1-ARG1=Theme 13:0-ARGM-MOD 15:0-rel 17:1-ARGM-TMP 
nw/wsj/15/wsj_1500.parse 18 4 gold release-v 80-1 Releasing release.01 2 ----- 1:2*5:1-ARG1=Theme 4:0-rel 6:1-ARGM-TMP 1:2*5:1-LINK-PSV 
nw/wsj/15/wsj_1500.parse 18 9 gold suggest-v 37.7-1-1 Statement suggest.01 2 ----- 1:3-ARG0=Agent;Speaker 8:0-ARGM-NEG 9:0-rel 10:1-ARG1=Topic;Message 
nw/wsj/15/wsj_1500.parse 19 2 gold show-v 78-1-1 IN show.01 1 ----- 0:1-ARG0 2:0-rel 3:1-ARG1 
nw/wsj/15/wsj_1500.parse 19 5 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 2 ----- 4:1-ARG1 5:0-rel 6:1-ARG2 8:1-ARGM-TMP 12:1-ARG3 
nw/wsj/15/wsj_1500.parse 20 1 gold compare-v 22.2-2 Evaluative_comparison compare.01 1 ----- 0:1-ARG1=Patient 1:0-rel 2:1-ARG2=Co-Patient 
nw/wsj/15/wsj_1500.parse 21 2 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/15/wsj_1500.parse 21 6 gold show-v 78-1-1 IN show.01 1 ----- 4:1-ARG0 6:0-rel 7:1-ARG1 
nw/wsj/15/wsj_1500.parse 21 32 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 1 ----- 18:1-ARG0=Cause 31:0-ARGM-MOD 32:0-rel 33:2-ARG1=Patient 
nw/wsj/15/wsj_1500.parse 22 5 gold make-v 29.3 Causation make.02 3 ----- 0:1-ARGM-ADV 2:1-ARG0=Agent 5:0-rel 6:3-ARG1=Theme 
nw/wsj/15/wsj_1500.parse 22 24 gold ensure-v 99 NF ensure.01 1 ----- 10:1*13:1*21:1-ARG0=Cause 24:0-rel 25:2-ARG1=Theme 
nw/wsj/15/wsj_1500.parse 22 37 gold decline-v 45.6-1 Change_position_on_a_scale decline.01 3 ----- 34:1-ARG1=Patient;Item 36:0-ARGM-NEG 37:0-rel 38:1-ARGM-MNR 
nw/wsj/15/wsj_1500.parse 23 3 gold remind-v 37.2 NF remind.01 1 ----- 0:1-ARGM-TMP 2:1-ARG0=Agent 3:0-rel 4:1-ARG2=Topic 6:1-ARG1=Recipient 
nw/wsj/15/wsj_1500.parse 23 12 gold allow-v 64 IN allow.01 1 ----- 7:1-ARG0 10:0-ARGM-MOD 11:0-ARGM-NEG 12:0-rel 13:3-ARG1 
nw/wsj/15/wsj_1500.parse 24 1 gold agree-v 36.1-1 IN agree.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:1*13:1-ARG1=Theme 
nw/wsj/15/wsj_1500.parse 24 6 gold hold-v 15.1-1 Detaining hold.01 2 ----- 5:1-ARG0=Agent 6:0-rel 7:1-ARG1=Theme 8:1-ARG2 
nw/wsj/15/wsj_1500.parse 24 23 gold push-v 23.2 Cause_change_of_position_on_a_scale push.01 1 ----- 20:1*24:1-ARG1=Patient 21:0-ARGM-MOD 23:0-rel 26:1-ARGM-ADV 
nw/wsj/15/wsj_1500.parse 25 3 gold warn-v 37.9-1 NF warn.01 1 ----- 2:1-ARG0=Agent 3:0-rel 0:1-ARG1-DSP=Topic 
nw/wsj/15/wsj_1500.parse 25 17 gold make-v 29.3 Causation make.02 3 ----- 0:0-ARGM-DIS 7:2-ARG0=Agent 15:0-ARGM-MOD 16:1-ARGM-MNR 17:0-rel 18:2-ARG1=Theme 
nw/wsj/15/wsj_1500.parse 25 20 gold sound-v 30.4 Appearance sound.01 1 ----- 18:1-ARG1=Stimulus;Phenomenon 20:0-rel 21:1-ARG2 
nw/wsj/15/wsj_1500.parse 27 4 gold decline-v 45.6-1 Change_position_on_a_scale decline.01 3 ----- 0:1-ARGM-LOC 3:1-ARG1=Patient;Item 4:0-rel 5:1-ARG4 12:1-ARG3 19:2-ARGM-TMP 
nw/wsj/15/wsj_1500.parse 28 1 gold suggest-v 37.7-1-1 Statement suggest.01 1 ----- 0:1-ARG0=Agent;Speaker 1:0-rel 2:1-ARG1=Topic;Message 
nw/wsj/15/wsj_1500.parse 28 6 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 1 ----- 4:1-ARG1=Patient;Item 6:0-rel 7:2-ARG4 
nw/wsj/15/wsj_1500.parse 28 16 gold force-v 59 NF force.01 1 ----- 3:1-ARGM-ADV 12:1*17:1-ARG1=Patient 14:0-ARGM-MOD 16:0-rel 19:1-ARG2=Result 
nw/wsj/15/wsj_1500.parse 28 34 gold ensure-v 99 NF ensure.01 1 ----- 12:1*17:1*18:1*26:1-ARG0=Cause 34:0-rel 35:1-ARG1=Theme 
nw/wsj/15/wsj_1500.parse 28 41 gold remain-v 47.1-1 State_continue remain.01 1 ----- 36:2-ARG1=Theme 41:0-rel 42:1-ARG3 
nw/wsj/15/wsj_1500.parse 30 2 gold post-v 11.1 Sending post.01 1 ----- 0:1-ARG0=Agent;Sender 2:0-rel 3:1-ARG1=Theme;Theme 4:1-ARGM-LOC 7:1-ARGM-ADV 
nw/wsj/15/wsj_1500.parse 31 3 gold say-v 37.7-1 IN say.01 1 ----- 0:2-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/15/wsj_1500.parse 31 9 gold begin-v 55.1-1 Activity_start begin.01 1 ----- 5:1,10:2-ARG1=Theme;Activity 9:0-rel 
nw/wsj/15/wsj_1500.parse 31 36 gold plunge-v 45.6-1 Motion_directional plunge.01 2 ----- 27:1*30:1*41:1-ARGM-TMP 31:1-ARG1=Patient 36:0-rel 37:2-ARG2=Extent 27:1*30:1-LINK-SLC 
nw/wsj/15/wsj_1500.parse 32 2 gold predict-v 78 Predicting predict.01 1 ----- 0:1-ARG0=Cause;Evidence/Speaker 2:0-rel 3:1-ARG1=Topic;Eventuality 
nw/wsj/15/wsj_1500.parse 32 13 gold shift-v 11.1 NF shift.01 1 ----- 4:1-ARGM-TMP 8:1-ARG0 12:0-ARGM-MOD 13:0-rel 14:1-ARG1=Theme 16:0-ARGM-DIR 17:1-ARG2=Destination 21:2-ARGM-ADV 
nw/wsj/15/wsj_1500.parse 32 22 gold keep-v 55.6 Activity_ongoing keep.01 8.12 ----- 8:1*21:1-ARG0 22:0-rel 23:2-ARG1 27:1-ARGM-PRP 
nw/wsj/15/wsj_1500.parse 33 11 gold quote-v 37.1.1-1 NF quote.01 3 ----- 0:1-ARGM-TMP 8:1*12:1-ARG1=Source 11:0-rel 13:1-ARG2=Topic 
nw/wsj/15/wsj_1500.parse 34 5 gold change-v 13.6-1 Exchange change.02 3 ----- 0:1-ARG1=Theme 4:1-ARGM-DIS 5:0-rel 6:1-ARG2=Co-Theme 7:1-ARGM-MNR 
nw/wsj/15/wsj_1500.parse 35 8 gold open-v 48.1.1 NF open.02 2 ----- 0:1-ARGM-LOC 2:1-ARGM-TMP 5:1-ARG1=Theme 8:0-rel 9:1-ARGM-PNC 11:1-ARGM-EXT 
nw/wsj/15/wsj_1500.parse 36 12 gold settle-v 46 Colonization settle.03 4 ----- 0:1-ARGM-LOC 4:1-ARGM-LOC 8:2-ARG1=Theme 12:0-rel 13:1-ARG4=Location 20:1-ARG2 
nw/wsj/15/wsj_1500.parse 37 0 gold estimate-v 54.4 Estimating estimate.01 1 ----- 0:0-rel 1:0-ARG1=Theme 
nw/wsj/15/wsj_1500.parse 38 10 gold quote-v 37.1.1-1 NF quote.01 3 ----- 0:1-ARGM-LOC 3:1-ARGM-LOC 6:1-ARGM-TMP 8:1*11:1-ARG1=Source 10:0-rel 12:1-ARG2=Topic 
