nw/wsj/18/wsj_1810.parse 0 12 gold slump-v 47.6 NF slump.01 2 ----- 10:1-ARG1=Theme 12:0-rel 
nw/wsj/18/wsj_1810.parse 1 1 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 0:1-ARG1 1:0-rel 2:1-ARG2 3:1-ARGM-TMP 
nw/wsj/18/wsj_1810.parse 2 7 gold hover-v 47.3 NF hover.01 2 ----- 0:2-ARG1=Theme 7:0-rel 8:1-ARG2=Location 12:2-ARGM-TMP 17:2-ARGM-PRD 
nw/wsj/18/wsj_1810.parse 3 1 gold help-v 72-1 Assistance help.01 null ----- 0:1*10:1-ARG2=Beneficiary;Benefited_party 1:0-rel 3:1-ARG0=Agent;Helper 
nw/wsj/18/wsj_1810.parse 3 15 gold gain-v 45.6-1 Change_position_on_a_scale gain.01 null ----- 0:2-ARGM-PRD 10:1*18:1-ARG1=Patient;Item 15:0-rel 16:1-ARG2=Extent;Difference 18:2-ARG4 
nw/wsj/18/wsj_1810.parse 4 6 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 null ----- 0:0-ARGM-DIS 1:1-ARG1=Patient;Item 6:0-rel 7:1-ARGM-TMP 13:1-ARGM-TMP 
nw/wsj/18/wsj_1810.parse 4 16 gold dump-v 9.3-1-1 NF dump.01 2 ----- 14:1-ARG0=Agent 16:0-rel 17:1-ARG1=Theme 
nw/wsj/18/wsj_1810.parse 5 11 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 7:1*9:1-ARG0=Agent;Buyer 11:0-rel 12:1-ARG1=Theme;Goods 
nw/wsj/18/wsj_1810.parse 5 22 gold lead-v 59 Causation lead.03 null ----- 17:1-ARG0=Agent 21:0-ARGM-MOD 22:0-rel 23:1-ARG2=Result 
nw/wsj/18/wsj_1810.parse 7 6 gold lower-v 9.4 NF lower.01 2 ----- 2:1-ARG0=Agent 5:0-ARGM-MOD 6:0-rel 7:1-ARG1=Theme 9:1-ARGM-TMP 
nw/wsj/18/wsj_1810.parse 7 12 gold help-v 72-1 Assistance help.01 null ----- 0:1*18:1-ARG0=Agent;Helper 12:0-rel 13:1-ARG1=Theme;Goal/Focal_entity 
nw/wsj/18/wsj_1810.parse 7 13 gold push-v 23.2 Cause_change_of_position_on_a_scale push.01 null ----- 0:1*18:1-ARG0=Agent 13:0-rel 14:1-ARG1=Patient 16:1-ARG2=Co-Patient 17:1-ARGM-TMP 
nw/wsj/18/wsj_1810.parse 7 19 gold boost-v 102 NF boost.01 1 ----- 0:1*18:1-ARG0=Agent 19:0-rel 20:1-ARG1=Theme 
nw/wsj/18/wsj_1810.parse 7 23 gold say-v 37.7-1 IN say.01 null ----- 22:1-ARG0=Agent 23:0-rel 24:1-ARG1=Topic 
nw/wsj/18/wsj_1810.parse 8 3 gold remain-v 47.1-1 State_continue remain.01 null ----- 0:0-ARGM-DIS 1:1-ARG1=Theme 3:0-rel 4:1-ARG3 8:2-ARGM-CAU 
nw/wsj/18/wsj_1810.parse 8 11 gold expect-v 62 IN expect.01 null ----- 10:1-ARG0=Experiencer 11:0-rel 12:3-ARG1=Theme 
nw/wsj/18/wsj_1810.parse 8 12 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 12:0-rel 13:0-ARG1 
nw/wsj/18/wsj_1810.parse 8 23 gold make-v 29.3 Causation make.02 null ----- 12:2*20:1*21:1-ARG0=Agent 22:0-ARGM-MOD 23:0-rel 24:3-ARG1=Theme 12:2*20:1-LINK-SLC 
nw/wsj/18/wsj_1810.parse 9 3 gold surprise-v 31.1 Experiencer_obj surprise.01 1 ----- 1:1-ARG1=Experiencer;Experiencer 3:0-rel 4:1-ARG0=Stimulus;Stimulus 
nw/wsj/18/wsj_1810.parse 9 8 gold see-v 30.1-1 Perception_experience see.01 null ----- 5:1-ARG0=Experiencer;Perceiver_passive 7:0-ARGM-NEG 8:0-rel 9:1,12:1-ARG1=Stimulus;Phenomenon 
nw/wsj/18/wsj_1810.parse 9 15 gold say-v 37.7-1 IN say.01 null ----- 1:2*17:1-ARG1=Topic 15:0-rel 18:2-ARG0=Agent 
nw/wsj/18/wsj_1810.parse 10 2 gold think-v 29.9-2 IN think.01 null ----- 1:1-ARG0 2:0-rel 3:1-ARG1 
nw/wsj/18/wsj_1810.parse 10 14 gold begin-v 55.1-1 Process_start begin.01 null ----- 4:2-ARG1=Theme;Event 13:1-ARGM-ADV 14:0-rel 
nw/wsj/18/wsj_1810.parse 10 27 gold issue-v 13.3 NF issue.01 null ----- 23:1*24:1*25:1-ARG0=Agent 27:0-rel 28:1-ARG1=Theme 23:1*24:1-LINK-SLC 
nw/wsj/18/wsj_1810.parse 11 13 gold add-v 37.7 NF add.01 null ----- 0:2*14:1-ARG1=Topic 11:1-ARG0=Agent 13:0-rel 
nw/wsj/18/wsj_1810.parse 13 6 gold react-v 31.3-9 NF react.01 1 ----- 2:1-ARG0=Experiencer 6:0-rel 7:1-ARG1=Stimulus 13:1-ARG2 
nw/wsj/18/wsj_1810.parse 13 21 gold say-v 37.7-1 IN say.01 null ----- 0:2*23:1-ARG1=Topic 21:0-rel 24:2-ARG0=Agent 
nw/wsj/18/wsj_1810.parse 14 3 gold begin-v 55.1-1 Process_start begin.01 null ----- 1:1,4:2-ARG1=Theme;Event 3:0-rel 
nw/wsj/18/wsj_1810.parse 16 3 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 0:1-ARG1 2:1-ARGM-TMP 3:0-rel 4:1-ARGM-CAU 
nw/wsj/18/wsj_1810.parse 17 14 gold conclude-v 97.2 Coming_to_believe conclude.01 null ----- 13:1-ARG0=Agent 14:0-rel 15:2-ARG1=Theme 
nw/wsj/18/wsj_1810.parse 19 2 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 0:1-ARG1 2:0-rel 
nw/wsj/18/wsj_1810.parse 23 10 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 null ----- 0:2-ARG1=Patient;Item 10:0-rel 11:1-ARG2=Extent;Difference 12:1-ARG4 
nw/wsj/18/wsj_1810.parse 24 2 gold drop-v 45.6-1 Change_position_on_a_scale drop.01 null ----- 0:1-ARG1=Patient;Item 2:0-rel 
