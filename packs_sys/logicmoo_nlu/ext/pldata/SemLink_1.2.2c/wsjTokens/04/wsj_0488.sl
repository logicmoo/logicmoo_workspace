nw/wsj/04/wsj_0488.parse 0 8 gold boost-v 102 NF boost.01 1 ----- 0:1*7:1-ARG1=Theme 8:0-rel 10:1-ARG0=Agent 
nw/wsj/04/wsj_0488.parse 1 5 gold climb-v 45.6-1 Change_position_on_a_scale climb.02 null ----- 0:1-ARG1=Patient;Item 5:0-rel 6:1-ARG2=Extent;Difference 8:1-ARGM-LOC 
nw/wsj/04/wsj_0488.parse 2 3 gold fix-v 54.4 Bail_decision fix.03 null ----- 1:1*4:1-ARG1=Theme 3:0-rel 5:1-ARG2=Value 9:1-ARGM-CAU 
nw/wsj/04/wsj_0488.parse 2 24 gold say-v 37.7-1 IN say.01 null ----- 1:2*25:1-ARG1=Topic 24:0-rel 26:2-ARG0=Agent 
nw/wsj/04/wsj_0488.parse 3 2 gold predict-v 78 Predicting predict.01 1 ----- 0:1-ARG0=Cause;Evidence/Speaker 2:0-rel 3:1-ARG1=Topic;Eventuality 
nw/wsj/04/wsj_0488.parse 3 15 gold push-v 23.2 Cause_change_of_position_on_a_scale push.01 null ----- 8:2*12:1*13:1-ARG0=Agent 15:0-rel 16:1-ARG1=Patient 19:1-ARG2=Co-Patient 8:2*12:1-LINK-SLC 
nw/wsj/04/wsj_0488.parse 4 12 gold continue-v 55.3 Process_continue continue.01 null ----- 9:1,13:2-ARG1 11:0-ARGM-MOD 12:0-rel 
nw/wsj/04/wsj_0488.parse 4 15 gold hold-v 15.1-1 Inhibit_movement hold.01 null ----- 9:1*13:1-ARG0=Agent 15:0-rel 16:1-ARG1=Theme 
nw/wsj/04/wsj_0488.parse 5 11 gold help-v 72-1 Assistance help.01 null ----- 3:2*12:1-ARG0=Agent;Helper 11:0-rel 12:2-ARG1=Theme;Goal/Focal_entity 
nw/wsj/04/wsj_0488.parse 5 20 gold lend-v 13.1 NF lend.01 4 ----- 3:2*12:1-ARG0=Agent 20:0-rel 21:1-ARG1=Theme 22:1-ARG2=Recipient 
nw/wsj/04/wsj_0488.parse 6 10 gold quote-v 37.1.1-1 NF quote.01 null ----- 0:1-ARGM-LOC 5:1-ARGM-TMP 7:1*11:1-ARG1=Source 10:0-rel 12:2-ARG2=Topic 
nw/wsj/04/wsj_0488.parse 7 2 gold quote-v 37.1.1-1 NF quote.01 null ----- 0:1*3:1-ARG1=Source 2:0-rel 4:1-ARG2=Topic 
nw/wsj/04/wsj_0488.parse 8 2 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 0:1-ARG1=Patient;Item 2:0-rel 3:1-ARGM-ADV 
nw/wsj/04/wsj_0488.parse 9 7 gold open-v 48.1.1 NF open.02 null ----- 0:1-ARGM-LOC 2:1-ARGM-TMP 4:1-ARG1=Theme 7:0-rel 8:1-ARGM-PNC 10:1-ARGM-MNR 
nw/wsj/04/wsj_0488.parse 11 19 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-ADV 16:2-ARG0=Agent 19:0-rel 20:1-ARG1=Topic 
nw/wsj/04/wsj_0488.parse 11 28 gold post-v 11.1 Sending post.01 null ----- 24:1-ARG0=Agent;Sender 26:0-ARGM-MOD 27:1-ARGM-TMP 28:0-rel 29:1-ARG1=Theme;Theme 
nw/wsj/04/wsj_0488.parse 12 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/04/wsj_0488.parse 12 4 gold make-v 26.1-1 IN make.01 null ----- 3:1*5:1-ARG1=Product 4:0-rel 6:1-ARGM-TMP 9:2-ARGM-PRP 3:1*5:1-LINK-PSV 
nw/wsj/04/wsj_0488.parse 12 31 gold fail-v 74-2 IN fail.01 null ----- 3:2*32:1-ARG1 30:1-ARGM-ADV 31:0-rel 32:2-ARG2 
nw/wsj/04/wsj_0488.parse 12 34 gold reassure-v 31.1 Experiencer_obj reassure.01 1 ----- 3:2*32:1-ARG0=Stimulus;Stimulus 34:0-rel 35:1-ARG1=Experiencer;Experiencer 
nw/wsj/04/wsj_0488.parse 13 3 gold televise-v 25.4 NF televise.01 1 ----- 3:0-rel 4:0-ARG1=Theme 
nw/wsj/04/wsj_0488.parse 13 13 gold reiterate-v 37.7-1 Statement reiterate.01 1 ----- 0:1-ARGM-LOC 11:1-ARG0=Agent;Speaker 13:0-rel 14:1-ARG1=Topic;Message 
nw/wsj/04/wsj_0488.parse 13 18 gold keep-v 55.6 Cause_to_continue keep.02 null ----- 11:1*16:1-ARG0=Agent 18:0-rel 20:2-ARG1=Theme 11:1*16:1-LINK-PRO 
nw/wsj/04/wsj_0488.parse 13 23 gold warn-v 37.9-1 NF warn.01 1 ----- 0:1-ARGM-LOC 11:1-ARG0=Agent 23:0-rel 24:1-ARGM-TMP 25:1-ARG1=Topic 
nw/wsj/04/wsj_0488.parse 13 38 gold provide-v 13.4.1-2 Supply provide.01 null ----- 26:2-ARG0=Agent 37:0-ARGM-MOD 38:0-rel 39:2-ARG1=Theme 
nw/wsj/04/wsj_0488.parse 14 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/04/wsj_0488.parse 14 12 gold depend-v 70 Reliance depend.01 null ----- 3:2-ARG0=Agent;Protagonist 11:0-ARGM-MOD 12:0-rel 13:1-ARG1=Theme;Means/Instrument/Intermediary/Benefit/Purpose 
nw/wsj/04/wsj_0488.parse 15 12 gold lead-v 51.7 Cotheme lead.01 null ----- 12:0-rel 13:0-ARG0=Agent;Theme 
nw/wsj/04/wsj_0488.parse 15 22 gold call-v 60 IN call.03 null ----- 11:2*19:1*20:1-ARG0=Agent 22:0-rel 23:1-ARG1=Topic 11:2*19:1-LINK-SLC 
nw/wsj/04/wsj_0488.parse 16 8 gold show-v 78-1-1 IN show.01 null ----- 0:1-ARGM-TMP 5:1-ARG0 8:0-rel 9:1-ARG1 
nw/wsj/04/wsj_0488.parse 16 13 gold hit-v 51.8 Impact hit.02 null ----- 10:1-ARG0=Agent 13:0-rel 14:2-ARG1=Destination 
nw/wsj/04/wsj_0488.parse 16 24 gold begin-v 55.1-1 Process_start begin.01 null ----- 23:1-ARG1=Theme;Event 24:0-rel 25:2-ARGM-TMP 
nw/wsj/04/wsj_0488.parse 17 13 gold fail-v 74-2 IN fail.01 null ----- 0:2*14:1-ARG1 12:1-ARGM-ADV 13:0-rel 14:2-ARG2 20:1-ARGM-ADV 
nw/wsj/04/wsj_0488.parse 17 25 gold maintain-v 29.5-2 Statement maintain.01 null ----- 2:1*23:1-ARG0=Agent;Speaker 25:0-rel 26:2-ARG1=Theme;Addressee 2:1*23:1-LINK-PRO 
nw/wsj/04/wsj_0488.parse 18 14 gold help-v 72-1 Assistance help.01 null ----- 0:1-ARGM-ADV 8:2-ARG0=Agent;Helper 12:0-ARGM-MOD 13:0-ARGM-NEG 14:0-rel 15:1-ARG1=Theme;Goal/Focal_entity 17:1-ARGM-ADV 
nw/wsj/04/wsj_0488.parse 18 21 gold continue-v 55.3 Process_continue continue.01 null ----- 18:2*22:2-ARG1 21:0-rel 
nw/wsj/04/wsj_0488.parse 19 11 gold acknowledge-v 37.10 Statement acknowledge.01 1 ----- 0:1*10:1-ARG0=Agent 11:0-rel 12:1-ARG1=Topic 
nw/wsj/04/wsj_0488.parse 20 15 gold paint-v 26.7-2-1 Communicate_categorization paint.02 3 ----- 12:1-ARG0=Agent 13:0-ARGM-MOD 15:0-rel 16:2-ARG1=Theme 
nw/wsj/04/wsj_0488.parse 20 26 gold say-v 37.7-1 IN say.01 null ----- 1:3*27:1-ARG1=Topic 25:1-ARG0=Agent 26:0-rel 
nw/wsj/04/wsj_0488.parse 21 1 gold predict-v 78 Predicting predict.01 1 ----- 0:1-ARG0=Cause;Evidence/Speaker 1:0-rel 2:1-ARG1=Topic;Eventuality 
nw/wsj/04/wsj_0488.parse 21 6 gold continue-v 55.3 Process_continue continue.01 null ----- 3:1*7:2-ARG1 5:0-ARGM-MOD 6:0-rel 27:1-ARGM-TMP 
nw/wsj/04/wsj_0488.parse 21 9 gold trade-v 13.6-1 Exchange trade.01 1 ----- 3:1*7:1-ARG1=Theme 9:0-rel 10:1-ARGM-MNR 16:1-ARGM-MNR 
nw/wsj/04/wsj_0488.parse 21 29 gold recover-v 13.5.2 NF recover.02 1 ----- 3:1*28:1-ARG0=Agent 29:0-rel 30:1-ARG1=Theme 
nw/wsj/04/wsj_0488.parse 22 13 gold help-v 72-1 Assistance help.01 null ----- 4:2*14:1-ARG0=Agent;Helper 13:0-rel 14:2-ARG1=Theme;Goal/Focal_entity 
nw/wsj/04/wsj_0488.parse 23 1 gold observe-v 29.5-2 Statement observe.02 null ----- 0:1-ARG0=Agent;Speaker 1:0-rel 2:1-ARG1=Predicate;Message 
nw/wsj/04/wsj_0488.parse 23 16 gold keep-v 55.6 Cause_to_continue keep.04 null ----- 3:2-ARG0=Agent 16:0-rel 17:2-ARG1=Theme 
nw/wsj/04/wsj_0488.parse 24 2 gold begin-v 55.1-1 Process_start begin.01 null ----- 0:1-ARG1=Theme;Event 2:0-rel 3:1-ARGM-TMP 4:1-ARGM-MNR 8:1-ARGM-LOC 11:2-ARGM-PRD 
nw/wsj/04/wsj_0488.parse 25 11 gold dominate-v 47.8-2 NF dominate.01 1 ----- 9:1*12:1-ARG1=Co-Theme 11:0-rel 13:1-ARG0=Theme 9:1*12:1-LINK-PSV 
nw/wsj/04/wsj_0488.parse 26 12 gold settle-v 46 Colonization settle.03 null ----- 0:1-ARGM-LOC 8:2-ARG1=Theme 12:0-rel 13:1-ARG4=Location 20:1-ARG2 
nw/wsj/04/wsj_0488.parse 27 0 gold estimate-v 54.4 Estimating estimate.01 1 ----- 0:0-rel 1:0-ARG1=Theme 
nw/wsj/04/wsj_0488.parse 28 10 gold quote-v 37.1.1-1 NF quote.01 null ----- 0:1-ARGM-LOC 6:1-ARGM-TMP 8:1*11:1-ARG1=Source 10:0-rel 12:1-ARG2=Topic 
