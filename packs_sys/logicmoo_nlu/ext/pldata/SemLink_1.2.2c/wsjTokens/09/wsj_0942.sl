nw/wsj/09/wsj_0942.parse 0 5 gold trade-v 13.6-1 Exchange trade.01 null ----- 2:0-ARG0=Agent 5:0-rel 6:0-ARG2=Co-Agent 
nw/wsj/09/wsj_0942.parse 0 7 gold give-v 13.1-1 Giving give.01 null ----- 0:2-ARG0=Agent;Donor 7:0-rel 8:1-ARG1=Theme;Theme 13:1-ARG2=Recipient;Recipient 
nw/wsj/09/wsj_0942.parse 0 23 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 null ----- 17:1-ARG0=Cause 23:0-rel 24:2-ARG1=Patient 
nw/wsj/09/wsj_0942.parse 1 27 gold call-v 29.3 IN call.01 null ----- 0:1-ARGM-LOC 24:1-ARG0=Agent 27:0-rel 28:1*32:1-ARG1=Theme 31:1-ARG2=Result 
nw/wsj/09/wsj_0942.parse 2 10 gold tell-v 37.2-1 Telling tell.01 null ----- 0:0-ARGM-DIS 1:3-ARG0=Agent;Speaker 10:0-rel 11:2-ARG2=Recipient;Addressee 17:2-ARG1=Topic;Message 
nw/wsj/09/wsj_0942.parse 2 27 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 25:1-ARG0 26:0-ARGM-MOD 27:0-rel 28:2-ARG1 
nw/wsj/09/wsj_0942.parse 3 9 gold submit-v 13.2-1-1 IN submit.01 null ----- 4:1*10:1-ARG1=Theme 9:0-rel 11:1-ARG2=Recipient 15:1-ARGM-TMP 4:1*10:1-LINK-PSV 
nw/wsj/09/wsj_0942.parse 3 17 gold propose-v 37.7-1 Statement propose.01 null ----- 0:1-ARG0=Agent;Speaker 3:1-ARGM-LOC 17:0-rel 18:2-ARG1=Topic;Message 
nw/wsj/09/wsj_0942.parse 3 19 gold curb-v 42.3 NF curb.01 1 ----- 0:1*18:1-ARG0=Agent 19:0-rel 20:2-ARG1=Patient 23:1-ARGM-TMP 0:1*18:1-LINK-PRO 
nw/wsj/09/wsj_0942.parse 3 27 gold eliminate-v 10.1 Removing eliminate.01 null ----- 0:1*18:1-ARG0=Agent;Agent/Cause 27:0-rel 28:1-ARG1=Theme;Theme 30:1-ARGM-TMP 0:1*18:1-LINK-PRO 
nw/wsj/09/wsj_0942.parse 4 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/09/wsj_0942.parse 4 11 gold intend-v 62 Purpose intend.01 null ----- 4:1*12:1-ARG2 11:0-rel 13:1-ARG1 
nw/wsj/09/wsj_0942.parse 4 20 gold remove-v 10.1 Removing remove.01 null ----- 18:1-ARG0=Agent;Agent/Cause 19:1-ARGM-MNR 20:0-rel 21:2-ARG1=Theme;Theme 
nw/wsj/09/wsj_0942.parse 5 3 gold react-v 31.3-9 NF react.01 1 ----- 0:0-ARGM-DIS 1:1*6:1-ARG0=Experiencer 3:0-rel 4:1-ARGM-MNR 6:2-ARGM-PRD 
nw/wsj/09/wsj_0942.parse 5 17 gold destroy-v 44 Destroying destroy.01 null ----- 9:2*15:1-ARG0=Agent;Cause/Destroyer 17:0-rel 18:2-ARG1=Patient;Undergoer 9:2*15:1-LINK-PRO 
nw/wsj/09/wsj_0942.parse 6 15 gold say-v 37.7-1 IN say.01 null ----- 1:2*16:1-ARG1=Topic 13:1-ARG0=Agent 15:0-rel 17:1-ARGM-LOC 
nw/wsj/09/wsj_0942.parse 7 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/09/wsj_0942.parse 7 6 gold irk-v 31.1 Experiencer_obj irk.01 null ----- 4:1*7:1-ARG1=Experiencer;Experiencer 6:0-rel 8:1-ARG0=Stimulus;Stimulus 
nw/wsj/09/wsj_0942.parse 7 12 gold set-v 9.1-2 IN set.01 null ----- 9:1-ARG0=Agent 12:0-rel 13:1-ARG1=Theme 
nw/wsj/09/wsj_0942.parse 7 18 gold insist-v 37.7 Statement insist.01 null ----- 9:1-ARG0=Agent;Speaker 18:0-rel 19:1-ARG1=Topic;Message 
nw/wsj/09/wsj_0942.parse 8 5 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 5:0-rel 6:1-ARG1=Topic 
nw/wsj/09/wsj_0942.parse 8 11 gold call-v 13.5.1 IN call.02 null ----- 7:1-ARG0=Agent 11:0-rel 12:1-ARG2=Beneficiary 15:2-ARG1=Theme 
nw/wsj/09/wsj_0942.parse 8 17 gold reach-v 13.5.1 NF reach.01 null ----- 15:1*18:1-ARG1=Theme 17:0-rel 19:1-ARG0=Agent 22:1-ARGM-TMP 24:1-ARGM-LOC 15:1*18:1-LINK-PSV 
nw/wsj/09/wsj_0942.parse 9 6 gold reply-v 37.7 Communication_response reply.01 1 ----- 0:2-ARG0=Agent;Speaker 6:0-rel 7:1-ARG2=Topic;Message/Trigger 
nw/wsj/09/wsj_0942.parse 10 1 gold say-v 37.7-1 IN say.01 null ----- 0:1*13:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 13:2-ARGM-ADV 
nw/wsj/09/wsj_0942.parse 10 5 gold surprise-v 31.1 Experiencer_obj surprise.01 1 ----- 3:1*6:1-ARG1=Experiencer;Experiencer 5:0-rel 7:1-ARG0=Stimulus;Stimulus 
nw/wsj/09/wsj_0942.parse 10 14 gold call-v 29.3 IN call.01 null ----- 0:1*13:1-ARG0=Agent 14:0-rel 15:1*17:1-ARG1=Theme 16:1-ARG2=Result 
nw/wsj/09/wsj_0942.parse 11 5 gold criticize-v 33 Judgment_communication criticize.01 null ----- 0:1*6:1-ARG1=Theme;Evaluee 3:1-ARGM-DIS 5:0-rel 7:1-ARG0=Agent;Communicator 
nw/wsj/09/wsj_0942.parse 11 11 gold develop-v 48.1.1 IN develop.01 null ----- 11:0-rel 12:0-ARG1=Location 
nw/wsj/09/wsj_0942.parse 11 16 gold say-v 37.7-1 IN say.01 null ----- 8:2*14:1*15:1-ARG0=Agent 16:0-rel 17:1-ARG1=Topic 8:2*14:1-LINK-SLC 
nw/wsj/09/wsj_0942.parse 12 29 gold need-v 32.1-1-1 NF need.01 null ----- 22:1*27:1-ARG0=Pivot 29:0-rel 30:1-ARG1=Theme 31:1-ARGM-TMP 
nw/wsj/09/wsj_0942.parse 13 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/09/wsj_0942.parse 13 14 gold develop-v 48.1.1 IN develop.01 null ----- 14:0-rel 15:0-ARG1=Location 
nw/wsj/09/wsj_0942.parse 14 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/09/wsj_0942.parse 14 8 gold allow-v 64 IN allow.01 null ----- 5:1-ARG0 7:0-ARGM-MOD 8:0-rel 9:2-ARG1 
nw/wsj/09/wsj_0942.parse 14 24 gold achieve-v 55.2 Accomplishment achieve.01 1 ----- 14:1*26:1-ARGM-MNR 17:1*25:1-ARG1=Theme 22:0-ARGM-MOD 24:0-rel 
nw/wsj/09/wsj_0942.parse 15 16 gold convert-v 26.6.1-1 IN convert.01 1 ----- 4:1*15:1-ARG0=Agent 16:0-rel 17:1-ARG1=Patient 20:1-ARG2=Result 
nw/wsj/09/wsj_0942.parse 15 27 gold exist-v 47.1-1 Existence exist.01 null ----- 27:0-rel 28:0-ARG1=Theme;Entity 
nw/wsj/09/wsj_0942.parse 16 8 gold convert-v 26.6.1-1 IN convert.01 1 ----- 7:1-ARG0=Agent 8:0-rel 9:1-ARG1=Patient 11:1-ARG2=Result 
nw/wsj/09/wsj_0942.parse 17 5 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 4:1-ARGM-DIS 5:0-rel 6:1-ARG1=Topic 
nw/wsj/09/wsj_0942.parse 17 10 gold raise-v 9.4 NF raise.01 null ----- 7:1-ARG0=Agent 8:0-ARGM-MOD 9:1-ARGM-TMP 10:0-rel 11:2-ARG1=Theme 15:1-ARGM-ADV 
nw/wsj/09/wsj_0942.parse 17 17 gold experience-v 30.2 Feeling experience.01 1 ----- 16:1-ARG0=Experiencer;Experiencer 17:0-rel 18:2-ARG1=Stimulus;Emotion/Emotional_state 
nw/wsj/09/wsj_0942.parse 18 2 gold establish-v 55.5-1 Intentionally_create establish.01 null ----- 0:1-ARG0 1:0-ARGM-MOD 2:0-rel 3:2-ARG1 
nw/wsj/09/wsj_0942.parse 18 7 gold prevent-v 67 Thwarting prevent.01 1 ----- 3:1*4:1*5:1-ARG3 7:0-rel 8:1*10:1-ARG1=Theme;Protagonist/Action 9:1-ARG2=Theme;Protagonist/Action 3:1*4:1-LINK-SLC 
nw/wsj/09/wsj_0942.parse 18 11 gold use-v 105 IN use.01 null ----- 8:1*10:1-ARG0 11:0-rel 12:2-ARG1 16:2-ARG2 
nw/wsj/09/wsj_0942.parse 18 18 gold impede-v 67 Hindering impede.01 1 ----- 8:1*10:1*16:1-ARG0=Agent 18:0-rel 19:1-ARG1=Theme 20:1-ARGM-MNR 
nw/wsj/09/wsj_0942.parse 19 13 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARGM-ADV 8:2-ARG0=Agent 13:0-rel 14:1-ARGM-LOC 16:1-ARG1=Topic 
nw/wsj/09/wsj_0942.parse 19 24 gold put-v 9.1-2 IN put.01 null ----- 17:1-ARG0=Agent 21:0-ARGM-MOD 22:0-ARGM-NEG 24:0-rel 25:1-ARG1=Theme 26:1-ARG2=Destination 
nw/wsj/09/wsj_0942.parse 19 32 gold encourage-v 37.9 NF encourage.01 null ----- 17:1-ARG0=Agent 31:0-ARGM-MOD 32:0-rel 33:1*34:1-ARG1=Recipient 34:2-ARG2=Topic 
nw/wsj/09/wsj_0942.parse 19 37 gold grow-v 26.2 IN grow.03 null ----- 33:1*34:1-ARG0=Agent 37:0-rel 38:2-ARG1=Product 43:1-ARGM-ADV 
nw/wsj/09/wsj_0942.parse 19 41 gold desire-v 32.1 Desiring desire.01 1 ----- 38:1*42:1-ARG1=Theme;Event/Focal_participant 39:1-ARG0=Pivot;Experiencer 41:0-rel 
nw/wsj/09/wsj_0942.parse 19 48 gold want-v 32.1-1-1 Desiring want.01 null ----- 45:1*49:1-ARG1=Theme;Event/Focal_participant 46:1-ARG0=Pivot;Experiencer 48:0-rel 
nw/wsj/09/wsj_0942.parse 20 10 gold have-v 100 IN have.03 null ----- 0:2-ARG0=Pivot 10:0-rel 11:2-ARG1=Theme 15:1-ARGM-ADV 
nw/wsj/09/wsj_0942.parse 20 27 gold have-v 100 IN have.03 null ----- 16:2-ARG0=Pivot 27:0-rel 28:2-ARG1=Theme 
nw/wsj/09/wsj_0942.parse 21 7 gold center-v 87.1 NF center.01 2 ----- 0:3-ARG1 7:0-rel 8:1-ARG2=Theme 
nw/wsj/09/wsj_0942.parse 21 13 gold stay-v 47.1-1 IN stay.01 null ----- 11:1-ARG1=Theme 13:0-rel 14:1-ARG3 24:2-ARGM-ADV 
