nw/wsj/17/wsj_1700.parse 0 7 gold suggest-v 37.7-1-1 Statement suggest.01 null ----- 1:2-ARG0=Agent;Speaker 6:1-ARGM-MNR 7:0-rel 8:1-ARG1=Topic;Message 
nw/wsj/17/wsj_1700.parse 0 13 gold help-v 72-1 Assistance help.01 null ----- 9:1*14:1-ARG0=Agent;Helper 12:0-ARGM-MOD 13:0-rel 14:2-ARG1=Theme;Goal/Focal_entity 
nw/wsj/17/wsj_1700.parse 2 35 gold end-v 55.4-1 Cause_to_end end.01 null ----- 25:1*32:1*33:1-ARG1=Theme 34:0-ARGM-MOD 35:0-rel 36:1-ARG2=Instrument 25:1*32:1-LINK-SLC 
nw/wsj/17/wsj_1700.parse 3 7 gold pull-v 23.2 IN pull.01 null ----- 2:1*4:1-ARG0=Agent 7:0,8:1-rel 9:1-ARG1=Patient 
nw/wsj/17/wsj_1700.parse 3 17 gold talk-v 37.5 IN talk.01 null ----- 0:2-ARGM-TMP 13:1*20:1-ARG0=Agent 17:0-rel 18:1-ARGM-TMP 19:1-ARG1=Topic 
nw/wsj/17/wsj_1700.parse 3 21 gold accept-v 77 IN accept.01 null ----- 13:1*20:1-ARG0 21:0-rel 22:2-ARG1 
nw/wsj/17/wsj_1700.parse 4 3 gold include-v 65 NF include.01 null ----- 0:1-ARG2=Location 2:0-ARGM-MOD 3:0-rel 4:2-ARG1=Theme 
nw/wsj/17/wsj_1700.parse 4 18 gold lead-v 51.7 Cotheme lead.02 null ----- 13:1*19:1-ARG1=Theme;Cotheme 18:0-rel 20:1-ARG0=Agent;Theme 13:1*19:1-LINK-PSV 
nw/wsj/17/wsj_1700.parse 5 6 gold end-v 55.4-1 Cause_to_end end.01 null ----- 4:1-ARG0=Agent 6:0-rel 7:2-ARG1=Theme 14:1-ARGM-MNR 
nw/wsj/17/wsj_1700.parse 6 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/17/wsj_1700.parse 7 5 gold include-v 65 NF include.01 null ----- 4:1-ARG0=Agent 5:0-rel 6:1-ARG1=Theme 7:1-ARG2=Location 
nw/wsj/17/wsj_1700.parse 7 19 gold require-v 103 NF require.01 null ----- 15:1-ARG0=Pivot 19:0-rel 20:1-ARG1=Theme 
nw/wsj/17/wsj_1700.parse 9 7 gold exile-v 36.4 NF exile.01 null ----- 0:1-ARGM-TMP 4:1*8:1-ARG1=Co-Agent 7:0-rel 
nw/wsj/17/wsj_1700.parse 9 22 gold build-v 26.1-1 Building build.01 null ----- 0:1-ARGM-TMP 19:1-ARG0=Agent;Agent 22:0-rel 23:2-ARG1=Product;Created_entity 
nw/wsj/17/wsj_1700.parse 9 32 gold threaten-v 31.1 NF threaten.01 null ----- 0:1-ARGM-TMP 19:1-ARG0=Stimulus 32:0-rel 33:1-ARG2 
nw/wsj/17/wsj_1700.parse 10 7 gold bow-v 40.3.3 NF bow.01 3 ----- 3:1*14:1-ARGM-TMP 4:1-ARG0=Agent 7:0-rel 8:1-ARG1=Recipient 
nw/wsj/17/wsj_1700.parse 11 7 gold begin-v 55.1-1 Process_start begin.01 null ----- 0:1-ARG1=Theme;Event 7:0-rel 8:1-ARGM-TMP 10:1-ARG2=Instrument 
nw/wsj/17/wsj_1700.parse 13 27 gold disappear-v 48.2 Departing disappear.01 1 ----- 24:1-ARG1=Patient;Theme 26:0-ARGM-MOD 27:0-rel 
nw/wsj/17/wsj_1700.parse 14 2 gold leave-v 51.2-1 Departing leave.01 null ----- 0:1-ARG0=Theme;Theme 1:0-ARGM-MOD 2:0-rel 3:2-ARG1=Initial_Location;Source 
nw/wsj/17/wsj_1700.parse 15 5 gold send-v 11.1-1 Sending send.01 null ----- 0:1-ARG0=Agent;Sender 5:0-rel 6:3-ARG1=Theme;Theme 
nw/wsj/17/wsj_1700.parse 15 11 gold die-v 48.2 Death die.01 null ----- 6:2*17:1-ARG1=Patient;Protagonist 11:0-rel 12:1-ARGM-CAU 16:1-ARGM-TMP 
nw/wsj/17/wsj_1700.parse 15 18 gold build-v 26.1-1 Building build.01 null ----- 6:2*17:1-ARG0=Agent;Agent 18:0-rel 19:3-ARG1=Product;Created_entity 26:1-ARGM-LOC 
nw/wsj/17/wsj_1700.parse 16 6 gold carry-v 54.3 Bringing carry.01 null ----- 0:1-ARG0=Location 4:1-ARGM-DIS 6:0-rel 7:2-ARG1=Value 
nw/wsj/17/wsj_1700.parse 17 2 gold cause-v 27 Causation cause.01 1 ----- 0:1-ARG0=Cause;Cause 2:0-rel 3:2;14:1-ARG1=Theme;Effect 7:1-ARGM-MNR 
nw/wsj/17/wsj_1700.parse 19 3 gold want-v 32.1-1-1 Desiring want.01 null ----- 0:1*4:1-ARG0=Pivot;Experiencer 2:0-ARGM-MOD 3:0-rel 4:2-ARG1=Theme;Event/Focal_participant 19:2-ARGM-ADV 
nw/wsj/17/wsj_1700.parse 19 6 gold believe-v 29.9-2 Opinion believe.01 null ----- 0:1*4:1-ARG0 6:0-rel 7:1-ARG1 
nw/wsj/17/wsj_1700.parse 19 30 gold credit-v 13.4.1-1 NF credit.01 1 ----- 25:1*28:1-ARG0=Agent 30:0-rel 31:2-ARG1=Recipient 
nw/wsj/17/wsj_1700.parse 21 6 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 6:0-rel 7:1-ARG1=Topic 
nw/wsj/17/wsj_1700.parse 22 7 gold swallow-v 39.3-2 NF swallow.01 6 ----- 5:1-ARG0=Agent 7:0-rel 8:1-ARG1=Patient 
nw/wsj/17/wsj_1700.parse 22 17 gold accept-v 77 IN accept.01 null ----- 11:1*15:1-ARG0 17:0-rel 18:2-ARG1 
nw/wsj/17/wsj_1700.parse 22 22 gold help-v 72-1 Assistance help.01 null ----- 18:1*19:1*20:1-ARG0=Agent;Helper 21:0-ARGM-MOD 22:0-rel 23:1-ARG2=Beneficiary;Benefited_party 26:1-ARG1=Theme;Goal/Focal_entity 18:1*19:1-LINK-SLC 
nw/wsj/17/wsj_1700.parse 25 8 gold offend-v 31.1 Experiencer_obj offend.01 1 ----- 2:1*7:1-ARG0=Stimulus;Stimulus 8:0-rel 9:2-ARG1=Experiencer;Experiencer 
nw/wsj/17/wsj_1700.parse 25 14 gold aid-v 72-1 Assistance aid.01 1 ----- 9:1*11:1*13:1-ARG0=Agent;Helper 12:1-ARGM-TMP 14:0-rel 15:1-ARG2=Beneficiary;Benefited_party 9:1*11:1-LINK-SLC 
nw/wsj/17/wsj_1700.parse 26 7 gold recognize-v 29.5-1 Becoming_aware recognize.02 null ----- 4:1-ARG0 7:0-rel 8:1-ARG1 
nw/wsj/17/wsj_1700.parse 26 12 gold play-v 26.7-1-1 Competition play.01 null ----- 9:1-ARG0=Agent 11:0-ARGM-NEG 12:0-rel 13:1-ARGM-MNR 
nw/wsj/17/wsj_1700.parse 27 4 gold lend-v 13.1 NF lend.01 4 ----- 1:1-ARG0=Agent 4:0-rel 5:1-ARG1=Theme 9:1-ARG2=Recipient 
nw/wsj/17/wsj_1700.parse 27 21 gold disturb-v 31.1 Experiencer_obj disturb.01 2 ----- 0:1-ARG0=Stimulus;Stimulus 19:0-ARGM-MOD 20:1-ARGM-ADV 21:0-rel 22:3-ARG1=Experiencer;Experiencer 
nw/wsj/17/wsj_1700.parse 28 18 gold conclude-v 97.2 Coming_to_believe conclude.01 null ----- 7:2-ARG0=Agent 18:0-rel 19:2-ARG1=Theme 
nw/wsj/17/wsj_1700.parse 28 23 gold fight-v 36.3-2 Hostile_encounter fight.01 null ----- 22:1-ARG0=Agent;Side_1 23:0-rel 24:1-ARGM-MNR 
nw/wsj/17/wsj_1700.parse 28 38 gold help-v 72-1 Assistance help.01 null ----- 27:1-ARG0=Agent;Helper 30:1-ARGM-MNR 36:0-ARGM-MOD 37:1-ARGM-TMP 38:0-rel 39:2-ARG1=Theme;Goal/Focal_entity 
nw/wsj/17/wsj_1700.parse 28 40 gold negotiate-v 36.1 NF negotiate.01 null ----- 27:1*39:1-ARG0=Agent 40:0-rel 41:2-ARG2=Theme 27:1*39:1-LINK-PRO 
nw/wsj/17/wsj_1700.parse 30 12 gold send-v 11.1-1 Sending send.01 null ----- 0:2*11:1-ARG0=Agent;Sender 12:0-rel 13:1-ARG1=Theme;Theme 15:1-ARG2=Destination;Goal/Recipient 0:2*11:1-LINK-PRO 
nw/wsj/17/wsj_1700.parse 31 19 gold put-v 9.1-2 IN put.01 null ----- 0:0-ARGM-DIS 1:1-ARGM-TMP 2:2-ARG0=Agent 19:0-rel 20:1-ARG1=Theme 22:1-ARG2=Destination 25:1-ARGM-ADV 
nw/wsj/17/wsj_1700.parse 31 32 gold get-v 26.6.2 IN get.03 null ----- 30:1-ARG1=Patient 32:0-rel 33:1-ARG2=Goal 
nw/wsj/17/wsj_1700.parse 31 39 gold rearm-v 13.4.2-1 NF rearm.01 null ----- 35:1-ARG0=Agent 37:0-ARGM-MOD 38:1-ARGM-TMP 39:0-rel 40:1-ARG1=Recipient 
nw/wsj/17/wsj_1700.parse 33 9 gold weight-v 13.4.2-1-1 NF weight.01 null ----- 0:1-ARGM-ADV 5:1*10:1-ARG1=Theme 8:1-ARGM-MNR 9:0-rel 11:1-ARG2 
nw/wsj/17/wsj_1700.parse 33 16 gold prevent-v 67 Thwarting prevent.01 1 ----- 15:1-ARG0=Agent;Preventing_cause 16:0-rel 17:2*25:1-ARG1=Theme;Protagonist/Action 24:1-ARG2=Theme;Protagonist/Action 
nw/wsj/17/wsj_1700.parse 34 14 gold know-v 29.5-1 IN know.01 1 ----- 12:1-ARG0=Agent 14:0-rel 15:1-ARG1=Theme 
nw/wsj/17/wsj_1700.parse 34 19 gold sit-v 50 Posture sit.01 null ----- 16:1-ARG1=Agent 19:0-rel 20:1-ARG2=Location 
nw/wsj/17/wsj_1700.parse 34 24 gold deprive-v 10.6 NF deprive.01 1 ----- 16:1-ARG0=Agent 24:0-rel 25:1-ARG2=Source 27:1-ARG1=Theme 
