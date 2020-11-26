nw/wsj/09/wsj_0922.parse 0 5 gold favor-v 31.2 Preference favor.01 1 ----- 0:2*3:1*4:1-ARG0=Experiencer;Experiencer 5:0-rel 6:1-ARG1=Attribute;Location_of_Event 0:2*3:1-LINK-SLC 
nw/wsj/09/wsj_0922.parse 0 12 gold add-v 22.1-2 Statement add.02 null ----- 0:3-ARG0=Agent 12:0-rel 13:2;21:2-ARG1=Patient 17:1-ARG2=Co-Patient 
nw/wsj/09/wsj_0922.parse 1 20 gold begin-v 55.1-1 Activity_start begin.01 null ----- 0:3*21:1-ARG0=Agent;Agent 20:0-rel 21:2-ARG1=Theme;Activity 
nw/wsj/09/wsj_0922.parse 1 22 gold link-v 22.1-2-1 Make_cognitive_connection link.01 null ----- 0:3*21:1-ARG0=Agent;Cognizer 22:0-rel 23:2-ARG1=Patient;Concept_1 29:1-ARG1=Patient;Concept_1 
nw/wsj/09/wsj_0922.parse 2 2 gold stress-v 9.9 NF stress.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Destination 
nw/wsj/09/wsj_0922.parse 2 12 gold help-v 72-1 Assistance help.01 null ----- 4:1*7:1*15:1-ARGM-MNR 9:1-ARG0=Agent;Helper 12:0-rel 13:1-ARG1=Theme;Goal/Focal_entity 4:1*7:1-LINK-SLC 
nw/wsj/09/wsj_0922.parse 3 2 gold make-v 29.3 Causation make.02 null ----- 0:1-ARG0=Agent 1:0-ARGM-MOD 2:0-rel 3:2-ARG1=Theme 
nw/wsj/09/wsj_0922.parse 3 20 gold allow-v 64 IN allow.01 null ----- 0:1-ARG0 1:0-ARGM-MOD 20:0-rel 21:3-ARG1 
nw/wsj/09/wsj_0922.parse 3 30 gold flow-v 47.2 Fluidic_motion flow.01 1 ----- 21:2-ARG1=Theme;Fluid 30:0-rel 31:1-ARGM-DIR 33:1-ARGM-DIR 
nw/wsj/09/wsj_0922.parse 4 8 gold present-v 13.4.1 NF present.01 null ----- 5:1*9:1-ARG1=Theme 8:0-rel 10:1-ARG2=Recipient 12:1-ARG0=Agent 5:1*9:1-LINK-PSV 
nw/wsj/09/wsj_0922.parse 4 19 gold tell-v 37.2-1 Telling tell.01 null ----- 17:1-ARG0=Agent;Speaker 19:0-rel 21:1-ARG2=Recipient;Addressee 24:1-ARGM-LOC 26:1-ARGM-TMP 0:1-ARG1-DSP=Topic;Message 
nw/wsj/09/wsj_0922.parse 5 2 gold propose-v 37.7-1 Statement propose.01 null ----- 0:1-ARG0=Agent;Speaker 2:0-rel 3:2-ARG1=Topic;Message 
nw/wsj/09/wsj_0922.parse 5 7 gold convene-v 47.5.2 Assemble convene.01 1 ----- 4:1*8:1-ARG1=Theme 7:0-rel 9:1-ARGM-TMP 11:2-ARGM-PRP 
nw/wsj/09/wsj_0922.parse 5 13 gold write-v 25.2 Text_creation write.01 null ----- 11:1-ARG0=Agent;Author 13:0-rel 14:3-ARG1=Theme;Text 
nw/wsj/09/wsj_0922.parse 5 20 gold allow-v 60 NF allow.01 null ----- 14:2-ARG0 20:0-rel 21:1-ARG1 
nw/wsj/09/wsj_0922.parse 6 3 gold propose-v 37.7-1 Statement propose.01 null ----- 0:1-ARG0=Agent;Speaker 2:1-ARGM-DIS 3:0-rel 4:2-ARG1=Topic;Message 
nw/wsj/09/wsj_0922.parse 7 4 gold link-v 22.1-2-1 Make_cognitive_connection link.01 null ----- 3:1-ARG0=Agent;Cognizer 4:0-rel 5:3-ARG1=Patient;Concept_1 
nw/wsj/09/wsj_0922.parse 8 0 gold say-v 37.7-1 IN say.01 null ----- 0:0-rel 1:1*7:2-ARG1=Topic 2:1-ARG0=Agent 
nw/wsj/09/wsj_0922.parse 10 3 gold imply-v 78 Evidence imply.01 1 ----- 0:1-ARG0=Cause;Support 3:0-rel 4:2-ARG1=Topic;Proposition 
nw/wsj/09/wsj_0922.parse 11 12 gold say-v 37.7-1 IN say.01 null ----- 12:0-rel 15:2-ARG0=Agent 3:2-ARG1-DSP=Topic 
nw/wsj/09/wsj_0922.parse 11 33 gold engage-v 107 NF engage.01 null ----- 30:1-ARG1 32:0-ARGM-MNR 33:0-rel 34:1-ARG2 
nw/wsj/09/wsj_0922.parse 13 6 gold dictate-v 37.1.1-1 NF dictate.01 1 ----- 0:1-ARGM-DIS 3:1*7:1-ARG1=Topic 6:0-rel 8:1-ARG0=Agent 
nw/wsj/09/wsj_0922.parse 14 6 gold limit-v 76 NF limit.01 null ----- 1:1*7:1-ARG1=Patient 6:0-rel 8:1-ARG0=Cause 
nw/wsj/09/wsj_0922.parse 14 14 gold announce-v 37.7-1 Statement announce.01 null ----- 0:1-ARGM-ADV 12:1-ARGM-TMP 13:1-ARG0=Agent;Speaker 14:0-rel 15:2-ARG1=Topic;Message 
nw/wsj/09/wsj_0922.parse 15 20 gold remain-v 47.1-1 State_continue remain.01 null ----- 0:1-ARGM-ADV 8:2-ARG1=Theme 20:0-rel 21:1-ARG3 
nw/wsj/09/wsj_0922.parse 16 4 gold begin-v 55.1-1 Activity_start begin.01 null ----- 0:0-ARGM-DIS 1:1-ARG0=Agent;Agent 4:0-rel 5:2-ARG1=Theme;Activity 
nw/wsj/09/wsj_0922.parse 16 7 gold link-v 22.1-2-1 Make_cognitive_connection link.01 null ----- 1:1*5:1-ARG0=Agent;Cognizer 7:0-rel 8:1-ARG1=Patient;Concept_1 
nw/wsj/09/wsj_0922.parse 17 2 gold need-v 32.1-1-1 Needing need.01 null ----- 1:1-ARG0=Pivot 2:0-rel 3:1-ARG1=Theme 
nw/wsj/09/wsj_0922.parse 17 8 gold say-v 37.7-1 IN say.01 null ----- 1:2*9:1-ARG1=Topic 8:0-rel 10:2*23:1-ARG0=Agent 23:2-ARGM-ADV 
nw/wsj/09/wsj_0922.parse 18 6 gold need-v 32.1-1-1 Required_event need.01 null ----- 1:1-ARGM-GOL 5:1*7:1-ARG0=Pivot 6:0-rel 7:2-ARG1=Theme 
nw/wsj/09/wsj_0922.parse 18 9 gold think-v 29.9-2 IN think.01 null ----- 5:1*7:1-ARG0 9:0-rel 10:1-ARG2 
nw/wsj/09/wsj_0922.parse 18 22 gold force-v 59 NF force.01 null ----- 11:1*19:1*20:1-ARG0=Agent 21:0-ARGM-MOD 22:0-rel 23:1-ARG1=Patient 24:2-ARG2=Result 11:1*19:1-LINK-SLC 
nw/wsj/09/wsj_0922.parse 18 26 gold price-v 54.4 NF price.01 1 ----- 23:1*24:1-ARG0=Agent 26:0-rel 27:1-ARG1=Theme 28:1-ARGM-MNR 29:1-ARGM-ADV 
nw/wsj/09/wsj_0922.parse 18 33 gold remove-v 10.2 Removing remove.01 null ----- 32:1-ARG0=Agent;Agent/Cause 33:0-rel 34:1-ARG1=Theme;Theme 
nw/wsj/09/wsj_0922.parse 21 9 gold conclude-v 97.2 Coming_to_believe conclude.01 null ----- 1:2-ARG0=Agent 8:1-ARGM-TMP 9:0-rel 10:1-ARG1=Theme 
nw/wsj/09/wsj_0922.parse 21 13 gold want-v 32.1-1-1 Desiring want.01 null ----- 11:2-ARG0=Pivot;Experiencer 13:0-rel 14:3-ARG1=Theme;Event/Focal_participant 
nw/wsj/09/wsj_0922.parse 21 27 gold succeed-v 74-1 IN succeed.01 null ----- 14:2-ARG1=Theme 27:0-rel 
nw/wsj/09/wsj_0922.parse 21 30 gold say-v 37.7-1 IN say.01 null ----- 1:3*31:1-ARG1=Topic 30:0-rel 32:2-ARG0=Agent 
nw/wsj/09/wsj_0922.parse 23 1 gold add-v 37.7 NF add.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/09/wsj_0922.parse 23 12 gold rejuvenate-v 31.1 Rejuvenation rejuvenate.01 2 ----- 7:1*10:1-ARG0=Stimulus 12:0-rel 13:1-ARG1=Experiencer 
nw/wsj/09/wsj_0922.parse 23 20 gold lie-v 47.6 Being_located lie.01 null ----- 18:1-ARG1=Theme 20:0-rel 21:1-ARG2=Location 
nw/wsj/09/wsj_0922.parse 23 33 gold need-v 32.1-1-1 Required_event need.01 null ----- 32:1*34:1-ARG0=Pivot 33:0-rel 34:2-ARG1=Theme 
nw/wsj/09/wsj_0922.parse 23 36 gold help-v 72-1 Assistance help.01 null ----- 32:1*34:1-ARG0=Agent;Helper 36:0-rel 37:1-ARG1=Theme;Goal/Focal_entity 39:1-ARGM-MNR 
nw/wsj/09/wsj_0922.parse 24 5 gold add-v 37.7 NF add.01 null ----- 4:1-ARG0=Agent 5:0-rel 0:3-ARG1-DSP=Topic 
nw/wsj/09/wsj_0922.parse 24 10 gold require-v 103 NF require.01 null ----- 0:2-ARG0=Pivot 9:0-ARGM-MOD 10:0-rel 11:4-ARG1=Theme 
nw/wsj/09/wsj_0922.parse 24 26 gold deal-v 83 Resolve_problem deal.01 null ----- 20:1*25:1-ARG0=Agent 26:0-rel 27:1-ARG1=Theme 20:1*25:1-LINK-PRO 
nw/wsj/09/wsj_0922.parse 25 15 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 15:0-rel 16:1-ARG1=Topic 
nw/wsj/09/wsj_0922.parse 25 28 gold want-v 32.1-1-1 Desiring want.01 null ----- 22:1*29:1-ARG1=Theme;Event/Focal_participant 24:1-ARGM-ADV 26:1-ARG0=Pivot;Experiencer 28:0-rel 
nw/wsj/09/wsj_0922.parse 26 14 gold imply-v 78 Evidence imply.01 1 ----- 7:1-ARG0=Cause;Support 14:0-rel 15:3-ARG1=Topic;Proposition 
nw/wsj/09/wsj_0922.parse 26 46 gold say-v 37.7-1 IN say.01 null ----- 45:1-ARG0=Agent 46:0-rel 47:1-ARG1=Topic 
nw/wsj/09/wsj_0922.parse 27 1 gold doubt-v 29.5-1 Certainty doubt.01 1 ----- 0:1-ARG0=Agent;Cognizer 1:0-rel 2:1*10:2-ARG1=Theme;Content 
nw/wsj/09/wsj_0922.parse 27 9 gold emerge-v 48.1.1 IN emerge.01 null ----- 3:2*10:2-ARG0=Theme 8:0-ARGM-MOD 9:0-rel 
nw/wsj/09/wsj_0922.parse 27 13 gold dominate-v 47.8-2 NF dominate.01 1 ----- 3:1*10:1*11:1-ARG0=Theme 12:0-ARGM-MOD 13:0-rel 14:1-ARG1=Co-Theme 3:1*10:1-LINK-SLC 
nw/wsj/09/wsj_0922.parse 27 17 gold warn-v 37.9-1 NF warn.01 1 ----- 0:1-ARG0=Agent 17:0-rel 18:1-ARG1=Topic 
nw/wsj/09/wsj_0922.parse 28 1 gold add-v 37.7 NF add.01 null ----- 0:1-ARG0=Agent 1:0-rel 4:3-ARG1=Topic 
nw/wsj/09/wsj_0922.parse 28 15 gold talk-v 37.5 IN talk.01 null ----- 4:2*13:1-ARG0=Agent 12:0-ARGM-MOD 15:0-rel 16:1-ARG2=Co-Agent 20:1-ARGM-TMP 22:1-ARGM-MNR 
nw/wsj/09/wsj_0922.parse 28 24 gold raise-v 9.4 NF raise.01 null ----- 4:2*13:1-ARG0=Agent 12:0-ARGM-MOD 24:0-rel 25:1-ARG1=Theme 28:1-ARGM-ADV 
nw/wsj/09/wsj_0922.parse 29 8 gold worry-v 31.1 Emotion_active worry.01 null ----- 1:2-ARG0=Stimulus 8:0-rel 9:1-ARG1=Experiencer 
nw/wsj/09/wsj_0922.parse 29 21 gold lead-v 59 Causation lead.03 null ----- 10:3-ARG0=Agent 20:0-ARGM-MOD 21:0-rel 22:1-ARG2=Result 
nw/wsj/09/wsj_0922.parse 29 43 gold see-v 30.1-1 Perception_experience see.01 null ----- 0:1-ARGM-ADV 39:2-ARG0=Experiencer;Perceiver_passive 43:0-rel 44:1-ARG1=Stimulus;Phenomenon 
nw/wsj/09/wsj_0922.parse 30 21 gold say-v 37.7-1 IN say.01 null ----- 1:2*22:1-ARG1=Topic 20:1-ARG0=Agent 21:0-rel 
nw/wsj/09/wsj_0922.parse 31 2 gold want-v 32.1-1-1 Desiring want.01 null ----- 1:1-ARG0=Pivot;Experiencer 2:0-rel 3:1-ARG1=Theme;Event/Focal_participant 
nw/wsj/09/wsj_0922.parse 31 8 gold try-v 61 Attempt try.01 null ----- 5:1*9:1-ARG0=Agent 6:0-ARGM-MOD 7:0-ARGM-NEG 8:0-rel 9:2-ARG1=Theme 
nw/wsj/09/wsj_0922.parse 31 13 gold destroy-v 44 Destroying destroy.01 null ----- 5:1*9:1-ARG0=Agent;Cause/Destroyer 13:0-rel 14:1-ARG1=Patient;Undergoer 
nw/wsj/09/wsj_0922.parse 32 8 gold keep-v 15.2 Storing keep.01 null ----- 0:1-ARGM-DIS 3:2-ARG0=Agent 7:0-ARGM-MOD 8:0-rel 9:1-ARG1=Theme 
nw/wsj/09/wsj_0922.parse 33 1 gold add-v 37.7 NF add.01 null ----- 0:1-ARG0=Agent 1:0-rel 4:2-ARG1=Topic 
nw/wsj/09/wsj_0922.parse 33 7 gold feel-v 30.1-1 Feeling feel.01 null ----- 5:1-ARG0=Experiencer 7:0-rel 8:1-ARG1=Stimulus 
nw/wsj/09/wsj_0922.parse 33 32 gold start-v 55.1-1 Process_start start.01 null ----- 23:1*28:1*29:1-ARG1=Theme;Event 31:1-ARGM-TMP 32:0-rel 33:1-ARGM-LOC 23:1*28:1-LINK-SLC 
