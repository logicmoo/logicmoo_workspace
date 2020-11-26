nw/wsj/06/wsj_0606.parse 0 4 gold prove-v 29.4-1-1 Turning_out prove.01 null ----- 3:0-ARGM-MOD 4:0-rel 5:2-ARG1=Theme 
nw/wsj/06/wsj_0606.parse 0 21 gold sign-v 25.1 NF sign.01 null ----- 0:1-ARGM-LOC 18:1-ARG0=Agent 21:0-rel 22:1-ARG1=Destination 
nw/wsj/06/wsj_0606.parse 0 31 gold build-v 26.1-1 Building build.01 null ----- 18:1*29:1-ARG0=Agent;Agent 31:0-rel 32:2-ARG1=Product;Created_entity 18:1*29:1-LINK-PRO 
nw/wsj/06/wsj_0606.parse 1 11 gold initial-v 25.3 NF initial.01 null ----- 0:3-ARG0=Agent 11:0-rel 12:2*20:2-ARG1=Theme 27:1-ARGM-ADV 
nw/wsj/06/wsj_0606.parse 1 22 gold build-v 26.1-1 Building build.01 null ----- 0:3*20:1-ARG0=Agent;Agent 22:0-rel 23:1-ARG1=Product;Created_entity 0:3*20:1-LINK-PRO 
nw/wsj/06/wsj_0606.parse 1 34 gold head-v 47.8 Leadership head.01 null ----- 29:1*32:1*33:1-ARG0=Theme 34:0-rel 35:2-ARG1=Co-Theme 29:1*32:1-LINK-SLC 
nw/wsj/06/wsj_0606.parse 1 43 gold build-v 26.1-1 Building build.01 null ----- 35:1*40:1*41:1-ARG0=Agent;Agent 43:0-rel 46:1-ARG1=Product;Created_entity 
nw/wsj/06/wsj_0606.parse 2 9 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 9:0-rel 10:1-ARG1=Topic 
nw/wsj/06/wsj_0606.parse 2 14 gold sign-v 13.5.3 NF sign.02 null ----- 11:1*15:1-ARG1=Theme 14:0-rel 16:1-ARGM-TMP 17:1-ARGM-LOC 19:1-ARG2 
nw/wsj/06/wsj_0606.parse 3 8 gold make-v 29.3 Causation make.02 null ----- 3:1-ARGM-DIS 6:0-ARGM-NEG 8:0-rel 9:2-ARG1=Theme 
nw/wsj/06/wsj_0606.parse 5 4 gold hold-v 15.1-1 Manipulation hold.01 null ----- 0:1-ARG0=Agent;Agent 3:0-ARGM-MOD 4:0-rel 5:2-ARG1=Theme;Entity 13:1-ARGM-ADV 
nw/wsj/06/wsj_0606.parse 5 16 gold sign-v 13.5.3 NF sign.02 null ----- 15:1*17:1-ARG1=Theme 16:0-rel 18:1-ARG2 15:1*17:1-LINK-PSV 
nw/wsj/06/wsj_0606.parse 6 5 gold use-v 105 IN use.01 null ----- 0:1*7:1-ARG0 4:0-ARGM-MOD 5:0-rel 6:1-ARG1 7:2-ARG2 
nw/wsj/06/wsj_0606.parse 7 5 gold use-v 105 IN use.01 null ----- 2:1*6:1-ARG1 5:0-rel 7:1-ARG2 2:1*6:1-LINK-PSV 
nw/wsj/06/wsj_0606.parse 7 9 gold make-v 26.1-1 Manufacturing make.01 null ----- 8:1-ARG0=Agent;Manufacturer 9:0-rel 10:1-ARG1=Product;Product 
nw/wsj/06/wsj_0606.parse 10 3 gold consider-v 29.9-1-1-1 Categorization consider.01 null ----- 1:1-ARGM-ADV 3:0-rel 4:2-ARG1=Theme;Item 
nw/wsj/06/wsj_0606.parse 10 20 gold raise-v 9.4 NF raise.01 null ----- 7:1*11:1-ARG0=Agent 20:0-rel 21:2-ARG1=Theme 7:1*11:1-LINK-PRO 
nw/wsj/06/wsj_0606.parse 12 23 gold cause-v 27 Causation cause.01 1 ----- 20:1*21:1*22:1-ARG0=Cause;Cause 23:0-rel 24:2-ARG1=Theme;Effect 20:1*21:1-LINK-SLC 
nw/wsj/06/wsj_0606.parse 12 27 gold remain-v 47.1-1 State_continue remain.01 null ----- 24:1-ARG1=Theme 27:0-rel 28:1-ARG3 29:1-ARGM-TMP 
nw/wsj/06/wsj_0606.parse 13 3 gold report-v 37.7-1 Statement report.01 null ----- 1:1*4:1-ARG1 2:1-ARGM-TMP 3:0-rel 
nw/wsj/06/wsj_0606.parse 13 19 gold stop-v 55.4-1 Preventing stop.01 null ----- 6:2*17:1-ARG0=Agent 19:0-rel 20:1-ARG1=Theme 
nw/wsj/06/wsj_0606.parse 15 5 gold dismiss-v 10.1 Firing dismiss.01 3 ----- 0:2-ARG0=Agent 5:0-rel 6:1-ARG1=Theme 
nw/wsj/06/wsj_0606.parse 15 15 gold file-v 9.10 Submitting_documents file.02 null ----- 10:1-ARG0=Agent 12:2-ARGM-TMP 15:0-rel 16:1-ARG1=Theme 
nw/wsj/06/wsj_0606.parse 16 9 gold sign-v 13.5.3 NF sign.02 null ----- 3:1*7:1-ARG0=Agent 9:0-rel 10:1-ARG2 12:2-ARG1=Theme 
nw/wsj/06/wsj_0606.parse 16 14 gold remove-v 10.2 Removing remove.01 null ----- 12:1-ARG0=Agent;Agent/Cause 14:0-rel 15:2-ARG1=Theme;Theme 
nw/wsj/06/wsj_0606.parse 16 31 gold use-v 105 IN use.01 null ----- 23:1*27:1*28:1*32:1-ARG1 31:0-rel 33:1-ARGM-LOC 23:1*27:1-LINK-SLC 
nw/wsj/06/wsj_0606.parse 17 14 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-DIS 2:1-ARGM-LOC 6:1-ARGM-TMP 8:1-ARG0=Agent 14:0-rel 15:1-ARG1=Topic 
nw/wsj/06/wsj_0606.parse 17 19 gold attempt-v 61 Attempt attempt.01 null ----- 16:1*20:1-ARG0=Agent 19:0-rel 20:2-ARG1=Theme 
nw/wsj/06/wsj_0606.parse 17 22 gold soothe-v 31.1 Experiencer_obj soothe.01 null ----- 16:1*20:1-ARG0=Stimulus;Stimulus 22:0-rel 23:2-ARG1=Experiencer;Experiencer 
nw/wsj/06/wsj_0606.parse 18 3 gold try-v 61 Attempt try.01 null ----- 1:1*6:1-ARG0=Agent 3:0-rel 4:1-ARGM-MNR 6:2-ARG1=Theme 
nw/wsj/06/wsj_0606.parse 18 8 gold tell-v 37.2-1 Telling tell.01 null ----- 1:1*6:1-ARG0=Agent;Speaker 8:0-rel 9:2-ARG2=Recipient;Addressee 13:1-ARG1=Topic;Message 
nw/wsj/06/wsj_0606.parse 18 33 gold encourage-v 37.9 NF encourage.01 null ----- 26:1*31:1-ARG0=Agent 33:0-rel 34:1*36:1-ARG1=Recipient 36:2-ARG2=Topic 
nw/wsj/06/wsj_0606.parse 18 46 gold tell-v 37.2-1 Telling tell.01 null ----- 1:3*52:1-ARG1=Topic;Message 44:1-ARG0=Agent;Speaker 46:0-rel 47:2-ARG2=Recipient;Addressee 
nw/wsj/06/wsj_0606.parse 20 12 gold have-v 100 IN have.03 null ----- 0:2*10:1-ARG0=Pivot 12:0-rel 13:2-ARG1=Theme 
nw/wsj/06/wsj_0606.parse 21 4 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-DIS 2:1-ARG0=Agent 4:0-rel 5:1-ARG1=Topic 
nw/wsj/06/wsj_0606.parse 22 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/06/wsj_0606.parse 22 27 gold include-v 65 NF include.01 null ----- 23:1*24:1*25:1-ARG2=Location 26:0-ARGM-MOD 27:0-rel 28:1-ARG1=Theme 23:1*24:1-LINK-SLC 
