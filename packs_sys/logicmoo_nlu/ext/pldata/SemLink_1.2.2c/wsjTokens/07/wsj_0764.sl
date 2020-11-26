nw/wsj/07/wsj_0764.parse 1 18 gold begin-v 55.1-1 Activity_start begin.01 null ----- 0:2-ARGM-TMP 4:3-ARG0=Agent;Agent 18:0-rel 19:1-ARG1=Theme;Activity 
nw/wsj/07/wsj_0764.parse 1 19 gold warn-v 37.9-1 NF warn.01 1 ----- 0:2-ARGM-TMP 4:3-ARG0=Agent 19:0-rel 20:1-ARG1=Topic 
nw/wsj/07/wsj_0764.parse 2 23 gold murder-v 42.1 Killing murder.01 1 ----- 20:1*24:1-ARG1=Patient;Victim 23:0-rel 25:1-ARG0=Agent;Killer/Cause 29:2-ARGM-TMP 
nw/wsj/07/wsj_0764.parse 4 3 gold enforce-v 63 NF enforce.01 1 ----- 0:1*4:1-ARG1=Theme 3:0-rel 5:1-ARGM-MNR 
nw/wsj/07/wsj_0764.parse 5 14 gold murder-v 42.1 Killing murder.01 1 ----- 0:1-ARGM-TMP 5:2*15:1-ARG1=Patient;Victim 14:0-rel 
nw/wsj/07/wsj_0764.parse 6 19 gold destroy-v 44 Destroying destroy.01 null ----- 5:1*18:1-ARG0=Agent;Cause/Destroyer 19:0-rel 20:2-ARG1=Patient;Undergoer 
nw/wsj/07/wsj_0764.parse 7 20 gold destroy-v 44 Destroying destroy.01 null ----- 18:1*21:1-ARG1=Patient;Undergoer 20:0-rel 
nw/wsj/07/wsj_0764.parse 8 6 gold threaten-v 31.1 NF threaten.01 null ----- 0:2*7:1-ARG2 5:1-ARGM-TMP 6:0-rel 8:1-ARGM-ADV 
nw/wsj/07/wsj_0764.parse 9 9 gold intimidate-v 31.1 Experiencer_obj intimidate.01 1 ----- 7:1*10:1-ARG1=Experiencer;Experiencer 9:0-rel 
nw/wsj/07/wsj_0764.parse 10 2 gold impose-v 63 NF impose.01 null ----- 0:1*3:1-ARG1=Theme 2:0-rel 4:1-ARG0=Agent 
nw/wsj/07/wsj_0764.parse 11 4 gold accept-v 77 IN accept.01 null ----- 1:1-ARG0 4:0-rel 5:1-ARG1 
nw/wsj/07/wsj_0764.parse 11 22 gold extend-v 47.1-1 NF extend.01 null ----- 15:2-ARG1=Theme 20:1-ARGM-TMP 21:0-ARGM-MOD 22:0-rel 23:1-ARG2 
nw/wsj/07/wsj_0764.parse 13 16 gold pose-v 37.1.1 NF pose.02 null ----- 12:1*14:1*17:1-ARG1=Topic 15:1-ARG0=Agent 16:0-rel 18:1-ARG2=Recipient 12:1*14:1-LINK-SLC 
nw/wsj/07/wsj_0764.parse 15 2 gold attest-v 29.5-2 NF attest.01 1 ----- 0:1-ARG0=Agent 1:0-ARGM-MOD 2:0-rel 3:1-ARG1=Theme 
nw/wsj/07/wsj_0764.parse 15 12 gold offer-v 13.3 NF offer.01 null ----- 4:2*13:1-ARG3=Goal 12:0-rel 14:1-ARG1=Theme 15:1-ARGM-TMP 17:1-ARGM-LOC 
nw/wsj/07/wsj_0764.parse 16 14 gold have-v 100 IN have.03 null ----- 0:2-ARG0=Pivot 13:0-ARGM-NEG 14:0-rel 15:2-ARG1=Theme 
nw/wsj/07/wsj_0764.parse 16 20 gold fight-v 36.3-2 Hostile_encounter fight.01 null ----- 0:2*18:1-ARG0=Agent;Side_1 15:1*17:1-ARGM-MNR 20:0-rel 21:1-ARG1=Co-Agent;Side_2 23:1-ARGM-MNR 0:2*18:1-LINK-PRO 15:1*17:1-LINK-SLC 
nw/wsj/07/wsj_0764.parse 17 12 gold punish-v 33 Rewards_and_punishments punish.01 1 ----- 0:1*8:1-ARG0=Agent;Agent 12:0-rel 13:2-ARG1=Theme;Evaluee 
nw/wsj/07/wsj_0764.parse 18 10 gold lead-v 51.7 Cotheme lead.01 null ----- 0:2-ARG0=Agent;Theme 9:0-ARGM-MOD 10:0-rel 11:1-ARG1=Theme;Cotheme 
nw/wsj/07/wsj_0764.parse 20 11 gold fight-v 36.3-2 Hostile_encounter fight.01 null ----- 0:1*8:1*9:1-ARG0=Agent;Side_1 11:0-rel 12:1-ARG1=Co-Agent;Side_2 0:1*8:1-LINK-SLC 
nw/wsj/07/wsj_0764.parse 21 6 gold raid-v 35.4 Attack raid.01 1 ----- 0:1*7:1-ARG1=Location 6:0-rel 8:1-ARG0=Agent 
nw/wsj/07/wsj_0764.parse 21 21 gold seize-v 10.5 NF seize.01 null ----- 14:2*22:1-ARG1=Theme 21:0-rel 
nw/wsj/07/wsj_0764.parse 22 23 gold confiscate-v 10.5 Removing confiscate.01 1 ----- 14:2*24:1-ARG1=Theme;Theme 23:0-rel 
nw/wsj/07/wsj_0764.parse 23 4 gold capture-v 10.5-1 NF capture.01 3 ----- 0:1-ARG0=Agent 3:1-ARGM-ADV 4:0-rel 5:2-ARG1=Theme 
nw/wsj/07/wsj_0764.parse 24 3 gold accomplish-v 55.2 Accomplishment accomplish.01 1 ----- 0:1*4:1-ARG1=Theme 3:0-rel 
nw/wsj/07/wsj_0764.parse 24 9 gold support-v 31.2 Taking_sides support.01 null ----- 6:1-ARG0=Experiencer 8:1-ARGM-MNR 9:0-rel 10:2-ARG1=Stimulus 
nw/wsj/07/wsj_0764.parse 24 31 gold capture-v 10.5-1 NF capture.01 null ----- 18:1-ARGM-DIS 23:2*32:1-ARG1=Theme 31:0-rel 
nw/wsj/07/wsj_0764.parse 27 4 gold serve-v 29.6-2 Assistance serve.01 null ----- 0:1*5:1-ARG0=Agent 4:0-rel 5:2-ARG1=Attribute 
nw/wsj/07/wsj_0764.parse 27 8 gold increase-v 45.6-1 Cause_change_of_position_on_a_scale increase.01 1 ----- 0:1*5:1-ARG0 6:1-ARGM-ADV 8:0-rel 10:1-ARGM-MNR 12:2-ARG1=Patient;Item 18:2-ARGM-ADV 
nw/wsj/07/wsj_0764.parse 27 19 gold create-v 27 Creating create.01 null ----- 18:1-ARG0=Cause 19:0-rel 20:2-ARG1=Theme 
nw/wsj/07/wsj_0764.parse 27 32 gold own-v 100 NF own.01 null ----- 23:2*27:1*28:1*30:1-ARG0=Pivot 32:0-rel 33:1-ARG1=Theme 23:2*27:1-LINK-SLC 
nw/wsj/07/wsj_0764.parse 28 7 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:1*5:1-ARG0=Agent;Buyer 7:0-rel 8:2-ARG1=Theme;Goods 
nw/wsj/07/wsj_0764.parse 28 19 gold consume-v 39.4-1 Ingestion consume.01 1 ----- 15:1*16:1*21:1-ARGM-LOC 17:1*20:1-ARG1=Patient;Ingestibles 19:0-rel 15:1*16:1-LINK-SLC 
nw/wsj/07/wsj_0764.parse 29 7 gold keep-v 15.2 Storing keep.01 null ----- 0:0-ARGM-DIS 1:2*8:1-ARG1=Theme 7:0-rel 9:2-ARGM-LOC 
nw/wsj/07/wsj_0764.parse 31 13 gold suppose-v 29.9-2 Opinion suppose.01 null ----- 13:0-rel 14:0-ARG1=Theme 
nw/wsj/07/wsj_0764.parse 33 2 gold point-v 40.3.1-1 IN point.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG2=Recipient 
nw/wsj/07/wsj_0764.parse 35 6 gold earn-v 13.5.1-1 NF earn.01 null ----- 0:1-ARGM-DIS 4:1-ARG0=Agent 5:0-ARGM-MOD 6:0-rel 7:3-ARG1=Theme 
nw/wsj/07/wsj_0764.parse 36 3 gold face-v 98 Confronting_problem face.01 null ----- 0:1-ARG0 3:0-rel 4:2-ARG1 
nw/wsj/07/wsj_0764.parse 36 11 gold tempt-v 31.1 Attempt_suasion tempt.01 1 ----- 10:1-ARG0=Stimulus;Speaker 11:0-rel 12:2-ARG1=Experiencer;Addressee 
nw/wsj/07/wsj_0764.parse 36 16 gold substitute-v 13.6-1-1 Replacing substitute.01 1 ----- 12:1*15:1-ARG0=Agent;Agent 16:0-rel 17:1-ARG1=Co-Theme;New 19:1-ARG3=Theme;Old 
nw/wsj/07/wsj_0764.parse 37 3 gold try-v 61 Attempt try.01 null ----- 0:1*4:1-ARG0=Agent 2:1-ARGM-TMP 3:0-rel 4:2-ARG1=Theme 
nw/wsj/07/wsj_0764.parse 37 6 gold impose-v 63 NF impose.01 null ----- 0:1*4:1-ARG0=Agent 6:0-rel 7:2-ARG1=Theme 
nw/wsj/07/wsj_0764.parse 39 1 gold take-v 10.5 IN take.01 null ----- 0:1*15:1-ARG0=Agent 1:0-rel 2:1-ARG1=Theme 3:1-ARG2=Source 15:2*30:2-ARGM-PRP 
nw/wsj/07/wsj_0764.parse 39 6 gold give-v 13.1-1 Giving give.01 null ----- 4:1*7:1-ARG1=Theme;Theme 6:0-rel 8:1-ARG2=Recipient;Recipient 10:1-ARG0=Agent;Donor 4:1*7:1-LINK-PSV 
nw/wsj/07/wsj_0764.parse 40 4 gold consume-v 39.4-1 Ingestion consume.01 1 ----- 3:1*5:1-ARG1=Patient;Ingestibles 4:0-rel 3:1*5:1-LINK-PSV 
nw/wsj/07/wsj_0764.parse 40 24 gold fight-v 36.3-2 IN fight.01 null ----- 11:2*22:1*23:1-ARG0=Agent 24:0-rel 25:1-ARG1=Co-Agent 11:2*22:1-LINK-SLC 
nw/wsj/07/wsj_0764.parse 42 10 gold found-v 55.5-1 Intentionally_create found.01 1 ----- 8:1*11:1-ARG1 10:0-rel 12:1-ARG0 8:1*11:1-LINK-PSV 
