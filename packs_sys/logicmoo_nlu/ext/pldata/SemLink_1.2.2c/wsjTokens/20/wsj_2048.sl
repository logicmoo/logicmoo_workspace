nw/wsj/20/wsj_2048.parse 0 2 gold generate-v 27 IN generate.01 1 ----- 0:1-ARG0 2:0-rel 3:2-ARG1=Theme 
nw/wsj/20/wsj_2048.parse 1 5 gold receive-v 13.5.2 Receiving receive.01 null ----- 0:1-ARG0=Agent;Donor 5:0-rel 6:3-ARG1=Theme;Theme 14:1-ARGM-CAU 
nw/wsj/20/wsj_2048.parse 2 18 gold come-v 48.1.1 NF come.03 null ----- 0:3-ARG1=Theme 18:0-rel 19:1-ARG2 23:1-ARGM-ADV 
nw/wsj/20/wsj_2048.parse 3 2 gold expect-v 62 IN expect.01 null ----- 0:1*3:1-ARG0=Experiencer 2:0-rel 3:2-ARG1=Theme 
nw/wsj/20/wsj_2048.parse 4 4 gold replenish-v 9.8 NF replenish.01 null ----- 0:1*5:1-ARG1=Destination 4:0-rel 6:1-ARG2=Theme 
nw/wsj/20/wsj_2048.parse 5 1 gold hit-v 18.4 Experience_bodily_harm hit.01 null ----- 0:1-ARGM-MNR 1:0-rel 2:1-ARG2=Theme 4:1-ARGM-LOC 
nw/wsj/20/wsj_2048.parse 5 22 gold cultivate-v 26.3-1 NF cultivate.01 null ----- 22:0-rel 23:0-ARG1 
nw/wsj/20/wsj_2048.parse 6 11 gold accept-v 13.5.2 IN accept.01 null ----- 6:1*9:1-ARG0=Agent 11:0-rel 12:3-ARG1=Theme 6:1*9:1-LINK-PRO 
nw/wsj/20/wsj_2048.parse 7 6 gold broadcast-v 37.4 NF broadcast.01 1 ----- 1:2-ARG0=Agent 6:0-rel 7:1-ARG1=Topic 
nw/wsj/20/wsj_2048.parse 8 8 gold begin-v 55.1-1 Activity_start begin.01 null ----- 0:1-ARGM-TMP 3:2*9:1-ARG0=Agent;Agent 8:0-rel 9:2-ARG1=Theme;Activity 
nw/wsj/20/wsj_2048.parse 8 10 gold use-v 105 IN use.01 null ----- 3:2*9:1-ARG0 10:0-rel 11:1-ARG1 15:2-ARG2 
nw/wsj/20/wsj_2048.parse 9 4 gold help-v 72-1 Assistance help.01 null ----- 0:1-ARG0=Agent;Helper 4:0-rel 5:2-ARG1=Theme;Goal/Focal_entity 
nw/wsj/20/wsj_2048.parse 10 2 gold help-v 72-1 Assistance help.01 null ----- 1:1-ARG0=Agent;Helper 2:0-rel 3:1-ARG1=Theme;Goal/Focal_entity 
nw/wsj/20/wsj_2048.parse 10 7 gold feel-v 30.1-1 Feeling feel.01 null ----- 6:1-ARG0=Experiencer 7:0-rel 8:1-ARG1=Stimulus 9:2-ARGM-PRD 
nw/wsj/20/wsj_2048.parse 10 10 gold talk-v 37.5 IN talk.01 null ----- 9:1-ARG0=Agent 10:0-rel 11:1-ARG2=Co-Agent 
nw/wsj/20/wsj_2048.parse 10 26 gold say-v 37.7-1 IN say.01 null ----- 1:3*27:1-ARG1=Topic 23:1-ARG0=Agent 26:0-rel 
nw/wsj/20/wsj_2048.parse 11 2 gold remain-v 47.1-1 State_continue remain.01 null ----- 0:1-ARG1=Theme 2:0-rel 3:1-ARG3 8:2-ARGM-TMP 
nw/wsj/20/wsj_2048.parse 12 5 gold delete-v 10.1 NF delete.01 1 ----- 0:1*4:1-ARG0=Agent 5:0-rel 6:2-ARG1=Theme 9:1-ARG2=Source 0:1*4:1-LINK-PRO 
nw/wsj/20/wsj_2048.parse 12 18 gold expect-v 62 IN expect.01 null ----- 0:1*19:2-ARG1=Theme 3:1-ARGM-TMP 16:1-ARGM-TMP 18:0-rel 
nw/wsj/20/wsj_2048.parse 12 21 gold join-v 22.1-2-1 Cause_to_amalgamate join.01 null ----- 0:1*19:1-ARG0=Agent;Agent 21:0-rel 22:1-ARG1=Patient;Part_1 24:1-ARGM-PRD 
nw/wsj/20/wsj_2048.parse 12 26 gold vote-v 29.3 NF vote.01 null ----- 25:1*27:1-ARG0=Agent 26:0-rel 27:2-ARG1=Theme 
nw/wsj/20/wsj_2048.parse 12 29 gold kill-v 42.1-1 Killing kill.01 null ----- 25:1*27:1-ARG0=Agent;Killer/Cause 29:0-rel 30:2-ARG1=Patient;Victim 
nw/wsj/20/wsj_2048.parse 12 35 gold force-v 59 NF force.01 null ----- 30:1*33:1*34:1-ARG0=Agent 35:0-rel 36:1*37:1-ARG1=Patient 37:2-ARG2=Result 30:1*33:1-LINK-SLC 
nw/wsj/20/wsj_2048.parse 12 39 gold provide-v 13.4.1-2 Supply provide.01 null ----- 36:1*37:1-ARG0=Agent 39:0-rel 40:1-ARG1=Theme 42:1-ARG2=Recipient 
nw/wsj/20/wsj_2048.parse 13 2 gold lobby-v 58.1 NF lobby.01 1 ----- 1:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/20/wsj_2048.parse 13 23 gold give-v 13.1-1 Giving give.01 null ----- 19:1*20:1*21:1-ARG0=Agent;Donor 22:0-ARGM-MOD 23:0-rel 24:2-ARG2=Recipient;Recipient 28:3-ARG1=Theme;Theme 19:1*20:1-LINK-SLC 
nw/wsj/20/wsj_2048.parse 14 4 gold urge-v 58.1 IN urge.01 null ----- 1:1-ARG0=Agent 3:0-ARGM-MOD 4:0-rel 6:1-ARG1=Recipient 9:1-ARG2=Topic 
nw/wsj/20/wsj_2048.parse 15 6 gold fight-v 36.3-2 IN fight.01 null ----- 0:2-ARG0=Agent 4:1-ARGM-DIS 5:0-ARGM-MOD 6:0-rel 7:3-ARG1=Co-Agent 
nw/wsj/20/wsj_2048.parse 16 3 gold oppose-v 22.2-3 NF oppose.01 null ----- 0:0-ARGM-DIS 1:1-ARG0=Patient 2:1-ARGM-TMP 3:0-rel 4:1-ARGM-CAU 7:2-ARG1=Co-Patient 26:1-ARGM-ADV 
nw/wsj/20/wsj_2048.parse 16 14 gold sponsor-v 29.8-1 NF sponsor.01 1 ----- 7:1*15:1-ARG1=Beneficiary 14:0-rel 16:1-ARG0=Agent 7:1*15:1-LINK-PSV 
nw/wsj/20/wsj_2048.parse 17 8 gold study-v 14-1 Scrutiny study.01 null ----- 5:1-ARG0=Agent 8:0-rel 9:2-ARG1=Topic 
nw/wsj/20/wsj_2048.parse 17 16 gold push-v 23.2 Subjective_influence push.01 null ----- 5:2-ARG0=Agent 14:0-ARGM-MOD 15:1-ARGM-TMP 16:0-rel 17:1-ARG1=Patient 18:1-ARG2=Co-Patient 
nw/wsj/20/wsj_2048.parse 18 5 gold have-v 100 IN have.03 null ----- 0:1-ARG0=Pivot 4:0-ARGM-MOD 5:0-rel 6:1-ARG1=Theme 8:1-ARGM-MNR 
nw/wsj/20/wsj_2048.parse 18 10 gold make-v 29.3 Causation make.02 null ----- 0:1*9:1-ARG0=Agent 10:0-rel 11:3-ARG1=Theme 0:1*9:1-LINK-PRO 
nw/wsj/20/wsj_2048.parse 18 16 gold oppose-v 22.2-3 NF oppose.01 null ----- 14:1-ARG0=Patient 16:0-rel 17:1-ARG1=Co-Patient 18:1-ARGM-MNR 
nw/wsj/20/wsj_2048.parse 19 12 gold say-v 37.7-1 IN say.01 null ----- 1:1-ARGM-CAU 8:1-ARG0=Agent 9:0-ARGM-MOD 10:1-ARGM-NEG 12:0-rel 13:1-ARG1=Topic 
nw/wsj/20/wsj_2048.parse 19 16 gold discourage-v 67 Attempt_suasion discourage.01 2 ----- 14:1-ARG0=Agent 16:0-rel 17:1-ARG1=Theme 18:1-ARG2=Theme 
nw/wsj/20/wsj_2048.parse 19 20 gold offer-v 13.3 NF offer.01 null ----- 17:1*19:1-ARG0=Agent 20:0-rel 21:1-ARG1=Theme 17:1*19:1-LINK-PRO 
nw/wsj/20/wsj_2048.parse 19 25 gold say-v 37.7-1 IN say.01 null ----- 1:2*26:1-ARG1=Topic 25:0-rel 27:2-ARG0=Agent 
nw/wsj/20/wsj_2048.parse 21 9 gold outrage-v 31.1 Experiencer_obj outrage.01 null ----- 0:2-ARG1=Experiencer;Experiencer 9:0-rel 10:1-ARGM-TMP 
nw/wsj/20/wsj_2048.parse 21 27 gold result-v 48.1.1 NF result.01 1 ----- 11:2-ARG1=Theme 27:0-rel 28:1-ARG2=Location 
nw/wsj/20/wsj_2048.parse 21 32 gold say-v 37.7-1 IN say.01 null ----- 29:1-ARG0=Agent 32:0-rel 33:1-ARG1=Topic 
nw/wsj/20/wsj_2048.parse 22 12 gold issue-v 13.3 NF issue.01 null ----- 0:2-ARGM-ADV 10:1*13:1-ARG1=Theme 12:0-rel 14:1-ARGM-MNR 
nw/wsj/20/wsj_2048.parse 23 8 gold say-v 37.7-1 IN say.01 null ----- 1:2*9:1-ARG1=Topic 6:1-ARG0=Agent 8:0-rel 10:1-ARGM-MNR 
nw/wsj/20/wsj_2048.parse 26 14 gold end-v 55.4-1 Cause_to_end end.01 null ----- 12:1-ARG0=Agent 14:0-rel 15:2-ARG1=Theme 
nw/wsj/20/wsj_2048.parse 26 22 gold get-v 13.5.1-1 IN get.01 null ----- 15:1*17:1*23:1-ARG1=Theme 18:1-ARG0=Agent 22:0-rel 24:1-ARGM-PRD 15:1*17:1-LINK-SLC 
nw/wsj/20/wsj_2048.parse 26 26 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 18:1*25:1-ARG0=Agent;Seller 26:0-rel 27:3-ARG1=Theme;Goods 34:1-ARG2=Recipient 18:1*25:1-LINK-PRO 
nw/wsj/20/wsj_2048.parse 27 4 gold want-v 32.1-1-1 Desiring want.01 null ----- 0:2-ARG0=Pivot;Experiencer 4:0-rel 5:2-ARG1=Theme;Event/Focal_participant 13:2-ARGM-ADV 
nw/wsj/20/wsj_2048.parse 27 7 gold stop-v 55.4-1 Process_stop stop.01 null ----- 5:1-ARG0=Agent 7:0-rel 8:2-ARG1=Theme 
nw/wsj/20/wsj_2048.parse 27 9 gold get-v 13.5.1-1 IN get.01 null ----- 5:1*8:1-ARG0=Agent 9:0-rel 10:1-ARG1=Theme 
nw/wsj/20/wsj_2048.parse 27 19 gold swell-v 45.5 Change_position_on_a_scale swell.01 null ----- 19:0-rel 20:0,21:0-ARG1=Patient 
nw/wsj/20/wsj_2048.parse 28 10 gold total-v 54.1-1 Amounting_to total.01 1 ----- 0:1-ARGM-TMP 3:3-ARG1=Theme;Attribute 10:0-rel 11:2-ARG2=Value;Value 
nw/wsj/20/wsj_2048.parse 30 8 gold join-v 22.1-2-1 Cause_to_amalgamate join.01 null ----- 0:2-ARG0=Agent;Agent 8:0-rel 9:3-ARG1=Patient;Part_1 
nw/wsj/20/wsj_2048.parse 32 8 gold lower-v 9.4 NF lower.01 null ----- 1:2*9:1-ARG1=Theme 6:0-ARGM-MOD 8:0-rel 10:1-ARG4 
nw/wsj/20/wsj_2048.parse 33 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/20/wsj_2048.parse 35 15 gold oppose-v 22.2-3 NF oppose.01 null ----- 0:2-ARG0=Patient 15:0-rel 16:2-ARG1=Co-Patient 
nw/wsj/20/wsj_2048.parse 36 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/20/wsj_2048.parse 36 23 gold find-v 13.5.1 IN find.01 null ----- 15:2*20:1*21:1-ARG0=Agent 22:1-ARGM-TMP 23:0-rel 24:2-ARG1=Theme 15:2*20:1-LINK-SLC 
nw/wsj/20/wsj_2048.parse 37 5 gold debate-v 36.1-1-1 Discussion debate.01 1 ----- 0:1-ARG0 4:0-ARGM-MOD 5:0-rel 6:1-ARG1 8:2-ARGM-TMP 
nw/wsj/20/wsj_2048.parse 38 9 gold oppose-v 22.2-3 NF oppose.01 null ----- 3:1*7:1*8:1-ARG0=Patient 9:0-rel 10:1-ARG1=Co-Patient 3:1*7:1-LINK-SLC 
nw/wsj/20/wsj_2048.parse 38 14 gold estimate-v 54.4 Estimating estimate.01 1 ----- 0:2-ARG0=Agent 14:0-rel 16:2-ARG1=Theme 
nw/wsj/20/wsj_2048.parse 38 19 gold cost-v 54.2 Expensiveness cost.01 null ----- 16:1-ARG1=Theme;Goods 19:0-rel 20:1-ARG3 21:3-ARG2=Value;Asset 
nw/wsj/20/wsj_2048.parse 38 30 gold lose-v 13.2 NF lose.02 null ----- 30:0-rel 31:0-ARG1=Theme 
nw/wsj/20/wsj_2048.parse 40 8 gold earn-v 13.5.1-1 NF earn.01 null ----- 4:1*5:1*6:1-ARG0=Agent 7:1-ARGM-TMP 8:0-rel 9:1-ARG1=Theme 4:1*5:1-LINK-SLC 
nw/wsj/20/wsj_2048.parse 40 12 gold say-v 37.7-1 IN say.01 null ----- 0:3-ARG0=Agent 12:0-rel 13:1-ARG1=Topic 
nw/wsj/20/wsj_2048.parse 40 16 gold prefer-v 32.1-1-1 Preference prefer.01 1 ----- 14:1*17:1-ARG0 15:0-ARGM-MOD 16:0-rel 17:2-ARG1 
nw/wsj/20/wsj_2048.parse 40 19 gold work-v 73-3 IN work.01 null ----- 14:1*17:1-ARG0=Agent 19:0-rel 20:1;25:1-ARGM-LOC 
nw/wsj/20/wsj_2048.parse 40 23 gold own-v 100 NF own.01 null ----- 14:1*17:1-ARG0=Pivot 23:0-rel 24:1*25:1-ARG1=Theme 
nw/wsj/20/wsj_2048.parse 40 31 gold take-v 10.5 IN take.01 null ----- 30:1-ARG0=Agent 31:0-rel 32:2-ARG1=Theme 
nw/wsj/20/wsj_2048.parse 40 37 gold say-v 37.7-1 IN say.01 null ----- 37:0-rel 38:1-ARG1=Topic 40:2-ARG0=Agent 
nw/wsj/20/wsj_2048.parse 40 51 gold recruit-v 29.7 NF recruit.01 null ----- 51:0-rel 52:0-ARG0=Agent 
