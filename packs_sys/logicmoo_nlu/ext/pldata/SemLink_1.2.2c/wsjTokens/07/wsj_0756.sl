nw/wsj/07/wsj_0756.parse 0 4 gold grapple-v 83-1-1 NF grapple.01 null ----- 1:1-ARG0=Agent 4:0-rel 5:1-ARG1=Theme 
nw/wsj/07/wsj_0756.parse 0 22 gold push-v 23.2 Cause_motion push.01 null ----- 20:1-ARG0=Agent 22:0-rel 23:1-ARG1=Patient 25:2-ARG2=Co-Patient 
nw/wsj/07/wsj_0756.parse 2 1 gold lay-v 9.2 Placing lay.01 null ----- 0:1-ARG0=Agent;Agent/Cause 1:0-rel 2:1-ARGM-DIR 3:2-ARG1=Theme;Theme 
nw/wsj/07/wsj_0756.parse 2 15 gold set-v 9.1-2 IN set.01 null ----- 14:1-ARG0=Agent 15:0-rel 16:1-ARG1=Theme 
nw/wsj/07/wsj_0756.parse 5 19 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-ADV 17:1-ARG0=Agent 19:0-rel 20:1-ARG1=Topic 
nw/wsj/07/wsj_0756.parse 5 24 gold write-v 25.2 Text_creation write.01 null ----- 21:1*25:1-ARG1=Theme;Text 24:0-rel 26:1-ARG0=Agent;Author 
nw/wsj/07/wsj_0756.parse 6 16 gold appoint-v 29.1 Change_of_leadership appoint.01 null ----- 0:2*17:1-ARG1=Theme;New_leader 15:1-ARGM-TMP 16:0-rel 18:2-ARG2=Result;Role 
nw/wsj/07/wsj_0756.parse 7 14 gold say-v 37.7-1 IN say.01 null ----- 0:1*15:1-ARG1=Topic 14:0-rel 16:1-ARG0=Agent 
nw/wsj/07/wsj_0756.parse 9 4 gold propose-v 37.7-1 Statement propose.01 null ----- 4:0-rel 5:0-ARG1=Topic;Message 
nw/wsj/07/wsj_0756.parse 9 23 gold announce-v 37.7-1 Statement announce.01 null ----- 20:1*24:1-ARG1=Topic;Message 23:0-rel 25:1-ARG0=Agent;Speaker 28:1-ARGM-TMP 20:1*24:1-LINK-PSV 
nw/wsj/07/wsj_0756.parse 9 32 gold drop-v 10.10 IN drop.05 null ----- 20:1*33:1-ARG1=Theme 31:1-ARGM-TMP 32:0-rel 20:1*33:1-LINK-PSV 
nw/wsj/07/wsj_0756.parse 10 24 gold alienate-v 31.1 NF alienate.01 1 ----- 1:1*8:1*22:1-ARG0=Stimulus 24:0-rel 25:3-ARG1=Experiencer 1:1*8:1-LINK-PRO 
nw/wsj/07/wsj_0756.parse 10 35 gold lose-v 13.2 NF lose.06 null ----- 25:2*30:1*31:1*33:1-ARG0=Agent 35:0,36:1-rel 37:1-ARGM-ADV 25:2*30:1-LINK-SLC 
nw/wsj/07/wsj_0756.parse 10 40 gold implement-v 55.5-1 Execute_plan implement.01 1 ----- 38:1*41:1-ARG1=Theme 40:0-rel 
nw/wsj/07/wsj_0756.parse 11 13 gold demand-v 60-1 Request demand.01 null ----- 10:1*11:1*12:1-ARG0 13:0-rel 14:1-ARG1 10:1*11:1-LINK-SLC 
nw/wsj/07/wsj_0756.parse 11 22 gold call-v 29.3 IN call.01 null ----- 20:1*23:1-ARG1=Theme 21:1-ARG0=Agent 22:0-rel 24:2-ARG2=Result 
nw/wsj/07/wsj_0756.parse 12 22 gold present-v 13.4.1 NF present.01 null ----- 0:1*23:1-ARG1=Theme 19:0-ARGM-MOD 20:1-ARGM-ADV 22:0-rel 24:1-ARG2=Recipient 28:1-ARGM-PNC 30:1-ARGM-TMP 
nw/wsj/07/wsj_0756.parse 13 17 gold start-v 55.1-1 Activity_start start.01 null ----- 8:1*15:1*16:1-ARG1=Theme;Activity 17:0-rel 18:1-ARGM-TMP 8:1*15:1-LINK-SLC 
nw/wsj/07/wsj_0756.parse 14 22 gold exist-v 47.1-1 Existence exist.01 null ----- 13:3-ARG1=Theme;Entity 19:1-ARGM-ADV 21:0-ARGM-NEG 22:0-rel 
nw/wsj/07/wsj_0756.parse 16 7 gold start-v 55.1-1 Process_start start.01 null ----- 0:2,8:2,9:0,10:0,11:1,13:1-ARG1=Theme;Event 4:0-ARGM-MOD 5:1-ARGM-ADV 6:1-ARGM-ADV 7:0-rel 
nw/wsj/07/wsj_0756.parse 16 10 gold have-v 100 IN have.03 null ----- 0:2*8:1-ARG0=Pivot 10:0-rel 11:1,13:1-ARG1=Theme 17:1-ARGM-TMP 22:1-ARGM-TMP 
nw/wsj/07/wsj_0756.parse 17 5 gold include-v 65 NF include.01 null ----- 0:2-ARG2=Location 5:0-rel 8:1-ARG1=Theme 
nw/wsj/07/wsj_0756.parse 18 8 gold abandon-v 51.2 Departing abandon.01 null ----- 0:2*9:1-ARG1=Initial_Location;Source 6:0-ARGM-MOD 8:0-rel 
nw/wsj/07/wsj_0756.parse 19 2 gold propose-v 37.7-1 Statement propose.01 null ----- 0:1*3:1-ARG0=Agent;Speaker 2:0-rel 3:2-ARG1=Topic;Message 
nw/wsj/07/wsj_0756.parse 19 17 gold allow-v 64 IN allow.01 null ----- 0:1*3:1-ARG0 17:0-rel 18:2-ARG1 
nw/wsj/07/wsj_0756.parse 19 23 gold flourish-v 47.1-1 Thriving flourish.01 1 ----- 18:1*25:1-ARG1=Theme 23:0-rel 25:2-ARGM-ADV 
nw/wsj/07/wsj_0756.parse 19 26 gold help-v 72-1 Assistance help.01 null ----- 18:1*25:1*27:1-ARG1=Theme;Goal/Focal_entity 26:0-rel 28:1-ARG0=Agent;Helper 
nw/wsj/07/wsj_0756.parse 20 20 gold lease-v 13.5.1 Renting lease.01 1 ----- 19:1*21:1-ARG1=Theme 20:0-rel 22:1-ARG0=Agent 19:1*21:1-LINK-PSV 
nw/wsj/07/wsj_0756.parse 20 25 gold own-v 100 NF own.01 null ----- 19:1*26:1-ARG1=Theme 25:0-rel 27:1-ARG0=Pivot 19:1*26:1-LINK-PSV 
nw/wsj/07/wsj_0756.parse 21 7 gold sanction-v 64 IN sanction.01 null ----- 0:2*8:1-ARG1=Theme 5:0-ARGM-MOD 7:0-rel 
nw/wsj/07/wsj_0756.parse 22 4 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 null ----- 0:1-ARG0=Cause 2:0-ARGM-MOD 3:1-ARG2 4:0-rel 5:2-ARG1=Patient 
nw/wsj/07/wsj_0756.parse 22 15 gold guard-v 29.8-1 NF guard.01 1 ----- 8:1*11:1*13:1-ARG0=Agent 12:1-ARGM-TMP 14:1-ARGM-MNR 15:0-rel 16:1-ARG1=Beneficiary 8:1*11:1-LINK-SLC 
nw/wsj/07/wsj_0756.parse 24 4 gold introduce-v 22.2-3-1 NF introduce.02 null ----- 0:1*5:1-ARG1=Patient 2:0-ARGM-MOD 4:0-rel 6:2-ARGM-PRP 
nw/wsj/07/wsj_0756.parse 25 29 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 25:1*26:1*27:1-ARG0=Agent;Buyer 28:0-ARGM-MOD 29:0-rel 32:1-ARG1=Theme;Goods 25:1*26:1-LINK-SLC 
nw/wsj/07/wsj_0756.parse 25 31 gold lease-v 13.5.1 Renting lease.01 1 ----- 25:1*26:1*27:1-ARG0=Agent 28:0-ARGM-MOD 31:0-rel 32:1-ARG1=Theme 25:1*26:1-LINK-SLC 
nw/wsj/07/wsj_0756.parse 25 34 gold turn-v 26.6.2 Cause_change turn.02 null ----- 25:1*26:1*27:1-ARG0=Agent 28:0-ARGM-MOD 34:0-rel 35:1-ARG1=Patient 36:1-ARG2=Goal 25:1*26:1-LINK-SLC 
nw/wsj/07/wsj_0756.parse 26 8 gold liquidate-v 42.1 Killing liquidate.01 2 ----- 0:1*6:1-ARG0=Agent;Killer/Cause 8:0-rel 9:1-ARG1=Patient;Victim 14:1-ARGM-TMP 
nw/wsj/07/wsj_0756.parse 27 7 gold introduce-v 22.2-3-1 NF introduce.02 null ----- 0:2*8:1-ARG1=Patient 5:0-ARGM-MOD 7:0-rel 9:1-ARGM-MNR 
nw/wsj/07/wsj_0756.parse 28 16 gold encourage-v 31.1 NF encourage.02 null ----- 0:1*13:1-ARG0=Stimulus 0:2-ARGM-PRP 15:0-ARGM-MOD 16:0-rel 17:1-ARG1=Experiencer 20:1-ARGM-ADV 
nw/wsj/07/wsj_0756.parse 28 22 gold issue-v 13.3 NF issue.01 null ----- 13:1*21:1-ARG0=Agent 22:0-rel 23:2-ARG1=Theme 13:1*21:1-LINK-PRO 
nw/wsj/07/wsj_0756.parse 28 26 gold guarantee-v 13.3 NF guarantee.01 1 ----- 23:1*24:1*25:1-ARG0=Agent 26:0-rel 27:1-ARG2=Goal 28:1-ARG1=Theme 23:1*24:1-LINK-SLC 
nw/wsj/07/wsj_0756.parse 28 32 gold purchase-v 13.5.2-1 Commerce_buy purchase.01 1 ----- 27:1*30:1-ARG0=Agent;Buyer 32:0-rel 33:1-ARG1=Theme;Goods 27:1*30:1-LINK-PRO 
nw/wsj/07/wsj_0756.parse 30 8 gold replace-v 13.6 IN replace.01 null ----- 0:2-ARG2=Co-Theme 7:0-ARGM-MOD 8:0-rel 9:1-ARG1=Theme 
nw/wsj/07/wsj_0756.parse 31 12 gold lead-v 59 Causation lead.03 null ----- 0:1*8:1-ARG0=Agent 0:2-ARGM-PRD 11:0-ARGM-MOD 12:0-rel 13:2-ARG2=Result 
nw/wsj/07/wsj_0756.parse 32 9 gold introduce-v 22.2-3-1 NF introduce.02 null ----- 0:1*6:1-ARG0=Agent 0:2-ARGM-PRP 8:0-ARGM-MOD 9:0-rel 10:2-ARG1=Patient 
nw/wsj/07/wsj_0756.parse 34 5 gold set-v 9.1-2 IN set.01 null ----- 4:0-ARGM-MNR 5:0-rel 6:0-ARG1=Theme 
nw/wsj/07/wsj_0756.parse 34 17 gold introduce-v 22.2-3-1 NF introduce.02 null ----- 13:2-ARG1=Patient 17:0-rel 
nw/wsj/07/wsj_0756.parse 35 7 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:1*5:1-ARG0=Agent;Buyer 7:0-rel 8:1-ARG1=Theme;Goods 13:1-ARG3=Asset 
nw/wsj/07/wsj_0756.parse 35 28 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 18:3*29:1-ARG1=Theme;Goods 26:0-ARGM-MOD 28:0-rel 30:1-ARG3 
nw/wsj/07/wsj_0756.parse 36 4 gold divide-v 23.1-1 Separating divide.02 null ----- 0:1*5:1-ARG1 2:0-ARGM-MOD 4:0-rel 6:1-ARG2 
nw/wsj/07/wsj_0756.parse 36 12 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 10:1*13:1-ARG1=Theme;Goods 12:0-rel 14:1-ARG3 10:1*13:1-LINK-PSV 
nw/wsj/07/wsj_0756.parse 36 15 gold fix-v 54.4 Bail_decision fix.03 null ----- 15:0-rel 16:0-ARG1=Theme 
nw/wsj/07/wsj_0756.parse 37 6 gold ensure-v 99 NF ensure.01 1 ----- 0:2-ARG0=Cause 5:0-ARGM-MOD 6:0-rel 7:1-ARG1=Theme 
nw/wsj/07/wsj_0756.parse 37 14 gold suffer-v 31.3-4 Catastrophe suffer.01 null ----- 8:1-ARG0=Experiencer 13:0-ARGM-NEG 14:0-rel 15:1-ARGM-MNR 
nw/wsj/07/wsj_0756.parse 39 10 gold take-v 11.3 Bringing take.01 null ----- 0:2*11:1-ARG1=Theme 8:0-ARGM-MOD 10:0-rel 12:1-ARGM-DIR 
nw/wsj/07/wsj_0756.parse 40 3 gold face-v 98 Confronting_problem face.01 null ----- 0:1-ARG0 2:0-ARGM-MOD 3:0-rel 4:2-ARG1 
nw/wsj/07/wsj_0756.parse 40 11 gold invest-v 13.4.2 NF invest.01 null ----- 0:1-ARG0=Agent 9:0-ARGM-MOD 10:1-ARGM-DIS 11:0-rel 12:1-ARG1=Theme 15:1-ARGM-LOC 
nw/wsj/07/wsj_0756.parse 41 2 gold receive-v 13.5.2 Receiving receive.01 null ----- 0:1-ARG0=Agent;Donor 1:0-ARGM-MOD 2:0-rel 3:1-ARG1=Theme;Theme 
nw/wsj/07/wsj_0756.parse 41 7 gold invest-v 13.4.2 NF invest.01 null ----- 0:1*5:1-ARG0=Agent 7:0-rel 8:1-ARG2=Recipient 0:1*5:1-LINK-PRO 
nw/wsj/07/wsj_0756.parse 42 12 gold introduce-v 22.2-3-1 NF introduce.02 null ----- 0:1-ARGM-LOC 6:2*13:1;14:2-ARG1=Patient 10:0-ARGM-MOD 12:0-rel 
nw/wsj/07/wsj_0756.parse 42 19 gold exchange-v 13.6-1 IN exchange.01 null ----- 6:1*14:1*15:1*20:1-ARG1=Theme 16:0-ARGM-MOD 18:1-ARGM-MNR 19:0-rel 21:1-ARG3=Co-Theme 6:1*14:1-LINK-SLC 
nw/wsj/07/wsj_0756.parse 44 24 gold oppose-v 22.2-3 NF oppose.01 null ----- 13:1*18:1*19:1*22:1-ARG0=Patient 24:0-rel 25:1-ARG1=Co-Patient 13:1*18:1-LINK-SLC 
nw/wsj/07/wsj_0756.parse 44 34 gold know-v 29.5-1 IN know.01 2 ----- 29:1*35:1-ARG1=Theme 34:0-rel 36:1-ARG2=Predicate 29:1*35:1-LINK-PSV 
nw/wsj/07/wsj_0756.parse 45 2 gold hint-v 37.7-2 NF hint.01 1 ----- 1:1-ARG0=Agent 2:0-rel 3:1-ARGM-MNR 4:1-ARG1=Topic 
nw/wsj/07/wsj_0756.parse 45 9 gold lose-v 13.2 NF lose.02 null ----- 5:1-ARG0=Agent 7:0-ARGM-MOD 8:1-ARGM-ADV 9:0-rel 10:1-ARG1=Theme 12:1-ARGM-TMP 
