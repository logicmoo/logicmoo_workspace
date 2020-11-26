nw/wsj/00/wsj_0041.parse 1 10 gold get-v 13.5.1-1 IN get.01 null ----- 4:1*9:1-ARG0=Agent 10:0-rel 11:1-ARG1=Theme 13:1-ARGM-LOC 
nw/wsj/00/wsj_0041.parse 1 36 gold scatter-v 9.7-1 IN scatter.01 1 ----- 32:1*37:1-ARG1=Theme 36:0-rel 38:1-ARG2=Destination 32:1*37:1-LINK-PSV 
nw/wsj/00/wsj_0041.parse 2 4 gold lead-v 51.7 Cotheme lead.01 null ----- 4:0-rel 5:0,6:0-ARG0=Agent;Theme 
nw/wsj/00/wsj_0041.parse 2 14 gold reach-v 13.5.1 NF reach.01 null ----- 0:0-ARGM-DIS 1:1-ARGM-LOC 10:1*20:1-ARG0=Agent 14:0-rel 15:2-ARG1=Theme 20:2-ARGM-ADV 
nw/wsj/00/wsj_0041.parse 2 21 gold raise-v 9.4 NF raise.01 null ----- 10:1*20:1-ARG0=Agent 21:0-rel 22:1-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 3 4 gold say-v 37.7-1 IN say.01 null ----- 4:0-rel 6:2-ARG0=Agent 0:1-ARG1-DSP=Topic 
nw/wsj/00/wsj_0041.parse 3 21 gold attack-v 33 Attack attack.01 null ----- 19:1-ARG0=Agent;Assailant 21:0-rel 22:1-ARGM-TMP 
nw/wsj/00/wsj_0041.parse 4 4 gold start-v 55.1-1 Process_start start.01 null ----- 0:1*2:1*3:1-ARG1=Theme;Event 4:0-rel 5:1-ARG2=Instrument 0:1*2:1-LINK-SLC 
nw/wsj/00/wsj_0041.parse 4 32 gold reach-v 13.5.1 NF reach.01 null ----- 0:2-ARG0=Agent 32:0-rel 33:1-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 5 3 gold get-v 13.5.1-1 IN get.01 null ----- 1:1*21:1-ARG0=Agent 3:0-rel 4:2-ARG1=Theme 7:1-ARGM-TMP 
nw/wsj/00/wsj_0041.parse 5 11 gold say-v 37.7-1 IN say.01 null ----- 11:0-rel 13:2-ARG0=Agent 0:1-ARG1-DSP=Topic 
nw/wsj/00/wsj_0041.parse 5 24 gold need-v 32.1-1-1 Required_event need.01 null ----- 21:1*25:1-ARG0=Pivot 24:0-rel 25:2-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 5 40 gold mean-v 29.5-1 NF mean.01 null ----- 34:2-ARGM-TMP 39:1-ARG0=Agent 40:0-rel 41:1-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 6 15 gold worry-v 31.1 Emotion_active worry.01 null ----- 8:1*13:1-ARG0=Stimulus 12:0-ARGM-MOD 15:0-rel 16:1-ARG1=Experiencer 
nw/wsj/00/wsj_0041.parse 11 3 gold fail-v 75-1-1 NF fail.01 null ----- 1:1-ARG1 3:0-rel 4:2-ARG2 10:1-ARGM-TMP 
nw/wsj/00/wsj_0041.parse 11 6 gold file-v 9.10 Submitting_documents file.01 null ----- 1:1*4:1-ARG0=Agent 6:0-rel 7:1-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 11 16 gold say-v 37.7-1 IN say.01 null ----- 1:2*17:1-ARG1=Topic 16:0-rel 18:1-ARG0=Agent 
nw/wsj/00/wsj_0041.parse 12 30 gold get-v 26.6.2 IN get.15 null ----- 0:0-ARGM-DIS 1:1-ARGM-TMP 2:2-ARG1=Patient 29:1-ARGM-ADV 30:0,31:0-rel 32:1-ARG2=Goal 
nw/wsj/00/wsj_0041.parse 13 10 gold fail-v 75-1-1 NF fail.01 null ----- 0:1*11:1-ARG1 9:1-ARGM-ADV 10:0-rel 11:2-ARG2 
nw/wsj/00/wsj_0041.parse 13 13 gold report-v 37.7-1 Statement report.01 null ----- 0:1*11:1-ARG0 13:0-rel 14:1-ARG1 17:1-ARGM-MNR 
nw/wsj/00/wsj_0041.parse 13 19 gold hide-v 16-1 Eclipse hide.01 1 ----- 0:1*11:1-ARG0=Agent;Obstruction 9:1-ARGM-DIS 19:0-rel 20:2-ARG1=Patient;Eclipsed 
nw/wsj/00/wsj_0041.parse 13 24 gold fail-v 74-2 IN fail.01 null ----- 24:0-rel 25:0,26:0-ARG1 
nw/wsj/00/wsj_0041.parse 14 5 gold say-v 37.7-1 IN say.01 null ----- 0:0,1:1*10:0,11:1,12:1,22:0,23:0-ARG1=Topic 5:0-rel 6:1-ARG1=Topic 7:1-ARG0=Agent 10:0*11:1-LINK-SLC 
nw/wsj/00/wsj_0041.parse 14 15 gold wait-v 47.1-1 IN wait.01 null ----- 10:0*11:1*21:1-ARGM-CAU 13:1-ARG1=Theme 14:1-ARGM-TMP 15:0-rel 16:1-ARGM-TMP 10:0*11:1-LINK-SLC 
nw/wsj/00/wsj_0041.parse 15 5 gold say-v 37.7-1 IN say.01 null ----- 0:2*6:1-ARGM-DSP 0:2*6:1-ARG1=Topic 5:0-rel 7:2-ARG0=Agent 
nw/wsj/00/wsj_0041.parse 15 21 gold prosecute-v 33 NF prosecute.01 1 ----- 0:1*18:1*19:1-ARG0=Agent 21:0-rel 22:2-ARG1=Theme 0:1*18:1-LINK-PSV 
nw/wsj/00/wsj_0041.parse 15 32 gold exist-v 47.1-1 Existence exist.01 null ----- 25:1*27:1*28:1-ARG1=Theme;Entity 29:1-ARGM-ADV 31:0-ARGM-NEG 32:0-rel 25:1*27:1-LINK-SLC 
nw/wsj/00/wsj_0041.parse 16 1 gold sting-v 20-1 Experiencer_obj sting.01 3 ----- 0:1*8:3-ARG1=Experiencer 1:0-rel 3:1-ARG0=Agent 
nw/wsj/00/wsj_0041.parse 16 21 gold unleash-v 23.3 NF unleash.01 1 ----- 0:1*2:1*8:3-ARG0=Agent 0:2-ARGM-PRD 20:1-ARGM-TMP 21:0-rel 22:2-ARG1=Patient 
nw/wsj/00/wsj_0041.parse 18 2 gold compare-v 22.2-2 Evaluative_comparison compare.01 null ----- 1:1-ARG0=Agent 2:0-rel 3:2-ARG1=Patient 
nw/wsj/00/wsj_0041.parse 18 9 gold say-v 37.7-1 IN say.01 null ----- 1:2*10:1-ARG1=Topic 9:0-rel 11:1-ARG0=Agent 
nw/wsj/00/wsj_0041.parse 19 2 gold say-v 37.7-1 IN say.01 null ----- 1:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/00/wsj_0041.parse 19 8 gold ban-v 67 Prohibiting ban.01 null ----- 7:1-ARG0=Agent;Principle 8:0-rel 9:2-ARG1=Theme;State_of_affairs 
nw/wsj/00/wsj_0041.parse 20 3 gold oppose-v 22.2-3 NF oppose.01 null ----- 0:1-ARG0=Patient 3:0-rel 4:2-ARG1=Co-Patient 
nw/wsj/00/wsj_0041.parse 21 1 gold claim-v 37.7-1 Statement claim.01 null ----- 0:1-ARG0=Agent;Speaker 1:0-rel 2:1-ARG1=Topic;Message 
nw/wsj/00/wsj_0041.parse 22 3 gold oppose-v 22.2-3 NF oppose.01 null ----- 0:1-ARG0=Patient 3:0-rel 4:2-ARG1=Co-Patient 
nw/wsj/00/wsj_0041.parse 22 10 gold choose-v 13.5.1 Choosing choose.01 null ----- 4:1*8:1-ARG0=Agent;Cognizer 10:0-rel 4:1*8:1-LINK-PRO 
nw/wsj/00/wsj_0041.parse 23 5 gold say-v 37.7-1 IN say.01 null ----- 0:1*6:1-ARGM-DSP 0:1*6:1-ARG1=Topic 5:0-rel 7:1-ARG0=Agent 
nw/wsj/00/wsj_0041.parse 23 15 gold name-v 29.3 IN name.01 null ----- 11:1*16:1-ARG1=Theme 15:0-rel 17:2-ARG2=Result 
nw/wsj/00/wsj_0041.parse 24 3 gold tell-v 37.2-1 Prevarication tell.01 null ----- 0:1*2:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/00/wsj_0041.parse 27 3 gold fail-v 75-1-1 NF fail.01 null ----- 0:1-ARG1 3:0-rel 4:2-ARG2 10:1-ARGM-TMP 
nw/wsj/00/wsj_0041.parse 27 6 gold file-v 9.10 Submitting_documents file.01 null ----- 0:1*4:1-ARG0=Agent 6:0-rel 7:1-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 27 16 gold insist-v 37.7 Statement insist.01 null ----- 15:1-ARG0=Agent;Speaker 16:0-rel 17:1-ARG1=Topic;Message 
nw/wsj/00/wsj_0041.parse 27 20 gold admit-v 29.5-2 IN admit.01 null ----- 18:1-ARG0=Agent 19:1-ARGM-MNR 20:0-rel 21:1-ARG1=Theme 25:2-ARGM-TMP 
nw/wsj/00/wsj_0041.parse 27 29 gold consider-v 29.9-1-1-1 Cogitation consider.02 null ----- 25:1*35:1-ARGM-TMP 26:1*30:1-ARG1=Theme;Topic 29:0-rel 31:1-ARG2=Attribute 
nw/wsj/00/wsj_0041.parse 28 15 gold insist-v 37.7 Statement insist.01 null ----- 14:1-ARG0=Agent;Speaker 15:0-rel 16:1-ARG1=Topic;Message 
nw/wsj/00/wsj_0041.parse 28 18 gold make-v 26.1-1 Cause_change make.01 null ----- 17:1-ARG0=Agent 18:0-rel 19:2-ARG1=Product 
nw/wsj/00/wsj_0041.parse 30 3 gold blame-v 33 IN blame.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG2=Attribute 7:1-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 31 2 gold say-v 37.7-1 IN say.01 null ----- 2:0-rel 5:1-ARG0=Agent 0:1-ARG1-DSP=Topic 
nw/wsj/00/wsj_0041.parse 31 11 gold know-v 29.5-1 IN know.01 2 ----- 0:0-ARGM-DIS 8:1-ARG0=Agent 10:0-ARGM-NEG 11:0-rel 12:1-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 31 23 gold get-v 26.6.2 IN get.05 null ----- 22:1-ARG0=Agent 23:0-rel 25:1-ARGM-DIR 27:1-ARG1=Patient 
nw/wsj/00/wsj_0041.parse 32 2 gold say-v 37.7-1 IN say.01 null ----- 2:0-rel 5:2-ARG0=Agent 0:1-ARG1-DSP=Topic 
nw/wsj/00/wsj_0041.parse 32 12 gold have-v 100 IN have.03 null ----- 10:1-ARG0=Pivot 12:0-rel 13:1-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 32 19 gold have-v 100 IN have.03 null ----- 16:1-ARG0=Pivot 19:0-rel 20:1-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 33 9 gold say-v 37.7-1 IN say.01 null ----- 0:3-ARG0=Agent 9:0-rel 10:1-ARG1=Topic 
nw/wsj/00/wsj_0041.parse 34 10 gold know-v 29.5-1 IN know.01 1 ----- 0:1-ARG0=Agent 10:0-rel 11:1-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 34 21 gold have-v 100 IN have.03 null ----- 20:1-ARG0=Pivot 21:0-rel 22:1-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 35 1 gold know-v 29.5-1 IN know.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:2-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 35 6 gold oppose-v 22.2-3 NF oppose.01 null ----- 3:1-ARG0=Patient 5:1-ARGM-ADV 6:0-rel 7:1-ARG1=Co-Patient 
nw/wsj/00/wsj_0041.parse 35 16 gold have-v 100 IN have.03 null ----- 15:1-ARG0=Pivot 16:0-rel 17:2-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 37 28 gold agree-v 36.1-1 IN agree.01 null ----- 23:2-ARG0=Agent 28:0-rel 29:1-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 38 5 gold superimpose-v 9.1 NF superimpose.01 null ----- 1:2*6:1-ARG1=Theme 5:0-rel 7:1-ARG2=Destination 1:2*6:1-LINK-PSV 
nw/wsj/00/wsj_0041.parse 38 14 gold talk-v 37.5 IN talk.01 null ----- 0:1-ARGM-ADV 12:1-ARG0=Agent 14:0-rel 15:1-ARG1=Topic 
nw/wsj/00/wsj_0041.parse 39 30 gold want-v 32.1-1-1 Desiring want.01 null ----- 22:1-ARGM-ADV 28:1*31:1-ARG0=Pivot;Experiencer 30:0-rel 31:3-ARG1=Theme;Event/Focal_participant 
nw/wsj/00/wsj_0041.parse 39 39 gold choose-v 13.5.1 Choosing choose.01 null ----- 37:1-ARG0=Agent;Cognizer 39:0-rel 
nw/wsj/00/wsj_0041.parse 39 42 gold give-v 13.1-1 Giving give.01 null ----- 28:1*41:1-ARG0=Agent;Donor 42:0-rel 43:1-ARG1=Theme;Theme 44:1-ARG2=Recipient;Recipient 
nw/wsj/00/wsj_0041.parse 40 5 gold say-v 37.7-1 IN say.01 null ----- 0:1*3:1*4:1-ARG0=Agent 5:0-rel 6:1-ARG1=Topic 0:1*3:1-LINK-SLC 
nw/wsj/00/wsj_0041.parse 40 9 gold want-v 32.1-1-1 Desiring want.01 null ----- 7:1*10:1-ARG0=Pivot;Experiencer 9:0-rel 10:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/00/wsj_0041.parse 40 33 gold deny-v 29.5-1 Statement deny.01 null ----- 28:1*30:1*34:1-ARG1=Predicate;Message 31:1-ARG0=Agent;Speaker 33:0-rel 28:1*30:1-LINK-SLC 
nw/wsj/00/wsj_0041.parse 40 44 gold transform-v 26.6.1 NF transform.01 1 ----- 0:2*43:1-ARG0=Agent 44:0-rel 45:1-ARG1=Patient 47:2-ARGM-EXT 52:1-ARG2=Result 
nw/wsj/00/wsj_0041.parse 41 2 gold prompt-v 59 NF prompt.02 1 ----- 0:1-ARG0=Agent 2:0-rel 3:2*12:1-ARG1=Patient 12:2-ARG2=Result 
nw/wsj/00/wsj_0041.parse 41 14 gold launch-v 55.5-1 IN launch.01 null ----- 3:2*12:1-ARG0 14:0-rel 15:2-ARG1 
nw/wsj/00/wsj_0041.parse 41 29 gold shake-v 31.1 Cause_to_move_in_place shake.01 null ----- 18:1*26:1*27:1-ARG0=Stimulus 29:0-rel 30:3-ARG1=Experiencer 18:1*26:1-LINK-PSV 
nw/wsj/00/wsj_0041.parse 42 17 gold suggest-v 37.7-1-1 Statement suggest.01 null ----- 15:1-ARG0=Agent;Speaker 17:0-rel 18:1-ARG1=Topic;Message 
nw/wsj/00/wsj_0041.parse 43 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 5:2-ARG1=Topic 
nw/wsj/00/wsj_0041.parse 43 12 gold have-v 100 IN have.03 null ----- 10:0-ARGM-NEG 11:1-ARG0=Pivot 12:0-rel 13:1-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 44 12 gold introduce-v 22.2-3-1 NF introduce.02 null ----- 8:1*10:1*11:1-ARG0=Agent 12:0-rel 13:2-ARG1=Patient 8:1*10:1-LINK-SLC 
nw/wsj/00/wsj_0041.parse 44 18 gold force-v 59 NF force.01 null ----- 13:1*15:1*16:1-ARG0=Agent 18:0-rel 19:2-ARG1=Patient 25:1-ARG2=Result 13:1*15:1-LINK-SLC 
nw/wsj/00/wsj_0041.parse 44 27 gold interrogate-v 37.1.3 NF interrogate.01 null ----- 19:2*28:1-ARG2=Recipient 27:0-rel 29:1-ARG1=Topic 33:1-ARG0=Agent 
nw/wsj/00/wsj_0041.parse 44 36 gold accuse-v 81 Judgement accuse.01 1 ----- 36:0-rel 37:0-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 45 7 gold talk-v 37.5 IN talk.01 null ----- 1:1*4:1-ARGM-TMP 5:1-ARG0=Agent 7:0-rel 8:1-ARG1=Topic 1:1*4:1-LINK-SLC 
nw/wsj/00/wsj_0041.parse 45 16 gold ask-v 37.1.2 Questioning ask.01 null ----- 0:1-ARGM-DIS 1:2-ARGM-TMP 15:1-ARG0=Agent;Speaker 16:0-rel 17:1-ARG2=Recipient;Addressee 18:1-ARG1=Topic;Message 
nw/wsj/00/wsj_0041.parse 45 23 gold try-v 61 Attempt try.01 null ----- 22:1*24:1-ARG0=Agent 23:0-rel 24:2-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 46 3 gold introduce-v 22.2-3-1 NF introduce.02 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Patient 6:2-ARGM-TMP 
nw/wsj/00/wsj_0041.parse 46 25 gold use-v 105 IN use.01 null ----- 21:1*26:1-ARG1 25:0-rel 27:1-ARG0 21:1*26:1-LINK-PSV 
nw/wsj/00/wsj_0041.parse 47 7 gold introduce-v 22.2-3-1 NF introduce.02 null ----- 5:1*8:1-ARG1=Patient 7:0-rel 10:1-ARGM-MNR 
nw/wsj/00/wsj_0041.parse 47 19 gold introduce-v 22.2-3-1 NF introduce.02 null ----- 15:1-ARGM-TMP 17:1-ARG0=Agent 19:0-rel 20:2-ARG1=Patient 
nw/wsj/00/wsj_0041.parse 48 3 gold grow-v 48.1.1 NF grow.02 null ----- 1:1-ARG1=Theme 3:0-rel 4:1-ARG2 
nw/wsj/00/wsj_0041.parse 48 22 gold say-v 37.7-1 IN say.01 null ----- 1:3*23:1-ARG1=Topic 22:0-rel 24:2-ARG0=Agent 
nw/wsj/00/wsj_0041.parse 49 3 gold manage-v 74-1-1 IN manage.02 null ----- 1:1*4:1-ARG0 3:0-rel 4:2-ARG1 
nw/wsj/00/wsj_0041.parse 49 6 gold get-v 26.6.2 IN get.03 null ----- 1:1*4:1-ARG1=Patient 6:0-rel 7:1-ARGM-DIR 8:1-ARG2=Goal 
nw/wsj/00/wsj_0041.parse 49 13 gold say-v 37.7-1 IN say.01 null ----- 11:1*15:1-ARG0=Agent 12:0-ARGM-MOD 13:0-rel 14:1-ARG1=Topic 15:2-ARGM-PRP 
nw/wsj/00/wsj_0041.parse 49 17 gold get-v 26.6.2 IN get.03 null ----- 11:1*15:1*19:1-ARG1=Patient 17:0-rel 18:1-ARG2=Goal 
nw/wsj/00/wsj_0041.parse 49 18 gold elect-v 29.1 Change_of_leadership elect.01 null ----- 11:1*15:1*19:1-ARG1=Theme;New_leader 18:0-rel 20:2-ARG2=Result;Role 
nw/wsj/00/wsj_0041.parse 49 33 gold put-v 9.1-2 IN put.01 null ----- 1:1*31:1-ARG0=Agent 33:0-rel 34:2-ARG1=Theme 41:1-ARG2=Destination 
nw/wsj/00/wsj_0041.parse 50 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARGM-TMP 5:1-ARG1=Topic 
nw/wsj/00/wsj_0041.parse 50 8 gold devote-v 79 NF devote.01 2 ----- 6:1-ARG0=Agent 7:0-ARGM-MOD 8:0-rel 9:2-ARG1=Theme 15:1-ARG2=Goal 
nw/wsj/00/wsj_0041.parse 50 22 gold last-v 54.2 NF last.01 null ----- 20:1-ARG1=Theme 22:0-rel 23:1-ARG2=Value 
nw/wsj/00/wsj_0041.parse 51 7 gold carry-v 11.4 Bringing carry.01 null ----- 0:1-ARGM-TMP 4:1-ARG0=Agent 7:0-rel 8:2-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 51 14 gold raise-v 9.4 NF raise.01 null ----- 11:2-ARG0=Agent 14:0-rel 15:2-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 53 2 gold begin-v 55.1-1 Process_start begin.01 null ----- 0:1-ARG1=Theme;Event 2:0-rel 3:2-ARGM-TMP 
nw/wsj/00/wsj_0041.parse 54 6 gold say-v 37.7-1 IN say.01 null ----- 1:2*7:1-ARG1=Topic 6:0-rel 8:1-ARG0=Agent 
nw/wsj/00/wsj_0041.parse 55 2 gold consider-v 29.9-1-1-1 Cogitation consider.01 null ----- 1:1-ARG0=Agent;Cognizer 2:0-rel 3:1-ARG1=Theme;Topic 
nw/wsj/00/wsj_0041.parse 56 11 gold get-v 26.6.2 IN get.15 null ----- 0:0-ARGM-DIS 1:1-ARGM-TMP 2:2-ARG1=Patient 11:0,12:0-rel 13:1-ARG2=Goal 
nw/wsj/00/wsj_0041.parse 57 13 gold purr-v 37.3 Communication_noise purr.02 2 ----- 10:1-ARG0=Agent;Speaker 13:0-rel 16:3-ARG1=Topic;Message 
nw/wsj/00/wsj_0041.parse 59 5 gold cry-v 37.3 Communication_noise cry.01 2 ----- 5:0-rel 7:2-ARG0=Agent;Speaker 0:3-ARG1-DSP=Topic;Message 
nw/wsj/00/wsj_0041.parse 59 24 gold respond-v 37.7 Communication_response respond.01 null ----- 20:1-ARG0=Agent;Speaker 24:0-rel 25:1-ARG2=Topic;Message/Trigger 
nw/wsj/00/wsj_0041.parse 61 8 gold ask-v 37.1.2 Questioning ask.01 null ----- 1:2*9:1-ARG1=Topic;Message 8:0-rel 10:1-ARG0=Agent;Speaker 
nw/wsj/00/wsj_0041.parse 62 20 gold contain-v 47.8 IN contain.01 null ----- 13:2-ARG0=Theme 20:0-rel 21:1-ARG1=Co-Theme 
nw/wsj/00/wsj_0041.parse 62 25 gold clean-v 10.3 NF clean.02 null ----- 13:2*26:1-ARG1=Theme 25:0,27:1-rel 
nw/wsj/00/wsj_0041.parse 62 29 gold cause-v 27 Causation cause.01 1 ----- 13:2-ARG0=Cause;Cause 29:0-rel 30:1-ARG1=Theme;Effect 
nw/wsj/00/wsj_0041.parse 64 3 gold tell-v 37.2-1 Prevarication tell.01 null ----- 0:1*1:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/00/wsj_0041.parse 65 13 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 13:0-rel 14:1-ARG1=Topic 
nw/wsj/00/wsj_0041.parse 66 2 gold dump-v 9.3-1-1 NF dump.01 null ----- 0:1*3:1-ARG1=Theme 2:0-rel 4:1-ARGM-LOC 
nw/wsj/00/wsj_0041.parse 67 12 gold devastate-v 31.1 Experiencer_obj devastate.01 null ----- 0:1-ARGM-DIS 3:1-ARGM-ADV 8:1-ARG0=Stimulus;Stimulus 12:0-rel 14:1-ARGM-CAU 
nw/wsj/00/wsj_0041.parse 67 16 gold raise-v 9.4 NF raise.01 null ----- 15:1-ARG0=Agent 16:0-rel 17:2-ARG1=Theme 
nw/wsj/00/wsj_0041.parse 68 3 gold build-v 26.1-1 Building build.01 null ----- 1:1-ARG0=Agent;Agent 3:0-rel 4:1-ARG2=Material;Components 
nw/wsj/00/wsj_0041.parse 69 21 gold appear-v 48.1.1 Coming_to_be appear.01 null ----- 0:1-ARGM-TMP 3:1-ARGM-LOC 14:3*24:1-ARG1=Theme;Entity 21:0-rel 22:1-ARGM-MNR 24:2-ARGM-ADV 
nw/wsj/00/wsj_0041.parse 69 25 gold say-v 37.7-1 IN say.01 null ----- 14:3*24:1-ARG0=Agent 25:0-rel 28:2-ARG1=Topic 
nw/wsj/00/wsj_0041.parse 69 30 gold want-v 32.1-1-1 Desiring want.01 null ----- 29:1*31:1-ARG0=Pivot;Experiencer 30:0-rel 31:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/00/wsj_0041.parse 69 33 gold keep-v 55.6 Cause_to_continue keep.02 null ----- 29:1*31:1-ARG0=Agent 33:0-rel 34:1,37:2-ARG1=Theme 
