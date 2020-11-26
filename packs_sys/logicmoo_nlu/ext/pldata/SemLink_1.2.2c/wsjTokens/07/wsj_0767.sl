nw/wsj/07/wsj_0767.parse 0 4 gold allow-v 64 IN allow.01 null ----- 0:1-ARG0 4:0-rel 5:2-ARG1 
nw/wsj/07/wsj_0767.parse 0 11 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 5:1-ARG0 11:0-rel 12:2-ARG1 
nw/wsj/07/wsj_0767.parse 0 13 gold offer-v 13.3 NF offer.01 null ----- 5:1*12:1-ARG0=Agent 13:0-rel 14:2-ARG1=Theme 
nw/wsj/07/wsj_0767.parse 0 23 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 23:0-rel 24:1-ARG1=Topic 
nw/wsj/07/wsj_0767.parse 2 8 gold champion-v 29.8-1 NF champion.01 null ----- 0:2-ARG0=Agent 8:0-rel 9:2-ARG1=Beneficiary 12:1-ARGM-LOC 
nw/wsj/07/wsj_0767.parse 3 18 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 9:2-ARG0 17:0-ARGM-MOD 18:0-rel 19:2-ARG1 
nw/wsj/07/wsj_0767.parse 3 20 gold push-v 59 Manipulation push.02 null ----- 9:2*19:1-ARG0=Agent 20:0-rel 21:1-ARG1=Patient 22:1-ARGM-LOC 25:1-ARGM-ADV 
nw/wsj/07/wsj_0767.parse 4 2 gold mean-v 29.5-1 NF mean.01 null ----- 1:1-ARG0=Agent 2:0-rel 3:1-ARG1=Theme 
nw/wsj/07/wsj_0767.parse 4 15 gold say-v 37.7-1 IN say.01 null ----- 0:1*16:1-ARG1=Topic 15:0-rel 17:3-ARG0=Agent 
nw/wsj/07/wsj_0767.parse 7 8 gold allow-v 64 IN allow.01 null ----- 0:1-ARGM-MNR 7:1-ARG0 8:0-rel 9:2-ARG1 28:2-ARGM-PRD 
nw/wsj/07/wsj_0767.parse 7 11 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 9:1*12:1-ARG0 11:0-rel 12:2*22:1-ARG1 
nw/wsj/07/wsj_0767.parse 7 13 gold offer-v 13.3 NF offer.01 null ----- 9:1*12:1-ARG0=Agent 13:0-rel 14:2*22:1-ARG1=Theme 18:1-ARG3=Goal 
nw/wsj/07/wsj_0767.parse 7 22 gold call-v 29.3 IN call.01 null ----- 14:1*23:1-ARG1=Theme 22:0-rel 24:2-ARG2=Result 14:1*23:1-LINK-PSV 
nw/wsj/07/wsj_0767.parse 8 10 gold choose-v 13.5.1 Choosing choose.01 null ----- 0:1-ARGM-TMP 1:1-ARGM-MNR 9:1-ARG0=Agent;Cognizer 10:0-rel 11:2-ARG1=Theme;Chosen 
nw/wsj/07/wsj_0767.parse 8 25 gold call-v 29.3 IN call.01 null ----- 20:1*26:1-ARG1=Theme 25:0-rel 27:2-ARG2=Result 20:1*26:1-LINK-PRO 
nw/wsj/07/wsj_0767.parse 8 33 gold offer-v 13.3 NF offer.01 null ----- 20:2*31:1*34:1-ARG1=Theme 32:1-ARG0=Agent 33:0-rel 35:1-ARG3=Goal 20:2*31:1-LINK-SLC 
nw/wsj/07/wsj_0767.parse 9 1 gold give-v 13.1-1 Giving give.01 null ----- 0:1-ARG0=Agent;Donor 1:0-rel 2:1-ARG1=Theme;Theme 9:1-ARG2=Recipient;Recipient 22:1-ARGM-CAU 
nw/wsj/07/wsj_0767.parse 9 28 gold offer-v 13.3 NF offer.01 null ----- 25:1*29:1-ARG1=Theme 28:0-rel 30:1-ARG3=Goal 33:1-ARG0=Agent 25:1*29:1-LINK-PSV 
nw/wsj/07/wsj_0767.parse 10 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/07/wsj_0767.parse 10 9 gold expire-v 48.2 Death expire.01 1 ----- 5:2-ARG1=Patient;Protagonist 9:0-rel 
nw/wsj/07/wsj_0767.parse 10 13 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 4:1-ARGM-CAU 10:1-ARG0 11:0-ARGM-MOD 12:0-ARGM-NEG 13:0-rel 14:2-ARG1 
nw/wsj/07/wsj_0767.parse 10 16 gold offer-v 13.3 NF offer.01 null ----- 10:1*14:1-ARG0=Agent 16:0-rel 17:1-ARG1=Theme 
nw/wsj/07/wsj_0767.parse 11 6 gold rule-v 29.3 Verdict rule.01 null ----- 0:0-ARGM-DIS 1:1-ARG0=Agent;Judge 3:1-ARGM-ADV 5:0-ARGM-NEG 6:0-rel 7:1-ARG1=Theme;Defendant 
nw/wsj/07/wsj_0767.parse 11 9 gold have-v 100 IN have.03 null ----- 8:1-ARG0=Pivot 9:0-rel 10:1-ARG1=Theme 18:1-ARGM-ADV 
nw/wsj/07/wsj_0767.parse 11 14 gold match-v 22.2-1 Compatibility match.01 1 ----- 8:1*12:1-ARG0=Agent 14:0-rel 15:2-ARG1=Patient;Item_1/Items 8:1*12:1-LINK-PRO 
nw/wsj/07/wsj_0767.parse 11 20 gold mean-v 29.5-1 NF mean.01 null ----- 19:1-ARG0=Agent 20:0-rel 21:2-ARG1=Theme 
nw/wsj/07/wsj_0767.parse 11 22 gold give-v 13.1-1 Giving give.01 null ----- 8:1*21:1-ARG0=Agent;Donor 22:0-rel 23:2-ARG1=Theme;Theme 8:1*21:1-LINK-PRO 
nw/wsj/07/wsj_0767.parse 12 15 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-DIS 2:2-ARG0=Agent 15:0-rel 16:1-ARG1=Topic 
nw/wsj/07/wsj_0767.parse 12 18 gold offer-v 13.3 NF offer.01 null ----- 17:1*32:1-ARG0=Agent 18:0-rel 19:1-ARG1=Theme 24:1-ARG3=Goal 32:2-ARGM-PRP 
nw/wsj/07/wsj_0767.parse 13 5 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 4:0-ARGM-NEG 5:0-rel 6:1-ARG1=Topic 
nw/wsj/07/wsj_0767.parse 13 16 gold say-v 37.7-1 IN say.01 null ----- 0:2*17:1-ARG1=Topic 15:1-ARG0=Agent 16:0-rel 
nw/wsj/07/wsj_0767.parse 14 13 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARGM-ADV 6:2-ARG0=Agent 13:0-rel 14:1-ARG1=Topic 
nw/wsj/07/wsj_0767.parse 14 16 gold expect-v 62 IN expect.01 null ----- 15:1*17:1-ARG0=Experiencer 16:0-rel 17:2-ARG1=Theme 
nw/wsj/07/wsj_0767.parse 15 13 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 13:0-rel 14:1-ARG1=Topic 
nw/wsj/07/wsj_0767.parse 15 16 gold expect-v 62 IN expect.01 null ----- 15:1-ARG0=Experiencer 16:0-rel 17:2-ARG1=Theme 
nw/wsj/07/wsj_0767.parse 15 20 gold propose-v 37.7-1 Statement propose.01 null ----- 17:1-ARG0=Agent;Speaker 20:0-rel 21:1-ARG1=Topic;Message 23:1-ARGM-TMP 
nw/wsj/07/wsj_0767.parse 16 1 gold applaud-v 33 Judgment applaud.01 2 ----- 0:1-ARG0=Agent;Cognizer 1:0-rel 2:2-ARG1=Theme;Evaluee 
nw/wsj/07/wsj_0767.parse 17 36 gold say-v 37.7-1 IN say.01 null ----- 0:1*37:1-ARG1=Topic 31:1-ARG0=Agent 36:0-rel 38:1-ARGM-LOC 
nw/wsj/07/wsj_0767.parse 18 12 gold concern-v 86.2-1 Topic concern.02 null ----- 9:1-ARG0=Theme 12:0-rel 13:1-ARG1=Co-Theme 
nw/wsj/07/wsj_0767.parse 18 24 gold allow-v 64 IN allow.01 null ----- 21:0-ARGM-MOD 22:0-ARGM-NEG 24:0-rel 25:2-ARG1 
nw/wsj/07/wsj_0767.parse 19 7 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 7:0-rel 8:1-ARG1=Topic 
nw/wsj/07/wsj_0767.parse 20 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:2-ARG1=Topic 
nw/wsj/07/wsj_0767.parse 21 2 gold believe-v 29.5-1 Awareness believe.01 null ----- 1:1-ARG0 2:0-rel 3:1-ARG1 
nw/wsj/07/wsj_0767.parse 21 12 gold say-v 37.7-1 IN say.01 null ----- 0:1*13:1-ARG1=Topic 12:0-rel 14:1-ARG0=Agent 
nw/wsj/07/wsj_0767.parse 22 3 gold file-v 9.10 Submitting_documents file.01 null ----- 0:1-ARGM-MNR 2:1*8:1-ARG0=Agent 3:0-rel 4:1-ARG1=Theme 6:1-ARG3 8:2-ARG4 
nw/wsj/07/wsj_0767.parse 22 9 gold accuse-v 81 Judgement_communication accuse.01 1 ----- 2:1*8:1-ARG0=Agent 9:0-rel 10:1-ARG1=Theme 11:1-ARG2=Attribute 
nw/wsj/07/wsj_0767.parse 24 13 gold claim-v 37.7-1 Statement claim.01 null ----- 4:1*12:1-ARG0=Agent;Speaker 13:0-rel 14:1-ARG1=Topic;Message 
nw/wsj/07/wsj_0767.parse 25 1 gold assemble-v 47.5.2 Gathering_up assemble.02 2 ----- 0:1*14:1-ARG0=Agent;Agent 1:0-rel 2:3-ARG1=Theme;Individuals 14:2-ARGM-PRP 
nw/wsj/07/wsj_0767.parse 25 16 gold try-v 61 Attempt try.01 null ----- 0:1*14:1*17:1-ARG0=Agent 16:0-rel 17:2-ARG1=Theme 
nw/wsj/07/wsj_0767.parse 26 3 gold make-v 26.1-1 Cause_change make.01 null ----- 1:1-ARG0=Agent 3:0-rel 4:1-ARG1=Product 5:1-ARG2=Material 
nw/wsj/07/wsj_0767.parse 26 14 gold say-v 37.7-1 IN say.01 null ----- 0:1*15:1-ARG1=Topic 14:0-rel 16:3-ARG0=Agent 
nw/wsj/07/wsj_0767.parse 26 23 gold say-v 37.7-1 IN say.01 null ----- 16:2*21:1*22:1-ARG0=Agent 23:0-rel 24:1-ARG1=Topic 16:2*21:1-LINK-SLC 
nw/wsj/07/wsj_0767.parse 26 26 gold expect-v 62 IN expect.01 null ----- 25:1-ARG0=Experiencer 26:0-rel 27:2-ARG1=Theme 
nw/wsj/07/wsj_0767.parse 26 34 gold deal-v 83 Resolve_problem deal.01 null ----- 27:1*33:1-ARG0=Agent 34:0-rel 35:1-ARG1=Theme 27:1*33:1-LINK-PRO 
nw/wsj/07/wsj_0767.parse 27 6 gold contribute-v 13.2-1-1 NF contribute.01 null ----- 0:2-ARG0=Agent 5:1-ARGM-DIS 6:0-rel 7:1-ARG2=Recipient 
