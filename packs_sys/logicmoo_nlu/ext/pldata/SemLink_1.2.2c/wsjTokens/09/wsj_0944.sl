nw/wsj/09/wsj_0944.parse 0 14 gold tell-v 37.2-1 IN tell.01 null ----- 7:1*13:1-ARG0=Agent 14:0-rel 15:1-ARG2=Recipient 16:2-ARG1=Topic 7:1*13:1-LINK-PRO 
nw/wsj/09/wsj_0944.parse 0 19 gold write-v 25.2 Text_creation write.01 null ----- 16:1*23:1-ARGM-MNR 17:1-ARG0=Agent;Author 18:0-ARGM-MOD 19:0-rel 20:1-ARG1=Theme;Text 
nw/wsj/09/wsj_0944.parse 1 8 gold receive-v 13.5.2 Receiving receive.01 null ----- 4:0*9:1-ARG1=Theme;Theme 6:1-ARG0=Agent;Donor 8:0-rel 10:2-ARG2=Source 
nw/wsj/09/wsj_0944.parse 3 5 gold die-v 48.2 Death die.01 null ----- 0:1*3:1*4:1-ARG1=Patient;Protagonist 5:0-rel 6:1-ARGM-TMP 0:1*3:1-LINK-SLC 
nw/wsj/09/wsj_0944.parse 3 9 gold bequeath-v 13.3 Giving bequeath.01 null ----- 0:2-ARG0=Agent;Donor 9:0-rel 10:2-ARG1=Theme;Theme 15:1-ARG2=Goal;Recipient 
nw/wsj/09/wsj_0944.parse 3 21 gold license-v 101 NF license.01 1 ----- 16:1*19:1*20:1-ARG0=Agent 21:0-rel 22:1-ARG1=Theme 23:1-ARG2 16:1*19:1-LINK-SLC 
nw/wsj/09/wsj_0944.parse 4 10 gold restrain-v 76 NF restrain.01 2 ----- 5:1*8:1-ARG0=Cause 10:0-rel 11:1*16:1-ARG1=Patient 15:1-ARG2=Goal 24:1-ARGM-CAU 5:1*8:1-LINK-PRO 
nw/wsj/09/wsj_0944.parse 5 12 gold permit-v 64 Grant_permission permit.01 1 ----- 6:1-ARG0=Agent 12:0-rel 13:2-ARG1=Theme 
nw/wsj/09/wsj_0944.parse 6 5 gold deny-v 29.5-1 Statement deny.01 null ----- 0:2-ARG0=Agent;Speaker 5:0-rel 6:1-ARG1=Predicate;Message 8:1-ARGM-CAU 
nw/wsj/09/wsj_0944.parse 6 15 gold fail-v 75-1-1 NF fail.01 null ----- 12:1*16:1-ARG1 15:0-rel 16:2-ARG2 
nw/wsj/09/wsj_0944.parse 6 30 gold call-v 29.3 IN call.01 null ----- 26:1*28:1*31:1-ARG1=Theme 29:1-ARG0=Agent 30:0-rel 32:1-ARG2=Result 26:1*28:1-LINK-SLC 
nw/wsj/09/wsj_0944.parse 7 7 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-ADV 5:1-ARG0=Agent 7:0-rel 8:1-ARG1=Topic 
nw/wsj/09/wsj_0944.parse 7 12 gold write-v 25.2 Text_creation write.01 null ----- 9:1-ARG0=Agent;Author 12:0-rel 14:2-ARG1=Theme;Text 
nw/wsj/09/wsj_0944.parse 8 5 gold believe-v 29.9-2 Awareness believe.01 null ----- 3:1-ARG0 5:0-rel 0:2-ARG1-DSP 
nw/wsj/09/wsj_0944.parse 8 10 gold justify-v 37.1.1 NF justify.01 1 ----- 0:1*11:1-ARG1=Topic 10:0-rel 12:1-ARGM-PRP 
nw/wsj/09/wsj_0944.parse 8 16 gold prove-v 29.4-1-1 Evidence prove.01 null ----- 14:1-ARG0 16:0-rel 17:3-ARG1=Theme 
nw/wsj/09/wsj_0944.parse 8 22 gold assert-v 29.5-2 Statement assert.03 1 ----- 17:1*18:1*23:1-ARG1=Theme;Addressee 19:1-ARG0=Agent;Speaker 22:0-rel 24:1-ARG3=Predicate;Message 17:1*18:1-LINK-SLC 
nw/wsj/09/wsj_0944.parse 9 9 gold write-v 37.7 Statement write.01 null ----- 7:1-ARG0 9:0-rel 0:1-ARG1-DSP 
nw/wsj/09/wsj_0944.parse 9 16 gold require-v 60-1 NF require.01 null ----- 1:1*18:1-ARG2 13:0-ARGM-MOD 14:0-ARGM-NEG 16:0-rel 17:1-ARGM-ADV 19:2-ARG1 
nw/wsj/09/wsj_0944.parse 10 9 gold outweigh-v 90-1 NF outweigh.01 2 ----- 1:1-ARGM-LOC 5:2-ARG0=Theme 9:0-rel 10:2-ARG1=Co-Theme 
nw/wsj/09/wsj_0944.parse 11 3 gold feel-v 30.1-1 Perception_experience feel.01 null ----- 0:0-ARGM-DIS 1:1-ARG0 3:0-rel 4:1-ARG1 
nw/wsj/09/wsj_0944.parse 11 4 gold constrain-v 76 Hindering constrain.01 1 ----- 1:1-ARG1=Patient;Protagonist/Action 4:0-rel 5:1-ARG0=Cause;Hindrance 
nw/wsj/09/wsj_0944.parse 11 14 gold forbid-v 67 Prohibiting forbid.01 1 ----- 6:1,9:1-ARG0=Agent;Principle 14:0-rel 15:2-ARG2 20:1-ARG1=Theme;State_of_affairs 
nw/wsj/09/wsj_0944.parse 11 21 gold quote-v 37.1.1-1 NF quote.01 null ----- 15:2-ARG0=Agent 21:0-rel 22:1-ARG1=Source 
nw/wsj/09/wsj_0944.parse 12 23 gold enliven-v 31.1 NF enliven.01 null ----- 15:1*22:1-ARG0=Stimulus 23:0-rel 24:1-ARG1=Experiencer 15:1*22:1-LINK-PRO 
nw/wsj/09/wsj_0944.parse 12 30 gold prove-v 78-1-1 Evidence prove.01 null ----- 29:1-ARG0 30:0-rel 31:2-ARG1 
nw/wsj/09/wsj_0944.parse 13 4 gold create-v 27 Creating create.01 null ----- 0:1-ARGM-TMP 1:1-ARG0 4:0-rel 5:2-ARG1 
nw/wsj/09/wsj_0944.parse 14 3 gold conclude-v 97.2 Coming_to_believe conclude.01 null ----- 0:1-ARG0=Agent 2:1-ARGM-MNR 3:0-rel 4:1-ARG1=Theme 
nw/wsj/09/wsj_0944.parse 14 26 gold prove-v 78-1-1 Evidence prove.01 null ----- 24:1-ARG0 26:0-rel 27:1-ARG1 
nw/wsj/09/wsj_0944.parse 14 30 gold fail-v 74-2 IN fail.01 null ----- 5:2-ARG1 19:1-ARGM-CAU 30:0-rel 31:1-ARG2 
nw/wsj/09/wsj_0944.parse 15 7 gold say-v 37.7-1 IN say.01 null ----- 5:1-ARG0 7:0-rel 0:1-ARG1-DSP 
nw/wsj/09/wsj_0944.parse 15 11 gold lay-v 9.2 Placing lay.01 null ----- 0:0-ARGM-DIS 1:1-ARG1=Theme;Theme 11:0-rel 12:2-ARG2=Destination;Goal 
nw/wsj/09/wsj_0944.parse 17 14 gold join-v 22.1-2-1 Cause_to_amalgamate join.01 null ----- 9:1*13:1-ARG1=Patient;Part_1 14:0-rel 16:1-ARG0=Agent;Agent 
nw/wsj/09/wsj_0944.parse 17 21 gold agree-v 36.1-1 IN agree.01 null ----- 0:1-ARGM-LOC 9:1*13:1*15:1-ARG0=Agent 13:2-ARG2=Co-Agent 21:0-rel 22:1-ARG1=Theme 
nw/wsj/09/wsj_0944.parse 17 24 gold deny-v 29.5-1 Statement deny.01 null ----- 23:1-ARG0=Agent;Speaker 24:0-rel 25:1-ARG1=Predicate;Message 
nw/wsj/09/wsj_0944.parse 17 30 gold doubt-v 29.5-1 Certainty doubt.01 1 ----- 23:1-ARG0 29:0-ARGM-NEG 30:0-rel 31:1-ARG1 
nw/wsj/09/wsj_0944.parse 17 55 gold outweigh-v 90-1 NF outweigh.01 2 ----- 49:2-ARG0=Theme 54:0-ARGM-MOD 55:0-rel 56:2-ARG1=Co-Theme 
nw/wsj/09/wsj_0944.parse 18 2 gold conclude-v 97.2 Coming_to_believe conclude.01 null ----- 1:1-ARG0=Agent 2:0-rel 12:1-ARG1=Theme 
nw/wsj/09/wsj_0944.parse 18 8 gold write-v 37.7 Statement write.01 null ----- 5:1-ARG0 8:0-rel 0:1-ARG1-DSP 
nw/wsj/09/wsj_0944.parse 19 4 gold file-v 9.10 Submitting_documents file.01 null ----- 1:1*5:1-ARG1=Theme 4:0-rel 6:1-ARGM-TMP 
nw/wsj/09/wsj_0944.parse 19 10 gold say-v 37.7-1 IN say.01 null ----- 9:1-ARG0 10:0-rel 0:3-ARG1-DSP 
nw/wsj/09/wsj_0944.parse 19 23 gold suppress-v 42.3 NF suppress.01 null ----- 0:2-ARGM-ADV 15:2*24:1-ARG1 20:0-ARGM-MOD 23:0-rel 
nw/wsj/09/wsj_0944.parse 21 19 gold live-v 47.1-1 NF live.01 null ----- 19:0-rel 20:0-ARG0 
nw/wsj/09/wsj_0944.parse 21 26 gold have-v 100 IN have.03 null ----- 18:2-ARG0=Pivot 26:0-rel 27:2-ARG1=Theme 
nw/wsj/09/wsj_0944.parse 21 33 gold apply-v 105 Using apply.02 null ----- 27:1*29:1*30:1-ARG1=Theme 32:0-ARGM-NEG 33:0-rel 34:1-ARG2=Predicate 27:1*29:1-LINK-SLC 
nw/wsj/09/wsj_0944.parse 22 2 gold think-v 29.9-2 IN think.01 null ----- 1:1-ARG0 2:0-rel 3:1-ARG1 
nw/wsj/09/wsj_0944.parse 22 19 gold haunt-v 31.1 NF haunt.01 1 ----- 4:1*17:1-ARG0=Stimulus 19:0-rel 20:1-ARG1=Experiencer 
nw/wsj/09/wsj_0944.parse 24 14 gold recognize-v 29.9-1-1 NF recognize.02 null ----- 0:2-ARG0 14:0-rel 15:1-ARG1 
nw/wsj/09/wsj_0944.parse 25 12 gold grant-v 13.3 NF grant.01 null ----- 1:1*3:1*13:1-ARG1 4:1-ARG0 8:0-ARGM-MOD 9:1-ARGM-MNR 12:0-rel 1:1*3:1-LINK-SLC 
nw/wsj/09/wsj_0944.parse 26 7 gold observe-v 29.5-2 Statement observe.02 null ----- 5:1-ARG0 7:0-rel 0:3-ARG1-DSP 
nw/wsj/09/wsj_0944.parse 26 19 gold involve-v 86.2-1 NF involve.01 null ----- 0:2-ARG2 11:0-ARGM-MOD 18:1-ARGM-DIS 19:0-rel 20:1-ARG1 
nw/wsj/09/wsj_0944.parse 27 20 gold use-v 105 IN use.01 null ----- 0:1-ARGM-DIS 15:1-ARG0 19:1-ARGM-MNR 20:0-rel 21:2-ARG1 
nw/wsj/09/wsj_0944.parse 29 10 gold claim-v 37.7-1 Statement claim.01 null ----- 0:1-ARGM-TMP 2:1*4:1-ARG0=Agent;Speaker 3:1-ARGM-LOC 9:0-ARGM-NEG 10:0-rel 11:1-ARG1=Topic;Message 
nw/wsj/09/wsj_0944.parse 30 4 gold agree-v 36.1-1 IN agree.01 null ----- 0:0-ARGM-DIS 1:1-ARGM-DIS 3:1-ARG0=Agent 4:0-rel 5:1-ARG1=Theme 
nw/wsj/09/wsj_0944.parse 30 8 gold impose-v 63 NF impose.01 null ----- 6:1-ARG0=Agent 7:0-ARGM-MOD 8:0-rel 9:1-ARG1=Theme 10:1-ARG2 
nw/wsj/09/wsj_0944.parse 30 25 gold donate-v 13.2-1-1 Giving donate.01 null ----- 14:1*24:1-ARG1=Theme;Theme 25:0-rel 29:1-ARG2=Recipient;Recipient 14:1*24:1-LINK-PRO 
nw/wsj/09/wsj_0944.parse 30 27 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 14:1*24:1*28:1-ARG1=Theme;Goods 27:0-rel 29:1-ARG2=Recipient 14:1*24:1-LINK-PRO 
nw/wsj/09/wsj_0944.parse 31 12 gold find-v 13.5.1 IN find.01 null ----- 0:0-ARGM-DIS 1:1-ARGM-LOC 10:1-ARG0 12:0-rel 13:2-ARG1 17:2-ARGM-LOC 
nw/wsj/09/wsj_0944.parse 32 4 gold gain-v 13.5.1-1 Getting gain.02 null ----- 1:1*10:1-ARGM-TMP 2:1-ARG0=Agent;Recipient 4:0-rel 5:2-ARG1=Theme;Theme 
nw/wsj/09/wsj_0944.parse 32 16 gold permit-v 64 Permitting permit.01 1 ----- 0:0-ARGM-DIS 1:2-ARGM-TMP 12:1*17:1-ARG2 13:0-ARGM-MOD 14:0-ARGM-NEG 16:0-rel 18:2-ARG1 
nw/wsj/09/wsj_0944.parse 32 20 gold deny-v 29.5-1 Statement deny.01 null ----- 12:1*17:1*18:1-ARG0=Agent;Speaker 20:0-rel 21:1-ARG2 22:2-ARG1=Predicate;Message 
nw/wsj/09/wsj_0944.parse 32 27 gold help-v 72-1 Assistance help.01 null ----- 24:1*25:1*26:1*28:1-ARG0=Agent;Helper 27:0-rel 28:2-ARG1=Theme;Goal/Focal_entity 24:1*25:1-LINK-SLC 
nw/wsj/09/wsj_0944.parse 32 30 gold establish-v 78-1-1 NF establish.01 null ----- 24:1*25:1*26:1*28:1-ARG0 30:0-rel 31:1-ARG1 24:1*25:1-LINK-SLC 
nw/wsj/09/wsj_0944.parse 33 4 gold understand-v 87.2-1 Awareness understand.01 null ----- 0:1-ARG0 4:0-rel 5:2-ARG1 
nw/wsj/09/wsj_0944.parse 34 9 gold have-v 100 IN have.03 null ----- 0:1*7:1-ARG0=Pivot 9:0-rel 10:1-ARG1=Theme 
nw/wsj/09/wsj_0944.parse 35 11 gold make-v 26.1-1 Intentionally_create make.01 null ----- 6:1*8:1*9:1-ARG0=Agent;Creator 11:0-rel 12:1-ARG1=Product;Created_entity 6:1*8:1-LINK-SLC 
nw/wsj/09/wsj_0944.parse 36 9 gold tear-v 23.2 IN tear.01 1 ----- 4:1*8:1-ARG1 9:0-rel 11:1-ARG2 
nw/wsj/09/wsj_0944.parse 37 3 gold quote-v 37.1.1-1 NF quote.01 null ----- 1:1-ARG0=Agent 3:0-rel 5:4-ARG1=Source 
nw/wsj/09/wsj_0944.parse 37 12 gold copyright-v 101 NF copyright.01 null ----- 12:0-rel 13:0-ARG1=Theme 
nw/wsj/09/wsj_0944.parse 37 19 gold have-v 100 IN have.03 null ----- 16:1-ARG0=Pivot 19:0-rel 20:1-ARG1=Theme 
nw/wsj/09/wsj_0944.parse 38 14 gold understand-v 87.2-1 Awareness understand.01 null ----- 13:1-ARG0 14:0-rel 11:2-ARG1-DSP 
nw/wsj/09/wsj_0944.parse 38 21 gold mean-v 29.5-1 NF mean.01 null ----- 19:1-ARG0=Agent 21:0-rel 22:2-ARG1=Theme 
nw/wsj/09/wsj_0944.parse 40 4 gold hand-v 11.1-1 Giving hand.01 1 ----- 0:1*5:1-ARG1=Theme 4:0-rel 6:1-ARGM-DIR 7:1-ARGM-TMP 9:1-ARG0=Agent 0:1*5:1-LINK-PSV 
nw/wsj/09/wsj_0944.parse 41 10 gold write-v 26.7-2-1 Text_creation write.01 null ----- 0:2-ARG0 10:0-rel 11:2-ARG1 
nw/wsj/09/wsj_0944.parse 42 4 gold base-v 97.1 NF base.02 null ----- 0:1*5:1-ARG1=Theme 3:1-ARGM-ADV 4:0-rel 6:1-ARG2=Source 
nw/wsj/09/wsj_0944.parse 42 12 gold tape-v 25.4 NF tape.02 2 ----- 7:1*8:1*13:1-ARG1=Theme 9:1-ARG0=Agent 12:0-rel 14:1-ARGM-COM 7:1*8:1-LINK-SLC 
nw/wsj/09/wsj_0944.parse 42 23 gold serve-v 104 NF serve.01 null ----- 18:1*20:1*21:1-ARG0 23:0-rel 24:1-ARG1 18:1*20:1-LINK-SLC 
nw/wsj/09/wsj_0944.parse 43 3 gold bring-v 11.3-1 Bringing bring.01 null ----- 0:1-ARG0=Instrument 2:1-ARGM-DIS 3:0-rel 4:1-ARG1=Theme 7:1-ARG2=Destination 
nw/wsj/09/wsj_0944.parse 44 10 gold prove-v 97.2 Reasoning prove.01 null ----- 5:1*8:1-ARG0 7:0-ARGM-MOD 10:0-rel 11:1-ARG1 
nw/wsj/09/wsj_0944.parse 44 21 gold contend-v 29.5-2 Statement contend.01 1 ----- 14:1-ARGM-ADV 19:1-ARG0=Agent;Speaker 21:0-rel 22:1-ARG1=Theme;Addressee 
nw/wsj/09/wsj_0944.parse 45 5 gold find-v 84 IN find.01 null ----- 0:1*6:1-ARG1 2:0-ARGM-MOD 3:0-ARGM-NEG 5:0-rel 7:1-ARGM-LOC 
nw/wsj/09/wsj_0944.parse 45 38 gold assume-v 93 Adopt_selection assume.01 null ----- 29:1-ARGM-PNC 37:1-ARG0 38:0-rel 39:1-ARG1 
nw/wsj/09/wsj_0944.parse 46 15 gold condemn-v 33 Judgment_communication condemn.01 1 ----- 7:1*13:1-ARG0 15:0-rel 16:1-ARG1 7:1*13:1-LINK-PRO 
nw/wsj/09/wsj_0944.parse 47 32 gold find-v 29.4-1-1-2 IN find.02 null ----- 30:1-ARG0=Agent 32:0-rel 33:1-ARG1=Theme 
nw/wsj/09/wsj_0944.parse 48 8 gold observe-v 37.7-1 Statement observe.02 null ----- 0:1-ARGM-LOC 5:1-ARG0 8:0-rel 9:1-ARG1 
nw/wsj/09/wsj_0944.parse 48 13 gold use-v 105 IN use.01 null ----- 10:1*24:1-ARGM-TMP 11:1*17:1-ARG0 13:0-rel 14:1-ARG1 16:1-ARG2 
nw/wsj/09/wsj_0944.parse 48 18 gold report-v 37.7-1 Statement report.01 null ----- 11:1*17:1-ARG0 18:0-rel 19:2-ARG1 
nw/wsj/09/wsj_0944.parse 48 22 gold say-v 37.7-1 IN say.01 null ----- 19:1*23:1-ARG1 20:1-ARG0 22:0-rel 
nw/wsj/09/wsj_0944.parse 48 28 gold assume-v 93 Adopt_selection assume.01 null ----- 10:2-ARGM-TMP 26:1-ARG0 28:0-rel 29:1-ARG1 
nw/wsj/09/wsj_0944.parse 48 42 gold purge-v 10.6 Removing purge.01 1 ----- 40:1*43:1-ARG1 42:0-rel 44:1-ARG2 40:1*43:1-LINK-PSV 
nw/wsj/09/wsj_0944.parse 48 51 gold know-v 29.5-1 IN know.01 1 ----- 50:1-ARG0 51:0-rel 
nw/wsj/09/wsj_0944.parse 49 2 gold have-v 100 IN have.03 null ----- 1:1-ARG0=Pivot 2:0-rel 3:2*10:2-ARG1=Theme 
nw/wsj/09/wsj_0944.parse 50 6 gold write-v 37.7 Statement write.01 null ----- 0:1-ARG0 3:1-ARGM-DIS 6:0-rel 7:1-ARG1 
nw/wsj/09/wsj_0944.parse 50 20 gold live-v 47.1-1 NF live.01 null ----- 14:1*17:1*18:1-ARG0 19:1-ARGM-TMP 20:0-rel 14:1*17:1-LINK-SLC 
nw/wsj/09/wsj_0944.parse 51 3 gold appear-v 48.1.1 Coming_to_be appear.01 null ----- 0:1-ARG1=Theme;Entity 3:0-rel 4:1-ARGM-LOC 
nw/wsj/09/wsj_0944.parse 53 9 gold show-v 78-1-1 IN show.01 null ----- 0:0-ARGM-DIS 2:1-ARGM-ADV 7:1-ARG0 9:0-rel 10:1-ARG1 
nw/wsj/09/wsj_0944.parse 55 5 gold appear-v 48.1.1 Coming_to_be appear.01 null ----- 0:1-ARGM-TMP 2:1-ARG1=Theme;Entity 5:0-rel 6:1-ARGM-LOC 
nw/wsj/09/wsj_0944.parse 56 3 gold contend-v 29.5-2 Statement contend.01 1 ----- 0:1-ARG0=Agent;Speaker 3:0-rel 4:2-ARG1=Theme;Addressee 
nw/wsj/09/wsj_0944.parse 57 9 gold write-v 37.7 Statement write.01 null ----- 0:1-ARG0 9:0-rel 10:1-ARG1 
nw/wsj/09/wsj_0944.parse 58 11 gold have-v 100 IN have.03 null ----- 8:1*9:1*10:1-ARG0=Pivot 11:0-rel 13:1-ARGM-ADV 19:2-ARG1=Theme 8:1*9:1-LINK-SLC 
nw/wsj/09/wsj_0944.parse 58 24 gold make-v 29.3 Causation make.02 null ----- 23:1-ARG0=Agent 24:0-rel 25:2-ARG1=Theme 
nw/wsj/09/wsj_0944.parse 59 3 gold write-v 37.7 Statement write.01 null ----- 1:1-ARG0 3:0-rel 4:1-ARGM-LOC 
nw/wsj/09/wsj_0944.parse 60 8 gold call-v 29.3 IN call.01 null ----- 4:1*6:1*7:1-ARG0=Agent 8:0-rel 9:1*10:1-ARG1=Theme 10:2-ARG2=Result 4:1*6:1-LINK-SLC 
nw/wsj/09/wsj_0944.parse 60 16 gold reveal-v 78-1-1 Reveal_secret reveal.01 null ----- 13:1-ARG0 16:0-rel 17:1-ARG1 
nw/wsj/09/wsj_0944.parse 61 2 gold know-v 29.5-1 IN know.01 1 ----- 0:1-ARG0 1:1-ARGM-TMP 2:0-rel 3:1-ARG1 
nw/wsj/09/wsj_0944.parse 62 13 gold say-v 37.7-1 IN say.01 null ----- 1:2*14:1-ARG1 10:1-ARG0 13:0-rel 
nw/wsj/09/wsj_0944.parse 64 10 gold give-v 13.1-1 Giving give.01 null ----- 1:1*4:1*11:1-ARG1=Theme;Theme 5:1-ARG0=Agent;Donor 10:0-rel 12:1-ARG2=Recipient;Recipient 1:1*4:1-LINK-SLC 
nw/wsj/09/wsj_0944.parse 64 24 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 null ----- 21:1-ARG0 22:0-ARGM-MOD 23:1-ARGM-MNR 24:0-rel 25:2-ARG1 
nw/wsj/09/wsj_0944.parse 65 15 gold distort-v 26.5 NF distort.01 2 ----- 12:1-ARG0=Agent 14:1-ARGM-MNR 15:0-rel 16:2-ARG1=Material 
nw/wsj/09/wsj_0944.parse 65 20 gold say-v 37.7-1 IN say.01 null ----- 16:1*21:1-ARG1 17:1-ARG0 20:0-rel 
nw/wsj/09/wsj_0944.parse 65 25 gold assert-v 29.5-2 Statement assert.03 2 ----- 23:1-ARG0=Agent;Speaker 25:0-rel 26:1-ARG1=Theme;Addressee 
nw/wsj/09/wsj_0944.parse 66 20 gold lose-v 13.2 NF lose.02 null ----- 7:2-ARG0=Agent 20:0-rel 21:2-ARG1=Theme 
nw/wsj/09/wsj_0944.parse 69 2 gold write-v 25.2 Text_creation write.01 null ----- 1:1-ARG0 2:0-rel 3:1-ARG1 
