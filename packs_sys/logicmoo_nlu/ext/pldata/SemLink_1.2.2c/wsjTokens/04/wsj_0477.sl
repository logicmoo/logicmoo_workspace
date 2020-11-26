nw/wsj/04/wsj_0477.parse 1 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/04/wsj_0477.parse 1 4 gold win-v 13.5.1 NF win.01 null ----- 3:1-ARG0=Agent 4:0-rel 5:2-ARG1=Theme 
nw/wsj/04/wsj_0477.parse 1 10 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 3:1*8:1-ARG0=Agent;Buyer 10:0-rel 11:3-ARG1=Theme;Goods 3:1*8:1-LINK-PRO 
nw/wsj/04/wsj_0477.parse 2 7 gold announce-v 37.7-1 Statement announce.01 null ----- 6:1-ARG0=Agent;Speaker 7:0-rel 8:1-ARG1=Topic;Message 
nw/wsj/04/wsj_0477.parse 2 11 gold prepare-v 55.5-1 Activity_prepare prepare.02 null ----- 9:1*12:1-ARG0=Agent 11:0-rel 12:2-ARG2 
nw/wsj/04/wsj_0477.parse 3 8 gold say-v 37.7-1 IN say.01 null ----- 0:3-ARG0=Agent 8:0-rel 9:1-ARG1=Topic 
nw/wsj/04/wsj_0477.parse 5 2 gold claim-v 37.7-1 Statement claim.01 null ----- 0:1-ARG0=Agent;Speaker 2:0-rel 3:2,11:1-ARG1=Topic;Message 
nw/wsj/04/wsj_0477.parse 7 1 gold lead-v 59 Causation lead.03 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG2=Result 
nw/wsj/04/wsj_0477.parse 7 17 gold expect-v 62 IN expect.01 null ----- 11:1*14:1,15:1,18:2-ARG1=Theme 17:0-rel 11:1*14:1-LINK-SLC 
nw/wsj/04/wsj_0477.parse 7 22 gold launch-v 55.5-1 IN launch.01 null ----- 11:1*14:1*15:1*18:1-ARG1 21:1-ARGM-MNR 22:0-rel 23:2-ARGM-TMP 27:1-ARGM-TMP 11:1*14:1-LINK-SLC 
nw/wsj/04/wsj_0477.parse 8 28 gold get-v 26.6.2 IN get.03 null ----- 9:2*26:1-ARG1=Patient 28:0-rel 29:1-ARG2=Goal 9:2*26:1-LINK-PRO 
nw/wsj/04/wsj_0477.parse 8 36 gold consider-v 29.9-1-1-1 Categorization consider.01 null ----- 33:1-ARGM-TMP 36:0-rel 37:2-ARG1=Theme;Item 
nw/wsj/04/wsj_0477.parse 10 7 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 0:1*5:1-ARG0=Agent;Recipient 7:0-rel 8:2-ARG1=Theme;Theme 
nw/wsj/04/wsj_0477.parse 11 2 gold hope-v 32.2-1 Desiring hope.01 1 ----- 0:1*3:1-ARG0=Pivot 2:0-rel 3:2-ARG1=Theme 12:1-ARGM-TMP 
nw/wsj/04/wsj_0477.parse 11 5 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:1*3:1-ARG0=Agent;Buyer 5:0-rel 6:2-ARG1=Theme;Goods 
nw/wsj/04/wsj_0477.parse 13 3 gold say-v 37.7-1 IN say.01 null ----- 2:1-ARG0=Agent 3:0-rel 0:2-ARG1-DSP=Topic 
nw/wsj/04/wsj_0477.parse 13 10 gold shock-v 31.1 Experiencer_obj shock.01 1 ----- 0:1-ARGM-TMP 7:1*18:1-ARG0=Stimulus;Stimulus 10:0-rel 11:2-ARG1=Experiencer;Experiencer 17:1-ARG2 
nw/wsj/04/wsj_0477.parse 13 19 gold indicate-v 78-1 Communication indicate.01 null ----- 7:1*18:1-ARG0=Cause;Communicator 19:0-rel 20:1-ARG1=Topic;Message 
nw/wsj/04/wsj_0477.parse 13 22 gold want-v 32.1-1-1 Desiring want.01 null ----- 21:1-ARG0=Pivot;Experiencer 22:0-rel 23:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/04/wsj_0477.parse 14 7 gold say-v 37.7-1 IN say.01 null ----- 1:2*8:1-ARG1=Topic 7:0-rel 9:1-ARG0=Agent 
nw/wsj/04/wsj_0477.parse 15 5 gold think-v 29.9-2 IN think.01 null ----- 4:1-ARG0 5:0-rel 6:1-ARG1 
nw/wsj/04/wsj_0477.parse 15 9 gold fit-v 54.3 NF fit.01 1 ----- 1:1*3:1*7:1-ARG1=Value 8:0-ARGM-MOD 9:0-rel 10:1-ARGM-LOC 1:1*3:1-LINK-SLC 
nw/wsj/04/wsj_0477.parse 16 8 gold comment-v 37.11-1-1 Statement comment.01 1 ----- 1:1*6:1-ARG0=Agent;Speaker 8:0-rel 9:1-ARGM-MNR 12:1-ARG1=Topic;Message 
nw/wsj/04/wsj_0477.parse 16 17 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-ADV 16:1-ARG0=Agent 17:0-rel 18:1-ARG1=Topic 
nw/wsj/04/wsj_0477.parse 16 21 gold hold-v 15.1-1 Manipulation hold.01 null ----- 19:1-ARG0=Agent;Agent 20:1-ARGM-TMP 21:0-rel 22:4-ARG1=Theme;Entity 
nw/wsj/04/wsj_0477.parse 16 47 gold announce-v 37.7-1 Statement announce.01 null ----- 37:1*41:1*48:1-ARG1=Topic;Message 42:2-ARG0=Agent;Speaker 46:1-ARGM-TMP 47:0-rel 37:1*41:1-LINK-SLC 
nw/wsj/04/wsj_0477.parse 17 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/04/wsj_0477.parse 17 6 gold believe-v 29.5-1 Awareness believe.01 null ----- 5:1-ARG0 6:0-rel 7:1-ARG1 
nw/wsj/04/wsj_0477.parse 17 10 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 8:1-ARG0=Agent;Buyer 10:0-rel 11:1-ARGM-TMP 
nw/wsj/04/wsj_0477.parse 17 17 gold move-v 45.6-1 Change_position_on_a_scale move.01 null ----- 15:1*26:1-ARG1 17:0-rel 18:1-ARGM-DIR 26:2-ARG2 32:1-ARGM-LOC 
nw/wsj/04/wsj_0477.parse 18 19 gold announce-v 37.7-1 Statement announce.01 null ----- 14:1*17:1*32:1-ARGM-TMP 18:1-ARG0=Agent;Speaker 19:0-rel 20:1-ARG1=Topic;Message 14:1*17:1-LINK-SLC 
nw/wsj/04/wsj_0477.parse 19 2 gold hold-v 15.1-1 Manipulation hold.01 null ----- 0:1-ARG0=Agent;Agent 1:1-ARGM-DIS 2:0-rel 3:2-ARG1=Theme;Entity 
nw/wsj/04/wsj_0477.parse 19 24 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 8:3*22:1*25:1-ARG1=Theme;Goods 23:1-ARG0=Agent;Buyer 24:0-rel 26:1-ARG3=Asset 31:2-ARGM-TMP 8:3*22:1-LINK-SLC 
nw/wsj/04/wsj_0477.parse 19 34 gold launch-v 55.5-1 IN launch.01 null ----- 33:1-ARG0 34:0-rel 35:1-ARG1 
nw/wsj/04/wsj_0477.parse 20 2 gold hold-v 15.1-1 Manipulation hold.01 null ----- 0:1-ARG0=Agent;Agent 2:0-rel 3:1-ARG1=Theme;Entity 
nw/wsj/04/wsj_0477.parse 21 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARGM-LOC 5:1-ARG1=Topic 
nw/wsj/04/wsj_0477.parse 22 3 gold stress-v 9.9 NF stress.01 null ----- 0:2-ARG0=Agent 3:0-rel 4:3-ARG1=Destination 
nw/wsj/04/wsj_0477.parse 22 8 gold announce-v 37.7-1 Statement announce.01 null ----- 4:1*9:1-ARG1=Topic;Message 7:1-ARGM-TMP 8:0-rel 4:1*9:1-LINK-PSV 
nw/wsj/04/wsj_0477.parse 23 2 gold suggest-v 37.7-1-1 Statement suggest.01 null ----- 0:1*5:1-ARG0=Agent;Speaker 1:1-ARGM-ADV 2:0-rel 4:1-ARGM-MNR 10:1-ARG1=Topic;Message 
nw/wsj/04/wsj_0477.parse 23 6 gold say-v 37.7-1 IN say.01 null ----- 0:1*5:1-ARG0=Agent 6:0-rel 7:1-ARG1=Topic 8:1-ARGM-MNR 
nw/wsj/04/wsj_0477.parse 23 12 gold regret-v 31.2-1 Experiencer_focus regret.01 1 ----- 11:1-ARG0=Experiencer 12:0-rel 13:1-ARG1=Attribute 
nw/wsj/04/wsj_0477.parse 24 5 gold say-v 37.7-1 IN say.01 null ----- 4:1-ARG0=Agent 5:0-rel 0:3-ARG1-DSP=Topic 
nw/wsj/04/wsj_0477.parse 24 13 gold win-v 13.5.1 NF win.01 null ----- 12:1-ARG0=Agent 13:0-rel 14:1-ARG1=Theme 
nw/wsj/04/wsj_0477.parse 24 23 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 null ----- 11:1-ARGM-ADV 18:1-ARG1=Patient;Item 22:0-ARGM-MOD 23:0-rel 24:1-ARGM-MNR 
nw/wsj/04/wsj_0477.parse 25 1 gold displease-v 31.1 Experiencer_obj displease.01 null ----- 0:1-ARG0=Stimulus;Stimulus 1:0-rel 2:3-ARG1=Experiencer;Experiencer 
nw/wsj/04/wsj_0477.parse 25 18 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 2:2*10:1*11:1*17:1-ARG0=Agent;Seller 18:0-rel 19:1-ARG1=Theme;Goods 22:1-ARGM-ADV 2:2*10:1-LINK-SLC 
nw/wsj/04/wsj_0477.parse 26 3 gold lead-v 59 Causation lead.03 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG2=Result 
nw/wsj/04/wsj_0477.parse 26 10 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 7:1-ARG0=Agent;Seller 8:1-ARGM-TMP 9:0-ARGM-MOD 10:0-rel 11:1-ARG2=Recipient 
nw/wsj/04/wsj_0477.parse 27 15 gold count-v 70 Reliance count.03 null ----- 6:2*12:1*13:1-ARG0=Agent;Protagonist 15:0-rel 16:1-ARG1=Theme;Means/Instrument/Intermediary/Benefit/Purpose 18:2-ARG2 6:2*12:1-LINK-SLC 
nw/wsj/04/wsj_0477.parse 27 20 gold help-v 72-1 Assistance help.01 null ----- 17:1*18:1*21:1-ARG0=Agent;Helper 20:0-rel 21:2-ARG1=Theme;Goal/Focal_entity 
nw/wsj/04/wsj_0477.parse 28 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-ARGM-NEG 3:0-rel 4:2-ARG1=Topic 
nw/wsj/04/wsj_0477.parse 28 11 gold support-v 31.2 Taking_sides support.01 null ----- 4:1*12:1-ARG1=Stimulus 9:1-ARG0=Experiencer 10:0-ARGM-MOD 11:0-rel 
nw/wsj/04/wsj_0477.parse 29 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARGM-ADV 3:1-ARG1=Topic 
nw/wsj/04/wsj_0477.parse 29 6 gold boost-v 102 NF boost.01 1 ----- 4:1*19:1-ARG0=Agent 5:0-ARGM-MOD 6:0-rel 7:1-ARG1=Theme 11:1-ARGM-ADV 15:1-ARGM-TMP 19:2-ARGM-PRP 24:2-ARGM-TMP 
nw/wsj/04/wsj_0477.parse 29 28 gold have-v 100 IN have.03 null ----- 27:1-ARG0=Pivot 28:0-rel 29:2-ARG1=Theme 
nw/wsj/04/wsj_0477.parse 30 2 gold intend-v 62 Purpose intend.01 null ----- 0:1*3:1-ARG0 1:1-ARGM-TMP 2:0-rel 3:2-ARG1 
nw/wsj/04/wsj_0477.parse 30 5 gold offer-v 13.3 NF offer.01 null ----- 0:1*3:1-ARG0=Agent 5:0-rel 6:2-ARG2 10:1-ARG1=Theme 
nw/wsj/04/wsj_0477.parse 30 16 gold receive-v 13.5.2 Receiving receive.01 null ----- 11:2*14:1*15:1-ARG0=Agent;Donor 16:0-rel 17:1-ARG1=Theme;Theme 19:1-ARGM-TMP 11:2*14:1-LINK-SLC 
nw/wsj/04/wsj_0477.parse 31 4 gold offer-v 13.3 NF offer.01 null ----- 0:1*2:1-ARG0=Agent 4:0-rel 5:1-ARG2 7:1-ARG1=Theme 
nw/wsj/04/wsj_0477.parse 31 17 gold receive-v 13.5.2 Receiving receive.01 null ----- 8:2,9:1*15:1*16:1-ARG0=Agent;Donor 17:0-rel 18:1-ARG1=Theme;Theme 8:2*15:1-LINK-SLC 
nw/wsj/04/wsj_0477.parse 32 4 gold offer-v 13.3 NF offer.01 null ----- 0:1-ARGM-DIS 2:1*5:1-ARG0=Agent 3:0-ARGM-MOD 4:0-rel 5:2-ARG1=Theme 
nw/wsj/04/wsj_0477.parse 32 7 gold swap-v 13.6-1-1 Exchange swap.01 1 ----- 2:1*5:1-ARG0=Agent 7:0-rel 8:1-ARG1=Theme 11:1-ARG3=Co-Theme 
nw/wsj/04/wsj_0477.parse 33 2 gold hold-v 15.1-1 Manipulation hold.01 null ----- 0:1-ARG0=Agent;Agent 1:1-ARGM-TMP 2:0-rel 3:3-ARG1=Theme;Entity 
nw/wsj/04/wsj_0477.parse 33 19 gold cost-v 54.2 Expensiveness cost.01 null ----- 11:2-ARG1=Theme;Goods 18:0-ARGM-MOD 19:0-rel 20:1-ARG3 21:2-ARG2=Value;Asset 25:1-ARGM-LOC 
nw/wsj/04/wsj_0477.parse 34 2 gold value-v 54.4 Assessing value.01 1 ----- 0:1-ARG0=Agent;Assessor 2:0-rel 3:1-ARG1=Theme;Feature 5:1-ARG2=Value;Value 11:1-ARGM-ADV 
nw/wsj/04/wsj_0477.parse 34 21 gold exchange-v 13.6-1 IN exchange.01 2 ----- 13:3*20:1-ARG0=Agent 21:0-rel 22:1-ARG1=Theme 23:1-ARG3=Co-Theme 25:1-ARGM-TMP 
nw/wsj/04/wsj_0477.parse 34 28 gold expire-v 48.2 Death expire.01 1 ----- 26:1-ARG1=Patient;Protagonist 28:0-rel 
