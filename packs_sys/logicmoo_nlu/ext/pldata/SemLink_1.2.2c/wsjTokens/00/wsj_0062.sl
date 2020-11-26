nw/wsj/00/wsj_0062.parse 0 18 gold get-v 26.6.2 IN get.03 null ----- 0:1-ARGM-TMP 10:2-ARG1=Patient 18:0-rel 19:1-ARG2=Goal 24:2-ARGM-PRD 
nw/wsj/00/wsj_0062.parse 0 25 gold fawn-v 28 NF fawn.01 1 ----- 10:2*24:1-ARG0=Agent 25:0-rel 26:1-ARG1=Patient 28:1-ARGM-LOC 
nw/wsj/00/wsj_0062.parse 0 31 gold offer-v 13.3 NF offer.01 null ----- 10:2*24:1-ARG0=Agent 31:0-rel 32:2-ARG1=Theme 
nw/wsj/00/wsj_0062.parse 2 19 gold find-v 84 IN find.01 null ----- 0:2*17:1-ARG0 19:0,20:1-rel 21:1-ARG1 
nw/wsj/00/wsj_0062.parse 3 1 gold found-v 55.5-1 Intentionally_create found.01 1 ----- 0:1*13:1-ARG1 1:0-rel 3:1-ARG0 
nw/wsj/00/wsj_0062.parse 3 24 gold give-v 13.1-1 Giving give.01 null ----- 22:1-ARG0=Agent;Donor 24:0-rel 25:1-ARG2=Recipient;Recipient 26:2-ARG1=Theme;Theme 
nw/wsj/00/wsj_0062.parse 4 2 gold combine-v 22.1-1-1 Cause_to_amalgamate combine.01 null ----- 0:1-ARG0=Agent;Agent 2:0-rel 3:4-ARG1=Patient;Part_1 
nw/wsj/00/wsj_0062.parse 4 21 gold happen-v 48.3 Event happen.01 null ----- 19:1*20:1-ARG1=Theme;Event 21:0-rel 22:1-ARGM-TMP 
nw/wsj/00/wsj_0062.parse 5 3 gold dump-v 9.3-1-1 NF dump.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Theme 6:1-ARGM-DIR 13:1-ARGM-PRP 
nw/wsj/00/wsj_0062.parse 6 4 gold call-v 29.3 Being_named call.01 null ----- 1:1*5:1-ARG1=Theme;Entity 4:0-rel 6:1-ARG2=Result;Name 1:1*5:1-LINK-PRO 
nw/wsj/00/wsj_0062.parse 8 10 gold alienate-v 31.1 NF alienate.01 1 ----- 0:1-ARGM-MNR 8:1-ARG0=Stimulus 10:0-rel 11:1-ARG1=Experiencer 
nw/wsj/00/wsj_0062.parse 8 18 gold raise-v 9.4 NF raise.01 null ----- 0:1-ARGM-PRD 8:1-ARG0=Agent 18:0-rel 19:2-ARG1=Theme 
nw/wsj/00/wsj_0062.parse 9 15 gold chastise-v 33 Judgment_direct_address chastise.01 null ----- 9:1*16:1-ARG1=Theme;Addressee 15:0-rel 17:1-ARGM-LOC 
nw/wsj/00/wsj_0062.parse 10 13 gold point-v 40.3.1-1 IN point.01 null ----- 12:1-ARG0=Agent 13:0-rel 14:1-ARG2=Recipient 
nw/wsj/00/wsj_0062.parse 10 31 gold say-v 37.7-1 IN say.01 null ----- 26:1*29:1,33:2-ARG1=Topic 30:1-ARG0=Agent 31:0-rel 26:1*29:1-LINK-SLC 
nw/wsj/00/wsj_0062.parse 11 11 gold create-v 27 Creating create.01 null ----- 4:2,6:1*8:1*9:1-ARG0=Cause 11:0-rel 12:1-ARG1=Theme 4:2*8:1-LINK-SLC 
nw/wsj/00/wsj_0062.parse 11 20 gold write-v 37.7 Statement write.01 null ----- 1:2*21:1-ARG1 18:1-ARG0 20:0-rel 
nw/wsj/00/wsj_0062.parse 12 2 gold think-v 29.9-2 IN think.01 null ----- 1:1-ARG0 2:0-rel 3:1-ARG1 
nw/wsj/00/wsj_0062.parse 12 9 gold call-v 29.3 Being_named call.01 null ----- 4:1*10:1-ARG1=Theme;Entity 7:1-ARGM-ADV 9:0-rel 11:2-ARG2=Result;Name 
nw/wsj/00/wsj_0062.parse 13 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/00/wsj_0062.parse 13 17 gold comment-v 37.11-1-1 Statement comment.01 1 ----- 3:1*15:1-ARG0=Agent;Speaker 17:0-rel 3:1*15:1-LINK-PRO 
nw/wsj/00/wsj_0062.parse 14 4 gold make-v 26.1-1 IN make.01 null ----- 0:2*5:1;6:1-ARG1=Product 4:0-rel 11:1-ARGM-TMP 27:2-ARGM-ADV 
nw/wsj/00/wsj_0062.parse 14 18 gold print-v 25.2 Text_creation print.01 null ----- 12:1*14:1*20:1-ARGM-TMP 15:1*19:1-ARG1=Theme;Text 18:0-rel 12:1*14:1-LINK-SLC 
nw/wsj/00/wsj_0062.parse 14 23 gold say-v 37.7-1 IN say.01 null ----- 6:1-ARG1=Topic 22:1-ARG0=Agent 23:0-rel 
nw/wsj/00/wsj_0062.parse 14 28 gold make-v 29.3 Causation make.02 null ----- 0:2;6:1*27:1-ARG0=Agent 28:0-rel 29:2-ARG1=Theme 
nw/wsj/00/wsj_0062.parse 15 1 gold admit-v 29.5-2 IN admit.01 null ----- 0:1-ARG0=Agent 1:0-rel 3:1-ARGM-DIS 5:1-ARG1=Theme 
nw/wsj/00/wsj_0062.parse 16 8 gold have-v 100 IN have.03 null ----- 0:1-ARG0=Pivot 3:1-ARGM-ADV 7:0-ARGM-NEG 8:0-rel 9:1-ARG1=Theme 18:1-ARGM-ADV 
nw/wsj/00/wsj_0062.parse 16 13 gold advertise-v 35.2 NF advertise.01 1 ----- 0:1*11:1-ARG0=Agent 13:0-rel 14:1-ARGM-LOC 0:1*11:1-LINK-PRO 
nw/wsj/00/wsj_0062.parse 17 9 gold risk-v 94-1 Daring risk.01 2 ----- 5:1*10:1-ARG0=Agent;Agent 8:0-ARGM-MOD 9:0-rel 10:2-ARG1=Theme;Action 
nw/wsj/00/wsj_0062.parse 18 3 gold need-v 32.1-1-1 Needing need.01 null ----- 1:1-ARG0=Pivot 2:1-ARGM-ADV 3:0-rel 4:3-ARG1=Theme 16:1-ARGM-ADV 
nw/wsj/00/wsj_0062.parse 18 22 gold make-v 29.3 Causation make.02 null ----- 17:1*20:1-ARG0=Agent 19:0-ARGM-MOD 22:0-rel 23:2-ARG1=Theme 
nw/wsj/00/wsj_0062.parse 18 29 gold say-v 37.7-1 IN say.01 null ----- 1:2*30:1-ARG1=Topic 29:0-rel 31:2-ARG0=Agent 
nw/wsj/00/wsj_0062.parse 19 8 gold require-v 103 NF require.01 null ----- 1:2-ARG0=Pivot 6:1-ARGM-ADV 8:0-rel 9:1-ARG1=Theme 
nw/wsj/00/wsj_0062.parse 19 11 gold have-v 100 IN have.03 null ----- 10:1-ARG0=Pivot 11:0-rel 12:2-ARG1=Theme 
nw/wsj/00/wsj_0062.parse 19 25 gold add-v 37.7 NF add.01 null ----- 24:1-ARG0=Agent 25:0-rel 26:1-ARG1=Topic 
nw/wsj/00/wsj_0062.parse 20 44 gold advertise-v 35.2 NF advertise.01 null ----- 35:1*36:1*37:1*43:1-ARG0=Agent 44:0-rel 35:1*36:1-LINK-SLC 
nw/wsj/00/wsj_0062.parse 20 49 gold rely-v 70 Reliance rely.01 1 ----- 46:0-ARGM-MOD 47:0-ARGM-NEG 49:0-rel 50:1-ARG1=Theme;Means/Instrument/Intermediary/Benefit/Purpose 52:2-ARG2 
nw/wsj/00/wsj_0062.parse 20 54 gold support-v 31.2 Taking_sides support.01 null ----- 35:1*36:1*37:1*52:1-ARG0=Experiencer 54:0-rel 55:1-ARG1=Stimulus 57:1-ARGM-TMP 35:1*36:1-LINK-SLC 
nw/wsj/00/wsj_0062.parse 21 4 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 4:0-rel 5:2-ARG1=Topic 
nw/wsj/00/wsj_0062.parse 21 24 gold have-v 100 IN have.03 null ----- 21:1-ARG0=Pivot 23:0-ARGM-NEG 24:0-rel 25:1-ARG1=Theme 
nw/wsj/00/wsj_0062.parse 21 29 gold advertise-v 35.2 NF advertise.01 1 ----- 21:1*27:1-ARG0=Agent 29:0-rel 30:1-ARGM-LOC 21:1*27:1-LINK-PRO 
nw/wsj/00/wsj_0062.parse 22 12 gold say-v 37.7-1 IN say.01 null ----- 1:2*13:1-ARG1=Topic 11:1-ARG0=Agent 12:0-rel 
nw/wsj/00/wsj_0062.parse 23 11 gold contend-v 29.5-2 Statement contend.01 1 ----- 0:0-ARGM-DIS 1:2-ARG0=Agent;Speaker 11:0-rel 12:1-ARG1=Theme;Addressee 
nw/wsj/00/wsj_0062.parse 24 5 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:2-ARG1=Theme;Goods 5:0-rel 6:1-ARG3 
nw/wsj/00/wsj_0062.parse 25 7 gold print-v 25.2 Text_creation print.01 null ----- 1:1*8:1-ARG1=Theme;Text 4:1-ARGM-DIS 7:0-rel 9:1-ARGM-LOC 
nw/wsj/00/wsj_0062.parse 26 15 gold print-v 25.2 Text_creation print.01 1 ----- 0:1-ARGM-ADV 5:2-ARG0=Agent;Author 15:0-rel 18:2-ARG1=Theme;Text 
nw/wsj/00/wsj_0062.parse 26 17 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1-ARGM-ADV 5:2-ARG0=Agent;Seller 17:0-rel 18:2-ARG1=Theme;Goods 
nw/wsj/00/wsj_0062.parse 27 11 gold say-v 37.7-1 IN say.01 null ----- 10:1-ARG0=Agent 11:0-rel 0:3-ARG1-DSP=Topic 
nw/wsj/00/wsj_0062.parse 27 18 gold have-v 100 IN have.03 null ----- 16:1-ARG0=Pivot 18:0-rel 19:2-ARG1=Theme 
nw/wsj/00/wsj_0062.parse 28 1 gold ask-v 37.1.2 Questioning ask.01 null ----- 0:1*18:1-ARG2=Recipient;Addressee 1:0-rel 3:1-ARG1=Topic;Message 
nw/wsj/00/wsj_0062.parse 28 8 gold scare-v 31.1 Experiencer_obj scare.02 2 ----- 4:1*9:1-ARG1=Experiencer;Experiencer 6:0-ARGM-MOD 8:0-rel 10:1-ARG3 11:1-ARG2 
nw/wsj/00/wsj_0062.parse 28 20 gold reply-v 37.7 Communication_response reply.01 1 ----- 0:1*2:1*18:1-ARG0=Agent;Speaker 0:2-ARG1=Recipient;Addressee 20:0-rel 23:3-ARG2=Topic;Message/Trigger 
nw/wsj/00/wsj_0062.parse 28 26 gold know-v 29.5-1 IN know.01 1 ----- 23:1-ARG0=Agent 25:0-ARGM-NEG 26:0-rel 
nw/wsj/00/wsj_0062.parse 29 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-ARGM-NEG 3:0-rel 4:1-ARG1=Topic 
nw/wsj/00/wsj_0062.parse 29 13 gold say-v 37.7-1 IN say.01 null ----- 12:1-ARG0=Agent 13:0-rel 0:3-ARG1-DSP=Topic 
nw/wsj/00/wsj_0062.parse 29 22 gold sleep-v 40.4 Sleep sleep.01 1 ----- 19:1-ARG0=Agent;Sleeper 20:0-ARGM-MOD 21:0-ARGM-NEG 22:0-rel 23:1-ARGM-TMP 26:1-ARGM-ADV 
nw/wsj/00/wsj_0062.parse 29 29 gold bow-v 40.3.3 NF bow.01 3 ----- 27:1-ARG0=Agent 29:0-rel 30:1-ARG1=Recipient 33:1-ARGM-CAU 
nw/wsj/00/wsj_0062.parse 32 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/00/wsj_0062.parse 32 20 gold supply-v 13.4.1-1 NF supply.01 1 ----- 4:2*18:1-ARG0=Agent 20:0-rel 21:3-ARG1=Theme 28:1-ARGM-LOC 30:1-ARGM-TMP 
nw/wsj/00/wsj_0062.parse 33 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/00/wsj_0062.parse 33 12 gold provide-v 13.4.1-2 Supply provide.01 null ----- 3:2*13:1-ARG1=Theme 10:0-ARGM-MOD 12:0-rel 14:1-ARG0=Agent 
nw/wsj/00/wsj_0062.parse 33 35 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 29:1*31:1,36:1,38:1-ARG1=Theme;Goods 33:1-ARG0=Agent;Buyer 34:1-ARGM-TMP 35:0-rel 29:1*31:1-LINK-SLC 
nw/wsj/00/wsj_0062.parse 34 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/00/wsj_0062.parse 34 5 gold make-v 29.3 Causation make.02 null ----- 3:1-ARG0=Agent 5:0-rel 6:2-ARG1=Theme 
nw/wsj/00/wsj_0062.parse 35 2 gold provide-v 13.4.1-2 Supply provide.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Theme 5:1-ARGM-PNC 
nw/wsj/00/wsj_0062.parse 35 13 gold say-v 37.7-1 IN say.01 null ----- 8:1,11:1,15:2-ARG1=Topic 12:1-ARG0=Agent 13:0-rel 8:1*11:1-LINK-SLC 
nw/wsj/00/wsj_0062.parse 35 18 gold value-v 54.4 Assessing value.01 null ----- 8:1*11:1*15:1*19:1-ARG1=Theme;Feature 16:0-ARGM-MOD 18:0-rel 20:1-ARG2=Value;Value 27:1-ARGM-TMP 8:1*11:1-LINK-SLC 
nw/wsj/00/wsj_0062.parse 36 1 gold plan-v 62 Purpose plan.01 null ----- 0:1*2:1-ARG0=Experiencer;Agent 1:0-rel 2:2-ARG1 
nw/wsj/00/wsj_0062.parse 36 4 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1*2:1-ARG0=Agent;Seller 4:0-rel 5:1-ARG1=Theme;Goods 8:1-ARG2=Recipient 11:1-ARG3 
nw/wsj/00/wsj_0062.parse 38 6 gold name-v 29.3 IN name.01 null ----- 0:2-ARG0=Agent 6:0-rel 7:2-ARG1=Theme 15:1-ARG2=Result 
nw/wsj/00/wsj_0062.parse 39 4 gold handle-v 15.1-1 NF handle.01 null ----- 0:1*5:1-ARG1=Theme 4:0-rel 6:1-ARG0=Agent 
nw/wsj/00/wsj_0062.parse 41 13 gold award-v 13.3 NF award.01 1 ----- 0:3-ARG0=Agent 13:0-rel 14:2-ARG1=Theme 22:1-ARG2=Goal 
nw/wsj/00/wsj_0062.parse 42 4 gold disclose-v 37.7 Reveal_secret disclose.01 null ----- 0:2*5:1;6:1-ARG1=Topic;Information 3:0-ARGM-NEG 4:0-rel 
nw/wsj/00/wsj_0062.parse 44 7 gold break-v 48.1.1 NF break.06 null ----- 0:1-ARG0 7:0-rel 8:1-ARG1=Theme 13:1-ARGM-TMP 
nw/wsj/00/wsj_0062.parse 45 12 gold provide-v 13.4.1-2 Supply provide.01 null ----- 11:1*13:1-ARG1=Theme 12:0-rel 14:1-ARG0=Agent 17:1-ARGM-PRP 11:1*13:1-LINK-PSV 
nw/wsj/00/wsj_0062.parse 45 19 gold damage-v 44 Damaging damage.01 1 ----- 18:1*20:1-ARG1=Patient;Patient 19:0-rel 21:1-ARGM-LOC 18:1*20:1-LINK-PSV 
nw/wsj/00/wsj_0062.parse 46 1 gold develop-v 26.1 IN develop.02 null ----- 0:1*13:1-ARG1=Product 1:0-rel 3:1-ARG0=Agent 
nw/wsj/00/wsj_0062.parse 48 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/00/wsj_0062.parse 48 6 gold complete-v 55.2 Activity_finish complete.01 null ----- 5:1-ARG0=Agent 6:0-rel 7:2-ARG1=Theme 
nw/wsj/00/wsj_0062.parse 50 5 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 0:2-ARG0=Agent;Recipient 5:0-rel 6:2-ARG1=Theme;Theme 
nw/wsj/00/wsj_0062.parse 51 3 gold disclose-v 37.7 Reveal_secret disclose.01 null ----- 0:1*4:1-ARG1=Topic;Information 2:0-ARGM-NEG 3:0-rel 
