nw/wsj/12/wsj_1206.parse 2 5 gold expect-v 62 IN expect.01 1 ----- 0:1*3:1*4:1-ARG0=Experiencer 5:0-rel 6:2-ARG1=Theme 0:1*3:1-LINK-SLC 
nw/wsj/12/wsj_1206.parse 2 8 gold release-v 80-1 Releasing release.01 2 ----- 0:1*3:1*4:1*6:1-ARG0=Cause 8:0-rel 9:1-ARG1=Theme 20:1-ARGM-TMP 0:1*3:1-LINK-SLC 
nw/wsj/12/wsj_1206.parse 5 3 gold leave-v 51.2-1 Causation leave.01 3 ----- 0:0-ARGM-DIS 1:1-ARG0=Theme 3:0-rel 4:2*12:1;17:1-ARG1=Initial_Location 12:2-ARG2 
nw/wsj/12/wsj_1206.parse 5 13 gold hold-v 15.1-1 Manipulation hold.01 10 ----- 4:2*12:1;17:1-ARG0=Agent;Agent 13:0-rel 14:1-ARG1=Theme;Entity 
nw/wsj/12/wsj_1206.parse 5 23 gold need-v 32.1-1-1 Needing need.01 2 ----- 18:1*20:1*24:1-ARG1=Theme 21:1-ARG0=Pivot 22:0-ARGM-MOD 23:0-rel 25:2-ARGM-PNC 18:1*20:1-LINK-SLC 
nw/wsj/12/wsj_1206.parse 6 5 gold say-v 37.7-1 IN say.01 6 ----- 0:1,6:2-ARG1=Topic 5:0-rel 
nw/wsj/12/wsj_1206.parse 6 9 gold consider-v 29.9-1-1-1 Cogitation consider.02 2 ----- 0:1*6:1-ARG0=Agent;Cognizer 9:0-rel 10:3-ARG1=Theme;Topic 
nw/wsj/12/wsj_1206.parse 6 18 gold force-v 59 NF force.01 1 ----- 0:1*16:1-ARG0=Agent 18:0-rel 19:1-ARG1=Patient 21:1-ARG2=Result 0:1*16:1-LINK-PRO 
nw/wsj/12/wsj_1206.parse 7 23 gold say-v 37.7-1 IN say.01 6 ----- 0:0-ARGM-DIS 1:3,24:2-ARG1=Topic 21:1-ARGM-DIS 23:0-rel 
nw/wsj/12/wsj_1206.parse 9 19 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 0:1-ARG0=Agent;Seller 17:0-ARGM-TMP 18:1-ARGM-TMP 19:0-rel 20:2-ARG1=Theme;Goods 24:1-ARG3 
nw/wsj/12/wsj_1206.parse 10 8 gold pile-v 9.7-1-1 IN pile.01 3 ----- 0:1-ARGM-TMP 3:1*16:1-ARG0=Agent 4:1-ARGM-TMP 8:0-rel 9:1-ARG1=Theme 10:1-ARG2=Destination 16:2-ARGM-ADV 
nw/wsj/12/wsj_1206.parse 10 17 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 3:1*16:1-ARG0=Agent;Seller 17:0-rel 18:1-ARG1=Theme;Goods 19:1-ARG3 24:1-ARG2=Recipient 
nw/wsj/12/wsj_1206.parse 11 9 gold take-v 11.3 Taking take.01 5 ----- 0:1-ARGM-LOC 5:1-ARG0 6:1-ARGM-MNR 9:0-rel 10:3-ARG1 18:1-ARG2 
nw/wsj/12/wsj_1206.parse 11 26 gold move-v 11.2 Cause_motion move.01 1 ----- 0:1-ARGM-LOC 5:1-ARG0 6:1-ARGM-MNR 26:0-rel 27:1-ARG1 28:1-ARG2 34:2-ARGM-ADV 
nw/wsj/12/wsj_1206.parse 11 35 gold make-v 29.3 Cause_change make.02 3 ----- 34:1-ARG0=Agent 35:0-rel 36:2-ARG1=Theme 
nw/wsj/12/wsj_1206.parse 13 6 gold put-v 9.1-2 IN put.01 7.4 ----- 0:1-ARGM-MNR 5:1-ARG0=Agent 6:0-rel 7:0-ARGM-DIR 8:1-ARG2=Destination 11:3-ARG1=Theme 25:2-ARGM-ADV 
nw/wsj/12/wsj_1206.parse 13 26 gold buy-v 13.5.1 Commerce_buy buy.01 1 ----- 5:1*25:1-ARG0=Agent;Buyer 26:0-rel 27:2-ARG1=Theme;Goods 
nw/wsj/12/wsj_1206.parse 14 5 gold have-v 100 IN have.03 1 ----- 3:1-ARG0=Pivot 4:1-ARGM-TMP 5:0-rel 6:3*22:1-ARG1=Theme 
nw/wsj/12/wsj_1206.parse 14 7 gold estimate-v 54.4 Estimating estimate.01 1 ----- 7:0-rel 8:1,11:0-ARG1=Theme 
nw/wsj/12/wsj_1206.parse 15 21 gold own-v 100 NF own.01 1 ----- 16:1*19:1*23:1-ARGM-TMP 20:1-ARG0=Pivot 21:0-rel 22:1-ARG1=Theme 16:1*19:1-LINK-SLC 
nw/wsj/12/wsj_1206.parse 16 4 gold risk-v 94-1 Daring risk.01 2 ----- 0:1-ARGM-TMP 2:1-ARG0=Agent;Agent 4:0-rel 5:2-ARG1=Theme;Action 
nw/wsj/12/wsj_1206.parse 16 6 gold lose-v 13.2 NF lose.02 6 ----- 2:1*5:1-ARG0=Agent 6:0-rel 7:2-ARG1=Theme 20:1-ARGM-ADV 
nw/wsj/12/wsj_1206.parse 17 5 gold hold-v 15.1-1 Manipulation hold.01 null ----- 5:0-rel 6:0-ARG0=Agent;Agent 
nw/wsj/12/wsj_1206.parse 18 13 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARGM-ADV 12:1-ARG0=Agent 13:0-rel 14:1-ARG1=Topic 
nw/wsj/12/wsj_1206.parse 18 22 gold liquidate-v 42.1 Killing liquidate.01 1 ----- 16:1*19:1*23:1-ARG1=Patient;Victim 18:0-ARGM-MOD 22:0-rel 24:1-ARGM-TMP 
nw/wsj/12/wsj_1206.parse 18 28 gold fetch-v 13.5.1-1 Bringing fetch.01 2 ----- 15:1-ARGM-ADV 26:1-ARG0=Agent 27:0-ARGM-MOD 28:0-rel 29:2-ARG1=Theme 40:2-ARGM-ADV 
nw/wsj/12/wsj_1206.parse 19 15 gold stay-v 47.1-1 IN stay.01 1 ----- 13:1*20:1-ARG1=Theme 15:0-rel 16:1-ARG3 
nw/wsj/12/wsj_1206.parse 20 14 gold expire-v 48.2 Death expire.01 1 ----- 0:3*12:1-ARG1=Patient;Protagonist 14:0-rel 15:1-ARGM-TMP 
nw/wsj/12/wsj_1206.parse 22 3 gold start-v 55.1-1 Process_start start.01 1 ----- 0:1-ARG1=Theme;Event 3:0-rel 4:1-ARGM-MNR 
nw/wsj/12/wsj_1206.parse 22 14 gold say-v 37.7-1 IN say.01 1 ----- 12:1-ARG0=Agent 14:0-rel 15:1-ARG1=Topic 
nw/wsj/12/wsj_1206.parse 22 17 gold like-v 31.2-1 NF like.01 2 ----- 16:1-ARG0=Experiencer 17:0-rel 18:1-ARG1=Attribute 
nw/wsj/12/wsj_1206.parse 22 23 gold say-v 37.7-1 IN say.01 1 ----- 0:3*24:1-ARG1=Topic 23:0-rel 25:3-ARG0=Agent 
nw/wsj/12/wsj_1206.parse 23 9 gold talk-v 37.5 IN talk.01 1 ----- 0:2-ARG0=Agent 8:0-ARGM-MOD 9:0-rel 10:1-ARGM-MNR 
nw/wsj/12/wsj_1206.parse 24 3 gold understand-v 77 Awareness understand.01 3 ----- 0:0-ARGM-DIS 3:0-rel 4:1-ARG1=Theme;Content/Topic 
nw/wsj/12/wsj_1206.parse 24 8 gold disappoint-v 31.1 Experiencer_obj disappoint.01 1 ----- 5:1*9:1-ARG1=Experiencer;Experiencer 8:0-rel 10:1-ARG0=Stimulus;Stimulus 
nw/wsj/12/wsj_1206.parse 24 15 gold manage-v 74-1-1 IN manage.02 1 ----- 11:1*16:1-ARG0 14:0-ARGM-NEG 15:0-rel 16:2-ARG1 
nw/wsj/12/wsj_1206.parse 24 18 gold boost-v 102 NF boost.01 1 ----- 11:1*16:1-ARG0=Agent 18:0-rel 19:2-ARG1=Theme 24:1-ARGM-TMP 
nw/wsj/12/wsj_1206.parse 25 3 gold think-v 29.9-2 IN think.01 1 ----- 0:1-ARG0 2:1-ARGM-ADV 3:0-rel 4:1-ARG1 
nw/wsj/12/wsj_1206.parse 25 11 gold extend-v 47.1-1 NF extend.01 2 ----- 10:1-ARG0 11:0-rel 12:1-ARG1=Theme 15:1-ARG2 23:2-ARGM-ADV 
nw/wsj/12/wsj_1206.parse 25 18 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 2 ----- 16:1-ARG1=Patient;Item 18:0-rel 19:1-ARGM-ADV 21:1-ARGM-TMP 
nw/wsj/12/wsj_1206.parse 25 24 gold allow-v 64 IN allow.01 1 ----- 23:1-ARG0 24:0-rel 25:2-ARG1 
nw/wsj/12/wsj_1206.parse 25 28 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 25:1-ARG0=Agent;Seller 28:0-rel 29:1-ARG1=Theme;Goods 30:2-ARGM-PNC 
nw/wsj/12/wsj_1206.parse 26 3 gold say-v 37.7-1 IN say.01 1 ----- 0:1,4:2-ARG1=Topic 3:0-rel 
nw/wsj/12/wsj_1206.parse 26 15 gold lift-v 9.4 NF lift.01 1 ----- 13:1-ARG0=Agent 15:0-rel 16:2-ARG1=Theme 
nw/wsj/12/wsj_1206.parse 27 8 gold discover-v 29.5-2 Becoming_aware discover.01 1 ----- 6:1-ARG0=Agent;Cognizer 8:0-rel 9:1-ARG1=Theme;Phenomenon 
nw/wsj/12/wsj_1206.parse 28 8 gold quote-v 37.1.1-1 NF quote.01 3 ----- 0:2*9:1-ARG1=Source 5:1-ARGM-TMP 8:0-rel 10:1-ARG2=Topic 25:1-ARGM-ADV 
nw/wsj/12/wsj_1206.parse 28 12 gold range-v 47.7 NF range.01 1 ----- 11:1-ARG1=Theme 12:0-rel 13:1-ARG3 17:1-ARG4 
nw/wsj/12/wsj_1206.parse 28 37 gold trade-v 13.6-1 Exchange trade.01 1 ----- 27:2*35:1*36:1-ARG0=Agent 37:0-rel 38:1-ARG1=Theme 27:2*35:1-LINK-SLC 
nw/wsj/12/wsj_1206.parse 29 4 gold see-v 30.1-1 Perception_experience see.01 1 ----- 0:1*1:1*2:1-ARG0=Experiencer;Perceiver_passive 4:0-rel 5:2-ARG1=Stimulus;Phenomenon 0:1*1:1-LINK-SLC 
nw/wsj/12/wsj_1206.parse 29 10 gold say-v 37.7-1 IN say.01 1 ----- 0:2-ARG0=Agent 10:0-rel 11:1-ARG1=Topic 
nw/wsj/12/wsj_1206.parse 29 13 gold offer-v 13.3 NF offer.01 1 ----- 12:1-ARG0=Agent 13:0-rel 14:2-ARG1=Theme 
nw/wsj/12/wsj_1206.parse 30 3 gold give-v 13.1-1 Giving give.01 1 ----- 0:1-ARG0=Agent;Donor 1:0-ARGM-MOD 2:0-ARGM-ADV 3:0-rel 4:2-ARG1=Theme;Theme 18:1-ARG2=Recipient;Recipient 32:1-ARGM-PNC 
nw/wsj/12/wsj_1206.parse 30 7 gold combine-v 22.1-1-1 Amalgamation combine.01 null ----- 7:0-rel 8:1-ARG3 
nw/wsj/12/wsj_1206.parse 30 42 gold accept-v 13.5.2 IN accept.01 1 ----- 39:1*40:1-ARG0=Agent 42:0-rel 43:2-ARG1=Theme 
nw/wsj/12/wsj_1206.parse 30 48 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 1 ----- 43:1*45:1*46:1-ARG0=Cause 47:0-ARGM-MOD 48:0-rel 49:2-ARG1=Patient 43:1*45:1-LINK-SLC 
nw/wsj/12/wsj_1206.parse 31 8 gold say-v 37.7-1 IN say.01 1 ----- 0:0-ARGM-DIS 1:1-ARG0=Agent 8:0-rel 9:1-ARG1=Topic 
nw/wsj/12/wsj_1206.parse 32 1 gold contend-v 29.5-2 Statement contend.01 1 ----- 0:1-ARG0=Agent;Speaker 1:0-rel 2:1-ARG1=Theme;Addressee 
nw/wsj/12/wsj_1206.parse 33 1 gold add-v 37.7 NF add.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/12/wsj_1206.parse 33 7 gold cost-v 54.2 Expensiveness cost.01 1 ----- 4:1*10:2-ARG1=Theme;Goods 6:0-ARGM-NEG 7:0-rel 8:1*10:1-ARG3 9:1-ARG2=Value;Asset 15:1-ARGM-CAU 
nw/wsj/12/wsj_1206.parse 33 32 gold think-v 29.9-2 IN think.01 1 ----- 31:1-ARG0 32:0-rel 33:1-ARG1 
nw/wsj/12/wsj_1206.parse 33 35 gold contribute-v 13.2-1-1 NF contribute.01 2 ----- 17:2*30:1*34:1-ARG0=Agent 35:0-rel 36:1-ARG2=Recipient 17:2*30:1-LINK-SLC 
nw/wsj/12/wsj_1206.parse 34 15 gold join-v 22.1-2-1 Cause_to_amalgamate join.01 2 ----- 0:2*13:1-ARG0=Agent;Agent 15:0-rel 16:1-ARG1=Patient;Part_1 
nw/wsj/12/wsj_1206.parse 34 23 gold say-v 37.7-1 IN say.01 6 ----- 0:2,24:2-ARG1=Topic 23:0-rel 
nw/wsj/12/wsj_1206.parse 34 27 gold review-v 34.1 NF review.01 1 ----- 0:2*24:1-ARG0=Agent 27:0-rel 28:1-ARG1=Theme 
nw/wsj/12/wsj_1206.parse 35 10 gold want-v 32.1-1-1 Desiring want.01 1 ----- 0:1*6:1-ARG0=Pivot;Experiencer 0:2-ARGM-PNC 10:0-rel 11:3*25:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/12/wsj_1206.parse 35 17 gold invest-v 13.4.2 NF invest.01 1 ----- 11:2-ARG0=Agent 17:0-rel 18:2;25:2-ARG1=Theme 21:1-ARG2=Recipient 
nw/wsj/12/wsj_1206.parse 36 7 gold involve-v 86.2-1 NF involve.01 1 ----- 0:1*3:1*4:1-ARG1 6:0-ARGM-NEG 7:0-rel 8:1-ARG2 0:1*3:1-LINK-SLC 
nw/wsj/12/wsj_1206.parse 36 11 gold say-v 37.7-1 IN say.01 1 ----- 0:2-ARG0=Agent 11:0-rel 12:1-ARG1=Topic 
nw/wsj/12/wsj_1206.parse 36 15 gold need-v 32.1-1-1 Needing need.01 1 ----- 13:1-ARG0=Pivot 15:0-rel 16:3-ARG1=Theme 25:2-ARGM-PNC 
nw/wsj/12/wsj_1206.parse 37 3 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/12/wsj_1206.parse 37 6 gold have-v 100 IN have.03 11 ----- 5:1-ARG0=Pivot 6:0-rel 7:2-ARG1=Theme 
nw/wsj/12/wsj_1206.parse 37 30 gold extract-v 10.1 Removing extract.01 2 ----- 28:1-ARG0=Agent;Agent/Cause 30:0-rel 31:1-ARG1=Theme;Theme 32:1-ARG2=Source;Source 
nw/wsj/12/wsj_1206.parse 37 36 gold say-v 37.7-1 IN say.01 1 ----- 36:0-rel 37:1-ARG1=Topic 39:1-ARG0=Agent 
nw/wsj/12/wsj_1206.parse 38 14 gold add-v 37.7 NF add.01 1 ----- 0:2*16:1-ARG1=Topic 13:1-ARG0=Agent 14:0-rel 
nw/wsj/12/wsj_1206.parse 39 7 gold tarnish-v 45.5 Corroding_caused tarnish.01 null ----- 0:1-ARG0 6:0-ARGM-MOD 7:0-rel 8:2-ARG1=Patient;Undergoer 
nw/wsj/12/wsj_1206.parse 39 13 gold provide-v 13.4.1-2 Supply provide.01 1 ----- 0:1-ARG0=Agent 6:0-ARGM-MOD 13:0-rel 14:1-ARG1=Theme 16:1-ARG2=Recipient 
nw/wsj/12/wsj_1206.parse 40 2 gold say-v 37.7-1 IN say.01 1 ----- 0:0-ARGM-DIS 1:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/12/wsj_1206.parse 40 7 gold have-v 100 IN have.03 2 ----- 4:1-ARG0=Pivot 7:0-rel 8:2-ARG1=Theme 
nw/wsj/12/wsj_1206.parse 40 12 gold gain-v 13.5.1-1 Getting gain.02 1 ----- 8:1*9:1*13:1-ARG1=Theme;Theme 10:1-ARG0=Agent;Recipient 12:0-rel 14:1-ARGM-MNR 8:1*9:1-LINK-SLC 
nw/wsj/12/wsj_1206.parse 40 16 gold put-v 9.1-2 IN put.01 1 ----- 4:1*15:1-ARG0=Agent 16:0-rel 17:1-ARG1=Theme 19:1-ARG2=Destination 4:1*15:1-LINK-PRO 
nw/wsj/12/wsj_1206.parse 41 18 gold have-v 100 IN have.03 11 ----- 0:1-ARGM-ADV 16:1-ARG0=Pivot 17:1-ARGM-ADV 18:0-rel 19:2-ARG1=Theme 
nw/wsj/12/wsj_1206.parse 42 2 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/12/wsj_1206.parse 42 11 gold put-v 9.1-2 IN put.01 4 ----- 4:1*10:1-ARG0=Agent 11:0-rel 12:1-ARG1=Theme 14:1-ARG2=Destination 4:1*10:1-LINK-PRO 
nw/wsj/12/wsj_1206.parse 43 7 gold try-v 61 Attempt try.01 1 ----- 0:0-ARGM-DIS 1:1-ARGM-ADV 4:1-ARG0=Agent 6:1-ARGM-ADV 7:0-rel 8:2-ARG1=Theme 
nw/wsj/12/wsj_1206.parse 43 10 gold salvage-v 10.5-1 NF salvage.01 1 ----- 4:1*8:1-ARG0=Agent 10:0-rel 11:1-ARG1=Theme 
nw/wsj/12/wsj_1206.parse 43 15 gold say-v 37.7-1 IN say.01 1 ----- 15:0-rel 16:1-ARG1=Topic 18:2-ARG0=Agent 
nw/wsj/12/wsj_1206.parse 45 4 gold know-v 29.5-1 IN know.01 2 ----- 1:1*2:1*3:1-ARG0=Agent 4:0-rel 5:1-ARG1=Theme 1:1*2:1-LINK-SLC 
nw/wsj/12/wsj_1206.parse 45 7 gold say-v 37.7-1 IN say.01 1 ----- 0:0-ARGM-DIS 1:2-ARG0=Agent 7:0-rel 8:1-ARG1=Topic 
nw/wsj/12/wsj_1206.parse 45 17 gold pour-v 43.4 Mass_motion pour.01 3 ----- 9:1*15:1-ARG0=Source;Source 17:0-rel 18:1-ARG1=Theme;Mass_theme 20:1-ARG3 9:1*15:1-LINK-PRO 
