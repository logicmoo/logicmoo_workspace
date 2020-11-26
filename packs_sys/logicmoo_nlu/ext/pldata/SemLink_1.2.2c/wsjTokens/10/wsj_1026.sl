nw/wsj/10/wsj_1026.parse 1 2 gold expect-v 62 IN expect.01 1 ----- 1:1-ARGM-ADV 2:0-rel 3:2-ARG1=Theme 
nw/wsj/10/wsj_1026.parse 1 5 gold open-v 48.1.1 NF open.02 2 ----- 0:1*3:1*24:1-ARG1=Theme 5:0-rel 6:4-ARGM-MNR 13:1-ARGM-CAU 
nw/wsj/10/wsj_1026.parse 1 27 gold manage-v 74-1-1 IN manage.02 1 ----- 0:2-ARGM-ADV 24:1-ARG0 27:0-rel 28:2-ARG1 
nw/wsj/10/wsj_1026.parse 1 30 gold start-v 55.1-1 Process_start start.01 2 ----- 24:1*28:1-ARG0=Agent 30:0-rel 31:1-ARG1=Theme;Event 33:3-ARGM-MNR 
nw/wsj/10/wsj_1026.parse 2 16 gold end-v 55.4-1 Cause_to_end end.01 1 ----- 9:1*14:1-ARG0=Agent 16:0-rel 17:1-ARG1=Theme 19:2-ARGM-MNR 22:1-ARGM-EXT 
nw/wsj/10/wsj_1026.parse 3 6 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 1 ----- 3:1-ARG1=Patient;Item 6:0-rel 7:1-ARG4 
nw/wsj/10/wsj_1026.parse 3 15 gold say-v 37.7-1 IN say.01 1 ----- 13:1-ARG0=Agent 15:0-rel 16:1-ARG1=Topic 
nw/wsj/10/wsj_1026.parse 3 18 gold show-v 78-1-1 IN show.01 1 ----- 8:1*12:1*17:1-ARG0 18:0-rel 19:1-ARG1 8:1*12:1-LINK-SLC 
nw/wsj/10/wsj_1026.parse 3 34 gold stop-v 67 Process_stop stop.03 2 ----- 30:1*31:1*32:1-ARG0=Agent 34:0-rel 35:1-ARG1=Theme 37:1-ARG2=Theme 30:1*31:1-LINK-SLC 
nw/wsj/10/wsj_1026.parse 4 14 gold dictate-v 37.1.1-1 NF dictate.01 1 ----- 1:1*11:1*12:1-ARG0=Agent 14:0-rel 15:1-ARG1=Topic 1:1*11:1-LINK-SLC 
nw/wsj/10/wsj_1026.parse 4 21 gold want-v 32.1-1-1 Desiring want.01 1 ----- 0:1-ARGM-ADV 19:1*22:1-ARG0=Pivot;Experiencer 21:0-rel 22:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/10/wsj_1026.parse 4 24 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 19:1*22:1-ARG0=Agent;Seller 24:0-rel 25:1-ARG1=Theme;Goods 
nw/wsj/10/wsj_1026.parse 4 29 gold say-v 37.7-1 IN say.01 1 ----- 0:2*30:1-ARG1=Topic 29:0-rel 31:2-ARG0=Agent 
nw/wsj/10/wsj_1026.parse 5 9 gold have-v 100 IN have.03 11 ----- 1:0-ARGM-DIS 2:1-ARGM-TMP 6:1-ARG0=Pivot 8:0-ARGM-MOD 9:0-rel 10:1-ARG1=Theme 
nw/wsj/10/wsj_1026.parse 6 8 gold show-v 78-1-1 IN show.01 1 ----- 4:1-ARG0 8:0-rel 9:2-ARG1 
nw/wsj/10/wsj_1026.parse 7 2 gold hit-v 18.4 Cause_harm hit.01 2 ----- 0:1-ARG0 2:0-rel 3:1-ARG1=Location 6:1-ARGM-TMP 9:2-ARGM-ADV 
nw/wsj/10/wsj_1026.parse 7 20 gold prevent-v 67 Thwarting prevent.01 1 ----- 0:1*9:1-ARG0=Agent;Preventing_cause 20:0-rel 21:1-ARG1=Theme;Protagonist/Action 22:1-ARG2=Theme;Protagonist/Action 
nw/wsj/10/wsj_1026.parse 8 7 gold show-v 78-1-1 IN show.01 1 ----- 0:2-ARG0 4:0-ARGM-MOD 5:1-ARGM-ADV 7:0-rel 8:2-ARG1 
nw/wsj/10/wsj_1026.parse 9 1 gold dismiss-v 10.1 Firing dismiss.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:2-ARG1=Theme 
nw/wsj/10/wsj_1026.parse 10 18 gold indicate-v 78-1 Communication indicate.01 1 ----- 0:2-ARG0=Cause;Communicator 18:0-rel 19:1-ARG1=Topic;Message 
nw/wsj/10/wsj_1026.parse 11 1 gold suggest-v 37.7-1-1 Statement suggest.01 2 ----- 0:1-ARG0=Agent;Speaker 1:0-rel 2:1-ARG1=Topic;Message 
nw/wsj/10/wsj_1026.parse 12 13 gold continue-v 55.3 Process_continue continue.01 1 ----- 12:1,14:1-ARG1 13:0-rel 
nw/wsj/10/wsj_1026.parse 12 17 gold say-v 37.7-1 IN say.01 1 ----- 0:3*19:1-ARG1=Topic 16:1-ARG0=Agent 17:0-rel 
nw/wsj/10/wsj_1026.parse 14 3 gold maintain-v 29.5-2 Statement maintain.01 1 ----- 2:1-ARG0=Agent;Speaker 3:0-rel 4:1-ARG1=Theme;Addressee 
nw/wsj/10/wsj_1026.parse 14 6 gold assume-v 93 Adopt_selection assume.01 1 ----- 0:1-ARGM-DIS 2:2-ARG0 6:0-rel 7:2-ARG1 
nw/wsj/10/wsj_1026.parse 15 7 gold affect-v 31.1 NF affect.01 1 ----- 4:1-ARG0=Stimulus 7:0-rel 8:2-ARG1=Experiencer 
nw/wsj/10/wsj_1026.parse 15 22 gold continue-v 55.3 Process_continue continue.01 1 ----- 0:2-ARGM-ADV 12:2,23:2-ARG1 16:2-ARGM-LOC 22:0-rel 29:2-ARGM-ADV 
nw/wsj/10/wsj_1026.parse 15 30 gold leave-v 51.2-1 Causation leave.01 3 ----- 29:1-ARG0=Theme 30:0-rel 31:3-ARG1=Initial_Location 35:2-ARG2 
nw/wsj/10/wsj_1026.parse 16 4 gold see-v 30.1-1 Perception_experience see.01 3 ----- 1:1-ARG0=Experiencer;Perceiver_passive 3:0-ARGM-NEG 4:0-rel 5:1-ARG1=Stimulus;Phenomenon 8:1-ARGM-LOC 
nw/wsj/10/wsj_1026.parse 16 13 gold say-v 37.7-1 IN say.01 1 ----- 1:2*14:1-ARG1=Topic 11:1-ARG0=Agent 13:0-rel 15:1-ARGM-LOC 
nw/wsj/10/wsj_1026.parse 19 5 gold move-v 45.6-1 Change_position_on_a_scale move.01 1 ----- 0:3-ARG1 5:0-rel 6:1-ARGM-DIR 7:1-ARGM-CAU 
nw/wsj/10/wsj_1026.parse 19 16 gold try-v 61 Attempt try.01 1 ----- 13:2-ARG0=Agent 16:0-rel 17:2-ARG1=Theme 
nw/wsj/10/wsj_1026.parse 19 28 gold continue-v 55.3 Process_continue continue.01 null ----- 28:0-rel 29:0,30:0-ARG1 
nw/wsj/10/wsj_1026.parse 20 12 gold remove-v 10.2 Removing remove.01 1 ----- 0:3-ARG0=Agent;Agent/Cause 11:1-ARGM-DIS 12:0-rel 13:2-ARG1=Theme;Theme 
nw/wsj/10/wsj_1026.parse 21 3 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 1 ----- 0:2-ARG1=Patient;Item 3:0-rel 5:1-ARGM-DIS 7:3-ARGM-CAU 
nw/wsj/10/wsj_1026.parse 23 22 gold support-v 31.2 Taking_sides support.01 1 ----- 8:3-ARG0=Experiencer 22:0-rel 23:1-ARG1=Stimulus 24:1-ARGM-TMP 
nw/wsj/10/wsj_1026.parse 23 28 gold say-v 37.7-1 IN say.01 1 ----- 0:2*30:1-ARG1=Topic 28:0-rel 31:2-ARG0=Agent 
nw/wsj/10/wsj_1026.parse 24 5 gold gain-v 45.6-1 Change_position_on_a_scale gain.01 2 ----- 0:2-ARG1=Patient;Item 5:0-rel 6:2-ARG2=Extent;Difference 10:2-ARGM-PNC 19:1-ARGM-LOC 
nw/wsj/10/wsj_1026.parse 26 9 gold support-v 31.2 Taking_sides support.01 1 ----- 0:3-ARG0=Experiencer 8:1-ARGM-DIS 9:0-rel 10:1-ARG1=Stimulus 
nw/wsj/10/wsj_1026.parse 27 19 gold say-v 37.7-1 IN say.01 1 ----- 1:3*20:1-ARG1=Topic 19:0-rel 21:2-ARG0=Agent 
nw/wsj/10/wsj_1026.parse 28 4 gold purchase-v 13.5.2-1 Commerce_buy purchase.01 1 ----- 0:1-ARG0=Agent;Buyer 4:0-rel 5:3-ARG1=Theme;Goods 11:1-ARGM-TMP 
nw/wsj/10/wsj_1026.parse 28 16 gold expect-v 62 IN expect.01 1 ----- 0:1,17:2-ARG1=Theme 16:0-rel 
nw/wsj/10/wsj_1026.parse 28 19 gold take-v 10.5 Taking take.01 2 ----- 0:1*17:1-ARG0=Agent 19:0-rel 20:1-ARG1=Theme 21:1-ARGM-TMP 
nw/wsj/10/wsj_1026.parse 28 26 gold say-v 37.7-1 IN say.01 1 ----- 0:2*28:1-ARG1=Topic 25:1-ARG0=Agent 26:0-rel 
nw/wsj/10/wsj_1026.parse 30 2 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 2 ----- 0:1-ARG1=Patient;Item 2:0-rel 3:1-ARG2=Extent;Difference 
nw/wsj/10/wsj_1026.parse 31 3 gold settle-v 46 Colonization settle.03 4 ----- 0:1-ARG1=Theme 3:0-rel 4:1-ARG4=Location 10:1-ARG2 
nw/wsj/10/wsj_1026.parse 31 16 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 2 ----- 15:1-ARG1=Patient;Item 16:0-rel 17:2-ARG4 
nw/wsj/10/wsj_1026.parse 32 3 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/10/wsj_1026.parse 33 4 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 0:1*1:1*2:1-ARG0=Agent;Seller 4:0-rel 5:1-ARG1=Theme;Goods 6:1-ARGM-TMP 8:1-ARGM-PNC 0:1*1:1-LINK-SLC 
nw/wsj/10/wsj_1026.parse 33 12 gold buy-v 13.5.1 Commerce_buy buy.01 1 ----- 0:1*1:1*2:1*11:1-ARG0=Agent;Buyer 12:0-rel 13:1-ARG1=Theme;Goods 14:1-ARGM-ADV 15:1-ARG3=Asset 0:1*1:1-LINK-SLC 
nw/wsj/10/wsj_1026.parse 33 21 gold buy-v 13.5.1 Commerce_buy buy.01 1 ----- 0:2-ARG0=Agent;Buyer 19:1-ARGM-TMP 21:0-rel 22:1-ARG1=Theme;Goods 23:1-ARGM-ADV 24:1-ARG3=Asset 27:2-ARGM-PNC 
nw/wsj/10/wsj_1026.parse 33 29 gold limit-v 76 NF limit.01 1 ----- 0:2*27:1-ARG0=Cause 29:0-rel 30:1-ARG1=Patient 
nw/wsj/10/wsj_1026.parse 34 3 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 2:1-ARGM-DIS 3:0-rel 4:1-ARG1=Topic 
nw/wsj/10/wsj_1026.parse 34 10 gold help-v 72-1 Assistance help.01 1 ----- 5:1*11:1-ARG1=Theme;Goal/Focal_entity 7:0-ARGM-MOD 10:0-rel 12:1-ARG0=Agent;Helper 
nw/wsj/10/wsj_1026.parse 34 28 gold purchase-v 13.5.2-1 Commerce_buy purchase.01 1 ----- 27:1-ARG0=Agent;Buyer 28:0-rel 29:1-ARG1=Theme;Goods 
nw/wsj/10/wsj_1026.parse 35 10 gold deny-v 29.5-1 Statement deny.01 1 ----- 0:1*3:1*4:1*11:1-ARG1=Predicate;Message 9:0-ARGM-NEG 10:0-rel 0:1*3:1-LINK-SLC 
nw/wsj/10/wsj_1026.parse 35 13 gold have-v 100 IN have.03 6 ----- 0:2-ARG0=Pivot 13:0-rel 14:2-ARG1=Theme 
nw/wsj/10/wsj_1026.parse 35 15 gold buy-v 13.5.1 Commerce_buy buy.01 1 ----- 14:1-ARG0=Agent;Buyer 15:0-rel 16:2-ARG1=Theme;Goods 20:1-ARGM-PNC 
nw/wsj/10/wsj_1026.parse 36 3 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/10/wsj_1026.parse 36 14 gold have-v 100 IN have.03 6 ----- 5:1-ARGM-ADV 11:1-ARG0=Pivot 12:0-ARGM-MOD 14:0-rel 15:2-ARG1=Theme 
nw/wsj/10/wsj_1026.parse 37 1 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/10/wsj_1026.parse 38 16 gold see-v 30.1-1 Perception_experience see.01 3 ----- 11:1*14:1*15:1-ARG0=Experiencer;Perceiver_passive 16:0-rel 17:1-ARG1=Stimulus;Phenomenon 19:1-ARGM-LOC 11:1*14:1-LINK-SLC 
nw/wsj/10/wsj_1026.parse 38 29 gold buy-v 13.5.1 Commerce_buy buy.01 1 ----- 22:1*25:1*26:1-ARG0=Agent;Buyer 28:1-ARGM-ADV 29:0-rel 30:1-ARG1=Theme;Goods 31:1-ARG3=Asset 22:1*25:1-LINK-SLC 
nw/wsj/10/wsj_1026.parse 38 45 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 22:1*25:1*26:1-ARG0=Agent;Seller 28:1-ARGM-ADV 45:0-rel 46:1-ARG1=Theme;Goods 47:2-ARGM-TMP 22:1*25:1-LINK-SLC 
nw/wsj/10/wsj_1026.parse 38 50 gold climb-v 45.6-1 Change_position_on_a_scale climb.02 1 ----- 47:1*55:1-ARGM-TMP 48:1-ARG1=Patient;Item 50:0-rel 51:2-ARG2=Extent;Difference 
nw/wsj/10/wsj_1026.parse 39 2 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/10/wsj_1026.parse 39 5 gold think-v 29.9-2 IN think.01 1 ----- 4:1-ARG0 5:0-rel 6:1-ARG1 
nw/wsj/10/wsj_1026.parse 39 10 gold turn-v 26.6.2 Undergo_change turn.02 2 ----- 7:1-ARG1=Patient 9:0-ARGM-MOD 10:0-rel 11:1-ARG2=Goal 17:1-ARGM-ADV 
nw/wsj/10/wsj_1026.parse 39 25 gold exceed-v 90-1 NF exceed.01 1 ----- 18:1*23:1-ARG0=Theme 25:0-rel 26:1-ARG1=Co-Theme 
nw/wsj/10/wsj_1026.parse 42 8 gold start-v 55.1-1 Process_start start.01 2 ----- 0:1*6:1*7:1-ARG1=Theme;Event 8:0-rel 9:1-ARGM-TMP 0:1*6:1-LINK-SLC 
nw/wsj/10/wsj_1026.parse 42 12 gold continue-v 55.3 Process_continue continue.01 1 ----- 0:2-ARG1 12:0-rel 
nw/wsj/10/wsj_1026.parse 43 3 gold end-v 55.4-1 Cause_to_end end.01 1 ----- 0:1-ARG1=Theme 3:0-rel 4:1-ARGM-EXT 12:1-ARGM-MNR 
nw/wsj/10/wsj_1026.parse 45 15 gold say-v 37.7-1 IN say.01 1 ----- 0:2-ARG0=Agent 15:0-rel 16:1-ARG1=Topic 
nw/wsj/10/wsj_1026.parse 45 23 gold issue-v 13.3 NF issue.01 1 ----- 17:1*20:1*24:1-ARG1=Theme 21:1-ARG0=Agent 23:0-rel 17:1*20:1-LINK-SLC 
nw/wsj/10/wsj_1026.parse 46 12 gold total-v 54.1-1 Amounting_to total.01 1 ----- 11:1-ARG1=Theme;Attribute 12:0-rel 13:1-ARG2=Value;Value 
nw/wsj/10/wsj_1026.parse 46 17 gold announce-v 37.7-1 Statement announce.01 1 ----- 11:2*18:1-ARG1=Topic;Message 17:0-rel 
nw/wsj/10/wsj_1026.parse 47 4 gold exist-v 47.1-1 Existence exist.01 1 ----- 0:1-ARG1=Theme;Entity 3:1-ARGM-MOD 4:0-rel 5:1-ARGM-LOC 9:1-ARGM-ADV 18:1-ARGM-ADV 
nw/wsj/10/wsj_1026.parse 47 12 gold release-v 80-1 Releasing release.01 2 ----- 10:1*13:1-ARG1=Theme 12:0-rel 14:1-ARGM-TMP 15:1-ARGM-TMP 
nw/wsj/10/wsj_1026.parse 48 1 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/10/wsj_1026.parse 48 16 gold result-v 48.1.1 NF result.01 1 ----- 3:1-ARGM-ADV 13:1-ARG2=Location 15:0-ARGM-MOD 16:0-rel 
nw/wsj/10/wsj_1026.parse 49 7 gold bring-v 11.3-1 Bringing bring.01 1 ----- 2:1*4:1*5:1-ARG0=Instrument 6:1-ARGM-ADV 7:0-rel 8:1-ARG1=Theme 9:1-ARG2=Destination 12:1-ARGM-TMP 2:1*4:1-LINK-SLC 
nw/wsj/10/wsj_1026.parse 49 18 gold say-v 37.7-1 IN say.01 1 ----- 0:2*20:1-ARG1=Topic 17:1-ARG0=Agent 18:0-rel 
nw/wsj/10/wsj_1026.parse 50 29 gold deliver-v 11.1 Delivery deliver.01 5 ----- 10:2*27:1-ARG0=Agent 29:0-rel 30:1-ARG1=Theme 31:1-ARGM-MNR 
nw/wsj/10/wsj_1026.parse 50 37 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 32:1*34:1*38:1-ARG1=Theme;Goods 35:1-ARG0=Agent;Seller 37:0-rel 39:1-ARGM-TMP 32:1*34:1-LINK-SLC 
nw/wsj/10/wsj_1026.parse 51 5 gold have-v 100 IN have.03 1 ----- 0:1-ARGM-DIS 2:1-ARG0=Pivot 5:0-rel 6:4-ARG1=Theme 
nw/wsj/10/wsj_1026.parse 51 15 gold store-v 15.2 Storing store.01 1 ----- 6:3*16:1-ARG1=Theme 15:0-rel 17:1-ARGM-LOC 6:3*16:1-LINK-PSV 
nw/wsj/10/wsj_1026.parse 51 26 gold negotiate-v 36.1 NF negotiate.01 1 ----- 21:1*23:1*27:1-ARG2=Theme 24:1-ARG0=Agent 26:0-rel 28:1-ARG1=Co-Agent 32:1-ARGM-TMP 21:1*23:1-LINK-SLC 
nw/wsj/10/wsj_1026.parse 52 1 gold think-v 29.9-2 IN think.01 1 ----- 0:1-ARG0 1:0-rel 2:1-ARG1 
nw/wsj/10/wsj_1026.parse 52 7 gold have-v 100 IN have.03 1 ----- 5:1-ARG0=Pivot 7:0-rel 8:2-ARG1=Theme 
nw/wsj/10/wsj_1026.parse 52 10 gold state-v 37.7-1 Statement state.01 1 ----- 8:1-ARG0=Agent;Speaker 10:0-rel 11:1-ARG1=Topic;Message 
nw/wsj/10/wsj_1026.parse 52 15 gold bring-v 11.3-1 Bringing bring.01 1 ----- 12:1-ARG0=Instrument 13:0-ARGM-MOD 14:0-ARGM-NEG 15:0-rel 16:1-ARG1=Theme 18:1-ARG2=Destination 20:1-ARGM-TMP 
nw/wsj/10/wsj_1026.parse 53 6 gold permit-v 64 Make_possible_to_do permit.01 1 ----- 0:1-ARG0=Agent 3:1-ARGM-ADV 5:0-ARGM-MOD 6:0-rel 7:1-ARG2=Theme 10:2-ARG1=Theme 
nw/wsj/10/wsj_1026.parse 53 12 gold deliver-v 11.1 Delivery deliver.01 5 ----- 7:1*10:1-ARG0=Agent 12:0-rel 13:1-ARG1=Theme 16:1-ARGM-DIS 19:1-ARGM-MNR 
nw/wsj/10/wsj_1026.parse 53 20 gold exist-v 47.1-1 Existence exist.01 null ----- 20:0-rel 21:0,22:0-ARG1=Theme;Entity 
