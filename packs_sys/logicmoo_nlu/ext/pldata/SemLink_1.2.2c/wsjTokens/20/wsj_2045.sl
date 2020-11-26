nw/wsj/20/wsj_2045.parse 0 2 gold get-v 26.6.2 IN get.03 null ----- 0:1*3:1-ARG1=Patient 1:1-ARGM-TMP 2:0-rel 3:2-ARG2=Goal 5:1-ARGM-CAU 
nw/wsj/20/wsj_2045.parse 0 4 gold notice-v 30.1-1 Becoming_aware notice.01 null ----- 0:1*3:1-ARG1=Stimulus;Phenomenon 4:0-rel 
nw/wsj/20/wsj_2045.parse 0 7 gold soar-v 45.6-1 Change_position_on_a_scale soar.01 2 ----- 6:1-ARG1=Patient;Item 7:0-rel 
nw/wsj/20/wsj_2045.parse 0 9 gold plunge-v 45.6-1 Motion_directional plunge.01 2 ----- 6:1-ARG1=Patient 9:0-rel 
nw/wsj/20/wsj_2045.parse 1 6 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 0:1*2:1*3:1-ARG1 5:0-ARGM-NEG 6:0-rel 9:1-ARG2 10:1-ARGM-TMP 0:1*2:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 1 8 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 null ----- 0:1*2:1*3:1-ARG1=Patient;Item 5:0-ARGM-NEG 8:0-rel 9:1-ARG2=Extent;Difference 10:1-ARGM-TMP 0:1*2:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 1 16 gold achieve-v 55.2 Accomplishment achieve.01 1 ----- 0:2-ARG0=Agent 15:1-ARGM-TMP 16:0-rel 17:2-ARG1=Theme 
nw/wsj/20/wsj_2045.parse 1 30 gold trade-v 13.6-1 Exchange trade.01 null ----- 28:0,29:0-ARGM-MNR 30:0-rel 31:0,32:0-ARG1=Theme 
nw/wsj/20/wsj_2045.parse 3 7 gold have-v 100 IN have.03 null ----- 0:1*5:1*6:1-ARG0=Pivot 7:0-rel 8:1-ARG1=Theme 0:1*5:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 3 13 gold end-v 55.4-1 Cause_to_end end.01 null ----- 0:2-ARG1=Theme 13:0-rel 14:1-ARGM-EXT 
nw/wsj/20/wsj_2045.parse 4 3 gold post-v 11.1 Sending post.01 null ----- 0:1-ARG0=Agent;Sender 3:0-rel 4:2-ARG1=Theme;Theme 
nw/wsj/20/wsj_2045.parse 5 35 gold indicate-v 78-1 Evidence indicate.01 null ----- 25:2*30:1*31:1*33:1-ARG0=Cause;Support 35:0-rel 36:1-ARG1=Topic;Proposition 25:2*30:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 5 47 gold take-v 10.5 Removing take.01 null ----- 0:1-ARGM-ADV 13:3*21:2-ARG0=Agent;Agent/Cause 47:0-rel 48:1-ARG1=Theme;Theme 50:2-ARG2=Source;Source 
nw/wsj/20/wsj_2045.parse 6 3 gold trade-v 13.6-1 Exchange trade.01 1 ----- 0:1-ARGM-TMP 2:1-ARG1=Theme 3:0-rel 4:1-ARGM-MNR 
nw/wsj/20/wsj_2045.parse 7 1 gold try-v 61 Attempt try.01 null ----- 0:1*2:1-ARG0=Agent 1:0-rel 2:2-ARG1=Theme 5:1-ARGM-TMP 
nw/wsj/20/wsj_2045.parse 7 8 gold run-v 18.4 Impact run.07 null ----- 0:1-ARG0=Theme;Impactor 8:0-rel 9:1-ARG1=Location;Impactee 
nw/wsj/20/wsj_2045.parse 7 26 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 25:1-ARG1=Theme;Goods 26:0-rel 27:1-ARG0=Agent;Seller 
nw/wsj/20/wsj_2045.parse 7 33 gold want-v 32.1-1-1 Desiring want.01 null ----- 28:1*31:1*32:1*34:1-ARG0=Pivot;Experiencer 33:0-rel 34:2-ARG1=Theme;Event/Focal_participant 28:1*31:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 7 36 gold fix-v 54.4 Bail_decision fix.03 null ----- 28:1*31:1*32:1*34:1-ARG0=Agent 36:0-rel 37:2-ARG1=Theme 28:1*31:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 8 5 gold trade-v 13.6-1 Exchange trade.01 1 ----- 1:2-ARG1=Theme 5:0-rel 6:1-ARG3=Co-Theme 
nw/wsj/20/wsj_2045.parse 8 22 gold have-v 100 IN have.03 null ----- 1:2-ARG0=Pivot 22:0-rel 23:2-ARG1=Theme 
nw/wsj/20/wsj_2045.parse 8 33 gold say-v 37.7-1 IN say.01 null ----- 1:3*34:1-ARG1=Topic 31:1-ARG0=Agent 33:0-rel 
nw/wsj/20/wsj_2045.parse 9 15 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 1:1*13:1-ARG0=Agent;Seller 8:1*11:1*17:1-ARGM-LOC 15:0-rel 16:1-ARG1=Theme;Goods 1:1*13:1-LINK-PRO 8:1*11:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 10 14 gold veer-v 47.7 Path_shape veer.01 null ----- 0:1-ARGM-DIS 7:3-ARG0=Theme;Road 14:0-rel 15:1-ARG1=Location;Path/Area 23:1-ARGM-CAU 
nw/wsj/20/wsj_2045.parse 10 17 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 7:3*16:1-ARG0=Agent;Buyer 17:0-rel 18:2-ARG1=Theme;Goods 7:3*16:1-LINK-PRO 
nw/wsj/20/wsj_2045.parse 10 37 gold make-v 29.3 Causation make.02 null ----- 30:1*33:1*34:1-ARG0=Agent 36:0-ARGM-NEG 37:0-rel 38:3-ARG1=Theme 30:1*33:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 11 13 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 13:0-rel 14:1-ARG1=Topic 
nw/wsj/20/wsj_2045.parse 11 21 gold motivate-v 59 Cause_to_start motivate.01 1 ----- 17:1*18:1*19:1-ARG0=Agent 21:0-rel 22:2-ARG1=Patient 17:1*18:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 11 25 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 22:1-ARG0=Agent;Buyer 25:0-rel 26:1-ARG1=Theme;Goods 
nw/wsj/20/wsj_2045.parse 12 23 gold supply-v 13.4.1-1 NF supply.01 1 ----- 13:1*21:1-ARG0=Agent 23:0-rel 24:1-ARG1=Theme 25:1-ARG2=Recipient 29:2-ARGM-PRD 13:1*21:1-LINK-PRO 
nw/wsj/20/wsj_2045.parse 12 31 gold help-v 72-1 Assistance help.01 null ----- 29:1*30:1-ARG0=Agent;Helper 31:0-rel 32:1-ARG2=Beneficiary;Benefited_party 35:1-ARG1=Theme;Goal/Focal_entity 36:1-ARGM-TMP 
nw/wsj/20/wsj_2045.parse 12 41 gold say-v 37.7-1 IN say.01 null ----- 1:3*42:1-ARG1=Topic 40:1-ARG0=Agent 41:0-rel 
nw/wsj/20/wsj_2045.parse 13 15 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 9:2*13:1-ARG0=Agent;Buyer 15:0-rel 16:1-ARG1=Theme;Goods 9:2*13:1-LINK-PRO 
nw/wsj/20/wsj_2045.parse 13 19 gold say-v 37.7-1 IN say.01 null ----- 18:1-ARG0=Agent 19:0-rel 20:1-ARG1=Topic 
nw/wsj/20/wsj_2045.parse 14 3 gold bear-v 31.2 NF bear.01 null ----- 1:1-ARG0=Experiencer 2:1-ARGM-TMP 3:0-rel 4:2-ARG1=Stimulus 
nw/wsj/20/wsj_2045.parse 14 12 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 7:1*10:1*30:1-ARGM-TMP 11:1*22:1-ARG0=Agent;Buyer 12:0-rel 13:1-ARG1=Theme;Goods 14:1-ARGM-TMP 7:1*10:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 14 18 gold crash-v 18.4-1 Impact crash.01 4 ----- 15:1-ARG1=Theme;Impactor 18:0-rel 
nw/wsj/20/wsj_2045.parse 14 23 gold lose-v 13.2 NF lose.02 null ----- 11:1*22:1-ARG0=Agent 23:0-rel 24:1-ARG1=Theme 25:1-ARGM-CAU 
nw/wsj/20/wsj_2045.parse 14 29 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 null ----- 26:1-ARG1=Patient;Item 28:1-ARGM-TMP 29:0-rel 
nw/wsj/20/wsj_2045.parse 14 35 gold say-v 37.7-1 IN say.01 null ----- 1:2*36:1-ARG1=Topic 33:1-ARG0=Agent 35:0-rel 
nw/wsj/20/wsj_2045.parse 15 9 gold want-v 32.1-1-1 Desiring want.01 null ----- 6:1*10:1-ARG0=Pivot;Experiencer 8:0-ARGM-NEG 9:0-rel 10:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/20/wsj_2045.parse 16 12 gold say-v 37.7-1 IN say.01 null ----- 11:1-ARG0=Agent 12:0-rel 0:2-ARG1-DSP=Topic 
nw/wsj/20/wsj_2045.parse 16 30 gold diminish-v 45.6-1 IN diminish.01 1 ----- 22:3-ARG1=Patient 30:0-rel 31:1-ARGM-TMP 
nw/wsj/20/wsj_2045.parse 17 11 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 11:0-rel 14:2-ARG1=Topic 
nw/wsj/20/wsj_2045.parse 17 26 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 16:2*23:1*28:1-ARGM-CAU 24:1-ARG0=Agent;Seller 26:0-rel 27:1-ARG1=Theme;Goods 16:2*23:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 18 7 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 2:1*4:1*9:1-ARGM-ADV 5:1-ARG0=Agent;Buyer 7:0-rel 8:1-ARG1=Theme;Goods 2:1*4:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 19 2 gold keep-v 55.6 Cause_to_continue keep.02 null ----- 0:1-ARG0=Agent 2:0-rel 6:2-ARG1=Theme 
nw/wsj/20/wsj_2045.parse 20 34 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 31:1-ARG0=Agent;Seller 33:1-ARGM-MNR 34:0-rel 35:1-ARG1=Theme;Goods 
nw/wsj/20/wsj_2045.parse 20 39 gold say-v 37.7-1 IN say.01 null ----- 0:3*40:1-ARG1=Topic 37:1-ARG0=Agent 39:0-rel 
nw/wsj/20/wsj_2045.parse 21 4 gold know-v 29.5-1 IN know.01 1 ----- 1:1-ARG0=Agent 3:0-ARGM-NEG 4:0-rel 5:2-ARG1=Theme 
nw/wsj/20/wsj_2045.parse 21 7 gold mean-v 29.5-1 NF mean.01 null ----- 5:1*8:1-ARG1=Theme 6:1-ARG0=Agent 7:0-rel 9:1-ARGM-TMP 
nw/wsj/20/wsj_2045.parse 21 37 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 28:1*34:1*38:1-ARG2 35:1-ARG1 37:0-rel 39:1-ARGM-TMP 28:1*34:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 21 48 gold say-v 37.7-1 IN say.01 null ----- 1:3*49:1-ARG1=Topic 47:1-ARG0=Agent 48:0-rel 
nw/wsj/20/wsj_2045.parse 22 15 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 15:0-rel 16:0,17:0-ARG1 
nw/wsj/20/wsj_2045.parse 22 29 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 6:1*28:1-ARG0=Agent;Seller 29:0-rel 6:1*28:1-LINK-PRO 
nw/wsj/20/wsj_2045.parse 23 2 gold observe-v 29.5-2 Statement observe.02 null ----- 0:1-ARG0=Agent;Speaker 2:0-rel 3:1-ARG1=Predicate;Message 
nw/wsj/20/wsj_2045.parse 23 12 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 4:1*10:1*11:1-ARG1 12:0-rel 13:1-ARG2 14:1-ARGM-TMP 4:1*10:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 24 5 gold react-v 31.3-9 NF react.01 1 ----- 1:1-ARG0=Experiencer 4:0-ARGM-NEG 5:0-rel 6:1-ARGM-EXT 7:1-ARG1=Stimulus 
nw/wsj/20/wsj_2045.parse 24 19 gold say-v 37.7-1 IN say.01 null ----- 1:3*20:1-ARG1=Topic 18:1-ARG0=Agent 19:0-rel 
nw/wsj/20/wsj_2045.parse 27 3 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 0:2-ARG1 3:0-rel 4:1-ARG2 5:1-ARGM-LOC 8:1-ARGM-TMP 
nw/wsj/20/wsj_2045.parse 27 13 gold try-v 61 Attempt try.01 null ----- 9:2*14:1-ARG0=Agent 13:0-rel 14:2-ARG1=Theme 
nw/wsj/20/wsj_2045.parse 27 16 gold assess-v 34.1 Assessing assess.01 1 ----- 9:2*14:1-ARG0=Agent;Assessor 16:0-rel 17:2-ARG1=Theme;Phenomenon 
nw/wsj/20/wsj_2045.parse 28 4 gold plummet-v 45.6-1 Change_position_on_a_scale plummet.01 1 ----- 1:2-ARG1=Patient;Item 4:0-rel 5:1-ARGM-TMP 
nw/wsj/20/wsj_2045.parse 29 15 gold expect-v 62 IN expect.01 null ----- 0:2-ARGM-TMP 12:1,16:2-ARG1=Theme 15:0-rel 19:1-ARGM-TMP 
nw/wsj/20/wsj_2045.parse 29 18 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 12:1*16:1-ARG1 18:0-rel 19:1-ARGM-TMP 
nw/wsj/20/wsj_2045.parse 31 4 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 0:1-ARG1 3:1-ARGM-DIS 4:0-rel 
nw/wsj/20/wsj_2045.parse 34 2 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 0:1-ARG1 2:0-rel 3:1-ARGM-CAU 
nw/wsj/20/wsj_2045.parse 36 15 gold ask-v 58.2 Request ask.02 null ----- 0:1-ARGM-ADV 5:1-ARG0 15:0-rel 16:1*17:1-ARG2 17:2-ARG1 
nw/wsj/20/wsj_2045.parse 36 19 gold accept-v 77 IN accept.01 null ----- 16:1*17:1-ARG0 19:0-rel 20:2-ARG1 
nw/wsj/20/wsj_2045.parse 37 3 gold perceive-v 30.2 Categorization perceive.01 3 ----- 1:1*4:1-ARG1=Stimulus 3:0-rel 5:1-ARG2 
nw/wsj/20/wsj_2045.parse 37 19 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 6:1*9:1*10:1-ARG0=Agent;Buyer 11:0-ARGM-MOD 19:0-rel 20:1-ARG1=Theme;Goods 6:1*9:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 37 26 gold say-v 37.7-1 IN say.01 null ----- 1:2*27:1-ARG1=Topic 24:1-ARG0=Agent 26:0-rel 
nw/wsj/20/wsj_2045.parse 38 9 gold help-v 72-1 Assistance help.01 null ----- 2:2*7:1*8:1-ARG0=Agent;Helper 9:0-rel 10:1-ARG1=Theme;Goal/Focal_entity 2:2*7:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 38 15 gold say-v 37.7-1 IN say.01 null ----- 0:2*16:1-ARG1=Topic 13:1-ARG0=Agent 15:0-rel 
nw/wsj/20/wsj_2045.parse 40 10 gold cause-v 27 Causation cause.01 1 ----- 1:1-ARGM-ADV 7:1-ARG0=Cause;Cause 9:0-ARGM-NEG 10:0-rel 11:1-ARG1=Theme;Effect 
nw/wsj/20/wsj_2045.parse 41 4 gold trade-v 13.6-1 Exchange trade.01 1 ----- 0:1-ARG1=Theme 4:0-rel 5:1-ARG3=Co-Theme 
nw/wsj/20/wsj_2045.parse 41 20 gold say-v 37.7-1 IN say.01 null ----- 0:2*21:1-ARG1=Topic 19:1-ARG0=Agent 20:0-rel 
nw/wsj/20/wsj_2045.parse 43 34 gold hurt-v 31.1 Cause_harm hurt.01 null ----- 26:1*29:1*30:1-ARG0=Stimulus;Agent/Cause 31:1-ARGM-DIS 33:0-ARGM-MOD 34:0-rel 35:2-ARG1=Experiencer;Victim 26:1*29:1-LINK-SLC 
nw/wsj/20/wsj_2045.parse 44 8 gold drop-v 45.6-1 Change_position_on_a_scale drop.01 null ----- 0:2-ARG1=Patient;Item 8:0-rel 9:1-ARG2=Extent;Difference 10:1-ARGM-TMP 
nw/wsj/20/wsj_2045.parse 45 5 gold support-v 31.2 Taking_sides support.01 null ----- 0:2-ARG1=Stimulus 4:1-ARGM-ADV 5:0-rel 6:1-ARGM-TMP 7:1-ARG0=Experiencer 
nw/wsj/20/wsj_2045.parse 45 13 gold make-v 26.1-1 IN make.01 null ----- 10:1-ARG0=Agent 13:0-rel 14:1-ARG1=Product 
nw/wsj/20/wsj_2045.parse 45 20 gold concern-v 86.2-1 Topic concern.02 null ----- 18:1-ARG0=Theme 20:0-rel 21:1-ARG1=Co-Theme 
nw/wsj/20/wsj_2045.parse 47 11 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 0:2-ARG1 11:0-rel 12:1-ARGM-LOC 
nw/wsj/20/wsj_2045.parse 47 19 gold interest-v 31.1 Experiencer_obj interest.01 null ----- 15:1*20:1-ARG1=Experiencer;Experiencer 19:0-rel 21:1-ARG2 
nw/wsj/20/wsj_2045.parse 47 23 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 15:1*22:1-ARG0=Agent;Buyer 23:0-rel 24:1-ARG2=Source 30:4-ARG1=Theme;Goods 15:1*22:1-LINK-PRO 
nw/wsj/20/wsj_2045.parse 49 15 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1-ARGM-TMP 4:4;18:1-ARG1=Theme;Goods 15:0-rel 17:1-ARGM-LOC 
nw/wsj/20/wsj_2045.parse 50 3 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 0:2-ARG1 3:0-rel 4:1-ARG2 5:1-ARGM-ADV 
nw/wsj/20/wsj_2045.parse 50 8 gold settle-v 46 Colonization settle.03 null ----- 6:1-ARG1=Theme 8:0-rel 9:1-ARGM-MNR 
nw/wsj/20/wsj_2045.parse 50 9 gold mix-v 22.1-1-1 Cause_to_amalgamate mix.01 null ----- 6:1-ARG1=Patient;Part_1 9:0-rel 
