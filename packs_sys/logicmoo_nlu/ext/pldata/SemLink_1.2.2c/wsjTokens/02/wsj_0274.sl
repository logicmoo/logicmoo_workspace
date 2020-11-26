nw/wsj/02/wsj_0274.parse 0 2 gold find-v 13.5.1 IN find.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:3-ARG1=Theme 
nw/wsj/02/wsj_0274.parse 1 1 gold increase-v 45.6-1 Cause_change_of_position_on_a_scale increase.01 1 ----- 1:0-rel 2:0-ARG1=Patient;Item 
nw/wsj/02/wsj_0274.parse 1 8 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:2-ARG0=Agent;Buyer 8:0,9:1-rel 10:2-ARG1=Theme;Goods 
nw/wsj/02/wsj_0274.parse 1 17 gold pool-v 22.1-2-1 NF pool.01 null ----- 10:1*13:1*14:1*18:1-ARG1=Patient 17:0-rel 10:1*13:1-LINK-SLC 
nw/wsj/02/wsj_0274.parse 1 20 gold package-v 22.3-2-1 Placing package.01 1 ----- 10:1*13:1*14:1*21:1-ARG1=Patient 20:0-rel 22:1-ARGM-PRP 10:1*13:1-LINK-SLC 
nw/wsj/02/wsj_0274.parse 1 29 gold know-v 29.5-1 IN know.01 2 ----- 25:2*30:1-ARG1=Theme 29:0-rel 31:1-ARG2=Predicate 25:2*30:1-LINK-PSV 
nw/wsj/02/wsj_0274.parse 2 11 gold issue-v 13.3 NF issue.01 null ----- 7:1*12:1-ARG1=Theme 11:0-rel 13:1-ARG0=Agent 7:1*12:1-LINK-PSV 
nw/wsj/02/wsj_0274.parse 2 36 gold flow-v 47.2 Fluidic_motion flow.01 1 ----- 0:3-ARG1=Theme;Fluid 35:1-ARGM-TMP 36:0-rel 37:1-ARGM-DIR 
nw/wsj/02/wsj_0274.parse 3 12 gold watch-v 30.2 Perception_active watch.01 null ----- 8:1*9:1*10:1-ARG0=Experiencer;Perceiver_agentive 12:0-rel 13:2-ARG1=Stimulus;Phenomenon 8:1*9:1-LINK-SLC 
nw/wsj/02/wsj_0274.parse 4 2 gold mark-v 29.1 NF mark.01 3 ----- 0:0-ARGM-DIS 1:1-ARG0=Agent 2:0-rel 3:2-ARG1=Theme 
nw/wsj/02/wsj_0274.parse 4 12 gold shun-v 52 Avoiding shun.01 1 ----- 7:1*10:1*11:1-ARG0=Agent;Agent 12:0-rel 13:1-ARG1=Theme;Undesirable_situation 15:1-ARGM-TMP 7:1*10:1-LINK-SLC 
nw/wsj/02/wsj_0274.parse 4 17 gold get-v 26.6.2 IN get.03 null ----- 7:1*10:1*11:1*16:1-ARG1=Patient 17:0-rel 18:1-ARG2=Goal 7:1*10:1-LINK-SLC 
nw/wsj/02/wsj_0274.parse 4 18 gold burn-v 40.8.3-2 Experience_bodily_harm burn.01 11 ----- 7:1*10:1*11:1*16:1*19:1-ARG1=Patient;Body_part 18:0-rel 20:1-ARG0=Experiencer;Experiencer 27:2-ARGM-TMP 7:1*10:1-LINK-SLC 
nw/wsj/02/wsj_0274.parse 5 4 gold say-v 37.7-1 IN say.01 null ----- 1:1-ARG0=Agent 2:0-ARGM-MOD 3:0-ARGM-NEG 4:0-rel 5:1-ARG1=Topic 
nw/wsj/02/wsj_0274.parse 5 20 gold make-v 26.1-1 IN make.01 null ----- 18:1-ARG0=Agent 20:0-rel 21:1-ARG1=Product 
nw/wsj/02/wsj_0274.parse 5 25 gold say-v 37.7-1 IN say.01 null ----- 1:3*26:1-ARG1=Topic 25:0-rel 27:4-ARG0=Agent 
nw/wsj/02/wsj_0274.parse 5 38 gold visit-v 36.3-1 IN visit.01 null ----- 27:3*36:1*37:1-ARG0=Agent 38:0-rel 39:1-ARG1=Co-Agent 40:2-ARGM-TMP 45:2-ARGM-PRP 27:3*36:1-LINK-SLC 
nw/wsj/02/wsj_0274.parse 5 47 gold explain-v 37.1.1 Statement explain.01 null ----- 27:3*36:1*37:1*45:1-ARG0=Agent;Speaker 47:0-rel 48:1*56:1-ARG1=Topic;Message 27:3*36:1-LINK-SLC 
nw/wsj/02/wsj_0274.parse 8 9 gold abound-v 47.5.1-2-1 NF abound.01 1 ----- 0:2-ARG1=Location 9:0-rel 
nw/wsj/02/wsj_0274.parse 9 13 gold have-v 100 IN have.03 null ----- 0:2-ARGM-TMP 4:2-ARG0=Pivot 13:0-rel 14:2-ARG1=Theme 
nw/wsj/02/wsj_0274.parse 10 7 gold claim-v 37.7-1 Statement claim.01 null ----- 0:2*5:1*6:1*8:1-ARG0=Agent;Speaker 7:0-rel 8:2-ARG1=Topic;Message 0:2*5:1-LINK-SLC 
nw/wsj/02/wsj_0274.parse 10 20 gold say-v 37.7-1 IN say.01 null ----- 0:3-ARG0=Agent 20:0-rel 21:1-ARG1=Topic 
nw/wsj/02/wsj_0274.parse 11 7 gold promise-v 13.3 Commitment promise.01 null ----- 0:0-ARGM-DIS 1:1*8:1-ARG0=Agent 5:1-ARGM-TMP 7:0-rel 8:2-ARG2=Theme 
nw/wsj/02/wsj_0274.parse 11 20 gold expect-v 62 IN expect.01 null ----- 18:0-ARGM-MOD 20:0-rel 21:2-ARG1=Theme 
nw/wsj/02/wsj_0274.parse 12 16 gold say-v 37.7-1 IN say.01 null ----- 1:2*17:1-ARG1=Topic 16:0-rel 18:2-ARG0=Agent 
nw/wsj/02/wsj_0274.parse 13 11 gold invest-v 13.4.2 NF invest.01 null ----- 1:1-ARGM-TMP 4:1-ARG0=Agent 11:0-rel 12:1-ARG1=Theme 
nw/wsj/02/wsj_0274.parse 14 3 gold say-v 37.7-1 IN say.01 null ----- 2:1-ARG0=Agent 3:0-rel 0:2-ARG1-DSP=Topic 
nw/wsj/02/wsj_0274.parse 14 10 gold help-v 72-1 Assistance help.01 null ----- 0:1-ARGM-TMP 7:1*11:1-ARG0=Agent;Helper 9:0-ARGM-MOD 10:0-rel 11:2-ARG1=Theme;Goal/Focal_entity 
nw/wsj/02/wsj_0274.parse 14 13 gold drive-v 51.4.2 Cause_motion drive.02 null ----- 7:1*11:1-ARG0=Agent 13:0-rel 14:1-ARG2 15:2-ARG1=Theme 
nw/wsj/02/wsj_0274.parse 15 13 gold act-v 29.6-1 NF act.01 null ----- 6:1*11:1-ARG0=Agent 13:0-rel 14:1-ARG1=Attribute 6:1*11:1-LINK-PRO 
nw/wsj/02/wsj_0274.parse 16 4 gold say-v 37.7-1 IN say.01 null ----- 0:0-ARGM-DIS 1:1-ARG0=Agent 4:0-rel 5:1-ARGM-MNR 6:1-ARG1=Topic 
nw/wsj/02/wsj_0274.parse 16 9 gold consider-v 29.9-1-1-1 Cogitation consider.02 null ----- 7:1*10:1-ARG0=Agent;Cognizer 9:0-rel 10:2-ARG1=Theme;Topic 
nw/wsj/02/wsj_0274.parse 16 11 gold ask-v 58.2 Request ask.02 null ----- 7:1*10:1*12:1-ARG0 11:0-rel 12:2-ARG1 
nw/wsj/02/wsj_0274.parse 16 14 gold join-v 22.1-2-1 Cause_to_amalgamate join.01 null ----- 7:1*10:1*12:1-ARG0=Agent;Agent 14:0-rel 15:1-ARG1=Patient;Part_1 
nw/wsj/02/wsj_0274.parse 16 19 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 19:0-rel 20:0-ARG0=Agent;Seller 
nw/wsj/02/wsj_0274.parse 19 26 gold assume-v 93 Adopt_selection assume.01 null ----- 0:1-ARGM-ADV 25:1-ARGM-EXT 26:0-rel 27:1-ARG1 
nw/wsj/02/wsj_0274.parse 19 31 gold support-v 31.2 Taking_sides support.01 null ----- 28:1-ARG0=Experiencer 30:0-ARGM-MOD 31:0-rel 32:1-ARG1=Stimulus 33:1-ARGM-ADV 
nw/wsj/02/wsj_0274.parse 22 9 gold exceed-v 90-1 NF exceed.01 1 ----- 0:2-ARG0=Theme 8:1-ARGM-TMP 9:0-rel 10:3-ARG1=Co-Theme 
nw/wsj/02/wsj_0274.parse 22 28 gold issue-v 13.3 NF issue.01 null ----- 25:1*29:1-ARG1=Theme 28:0-rel 25:1*29:1-LINK-PSV 
nw/wsj/02/wsj_0274.parse 23 3 gold offer-v 13.3 NF offer.01 null ----- 0:1-ARGM-DIS 2:1-ARG0=Agent 3:0-rel 4:1-ARG1=Theme 
nw/wsj/02/wsj_0274.parse 24 5 gold offer-v 13.3 NF offer.01 null ----- 0:1-ARGM-TMP 4:1-ARG0=Agent 5:0-rel 6:3-ARG1=Theme 
nw/wsj/02/wsj_0274.parse 25 9 gold discover-v 30.2 Becoming_aware discover.01 null ----- 3:1*6:1*10:1-ARG1=Stimulus;Phenomenon 7:1-ARG0=Experiencer;Cognizer 9:0-rel 11:2-ARGM-TMP 3:1*6:1-LINK-SLC 
nw/wsj/02/wsj_0274.parse 25 14 gold dip-v 9.3 NF dip.02 1 ----- 11:1*24:1-ARGM-TMP 12:1-ARG0=Agent 13:1-ARGM-TMP 14:0-rel 15:1-ARG1=Theme 17:1-ARG2=Destination 20:3-ARGM-TMP 
nw/wsj/02/wsj_0274.parse 26 22 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 13:2*20:1-ARG0=Agent;Buyer 22:0-rel 23:1-ARGM-DIR 24:1-ARG1=Theme;Goods 26:1-ARGM-TMP 13:2*20:1-LINK-PRO 
nw/wsj/02/wsj_0274.parse 27 12 gold force-v 59 NF force.01 null ----- 11:1-ARG0=Agent 12:0-rel 13:1*14:1-ARG1=Patient 14:2-ARG2=Result 
nw/wsj/02/wsj_0274.parse 27 29 gold carry-v 54.3 Bringing carry.01 null ----- 22:1*30:1-ARG1=Value 25:1-ARG0=Location 29:0-rel 
nw/wsj/02/wsj_0274.parse 28 14 gold understand-v 77 Awareness understand.01 null ----- 11:1-ARG0=Agent;Cognizer 13:0-ARGM-NEG 14:0-rel 15:1-ARG1=Theme;Content/Topic 
nw/wsj/02/wsj_0274.parse 28 20 gold say-v 37.7-1 IN say.01 null ----- 1:4*21:1-ARG1=Topic 20:0-rel 22:2-ARG0=Agent 
nw/wsj/02/wsj_0274.parse 30 1 gold compound-v 22.1-1-1 Cause_to_amalgamate compound.01 1 ----- 0:1-ARG0=Agent;Agent 1:0-rel 2:2-ARG1=Patient;Part_1 
nw/wsj/02/wsj_0274.parse 30 17 gold require-v 103 NF require.01 null ----- 15:1-ARG0=Pivot 17:0-rel 18:1-ARG2=Source 19:2-ARG1=Theme 
nw/wsj/02/wsj_0274.parse 31 21 gold pose-v 37.1.1 NF pose.02 1 ----- 17:1-ARG0=Agent 21:0-rel 22:1-ARG1=Topic 
nw/wsj/02/wsj_0274.parse 34 6 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:1-ARGM-TMP 3:1-ARG0=Agent;Buyer 6:0-rel 7:4-ARG1=Theme;Goods 
nw/wsj/02/wsj_0274.parse 34 20 gold issue-v 13.3 NF issue.01 null ----- 16:2*21:1-ARG1=Theme 20:0-rel 22:1-ARG2=Goal 16:2*21:1-LINK-PSV 
nw/wsj/02/wsj_0274.parse 34 29 gold use-v 105 IN use.01 null ----- 25:1*26:1*30:1-ARG1 27:1*31:1-ARG0 29:0-rel 31:2-ARG2 25:1*26:1-LINK-SLC 
nw/wsj/02/wsj_0274.parse 34 33 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 27:1*31:1-ARG0=Agent;Buyer 33:0-rel 34:1-ARG1=Theme;Goods 35:1-ARG2=Source 
nw/wsj/02/wsj_0274.parse 35 29 gold offer-v 13.3 NF offer.01 null ----- 8:2*26:1*27:1*30:1-ARG1=Theme 29:0-rel 31:1-ARG3=Goal 33:1-ARGM-TMP 8:2*26:1-LINK-SLC 
nw/wsj/02/wsj_0274.parse 36 18 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 null ----- 0:1-ARGM-DIS 3:2-ARG0=Cause 18:0-rel 19:2-ARG1=Patient 
nw/wsj/02/wsj_0274.parse 37 5 gold offer-v 13.3 NF offer.01 null ----- 0:1-ARG0=Agent 2:1-ARGM-TMP 5:0-rel 6:2-ARG1=Theme 
nw/wsj/02/wsj_0274.parse 37 14 gold guarantee-v 29.5-2 NF guarantee.01 2 ----- 6:1*12:1*13:1-ARG0=Agent 14:0-rel 15:1-ARG1=Predicate 6:1*12:1-LINK-SLC 
nw/wsj/02/wsj_0274.parse 37 29 gold offer-v 13.3 NF offer.01 null ----- 6:1*12:1*13:1-ARG0=Agent 29:0-rel 30:1-ARG1=Theme 6:1*12:1-LINK-SLC 
