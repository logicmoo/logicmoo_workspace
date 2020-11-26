nw/wsj/16/wsj_1677.parse 0 2 gold get-v 26.6.2 IN get.04 null ----- 0:1-ARGM-TMP 1:1-ARG0=Agent 2:0-rel 3:2-ARG1=Patient 
nw/wsj/16/wsj_1677.parse 0 5 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 3:1-ARG0=Agent;Buyer 5:0-rel 6:1-ARG1=Theme;Goods 7:1-ARGM-PNC 
nw/wsj/16/wsj_1677.parse 0 10 gold get-v 26.6.2 IN get.04 null ----- 8:1-ARG0=Agent 9:0-ARGM-MOD 10:0-rel 11:1-ARG1=Patient 
nw/wsj/16/wsj_1677.parse 1 3 gold give-v 13.1-1 Giving give.01 null ----- 0:1-ARGM-TMP 1:1-ARG0=Agent;Donor 3:0-rel 4:1-ARG2=Recipient;Recipient 5:2-ARG1=Theme;Theme 
nw/wsj/16/wsj_1677.parse 2 2 gold plan-v 62 Purpose plan.01 null ----- 0:1-ARGM-TMP 1:1*3:1-ARG0=Experiencer;Agent 2:0-rel 3:2-ARG1 
nw/wsj/16/wsj_1677.parse 2 5 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 1:1*3:1-ARG0=Agent;Seller 5:0-rel 6:1-ARG2=Recipient 7:2-ARG1=Theme;Goods 
nw/wsj/16/wsj_1677.parse 2 10 gold sift-v 23.3 Scouring sift.01 2 ----- 7:1*8:1*9:1-ARG0=Agent 10:0-rel 11:1-ARG1=Patient 15:2-ARGM-PRP 7:1*8:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 2 17 gold give-v 13.1-1 Giving give.01 null ----- 7:1*8:1*9:1*15:1-ARG0=Agent;Donor 17:0-rel 18:1-ARG2=Recipient;Recipient 19:2-ARG1=Theme;Theme 7:1*8:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 2 22 gold want-v 32.1-1-1 Desiring want.01 null ----- 20:1*23:1-ARG0=Pivot;Experiencer 21:1-ARGM-ADV 22:0-rel 23:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/16/wsj_1677.parse 2 25 gold know-v 29.5-1 IN know.01 1 ----- 19:1*26:1-ARG1=Theme 20:1*23:1-ARG0=Agent 25:0-rel 
nw/wsj/16/wsj_1677.parse 3 2 gold range-v 47.7 NF range.01 1 ----- 0:1-ARG1=Theme 2:0-rel 3:1-ARG3 10:1-ARG4 
nw/wsj/16/wsj_1677.parse 3 14 gold sit-v 47.6 Being_located sit.01 null ----- 11:1*12:1*13:1-ARG1=Theme 14:0-rel 15:1-ARG2=Location 11:1*12:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 3 20 gold pick-v 13.5.1 Choosing pick.01 null ----- 11:1*12:1*13:1-ARG0=Agent;Cognizer 20:0-rel 21:2-ARG1=Theme;Chosen 25:1-ARG2=Source 11:1*12:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 3 23 gold select-v 13.5.2 Choosing select.01 1 ----- 23:0-rel 24:0-ARG1=Theme;Chosen 
nw/wsj/16/wsj_1677.parse 4 6 gold want-v 32.1-1-1 Desiring want.01 null ----- 4:1*7:1-ARG1=Theme;Event/Focal_participant 5:1-ARG0=Pivot;Experiencer 6:0-rel 
nw/wsj/16/wsj_1677.parse 4 10 gold say-v 37.7-1 IN say.01 null ----- 1:2*11:1-ARG1=Topic 10:0-rel 12:2-ARG0=Agent 
nw/wsj/16/wsj_1677.parse 4 25 gold spot-v 30.2 Becoming_aware spot.01 1 ----- 20:1*23:1*24:1-ARG0=Experiencer;Cognizer 25:0-rel 26:1-ARG1=Stimulus;Phenomenon 20:1*23:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 5 13 gold find-v 84 IN find.03 null ----- 1:1*11:1-ARG0=Agent 13:0,14:1-rel 15:2-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 5 18 gold need-v 32.1-1-1 Needing need.01 null ----- 15:1*19:1-ARG1=Theme 16:1-ARG0=Pivot 17:1-ARGM-ADV 18:0-rel 
nw/wsj/16/wsj_1677.parse 6 26 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 26:0-rel 29:2-ARG1=Topic 
nw/wsj/16/wsj_1677.parse 6 33 gold start-v 55.1-1 Activity_start start.01 null ----- 29:1*34:1-ARG0=Agent;Agent 33:0-rel 34:2-ARG1=Theme;Activity 40:1-ARGM-CAU 
nw/wsj/16/wsj_1677.parse 6 44 gold have-v 100 IN have.03 null ----- 41:1-ARG0=Pivot 43:0-ARGM-NEG 44:0-rel 45:2-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 7 3 gold start-v 55.1-1 Activity_start start.01 null ----- 0:1-ARG0=Agent;Agent 3:0-rel 4:3-ARG1=Theme;Activity 
nw/wsj/16/wsj_1677.parse 7 11 gold call-v 29.3 IN call.01 null ----- 4:1*12:1-ARG1=Theme 11:0-rel 13:2-ARG2=Result 4:1*12:1-LINK-PRO 
nw/wsj/16/wsj_1677.parse 7 18 gold provide-v 13.4.1-2 Supply provide.01 null ----- 4:2*16:1*17:1-ARG0=Agent 18:0-rel 19:2-ARG1=Theme 4:2*16:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 8 12 gold say-v 37.7-1 IN say.01 null ----- 1:2*13:1-ARG1=Topic 11:1-ARG0=Agent 12:0-rel 
nw/wsj/16/wsj_1677.parse 9 6 gold carry-v 11.4 Bringing carry.01 null ----- 0:2-ARG0=Agent 6:0-rel 7:1-ARG1=Theme 11:1-ARGM-MNR 12:1-ARGM-DIR 
nw/wsj/16/wsj_1677.parse 9 25 gold appear-v 48.1.1 Coming_to_be appear.01 null ----- 21:1*26:1-ARGM-TMP 22:2;27:2-ARG1=Theme;Entity 25:0-rel 
nw/wsj/16/wsj_1677.parse 10 3 gold develop-v 26.1 IN develop.02 null ----- 0:1*4:1-ARG1=Product 3:0-rel 5:1-ARG0=Agent 0:1*4:1-LINK-PSV 
nw/wsj/16/wsj_1677.parse 10 20 gold scan-v 30.2 IN scan.01 1 ----- 0:2*23:1-ARG0=Experiencer 20:0-rel 21:1-ARG1=Stimulus 23:2-ARGM-MNR 
nw/wsj/16/wsj_1677.parse 11 17 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 17:0-rel 18:1-ARG1=Topic 
nw/wsj/16/wsj_1677.parse 11 20 gold use-v 105 IN use.01 null ----- 19:1*22:1-ARG0 20:0-rel 21:1-ARG1 22:2-ARG2 
nw/wsj/16/wsj_1677.parse 11 24 gold track-v 35.3 NF track.01 2 ----- 19:1*22:1-ARG0=Agent 24:0-rel 25:1-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 12 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/16/wsj_1677.parse 12 4 gold tell-v 37.2-1 Request tell.01 null ----- 3:1-ARG0=Agent;Speaker 4:0-rel 5:1*6:1-ARG2=Recipient;Addressee 6:2-ARG1=Topic;Message 
nw/wsj/16/wsj_1677.parse 13 8 gold catch-v 13.5.1 NF catch.01 null ----- 6:1-ARG0=Agent 8:0-rel 9:1-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 13 13 gold say-v 37.7-1 IN say.01 null ----- 1:2*14:1-ARG1=Topic 12:1-ARG0=Agent 13:0-rel 
nw/wsj/16/wsj_1677.parse 14 11 gold limit-v 76 NF limit.01 null ----- 11:0-rel 12:0-ARG1=Patient 
nw/wsj/16/wsj_1677.parse 14 28 gold include-v 65 NF include.01 null ----- 20:2*29:1-ARG1=Theme 28:0-rel 
nw/wsj/16/wsj_1677.parse 16 8 gold try-v 61 Attempt try.01 null ----- 1:1*5:1*6:1-ARG0=Agent 8:0-rel 9:1-ARG1=Theme 1:1*5:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 17 15 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 15:0-rel 18:2-ARG1=Topic 
nw/wsj/16/wsj_1677.parse 17 20 gold see-v 30.1-1 Perception_experience see.01 null ----- 18:1-ARG0=Experiencer;Perceiver_passive 20:0-rel 21:3-ARG1=Stimulus;Phenomenon 
nw/wsj/16/wsj_1677.parse 18 8 gold see-v 30.1-1 Grasp see.01 null ----- 6:1-ARG0=Experiencer 8:0-rel 9:2-ARG1=Stimulus 
nw/wsj/16/wsj_1677.parse 18 18 gold use-v 105 IN use.01 null ----- 9:1*10:1*19:1-ARG1 14:2-ARG0 18:0-rel 9:1*10:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 19 1 gold use-v 105 IN use.01 null ----- 0:1*6:1-ARG0 1:0-rel 2:1-ARG1 6:2-ARG2 
nw/wsj/16/wsj_1677.parse 19 8 gold carry-v 11.4 Bringing carry.01 null ----- 0:1*6:1-ARG0=Agent 8:0-rel 9:2-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 19 11 gold provide-v 13.4.1-2 Supply provide.01 null ----- 9:1*12:1-ARG1=Theme 11:0-rel 13:1-ARG0=Agent 9:1*12:1-LINK-PSV 
nw/wsj/16/wsj_1677.parse 19 33 gold carry-v 11.4 Bringing carry.01 null ----- 28:1*31:1*32:1-ARG0=Agent 33:0-rel 34:1-ARG1=Theme 28:1*31:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 20 3 gold attach-v 22.3-2-1 Inchoative_attaching attach.01 null ----- 0:1*4:1-ARG1=Patient;Connector 3:0-rel 5:1-ARG1=Patient;Connector 0:1*4:1-LINK-PSV 
nw/wsj/16/wsj_1677.parse 20 11 gold receive-v 13.5.2 Receiving receive.01 null ----- 0:2-ARG0=Agent;Donor 11:0-rel 12:1-ARG1=Theme;Theme 
nw/wsj/16/wsj_1677.parse 21 3 gold devise-v 55.5-1 Coming_up_with devise.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:2-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 21 10 gold sort-v 29.10 Differentiation sort.01 1 ----- 4:1*7:1*8:1-ARG0=Agent 10:0-rel 11:1-ARG1=Theme 15:1-ARGM-TMP 4:1*7:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 22 12 gold adapt-v 26.9 Adjusting adapt.01 1 ----- 0:2-ARG0=Agent 12:0-rel 13:2*22:1-ARG1=Patient 22:2-ARGM-PRP 
nw/wsj/16/wsj_1677.parse 22 18 gold call-v 29.3 IN call.01 null ----- 13:1*19:1-ARG1=Theme 18:0-rel 20:2-ARG2=Result 13:1*19:1-LINK-PRO 
nw/wsj/16/wsj_1677.parse 23 2 gold select-v 13.5.2 Choosing select.01 1 ----- 0:1-ARG0=Agent;Cognizer 1:1-ARGM-MNR 2:0-rel 3:1,4:1-ARG1=Theme;Chosen 7:1-ARG3 
nw/wsj/16/wsj_1677.parse 24 6 gold need-v 32.1-1-1 Needing need.01 null ----- 1:1*2:1*3:1-ARG0=Pivot 5:0-ARGM-NEG 6:0-rel 7:1-ARG1=Theme 10:2-ARGM-TMP 1:1*2:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 24 17 gold offer-v 13.3 NF offer.01 null ----- 0:1-ARG3=Goal 14:1-ARG0=Agent 17:0-rel 18:1-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 25 11 gold use-v 105 IN use.01 null ----- 0:2*24:1-ARG0 11:0-rel 12:2-ARG1 24:2-ARG2 
nw/wsj/16/wsj_1677.parse 25 14 gold develop-v 26.1 IN develop.02 null ----- 12:1*15:1-ARG1=Product 14:0-rel 16:1-ARG0=Agent 12:1*15:1-LINK-PSV 
nw/wsj/16/wsj_1677.parse 25 32 gold send-v 11.1-1 Sending send.01 null ----- 28:1*30:1*33:1-ARG1=Theme;Theme 31:1-ARG0=Agent;Sender 32:0-rel 34:1-ARGM-MNR 35:1-ARG2=Destination;Goal/Recipient 37:1-ARGM-TMP 28:1*30:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 26 9 gold take-v 10.5 IN take.01 null ----- 4:1*7:1*8:1-ARG0=Agent 9:0-rel 10:2-ARG1=Theme 4:1*7:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 26 17 gold turn-v 26.6.2 Cause_change turn.02 null ----- 4:1*7:1*8:1-ARG0=Agent 17:0-rel 18:1-ARG1=Patient 19:1-ARG2=Goal 4:1*7:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 26 24 gold say-v 37.7-1 IN say.01 null ----- 1:2*25:1-ARG1=Topic 24:0-rel 26:2-ARG0=Agent 
nw/wsj/16/wsj_1677.parse 27 13 gold have-v 100 IN have.03 null ----- 0:2-ARG0=Pivot 12:0-ARGM-MOD 13:0-rel 14:2-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 27 24 gold provide-v 13.4.1-2 Supply provide.01 null ----- 0:2-ARG0=Agent 24:0-rel 25:2-ARG1=Theme 30:1-ARGM-LOC 
nw/wsj/16/wsj_1677.parse 29 2 gold want-v 32.1-1-1 Desiring want.01 null ----- 1:1-ARG0=Pivot;Experiencer 2:0-rel 3:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/16/wsj_1677.parse 29 17 gold say-v 37.7-1 IN say.01 null ----- 1:2*18:1-ARG1=Topic 15:1-ARG0=Agent 17:0-rel 
nw/wsj/16/wsj_1677.parse 30 2 gold track-v 35.3 NF track.01 2 ----- 0:1-ARG0=Agent 1:1-ARGM-DIS 2:0-rel 3:2-ARG1=Theme 7:1-ARG2 
nw/wsj/16/wsj_1677.parse 31 4 gold go-v 47.7 Motion go.01 null ----- 0:2*9:1-ARG1=Theme;Theme 4:0-rel 5:1-ARG4 8:1-ARGM-MNR 
nw/wsj/16/wsj_1677.parse 31 10 gold use-v 105 IN use.01 null ----- 0:2*9:1-ARG0 10:0-rel 11:1-ARG1 
nw/wsj/16/wsj_1677.parse 32 15 gold supply-v 13.4.1-1 NF supply.01 1 ----- 0:1-ARGM-ADV 11:1-ARG0=Agent 13:0-ARGM-MOD 14:1-ARGM-DIS 15:0-rel 16:2-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 32 19 gold mention-v 37.7-1 Statement mention.01 1 ----- 16:1*17:1*18:1-ARG0=Agent;Speaker 19:0-rel 21:2-ARG1=Topic;Message 16:1*17:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 32 34 gold say-v 37.7-1 IN say.01 null ----- 0:2*36:1-ARG1=Topic 32:1-ARG0=Agent 34:0-rel 
nw/wsj/16/wsj_1677.parse 33 7 gold weigh-v 54.1-1 Dimension weigh.01 3 ----- 0:1-ARGM-DIS 2:2-ARG0=Agent 6:0-ARGM-MOD 7:0-rel 8:2-ARG1=Theme;Object 13:1-ARG2 
nw/wsj/16/wsj_1677.parse 33 13 gold base-v 97.1 NF base.02 null ----- 13:0-rel 14:1-ARG2=Source 
nw/wsj/16/wsj_1677.parse 33 19 gold match-v 22.2-1 Compatibility match.01 1 ----- 15:1*25:1-ARGM-MNR 17:1-ARG1=Patient;Item_1/Items 19:0-rel 20:2-ARG1=Patient;Item_1/Items 
nw/wsj/16/wsj_1677.parse 34 1 gold compare-v 22.2-2 Evaluative_comparison compare.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:2-ARG1=Patient 
nw/wsj/16/wsj_1677.parse 34 18 gold get-v 13.5.1-1 IN get.01 null ----- 11:2-ARG0=Agent 18:0-rel 19:1-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 35 7 gold appear-v 48.1.1 Coming_to_be appear.01 null ----- 3:1*11:1-ARGM-TMP 5:1-ARG1=Theme;Entity 7:0-rel 8:1-ARGM-LOC 
nw/wsj/16/wsj_1677.parse 35 17 gold appear-v 48.1.1 Coming_to_be appear.01 null ----- 14:1*23:1-ARGM-TMP 16:1-ARG1=Theme;Entity 17:0-rel 18:1-ARGM-LOC 
nw/wsj/16/wsj_1677.parse 37 25 gold start-v 55.1-1 Process_start start.01 null ----- 8:2*23:1*24:1-ARG0=Agent 25:0-rel 26:1-ARG1=Theme;Event 28:1-ARGM-TMP 8:2*23:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 37 31 gold rely-v 70 Reliance rely.01 1 ----- 0:2-ARG0=Agent;Protagonist 31:0-rel 32:1-ARG1=Theme;Means/Instrument/Intermediary/Benefit/Purpose 36:2-ARG2 43:1-ARGM-PRP 
nw/wsj/16/wsj_1677.parse 37 38 gold code-v 29.10 NF code.01 1 ----- 33:1*36:1-ARG0=Agent 38:0-rel 39:1-ARG1=Theme 41:1-ARGM-MNR 
nw/wsj/16/wsj_1677.parse 37 47 gold select-v 13.5.2 Choosing select.01 1 ----- 0:2*45:1-ARG0=Agent;Cognizer 47:0-rel 48:2-ARG1=Theme;Chosen 
nw/wsj/16/wsj_1677.parse 38 3 gold find-v 13.5.1 IN find.01 null ----- 1:1-ARG0=Agent 3:0-rel 4:2-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 39 7 gold say-v 37.7-1 IN say.01 null ----- 0:2*8:1-ARG1=Topic 7:0-rel 9:2-ARG0=Agent 
nw/wsj/16/wsj_1677.parse 40 3 gold write-v 25.2 Text_creation write.01 null ----- 0:1-ARG0=Agent;Author 2:1-ARGM-DIS 3:0-rel 4:2-ARG1=Theme;Text 
nw/wsj/16/wsj_1677.parse 41 5 gold code-v 29.10 NF code.02 null ----- 1:1*6:1-ARG1=Theme 5:0-rel 
nw/wsj/16/wsj_1677.parse 41 8 gold put-v 9.1-2 IN put.01 null ----- 1:1*9:1-ARG1=Theme 8:0-rel 10:1-ARG2=Destination 
nw/wsj/16/wsj_1677.parse 41 18 gold pick-v 13.5.1 Choosing pick.01 null ----- 0:1-ARGM-TMP 15:2-ARG0=Agent;Cognizer 18:0-rel 19:2-ARG1=Theme;Chosen 
nw/wsj/16/wsj_1677.parse 41 26 gold lay-v 9.2 Placing lay.01 null ----- 0:1-ARGM-TMP 15:2-ARG0=Agent;Agent/Cause 26:0-rel 27:1-ARG1=Theme;Theme 28:1-ARGM-PRD 29:1-ARGM-MNR 
nw/wsj/16/wsj_1677.parse 41 44 gold send-v 11.1-1 Sending send.01 null ----- 41:1*45:1-ARG1=Theme;Theme 44:0-rel 46:1-ARGM-MNR 47:1-ARGM-DIR 50:1-ARG2=Destination;Goal/Recipient 
nw/wsj/16/wsj_1677.parse 42 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/16/wsj_1677.parse 42 11 gold send-v 11.1-1 Sending send.01 null ----- 4:2-ARG0=Agent;Sender 8:0-ARGM-MOD 11:0-rel 12:3-ARG1=Theme;Theme 
nw/wsj/16/wsj_1677.parse 43 7 gold see-v 30.1-1 Perception_experience see.01 null ----- 0:1*4:1*5:1-ARG0=Experiencer;Perceiver_passive 6:1-ARGM-NEG 7:0-rel 8:1-ARG1=Stimulus;Phenomenon 0:1*4:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 43 14 gold sort-v 29.10 Differentiation sort.01 1 ----- 0:2*12:1-ARG0=Agent 14:0-rel 15:1-ARG1=Theme 19:1-ARGM-MNR 
nw/wsj/16/wsj_1677.parse 44 19 gold distribute-v 13.2-1 Dispersal distribute.01 null ----- 5:2,7:1*20:1-ARG1=Theme 19:0-rel 21:1-ARG2=Recipient 5:2*20:1-LINK-PSV 
nw/wsj/16/wsj_1677.parse 45 3 gold interconnect-v 22.2-2-1 NF interconnect.01 null ----- 3:0-rel 4:0-ARG1=Patient 
nw/wsj/16/wsj_1677.parse 45 5 gold make-v 29.3 Causation make.02 null ----- 1:1-ARG0=Agent 5:0-rel 6:3-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 45 11 gold sort-v 29.10 Differentiation sort.02 1 ----- 9:1-ARG0=Agent 11:0,12:1-rel 13:3-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 45 24 gold say-v 37.7-1 IN say.01 null ----- 1:2*25:1-ARG1=Topic 24:0-rel 26:2-ARG0=Agent 
nw/wsj/16/wsj_1677.parse 45 37 gold start-v 55.1-1 Activity_start start.01 null ----- 29:1*34:1*35:1-ARG0=Agent;Agent 37:0-rel 38:2-ARG1=Theme;Activity 29:1*34:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 45 44 gold cope-v 83-1 NF cope.01 1 ----- 38:1*41:1*42:1-ARG0=Agent 44:0-rel 45:1-ARG1=Theme 38:1*41:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 46 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/16/wsj_1677.parse 46 11 gold license-v 101 NF license.01 1 ----- 4:2*25:1-ARG2 11:0-rel 12:2-ARG1=Theme 18:1-ARG0=Agent 
nw/wsj/16/wsj_1677.parse 46 13 gold know-v 29.5-1 IN know.01 2 ----- 12:1*14:1-ARG1=Theme 13:0-rel 15:1-ARG2=Predicate 12:1*14:1-LINK-PSV 
nw/wsj/16/wsj_1677.parse 46 24 gold plan-v 62 Purpose plan.01 null ----- 4:2*25:1-ARG0=Experiencer;Agent 24:0-rel 25:2-ARG1 
nw/wsj/16/wsj_1677.parse 46 27 gold develop-v 26.1 IN develop.02 null ----- 4:2*25:1-ARG0=Agent 27:0-rel 28:1-ARG1=Product 29:1-ARG4 
nw/wsj/16/wsj_1677.parse 47 3 gold devise-v 55.5-1 Coming_up_with devise.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:2-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 47 11 gold categorize-v 29.10 NF categorize.01 null ----- 7:1*12:1-ARG1 10:1-ARGM-MNR 11:0-rel 13:1-ARG2 16:1-ARGM-ADV 
nw/wsj/16/wsj_1677.parse 47 21 gold designate-v 29.1 IN designate.01 null ----- 21:0-rel 22:0-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 47 24 gold require-v 103 NF require.01 null ----- 17:1-ARG0=Pivot 24:0-rel 25:1-ARG1=Theme 26:1-ARGM-TMP 
nw/wsj/16/wsj_1677.parse 48 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/16/wsj_1677.parse 48 20 gold know-v 29.5-1 IN know.01 3 ----- 17:1*18:1*21:1-ARG0=Agent 20:0-rel 21:2-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 48 26 gold bother-v 31.1 NF bother.01 2 ----- 17:1*18:1*21:1-ARG0=Stimulus 26:0-rel 27:1-ARG1=Experiencer 28:1-ARGM-TMP 29:1-ARGM-TMP 
nw/wsj/16/wsj_1677.parse 49 10 gold set-v 9.1-2 IN set.01 null ----- 0:0-ARGM-DIS 1:1-ARGM-ADV 8:1-ARG0=Agent 9:0-ARGM-MOD 10:0-rel 11:1-ARG1=Theme 12:1-ARG2=Destination 
nw/wsj/16/wsj_1677.parse 49 20 gold bother-v 31.1 NF bother.01 2 ----- 17:1-ARG0=Stimulus 19:0-ARGM-NEG 20:0-rel 21:1-ARG1=Experiencer 22:1-ARG2 24:1-ARGM-TMP 
nw/wsj/16/wsj_1677.parse 50 2 gold call-v 29.3 IN call.01 null ----- 0:1*3:1-ARG1=Theme 2:0-rel 4:2-ARG2=Result 0:1*3:1-LINK-PRO 
nw/wsj/16/wsj_1677.parse 50 21 gold sort-v 29.10 Differentiation sort.01 2 ----- 0:3*18:1*19:1-ARG0=Agent 21:0-rel 22:2-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 50 23 gold send-v 11.1-1 Sending send.01 null ----- 22:1*24:1-ARG1=Theme;Theme 23:0-rel 25:1-ARGM-LOC 22:1*24:1-LINK-PSV 
nw/wsj/16/wsj_1677.parse 51 4 gold make-v 29.3 Causation make.02 null ----- 0:1*2:1*3:1-ARG0=Agent 4:0-rel 5:2-ARG1=Theme 0:1*2:1-LINK-SLC 
nw/wsj/16/wsj_1677.parse 51 10 gold sift-v 23.3 Scouring sift.01 2 ----- 8:1-ARG0=Agent 10:0-rel 11:1-ARG1=Patient 
nw/wsj/16/wsj_1677.parse 51 17 gold look-v 30.4 Appearance look.02 null ----- 15:1-ARG0=Stimulus;Phenomenon 17:0-rel 18:1-ARG1 
nw/wsj/16/wsj_1677.parse 52 15 gold allow-v 64 IN allow.01 null ----- 0:2-ARG0 15:0-rel 16:2-ARG1 
nw/wsj/16/wsj_1677.parse 52 19 gold put-v 9.1-2 IN put.01 null ----- 16:1-ARG0=Agent 19:0-rel 20:1-ARG1=Theme 22:1-ARG2=Destination 
nw/wsj/16/wsj_1677.parse 53 13 gold have-v 100 IN have.03 null ----- 0:2-ARG0=Pivot 9:1-ARGM-DIS 12:0-ARGM-MOD 13:0-rel 14:2-ARG1=Theme 21:2-ARGM-MNR 
nw/wsj/16/wsj_1677.parse 53 27 gold have-v 100 IN have.03 null ----- 23:1-ARG0=Pivot 26:0-ARGM-MOD 27:0-rel 28:1-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 54 7 gold have-v 100 IN have.03 null ----- 1:1-ARGM-ADV 4:1-ARG0=Pivot 6:0-ARGM-NEG 7:0-rel 8:2-ARG1=Theme 
nw/wsj/16/wsj_1677.parse 54 15 gold say-v 37.7-1 IN say.01 null ----- 1:2*16:1-ARG1=Topic 15:0-rel 17:2-ARG0=Agent 
