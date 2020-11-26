nw/wsj/18/wsj_1844.parse 0 15 gold announce-v 37.7-1 Statement announce.01 null ----- 0:2-ARG0=Agent;Speaker 15:0-rel 16:2-ARG1=Topic;Message 
nw/wsj/18/wsj_1844.parse 0 22 gold remove-v 10.1 Removing remove.01 null ----- 16:1*19:1*20:1-ARG0=Agent;Agent/Cause 21:1-ARGM-EXT 22:0-rel 23:1-ARG1=Theme;Theme 24:1-ARG2 16:1*19:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 0 29 gold include-v 65 NF include.01 null ----- 16:1*19:1*20:1*30:1-ARG2=Location 29:0-rel 30:2-ARG1=Theme 16:1*19:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 0 31 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 16:1*19:1*20:1*30:1-ARG0=Agent;Seller 31:0-rel 32:2-ARG1=Theme;Goods 38:1-ARG2=Recipient 16:1*19:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 1 8 gold expect-v 62 IN expect.01 null ----- 0:1*4:1*5:1*9:1-ARG1=Theme 7:1-ARGM-TMP 8:0-rel 0:1*4:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 1 11 gold transform-v 26.6.1 NF transform.01 1 ----- 0:2-ARG0=Agent 11:0-rel 12:2-ARG1=Patient 16:1-ARG3=Material 22:1-ARG2=Result 
nw/wsj/18/wsj_1844.parse 2 16 gold head-v 47.8 Leadership head.01 null ----- 3:1*17:1-ARG1=Co-Theme 16:0-rel 18:1-ARGM-MNR 19:1-ARG0=Theme 3:1*17:1-LINK-PSV 
nw/wsj/18/wsj_1844.parse 4 20 gold get-v 13.5.1-1 IN get.01 null ----- 14:1*17:1-ARGM-MNR 18:1-ARG0=Agent 20:0-rel 21:2-ARG1=Theme 14:1*17:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 4 33 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 31:1-ARG0=Agent;Buyer 33:0-rel 34:1-ARG1=Theme;Goods 35:1-ARGM-MNR 
nw/wsj/18/wsj_1844.parse 5 7 gold give-v 13.1-1 Giving give.01 null ----- 0:1-ARGM-ADV 3:1-ARGM-TMP 5:1-ARG0=Agent;Donor 7:0-rel 8:1-ARG2=Recipient;Recipient 9:2-ARG1=Theme;Theme 
nw/wsj/18/wsj_1844.parse 6 2 gold give-v 13.1-1 Giving give.01 null ----- 0:1-ARG0=Agent;Donor 1:1-ARGM-DIS 2:0-rel 3:2-ARG2=Recipient;Recipient 23:2-ARG1=Theme;Theme 
nw/wsj/18/wsj_1844.parse 6 12 gold generate-v 27 IN generate.01 1 ----- 3:1*9:1*10:1-ARG0 11:1-ARGM-TMP 12:0-rel 13:3-ARG1=Theme 3:1*9:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 6 36 gold use-v 105 IN use.01 null ----- 23:1*26:1*37:1-ARG1 27:1*34:1-ARG0 36:0-rel 23:1*26:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 8 7 gold hold-v 15.1-1 Manipulation hold.01 null ----- 2:1*4:1*5:1-ARG0=Agent;Agent 7:0-rel 8:2-ARG1=Theme;Entity 2:1*4:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 8 30 gold raise-v 9.4 NF raise.01 null ----- 2:2*28:1-ARG0=Agent 30:0-rel 31:1-ARG1=Theme 33:1-ARG4=Destination 
nw/wsj/18/wsj_1844.parse 9 5 gold raise-v 9.4 NF raise.01 null ----- 4:1-ARG0=Agent 5:0-rel 6:1-ARG4=Destination 9:2-ARG1=Theme 
nw/wsj/18/wsj_1844.parse 9 27 gold own-v 100 NF own.01 null ----- 19:1*25:1*28:1-ARG1=Theme 26:1-ARG0=Pivot 27:0-rel 29:1-ARGM-COM 19:1*25:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 10 3 gold have-v 100 IN have.03 null ----- 0:1-ARG0=Pivot 1:0-ARGM-MOD 2:1-ARGM-DIS 3:0-rel 4:1-ARG1=Theme 
nw/wsj/18/wsj_1844.parse 10 8 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:1*6:1-ARG0=Agent;Buyer 8:0-rel 9:2-ARG1=Theme;Goods 19:1-ARGM-TMP 0:1*6:1-LINK-PRO 
nw/wsj/18/wsj_1844.parse 10 10 gold remain-v 47.1-1 State_continue remain.01 null ----- 10:0-rel 11:0,12:0-ARG1=Theme 
nw/wsj/18/wsj_1844.parse 11 2 gold place-v 9.1-2 Placing place.01 null ----- 0:1-ARG0=Agent;Agent/Cause 2:0-rel 3:1-ARG1=Theme;Theme 6:1-ARGM-MNR 8:2-ARG2=Destination;Goal 
nw/wsj/18/wsj_1844.parse 12 4 gold include-v 65 NF include.01 null ----- 0:1-ARG2=Location 3:0-ARGM-MOD 4:0-rel 5:3-ARG1=Theme 
nw/wsj/18/wsj_1844.parse 15 20 gold work-v 73-3 IN work.01 null ----- 16:1*17:1*26:1-ARGM-LOC 18:1-ARG0=Agent 20:0-rel 21:1-ARGM-TMP 16:1*17:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 17 1 gold imply-v 78 Evidence imply.01 1 ----- 0:1-ARG0=Cause;Support 1:0-rel 2:2-ARG1=Topic;Proposition 
nw/wsj/18/wsj_1844.parse 17 8 gold say-v 37.7-1 IN say.01 null ----- 0:2*9:1-ARG1=Topic 7:1-ARG0=Agent 8:0-rel 
nw/wsj/18/wsj_1844.parse 18 1 gold add-v 37.7 NF add.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/18/wsj_1844.parse 18 7 gold agree-v 36.1-1 IN agree.01 null ----- 3:1*8:1-ARG0=Agent 7:0-rel 8:2-ARG1=Theme 
nw/wsj/18/wsj_1844.parse 18 10 gold provide-v 13.4.1-2 Supply provide.01 null ----- 3:1*8:1-ARG0=Agent 10:0-rel 11:2-ARG1=Theme 
nw/wsj/18/wsj_1844.parse 19 18 gold say-v 37.7-1 IN say.01 null ----- 16:1-ARG0=Agent 18:0-rel 19:1-ARG1=Topic 
nw/wsj/18/wsj_1844.parse 19 22 gold plan-v 62 Purpose plan.01 null ----- 20:1*23:1-ARG0=Experiencer;Agent 21:1-ARGM-ADV 22:0-rel 23:2-ARG1 
nw/wsj/18/wsj_1844.parse 21 2 gold have-v 100 IN have.03 null ----- 0:1-ARG0=Pivot 2:0-rel 3:2-ARG1=Theme 10:1-ARGM-TMP 
nw/wsj/18/wsj_1844.parse 21 13 gold rank-v 29.6 Occupy_rank rank.01 1 ----- 0:1-ARG1=Attribute 13:0-rel 14:1-ARG2=Attribute 
nw/wsj/18/wsj_1844.parse 22 11 gold have-v 100 IN have.03 null ----- 0:2-ARG0=Pivot 10:0-ARGM-MOD 11:0-rel 12:2-ARG1=Theme 28:2-ARGM-ADV 
nw/wsj/18/wsj_1844.parse 22 29 gold bring-v 11.3-1 Bringing bring.01 null ----- 28:1-ARG0=Instrument 29:0-rel 30:1-ARG1=Theme 31:1-ARG3 
nw/wsj/18/wsj_1844.parse 24 15 gold prove-v 29.4-1-1 Turning_out prove.01 null ----- 14:0-ARGM-MOD 15:0-rel 16:2-ARG1=Theme 
nw/wsj/18/wsj_1844.parse 25 20 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARGM-TMP 19:1-ARG0=Agent 20:0-rel 21:1-ARG1=Topic 
nw/wsj/18/wsj_1844.parse 26 5 gold win-v 13.5.1 NF win.01 null ----- 0:1-ARGM-TMP 3:1-ARG0=Agent 5:0-rel 6:1-ARG1=Theme 8:1-ARGM-PRD 
nw/wsj/18/wsj_1844.parse 27 24 gold elude-v 52 IN elude.01 1 ----- 0:0-ARGM-DIS 1:2-ARG0=Agent 23:1-ARGM-MNR 24:0-rel 25:1-ARG1=Theme 
nw/wsj/18/wsj_1844.parse 28 11 gold call-v 29.3 IN call.01 null ----- 8:1*17:1-ARG0=Agent 11:0-rel 12:1*14:1-ARG1=Theme 14:2-ARG2=Result 17:2*34:1-ARGM-ADV 
nw/wsj/18/wsj_1844.parse 28 18 gold add-v 37.7 NF add.01 null ----- 8:1*17:1-ARG0=Agent 18:0-rel 19:1-ARGM-MNR 22:2*34:1-ARG1=Topic 
nw/wsj/18/wsj_1844.parse 29 22 gold see-v 30.1-1 Perception_experience see.01 null ----- 8:2*19:1*23:1-ARG1=Stimulus;Phenomenon 20:1-ARG0=Experiencer;Perceiver_passive 22:0-rel 8:2*19:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 30 9 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-ADV 6:1-ARG0=Agent 9:0-rel 10:1-ARG1=Topic 
nw/wsj/18/wsj_1844.parse 30 19 gold combine-v 22.1-1-1 Cause_to_amalgamate combine.01 null ----- 11:1*17:1-ARG0=Agent;Agent 15:1*16:1-ARGM-MNR 19:0-rel 20:1-ARG1=Patient;Part_1 23:1-ARGM-LOC 11:1*17:1-LINK-PRO 15:1*16:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 31 16 gold found-v 55.5-1 Intentionally_create found.01 1 ----- 8:1*14:1*15:1-ARG0 16:0-rel 17:1-ARG1 8:1*14:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 31 20 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 0:2-ARGM-DIS 5:1-ARG0=Agent;Recipient 19:0-ARGM-MOD 20:0-rel 21:3-ARG1=Theme;Theme 28:2-ARGM-PRD 
nw/wsj/18/wsj_1844.parse 31 29 gold create-v 27 Creating create.01 null ----- 28:1-ARG0=Cause 29:0-rel 30:2-ARG1=Theme 
nw/wsj/18/wsj_1844.parse 32 2 gold receive-v 13.5.2 Receiving receive.01 null ----- 0:1-ARG0=Agent;Donor 1:0-ARGM-MOD 2:0-rel 3:1-ARG1=Theme;Theme 6:1-ARG3 22:1-ARGM-ADV 
nw/wsj/18/wsj_1844.parse 32 19 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 11:1*16:1*20:1-ARG1=Theme;Goods 17:1-ARG0=Agent;Buyer 19:0-rel 11:1*16:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 32 24 gold say-v 37.7-1 IN say.01 null ----- 23:1-ARG0=Agent 24:0-rel 25:1-ARG1=Topic 
nw/wsj/18/wsj_1844.parse 32 27 gold expect-v 62 IN expect.01 null ----- 26:1*28:1-ARG0=Experiencer 27:0-rel 28:2-ARG1=Theme 
nw/wsj/18/wsj_1844.parse 33 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/18/wsj_1844.parse 33 6 gold plan-v 62 Purpose plan.01 null ----- 5:1*7:1-ARG0=Experiencer;Agent 6:0-rel 7:2-ARG1 
nw/wsj/18/wsj_1844.parse 33 9 gold remain-v 47.1-1 State_continue remain.01 null ----- 5:1*7:1-ARG1=Theme 9:0-rel 10:1-ARG3 23:2-ARGM-PRD 
nw/wsj/18/wsj_1844.parse 33 24 gold serve-v 29.6-2 Assistance serve.01 null ----- 5:1*7:1*23:1-ARG0=Agent 24:0-rel 25:1-ARG1=Attribute 
nw/wsj/18/wsj_1844.parse 34 9 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 0:2-ARG0 8:0-ARGM-MOD 9:0-rel 10:2-ARG1 
nw/wsj/18/wsj_1844.parse 35 5 gold sign-v 13.5.3 NF sign.02 null ----- 0:2-ARG0=Agent 5:0-rel 6:2-ARG1=Theme 
nw/wsj/18/wsj_1844.parse 35 16 gold receive-v 13.5.2 Receiving receive.01 null ----- 12:1-ARG0=Agent;Donor 15:0-ARGM-MOD 16:0-rel 17:2-ARG1=Theme;Theme 
nw/wsj/18/wsj_1844.parse 35 28 gold peg-v 54.4 Categorization peg.01 2 ----- 17:1*24:1*29:1-ARG1=Theme;Item 25:1-ARG0=Agent;Cognizer 28:0-rel 30:1-ARG2=Value;Category 17:1*24:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 36 13 gold follow-v 51.6 Cotheme follow.02 null ----- 0:1*11:1-ARG0=Agent;Theme 13:0-rel 14:2-ARG1=Theme;Cotheme 
nw/wsj/18/wsj_1844.parse 36 22 gold hold-v 15.1-1 Manipulation hold.01 null ----- 22:0-rel 23:0-ARG0=Agent;Agent 
nw/wsj/18/wsj_1844.parse 36 34 gold say-v 37.7-1 IN say.01 null ----- 34:0-rel 35:1-ARG1=Topic 37:2-ARG0=Agent 
nw/wsj/18/wsj_1844.parse 37 7 gold hold-v 15.1-1 Detaining hold.01 null ----- 0:1-ARGM-ADV 5:1-ARG0=Agent 6:0-ARGM-MOD 7:0-rel 8:1-ARG1=Theme 
nw/wsj/18/wsj_1844.parse 38 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/18/wsj_1844.parse 39 12 gold intend-v 62 Purpose intend.01 null ----- 0:1-ARGM-PNC 3:1-ARGM-TMP 5:2-ARG2 12:0-rel 13:2-ARG1 
nw/wsj/18/wsj_1844.parse 41 2 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 1:1*8:1-ARG0=Agent;Buyer 2:0-rel 3:1-ARG1=Theme;Goods 5:1-ARGM-MNR 
nw/wsj/18/wsj_1844.parse 41 9 gold obtain-v 13.5.2-1 Getting obtain.01 null ----- 0:1-ARGM-MNR 1:1*8:1-ARG0=Agent;Recipient 9:0-rel 10:3-ARG1=Theme;Theme 
nw/wsj/18/wsj_1844.parse 42 3 gold win-v 13.5.1 NF win.01 null ----- 0:1-ARG0=Agent 1:1-ARGM-DIS 3:0-rel 4:2-ARG1=Theme 
nw/wsj/18/wsj_1844.parse 43 6 gold get-v 26.6.2 IN get.05 null ----- 3:1-ARG1=Patient 6:0-rel 7:1-ARG2=Goal 10:1-ARGM-COM 
nw/wsj/18/wsj_1844.parse 43 21 gold want-v 32.1-1-1 Desiring want.01 null ----- 20:1*22:1-ARG0=Pivot;Experiencer 21:0-rel 22:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/18/wsj_1844.parse 44 2 gold help-v 72-1 Assistance help.01 null ----- 0:1*3:1*9:1-ARG0=Agent;Helper 2:0-rel 3:2-ARG1=Theme;Goal/Focal_entity 
nw/wsj/18/wsj_1844.parse 44 10 gold say-v 37.7-1 IN say.01 null ----- 0:1*3:1*9:1-ARG0=Agent 0:2-ARGM-PRP 10:0-rel 11:1-ARG1=Topic 
nw/wsj/18/wsj_1844.parse 44 13 gold plan-v 62 Purpose plan.01 null ----- 0:2-ARGM-PNC 12:1-ARG0=Experiencer;Agent 13:0-rel 14:2-ARG1 20:1-ARGM-TMP 
nw/wsj/18/wsj_1844.parse 45 2 gold add-v 37.7 NF add.01 null ----- 0:0-ARGM-DIS 1:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/18/wsj_1844.parse 45 20 gold arrange-v 26.1 IN arrange.01 2 ----- 5:1-ARGM-ADV 18:1-ARG0=Agent 20:0-rel 21:3-ARG1=Product 
nw/wsj/18/wsj_1844.parse 47 3 gold bring-v 11.3-1 Bringing bring.01 null ----- 0:2-ARG0=Instrument 3:0-rel 4:1-ARG1=Theme 9:1-ARG3 13:1-ARGM-TMP 15:1-ARGM-ADV 
nw/wsj/18/wsj_1844.parse 47 29 gold survey-v 30.2 NF survey.01 2 ----- 16:2*26:1-ARG0=Experiencer 27:1-ARGM-ADV 29:0-rel 30:1-ARG1=Stimulus 
nw/wsj/18/wsj_1844.parse 47 34 gold wring-v 40.3.2 Manipulation wring.01 1 ----- 16:2*26:1-ARG0=Agent 27:1-ARGM-ADV 34:0-rel 35:1-ARG1=Patient 37:1-ARGM-CAU 
nw/wsj/18/wsj_1844.parse 48 3 gold report-v 37.7-1 Statement report.01 null ----- 1:1-ARG0 3:0-rel 4:2-ARG1 
nw/wsj/18/wsj_1844.parse 48 10 gold damage-v 44 Damaging damage.01 1 ----- 0:1-ARGM-ADV 8:1-ARG0=Agent;Agent/Cause 10:0-rel 11:2-ARG1=Patient;Patient 
nw/wsj/18/wsj_1844.parse 48 34 gold say-v 37.7-1 IN say.01 null ----- 30:2-ARG0 34:0-rel 35:1-ARG1 
nw/wsj/18/wsj_1844.parse 49 14 gold watch-v 30.2 Perception_active watch.01 null ----- 0:2-ARG0 14:0-rel 15:2-ARG1 
nw/wsj/18/wsj_1844.parse 49 16 gold drop-v 47.7 Motion_directional drop.01 null ----- 15:1-ARG1 16:0-rel 17:1-ARG3 
nw/wsj/18/wsj_1844.parse 49 22 gold feel-v 35.1 Perception_experience feel.03 null ----- 0:2-ARG0=Agent 21:0-ARGM-DIS 22:0-rel 23:2-ARG1=Theme 
nw/wsj/18/wsj_1844.parse 49 25 gold sway-v 47.3 NF sway.01 1 ----- 23:1-ARG1 25:0-rel 26:2-ARG2 
nw/wsj/18/wsj_1844.parse 50 19 gold say-v 37.7-1 IN say.01 null ----- 14:2-ARG0 19:0-rel 20:1-ARG1 
nw/wsj/18/wsj_1844.parse 51 34 gold say-v 37.7-1 IN say.01 null ----- 32:1-ARG0 34:0-rel 35:1-ARG1 
nw/wsj/18/wsj_1844.parse 52 13 gold add-v 37.7 NF add.01 null ----- 0:3*15:1-ARG1=Topic 11:1-ARG0=Agent 13:0-rel 
nw/wsj/18/wsj_1844.parse 53 4 gold stop-v 55.4-1 Process_stop stop.01 null ----- 1:1-ARG1=Theme 4:0-rel 
nw/wsj/18/wsj_1844.parse 53 8 gold say-v 37.7-1 IN say.01 null ----- 1:2*9:1-ARG1 7:1-ARG0 8:0-rel 
nw/wsj/18/wsj_1844.parse 54 14 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0 14:0-rel 15:1-ARG1 
nw/wsj/18/wsj_1844.parse 54 20 gold live-v 46 Residence live.01 null ----- 16:1*18:1*19:1-ARG0 20:0-rel 21:1-ARGM-LOC 16:1*18:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 54 36 gold get-v 26.6.2 IN get.05 null ----- 29:1*34:1-ARG1=Patient 36:0-rel 37:1-ARG2=Goal 
nw/wsj/18/wsj_1844.parse 54 39 gold spend-v 66-1 NF spend.02 null ----- 16:2*26:1-ARG0=Agent 26:2-ARGM-ADV 39:0-rel 40:1-ARG1=Asset 42:1-ARGM-LOC 
nw/wsj/18/wsj_1844.parse 57 10 gold award-v 13.3 NF award.01 1 ----- 0:2-ARG0=Agent 10:0-rel 11:1-ARG1=Theme 16:1-ARG2=Goal 
nw/wsj/18/wsj_1844.parse 58 9 gold handle-v 15.1-1 NF handle.01 null ----- 0:2-ARG0=Agent 8:0-ARGM-MOD 9:0-rel 10:2-ARG1=Theme 
nw/wsj/18/wsj_1844.parse 59 5 gold bill-v 54.5 Commerce_collect bill.01 1 ----- 0:1*3:1*4:1-ARG0=Agent 5:0-rel 6:2-ARG1=Asset 11:1-ARGM-TMP 14:1-ARGM-PRD 0:1*3:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 59 22 gold handle-v 15.1-1 NF handle.01 null ----- 0:2*23:1-ARG1=Theme 21:1-ARGM-TMP 22:0-rel 24:1-ARGM-LOC 
nw/wsj/18/wsj_1844.parse 61 6 gold name-v 29.3 IN name.01 null ----- 0:2*7:1-ARG1=Theme 6:0-rel 8:2-ARG2=Result 
nw/wsj/18/wsj_1844.parse 65 4 gold head-v 47.8 Leadership head.01 null ----- 0:1*5:1-ARG1=Co-Theme 2:0-ARGM-MOD 4:0-rel 6:1-ARG0=Theme 
nw/wsj/18/wsj_1844.parse 67 8 gold call-v 29.3 IN call.01 null ----- 7:1-ARGM-TMP 8:0-rel 9:1-ARG1=Theme 10:2-ARG2=Result 
nw/wsj/18/wsj_1844.parse 67 19 gold call-v 29.3 IN call.01 null ----- 0:3*20:1-ARG1=Theme 16:0-ARGM-MOD 17:1-ARGM-TMP 19:0-rel 21:2-ARG2=Result 24:2-ARGM-PRP 
nw/wsj/18/wsj_1844.parse 67 26 gold match-v 22.2-1 Compatibility match.01 1 ----- 24:1-ARG0=Agent 26:0-rel 27:2-ARG1=Patient;Item_1/Items 
nw/wsj/18/wsj_1844.parse 70 8 gold award-v 13.3 NF award.01 1 ----- 0:2-ARG0=Agent 8:0-rel 9:1-ARG1=Theme 15:1-ARG2=Goal 
nw/wsj/18/wsj_1844.parse 71 2 gold set-v 9.1-2 IN set.01 null ----- 0:1*3:1-ARG1 2:0-rel 4:1-ARG2 
nw/wsj/18/wsj_1844.parse 72 4 gold introduce-v 55.5-1 NF introduce.02 null ----- 0:1*5:1-ARG1 4:0-rel 6:1-ARGM-TMP 8:1-ARGM-LOC 0:1*5:1-LINK-PSV 
nw/wsj/18/wsj_1844.parse 73 10 gold say-v 37.7-1 IN say.01 null ----- 5:1,8:1,12:2-ARG1 9:1-ARG0 10:0-rel 5:1*8:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 73 14 gold compete-v 36.4-1 Competition compete.01 1 ----- 5:1*8:1*12:1-ARG0=Agent 13:0-ARGM-MOD 14:0-rel 15:1-ARG1=Co-Agent 5:1*8:1-LINK-SLC 
nw/wsj/18/wsj_1844.parse 73 36 gold see-v 30.1-1 Perception_experience see.01 null ----- 27:1*32:1*37:1-ARG1 33:1-ARG0 35:1-ARGM-TMP 36:0-rel 27:1*32:1-LINK-SLC 
