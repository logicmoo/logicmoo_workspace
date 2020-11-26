nw/wsj/20/wsj_2040.parse 1 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:1-ARGM-DIS 3:0-rel 4:1-ARG1=Topic 
nw/wsj/20/wsj_2040.parse 1 6 gold expect-v 62 IN expect.01 null ----- 5:1*7:1-ARG0=Experiencer 6:0-rel 7:2-ARG1=Theme 
nw/wsj/20/wsj_2040.parse 1 9 gold report-v 37.7-1 Statement report.01 null ----- 5:1*7:1-ARG0 9:0-rel 10:3-ARG1 
nw/wsj/20/wsj_2040.parse 2 3 gold bring-v 11.3-1 Bringing bring.01 null ----- 0:1-ARG0=Instrument 3:0-rel 4:3-ARG1=Theme 14:1-ARG3 
nw/wsj/20/wsj_2040.parse 4 1 gold get-v 26.6.2 IN get.05 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Patient 4:1-ARG2=Goal 
nw/wsj/20/wsj_2040.parse 4 14 gold say-v 37.7-1 IN say.01 null ----- 0:3*15:1-ARG1=Topic 14:0-rel 16:2-ARG0=Agent 28:1-ARGM-ADV 
nw/wsj/20/wsj_2040.parse 4 30 gold approve-v 31.3-6 NF approve.01 null ----- 16:2*29:1-ARG0=Experiencer 30:0-rel 31:1-ARG1=Stimulus 16:2*29:1-LINK-PRO 
nw/wsj/20/wsj_2040.parse 5 4 gold have-v 100 IN have.03 null ----- 0:1-ARG0=Pivot 4:0-rel 5:1-ARG1=Theme 
nw/wsj/20/wsj_2040.parse 5 18 gold say-v 37.7-1 IN say.01 null ----- 16:1-ARG0=Agent 18:0-rel 19:1-ARG1=Topic 
nw/wsj/20/wsj_2040.parse 6 1 gold expect-v 62 IN expect.01 null ----- 0:1-ARG0=Experiencer 1:0-rel 2:2-ARG1=Theme 
nw/wsj/20/wsj_2040.parse 7 2 gold report-v 37.7-1 Statement report.01 null ----- 0:1-ARG0 1:1-ARGM-TMP 2:0-rel 3:2-ARG1 21:1-ARGM-ADV 
nw/wsj/20/wsj_2040.parse 7 14 gold call-v 29.3 IN call.01 null ----- 3:1*12:1*15:1-ARG1=Theme 13:1-ARG0=Agent 14:0-rel 16:2-ARG2=Result 3:1*12:1-LINK-SLC 
nw/wsj/20/wsj_2040.parse 7 25 gold post-v 11.1 Sending post.01 null ----- 22:1-ARG0=Agent;Sender 25:0-rel 26:2-ARG1=Theme;Theme 35:1-ARGM-TMP 
nw/wsj/20/wsj_2040.parse 7 37 gold add-v 22.1-2 Statement add.02 null ----- 22:1*36:1-ARG0=Agent 37:0-rel 38:2-ARG1=Patient 42:1-ARG2=Co-Patient 22:1*36:1-LINK-PRO 
nw/wsj/20/wsj_2040.parse 8 5 gold post-v 11.1 Sending post.01 null ----- 0:1-ARG0=Agent;Sender 5:0-rel 6:1-ARG1=Theme;Theme 
nw/wsj/20/wsj_2040.parse 9 7 gold post-v 11.1 Sending post.01 null ----- 0:2-ARG0=Agent;Sender 7:0-rel 8:1-ARG1=Theme;Theme 
nw/wsj/20/wsj_2040.parse 10 11 gold report-v 37.7-1 Statement report.01 null ----- 0:2-ARG0 11:0-rel 12:1-ARG1 19:1-ARGM-TMP 
nw/wsj/20/wsj_2040.parse 10 15 gold climb-v 45.6-1 Change_position_on_a_scale climb.02 null ----- 13:1-ARG1=Patient;Item 15:0-rel 16:1-ARG2=Extent;Difference 
nw/wsj/20/wsj_2040.parse 10 30 gold grow-v 45.6-1 IN grow.01 null ----- 20:2-ARG1=Patient 30:0-rel 31:1-ARG2=Extent 
nw/wsj/20/wsj_2040.parse 12 4 gold disappoint-v 31.1 Experiencer_obj disappoint.01 1 ----- 0:1*5:1-ARG1=Experiencer;Experiencer 2:1-ARGM-MNR 4:0-rel 6:1-ARG0=Stimulus;Stimulus 
nw/wsj/20/wsj_2040.parse 13 15 gold make-v 29.3 Causation make.02 null ----- 11:1*12:1*13:1-ARG0=Agent 15:0-rel 16:2-ARG1=Theme 11:1*12:1-LINK-SLC 
nw/wsj/20/wsj_2040.parse 13 17 gold think-v 29.9-2 IN think.01 null ----- 16:1-ARG0 17:0-rel 18:1-ARG1 
nw/wsj/20/wsj_2040.parse 13 21 gold set-v 9.1-2 IN set.01 null ----- 19:1-ARG0=Agent 21:0-rel 22:1-ARG1=Theme 24:1-ARG2=Destination 
nw/wsj/20/wsj_2040.parse 13 28 gold say-v 37.7-1 IN say.01 null ----- 1:2*29:1-ARG1=Topic 28:0-rel 30:2-ARG0=Agent 
nw/wsj/20/wsj_2040.parse 14 8 gold grow-v 45.6-1 IN grow.01 null ----- 0:2-ARG1=Patient 8:0-rel 9:1-ARG2=Extent 
nw/wsj/20/wsj_2040.parse 15 4 gold continue-v 55.3 Process_continue continue.01 null ----- 1:1-ARG0 4:0-rel 5:2-ARG1 
nw/wsj/20/wsj_2040.parse 15 7 gold drive-v 11.5 Cause_motion drive.01 null ----- 1:1*5:1-ARG0=Agent 7:0-rel 8:1-ARG1=Theme 
nw/wsj/20/wsj_2040.parse 15 13 gold say-v 37.7-1 IN say.01 null ----- 1:2-ARG1=Topic 13:0-rel 14:2-ARG0=Agent 
nw/wsj/20/wsj_2040.parse 16 27 gold say-v 37.7-1 IN say.01 null ----- 25:1-ARG0=Agent 27:0-rel 0:5-ARG1-DSP=Topic 
nw/wsj/20/wsj_2040.parse 17 16 gold say-v 37.7-1 IN say.01 null ----- 14:1-ARG0=Agent 16:0-rel 0:2-ARG1-DSP=Topic 
nw/wsj/20/wsj_2040.parse 17 39 gold bring-v 11.3-1 Bringing bring.01 null ----- 38:1-ARG0=Instrument 39:0-rel 40:1*42:1-ARG1=Theme 42:2-ARG3 
nw/wsj/20/wsj_2040.parse 19 3 gold wait-v 47.1-1 IN wait.01 null ----- 1:1*4:1-ARG1=Theme 3:0-rel 4:2-ARG2 
nw/wsj/20/wsj_2040.parse 19 6 gold see-v 30.1-1 Grasp see.01 null ----- 1:1*4:1-ARG0=Experiencer 6:0-rel 7:1-ARG1=Stimulus 
nw/wsj/20/wsj_2040.parse 19 10 gold take-v 11.3 IN take.01 null ----- 8:1-ARG0 9:0-ARGM-MOD 10:0-rel 11:2-ARG1 
nw/wsj/20/wsj_2040.parse 19 33 gold say-v 37.7-1 IN say.01 null ----- 30:1-ARG0=Agent 33:0-rel 34:1-ARG1=Topic 
nw/wsj/20/wsj_2040.parse 20 2 gold reiterate-v 37.7-1 Statement reiterate.01 1 ----- 1:1-ARG0=Agent;Speaker 2:0-rel 3:2-ARG1=Topic;Message 
nw/wsj/20/wsj_2040.parse 21 12 gold call-v 29.3 IN call.01 null ----- 0:2-ARG0=Agent 12:0-rel 13:3-ARG1=Theme 26:1-ARG2=Result 
nw/wsj/20/wsj_2040.parse 22 2 gold concern-v 31.1 Cause_emotion concern.01 null ----- 2:0-rel 4:0-ARG0=Stimulus 
nw/wsj/20/wsj_2040.parse 23 3 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 3:0-rel 5:1-ARGM-DIS 7:1-ARG1=Topic 
nw/wsj/20/wsj_2040.parse 23 11 gold maintain-v 29.5-2 Statement maintain.01 null ----- 8:1-ARG0=Agent;Speaker 11:0-rel 12:1-ARG1=Theme;Addressee 14:1-ARGM-MNR 
nw/wsj/20/wsj_2040.parse 25 2 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 0:1*3:1-ARG0 2:0-rel 3:2-ARG1 16:2-ARGM-ADV 
nw/wsj/20/wsj_2040.parse 25 5 gold generate-v 27 IN generate.01 1 ----- 0:1*3:1-ARG0 5:0-rel 6:2-ARG1=Theme 
nw/wsj/20/wsj_2040.parse 26 2 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 null ----- 0:1-ARG1 2:0-rel 3:3-ARG2 15:1-ARG3 20:1-ARGM-PRD 
nw/wsj/20/wsj_2040.parse 27 10 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 0:1-ARGM-DIS 4:2-ARG1 10:0-rel 11:3-ARG2 20:1-ARG4 
nw/wsj/20/wsj_2040.parse 28 2 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 null ----- 0:1-ARG1 2:0-rel 3:1-ARG2 4:1-ARG4 9:1-ARG3 15:1-ARGM-ADV 
nw/wsj/20/wsj_2040.parse 28 18 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 16:1-ARG0 18:0-rel 19:2-ARG1 
nw/wsj/20/wsj_2040.parse 29 10 gold give-v 13.1-1 Giving give.01 null ----- 0:2-ARG0=Agent;Donor 10:0-rel 11:1-ARG2=Recipient;Recipient 13:4-ARG1=Theme;Theme 
nw/wsj/20/wsj_2040.parse 29 36 gold match-v 22.2-1 Compatibility match.01 1 ----- 33:1*37:1-ARG1=Patient;Item_1/Items 36:0-rel 38:1-ARG1=Patient;Item_1/Items 33:1*37:1-LINK-PSV 
nw/wsj/20/wsj_2040.parse 31 3 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1*19:1-ARG0=Agent;Seller 3:0-rel 4:2-ARG1=Theme;Goods 
nw/wsj/20/wsj_2040.parse 31 18 gold manage-v 74-1-1 IN manage.02 null ----- 0:1*19:1-ARG0 18:0-rel 19:2-ARG1 
nw/wsj/20/wsj_2040.parse 31 33 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 null ----- 27:1*31:1*32:1-ARG1 33:0-rel 34:1-ARG2 36:1-ARGM-TMP 40:1-ARG4 45:1-ARG3 27:1*31:1-LINK-SLC 
nw/wsj/20/wsj_2040.parse 33 22 gold have-v 100 IN have.03 null ----- 16:1*20:1,23:1,24:1-ARG1=Theme 21:1-ARG0=Pivot 22:0-rel 16:1*20:1-LINK-SLC 
nw/wsj/20/wsj_2040.parse 34 45 gold have-v 100 IN have.03 null ----- 37:2*41:1*50:1-ARGM-TMP 42:1-ARG0=Pivot 44:1-ARGM-TMP 45:0-rel 46:1-ARG1=Theme 37:2*41:1-LINK-SLC 
nw/wsj/20/wsj_2040.parse 36 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0 2:0-rel 3:1-ARG1 
nw/wsj/20/wsj_2040.parse 37 14 gold say-v 37.7-1 IN say.01 null ----- 0:2*15:1-ARG1 12:1-ARG0 14:0-rel 
nw/wsj/20/wsj_2040.parse 38 5 gold bring-v 11.3-1 Bringing bring.01 null ----- 0:2-ARG0=Instrument 5:0-rel 6:2-ARG1=Theme 14:1-ARG3 20:2-ARGM-PRD 
nw/wsj/20/wsj_2040.parse 39 17 gold receive-v 13.5.2 Receiving receive.01 null ----- 15:1*18:1-ARG1=Theme;Theme 16:1-ARG0=Agent;Donor 17:0-rel 19:1-ARG4 
nw/wsj/20/wsj_2040.parse 40 4 gold drop-v 45.6-1 Change_position_on_a_scale drop.01 null ----- 0:2-ARG1 4:0-rel 5:1-ARG2 8:1-ARG4 
nw/wsj/20/wsj_2040.parse 41 26 gold say-v 37.7-1 IN say.01 null ----- 1:2*27:1-ARG1 24:1-ARG0 26:0-rel 
nw/wsj/20/wsj_2040.parse 42 4 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 0:2-ARG1 4:0-rel 5:1-ARG2 8:1-ARG4 
nw/wsj/20/wsj_2040.parse 44 11 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0 11:0-rel 12:1-ARG1 
nw/wsj/20/wsj_2040.parse 44 14 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 13:1-ARG0 14:0-rel 15:2-ARG1 
nw/wsj/20/wsj_2040.parse 46 2 gold grow-v 45.6-1 IN grow.01 null ----- 0:1-ARG1 2:0-rel 3:1-ARG2 6:1-ARG4 12:1-ARGM-TMP 
