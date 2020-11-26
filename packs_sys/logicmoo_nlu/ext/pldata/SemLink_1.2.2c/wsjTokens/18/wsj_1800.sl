nw/wsj/18/wsj_1800.parse 0 1 gold send-v 11.1-1 Sending send.01 null ----- 0:1-ARG0=Agent;Sender 1:0-rel 2:1-ARG2=Destination;Goal/Recipient 4:2-ARG1=Theme;Theme 
nw/wsj/18/wsj_1800.parse 0 16 gold provide-v 13.4.1-2 Supply provide.01 null ----- 4:1-ARG0=Agent 16:0-rel 17:2-ARG1=Theme 21:1-ARG2=Recipient 
nw/wsj/18/wsj_1800.parse 1 16 gold contrast-v 22.2-2-1 NF contrast.01 1 ----- 13:1-ARG1=Patient 16:0-rel 17:1-ARG2=Co-Patient 
nw/wsj/18/wsj_1800.parse 1 23 gold underlie-v 47.8 NF underlie.01 null ----- 23:0-rel 24:0-ARG0=Theme 
nw/wsj/18/wsj_1800.parse 2 1 gold estimate-v 54.4 Estimating estimate.01 1 ----- 1:0-rel 2:1,5:0-ARG2=Value 
nw/wsj/18/wsj_1800.parse 2 7 gold add-v 22.1-2 Statement add.02 null ----- 0:1*8:1-ARG1=Patient 7:0-rel 9:1-ARGM-PNC 
nw/wsj/18/wsj_1800.parse 3 5 gold drive-v 51.4.2 Operate_vehicle drive.02 null ----- 5:0-rel 6:0-ARG1=Theme;Vehicle 
nw/wsj/18/wsj_1800.parse 4 7 gold allocate-v 13.3 NF allocate.01 1 ----- 0:2*8:1-ARG1=Theme 7:0-rel 9:1-ARG2=Goal 
nw/wsj/18/wsj_1800.parse 4 38 gold designate-v 29.1 IN designate.01 null ----- 35:1*39:1-ARG1=Theme 38:0-rel 40:1-ARG2=Result 
nw/wsj/18/wsj_1800.parse 5 20 gold help-v 72-1 Assistance help.01 null ----- 11:1*17:1*18:1-ARG0=Agent;Helper 20:0-rel 21:1-ARG1=Theme;Goal/Focal_entity 11:1*17:1-LINK-SLC 
nw/wsj/18/wsj_1800.parse 5 30 gold ask-v 60-1 Request ask.02 null ----- 25:1*31:1-ARG2 30:0-rel 32:2-ARG1 
nw/wsj/18/wsj_1800.parse 6 11 gold provide-v 13.4.1-2 Supply provide.01 null ----- 0:1-ARGM-LOC 4:3*12:1;19:3-ARG1=Theme 11:0-rel 13:1-ARGM-PRP 
nw/wsj/18/wsj_1800.parse 7 6 gold grow-v 45.6-1 IN grow.01 null ----- 0:3-ARG1=Patient 5:0-ARGM-MOD 6:0-rel 7:1-ARG4 
nw/wsj/18/wsj_1800.parse 7 16 gold add-v 22.1-2 Statement add.02 null ----- 14:1-ARG0=Agent 15:1-ARGM-ADV 16:0-rel 17:2-ARG1=Patient 21:1-ARGM-PNC 
nw/wsj/18/wsj_1800.parse 9 5 gold provide-v 13.4.1-2 Supply provide.01 null ----- 0:1*6:1-ARG1=Theme 5:0-rel 7:1-ARG2=Recipient 0:1*6:1-LINK-PSV 
nw/wsj/18/wsj_1800.parse 10 7 gold have-v 100 IN have.03 null ----- 0:1-ARGM-ADV 4:1-ARG0=Pivot 7:0-rel 8:2-ARG1=Theme 
nw/wsj/18/wsj_1800.parse 11 3 gold attach-v 22.3-2-1 Inchoative_attaching attach.01 null ----- 1:1*4:1-ARG1=Patient;Connector 3:0-rel 5:1-ARG1=Patient;Connector 1:1*4:1-LINK-PSV 
nw/wsj/18/wsj_1800.parse 11 24 gold intend-v 62 Purpose intend.01 null ----- 24:0-rel 25:1-ARG2 26:1-ARG1 
nw/wsj/18/wsj_1800.parse 12 5 gold lobby-v 58.1 NF lobby.01 1 ----- 0:2-ARG0=Agent 5:0-rel 6:1-ARG1=Topic 
nw/wsj/18/wsj_1800.parse 13 2 gold consider-v 29.9-1-1-1 Categorization consider.01 null ----- 2:0-rel 3:2-ARG1=Theme;Item 5:1-ARGM-TMP 
nw/wsj/18/wsj_1800.parse 13 14 gold use-v 105 IN use.01 null ----- 8:1*15:1-ARG1 14:0-rel 16:1-ARG2 19:2-ARGM-PRP 
nw/wsj/18/wsj_1800.parse 13 21 gold raise-v 9.4 NF raise.01 null ----- 19:1-ARG0=Agent 21:0-rel 22:2-ARG1=Theme 
nw/wsj/18/wsj_1800.parse 14 24 gold move-v 11.2 Cause_motion move.01 null ----- 7:2*19:1-ARGM-MNR 20:1-ARG0 22:1-ARGM-MNR 24:0-rel 26:1-ARG1 29:1-ARGM-DIR 32:1-ARG2 35:2-ARGM-PRP 7:2*19:1-LINK-SLC 
nw/wsj/18/wsj_1800.parse 14 37 gold justify-v 37.1.1 NF justify.01 1 ----- 20:1*35:1-ARG0=Agent 37:0-rel 38:1-ARG1=Topic 
nw/wsj/18/wsj_1800.parse 15 8 gold estimate-v 54.4 Estimating estimate.01 1 ----- 8:0-rel 9:1-ARG2=Value 
nw/wsj/18/wsj_1800.parse 15 28 gold own-v 100 NF own.01 null ----- 24:1*26:1*27:1-ARG0=Pivot 28:0-rel 29:2-ARG1=Theme 36:1-ARGM-TMP 24:1*26:1-LINK-SLC 
nw/wsj/18/wsj_1800.parse 16 42 gold end-v 55.4-1 Cause_to_end end.01 null ----- 40:1-ARG0=Agent 42:0-rel 43:2-ARG1=Theme 
nw/wsj/18/wsj_1800.parse 16 51 gold delay-v 53.1-1 IN delay.01 2 ----- 43:1*45:1*49:1-ARG0=Agent 46:1-ARG2 51:0-rel 52:2-ARG1=Theme 43:1*45:1-LINK-SLC 
nw/wsj/18/wsj_1800.parse 16 55 gold estimate-v 54.4 Estimating estimate.01 1 ----- 55:0-rel 57:0-ARG2=Value 58:0-ARG1=Theme 
nw/wsj/18/wsj_1800.parse 17 5 gold reach-v 13.5.1 NF reach.01 null ----- 0:1-ARG0=Agent 5:0-rel 6:1-ARG1=Theme 7:1-ARGM-TMP 9:1-ARGM-PRD 
nw/wsj/18/wsj_1800.parse 17 23 gold stall-v 53.1-1 NF stall.01 1 ----- 19:1*24:1-ARG1=Theme 23:0-rel 25:1-ARGM-CAU 
nw/wsj/18/wsj_1800.parse 17 45 gold provide-v 13.4.1-2 Supply provide.01 null ----- 36:1*42:1*43:1-ARG0=Agent 45:0-rel 46:1-ARG1=Theme 48:1-ARG2=Recipient 36:1*42:1-LINK-SLC 
nw/wsj/18/wsj_1800.parse 17 53 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 null ----- 49:1*51:1-ARG0=Cause 53:0-rel 54:2-ARG1=Patient 49:1*51:1-LINK-PRO 
nw/wsj/18/wsj_1800.parse 18 5 gold receive-v 13.5.2 Receiving receive.01 null ----- 0:2-ARG0=Agent;Donor 5:0-rel 6:1-ARG1=Theme;Theme 9:1-ARGM-TMP 12:1-ARGM-MNR 
nw/wsj/18/wsj_1800.parse 18 26 gold insist-v 37.7 Statement insist.01 null ----- 19:1*25:1-ARG0=Agent;Speaker 26:0-rel 27:1-ARG1=Topic;Message 19:1*25:1-LINK-PRO 
nw/wsj/18/wsj_1800.parse 19 24 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 null ----- 22:1-ARG0=Cause 24:0-rel 25:1-ARG1=Patient 
nw/wsj/18/wsj_1800.parse 19 40 gold believe-v 29.9-2 IN believe.01 null ----- 29:1-ARGM-PRD 38:1-ARG0 40:0-rel 41:2-ARG1 
nw/wsj/18/wsj_1800.parse 19 44 gold annoy-v 31.1 Experiencer_obj annoy.01 null ----- 38:1*41:1-ARG1=Experiencer;Experiencer 44:0-rel 45:1-ARGM-ADV 47:1-ARG0=Stimulus;Stimulus 
nw/wsj/18/wsj_1800.parse 19 50 gold move-v 11.2 IN move.01 null ----- 48:1-ARG1 50:0-rel 51:1-ARG2 53:1-ARGM-DIR 
