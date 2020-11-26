nw/wsj/02/wsj_0257.parse 0 3 gold write-v 25.2 Text_creation write.01 null ----- 0:1-ARG0=Agent;Author 3:0-rel 4:2-ARG1=Theme;Text 
nw/wsj/02/wsj_0257.parse 3 17 gold make-v 29.3 Causation make.02 null ----- 0:1*16:1-ARG0=Agent 17:0-rel 18:2-ARG1=Theme 
nw/wsj/02/wsj_0257.parse 5 8 gold combine-v 22.1-1-1 Cause_to_amalgamate combine.01 null ----- 0:1*7:1-ARG0=Agent;Agent 8:0-rel 9:1-ARG1=Patient;Part_1 12:1-ARG2=Co-Patient;Part_2 
nw/wsj/02/wsj_0257.parse 6 19 gold spin-v 26.7-2-1 Processing_materials spin.03 2 ----- 0:1-ARGM-LOC 18:1-ARG0=Agent 19:0-rel 20:2*30:2-ARG1=Theme 
nw/wsj/02/wsj_0257.parse 7 17 gold accuse-v 81 Notification_of_charges accuse.01 1 ----- 12:1*18:1-ARG1=Theme;Accused 17:0-rel 19:1-ARG2=Attribute;Charges 12:1*18:1-LINK-PSV 
nw/wsj/02/wsj_0257.parse 7 21 gold steal-v 10.5-1 Theft steal.01 null ----- 12:1*20:1-ARG0=Agent;Perpetrator 21:0-rel 22:1-ARG1=Theme;Goods 23:1-ARG2=Source;Source/Victim 
nw/wsj/02/wsj_0257.parse 7 31 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 12:1*20:1-ARG0=Agent;Seller 31:0-rel 32:1-ARG1=Theme;Goods 33:1-ARG2=Recipient 
nw/wsj/02/wsj_0257.parse 8 15 gold use-v 105 IN use.01 null ----- 0:3-ARG0 14:1-ARGM-ADV 15:0-rel 16:1-ARG1 20:2-ARG2 
nw/wsj/02/wsj_0257.parse 8 36 gold search-v 35.2 IN search.01 1 ----- 0:3*20:1*35:1-ARG0=Agent 36:0-rel 37:1-ARG2=Theme 
nw/wsj/02/wsj_0257.parse 9 43 gold collect-v 13.5.2 Gathering_up collect.01 null ----- 39:1*41:1*44:1-ARG1=Theme 42:1-ARG0=Agent 43:0-rel 39:1*41:1-LINK-SLC 
nw/wsj/02/wsj_0257.parse 10 1 gold promise-v 13.3 Commitment promise.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:3-ARG2=Theme 
nw/wsj/02/wsj_0257.parse 11 2 gold draw-v 10.1 Cause_motion draw.02 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Theme 5:1-ARG2=Source 
nw/wsj/02/wsj_0257.parse 11 12 gold lay-v 9.2 Placing lay.01 null ----- 11:1-ARG0=Agent;Agent/Cause 12:0-rel 13:1-ARG1=Theme;Theme 14:1-ARG2=Destination;Goal 21:2-ARGM-ADV 
nw/wsj/02/wsj_0257.parse 11 22 gold make-v 29.3 Causation make.02 null ----- 11:1*21:1-ARG0=Agent 22:0-rel 23:2-ARG1=Theme 
nw/wsj/02/wsj_0257.parse 12 4 gold discover-v 84 NF discover.01 null ----- 0:1-ARG0 4:0-rel 5:1-ARG1 
nw/wsj/02/wsj_0257.parse 12 13 gold call-v 29.3 Being_named call.01 null ----- 6:1*14:1-ARG1=Theme;Entity 13:0-rel 15:2-ARG2=Result;Name 6:1*14:1-LINK-PRO 
nw/wsj/02/wsj_0257.parse 13 1 gold use-v 105 IN use.01 null ----- 0:1*6:1-ARG0 1:0-rel 2:1-ARG1 
nw/wsj/02/wsj_0257.parse 13 9 gold substitute-v 13.6-1-1 Replacing substitute.01 1 ----- 0:1*6:1-ARG0=Agent;Agent 0:2-ARGM-PRD 8:0-ARGM-MOD 9:0-rel 10:1-ARG1=Co-Theme;New 16:1-ARG3=Theme;Old 
nw/wsj/02/wsj_0257.parse 14 6 gold lay-v 9.2 Placing lay.01 null ----- 1:2*7:1-ARG1=Theme;Theme 6:0-rel 
nw/wsj/02/wsj_0257.parse 15 3 gold scan-v 30.2 IN scan.01 1 ----- 0:1-ARG0 3:0-rel 4:1-ARG1 6:1-ARGM-LOC 15:1-ARGM-TMP 17:2-ARGM-TMP 
nw/wsj/02/wsj_0257.parse 15 27 gold ask-v 60-1 Request ask.02 null ----- 17:1*40:1-ARGM-TMP 25:1*28:1-ARG2 27:0-rel 29:2-ARG1 
nw/wsj/02/wsj_0257.parse 15 31 gold switch-v 26.6.2-1 IN switch.01 1 ----- 25:1*28:1*29:1-ARG0 31:0-rel 32:1-ARG2 
nw/wsj/02/wsj_0257.parse 15 34 gold help-v 72-1 Assistance help.01 null ----- 25:1*33:1-ARG0=Agent;Helper 34:0-rel 35:1-ARG1=Theme;Goal/Focal_entity 25:1*33:1-LINK-PRO 
nw/wsj/02/wsj_0257.parse 16 1 gold discover-v 84 NF discover.01 null ----- 0:1-ARG0 1:0-rel 2:2-ARG1 
nw/wsj/02/wsj_0257.parse 16 24 gold name-v 29.3 IN name.01 null ----- 22:1*25:1-ARG1=Theme 24:0-rel 26:1-ARG2=Result 22:1*25:1-LINK-PRO 
nw/wsj/02/wsj_0257.parse 16 33 gold have-v 100 IN have.03 null ----- 22:2*31:1*32:1-ARG0=Pivot 33:0-rel 34:1-ARG1=Theme 22:2*31:1-LINK-SLC 
nw/wsj/02/wsj_0257.parse 17 2 gold suspect-v 29.5-1 Awareness suspect.01 null ----- 0:1-ARG0=Agent;Cognizer 2:0-rel 3:1-ARG1=Theme;Content 
nw/wsj/02/wsj_0257.parse 17 14 gold have-v 100 IN have.03 null ----- 9:1*12:1*13:1-ARG0=Pivot 14:0-rel 15:2-ARG1=Theme 9:1*12:1-LINK-SLC 
nw/wsj/02/wsj_0257.parse 19 1 gold find-v 84 IN find.01 null ----- 0:1-ARG0 1:0-rel 2:1-ARG1 
nw/wsj/02/wsj_0257.parse 20 12 gold lay-v 9.2 Placing lay.01 null ----- 10:1-ARG0=Agent;Agent/Cause 11:0-ARGM-MOD 12:0-rel 13:1-ARG1=Theme;Theme 14:1-ARG2=Destination;Goal 
nw/wsj/02/wsj_0257.parse 20 20 gold monitor-v 35.4 NF monitor.01 1 ----- 18:1-ARG0=Agent 19:0-ARGM-MOD 20:0-rel 21:2-ARG1=Location 
nw/wsj/02/wsj_0257.parse 21 2 gold discover-v 84 NF discover.01 null ----- 1:1*22:1-ARG0 2:0-rel 3:1-ARG1 
nw/wsj/02/wsj_0257.parse 21 16 gold name-v 29.3 IN name.01 null ----- 13:1*17:1-ARG1=Theme 16:0-rel 18:2-ARG2=Result 13:1*17:1-LINK-PRO 
nw/wsj/02/wsj_0257.parse 21 37 gold alert-v 37.9 NF alert.01 1 ----- 25:2*34:1*35:1-ARG0=Agent 37:0-rel 38:1-ARG1=Recipient 39:2-ARG2=Topic 25:2*34:1-LINK-SLC 
nw/wsj/02/wsj_0257.parse 22 3 gold sleep-v 40.4 Sleep sleep.01 1 ----- 0:1-ARGM-TMP 2:1-ARG0=Agent;Sleeper 3:0-rel 4:1-ARGM-LOC 
nw/wsj/02/wsj_0257.parse 23 2 gold complain-v 37.8 Statement complain.01 null ----- 0:1-ARG0=Agent;Speaker 2:0-rel 3:1-ARG1=Topic;Message 
nw/wsj/02/wsj_0257.parse 24 3 gold paw-v 18.2 NF paw.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Patient 
nw/wsj/02/wsj_0257.parse 24 10 gold use-v 105 IN use.01 null ----- 0:1-ARG0 10:0-rel 11:2-ARG1 17:1-ARG2 
nw/wsj/02/wsj_0257.parse 24 25 gold use-v 105 IN use.01 null ----- 21:2,23:1*26:1-ARG1 25:0-rel 27:1-ARG0 21:2*26:1-LINK-PSV 
nw/wsj/02/wsj_0257.parse 26 19 gold alert-v 37.9 NF alert.01 1 ----- 6:1*11:1*17:1-ARG0=Agent 19:0-rel 20:1-ARG1=Recipient 
nw/wsj/02/wsj_0257.parse 26 22 gold keep-v 55.6 Cause_to_continue keep.02 null ----- 6:1*11:1-ARG0=Agent 22:0-rel 23:1,26:2-ARG1=Theme 
nw/wsj/02/wsj_0257.parse 26 30 gold avoid-v 52 Avoiding avoid.01 null ----- 6:1*11:1*28:1-ARG0=Agent;Agent 30:0-rel 31:2-ARG1=Theme;Undesirable_situation 
nw/wsj/02/wsj_0257.parse 26 32 gold arouse-v 31.1 Experiencer_obj arouse.01 1 ----- 6:1*11:1*28:1*31:1-ARG0=Stimulus;Stimulus 32:0-rel 33:1-ARG1=Experiencer;Experiencer 
nw/wsj/02/wsj_0257.parse 27 16 gold drag-v 11.4 Cause_motion drag.01 1 ----- 0:1-ARGM-TMP 2:1-ARGM-ADV 14:1*22:1-ARG0 15:0-ARGM-MOD 16:0-rel 17:1-ARG1 19:1-ARG2 22:2-ARGM-PRP 
nw/wsj/02/wsj_0257.parse 27 24 gold create-v 27 Cause_to_start create.01 null ----- 14:1*22:1-ARG0 24:0-rel 25:1-ARG1 31:2-ARGM-PRP 
nw/wsj/02/wsj_0257.parse 27 33 gold frustrate-v 31.1 Experiencer_obj frustrate.01 2 ----- 14:1*22:1*31:1-ARG0=Stimulus;Stimulus 33:0-rel 34:1-ARG1=Experiencer;Experiencer 
nw/wsj/02/wsj_0257.parse 28 3 gold show-v 78-1-1 IN show.01 null ----- 0:1-ARG0 2:1-ARGM-TMP 3:0-rel 4:1-ARG1 
nw/wsj/02/wsj_0257.parse 28 9 gold have-v 100 IN have.03 null ----- 8:1-ARG0=Pivot 9:0-rel 10:1-ARG1=Theme 
nw/wsj/02/wsj_0257.parse 29 2 gold tell-v 37.2-1 Telling tell.01 null ----- 0:1-ARG0 2:0-rel 3:1-ARG2 4:1-ARG1 
nw/wsj/02/wsj_0257.parse 32 3 gold get-v 13.5.1-1 IN get.01 null ----- 0:1-ARGM-TMP 2:1-ARG0=Agent 3:0-rel 4:1-ARG1=Theme 
nw/wsj/02/wsj_0257.parse 33 5 gold link-v 22.1-2-1 Make_cognitive_connection link.01 null ----- 2:1-ARG0 5:0-rel 6:1-ARG1 
nw/wsj/02/wsj_0257.parse 35 8 gold find-v 84 IN find.01 null ----- 0:1-ARG0 3:1-ARGM-TMP 7:1-ARGM-TMP 8:0-rel 9:1-ARG1 
nw/wsj/02/wsj_0257.parse 36 15 gold brief-v 37.9 NF brief.01 null ----- 2:1*6:1*13:1-ARG0=Agent 15:0-rel 16:2-ARG1=Recipient 20:1-ARG2=Topic 
nw/wsj/02/wsj_0257.parse 37 1 gold savor-v 31.2 NF savor.01 1 ----- 0:1-ARG0 1:0-rel 2:2-ARG1 
nw/wsj/02/wsj_0257.parse 38 8 gold scold-v 33 Judgment_direct_address scold.01 1 ----- 0:2-ARGM-LOC 5:1*9:1-ARG1=Theme;Addressee 7:1-ARGM-MNR 8:0-rel 10:1-ARG0=Agent;Communicator 17:1-ARG2=Attribute;Reason 
nw/wsj/02/wsj_0257.parse 38 19 gold consort-v 36.1 NF consort.01 null ----- 5:1*9:1*18:1-ARG0=Agent 19:0-rel 20:1-ARG1=Co-Agent 
nw/wsj/02/wsj_0257.parse 40 2 gold develop-v 48.1.1 IN develop.01 null ----- 0:1-ARG1 2:0-rel 3:3-ARG2 
nw/wsj/02/wsj_0257.parse 40 20 gold make-v 29.3 Causation make.02 null ----- 13:1*18:1*19:1-ARG0=Agent 20:0-rel 21:2-ARG1=Theme 13:1*18:1-LINK-SLC 
