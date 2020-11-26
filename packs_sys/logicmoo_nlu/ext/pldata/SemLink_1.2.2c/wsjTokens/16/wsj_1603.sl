nw/wsj/16/wsj_1603.parse 1 19 gold ring-v 43.2 IN ring.01 null ----- 19:0-rel 20:0-ARG1=Theme 
nw/wsj/16/wsj_1603.parse 1 26 gold translate-v 26.6.1 IN translate.01 null ----- 19:1*23:1*27:1-ARG1=Patient 26:0-rel 28:1-ARG2=Result 
nw/wsj/16/wsj_1603.parse 2 2 gold find-v 84 IN find.01 null ----- 0:1-ARG0 2:0-rel 3:1-ARG1 
nw/wsj/16/wsj_1603.parse 2 5 gold turn-v 26.6.2 Cause_change turn.02 null ----- 0:1*4:1-ARG0=Agent 5:0-rel 6:1-ARG1=Patient 10:1-ARG2=Goal 0:1*4:1-LINK-PRO 
nw/wsj/16/wsj_1603.parse 3 7 gold cope-v 83-1 NF cope.01 1 ----- 3:1*10:1-ARGM-MNR 4:1-ARG0=Agent 7:0-rel 8:1-ARG1=Theme 
nw/wsj/16/wsj_1603.parse 5 4 gold live-v 47.1-1 NF live.01 null ----- 4:0-rel 5:0-ARG0=Theme 
nw/wsj/16/wsj_1603.parse 5 9 gold die-v 48.2 Death die.01 null ----- 7:1-ARG1=Patient;Protagonist 9:0-rel 10:1-ARGM-MNR 
nw/wsj/16/wsj_1603.parse 7 3 gold make-v 29.3 Causation make.02 null ----- 0:1-ARG0=Agent 3:0-rel 4:2-ARG1=Theme 8:1-ARGM-TMP 
nw/wsj/16/wsj_1603.parse 11 7 gold jail-v 9.10 Imprisonment jail.01 1 ----- 0:1-ARGM-CAU 4:1*8:1-ARG1=Theme 7:0-rel 9:1-ARGM-TMP 11:1-ARGM-TMP 
nw/wsj/16/wsj_1603.parse 12 4 gold register-v 101 NF register.02 null ----- 1:1*5:1-ARG1=Theme 4:0-rel 
nw/wsj/16/wsj_1603.parse 12 7 gold use-v 105 IN use.01 null ----- 1:1*5:1-ARG0 7:0-rel 8:1-ARG1 
nw/wsj/16/wsj_1603.parse 12 13 gold accuse-v 81 Notification_of_charges accuse.01 1 ----- 0:1-ARGM-ADV 11:1*14:1-ARG1=Theme;Accused 13:0-rel 15:1-ARG2=Attribute;Charges 
nw/wsj/16/wsj_1603.parse 12 17 gold conduct-v 51.7 Cotheme conduct.01 null ----- 11:1*14:1*16:1-ARG0=Agent;Theme 17:0-rel 18:1-ARG1=Theme;Cotheme 
nw/wsj/16/wsj_1603.parse 13 3 gold hold-v 15.1-1 Detaining hold.01 null ----- 0:1-ARGM-TMP 1:1*4:1-ARG1=Theme 3:0-rel 5:1-ARGM-TMP 8:1-ARGM-MNR 
nw/wsj/16/wsj_1603.parse 14 11 gold gather-v 22.3-2 Come_together gather.01 null ----- 6:1*13:1-ARGM-TMP 7:2-ARG0=Agent 11:0-rel 12:1-ARG1=Patient 
nw/wsj/16/wsj_1603.parse 16 5 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1-ARGM-TMP 3:1-ARG0=Agent;Seller 5:0-rel 6:2-ARG1=Theme;Goods 
nw/wsj/16/wsj_1603.parse 18 2 gold earn-v 13.5.1-1 NF earn.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Theme 6:1-ARGM-LOC 
nw/wsj/16/wsj_1603.parse 19 16 gold use-v 105 IN use.01 null ----- 6:1*10:1*17:1-ARG1 11:2-ARG0 16:0-rel 18:1-ARG2 6:1*10:1-LINK-SLC 
nw/wsj/16/wsj_1603.parse 19 25 gold demand-v 60-1 Request demand.01 null ----- 0:1-ARG0 2:1-ARGM-TMP 25:0-rel 26:1-ARG1 
nw/wsj/16/wsj_1603.parse 21 11 gold win-v 13.5.1 NF win.01 null ----- 10:1-ARG0=Agent 11:0-rel 
nw/wsj/16/wsj_1603.parse 21 16 gold say-v 37.7-1 IN say.01 null ----- 1:2*17:1-ARG1=Topic 15:1-ARG0=Agent 16:0-rel 
nw/wsj/16/wsj_1603.parse 22 14 gold work-v 73-3 IN work.01 null ----- 14:0-rel 15:0-ARG0=Agent 
nw/wsj/16/wsj_1603.parse 23 0 gold say-v 37.7-1 IN say.01 null ----- 0:0-rel 1:1*6:3-ARG1=Topic 2:1-ARG0=Agent 
nw/wsj/16/wsj_1603.parse 24 7 gold accuse-v 81 Notification_of_charges accuse.01 1 ----- 0:0-ARGM-DIS 1:2-ARGM-TMP 5:1-ARG0=Agent;Arraign_authority 7:0-rel 8:1*10:1-ARG1=Theme;Accused 9:1-ARG2=Attribute;Charges 
nw/wsj/16/wsj_1603.parse 24 11 gold steal-v 10.5-1 Theft steal.01 null ----- 8:1*10:1-ARG0=Agent;Perpetrator 11:0-rel 12:1-ARG1=Theme;Goods 
nw/wsj/16/wsj_1603.parse 24 14 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 8:1*10:1-ARG0=Agent;Recipient 14:0-rel 15:1-ARG1=Theme;Theme 16:1-ARGM-MNR 
nw/wsj/16/wsj_1603.parse 24 19 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 8:1*10:1-ARG0=Agent;Buyer 19:0-rel 20:1-ARG1=Theme;Goods 
nw/wsj/16/wsj_1603.parse 24 20 gold steal-v 10.5-1 Theft steal.01 null ----- 20:0-rel 21:0-ARG1=Theme;Goods 
nw/wsj/16/wsj_1603.parse 25 1 gold warn-v 37.9-1 NF warn.01 1 ----- 0:1*13:1-ARG2=Recipient 1:0-rel 3:1-ARG1=Topic 
nw/wsj/16/wsj_1603.parse 25 9 gold jail-v 9.10 Imprisonment jail.01 1 ----- 4:1*6:1*10:1-ARG1=Theme 9:0-rel 11:1-ARGM-TMP 
nw/wsj/16/wsj_1603.parse 26 3 gold give-v 13.1-1 Giving give.01 null ----- 0:1*4:1-ARG2=Recipient;Recipient 3:0-rel 5:2-ARG1=Theme;Theme 12:1-ARGM-TMP 
nw/wsj/16/wsj_1603.parse 26 10 gold leave-v 51.2-1 Departing leave.01 null ----- 0:1*8:1-ARG0=Theme;Theme 5:1*7:1*11:1-ARGM-TMP 10:0-rel 0:1*8:1-LINK-PRO 5:1*7:1-LINK-SLC 
nw/wsj/16/wsj_1603.parse 26 18 gold confiscate-v 10.5 Removing confiscate.01 1 ----- 13:1*19:1-ARG1=Theme;Theme 18:0-rel 
nw/wsj/16/wsj_1603.parse 27 7 gold wear-v 41.3.1 Wearing wear.01 null ----- 1:1*4:1*8:1-ARG1=Theme;Clothing 5:1-ARG0=Agent;Wearer 7:0-rel 1:1*4:1-LINK-SLC 
nw/wsj/16/wsj_1603.parse 27 12 gold move-v 11.2 Motion move.01 null ----- 0:1-ARGM-ADV 10:1-ARG1 12:0-rel 13:1-ARG2 
nw/wsj/16/wsj_1603.parse 27 16 gold own-v 100 NF own.01 null ----- 14:1*17:1-ARG1=Theme 16:0-rel 18:1-ARG0=Pivot 14:1*17:1-LINK-PSV 
nw/wsj/16/wsj_1603.parse 29 1 gold rejoin-v 22.1-2-1 NF rejoin.01 1 ----- 0:1*15:1-ARG0=Agent 1:0-rel 2:1-ARG1=Patient 4:1-ARGM-TMP 
nw/wsj/16/wsj_1603.parse 29 8 gold begin-v 55.1-1 Activity_start begin.01 null ----- 0:1*15:1-ARG0=Agent;Agent 8:0-rel 9:2-ARG1=Theme;Activity 15:2-ARGM-PRD 
nw/wsj/16/wsj_1603.parse 30 4 gold keep-v 55.6 Activity_ongoing keep.02 null ----- 0:2*7:1-ARG0=Agent 4:0-rel 5:1-ARG1=Theme 6:1-ARGM-MNR 
nw/wsj/16/wsj_1603.parse 30 11 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:2*7:1-ARG0=Agent;Seller 11:0-rel 12:1-ARG1=Theme;Goods 
nw/wsj/16/wsj_1603.parse 31 2 gold achieve-v 55.2 Accomplishment achieve.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Theme 5:1-ARGM-TMP 
nw/wsj/16/wsj_1603.parse 32 19 gold allow-v 64 IN allow.01 null ----- 0:1-ARGM-TMP 3:2-ARGM-TMP 17:1-ARG0 19:0-rel 20:2-ARG1 
nw/wsj/16/wsj_1603.parse 32 23 gold resume-v 55.1-1 Process_resume resume.01 null ----- 20:1-ARG0=Agent 23:0-rel 24:1-ARG1=Theme;Process 
nw/wsj/16/wsj_1603.parse 33 6 gold invite-v 102 Request invite.01 null ----- 0:1-ARGM-TMP 4:1*7:1-ARG1 6:0-rel 8:1-ARG2 9:1-ARGM-PRD 15:2-ARGM-PRP 
nw/wsj/16/wsj_1603.parse 34 8 gold commend-v 33 Judgment_communication commend.01 1 ----- 0:1*6:1-ARG0=Agent;Communicator 8:0-rel 9:1-ARG1=Theme;Evaluee 
nw/wsj/16/wsj_1603.parse 36 3 gold pack-v 9.7-1-1 Filling pack.01 2 ----- 0:2;10:2-ARG0=Agent;Agent 3:0-rel 4:2-ARG1=Destination;Goal 
nw/wsj/16/wsj_1603.parse 37 1 gold sleep-v 40.4 Sleep sleep.01 1 ----- 0:1-ARG0=Agent;Sleeper 1:0-rel 2:1-ARGM-LOC 
nw/wsj/16/wsj_1603.parse 38 18 gold cover-v 9.8 Adorning cover.02 null ----- 11:2*16:1*17:1-ARG2=Theme;Theme 18:0-rel 19:2-ARG1=Destination;Location 11:2*16:1-LINK-SLC 
nw/wsj/16/wsj_1603.parse 39 27 gold make-v 26.1-1 Causation make.01 null ----- 26:0-ARGM-TMP 27:0-rel 28:0-ARG1=Product 
nw/wsj/16/wsj_1603.parse 39 35 gold fill-v 9.8 Filling fill.01 null ----- 33:1*36:1-ARG1=Destination;Goal 35:0-rel 37:1-ARG2=Theme;Theme 33:1*36:1-LINK-PSV 
nw/wsj/16/wsj_1603.parse 40 2 gold talk-v 37.5 IN talk.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARGM-MNR 4:1-ARG1=Topic 
nw/wsj/16/wsj_1603.parse 41 5 gold have-v 100 IN have.03 null ----- 0:1-ARGM-DIS 2:1-ARGM-DIS 4:1-ARG0=Pivot 5:0-rel 6:1-ARG1=Theme 
nw/wsj/16/wsj_1603.parse 42 3 gold allow-v 64 IN allow.01 null ----- 0:1*8:1-ARGM-TMP 1:1-ARG0 3:0-rel 4:2-ARG1 
nw/wsj/16/wsj_1603.parse 42 6 gold resume-v 55.1-1 Process_resume resume.01 null ----- 4:1-ARG0=Agent 6:0-rel 7:1-ARG1=Theme;Process 
nw/wsj/16/wsj_1603.parse 42 11 gold release-v 80-1 Releasing release.01 null ----- 0:2-ARGM-TMP 10:1-ARG0=Cause 11:0-rel 12:3-ARG1=Theme 
nw/wsj/16/wsj_1603.parse 43 3 gold return-v 13.2-2 NF return.02 null ----- 0:1-ARG0=Agent 2:0-ARGM-NEG 3:0-rel 4:2-ARG1=Theme 
nw/wsj/16/wsj_1603.parse 44 3 gold return-v 13.2-2 NF return.02 null ----- 0:0-ARGM-NEG 2:1-ARG0=Agent 3:0-rel 4:2-ARG1=Theme 
nw/wsj/16/wsj_1603.parse 44 11 gold value-v 54.4 Assessing value.01 1 ----- 4:1*9:1*12:1-ARG1=Theme;Feature 10:1-ARG0=Agent;Assessor 11:0-rel 13:1-ARG2=Value;Value 4:1*9:1-LINK-SLC 
nw/wsj/16/wsj_1603.parse 45 1 gold want-v 32.1-1-1 Desiring want.01 null ----- 0:1*2:1-ARG0=Pivot;Experiencer 1:0-rel 2:2-ARG1=Theme;Event/Focal_participant 11:1-ARGM-DIS 
nw/wsj/16/wsj_1603.parse 45 4 gold recover-v 13.5.2 NF recover.02 1 ----- 0:1*2:1-ARG0=Agent 4:0-rel 5:2-ARG1=Theme 
nw/wsj/16/wsj_1603.parse 46 2 gold want-v 32.1-1-1 Desiring want.01 null ----- 1:1-ARG0=Pivot;Experiencer 2:0-rel 3:1-ARG1=Theme;Event/Focal_participant 5:1-ARGM-DIR 
nw/wsj/16/wsj_1603.parse 46 9 gold say-v 37.7-1 IN say.01 null ----- 1:2*10:1-ARG1=Topic 8:1-ARG0=Agent 9:0-rel 
nw/wsj/16/wsj_1603.parse 49 6 gold seize-v 10.5 NF seize.01 2 ----- 0:1-ARGM-TMP 5:1*14:1-ARG0=Agent 6:0-rel 7:2-ARG1=Theme 14:2-ARGM-PRP 
nw/wsj/16/wsj_1603.parse 49 9 gold provide-v 13.4.1-2 Supply provide.01 null ----- 7:1*10:1-ARG1=Theme 9:0-rel 11:1-ARG0=Agent 7:1*10:1-LINK-PSV 
nw/wsj/16/wsj_1603.parse 49 16 gold probe-v 35.2 Scrutiny probe.01 1 ----- 5:1*14:1-ARG0=Agent;Cognizer 16:0-rel 17:2-ARG1=Location;Ground 
nw/wsj/16/wsj_1603.parse 50 7 gold have-v 100 IN have.03 null ----- 0:2-ARG0=Pivot 7:0-rel 8:1-ARG1=Theme 
nw/wsj/16/wsj_1603.parse 50 14 gold lose-v 13.2 NF lose.02 null ----- 12:1-ARG0=Agent 14:0-rel 15:2-ARG1=Theme 21:1-ARGM-TMP 25:1-ARGM-CAU 
nw/wsj/16/wsj_1603.parse 51 1 gold appoint-v 29.1 Change_of_leadership appoint.01 null ----- 0:1*9:1-ARG1=Theme;New_leader 1:0-rel 2:2-ARG2=Result;Role 5:1-ARGM-TMP 
nw/wsj/16/wsj_1603.parse 51 12 gold turn-v 26.6.2 Cause_change turn.02 null ----- 0:1*9:1-ARG0=Agent 0:2-ARGM-PRD 11:1-ARGM-MNR 12:0-rel 13:1-ARG1=Patient 17:1-ARG2=Goal 
nw/wsj/16/wsj_1603.parse 52 1 gold soar-v 45.6-1 Change_position_on_a_scale soar.01 2 ----- 0:1-ARG1=Patient;Item 1:0-rel 2:1-ARGM-ADV 
nw/wsj/16/wsj_1603.parse 53 2 gold electrify-v 31.1 NF electrify.01 1 ----- 0:1*3:1-ARG1=Experiencer 2:0-rel 4:1-ARG0=Stimulus 
nw/wsj/16/wsj_1603.parse 53 10 gold appall-v 31.1 NF appall.01 1 ----- 0:1*11:1-ARG1=Experiencer 10:0-rel 12:1-ARG0=Stimulus 
nw/wsj/16/wsj_1603.parse 54 2 gold recount-v 37.7-1 Statement recount.01 1 ----- 0:1-ARG0=Agent;Speaker 2:0-rel 3:2*13:2-ARG1=Topic;Message 
nw/wsj/16/wsj_1603.parse 54 11 gold wound-v 31.1 NF wound.01 null ----- 11:0-rel 12:0-ARG1=Experiencer 
nw/wsj/16/wsj_1603.parse 54 15 gold prove-v 29.4-1-1 Reasoning prove.01 null ----- 10:1*13:1-ARG0 15:0-rel 17:1-ARGM-MNR 19:1-ARG1=Theme 10:1*13:1-LINK-PRO 
nw/wsj/16/wsj_1603.parse 55 17 gold confiscate-v 10.5 Removing confiscate.01 1 ----- 2:1*22:1-ARGM-MNR 3:3-ARG0=Agent;Agent/Cause 11:1-ARGM-TMP 17:0-rel 18:1-ARG1=Theme;Theme 19:1-ARG2=Source;Source 
nw/wsj/16/wsj_1603.parse 56 22 gold stir-v 31.1 Experiencer_obj stir.02 2 ----- 11:2*20:1*21:1-ARG0=Stimulus;Stimulus 22:0-rel 23:1-ARG1=Experiencer;Experiencer 24:1-ARGM-MNR 11:2*20:1-LINK-SLC 
nw/wsj/16/wsj_1603.parse 56 34 gold veil-v 9.9 NF veil.01 null ----- 33:0-ARGM-MNR 34:0-rel 35:0-ARG1=Destination 
nw/wsj/16/wsj_1603.parse 57 3 gold make-v 26.1-1 IN make.01 null ----- 0:1-ARG0=Agent 2:1-ARGM-MNR 3:0-rel 4:1-ARG1=Product 
nw/wsj/16/wsj_1603.parse 58 12 gold react-v 31.3-9 NF react.01 1 ----- 1:3-ARG0=Experiencer 12:0-rel 13:1-ARGM-MNR 15:1-ARG1=Stimulus 
nw/wsj/16/wsj_1603.parse 58 20 gold say-v 37.7-1 IN say.01 null ----- 1:4*22:1-ARG1=Topic 20:0-rel 23:2-ARG0=Agent 
nw/wsj/16/wsj_1603.parse 59 9 gold fire-v 10.10 Firing fire.02 null ----- 0:1-ARGM-TMP 6:1*10:1-ARG1=Theme 9:0-rel 11:1-ARGM-TMP 
nw/wsj/16/wsj_1603.parse 60 9 gold continue-v 55.3 Process_continue continue.01 null ----- 3:2,5:1*7:1*8:1-ARG1 9:0-rel 10:1-ARGM-TMP 3:2*7:1-LINK-SLC 
nw/wsj/16/wsj_1603.parse 61 16 gold say-v 37.7-1 IN say.01 null ----- 1:2*17:1-ARG1=Topic 16:0-rel 18:2-ARG0=Agent 
nw/wsj/16/wsj_1603.parse 63 11 gold bear-v 31.2 NF bear.01 null ----- 8:1*12:1-ARG1=Stimulus 9:1-ARG0=Experiencer 11:0-rel 
nw/wsj/16/wsj_1603.parse 66 5 gold keep-v 55.6 Cause_to_continue keep.04 null ----- 3:1-ARG0=Agent 5:0-rel 6:1-ARG1=Theme 9:1-ARG2 11:1-ARGM-TMP 
nw/wsj/16/wsj_1603.parse 67 4 gold wait-v 47.1-1 IN wait.01 null ----- 0:1*8:1-ARG1=Theme 3:0-ARGM-NEG 4:0-rel 5:1-ARG2 8:2-ARGM-PRP 
nw/wsj/16/wsj_1603.parse 67 10 gold get-v 13.5.1-1 IN get.01 null ----- 0:1*8:1-ARG0=Agent 10:0-rel 11:1-ARG1=Theme 
nw/wsj/16/wsj_1603.parse 68 11 gold establish-v 97.1 NF establish.01 null ----- 0:1*9:1-ARG0 11:0-rel 12:1-ARG1 16:1-ARG2 
nw/wsj/16/wsj_1603.parse 69 5 gold include-v 65 NF include.01 null ----- 0:1*3:1*4:1-ARG2=Location 5:0-rel 6:2-ARG1=Theme 0:1*3:1-LINK-SLC 
nw/wsj/16/wsj_1603.parse 69 7 gold alleviate-v 80 NF alleviate.01 1 ----- 6:1-ARG0=Cause 7:0-rel 8:1-ARG1=Theme 12:1-ARGM-TMP 
nw/wsj/16/wsj_1603.parse 69 17 gold help-v 72-1 Assistance help.01 null ----- 0:2-ARG0=Agent;Helper 17:0-rel 18:1-ARG1=Theme;Goal/Focal_entity 
nw/wsj/16/wsj_1603.parse 71 8 gold fight-v 36.3-2 Hostile_encounter fight.01 null ----- 0:2*6:1*7:1-ARG0=Agent;Side_1 8:0-rel 9:1-ARG1=Co-Agent;Side_2 0:2*6:1-LINK-SLC 
nw/wsj/16/wsj_1603.parse 71 21 gold switch-v 26.6.2-1 IN switch.01 1 ----- 0:3-ARGM-PRD 20:1-ARG0=Agent 21:0-rel 22:1-ARGM-MNR 23:1-ARG2=Goal 25:1-ARGM-TMP 
nw/wsj/16/wsj_1603.parse 73 2 gold ride-v 51.4.2 IN ride.01 null ----- 1:1-ARG0=Agent 2:0-rel 3:1-ARGM-DIR 
nw/wsj/16/wsj_1603.parse 73 26 gold keep-v 55.6 Cause_to_continue keep.04 null ----- 0:1-ARGM-ADV 6:2-ARG0=Agent 26:0-rel 27:1-ARG1=Theme 28:1-ARG2 
nw/wsj/16/wsj_1603.parse 75 3 gold steer-v 51.7 NF steer.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1*9:1-ARG1=Theme 6:1-ARG2=Destination 9:2-ARGM-PRP 
nw/wsj/16/wsj_1603.parse 75 11 gold see-v 30.1-1 Perception_experience see.01 null ----- 4:1*9:1-ARG0=Experiencer;Perceiver_passive 11:0-rel 12:2-ARG1=Stimulus;Phenomenon 
nw/wsj/16/wsj_1603.parse 76 3 gold respond-v 37.7 Communication_response respond.01 null ----- 0:1-ARG0=Agent;Speaker 3:0-rel 4:1-ARG2=Topic;Message/Trigger 
nw/wsj/16/wsj_1603.parse 76 6 gold declare-v 29.4-1-1-1 Statement declare.01 null ----- 5:1-ARG0=Agent;Speaker 6:0-rel 7:1,8:2-ARG1=Theme;Addressee 
nw/wsj/16/wsj_1603.parse 77 3 gold call-v 29.3 IN call.01 null ----- 1:1-ARG0=Agent 3:0-rel 4:1*5:1-ARG1=Theme 5:2-ARG2=Result 
nw/wsj/16/wsj_1603.parse 77 13 gold tell-v 37.2-1 Telling tell.01 null ----- 1:2*15:1-ARG1=Topic;Message 11:1-ARG0=Agent;Speaker 13:0-rel 14:1-ARG2=Recipient;Addressee 
nw/wsj/16/wsj_1603.parse 79 3 gold turn-v 26.6.2 Cause_change turn.02 null ----- 0:1-ARG0=Agent 2:1-ARGM-DIS 3:0-rel 4:1-ARG1=Patient 7:1-ARG2=Goal 
nw/wsj/16/wsj_1603.parse 79 9 gold bud-v 45.5 NF bud.01 null ----- 9:0-rel 10:0-ARG1=Patient 
nw/wsj/16/wsj_1603.parse 79 16 gold remain-v 47.1-1 State_continue remain.01 null ----- 13:2-ARG1=Theme 16:0-rel 17:1-ARG3 
nw/wsj/16/wsj_1603.parse 82 6 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-TMP 3:1-ARG0=Agent 6:0-rel 7:1-ARG1=Topic 
nw/wsj/16/wsj_1603.parse 82 9 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 8:1-ARG0=Agent;Buyer 9:0-rel 10:3-ARG1=Theme;Goods 
nw/wsj/16/wsj_1603.parse 83 12 gold transport-v 11.1 Bringing transport.01 1 ----- 3:2*9:1*10:1-ARG1=Theme 12:0-rel 13:1-ARG1=Theme 15:1-ARG2=Destination 3:2*9:1-LINK-SLC 
nw/wsj/16/wsj_1603.parse 84 14 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 6:2*11:1*12:1*15:1-ARG1=Theme;Goods 14:0-rel 16:1-ARGM-COM 19:1-ARGM-MNR 6:2*11:1-LINK-SLC 
nw/wsj/16/wsj_1603.parse 85 7 gold start-v 55.1-1 Activity_start start.01 null ----- 0:1-ARGM-TMP 4:1*8:2-ARG1=Theme;Activity 7:0-rel 14:2-ARGM-PRD 
nw/wsj/16/wsj_1603.parse 85 15 gold take-v 10.5 Taking take.01 null ----- 4:1*14:1-ARG0=Agent 15:0-rel 16:2-ARG1=Theme 
nw/wsj/16/wsj_1603.parse 85 22 gold establish-v 55.5-1 Intentionally_create establish.01 null ----- 21:0-ARGM-TMP 22:0-rel 26:0,28:0,29:0-ARG1 
nw/wsj/16/wsj_1603.parse 85 39 gold own-v 100 NF own.01 null ----- 34:1*37:1*38:1-ARG0=Pivot 39:0-rel 42:2-ARG1=Theme 34:1*37:1-LINK-SLC 
nw/wsj/16/wsj_1603.parse 86 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/16/wsj_1603.parse 87 1 gold expect-v 62 IN expect.01 null ----- 0:1-ARG0=Experiencer 1:0-rel 2:2-ARG1=Theme 
nw/wsj/16/wsj_1603.parse 87 7 gold gain-v 45.6-1 Change_position_on_a_scale gain.01 null ----- 2:1-ARG1=Patient;Item 7:0-rel 8:1-ARGM-TMP 
nw/wsj/16/wsj_1603.parse 88 12 gold create-v 27 Creating create.01 null ----- 8:2-ARG0=Cause 11:0-ARGM-MOD 12:0-rel 13:2-ARG1=Theme 
nw/wsj/16/wsj_1603.parse 89 4 gold mind-v 31.3-2 NF mind.01 1 ----- 1:1-ARG0=Experiencer 3:0-ARGM-NEG 4:0-rel 5:2-ARG1=Stimulus 
nw/wsj/16/wsj_1603.parse 89 13 gold say-v 37.7-1 IN say.01 null ----- 1:2*14:1-ARG1=Topic 12:1-ARG0=Agent 13:0-rel 
nw/wsj/16/wsj_1603.parse 90 16 gold compete-v 36.4-1 Competition compete.01 1 ----- 6:1-ARGM-ADV 13:1-ARG0=Agent 14:0-ARGM-MOD 15:0-ARGM-NEG 16:0-rel 17:1-ARG1=Co-Agent 
