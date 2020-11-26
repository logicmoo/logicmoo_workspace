nw/wsj/22/wsj_2229.parse 0 7 gold remind-v 37.2 NF remind.01 1 ----- 0:2-ARG0=Agent 7:0-rel 8:1-ARG2=Topic 9:1-ARG1=Recipient 
nw/wsj/22/wsj_2229.parse 0 16 gold tell-v 37.2-1 Speak_on_topic tell.01 null ----- 10:1*12:1*17:1-ARG1=Topic 13:1-ARG0=Agent 16:0-rel 10:1*12:1-LINK-SLC 
nw/wsj/22/wsj_2229.parse 1 2 gold ask-v 37.1.2 Questioning ask.01 null ----- 0:1*9:1-ARGM-TMP 1:1*12:1-ARG2=Recipient;Addressee 2:0-rel 5:2-ARG1=Topic;Message 
nw/wsj/22/wsj_2229.parse 1 15 gold reply-v 37.7 Communication_response reply.01 1 ----- 0:2-ARGM-TMP 1:1*4:1*12:1-ARG0=Agent;Speaker 15:0-rel 18:2-ARG2=Topic;Message/Trigger 
nw/wsj/22/wsj_2229.parse 4 20 gold erect-v 26.1 Building erect.01 1 ----- 15:1*21:1-ARG1=Product;Created_entity 17:0-ARGM-MOD 20:0-rel 22:2-ARGM-PRP 
nw/wsj/22/wsj_2229.parse 4 24 gold stop-v 55.4-1 Halt stop.01 null ----- 15:1*21:1*22:1-ARG0=Agent 24:0-rel 25:1-ARG1=Theme;Theme 27:1-ARGM-TMP 
nw/wsj/22/wsj_2229.parse 5 2 gold begin-v 55.1-1 Process_start begin.01 null ----- 0:1-ARG1=Theme;Event 2:0-rel 3:1-ARGM-TMP 5:2-ARGM-TMP 
nw/wsj/22/wsj_2229.parse 6 5 gold expect-v 62 IN expect.01 null ----- 1:1-ARG0=Experiencer 5:0-rel 6:2-ARG1=Theme 
nw/wsj/22/wsj_2229.parse 6 8 gold patch-v 9.9 NF patch.01 5 ----- 1:1*6:1-ARG0=Agent 8:0-rel 9:1-ARGM-PRD 10:2-ARG1=Destination 
nw/wsj/22/wsj_2229.parse 6 23 gold get-v 13.5.1-1 IN get.01 null ----- 21:1-ARG0=Agent 23:0-rel 24:1-ARG1=Theme 25:1-ARG2=Source 
nw/wsj/22/wsj_2229.parse 6 47 gold end-v 55.4-1 Cause_to_end end.01 null ----- 38:2-ARG1=Theme 47:0-rel 
nw/wsj/22/wsj_2229.parse 7 5 gold make-v 26.1-1 IN make.07 null ----- 0:3;13:2-ARG0=Material 5:0,6:1-rel 7:1-ARG1=Product 10:1-ARGM-DIS 
nw/wsj/22/wsj_2229.parse 7 22 gold signal-v 37.4 NF signal.01 2 ----- 14:2-ARG0=Agent 22:0-rel 23:2-ARG1=Topic 
nw/wsj/22/wsj_2229.parse 7 39 gold try-v 61 Attempt try.01 null ----- 38:1*40:1-ARG0=Agent 39:0-rel 40:2-ARG1=Theme 
nw/wsj/22/wsj_2229.parse 7 42 gold lower-v 9.4 NF lower.01 2 ----- 38:1*40:1-ARG0=Agent 42:0-rel 43:1-ARG1=Theme 
nw/wsj/22/wsj_2229.parse 8 8 gold hear-v 30.1-1-1 IN hear.01 null ----- 0:1-ARGM-DIS 3:2*9:1;10:1-ARG1=Stimulus 8:0-rel 21:1-ARGM-ADV 
nw/wsj/22/wsj_2229.parse 8 25 gold suggest-v 37.7-1-1 Statement suggest.01 null ----- 22:1-ARG0=Agent;Speaker 25:0-rel 26:1-ARG1=Topic;Message 
nw/wsj/22/wsj_2229.parse 8 29 gold play-v 26.7-1-1 Cause_to_make_noise play.01 null ----- 27:1-ARG0=Agent 28:1-ARGM-ADV 29:0-rel 30:2-ARG1=Theme 
nw/wsj/22/wsj_2229.parse 8 34 gold write-v 25.2 Text_creation write.01 null ----- 30:1*32:1*35:1-ARG1=Theme;Text 33:1-ARG0=Agent;Author 34:0-rel 30:1*32:1-LINK-SLC 
nw/wsj/22/wsj_2229.parse 9 3 gold spook-v 31.1 Experiencer_obj spook.01 1 ----- 0:1*1:1-ARG0=Stimulus;Stimulus 2:1-ARGM-ADV 3:0-rel 4:2-ARG1=Experiencer;Experiencer 
nw/wsj/22/wsj_2229.parse 11 2 gold lead-v 51.7 Cotheme lead.01 null ----- 0:1-ARG0=Agent;Theme 2:0-rel 3:1-ARG1=Theme;Cotheme 5:1-ARGM-DIR 6:1-ARGM-ADV 
nw/wsj/22/wsj_2229.parse 12 8 gold open-v 48.1.1 NF open.02 null ----- 0:2-ARG1=Theme 8:0-rel 9:1-ARGM-TMP 10:1-ARGM-EXT 15:1-ARGM-MNR 
nw/wsj/22/wsj_2229.parse 13 1 gold open-v 48.1.1 NF open.02 null ----- 0:1-ARG1=Theme 1:0-rel 2:1-ARGM-TMP 3:1-ARGM-EXT 8:1-ARGM-MNR 
nw/wsj/22/wsj_2229.parse 16 3 gold reveal-v 78-1-1 Evidence reveal.01 null ----- 0:1-ARG0 3:0-rel 4:1-ARG1 
nw/wsj/22/wsj_2229.parse 16 10 gold secure-v 13.5.1-1 Getting secure.01 null ----- 5:1*12:1-ARGM-LOC 6:1*11:1-ARG1=Theme;Theme 10:0-rel 
nw/wsj/22/wsj_2229.parse 16 21 gold affect-v 31.1 NF affect.01 null ----- 14:2*22:1-ARG1=Experiencer 20:0-ARGM-NEG 21:0-rel 23:1-ARGM-TMP 
nw/wsj/22/wsj_2229.parse 17 33 gold concentrate-v 87.1 NF concentrate.01 null ----- 29:1*35:1-ARGM-LOC 30:1*34:1-ARG2 33:0-rel 
nw/wsj/22/wsj_2229.parse 20 2 gold react-v 31.3-9 NF react.01 1 ----- 0:1-ARG0=Experiencer 2:0-rel 3:2-ARG2 
nw/wsj/22/wsj_2229.parse 20 19 gold force-v 59 NF force.01 null ----- 0:1*18:1-ARG0=Agent 19:0-rel 20:1-ARG1=Patient 21:2-ARG2=Result 
nw/wsj/22/wsj_2229.parse 20 26 gold plan-v 62 Purpose plan.01 null ----- 26:0-rel 27:1,30:0,32:0-ARG1 
nw/wsj/22/wsj_2229.parse 21 10 gold decline-v 45.6-1 Change_position_on_a_scale decline.01 null ----- 4:2-ARG1=Patient;Item 10:0-rel 11:1-ARGM-LOC 
nw/wsj/22/wsj_2229.parse 21 18 gold plan-v 62 Purpose plan.01 null ----- 15:1-ARG0=Experiencer;Agent 18:0-rel 19:2-ARG1 
nw/wsj/22/wsj_2229.parse 21 21 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 19:1-ARG0=Agent;Seller 21:0-rel 22:3-ARG1=Theme;Goods 29:1-ARGM-TMP 
nw/wsj/22/wsj_2229.parse 21 34 gold experience-v 30.2 Feeling experience.01 1 ----- 15:2-ARG0=Experiencer;Experiencer 34:0-rel 35:1-ARG1=Stimulus;Emotion/Emotional_state 
nw/wsj/22/wsj_2229.parse 22 7 gold put-v 9.1-2 IN put.01 null ----- 6:1-ARG0=Agent 7:0-rel 8:1-ARG2=Destination 9:2-ARG1=Theme 
nw/wsj/22/wsj_2229.parse 22 16 gold give-v 13.1-1 Giving give.01 null ----- 9:1*12:1*17:1-ARG1=Theme;Theme 13:1-ARG0=Agent;Donor 16:0-rel 18:1-ARG2=Recipient;Recipient 20:1-ARGM-ADV 9:1*12:1-LINK-SLC 
nw/wsj/22/wsj_2229.parse 22 28 gold see-v 30.1-1 Perception_experience see.01 null ----- 25:1*29:1-ARG1 26:1-ARG0 28:0-rel 
nw/wsj/22/wsj_2229.parse 23 27 gold remain-v 47.1-1 State_continue remain.01 null ----- 18:1-ARG1=Theme 27:0-rel 28:1-ARG3 
nw/wsj/22/wsj_2229.parse 23 38 gold distract-v 31.1 NF distract.01 1 ----- 33:1-ARG0=Stimulus 36:0-ARGM-MOD 37:0-ARGM-NEG 38:0-rel 39:1-ARG1=Experiencer 40:1-ARG2 
nw/wsj/22/wsj_2229.parse 24 15 gold cause-v 27 Causation cause.01 1 ----- 9:2-ARG0=Cause;Cause 15:0-rel 16:1-ARG1=Theme;Effect 
nw/wsj/22/wsj_2229.parse 25 20 gold confirm-v 78-1-1 Evidence confirm.01 null ----- 3:3*18:1-ARG0 20:0-rel 21:2-ARG1 
nw/wsj/22/wsj_2229.parse 26 8 gold approve-v 64 NF approve.01 null ----- 2:1*4:1*16:1-ARGM-LOC 5:1-ARG0 8:0-rel 9:1-ARG1 2:1*4:1-LINK-SLC 
nw/wsj/22/wsj_2229.parse 28 34 gold impose-v 63 NF impose.01 null ----- 23:2*33:1-ARG0=Agent 34:0-rel 35:1-ARG1=Theme 40:1-ARG2 
nw/wsj/22/wsj_2229.parse 29 10 gold allow-v 64 IN allow.01 null ----- 9:1-ARG0 10:0-rel 11:3-ARG1 
nw/wsj/22/wsj_2229.parse 31 18 gold recognize-v 29.5-1 Becoming_aware recognize.02 null ----- 14:1-ARG0 17:0-ARGM-MOD 18:0-rel 19:1-ARG1 21:2-ARGM-MNR 
nw/wsj/22/wsj_2229.parse 31 41 gold impose-v 63 NF impose.01 null ----- 32:1*39:1-ARG0=Agent 41:0-rel 42:1-ARG1=Theme 
nw/wsj/22/wsj_2229.parse 32 5 gold get-v 26.6.2 IN get.03 null ----- 0:1-ARGM-DIS 2:1-ARG1=Patient 5:0-rel 6:1-ARG2=Goal 
nw/wsj/22/wsj_2229.parse 32 14 gold ride-v 51.4.2 IN ride.01 null ----- 13:1-ARG0=Agent 14:0-rel 15:1-ARG1=Theme 17:1-ARGM-DIR 18:1-ARGM-TMP 
nw/wsj/22/wsj_2229.parse 34 5 gold get-v 26.6.2 IN get.03 null ----- 0:1-ARG1=Patient 5:0-rel 6:1-ARG2=Goal 
nw/wsj/22/wsj_2229.parse 34 6 gold kill-v 42.1-1 Killing kill.01 null ----- 0:1*7:1-ARG1 6:0-rel 8:1-ARGM-TMP 12:1-ARGM-TMP 
nw/wsj/22/wsj_2229.parse 35 2 gold identify-v 88.2 NF identify.02 null ----- 1:1-ARG0=Experiencer 2:0-rel 3:1-ARG1=Stimulus 
nw/wsj/22/wsj_2229.parse 35 28 gold come-v 48.1.1 NF come.03 null ----- 12:3*26:1-ARG1=Theme 28:0-rel 29:1-ARG2 
nw/wsj/22/wsj_2229.parse 36 11 gold have-v 100 IN have.03 null ----- 0:1-ARGM-ADV 9:1*13:1-ARG0=Pivot 10:1-ARGM-ADV 11:0-rel 12:2-ARG1=Theme 23:1-ARGM-ADV 
nw/wsj/22/wsj_2229.parse 36 15 gold fear-v 31.2-1 Experiencer_focus fear.01 null ----- 9:1*13:1-ARG0 15:0-rel 16:1-ARGM-CAU 
nw/wsj/22/wsj_2229.parse 37 5 gold understand-v 87.2-1 Grasp understand.01 null ----- 4:1-ARG0 5:0-rel 6:1-ARG1 7:2-ARGM-MNR 
nw/wsj/22/wsj_2229.parse 38 7 gold harm-v 31.1 NF harm.01 1 ----- 1:1*8:1-ARG1=Experiencer 5:1-ARGM-DIS 6:0-ARGM-NEG 7:0-rel 9:1-ARGM-EXT 11:1-ARG0=Stimulus 
nw/wsj/22/wsj_2229.parse 38 28 gold impose-v 63 NF impose.01 null ----- 0:1-ARGM-ADV 24:1-ARG0=Agent 26:0-ARGM-MOD 27:1-ARGM-ADV 28:0-rel 29:1-ARG1=Theme 
nw/wsj/22/wsj_2229.parse 39 18 gold draw-v 25.2 IN draw.01 null ----- 3:1-ARG0 18:0-rel 19:1-ARG1 21:1-ARGM-LOC 
nw/wsj/22/wsj_2229.parse 39 44 gold provide-v 13.4.1-2 Supply provide.01 null ----- 36:1*38:1*45:1-ARG1=Theme 39:2-ARG0=Agent 44:0-rel 46:1-ARG2=Recipient 36:1*38:1-LINK-SLC 
nw/wsj/22/wsj_2229.parse 40 6 gold concentrate-v 87.1 NF concentrate.01 1 ----- 3:1-ARG0=Experiencer 6:0-rel 7:1-ARG1=Theme 
nw/wsj/22/wsj_2229.parse 40 28 gold end-v 55.4-1 Cause_to_end end.01 null ----- 21:1*22:1*23:1*25:1-ARG1=Theme 26:1-ARGM-NEG 28:0-rel 21:1*22:1-LINK-SLC 
nw/wsj/22/wsj_2229.parse 41 13 gold teach-v 37.1.1-1-1 Education_teaching teach.01 1 ----- 0:2-ARG0=Agent 13:0-rel 14:1-ARGM-LOC 
