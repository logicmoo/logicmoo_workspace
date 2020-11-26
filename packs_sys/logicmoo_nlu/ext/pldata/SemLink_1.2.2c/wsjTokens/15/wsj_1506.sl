nw/wsj/15/wsj_1506.parse 0 3 gold require-v 103 NF require.01 2 ----- 0:1*4:1-ARG2=Source 3:0-rel 6:1-ARG1=Theme 
nw/wsj/15/wsj_1506.parse 0 7 gold notify-v 37.9 Telling notify.01 1 ----- 0:1*4:1*5:1-ARG0=Agent;Speaker 7:0-rel 8:1-ARG1=Recipient;Addressee 10:1-ARG2=Topic;Message 
nw/wsj/15/wsj_1506.parse 0 12 gold know-v 29.5-1 IN know.01 1 ----- 11:1-ARG0=Agent 12:0-rel 13:1-ARG2=Predicate 
nw/wsj/15/wsj_1506.parse 0 25 gold say-v 37.7-1 IN say.01 1 ----- 0:2-ARG1=Topic 23:1-ARG0=Agent 25:0-rel 
nw/wsj/15/wsj_1506.parse 1 12 gold handle-v 15.1-1 NF handle.01 2 ----- 11:1-ARG0=Agent 12:0-rel 13:1-ARG1=Theme 
nw/wsj/15/wsj_1506.parse 2 7 gold know-v 29.5-1 IN know.01 null ----- 0:2*8:1-ARG1=Theme 7:0-rel 9:1-ARGM-TMP 
nw/wsj/15/wsj_1506.parse 2 12 gold disclose-v 37.7 Reveal_secret disclose.01 1 ----- 10:1-ARG0=Agent;Speaker 12:0-rel 13:1-ARG1=Topic;Information 14:1-ARGM-MNR 15:1-ARG2=Recipient;Addressee 19:1-ARGM-LOC 24:1-ARGM-TMP 
nw/wsj/15/wsj_1506.parse 3 5 gold say-v 37.7-1 IN say.01 1 ----- 0:2-ARG0=Agent 5:0-rel 6:1,16:3-ARG1=Topic 
nw/wsj/15/wsj_1506.parse 3 24 gold contend-v 29.5-2 Statement contend.01 1 ----- 22:1-ARG0=Agent;Speaker 24:0-rel 25:1-ARG1=Theme;Addressee 
nw/wsj/15/wsj_1506.parse 3 27 gold contribute-v 13.2-1-1 NF contribute.01 2 ----- 16:2*21:1*26:1-ARG0=Agent 27:0-rel 28:1-ARG2=Recipient 16:2*21:1-LINK-SLC 
nw/wsj/15/wsj_1506.parse 3 43 gold oust-v 10.1 Change_of_leadership oust.01 1 ----- 22:1*41:1-ARG0=Agent;Selector 43:0-rel 44:2-ARG1=Theme;Old_leader/Old_order 22:1*41:1-LINK-PRO 
nw/wsj/15/wsj_1506.parse 5 1 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/15/wsj_1506.parse 5 12 gold affect-v 31.1 NF affect.01 1 ----- 7:1-ARG0=Stimulus 11:0-ARGM-NEG 12:0-rel 13:1-ARG1=Experiencer 
nw/wsj/15/wsj_1506.parse 5 17 gold lend-v 13.1 NF lend.01 4 ----- 3:1*15:1-ARG0=Agent 17:0-rel 18:1-ARG1=Theme 21:1-ARG2=Recipient 3:1*15:1-LINK-PRO 
nw/wsj/15/wsj_1506.parse 6 4 gold consider-v 29.9-1-1-1 Cogitation consider.01 2 ----- 0:1*5:1-ARG1=Theme;Topic 3:1-ARGM-TMP 4:0-rel 12:1-ARGM-ADV 13:1-ARGM-CAU 
nw/wsj/15/wsj_1506.parse 6 8 gold say-v 37.7-1 IN say.01 1 ----- 7:1-ARG0=Agent 8:0-rel 0:2-ARG1-DSP=Topic 
nw/wsj/15/wsj_1506.parse 6 18 gold think-v 29.9-2 IN think.01 1 ----- 14:1-ARG0 17:0-ARGM-NEG 18:0-rel 19:1-ARG1 
nw/wsj/15/wsj_1506.parse 6 23 gold intend-v 62 Purpose intend.01 1 ----- 20:1-ARG0 23:0-rel 24:2-ARG1 
nw/wsj/15/wsj_1506.parse 6 26 gold kill-v 42.1-1 Killing kill.01 1 ----- 20:1*24:1-ARG0=Agent;Killer/Cause 26:0-rel 27:1-ARG1=Patient;Victim 
nw/wsj/15/wsj_1506.parse 6 35 gold imprison-v 9.10 Imprisonment imprison.01 1 ----- 20:1*33:1-ARG0=Agent 35:0-rel 36:1-ARG1=Theme 
nw/wsj/15/wsj_1506.parse 7 9 gold hint-v 37.7-2 NF hint.01 1 ----- 4:1-ARG0=Agent 9:0-rel 10:1-ARG1=Topic 
nw/wsj/15/wsj_1506.parse 7 19 gold drop-v 10.10 IN drop.05 8 ----- 11:1*16:1*20:1-ARG1=Theme 19:0-rel 21:1-ARG2=Source 
nw/wsj/15/wsj_1506.parse 8 3 gold launch-v 55.5-1 IN launch.01 2 ----- 0:1*4:1-ARG1 3:0-rel 5:1-ARGM-LOC 15:2-ARGM-TMP 
nw/wsj/15/wsj_1506.parse 8 28 gold complain-v 37.8 Statement complain.01 1 ----- 22:1*24:1*32:1-ARGM-LOC 26:1-ARG0=Agent;Speaker 28:0-rel 29:1-ARG1=Topic;Message 22:1*24:1-LINK-SLC 
nw/wsj/15/wsj_1506.parse 9 9 gold report-v 37.7-1 Statement report.01 1 ----- 2:2*10:1-ARG1 8:1-ARGM-TMP 9:0-rel 11:1-ARGM-TMP 13:1-ARG0 2:2*10:1-LINK-PSV 
nw/wsj/15/wsj_1506.parse 9 31 gold pressure-v 59 Attempt_suasion pressure.01 1 ----- 2:2*29:1-ARG0=Agent;Speaker 31:0-rel 32:1-ARG1=Patient;Addressee 2:2*29:1-LINK-PRO 
nw/wsj/15/wsj_1506.parse 10 22 gold oust-v 10.1 Change_of_leadership oust.01 1 ----- 13:1*20:1-ARG0=Agent;Selector 22:0-rel 23:1-ARG1=Theme;Old_leader/Old_order 13:1*20:1-LINK-PRO 
nw/wsj/15/wsj_1506.parse 11 2 gold issue-v 13.3 NF issue.01 1 ----- 0:1*3:1-ARG1=Theme 2:0-rel 4:1-ARG0=Agent 0:1*3:1-LINK-PSV 
nw/wsj/15/wsj_1506.parse 11 29 gold continue-v 55.3 Process_continue continue.01 null ----- 29:0-rel 30:0-ARG1 
nw/wsj/15/wsj_1506.parse 11 33 gold shift-v 11.1 NF shift.01 1 ----- 23:1*31:1-ARG0 33:0-rel 34:2-ARG1=Theme 23:1*31:1-LINK-PRO 
nw/wsj/15/wsj_1506.parse 12 2 gold add-v 37.7 NF add.01 1 ----- 0:1-ARG0=Agent 2:0-rel 5:2-ARG1=Topic 
nw/wsj/15/wsj_1506.parse 12 8 gold choose-v 13.5.1 Choosing choose.01 2 ----- 5:1-ARG0=Agent;Cognizer 7:1-ARGM-ADV 8:0-rel 9:2-ARG1=Theme;Chosen 
nw/wsj/15/wsj_1506.parse 13 20 gold work-v 73-3 IN work.01 1 ----- 17:1,19:0-ARG0=Agent 20:0-rel 21:2-ARGM-PNC 
nw/wsj/15/wsj_1506.parse 13 23 gold develop-v 26.2 IN develop.02 2 ----- 14:1*16:1*24:1-ARG1=Product 17:1*21:1-ARG0=Agent 23:0-rel 14:1*16:1-LINK-SLC 
nw/wsj/15/wsj_1506.parse 14 2 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 2:0-rel 5:2-ARG1=Topic 
nw/wsj/15/wsj_1506.parse 14 11 gold stop-v 55.4-1 Activity_stop stop.01 1 ----- 9:1,12:1-ARG1=Theme;Activity 11:0-rel 
nw/wsj/15/wsj_1506.parse 14 18 gold develop-v 26.1 IN develop.02 1 ----- 9:1*16:1-ARG0=Agent 18:0-rel 19:2-ARG1=Product 
nw/wsj/15/wsj_1506.parse 14 27 gold help-v 72-1 Assistance help.01 1 ----- 19:1*24:1-ARGM-MNR 25:1-ARG0=Agent;Helper 27:0-rel 28:1-ARG1=Theme;Goal/Focal_entity 30:1-ARGM-TMP 19:1*24:1-LINK-SLC 
nw/wsj/15/wsj_1506.parse 15 2 gold invite-v 102 Request invite.01 3 ----- 0:1-ARG0 2:0-rel 3:1-ARG1 5:2-ARG2 
nw/wsj/15/wsj_1506.parse 15 7 gold send-v 11.1-1 Sending send.01 1 ----- 3:1*5:1-ARG0=Agent;Sender 7:0-rel 8:1-ARG1=Theme;Theme 10:1-ARG2=Destination;Goal/Recipient 
nw/wsj/15/wsj_1506.parse 16 13 gold say-v 37.7-1 IN say.01 1 ----- 0:4-ARG0=Agent 13:0-rel 14:1-ARG3 17:1-ARG1=Topic 
nw/wsj/15/wsj_1506.parse 16 21 gold tear-v 23.2 IN tear.01 1 ----- 19:1*22:1-ARG1=Patient 21:0-rel 23:1-ARG2=Co-Patient 19:1*22:1-LINK-PSV 
nw/wsj/15/wsj_1506.parse 16 44 gold leak-v 43.4 Fluidic_motion leak.01 1 ----- 36:2*41:1*42:1-ARG0=Source;Source 44:0-rel 36:2*41:1-LINK-SLC 
nw/wsj/15/wsj_1506.parse 16 46 gold present-v 13.4.1 NF present.01 1 ----- 36:3-ARG0=Agent 46:0-rel 47:1-ARG1=Theme 49:1-ARGM-MNR 
nw/wsj/15/wsj_1506.parse 17 2 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/15/wsj_1506.parse 17 12 gold make-v 29.3 Cause_change make.02 3 ----- 4:1*11:1-ARG0=Agent 12:0-rel 13:2-ARG1=Theme 
nw/wsj/15/wsj_1506.parse 18 7 gold stress-v 9.9 NF stress.01 1 ----- 0:0-ARGM-DIS 1:2-ARG0=Agent 7:0-rel 8:1-ARG1=Destination 
nw/wsj/15/wsj_1506.parse 18 14 gold impose-v 63 NF impose.01 1 ----- 9:1*15:1-ARG1=Theme 13:0-ARGM-NEG 14:0-rel 16:1-ARG2 20:1-ARG0=Agent 
nw/wsj/15/wsj_1506.parse 19 7 gold agree-v 36.1-1 IN agree.01 1 ----- 2:2-ARG0=Agent 7:0-rel 0:2-ARG1-DSP=Theme 
nw/wsj/15/wsj_1506.parse 19 25 gold initiate-v 55.5-1 Activity_start initiate.01 1 ----- 15:2*22:1*23:1*26:1-ARG1=Theme 25:0-rel 27:1-ARG0=Agent 30:1-ARGM-TMP 15:2*22:1-LINK-SLC 
nw/wsj/15/wsj_1506.parse 19 34 gold stretch-v 47.7 Cause_expansion stretch.01 2 ----- 15:2*22:1*23:1-ARG1 34:0-rel 35:1-ARG4 15:2*22:1-LINK-SLC 
nw/wsj/15/wsj_1506.parse 20 27 gold ban-v 67 Prohibiting ban.01 1 ----- 23:1-ARG0=Agent;Principle 27:0-rel 28:2-ARG1=Theme;State_of_affairs 
nw/wsj/15/wsj_1506.parse 21 10 gold differ-v 36.4-1 NF differ.01 2 ----- 0:1-ARGM-DIS 3:1-ARGM-TMP 4:2-ARG0=Agent 9:1-ARGM-TMP 10:0-rel 11:1-ARG2=Topic 
nw/wsj/15/wsj_1506.parse 21 16 gold agree-v 36.1-1 IN agree.01 3 ----- 16:0-rel 17:1-ARG1=Theme 
nw/wsj/15/wsj_1506.parse 22 3 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/15/wsj_1506.parse 22 7 gold require-v 103 NF require.01 2 ----- 5:1*8:1-ARG1=Theme 7:0-rel 9:2-ARGM-ADV 
nw/wsj/15/wsj_1506.parse 22 14 gold get-v 13.5.1-1 IN get.01 11.8 ----- 11:1-ARG0=Agent 14:0-rel 15:1,17:1-ARG1=Theme 
nw/wsj/15/wsj_1506.parse 23 8 gold say-v 37.7-1 IN say.01 1 ----- 1:2-ARG0=Agent 8:0-rel 9:1-ARG1=Topic 
nw/wsj/15/wsj_1506.parse 23 17 gold involve-v 86.2-1 NF involve.01 2 ----- 14:1-ARG2 16:1-ARGM-MNR 17:0-rel 18:1-ARG1 
nw/wsj/15/wsj_1506.parse 24 24 gold lead-v 59 Causation lead.03 1 ----- 21:1-ARG0=Agent 22:0-ARGM-MOD 24:0-rel 25:1-ARG2=Result 
nw/wsj/15/wsj_1506.parse 25 4 gold call-v 29.3 IN call.01 5 ----- 0:1-ARG0=Agent 4:0-rel 5:1-ARG1=Theme 9:1-ARG2=Result 
nw/wsj/15/wsj_1506.parse 25 12 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 12:0-rel 13:1-ARG1=Topic 
nw/wsj/15/wsj_1506.parse 25 16 gold make-v 29.3 Causation make.02 3 ----- 14:1-ARG0=Agent 15:0-ARGM-MOD 16:0-rel 17:2-ARG1=Theme 
nw/wsj/15/wsj_1506.parse 25 23 gold listen-v 30.3 Perception_active listen.01 1 ----- 17:1*20:1-ARG0=Experiencer;Perceiver_agentive 22:1-ARGM-ADV 23:0-rel 24:1-ARG1=Stimulus;Phenomenon 
nw/wsj/15/wsj_1506.parse 25 32 gold get-v 26.6.2 IN get.03 3 ----- 30:1-ARG1=Patient 31:0-ARGM-MOD 32:0-rel 33:1-ARG2=Goal 
nw/wsj/15/wsj_1506.parse 26 24 gold win-v 13.5.1 NF win.01 2 ----- 16:1*22:1-ARG0=Agent 24:0-rel 25:2-ARG1=Theme 
nw/wsj/15/wsj_1506.parse 27 8 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARGM-DIS 3:2-ARG0=Agent 8:0-rel 9:1-ARG1=Topic 
nw/wsj/15/wsj_1506.parse 27 29 gold avoid-v 52 Avoiding avoid.01 1 ----- 27:1-ARG0=Agent;Agent 29:0-rel 30:1-ARG1=Theme;Undesirable_situation 33:1-ARGM-TMP 
nw/wsj/15/wsj_1506.parse 28 12 gold vote-v 29.3 NF vote.01 1 ----- 5:1*9:1*14:1-ARG1=Theme 10:1-ARG0=Agent 11:1-ARGM-NEG 12:0-rel 5:1*9:1-LINK-SLC 
nw/wsj/15/wsj_1506.parse 28 20 gold exercise-v 26.8 NF exercise.01 1 ----- 0:1-ARGM-ADV 16:1-ARG0=Agent 19:0-ARGM-MOD 20:0-rel 21:1-ARG1=Theme 22:1-ARGM-ADV 
nw/wsj/15/wsj_1506.parse 29 14 gold provide-v 13.4.1-2 Supply provide.01 1 ----- 12:1-ARG0=Agent 14:0-rel 15:3-ARG1=Theme 
nw/wsj/15/wsj_1506.parse 29 24 gold permit-v 64 Permitting permit.01 1 ----- 15:2*21:1*22:1-ARG0=Agent;Principle 23:0-ARGM-MOD 24:0-rel 25:1-ARG2=Theme;State_of_affairs 27:1-ARG1=Theme;State_of_affairs 15:2*21:1-LINK-SLC 
nw/wsj/15/wsj_1506.parse 30 3 gold react-v 31.3-9 NF react.01 1 ----- 0:1-ARG0=Experiencer 3:0-rel 4:1-ARG1=Stimulus 
nw/wsj/15/wsj_1506.parse 30 18 gold blame-v 33 IN blame.01 1 ----- 7:1*14:1*16:1-ARG0=Agent 18:0-rel 19:1-ARG1=Theme 20:1-ARG2=Attribute 
nw/wsj/15/wsj_1506.parse 30 26 gold say-v 37.7-1 IN say.01 1 ----- 23:1-ARG0=Agent 26:0-rel 27:1-ARG1=Topic 
nw/wsj/15/wsj_1506.parse 31 11 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARGM-DIS 2:1-ARGM-TMP 4:2-ARG0=Agent 11:0-rel 12:1-ARG1=Topic 
nw/wsj/15/wsj_1506.parse 33 3 gold agree-v 36.1-1 IN agree.01 3 ----- 0:1-ARG0=Agent 3:0-rel 4:2-ARG1=Theme 
nw/wsj/15/wsj_1506.parse 34 18 gold exist-v 47.1-1 Existence exist.01 1 ----- 12:2*16:1*17:1-ARG1=Theme;Entity 18:0-rel 12:2*16:1-LINK-SLC 
nw/wsj/15/wsj_1506.parse 35 3 gold contribute-v 13.2-1-1 NF contribute.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG2=Recipient 
