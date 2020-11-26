nw/wsj/18/wsj_1803.parse 1 24 gold expect-v 62 IN expect.01 null ----- 22:1,25:2-ARG1=Theme 24:0-rel 
nw/wsj/18/wsj_1803.parse 1 37 gold say-v 37.7-1 IN say.01 null ----- 32:3-ARG0=Agent 37:0-rel 38:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 2 20 gold say-v 37.7-1 IN say.01 null ----- 18:1-ARG0=Agent 20:0-rel 21:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 2 23 gold expect-v 62 IN expect.01 null ----- 22:1-ARG0=Experiencer 23:0-rel 24:2-ARG1=Theme 
nw/wsj/18/wsj_1803.parse 3 28 gold hit-v 18.4 Cause_harm hit.01 null ----- 23:1*25:1-ARGM-TMP 26:1-ARG2=Theme 28:0-rel 29:1-ARGM-TMP 23:1*25:1-LINK-SLC 
nw/wsj/18/wsj_1803.parse 4 1 gold get-v 26.6.2 IN get.05 null ----- 0:1-ARG1=Patient 1:0-rel 2:1-ARG2=Goal 7:1-ARGM-TMP 
nw/wsj/18/wsj_1803.parse 5 6 gold drive-v 11.5 Operate_vehicle drive.01 null ----- 4:1-ARG0=Agent 6:0-rel 7:1-ARGM-LOC 
nw/wsj/18/wsj_1803.parse 5 13 gold restrict-v 76 NF restrict.01 1 ----- 1:2*14:1-ARG1=Patient 12:1-ARGM-ADV 13:0-rel 
nw/wsj/18/wsj_1803.parse 5 17 gold say-v 37.7-1 IN say.01 null ----- 1:3*18:1-ARG1=Topic 17:0-rel 19:2-ARG0=Agent 
nw/wsj/18/wsj_1803.parse 6 17 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 17:0-rel 18:1*36:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 6 21 gold urge-v 58.1 IN urge.01 null ----- 19:1-ARG0=Agent 21:0-rel 22:1*24:1-ARG1=Recipient 24:2*36:1-ARG2=Topic 
nw/wsj/18/wsj_1803.parse 7 6 gold resume-v 55.1-1 Process_resume resume.01 null ----- 1:1-ARG1=Theme;Process 4:0-ARGM-MOD 5:1-ARGM-ADV 6:0-rel 7:1-ARGM-TMP 
nw/wsj/18/wsj_1803.parse 7 11 gold say-v 37.7-1 IN say.01 null ----- 1:2*12:1-ARG1=Topic 10:1-ARG0=Agent 11:0-rel 
nw/wsj/18/wsj_1803.parse 9 15 gold inform-v 37.9 Telling inform.01 null ----- 0:1-ARGM-TMP 12:1-ARGM-TMP 13:1*16:1-ARG1=Recipient;Addressee 15:0-rel 17:1-ARG2=Topic;Message 
nw/wsj/18/wsj_1803.parse 9 25 gold say-v 37.7-1 IN say.01 null ----- 0:3*26:1-ARG1=Topic 23:1-ARG0=Agent 25:0-rel 
nw/wsj/18/wsj_1803.parse 10 8 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 8:0-rel 9:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 10 19 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 null ----- 10:2*20:1-ARG1=Patient 19:0-rel 21:1-ARG4=Goal 
nw/wsj/18/wsj_1803.parse 11 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 11 17 gold damage-v 44 Damaging damage.01 1 ----- 6:2*18:1-ARG1=Patient;Patient 17:0-rel 19:1-ARGM-LOC 
nw/wsj/18/wsj_1803.parse 11 29 gold impede-v 67 Hindering impede.01 1 ----- 5:1-ARGM-ADV 23:2-ARG0=Agent 29:0-rel 30:2-ARG1=Theme 
nw/wsj/18/wsj_1803.parse 12 2 gold note-v 30.2 Statement note.02 null ----- 0:1-ARG0=Experiencer 2:0-rel 3:4-ARG1=Stimulus 18:1-ARGM-DIS 
nw/wsj/18/wsj_1803.parse 13 5 gold prevent-v 67 Thwarting prevent.01 1 ----- 0:1-ARGM-DIS 3:1-ARG0=Agent;Preventing_cause 5:0-rel 6:1-ARG1=Theme;Protagonist/Action 11:1-ARG2=Theme;Protagonist/Action 15:2-ARGM-ADV 
nw/wsj/18/wsj_1803.parse 13 16 gold cause-v 27 Causation cause.01 1 ----- 3:1*15:1-ARG0=Cause;Cause 16:0-rel 17:1-ARG1=Theme;Effect 3:1*15:1-LINK-PRO 
nw/wsj/18/wsj_1803.parse 14 3 gold report-v 37.7-1 Statement report.01 null ----- 0:0-ARGM-DIS 1:1-ARG0=Agent;Speaker 3:0-rel 4:1-ARG1=Topic;Message 
nw/wsj/18/wsj_1803.parse 15 2 gold stop-v 55.4-1 Preventing stop.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Theme 5:1-ARGM-TMP 7:1-ARGM-TMP 
nw/wsj/18/wsj_1803.parse 15 10 gold inspect-v 35.4 Inspecting inspect.01 1 ----- 8:1-ARG0=Agent 10:0-rel 11:1-ARG1=Location 
nw/wsj/18/wsj_1803.parse 15 13 gold resume-v 55.1-1 Process_resume resume.01 null ----- 0:1-ARG0=Agent 13:0-rel 14:1-ARG1=Theme;Process 15:1-ARGM-TMP 18:2-ARGM-TMP 
nw/wsj/18/wsj_1803.parse 15 20 gold find-v 13.5.1 IN find.01 null ----- 18:1*23:1-ARGM-TMP 19:1-ARG0=Agent 20:0-rel 21:1-ARG1=Theme 
nw/wsj/18/wsj_1803.parse 16 6 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 6:0-rel 7:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 17 8 gold cause-v 27 Causation cause.01 1 ----- 7:1*9:1-ARG1=Theme;Effect 8:0-rel 10:1-ARG0=Cause;Cause 7:1*9:1-LINK-PSV 
nw/wsj/18/wsj_1803.parse 17 15 gold get-v 26.6.2 IN get.05 null ----- 11:1*13:1-ARG1=Patient 15:0-rel 16:1-ARG2=Goal 11:1*13:1-LINK-PRO 
nw/wsj/18/wsj_1803.parse 18 17 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 17:0-rel 18:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 18 30 gold use-v 105 IN use.01 null ----- 21:1*27:1*28:1*31:1-ARG1 30:0-rel 32:1-ARG0 36:1-ARGM-LOC 21:1*27:1-LINK-SLC 
nw/wsj/18/wsj_1803.parse 19 6 gold expect-v 62 IN expect.01 null ----- 0:0-ARGM-DIS 1:2,7:2-ARG1=Theme 6:0-rel 
nw/wsj/18/wsj_1803.parse 19 9 gold resume-v 55.1-1 Process_resume resume.01 null ----- 1:2*7:1-ARG1=Theme;Process 9:0-rel 10:1-ARGM-TMP 
nw/wsj/18/wsj_1803.parse 20 2 gold have-v 100 IN have.03 null ----- 1:1-ARG0=Pivot 2:0-rel 3:2-ARG1=Theme 
nw/wsj/18/wsj_1803.parse 20 11 gold say-v 37.7-1 IN say.01 null ----- 1:2*12:1-ARG1=Topic 11:0-rel 13:1-ARG0=Agent 
nw/wsj/18/wsj_1803.parse 21 2 gold have-v 100 IN have.03 null ----- 1:1-ARG0=Pivot 2:0-rel 3:2-ARG1=Theme 9:1-ARGM-ADV 11:1-ARGM-ADV 
nw/wsj/18/wsj_1803.parse 22 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 23 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 23 6 gold terminate-v 55.4 Activity_stop terminate.01 1 ----- 5:1*12:1-ARG0=Agent;Agent 6:0-rel 7:2-ARG1=Theme;Activity 12:2-ARGM-PRD 
nw/wsj/18/wsj_1803.parse 23 13 gold rely-v 70 Reliance rely.01 1 ----- 5:1*12:1-ARG0=Agent;Protagonist 13:0-rel 14:1-ARG1=Theme;Means/Instrument/Intermediary/Benefit/Purpose 16:2-ARG2 
nw/wsj/18/wsj_1803.parse 23 18 gold ferry-v 51.4.1 Bringing ferry.01 1 ----- 15:1*16:1-ARG0=Agent 18:0-rel 19:1-ARG1=Theme 20:1-ARG3=Location 
nw/wsj/18/wsj_1803.parse 24 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 24 4 gold plan-v 62 Purpose plan.01 null ----- 3:1*5:1-ARG0=Experiencer;Agent 4:0-rel 5:2-ARG1 
nw/wsj/18/wsj_1803.parse 24 7 gold resume-v 55.1-1 Process_resume resume.01 null ----- 3:1*5:1-ARG0=Agent 7:0-rel 8:2-ARG1=Theme;Process 13:1-ARGM-TMP 
nw/wsj/18/wsj_1803.parse 25 4 gold suffer-v 31.3-4 Catastrophe suffer.01 null ----- 0:2-ARG0=Experiencer 4:0-rel 5:1-ARG1=Stimulus 8:1-ARGM-ADV 
nw/wsj/18/wsj_1803.parse 26 12 gold say-v 37.7-1 IN say.01 null ----- 12:0-rel 13:1-ARG1=Topic 15:1*18:1-ARG0=Agent 18:2-ARGM-ADV 
nw/wsj/18/wsj_1803.parse 26 19 gold add-v 37.7 NF add.01 null ----- 15:1*18:1-ARG0=Agent 19:0-rel 20:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 26 37 gold provide-v 13.4.1-2 Supply provide.01 null ----- 21:1*35:1-ARG0=Agent 37:0-rel 38:1-ARG1=Theme 40:1-ARG2=Recipient 
nw/wsj/18/wsj_1803.parse 27 15 gold say-v 37.7-1 IN say.01 null ----- 1:2*17:1-ARG1=Topic 13:1-ARG0=Agent 15:0-rel 
nw/wsj/18/wsj_1803.parse 28 12 gold get-v 26.6.2 IN get.05 null ----- 5:1*14:1-ARGM-MNR 6:1*9:1-ARG1=Patient 12:0-rel 13:1-ARG2=Goal 
nw/wsj/18/wsj_1803.parse 29 1 gold add-v 37.7 NF add.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 31 15 gold rain-v 57 Precipitation rain.01 null ----- 0:1-ARG0 15:0-rel 16:2-ARG1=Theme;Precipitation 20:0-ARGM-DIR 21:1-ARG2 
nw/wsj/18/wsj_1803.parse 31 28 gold suffer-v 31.3-4 Catastrophe suffer.01 null ----- 22:1*24:2*27:1-ARG0=Experiencer 28:0-rel 29:1-ARG1=Stimulus 22:1*24:2-LINK-SLC 
nw/wsj/18/wsj_1803.parse 32 7 gold damage-v 44 Damaging damage.01 1 ----- 0:2*8:1-ARG1=Patient;Patient 5:1-ARGM-DIS 7:0-rel 
nw/wsj/18/wsj_1803.parse 33 5 gold divert-v 31.1 Cause_to_experience divert.01 1 ----- 0:1-ARGM-TMP 3:1*6:1-ARG1=Experiencer;Experiencer 5:0-rel 7:1-ARG3 
nw/wsj/18/wsj_1803.parse 33 14 gold wait-v 47.1-1 IN wait.01 null ----- 3:1*12:1-ARG1=Theme 14:0-rel 15:1-ARGM-TMP 17:2-ARG2 
nw/wsj/18/wsj_1803.parse 33 19 gold resume-v 55.1-1 Process_resume resume.01 null ----- 3:1*12:1*17:1-ARG0=Agent 19:0-rel 20:1-ARG1=Theme;Process 
nw/wsj/18/wsj_1803.parse 34 6 gold damage-v 44 Damaging damage.01 1 ----- 0:2*7:1-ARG1=Patient;Patient 5:0-ARGM-NEG 6:0-rel 
nw/wsj/18/wsj_1803.parse 34 13 gold limit-v 76 NF limit.01 null ----- 10:1*14:1-ARG1=Patient 13:0-rel 15:1-ARGM-TMP 16:1-ARG2=Goal 34:2-ARGM-CAU 
nw/wsj/18/wsj_1803.parse 34 52 gold say-v 37.7-1 IN say.01 null ----- 0:4*54:1-ARG1=Topic 49:1-ARG0=Agent 52:0-rel 
nw/wsj/18/wsj_1803.parse 35 8 gold divert-v 31.1 Cause_to_experience divert.01 1 ----- 0:1-ARGM-TMP 6:1*9:1-ARG1=Experiencer;Experiencer 8:0-rel 10:1-ARG3 
nw/wsj/18/wsj_1803.parse 36 37 gold cause-v 27 Causation cause.01 1 ----- 32:1*35:1-ARG0=Cause;Cause 33:1-ARGM-ADV 37:0-rel 38:1-ARG1=Theme;Effect 39:1-ARGM-GOL 
nw/wsj/18/wsj_1803.parse 37 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 37 18 gold cause-v 27 Causation cause.01 1 ----- 16:1*19:1-ARG1=Theme;Effect 18:0-rel 20:2-ARGM-TMP 16:1*19:1-LINK-PSV 
nw/wsj/18/wsj_1803.parse 38 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 38 10 gold injure-v 40.8.3-2 Experience_bodily_harm injure.01 1 ----- 5:2*11:1-ARG1=Patient;Body_part 10:0-rel 12:1-ARGM-LOC 
nw/wsj/18/wsj_1803.parse 38 24 gold evacuate-v 10.2 IN evacuate.01 null ----- 20:1*25:1-ARG1=Source 24:0-rel 26:1-ARGM-TMP 
nw/wsj/18/wsj_1803.parse 39 11 gold try-v 61 Attempt try.01 null ----- 10:1*12:1-ARG0=Agent 11:0-rel 12:2-ARG1=Theme 
nw/wsj/18/wsj_1803.parse 39 14 gold get-v 13.5.1-1 IN get.01 null ----- 10:1*12:1-ARG0=Agent 14:0-rel 15:2-ARG1=Theme 
nw/wsj/18/wsj_1803.parse 39 21 gold say-v 37.7-1 IN say.01 null ----- 19:1-ARG0=Agent 21:0-rel 22:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 39 35 gold try-v 61 Attempt try.01 null ----- 34:1*36:1-ARG0=Agent 35:0-rel 36:2-ARG1=Theme 
nw/wsj/18/wsj_1803.parse 39 38 gold get-v 26.6.2 IN get.05 null ----- 34:1*36:1-ARG0=Agent 38:0-rel 39:1-ARG2=Goal 40:2-ARGM-PRP 
nw/wsj/18/wsj_1803.parse 39 42 gold help-v 72-1 Assistance help.01 null ----- 34:1*40:1-ARG0=Agent;Helper 42:0-rel 43:1-ARG1=Theme;Goal/Focal_entity 
nw/wsj/18/wsj_1803.parse 40 19 gold say-v 37.7-1 IN say.01 null ----- 0:3*21:1-ARG1=Topic 15:1-ARG0=Agent 19:0-rel 
nw/wsj/18/wsj_1803.parse 41 11 gold experience-v 30.2 Feeling experience.01 1 ----- 0:1-ARGM-MNR 5:2-ARG0=Experiencer;Experiencer 9:0-ARGM-MOD 11:0-rel 12:2-ARG1=Stimulus;Emotion/Emotional_state 
nw/wsj/18/wsj_1803.parse 42 2 gold think-v 29.9-2 IN think.01 null ----- 1:1-ARG0 2:0-rel 3:1-ARG1 
nw/wsj/18/wsj_1803.parse 42 17 gold land-v 9.10-1 NF land.01 1 ----- 6:2*14:1*15:1-ARG1=Theme 17:0-rel 18:1-ARGM-LOC 6:2*14:1-LINK-SLC 
nw/wsj/18/wsj_1803.parse 42 24 gold say-v 37.7-1 IN say.01 null ----- 1:2*25:1-ARG1=Topic 21:1-ARG0=Agent 24:0-rel 
nw/wsj/18/wsj_1803.parse 43 5 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 5:0-rel 6:1-ARG1=Topic 
nw/wsj/18/wsj_1803.parse 43 19 gold affect-v 31.1 NF affect.01 null ----- 7:2*20:1-ARG1=Experiencer 18:0-ARGM-NEG 19:0-rel 
nw/wsj/18/wsj_1803.parse 43 24 gold get-v 13.5.1-1 IN get.01 null ----- 23:1-ARG0=Agent 24:0-rel 25:2-ARG1=Theme 
nw/wsj/18/wsj_1803.parse 45 9 gold say-v 37.7-1 IN say.01 null ----- 1:2*10:1-ARG1=Topic 8:1-ARG0=Agent 9:0-rel 
