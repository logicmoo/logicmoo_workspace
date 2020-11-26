nw/wsj/20/wsj_2021.parse 0 6 gold limit-v 76 NF limit.01 null ----- 6:0-rel 7:0,8:0-ARG1=Patient 
nw/wsj/20/wsj_2021.parse 0 11 gold differentiate-v 23.1-1 Differentiation differentiate.01 1 ----- 1:1*10:1-ARG0=Agent 11:0-rel 12:1-ARG1=Patient 
nw/wsj/20/wsj_2021.parse 1 2 gold involve-v 86.2-1 NF involve.01 null ----- 0:1-ARG2 2:0-rel 3:2-ARG1 
nw/wsj/20/wsj_2021.parse 2 1 gold start-v 55.1-1 Activity_start start.01 null ----- 0:1-ARG1=Theme;Activity 1:0-rel 2:1-ARG2=Instrument 
nw/wsj/20/wsj_2021.parse 2 6 gold make-v 29.3 Causation make.02 null ----- 3:2,7:2-ARG1=Theme 6:0-rel 9:1-ARG0=Agent 15:1-ARGM-TMP 
nw/wsj/20/wsj_2021.parse 3 11 gold follow-v 47.8 Cotheme follow.01 null ----- 0:1-ARGM-TMP 1:3*12:1-ARG2=Co-Theme;Cotheme 11:0-rel 13:1-ARG1=Theme;Theme 21:1-ARGM-TMP 
nw/wsj/20/wsj_2021.parse 4 6 gold dedicate-v 79 NF dedicate.01 null ----- 0:2*7:1-ARG1=Theme 6:0-rel 8:1-ARGM-ADV 9:1-ARG2=Goal 
nw/wsj/20/wsj_2021.parse 5 4 gold see-v 30.1-1 Perception_experience see.01 null ----- 0:1-ARGM-TMP 1:1-ARG0=Experiencer;Perceiver_passive 4:0-rel 5:2-ARG1=Stimulus;Phenomenon 
nw/wsj/20/wsj_2021.parse 5 16 gold promulgate-v 37.7 NF promulgate.01 null ----- 8:2*14:1*15:1-ARG0=Agent 16:0-rel 17:1-ARG1=Topic 8:2*14:1-LINK-SLC 
nw/wsj/20/wsj_2021.parse 6 9 gold eliminate-v 10.1 Removing eliminate.01 null ----- 0:1-ARG0=Agent;Agent/Cause 9:0-rel 10:1-ARG1=Theme;Theme 
nw/wsj/20/wsj_2021.parse 7 6 gold handle-v 15.1-1 NF handle.01 null ----- 0:1*7:1-ARG1=Theme 6:0-rel 8:1-ARGM-LOC 
nw/wsj/20/wsj_2021.parse 8 3 gold start-v 55.1-1 Activity_start start.01 null ----- 0:1-ARG1=Theme;Activity 2:1-ARGM-ADV 3:0-rel 4:1-ARGM-TMP 6:1-ARG2=Instrument 
nw/wsj/20/wsj_2021.parse 9 5 gold begin-v 55.1-1 Process_start begin.01 null ----- 0:1-ARGM-TMP 3:1*6:2-ARG1=Theme;Event 5:0-rel 9:2-ARGM-PRD 
nw/wsj/20/wsj_2021.parse 9 8 gold appear-v 48.1.1 Coming_to_be appear.01 null ----- 3:1*6:1-ARG1=Theme;Entity 8:0-rel 
nw/wsj/20/wsj_2021.parse 9 10 gold predict-v 78 Predicting predict.01 1 ----- 3:1*9:1-ARG0=Cause;Evidence/Speaker 10:0-rel 11:1-ARG1=Topic;Eventuality 3:1*9:1-LINK-PRO 
nw/wsj/20/wsj_2021.parse 11 1 gold write-v 25.2 Statement write.01 null ----- 0:1*5:1-ARG0=Agent 1:0-rel 2:1-ARGM-TMP 
nw/wsj/20/wsj_2021.parse 11 7 gold suggest-v 37.7-1-1 Statement suggest.01 null ----- 0:1*5:1-ARG0=Agent;Speaker 0:2-ARGM-PRD 7:0-rel 8:1-ARG1=Topic;Message 
nw/wsj/20/wsj_2021.parse 11 14 gold eliminate-v 10.1 Killing eliminate.01 null ----- 9:1*15:1-ARG1=Theme 12:0-ARGM-MOD 14:0-rel 16:1-ARGM-TMP 
nw/wsj/20/wsj_2021.parse 11 23 gold suggest-v 37.7-1-1 Statement suggest.01 null ----- 21:1-ARG0=Agent;Speaker 23:0-rel 24:1-ARG1=Topic;Message 
nw/wsj/20/wsj_2021.parse 11 29 gold end-v 55.4-1 Cause_to_end end.01 null ----- 25:2-ARG1=Theme 28:0-ARGM-MOD 29:0-rel 30:1-ARGM-TMP 
nw/wsj/20/wsj_2021.parse 12 2 gold predict-v 78 Predicting predict.01 1 ----- 0:1-ARG0=Cause;Evidence/Speaker 2:0-rel 3:2-ARG1=Topic;Eventuality 
nw/wsj/20/wsj_2021.parse 14 5 gold categorize-v 29.10 NF categorize.01 null ----- 0:2*6:1-ARG1 5:0-rel 7:1-ARG2 10:1-ARGM-MNR 
nw/wsj/20/wsj_2021.parse 14 15 gold ban-v 67 Prohibiting ban.01 null ----- 13:1*16:1-ARG1=Theme;State_of_affairs 15:0-rel 17:1-ARG2 20:1-ARGM-CAU 
nw/wsj/20/wsj_2021.parse 15 22 gold require-v 103 NF require.01 null ----- 9:2*18:1*21:1-ARG0=Pivot 22:0-rel 23:2-ARG1=Theme 9:2*18:1-LINK-SLC 
nw/wsj/20/wsj_2021.parse 15 23 gold burgeon-v 45.5 NF burgeon.01 null ----- 23:0-rel 24:0,25:0-ARG1=Patient 
nw/wsj/20/wsj_2021.parse 16 6 gold oppose-v 22.2-3 NF oppose.01 null ----- 0:1-ARGM-TMP 2:1-ARG0=Patient 5:1-ARGM-MNR 6:0-rel 7:2-ARG1=Co-Patient 
nw/wsj/20/wsj_2021.parse 17 3 gold require-v 103 NF require.01 null ----- 0:1-ARG0=Pivot 3:0-rel 4:2-ARG1=Theme 8:1-ARGM-PNC 
nw/wsj/20/wsj_2021.parse 18 4 gold oppose-v 22.2-3 NF oppose.01 null ----- 1:1-ARG0=Patient 4:0-rel 5:3-ARG1=Co-Patient 
nw/wsj/20/wsj_2021.parse 18 12 gold claim-v 37.7-1 Statement claim.01 null ----- 11:1-ARG0=Agent;Speaker 12:0-rel 9:2-ARG1-DSP=Topic;Message 
nw/wsj/20/wsj_2021.parse 18 17 gold deplete-v 10.6 NF deplete.01 null ----- 5:1*9:1*16:1-ARG0=Agent 17:0-rel 18:1-ARG1=Theme 5:1*9:1-LINK-SLC 
nw/wsj/20/wsj_2021.parse 19 7 gold oppose-v 22.2-3 NF oppose.01 null ----- 6:1-ARG0=Patient 7:0-rel 8:2-ARG1=Co-Patient 
nw/wsj/20/wsj_2021.parse 19 19 gold oppose-v 22.2-3 NF oppose.01 null ----- 0:1-ARG0=Patient 18:1-ARGM-ADV 19:0-rel 20:3-ARG1=Co-Patient 
nw/wsj/20/wsj_2021.parse 20 7 gold prove-v 29.4-1-1 Turning_out prove.01 null ----- 7:0-rel 8:0-ARG1=Theme 
nw/wsj/20/wsj_2021.parse 21 9 gold solve-v 84 NF solve.01 null ----- 8:1-ARG0=Agent 9:0-rel 10:1-ARG1=Theme 12:1-ARGM-LOC 16:1-ARGM-TMP 
nw/wsj/20/wsj_2021.parse 24 6 gold present-v 13.4.1 NF present.01 null ----- 4:1*7:1-ARG1=Theme 6:0-rel 8:1-ARG0=Agent 4:1*7:1-LINK-PSV 
nw/wsj/20/wsj_2021.parse 24 18 gold work-v 73-3 IN work.01 null ----- 17:1-ARG0=Agent 18:0-rel 19:1-ARGM-LOC 
nw/wsj/20/wsj_2021.parse 25 1 gold consider-v 29.9-1-1-1 Cogitation consider.01 null ----- 0:1-ARG0=Agent;Cognizer 1:0-rel 3:1-ARGM-ADV 6:2-ARG1=Theme;Topic 
nw/wsj/20/wsj_2021.parse 25 29 gold judge-v 29.4-1-1-2 NF judge.01 2 ----- 13:2-ARG0=Agent 29:0-rel 30:1-ARG2=Result 
nw/wsj/20/wsj_2021.parse 26 16 gold try-v 61 Attempt try.01 null ----- 14:1*17:1-ARG0=Agent 16:0-rel 17:2-ARG1=Theme 
nw/wsj/20/wsj_2021.parse 27 3 gold intend-v 62 Purpose intend.01 null ----- 0:0-ARGM-DIS 3:0-rel 4:1-ARG2 5:1-ARG1 13:1-ARGM-ADV 
nw/wsj/20/wsj_2021.parse 27 6 gold dictate-v 37.1.1-1 NF dictate.01 1 ----- 2:1*4:1-ARG0=Agent 6:0-rel 7:2-ARG1=Topic 2:1*4:1-LINK-PRO 
nw/wsj/20/wsj_2021.parse 28 3 gold ask-v 60-1 Request ask.02 null ----- 0:1*4:1-ARG2 3:0-rel 5:2-ARG1 
nw/wsj/20/wsj_2021.parse 28 13 gold deliver-v 11.1 Delivery deliver.01 null ----- 0:1*4:1*5:1-ARG0=Agent 12:0-ARGM-NEG 13:0-rel 14:2-ARG1=Theme 
nw/wsj/20/wsj_2021.parse 29 3 gold conclude-v 97.2 Coming_to_believe conclude.01 null ----- 0:1-ARG0=Agent 2:1-ARGM-MNR 3:0-rel 4:1-ARG1=Theme 
nw/wsj/20/wsj_2021.parse 30 1 gold lead-v 59 Causation lead.03 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG2=Result 
nw/wsj/20/wsj_2021.parse 31 6 gold work-v 73-3 IN work.01 null ----- 1:1*4:1-ARG0=Agent 6:0-rel 7:1-ARGM-MNR 8:1-ARG1=Theme 
nw/wsj/20/wsj_2021.parse 31 24 gold term-v 29.3 IN term.01 1 ----- 0:1-ARGM-ADV 20:1*25:1-ARG1=Theme 21:1-ARGM-DIS 22:0-ARGM-MOD 24:0-rel 26:2-ARG3=Result 
nw/wsj/20/wsj_2021.parse 32 13 gold deal-v 83 Resolve_problem deal.01 null ----- 10:1*14:1-ARG1=Theme 13:0-rel 16:1-ARGM-MNR 
nw/wsj/20/wsj_2021.parse 35 11 gold entwine-v 22.2-2-1 NF entwine.01 null ----- 0:2-ARG1=Patient 10:0-ARGM-MNR 11:0-rel 12:1-ARG2=Co-Patient 
nw/wsj/20/wsj_2021.parse 36 14 gold advise-v 37.9-1 IN advise.01 1 ----- 12:1-ARG0=Agent 14:0-rel 15:1-ARG2=Topic 
