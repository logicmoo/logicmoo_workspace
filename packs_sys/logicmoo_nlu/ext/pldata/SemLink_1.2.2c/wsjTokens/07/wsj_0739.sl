nw/wsj/07/wsj_0739.parse 0 6 gold make-v 26.1-1 Manufacturing make.01 null ----- 0:1*7:1-ARG1=Product;Product 6:0-rel 8:1-ARG0=Agent;Manufacturer 0:1*7:1-LINK-PSV 
nw/wsj/07/wsj_0739.parse 1 2 gold report-v 37.7-1 Statement report.01 null ----- 0:1-ARG0 2:0-rel 3:1-ARG1 
nw/wsj/07/wsj_0739.parse 1 5 gold manage-v 74-1-1 IN manage.02 null ----- 4:1*6:1-ARG0 5:0-rel 6:2-ARG1 
nw/wsj/07/wsj_0739.parse 1 8 gold induce-v 59 IN induce.02 2 ----- 4:1*6:1-ARG0=Agent 8:0-rel 9:1-ARG1=Patient 10:2-ARG2=Result 
nw/wsj/07/wsj_0739.parse 1 26 gold cause-v 27 Causation cause.01 1 ----- 22:1*24:1*25:1-ARG0=Cause;Cause 26:0-rel 27:1-ARG1=Theme;Effect 22:1*24:1-LINK-SLC 
nw/wsj/07/wsj_0739.parse 2 2 gold show-v 78-1-1 IN show.01 null ----- 0:1-ARG0 2:0-rel 3:1-ARG1 
nw/wsj/07/wsj_0739.parse 2 22 gold report-v 37.7-1 Statement report.01 null ----- 20:1-ARG0=Agent;Speaker 22:0-rel 23:1-ARG1=Topic;Message 25:1-ARGM-LOC 
nw/wsj/07/wsj_0739.parse 3 26 gold give-v 13.1-1 Giving give.01 null ----- 13:1-ARG1=Theme;Theme 26:0-rel 27:1-ARG2=Recipient;Recipient 
nw/wsj/07/wsj_0739.parse 4 6 gold prevent-v 67 Preventing prevent.01 1 ----- 0:1*5:1-ARG3 6:0-rel 7:2-ARG1=Theme;Event 0:1*5:1-LINK-PRO 
nw/wsj/07/wsj_0739.parse 4 12 gold afflict-v 31.1 NF afflict.01 1 ----- 7:1*9:1*10:1-ARG0=Stimulus 11:1-ARGM-TMP 12:0-rel 13:3-ARG1=Experiencer 23:2-ARGM-PRD 7:1*9:1-LINK-SLC 
nw/wsj/07/wsj_0739.parse 4 24 gold cause-v 27 Causation cause.01 1 ----- 7:1*9:1*10:1*23:1-ARG0=Cause;Cause 24:0-rel 25:1-ARG1=Theme;Effect 7:1*9:1-LINK-SLC 
nw/wsj/07/wsj_0739.parse 4 26 gold estimate-v 54.4 Estimating estimate.01 1 ----- 26:0-rel 27:1-ARG2=Value 29:0-ARG1=Theme 
nw/wsj/07/wsj_0739.parse 5 5 gold cause-v 27 Causation cause.01 1 ----- 0:1-ARG0=Cause;Cause 3:1-ARGM-DIS 5:0-rel 6:2-ARG1=Theme;Effect 
nw/wsj/07/wsj_0739.parse 6 2 gold stem-v 48.1.1 NF stem.01 1 ----- 0:1-ARG1=Theme 2:0-rel 3:1-ARG2 
nw/wsj/07/wsj_0739.parse 6 21 gold cause-v 27 Causation cause.01 1 ----- 13:1*19:1*20:1-ARG0=Cause;Cause 21:0-rel 22:1-ARG1=Theme;Effect 13:1*19:1-LINK-SLC 
nw/wsj/07/wsj_0739.parse 7 9 gold use-v 105 IN use.01 null ----- 3:1*5:1*8:1*15:1-ARG1 9:0-rel 11:1-ARG2 3:1*5:1-LINK-SLC 
nw/wsj/07/wsj_0739.parse 9 6 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 6:0-rel 7:1-ARG1=Topic 
nw/wsj/07/wsj_0739.parse 9 10 gold succeed-v 74-1 IN succeed.01 null ----- 8:1-ARG0=Agent 10:0-rel 11:1-ARG1=Theme 
nw/wsj/07/wsj_0739.parse 9 13 gold get-v 26.6.2 IN get.04 null ----- 8:1*12:1-ARG0=Agent 13:0-rel 14:2-ARG1=Patient 8:1*12:1-LINK-PRO 
nw/wsj/07/wsj_0739.parse 9 29 gold use-v 105 IN use.01 null ----- 21:1*25:1*26:1*30:1-ARG1 27:0-ARGM-MOD 29:0-rel 31:1-ARG2 21:1*25:1-LINK-SLC 
nw/wsj/07/wsj_0739.parse 10 2 gold report-v 37.7-1 Statement report.01 null ----- 0:1-ARG0=Agent;Speaker 2:0-rel 3:1-ARG1=Topic;Message 
nw/wsj/07/wsj_0739.parse 10 10 gold pluck-v 13.5.1 Removing pluck.01 4 ----- 4:1*8:1-ARG0=Agent 10:0-rel 11:2-ARG1=Theme 19:1-ARG2=Source 
nw/wsj/07/wsj_0739.parse 11 7 gold take-v 103 Have_as_requirement take.10 null ----- 6:1*11:2-ARG0 7:0-rel 8:2-ARG1 
nw/wsj/07/wsj_0739.parse 12 14 gold cause-v 27 Causation cause.01 1 ----- 10:1*12:1*13:1-ARG0=Cause;Cause 14:0-rel 15:1-ARG1=Theme;Effect 10:1*12:1-LINK-SLC 
nw/wsj/07/wsj_0739.parse 12 21 gold transfer-v 13.2-2 NF transfer.01 null ----- 0:1-ARGM-ADV 1:1-ARGM-LOC 4:3*22:1-ARG1=Theme 18:0-ARGM-MOD 21:0-rel 23:1-ARG2=Recipient 
nw/wsj/07/wsj_0739.parse 12 27 gold call-v 29.3 IN call.01 null ----- 24:1*28:1-ARG1=Theme 27:0-rel 29:2-ARG2=Result 24:1*28:1-LINK-PRO 
nw/wsj/07/wsj_0739.parse 13 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/07/wsj_0739.parse 14 18 gold cause-v 27 Causation cause.01 1 ----- 13:1*16:1*17:1-ARG0=Cause;Cause 18:0-rel 19:1-ARG1=Theme;Effect 13:1*16:1-LINK-SLC 
nw/wsj/07/wsj_0739.parse 15 3 gold use-v 105 IN use.01 null ----- 2:1*21:1-ARG0 3:0-rel 4:2-ARG1 
nw/wsj/07/wsj_0739.parse 15 8 gold call-v 29.3 IN call.01 null ----- 4:1*9:1-ARG1=Theme 8:0-rel 10:2-ARG2=Result 14:1-ARGM-PRD 4:1*9:1-LINK-PRO 
nw/wsj/07/wsj_0739.parse 15 16 gold introduce-v 22.2-3-1 NF introduce.02 null ----- 15:1-ARG0=Agent 16:0-rel 17:1-ARG1=Patient 18:1-ARG2=Co-Patient 
nw/wsj/07/wsj_0739.parse 15 22 gold transfer-v 11.1 NF transfer.01 1 ----- 0:1-ARGM-TMP 2:1*21:1-ARG0=Agent 2:2-ARGM-MNR 22:0-rel 23:2-ARG1=Theme 26:1-ARG2=Destination 
nw/wsj/07/wsj_0739.parse 16 8 gold make-v 26.1-1 Intentionally_create make.01 null ----- 0:1-ARG0=Agent;Creator 5:1-ARGM-ADV 7:0-ARGM-NEG 8:0-rel 9:1-ARG1=Product;Created_entity 
nw/wsj/07/wsj_0739.parse 17 4 gold accompany-v 51.7 Cotheme accompany.01 2 ----- 0:0-ARGM-DIS 1:1*5:1-ARG1=Theme;Cotheme 4:0-rel 6:1-ARG0=Agent;Theme 
nw/wsj/07/wsj_0739.parse 17 12 gold call-v 29.3 IN call.01 null ----- 7:2*13:1-ARG1=Theme 12:0-rel 14:2-ARG2=Result 7:2*13:1-LINK-PRO 
nw/wsj/07/wsj_0739.parse 18 7 gold begin-v 55.1-1 Activity_start begin.01 null ----- 0:2,8:2-ARG1=Theme;Activity 7:0-rel 
nw/wsj/07/wsj_0739.parse 19 1 gold show-v 78-1-1 IN show.01 null ----- 0:1-ARG0 1:0-rel 2:1-ARG1 
