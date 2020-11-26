nw/wsj/09/wsj_0969.parse 0 4 gold name-v 29.3 IN name.01 null ----- 0:2-ARG0=Agent 4:0-rel 5:2-ARG1=Theme 
nw/wsj/09/wsj_0969.parse 0 10 gold help-v 72-1 Assistance help.01 null ----- 5:1*7:1*8:1*11:1-ARG0=Agent;Helper 10:0-rel 11:2-ARG1=Theme;Goal/Focal_entity 5:1*7:1-LINK-SLC 
nw/wsj/09/wsj_0969.parse 1 12 gold name-v 29.3 IN name.01 null ----- 0:2*13:1-ARG1=Theme 12:0-rel 14:2-ARG2=Result 
nw/wsj/09/wsj_0969.parse 3 2 gold call-v 60 IN call.03 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 5:1-ARGM-TMP 7:1-ARGM-TMP 
nw/wsj/09/wsj_0969.parse 3 12 gold sign-v 13.5.3 NF sign.02 null ----- 8:1*10:1-ARG0=Agent 12:0-rel 13:2-ARG1=Theme 
nw/wsj/09/wsj_0969.parse 5 6 gold set-v 9.1-2 IN set.01 null ----- 0:2*7:1-ARG1=Theme 6:0-rel 
nw/wsj/09/wsj_0969.parse 5 12 gold expect-v 62 IN expect.01 null ----- 10:1,13:2-ARG1=Theme 12:0-rel 
nw/wsj/09/wsj_0969.parse 5 15 gold begin-v 55.1-1 Process_start begin.01 null ----- 10:1*13:1-ARG1=Theme;Event 15:0-rel 16:1-ARGM-TMP 
