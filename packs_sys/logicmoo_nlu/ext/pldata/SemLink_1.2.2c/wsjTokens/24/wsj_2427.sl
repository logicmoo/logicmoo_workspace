nw/wsj/24/wsj_2427.parse 0 3 gold end-v 55.4-1 Cause_to_end end.01 2 ----- 0:1-ARG0=Agent 3:0-rel 4:2-ARG1=Theme 21:1-ARGM-TMP 22:1-ARGM-TMP 
nw/wsj/24/wsj_2427.parse 0 25 gold get-v 26.6.2 IN get.03 11.3 ----- 23:1-ARG1=Patient 25:0-rel 26:1-ARG2=Goal 28:1-ARGM-LOC 
nw/wsj/24/wsj_2427.parse 1 17 gold threaten-v 31.1 NF threaten.01 2 ----- 6:2-ARG0=Stimulus 17:0-rel 18:2-ARG2 
nw/wsj/24/wsj_2427.parse 2 3 gold begin-v 55.1-1 Activity_start begin.01 2 ----- 0:1-ARG0=Agent;Agent 3:0-rel 4:2-ARG1=Theme;Activity 
nw/wsj/24/wsj_2427.parse 2 13 gold make-v 26.1-1 Manufacturing make.01 2 ----- 8:1*11:1*12:1-ARG0=Agent;Manufacturer 13:0-rel 14:1-ARG1=Product;Product 8:1*11:1-LINK-SLC 
nw/wsj/24/wsj_2427.parse 2 24 gold vote-v 29.3 NF vote.01 1 ----- 0:1*23:1-ARG0=Agent 24:0-rel 25:1-ARG2=Result 28:2-ARG1=Theme 0:1*23:1-LINK-PRO 
nw/wsj/24/wsj_2427.parse 2 30 gold abandon-v 51.2 Departing abandon.01 1 ----- 0:1*23:1*28:1-ARG0=Theme;Theme 30:0-rel 31:1-ARG1=Initial_Location;Source 0:1*23:1-LINK-PRO 
nw/wsj/24/wsj_2427.parse 3 3 gold base-v 97.1 NF base.02 1 ----- 0:1*4:1-ARG1=Theme 3:0-rel 5:1-ARG2=Source 
nw/wsj/24/wsj_2427.parse 3 24 gold leave-v 51.2-1 Departing leave.01 1 ----- 17:1*20:1*27:1-ARGM-TMP 21:1-ARG0=Theme;Theme 24:0-rel 25:1-ARG1=Initial_Location;Source 17:1*20:1-LINK-SLC 
nw/wsj/24/wsj_2427.parse 4 15 gold attach-v 22.3-2-1 Inchoative_attaching attach.01 1 ----- 12:1*16:1-ARG1=Patient;Connector 15:0-rel 17:1-ARG1=Patient;Connector 12:1*16:1-LINK-PSV 
nw/wsj/24/wsj_2427.parse 4 23 gold pose-v 37.1.1 NF pose.02 1 ----- 0:2-ARG0=Agent 22:0-ARGM-MOD 23:0-rel 24:1-ARG1=Topic 28:1-ARG2=Recipient 
nw/wsj/24/wsj_2427.parse 5 2 gold complain-v 37.8 Statement complain.01 1 ----- 0:1-ARG0=Agent;Speaker 2:0-rel 3:1-ARG1=Topic;Message 
nw/wsj/24/wsj_2427.parse 5 19 gold say-v 37.7-1 IN say.01 1 ----- 11:1*17:1-ARG1=Topic 18:1-ARG0=Agent 19:0-rel 21:2-ARG1=Topic 11:1*17:1-LINK-SLC 
nw/wsj/24/wsj_2427.parse 5 22 gold fail-v 74-2 IN fail.01 1 ----- 11:1*17:1*21:1-ARG1 22:0-rel 23:2-ARG2 11:1*17:1-LINK-SLC 
nw/wsj/24/wsj_2427.parse 5 25 gold take-v 11.3 IN take.01 7 ----- 11:1*17:1*21:1*23:1-ARG0=Instrument 25:0-rel 26:1-ARG2=Initial_Location 28:2-ARG1=Theme 11:1*17:1-LINK-SLC 
