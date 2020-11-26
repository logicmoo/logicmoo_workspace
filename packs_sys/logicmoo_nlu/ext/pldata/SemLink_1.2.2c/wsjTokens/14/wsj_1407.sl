nw/wsj/14/wsj_1407.parse 0 7 gold admit-v 29.5-2 IN admit.01 1 ----- 0:1-ARG0=Agent 7:0-rel 8:2-ARG1=Theme 
nw/wsj/14/wsj_1407.parse 0 25 gold help-v 72-1 Assistance help.01 1 ----- 0:1*24:1-ARG0=Agent;Helper 25:0-rel 26:2-ARG1=Theme;Goal/Focal_entity 37:1-ARGM-TMP 0:1*24:1-LINK-PRO 
nw/wsj/14/wsj_1407.parse 0 27 gold funnel-v 9.3-1-1 NF funnel.01 1 ----- 0:2*26:1-ARG0=Agent 27:0-rel 28:1-ARG1=Theme 31:1-ARG2=Destination 0:2*26:1-LINK-PRO 
nw/wsj/14/wsj_1407.parse 1 19 gold admit-v 29.5-2 IN admit.01 1 ----- 0:2-ARG0=Agent 19:0-rel 20:2-ARG1=Theme 
nw/wsj/14/wsj_1407.parse 1 21 gold participate-v 73-2 Participation participate.01 1 ----- 0:2*20:1-ARG0=Agent;Participants/Participant_1 21:0-rel 22:1-ARG1=Theme;Event/Institution 
nw/wsj/14/wsj_1407.parse 1 28 gold bribe-v 59-1 NF bribe.01 1 ----- 23:1*25:1-ARGM-LOC 26:1-ARG0=Agent 28:0-rel 29:2-ARG1=Patient 23:1*25:1-LINK-SLC 
nw/wsj/14/wsj_1407.parse 2 5 gold leave-v 51.2-1 Quitting leave.01 1 ----- 0:1*3:1*4:1-ARG0=Theme 5:0-rel 6:1-ARG1=Initial_Location 8:1-ARGM-TMP 0:1*3:1-LINK-SLC 
nw/wsj/14/wsj_1407.parse 3 13 gold file-v 9.10 Submitting_documents file.01 1 ----- 12:1*14:1-ARG1=Theme 13:0-rel 15:1-ARG0=Agent 17:1-ARG4 25:1-ARG2=Destination 12:1*14:1-LINK-PSV 
nw/wsj/14/wsj_1407.parse 4 1 gold merge-v 22.1-1-1 Amalgamation merge.01 1 ----- 0:1-ARG1=Patient;Part_1 1:0-rel 2:1-ARG2=Co-Patient;Part_2 5:2-ARGM-PNC 10:1-ARGM-TMP 
nw/wsj/14/wsj_1407.parse 5 2 gold file-v 9.10 Submitting_documents file.01 1 ----- 0:1*3:1-ARG1=Theme 2:0-rel 4:1-ARG0=Agent 0:1*3:1-LINK-PSV 
nw/wsj/14/wsj_1407.parse 5 6 gold indicate-v 78-1 Evidence indicate.01 1 ----- 0:2-ARG0=Cause;Support 6:0-rel 7:1-ARG1=Topic;Proposition 
nw/wsj/14/wsj_1407.parse 5 10 gold try-v 61 Attempt try.01 1 ----- 8:1-ARG0=Agent 10:0-rel 11:2-ARG1=Theme 
nw/wsj/14/wsj_1407.parse 5 13 gold steer-v 51.7 NF steer.01 1 ----- 8:1*11:1-ARG0=Agent 13:0-rel 14:1-ARG2=Destination 16:2-ARG1=Theme 
nw/wsj/14/wsj_1407.parse 6 8 gold provide-v 13.4.1-2 Supply provide.01 1 ----- 0:1-ARG0=Agent 3:1-ARGM-ADV 7:1-ARGM-MNR 8:0-rel 9:1-ARG2=Recipient 11:1-ARG1=Theme 
nw/wsj/14/wsj_1407.parse 6 16 gold intend-v 62 Purpose intend.01 1 ----- 12:1-ARG2 16:0-rel 17:2-ARG1 
nw/wsj/14/wsj_1407.parse 6 19 gold give-v 13.1-1 Giving give.01 3 ----- 12:1*17:1-ARG0=Agent;Donor 19:0-rel 20:1-ARG2=Recipient;Recipient 21:2-ARG1=Theme;Theme 12:1*17:1-LINK-PSV 
nw/wsj/14/wsj_1407.parse 6 30 gold say-v 37.7-1 IN say.01 4 ----- 0:2*32:1-ARG1=Topic 28:1-ARG0=Agent 30:0-rel 
nw/wsj/14/wsj_1407.parse 7 3 gold eliminate-v 10.1 Removing eliminate.01 4 ----- 0:1*4:1-ARG1=Theme;Theme 1:1-ARGM-TMP 3:0-rel 5:1-ARG2=Source;Source 8:1-ARGM-ADV 
nw/wsj/14/wsj_1407.parse 7 10 gold receive-v 13.5.2 Receiving receive.01 1 ----- 0:1*4:1*9:1-ARG0=Agent;Donor 10:0-rel 11:1-ARG1=Theme;Theme 
nw/wsj/14/wsj_1407.parse 8 1 gold file-v 9.10 Submitting_documents file.01 1 ----- 0:1*2:1-ARG1=Theme 1:0-rel 3:1-ARG0=Agent 0:1*2:1-LINK-PSV 
nw/wsj/14/wsj_1407.parse 8 6 gold indicate-v 78-1 Evidence indicate.01 1 ----- 0:2-ARG0=Cause;Support 5:1-ARGM-DIS 6:0-rel 7:1-ARG1=Topic;Proposition 
nw/wsj/14/wsj_1407.parse 8 14 gold request-v 58.2 NF request.01 1 ----- 8:2-ARG0=Agent 14:0-rel 18:2-ARG1=Topic 23:1-ARG2=Recipient 30:1-ARGM-CAU 
nw/wsj/14/wsj_1407.parse 8 16 gold obtain-v 13.5.2-1 Getting obtain.01 1 ----- 8:2-ARG0=Agent;Recipient 16:0-rel 18:2-ARG1=Theme;Theme 23:1-ARG2=Source;Source 30:1-ARGM-CAU 
nw/wsj/14/wsj_1407.parse 8 39 gold request-v 58.2 NF request.01 1 ----- 31:1*33:1*40:1-ARG1=Topic 34:1-ARG0=Agent 39:0-rel 31:1*33:1-LINK-SLC 
nw/wsj/14/wsj_1407.parse 9 19 gold admit-v 29.5-2 IN admit.01 1 ----- 0:3-ARG0=Agent 19:0-rel 20:2-ARG1=Theme 
nw/wsj/14/wsj_1407.parse 9 21 gold participate-v 73-2 Participation participate.01 1 ----- 0:3*20:1-ARG0=Agent;Participants/Participant_1 21:0-rel 22:1-ARG1=Theme;Event/Institution 
nw/wsj/14/wsj_1407.parse 10 2 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/14/wsj_1407.parse 10 11 gold participate-v 73-2 Participation participate.01 1 ----- 4:2*9:1*10:1-ARG0=Agent;Participants/Participant_1 11:0-rel 12:1-ARG1=Theme;Event/Institution 4:2*9:1-LINK-SLC 
nw/wsj/14/wsj_1407.parse 10 16 gold leave-v 51.2-1 Quitting leave.01 1 ----- 4:3-ARG0=Theme 16:0-rel 17:1-ARG1=Initial_Location 
