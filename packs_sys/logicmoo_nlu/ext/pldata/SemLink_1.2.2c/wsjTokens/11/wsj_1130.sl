nw/wsj/11/wsj_1130.parse 0 6 gold file-v 9.10 Submitting_documents file.01 1 ----- 0:2-ARG0=Agent 6:0-rel 7:1-ARG1=Theme 8:1-ARG3 15:2-ARG4 
nw/wsj/11/wsj_1130.parse 1 2 gold set-v 9.1-2 IN set.01 3 ----- 0:1-ARG0=Agent 2:0-rel 3:2-ARG1=Theme 7:1-ARG2=Destination 
nw/wsj/11/wsj_1130.parse 3 4 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 3:0-ARGM-ADV 4:0-rel 5:1-ARG1=Topic 
nw/wsj/11/wsj_1130.parse 4 5 gold file-v 9.10 Submitting_documents file.01 1 ----- 0:1-ARGM-TMP 3:1-ARG0=Agent 5:0-rel 6:2;17:1-ARG1=Theme 9:1-ARG3 12:1-ARG2=Destination 
nw/wsj/11/wsj_1130.parse 4 17 gold ask-v 58.1 Request ask.02 2 ----- 3:1-ARG0 17:0-rel 18:1-ARG1 
nw/wsj/11/wsj_1130.parse 6 11 gold disclose-v 37.7 Reveal_secret disclose.01 1 ----- 4:1,12:1,16:2-ARG1=Topic;Information 11:0-rel 13:1-ARGM-TMP 4:1*12:1-LINK-PSV 
nw/wsj/11/wsj_1130.parse 6 19 gold suffer-v 31.3-4 Catastrophe suffer.01 2 ----- 4:2*16:1*17:1*20:1-ARG1=Stimulus 19:0-rel 21:1-ARG0=Experiencer 4:2*16:1-LINK-SLC 
nw/wsj/11/wsj_1130.parse 7 2 gold invest-v 13.4.2 NF invest.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Theme 5:1-ARGM-PNC 
nw/wsj/11/wsj_1130.parse 8 9 gold allege-v 37.7 NF allege.01 1 ----- 0:1-ARGM-LOC 4:2-ARG0=Agent 9:0-rel 10:1-ARG1=Topic 
nw/wsj/11/wsj_1130.parse 8 12 gold encourage-v 37.9 NF encourage.01 1 ----- 11:1-ARG0=Agent 12:0-rel 13:2*19:1-ARG1=Recipient 20:1-ARG2=Topic 
nw/wsj/11/wsj_1130.parse 8 21 gold engage-v 107 NF engage.01 1 ----- 13:2*19:1-ARG0 21:0-rel 22:1-ARG2 
