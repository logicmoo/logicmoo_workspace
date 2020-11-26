nw/wsj/18/wsj_1899.parse 0 5 gold dispatch-v 11.1 Sending dispatch.01 1 ----- 0:1-ARG0=Agent;Sender 5:0-rel 6:2-ARG1=Theme;Theme 15:2-ARG2=Destination;Goal/Recipient 
nw/wsj/18/wsj_1899.parse 0 17 gold aid-v 72-1 Assistance aid.01 1 ----- 0:1*15:1-ARG0=Agent;Helper 17:0-rel 18:1-ARG2=Beneficiary;Benefited_party 
nw/wsj/18/wsj_1899.parse 1 23 gold work-v 73-3 IN work.01 null ----- 16:1*21:1*22:1-ARG0=Agent 23:0-rel 16:1*21:1-LINK-SLC 
nw/wsj/18/wsj_1899.parse 2 5 gold use-v 105 IN use.01 null ----- 0:3*8:1-ARG0 5:0-rel 6:1-ARG1 8:2-ARG2 
nw/wsj/18/wsj_1899.parse 2 10 gold push-v 59 Subjective_influence push.02 null ----- 0:3*8:1-ARG0=Agent 10:0-rel 11:1*15:1-ARG1=Patient 14:1-ARGM-MNR 15:2-ARG2=Result 
nw/wsj/18/wsj_1899.parse 2 20 gold establish-v 55.5-1 Intentionally_create establish.01 null ----- 18:1-ARG0 20:0-rel 21:2-ARG1 
nw/wsj/18/wsj_1899.parse 3 3 gold help-v 72-1 Assistance help.01 null ----- 0:1-ARG0=Agent;Helper 2:0-ARGM-MOD 3:0-rel 4:1*19:1-ARG2=Beneficiary;Benefited_party 6:1-ARG1=Theme;Goal/Focal_entity 
nw/wsj/18/wsj_1899.parse 3 6 gold cope-v 83-1 NF cope.01 1 ----- 4:1*19:1-ARG0=Agent 6:0-rel 7:1-ARGM-TMP 18:1-ARGM-TMP 
nw/wsj/18/wsj_1899.parse 3 20 gold wait-v 47.1-1 IN wait.01 null ----- 4:1*19:1-ARG1=Theme 20:0-rel 21:1-ARG2 
nw/wsj/18/wsj_1899.parse 4 6 gold speak-v 37.5 IN speak.01 null ----- 1:3-ARG0=Agent 6:0-rel 7:1-ARGM-LOC 9:1-ARG1=Topic 
nw/wsj/18/wsj_1899.parse 4 16 gold check-v 35.2 Inspecting check.01 null ----- 13:1-ARG0=Agent 16:0-rel 17:2-ARG1=Location 
nw/wsj/18/wsj_1899.parse 4 24 gold dispatch-v 11.1 Sending dispatch.01 1 ----- 0:1-ARGM-TMP 22:1-ARG0=Agent;Sender 24:0-rel 25:1-ARG1=Theme;Theme 27:2-ARGM-PNC 
nw/wsj/18/wsj_1899.parse 4 30 gold help-v 72-1 Assistance help.01 null ----- 25:1*27:1*28:1-ARG0=Agent;Helper 30:0-rel 25:1*27:1-LINK-SLC 
nw/wsj/18/wsj_1899.parse 5 6 gold place-v 9.1-2 Placing place.01 null ----- 0:0-ARGM-DIS 1:2*14:1-ARG0=Agent;Agent/Cause 6:0-rel 7:1-ARG1=Theme;Theme 11:1-ARG2=Destination;Goal 14:2-ARGM-PRD 
nw/wsj/18/wsj_1899.parse 5 18 gold send-v 11.1-1 Sending send.01 null ----- 1:2*14:1*16:1-ARG0=Agent;Sender 18:0-rel 19:1-ARGM-DIR 20:1-ARG1=Theme;Theme 
nw/wsj/18/wsj_1899.parse 5 23 gold alert-v 37.9 NF alert.01 1 ----- 0:0-ARGM-DIS 1:2*14:1*16:1-ARG0=Agent 23:0-rel 24:1-ARG1=Recipient 26:1-ARG2=Topic 
nw/wsj/18/wsj_1899.parse 5 29 gold have-v 100 IN have.03 null ----- 27:1-ARG0=Pivot 29:0-rel 30:1-ARG1=Theme 33:1-ARGM-MNR 
nw/wsj/18/wsj_1899.parse 6 5 gold initiate-v 55.5-1 Activity_start initiate.01 1 ----- 0:1-ARG0=Agent 5:0-rel 6:3-ARG1=Theme 
nw/wsj/18/wsj_1899.parse 6 17 gold fork-v 9.10 NF fork.01 3 ----- 12:1-ARG0=Agent 17:0,18:1-rel 19:2-ARG3 26:1-ARGM-PNC 
nw/wsj/18/wsj_1899.parse 6 33 gold say-v 37.7-1 IN say.01 null ----- 0:3*35:1-ARG1=Topic 30:1-ARG0=Agent 33:0-rel 
nw/wsj/18/wsj_1899.parse 7 5 gold send-v 11.1-1 Sending send.01 null ----- 0:1-ARG0=Agent;Sender 5:0-rel 6:2-ARG1=Theme;Theme 
nw/wsj/18/wsj_1899.parse 8 3 gold pull-v 23.2 IN pull.01 null ----- 0:1-ARG0=Agent 2:1-ARGM-DIS 3:0-rel 4:2;12:2-ARG1=Patient 7:1-ARG2=Co-Patient 
nw/wsj/18/wsj_1899.parse 8 15 gold help-v 72-1 Assistance help.01 null ----- 4:1*12:1*13:1-ARG0=Agent;Helper 15:0-rel 16:2-ARG1=Theme;Goal/Focal_entity 4:1*12:1-LINK-SLC 
nw/wsj/18/wsj_1899.parse 8 21 gold send-v 11.1-1 Sending send.01 null ----- 0:1-ARG0=Agent;Sender 2:1-ARGM-DIS 21:0-rel 22:1-ARG1=Theme;Theme 23:1-ARG2=Destination;Goal/Recipient 26:1-ARGM-ADV 
