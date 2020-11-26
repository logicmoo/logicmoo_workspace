nw/wsj/17/wsj_1731.parse 1 3 gold dip-v 45.6-1 IN dip.01 2 ----- 0:1-ARG1=Patient 3:0-rel 4:1-ARG4 11:2-ARGM-TMP 
nw/wsj/17/wsj_1731.parse 2 2 gold show-v 78-1-1 IN show.01 null ----- 0:1-ARG0 2:0-rel 3:1-ARG1 
nw/wsj/17/wsj_1731.parse 2 7 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 4:1-ARG1 7:0-rel 8:3-ARG2 
nw/wsj/17/wsj_1731.parse 2 17 gold expect-v 62 IN expect.01 null ----- 16:1-ARG1=Theme 17:0-rel 
nw/wsj/17/wsj_1731.parse 4 3 gold fuel-v 9.9 NF fuel.01 2 ----- 0:1-ARG2=Theme 3:0-rel 4:2-ARG1=Destination 
nw/wsj/17/wsj_1731.parse 4 7 gold damage-v 44 Damaging damage.01 1 ----- 4:1;12:1-ARG0=Agent;Agent/Cause 7:0-rel 8:1-ARG1=Patient;Patient 
nw/wsj/17/wsj_1731.parse 5 5 gold say-v 37.7-1 IN say.01 null ----- 0:0-ARGM-DIS 1:2-ARG0=Agent 5:0-rel 6:1-ARG1=Topic 
nw/wsj/17/wsj_1731.parse 5 15 gold prompt-v 59 NF prompt.02 1 ----- 7:1*14:1-ARG1=Patient 15:0-rel 17:1-ARG0=Agent 28:1-ARGM-TMP 
nw/wsj/17/wsj_1731.parse 6 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/17/wsj_1731.parse 6 16 gold fuel-v 9.9 NF fuel.01 2 ----- 3:2*17:1-ARG1=Destination 16:0-rel 18:2-ARG2=Theme 3:2*17:1-LINK-PSV 
nw/wsj/17/wsj_1731.parse 7 4 gold begin-v 55.1-1 Activity_start begin.01 null ----- 1:1,5:2-ARG1=Theme;Activity 4:0-rel 
nw/wsj/17/wsj_1731.parse 7 7 gold disassociate-v 23.1 NF disassociate.01 null ----- 1:1*5:1-ARG0=Agent 7:0-rel 8:1-ARG1=Patient 9:1-ARG2=Co-Patient 
nw/wsj/17/wsj_1731.parse 7 14 gold say-v 37.7-1 IN say.01 null ----- 1:2*15:1-ARG1=Topic 14:0-rel 16:1-ARG0=Agent 
nw/wsj/17/wsj_1731.parse 8 10 gold quote-v 37.1.1-1 NF quote.01 null ----- 0:1-ARGM-LOC 5:1-ARGM-TMP 7:1*11:1-ARG1=Source 10:0-rel 12:2-ARG2=Topic 
nw/wsj/17/wsj_1731.parse 9 2 gold quote-v 37.1.1-1 NF quote.01 null ----- 0:1*3:1-ARG1=Source 2:0-rel 4:1-ARG2=Topic 
nw/wsj/17/wsj_1731.parse 10 7 gold open-v 48.1.1 NF open.02 null ----- 0:1-ARGM-LOC 2:1-ARGM-TMP 4:1-ARG1=Theme 7:0-rel 8:1-ARGM-PNC 10:1-ARGM-EXT 14:1-ARGM-MNR 
nw/wsj/17/wsj_1731.parse 11 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/17/wsj_1731.parse 11 20 gold indicate-v 78-1 Evidence indicate.01 null ----- 18:1*21:1-ARG1=Topic;Proposition 19:1-ARGM-TMP 20:0-rel 
nw/wsj/17/wsj_1731.parse 12 10 gold show-v 78-1-1 IN show.01 null ----- 4:2-ARG0 10:0-rel 11:1-ARG1 
nw/wsj/17/wsj_1731.parse 13 9 gold know-v 29.5-1 IN know.01 2 ----- 4:1*10:1-ARG1=Theme 9:0-rel 11:1-ARG2=Predicate 4:1*10:1-LINK-PSV 
nw/wsj/17/wsj_1731.parse 13 24 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 4:2-ARG1 24:0-rel 25:2-ARG2 
nw/wsj/17/wsj_1731.parse 13 39 gold signal-v 37.4 NF signal.01 2 ----- 3:1-ARGM-CAU 37:1-ARG0=Agent 38:0-ARGM-MOD 39:0-rel 40:2-ARG1=Topic 
nw/wsj/17/wsj_1731.parse 14 1 gold suggest-v 37.7-1-1 Statement suggest.01 null ----- 0:1-ARG0=Agent;Speaker 1:0-rel 3:1-ARGM-ADV 5:1-ARG1=Topic;Message 
nw/wsj/17/wsj_1731.parse 14 9 gold hold-v 15.1-1 Inhibit_movement hold.01 null ----- 6:1-ARG0=Agent 8:0-ARGM-MOD 9:0-rel 10:2-ARG1=Theme 15:1-ARGM-MNR 18:2-ARGM-PRD 
nw/wsj/17/wsj_1731.parse 14 36 gold prevail-v 47.1-1 NF prevail.02 2 ----- 30:1*34:1*35:1-ARG1=Theme 36:0-rel 37:2-ARGM-TMP 30:1*34:1-LINK-SLC 
nw/wsj/17/wsj_1731.parse 15 11 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0 11:0-rel 12:1-ARG1 
nw/wsj/17/wsj_1731.parse 15 17 gold climb-v 45.6-1 Change_position_on_a_scale climb.02 null ----- 13:1-ARG1 17:0-rel 18:3-ARG2 27:1-ARGM-TMP 
nw/wsj/17/wsj_1731.parse 17 14 gold say-v 37.7-1 IN say.01 null ----- 1:2*15:1-ARG1 12:1*17:1-ARG0 14:0-rel 17:2-ARGM-ADV 
nw/wsj/17/wsj_1731.parse 17 18 gold predict-v 78 Predicting predict.01 1 ----- 12:1*17:1-ARG0=Cause;Evidence/Speaker 18:0-rel 19:1-ARG1=Topic;Eventuality 
nw/wsj/17/wsj_1731.parse 17 26 gold stay-v 47.1-1 IN stay.01 null ----- 20:1-ARGM-TMP 23:1-ARG1 25:0-ARGM-MOD 26:0-rel 27:2-ARGM-LOC 
nw/wsj/17/wsj_1731.parse 18 4 gold match-v 22.2-1 Compatibility match.01 null ----- 4:0-rel 5:1,8:0-ARG1=Patient;Item_1/Items 
nw/wsj/17/wsj_1731.parse 18 14 gold drain-v 10.3-1 Removing drain.01 2 ----- 12:1-ARG1 14:0-rel 15:1-ARG2 16:1-ARG1 
nw/wsj/17/wsj_1731.parse 19 6 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 4:1,7:2-ARG1 6:0-rel 
nw/wsj/17/wsj_1731.parse 19 9 gold post-v 11.1 Sending post.01 null ----- 4:1*7:1-ARG0=Agent;Sender 9:0-rel 10:2-ARG1=Theme;Theme 
nw/wsj/17/wsj_1731.parse 20 14 gold say-v 37.7-1 IN say.01 null ----- 14:0-rel 15:1-ARG1 17:2*35:1-ARG0 35:2-ARGM-ADV 
nw/wsj/17/wsj_1731.parse 20 36 gold add-v 37.7 NF add.01 null ----- 17:2*35:1-ARG0=Agent 36:0-rel 37:1-ARG1=Topic 
nw/wsj/17/wsj_1731.parse 21 2 gold comment-v 37.11-1-1 Statement comment.02 1 ----- 0:1-ARG0=Agent;Speaker 2:0-rel 3:1-ARG1 
nw/wsj/17/wsj_1731.parse 21 6 gold remain-v 47.1-1 State_continue remain.01 null ----- 4:1-ARG1=Theme 6:0-rel 7:1-ARG3 9:1-ARGM-ADV 12:1-ARGM-DIS 
nw/wsj/17/wsj_1731.parse 22 13 gold call-v 29.3 IN call.01 null ----- 0:2*22:1-ARG0=Agent 13:0-rel 14:1*17:1-ARG1=Theme 16:1-ARG2=Result 22:2-ARGM-PRD 
nw/wsj/17/wsj_1731.parse 22 27 gold remain-v 47.1-1 State_continue remain.01 null ----- 25:1-ARG1=Theme 27:0-rel 28:2-ARG3 37:2-ARGM-ADV 
nw/wsj/17/wsj_1731.parse 23 9 gold drop-v 45.6-1 Change_position_on_a_scale drop.01 null ----- 0:1-ARGM-LOC 8:1-ARG1 9:0-rel 10:1-ARG2 13:1-ARG4 19:1-ARGM-LOC 
nw/wsj/17/wsj_1731.parse 24 0 gold estimate-v 54.4 Estimating estimate.01 1 ----- 0:0-rel 1:0-ARG1=Theme 
