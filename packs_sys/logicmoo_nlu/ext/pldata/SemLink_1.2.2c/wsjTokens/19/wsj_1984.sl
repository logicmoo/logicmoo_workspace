nw/wsj/19/wsj_1984.parse 1 3 gold assume-v 93 Adopt_selection assume.01 1 ----- 2:1-ARG0 3:0-rel 0:2-ARG1-DSP 
nw/wsj/19/wsj_1984.parse 3 5 gold frame-v 9.9 NF frame.01 3 ----- 1:1*6:1-ARG1=Destination 3:1-ARGM-DIS 5:0-rel 7:1-ARGM-MNR 
nw/wsj/19/wsj_1984.parse 4 21 gold recite-v 26.7-1-1 NF recite.01 2 ----- 0:1-ARGM-DIS 3:1-ARGM-TMP 6:3-ARG0=Agent 21:0-rel 22:2-ARG1=Theme 34:2-ARGM-ADV 
nw/wsj/19/wsj_1984.parse 4 35 gold conclude-v 55.4-1 IN conclude.02 1 ----- 6:3*34:1-ARG0=Agent 35:0-rel 36:1-ARG2=Instrument 
nw/wsj/19/wsj_1984.parse 4 38 gold affect-v 31.1 NF affect.01 1 ----- 37:1-ARG0=Stimulus 38:0-rel 39:3-ARG1=Experiencer 
nw/wsj/19/wsj_1984.parse 9 3 gold feel-v 29.5-1 Opinion feel.02 3 ----- 0:1-ARG0=Agent 2:1-ARGM-ADV 3:0-rel 4:1-ARG1=Theme 
nw/wsj/19/wsj_1984.parse 10 16 gold confiscate-v 10.5 Removing confiscate.01 1 ----- 12:1-ARG0=Agent;Agent/Cause 16:0-rel 17:2-ARG1=Theme;Theme 
nw/wsj/19/wsj_1984.parse 10 27 gold owe-v 13.3 NF owe.01 1 ----- 22:1*25:1*26:1-ARG0=Agent 27:0-rel 28:1-ARG2=Goal 30:2-ARG1 22:1*25:1-LINK-SLC 
nw/wsj/19/wsj_1984.parse 11 8 gold have-v 100 IN have.03 1 ----- 0:1-ARG0=Pivot 2:1-ARGM-ADV 8:0-rel 9:2-ARG1=Theme 
nw/wsj/19/wsj_1984.parse 11 13 gold license-v 101 NF license.01 1 ----- 9:1*14:1-ARG1=Theme 13:0-rel 15:1-ARGM-LOC 9:1*14:1-LINK-PSV 
nw/wsj/19/wsj_1984.parse 11 18 gold shelter-v 16 NF shelter.01 2 ----- 9:1*19:1-ARG1=Patient 18:0-rel 20:1-ARGM-LOC 9:1*19:1-LINK-PSV 
nw/wsj/19/wsj_1984.parse 12 2 gold live-v 46 Residence live.01 2 ----- 0:2-ARG0=Theme;Resident 2:0-rel 3:1-ARGM-LOC 
nw/wsj/19/wsj_1984.parse 13 2 gold seize-v 10.5 NF seize.01 3 ----- 0:1*3:1-ARG1=Theme 2:0-rel 4:1-ARG0=Agent 0:1*3:1-LINK-PSV 
nw/wsj/19/wsj_1984.parse 14 4 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 0:1-ARGM-ADV 1:1-ARG0=Agent;Seller 3:0-ARGM-MOD 4:0,5:1-rel 6:2-ARG1=Theme;Goods 
nw/wsj/19/wsj_1984.parse 14 22 gold call-v 13.5.1 IN call.02 2 ----- 12:1-ARGM-TMP 17:1*23:1-ARG1=Theme 22:0-rel 24:1-ARGM-DIR 
nw/wsj/19/wsj_1984.parse 14 33 gold die-v 48.2 Death die.01 null ----- 33:0-rel 34:0-ARG1=Patient;Protagonist 
nw/wsj/19/wsj_1984.parse 15 11 gold guess-v 29.5-1 Estimating guess.01 1 ----- 9:1-ARG0=Agent 10:0-ARGM-MOD 11:0-rel 12:1-ARG1=Theme 
nw/wsj/19/wsj_1984.parse 15 18 gold lead-v 51.7 Cotheme lead.01 5 ----- 0:2*7:1*15:1*19:1-ARG1=Theme;Cotheme 8:1-ARGM-ADV 18:0-rel 20:1-ARG4=Destination 0:2*7:1-LINK-SLC 
nw/wsj/19/wsj_1984.parse 15 25 gold order-v 60-1 NF order.01 1 ----- 0:3*26:1-ARG1=Recipient 25:0-rel 27:2-ARG2=Topic 
nw/wsj/19/wsj_1984.parse 16 11 gold call-v 29.3 IN call.01 5 ----- 5:1*12:1-ARG1=Theme 11:0-rel 15:2-ARG2=Result 22:2-ARGM-PNC 5:1*12:1-LINK-PSV 
nw/wsj/19/wsj_1984.parse 17 7 gold make-v 26.1-1 Intentionally_create make.01 2 ----- 0:1*3:1*4:1*8:1-ARG1=Product;Created_entity 7:0-rel 9:1-ARGM-LOC 0:1*3:1-LINK-SLC 
nw/wsj/19/wsj_1984.parse 17 23 gold send-v 11.1-1 Sending send.01 1 ----- 22:1-ARG0=Agent;Sender 23:0-rel 24:3-ARG1=Theme;Theme 30:1-ARG2=Destination;Goal/Recipient 
nw/wsj/19/wsj_1984.parse 17 37 gold attempt-v 61 Attempt attempt.01 1 ----- 34:1-ARG0=Agent 37:0-rel 38:2-ARG1=Theme 
nw/wsj/19/wsj_1984.parse 17 40 gold regain-v 13.5.2 NF regain.01 1 ----- 38:1-ARG0=Agent 40:0-rel 41:2-ARG1=Theme 
nw/wsj/19/wsj_1984.parse 17 64 gold lead-v 51.7 Cotheme lead.02 4 ----- 58:1*62:1*63:1-ARG0=Agent;Theme 64:0-rel 65:2-ARG1=Theme;Cotheme 58:1*62:1-LINK-SLC 
nw/wsj/19/wsj_1984.parse 18 2 gold handle-v 15.1-1 NF handle.01 2 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Theme 5:1-ARG2 
nw/wsj/19/wsj_1984.parse 19 4 gold assemble-v 47.5.2 Gathering_up assemble.02 2 ----- 0:1-ARG0=Agent;Agent 4:0-rel 5:2-ARG1=Theme;Individuals 
nw/wsj/19/wsj_1984.parse 20 15 gold mix-v 22.1-1-1 Cause_to_amalgamate mix.01 1 ----- 0:1*14:1-ARG0=Agent;Agent 15:0-rel 16:2-ARG1=Patient;Part_1 
nw/wsj/19/wsj_1984.parse 20 46 gold serve-v 29.6-2 Assistance serve.01 2 ----- 39:1*44:1*45:1-ARG0=Agent 46:0-rel 47:1-ARG1=Attribute 39:1*44:1-LINK-SLC 
nw/wsj/19/wsj_1984.parse 20 69 gold have-v 100 IN have.03 2 ----- 67:1-ARG0=Pivot 69:0-rel 70:2-ARG1=Theme 81:1-ARGM-LOC 
nw/wsj/19/wsj_1984.parse 21 11 gold knit-v 26.1 NF knit.01 1 ----- 6:1-ARGM-MNR 9:1-ARG0=Agent 11:0-rel 12:1-ARG1=Product 
nw/wsj/19/wsj_1984.parse 21 31 gold wear-v 41.3.1 Wearing wear.01 1 ----- 23:1*30:1-ARG0=Agent;Wearer 31:0-rel 32:2-ARG1=Theme;Clothing 38:2-ARGM-PNC 
nw/wsj/19/wsj_1984.parse 22 12 gold lie-v 47.6 Being_located lie.01 1 ----- 6:2-ARG1=Theme 11:0-ARGM-NEG 12:0-rel 13:1-ARG2=Location 
nw/wsj/19/wsj_1984.parse 23 3 gold have-v 100 IN have.03 4 ----- 0:1-ARG0=Pivot 2:1-ARGM-DIS 3:0-rel 4:1-ARG1=Theme 5:1-ARGM-MNR 
nw/wsj/19/wsj_1984.parse 24 17 gold play-v 26.7-1-1 Competition play.01 2 ----- 15:1-ARG0=Agent 17:0-rel 18:1-ARG1=Theme 20:2-ARGM-MNR 
nw/wsj/19/wsj_1984.parse 25 9 gold experience-v 30.2 Feeling experience.01 1 ----- 6:1*7:1*8:1-ARG0=Experiencer;Experiencer 9:0-rel 10:1-ARG1=Stimulus;Emotion/Emotional_state 6:1*7:1-LINK-SLC 
nw/wsj/19/wsj_1984.parse 25 19 gold handle-v 15.1-1 NF handle.01 2 ----- 17:1-ARG0=Agent 19:0-rel 20:1-ARG1=Theme 22:1-ARGM-LOC 
nw/wsj/19/wsj_1984.parse 26 4 gold go-v 47.7 Motion go.01 10 ----- 0:2-ARG1=Theme;Theme 4:0-rel 5:1-ARG4 10:1-ARGM-DIS 
nw/wsj/19/wsj_1984.parse 27 2 gold deplore-v 31.2 Judgment deplore.01 null ----- 0:1-ARG0=Experiencer;Cognizer 2:0-rel 3:4-ARG1=Attribute;Role 
nw/wsj/19/wsj_1984.parse 27 15 gold see-v 30.1-1 Grasp see.01 2 ----- 3:3*13:1*16:1-ARG1=Stimulus 14:1-ARG0=Experiencer 15:0-rel 17:1-ARGM-LOC 3:3*13:1-LINK-SLC 
nw/wsj/19/wsj_1984.parse 28 3 gold begin-v 55.1-1 Process_start begin.01 1 ----- 1:1-ARG1=Theme;Event 3:0-rel 
nw/wsj/19/wsj_1984.parse 28 15 gold announce-v 37.7-1 Statement announce.01 1 ----- 0:1-ARGM-TMP 5:2-ARG0=Agent;Speaker 15:0-rel 18:3-ARG1=Topic;Message 
nw/wsj/19/wsj_1984.parse 29 3 gold speak-v 37.5 IN speak.01 1 ----- 0:2-ARG0=Agent 3:0-rel 4:1-ARGM-ADV 
nw/wsj/19/wsj_1984.parse 30 4 gold intone-v 26.7-1 NF intone.01 null ----- 0:1-ARGM-TMP 3:1-ARG0=Agent 4:0-rel 7:2-ARG1=Theme 
nw/wsj/19/wsj_1984.parse 31 26 gold lose-v 13.2 NF lose.02 5 ----- 15:1-ARGM-TMP 19:2-ARG0=Agent 26:0-rel 27:1-ARG1=Theme 29:1-ARGM-TMP 
nw/wsj/19/wsj_1984.parse 32 8 gold fill-v 9.8 Filling fill.01 null ----- 0:2*9:1-ARG1=Destination;Goal 5:1-ARGM-DIS 8:0-rel 10:1-ARG2=Theme;Theme 
nw/wsj/19/wsj_1984.parse 33 8 gold provide-v 13.4.1-2 Supply provide.01 1 ----- 0:1-ARGM-LOC 5:1-ARG0=Agent 8:0-rel 9:1-ARG2=Recipient 10:4-ARG1=Theme 
nw/wsj/19/wsj_1984.parse 33 21 gold disappear-v 48.2 Departing disappear.01 1 ----- 10:2*16:1*17:1-ARG1=Patient;Theme 19:1-ARGM-ADV 21:0-rel 22:1-ARGM-DIR 10:2*16:1-LINK-SLC 
