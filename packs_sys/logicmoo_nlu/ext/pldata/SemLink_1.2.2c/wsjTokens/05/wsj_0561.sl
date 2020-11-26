nw/wsj/05/wsj_0561.parse 0 25 gold measure-v 54.1-1 Dimension measure.01 1 ----- 19:1*23:1*24:1-ARG2 25:0-rel 26:1-ARG1=Theme;Object 19:1*23:1-LINK-SLC 
nw/wsj/05/wsj_0561.parse 1 4 gold measure-v 54.1-1 Dimension measure.01 3 ----- 0:1*2:1*3:1-ARG1=Theme;Object 4:0-rel 5:1-ARG3=Value;Measurement 6:1-ARG2 0:1*2:1-LINK-SLC 
nw/wsj/05/wsj_0561.parse 3 1 gold put-v 9.1-2 IN put.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:2-ARG1=Theme 9:1-ARG2=Destination 11:1-ARGM-PNC 
nw/wsj/05/wsj_0561.parse 4 1 gold think-v 29.9-2 IN think.01 null ----- 0:1*3:1-ARG0 1:0-rel 2:1-ARG1 
nw/wsj/05/wsj_0561.parse 4 5 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:1*3:1-ARG0=Agent;Buyer 4:0-ARGM-MOD 5:0-rel 6:2-ARG1=Theme;Goods 16:1-ARGM-ADV 
nw/wsj/05/wsj_0561.parse 5 20 gold put-v 9.1-2 IN put.01 null ----- 4:2*19:1-ARG0=Agent 20:0-rel 21:1-ARG1=Theme 25:1-ARG2=Destination 
nw/wsj/05/wsj_0561.parse 5 33 gold rate-v 54.4 NF rate.01 1 ----- 0:1-ARGM-MNR 4:2*19:1-ARG1=Theme 18:1-ARGM-PRD 33:0-rel 34:1-ARG2=Value 
nw/wsj/05/wsj_0561.parse 6 12 gold make-v 29.3 Causation make.02 null ----- 0:2-ARG0=Agent 9:0-ARGM-MOD 10:0-ARGM-NEG 12:0-rel 13:2-ARG1=Theme 
nw/wsj/05/wsj_0561.parse 8 1 gold combine-v 22.1-1-1 Amalgamation combine.01 null ----- 0:1*32:1-ARG1=Patient;Part_1 1:0-rel 3:1-ARG2=Co-Patient;Part_2 
nw/wsj/05/wsj_0561.parse 8 33 gold give-v 13.1-1 Giving give.01 null ----- 0:2-ARGM-ADV 32:1-ARG0=Agent;Donor 33:0-rel 34:1-ARG2=Recipient;Recipient 35:2-ARG1=Theme;Theme 
nw/wsj/05/wsj_0561.parse 9 7 gold lose-v 13.2 NF lose.02 null ----- 5:1-ARG0=Agent 7:0-rel 8:1-ARGM-MNR 
nw/wsj/05/wsj_0561.parse 12 1 gold lead-v 51.7 Cotheme lead.01 null ----- 0:1*13:1-ARG0=Agent;Theme 1:0-rel 2:1-ARGM-TMP 6:1-ARGM-LOC 
nw/wsj/05/wsj_0561.parse 12 12 gold manage-v 74-1-1 IN manage.02 null ----- 0:1*13:1-ARG0 12:0-rel 13:2-ARG1 
nw/wsj/05/wsj_0561.parse 12 15 gold stir-v 31.1 Experiencer_obj stir.02 2 ----- 0:1*13:1-ARG0=Stimulus;Stimulus 15:0-rel 16:1-ARG1=Experiencer;Experiencer 18:1-ARGM-TMP 
nw/wsj/05/wsj_0561.parse 13 12 gold trail-v 51.6 Cotheme trail.01 1 ----- 11:1*17:1-ARG0=Agent;Theme 12:0-rel 13:2-ARGM-ADV 
nw/wsj/05/wsj_0561.parse 13 18 gold score-v 13.5.1-1 Getting score.01 1 ----- 9:1*43:1-ARGM-TMP 11:1*17:1-ARG0 11:2-ARGM-PRD 18:0-rel 19:1-ARGM-TMP 
nw/wsj/05/wsj_0561.parse 13 22 gold bring-v 11.3-1 Bringing bring.01 null ----- 9:1*43:1-ARGM-TMP 11:1*17:1-ARG0=Instrument 11:2-ARGM-ADV 22:0-rel 23:2-ARG1=Theme 33:1-ARG2=Destination 36:1-ARGM-MNR 
nw/wsj/05/wsj_0561.parse 14 15 gold catch-v 13.5.1 NF catch.01 null ----- 9:3*16:1-ARG1=Theme 15:0-rel 17:1-ARGM-LOC 21:1-ARG0=Agent 24:1-ARGM-TMP 
nw/wsj/05/wsj_0561.parse 14 19 gold warn-v 37.9-1 NF warn.01 1 ----- 19:0-rel 20:0-ARG0=Agent 
nw/wsj/05/wsj_0561.parse 14 29 gold exhale-v 40.1.3-1 Breathing exhale.01 null ----- 25:2-ARG0=Pivot;Agent 29:0-rel 30:1-ARGM-MNR 
nw/wsj/05/wsj_0561.parse 15 15 gold tell-v 37.2-1 Telling tell.01 null ----- 1:1-ARG0=Agent;Speaker 15:0-rel 16:1-ARG2=Recipient;Addressee 17:1-ARG1=Topic;Message 
nw/wsj/05/wsj_0561.parse 15 31 gold say-v 37.7-1 IN say.01 null ----- 1:2*32:1-ARG1=Topic 31:0-rel 33:1*38:1-ARG3 34:2-ARG0=Agent 
nw/wsj/05/wsj_0561.parse 16 2 gold tell-v 37.2-1 Request tell.01 null ----- 1:1-ARG0=Agent;Speaker 2:0-rel 3:1*4:1-ARG2=Recipient;Addressee 4:2-ARG1=Topic;Message 
nw/wsj/05/wsj_0561.parse 16 6 gold make-v 29.3 Causation make.02 null ----- 3:1*4:1-ARG0=Agent 6:0-rel 7:2-ARG1=Theme 
nw/wsj/05/wsj_0561.parse 19 7 gold win-v 13.5.1 NF win.01 null ----- 2:1-ARG0=Agent 6:0-ARGM-NEG 7:0-rel 8:1-ARGM-MNR 
nw/wsj/05/wsj_0561.parse 20 3 gold win-v 13.5.1 NF win.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Theme 7:2-ARGM-ADV 
nw/wsj/05/wsj_0561.parse 20 15 gold sound-v 30.4 Appearance sound.01 null ----- 13:1-ARG1=Stimulus;Phenomenon 14:0-ARGM-MOD 15:0-rel 16:1-ARG2 
nw/wsj/05/wsj_0561.parse 22 4 gold win-v 13.5.1 NF win.01 null ----- 0:1-ARG0=Agent 4:0-rel 5:2-ARG1=Theme 
nw/wsj/05/wsj_0561.parse 22 12 gold win-v 13.5.1 NF win.01 null ----- 10:1-ARG0=Agent 12:0-rel 13:2-ARG1=Theme 
nw/wsj/05/wsj_0561.parse 23 7 gold allow-v 64 IN allow.01 null ----- 0:1-ARGM-MNR 6:1-ARG0 7:0-rel 8:2-ARG1 
nw/wsj/05/wsj_0561.parse 23 10 gold suggest-v 37.7-1-1 Statement suggest.01 null ----- 8:1-ARG0=Agent;Speaker 10:0-rel 11:1-ARG1=Topic;Message 
nw/wsj/05/wsj_0561.parse 24 2 gold include-v 65 NF include.01 null ----- 0:1-ARG2=Location 2:0-rel 3:4-ARG1=Theme 
nw/wsj/05/wsj_0561.parse 24 15 gold ride-v 51.4.2 IN ride.01 null ----- 3:3*13:1*14:1-ARG0=Agent 15:0-rel 16:3-ARG1=Theme 31:1-ARGM-GOL 3:3*13:1-LINK-SLC 
nw/wsj/05/wsj_0561.parse 24 39 gold bewitch-v 31.1 Experiencer_obj bewitch.01 null ----- 39:0-rel 42:0,43:0-ARG1=Experiencer;Experiencer 
nw/wsj/05/wsj_0561.parse 24 41 gold bother-v 31.1 NF bother.01 null ----- 41:0-rel 42:0,43:0-ARG1=Experiencer 
nw/wsj/05/wsj_0561.parse 25 6 gold get-v 13.5.1-1 IN get.01 null ----- 0:1-ARG0=Agent 4:1-ARGM-DIS 6:0-rel 7:2-ARG1=Theme 13:1-ARGM-LOC 
nw/wsj/05/wsj_0561.parse 26 1 gold lead-v 51.7 Cotheme lead.01 null ----- 0:1*28:1-ARG0=Agent;Theme 1:0-rel 2:1-ARG1=Theme;Cotheme 5:1-ARGM-ADV 10:1-ARGM-ADV 
nw/wsj/05/wsj_0561.parse 27 12 gold come-v 48.1.1 NF come.03 null ----- 0:2-ARG1=Theme 12:0-rel 13:1-ARG2 19:1-ARGM-TMP 
nw/wsj/05/wsj_0561.parse 28 1 gold ask-v 37.1.2 Questioning ask.01 null ----- 0:1*13:1-ARG2=Recipient;Addressee 1:0-rel 3:2-ARG1=Topic;Message 
nw/wsj/05/wsj_0561.parse 28 14 gold allow-v 29.5-1 NF allow.03 null ----- 0:1*2:1*13:1-ARG0=Agent 0:2-ARGM-ADV 14:0-rel 15:1-ARG1=Theme 
nw/wsj/05/wsj_0561.parse 28 18 gold play-v 26.7-1-1 Competition play.01 null ----- 16:1-ARG0=Agent 17:0-ARGM-MOD 18:0-rel 19:1-ARG2 21:1-ARGM-MNR 24:1-ARGM-ADV 25:1-ARGM-LOC 
nw/wsj/05/wsj_0561.parse 29 8 gold get-v 13.5.1-1 IN get.01 null ----- 0:3-ARG0=Agent 8:0-rel 9:2-ARG1=Theme 
nw/wsj/05/wsj_0561.parse 31 10 gold get-v 13.5.1-1 IN get.01 null ----- 7:1-ARG0=Agent 10:0-rel 11:2-ARG1=Theme 
nw/wsj/05/wsj_0561.parse 32 15 gold set-v 9.1-2 IN set.01 null ----- 0:2-ARG0=Agent 15:0-rel 18:3-ARG1=Theme 
nw/wsj/05/wsj_0561.parse 32 29 gold steal-v 10.5-1 Theft steal.01 null ----- 29:0-rel 30:0-ARG1=Theme;Goods 
nw/wsj/05/wsj_0561.parse 33 24 gold contribute-v 13.2-1-1 NF contribute.01 null ----- 23:1-ARG0=Agent 24:0-rel 25:2-ARG1=Theme 
nw/wsj/05/wsj_0561.parse 33 41 gold stop-v 55.4-1 Process_stop stop.01 null ----- 28:2,32:1,36:1*39:1*40:1-ARG0=Agent 41:0-rel 42:1-ARG1=Theme 45:1-ARGM-TMP 28:2*39:1-LINK-SLC 
nw/wsj/05/wsj_0561.parse 34 2 gold think-v 29.9-2 IN think.01 null ----- 1:1*4:1-ARG0 2:0-rel 3:1-ARG1 
nw/wsj/05/wsj_0561.parse 34 10 gold get-v 26.6.2 IN get.04 null ----- 1:1*4:1-ARG0=Agent 5:0-ARGM-MOD 10:0-rel 11:2-ARG1=Patient 
nw/wsj/05/wsj_0561.parse 34 35 gold wear-v 41.3.1 Wearing wear.01 null ----- 30:1*33:1*36:1-ARG1=Theme;Clothing 34:1-ARG0=Agent;Wearer 35:0-rel 37:1-ARGM-MNR 30:1*33:1-LINK-SLC 
nw/wsj/05/wsj_0561.parse 36 13 gold allow-v 65 IN allow.01 null ----- 0:1-ARG0 13:0-rel 14:2-ARG1 19:1-ARGM-TMP 22:1-ARGM-TMP 23:1-ARGM-TMP 
nw/wsj/05/wsj_0561.parse 36 29 gold cause-v 27 Causation cause.01 1 ----- 24:1*30:1-ARG1=Theme;Effect 29:0-rel 31:1-ARG0=Cause;Cause 24:1*30:1-LINK-PSV 
nw/wsj/05/wsj_0561.parse 39 11 gold lift-v 9.4 NF lift.01 6 ----- 4:1-ARG0=Agent 11:0-rel 12:2-ARG1=Theme 15:1-ARGM-MNR 
nw/wsj/05/wsj_0561.parse 40 6 gold toast-v 33 NF toast.01 null ----- 3:1-ARG0=Agent 6:0-rel 7:1-ARG1=Theme 9:1-ARGM-MNR 
nw/wsj/05/wsj_0561.parse 40 27 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-TMP 2:1-ARGM-TMP 26:1-ARG0=Agent 27:0-rel 28:1-ARG1=Topic 
nw/wsj/05/wsj_0561.parse 40 30 gold think-v 29.9-2 IN think.01 null ----- 29:1-ARG0 30:0-rel 31:1-ARG1 
nw/wsj/05/wsj_0561.parse 41 2 gold give-v 13.1-1 Giving give.01 null ----- 1:1-ARG0=Agent;Donor 2:0-rel 3:0-ARG2=Recipient;Recipient 4:3-ARG1=Theme;Theme 
nw/wsj/05/wsj_0561.parse 41 21 gold win-v 13.5.1 NF win.01 null ----- 19:1-ARG0=Agent 20:1-ARGM-TMP 21:0-rel 22:1-ARG1=Theme 
nw/wsj/05/wsj_0561.parse 41 28 gold say-v 37.7-1 IN say.01 null ----- 1:3*29:1-ARG1=Topic 27:1-ARG0=Agent 28:0-rel 31:1-ARG3 
nw/wsj/05/wsj_0561.parse 42 41 gold feel-v 30.1-1 Feeling feel.01 null ----- 32:2*43:1-ARG0 41:0-rel 42:1-ARG1 
nw/wsj/05/wsj_0561.parse 42 42 gold oblige-v 59 NF oblige.02 1 ----- 32:2*43:1-ARG1=Patient 42:0-rel 43:2-ARG2=Result 
nw/wsj/05/wsj_0561.parse 42 45 gold dampen-v 42.3 NF dampen.02 1 ----- 32:2*43:1-ARG0=Agent 45:0-rel 46:1-ARG1=Patient 
nw/wsj/05/wsj_0561.parse 43 9 gold caution-v 37.9-1 Statement caution.01 1 ----- 1:3*10:1-ARG1=Topic;Message 8:1-ARG0=Agent;Speaker 9:0-rel 
nw/wsj/05/wsj_0561.parse 44 8 gold get-v 26.6.2 IN get.03 null ----- 6:1-ARG1=Patient 8:0-rel 9:1-ARG2=Goal 
nw/wsj/05/wsj_0561.parse 45 3 gold add-v 37.7 NF add.01 null ----- 0:1-ARG0=Agent 1:0-ARGM-MOD 3:0-rel 4:2-ARG1=Topic 
nw/wsj/05/wsj_0561.parse 46 3 gold predict-v 78 Predicting predict.01 1 ----- 2:1-ARGM-MNR 3:0-rel 4:1-ARG1=Topic;Eventuality 
nw/wsj/05/wsj_0561.parse 46 8 gold allow-v 64 IN allow.01 null ----- 5:1-ARG0 7:0-ARGM-MOD 8:0-rel 9:2-ARG1 
nw/wsj/05/wsj_0561.parse 46 28 gold occur-v 48.3 Event occur.01 null ----- 24:1-ARG1=Theme;Event 28:0-rel 
nw/wsj/05/wsj_0561.parse 46 34 gold win-v 13.5.1 NF win.01 null ----- 30:1-ARG0=Agent 34:0-rel 35:1-ARG1=Theme 36:1-ARGM-TMP 
nw/wsj/05/wsj_0561.parse 47 2 gold include-v 65 NF include.01 null ----- 0:1-ARG2 2:0-rel 3:2-ARG1 
