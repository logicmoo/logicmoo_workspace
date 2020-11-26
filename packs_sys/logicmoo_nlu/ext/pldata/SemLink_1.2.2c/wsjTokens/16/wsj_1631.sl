nw/wsj/16/wsj_1631.parse 0 4 gold shake-v 40.3.2 Body_movement shake.01 null ----- 0:1-ARGM-DIS 3:1-ARG0=Agent;Agent 4:0-rel 5:1-ARG1=Patient;Body_part 6:1-ARGM-COM 
nw/wsj/16/wsj_1631.parse 1 12 gold call-v 29.3 IN call.01 null ----- 10:1*13:1-ARG1=Theme 12:0-rel 14:2-ARG2=Result 10:1*13:1-LINK-PRO 
nw/wsj/16/wsj_1631.parse 1 18 gold make-v 29.3 Causation make.02 null ----- 10:2*16:1*17:1-ARG0=Agent 18:0-rel 19:3-ARG1=Theme 10:2*16:1-LINK-SLC 
nw/wsj/16/wsj_1631.parse 1 25 gold develop-v 26.2 IN develop.02 null ----- 22:1-ARG0=Agent 24:1-ARGM-EXT 25:0-rel 26:2-ARG1=Product 
nw/wsj/16/wsj_1631.parse 2 4 gold get-v 26.6.2 IN get.05 null ----- 0:1-ARG1=Patient 3:0-ARGM-NEG 4:0-rel 5:1-ARG2=Goal 7:1-ARGM-TMP 
nw/wsj/16/wsj_1631.parse 3 4 gold call-v 29.3 IN call.01 null ----- 3:1-ARG0=Agent 4:0-rel 5:1*6:1-ARG1=Theme 6:2-ARG2=Result 
nw/wsj/16/wsj_1631.parse 3 19 gold make-v 29.3 Causation make.02 null ----- 0:1*17:1-ARG0=Agent 19:0-rel 20:2-ARG1=Theme 
nw/wsj/16/wsj_1631.parse 4 4 gold happen-v 48.3 Event happen.01 null ----- 0:1*5:1-ARGM-TMP 1:2-ARG1=Theme;Event 4:0-rel 
nw/wsj/16/wsj_1631.parse 4 16 gold make-v 29.3 Causation make.02 null ----- 13:1-ARG0=Agent 15:1-ARGM-ADV 16:0-rel 17:2-ARG1=Theme 
nw/wsj/16/wsj_1631.parse 4 18 gold disappear-v 48.2 Departing disappear.01 1 ----- 17:1-ARG1=Patient;Theme 18:0-rel 
nw/wsj/16/wsj_1631.parse 4 27 gold cover-v 9.8 Adorning cover.02 null ----- 22:1*28:1-ARG1=Destination;Location 25:0-ARGM-MOD 27:0-rel 29:1-ARG2=Theme;Theme 
nw/wsj/16/wsj_1631.parse 5 5 gold bet-v 54.5 NF bet.01 2 ----- 0:1-ARGM-DIS 3:1-ARG0=Agent 4:0-ARGM-MOD 5:0-rel 6:1-ARG2 
nw/wsj/16/wsj_1631.parse 5 8 gold think-v 29.9-2 IN think.01 null ----- 7:1-ARG0 8:0-rel 9:1-ARG1 
nw/wsj/16/wsj_1631.parse 5 13 gold crash-v 18.4-1 Impact crash.01 4 ----- 10:1-ARG1=Theme;Impactor 13:0-rel 14:2-ARGM-TMP 
nw/wsj/16/wsj_1631.parse 7 13 gold happen-v 48.3 Event happen.01 null ----- 11:1-ARG1=Theme;Event 12:1-ARGM-TMP 13:0-rel 
nw/wsj/16/wsj_1631.parse 9 18 gold declare-v 37.7-1 Statement declare.02 null ----- 0:1*10:4-ARG0=Agent;Speaker 0:2-ARGM-ADV 17:1-ARGM-TMP 18:0-rel 19:1-ARG1=Topic;Message 
nw/wsj/16/wsj_1631.parse 9 22 gold record-v 25.4 Recording record.01 1 ----- 20:1-ARG0=Agent 21:0-ARGM-MOD 22:0-rel 23:1-ARG1=Theme 25:1-ARGM-PRD 
nw/wsj/16/wsj_1631.parse 10 15 gold hold-v 29.5-1 IN hold.02 null ----- 1:1-ARG0=Agent 7:1-ARGM-DIS 9:2-ARGM-ADV 15:0-rel 16:1-ARG1=Theme 
nw/wsj/16/wsj_1631.parse 10 24 gold quack-v 38 Make_noise quack.01 null ----- 18:1-ARG0=Agent;Noisy_event/Sound_source 24:0-rel 25:1-ARGM-MNR 
nw/wsj/16/wsj_1631.parse 11 11 gold insist-v 37.7 Statement insist.01 null ----- 9:1*13:1-ARG0=Agent;Speaker 10:1-ARGM-MNR 11:0-rel 12:1-ARG1=Topic;Message 
nw/wsj/16/wsj_1631.parse 11 14 gold associate-v 22.2-2 NF associate.01 1 ----- 6:1*8:1*15:1-ARG1=Patient 9:1*13:1-ARG0=Agent 14:0-rel 16:1-ARG2=Co-Patient 6:1*8:1-LINK-SLC 
nw/wsj/16/wsj_1631.parse 11 18 gold dread-v 31.2 NF dread.01 null ----- 18:0-rel 19:0,20:0,21:0-ARG1=Attribute 
nw/wsj/16/wsj_1631.parse 11 26 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 null ----- 0:1-ARGM-TMP 23:1-ARG1=Patient;Item 26:0-rel 27:1-ARG2=Extent;Difference 
nw/wsj/16/wsj_1631.parse 12 12 gold lose-v 13.2 NF lose.02 null ----- 0:1-ARGM-LOC 11:1-ARG0=Agent 12:0-rel 13:1-ARG1=Theme 
nw/wsj/16/wsj_1631.parse 13 1 gold hear-v 30.1-1-1 IN hear.01 null ----- 0:1-ARG0=Experiencer 1:0-rel 2:2-ARG1=Stimulus 7:1-ARG2 
nw/wsj/16/wsj_1631.parse 13 11 gold try-v 61 Attempt try.01 null ----- 8:1*9:1*10:1*12:1-ARG0=Agent 11:0-rel 12:2-ARG1=Theme 8:1*9:1-LINK-SLC 
nw/wsj/16/wsj_1631.parse 13 14 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 8:1*9:1*10:1*12:1-ARG0=Agent;Seller 14:0-rel 15:1-ARG2=Recipient 16:1-ARG1=Theme;Goods 8:1*9:1-LINK-SLC 
nw/wsj/16/wsj_1631.parse 14 6 gold call-v 29.3 IN call.01 null ----- 0:1*3:1*7:1-ARG1=Theme 6:0-rel 8:2-ARG2=Result 
nw/wsj/16/wsj_1631.parse 14 21 gold carry-v 54.3 Bringing carry.01 null ----- 12:1-ARGM-ADV 13:1-ARG0=Location 21:0-rel 22:3-ARG1=Value 
nw/wsj/16/wsj_1631.parse 14 33 gold terrify-v 31.1 Experiencer_obj terrify.01 null ----- 33:0-rel 34:0-ARG1=Experiencer;Experiencer 
nw/wsj/16/wsj_1631.parse 14 37 gold raise-v 9.4 NF raise.01 null ----- 27:1*32:1*42:1-ARGM-TMP 33:1-ARG0=Agent 35:0-ARGM-MOD 36:0-ARGM-NEG 37:0-rel 38:1-ARG1=Theme 39:1-ARGM-MNR 27:1*32:1-LINK-SLC 
nw/wsj/16/wsj_1631.parse 16 17 gold call-v 29.3 IN call.01 null ----- 10:2-ARG0=Agent 16:1-ARGM-TMP 17:0-rel 18:1*20:1-ARG1=Theme 20:2-ARG2=Result 
nw/wsj/16/wsj_1631.parse 20 20 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 19:1-ARG0=Agent;Seller 20:0-rel 21:2-ARG1=Theme;Goods 
nw/wsj/16/wsj_1631.parse 21 12 gold overwhelm-v 31.1 NF overwhelm.01 1 ----- 7:2*11:1-ARG1=Experiencer 12:0-rel 14:1-ARG0=Stimulus 
nw/wsj/16/wsj_1631.parse 21 20 gold dream-v 62 NF dream.02 3 ----- 15:1*22:1-ARG1=Theme 20:0,21:1-rel 15:1*22:1-LINK-PSV 
nw/wsj/16/wsj_1631.parse 21 27 gold peddle-v 13.1 NF peddle.01 1 ----- 15:2*23:1*28:1-ARG1=Theme 25:1-ARG0=Agent 27:0-rel 15:2*23:1-LINK-SLC 
nw/wsj/16/wsj_1631.parse 21 32 gold feel-v 30.1-1 Feeling feel.01 null ----- 2:1*6:1*33:1-ARG1=Stimulus 7:2*11:1*13:1-ARG0=Experiencer 11:2-ARGM-ADV 31:0-ARGM-NEG 32:0-rel 2:1*6:1-LINK-SLC 
nw/wsj/16/wsj_1631.parse 22 9 gold grouse-v 37.8 NF grouse.01 null ----- 1:2*10:1-ARG1=Topic 9:0-rel 11:2-ARG0=Agent 
nw/wsj/16/wsj_1631.parse 23 15 gold stress-v 9.9 NF stress.01 null ----- 0:1*13:1-ARG0=Agent 14:1-ARGM-ADV 15:0-rel 16:2-ARG1=Destination 31:2-ARGM-ADV 
nw/wsj/16/wsj_1631.parse 23 38 gold limit-v 76 NF limit.01 null ----- 33:1*39:1-ARG1=Patient 35:0-ARGM-MOD 37:1-ARGM-MNR 38:0-rel 40:1-ARGM-ADV 
nw/wsj/16/wsj_1631.parse 24 9 gold suggest-v 37.7-1-1 Statement suggest.01 null ----- 0:2-ARG0=Agent;Speaker 8:0-ARGM-NEG 9:0-rel 10:2-ARG1=Topic;Message 19:1-ARGM-ADV 
nw/wsj/16/wsj_1631.parse 24 12 gold involve-v 86.2-1 NF involve.01 null ----- 10:1*13:1-ARG1 12:0-rel 14:1-ARG2 10:1*13:1-LINK-PSV 
nw/wsj/16/wsj_1631.parse 24 16 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 15:1-ARG0=Agent;Buyer 16:0-rel 17:1-ARG1=Theme;Goods 
nw/wsj/16/wsj_1631.parse 26 3 gold imply-v 78 Evidence imply.01 1 ----- 0:1*2:1-ARG0=Cause;Support 1:0-ARGM-MOD 3:0-rel 4:2-ARG1=Topic;Proposition 
nw/wsj/16/wsj_1631.parse 26 8 gold invest-v 13.4.2 NF invest.01 null ----- 7:1-ARG0=Agent 8:0-rel 9:1-ARG2=Recipient 
nw/wsj/16/wsj_1631.parse 27 7 gold get-v 13.5.1-1 IN get.01 null ----- 6:1-ARG0=Agent 7:0-rel 8:4*19:1-ARG1=Theme 13:1-ARG2=Source 
nw/wsj/16/wsj_1631.parse 28 11 gold pour-v 43.4 Mass_motion pour.01 3 ----- 0:1-ARGM-DIS 2:1-ARG0=Source;Source 5:3-ARGM-PRD 11:0-rel 12:2-ARG1=Theme;Mass_theme 18:1-ARG3 32:2-ARGM-ADV 
nw/wsj/16/wsj_1631.parse 28 24 gold promise-v 13.3 Commitment promise.01 null ----- 19:1*22:1*23:1-ARG0=Agent 24:0-rel 25:2-ARG2=Theme 19:1*22:1-LINK-SLC 
nw/wsj/16/wsj_1631.parse 28 40 gold use-v 105 IN use.01 null ----- 38:1*45:1-ARG0 40:0-rel 41:2-ARG1 45:2-ARG2 
nw/wsj/16/wsj_1631.parse 29 6 gold perform-v 26.7-1 NF perform.01 null ----- 0:1*15:1-ARGM-TMP 1:2-ARG0=Agent 6:0-rel 7:2-ARGM-MNR 
nw/wsj/16/wsj_1631.parse 29 13 gold tarnish-v 45.5 Corroding_caused tarnish.01 null ----- 10:1*14:1-ARG1=Patient;Undergoer 13:0-rel 
nw/wsj/16/wsj_1631.parse 29 23 gold give-v 13.1-1 Giving give.01 null ----- 20:1-ARG0=Agent;Donor 23:0-rel 24:1-ARG2=Recipient;Recipient 25:1-ARG1=Theme;Theme 
nw/wsj/16/wsj_1631.parse 30 5 gold happen-v 48.3 Event happen.01 null ----- 0:1*2:1-ARG1=Theme;Event 5:0-rel 6:1-ARGM-TMP 7:1-ARG2 
nw/wsj/16/wsj_1631.parse 30 8 gold limit-v 76 NF limit.01 null ----- 8:0-rel 9:0-ARG1=Patient 
nw/wsj/16/wsj_1631.parse 31 4 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1*5:1-ARG1=Theme;Goods 2:1-ARGM-TMP 4:0-rel 
nw/wsj/16/wsj_1631.parse 31 25 gold have-v 100 IN have.03 null ----- 19:1*23:1*26:1-ARG1=Theme 24:1-ARG0=Pivot 25:0-rel 27:1-ARGM-LOC 19:1*23:1-LINK-SLC 
nw/wsj/16/wsj_1631.parse 33 7 gold prevail-v 47.1-1 NF prevail.02 2 ----- 0:1-ARGM-LOC 2:1-ARG1=Theme 6:1-ARGM-TMP 7:0-rel 
nw/wsj/16/wsj_1631.parse 34 9 gold remain-v 47.1-1 State_continue remain.01 null ----- 0:1-ARGM-DIS 3:1-ARG1=Theme 9:0-rel 10:1,12:1-ARG3 14:1-ARGM-CAU 
nw/wsj/16/wsj_1631.parse 34 18 gold carry-v 54.3 Bringing carry.01 null ----- 15:1-ARG0=Location 17:0-ARGM-NEG 18:0-rel 19:1-ARG1=Value 
nw/wsj/16/wsj_1631.parse 35 1 gold get-v 26.6.2 IN get.05 null ----- 0:1-ARG1=Patient 1:0-rel 2:1-ARG2=Goal 
nw/wsj/16/wsj_1631.parse 36 9 gold charge-v 54.5 Commerce_collect charge.01 null ----- 0:1*8:1-ARG0=Agent 9:0-rel 10:2-ARG1=Asset 
nw/wsj/16/wsj_1631.parse 36 21 gold impose-v 63 NF impose.01 null ----- 20:1-ARG0=Agent 21:0-rel 22:1-ARG1=Theme 
nw/wsj/16/wsj_1631.parse 37 10 gold fly-v 51.4.2 Motion fly.01 null ----- 9:1-ARG0=Agent 10:0-rel 11:1-ARG1=Theme 
nw/wsj/16/wsj_1631.parse 37 18 gold impose-v 63 NF impose.01 null ----- 0:1-ARGM-TMP 4:1-ARGM-DIS 6:2-ARG0=Agent 18:0-rel 19:2-ARG1=Theme 30:2-ARGM-TMP 
nw/wsj/16/wsj_1631.parse 37 33 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 31:1-ARG0=Agent;Seller 33:0-rel 34:1-ARG1=Theme;Goods 
nw/wsj/16/wsj_1631.parse 38 5 gold call-v 29.3 IN call.01 null ----- 0:0-ARGM-MOD 1:0-ARGM-NEG 2:1*6:1-ARG1=Theme 3:1-ARGM-ADV 5:0-rel 7:2-ARG2=Result 
nw/wsj/16/wsj_1631.parse 39 6 gold debate-v 36.1-1-1 Discussion debate.01 1 ----- 0:1-ARG0 6:0-rel 7:1-ARG1 
nw/wsj/16/wsj_1631.parse 39 14 gold expect-v 62 IN expect.01 null ----- 11:1-ARG0=Experiencer 13:0-ARGM-NEG 14:0-rel 15:1-ARG1=Theme 18:1-ARGM-TMP 
nw/wsj/16/wsj_1631.parse 40 4 gold expect-v 62 IN expect.01 null ----- 0:0-ARGM-DIS 1:1-ARG0=Experiencer 3:0-ARGM-NEG 4:0-rel 5:2-ARG1=Theme 15:1-ARGM-ADV 17:2-ARGM-ADV 
nw/wsj/16/wsj_1631.parse 40 26 gold enjoy-v 31.2-1 NF enjoy.01 null ----- 21:1*23:1*24:1-ARG0=Experiencer 25:1-ARGM-TMP 26:0-rel 27:1-ARG1=Attribute 21:1*23:1-LINK-SLC 
nw/wsj/16/wsj_1631.parse 42 7 gold suffer-v 31.3-4 Catastrophe suffer.01 null ----- 0:1-ARGM-DIS 2:1-ARG0=Experiencer 4:1-ARGM-ADV 7:0-rel 8:2-ARG1=Stimulus 15:1-ARGM-TMP 
nw/wsj/16/wsj_1631.parse 43 12 gold blame-v 33 IN blame.01 null ----- 0:2-ARG0=Agent 12:0-rel 13:1-ARG2=Attribute 14:1-ARG1=Theme 
nw/wsj/16/wsj_1631.parse 43 24 gold use-v 105 IN use.01 null ----- 23:1*25:1-ARG1 24:0-rel 26:1-ARG0 23:1*25:1-LINK-PSV 
nw/wsj/16/wsj_1631.parse 43 35 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 23:2*32:1*33:1-ARG0=Agent;Buyer 35:0-rel 38:1-ARG1=Theme;Goods 40:2-ARGM-TMP 
nw/wsj/16/wsj_1631.parse 43 37 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 23:2*32:1*33:1-ARG0=Agent;Seller 37:0-rel 38:1-ARG1=Theme;Goods 40:2-ARGM-TMP 
nw/wsj/16/wsj_1631.parse 43 44 gold prevail-v 47.1-1 NF prevail.02 2 ----- 40:1*45:1-ARGM-TMP 41:1-ARG1=Theme 44:0-rel 
nw/wsj/16/wsj_1631.parse 44 4 gold say-v 37.7-1 IN say.01 null ----- 3:1-ARG0=Agent 4:0-rel 0:2-ARG1-DSP=Topic 
nw/wsj/16/wsj_1631.parse 45 8 gold call-v 29.3 IN call.01 null ----- 2:1*9:1-ARG1=Theme 8:0-rel 10:1-ARG2=Result 2:1*9:1-LINK-PRO 
nw/wsj/16/wsj_1631.parse 45 17 gold mean-v 29.5-1 NF mean.01 null ----- 0:1-ARGM-TMP 2:2-ARG0=Agent 15:1-ARGM-NEG 17:0-rel 18:2-ARG1=Theme 
nw/wsj/16/wsj_1631.parse 47 2 gold say-v 37.7-1 IN say.01 null ----- 2:0-rel 5:1-ARG0=Agent 0:2-ARG1-DSP=Topic 
nw/wsj/16/wsj_1631.parse 47 11 gold mean-v 29.5-1 NF mean.01 null ----- 0:1-ARGM-TMP 8:1-ARG0=Agent 10:1-ARGM-TMP 11:0-rel 13:2-ARG1=Theme 
nw/wsj/16/wsj_1631.parse 48 7 gold add-v 37.7 NF add.01 null ----- 6:1-ARG0=Agent 7:0-rel 0:2-ARG1-DSP=Topic 
nw/wsj/16/wsj_1631.parse 48 23 gold swallow-v 39.3-2 NF swallow.01 2 ----- 21:1-ARG1=Patient 23:0-rel 24:1-ARGM-DIR 
