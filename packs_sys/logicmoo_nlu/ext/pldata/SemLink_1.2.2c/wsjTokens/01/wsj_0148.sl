nw/wsj/01/wsj_0148.parse 0 7 gold stop-v 67 Process_stop stop.03 null ----- 3:1*4:1-ARGM-MNR 5:1-ARG0=Agent 7:0-rel 8:1*11:1-ARG1=Theme 10:1-ARG2=Theme 3:1*4:1-LINK-SLC 
nw/wsj/01/wsj_0148.parse 0 12 gold plunge-v 45.6-1 Motion_directional plunge.01 null ----- 8:1*11:1-ARG1=Patient 12:0-rel 13:1-ARG2=Extent 15:1-ARGM-MNR 
nw/wsj/01/wsj_0148.parse 1 18 gold disconnect-v 23.1-2 NF disconnect.01 2 ----- 10:1-ARG0=Agent 16:1-ARGM-TMP 17:1-ARGM-MNR 18:0-rel 19:1*21:1-ARG1=Patient 
nw/wsj/01/wsj_0148.parse 2 11 gold put-v 9.1-2 IN put.01 null ----- 0:1-ARGM-LOC 5:1*12:1-ARG1=Theme 11:0-rel 13:1-ARG2=Destination 15:2-ARGM-TMP 
nw/wsj/01/wsj_0148.parse 2 19 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 16:1-ARG1=Patient;Item 19:0-rel 22:1-ARG2=Extent;Difference 
nw/wsj/01/wsj_0148.parse 2 21 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 null ----- 15:1*24:1-ARGM-TMP 16:1-ARG1=Patient;Item 21:0-rel 22:1-ARG2=Extent;Difference 
nw/wsj/01/wsj_0148.parse 3 13 gold scan-v 35.4 IN scan.01 1 ----- 7:1*11:1*12:1-ARG0=Agent 13:0-rel 14:1-ARG2=Theme 7:1*11:1-LINK-SLC 
nw/wsj/01/wsj_0148.parse 4 11 gold allow-v 60 NF allow.01 null ----- 0:1-ARGM-LOC 6:2,12:2-ARG1 10:0-ARGM-NEG 11:0-rel 
nw/wsj/01/wsj_0148.parse 4 14 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 null ----- 6:2*12:1-ARG1=Patient;Item 14:0-rel 15:2-ARG2=Extent;Difference 19:1-ARG3 25:1-ARGM-TMP 
nw/wsj/01/wsj_0148.parse 5 4 gold resume-v 55.1-1 Process_resume resume.01 null ----- 2:1*5:1-ARGM-TMP 3:1-ARG1=Theme;Process 4:0-rel 
nw/wsj/01/wsj_0148.parse 5 10 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 null ----- 2:2-ARGM-TMP 7:1-ARG1=Patient;Item 10:0-rel 11:1-ARG2=Extent;Difference 13:1-ARG3 
nw/wsj/01/wsj_0148.parse 6 4 gold allow-v 60 NF allow.01 null ----- 0:1-ARGM-DIS 2:1-ARG0 4:0-rel 5:2-ARG1 
nw/wsj/01/wsj_0148.parse 6 20 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 null ----- 15:1-ARG1=Patient;Item 20:0-rel 21:1-ARG2=Extent;Difference 
nw/wsj/01/wsj_0148.parse 6 32 gold slide-v 11.2-1 Motion slide.02 3 ----- 30:1-ARG1=Theme 32:0-rel 33:1-ARG2 37:1-ARGM-TMP 
nw/wsj/01/wsj_0148.parse 7 11 gold launch-v 55.5-1 IN launch.01 null ----- 3:1*12:1-ARG1 11:0-rel 13:1-ARG0 19:1-ARGM-TMP 23:2-ARGM-PRP 
nw/wsj/01/wsj_0148.parse 7 25 gold offer-v 13.3 NF offer.01 null ----- 23:1-ARG0=Agent 25:0-rel 26:1-ARG1=Theme 
nw/wsj/01/wsj_0148.parse 8 8 gold launch-v 55.5-1 IN launch.01 null ----- 0:2*9:1-ARG1 8:0-rel 10:1-ARGM-TMP 
nw/wsj/01/wsj_0148.parse 9 1 gold use-v 105 IN use.01 null ----- 0:1*16:1-ARG1 1:0-rel 3:1-ARG0 8:2-ARG2 
nw/wsj/01/wsj_0148.parse 9 17 gold handle-v 15.1-1 NF handle.01 null ----- 0:1*2:1*16:1-ARG0=Agent 0:2-ARGM-PRD 17:0-rel 18:2-ARG1=Theme 
nw/wsj/01/wsj_0148.parse 10 6 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 4:1-ARG0=Agent;Buyer 6:0-rel 8:1-ARG4=Beneficiary 17:1*33:2-ARG1=Theme;Goods 18:1*42:1-ARG3=Asset 19:1*48:1-ARGM-TMP 
nw/wsj/01/wsj_0148.parse 10 13 gold purchase-v 13.5.2-1 Commerce_buy purchase.01 1 ----- 9:1*10:1*11:1-ARG0=Agent;Buyer 13:0-rel 14:1-ARG1=Theme;Goods 9:1*10:1-LINK-SLC 
nw/wsj/01/wsj_0148.parse 10 21 gold deliver-v 11.1 Delivery deliver.01 null ----- 4:1-ARG0=Agent 17:1*33:2-ARG1=Theme 18:1*42:1-ARGM-MNR 19:1*48:1-ARGM-TMP 21:0-rel 23:1-ARGM-GOL 
nw/wsj/01/wsj_0148.parse 10 27 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 24:1*25:1*26:1-ARG0=Agent;Seller 27:0-rel 28:1-ARG1=Theme;Goods 24:1*25:1-LINK-SLC 
nw/wsj/01/wsj_0148.parse 11 4 gold nullify-v 106 NF nullify.01 1 ----- 0:1*5:1-ARG1=Patient 3:1-ARGM-ADV 4:0-rel 6:1-ARG0=Agent 10:1-ARGM-TMP 
nw/wsj/01/wsj_0148.parse 12 8 gold follow-v 51.6 Cotheme follow.02 null ----- 2:2-ARG0=Agent;Theme 8:0-rel 9:2-ARG1=Theme;Cotheme 
nw/wsj/01/wsj_0148.parse 12 14 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 2:2*13:1-ARG0=Agent;Buyer 14:0-rel 17:3-ARG1=Theme;Goods 2:2*13:1-LINK-PRO 
nw/wsj/01/wsj_0148.parse 12 16 gold hold-v 15.1-1 Manipulation hold.01 null ----- 2:2*13:1-ARG0=Agent;Agent 16:0-rel 17:3-ARG1=Theme;Entity 2:2*13:1-LINK-PRO 
nw/wsj/01/wsj_0148.parse 12 24 gold match-v 22.2-1 Compatibility match.01 1 ----- 17:2*21:1*22:1-ARG1=Patient;Item_1/Items 24:0-rel 25:2-ARG1=Patient;Item_1/Items 17:2*21:1-LINK-SLC 
nw/wsj/01/wsj_0148.parse 13 10 gold swap-v 13.6-1-1 Exchange swap.01 1 ----- 0:1*9:1-ARG0=Agent 10:0-rel 11:1-ARG1=Theme 13:1-ARG3=Co-Theme 15:2-ARGM-TMP 
nw/wsj/01/wsj_0148.parse 14 13 gold involve-v 103 NF involve.01 null ----- 7:3-ARG2 13:0-rel 14:2-ARG1 
nw/wsj/01/wsj_0148.parse 15 10 gold employ-v 105 Using employ.02 2 ----- 4:1*8:1*9:1-ARG0=Agent 10:0-rel 11:1-ARG1=Theme 4:1*8:1-LINK-SLC 
nw/wsj/01/wsj_0148.parse 16 5 gold call-v 29.3 Labeling call.01 null ----- 0:1*6:1-ARG1=Theme 4:1-ARGM-ADV 5:0-rel 7:2-ARG2=Result 12:1-ARGM-CAU 0:1*6:1-LINK-PRO 
nw/wsj/01/wsj_0148.parse 16 21 gold devise-v 55.5-1 Coming_up_with devise.01 1 ----- 0:2-ARG0=Agent 21:0-rel 22:2-ARG1=Theme 
nw/wsj/01/wsj_0148.parse 16 32 gold know-v 29.5-1 IN know.01 2 ----- 22:1*28:1*29:1*33:1-ARG1=Theme 31:1-ARGM-MNR 32:0-rel 34:1-ARG2=Predicate 22:1*28:1-LINK-SLC 
nw/wsj/01/wsj_0148.parse 17 6 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 5:1-ARG0=Agent;Buyer 6:0-rel 9:2-ARG1=Theme;Goods 12:1-ARGM-ADV 
nw/wsj/01/wsj_0148.parse 17 8 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 5:1-ARG0=Agent;Seller 8:0-rel 9:2-ARG1=Theme;Goods 12:1-ARGM-TMP 
nw/wsj/01/wsj_0148.parse 18 4 gold try-v 61 Attempt try.01 null ----- 0:1*3:1*5:1-ARG0=Agent 4:0-rel 5:2-ARG1=Theme 
nw/wsj/01/wsj_0148.parse 18 7 gold capture-v 10.5-1 NF capture.01 3 ----- 0:1*3:1*5:1-ARG0=Agent 7:0-rel 8:2-ARG1=Theme 
nw/wsj/01/wsj_0148.parse 19 16 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:1-ARGM-ADV 10:1-ARGM-DIS 13:1-ARG0=Agent;Buyer 15:0-ARGM-MOD 16:0-rel 17:1-ARG1=Theme;Goods 
nw/wsj/01/wsj_0148.parse 19 19 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1-ARG3 10:1-ARGM-DIS 13:1-ARG0=Agent;Seller 15:0-ARGM-MOD 19:0-rel 20:1-ARG1=Theme;Goods 
nw/wsj/01/wsj_0148.parse 20 8 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 6:1-ARG0=Agent;Buyer 8:0-rel 11:2-ARG1=Theme;Goods 18:1-ARGM-TMP 
nw/wsj/01/wsj_0148.parse 20 10 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 6:1-ARG0=Agent;Seller 10:0-rel 11:2-ARG1=Theme;Goods 18:1-ARGM-TMP 
nw/wsj/01/wsj_0148.parse 22 20 gold trade-v 13.6-1 Exchange trade.01 1 ----- 8:2*21:1-ARG1=Theme 20:0-rel 22:1-ARGM-LOC 8:2*21:1-LINK-PSV 
nw/wsj/01/wsj_0148.parse 22 48 gold trade-v 13.6-1 Exchange trade.01 1 ----- 29:2*49:1-ARG1=Theme 48:0-rel 50:1-ARGM-LOC 29:2*49:1-LINK-PSV 
nw/wsj/01/wsj_0148.parse 22 64 gold trade-v 13.6-1 Exchange trade.01 1 ----- 59:1*65:1-ARG1=Theme 64:0-rel 66:1-ARGM-LOC 59:1*65:1-LINK-PSV 
nw/wsj/01/wsj_0148.parse 22 74 gold base-v 97.1 NF base.02 null ----- 59:1*75:1-ARG1=Theme 74:0-rel 76:1-ARG2=Source 59:1*75:1-LINK-PSV 
nw/wsj/01/wsj_0148.parse 22 79 gold select-v 13.5.2 Choosing select.01 1 ----- 77:1*80:1-ARG1=Theme;Chosen 79:0-rel 81:1-ARG2=Source 77:1*80:1-LINK-PSV 
nw/wsj/01/wsj_0148.parse 23 6 gold give-v 13.1-1 Giving give.01 null ----- 5:1-ARG0=Agent;Donor 6:0-rel 7:1-ARG2=Recipient;Recipient 8:2-ARG1=Theme;Theme 
nw/wsj/01/wsj_0148.parse 23 20 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 7:1*18:1-ARG0=Agent;Buyer 20:0-rel 31:2-ARG1=Theme;Goods 38:1-ARGM-TMP 42:1-ARG3=Asset 7:1*18:1-LINK-PRO 
nw/wsj/01/wsj_0148.parse 23 26 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 7:1*18:1-ARG0=Agent;Seller 26:0-rel 28:1-ARG1=Theme;Goods 31:2-ARGM-PRD 38:1-ARGM-TMP 42:1-ARG3 7:1*18:1-LINK-PRO 
nw/wsj/01/wsj_0148.parse 23 36 gold underlie-v 47.8 NF underlie.01 null ----- 36:0-rel 37:0-ARG0=Theme 
nw/wsj/01/wsj_0148.parse 23 47 gold know-v 29.5-1 IN know.01 2 ----- 43:1*48:1-ARG1=Theme 47:0-rel 49:1-ARG2=Predicate 43:1*48:1-LINK-PSV 
nw/wsj/01/wsj_0148.parse 24 5 gold underlie-v 47.8 NF underlie.01 null ----- 5:0-rel 6:0-ARG0=Theme 
nw/wsj/01/wsj_0148.parse 26 12 gold occur-v 48.3 Event occur.01 null ----- 6:2-ARG1=Theme;Event 12:0-rel 13:1-ARGM-MNR 
