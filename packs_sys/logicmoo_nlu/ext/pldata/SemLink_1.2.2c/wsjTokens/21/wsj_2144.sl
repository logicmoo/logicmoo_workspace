nw/wsj/21/wsj_2144.parse 0 11 gold report-v 37.7-1 Statement report.01 null ----- 1:1-ARGM-TMP 6:1-ARG0 10:0-ARGM-MOD 11:0-rel 12:3-ARG1 
nw/wsj/21/wsj_2144.parse 1 3 gold win-v 13.5.1 NF win.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Theme 6:1-ARGM-TMP 
nw/wsj/21/wsj_2144.parse 2 7 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 5:1-ARGM-TMP 7:0-rel 8:1-ARG1=Topic 
nw/wsj/21/wsj_2144.parse 2 13 gold start-v 55.1-1 Activity_start start.01 null ----- 9:1-ARG0=Agent;Agent 12:0-ARGM-MOD 13:0-rel 14:2-ARG1=Theme;Activity 
nw/wsj/21/wsj_2144.parse 2 15 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 9:1*14:1-ARG0=Agent;Seller 15:0-rel 16:1-ARG1=Theme;Goods 17:1-ARGM-LOC 
nw/wsj/21/wsj_2144.parse 4 3 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1-ARG0=Agent;Seller 3:0-rel 4:1-ARG1=Theme;Goods 6:1-ARGM-MNR 
nw/wsj/21/wsj_2144.parse 4 17 gold gain-v 13.5.1-1 Getting gain.02 null ----- 10:1*14:1*15:1-ARG0=Agent;Recipient 17:0-rel 18:1-ARG1=Theme;Theme 20:1-ARG2=Source;Source 10:1*14:1-LINK-SLC 
nw/wsj/21/wsj_2144.parse 6 3 gold wrap-v 9.7-1 Filling wrap.01 1 ----- 0:1*9:1-ARG0=Agent;Agent 3:0-rel 4:1-ARG2=Theme;Theme 5:1-ARG1=Destination;Goal 9:2-ARGM-ADV 
nw/wsj/21/wsj_2144.parse 6 15 gold permit-v 65 Make_possible_to_do permit.01 1 ----- 0:1*9:1*14:1-ARG0=Agent 15:0-rel 16:1-ARG2=Agent 17:2-ARG1=Theme 
nw/wsj/21/wsj_2144.parse 6 19 gold pass-v 48.3 Process_end pass.03 null ----- 16:1*17:1-ARG1=Theme 19:0-rel 20:1-ARGM-DIR 
nw/wsj/21/wsj_2144.parse 8 5 gold seize-v 10.5 NF seize.01 3 ----- 0:1-ARGM-TMP 3:1-ARG0=Agent 5:0-rel 6:2-ARG1=Theme 17:1-ARGM-TMP 
nw/wsj/21/wsj_2144.parse 8 11 gold make-v 26.1-1 Manufacturing make.01 null ----- 6:1*12:1-ARG1=Product;Product 11:0-rel 13:1-ARG0=Agent;Manufacturer 6:1*12:1-LINK-PSV 
nw/wsj/21/wsj_2144.parse 8 19 gold show-v 78-1-1 IN show.01 null ----- 18:1-ARG0 19:0-rel 20:2-ARG1 
nw/wsj/21/wsj_2144.parse 9 4 gold debate-v 36.1-1-1 Discussion debate.01 1 ----- 0:1-ARGM-TMP 2:1-ARG0 4:0-rel 5:2-ARG1 10:2-ARGM-ADV 
nw/wsj/21/wsj_2144.parse 9 11 gold claim-v 37.7-1 Statement claim.01 null ----- 10:1-ARG0=Agent;Speaker 11:0-rel 12:1-ARG1=Topic;Message 
nw/wsj/21/wsj_2144.parse 9 14 gold cause-v 27 Causation cause.01 1 ----- 13:1-ARG0=Cause;Cause 14:0-rel 15:1-ARG1=Theme;Effect 
nw/wsj/21/wsj_2144.parse 11 2 gold overcome-v 90 NF overcome.01 1 ----- 0:1*8:1-ARG0=Theme 2:0-rel 3:1-ARG1=Co-Theme 
nw/wsj/21/wsj_2144.parse 11 7 gold keep-v 55.6 Activity_ongoing keep.02 null ----- 7:0-rel 8:2-ARG1=Theme 
nw/wsj/21/wsj_2144.parse 12 7 gold cost-v 54.2 Expensiveness cost.01 null ----- 0:1*5:1*6:1-ARG1=Theme;Goods 7:0-rel 8:2-ARG2=Value;Asset 0:1*5:1-LINK-SLC 
nw/wsj/21/wsj_2144.parse 12 15 gold expect-v 62 IN expect.01 null ----- 0:2,16:2-ARG1=Theme 15:0-rel 
nw/wsj/21/wsj_2144.parse 12 18 gold last-v 54.2 NF last.01 null ----- 0:2*16:1-ARG1=Theme 18:0-rel 19:1-ARG2=Value 
nw/wsj/21/wsj_2144.parse 13 12 gold wear-v 41.3.1 Wearing wear.01 null ----- 9:1*13:1-ARG1=Theme;Clothing 12:0-rel 14:1-ARGM-TMP 
nw/wsj/21/wsj_2144.parse 13 22 gold offer-v 13.3 NF offer.01 null ----- 0:1-ARGM-TMP 2:2*23:1-ARG1=Theme 22:0-rel 
nw/wsj/21/wsj_2144.parse 14 17 gold cost-v 54.2 Expensiveness cost.01 null ----- 13:2-ARG1=Theme;Goods 17:0-rel 18:2-ARG2=Value;Asset 
nw/wsj/21/wsj_2144.parse 15 8 gold caution-v 37.9-1 Statement caution.01 1 ----- 0:1-ARGM-TMP 2:2-ARG0=Agent;Speaker 8:0-rel 9:1-ARG2=Recipient;Addressee 10:1-ARG1=Topic;Message 
nw/wsj/21/wsj_2144.parse 15 15 gold result-v 48.1.1 NF result.01 1 ----- 11:1-ARG2=Location 14:0-ARGM-MOD 15:0-rel 16:1-ARG1=Theme 
nw/wsj/21/wsj_2144.parse 15 18 gold wear-v 41.3.1 Wearing wear.01 null ----- 9:1*17:1-ARG0=Agent;Wearer 18:0-rel 19:1-ARG1=Theme;Clothing 20:3-ARGM-TMP 9:1*17:1-LINK-PRO 
nw/wsj/21/wsj_2144.parse 16 8 gold use-v 105 IN use.01 null ----- 4:1-ARG0 8:0-rel 9:1-ARG1 
nw/wsj/21/wsj_2144.parse 18 2 gold remain-v 47.1-1 State_continue remain.01 null ----- 1:1-ARG1=Theme 2:0-rel 3:1-ARG3 5:1-ARGM-LOC 
