nw/wsj/09/wsj_0941.parse 0 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/09/wsj_0941.parse 0 11 gold test-v 35.4 IN test.01 1 ----- 5:1-ARG0=Agent 11:0,12:0-rel 13:2-ARG1=Location 19:1-ARGM-LOC 22:1-ARGM-TMP 
nw/wsj/09/wsj_0941.parse 1 6 gold expect-v 62 IN expect.01 null ----- 0:1*3:1*4:1*7:1-ARG1=Theme 6:0-rel 0:1*3:1-LINK-SLC 
nw/wsj/09/wsj_0941.parse 1 9 gold mark-v 29.1 NF mark.01 3 ----- 0:2-ARG0=Agent 9:0-rel 10:3-ARG1=Theme 
nw/wsj/09/wsj_0941.parse 1 18 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 16:1-ARG0=Agent;Seller 18:0-rel 19:1-ARG1=Theme;Goods 
nw/wsj/09/wsj_0941.parse 1 25 gold mark-v 29.1 NF mark.01 3 ----- 0:2-ARG0=Agent 25:0-rel 26:3-ARG1=Theme 
nw/wsj/09/wsj_0941.parse 1 41 gold generate-v 27 IN generate.01 1 ----- 31:1*39:1*40:1-ARG0 41:0-rel 42:3-ARG1=Theme 49:1-ARGM-TMP 31:1*39:1-LINK-SLC 
nw/wsj/09/wsj_0941.parse 2 2 gold hope-v 32.2-1 Desiring hope.01 1 ----- 0:1*3:1-ARG0=Pivot 2:0-rel 3:2-ARG1=Theme 
nw/wsj/09/wsj_0941.parse 2 12 gold distribute-v 13.2-1 Dispersal distribute.01 1 ----- 6:2-ARG0=Agent 12:0-rel 13:1-ARG1=Theme 15:1-ARGM-LOC 
nw/wsj/09/wsj_0941.parse 3 2 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1*3:1-ARG1=Theme;Goods 2:0-rel 4:1-ARG0=Agent;Seller 0:1*3:1-LINK-PSV 
nw/wsj/09/wsj_0941.parse 3 22 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 20:1*23:1-ARG1=Theme;Goods 22:0-rel 24:1-ARG0=Agent;Seller 20:1*23:1-LINK-PSV 
nw/wsj/09/wsj_0941.parse 3 33 gold distribute-v 13.2-1 Dispersal distribute.01 null ----- 0:3-ARG1=Theme 33:0-rel 34:1-ARG2=Recipient 37:1-ARGM-LOC 
nw/wsj/09/wsj_0941.parse 5 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/09/wsj_0941.parse 5 8 gold call-v 29.3 IN call.01 null ----- 4:1*9:1-ARG1=Theme 6:0-ARGM-MOD 8:0-rel 10:2-ARG2=Result 4:1*9:1-LINK-PRO 
nw/wsj/09/wsj_0941.parse 5 18 gold come-v 48.1.1 NF come.03 null ----- 4:1-ARG1=Theme 17:0-ARGM-MOD 18:0-rel 19:1-ARG2 
nw/wsj/09/wsj_0941.parse 5 26 gold use-v 105 IN use.01 null ----- 25:1*27:1-ARG1 26:0-rel 28:1-ARGM-LOC 25:1*27:1-LINK-PSV 
nw/wsj/09/wsj_0941.parse 6 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/09/wsj_0941.parse 6 6 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 4:1-ARG0=Agent;Seller 5:0-ARGM-MOD 6:0-rel 7:1*9:1-ARG1=Theme;Goods 9:2-ARGM-PRD 
nw/wsj/09/wsj_0941.parse 6 21 gold package-v 22.3-2-1 Placing package.01 1 ----- 4:1-ARG0=Agent 20:0-ARGM-MOD 21:0-rel 22:1-ARG1=Patient 23:1-ARG2=Co-Patient 
