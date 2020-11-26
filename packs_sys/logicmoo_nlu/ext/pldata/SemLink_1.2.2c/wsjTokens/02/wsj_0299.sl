nw/wsj/02/wsj_0299.parse 0 6 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 6:0-rel 7:1-ARG1=Topic 
nw/wsj/02/wsj_0299.parse 0 9 gold issue-v 13.3 NF issue.01 null ----- 8:1-ARG0=Agent 9:0-rel 10:3-ARG1=Theme 
nw/wsj/02/wsj_0299.parse 1 3 gold price-v 54.4 NF price.01 null ----- 0:1*4:1-ARG1=Theme 3:0-rel 5:1-ARGM-MNR 11:1-ARG2=Value 14:2-ARGM-PRP 
nw/wsj/02/wsj_0299.parse 2 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/02/wsj_0299.parse 2 7 gold expect-v 62 IN expect.01 null ----- 4:1*8:2-ARG1=Theme 7:0-rel 
nw/wsj/02/wsj_0299.parse 2 11 gold rate-v 54.4 NF rate.01 1 ----- 4:1*8:1*12:1-ARG1=Theme 11:0-rel 13:2-ARG2=Value 17:1-ARG0=Agent 
nw/wsj/02/wsj_0299.parse 3 11 gold begin-v 55.1-1 Process_start begin.01 null ----- 9:1-ARG1=Theme;Event 11:0-rel 12:1-ARGM-TMP 
nw/wsj/02/wsj_0299.parse 6 9 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1-ARGM-ADV 6:1-ARG0=Agent;Seller 8:0-ARGM-MOD 9:0-rel 10:2-ARG1=Theme;Goods 18:1-ARG2=Recipient 
nw/wsj/02/wsj_0299.parse 6 29 gold transfer-v 13.2-2 NF transfer.01 1 ----- 19:1*25:1*26:1-ARG0=Agent 27:0-ARGM-MOD 28:1-ARGM-TMP 29:0-rel 30:1-ARG1=Theme 31:1-ARG2=Recipient 19:1*25:1-LINK-SLC 
nw/wsj/02/wsj_0299.parse 7 3 gold issue-v 13.3 NF issue.01 null ----- 0:1-ARG0=Agent 2:0-ARGM-MOD 3:0-rel 4:1-ARG1=Theme 
nw/wsj/02/wsj_0299.parse 8 4 gold provide-v 13.4.1-2 Supply provide.01 null ----- 0:1*5:1-ARG1=Theme 2:0-ARGM-MOD 4:0-rel 6:1-ARG0=Agent 27:1-ARG2=Recipient 
nw/wsj/02/wsj_0299.parse 9 3 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 0:1-ARG0 2:0-ARGM-MOD 3:0-rel 4:2-ARG1 
