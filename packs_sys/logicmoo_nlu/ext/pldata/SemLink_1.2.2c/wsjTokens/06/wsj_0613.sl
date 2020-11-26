nw/wsj/06/wsj_0613.parse 0 29 gold sever-v 23.1 NF sever.01 2 ----- 26:1-ARG0=Agent 28:0-ARGM-MOD 29:0-rel 30:1-ARG1=Patient 
nw/wsj/06/wsj_0613.parse 1 5 gold add-v 22.1-2 Statement add.02 null ----- 0:1-ARGM-TMP 3:1*20:1-ARG0=Agent 5:0-rel 6:1-ARG1=Patient 7:1-ARG2=Co-Patient 19:1-ARGM-MNR 
nw/wsj/06/wsj_0613.parse 1 21 gold order-v 60-1 NF order.01 null ----- 3:1*20:1-ARG0=Agent 21:0-rel 22:1*28:1-ARG1=Recipient 28:2-ARG2=Topic 
nw/wsj/06/wsj_0613.parse 2 19 gold say-v 37.7-1 IN say.01 null ----- 18:1-ARG0=Agent 19:0-rel 20:1-ARG1=Topic 
nw/wsj/06/wsj_0613.parse 3 6 gold expect-v 62 IN expect.01 null ----- 1:1*7:1-ARG1=Theme 4:1-ARGM-TMP 6:0-rel 
nw/wsj/06/wsj_0613.parse 4 7 gold spark-v 27 NF spark.01 1 ----- 0:2*8:1-ARG1=Theme 6:0-ARGM-NEG 7:0-rel 9:1-ARG0=Cause 
nw/wsj/06/wsj_0613.parse 4 16 gold indicate-v 78-1 Evidence indicate.01 null ----- 0:2-ARG0=Cause;Support 15:0-ARGM-NEG 16:0-rel 17:2-ARG1=Topic;Proposition 
nw/wsj/06/wsj_0613.parse 5 5 gold link-v 22.1-2-1 Make_cognitive_connection link.01 null ----- 4:0-ARGM-MNR 5:0-rel 6:0-ARG1=Patient;Concept_1 
nw/wsj/06/wsj_0613.parse 5 35 gold differ-v 23.4 Similarity differ.02 null ----- 35:0-rel 36:1,39:0-ARG1=Theme 
nw/wsj/06/wsj_0613.parse 5 41 gold diverge-v 23.4 NF diverge.01 null ----- 41:0-rel 42:1,45:0-ARG0=Theme 
