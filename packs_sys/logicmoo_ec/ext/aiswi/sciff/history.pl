:-module(history,[hap/2,history_is_empty/1]).


%%%% ../myprj/trace.txt %%%%
hap(start,0).
hap(sono(2,2,blu,fiore),0).
hap(sono(2,3,blua,tux),2).
hap(sono(3,1,blub,fiore),3).
hap(sono(10,10,bluc,teschio),6).
history_is_empty(no).
