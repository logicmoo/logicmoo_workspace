ABOUT
-----
This piece of software is a final implementation of my bachelor thesis. In my 
theasis I tried to compare different aproaches of planning. Especially two 
methods of classical planinng: forward and backward. 
Those two methods were implemented with five different ways of searching 
state space:
- DFS
- BFS
- IDDFS
- A*
- WA*, where W=5

And there are also four heuristics:
- h0 - no heuristic at all
- h_diff
- h_add 
- h_max

By combining any meaningful method, search algorithm and heuristic we got
several planners e.g. dfs-forward-h-diff.

All those planners were tested on five domains: blocks, elevators, freecell, 
gripper and hanoi.


INSTALATION
-----------
There is no installation procedure, simply copy whole directory to your
harddrive. Make sure that you have already installed SICStus Prolog.
We used version 4.0.8.

RUNNING
-------
In order to run all planners you can used scripts
./run.sh forward
./run.sh backward 
./run.sh all
Script will sequentialy run all scripts for all domain. However there is a time
limit , which is specified in common.pl. When the time limit is exceeded script 
will give up with actual domain and try the following one.
All errors are written to file error.log.

Each time that script is running new statistics are append to files in directory
output/. The average values are computed in files *.avg, which are also used for
generating graphs.

Graphs
------
There is one more script that someone can used to create nice graphs. To do that
someone need installed gnuplot. Once the script run.sh is done, someone can 
run script ./plot.sh to create graph which are stored in directory charts/.

AUTHOR
------
Robet Sasak
robert.sasak@gmail.com 

 

