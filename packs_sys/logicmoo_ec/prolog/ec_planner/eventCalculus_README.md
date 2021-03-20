Abductive Event Calculus Planners
This is the companion website to my paper An Abductive Event Calculus Planner. The website contains the following.  

	Version 4.2 of the planner. This is the bare, uncommented, pure but inefficient version given in the appendix of the paper. 

	Version 1.9a of the planner. This is the faster version that was used for the benchmarks described in Section 6 of the paper. It's reasonably efficient at partial-order planning, but doesn't handle hierarchical planning well. 
	Version 1.15 of the planner. This version of the planner uses an iterative deepening search strategy, and has special code for hierarchical decomposition. It's less efficient for straight partial-order planning than Version 1.9a. 
	Code for each of the examples given in the paper. The mail delivery example only works with Version 1.15 of the planner. The other two examples work with all three versions. 

	The shopping trip example
	The plant safety example
	The mail delivery example

	Code for the two benchmark examples described in Section 7 of the paper. 

	Test A
	Test B

All the code is written in LPA MacProlog 32, but should be easy to port to other Prolog systems. 

Click here to get to my home page. 

