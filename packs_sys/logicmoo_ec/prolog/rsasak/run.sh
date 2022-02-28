#!/bin/sh

# This is a script for sequential running all parameters for all domains.
# Domain are stored in directory test. Each domain directory contain file 
# name-domain.pddl where name is name of the domain and also directory
# e.g. test/blocks/blocks-domain.pddl
# Remaining *.pddl files in the directory are considered as problem definitions. 
# 
# There is a time limit for running certain planner which is specified in file 
# common.pl
# When the planner is not able to find a solution with in that time, script will
# jump to another domain.
# 
# Example of running a planner:
# sicstus -l dfs-domain-h0.pl --goal "command_line." -a test/blocks/domain-blocks.pddl test/blocks/blocks-03-0.pddl

test()
{
	for problem in test/$2/*.pddl
	do
		case $problem in
			*domain*)
			;;
			*)
				echo "$1 $problem"
				output=`sicstus -l $1.pl --goal "command_line." -a test/$2/domain-$2.pddl $problem`   2>> error.log  
				if [ "$output" = "" ]
				then
					break
				else	
					echo "$output"
					echo $output >> output/$2-$1.out
				fi
			;;
		esac
	done
}

lunch()
{
# List of problem sets
  test $1 blocks
  test $1 elevators
	test $1 gripper
  test $1 hanoi
}

case $1 in
	"all")
		for direction in backward
		do
		  lunch "$direction"-a-star-h_0
      lunch "$direction"-a-star-h_add
      lunch "$direction"-a-star-h_diff
      lunch "$direction"-a-star-h_max
      lunch "$direction"-bfs
      lunch "$direction"-dfs-h_0
      lunch "$direction"-dfs-h_add
      lunch "$direction"-dfs-h_diff
      lunch "$direction"-dfs-h_max
      lunch "$direction"-iddfs
      lunch "$direction"-wa-star-h_add
      lunch "$direction"-wa-star-h_diff
      lunch "$direction"-wa-star-h_max
		done
		;;
	"forward")
		for direction in forward 
		do
		  lunch "$direction"-a-star-h_0
      lunch "$direction"-a-star-h_add
      lunch "$direction"-a-star-h_diff
      lunch "$direction"-a-star-h_max
      lunch "$direction"-bfs
      lunch "$direction"-dfs-h_0
      lunch "$direction"-dfs-h_add
      lunch "$direction"-dfs-h_diff
      lunch "$direction"-dfs-h_max
      lunch "$direction"-iddfs
      lunch "$direction"-wa-star-h_add
      lunch "$direction"-wa-star-h_diff
      lunch "$direction"-wa-star-h_max
		done
		;;
	"backward")
		for direction in backward
		do
		  lunch "$direction"-a-star-h_0
      lunch "$direction"-a-star-h_add
      lunch "$direction"-a-star-h_diff
      lunch "$direction"-a-star-h_max
      lunch "$direction"-bfs
      lunch "$direction"-dfs-h_0
      lunch "$direction"-dfs-h_add
      lunch "$direction"-dfs-h_diff
      lunch "$direction"-dfs-h_max
      lunch "$direction"-iddfs
      lunch "$direction"-wa-star-h_add
      lunch "$direction"-wa-star-h_diff
      lunch "$direction"-wa-star-h_max
		done
		;;
	*)
	 echo "Following options are allowed:"
   echo " ./run.sh forward      Runs all forward planners."
   echo " ./run.sh backward     Runs all backward planners."
   echo " ./run.sh all          Runs forward and backward."  	
esac 
# delete all empty files
find ./output/ -maxdepth 1 -empty -exec rm {} \;

# Each time that someone run this script new statistics are append. By following
# script we create .avg file with average values. 
./average.sh
