BEGIN {FS="\t";}
/^#/ { next; }
// {
  MYFILE=$1;
  MYGOAL=$2;
  MYOUTFILE=$3;
  MYCMDLINE="./test.sh tests/" MYFILE " \"" MYGOAL "\" tests/" MYOUTFILE;
  system(MYCMDLINE);
}
