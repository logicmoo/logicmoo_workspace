# GDB setup file for debugging SWI-Prolog

# Break on fatal errors and intended traps

set breakpoint pending on
#break trap_gdb
break sysError
break fatalError
#comment this next line out?
break trap_gdb
set breakpoint pending off

set pagination off

# Pass `normal signals'

handle SIGPIPE noprint nostop pass
handle SIGUSR1 noprint nostop pass
handle SIGUSR2 noprint nostop pass
handle SIGINT print nostop pass
handle SIGTTOU pass nostop noprint
handle SIGABRT stop print nopass
handle SIGTSTP nopass stop print

set backtrace past-main on

set backtrace past-entry on
set backtrace limit unlimited

set auto-load safe-path /

define tbt
  thread apply all backtrace
end

handle SIGSEGV nostop noprint pass
catch signal SIGSEGV
commands
  bt
  handle SIGSEGV stop print nopass
	#  catch signal SIGSEGV
	#  commands bt continue
  # .. your breakpoint number here
  disable 1
  cont
end

set print thread-events on
set print thread-events off

set print inferior-events on
set print inferior-events off

# Some common ways to start the system with the test-suite loaded

define pl
  dont-repeat
  run -f kaggle_arc.pl -O -F none
end

define qpl
  dont-repeat
  run -q -f kaggle_arc.pl -O -F none
end

define test
  dont-repeat
  run -q -f kaggle_arc.pl -O -F none -g demo,halt -t 'halt(1)'
end

define boot
  dont-repeat
  run -O -o pl.prc -b ../boot/init.pl
end

# Run under the efence memory debugger

define ef
#  set environment LD_PRELOAD=libefence.so.0.0
end

# Set home, so we can run libraries from the packages.  edit to
# suit your installation

define public
#  set environment SWI_HOME_DIR=/home/jan/lib/swipl
end

run
print $_exitcode
# now using -return-child-result
# quit $_exitcode
print "type# quit $_exitcode"


