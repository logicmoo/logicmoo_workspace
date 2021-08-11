set breakpoint pending on
break trap_gdb
break sysError
break PL_clear_foreign_exception
break __asan_report_error
set breakpoint pending off

handle SIGPIPE noprint nostop pass
handle SIGUSR2 noprint nostop pass

set print thread-events off

define safe-bt
  call PL_backtrace(20, 0x1)
end

define prolog-bt
  call PL_backtrace(20, 0)
end


break pl-vmi.c:3710
