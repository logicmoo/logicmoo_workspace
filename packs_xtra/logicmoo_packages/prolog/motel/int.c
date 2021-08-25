#include <signal.h>
#include <stdio.h>
#include "runtime.h"

int int_event();

void int_handler(sig)
     int sig;
{
  SP_event(int_event, NULL);
  SP_continue();
}

int int_event()
{
  SP_pred_ref pred;
  
  pred = SP_predicate("abort", 0, NULL);
  SP_reinstall_signal(SIGINT,int_handler);
  return SP_query(pred);
}

int int_init()
{
  SP_signal(SIGINT,int_handler);
}