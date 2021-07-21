# Redis demos

## Using consumer groups

The files primes.pl, common.pl and  prime-node.pl   provide  a  demo for
using consumer groups to organize a dynamic cluster. To run this demo:

  - Edit common.pl to update the location of the Redis server

  - Run this to start the consumer.

        swipl primes.pl
	?- r_primes(10).

  - Now start a compute node using

        swipl prime-node.pl alice

Now you should see messages  in   the  console running `swipl primes.pl`
telling which node answered whether some number   is  prime and how much
time elapsed to produced this answer. The argument `10` is the number of
calls per second.

Now you can start playing around with  the cluster. In particular, add a
new compute node:

    swipl prime-node.pl bob

You will see that you get answers  from   both  nodes. You can both more
compute and client nodes, kill compute and   client  nodes, etc. You can
also shut down the redis server and bring   it up again. You'll see some
messages as configured by the debug/3 calls in common.pl. After a little
while your cluster should work fine again.
