# Understanding the Pengine concept

Maybe we should try to tell a story that most Prolog programmers can
understand? This is how I believe you can think about it in very
simple terms: Suppose you are sitting in front of two computers A and
B, both running a terminal/shell. You can start Prolog in the shell of A by
executing swipl. You can also start Prolog in the shell of B by
executing swipl on _that_ computer. In a sense, you have 
created two Prolog engines, both of them waiting for your next commands.

Now you can ask A a query, or start some kind of other process, and
then turn to B and (say) ask another query. You are in control,
since you can choose what to do and in which Prolog - A or B - guided
by what is printed on the two screens.

Fortunately, equipped with the pengines library you don't need two
computers or two SWI-Prolog main processes to program in this way.
You can run pengine_create([name('A')]) and pengine_create([name('B')]) 
in the same shell. Better yet, it doesn't
even have to be you doing the programming! Instead, you can write a
program that based on what comes back from A and/or B sends the next
command (or whatever needs to be done) to A and/or to B.

When writing a web application, you may want to write your
"controller" in JavaScript instead. That's fine, and a perfectly good
way to use Pengines. The commands and handlers that are defined by
pengines.js give you a way to do just that, and to implement JS
widgets that send commands to a Prolog running on a server, in the
Prolog context that it has there (and that you if you own the server
can create yourself). It can be seen as just an extension of what you
can do in front of a CLI. It also means that Prolog applications
written to be run from a CLI are extremely easy to port to Pengines.
You basically only have to replace calls to write/1 with calls to
pengine_output/1 and calls to read/1 with calls to pengine_input/2.
For your users' experience it may of course mean a lot since you can
choose to respond with JSON and use the JSON to influence the behaviour
of a nice GUI, rather than a behaviour in terms of what is read from
or written to a CLI.

Another way to program with Pengines is to call a goal on B if your
are on A. That's what you do with pengine_rpc/3. This should be
even easier to grasp than the rest, since you can think of it as a
kind of call/1 that calls its (possibly non-deterministic) goal not
locally but remotely, and in the Prolog context of the remote pengine
server (perhaps also in union with code (if any) that you to sent
along).

pengine_rpc/3 is defined in terms of the other (core) pengine
predicates. That is also kind of nice, I think.
