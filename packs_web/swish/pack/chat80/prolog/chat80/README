/*  @(#)README	24.1 4/13/88 */

This README file is a simple description about how to run the 
programs in this directory.  Read BENCH.DOC for a better
description.

To run chat:

	| ?- compile(chat).
	.......
        | ?- demo(mini).            % Runs a small set of demo questions
        | ?- demo(main).            % Runs a larger set of demo questions
        | ?- test_chat.             % Runs the large set of demo questions
                                    %   and produces a table of statistics

or you can run chat interactively by giving the goal
        | ?- hi.                    % prompts for questions
then gives you the prompt
 
        Question:
 
Chat is now expecting a question.  E.g.
 
        Question: where is France?
 
Capital letters are not distinguished from lower case.  If Chat does not
understand a word it prompts you to re-type it (or a synonym for it).  This
word must be terminated with a period.
 
Apart from questions, the following directives are accepted by Chat:

        Question: bye.                  (exits back to the Prolog top level)

        Question: trace.                (turns on Chat's tracing mechanism)
        Question: do not trace.         (turns off the tracing mechanism)
        Question: do mini demo.         (calls demo(mini))
        Question: do main demo.         (calls demo(main))
        Question: test chat.            (calls test_chat)
