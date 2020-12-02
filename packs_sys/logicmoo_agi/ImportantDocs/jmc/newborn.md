AI as Sport

Kasparov versus Deep Blue. Computer Chess Comes of Age. MONTY NEW-

BORN. Springer-Verlag, New York, 1996. xiv, 322 pp., illus. $29.95 or DM

49.90. ISBN 0-387-94820-1.

This review appeared in Science on 1997 June 6. The version here may be

improved from time to time.

Last month’s victory of the IBM computer Deep Blue over world chess

champion Garry Kasparov culminates a 22-year engineering eﬀort. It was

also a major sporting event.

Monty Newborn, who has been an active organizer of computer chess

tournaments and helped arrange the Kasparov-Deep Blue matches, here re-

counts the 50 years of computer chess that led to the preceding, 1996, match,

won by Kasparov.

Ideas about chess algorithms as well as advances in computer hardware

were involved. However, it is a measure of our limited understanding of the

principles of artiﬁcial intelligence (AI) that this level of play requires many

millions of times as much computing as a human chess player does. Moreover,

the ﬁxation of most computer chess work on success in tournament play has

come at scientiﬁc cost.

In 1965 the Russian mathematician Alexander Kronrod said, ”Chess is the

Drosophila of artiﬁcial intelligence.” However, computer chess has developed

much as genetics might have if the geneticists had concentrated their eﬀorts

starting in 1910 on breeding racing Drosophila. We would have some science,

but mainly we would have very fast fruit ﬂies.

Three features of human chess play are required by computer programs

when they face harder problems than chess. Two of them were used by

early chess programs but were abandoned in substituting computer power

for thought.

1) Human chess players cannot examine all moves at every position they

think about and therefore must forward prune the move tree and select the

more promising moves for exploration; early chess programs also pruned.

About 1969 forward pruning was eliminated, and computer power was relied

upon to examine all moves. It made the programs work better, because early

programs sometimes pruned good moves. Eliminating pruning was possible,

because there are only about 40 moves in a position. In the game of Go,

there are up to 361 moves in a position, so even computers must forward

prune.1

2) An early Soviet program could consider certain moves as analogous to

moves that had been found to be bad in a parallel position. It would prune

them unless something was observed that would rehabilitate them. This also

was abandoned. Present programs spend most of their time rejecting the

same moves millions of times apiece.

3) Human chess players often partition a position into subpositions, for

example the king’s side and queen’s side. We analyze the subpositions some-

what separately and then consider their interaction. Present chess programs

only consider the position as a whole, and computer power makes up for

the ineﬃciency. Computer Go programs are weak, because partitioning is

absolutely necessary for Go. We still don’t know how to make computers

partition eﬀectively. Imagine playing wide chess on an 8-by-32 board. Hu-

mans would play almost as well as on the normal board, because humans

use partitioning on all problems, but programs based on present principles

would be unable to search deeply.

Much would have been learned had these important intellectual mecha-

nisms not been rejected for tournament chess programs.

Newborn developed chess programs named Ostrich between the 1960s

and 1982, especially versions running on parallel processors. His book is an

accurate history of tournament-oriented computer chess and explains many

of the ideas present in today’s programs. Like other chess books, it includes

the scores of many of the important games. However, his conventional chess

commentary takes almost no advantage of the possibilities computers oﬀer

to determine which lines of play were actually examined and how much time

was spent on them.

Now that computers have reached world-champion level, it is time for

chess to become a Drosophila again. Champion-level play is possible with

enormously less computation than Deep Blue and its recent competitors use.

Tournaments should admit programs only with severe limits on computation.

This would concentrate attention on scientiﬁc advances. Perhaps a personal

computer manufacturer would sponsor a tournament with one second allowed

per move on a machine of a single design. Tournaments in which players use

1Newborn mentions the abandonment of forward pruning as an important advance

made by Slate and Atkin in 1969, and David Slate mentioned it in his talk at the AAAI

awards ceremony in 1997 July. However, I was informed that Deep Blue does some forward

pruning deep in the tree.

computers to check out lines of play would be man-machine collaboration

rather than just competition.

Besides AI work aimed at tournament play, particular aspects of the

game have illuminated the intellectual mechanisms involved. Barbara Liskov

[?] demonstrated that what chess books teach about how to win certain

endgames is not a program but more like a predicate comparing two positions

to see if one is an improvement on the other. Such qualitative comparisons

are an important feature of human intelligence and are needed for AI. Donald

Michie, Ivan Bratko, Alen Shapiro, David Wilkins, and others have also used

chess as a Drosophila to study intelligence. Newborn ignores this work,

because it is not oriented to tournament play.

Research support agencies have trouble with the idea of chess as a Drosophila.When I explained to a DARPA program manager in the 70s how a student’s

program that discovered mating combinations contributed to recognition of

complex patterns in general, he replied, ”Well all right, but when he publishes

his dissertation, would he kindly not acknowledge our support.” I suspect

that in big companies today, it may be easier to justify work on computer

chess as a means of getting publicity than as a research tool.

An extended commentary on making computer chess more scientiﬁc is

available at http://www-formal.stanford.edu/jmc/chess.html2.

John McCarthy

Department of Computer Science,

Stanford University,

Stanford, CA 94305-9120, USA

/@steam.stanford.edu:/u/ftp/jmc/newborn.tex: begun Fri Jun 13 17:24:28 1997, latexed August 25, 1997 at 5:57 p.m.

2http://www-formal.stanford.edu/jmc/chess.html

