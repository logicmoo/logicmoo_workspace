# sokoban
Prolog sokoban engine with python frontend.
The prolog code is based on https://github.com/jgsogo/sokoban

# Sokoban board generator

We are using

https://gitlab.com/awarelab/gym-sokoban

This should be installed separately. It has dependency on scipy and it may be worthy to replace the original scipy
with scipy==1.1.0.

The encoding of the board is the following

    wall = 0
    empty = 1
    target = 2
    box_target = 3
    box = 4
    player = 5
    player_target = 6
