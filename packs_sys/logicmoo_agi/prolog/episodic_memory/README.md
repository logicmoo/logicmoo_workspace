# Episodic Memory 


# Douglas TODOs
````

player_X1@[does]> s
s
iD(Agent,intend_todo( [ go_dir(player_X1,walk,south) ])).
iD(Agent,intend_todo( [])).
iD(Agent,timestamp(63,653)).
iD(Agent,attempts(player_X1,
                go_dir(player_X1,walk,south))).

% aXiom(go_dir(player_X1,walk,south)).

player was in kitchen but left walking south
iD(Agent,h(in,player_X1,garden)).
iD(Agent,h(in,player_X1,garden)).
player came walking north in garden
( Success: walk south )
player_X1@[does]> look
look
iD(Agent,intend_todo( [ look(player_X1) ])).
iD(Agent,intend_todo( [])).
iD(Agent,timestamp(64,656.6)).
iD(Agent,attempts(player_X1,
                look(player_X1))).

% aXiom(look(player_X1)).

player does examine see in garden

% aXiom(examine(player_X1, see, in,
%          garden)).

player does sub examine see in garden 3

% aXiom(sub__examine(player_X1,see,in,garden,3)).

(...verbose...: player sees the garden "this is the garden", is large , thus, has an interior and can have exits. )
iD(Agent,h(in,player_X1,garden)).
iD(Agent,h(in,fountain_X1,garden)).
iD(Agent,h(in,rock_X1,garden)).
iD(Agent,h(in,rock_X2,garden)).
iD(Agent,h(in,mushroom_X1,garden)).
iD(Agent,h(in,brklamp,garden)).
Player_X1 sees in garden: fountain , rock , rock , mushroom and brklamp.
iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,fountain_X1,2) ])).
iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,fountain_X1,2),
                    sub__examine(player_X1,see,child,rock_X1,2) ])).
iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,fountain_X1,2),
                    sub__examine(player_X1,see,child,rock_X1,2),
                    sub__examine(player_X1,see,child,'rock_X2',2) ])).
iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,fountain_X1,2),
                    sub__examine(player_X1,see,child,rock_X1,2),
                    sub__examine(player_X1,see,child,'rock_X2',2),
                    sub__examine(player_X1,see,child,mushroom_X1,2) ])).
iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,fountain_X1,2),
                    sub__examine(player_X1,see,child,rock_X1,2),
                    sub__examine(player_X1,see,child,'rock_X2',2),
                    sub__examine(player_X1,see,child,mushroom_X1,2),
                    sub__examine(player_X1,see,child,brklamp,2) ])).
Exits in garden are: north.


% player_X1 @ somewhere: already about todo: sub__examine(player_X1,see,child,fountain_X1,2)

iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,rock_X1,2),
                    sub__examine(player_X1,see,child,'rock_X2',2),
                    sub__examine(player_X1,see,child,mushroom_X1,2),
                    sub__examine(player_X1,see,child,brklamp,2) ])).
iD(Agent,timestamp(65,656.7)).
iD(Agent,attempts(player_X1,
                sub__examine(player_X1,see,child,fountain_X1,2))).

% aXiom(sub__examine(player_X1,see,child,fountain_X1,2)).

(...verbose...: player sees the fountain can have exits , opened , thus, has an interior and has a surface. )
(...verbose...: nothing in fountain )
(...verbose...: nothing on fountain )

% player_X1 @ somewhere: already about todo: sub__examine(player_X1,see,child,rock_X1,2)

iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,'rock_X2',2),
                    sub__examine(player_X1,see,child,mushroom_X1,2),
                    sub__examine(player_X1,see,child,brklamp,2) ])).
iD(Agent,timestamp(66,656.7)).
iD(Agent,attempts(player_X1,
                sub__examine(player_X1,see,child,rock_X1,2))).

% aXiom(sub__examine(player_X1,see,child,rock_X1,2)).



% player_X1 @ somewhere: already about todo: sub__examine(player_X1,see,child,rock_X2,2)

iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,mushroom_X1,2),
                    sub__examine(player_X1,see,child,brklamp,2) ])).
iD(Agent,timestamp(67,656.8)).
iD(Agent,attempts(player_X1,
                sub__examine(player_X1,see,child,'rock_X2',2))).

% aXiom(sub__examine(player_X1,see,child,'rock_X2',2)).



% player_X1 @ somewhere: already about todo: sub__examine(player_X1,see,child,mushroom_X1,2)

iD(Agent,intend_todo( [ sub__examine(player_X1,see,child,brklamp,2) ])).
iD(Agent,timestamp(68,656.8)).
iD(Agent,attempts(player_X1,
                sub__examine(player_X1,see,child,mushroom_X1,2))).

% aXiom(sub__examine(player_X1,see,child,mushroom_X1,2)).



% player_X1 @ somewhere: already about todo: sub__examine(player_X1,see,child,brklamp,2)

iD(Agent,intend_todo( [])).
iD(Agent,timestamp(69,656.9)).
iD(Agent,attempts(player_X1,
                sub__examine(player_X1,see,child,brklamp,2))).

% aXiom(sub__examine(player_X1,see,child,brklamp,2)).

(...verbose...: player sees the brklamp inherits shiny! and is glowing. )
player_X1@[does]>

````
