;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; IPRel: interpersonal relations
;

fluent FriendOf(agent,agent)

fluent NeutralOf(agent,agent)

fluent EnemyOf(agent,agent)

event BecomeFriends(agent,agent)
event BecomeNeutral(agent,agent)
event BecomeEnemies(agent,agent)

[agent1,agent2,time]
HoldsAt(FriendOf(agent1,agent2),time) ->
!Holds(EnemyOf(agent1,agent2),time).

[agent1,agent2,time]
HoldsAt(NeutralOf(agent1,agent2),time) ->
!Holds(EnemyOf(agent1,agent2),time).

[agent1,agent2,time]
HoldsAt(FriendOf(agent1,agent2),time) ->
HoldsAt(FriendOf(agent2,agent1),time).

[agent1,agent2,time]
HoldsAt(NeutralOf(agent1,agent2),time) ->
HoldsAt(NeutralOf(agent2,agent1),time).

[agent1,agent2,time]
HoldsAt(EnemyOf(agent1,agent2),time) ->
HoldsAt(EnemyOf(agent2,agent1),time).

[agent1,agent2,time]
Initiates(BecomeFriends(agent1,agent2),FriendOf(agent1,agent2),time).

[agent1,agent2,time]
Initiates(BecomeFriends(agent1,agent2),FriendOf(agent2,agent1),time).

[agent1,agent2,time]
Terminates(BecomeFriends(agent1,agent2),NeutralOf(agent1,agent2),time).

[agent1,agent2,time]
Terminates(BecomeFriends(agent1,agent2),NeutralOf(agent2,agent1),time).

[agent1,agent2,time]
Terminates(BecomeFriends(agent1,agent2),EnemyOf(agent1,agent2),time).

[agent1,agent2,time]
Terminates(BecomeFriends(agent1,agent2),EnemyOf(agent2,agent1),time).

[agent1,agent2,time]
Initiates(BecomeEnemies(agent1,agent2),EnemyOf(agent1,agent2),time).

[agent1,agent2,time]
Initiates(BecomeEnemies(agent1,agent2),EnemyOf(agent2,agent1),time).

[agent1,agent2,time]
Terminates(BecomeEnemies(agent1,agent2),NeutralOf(agent1,agent2),time).

[agent1,agent2,time]
Terminates(BecomeEnemies(agent1,agent2),NeutralOf(agent2,agent1),time).

[agent1,agent2,time]
Terminates(BecomeEnemies(agent1,agent2),FriendOf(agent1,agent2),time).

[agent1,agent2,time]
Terminates(BecomeEnemies(agent1,agent2),FriendOf(agent2,agent1),time).

[agent1,agent2,time]
Initiates(BecomeNeutral(agent1,agent2),NeutralOf(agent1,agent2),time).

[agent1,agent2,time]
Initiates(BecomeNeutral(agent1,agent2),NeutralOf(agent2,agent1),time).

[agent1,agent2,time]
Terminates(BecomeNeutral(agent1,agent2),FriendOf(agent1,agent2),time).

[agent1,agent2,time]
Terminates(BecomeNeutral(agent1,agent2),FriendOf(agent2,agent1),time).

[agent1,agent2,time]
Terminates(BecomeNeutral(agent1,agent2),EnemyOf(agent1,agent2),time).

[agent1,agent2,time]
Terminates(BecomeNeutral(agent1,agent2),EnemyOf(agent2,agent1),time).

[agent1,agent2,time]
HoldsAt(FriendOf(agent1,agent2),time) ->
HoldsAt(Like(agent1,agent2),time).

[agent1,agent2,time]
HoldsAt(EnemyOf(agent1,agent2),time) ->
HoldsAt(Dislike(agent1,agent2),time).

fluent AcquaintanceOf(agent,agent)

[agent,time] HoldsAt(AcquaintanceOf(agent,agent),time).

[agent1,agent2,time]
HoldsAt(AcquaintanceOf(agent1,agent2),time) ->
HoldsAt(AcquaintanceOf(agent2,agent1),time).

event Introduce(agent,agent)

[agent1,agent2,time]
Initiates(Introduce(agent1,agent2),
          AcquaintanceOf(agent1,agent2),
          time).

[agent1,agent2,time]
Initiates(Introduce(agent1,agent2),
          AcquaintanceOf(agent2,agent1),
          time).

event IntroduceMutual(agent,agent,agent)

[agent1,agent2,agent3,time]
Initiates(IntroduceMutual(agent1,agent2,agent3),
          AcquaintanceOf(agent2,agent3),
          time).

[agent1,agent2,agent3,time]
Initiates(IntroduceMutual(agent1,agent2,agent3),
          AcquaintanceOf(agent3,agent2),
          time).

[agent1,agent2,agent3,time]
Happens(IntroduceMutual(agent1,agent2,agent3),time) ->
HoldsAt(AcquaintanceOf(agent1,agent2),time) &
HoldsAt(AcquaintanceOf(agent1,agent3),time).

; End of file.
