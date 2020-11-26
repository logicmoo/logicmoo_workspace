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
; A complete story understanding program will require representations
; of common human needs \fullcite{SchankAbelson:1977}.
;
; @book{SchankAbelson:1977,
;   author = "Schank, Roger C. and Abelson, Robert P.",
;   year = "1977",
;   title = "Scripts, Plans, Goals, and Understanding: An Inquiry into Human Knowledge Structures",
;   address = "Hillsdale, NJ",
;   publisher = "Lawrence Erlbaum",
; }
;
; The PlayNeed representation deals with one type of need, the need
; to play.

; Our underlying theory of human needs consists of the following sequence:
; (1) A need is unsatisfied.
; (2) Given certain stimuli and an unsatisfied need, an intention
; to satisfy the need is activated.
; (3) The intention is acted upon.
; (4) The need is satisfied.

; agent has an unsatisfied need to play.
fluent HungryToPlay(agent)
; agent has the intention to play outside.
fluent IntentionToPlay(agent,outside)
; agent has a satisfied need to play.
fluent SatiatedFromPlay(agent)

; At any time, an agent is in one of three states with respect
; to the need to play:
xor HungryToPlay, IntentionToPlay, SatiatedFromPlay

; agent intends to play at location outside.
event IntendToPlay(agent,outside)
; agent plays at location outside.
event Play(agent,outside)

; agent acts on the intention to play outside.
fluent ActOnIntentionToPlay(agent,outside)
noninertial ActOnIntentionToPlay

; A trigger axiom activates an intention for an agent to play when
; the agent has an unsatisfied need for play, the agent likes snow,
; the agent is awake, and
; the agent is in a room that looks out onto an outside area where it
; is snowing:
[agent,room,outside,time]
HoldsAt(HungryToPlay(agent),time) &
HoldsAt(LikeSnow(agent),time) &
HoldsAt(At(agent,room),time) &
LookOutOnto(room)=outside &
HoldsAt(Awake(agent),time) &
HoldsAt(Snowing(outside),time) ->
Happens(IntendToPlay(agent,outside),time).

; A story understanding program will need a detailed representation
; of intention \fullcite{CohenLevesque:1990}.
;
; @article{CohenLevesque:1990,
;   author = "Philip R. Cohen and Hector J. Levesque",
;   year = "1990",
;   title = "Intention is choice with commitment",
;   journal = "Artificial Intelligence",
;   volume = "42",
;   pages = "213--261",
; }
;
; In our simplified representation, once an intention to
; perform $e$ is activated, it persists until it is acted
; upon. Intentions are represented by inertial fluents.
; If an intention to perform $e$ is active at time point $t$,
; the agent may or may not perform $e$ at time point $t$.
; That is, we do not know exactly when the agent will act on the
; intention.
; This is a case of nondeterminism,
; which we handle by introducing a noninertial fluent corresponding
; to each intention fluent that
; indicates whether the agent does or does not in fact act
; on an intention at a given time.
; Since each ground term of the new noninertial fluent multiplies the
; number of models by $2^{n}$ where $n$ is the number of time points,
; in practice we may constrain the truth value of the fluent
; at various time points.
; In the case of the need to play,
; HoldsAt(ActOnIntentionToPlay(agent, outside), time)
; represents that
; HoldsAt(IntentionToPlay(agent, outside), time) is acted
; upon at time.

; Effect axioms state that
; if an agent intends to play in an outside area,
; the agent will have an intention to play in the outside area
; and will no longer be in the hungry-to-play state:
[agent,outside,time]
Initiates(IntendToPlay(agent,outside),IntentionToPlay(agent,outside),time).

[agent,outside,time]
Terminates(IntendToPlay(agent,outside),HungryToPlay(agent),time).

; A trigger axiom states that if an agent has the intention
; to play in an outside area,
; the agent acts on the intention to play in the outside area, and
; the agent is at the outside area,
; the agent plays in the outside area:
[agent,outside,time]
HoldsAt(IntentionToPlay(agent,outside),time) &
HoldsAt(ActOnIntentionToPlay(agent,outside),time) &
HoldsAt(At(agent,outside),time) ->
Happens(Play(agent,outside),time).

; Effect axioms state that if an agent plays in an
; outside area, the agent will be satiated from play
; and will no longer have an intention to play in
; the outside area:
[agent,outside,time]
Initiates(Play(agent,outside),SatiatedFromPlay(agent),time).

[agent,outside,time]
Terminates(Play(agent,outside),IntentionToPlay(agent,outside),time).

; End of file.
