PARTIAL FORMALIZATIONS AND THE

LEMMINGS GAME

John McCarthy, Stanford University, jmc@cs.stanford.edu ∗

1998 Mar 2

Abstract

The computer game Lemmings can serve as a new Drosophila for AI research con-necting logical formalizations with information that is incompletely formalizable inpractice. In this article we discuss the features of the Lemmings world that make ita challenge to both experimental and theoretical AI and present some steps towardformalizing the game using situation calculus.

Preliminary versions of this paper lack important formulas and references to theliterature on reactive AI and to computer vision. The formulas present are not yetintegrated into a coherent whole. 1

- ∗Work partly supported by ARPA (ONR) grant N00014-94-1-0775 and partly done while the author was

- Meyerhoﬀ Visiting Professor at the Weizmann Institute of Science, Rehovot, Israel

- 1This document is reachable from the WWW page http://www-formal.stanford.edu/jmc/home.html.

Contents

1 Introduction

2 Description of the Lemmings Games

3 What Needs to be Formalized?

4 An Example of Lemming Play

5 Physics of the Common Sense Lemming World

6 The Problem of Formalization

7 Objects in Space in the Lemming World

7.1 Geometric Objects and Relations . . . . . . . . . . . . . . . . . . . . . .

7.2 Geography of Game1 . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

7.3 Geography of Game7 . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

8 Predicting the Future

8.1 Persistent Behaviors . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

8.2 Attachment . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

9 Programs for Playing Lemmings

9.1 Planning . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

9.2 First Experiments with Lemmings

. . . . . . . . . . . . . . . . . . . . .

9.3 Tree Search . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

10 Some Formulas for a Simple Lemmings Game

10.1 Next Steps

. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

11 Remarks

11.1 Nonmonotonic Reasoning . . . . . . . . . . . . . . . . . . . . . . . . . .

. . . . . . . . . . . . . . . . . . . . . .

11.2 Maybe projection isn’t all Logic.

11.3 Causal Space and Restricted Situations

. . . . . . . . . . . . . . . . . .

11.4 Formalizing Stratagems

. . . . . . . . . . . . . . . . . . . . . . . . . . .

11.5 Agre and Chapman . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

12 References

Introduction

Scientiﬁc progress in any ﬁeld beneﬁts from “models” 2 that allow its phenomena tobe studied with a minimum of tedious technicalities and which provide a maximum ofscientiﬁc information for the work expended. Genetics has used the fruit ﬂy Drosophilasince about 1910. The motivation for this is not to breed better fruit ﬂies but isassociated with technical facts like it being possible to store a thousand fruit ﬂies in abottle and the generation time being a few days. AI can beneﬁt from having its ownsimple “models”. Here’s what looks like a good one that will be useful for solving manyimportant problems in theoretical and experimental AI.3

The computer game Lemmings (by Psygnosis Ltd.)[Psygnosis 93] provides a frame-work for formalization of facts about concurrent processes at the level of human inter-action with the processes, i.e. when the process with which the human interacts is onlypartly known and runs too fast for the human to follow. Our examples are all takenfrom the sampler Lemmings Jr.

The paper concerns using Lemmings as a model for the logical approach to AIincluding both theoretical and experimental work, but other approaches may also ﬁndit challenging and useful. The designers of Lemmings invented games that challengehuman ingenuity but keep within a limited lemming physics framework that modelsan interesting part of the common sense informatic situation. We argue that AI needsjust that kind of challenge.

The problems presented by Lemmings are simpler than real-world problems in manyimportant respects. For example, Lemmings is deterministic. The same actions inlocally similar situations always have the same local eﬀects. Variations are introducedby limited human ability to repeat the times of actions precisely. However, this is oftenunimportant.

While much can be learned from theoretical work on formalizing the facts aboutthe Lemmings world, getting the full beneﬁt will probably require actual programs toplay the game. In principle, experimenting with programs in a domain where humanunderstanding is adequate should not be required. However, there is so much room forwishful thinking about what facts have to be taken into account that the formalizationsare unlikely to be adequate unless they guide an actual program.

2 Description of the Lemmings Games

A Lemmings game is presented as a picture on the computer screen. The picturecontains two kinds of objects besides empty space.

- 2Biologists speak of a fruitﬂy or mouse model of a biological phenomenon. Here we can speak of a

- lemmings model of scene parsing, etc.

- 3Alas, there is in 1994 good reason to write about the importance of basic research models “like a

- missionary talking to cannibals rather than with a decent terseness and restraint”, to use the words of G.

- H. Hardy [Hardy, 1937].

• solid-objects These include surfaces on which lemmings can walk, cliﬀs that canbe climbed or from which lemmings can fall, hazards like ﬂames or water thatkill lemmings that enter them, bridges built by lemmings. Objects other thanlemmings do not move, although they may be damaged by explosions.

• lemmings There are ten kinds of lemmings. Each is represented by a characteristicpicture of a man about 10 pixels high.

There is a lemming source from which lemmings are emitted until the quota for thegame is reached. There is a goal or lemming sink. The object of the game is to get aspeciﬁed fraction of the lemmings to the goal. If the player doesn’t succeed, he gets totry the game over.

All lemmings are initially emitted as walkers. Subject to certain restrictions, theplayer may designate any lemming as having one of the 9 qualities other than walker.Here are the capabilities of the diﬀerent kinds of lemming.

• walker Walkers walk either to the left or to the right at a steady pace on thehorizontal or on moderate slopes. They cannot stand still. When a walker comesto a wall it reverses. When it comes to a cliﬀ it falls oﬀ. If the fall is from toogreat a height it dies, i.e. disappears from the scene and therefore doesn’t add tothe score.

• climber Climbers behave like walkers, but when one reaches a wall it climbs it.

• ﬂoater Floaters behave like walkers but can safely fall from any height.

• bomber When a lemming is designated a bomber, there is a 5 second countdown,and then it explodes, removing itself and all material from a small circular neigh-borhood of its location. Explosions do not aﬀect other lemmings.

• blocker When designated as a blocker, a lemming stands still until its status ischanged by exploding it or otherwise changing its status. Walking lemmings arereﬂected from blockers. Its status cannot be changed by redesignating it.

• builder When designated a builder a lemming starts building a bridge diagonallyupward in the direction it was walking. The slope is 1/2. Building continues for12 steps or until the bridge hits something. Lemmings walking to a bridge in thesame direction the bridge is being built climb the bridge. A lemming coming tothe end will fall oﬀ.

• basher When a lemming comes to a wall and is designated a basher, it digs ahorizontal hole.

• miner A miner is like a basher except that the hole is diagonally downward.

• digger A digger is like a basher except that the hole is vertically down.

The above description does not fully describe the behavior of lemming, e.g. howdeep a hole has to get before it traps walkers. Properties like this are consequences ofthe lemming program, are not described in the manual and are imprecisely learned byexperience.

The game ﬁle bought from Psygnosis contains a number of opportunities to play(10 in the sampler and 120 in the original full version). We call each of them a game.The games are organized into levels of increasing diﬃculty, but we won’t use the wordlevel that way; instead we use it in its geometrical sense. Each sequence of actions bythe user and the program is called a play of the game.

3 What Needs to be Formalized?

Here are some of the phenomena of playing Lemmings that are shared by real time,real world problems and which are still not well treated in AI systems.

1. A person playing Lemmings is dealing with a process that runs too fast to be fullyobserved. No-one can follow the motion of 5 lemmings, let alone the 80 commonin some levels. Actually, two lemmings is often too much if their images overlap.2. The laws of motion of the lemming world are quantitative and are characterizedby the computer program. However, the player’s knowledge of them is qualitativeand partial.

3. When the player looks at a lemming scene, he initially extracts into sentencesonly part of the information he may eventually use to make his decisions. Otherinformation remains in the scene to be used if wanted. This illustrates the partialtruth of Herbert Simon’s slogan “The world is its own best model”. The world isits own most comprehensive model but often not its own most intelligible model.4. The player’s knowledge of a particular situation is partial. Important parts ofthe scene are often out of sight.

5. There are usually too many lemmings in the scene for them all to be individuatedin the player’s mind. He has to refer to groups of lemmings—even groups whosecomposition changes.

6. Each Lemmings game has a its own geometry, and the scene must be appropri-ately parsed into regions of tactical signiﬁcance, e.g. regions whose points areall accessible by walkers. Digging holes and placing blockers both change theeﬀective division into regions.

7. Nevertheless, successful play also requires that important features of the scene beverbalized and that plans be made. Projection of the eﬀects of carrying out plansis done in a verbal (for computers, logical) language.

8. It is possible to learn from experience and try again. Here Lemmings is easierthan the real world, because the exact initial situation is repeated when ever aplay is restarted. Moreover, Lemmings is deterministic. The same sequence ofactions will always have the same eﬀects.

People formulate in natural language facts about the Lemmings world. We canexpect to devise formal tools of equal power. Besides facts, people tell each otherlemming stratagems, both local and global. We will not try to formalize the physicalskill of making several moves accurately and in quick succession. In the actual game,vigourous use of the pause facility can avoid much of the need for practicing fastreactions.

4 An Example of Lemming Play

At one stage in Game7 in Lemmings Jr. the way I ﬁrst solved it, the situation can bedescribed as follows. See Figure [?].

All but one of the living lemmings are in two groups. Some of the lemmings ineach group may be still pouring out of a lemming source, but once out the lemmingsin each group are bounded by a pair of blockers. These lemmings will go back andforth between the blockers until the player does something about it. In this descriptionnothing is said about the individual lemmings in the two groups. However, when welater give a special role to a lemming in each group, the aggregation changes.

The remaining lemming has parachuted to the lower level and is building the bridgesneeded for all the lemmings to reach the goal. When this lemming has built the bridges,it will vanish into the goal. Then the player starts a random lemming in each groupdigging. Eventually each group will fall through to the lower level and march back andforth between barriers. One of these lemmings will be set to building a bridge overthe leftmost barrier. When this is done all the remaining lemmings will pass over thebarrier and over the bridges built earlier and on to the goal.

The beginning of this game requires more consideration of parallel processes. Theﬁrst lemming out of the left source is made into a blocker to prevent other lemmingsfrom falling into some water. The second lemming is made into a parachutist and willjump down to build bridges. The third lemming is made into a blocker immediatelyafter it passes the left lemming source after being reﬂected by the blocker. While all thisis being completed, lemmings are coming out of the rightmost source and marchingto the right. As soon as the player ﬁnishes the tasks on the left he makes the leadlemming on the right into a blocker so as to keep the lemmings marching to the rightfrom reaching something that will kill them. After the rightmost blocker has beeninstalled a left hand blocker for the right hand group is put in place to the left of theright hand lemming source. There is now plenty of time for bridge building.

In the meantime the parachutist is safely marching back and forth between twobarriers. He needs to build a bridge over the left barrier, but he cannot reach highenough on this barrier if he starts toward it. Instead he must build towards the rightbarrier starting from a point that will end a bridge at the right height. Too high andhe will go over the right barrier, which isn’t wanted. At the right height, when hereverses he can be made to build a bridge toward the left barrier starting at the top ofthe bridge he has just built. This will get him over the left barrier. After that he mustbuild a bridge over a lemming trap and another bridge up to the slope leading to thegoal.

Finally, a lemming in each group is made into a digger. As the hole it digs getsdeeper all the lemmings fall into it. Finally it breaks through the ceiling of the lowerlevel, and all the lemmings fall down and can march to goal over the bridges that havebeen built.

One more tactical fact. All the remaining lemmings from the two sources must beallowed to fall into the holes being dug before the diggers are allowed to break throughthe ceiling of the lower region. Otherwise, some lemmings will fall through the holeand down to the lower level in one fall, and this will kill them.

The four lemmings used as blockers cannot be saved, and it is convenient to blowthem up when the rest of the work has been done.

The above description is accurate for the way I did it then and indicates the kindsof formalizations that my old method required. There are better ways of solving thatgame, and they require formalizations of diﬀerent facts about the lemming world anddiﬀerent facts about the speciﬁc situations.

5 Physics of the Common Sense Lemming WorldThe real physics of the lemming world is embodied in the Psygnosis program for playingthe machine side of lemming games. Presumably there is a general lemming mover, andthe designers of particular games build structures with pictures glued onto them. Let’scall it the lemming interpreter, since it also interprets the actions of players. Theseactions consist almost entirely of designating particular lemmings as having powersselected from the list of 9 possibilities.

However, the human player doesn’t know the lemming interpreter, and it wouldn’tdo him much good if he did. What the human knows is the common sense physics of thelemming world. Our goal is to ﬁnd an epistemologically adequate [McCarthy and Hayes, 1969]way of expressing this physics. Epistemological adequacy requires that the facts thatare actually observable be expressible and that the general facts giving the eﬀects ofactions be expressed in a way that makes the observations useful.

Here are some aspects of the common sense physics of the lemming world.

1. Lemmings move to the right or left and sometimes climb. I suppose lemmingscannot stand still except for blockers and have only one possible speed. Playersuse this fact in order to time explosions. If a lemming is triggered while at placex to explode 5 seconds later at place y, triggering at x will always result in anexplosion at y.

2. Except for blockers, lemmings don’t interact with each other, e.g. they interpen-etrate rather than collide.

3. The lemming world has kinematics but apparently not dynamics. Thus the pro-gram uses velocities but not forces and accelerations. Our own projections ofplans is rarely even kinematic but merely geometric. By non-kinematic I meanthat the velocities of the lemmings are not used in projection.

4. Lemmings fall at constant velocity but are destroyed if they fall from too great aheight.

lemmings.

5. Bridges are inﬁnitely strong and don’t break under the weight of any number of6. Explosions kill only the lemming that has been told to explode. Others areunharmed even if they are within the explosion zone. Structures that seem tohave been supported by structure that is destroyed do not collapse but “hang inthe air”.

7. There seems to be no random element in lemming games except that randomnesscoming from the human player. As is usual in sports, this permits the human tolearn precise procedures from repeated plays of a game. Thus he can learn thebest place to start a bridge.

8. Lemming time is sometimes important. For example, a digger needs a certainamount of lemming time before the hole is deep enough to trap other lemmings.Before that, a walker falling into the hole will climb on out of it. On a largerscale, the total lemming time available for a game sometimes aﬀects strategy.

6 The Problem of Formalization

The Lemmings player cannot succeed only by reacting directly to the current situation.He 4 must plan ahead, often to the end of the game in which the situation will be verydiﬀerent from the situation when the plan is made. We propose to do this by formalizingthe relevant facts in logic and projecting by logical reasoning. I can’t prove there is noother way of doing the projection.

Here are some requirements for projection.

1. The ontology (set of entities over which variables range) of the Lemmings worldincludes lemmings, sets of lemmings, regions, lemming traps, bridges, events in-cluding actions, situations, ﬂuents and others. I suppose we should include in theontology the predicate and function symbols we shall need in the formalism.

2. The events that create, destroy and transform the diﬀerent kinds of object needspecial treatment.

3. There must be names for sets of lemmings determined by some properties withoutthe user having to know particular lemmings or even how many there are in theset. Naturally, sets with one element play a special role.

- 4The pronoun “he” embraces women, men and robots.

Maybe the only kind of lemming that has to be treated in sets is walker. Otherscan be treated individually, because only a few can exist at a time. What is likelyto be interesting is the set of walkers in a region. The events that happen to setsof walker as sets need to be distinguished, e.g. a set of walkers can fall into ahole. If each walker in a set falls into a hole, then the set falls into the hole. (Ifyou like old jargon, falling into a hole is an intensive property like temperature.)However, we should be able to draw conclusions about what happens to a set oflemmings without always having to reason about its members. The cardinalityof a set is important when a certain resource, e.g. the number of lemmings thatcan be designated as climbers, is limiting.

4. Natural language discussion of Lemmings suggests that we provide for ﬂuents de-noting continuous action, e.g. we should be able to say holds(walking(l, c0, R), s),meaning that lemming l is walking to the right in region c0 in situation s. We willalso want to say holds(walking2(l, c0), s), omitting the direction. The former isstopped by encountering a wall, and the latter is not stopped by encountering awall but is stopped by l becoming a digger or falling into a hole dug by a digger.5. There must be a way of dealing with parallel processes. “While I am makinggroup A of lemmings do this, group B is doing that.”

6. Counterfactuals are used in human reasoning about Lemmings, e.g. “if I had onemore bridge-builder I could . . .”.

The formalism must be elaboration tolerant. A person plays Lemmings with anincomplete knowledge of the properties of the kinds of lemmings and their interactionwith features of the scene. An adequate formalization has the property that it can beelaborated to take into account new phenomena by adding statements rather than bycompletely redoing it.

7 Objects in Space in the Lemming World

Space in the lemming world is two dimensional. Nevertheless, an epistemologicallyadequate representation of space and objects for the Lemmings world requires featuresthat go beyond what has been treated in the AI literature.

The screen in the Lemmings world is a pixel map. Like all modern computer gamesit uses colors, but the literature says Lemmings is playable on a gray scale display. Wesuppose this pixel map is accessible to a program that plays the game. To play thegame it also needs the ability to imitate moving and depressing the mouse. There arekeyboard shortcuts for some of the mouse actions, but we ignore them, because we arenot trying to formalize human speed of action.

The problem for AI is that a human player does not decide what to do operatingdirectly from the pixel map. Instead the human considers the screen as divided intoregions, some of which are chambers consisting of empty space that can contain lem-mings and possibly other objects and some of which are solid material. Some of thesolid material can be turned into space by digging. Separated regions are sometimescombined when lemmings build bridges.

Human players do not convert the picture into an internal form from which all deci-sions are made without further reference to the picture. Information from the picture isput into mental forms that are not well understood physiologically or psychologically,and decisions about what to do involve repeated reference to the picture. In particular,one often looks at the details of particular areas of the scene or puts newly relevantglobal features into the mental model. On the other hand, important facts about thepicture can be internalized well enough so that I player can get a new idea about howto win while out of sight of the game.

We claim that these aspects of the human way of handling visual information are notjust peculiarities of humans. Their main aspects are features of what we have called thecommon sense informatic situation [McCarthy, 1989]. Computer programs operatingin the real world, e.g. controlling mobile robots, also face the common sense informaticsituation. This is characterized by incomplete information about the situation itselfand incomplete information about the laws that determine the eﬀects of actions.

Therefore, we plan that a Lemmings program will parse the scene into a collectionof regions and relations among them. The initial parsing will be later extendable byfurther reference to the pixel map. Like the human the program will use the scene asits own most comprehensive model, although not always its most intelligible or usefulmodel.

The parsing therefore has two unusual aspects.

1. The parsing is not intended to be conclusive. The parser may be asked to getmore detail about something it returned previously.

2. The parser looks for common simple regions which it may have to elaborate laterrather than having a universally applicable scheme for parsing anything.

3. The parser gives names to features it ﬁnds and generates sentences characterizingthem. These sentences are later used by reasoning programs that decide what todo.

The simplest kind of region may be called a chamber. The simplest kind ofchamber has an essentially horizontal ﬂoor and walls at each end that reﬂectwalkers.5

In section 8, page 12, we will discuss actions and events using situation calculus.S0 stands for the situation when a particular game called Game0 starts. Game0 is a- 5Within regions the horizontal location of a position may be important. For example, it usually matters

- where a bridge is started within a region. I remember positions by irregularities in color or relief. Should we

- be purist and require this of programs, or should we let them use co-ordinates?

simpliﬁcation of the ﬁrst game in Lemmings Jr. There is only one lemming. See ﬁgure[?].

A simple chamber will be rectangular on the screen with walls at both ends. Wemake c0 a simple chamber in S0 and can rely on persistence to keep it a simple chamberin S2 and beyond. This would be used in planning to solve Game0. Alternatively, ifwe are actually playing Game0, we can observe that c0 is a simple chamber in S2.This requires a mechanism for associating names with features of the picture and withsituations depicted on the screen. We haven’t decided how to do this yet.

Consider chambers which conﬁne lemmings until something is done about it. Theyare bounded by simple closed curves, but that high level of generality isn’t the commonsense way of thinking about them. One could suppose that chambers in the lemmingworld are a subclass of the interiors of simple closed curves and try to formalize asuitable subclass. This is a bad idea, because we haven’t seen all the chambers Psyg-nosis Ltd. has chosen to use, and they are likely to invent new ones with each newlemming variant they market. Their artists draw the chambers and the regions thatsurround them according to the requirements of the game designers and their artistictaste. These drawings are then digitized. No human player can represent a digitizedscreen pixel by pixel, and our formalism can’t do this either.

Instead we start with simple ﬁgures and elaborate them. The simple chamber is oneof these starts. A program that reasoned about the game would observe the screen ofGame0 and recognize the upper chamber as a simple chamber and give it a name. InGame0, the ﬂoor of c0 is not entirely even, but these surface rills make no diﬀerence. Itdoesn’t matter where l0 digs its hole. In other lemming games it does make a diﬀerence.For example, it might have been necessary in Game0 to dig in a low place in order thatthe lemmings should not fall too far.

We propose to handle unevenness by allowing additional statements about chambersthat have been identiﬁed as simple chambers. Complete descriptions of surfaces are notrequired, but relevant observations may be expressed. Perhaps this is a form of beliefrevision, e.g. the player formerly believed the chamber was simple and has changed hsimind; some of the considerations advanced in the study of belief revision might thenapply. Another way of handling this idea is to say that c0 is a certain kind of simplechamber and express the unevenness in describing what kind.

We also will handle the chamber with the digger’s hole by elaborating the simplechamber description. This will be belief update rather than belief revision.

We can implement the above using a notion of chamber type, or more generally,type of lemming region. simple-chamber would be the type that is instantiated byc0. at right-end, lemming-sink, simple-chamber) is is instantiated by c1. (We are notcommitted to this particular notation for elaborating simple ﬁgures.) We need typesin the formalism and not just individual regions, because we need to make new typesby elaborating simpler ones, e.g. we make a chamber with the lemming sink at oneend by elaborating simple chambers.

7.1 Geometric Objects and Relations

Here are some useful geometric objects and relations.

1. Two dimensional objects—regions including chambers. We have already discussedthem.

used.

2. One dimensional objects—segments and walls. A segment is a surface over whichlemmings can walk. Mostly we are concerned with segments bounded by walls ordrop-oﬀs or other impediment to walkers. However, segments bounded by beingabove a hill at a lower level may need to be an object because digging in thatsegment has a diﬀerent result than digging elsewhere.

On walls useful segments are bounded by overhangs as well as ﬂoors and ceilings,because climbers can’t climb overhangs.

3. Bridges and segments of bridges require special treatment, because they are dy-namic until completed. Even when it is complete, it is sometimes necessary tobreak a bridge into two segments using a digger.

4. The adjacency relations and containment relations among geometric objects are7.2 Geography of Game1

The basic representation of a Lemmings scene is the pixel map provided by the Lem-mings program. However, additional representations are needed for the following pur-poses.

1. Prediction of what will happen. When a person does it, the person does notgenerate an new pixel map. To do that the person would need to be able tosimulate the Lemmings program, and we can’t do that. Apparently prediction isbased on a representation of scenes simple enough for a person to update.

2. Planning a strategy.

[the actual geography to be provided]

7.3 Geography of Game7

[to be provided]

8 Predicting the Future

Prediction is diﬃcult—especially of the future.

We use situation calculus.

holds(p, s) means that the proposition (i.e. propositional ﬂuent) p holds in situationvalue(exp, s) is the value of the expression exp in situation s.

F (p) is the propositional ﬂuent that proposition p will hold in the future. F isa reiﬁed temporal logic operator. We usually omit parentheses for functions of oneargument so that F (f luent(p)) would be written F f luent p.

next(p, s) is the next situation after s in which p holds. It is related to F by thes.

axiom

holds(F p, s) ⊃ holds(p, next(p, s)).

(1)The function F plays the role of a temporal logic future operator. next(p, s) denotesthe next situation after s in which p holds. We intend to say nothing about the valueof next(p, s) when F p doesn’t hold.

[An alternative is to regard next(p, s) as an individual concept as discussed in[McCarthy, 1979b]. We would then write

exists next(p, s) ⊃ holds(p, next(p, s))

as the axiom.]

We also have a conditional future function F (cid:48) p. The intent is that it predicts thatp will hold if nothing to prevent it happens. There will be an axiom of the form

holds(F (cid:48) p, s) ∧ (cid:104)nothing prevents p(cid:105) ⊃ holds(F p, s).

(2)It isn’t yet clear how to write the condition (cid:104)nothing prevents p(cid:105).

Here is one way.

What will surely occur is any p such that F (cid:48)(p, s) and such that there is no p(cid:48)occurring earlier and preventing p. Therefore, we have

F (cid:48)(p, s) ∧ (∀p(cid:48))¬(p(cid:48) (cid:54)= p ∧ F (cid:48)(p(cid:48), s) ∧ time(p(cid:48), s) < time(p, s) ∧ prevents(p(cid:48), p)) ⊃ F (p, s)(3)We can put this in a form that is less explicit about time, namely

F (cid:48)(p, s) ∧ (∀p(cid:48))¬(p(cid:48) (cid:54)= p ∧ F (cid:48)(p(cid:48), s) ∧ precedes(p(cid:48), p, s) ∧ prevents(p(cid:48), p) ⊃ F (p, s) (4)Another possibility is to consider the ﬂuents p as having associated times or at leasta temporal order. This makes them richer objects than we have previously taken themto be. The equations then become

F (cid:48)(p, s) ∧ (∀p(cid:48))¬(p(cid:48) (cid:54)= p ∧ F (cid:48)(p(cid:48), s) ∧ time(p(cid:48)) < time(p) ∧ prevents(p(cid:48), p) ⊃ F (p, s) (5)and

follows.

where

and

F (cid:48)(p, s) ∧ (∀p(cid:48))¬(p(cid:48) (cid:54)= p ∧ F (cid:48)(p(cid:48), s) ∧ precedes(p(cid:48), p) ∧ prevents(p(cid:48), p) ⊃ F (p, s).

(6)Probably the basic inference task is predicting what happens in a situation whenthere is no intervention. This is just predicting what would be observed by runningthe program, but predictions used for planning need to be done in an epistemologicallyadequate way, i.e. using only the information that would be available to a humanplayer. The result will be qualitative, sometimes uncertain and sometimes incorrect.

The basic fact about lemmings walking can be formulated in ﬁrst order logic asholds(rightist l, s)

∧ holds(at(l, p1), s)

∧ holds(on(p1, surf ace1)

∧ holds(on(p2, surf ace1)

∧ holds(to-the-right-of (p1, p2, surf ace1), s)

¬(∃x)holds(prevents-walk(x, p1, p2), s)

⊃

holds(F at(l, p2), s),

(7)holds(prevents-walk(x, p1, p2), s) ≡ prevents-walk1(x, p1, p2, surf ace1, s)

(8)(∃p(cid:48))(holds(between(p1, p(cid:48), p2, surf ace), s)

∧ holds(at(x, p(cid:48)), s)

∧ holds(stops-walkers x, s)

⊃

prevents-walk1(x, p1, p2, surf ace, s).

(9)We have made prevents-walk a term so it can be an argument of holds, whereasprevents-walk1 is a predicate so it can be circumscribed. Maybe there will be someway to get rid of this bureaucratic complication.

We would circumscribe the predicate prevents-walk1, and if there were no obstacle,we would be able to infer that lemming l would indeed reach point p2.

Additional phenomena that might prevent walking to a destination are to be treatedby adding sentences whose conclusion is prevents-walk1(x, p1, p2, surf ace, s), i.e. theconclusion of (9).

The above equations are unpleasantly long, and in more complicated cases mightget quite a bit longer. In dealing with situations that actually arise in playing a game,these equations would be paralleled by a program that would establish that lemming lwould reach p2 in the concrete situation. In subsection 8.2 we discuss how a programcan be connected to a predicate or function in such a way that when it is necessary toreason logically about the predicate applied to constant arguments the program canbe used to get the value. We call this attachment of the predicate to the program. Itis a special case of partial evaluation.

We might hope to use program in more abstract cases, e.g. thinking about hypothet-ical situations, like “if I had a blocker there . . .”. Constant symbols and constructionfunctions would be needed to construct the a representation of the hypthetical situa-tion itself and the other constants on which the program would operate. Presumablythe hypthetical situations would be objects in the sense of object-oriented languages,e.g. CLOS.

However, only equations retain elaboration tolerance, e.g. permit the introductionof new lemming properties by making an assertion.

8.1 Persistent Behaviors

Tom Costello [?] has proposed generalizing the notion of persistence. Instead of a ﬂuentpersisting, he proposes that a ﬂuxion (a further borrowing from Newton) persists. Aﬂuxion is a function of time. The following is an adaptation of his idea.

Consider a lemming walking. Consider the following formulas:

occurs(start walking, s) ⊃ holds(walking, s)

(10)and

hold.

continues(walking, s1, s2) ⊃ value(x, s2) = value(x, s1) + v(time(s2) − time(s1)),

where v is the velocity of the lemming, and we have the deﬁnition

continues(p, s1, s2) ≡ (∀s)(s1 ≤ s ≤ s2 ⊃ holds(p, s)).

walking is thus treated as an ordinary persistent ﬂuent in (10).

position of the lemming is given by the auxiliary formula (11).

Its eﬀect on theA persistent ﬂuent continues to hold until something stops it, and then it ceases to(11)(12)persistent p ∧ holds(p, s) ∧ holds(F stops p, s)

⊃

¬holds(p, next(stops p, s))

∧(∀s(cid:48))(s ≤ s(cid:48) ≤ next(stops p, s) ⊃ holds(p, s(cid:48)))

(13)persistent p ∧ holds(p, s) ∧ ¬holds(F stops p, s) ⊃ (∀s(cid:48))(s ≤ s(cid:48) ⊃ holds(p, s(cid:48))),

(14)For completeness, we should also include

but there isn’t much that lasts forever.

8.2 Attachment

A computer program doing logical inference need not compute 53 + 18 by deriving53 + 18 = 71 from the laws of arithmetic.

Instead it should take advantage of thecomputer and get this result from the computer’s arithmetic. This is accomplished invarious interactive theorem provers by attaching a program, in Lisp for instance, to thefunction name, in this case +. The following considerations apply.

1. As a proof step, the prover is given an expression to evaluate, e.g. the commandis

evaluate “53 + 17”.

(15)The result of the evaluation is a sentence, e.g. 53 + 17 = 71, which can then beused for further inference like any other logical sentence.

2. The most straightforward evaluators can only be given ground expressions toevaluate. Fancier ones can do algebraic and logical simpliﬁcation of expressionsinvolving variables. For Lemmings we contemplate only the ground case.

3. For Lemmings the most important expressions to evaluate will involve the sceneas an argument.

4. The attachments are done by the builder of the system, and therefore have thesame inferential status as the axioms.

In principle, one might use a programveriﬁcation system to show that the functions deﬁned by the program satisfy theaxioms involving the function names.

I am not aware that this has ever beendone.

5. As described in [McCarthy, 1962], attachments can be combined with reﬂexionprinciples to allow the results of decision procedures to be entered as formulas. Idon’t think Lemmings will require this.

Richard Weyhrauch [?] has the most elaborate system involving attachment.

Idon’t think Lemmings will require its complexities.

9 Programs for Playing Lemmings

How can the above facts and proposed formalisms be used in a program for playingLemmings?

9.1 Planning

The simplest kind of planning for Lemmings is to give a sequence of objectives, eachof which is to be achieved reactively on the basis of the scene visible at the timeof completion of the previous objective. Let’s see how this works out for Game7 ofLemmings Jr.. We give one such plan but note where there may be some problemswith a purely reactive system for achieving the successive objectives.

In Game7 there are two lemming sources that open approximately simultaneously,In the middle of the platform there is a pond that drownsboth above a platform.

lemmings that fall in.

It might be surmounted by a long bridge, but none of thesolutions I have attempted involve doing this. As in all Lemmings games, the lemmingswalk to the right when they drop out of the source. The lemmings must be got downfrom the platform and then marched to the left over or through various obstacles.Here’s the plan.

1. Start ﬁrst left lemming (lemming from left lemming source) digging just after ithas moved away from under source.

2. Start ﬁrst right lemming digging just after it has moved away.

3. Start any lemming that over-runs the hole being dug to digging its own hole.There won’t be more than two.

4. If a left lemming bounces left (happens rarely) make it a ﬂoater.

5. Increase rate of lemmings coming from source to maximum.

6. Make left digger a basher before it breaks through ceiling.

7. Make ﬁrst right digger a miner as soon as its hole is deep enough so that lemmingswon’t climb out of it.

8. When left basher stops at iron wall, make it a digger.

9. When left digger digs far enough to clear iron wall, make it a miner.

10. Comment: These steps will get the lemmings safely down. There are now twocases according to whether a jumper is between the two mounds on the lowerlevel or not.

If yes, go to step 12

11. Make ﬁrst lemming to approach the right tower from right into a climber.

12. Make sure the lemming going to the left between the two towers is a climber goingleft.

13. When the climber has passed the left tower, make it a builder at a place whereit will build over the lemming trap in one round of building. The right placeis determined by experience. If the most obvious place is chosen, the monsterreaches out of the trap. If experience is to be used, then there must be a memoryof places from play to play of the game. Otherwise, the place must somehow bedesignated by the planner to the reactive player. When I am playing the game,I remember landmarks like bushes, but it would be easy to devise a co-ordinatesystem and give the x-co-ordinate. This might be considered contrary to the ideamaking the program solve the problem that humans solve, using very limitedcapabilities of numerical estimation.

14. When the builder has passed the lemming trap and become a walker again, makeit a builder again when it has reached the point where a two section bridge willreach the slope leading to the goal with the lemming continuing to the goal.Again experience is involved to learn the right place. Again measurement mightreplace experience, since a Lemmings bridge rises on pixel for every two pixel itgoes horizontally.

15. When the builder has reached the end of the ﬁrst stage of bridge building, makeit a builder again. (A two stage bridge is needed and suﬃces.)

16. When the builder has reached the slope, designate a lemming on the right as abasher, just as it reaches the right tower going left.

17. When the basher has broken through and become a walker, designate it a basheragain when it reaches the left tower.

18. This is all that needs to be done. All the lemmings are saved.

We believe that each of the steps mentioned above can be acomplished by reactiveprogramming except that step 12 involves remembering whether it is necessary todesignate the lemming between the towers a climber. If it was the jumper, then it isnecessary, but if it just climbed it is unnecessary. The conditional element could beavoided in this case, because it does no harm to designate a lemming a climber thatis already a climber. However, it is probably a good idea to allow reactive programssome reference to memory.

In this example, it seems that all the reactive execution is done subordinate tosingle steps generated by the plan. It is not clear whether this is always possible inLemmings. Moreover, it makes the plan rather rigid. There is no provision for goingback to the planning level if the reactive level encounters an unexpected diﬃculty.

Consider the communication between the logical planner and the reactive executorof steps. The following considerations apply.

1. Approximately as proposed in [McCarthy, 1959], when the planner infers a sen-tence of the form

should-achieve p

(16)as a side-eﬀect, the reactive achiever is called with p as argument. When the goalis achieved, the observation program takes another look at the scene and modiﬁesthe logical database accordingly. Inference then resumes.

2. Any information needed to achieve a goal that cannot be obtained by the achieverdirectly from the scene must be provided to the goal-achiever in advance.

3. This scheme provides for no other communication between the logic part of thesystem and the reactive part and is therefore less sophisticated than many ex-isting systems that plan and execute. At least conceptually, however, it seemsappropriate to begin with this primitive interaction and then study what morethis particular fruit ﬂy may require.

9.2 First Experiments with Lemmings

Perhaps I am too unambitious, but Lemmings strikes me as presenting considerableexperimental diﬃculties, and I advocate starting slowly. Here are some steps.

1. A tool is needed for programs that interact with the Lemmings program. It shouldbegin with interaction with the pixel maps displayed by the program. Programslike Planet X bring the pixel map of a Macintosh into an X-window, and ananalogous program could bring it into an X-window reachable by a CommonLisp or C++ program. Alternatively, the Lemmings pixel map might be accesseddirectly by programs in the Macintosh or in a PC.

2. Another early step is the parsing of the pixel map into regions. The result of sucha parsing is a tree of regions and subregions. Very likely the parsings will haveto be extendable when the program decides that it is necessary to go back to thepixel map for more information.

3. The ﬁrst programs to play the game should concentrate on the easier games.Many of these can be solved greedily, i.e. by reacting to the scene with actionsthat move the situation closer to the goal.

4. The next step is tree search as discussed in the next subsection.

9.3 Tree Search

Perhaps Lemmings games can be solved by tree search. If so, it will be a very sophis-ticated tree search.

First consider a brute force tree search. The actions available to a player can bedescribed as clicking the mouse on a point on the computer screen or doing nothing.An action must be taken at an interval which is less than one second but probablynot less than 0.1 seconds. Some parts of the screen are unresponsive, so we can regarddoing nothing as clicking on such a place. The screen on a Macintosh PowerBook is640 by 480 pixels. Thus the player has 307200 choices every 0.1 seconds. Even if theplayer could fully observe the consequences of each choice, the branching would maketree search of the full tree impractical.

What must we do to make tree search practical? We ﬁrst consider what reducedtrees to use.

1. First we merge clicking on a quality and clicking on a lemming. This eliminatesclicking on two qualities in succession which is pointless, because it is equivalentto clicking on just the second quality.

2. Now we choose a delay. The most common delay is 0. Most things you wantto do are done right away. We want to avoid branching on times, so we choosetimes by events. Thus a clicking on a lemming x the next time lemming y is in aparticular position is a choice. Naturally, the case when x is the same lemmingas y is distinguished. We still have too much branching, and we must avoid evengenerating far-oﬀ events, at least until earlier parts of the tree have been searched.3. Now we choose the lemming that is to receive the quality. For this search to beeﬃcient the lemmings must be grouped into equivalence classes, so that choosingonly one lemming in the class is considered. Indeed when the lemmings are close-packed, it isn’t possible to tell one walker from another.

4. A common type of event is a lemming, usually the one we intend to modify,reaching a certain place. If the lemmings move a pixel at a time (I’m not sure ofthat.), then there may be several hundred places where we could choose to click.Many consecutive choices of place are likely to be equivalent. In order to reducethe choices the horizontal or vertical line along which the lemming is moving hasto be divided into heuristically diﬀerent regions.

Here is where a real diﬀerence with previous tree search algorithms must arise.We need to make the division into regions tentative and reﬁne it when this isrequired.

5. In one game a blocker is needed, but 100 percent of the lemmings are to be saved,and therefore having to blow up the blocker will result in a loss. The tree search,if that’s what it is, is abstract, because the node in which there is a leftoverblocker is at the end of the game. The solution is that the very ﬁrst lemming outof the source is made into a ﬂoater (parachute jumper) and then into a blocker;the very last lemming out of the source is made into a ﬂoater and then into aminer just when it can undermine the blocker. Both then parachute to safety.

If we regard this reasoning as tree search, then we consider making the ﬁrstlemming a blocker as one edge, solving the rest of the game as another edge, seethis as a loss, backtrack and put in making the lemming into a ﬂoater, etc.

6. In building a bridge or digging a hole, the precise place to start is sometimesimportant.

The tree search algorithm itself has to decide in what order to examine the nodes.This clearly requires many heuristics, most speciﬁcally considering ﬁrst moves andsequences of moves that are recommended by some idea. This is where logic will surelycreep back in.

It is not obvious that all the above notions can be applied in a way that will reducethe tree search to managable proportions. My intuition is that they can—at least forlarge parts of many games.

Tree search often becomes ineﬃcient when the sequence of actions is divisible intoactions that independently aﬀect diﬀerent aspects of the situation. Then it usuallybecomes more eﬃcient to break the problem into parts that can be thought aboutseparately, solve the separate problems and then combine the results. I think Lemmingshas enough of this to make a pure tree search algorithm impractical.

(This phenomenon is what makes pure tree search algorithms impractical for Go.It is necessary to consider the diﬀerent areas of the board separately and then combinethe results.)

Lemmings tree searches can include retries of games. For example, when the placea blocker is exploded turns out to be too thin, the program should take another lookat the pixel map and ﬁnd a thicker place to explode the blocker.

10 Some Formulas for a Simple Lemmings GameGame0 is a simpliﬁcation of the ﬁrst game of Lemmings Jr. There is only one lemming.We give a sequence of formulas narrating the journey of a lemming l0 from thelemming source to the lemming sink. It applies to the journey of the lemming chosento dig even when other lemmings are present. For each formula we say where we expectit to come from.

holds(F open source, S0)

(17)As is conventional in situation calculus formalizations, S0 is the initial situation. Theformula states that the lemming source will open. This is inferred from a general factabout the start of lemming games. Some lemming games have more than one source.However, we may take it as an observation of the screen that there is one lemmingsource in this game.

Let S1 = next(open source, S0). We now have

holds(F f alls l0, S1).

This follows from (8). It is a general fact about Lemmings that a lemming will fall outif not all have fallen out. Calling one of them l0 is allowable, but we’ll do this moreprecisely in a later version of these formulas.

Let

S2 = next(f alls l0, S1).

(18)This is the next situation in which l0 is falling.

holds(simple-chamber c0, S0)

A simple chamber will be rectangular on the screen with walls at both ends. We makec0 a simple chamber in S0 and can rely on persistence to keep it a simple chamberin S2 and beyond. This would be used in planning to solve Game0. Alternatively,if we are actually playing Game0, we can observe that c0 is a simple chamber in S2.This requires a mechanism for associating names with features of the picture and withsituations depicted on the screen.

Next we want

Let p0 = value(below source, c0), S0).

This is the point on c0 immediately below the source. I suppose we should assert(and later infer) it exists before we name it.

holds(above(source, f loor c0), S0).

(∃p)(holds(above(source, p), S0) ∧ holds(on(p, f loor c0), S0).

p0 = value(below(source, c0), S0).

Now we can have

We are now ready for

and consequently

with

Now that l0 is down on the ﬂoor of c0, it will walk around until the player turnsit into a digger. The details of that, while part of the “biography” of l0, do not comeinto postulating

because this refers to an action of the player in designating l0 to be a digger. From itwe get

holds(F at(l0, p0), S2)

holds(at(l0, p0), S3)

S3 = next(at(l0, p0), S2).

holds(F digger l0, S3),

S4 = next(digger l0, S3).

The fact that l0 is still in c0 does come into inferring from

p1 = value(location(l0), S4)

holds(on(p1, f loor c0), S4).

Now we need more geography, including the lower chamber c1. We need to say thatc1 is a simple chamber modiﬁed by putting the lemming sink at its right end. We alsomust say that every point of the ﬂoor of c0 is above the ceiling of c1.

I am not satisﬁed with the suggestions given above for describing chamber typesas modiﬁcations of basic types, so we will omit this for now. On the other hand, it isstraightforward to say that the ﬂoor of co is above the ceiling of c0. We have

(∀sp)(holds(on(p, f loor c0), s) ⊃ (∃p(cid:48))(holds(on(p(cid:48), ceiling c1), s)∧holds(above(p, p(cid:48)), s)))and from it we get

p2 = value(below(p1, ceiling c1), S4)

We have used holds(diggable(p1, c1), S0).

Similarly we want to infer that l0 will fall to a point p3 on f loor c1, giving

S5 = next(at(l0, p2), S4).

S6 = next(at(l0, p3), S5).

holds(walker l0, S6)

F (at(l0, lemming-sink), S6)

Now we have two cases according to whether holds(rightist l0, S6) or holds(lef tist l0, S6).In either case we can show

I used holds(p, s) instead of just p(s), because we might want to quantify over p.However, we haven’t used this in the formulas given so far.

There are many more non-trivial matters that require careful consideration.

that

and

.

10.1 Next Steps

Game0 is simple enough so that it is almost a conventional situation calculus problem.Even Game1 requires more.

1. Game1, the ﬁrst game of Lemmings Jr. has 10 lemmings. We must introduce anobject for the group of them, and then separate out the lemming designated tobe the digger.

2. When the hole is shallow, lemmings pass over it. When it gets deep the fallin. They continue to alternate directions while in the hole, and the direction alemming has when it falls out of the hole determines which way it will walk onthe ﬂoor of c1.

3. Even in Game0, the sentences about the future are derived by nonmonotonicreasoning. We haven’t said how this is done. The simplest aspect of this is thatthe geometric relations of the chambers tend to persist, but the positions of thelemmings don’t.

4. Continued events like digging or falling need both macroscopic and microscopicformalizations. At a macroscopic level, a lemming falls to the ﬂoor or digs to theceiling of the lower chamber or walks to the end of the chamber. At a microscopiclevel the falling or digging is continuous and can be interrupted or can cause otherevents part way.

5. The geography involves describing the scene to the extent needed as as collectionof related instances of spatial types. The formalization must tolerate elabora-tions like the change in the ﬂoor of a chamber caused by a basher (horizontaldigger) plowing it up. When an elaboration is not required, it shouldn’t aﬀectthe statements that are made or the reasoning done with them.

6. Concurrent action needs to be describable in whatever level of detail is needed todraw the required conclusions.

7. What a person learns and can communicate to another person is often that in acertain kind of situation certain changes can be achieved. Therefore, to be able togive advice to a Lemmings program will require a formalization of can. Somethingis known about this problem, less about how to formalize how something that canbe done is to be accomplished.

11 Remarks

Some of the ideas in this section may have to be abandoned. Some of the ideas discussedinformally in the present draft may be included in the more formal sections of the paper.11.1 Nonmonotonic Reasoning

Lemmings may present new problems for nonmonotonic reasoning.

1. It is a general feature of nonmonotonic reasoning to implicitly infer that thefacts being taken into account are all that are relevant to the phenomenon beingreasoned about. However, this has not been put into the formalism explicitly.Making it explicit may be important for reasoning about Lemmings. A Lemmingsplayer learns to win a particular game by repeated trying. He uses the fact thatthe same action in the same situation always has the same result. Formally this isassumption is incorporated in the use of a function result to get a new situations(cid:48) = result(e, s), i.e. the new situation is determined by the old situation and theevent (action) that occurs in the old situation.

However, a player uses more than the functionality of the result of an event,because he wants to learn not merely from exact repetitions of the situationbut from repetitions of those aspects of the situation that are relevant to thephenomenon he is learning about.

Therefore, we need to be able to express as a circumscription jumping to theconclusion that we have repeated the relevant aspects of a previously examinedsituation. In Lemmings, this is related to locality, i.e. it doesn’t matter what thelemmings elsewhere in the scene are doing.

2. Something like the philosophers’ paradox of the heap comes up in Lemmings,and a solution suggests itself that may have more general application. Supposea lemming is removed from a bunch of lemmings. We draw the nonmonotonicconclusion that we still have a bunch.

If you repeatedly draw this conclusionenough times, one of the conclusions will be false, because you will have eliminatedthe whole bunch. So what? The reasoning was nonmonotonic and admittedlyrisky.

There is a related fact about nonmonotonic reasoning. Suppose for a variable xyou infer nonmonotically p(x). Your nonmonotonic logic should be such that thisdoes not allow you to infer (∀x)p(x), i.e. universal generalization is not allowed.11.2 Maybe projection isn’t all Logic.

In AI projection can be done by running the progam, provided the complete stateof the system is available, and the program describing the evolution of the system isavailable.

Suppose the neither a complete description of the situation nor a complete descrip-tion of the process is available. Situation calculus ([McCarthy and Hayes, 1969]) wasinvented to deal with this by allowing logical inference from facts about a situation forwhich we do not have a complete description.

However, it is possible that humans do some projection by non-logical means, i.e.by running some mental process on a representation of the situation. If so, such mentalsimulation must use a greatly schematized representation of situations. The idea is tobe able to answer a question, “What will happen if . . . ?” by mental simulation. Aprogram could also use some kind of schematized simulation. Its output seems to bepropositional. Subjectively, such simulations usually seem to be fragmentary.

11.3 Causal Space and Restricted Situations

Suppose a miner has been started digging in an upper level. If there is nothing thatblocks digging, it will break through and fall to the next level down provided nothinghappens to interrupt this. I want to use a formalism like that proposed in the draft[McCarthy 94]

The most straightforward way of saying that the miner will eventually fall throughis to say he will if no events occur. This is much too strong a condition. Here’s anidea.

1. Introduce a concept of causal space. In many of the approximate theories we willwant to use, causal space will not correspond to real space.

2. Causal space has points. Events occur at points. Points persist from situation tosituation. Thus we can talk about the same point in related situations.

3. Distance in causal space is deﬁned by the time of propagation of eﬀects of events.Events cannot aﬀect ﬂuents associated with far away points in a short time.

4. Regions are sets of causal points, normally they will be built up from somethingif a point is in the “middle” of a region,like open sets in causal space, i.e.

suﬃciently nearby points will also be in the region.

5. The fewer points there are in causal space close to a given point, the more con-veniently local computations can be done. We will want to work with spaceswithout very unlikely causal connections. For example, President Clinton couldconceivably wake up at 5am with a sudden desire to telephone John McCarthy,but it is better to ignore the possibility and put him at a much larger causaldistance than this possibility allows.

This suggests regarding causal space as a metric space. Indeed it may be a metricspace, but we want to reserve judgment on what if any topology we shall want touse.

11.4 Formalizing Stratagems

It is stated in the Lemmings manual, and I observed it many times, that when a bridgebuilder bumps its head on a ceiling, it reverses course. An ordinary walker keepsgoing. To me this was an annoyance.

It meant that to be able to reﬂect walkers,it was necessary to start bridge building several times. Recently, I found a use forthe phenomenon, doubtless a use intended by the game designers.

If we catch theex-builder just after it has reversed, it will build a bridge in the opposite direction andwill not be followed by other lemmings. This sometimes permits building a ladder ofbridges without other lemmings falling oﬀ the partial bridges. After the builder hasreached the destination the gaps can be closed, and the mob will follow.

I thought of the stratagem while away from the game.

The possibility of the stratagem is implied by previously known lemming physics.However,

1. The stratagem is formulatable in natural language and in logic and can be com-municated from one person to another. We need to ﬁgure out how to expresssuch stratagems formally.

2. It is not obvious how a program could be made to search for such stratagems.

11.5 Agre and Chapman

Agre and Chapman ([Agre and Chapman, 1987]) describe a program for playing thegame of Pengo. According to that article, Pengo does not require planning. An ade-quate game is played by a program that reacts reﬂexively to rather simple patterns inthe position. For example, their program Pengi uses terms like

the-block-that-the-block-I-just-kicked-will-collide-with

and propositions like

there-is-no-block-suitable-f or-kicking-at-the-bee,

where the dashes indicate that each of the above two expressions is elementary andencoded by some program.

Since the program is speciﬁc to the Pengo game, it doesn’t correspond to the humanability to learn the rules and then learn how to play eﬀectively.

A program deciding what to do based on a ﬁxed set of such terms and propositionswould not work for Lemmings. Thus winning one of the games requires making thevery ﬁrst lemming into a ﬂoater and using this quality only after the last lemming hasemerged. Discovering this requires that the program do planning. The argument theprogram has to make is something like this.

I need to make the ﬁrst lemming into a blocker, but if I plan to blow him up

at the end I won’t save all the lemmings—which is required in this game.

Therefore, he must be undermined rather than blown up. However, if he

is undermined as just a blocker, he will fall to his death when undermined.

Therefore, he must be made into a ﬂoater before he is made a blocker, i.e.

at the very beginning. The very last lemming out of the source must also

be made a ﬂoater and then a miner.

Actually there is a way out whereby it could be claimed that a reactive strategywill work for Lemmings, but I doubt that the advocates of reactive programs wouldwant to take it. Namely, if we put a suﬃciently rich mental structure into the externalsituation, then we could imagine a reactive program operating mainly in the mentalpart of the space could do anything a planner can do. It would be interesting to tryto work out details of this idea.

12 References

References

[Agre and Chapman, 1987] Agre, Philip (pagre@weber.ucsd.edu) and DavidChapman (zvona@sail.stanford.edu): “Pengi: An Implementation of aTheory of Activity”, AAAI National Conference, 1987. Morgan-Kaufman.[Hardy, 1937] Hardy, G. H. (1937): A course of pure mathematics, 7th ed., Cam-bridge University Press. The quotation is from the preface to the 7th edition.[McCarthy, 1959] McCarthy, John (1959):

“Programs with Common Sense”,in Proceedings of the Teddington Conference on the Mechanization ofThought Processes, Her Majesty’s Stationery Oﬃce, London. Reprinted in[McCarthy, 1990].

[McCarthy, 1962] McCarthy, John (1962): “Computer Programs for CheckingMathematical Proofs”, Amer. Math. Soc. Proc. of Symposia in Pure Math.,Vol. 5.

[McCarthy and Hayes, 1969] McCarthy, John and P.J. Hayes: “Some Philosoph-ical Problems from the Standpoint of Artiﬁcial Intelligence”, in D. Michie(ed), Machine Intelligence 4, American Elsevier, New York, NY, 1969.Reprinted in [McCarthy, 1990].

[McCarthy, 1979b] McCarthy, John (1979b): “First Order Theories of Individ-ual Concepts and Propositions”,

in Michie, Donald (ed.) Machine In-telligence 9, (University of Edinburgh Press, Edinburgh). Reprinted in[McCarthy, 1990]..

[McCarthy, 1989] McCarthy, John (1989): “Artiﬁcial Intelligence and Logic” inThomason, Richmond (ed.) Philosophical Logic and Artiﬁcial Intelligence(Dordrecht ; Kluwer Academic, c1989).

[McCarthy 94] McCarthy, John (1994):

“Situation Calculus with ConcurrentEvents and Narrative”, manuscript ﬁle, /u/jmc/e93/narrative.tex or withURL http://www-formal.stanford.edu/pub/jmc/narrative.dvi.

[McCarthy, 1990] McCarthy, John (1990): Formalizing Common Sense, Ablex,Norwood, New Jersey, 1990.

[Psygnosis 93] “Lemmings Manual” - on-line document included with the game. Thegame may be purchased from computer stores or by calling the company at44 (51) 709-5755 in England or 800 458-7794 in the U.S.

[McCarthy, 1994b] McCarthy, John (1994b): “Partial Formalizations and the Lem-URLmings

http://www-formal.stanford.edu/pub/jmc/lemmings.dvi. This is a refer-ence to the present article.

Game”,

