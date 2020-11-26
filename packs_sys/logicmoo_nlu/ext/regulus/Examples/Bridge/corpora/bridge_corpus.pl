% Cards, suits, players. Who has what cards.
%
% Vocabulary:
% north, south, east, west, hand
% ace, king, queen, jack, ten, nine, eight, seven, six, five, four, three, two [and plurals of at least face cards]
% suit
% board
% have
% long, longer, longest
% singleton, doubleton, tripleton, void, bare
% honour
% dealer
% player nouns: partner, l h o, r h o
sent('play the five of diamonds').
sent('win the trick in dummy').
sent('play the six of hearts').
sent('play small').
sent('play the ace').
sent('i said play the ace').
sent('who is on lead').
sent('am i in hand').
sent('play the king').
sent('the king of spades').
sent('can i have my card back').
sent('i want to play the ace').
sent('play small from dummy').
sent('now i want to play the queen of clubs').
sent('put the king on it').
sent('now play the jack of clubs').
sent('sorry lead the seven of clubs').
sent('now i \'m going to play the jack').
sent('can i take that back').
sent('i want to play the six instead').
/*
sent('does north have the jack of diamonds').
sent('who has the queen of clubs').
sent('how many aces does south have').
sent('how many spades does east have').
sent('does south have the ace or king of spades').
sent('does north have more than three hearts').
sent('does east have at least two diamonds').
sent('do you have more hearts than spades').
sent('what is your longest suit').
sent('do you have a singleton').
sent('do you have a void').
sent('how many clubs are there in your hand').
sent('what honours do you have').
sent('what spade honours do you hold').
sent('who is dealer').
sent('who is your right hand opponent').
sent('who is your left hand opponent').
sent('do you have any bare honours').
sent('shall we play the next board').
sent('did you have a yarborough').
sent('which direction are you sitting').
sent('did declarer make').
sent('did declarer make the contract').
sent('did you go down').
sent('did you make the king of spades').
sent('did you make a long spade trick').
sent('did you make a ruff in dummy').
sent('what lead did west make').
sent('what discard did you make on the king of trumps').
sent('how many did you go down').
sent('are you one down').
sent('are you two down').
sent('how many overtricks did you make').
sent('are you sitting east west').
sent('are you sitting north south').
sent('is vulnerability favourable').
sent('is vulnerability unfavourable').
sent('was that a flat board').

% Bids. Who bid what.
%
% all bids: one club ... seven no trumps
% declarer, bidder
% bidding verbs: bid, open, overcall, double, pass, no bid
sent('who bid one no trump').
sent('who bid four spades').
sent('what did north respond to one club').
sent('who opened').
sent('did east overcall').
sent('did anyone double').
sent('what was the final contract').
sent('who is declarer').
sent('who redoubled three clubs').
sent('did partner pass four spades').
sent('are spades trumps').
sent('which suit is trumps').
sent('who was the opening bidder').
sent('whose call is it').

% Basic play in a suit
%
% playing verbs: finesse, take, establish, ruff, cash, cross, run, cover, take [finesse etc], work
% card disposition verbs: be [on side, etc], break
% entry, finesse, ruffing finesse
% prepositions: in + suit, with
% pp: on side, offside
sent('can you finesse in diamonds').
sent('is the queen of diamonds on side').
sent('how many tricks can you take in diamonds if the queen is on side').
sent('do you have an entry in spades').
sent('can you establish clubs').
sent('can you establish clubs if they break three three').
sent('does the ruffing finesse in spades work').
sent('cash the ace and king of spades').
sent('cross to dummy with a heart').
sent('ruff a club').
sent('trump the club with the king').
sent('run the diamonds').
sent('cover the queen with the king').
sent('capture the king with the ace').
sent('cross to the closed hand').
sent('can you clear clubs').
sent('can you eliminate clubs').
sent('how should i have developed clubs').
sent('how should i have established the club suit').
sent('did you drop the king').
sent('how many tricks did you drop when you led the trump').
sent('would an elimination have let you endplay north').
sent('is anybody squeezed in the three card ending').
sent('is there an endplay against east').
sent('can you endplay east in spades').
sent('do hearts split evenly').
sent('do hearts break evenly').
sent('if west follows to three round of spades can you endplay him').
sent('did you have a free finesse in spades').

% Basic bidding
%
% hand adjectives: balanced, unbalanced, flat, four four three two etc
% hand type nouns: maximum
% abstract nouns
% high card point
% need
% convention nouns: preempt, weak two
% hand adjectives: strong
% bid adjectives
sent('do you have a balanced hand').
sent('how many high card points do you have').
sent('do you have a balanced hand with twelve to fourteen high card points').
sent('can you open one no trump').
sent('can you open one spade').
sent('how many hearts do you need to bid two hearts').
sent('are you strong enough to rebid two no trumps').
sent('can you open with a preempt').
sent('is that a weak two').
sent('do you have a maximum').
sent('did you preempt').
sent('was three hearts preemptive').
sent('do you have a preemptive hand').
sent('are you four four three two').
sent('what shape were you').
sent('was your hand shapely enough to bid slam').
sent('were your spades chunky enough to rebid').
sent('what bid should i have made').
sent('should north south bid five spades').
sent('should east west bid six clubs').
sent('should i have bid on with a feature in your suit').
sent('was queen third enough to raise spades').
sent('is jack fourth good enough suit quality to open').
sent('is it okay to rebid jack fifth').
sent('was it alright to open three spades on jack sixth').
sent('do we have a fit in spades').
sent('should i have jump raised you with such a good fit').
sent('was it better to play our four four spade fit or our five four club fit').
sent('does my spade holding fit well with yours').
sent('with such a flat hand should you prefer to play in no trumps').
sent('did you have a flat hand').
sent('what should i have bid first in hand').
sent('if i had been second in hand should i have made the same bid').
sent('do we open light third in hand').
sent('was i too weak to preempt fourth in hand').

% More play
% play verbs: make, establish, discard, duck, shift to, block
% play nouns: safety play, trick, top trick
% abstract nouns: break
% round
% place nouns: table, dummy, hand
% distribution phrases: two one, three one, two two, etc
% happen
% immediately
sent('how do you make if spades break').
sent('can you make if spades are four one').
sent('what can you do if the king is offside').
sent('what happens if you immediately finesse in diamonds').
sent('how many top tricks do you have').
sent('how many entries do you have to hand').
sent('what happens if you play to establish clubs').
sent('can you discard a diamond on the clubs').
sent('finesse twice in clubs').
sent('what is the safety play in trumps').
sent('duck a round of diamonds and then ruff on table when you get back in').
sent('shift to a spade').
sent('did you want to block spades').
sent('are spades now blocked').
sent('would you have made without the bad break in trumps').
sent('can you cash out').
sent('can you force an entry to dummy').
sent('can you force the opponents to give you an entry to dummy').
sent('can you force declarer').
sent('can you claim').
sent('is four spades cold').
sent('did you have a count on west \'s hand').
sent('did you have a count on the spade suit').
sent('could you count west \'s spades').
sent('could you count to nine tricks').
sent('can you play a crossruff').
sent('which is the danger hand').
sent('could you avoid the danger hand getting on lead').
sent('could you have made a discovery play').
sent('could you have made double dummy').
sent('was there a double dummy line').
sent('take the double finesse in spades').
sent('can you make on a dummy reversal').
sent('was our distribution duplicated').
sent('were our spade values duplicated').
sent('did we have wasted values in spades').
sent('were our spade cards working').
sent('did i have a non working honour in spades').
sent('can you establish diamonds if they break').
sent('can you establish clubs and then cash them').
sent('should west exit with a spade').
sent('could west leave himself with an exit card').
sent('could you have made an extra trick').
sent('should i go in with the queen').
sent('should i go up with the ace').
sent('is the spade suit good').
sent('are the spades good').
sent('is the queen of spades good').

% More bidding
% bid verbs: reply, force, show, balance, overbid, underbid
% bid nouns: rebid, game, slam, overbid, underbid, force, change of suit
% bid adjective: artificial, unusual, gambling
% convention nouns: transfer, stayman, blackwood, unusual two no trump
sent('is four clubs a slam invite').
sent('is two diamonds a transfer').
sent('is two clubs stayman').
sent('what is your rebid if partner replies one no trump').
sent('what do you do if r h o overcalls two spades').
sent('is that forcing to game').
sent('how many aces does five diamonds show').
sent('is that bid artificial').
sent('did you decide to balance').
sent('was that a balancing bid').
sent('was four no trumps blackwood').
sent('was two no trumps unusual').
sent('was three no trumps gambling').
sent('was that bid a game force').
sent('was that bid an overbid').
sent('was that bid an underbid').
sent('was change of suit forcing').
sent('could i have been more competitive in the auction').
sent('could i have competed more').
sent('was your bid competitive').
sent('was your bid competitive or constructive').
sent('was that bid obstructive').
sent('should i have corrected hearts to spades').
sent('should i have preferred hearts to spades').
sent('should i have given preference to hearts').
sent('should i have converted hearts to spades').
sent('should i have converted spades').
sent('was five clubs a cue bid').
sent('was four hearts to play').
sent('was four hearts a signoff').
sent('what denomination should we play in').
sent('was that jump forcing').
sent('was the jump raise forcing').
sent('was the double jump forcing').
sent('should i have jump raised').
sent('should i have jump shifted').
sent('could you have made on a double squeeze').
sent('could you make on a squeeze').
sent('could you play a simple squeeze against west').
sent('does the doubler have to have the king of spades').
sent('what should i have rebid with my extras').
sent('what should i have rebid with my extra values').
sent('was your two spade bid false preference').
sent('was your jump to game fast arrival').
sent('was that bid forcing to game').
sent('was one spade fourth suit forcing').
sent('was your one spade call a free bid').
sent('was one spade a free bid').
sent('was two clubs a free raise').
sent('was three hearts a game try').
sent('was three hearts a game invite').
sent('will four spades go for eight hundred').
sent('should i have gone for the slam').

% Defence
% Defence nouns: attitude, lead
% Defence verbs: lead, overlead, underlead
% Defence adjectives: fourth highest, mud

sent('was the eight of diamonds attitude').
sent('was your lead fourth highest').
sent('was your lead fourth best').
sent('was your lead mud').
sent('why did you lead the king of diamonds').
sent('was the ace tripleton').
sent('what is your carding').
sent('what is your signalling').
sent('what signals are we using').
sent('was your eight of diamonds a signal').
sent('does the spade shift beat the contract').
sent('was your two of hearts a count card').
sent('did your two of hearts give count').
sent('could you have defended better').
sent('was there a better defence').
sent('which defender do you want on lead').
sent('was there a way to defeat the contract').
sent('what was your discard on the first round of trumps').
sent('could you have broken up the squeeze').
sent('could you stop declarer making').
sent('why did you echo your trumps').
sent('did your trump echo mean you want a ruff').
sent('should east have falsecarded in spades').
sent('was the queen of spades a false card').
*/