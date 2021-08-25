%-------------------------------
% LEXICON
%-------------------------------

% Copyright © 2002 Teknowledge Corporation
% This software released under the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.

% General Notes:

%    NUMBER--
%       ACE only handles singular determiners and nouns at present, but
%    the plural form is required for future extensibility. So number is
%    included for nouns, verbs, and determiner lexicon entries. But to
%    simplify the exposition only singular forms are included below and
%    the grammatical rules do not check number since only singular forms
%    are available at present.

%    COMPOUNDS--
%       Typical lexicon entries are single words many entries (e.g., verbs,
%    nouns, adjectives) can be compounds, consisting of two or more words.
%    Examples are 'dry clean', 'credit card', and 'first class'. Note: the current
%    implementation does allow compounds of more than two words. In the
%    case where WORD would be a single word in a lexical entry it is replaced
%    by a list such as [WORD1, WORD2]. E.g., the compound 'dry clean' is specified
%    as [dry,clean] in the verb lexical entry where a simple noun such as 'enter'
%    would be specified as an atom.

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

:-write('Loading CELT Parser Version 2(b) ...lexicon...syntactic and semantic translation for simple sentences and queries'),nl.

%-------------------------------
% Determiners (Built into ACE)
%-------------------------------

% determiner_in_lexicon(Determiner,Definite_Or_Not).
% where Definite_Or_Not is one of [definite, indefinite]

% reminder: ACE only handles singular determiners and nouns at present
determiner_in_lexicon(a,indefinite).
determiner_in_lexicon(an,indefinite).
determiner_in_lexicon(the,definite).
determiner_in_lexicon(this,definite).

%-------------------------------
% Nouns (User Defined)
%-------------------------------

% noun_in_lexicon(Common_noun,Type,Gender,Countability,Number,SUO_concept,Synset_ID).
% see P. 47, ACE manual for specs.

% Type is one of [person,time,object]

% Notes on noun lexicon entries:
%    1. _ used for gender means either masculine or feminine, as it
%    unifies with either, and is considered equivalent to 'neuter',
%    which is not used. So values should be either masculine, feminine,
%    or _ for either or neuter.
%    2. ACE only handles singular determiners and nouns at present, but
%    the plural form is required for future extensibility.
%    3. Current grammar rules only handle compound nouns of exactly
%    two words.

% simple nouns, neuter gender

noun_in_lexicon(card,object,_,count,singular,'BankCard', 104863407).
noun_in_lexicon(bank,object,_,count,singular,'Bank-FinancialOrganization', 106227059).
noun_in_lexicon(teller,person,_,count,singular,'OccupationalRole',107635368).
noun_in_lexicon(banker,person,_,count,singular,'OccupationalRole',107100408).
noun_in_lexicon(code,object,_,count,singular,'Text',104790774).        
noun_in_lexicon(train,object,_,count,singular,'TransportationDevice',103528724).
noun_in_lexicon(station,object,_,count,singular,'Building',103209448). % Used word sense of 'train station' here
noun_in_lexicon(slot,object,_,count,singular,'ATMSlot',103355826).
noun_in_lexicon(visitor,person,_,count,singular,'SocialRole',107668613).
noun_in_lexicon(milkman,person,_,count,singular,'OccupationalRole',107410407).
noun_in_lexicon(machine,object,_,count,singular,'Machine',102949521).
noun_in_lexicon(tree,object,_,count,singular,'Plant',109396070).

% example of a mass noun, commented out here as it is in CELT's imported vocabulary:
% noun_in_lexicon(water,object,_,mass,singular,'Liquid',110650211).

% nouns that are roles or otherwise require translations beyond
% simple translations like (instance ?sample Sample) for some example
% class Sample.

noun_in_lexicon(customer,person,_,count,singular,'customer',107197309).
noun_in_lexicon([customer,representative],person,_,count,singular,'customerRepresentative',107543954). % Used word sense of 'salesperson'

% translation_template(+Role,+Position,+Suggested_Var_Names, +List_of_SUO_types)
% is interpreted to mean that to say that if some SUO var plays role Role use this template
% with the SUO var in position (starting with 1) Position and constrain
% the other variables (which most also be introduced) to the SUO types
% given.

translation_template(customer,1,
		     ['?customer','?salesperson'],           % Informal documentation for each arg
		     ['CognitiveAgent','CognitiveAgent']).    % Type restrictions
translation_template('customerRepresentative',2,
		     ['?salesperson','?customer','?organization'],
		     ['CognitiveAgent','CognitiveAgent','Organization']).

% verb_translation_template(SUMO_relation_from_verb,Transitivity,Lambda_Expression)
% generates special code for verbs that have mapped to SUMO relations using the
% lambda expression supplied. Note: the first argument is the SUMO relation the verb
% maps to, not the verb itself. In the example below, it is 'possesses' and not 'owns'
% as 'owns' has already been mapped to 'possesses'.

% verb_translation_templates are used to map sentences with stative verbs to
% simple relations where the arguments are easily expressed by their grammatical
% positions of subject, direct object, and / or indirect object.

% translate 'X owns Y' to '[possesses, X, Y]'
% instead of ...[event,'?event','possesses'],[agent,'?event',X],[patient,'?event',Y]...

verb_translation_template(possesses,transitive,X^Y^[possesses,X,Y]). % same as stative_verb(possesses,transitive,X^Y^[possesses,X,Y]).

% translate 'pilot' to '[and,[instance,X,'Human'],[attribute,X,'Pilot']]'
% instead of [instance,X,'Pilot'].
noun_translation_template(pilot,X^[and,[instance,X,'Human'],[attribute,X,'Pilot']]).

% simple nouns, masculine gender

% Note: these are now commented out as they are present in the CELT common noun lexicon.

% noun_in_lexicon(man,person,masculine,count,singular,['Male','Human','FullyFormed'],107391044).
% noun_in_lexicon(boy,person,masculine,count,singular,['Male','Human'],107389783).
% noun_in_lexicon(male,object,masculine,count,singular,['Male','Human'],100865379).

% simple nouns, feminine gender

% Note: these are now commented out as they are present in the CELT common noun lexicon.

% noun_in_lexicon(woman,person,feminine,count,singular,['Female','Human','FullyFormed'],107684780).
% noun_in_lexicon(girl,person,feminine,count,singular,['Female','Human'],107290496).
% noun_in_lexicon(female,object,feminine,count,singular,['Female','Human'],100865075).

% compound nouns
noun_in_lexicon([credit,card],object,_,count,singular,'CreditCard',109633105).

%-------------------------------
% Pronouns (Built into ACE)
%-------------------------------

% pronoun_in_lexicon(Personal_pronoun,Case,Gender,Type).
% see P. 58, ACE manual for specs.

% Notes on pronoun lexicon entries:
%    1. Just as ACE only handles singular determiners and nouns at present
%    it only handle singular pronouns, too, so we do not need 'they' or 'them'
%    in the pronouns below.
%    2. _ used for gender means either masculine or feminine, as it
%    unifies with either, and is considered equivalent to 'neuter',
%    which is not used. So values should be either masculine, feminine,
%    or _ for either or neuter.

pronoun_in_lexicon(he,nominative,masculine,['CognitiveAgent','Male']).
pronoun_in_lexicon(she,nominative,feminine,['CognitiveAgent','Female']).
pronoun_in_lexicon(it,_,_,'Object').
pronoun_in_lexicon(him,accusative,masculine,['CognitiveAgent','Male']).
pronoun_in_lexicon(her,accusative,feminine,['CognitiveAgent','Female']).

%-------------------------------
% Relative Pronouns (Built into ACE)
%-------------------------------

% relative_pronoun_in_lexicon(Rel,Rel_Type).  
% Rel_Type is one of [person,thing], or _ to refer to both.
% see P. 23, ACE manual for specs.

relative_pronoun_in_lexicon(who,person).   % who refers to people
relative_pronoun_in_lexicon(whom,person).  % whom also refers to people
relative_pronoun_in_lexicon(which,object). % which refers to things
relative_pronoun_in_lexicon(that,person).  % that can refer to people, things, etc.
relative_pronoun_in_lexicon(that,object).  % that can refer to people, things, etc.

%-------------------------------
% Proper Nouns (User Defined)
%-------------------------------

% proper_noun_in_lexicon(+Name,-Type,-Gender,-Number,-SUO_type,-SUO_constant,-Synset_ID).
% where Type is one of [person,time,object] (the same as nouns).

proper_noun_in_lexicon('Jane',person,feminine,singular,['Human','Female','FullyFormed'],'Jane-1',empty).
proper_noun_in_lexicon('Karla',person,feminine,singular,['Human','Female','FullyFormed'],'Karla-1',empty).
proper_noun_in_lexicon('SimpleMat',object,_,singular,'Artifact','SimpleMat-1',empty).
proper_noun_in_lexicon(['Mr','Miller'],person,masculine,singular,['Human','Male','FullyFormed'],'MrMiller',empty).
proper_noun_in_lexicon(['The','New','York','Times'],object,_,singular,'Artifact','TheNewYorkTimes',empty).

% 50 most common names from http://www.lifesmith.com/comnames.html, also see http://www.behindthename.com/top.html

% Common names of males
proper_noun_in_lexicon('James',person,masculine,singular,['Human','Male','FullyFormed'],'James-1',empty).
proper_noun_in_lexicon('Jim',person,masculine,singular,['Human','Male','FullyFormed'],'James-1',empty).

proper_noun_in_lexicon('Christopher',person,masculine,singular,['Human','Male','FullyFormed'],'Christopher-1',empty).
proper_noun_in_lexicon('Chris',person,masculine,singular,['Human','Male','FullyFormed'],'Christopher-1',empty).

proper_noun_in_lexicon('Ronald',person,masculine,singular,['Human','Male','FullyFormed'],'Ronald-1',empty).
proper_noun_in_lexicon('Ron',person,masculine,singular,['Human','Male','FullyFormed'],'Ronald-1',empty).

proper_noun_in_lexicon('John',person,masculine,singular,['Human','Male','FullyFormed'],'John-1',empty).

proper_noun_in_lexicon('Daniel',person,masculine,singular,['Human','Male','FullyFormed'],'Daniel-1',empty).
proper_noun_in_lexicon('Dan',person,masculine,singular,['Human','Male','FullyFormed'],'Daniel-1',empty).

proper_noun_in_lexicon('Anthony',person,masculine,singular,['Human','Male','FullyFormed'],'Anthony-1',empty).
proper_noun_in_lexicon('Tony',person,masculine,singular,['Human','Male','FullyFormed'],'Anthony-1',empty).

proper_noun_in_lexicon('Robert',person,masculine,singular,['Human','Male','FullyFormed'],'Robert-1',empty).
proper_noun_in_lexicon('Bob',person,masculine,singular,['Human','Male','FullyFormed'],'Robert-1',empty).

proper_noun_in_lexicon('Paul',person,masculine,singular,['Human','Male','FullyFormed'],'Paul-1',empty).

proper_noun_in_lexicon('Kevin',person,masculine,singular,['Human','Male','FullyFormed'],'Kevin-1',empty).

proper_noun_in_lexicon('Michael',person,masculine,singular,['Human','Male','FullyFormed'],'Michael-1',empty).
proper_noun_in_lexicon('Mike',person,masculine,singular,['Human','Male','FullyFormed'],'Michael-1',empty).

proper_noun_in_lexicon('Mark',person,masculine,singular,['Human','Male','FullyFormed'],'Mark-1',empty).

proper_noun_in_lexicon('Jason',person,masculine,singular,['Human','Male','FullyFormed'],'Jason-1',empty).

proper_noun_in_lexicon('William',person,masculine,singular,['Human','Male','FullyFormed'],'William-1',empty).
proper_noun_in_lexicon('Bill',person,masculine,singular,['Human','Male','FullyFormed'],'William-1',empty).

proper_noun_in_lexicon('Donald',person,masculine,singular,['Human','Male','FullyFormed'],'Donald-1',empty).
proper_noun_in_lexicon('Don',person,masculine,singular,['Human','Male','FullyFormed'],'Donald-1',empty).

proper_noun_in_lexicon('Jeff',person,masculine,singular,['Human','Male','FullyFormed'],'Jeff-1',empty).

proper_noun_in_lexicon('David',person,masculine,singular,['Human','Male','FullyFormed'],'David-1',empty).
proper_noun_in_lexicon('Dave',person,masculine,singular,['Human','Male','FullyFormed'],'David-1',empty).

proper_noun_in_lexicon('George',person,masculine,singular,['Human','Male','FullyFormed'],'George-1',empty).

proper_noun_in_lexicon('Richard',person,masculine,singular,['Human','Male','FullyFormed'],'Richard-1',empty).
proper_noun_in_lexicon('Dick',person,masculine,singular,['Human','Male','FullyFormed'],'Richard-1',empty).

proper_noun_in_lexicon('Kenneth',person,masculine,singular,['Human','Male','FullyFormed'],'Kenneth-1',empty).
proper_noun_in_lexicon('Ken',person,masculine,singular,['Human','Male','FullyFormed'],'Kenneth-1',empty).

proper_noun_in_lexicon('Charles',person,masculine,singular,['Human','Male','FullyFormed'],'Charles-1',empty).
proper_noun_in_lexicon('Charlie',person,masculine,singular,['Human','Male','FullyFormed'],'Charles-1',empty).

proper_noun_in_lexicon('Steven',person,masculine,singular,['Human','Male','FullyFormed'],'Steven-1',empty).
proper_noun_in_lexicon('Steve',person,masculine,singular,['Human','Male','FullyFormed'],'Steven-1',empty).

proper_noun_in_lexicon('Joseph',person,masculine,singular,['Human','Male','FullyFormed'],'Joseph-1',empty).
proper_noun_in_lexicon('Joe',person,masculine,singular,['Human','Male','FullyFormed'],'Joseph-1',empty).

proper_noun_in_lexicon('Edward',person,masculine,singular,['Human','Male','FullyFormed'],'Edward-1',empty).
proper_noun_in_lexicon('Ed',person,masculine,singular,['Human','Male','FullyFormed'],'Edward-1',empty).

proper_noun_in_lexicon('Thomas',person,masculine,singular,['Human','Male','FullyFormed'],'Thomas-1',empty).
proper_noun_in_lexicon('Tom',person,masculine,singular,['Human','Male','FullyFormed'],'Tom-1',empty).

proper_noun_in_lexicon('Brian',person,masculine,singular,['Human','Male','FullyFormed'],'Brian-1',empty).
proper_noun_in_lexicon('Rick',person,masculine,singular,['Human','Male','FullyFormed'],'Brian-1',empty).

% Common names of females

proper_noun_in_lexicon('Mary',person,feminine,singular,['Human','Female','FullyFormed'],'Mary-1',empty).

proper_noun_in_lexicon('Lisa',person,feminine,singular,['Human','Female','FullyFormed'],'Lisa-1',empty).

proper_noun_in_lexicon('Michelle',person,feminine,singular,['Human','Female','FullyFormed'],'Michelle-1',empty).

proper_noun_in_lexicon('Patricia',person,feminine,singular,['Human','Female','FullyFormed'],'Patricia-1',empty).
proper_noun_in_lexicon('Pat',person,feminine,singular,['Human','Female','FullyFormed'],'Patricia-1',empty).

proper_noun_in_lexicon('Nancy',person,feminine,singular,['Human','Female','FullyFormed'],'Nancy-1',empty).

proper_noun_in_lexicon('Laura',person,feminine,singular,['Human','Female','FullyFormed'],'Laura-1',empty).

proper_noun_in_lexicon('Linda',person,feminine,singular,['Human','Female','FullyFormed'],'Linda-1',empty).

proper_noun_in_lexicon('Karen',person,feminine,singular,['Human','Female','FullyFormed'],'Karen-1',empty).

proper_noun_in_lexicon('Sarah',person,feminine,singular,['Human','Female','FullyFormed'],'Sarah-1',empty).
proper_noun_in_lexicon('Sara',person,feminine,singular,['Human','Female','FullyFormed'],'Sarah-1',empty).

proper_noun_in_lexicon('Barbara',person,feminine,singular,['Human','Female','FullyFormed'],'Barbara-1',empty).

proper_noun_in_lexicon('Betty',person,feminine,singular,['Human','Female','FullyFormed'],'Betty-1',empty).

proper_noun_in_lexicon('Kimberly',person,feminine,singular,['Human','Female','FullyFormed'],'Kimberly-1',empty).
proper_noun_in_lexicon('Kim',person,feminine,singular,['Human','Female','FullyFormed'],'Kimberly-1',empty).

proper_noun_in_lexicon('Elizabeth',person,feminine,singular,['Human','Female','FullyFormed'],'Elizabeth-1',empty).
proper_noun_in_lexicon('Liz',person,feminine,singular,['Human','Female','FullyFormed'],'Elizabeth-1',empty).

proper_noun_in_lexicon('Helen',person,feminine,singular,['Human','Female','FullyFormed'],'Helen-1',empty).

proper_noun_in_lexicon('Deborah',person,feminine,singular,['Human','Female','FullyFormed'],'Deborah-1',empty).
proper_noun_in_lexicon('Debbie',person,feminine,singular,['Human','Female','FullyFormed'],'Deborah-1',empty).

proper_noun_in_lexicon('Jennifer',person,feminine,singular,['Human','Female','FullyFormed'],'Jennifer-1',empty).
proper_noun_in_lexicon('Jenny',person,feminine,singular,['Human','Female','FullyFormed'],'Jennifer-1',empty).

proper_noun_in_lexicon('Sandra',person,feminine,singular,['Human','Female','FullyFormed'],'Sandra-1',empty).

proper_noun_in_lexicon('Maria',person,feminine,singular,['Human','Female','FullyFormed'],'Maria-1',empty).

proper_noun_in_lexicon('Donna',person,feminine,singular,['Human','Female','FullyFormed'],'Donna-1',empty).

proper_noun_in_lexicon('Susan',person,feminine,singular,['Human','Female','FullyFormed'],'Susan-1',empty).

proper_noun_in_lexicon('Carol',person,feminine,singular,['Human','Female','FullyFormed'],'Carol-1',empty).

proper_noun_in_lexicon('Margaret',person,feminine,singular,['Human','Female','FullyFormed'],'Margaret-1',empty).

proper_noun_in_lexicon('Ruth',person,feminine,singular,['Human','Female','FullyFormed'],'Ruth-1',empty).

proper_noun_in_lexicon('Dorothy',person,feminine,singular,['Human','Female','FullyFormed'],'Dorothy-1',empty).

proper_noun_in_lexicon('Sharon',person,feminine,singular,['Human','Female','FullyFormed'],'Sharon-1',empty).

proper_noun_in_lexicon('Monday',time,_,singular,'Day','Monday',110882888).
proper_noun_in_lexicon('Tuesday',time,_,singular,'Day','Tuesday',110883014).
proper_noun_in_lexicon('Wednesday',time,_,singular,'Day','Wednesday',110883142).
proper_noun_in_lexicon('Thursday',time,_,singular,'Day','Thursday',110883253).
proper_noun_in_lexicon('Friday',time,_,singular,'Day','Friday',110883362).
proper_noun_in_lexicon('Saturday',time,_,singular,'Day','Saturday',110883469).
proper_noun_in_lexicon('Sunday',time,_,singular,'Day','Sunday',110883776).

proper_noun_in_lexicon('January',time,_,singular,'Month','January',110922756).
proper_noun_in_lexicon('February',time,_,singular,'Month','February',110923216).
proper_noun_in_lexicon('March',time,_,singular,'Month','March',110923619).
proper_noun_in_lexicon('April',time,_,singular,'Month','April',110923938).
proper_noun_in_lexicon('May',time,_,singular,'Month','May',110924252).
proper_noun_in_lexicon('June',time,_,singular,'Month','June',110924593).
proper_noun_in_lexicon('July',time,_,singular,'Month','July',110924973).
proper_noun_in_lexicon('August',time,_,singular,'Month','August',110925280).
proper_noun_in_lexicon('September',time,_,singular,'Month','September',110925564).
proper_noun_in_lexicon('October',time,_,singular,'Month','October',110925921).
proper_noun_in_lexicon('November',time,_,singular,'Month','November',110926231).
proper_noun_in_lexicon('December',time,_,singular,'Month','December',110926618).

% these are here temporarily to allow them to be referred to without determiners:
% e.g., 'he arrives at midnight.'
proper_noun_in_lexicon(midnight,time,_,singular,'TimeMeasure','TimeMeasure',110887026).
proper_noun_in_lexicon(noon,time,_,singular,'TimeMeasure','TimeMeasure',110884479).


%-------------------------------
% Adjectives (User Defined)
%-------------------------------

% adjective_in_lexicon(Adjective,Root,Kind,Grade,SUO_concept)

% Note: SUO_concept is a lambda expression for two-place adjectives.

% Kind is one of [normal, two_place]
% Grade is one of [positive,comparative,superlative]
% ...note: use positive if the adjective is normally ungraded.

% graded adjectives:
adjective_in_lexicon(large,large,normal,positive,'largeness-WN01328712').
adjective_in_lexicon(larger,large,normal,comparative,'largeness-WN01328712').
adjective_in_lexicon(largest,large,normal,superlative,'largeness-WN01328712').

adjective_in_lexicon(small,small,normal,positive,'smallness-WN01336443').
adjective_in_lexicon(smaller,small,normal,comparative,'smallness-WN01336443').
adjective_in_lexicon(smallest,small,normal,superlative,'smallness-WN01336443').

adjective_in_lexicon(old,old,normal,positive,'oldness-WN03854196').
adjective_in_lexicon(older,old,normal,comparative,'oldness-WN03854196').
adjective_in_lexicon(oldest,old,normal,superlative,'oldness-WN03854196').

adjective_in_lexicon(new,new,normal,positive,'newness-WN03855074').
adjective_in_lexicon(newer,new,normal,comparative,'newness-WN03855074').
adjective_in_lexicon(newest,new,normal,superlative,'newness-WN03855074').

adjective_in_lexicon(hungry,hungry,normal,positive,'hungry-WN01216853').
adjective_in_lexicon(hungrier,hungry,normal,comparative,'hungry-WN01216853').
adjective_in_lexicon(hungriest,hungry,normal,superlative,'hungry-WN01216853').

adjective_in_lexicon(thirsty,thirsty,normal,positive,'thirsty-WN01217685').
adjective_in_lexicon(thirstier,thirsty,normal,comparative,'thirsty-WN01217685').
adjective_in_lexicon(thirstiest,thirst,normal,superlative,'thirsty-WN01217685').

adjective_in_lexicon(rich,rich,normal,positive,'richness-WN01950464').
adjective_in_lexicon(richer,rich,normal,comparative,'richness-WN01950464').
adjective_in_lexicon(richest,rich,normal,superlative,'richness-WN01950464').

adjective_in_lexicon(fast,fast,normal,positive,'speed-WN00212796').
adjective_in_lexicon(faster,fast,normal,comparative,'speed-WN00212796').
adjective_in_lexicon(fastest,fast,normal,superlative,'speed-WN00212796').

adjective_in_lexicon(expensive,expensive,normal,positive,'expensiveness-WN04009070').
adjective_in_lexicon([more,expensive],expensive,normal,comparative,'expensiveness-WN04009070').
adjective_in_lexicon([most,expensive],expensive,normal,superlative,'expensiveness-WN04009070').

adjective_in_lexicon(solid,solid,normal,positive,'Solid').
adjective_in_lexicon(liquid,liquid,normal,positive,'Liquid').
adjective_in_lexicon(gas,gas,normal,positive,'Gas').
adjective_in_lexicon(sweet,sweet,normal,positive,'Sweet').
adjective_in_lexicon(bitter,bitter,normal,positive,'Bitter').

adjective_in_lexicon(red,red,normal,positive,'Red').
adjective_in_lexicon(green,green,normal,positive,'Green').
adjective_in_lexicon(blue,blue,normal,positive,'Blue').
adjective_in_lexicon(yellow,yellow,normal,positive,'Yellow').

adjective_in_lexicon(open,open,normal,positive,'open-WN00037427').
adjective_in_lexicon(fillable,fillable,normal,positive,'Fillable').
adjective_in_lexicon(pliable,pliable,normal,positive,'Pliable').
adjective_in_lexicon(rigid,rigid,normal,positive,'Rigid').

adjective_in_lexicon(smooth,smooth,normal,positive,'Smooth').
adjective_in_lexicon(rough,rough,normal,positive,'Rough').

adjective_in_lexicon(dry,dry,normal,positive,'Dry').
adjective_in_lexicon(anhydrous,anhydrous,normal,positive,'Anhydrous').
adjective_in_lexicon(damp,damp,normal,positive,'Damp').
adjective_in_lexicon(wet,wet,normal,positive,'Wet').

adjective_in_lexicon(fragile,fragile,normal,positive,'Fragile').
adjective_in_lexicon(unbreakable,unbreakable,normal,positive,'Unbreakable').

adjective_in_lexicon(living,living,normal,positive,'Living').
adjective_in_lexicon(dead,unbreakable,normal,positive,'Dead').

adjective_in_lexicon(larval,larval,normal,positive,'Larval').
adjective_in_lexicon(embryonic,embryonic,normal,positive,'Embryonic').
adjective_in_lexicon(fetal,fetal,normal,positive,'Fetal').

adjective_in_lexicon(aggressive,aggressive,normal,positive,'Aggressive').
adjective_in_lexicon(docile,docile,normal,positive,'Docile').

adjective_in_lexicon(asleep,asleep,normal,positive,'Asleep').
adjective_in_lexicon(unconscious,unconscious,normal,positive,'Unconscious').
adjective_in_lexicon(awake,awake,normal,positive,'Awake').

% compound adjectives:
adjective_in_lexicon([first,class],first_class,normal,positive,'first_class-WN03718753').

% two-place adjectives (take a prepositional complement, the preposition is included here)
% Note: SUO_concept is a lambda expression for two-place adjectives.
adjective_in_lexicon([identical,to],identical,two_place,ungraded,X^Y^[equal,X,Y]).
adjective_in_lexicon([compatible,with],compatible_with,two_place,ungraded,X^Y^[compatible,X,Y]).
adjective_in_lexicon([greater,than],greater_than,two_place,ungraded,X^Y^[greaterThan,X,Y]).
adjective_in_lexicon([less,than],less_than,two_place,ungraded,X^Y^[lessThan,X,Y]).
adjective_in_lexicon([different,from],different_from,two_place,ungraded,X^Y^[not,[equal,X,Y]]).

% ungraded adjectives (note: use positive for the grade)
adjective_in_lexicon(valid,valid,normal,positive,'validation-WN00099890').
adjective_in_lexicon(invalid,invalid,normal,positive,'incorrectness-WN03838242').
adjective_in_lexicon('Swiss','Swiss',normal,positive,'Germanism-WN00265812'). % change to 'Swiss' in Wordnet when available.

% Notes on adjective entries:
%    1. ACE does not allow 'that' as the preposition for
%    two-place adjectives, see p. 52 for explanation.

%-------------------------------
% Prepositions (Built into ACE)
%-------------------------------

% preposition_in_lexicon(Preposition,Modification_type,Noun_type,SUO_concept).
% see P. 54, ACE manual for specs. Note that 'at', 'on', and 'in' can apply to
% either locations or times and if the object noun is of type 'time' then the
% temporal interpretation of the adverb is taken.

% Noun_type is one of [person,time,object]
% Modification_type is one of [location, origin, direction, time, start, end, duration, instrument, comitative]

preposition_in_lexicon(at,location,object,'located').               % e.g., 'at the house'
preposition_in_lexicon(on,location,object,                          % e.g., 'on the floor'
		       Event^Object^[orientation,Event,Object,'On']).
preposition_in_lexicon(in,location,object,'located').               % e.g., 'in the house'
preposition_in_lexicon(to,location,object,'located').               % e.g., 'to the house'
preposition_in_lexicon(to,location,time,  'EndFn').                 % e.g., 'to the opening' (e.g., 'He waits to the opening.') like until.

preposition_in_lexicon(at,time,time,'overlapsTemporally').          % e.g., 'at 9 AM'
preposition_in_lexicon(on,time,time,'overlapsTemporally').          % e.g., 'on Monday'
preposition_in_lexicon(in,time,time,'overlapsTemporally').          % e.g., 'in an hour'

preposition_in_lexicon(for,direction,person,'destination').         % e.g., 'for John'
preposition_in_lexicon(for,duration,time,'duration').               % e.g., 'for one hour'

preposition_in_lexicon(from,origin,object,'origin').                % e.g., 'from the station'
preposition_in_lexicon(from,start,time,'BeginFn').                  % e.g., 'from noon'

preposition_in_lexicon(into,direction,object,'located').            % e.g., 'into the office'

preposition_in_lexicon(through,direction,object,'traverses').       % e.g., 'through the door'
preposition_in_lexicon(through,duration,time,'overlapsTemporally'). % e.g., 'through Monday'

preposition_in_lexicon(until,end,time,'EndFn').                     % e.g., 'until Tuesday'

preposition_in_lexicon(with,instrument,object,'instrument').        % e.g., 'with a key'
preposition_in_lexicon(with,comitative,person,                      % e.g., 'with a policeman',
		       Event^Person^[agent,Event,Person]).          %  in this case person is an additional agent of the event

% Examples of additional prepositions added with 2-arg Lambda expressions for Event and Object
% for additional adverbs taking an object of CELT type 'object'.

preposition_in_lexicon(across,direction,object,Event^Object^[traverses,Event,Object]).
preposition_in_lexicon(toward,location,object,Event^Object^[direction,Event,Object]).
preposition_in_lexicon(inside,location,object,Event^Object^[exactlyLocated,Event,Object]).
preposition_in_lexicon(outside,location,object,Event^Object^[not,[located,Event,Object]]).
preposition_in_lexicon(within,location,object,Event^Object^[located,Event,Object]).
preposition_in_lexicon(around,direction,object,Event^Object^[orientation,Event,Object,'Near']).
preposition_in_lexicon(above,location,object,Event^Object^[orientation,Event,Object,'Above']).
preposition_in_lexicon(over,location,object,Event^Object^[orientation,Event,Object,'Above']).
preposition_in_lexicon(below,location,object,Event^Object^[orientation,Event,Object,'Below']).
preposition_in_lexicon(under,location,object,Event^Object^[orientation,Event,Object,'Below']).
preposition_in_lexicon(atop,location,object,Event^Object^[orientation,Event,Object,'On']).

% Examples of additional prepositions added with 2-arg Lambda expressions for Event and Person
% for additional adverbs taking an object of CELT type 'person'.
preposition_in_lexicon(inside,direction,person,Event^Person^[direction,Event,Person]).
preposition_in_lexicon(outside,direction,person,Event^Person^[direction,Event,Person]).
preposition_in_lexicon(through,direction,person,Event^Person^[penetrates,Event,Person]).

% Examples of additional prepositions added with 2-arg Lambda expressions for Event and Time
% for additional adverbs taking an object of CELT type 'time'.
preposition_in_lexicon(before,direction,time,Event^Time^[lessThan,[beginfn,Event],[beginfn,Time]]).
preposition_in_lexicon(after,direction,time,Event^Time^[greaterThan,[endfn,Event],[endfn,Time]]).

% Note: from...to is not yet handled, use from...until in the meantime as a workaround
temporal_expression_in_lexicon(from,to,start,end,'start_time','end_time'). % e.g., 'from 10 to 12', must occur in from...to

% special prepositions: 'of' and 'to' are built into grammatical rules for noun and verb modifiers,
% so we do NOT need any additional entries for them like the following:

% preposition_in_lexicon(of,noun_attribute).            % special, only preposition that can be used as a noun modifier
% preposition_in_lexicon(to,ditransitive_complement).   % special, only preposition used with indirect objects in ACE

% Notes on preposition lexicon entries:
%    1. 'to' as modification type 'end' should
%    only occur in a 'from...to' prepositional phrase.
%    Currently this is not checked for to simplify the grammar.

%-------------------------------
% Query Verb Phrase Modifiers (Built into ACE)
%-------------------------------

% See page 61.

% location
query_verb_phrase_modifier_in_lexicon(where,[location],'located').                 % Where does John work?

% origin
query_verb_phrase_modifier_in_lexicon([where,from],apart,[origin],'located').      % Where does the train arrive from?
query_verb_phrase_modifier_in_lexicon([from,where],together,[origin],'located').   % From where does the train arrive?

% direction
query_verb_phrase_modifier_in_lexicon([where,to],apart,[direction],'located').     % Where does John insert the card into?
query_verb_phrase_modifier_in_lexicon([where,into],apart,[direction],'located').   % Where does the machine move to?

% time
query_verb_phrase_modifier_in_lexicon(when,[time],'located').                      % When does the train arrive?

% start
query_verb_phrase_modifier_in_lexicon([since,when],together,[start],'located').    % Since when does the employee work?

% end
query_verb_phrase_modifier_in_lexicon([until,when],together,[end],'located').      % Until when is the shop open?

% duration
query_verb_phrase_modifier_in_lexicon([how,long],together,[duration],'located').   % How long does the machine work?

% frequency
query_verb_phrase_modifier_in_lexicon([how,often],together,[frequency],'located'). % How often does the milkman come?

% instrument
query_verb_phrase_modifier_in_lexicon([with,what],together,[instrument],'located').% With what does John open the door?

% manner
query_verb_phrase_modifier_in_lexicon(how,[manner],'located').                     % How does John insert the card?

% comitative

query_verb_phrase_modifier_in_lexicon([with,whom],together,[comitative],'located').% With whom does the employee enter the room?
query_verb_phrase_modifier_in_lexicon([with,who],together,[comitative],'located'). % With who does the employee enter the room?

%-------------------------------
% Verbs (User Defined)
%-------------------------------

% verb_in_lexicon(Verb,Root,Transitivity,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID).
% Kind_of_verb is one of [simple,compound,phrasal,prepositional].
% Event_or_state is one of [event,state]

% Note: The copula verb 'be' is handled directly in the grammar rules and has no entry here.

% Simple verbs:         verb_in_lexicon(Verb,Root,Transitivity,Number,simple).
% Compound verbs:       verb_in_lexicon([Word1,Word2],Root,Transitivity,Number,compound).
% Phrasal verbs:        verb_in_lexicon([Verb,Prep],Root,Transitivity,Number,phrasal).
% Prepositional verbs:  verb_in_lexicon([Verb,Prep],Root,Transitivity,Number,prepositional).

% simple verbs
% ...intransitive
% Intransitivity is a term that describes a verb or clause that is unable to take a direct object.
verb_in_lexicon(stays,stay,[intransitive,no,no],singular,simple,state,'Motion',200078446).
verb_in_lexicon(decomposes,decompose,[intransitive,no,no],singular,simple,event,'ChemicalDecomposition',200143411).
verb_in_lexicon(turns,turn,[intransitive,no,no],singular,simple,event,'DirectionChange',2013101287).
verb_in_lexicon(floats,float,[intransitive,no,no],singular,simple,event,'Floats',201298374).
verb_in_lexicon(works,work,[intransitive,no,no],singular,simple,event,'Works',201643531).
verb_in_lexicon(runs,run,[intransitive,no,no],singular,simple,event,'Runs',201314495).
verb_in_lexicon(comes,come,[intransitive,no,no],singular,simple,event,'Comes',201262658).
verb_in_lexicon(stop,stops,[intransitive,no,no],singular,simple,event,'Stops',201270990).
verb_in_lexicon(move,moves,[intransitive,no,no],singular,simple,event,'Movement',201253107).

% ...transitive
% A transitive verb is a verb that takes a direct object.
verb_in_lexicon(enters,enter,[no,transitive,no],singular,simple,event,'Movement',201376901).
verb_in_lexicon(inserts,insert,[no,transitive,no],singular,simple,event,'Putting',200974668).
verb_in_lexicon(uses,use,[no,transitive,no],singular,simple,event,'Applies',200788109).           % Note: 'Applies' not in SUMO?
verb_in_lexicon(beats,beat,[no,transitive,no],singular,simple,event,'Hits',200958408).            % Note: 'Hits' not in SUMO?
verb_in_lexicon(owns,own,[no,transitive,no],singular,simple,state,'possesses',201509295).

% ...ditransitive
% Ditransitivity is a term which describes a verb or clause which takes two objects.
verb_in_lexicon(gives,give,[no,no,ditransitive],singular,simple,event,'Giving',201583087).
verb_in_lexicon(puts,put,[no,no,ditransitive],singular,simple,event,'Putting',201026409).
verb_in_lexicon(attaches,attach,[no,no,ditransitive],singular,simple,event,'Attaching',200885494).
verb_in_lexicon(detaches,detach,[no,no,ditransitive],singular,simple,event,'Detaching',200887219).
verb_in_lexicon(unties,untie,[no,no,ditransitive],singular,simple,event,'Untying',200877495).
verb_in_lexicon(separates,sepate,[no,no,ditransitive],singular,simple,event,'Separating',201788872).
verb_in_lexicon(combines,combine,[no,no,ditransitive],singular,simple,event,'Combining',200133085).

% compound verbs
verb_in_lexicon([dry,cleans],[dry,clean],[no,transitive,no],singular,compound,event,'Cleaning',201056234).

% phrasal verbs (see P. 49)
verb_in_lexicon([hands,out],[hand,out],[no,transitive,no],singular,phrasal,event,'Giving',201507640).
verb_in_lexicon([fills,out],[fill,out],[no,transitive,no],singular,phrasal,event,'Writing',200690164).
verb_in_lexicon([takes,off],[take,off],[no,transitive,no],singular,phrasal,event,'Removing',201095514).

% prepositional verbs (see P.49)
verb_in_lexicon([gives,to],[give,to],[no,no,ditransitive],singular,prepositional,event,'Giving',201505951).
verb_in_lexicon([calls,for],[call,for],[no,transitive,no],singular,prepositional,event,'Communication',200510998).
verb_in_lexicon([calls,on],[call,for],[no,transitive,no],singular,prepositional,event,'Communication',200539812).

% Notes on verb lexicon entries:
%    1. ACE only handles singular determiners and nouns at present, but
%    the plural form of nouns and verbs is required for future extensibility.
%    2. Compound verbs always have two words (e.g., 'dry clean') in the
%    current implementation.

%-------------------------------
% Adverbs (User Defined)
%-------------------------------

% adverb_in_lexicon(Adverb,Modification_type,Sigma_Concept).
% see P. 53 & p.56 ACE manual for specs.
% Modification_type is one of ...
%   [location,origin,direction,time,start,end,duration,frequency,instrument,manner,comitative]
% (see p. 57).

% Notes on adverb lexicon entries:
%    1. Many kinds of adverbs are disallowed
% in ACE, e.g., adverbs of degree ('barely'),
% see P. 54 for rationale.

adverb_in_lexicon(underground,location,'underpass-WN03560180').
adverb_in_lexicon(forward,direction,'front-WN02724935').
adverb_in_lexicon(now,time,'present-WN04776839').
adverb_in_lexicon(permanently,duration,'permanence-WN03944434').
adverb_in_lexicon(daily,frequency,'daily-WN04738867'). %need to get other sense here, this one is for the daily news..
adverb_in_lexicon(correctly,manner,'correctness-WN03836923').
adverb_in_lexicon(manually,manner,'manual_labor-WN00410579').
adverb_in_lexicon(slowly,manner,'slowness-WN03951223').
adverb_in_lexicon(punctually,manner,'punctuality-WN03940380').
