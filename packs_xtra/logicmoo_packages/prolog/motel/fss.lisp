;
; %A%
;

(load "motel.lisp")
(start-motel)

(prolog-goal '(
(sb__defenv fss_env fss_env)
(sb__initenv fss_env)
(sb__primconcept LEXICON (:list
             (supers (:list SBONE))))
(sb__primconcept FSS (:list
             (supers (:list SBONE))))
(sb__primconcept POINTING (:list
             (supers (:list FSS))))
(sb__primconcept VAGUE__P (:list
             (supers (:list POINTING))))
(sb__primconcept STANDARD__P (:list
             (supers (:list POINTING))))
(sb__primconcept ENCIRCLING__P (:list
             (supers (:list POINTING))))
(sb__primconcept EXACT__P (:list
             (supers (:list POINTING))))
(sb__primconcept TIME (:list
             (supers (:list FSS))))
(sb__primconcept MOMENT (:list
             (supers (:list TIME))))
(sb__primconcept PERIOD (:list
             (supers (:list TIME))))
(sb__primconcept INTERVAL (:list
             (supers (:list TIME))))
(sb__primconcept YEARLY (:list
             (supers (:list INTERVAL))))
(sb__primconcept JAEHRLICH (:list
             (supers (:list YEARLY LEXICON))))
(sb__primconcept WEEKLY (:list
             (supers (:list INTERVAL))))
(sb__primconcept WOECHENTLICH (:list
             (supers (:list LEXICON WEEKLY))))
(sb__primconcept DAILY (:list
             (supers (:list INTERVAL))))
(sb__defconcept TAEGLICH (:list
            (supers (:list DAILY LEXICON))))
(sb__primconcept MONTHLY (:list
             (supers (:list INTERVAL))))
(sb__defconcept MONAT (:list
            (supers (:list MONTHLY LEXICON))))
(sb__primconcept SPEECH__ACT (:list
             (supers (:list FSS))))
(sb__primconcept ORDER (:list
             (supers (:list SPEECH__ACT))))
(sb__primconcept ASSERTION (:list
             (supers (:list SPEECH__ACT))))
(sb__primconcept QUESTION (:list
             (supers (:list SPEECH__ACT))))
(sb__primconcept INTERJECTION (:list
             (supers (:list SPEECH__ACT))))
(sb__primconcept DETERMINER (:list
             (supers (:list FSS))))
(sb__defconcept D (:list
            (supers (:list DETERMINER LEXICON))))
(sb__primconcept INDEFINITE (:list
             (supers (:list DETERMINER))))
(sb__defconcept EIN (:list
            (supers (:list INDEFINITE LEXICON))))
(sb__primconcept CARDINAL (:list
             (supers (:list INDEFINITE))))
(sb__defconcept number35 (:list
            (supers (:list CARDINAL LEXICON))))
(sb__primconcept number50 (:list
             (supers (:list CARDINAL LEXICON))))
(sb__defconcept FUENFUNDDREISSIG (:list
            (supers (:list CARDINAL LEXICON))))
(sb__primconcept INTERROGATIVE (:list
             (supers (:list DETERMINER))))
(sb__primconcept DEFINITE (:list
             (supers (:list DETERMINER))))
(sb__primconcept DEMONSTRATIVE (:list
             (supers (:list DEFINITE))))
(sb__defconcept DER (:list
            (supers (:list DEFINITE LEXICON))))
(sb__defconcept DIE (:list
            (supers (:list DEFINITE LEXICON))))
(sb__primconcept POSSESSIVE (:list
             (supers (:list DEFINITE))))
(sb__defconcept DEM (:list
            (supers (:list LEXICON DEFINITE))))
(sb__defconcept MEIN (:list
            (supers (:list LEXICON DETERMINER))))
(sb__primconcept PROPERTY__FILLER (:list
             (supers (:list FSS))))
(sb__primconcept ADJECTIVE__PROPERTY (:list
             (supers (:list PROPERTY__FILLER))))
(sb__primconcept TRUTH__VALUE (:list
             (supers (:list ADJECTIVE__PROPERTY))))
(sb__defconcept ABSTRACT__THING (:list
            (supers (:list THING))
            (nr (sb__primelemrole TRUTH__MOD (domain-range ABSTRACT__THING TRUTH__VALUE bot)) 1 1 1)))
(sb__primconcept NAME (:list
             (supers (:list ABSTRACT__THING))))
(sb__defconcept THING (:list
            (supers (:list PROPERTY__FILLER))
            (nr (sb__primelemrole DET (domain-range THING DETERMINER bot)) 1 1 1)
            (nr (sb__primelemrole DEICTIC__MOD (domain-range THING POINTING bot)) 1 1 1)
            (nr (sb__primelemrole NAMED (domain-range THING NAME bot)) 1 1 1)))
(sb__defconcept PREDICATE (:list
            (supers (:list FSS))
            (nr (sb__primelemrole SUBJECT (domain-range PREDICATE THING bot)) 1 1 1)
            (nr (sb__primelemrole PURPOSE (domain-range PREDICATE PREDICATE bot)) 1 1 1)
            (nr (sb__primelemrole TIME (domain-range PREDICATE TIME bot)) 1 1 1)
            (nr (sb__primelemrole ILLOC (domain-range PREDICATE SPEECH__ACT bot)) 1 1 1)
            (nr (sb__primelemrole CAUSE (domain-range PREDICATE PREDICATE bot)) 1 1 1)
            (nr (sb__primelemrole RESULT (domain-range PREDICATE THING bot)) 1 1 1)
            (nr (sb__primelemrole LOCATION (domain-range PREDICATE THING bot)) 1 1 1)
            (necres SUBJECT NEC)))
(sb__primconcept STATE (:list
             (supers (:list PREDICATE))
             (restrict-inh TIME__STATE (restricts TIME (range PERIOD bot)))))
(sb__attributes STATE 
            SUBJECT 
            (ROLE__TO__CASE NOMINATIV))
(sb__attributes STATE 
            LOCATION 
            (ROLE__TO__CASE (PREP__STEMS IN)))
(sb__defconcept HUMAN (:list
            (supers (:list ANIMATE))
            (nr (sb__primelemrole VOLITION (domain-range HUMAN VOLITIONAL__SQ bot)) 1 1 1)))
(sb__defconcept GEOGRAPHICAL__OBJECT (:list
            (supers (:list INANIMATE))
            (nr (sb__primelemrole ORIGIN__MOD (domain-range GEOGRAPHICAL__OBJECT ORIGIN bot)) 1 1 1)))
(sb__primconcept WOHN (:list
             (supers (:list LEXICON STATE))
             (restrict-inh AGENT (restricts SUBJECT (range HUMAN THING)))
             (restrict-inh LOCATION__WOHN (restricts LOCATION (range GEOGRAPHICAL__OBJECT bot)))))
(sb__attributes WOHN 
            AGENT 
            (ROLE__TO__CASE NOMINATIV) )
(sb__attributes WOHN 
            LOCATION 
            (ROLE__TO__CASE (PREP__STEMS IN)) )
(sb__primconcept QUALITATIVE (:list
             (supers (:list ADJECTIVE__PROPERTY))))
(sb__primconcept QUALITY (:list
             (supers (:list QUALITATIVE))))
(sb__primconcept COLOUR (:list
             (supers (:list QUALITY))))
(sb__defconcept CONCRETE__THING (:list
            (supers (:list THING))
            (nr (sb__primelemrole COLOUR__MOD (domain-range CONCRETE__THING COLOUR bot)) 1 1 1)))
(sb__primconcept RELATION (:list
             (supers (:list ADJECTIVE__PROPERTY))))
(sb__defconcept INDIVIDUAL (:list
            (supers (:list CONCRETE__THING))
            (nr (sb__primelemrole RELATIVE__MOD (domain-range INDIVIDUAL RELATION bot)) 1 1 1)))
(sb__defconcept MASS__NOUN (:list
            (supers (:list CONCRETE__THING))))
(sb__primconcept MATERIAL (:list
             (supers (:list MASS__NOUN))))
(sb__defconcept INANIMATE (:list
            (supers (:list INDIVIDUAL))
            (nr (sb__primelemrole MATERIAL__MOD (domain-range INANIMATE MATERIAL bot)) 1 1 1)))
(sb__primconcept WEIGHT (:list
             (supers (:list QUALITY))))
(sb__defconcept MOTION (:list
            (supers (:list ACTION))
            (nr (sb__primelemrole DESTINATION (domain-range MOTION GEOGRAPHICAL__OBJECT bot)) 1 1 1)
            (nr (sb__primelemrole SOURCE (domain-range MOTION GEOGRAPHICAL__OBJECT bot)) 1 1 1)))
(sb__attributes MOTION 
            AGENT 
            (ROLE__TO__CASE NOMINATIV) )
(sb__attributes MOTION 
            LOCATION 
            (ROLE__TO__CASE DATIV) )
(sb__defconcept MOTION__BY__MEANS (:list
            (supers (:list MOTION))
            (necres (sb__primelemrole MEANS (domain-range MOTION__BY__MEANS TOUCHABLE__OBJECT TOUCHABLE__OBJECT)) NEC)))
(sb__attributes MOTION__BY__MEANS 
            DESTINATION 
            (ROLE__TO__CASE (PREP__STEMS NACH)) )
(sb__attributes MOTION__BY__MEANS 
            SOURCE 
            (ROLE__TO__CASE (PREP__STEMS VON)) )
(sb__attributes MOTION__BY__MEANS 
            AGENT 
            (ROLE__TO__CASE NOMINATIV))
(sb__attributes MOTION__BY__MEANS 
            MEANS 
            (ROLE__TO__CASE (PREP__STEMS MIT)) )
(sb__defconcept FAHR (:list
            (supers (:list LEXICON MOTION__BY__MEANS))))
(sb__attributes FAHR 
            SOURCE 
            (ROLE__TO__CASE (PREP__STEMS VON)) )
(sb__attributes FAHR 
            AGENT 
            (ROLE__TO__CASE NOMINATIV) )
(sb__attributes FAHR 
            DESTINATION 
            (ROLE__TO__CASE (PREP__STEMS NACH)) )
(sb__attributes FAHR 
            MEANS 
            (ROLE__TO__CASE (PREP__STEMS MIT)) )
(sb__defconcept GEH (:list
            (supers (:list LEXICON MOTION))))
(sb__defconcept TREFF (:list
            (supers (:list LEXICON ACTION))))
(sb__defconcept WERF (:list
            (supers (:list LEXICON ACTION))
            (necres CONCERNED NEC)))
(sb__defconcept CAUSATIVE (:list
            (supers (:list ACTION))))
(sb__defconcept PRODUCTIVE (:list
            (supers (:list CAUSATIVE))
            (necres (sb__primelemrole RESULT (domain-range PRODUCTIVE THING THING)) NEC)))
(sb__defconcept WRITE (:list
            (supers (:list PRODUCTIVE))
            (necres (sb__primelemrole CONCERNED (domain-range WRITE THING THING)) NEC)))
(sb__primconcept CANVAS (:list
             (supers (:list TOUCHABLE__OBJECT))))
(sb__defconcept ENTER (:list
            (supers (:list WRITE))
            (restrict-inh LOCATION__ENTER (restricts LOCATION (range CANVAS CANVAS)))))
(sb__defconcept ARBEIT (:list
            (supers (:list LEXICON ACTION))))
(sb__primconcept REPEAT (:list
             (supers (:list ACTION))))
(sb__attributes REPEAT 
            RESULT 
            (ROLE__TO__CASE AKKUSATIV) )
(sb__primconcept WIEDERHOL (:list
             (supers (:list REPEAT LEXICON))))
(sb__attributes WIEDERHOL 
            RESULT 
            (ROLE__TO__CASE AKKUSATIV) )
(sb__defconcept TRANSACTION (:list
            (supers (:list ACTION))
            (nr (sb__primelemrole BENEFICATIVE (domain-range TRANSACTION HUMAN bot)) 1 1 1)))
(sb__defconcept SCHENK (:list
            (supers (:list LEXICON TRANSACTION))
            (necres CONCERNED NEC)))
(sb__attributes SCHENK 
            BENEFICATIVE 
            (ROLE__TO__CASE DATIV) )
(sb__primconcept ZAHL (:list
             (supers (:list LEXICON TRANSACTION))))
(sb__defconcept KAUF (:list
            (supers (:list LEXICON TRANSACTION))
            (necres CONCERNED NEC)))
(sb__attributes KAUF 
            LOCATION 
            (ROLE__TO__CASE (PREP__STEMS IN)) )
(sb__attributes KAUF 
            BENEFICATIVE 
            (ROLE__TO__CASE DATIV) )
(sb__attributes KAUF 
            RESULT 
            (ROLE__TO__CASE AKKUSATIV) )
(sb__attributes KAUF 
            AGENT 
            (ROLE__TO__CASE NOMINATIV) )
(sb__primconcept REASON (:list
             (supers (:list ACTION))))
(sb__attributes REASON 
            AGENT 
            (ROLE__TO__CASE NOMINATIV) )
(sb__attributes REASON 
            RESULT 
            (ROLE__TO__CASE AKKUSATIV) )
(sb__primconcept VERURSACH (:list
             (supers (:list LEXICON REASON))))
(sb__attributes VERURSACH 
            AGENT 
            (ROLE__TO__CASE NOMINATIV) )
(sb__attributes VERURSACH 
            RESULT 
            (ROLE__TO__CASE AKKUSATIV) )
(sb__defconcept UNTERRICHT (:list
            (supers (:list PREDICATE LEXICON))))
(sb__primconcept PROPERTY (:list
             (supers (:list PREDICATE))))
(sb__primelemrole HAS__PROPERTY 
              (domain-range PROPERTY PROPERTY__FILLER bot))
(sb__attributes PROPERTY 
            RESULT 
            (ROLE__TO__CASE PRAEDIKATSNOMINATIV) )
(sb__attributes PROPERTY 
            SUBJECT 
            (ROLE__TO__CASE NOMINATIV) )
(sb__attributes PROPERTY 
            HAS__PROPERTY 
            (ROLE__TO__CASE ADJEKTIVALERGAENZUNG) )
(sb__defconcept VALUE__PROPERTY (:list
            (supers (:list PROPERTY))
            (nr (sb__primelemrole MEASURE (domain-range VALUE__PROPERTY ABSTRACT__THING bot)) 1 1 1)))
(sb__defconcept COST (:list
            (supers (:list VALUE__PROPERTY))
            (necres (sb__primelemrole MEASURE (domain-range COST ABSTRACT__THING ABSTRACT__THING)) NEC)))
(sb__attributes COST 
            MEASURE 
            (ROLE__TO__CASE AKKUSATIV))
(sb__attributes COST 
            SUBJECT 
            (ROLE__TO__CASE NOMINATIV) )
(sb__defconcept KOST (:list
            (supers (:list COST LEXICON))))
(sb__attributes KOST 
            SUBJECT 
            (ROLE__TO__CASE NOMINATIV) )
(sb__attributes KOST 
            MEASURE 
            (ROLE__TO__CASE AKKUSATIV) )
(sb__primconcept DEDUCTE (:list
             (supers (:list PROPERTY))))
(sb__primconcept ABSETZBAR (:list
             (supers (:list DEDUCTE LEXICON))))
(sb__primconcept SEIN (:list
             (supers (:list LEXICON PROPERTY))))
(sb__attributes SEIN 
            SUBJECT 
            (ROLE__TO__CASE NOMINATIV) )
(sb__attributes SEIN 
            RESULT 
            (ROLE__TO__CASE PRAEDIKATSNOMINATIV) )
(sb__primconcept BESITZEN (:list
             (supers (:list LEXICON PROPERTY))))
(sb__attributes BESITZEN 
            SUBJECT 
            (ROLE__TO__CASE NOMINATIV) )
(sb__attributes BESITZEN 
            HAS__PROPERTY 
            (ROLE__TO__CASE ADJEKTIVALERGAENZUNG) )
(sb__attributes BESITZEN 
            RESULT 
            (ROLE__TO__CASE AKKUSATIV) )
(sb__primconcept HABEN (:list
             (supers (:list LEXICON PROPERTY))
             (restrict-inh HAS__PROPERTY__HABEN (restricts HAS__PROPERTY (range THING bot)))))
(sb__primconcept ORIGIN (:list
             (supers (:list ADJECTIVE__PROPERTY))))
(sb__primconcept STATE__Q (:list
             (supers (:list QUALITATIVE))))
(sb__primconcept VOLITIONAL__SQ (:list
             (supers (:list STATE__Q))))
(sb__primconcept PHYSICAL__SQ (:list
             (supers (:list STATE__Q))))
(sb__primconcept KLEIN (:list
             (supers (:list LEXICON PHYSICAL__SQ))))
(sb__primconcept GROSS (:list
             (supers (:list LEXICON PHYSICAL__SQ))))
(sb__primconcept ROT (:list
             (supers (:list COLOUR LEXICON))))
(sb__primconcept WORTH (:list
             (supers (:list QUALITY))))
(sb__primconcept VOELKLINGEN (:list
             (supers (:list NAME))))
(sb__primconcept GI (:list
             (supers (:list ABSTRACT__THING LEXICON))))
(sb__defconcept INDICATION__OF__QUANTITY (:list
            (supers (:list ABSTRACT__THING))
            (restrict-inh QUANTITY (restricts DET (range CARDINAL CARDINAL)))
            (necres QUANTITY NEC)))
(sb__defconcept DM (:list
            (supers (:list INDICATION__OF__QUANTITY LEXICON))))
(sb__primconcept PROFESSION (:list
             (supers (:list ABSTRACT__THING))))
(sb__primconcept SCHREINER (:list
             (supers (:list PROFESSION LEXICON))))
(sb__primconcept INFORMATIKER (:list
             (supers (:list PROFESSION LEXICON))))
(sb__primconcept ACTION__CONTENT (:list
             (supers (:list ABSTRACT__THING ACTION))))
(sb__primconcept MOTION__CONTENT (:list
             (supers (:list ACTION__CONTENT MOTION))))
(sb__primconcept MOTION__BY__MEANS__CONTENT (:list
             (supers (:list MOTION__BY__MEANS ACTION__CONTENT))))
(sb__primconcept COST (:list
             (supers (:list ABSTRACT__THING))))
(sb__primconcept KOSTEN (:list
             (supers (:list LEXICON COST))))
(sb__primconcept GELD (:list
             (supers (:list LEXICON COST))))
(sb__primconcept PROFESSION (:list
             (supers (:list ABSTRACT__THING))))
(sb__primconcept INFORMATIKER (:list
             (supers (:list LEXICON PROFESSION))))
(sb__primconcept TAX__ACTION (:list
             (supers (:list ABSTRACT__THING))))
(sb__primconcept STEUERHANDLUNG (:list
             (supers (:list LEXICON TAX__ACTION))))
(sb__primconcept NUMBER (:list
             (supers (:list ABSTRACT__THING))))
(sb__defconcept FAHRT (:list
            (supers (:list THING LEXICON))))
(sb__defconcept DAS (:list
            (supers (:list THING LEXICON))))
(sb__defconcept ANIMATE (:list
            (supers (:list INDIVIDUAL))
            (nr (sb__primelemrole PHYSIS__MOD (domain-range ANIMATE PHYSICAL__SQ bot)) 1 1 1)))
(sb__primconcept FRAU (:list
             (supers (:list HUMAN LEXICON))))
(sb__defconcept MANN (:list
            (supers (:list HUMAN LEXICON))))
(sb__attributes MANN 
            PHYSIS__MOD 
            (ROLE__TO__CASE ADJ) )
(sb__primconcept SIE (:list
             (supers (:list HUMAN LEXICON))))
(sb__defconcept JUNGE (:list
            (supers (:list HUMAN LEXICON))))
(sb__primconcept SIE (:list
             (supers (:list LEXICON HUMAN))))
(sb__primconcept PERSON (:list
             (supers (:list LEXICON HUMAN))))
(sb__defconcept KARL (:list
            (supers (:list LEXICON HUMAN))))
(sb__defconcept ICH (:list
            (supers (:list LEXICON HUMAN))))
(sb__defconcept PETER (:list
            (supers (:list LEXICON HUMAN))))
(sb__primconcept PLANT (:list
             (supers (:list ANIMATE))))
(sb__defconcept ANIMAL (:list
            (supers (:list ANIMATE))
            (nr (sb__primelemrole VOLITION (domain-range ANIMAL VOLITIONAL__SQ bot)) 1 1 1)))
(sb__primconcept TOWN (:list
             (supers (:list GEOGRAPHICAL__OBJECT))))
(sb__defconcept VOELKLINGEN (:list
            (supers (:list TOWN LEXICON))))
(sb__attributes VOELKLINGEN 
            NAMED 
            (ANNOTATE__FILLER T) )
(sb__defconcept SAARBRUECKEN (:list
            (supers (:list TOWN LEXICON))))
(sb__attributes SAARBRUECKEN 
            NAMED 
            (ANNOTATE__FILLER T) )
(sb__defconcept DUDWEILER (:list
            (supers (:list TOWN LEXICON))))
(sb__attributes DUDWEILER 
            NAMED 
            (ANNOTATE__FILLER T) )
(sb__defconcept SAARLOUIS (:list
            (supers (:list LEXICON TOWN))))
(sb__attributes SAARLOUIS 
            NAMED 
            (ANNOTATE__FILLER T) )
(sb__primconcept BERLIN (:list
             (supers (:list LEXICON TOWN))))
(sb__defconcept WALD (:list
            (supers (:list GEOGRAPHICAL__OBJECT LEXICON))))
(sb__defconcept HIER (:list
            (supers (:list GEOGRAPHICAL__OBJECT LEXICON))))
(sb__defconcept ORT (:list
            (supers (:list LEXICON GEOGRAPHICAL__OBJECT))))
(sb__primconcept INFORMATION (:list
             (supers (:list INANIMATE))))
(sb__primconcept STRING (:list
             (supers (:list INFORMATION))))
(sb__primconcept SYSTEM (:list
             (supers (:list HUMAN INANIMATE))))
(sb__defconcept VEHICLE (:list
            (supers (:list TOUCHABLE__OBJECT))
            (nr (sb__primelemrole WORTH__MOD (domain-range VEHICLE WORTH bot)) 1 1 1)))
(sb__defconcept FAHRRAD (:list
            (supers (:list VEHICLE LEXICON))))
(sb__primconcept BUS (:list
             (supers (:list VEHICLE))))
(sb__defconcept BUS (:list
            (supers (:list BUS LEXICON))))
(sb__primconcept MOTORRAD (:list
             (supers (:list VEHICLE LEXICON))))
(sb__primconcept MOTORRAD (:list
             (supers (:list VEHICLE LEXICON))))
(sb__attributes MOTORRAD 
            COLOUR__MOD 
            (ROLE__TO__CASE ADJ) )
(sb__defconcept BUCH (:list
            (supers (:list LEXICON TOUCHABLE__OBJECT))))
(sb__primconcept KOCHBUCH (:list
             (supers (:list LEXICON TOUCHABLE__OBJECT))))
(sb__primconcept RESULT (:list
             (supers (:list TOUCHABLE__OBJECT))))
(sb__defconcept AUKTION (:list
            (supers (:list LEXICON THING))))
(sb__defconcept WAS (:list
            (supers (:list LEXICON THING))))))
