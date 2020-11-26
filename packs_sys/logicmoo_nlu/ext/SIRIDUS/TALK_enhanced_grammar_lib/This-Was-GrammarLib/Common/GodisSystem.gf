--# -path=.:prelude

abstract GodisSystem = PredefAbs ** {

cat

Action;
Reason;

Proposition;

ShortAns;

Question;
ListYNQ;

Move; 
QMove; 
ListMove; 
S;


fun

whqAction,
whqIssue    : Question;

ynq         : Proposition -> Question;

ynqBase     : Proposition -> ListYNQ;
ynqCons     : Proposition -> ListYNQ -> ListYNQ;
altq        : ListYNQ -> Question;

not         : Proposition -> Proposition;
fail        : Question -> Reason -> Proposition;
done        : Action -> Proposition;
issue       : Question -> Proposition;
action      : Action -> Proposition;

notS        : ShortAns -> ShortAns;

answer_yes, 
answer_no   : Move;

greet, 
quit        : Move;
ask         : Question -> QMove;
answer      : Proposition -> Move;
shortAns    : ShortAns -> Move;

request,
confirm     : Action -> Move;
reportFailure : Action -> Reason -> Move;

icm_acc_pos,
icm_con_neg,
icm_per_neg,
icm_sem_neg,
icm_und_neg,
icm_reraise,
icm_loadplan,
icm_accommodate       : Move;

icm_per_pos           : String -> Move;
icm_sem_pos_prop      : Proposition -> Move;
icm_sem_pos_shortAns  : ShortAns -> Move;
icm_und_pos,
icm_und_int           : Proposition -> Move;
icm_reraise_act       : Action -> Move;
icm_reraise_que,
icm_accommodate_que,
icm_reaccommodate_que : Question -> QMove;

BaseMove              : Move -> ListMove;
ConsMove              : Move -> ListMove -> ListMove;
BaseQMove             : QMove -> ListMove;
ConsQMove             : QMove -> ListMove -> ListMove;

godis_utterance       : ListMove -> S;

}
