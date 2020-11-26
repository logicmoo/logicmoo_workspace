--# -path=.:prelude

abstract GodisUser = {

cat 

S;
Question;
Action;
Answer;
ShortAns;

fun

quit_S     : S;

request_S  : Action -> S;
answer_S   : Answer -> S;
ask_S      : Question -> S;
shortans_S : ShortAns -> S;

}
