--# -path=.:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/swedish:resource-1.0/scandinavian

concrete GodisUserSwe of GodisUser = 
    open Prelude, GodisLangSwe in {

lincat 

S = SS;

Question = UserQuestion;
Answer   = UserAnswer;
ShortAns = UserShortAns;
Action   = UserAction;

lin

quit_S = ss ["hejdå"];

answer_S   x = x;
ask_S      x = x;
shortans_S x = x;
request_S  x = x;

}
