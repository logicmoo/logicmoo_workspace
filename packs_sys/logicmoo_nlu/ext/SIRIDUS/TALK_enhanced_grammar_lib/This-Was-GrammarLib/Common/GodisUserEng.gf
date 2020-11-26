--# -path=.:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/english

concrete GodisUserEng of GodisUser = 
    open Prelude, GodisLangEng in {

lincat 

S = SS;

Question = UserQuestion;
Action   = UserAction;
Answer   = UserAnswer;
ShortAns = UserShortAns;

lin

quit_S = variants{ss ["good bye"]; ss "quit"};

request_S  x = x;
answer_S   x = x;
ask_S      x = x;
shortans_S x = x;

}
