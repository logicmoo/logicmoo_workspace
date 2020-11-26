--# -path=.:prelude

concrete GodisUserSem of GodisUser = 
    open Prolog in {

lincat 

S,
Question,
Answer,
ShortAns,
Action    = PStr;

lin

quit_S = pm1 (pStr "quit");

answer_S   x = x;
ask_S      x = x;
shortans_S x = x;
request_S  x = x;

}
