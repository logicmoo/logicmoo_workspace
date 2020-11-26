
resource Prolog = {

oper

PStr      : Type = {s : Str};
PPStr     : Type = PStr -> PStr;
PPPStr    : Type = PStr -> PStr -> PStr;
PPPPStr   : Type = PStr -> PStr -> PStr -> PStr;
PPPPPStr  : Type = PStr -> PStr -> PStr -> PStr -> PStr;
PPPPPPStr : Type = PStr -> PStr -> PStr -> PStr -> PStr -> PStr;

pStr     : Str -> PStr   = \str   -> {s = str};
pOper    : Str -> PPPStr = \f,x,y -> pStr (x.s ++ f ++ y.s);
pPrefix  : Str -> PPStr  = \f,x   -> pStr (f ++ x.s);
pPostfix : Str -> PPStr  = \f,x   -> pStr (x.s ++ f);
pEmbed   : Str -> Str -> PPStr = \f,g,x -> pStr (f ++ x.s ++ g);
pParen   : PPStr = pEmbed "(" ")";
pBrackets: PPStr = pEmbed "[" "]";

pNil   : PStr = pStr "''";
pEmpty : PStr = pStr [];

pp0 : Str -> PStr      = pStr;
pp1 : Str -> PPStr     = \pred,arg -> pPrefix pred (pParen arg);
pp2 : Str -> PPPStr    = \pred,arg1,arg2 -> pp1 pred (pSeq arg1 arg2);
pp3 : Str -> PPPPStr   = \pred,arg1,arg2 -> pp2 pred (pSeq arg1 arg2);
pp4 : Str -> PPPPPStr  = \pred,arg1,arg2 -> pp3 pred (pSeq arg1 arg2);
pp5 : Str -> PPPPPPStr = \pred,arg1,arg2 -> pp4 pred (pSeq arg1 arg2);

pList0  : PStr      = pBrackets pEmpty;
pList1  : PPStr     = \x -> pBrackets x;
pList2  : PPPStr    = \x,y -> pList1 (pSeq x y);
pList3  : PPPPStr   = \x,y -> pList2 (pSeq x y);
pList4  : PPPPPStr  = \x,y -> pList3 (pSeq x y);
pList5  : PPPPPPStr = \x,y -> pList4 (pSeq x y);

pSeq    : PPPStr   = pOper ",";

pConcat : PPPStr   = \x,y -> pStr (x.s ++ y.s);

-- the following operations are Godis-specific

pWhQ : Str -> PStr  = \pred -> pOper "^" pVar (pp1 pred pVar);
pVar : PStr = pStr "X";

pm0 : PStr = pList0;
pm1 : PPStr = pList1;
pm2 : PPPStr = pList2;
pm3 : PPPPStr = pList3;
pm4 : PPPPPStr = pList4;
pm5 : PPPPPPStr = pList5;

}
