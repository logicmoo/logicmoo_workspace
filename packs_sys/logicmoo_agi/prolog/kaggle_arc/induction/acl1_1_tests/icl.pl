/* icl.pl: 

Starting from the output of the i(Rules,Delta), it produces the following input files for ICL
        the file <file_stem>.kb containing the positive and negative
                interpretations for learning constraints starting from the
                abduced literals in Delta
        the file <file_stem>.s containing the settings, obtained copying a 
                file prototype.s containing default settings

The file <file_stem>.kb contains:
        one positive interpretation (or model) containing all the abducible
                abduced for positive example
        one negative interpretation (or model) for each abducible
                abduced for every negative example containing the complement of that 
                abducible

*/
:-use_module(library(system)).

call_ICL(FileString,Delta):-
        write_kb(FileString,Delta),
        write_s(FileString).


write_kb(FileString,Delta):-
        append(FileString,".kb",FileStringKb),
        name(FileKb,FileStringKb),
        open(FileKb,write,Stream),
        assert(new_model_number(0)),
        write_neg_models(Stream,Delta),
        format(Stream,"begin(model(positive)).~Npos.~N ~N",[]),
        write_pos_model(Stream,Delta),
        format(Stream,"end(model(positive)).~N ~N",[]),
        close(Stream),
        retract(new_model_number(_)).

write_neg_models(_Stream,[]):-!.

write_neg_models(Stream,[[not(_EX)|Abduced]|RestModels]):-!,
        write_facts_neg(Stream,Abduced),
        write_neg_models(Stream,RestModels).

write_neg_models(Stream,[[_EX|_Abduced]|RestModels]):-
        write_neg_models(Stream,RestModels).

write_pos_model(_Stream,[]):-!.

write_pos_model(Stream,[[not(_EX)|_Abduced]|RestModels]):-!,
        write_pos_model(Stream,RestModels).

write_pos_model(Stream,[[_EX|Abduced]|RestModels]):-
        write_facts_pos(Stream,Abduced),
               write_pos_model(Stream,RestModels).
               
atomize(not(EX),AtomicEX):-!,
/* we assume that EX is of the form p(a,b,c..) where a,b,c.. are atomic */
        EX=..List,
        new_number(N),
        append([not|List],[N],List1),
        concat(List1,"",String),
        name(AtomicEX,String).

atomize(EX,AtomicEX):-
/* we assume that EX is of the form p(a,b,c..) where a,b,c.. are atomic */
        EX=..List,
        new_number(N),
        append(List,[N],List1),
        concat(List1,"",String),
        name(AtomicEX,String).

new_number(N):-
        retract(new_model_number(N)),
        N1 is N + 1,
        assert(new_model_number(N1)).

concat([H],Input,Output):-!,
        name(H,Hstr),
        append(Input,Hstr,Output).

concat([H|T],Input,Output):-
        name(H,Hstr),
        append(Hstr,"_",Hstr1),
        append(Input,Hstr1,Input1),
        concat(T,Input1,Output).

write_facts_neg(_Stream,[]):-!.

write_facts_neg(Stream,[A|Rest]):-
        neg(A,NA),
        atomize(NA,NAatm),
        format(Stream,"begin(model(~p)).~Nneg.~N ~N",[NAatm]),
        format(Stream,"~p.~N",[NA]),
        format(Stream,"end(model(~p)).~N ~N",[NAatm]),
        write_facts_neg(Stream,Rest).

write_facts_pos(_Stream,[]):-!.

write_facts_pos(Stream,[A|Rest]):-
        format(Stream,"~p.~N",[A]),
        write_facts_pos(Stream,Rest).

write_s(FileString):-
        append(FileString,".s",FileS),
        append("cp prototype.s ",FileS,CommandString),
        name(Command,CommandString),
        system(Command).
        
