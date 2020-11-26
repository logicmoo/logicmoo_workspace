package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_compile_last_grammar_1.java
 * @procedure compile_last_grammar/1 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_compile_last_grammar_1 extends Predicate {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("best_parse_cats", 2);
    static SymbolTerm f3 = SymbolTerm.makeSymbol("best_parse_cats", 1);

    public Term arg1;

    public PRED_compile_last_grammar_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_compile_last_grammar_1(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4;
        Predicate p1, p2, p3;
        a1 = arg1.dereference();

        a2 = new VariableTerm(engine);
        Term[] h2 = {a1, a2};
        a3 = new StructureTerm(f1, h2);
        Term[] h4 = {new VariableTerm(engine)};
        a4 = new StructureTerm(f3, h4);
        p1 = new PRED_retractall_1(a4, cont);
        p2 = new PRED_compile_grammar3_1(a1, p1);
        p3 = new PRED_assert_1(a3, p2);
        return new PRED_best_parse_cats_1(a2, p3);
    }

    public int arity() { return 1; }

    public String toString() {
        return "compile_last_grammar(" + arg1 + ")";
    }
}

