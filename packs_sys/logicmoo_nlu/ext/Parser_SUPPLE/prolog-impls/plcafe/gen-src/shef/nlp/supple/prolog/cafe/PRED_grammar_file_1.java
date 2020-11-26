package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_grammar_file_1.java
 * @procedure grammar_file/1 in dynamic.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_grammar_file_1 extends Predicate {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("grammar_file", 1);
    static SymbolTerm f3 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s4 = SymbolTerm.makeSymbol("shef.nlp.supple.prolog.cafe");

    public Term arg1;

    public PRED_grammar_file_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_grammar_file_1(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4;
        Predicate p1;
        a1 = arg1.dereference();

        Term[] h2 = {a1};
        a2 = new StructureTerm(f1, h2);
        a3 = new VariableTerm(engine);
        Term[] h5 = {s4, a3};
        a4 = new StructureTerm(f3, h5);
        p1 = new PRED_translated_goal_1(a4, cont);
        return new PRED_clause_2(a2, a3, p1);
    }

    public int arity() { return 1; }

    public String toString() {
        return "grammar_file(" + arg1 + ")";
    }
}

