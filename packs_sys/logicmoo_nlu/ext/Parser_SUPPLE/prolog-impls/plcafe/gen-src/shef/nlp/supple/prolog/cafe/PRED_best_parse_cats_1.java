package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_best_parse_cats_1.java
 * @procedure best_parse_cats/1 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_best_parse_cats_1 extends Predicate {
    static Predicate best_parse_cats_1_1 = new PRED_best_parse_cats_1_1();
    static Predicate best_parse_cats_1_2 = new PRED_best_parse_cats_1_2();
    static Predicate best_parse_cats_1_var = new PRED_best_parse_cats_1_var();

    public Term arg1;

    public PRED_best_parse_cats_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_best_parse_cats_1(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.switch_on_term(
                                   best_parse_cats_1_var,
                                   best_parse_cats_1_1,
                                   best_parse_cats_1_var,
                                   best_parse_cats_1_1,
                                   best_parse_cats_1_1
                                   );
    }

    public int arity() { return 1; }

    public String toString() {
        return "best_parse_cats(" + arg1 + ")";
    }
}

class PRED_best_parse_cats_1_var extends PRED_best_parse_cats_1 {
    static Predicate best_parse_cats_1_var_1 = new PRED_best_parse_cats_1_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(best_parse_cats_1_1, best_parse_cats_1_var_1);
    }
}

class PRED_best_parse_cats_1_var_1 extends PRED_best_parse_cats_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust(best_parse_cats_1_2);
    }
}

class PRED_best_parse_cats_1_1 extends PRED_best_parse_cats_1 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("best_parse_cats", 1);
    static SymbolTerm f3 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s4 = SymbolTerm.makeSymbol("shef.nlp.supple.prolog.cafe");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        Term[] h2 = {a1};
        a2 = new StructureTerm(f1, h2);
        a3 = new VariableTerm(engine);
        Term[] h5 = {s4, a3};
        a4 = new StructureTerm(f3, h5);
        p1 = new PRED_translated_goal_1(a4, cont);
        return new PRED_clause_2(a2, a3, p1);
    }
}

class PRED_best_parse_cats_1_2 extends PRED_best_parse_cats_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("dummy");

    public Predicate exec(Prolog engine) {
        Term a1;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        return cont;
    }
}

