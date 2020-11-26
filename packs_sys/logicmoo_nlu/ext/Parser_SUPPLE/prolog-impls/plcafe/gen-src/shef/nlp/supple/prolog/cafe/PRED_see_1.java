package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_see_1.java
 * @procedure see/1 in supple_utils.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_see_1 extends Predicate {
    static Predicate see_1_1 = new PRED_see_1_1();
    static Predicate see_1_2 = new PRED_see_1_2();
    static Predicate see_1_sub_1 = new PRED_see_1_sub_1();

    public Term arg1;

    public PRED_see_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_see_1(){}
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
        return engine.jtry(see_1_1, see_1_sub_1);
    }

    public int arity() { return 1; }

    public String toString() {
        return "see(" + arg1 + ")";
    }
}

class PRED_see_1_sub_1 extends PRED_see_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust(see_1_2);
    }
}

class PRED_see_1_1 extends PRED_see_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("read");

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        Predicate p1;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        a2 = new VariableTerm(engine);
        p1 = new PRED_set_input_1(a2, cont);
        return new PRED_open_3(a1, s1, a2, p1);
    }
}

class PRED_see_1_2 extends PRED_see_1 {

    public Predicate exec(Prolog engine) {
        Term a1;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        return new PRED_set_input_1(a1, cont);
    }
}

