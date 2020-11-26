package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_$dummy_supple46pl_5_1.java
 * @procedure $dummy_supple.pl_5/1 in supple.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_$dummy_supple46pl_5_1 extends Predicate {
    static Predicate $dummy_supple46pl_5_1_1 = new PRED_$dummy_supple46pl_5_1_1();
    static Predicate $dummy_supple46pl_5_1_2 = new PRED_$dummy_supple46pl_5_1_2();
    static Predicate $dummy_supple46pl_5_1_sub_1 = new PRED_$dummy_supple46pl_5_1_sub_1();

    public Term arg1;

    public PRED_$dummy_supple46pl_5_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_$dummy_supple46pl_5_1(){}
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
        return engine.jtry($dummy_supple46pl_5_1_1, $dummy_supple46pl_5_1_sub_1);
    }

    public int arity() { return 1; }

    public String toString() {
        return "$dummy_supple.pl_5(" + arg1 + ")";
    }
}

class PRED_$dummy_supple46pl_5_1_sub_1 extends PRED_$dummy_supple46pl_5_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust($dummy_supple46pl_5_1_2);
    }
}

class PRED_$dummy_supple46pl_5_1_1 extends PRED_$dummy_supple46pl_5_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("");

    public Predicate exec(Prolog engine) {
        Term a1;
        Predicate p1;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        p1 = new PRED_buchart_gensym_2(s1, a1, cont);
        return new PRED_var_1(a1, p1);
    }
}

class PRED_$dummy_supple46pl_5_1_2 extends PRED_$dummy_supple46pl_5_1 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

