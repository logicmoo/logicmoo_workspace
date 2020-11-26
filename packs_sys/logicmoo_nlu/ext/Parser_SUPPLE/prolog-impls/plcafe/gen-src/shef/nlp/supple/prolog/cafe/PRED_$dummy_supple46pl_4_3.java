package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_$dummy_supple46pl_4_3.java
 * @procedure $dummy_supple.pl_4/3 in supple.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_$dummy_supple46pl_4_3 extends Predicate {
    static Predicate $dummy_supple46pl_4_3_1 = new PRED_$dummy_supple46pl_4_3_1();
    static Predicate $dummy_supple46pl_4_3_2 = new PRED_$dummy_supple46pl_4_3_2();
    static Predicate $dummy_supple46pl_4_3_sub_1 = new PRED_$dummy_supple46pl_4_3_sub_1();

    public Term arg1, arg2, arg3;

    public PRED_$dummy_supple46pl_4_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_$dummy_supple46pl_4_3(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry($dummy_supple46pl_4_3_1, $dummy_supple46pl_4_3_sub_1);
    }

    public int arity() { return 3; }

    public String toString() {
        return "$dummy_supple.pl_4(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_$dummy_supple46pl_4_3_sub_1 extends PRED_$dummy_supple46pl_4_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust($dummy_supple46pl_4_3_2);
    }
}

class PRED_$dummy_supple46pl_4_3_1 extends PRED_$dummy_supple46pl_4_3 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("edge");
    static SymbolTerm f3 = SymbolTerm.makeSymbol("offsets", 2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        Term[] h4 = {a1, a2};
        a5 = new StructureTerm(f3, h4);
        Term[] h5 = {s2, a5};
        a4 = new StructureTerm(f1, h5);
        return new PRED_member_2(a4, a3, cont);
    }
}

class PRED_$dummy_supple46pl_4_3_2 extends PRED_$dummy_supple46pl_4_3 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

