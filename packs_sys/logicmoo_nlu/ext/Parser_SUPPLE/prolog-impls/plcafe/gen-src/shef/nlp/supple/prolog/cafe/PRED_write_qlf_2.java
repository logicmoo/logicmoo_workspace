package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_write_qlf_2.java
 * @procedure write_qlf/2 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_write_qlf_2 extends Predicate {
    static Predicate write_qlf_2_1 = new PRED_write_qlf_2_1();
    static Predicate write_qlf_2_2 = new PRED_write_qlf_2_2();
    static Predicate write_qlf_2_sub_1 = new PRED_write_qlf_2_sub_1();

    public Term arg1, arg2;

    public PRED_write_qlf_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_write_qlf_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(write_qlf_2_1, write_qlf_2_sub_1);
    }

    public int arity() { return 2; }

    public String toString() {
        return "write_qlf(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_write_qlf_2_sub_1 extends PRED_write_qlf_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(write_qlf_2_2);
    }
}

class PRED_write_qlf_2_1 extends PRED_write_qlf_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a2, engine.trail) ) return engine.fail();
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_write_qlf_2_2 extends PRED_write_qlf_2 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a2.isList() ){
            a3 = ((ListTerm)a2).car();
            a4 = ((ListTerm)a2).cdr();
        } else if ( a2.isVariable() ){
            a3 = new VariableTerm(engine);
            a4 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(a3, a4), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        p1 = new PRED_write_qlf_2(a1, a4, cont);
        p2 = new PRED_nl_1(a1, p1);
        return new PRED_write_canonical_2(a1, a3, p2);
    }
}

