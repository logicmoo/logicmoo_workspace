package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_memberchk_2.java
 * @procedure memberchk/2 in supple_utils.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_memberchk_2 extends Predicate {
    static PRED_memberchk_2 entry_code;
    static Predicate memberchk_2_1 = new PRED_memberchk_2_1();
    static Predicate memberchk_2_2 = new PRED_memberchk_2_2();
    static Predicate memberchk_2_sub_1 = new PRED_memberchk_2_sub_1();

    public Term arg1, arg2;

    public PRED_memberchk_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_memberchk_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        entry_code = this;
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(memberchk_2_1, memberchk_2_sub_1);
    }

    public int arity() { return 2; }

    public String toString() {
        return "memberchk(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_memberchk_2_sub_1 extends PRED_memberchk_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(memberchk_2_2);
    }
}

class PRED_memberchk_2_1 extends PRED_memberchk_2 {

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a2.isList() ){
            if ( !a1.unify(((ListTerm)a2).car(), engine.trail) )
                return engine.fail();
        } else if ( a2.isVariable() ){
            if ( !a2.unify(new ListTerm(a1, new VariableTerm(engine)), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_memberchk_2_2 extends PRED_memberchk_2 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a2.isList() ){
            a3 = ((ListTerm)a2).cdr();
        } else if ( a2.isVariable() ){
            a3 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(new VariableTerm(engine), a3), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        engine.aregs[1] = a1;
        engine.aregs[2] = a3;
        engine.cont = cont;
        return entry_code.call(engine);
    }
}

