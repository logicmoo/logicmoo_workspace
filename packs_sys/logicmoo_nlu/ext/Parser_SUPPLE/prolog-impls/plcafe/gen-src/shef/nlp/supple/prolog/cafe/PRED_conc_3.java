package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_conc_3.java
 * @procedure conc/3 in supple_utils.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_conc_3 extends Predicate {
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate conc_3_1 = new PRED_conc_3_1();
    static Predicate conc_3_2 = new PRED_conc_3_2();
    static Predicate conc_3_var = new PRED_conc_3_var();

    public Term arg1, arg2, arg3;

    public PRED_conc_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_conc_3(){}
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
        return engine.switch_on_term(
                                   conc_3_var,
                                   fail_0,
                                   conc_3_1,
                                   fail_0,
                                   conc_3_2
                                   );
    }

    public int arity() { return 3; }

    public String toString() {
        return "conc(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_conc_3_var extends PRED_conc_3 {
    static Predicate conc_3_var_1 = new PRED_conc_3_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(conc_3_1, conc_3_var_1);
    }
}

class PRED_conc_3_var_1 extends PRED_conc_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust(conc_3_2);
    }
}

class PRED_conc_3_1 extends PRED_conc_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        if ( !a2.unify(a3, engine.trail) ) return engine.fail();
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_conc_3_2 extends PRED_conc_3 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a4 = ((ListTerm)a1).car();
            a5 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a4 = new VariableTerm(engine);
            a5 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a4, a5), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a3.isList() ){
            if ( !a4.unify(((ListTerm)a3).car(), engine.trail) )
                return engine.fail();
            a6 = ((ListTerm)a3).cdr();
        } else if ( a3.isVariable() ){
            a6 = new VariableTerm(engine);
            if ( !a3.unify(new ListTerm(a4, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a7 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a7, cont);
        p2 = new PRED_conc_3(a5, a2, a6, p1);
        return new PRED_$get_level_1(a7, p2);
    }
}

