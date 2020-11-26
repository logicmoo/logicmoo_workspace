package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_name_conc1_2.java
 * @procedure name_conc1/2 in supple_utils.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_name_conc1_2 extends Predicate {
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate name_conc1_2_1 = new PRED_name_conc1_2_1();
    static Predicate name_conc1_2_2 = new PRED_name_conc1_2_2();
    static Predicate name_conc1_2_var = new PRED_name_conc1_2_var();

    public Term arg1, arg2;

    public PRED_name_conc1_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_name_conc1_2(){}
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
        return engine.switch_on_term(
                                   name_conc1_2_var,
                                   fail_0,
                                   name_conc1_2_1,
                                   fail_0,
                                   name_conc1_2_2
                                   );
    }

    public int arity() { return 2; }

    public String toString() {
        return "name_conc1(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_name_conc1_2_var extends PRED_name_conc1_2 {
    static Predicate name_conc1_2_var_1 = new PRED_name_conc1_2_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(name_conc1_2_1, name_conc1_2_var_1);
    }
}

class PRED_name_conc1_2_var_1 extends PRED_name_conc1_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(name_conc1_2_2);
    }
}

class PRED_name_conc1_2_1 extends PRED_name_conc1_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        if ( !s1.unify(a2, engine.trail) ) return engine.fail();
        return cont;
    }
}

class PRED_name_conc1_2_2 extends PRED_name_conc1_2 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a3 = ((ListTerm)a1).car();
            a4 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a3 = new VariableTerm(engine);
            a4 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a3, a4), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a5 = new VariableTerm(engine);
        a6 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a5, cont);
        p2 = new PRED_conc_3(a6, a7, a2, p1);
        p3 = new PRED_name_conc1_2(a4, a7, p2);
        p4 = new PRED_name_2(a3, a6, p3);
        return new PRED_$get_level_1(a5, p4);
    }
}

