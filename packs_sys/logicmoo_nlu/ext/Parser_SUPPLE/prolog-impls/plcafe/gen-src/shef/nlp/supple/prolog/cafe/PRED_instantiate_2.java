package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_instantiate_2.java
 * @procedure instantiate/2 in update_name_match.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_instantiate_2 extends Predicate {
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate instantiate_2_1 = new PRED_instantiate_2_1();
    static Predicate instantiate_2_2 = new PRED_instantiate_2_2();
    static Predicate instantiate_2_var = new PRED_instantiate_2_var();

    public Term arg1, arg2;

    public PRED_instantiate_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_instantiate_2(){}
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
                                   instantiate_2_var,
                                   fail_0,
                                   instantiate_2_1,
                                   fail_0,
                                   instantiate_2_2
                                   );
    }

    public int arity() { return 2; }

    public String toString() {
        return "instantiate(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_instantiate_2_var extends PRED_instantiate_2 {
    static Predicate instantiate_2_var_1 = new PRED_instantiate_2_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(instantiate_2_1, instantiate_2_var_1);
    }
}

class PRED_instantiate_2_var_1 extends PRED_instantiate_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(instantiate_2_2);
    }
}

class PRED_instantiate_2_1 extends PRED_instantiate_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        return cont;
    }
}

class PRED_instantiate_2_2 extends PRED_instantiate_2 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1;
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
        p1 = new PRED_instantiate_2(a4, a2, cont);
        return new PRED_member_2(a3, a2, p1);
    }
}

