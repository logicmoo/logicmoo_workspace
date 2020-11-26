package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_is_list_1.java
 * @procedure is_list/1 in supple_utils.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_is_list_1 extends Predicate {
    static PRED_is_list_1 entry_code;
    static Predicate is_list_1_1 = new PRED_is_list_1_1();
    static Predicate is_list_1_2 = new PRED_is_list_1_2();
    static Predicate is_list_1_3 = new PRED_is_list_1_3();
    static Predicate is_list_1_con = new PRED_is_list_1_con();
    static Predicate is_list_1_lis = new PRED_is_list_1_lis();
    static Predicate is_list_1_var = new PRED_is_list_1_var();

    public Term arg1;

    public PRED_is_list_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_is_list_1(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        entry_code = this;
        engine.aregs[1] = arg1;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.switch_on_term(
                                   is_list_1_var,
                                   is_list_1_1,
                                   is_list_1_con,
                                   is_list_1_1,
                                   is_list_1_lis
                                   );
    }

    public int arity() { return 1; }

    public String toString() {
        return "is_list(" + arg1 + ")";
    }
}

class PRED_is_list_1_var extends PRED_is_list_1 {
    static Predicate is_list_1_var_1 = new PRED_is_list_1_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(is_list_1_1, is_list_1_var_1);
    }
}

class PRED_is_list_1_var_1 extends PRED_is_list_1 {
    static Predicate is_list_1_var_2 = new PRED_is_list_1_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(is_list_1_2, is_list_1_var_2);
    }
}

class PRED_is_list_1_var_2 extends PRED_is_list_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust(is_list_1_3);
    }
}

class PRED_is_list_1_con extends PRED_is_list_1 {
    static Predicate is_list_1_con_1 = new PRED_is_list_1_con_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(is_list_1_1, is_list_1_con_1);
    }
}

class PRED_is_list_1_con_1 extends PRED_is_list_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust(is_list_1_2);
    }
}

class PRED_is_list_1_lis extends PRED_is_list_1 {
    static Predicate is_list_1_lis_1 = new PRED_is_list_1_lis_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(is_list_1_1, is_list_1_lis_1);
    }
}

class PRED_is_list_1_lis_1 extends PRED_is_list_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust(is_list_1_3);
    }
}

class PRED_is_list_1_1 extends PRED_is_list_1 {

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        Predicate p1, p2, p3;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        a2 = new VariableTerm(engine);
        p1 = new PRED_fail_0(cont);
        p2 = new PRED_$cut_1(a2, p1);
        p3 = new PRED_var_1(a1, p2);
        return new PRED_$get_level_1(a2, p3);
    }
}

class PRED_is_list_1_2 extends PRED_is_list_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        return cont;
    }
}

class PRED_is_list_1_3 extends PRED_is_list_1 {

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a2 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a2 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(new VariableTerm(engine), a2), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        engine.aregs[1] = a2;
        engine.cont = cont;
        return entry_code.call(engine);
    }
}

