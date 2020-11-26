package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_semflatten_2.java
 * @procedure semflatten/2 in semantics.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_semflatten_2 extends Predicate {
    static Predicate semflatten_2_1 = new PRED_semflatten_2_1();
    static Predicate semflatten_2_2 = new PRED_semflatten_2_2();
    static Predicate semflatten_2_3 = new PRED_semflatten_2_3();
    static Predicate semflatten_2_4 = new PRED_semflatten_2_4();
    static Predicate semflatten_2_5 = new PRED_semflatten_2_5();
    static Predicate semflatten_2_con = new PRED_semflatten_2_con();
    static Predicate semflatten_2_int = new PRED_semflatten_2_int();
    static Predicate semflatten_2_lis = new PRED_semflatten_2_lis();
    static Predicate semflatten_2_str = new PRED_semflatten_2_str();
    static Predicate semflatten_2_var = new PRED_semflatten_2_var();

    public Term arg1, arg2;

    public PRED_semflatten_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_semflatten_2(){}
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
                                   semflatten_2_var,
                                   semflatten_2_int,
                                   semflatten_2_con,
                                   semflatten_2_str,
                                   semflatten_2_lis
                                   );
    }

    public int arity() { return 2; }

    public String toString() {
        return "semflatten(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_semflatten_2_var extends PRED_semflatten_2 {
    static Predicate semflatten_2_var_1 = new PRED_semflatten_2_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(semflatten_2_1, semflatten_2_var_1);
    }
}

class PRED_semflatten_2_var_1 extends PRED_semflatten_2 {
    static Predicate semflatten_2_var_2 = new PRED_semflatten_2_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(semflatten_2_2, semflatten_2_var_2);
    }
}

class PRED_semflatten_2_var_2 extends PRED_semflatten_2 {
    static Predicate semflatten_2_var_3 = new PRED_semflatten_2_var_3();

    public Predicate exec(Prolog engine) {
        return engine.retry(semflatten_2_3, semflatten_2_var_3);
    }
}

class PRED_semflatten_2_var_3 extends PRED_semflatten_2 {
    static Predicate semflatten_2_var_4 = new PRED_semflatten_2_var_4();

    public Predicate exec(Prolog engine) {
        return engine.retry(semflatten_2_4, semflatten_2_var_4);
    }
}

class PRED_semflatten_2_var_4 extends PRED_semflatten_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(semflatten_2_5);
    }
}

class PRED_semflatten_2_int extends PRED_semflatten_2 {
    static Predicate semflatten_2_int_1 = new PRED_semflatten_2_int_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(semflatten_2_1, semflatten_2_int_1);
    }
}

class PRED_semflatten_2_int_1 extends PRED_semflatten_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(semflatten_2_5);
    }
}

class PRED_semflatten_2_con extends PRED_semflatten_2 {
    static Predicate semflatten_2_con_1 = new PRED_semflatten_2_con_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(semflatten_2_1, semflatten_2_con_1);
    }
}

class PRED_semflatten_2_con_1 extends PRED_semflatten_2 {
    static Predicate semflatten_2_con_2 = new PRED_semflatten_2_con_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(semflatten_2_4, semflatten_2_con_2);
    }
}

class PRED_semflatten_2_con_2 extends PRED_semflatten_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(semflatten_2_5);
    }
}

class PRED_semflatten_2_str extends PRED_semflatten_2 {
    static Predicate semflatten_2_str_1 = new PRED_semflatten_2_str_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(semflatten_2_1, semflatten_2_str_1);
    }
}

class PRED_semflatten_2_str_1 extends PRED_semflatten_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(semflatten_2_5);
    }
}

class PRED_semflatten_2_lis extends PRED_semflatten_2 {
    static Predicate semflatten_2_lis_1 = new PRED_semflatten_2_lis_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(semflatten_2_1, semflatten_2_lis_1);
    }
}

class PRED_semflatten_2_lis_1 extends PRED_semflatten_2 {
    static Predicate semflatten_2_lis_2 = new PRED_semflatten_2_lis_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(semflatten_2_2, semflatten_2_lis_2);
    }
}

class PRED_semflatten_2_lis_2 extends PRED_semflatten_2 {
    static Predicate semflatten_2_lis_3 = new PRED_semflatten_2_lis_3();

    public Predicate exec(Prolog engine) {
        return engine.retry(semflatten_2_3, semflatten_2_lis_3);
    }
}

class PRED_semflatten_2_lis_3 extends PRED_semflatten_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(semflatten_2_5);
    }
}

class PRED_semflatten_2_1 extends PRED_semflatten_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("MISSING");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        Predicate p1, p2, p3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        a3 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a3, cont);
        p2 = new PRED_$unify_2(a1, s1, p1);
        p3 = new PRED_var_1(a1, p2);
        return new PRED_$get_level_1(a3, p3);
    }
}

class PRED_semflatten_2_2 extends PRED_semflatten_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("MISSING");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1, p2, p3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a3 = ((ListTerm)a1).car();
            if ( !s1.unify(((ListTerm)a1).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a1.isVariable() ){
            a3 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a3, s1), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a4 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a4, cont);
        p2 = new PRED_$unify_2(a3, s2, p1);
        p3 = new PRED_var_1(a3, p2);
        return new PRED_$get_level_1(a4, p3);
    }
}

class PRED_semflatten_2_3 extends PRED_semflatten_2 {

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
        p2 = new PRED_append_3(a6, a7, a2, p1);
        p3 = new PRED_semflatten_2(a4, a7, p2);
        p4 = new PRED_semflatten_2(a3, a6, p3);
        return new PRED_$get_level_1(a5, p4);
    }
}

class PRED_semflatten_2_4 extends PRED_semflatten_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        if ( !s1.unify(a2, engine.trail) ) return engine.fail();
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_semflatten_2_5 extends PRED_semflatten_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a2.isList() ){
            if ( !a1.unify(((ListTerm)a2).car(), engine.trail) )
                return engine.fail();
            if ( !s1.unify(((ListTerm)a2).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a2.isVariable() ){
            if ( !a2.unify(new ListTerm(a1, s1), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return new PRED_$neck_cut_0(cont);
    }
}

