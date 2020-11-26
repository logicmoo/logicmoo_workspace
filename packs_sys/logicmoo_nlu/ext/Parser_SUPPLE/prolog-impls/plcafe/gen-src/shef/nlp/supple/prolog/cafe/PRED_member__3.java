package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_member__3.java
 * @procedure member_/3 in supple_utils.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_member__3 extends Predicate {
    static PRED_member__3 entry_code;
    static Predicate member__3_1 = new PRED_member__3_1();
    static Predicate member__3_2 = new PRED_member__3_2();
    static Predicate member__3_var = new PRED_member__3_var();

    public Term arg1, arg2, arg3;

    public PRED_member__3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_member__3(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        entry_code = this;
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.switch_on_term(
                                   member__3_var,
                                   member__3_1,
                                   member__3_1,
                                   member__3_1,
                                   member__3_var
                                   );
    }

    public int arity() { return 3; }

    public String toString() {
        return "member_(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_member__3_var extends PRED_member__3 {
    static Predicate member__3_var_1 = new PRED_member__3_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(member__3_1, member__3_var_1);
    }
}

class PRED_member__3_var_1 extends PRED_member__3 {

    public Predicate exec(Prolog engine) {
        return engine.trust(member__3_2);
    }
}

class PRED_member__3_1 extends PRED_member__3 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !a2.unify(a3, engine.trail) ) return engine.fail();
        return cont;
    }
}

class PRED_member__3_2 extends PRED_member__3 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5;
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
        engine.aregs[1] = a5;
        engine.aregs[2] = a4;
        engine.aregs[3] = a3;
        engine.cont = cont;
        return entry_code.call(engine);
    }
}

