package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_member_2.java
 * @procedure member/2 in supple_utils.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_member_2 extends Predicate {

    public Term arg1, arg2;

    public PRED_member_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_member_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4;
        a1 = arg1.dereference();
        a2 = arg2.dereference();

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
        return new PRED_member__3(a4, a3, a1, cont);
    }

    public int arity() { return 2; }

    public String toString() {
        return "member(" + arg1 + ", " + arg2 + ")";
    }
}

