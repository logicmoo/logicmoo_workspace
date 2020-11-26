package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_is_better_than_score_2.java
 * @procedure is_better_than_score/2 in best_parse.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_is_better_than_score_2 extends Predicate {
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate is_better_than_score_2_1 = new PRED_is_better_than_score_2_1();
    static Predicate is_better_than_score_2_2 = new PRED_is_better_than_score_2_2();
    static Predicate is_better_than_score_2_var = new PRED_is_better_than_score_2_var();

    public Term arg1, arg2;

    public PRED_is_better_than_score_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_is_better_than_score_2(){}
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
                                   is_better_than_score_2_var,
                                   fail_0,
                                   fail_0,
                                   is_better_than_score_2_var,
                                   fail_0
                                   );
    }

    public int arity() { return 2; }

    public String toString() {
        return "is_better_than_score(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_is_better_than_score_2_var extends PRED_is_better_than_score_2 {
    static Predicate is_better_than_score_2_var_1 = new PRED_is_better_than_score_2_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(is_better_than_score_2_1, is_better_than_score_2_var_1);
    }
}

class PRED_is_better_than_score_2_var_1 extends PRED_is_better_than_score_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(is_better_than_score_2_2);
    }
}

class PRED_is_better_than_score_2_1 extends PRED_is_better_than_score_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("-", 2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5;
        Predicate p1;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a1.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a1).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a1).args();
            a3 = args[0];
            a4 = args[1];
        } else if (a1.isVariable() ){
            a3 = new VariableTerm(engine);
            a4 = new VariableTerm(engine);
            Term[] args = {a3, a4};
            if ( !a1.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a2.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a2).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a2).args();
            if ( !a3.unify(args[0],  engine.trail) )
                return engine.fail();
            a5 = args[1];
        } else if (a2.isVariable() ){
            a5 = new VariableTerm(engine);
            Term[] args = {a3, a5};
            if ( !a2.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        p1 = new PRED_$less_than_2(a4, a5, cont);
        return new PRED_$neck_cut_0(p1);
    }
}

class PRED_is_better_than_score_2_2 extends PRED_is_better_than_score_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("-", 2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a1.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a1).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a1).args();
            a3 = args[0];
        } else if (a1.isVariable() ){
            a3 = new VariableTerm(engine);
            Term[] args = {a3, new VariableTerm(engine)};
            if ( !a1.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a2.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a2).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a2).args();
            a4 = args[0];
        } else if (a2.isVariable() ){
            a4 = new VariableTerm(engine);
            Term[] args = {a4, new VariableTerm(engine)};
            if ( !a2.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return new PRED_$greater_than_2(a3, a4, cont);
    }
}

