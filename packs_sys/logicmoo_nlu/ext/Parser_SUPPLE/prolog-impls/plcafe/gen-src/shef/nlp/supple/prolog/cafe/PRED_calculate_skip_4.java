package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_calculate_skip_4.java
 * @procedure calculate_skip/4 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_calculate_skip_4 extends Predicate {
    static PRED_calculate_skip_4 entry_code;
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate calculate_skip_4_1 = new PRED_calculate_skip_4_1();
    static Predicate calculate_skip_4_2 = new PRED_calculate_skip_4_2();
    static Predicate calculate_skip_4_var = new PRED_calculate_skip_4_var();

    public Term arg1, arg2, arg3, arg4;

    public PRED_calculate_skip_4(Term a1, Term a2, Term a3, Term a4, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        this.cont = cont;
    }

    public PRED_calculate_skip_4(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        arg4 = args[3]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        entry_code = this;
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.aregs[4] = arg4;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.switch_on_term(
                                   calculate_skip_4_var,
                                   fail_0,
                                   calculate_skip_4_1,
                                   fail_0,
                                   calculate_skip_4_2
                                   );
    }

    public int arity() { return 4; }

    public String toString() {
        return "calculate_skip(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ")";
    }
}

class PRED_calculate_skip_4_var extends PRED_calculate_skip_4 {
    static Predicate calculate_skip_4_var_1 = new PRED_calculate_skip_4_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(calculate_skip_4_1, calculate_skip_4_var_1);
    }
}

class PRED_calculate_skip_4_var_1 extends PRED_calculate_skip_4 {

    public Predicate exec(Prolog engine) {
        return engine.trust(calculate_skip_4_2);
    }
}

class PRED_calculate_skip_4_1 extends PRED_calculate_skip_4 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        a5 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a5, cont);
        p2 = new PRED_$minus_3(a3, a2, a4, p1);
        return new PRED_$get_level_1(a5, p2);
    }
}

class PRED_calculate_skip_4_2 extends PRED_calculate_skip_4 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("edge", 10);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a5 = ((ListTerm)a1).car();
            a6 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a5, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a5).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a5).args();
            a7 = args[0];
            a8 = args[1];
        } else if (a5.isVariable() ){
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            Term[] args = {a7, a8, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine)};
            if ( !a5.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a9 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        p1 = new PRED_$plus_3(a10, a9, a4, cont);
        p2 = new PRED_$minus_3(a7, a2, a10, p1);
        engine.aregs[1] = a6;
        engine.aregs[2] = a8;
        engine.aregs[3] = a3;
        engine.aregs[4] = a9;
        engine.cont = p2;
        return entry_code.call(engine);
    }
}

