package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_sort_edges_5.java
 * @procedure sort_edges/5 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_sort_edges_5 extends Predicate {
    static PRED_sort_edges_5 entry_code;
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate sort_edges_5_1 = new PRED_sort_edges_5_1();
    static Predicate sort_edges_5_2 = new PRED_sort_edges_5_2();
    static Predicate sort_edges_5_3 = new PRED_sort_edges_5_3();
    static Predicate sort_edges_5_lis = new PRED_sort_edges_5_lis();
    static Predicate sort_edges_5_var = new PRED_sort_edges_5_var();

    public Term arg1, arg2, arg3, arg4, arg5;

    public PRED_sort_edges_5(Term a1, Term a2, Term a3, Term a4, Term a5, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        arg5 = a5; 
        this.cont = cont;
    }

    public PRED_sort_edges_5(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        arg4 = args[3]; 
        arg5 = args[4]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        entry_code = this;
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.aregs[4] = arg4;
        engine.aregs[5] = arg5;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.switch_on_term(
                                   sort_edges_5_var,
                                   fail_0,
                                   sort_edges_5_1,
                                   fail_0,
                                   sort_edges_5_lis
                                   );
    }

    public int arity() { return 5; }

    public String toString() {
        return "sort_edges(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ")";
    }
}

class PRED_sort_edges_5_var extends PRED_sort_edges_5 {
    static Predicate sort_edges_5_var_1 = new PRED_sort_edges_5_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(sort_edges_5_1, sort_edges_5_var_1);
    }
}

class PRED_sort_edges_5_var_1 extends PRED_sort_edges_5 {
    static Predicate sort_edges_5_var_2 = new PRED_sort_edges_5_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(sort_edges_5_2, sort_edges_5_var_2);
    }
}

class PRED_sort_edges_5_var_2 extends PRED_sort_edges_5 {

    public Predicate exec(Prolog engine) {
        return engine.trust(sort_edges_5_3);
    }
}

class PRED_sort_edges_5_lis extends PRED_sort_edges_5 {
    static Predicate sort_edges_5_lis_1 = new PRED_sort_edges_5_lis_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(sort_edges_5_2, sort_edges_5_lis_1);
    }
}

class PRED_sort_edges_5_lis_1 extends PRED_sort_edges_5 {

    public Predicate exec(Prolog engine) {
        return engine.trust(sort_edges_5_3);
    }
}

class PRED_sort_edges_5_1 extends PRED_sort_edges_5 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        if ( !a2.unify(a3, engine.trail) ) return engine.fail();
        if ( !a4.unify(a5, engine.trail) ) return engine.fail();
        return cont;
    }
}

class PRED_sort_edges_5_2 extends PRED_sort_edges_5 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("edge", 10);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("[]");
    static IntegerTerm s3 = new IntegerTerm(1);
    static IntegerTerm s4 = new IntegerTerm(0);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a6 = ((ListTerm)a1).car();
            a7 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a6 = new VariableTerm(engine);
            a7 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a6, a7), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a6.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a6).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a6).args();
            a8 = args[0];
            a9 = args[1];
            a10 = args[2];
            if ( !s2.unify(args[3],  engine.trail) )
                return engine.fail();
            a11 = args[5];
            if ( !s3.unify(args[6],  engine.trail) )
                return engine.fail();
            a12 = args[7];
            a13 = args[8];
            a14 = args[9];
        } else if (a6.isVariable() ){
            a8 = new VariableTerm(engine);
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            a11 = new VariableTerm(engine);
            a12 = new VariableTerm(engine);
            a13 = new VariableTerm(engine);
            a14 = new VariableTerm(engine);
            Term[] args = {a8, a9, a10, s2, new VariableTerm(engine), a11, s3, a12, a13, a14};
            if ( !a6.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        Term[] h5 = {a8, a9, a10, s2, s4, a11, s3, a12, a13, a14};
        a15 = new StructureTerm(f1, h5);
        a16 = new VariableTerm(engine);
        a17 = new VariableTerm(engine);
        p1 = new PRED_sort_edges_5(a7, a16, a3, a17, a5, cont);
        p2 = new PRED_$dummy_parse_file46pl_15_3(a9, a4, a17, p1);
        return new PRED_add_to_sorted_edges_3(a15, a2, a16, p2);
    }
}

class PRED_sort_edges_5_3 extends PRED_sort_edges_5 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a6 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a6 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(new VariableTerm(engine), a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        engine.aregs[1] = a6;
        engine.aregs[2] = a2;
        engine.aregs[3] = a3;
        engine.aregs[4] = a4;
        engine.aregs[5] = a5;
        engine.cont = cont;
        return entry_code.call(engine);
    }
}

