package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_next_edges_3.java
 * @procedure next_edges/3 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_next_edges_3 extends Predicate {
    static PRED_next_edges_3 entry_code;
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate next_edges_3_1 = new PRED_next_edges_3_1();
    static Predicate next_edges_3_2 = new PRED_next_edges_3_2();
    static Predicate next_edges_3_3 = new PRED_next_edges_3_3();
    static Predicate next_edges_3_lis = new PRED_next_edges_3_lis();
    static Predicate next_edges_3_var = new PRED_next_edges_3_var();

    public Term arg1, arg2, arg3;

    public PRED_next_edges_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_next_edges_3(){}
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
                                   next_edges_3_var,
                                   fail_0,
                                   next_edges_3_1,
                                   fail_0,
                                   next_edges_3_lis
                                   );
    }

    public int arity() { return 3; }

    public String toString() {
        return "next_edges(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_next_edges_3_var extends PRED_next_edges_3 {
    static Predicate next_edges_3_var_1 = new PRED_next_edges_3_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(next_edges_3_1, next_edges_3_var_1);
    }
}

class PRED_next_edges_3_var_1 extends PRED_next_edges_3 {
    static Predicate next_edges_3_var_2 = new PRED_next_edges_3_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(next_edges_3_2, next_edges_3_var_2);
    }
}

class PRED_next_edges_3_var_2 extends PRED_next_edges_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust(next_edges_3_3);
    }
}

class PRED_next_edges_3_lis extends PRED_next_edges_3 {
    static Predicate next_edges_3_lis_1 = new PRED_next_edges_3_lis_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(next_edges_3_2, next_edges_3_lis_1);
    }
}

class PRED_next_edges_3_lis_1 extends PRED_next_edges_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust(next_edges_3_3);
    }
}

class PRED_next_edges_3_1 extends PRED_next_edges_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        if ( !s1.unify(a3, engine.trail) ) return engine.fail();
        return cont;
    }
}

class PRED_next_edges_3_2 extends PRED_next_edges_3 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("edge", 10);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("[]");
    static IntegerTerm s3 = new IntegerTerm(0);
    static IntegerTerm s4 = new IntegerTerm(1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16;
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
        if ( a4.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a4).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a4).args();
            a6 = args[0];
            a7 = args[1];
            a8 = args[2];
            if ( !s2.unify(args[3],  engine.trail) )
                return engine.fail();
            a9 = args[5];
            a10 = args[7];
            a11 = args[8];
            a12 = args[9];
        } else if (a4.isVariable() ){
            a6 = new VariableTerm(engine);
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            a11 = new VariableTerm(engine);
            a12 = new VariableTerm(engine);
            Term[] args = {a6, a7, a8, s2, new VariableTerm(engine), a9, new VariableTerm(engine), a10, a11, a12};
            if ( !a4.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a13 = new VariableTerm(engine);
        a14 = new VariableTerm(engine);
        Term[] h5 = {a6, a7, a8, s2, s3, a9, s4, a10, a11, a12};
        a16 = new StructureTerm(f1, h5);
        a15 = new ListTerm(a16, a13);
        p1 = new PRED_append_3(a15, a14, a3, cont);
        p2 = new PRED_next_edges_3(a5, a2, a14, p1);
        return new PRED_child_edges_3(a9, a2, a13, p2);
    }
}

class PRED_next_edges_3_3 extends PRED_next_edges_3 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a4 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a4 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(new VariableTerm(engine), a4), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        engine.aregs[1] = a4;
        engine.aregs[2] = a2;
        engine.aregs[3] = a3;
        engine.cont = cont;
        return entry_code.call(engine);
    }
}

